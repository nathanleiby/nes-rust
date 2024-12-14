use bitflags::bitflags;

use crate::{addr_register::AddrRegister, render::Frame, rom::Mirroring};

// TODO: move UI rendering stuff that ties to SDL2 out of PPU

#[derive(Default)]
pub struct PpuRegisters {
    /// Controller (0x2000) - instructs PPU on general logic flow
    control: ControlRegister,

    // Mask (0x2001) - instructs PPU how to render sprites and background
    mask: MaskRegister,

    // Status (0x2002) - reports PPU status
    status: StatusRegister,

    /// Address in Object Attribute Memory - the space responsible for sprites
    // TODO: OAMADDR is set to 0 during each of ticks 257–320 (the sprite tile loading interval) of the pre-render and visible scanlines. This also means that at the end of a normal complete rendered frame, OAMADDR will always have returned to 0.
    oam_address: u8,

    /// Scroll (0x2005) - instructs PPU how to set a viewport
    scroll: PpuScrollRegister,

    /// Address (0x2006) and Data (0x2007) - provides access to memory map available for PPU
    address: AddrRegister,
}

pub struct Ppu {
    /// CHR ROM (also called "pattern tables")
    chr_rom: Vec<u8>,

    /// VRAM (also called "name tables")
    /// 1024 bytes make up a single "Frame"
    /// The PPU has 2048 bytes here, representing 2 frames.
    /// NOTE: the PPU is addressable via 4096 bytes, meaning there's some remapping of those additional bytes.
    ///         This is either via Mirroring, or maps to extra RAM on the Cartridge.
    ///
    /// The background of the 256x240 screen is made by doing lookups of 32x30 bytes (0x2000 to 0x23BF), each byte an 8x8 sprite from the selected "Pattern Table"
    /// Top Left  = 0x2000, Top Right = 0x201F
    ///     ...                 ...
    /// Bot Let   = 0x23A0, Bot Right = 0x23BF
    /// The choice of Pattern Table Bank is determined by the PPU's Control Register (BACKGROUND_PATTERN_ADDR)
    ///
    /// The remaining 64 bytes make up the Color Palette.
    /// A
    vram: [u8; 2048],

    // TODO: Are we only allowed to use 32 of the possible 64 NES colors?
    /// Palette Table
    palettes: [u8; 32],
    oam_data: [u8; 256],

    mirroring: Mirroring,

    registers: PpuRegisters,

    nmi_interrupt: Option<()>,
    read_data_buffer: u8,

    /// scanline within the Frame. There are 262 total (0..=261)
    scanline: usize,
    /// clock_cycles within the Frame (0..=261*341)
    clock_cycles: usize,
}

#[derive(PartialEq, Eq, Debug, Default, Copy, Clone)]
/// There are 4 possible background palettes (0,1,2,3) and 4 possible sprite palettes (4,5,6,7).
/// These 8 palettes each are made up of four numbers, which are indices into ppu.palettes (Palette Table)
pub struct PaletteIdx(usize);

#[derive(Default, Debug, PartialEq, Eq)]
pub struct Sprite {
    pub x: u8,
    pub y: u8,
    pub use_tile_bank_1: bool,
    pub tile_idx: u8,
    pub is_8_by_16: bool,
    pub palette_idx: PaletteIdx,
    pub behind_background: bool,
    pub flip_horizontal: bool,
    pub flip_vertical: bool,
}

impl Ppu {
    pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        Ppu {
            chr_rom,
            mirroring,
            vram: [0; 2048],
            palettes: [0; 32],
            oam_data: [0; 256],
            nmi_interrupt: None,

            registers: Default::default(),
            read_data_buffer: 0,

            scanline: 0,
            clock_cycles: 0,
        }
    }

    fn palette_from_palette_idx(&self, palette_idx: PaletteIdx) -> [u8; 4] {
        let p_idx = palette_idx.0;
        assert!(p_idx < 8);

        let is_background = p_idx < 4;
        let start = p_idx * 4;
        [
            // The first entry is a special case
            if is_background { self.palettes[0] } else { 0 },
            self.palettes[start + 1],
            self.palettes[start + 2],
            self.palettes[start + 3],
        ]
    }

    // Gets the Palette for a given tile
    /// See: https://www.nesdev.org/wiki/PPU_attribute_tables
    fn palette_for_bg_tile(&self, pos: (usize, usize)) -> PaletteIdx {
        let (x, y) = pos;

        // For background tiles, the last 64 bytes of each nametable are reserved
        // for assigning a specific palette to a part of the background.
        // This section is called an attribute table.
        let bank = if self
            .registers
            .control
            .contains(ControlRegister::BACKGROUND_PATTERN_ADDR)
        {
            1
        } else {
            0
        };

        let nametable_size = 1024;
        let attr_table_size = 64;
        let nt_end = nametable_size * (bank + 1);

        let attr_table = &self.vram[nt_end - attr_table_size..nt_end];
        let attr_table_idx = (y / 4) * 8 + (x / 4);
        let attr_table_byte = attr_table[attr_table_idx];

        let meta_tile = (x % 4 < 2, y % 4 < 2);
        let shift = match meta_tile {
            (true, true) => 0,   // top-left
            (false, true) => 2,  // top-right
            (true, false) => 4,  // bottom-left
            (false, false) => 6, // bottom-right
        };

        let palette_idx = ((attr_table_byte >> shift) & 0b11) as usize;
        assert!(palette_idx < 4);

        PaletteIdx(palette_idx)
    }

    // tile_n can be thought of as the offset in the pattern table  (CHR ROM)
    // pos (*8) relates to the value in the name table.
    pub fn draw_background(&self, frame: &mut Frame) {
        // Determine which nametable is being used for the current screen (by reading bit 0 and bit 1 from Control register)
        let which_nametable = self
            .registers
            .control
            .intersection(ControlRegister::NAMETABLE)
            .bits() as usize;
        assert!(which_nametable <= 3);
        // TODO: which_nametable isn't being used.. this likely relates to scrolling+mirroring

        // Determine which CHR ROM bank (Pattern Table) is used for background tiles (by reading bit 4 from Control Register)
        let bank = if self
            .registers
            .control
            .contains(ControlRegister::BACKGROUND_PATTERN_ADDR)
        {
            1
        } else {
            0
        };
        // let bank = self
        //     .registers
        //     .control
        //     .intersection(ControlRegister::BACKGROUND_PATTERN_ADDR)
        //     .bits() as usize;
        assert!(bank <= 1);

        let rows = 30;
        let cols = 32;
        for y in 0..rows {
            for x in 0..cols {
                let tile_n = self.vram[y * cols + x] as usize;
                let bgp_idx = self.palette_for_bg_tile((x, y));
                let palette = self.palette_from_palette_idx(bgp_idx);
                frame.draw_bg_tile(&self.chr_rom, bank, tile_n, (x, y), palette);
            }
        }
    }

    pub fn draw_sprites(&self, frame: &mut Frame) {
        // Priority between sprites is determined by their address inside OAM.
        // .. the sprite data that occurs first will overlap any other sprites after it..
        // So let's draw things in reverse to handle that
        for b in self.oam_data.chunks(4).rev() {
            let sprite = self.parse_sprite_from_oam_data(b);
            let palette = self.palette_from_palette_idx(sprite.palette_idx);
            let _ = frame.draw_sprite(&self.chr_rom, &sprite, palette);
            // if visible && palette.iter().any(|x| *x != 0) {
            //     println!("palette: {:?}", palette);
            //     println!("Drew Sprite: {:?}", sprite);
            // }
        }
    }

    fn parse_sprite_from_oam_data(&self, b: &[u8]) -> Sprite {
        assert!(b.len() == 4);

        // parse sprite (4 bytes)
        let y = b[0];

        let tile_data = b[1];
        // this is a number 0 or 1, and maps to 0x0000 or 0x1000 within pattern table (chr rom)
        let (tile_bank, tile_idx, is_8_by_16) = if self
            .registers
            .control
            .contains(ControlRegister::SPRITE_SIZE)
        {
            // For 8x16 sprites (bit 5 of PPUCTRL set), the PPU ignores the pattern table selection and selects a pattern table from bit 0 of this number.
            let tile_bank = tile_data & 0b1;
            let tile_idx = tile_data & 0b1111_1110;

            (tile_bank, tile_idx, true)
        } else {
            // For 8x8 sprites, this is the tile number of this sprite within the pattern table selected in bit 3 of PPUCTRL ($2000).
            let tile_bank = if self
                .registers
                .control
                .contains(ControlRegister::SPRITE_PATTERN_ADDR)
            {
                1
            } else {
                0
            };

            (tile_bank, tile_data, false)
        };

        let attributes = b[2];
        let palette_idx = PaletteIdx((attributes & 0b11) as usize + 4);
        let foreground_priority = attributes & (1 << 5) > 0;
        let flip_horizontal = attributes & (1 << 6) > 0;
        let flip_vertical = attributes & (1 << 7) > 0;

        // let sprite_data = self.chr_rom[tile_bank as usize * 1024 + tile_idx];

        let x = b[3];

        Sprite {
            x,
            y,
            use_tile_bank_1: tile_bank == 1,
            tile_idx,
            is_8_by_16,
            palette_idx,
            behind_background: foreground_priority,
            flip_horizontal,
            flip_vertical,
        }
    }

    pub fn tick(&mut self, cycles: usize) -> bool {
        let mut should_rerender = false;

        self.clock_cycles += cycles;
        // each scanline lasts for 341 PPU clock cycles
        let scanline = self.clock_cycles / 341;

        if self.scanline < 241 && scanline >= 241 {
            // upon entering scanline 241, PPU triggers NMI interrupt
            if self.is_vblank_nmi_enabled() {
                self.nmi_interrupt = Some(());
            }
            // The VBlank flag of the PPU is set at tick 1 (the second tick) of scanline 241
            // Here we are approximating that by clearing on dot 0 or above.
            self.set_ppu_vblank_status(true);
            // TODO
            // self.registers
            //     .status
            //     .remove(StatusRegister::SPRITE_0_HIT_FLAG);
            should_rerender = true
        }

        // the PPU renders 262 scan lines per frame
        if self.scanline < 262 && scanline >= 262 {
            // If the vblank flag is not cleared by reading, it will be cleared automatically on dot 1 of the prerender scanline.
            // Here we are approximating that by clearing on dot 0 or above.
            self.set_ppu_vblank_status(false);
            self.nmi_interrupt = None;
            // TODO
            // self.registers
            //     .status
            //     .remove(StatusRegister::SPRITE_0_HIT_FLAG);
        }

        self.clock_cycles %= 262 * 341;
        self.scanline = scanline % 262;

        should_rerender
    }

    fn set_ppu_vblank_status(&mut self, is_vblank_active: bool) {
        self.registers
            .status
            .set(StatusRegister::VBLANK_FLAG, is_vblank_active)
    }

    fn is_vblank_nmi_enabled(&mut self) -> bool {
        self.registers
            .control
            .contains(ControlRegister::VBLANK_NMI_ENABLE)
    }

    /// returns (scanline, clock_cycles)
    pub fn get_tick_status(&self) -> (usize, usize) {
        (self.scanline, self.clock_cycles)
    }

    pub fn poll_nmi_interrupt(&mut self) -> Option<()> {
        self.nmi_interrupt.take()
    }

    pub fn write_to_ctrl(&mut self, data: u8) {
        let before = &self.registers.control;
        let after = ControlRegister::from_bits_truncate(data);

        // should we toggle an NMI interrupt?
        let is_vblank_state = self.registers.status.contains(StatusRegister::VBLANK_FLAG);
        let enabled_vblank_nmi = after.contains(ControlRegister::VBLANK_NMI_ENABLE)
            && !before.contains(ControlRegister::VBLANK_NMI_ENABLE);
        if is_vblank_state && enabled_vblank_nmi {
            self.nmi_interrupt = Some(());
        }

        // update the register's value
        self.registers.control = after;
    }

    pub fn write_to_mask(&mut self, data: u8) {
        self.registers.mask = MaskRegister::from_bits_truncate(data);
    }

    pub fn write_to_scroll_register(&mut self, data: u8) {
        if self.registers.scroll.is_y_scroll {
            self.registers.scroll.y_scroll = data;
        } else {
            self.registers.scroll.x_scroll = data;
        }
        self.registers.scroll.is_y_scroll = !self.registers.scroll.is_y_scroll;
    }

    fn increment_vram_addr(&mut self) {
        self.registers
            .address
            .increment(self.registers.control.vram_increment_amount());
    }

    pub fn write_to_addr(&mut self, data: u8) {
        self.registers.address.external_write(data);
    }

    pub fn read_from_data(&mut self) -> u8 {
        let addr = self.registers.address.get();
        self.increment_vram_addr();

        let val = match addr {
            0..0x2000 => self.chr_rom[addr as usize],
            0x2000..0x3F00 => {
                let mirrored = self.mirror_vram_addr(addr);
                self.vram[mirrored as usize]
            }
            0x3F00..0x4000 => {
                let addr = (addr - 0x3F00) % (self.palettes.len() as u16);
                self.palettes[addr as usize]
            }
            0x4000..=0xFFFF => todo!("read_data doesn't yet handle the mirrors range"),
        };

        let out = self.read_data_buffer;
        self.read_data_buffer = val;
        out
    }

    fn mirror_vram_addr(&mut self, addr: u16) -> u16 {
        // account for offset (0x2000) and remapping (0x3NNN -> 0x2NNN)
        let base = addr & 0x0FFF;
        let name_table_idx = base / 0x0400;

        // and ROM-configured mirroring

        match (self.mirroring, name_table_idx) {
            (Mirroring::Horizontal, 1) | (Mirroring::Horizontal, 2) => base - 0x0400,
            (Mirroring::Vertical, 2) | (Mirroring::Vertical, 3) | (Mirroring::Horizontal, 3) => {
                base - 0x0800
            }
            (Mirroring::Horizontal, _) | (Mirroring::Vertical, _) => base,
            (Mirroring::FourScreen, _) => {
                todo!("Four Screen requires specicial setup with more RAM. More info: https://www.nesdev.org/wiki/Mirroring#4-Screen")
            }
        }
    }

    pub fn write_to_data(&mut self, data: u8) {
        let addr = self.registers.address.get();
        self.increment_vram_addr();

        match addr {
            0..0x2000 => panic!("attempt to write to CHR ROM (read-only)"),
            0x2000..0x3F00 => {
                let mirrored = self.mirror_vram_addr(addr);
                self.vram[mirrored as usize] = data;
            }
            0x3F00..0x4000 => {
                let addr = (addr - 0x3F00) % (self.palettes.len() as u16);
                self.palettes[addr as usize] = data;
            }
            0x4000..=0xFFFF => todo!("read_data doesn't yet handle the mirrors range"),
        }
    }

    pub fn write_to_oam_data(&mut self, data: u8) {
        self.oam_data[self.registers.oam_address as usize] = data;
        self.registers.oam_address += 1;
    }

    pub fn read_from_oam_data(&mut self) -> u8 {
        self.oam_data[self.registers.oam_address as usize]
    }

    pub fn write_to_oam_address(&mut self, data: u8) {
        self.registers.oam_address = data;
    }

    pub fn read_from_status(&mut self) -> u8 {
        let result = self.registers.status.bits();

        // Reading this register has the side effect of clearing the PPU's internal w register.
        // It is commonly read before writes to PPUSCROLL and PPUADDR to ensure the writes occur in the correct order.
        self.reset_latch();

        // Reading PPUSTATUS will return the current state of the Vblank flag and then clear it
        self.set_ppu_vblank_status(false);

        result
    }

    fn reset_latch(&mut self) {
        self.registers.address.reset_latch();
        self.registers.scroll.is_y_scroll = false;
    }
}

bitflags! {
    /// 7  bit  0
    /// ---- ----
    /// VPHB SINN
    /// |||| ||||
    /// |||| ||++- Base nametable address
    /// |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
    /// |||| |+--- VRAM address increment per CPU read/write of PPUDATA
    /// |||| |     (0: add 1, going across; 1: add 32, going down)
    /// |||| +---- Sprite pattern table address for 8x8 sprites
    /// ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
    /// |||+------ Background pattern table address (0: $0000; 1: $1000)
    /// ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels – see PPU OAM#Byte 1)
    /// |+-------- PPU master/slave select
    /// |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
    /// +--------- Vblank NMI enable (0: off, 1: on)
    #[derive(Default, Clone, Copy)]
    pub struct ControlRegister: u8 {
        const NAMETABLE = 0b11;
        // Current nametable bits in PPUCTRL bits 0 and 1 can equivalently be considered the most significant bit of the scroll coordinates, which are 9 bits wide
        /// X scroll position bit 8 (i.e. add 256 to X)
        const X_SCROLL = 1 << 0;
        /// Y scroll position bit 8 (i.e. add 240 to Y)
        const Y_SCROLL = 1 << 1;

        const VRAM_INCREMENT = 1 << 2;
        const SPRITE_PATTERN_ADDR = 1 << 3;
        const BACKGROUND_PATTERN_ADDR = 1 << 4;
        const SPRITE_SIZE = 1 << 5;
        const MASTER_SLAVE_SELECT = 1 << 6;
        const VBLANK_NMI_ENABLE = 1 << 7;
    }
}

bitflags! {
    // 7  bit  0
    // ---- ----
    // VSOx xxxx
    // |||| ||||
    // |||+-++++- (PPU open bus or 2C05 PPU identifier)
    // ||+------- Sprite overflow flag
    // |+-------- Sprite 0 hit flag
    // +--------- Vblank flag, cleared on read. Unreliable; see below.
    #[derive(Default)]
    pub struct StatusRegister: u8 {
        const OPEN_BUS = 0b0001_1111;
        const SPRITE_OVERFLOW_FLAG = 1 << 5;
        const SPRITE_0_HIT_FLAG = 1 << 6;
        const VBLANK_FLAG = 1 << 7;
    }
}

bitflags! {
    /// 7  bit  0
    /// ---- ----
    /// BGRs bMmG
    /// |||| ||||
    /// |||| |||+- Greyscale (0: normal color, 1: greyscale)
    /// |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
    /// |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
    /// |||| +---- 1: Enable background rendering
    /// |||+------ 1: Enable sprite rendering
    /// ||+------- Emphasize red (green on PAL/Dendy)
    /// |+-------- Emphasize green (red on PAL/Dendy)
    /// +--------- Emphasize blue
    #[derive(Default)]
    pub struct MaskRegister: u8 {
        const GREYSCALE = 1 << 0;
        const SHOW_BACKGROUND = 1 << 1;
        const SHOW_SPRITES = 1 << 2;
        const ENABLE_BACKGROUND_RENDERING = 1 << 3;
        const ENABLE_SPRITE_RENDERING = 1 << 4;
        const EMPHASIZE_RED = 1 << 5;
        const EMPHASIZE_GREEN = 1 << 6;
        const EMPHASIZE_BLUE = 1 << 7;
    }
}

impl ControlRegister {
    pub fn vram_increment_amount(&self) -> u8 {
        if self.contains(ControlRegister::VRAM_INCREMENT) {
            32
        } else {
            1
        }
    }
}

#[derive(Default)]
struct PpuScrollRegister {
    x_scroll: u8,
    y_scroll: u8,

    // TODO: Actually controlled by w_register, which is shared with AddrRegister
    is_y_scroll: bool,
}

#[cfg(test)]
mod tests {
    use crate::{assert_eq_bits, rom::Rom};

    use super::*;

    // #[test]
    // fn test_addr_and_data_register() {
    //     todo!()
    // }

    fn get_ppu_vblank_status(ppu: &mut Ppu) -> bool {
        ppu.registers.status.contains(StatusRegister::VBLANK_FLAG)
    }

    fn new_test_ppu() -> Ppu {
        let rom = Rom::new_test_rom(vec![]);
        Ppu::new(rom.chr_rom.clone(), rom.mirroring)
    }

    #[test]
    fn test_tick() {
        let mut ppu = new_test_ppu();

        assert_eq!(ppu.get_tick_status(), (0, 0));
        assert_eq!(ppu.nmi_interrupt, None);
        assert!(!ppu.registers.status.contains(StatusRegister::VBLANK_FLAG),);
        assert!(!ppu.is_vblank_nmi_enabled());

        ppu.registers
            .control
            .insert(ControlRegister::VBLANK_NMI_ENABLE);
        assert!(ppu.is_vblank_nmi_enabled());

        ppu.tick(341);
        assert_eq!(ppu.get_tick_status(), (1, 341));
        assert!(!get_ppu_vblank_status(&mut ppu));

        let should_rerender = ppu.tick(240 * 341);
        assert_eq!(ppu.get_tick_status(), (241, 241 * 341));
        assert!(should_rerender);
        assert!(ppu.poll_nmi_interrupt().is_some());
        assert!(ppu.poll_nmi_interrupt().is_none());
        assert!(get_ppu_vblank_status(&mut ppu));

        let should_rerender = ppu.tick(21 * 341);
        assert_eq!(
            ppu.get_tick_status(),
            (0, 0),
            "should wrap back scanline to 0 after 262 scanlines"
        );
        assert!(!should_rerender);
        assert!(ppu.poll_nmi_interrupt().is_none());
        assert!(!get_ppu_vblank_status(&mut ppu));
    }

    #[test]
    fn test_read_status_resets_vblank_flag() {
        let mut ppu = new_test_ppu();
        let status = ppu.read_from_status();
        assert_eq!(status, 0);
        ppu.registers.status.insert(StatusRegister::VBLANK_FLAG);

        let status = ppu.read_from_status();
        assert_eq!(status, 1 << 7);
        let status = ppu.read_from_status();
        assert_eq!(
            status, 0,
            "the first read from status should have reset the VBlank flag"
        );
    }

    #[test]
    fn test_write_to_ctrl_can_trigger_nmi_if_status_in_vblank() {
        let mut ppu = new_test_ppu();
        assert_eq_bits!(ppu.registers.control.bits(), 0);
        assert!(ppu.poll_nmi_interrupt().is_none());
        ppu.write_to_ctrl(ControlRegister::VBLANK_NMI_ENABLE.bits());
        assert!(ppu.poll_nmi_interrupt().is_none());

        ppu.registers.status.insert(StatusRegister::VBLANK_FLAG);
        ppu.write_to_ctrl(ControlRegister::VBLANK_NMI_ENABLE.bits());
        assert!(ppu.poll_nmi_interrupt().is_none());

        ppu.registers.control = ControlRegister::empty();
        ppu.write_to_ctrl(ControlRegister::VBLANK_NMI_ENABLE.bits());
        assert!(ppu.poll_nmi_interrupt().is_some());

        // should consume the interrupt by polling it
        assert!(ppu.poll_nmi_interrupt().is_none());
    }

    #[test]
    fn test_palette_for_bg_tile() {
        let mut ppu = new_test_ppu();

        // All of the reads in the next group (x in 0..=3, y in 0..=3) map to this part of vram
        // This is the 0th index in the attribute table
        let attr_table_start = 1024 - 64;
        ppu.vram[attr_table_start] = 0b00011011;

        assert_eq!(ppu.palette_for_bg_tile((0, 0)), PaletteIdx(3));
        assert_eq!(ppu.palette_for_bg_tile((1, 0)), PaletteIdx(3));
        assert_eq!(ppu.palette_for_bg_tile((0, 1)), PaletteIdx(3));
        assert_eq!(ppu.palette_for_bg_tile((1, 1)), PaletteIdx(3));

        assert_eq!(ppu.palette_for_bg_tile((2, 0)), PaletteIdx(2));
        assert_eq!(ppu.palette_for_bg_tile((3, 0)), PaletteIdx(2));
        assert_eq!(ppu.palette_for_bg_tile((2, 1)), PaletteIdx(2));
        assert_eq!(ppu.palette_for_bg_tile((3, 1)), PaletteIdx(2));

        assert_eq!(ppu.palette_for_bg_tile((0, 2)), PaletteIdx(1));
        assert_eq!(ppu.palette_for_bg_tile((1, 3)), PaletteIdx(1));
        assert_eq!(ppu.palette_for_bg_tile((0, 2)), PaletteIdx(1));
        assert_eq!(ppu.palette_for_bg_tile((1, 3)), PaletteIdx(1));

        assert_eq!(ppu.palette_for_bg_tile((2, 2)), PaletteIdx(0));
        assert_eq!(ppu.palette_for_bg_tile((3, 3)), PaletteIdx(0));
        assert_eq!(ppu.palette_for_bg_tile((2, 2)), PaletteIdx(0));
        assert_eq!(ppu.palette_for_bg_tile((3, 3)), PaletteIdx(0));

        ppu.vram[attr_table_start + 1] = 0b11111111;
        for x in 4..=7 {
            for y in 0..=3 {
                assert_eq!(ppu.palette_for_bg_tile((x, y)), PaletteIdx(3));
            }
        }

        ppu.vram[attr_table_start + 2] = 0b01010101;
        for x in 8..=11 {
            for y in 0..=3 {
                assert_eq!(ppu.palette_for_bg_tile((x, y)), PaletteIdx(1));
            }
        }

        ppu.vram[attr_table_start + 7] = 0b10101010;
        for x in 28..=31 {
            for y in 0..=3 {
                assert_eq!(ppu.palette_for_bg_tile((x, y)), PaletteIdx(2));
            }
        }

        ppu.vram[attr_table_start + 9] = 0b10101010;
        for x in 4..=7 {
            for y in 4..=7 {
                assert_eq!(ppu.palette_for_bg_tile((x, y)), PaletteIdx(2));
            }
        }
    }

    #[test]
    fn test_palette_from_bg_palette_idx() {
        let mut ppu = new_test_ppu();

        ppu.palettes[0] = 255;
        ppu.palettes[1..32].copy_from_slice(Vec::from_iter(1..32).as_slice());

        assert_eq!(ppu.palette_from_palette_idx(PaletteIdx(0)), [255, 1, 2, 3]);
        assert_eq!(ppu.palette_from_palette_idx(PaletteIdx(1)), [255, 5, 6, 7]);
        assert_eq!(
            ppu.palette_from_palette_idx(PaletteIdx(2)),
            [255, 9, 10, 11]
        );
        assert_eq!(
            ppu.palette_from_palette_idx(PaletteIdx(3)),
            [255, 13, 14, 15]
        );

        // sprite palette
        assert_eq!(ppu.palette_from_palette_idx(PaletteIdx(4)), [0, 17, 18, 19]);

        assert_eq!(ppu.palette_from_palette_idx(PaletteIdx(5)), [0, 21, 22, 23]);
    }

    #[test]
    fn test_parse_sprite_from_oam_data() {
        let mut ppu = new_test_ppu();

        let expected = Sprite {
            palette_idx: PaletteIdx(4),
            ..Default::default()
        };
        assert_eq!(ppu.parse_sprite_from_oam_data(&[0, 0, 0, 0]), expected);

        ppu.registers
            .control
            .insert(ControlRegister::SPRITE_PATTERN_ADDR);
        let expected = Sprite {
            x: 5,
            y: 10,
            tile_idx: 123,
            palette_idx: PaletteIdx(6),
            flip_horizontal: true,
            flip_vertical: true,
            behind_background: true,
            use_tile_bank_1: true,
            ..Default::default()
        };
        assert_eq!(
            ppu.parse_sprite_from_oam_data(&[10, 123, 0b1110_0010, 5]),
            expected
        );

        ppu.registers.control.insert(ControlRegister::SPRITE_SIZE);
        let expected = Sprite {
            x: 1,
            y: 2,
            tile_idx: 16,
            palette_idx: PaletteIdx(7),
            flip_vertical: true,
            behind_background: true,
            is_8_by_16: true,
            ..Default::default()
        };
        assert_eq!(
            ppu.parse_sprite_from_oam_data(&[2, 0b0001_0000, 0b1010_0011, 1]),
            expected
        );
    }
}
