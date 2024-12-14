use bitflags::bitflags;

use crate::{
    addr_register::AddrRegister,
    palette::{self, SYSTEM_PALETTE},
    rom::Mirroring,
    utility::{addr_from, split_addr},
};

// TODO: move UI rendering stuff that ties to SDL2 out of PPU
pub struct Frame {
    pub data: Vec<u8>,
}

impl Frame {
    const WIDTH: usize = 256;
    const HEIGHT: usize = 240;

    pub fn new() -> Self {
        Self {
            data: vec![0; Frame::WIDTH * Frame::HEIGHT * 3],
        }
    }

    pub fn set_pixel(&mut self, x: usize, y: usize, rgb: (u8, u8, u8)) {
        let base = y * 3 * Frame::WIDTH + x * 3;
        if base + 2 < self.data.len() {
            self.data[base] = rgb.0;
            self.data[base + 1] = rgb.1;
            self.data[base + 2] = rgb.2
        }
    }

    // tile_n can be thought of as the offset in the pattern table  (CHR ROM)
    // pos (*8) relates to the value in the name table.
    /// VRAM (also called "name tables")
    pub fn draw_tile(
        &mut self,
        chr_rom: &[u8],
        bank: usize,
        tile_n: usize,
        pos: (usize, usize),
        palette: [u8; 4],
    ) {
        assert!(bank <= 1);

        let tile_size_bytes = 16;
        let bank_size_bytes: usize = 4096;
        let tile_size = 8;

        let (x, y) = pos;
        for row in 0..tile_size {
            let first_byte_idx = bank * bank_size_bytes + tile_n * tile_size_bytes + row;
            let first_byte = chr_rom[first_byte_idx];
            let second_byte_idx = bank * bank_size_bytes + tile_n * tile_size_bytes + 8 + row;
            let second_byte = chr_rom[second_byte_idx];

            if x == 0 && y == 0 {
                log::info!(
                    "(x:{:2},y:{:2}) 1: [{:04x}] {:04x} , 2: [{:04x}] {:04x}",
                    x,
                    y,
                    first_byte_idx,
                    first_byte,
                    second_byte_idx,
                    second_byte
                );
            }

            for col in 0..tile_size {
                let which_bit = 1 << (7 - col);
                let lo_bit = first_byte & which_bit > 0;
                let hi_bit = second_byte & which_bit > 0;
                let palette_idx: u8 = ((hi_bit as u8) << 1) + (lo_bit as u8);
                assert!(palette_idx < 4, "palette_idx was {}", palette_idx);
                let color = SYSTEM_PALETTE[palette[palette_idx as usize] as usize];

                self.set_pixel((x * tile_size) + col, (y * tile_size) + row, color);
            }
        }
    }
}

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

#[derive(PartialEq, Eq, Debug)]
/// There are 4 possible background palettes (0,1,2,3)
/// These each map to a set of 4 indices, which are lookups in the ppu.palettes (Palette Table)
struct BGPaletteIdx(usize);

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

    fn palette_from_bg_palette_idx(&self, bgp_idx: BGPaletteIdx) -> [u8; 4] {
        let palette_idx = bgp_idx.0;
        assert!(palette_idx < 4);
        let start = (palette_idx * 4 + 1) as usize;

        // The first entry is a special case for background
        [
            self.palettes[0],
            self.palettes[start],
            self.palettes[start + 1],
            self.palettes[start + 2],
        ]
    }

    // TODO: Improve my logic here. Potentially just use memory addrs
    //      instead of my (x,y) tile abstraction which is confusiong since it's x*8 for a bg tile
    //      or even x*8*2 when thinking of meta tiles..
    /// Returns a BGPaletteIdx, which can be used to look up a background palette
    fn palette_for_tile(&self, pos: (usize, usize)) -> BGPaletteIdx {
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
        let bank_size = 1024;
        let attr_table_size = 64;
        let bank_end = bank_size * (bank + 1);

        let attr_table = &self.vram[bank_end - attr_table_size..bank_end];
        let attr_table_idx = (y / 4) * 4 + (x / 4);
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

        BGPaletteIdx(palette_idx)
    }

    // tile_n can be thought of as the offset in the pattern table  (CHR ROM)
    // pos (*8) relates to the value in the name table.
    /// VRAM (also called "name tables")
    pub fn draw_background(&self, frame: &mut Frame) {
        // Determine which nametable is being used for the current screen (by reading bit 0 and bit 1 from Control register)
        let which_nametable = self
            .registers
            .control
            .intersection(ControlRegister::NAMETABLE)
            .bits() as usize;
        assert!(which_nametable <= 3);

        // Determine which CHR ROM bank is used for background tiles (by reading bit 4 from Control Register)
        let bank = (self
            .registers
            .control
            .intersection(ControlRegister::BACKGROUND_PATTERN_ADDR)
            .bits()
            > 0) as usize;
        assert!(bank <= 1);

        let rows = 30;
        let cols = 32;
        for y in 0..rows {
            for x in 0..cols {
                let tile_n = self.vram[y * cols + x] as usize;
                let bgp_idx = self.palette_for_tile((x, y));
                let palette = self.palette_from_bg_palette_idx(bgp_idx);
                frame.draw_tile(&self.chr_rom, bank, tile_n, (x, y), palette);
            }
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
            self.registers
                .status
                .remove(StatusRegister::SPRITE_0_HIT_FLAG);
            should_rerender = true
        }

        // the PPU renders 262 scan lines per frame
        if self.scanline < 262 && scanline >= 262 {
            // If the vblank flag is not cleared by reading, it will be cleared automatically on dot 1 of the prerender scanline.
            // Here we are approximating that by clearing on dot 0 or above.
            self.set_ppu_vblank_status(false);
            self.nmi_interrupt = None;
            self.registers
                .status
                .remove(StatusRegister::SPRITE_0_HIT_FLAG);
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
        let mirrored = match (self.mirroring, name_table_idx) {
            (Mirroring::Horizontal, 1) | (Mirroring::Horizontal, 2) => base - 0x0400,
            (Mirroring::Vertical, 2) | (Mirroring::Vertical, 3) | (Mirroring::Horizontal, 3) => {
                base - 0x0800
            }
            (Mirroring::Horizontal, _) | (Mirroring::Vertical, _) => base,
            (Mirroring::FourScreen, _) => {
                todo!("Four Screen requires specicial setup with more RAM. More info: https://www.nesdev.org/wiki/Mirroring#4-Screen")
            }
        };
        mirrored
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
        assert_eq!(
            ppu.registers.status.contains(StatusRegister::VBLANK_FLAG),
            false,
        );
        assert_eq!(ppu.is_vblank_nmi_enabled(), false);

        ppu.registers
            .control
            .insert(ControlRegister::VBLANK_NMI_ENABLE);
        assert_eq!(ppu.is_vblank_nmi_enabled(), true);

        ppu.tick(1 * 341);
        assert_eq!(ppu.get_tick_status(), (1, 341));
        assert_eq!(get_ppu_vblank_status(&mut ppu), false);

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

        assert_eq!(ppu.palette_for_tile((0, 0)), BGPaletteIdx(3));
        assert_eq!(ppu.palette_for_tile((1, 0)), BGPaletteIdx(3));
        assert_eq!(ppu.palette_for_tile((0, 1)), BGPaletteIdx(3));
        assert_eq!(ppu.palette_for_tile((1, 1)), BGPaletteIdx(3));

        assert_eq!(ppu.palette_for_tile((2, 0)), BGPaletteIdx(2));
        assert_eq!(ppu.palette_for_tile((3, 0)), BGPaletteIdx(2));
        assert_eq!(ppu.palette_for_tile((2, 1)), BGPaletteIdx(2));
        assert_eq!(ppu.palette_for_tile((3, 1)), BGPaletteIdx(2));

        assert_eq!(ppu.palette_for_tile((0, 2)), BGPaletteIdx(1));
        assert_eq!(ppu.palette_for_tile((1, 3)), BGPaletteIdx(1));
        assert_eq!(ppu.palette_for_tile((0, 2)), BGPaletteIdx(1));
        assert_eq!(ppu.palette_for_tile((1, 3)), BGPaletteIdx(1));

        assert_eq!(ppu.palette_for_tile((2, 2)), BGPaletteIdx(0));
        assert_eq!(ppu.palette_for_tile((3, 3)), BGPaletteIdx(0));
        assert_eq!(ppu.palette_for_tile((2, 2)), BGPaletteIdx(0));
        assert_eq!(ppu.palette_for_tile((3, 3)), BGPaletteIdx(0));

        ppu.vram[attr_table_start + 1] = 0b11111111;
        for x in 4..=7 {
            for y in 0..=3 {
                assert_eq!(ppu.palette_for_tile((x, y)), BGPaletteIdx(3));
            }
        }

        ppu.vram[attr_table_start + 2] = 0b01010101;
        for x in 8..=11 {
            for y in 0..=3 {
                assert_eq!(ppu.palette_for_tile((x, y)), BGPaletteIdx(1));
            }
        }

        ppu.vram[attr_table_start + 5] = 0b10101010;
        for x in 4..=7 {
            for y in 4..=7 {
                assert_eq!(ppu.palette_for_tile((x, y)), BGPaletteIdx(2));
            }
        }
    }

    #[test]
    fn test_palette_from_bg_palette_idx() {
        let mut ppu = new_test_ppu();

        ppu.palettes[0] = 255;
        ppu.palettes[1..=3].copy_from_slice(&[1, 2, 3]); // bg_pallete idx=0
        ppu.palettes[5..=7].copy_from_slice(&[5, 6, 7]); // bg_pallete idx=1
        ppu.palettes[9..=11].copy_from_slice(&[9, 10, 11]); // bg_pallete idx=2
        ppu.palettes[13..=15].copy_from_slice(&[13, 14, 15]); // bg_pallete idx=3

        assert_eq!(
            ppu.palette_from_bg_palette_idx(BGPaletteIdx(0)),
            [255, 1, 2, 3]
        );
        assert_eq!(
            ppu.palette_from_bg_palette_idx(BGPaletteIdx(1)),
            [255, 5, 6, 7]
        );
        assert_eq!(
            ppu.palette_from_bg_palette_idx(BGPaletteIdx(2)),
            [255, 9, 10, 11]
        );
        assert_eq!(
            ppu.palette_from_bg_palette_idx(BGPaletteIdx(3)),
            [255, 13, 14, 15]
        );
    }
}
