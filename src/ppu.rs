use bitflags::bitflags;

use crate::{
    addr_register::AddrRegister,
    pallete::SYSTEM_PALLETE,
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
    pub fn draw_tile(&mut self, chr_rom: &[u8], bank: usize, tile_n: usize, pos: (usize, usize)) {
        assert!(bank <= 1);

        let tile_size_bytes = 16;
        let bank_size_bytes: usize = 4096;

        let (x, y) = pos;
        for row in 0..8 {
            let first_byte = chr_rom[bank * bank_size_bytes + tile_n * tile_size_bytes + row];
            let second_byte = chr_rom[bank * bank_size_bytes + tile_n * tile_size_bytes + 8 + row];

            for col in 0..8 {
                let which_bit = 1 << (7 - col);
                let lo_bit = first_byte & which_bit > 0;
                let hi_bit = second_byte & which_bit > 0;
                let palette_idx: u8 = (hi_bit as u8) << 1 + (lo_bit as u8);
                // TODO: lookup palette color
                // let color = SYSTEM_PALLETE[(palette_idx * 3) as usize];
                let color = match palette_idx {
                    0 => SYSTEM_PALLETE[0x01],
                    1 => SYSTEM_PALLETE[0x23],
                    2 => SYSTEM_PALLETE[0x27],
                    3 => SYSTEM_PALLETE[0x30],
                    _ => panic!("can't be"),
                };

                self.set_pixel(x + col, y + row, color);
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
    palettes: [u8; 32],
    oam_data: [u8; 256],

    mirroring: Mirroring,

    registers: PpuRegisters,

    #[allow(dead_code)]
    nmi_interrupt: bool,
    read_data_buffer: u8,

    /// scanline within the Frame. There are 262 total (0..=261)
    scanline: usize,
    /// clock_cycles within the Frame (0..=261*341)
    clock_cycles: usize,
}

impl Ppu {
    pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        Ppu {
            chr_rom,
            mirroring,
            vram: [0; 2048],
            palettes: [0; 32],
            oam_data: [0; 256],
            nmi_interrupt: false,

            registers: Default::default(),
            read_data_buffer: 0,

            scanline: 0,
            clock_cycles: 0,
        }
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
        let bank = self
            .registers
            .control
            .intersection(ControlRegister::BACKGROUND_PATTERN_ADDR)
            .bits() as usize;
        assert!(bank <= 1);

        let tile_size = 8;
        for y in 0..30 {
            for x in 0..32 {
                // // Get the relevant tile_n
                // let tile_idx = 0x2000 + (0x0020 * y) + x;
                // assert!(tile_idx >= 0x2000 && tile_idx < 0x23C0);
                // // let tile_n = self.chr_rom[0x2000 + (0x0020 * y) + x] as usize;
                // let tile_n = self.chr_rom[0x2000 + (0x0020 * y) + x] as usize;
                // assert!(tile_n < 512);

                let tile_n = self.vram[y * 30 + x] as usize;

                frame.draw_tile(&self.chr_rom, bank, tile_n, (x * tile_size, y * tile_size));
            }
        }
    }

    // TODO: Write some unit tests around tick() .. seems like it's not triggering
    pub fn tick(&mut self, cycles: usize) -> bool {
        let mut should_rerender = false;

        self.clock_cycles += cycles;
        // each scanline lasts for 341 PPU clock cycles
        let scanline = self.clock_cycles / 341;

        if self.scanline < 241 && scanline >= 241 && self.is_vblank_nmi_enabled() {
            // upon entering scanline 241, PPU triggers NMI interrupt
            self.nmi_interrupt = true;
            // The VBlank flag of the PPU is set at tick 1 (the second tick) of scanline 241
            // Here we are approximating that by clearing on dot 0 or above.
            self.set_ppu_vblank_status(true);
            should_rerender = true
        }

        // the PPU renders 262 scan lines per frame
        if self.scanline < 262 && scanline >= 262 {
            // If the vblank flag is not cleared by reading, it will be cleared automatically on dot 1 of the prerender scanline.
            // Here we are approximating that by clearing on dot 0 or above.
            self.set_ppu_vblank_status(false);
            self.nmi_interrupt = false;
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

    pub fn is_nmi_interrupt_triggered(&self) -> bool {
        self.nmi_interrupt
    }

    pub fn write_to_ctrl(&mut self, data: u8) {
        let before = &self.registers.control;
        let after = ControlRegister::from_bits_truncate(data);

        // should we toggle an NMI interrupt?
        let is_vblank_state = self.registers.status.contains(StatusRegister::VBLANK_FLAG);
        let enabled_vblank_nmi = after.contains(ControlRegister::VBLANK_NMI_ENABLE)
            && !before.contains(ControlRegister::VBLANK_NMI_ENABLE);
        if is_vblank_state && enabled_vblank_nmi {
            self.nmi_interrupt = true;
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
                // account for offset (0x2000) and remapping (0x3NNN -> 0x2NNN)
                let base = addr & 0x0FFF;
                let name_table_idx = base / 0x0400;

                // and ROM-configured mirroring
                let mirrored = match (self.mirroring, name_table_idx) {
                    (Mirroring::Horizontal, 1) | (Mirroring::Horizontal, 2) => base - 0x0400,
                    (Mirroring::Vertical, 2)
                    | (Mirroring::Vertical, 3)
                    | (Mirroring::Horizontal, 3) => base - 0x0800,
                    (Mirroring::Horizontal, _) | (Mirroring::Vertical, _) => base,
                    (Mirroring::FourScreen, _) => {
                        todo!("Four Screen requires specicial setup with more RAM. More info: https://www.nesdev.org/wiki/Mirroring#4-Screen")
                    }
                };
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

    pub fn write_to_data(&mut self, data: u8) {
        let addr = self.registers.address.get();
        self.increment_vram_addr();

        match addr {
            0..0x2000 => panic!("attempt to write to CHR ROM (read-only)"),
            0x2000..0x3F00 => {
                // account for offset (0x2000) and remapping (0x3NNN -> 0x2NNN)
                let base = addr & 0x0FFF;
                let name_table_idx = base / 0x0400;

                // and ROM-configured mirroring
                let mirrored = match (self.mirroring, name_table_idx) {
                    (Mirroring::Horizontal, 1) => base - 0x0400,
                    (Mirroring::Horizontal, 3) => base - 0x0C00,
                    (Mirroring::Vertical, 2) => base - 0x0800,
                    (Mirroring::Vertical, 3) => base - 0x0C00,
                    (Mirroring::FourScreen, _) => {
                        todo!("NYI. More info: https://www.nesdev.org/wiki/Mirroring#4-Screen")
                    }
                    _ => base,
                };
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
        // Reading this register has the side effect of clearing the PPU's internal w register.
        // It is commonly read before writes to PPUSCROLL and PPUADDR to ensure the writes occur in the correct order.
        self.reset_latch();

        let result = self.registers.status.bits();

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
    use crate::rom::Rom;

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
        assert_eq!(ppu.nmi_interrupt, false);
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
        assert!(ppu.is_nmi_interrupt_triggered());
        assert!(get_ppu_vblank_status(&mut ppu));

        let should_rerender = ppu.tick(21 * 341);
        assert_eq!(
            ppu.get_tick_status(),
            (0, 0),
            "should wrap back scanline to 0 after 262 scanlines"
        );
        assert!(!should_rerender);
        assert!(!ppu.is_nmi_interrupt_triggered());
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
}
