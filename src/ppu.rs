use bitflags::bitflags;

use crate::{
    rom::Mirroring,
    utility::{addr_from, split_addr},
};

pub static SYSTEM_PALLETE: [(u8, u8, u8); 64] = [
    (0x80, 0x80, 0x80),
    (0x00, 0x3D, 0xA6),
    (0x00, 0x12, 0xB0),
    (0x44, 0x00, 0x96),
    (0xA1, 0x00, 0x5E),
    (0xC7, 0x00, 0x28),
    (0xBA, 0x06, 0x00),
    (0x8C, 0x17, 0x00),
    (0x5C, 0x2F, 0x00),
    (0x10, 0x45, 0x00),
    (0x05, 0x4A, 0x00),
    (0x00, 0x47, 0x2E),
    (0x00, 0x41, 0x66),
    (0x00, 0x00, 0x00),
    (0x05, 0x05, 0x05),
    (0x05, 0x05, 0x05),
    (0xC7, 0xC7, 0xC7),
    (0x00, 0x77, 0xFF),
    (0x21, 0x55, 0xFF),
    (0x82, 0x37, 0xFA),
    (0xEB, 0x2F, 0xB5),
    (0xFF, 0x29, 0x50),
    (0xFF, 0x22, 0x00),
    (0xD6, 0x32, 0x00),
    (0xC4, 0x62, 0x00),
    (0x35, 0x80, 0x00),
    (0x05, 0x8F, 0x00),
    (0x00, 0x8A, 0x55),
    (0x00, 0x99, 0xCC),
    (0x21, 0x21, 0x21),
    (0x09, 0x09, 0x09),
    (0x09, 0x09, 0x09),
    (0xFF, 0xFF, 0xFF),
    (0x0F, 0xD7, 0xFF),
    (0x69, 0xA2, 0xFF),
    (0xD4, 0x80, 0xFF),
    (0xFF, 0x45, 0xF3),
    (0xFF, 0x61, 0x8B),
    (0xFF, 0x88, 0x33),
    (0xFF, 0x9C, 0x12),
    (0xFA, 0xBC, 0x20),
    (0x9F, 0xE3, 0x0E),
    (0x2B, 0xF0, 0x35),
    (0x0C, 0xF0, 0xA4),
    (0x05, 0xFB, 0xFF),
    (0x5E, 0x5E, 0x5E),
    (0x0D, 0x0D, 0x0D),
    (0x0D, 0x0D, 0x0D),
    (0xFF, 0xFF, 0xFF),
    (0xA6, 0xFC, 0xFF),
    (0xB3, 0xEC, 0xFF),
    (0xDA, 0xAB, 0xEB),
    (0xFF, 0xA8, 0xF9),
    (0xFF, 0xAB, 0xB3),
    (0xFF, 0xD2, 0xB0),
    (0xFF, 0xEF, 0xA6),
    (0xFF, 0xF7, 0x9C),
    (0xD7, 0xE8, 0x95),
    (0xA6, 0xED, 0xAF),
    (0xA2, 0xF2, 0xDA),
    (0x99, 0xFF, 0xFC),
    (0xDD, 0xDD, 0xDD),
    (0x11, 0x11, 0x11),
    (0x11, 0x11, 0x11),
];

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
                let color = SYSTEM_PALLETE[palette_idx as usize];

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
    vram: [u8; 2048],
    palettes: [u8; 32],
    oam_data: [u8; 256],

    mirroring: Mirroring,

    registers: PpuRegisters,

    #[allow(dead_code)]
    nmi_interrupt: bool,
    read_data_buffer: u8,

    scanline: usize,
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

    pub fn tick(&mut self, cycles: usize) {
        self.clock_cycles += cycles;
        // each scanline lasts for 341 PPU clock cycles
        let scanline = self.clock_cycles / 341;
        if self.scanline < 241
            && scanline >= 241
            && self
                .registers
                .control
                .contains(ControlRegister::VBLANK_NMI_ENABLE)
        {
            // upon entering scanline 241, PPU triggers NMI interrupt
            self.nmi_interrupt = true;
            // The VBlank flag of the PPU is set at tick 1 (the second tick) of scanline 241
            // Here we are approximating that. There's a rare edge cases
            self.registers.status &= StatusRegister::VBLANK_FLAG
        }

        // the PPU renders 262 scan lines per frame
        if self.scanline < 262 && scanline >= 262 {
            // TODO: any behavior at end of frame?
            self.nmi_interrupt = false;
        }

        self.clock_cycles %= 262 * 341;
        self.scanline = scanline % 262;
    }

    /// returns (scanline, clock_cycles)
    pub fn get_tick_status(&self) -> (usize, usize) {
        (self.scanline, self.clock_cycles)
    }

    #[allow(dead_code)]
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
        self.registers.address.is_lo_byte = false;
        self.registers.scroll.is_y_scroll = false;

        self.registers.status.bits()
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
    #[derive(Default)]
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

#[derive(Default)]
struct AddrRegister {
    hi: u8,
    lo: u8,

    // TODO: Actually controlled by w_register, which is shared with PpuScrollRegister
    is_lo_byte: bool,
}

const ADDR_REGISTER_MIRROR_MASK: u16 = 0b0011_1111_1111_1111; // 0x3FFF

impl AddrRegister {
    fn get(&self) -> u16 {
        addr_from(self.lo, self.hi)
    }

    fn set(&mut self, addr: u16) {
        (self.lo, self.hi) = split_addr(addr);
    }

    pub fn external_write(&mut self, data: u8) {
        if self.is_lo_byte {
            self.lo = data;
        } else {
            self.hi = data;
        }

        // mirror down addr above 0x3fff
        self.set(self.get() & ADDR_REGISTER_MIRROR_MASK);

        self.is_lo_byte = !self.is_lo_byte;
    }

    /// increment is called when there's a read/write to the corresponding `data` register
    // TODO: Should `inc` explicitly only support the valid args of 1 or 32?
    fn increment(&mut self, inc: u8) {
        let base = self.get();
        let updated = base.wrapping_add(inc as u16);

        self.set(updated & ADDR_REGISTER_MIRROR_MASK);
    }

    #[allow(dead_code)]
    pub fn reset_to_hi_bit(&mut self) {
        self.is_lo_byte = false;
    }
}

#[cfg(test)]
mod tests {

    use crate::assert_eq_hex;

    use super::*;

    #[test]
    fn test_addr_register_external_writes_and_increment() {
        let mut ar = AddrRegister::default();
        assert_eq_hex!(ar.get(), 0);

        // hi bit
        ar.external_write(0x12);
        assert_eq_hex!(ar.get(), 0x1200);

        // lo bit
        ar.external_write(0x34);
        assert_eq_hex!(ar.get(), 0x1234);

        ar.set(0x0123);
        assert_eq_hex!(ar.get(), 0x0123);

        ar.increment(1);
        assert_eq_hex!(ar.get(), 0x0124);

        ar.increment(0x20);
        assert_eq_hex!(ar.get(), 0x0144);

        ar.set(0x3FFF);
        assert_eq_hex!(ar.get(), 0x3FFF);

        ar.increment(1);
        assert_eq_hex!(ar.get(), 0, "increment should wrap around due to mirroring");
    }

    #[test]
    fn test_addr_register_special_cases() {
        let mut ar = AddrRegister::default();
        assert_eq_hex!(ar.get(), 0);

        ar.external_write(0x50);
        assert_eq_hex!(ar.get(), 0x1000, "hi bit should wrap due to mirroring");

        ar.external_write(0xFF);
        assert_eq_hex!(ar.get(), 0x10FF, "should modify lo bit");

        ar.external_write(0xBC);
        assert_eq_hex!(
            ar.get(),
            0x3CFF,
            "should be hi bit, wrapped due to mirroring"
        );

        ar.reset_to_hi_bit();
        ar.external_write(0xAB);
        assert_eq_hex!(ar.get(), 0x2BFF, "should be hi bit again, after reset");
    }
}
