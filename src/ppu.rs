use std::ops::Range;

use crate::{
    core::Mem,
    rom::Mirroring,
    utility::{addr_from, split_addr},
};

#[derive(Default)]
struct PpuRegisters {
    /// Controller (0x2000) - instructs PPU on general logic flow
    controller: u8,

    /// Mask (0x2001) - instructs PPU how to render sprites and background
    mask: u8,

    // Status (0x2002) - reports PPU status
    status: u8,

    /// Object Attribute Memory - the space responsible for sprites
    oam_address: u8,
    oam_data: u8,

    /// Scroll (0x2005) - instructs PPU how to set a viewport
    scroll: u8,

    /// Address (0x2006) - provides access to memory map available for PPU
    address: AddrRegister,
    /// Data (0x2007) - provides access to the memory map available for PPU
    data: u8,

    oam_dma: u8,
}

pub struct Ppu {
    /// CHR ROM (also called "pattern tables")
    chr_rom: Vec<u8>,
    /// VRAM (also called "name tables")
    vram: [u8; 2048],
    palettes: [u8; 32],
    oam_data: [u8; 256],

    mirroring: Mirroring,

    pub registers: PpuRegisters,

    nmi_interrupt: bool,
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
        }
    }

    pub fn is_nmi_interrupt_triggered(&self) -> bool {
        return self.nmi_interrupt;
    }
}

#[derive(Default)]
struct AddrRegister {
    hi: u8,
    lo: u8,

    // This represents the `w` register, where false means w=0
    is_lo_byte: bool,
}

const ADDR_REGISTER_MIRROR_MASK: u16 = 0b0011_1111_1111_1111; // 0x3FFF

impl AddrRegister {
    pub fn new() -> Self {
        AddrRegister {
            hi: 0,
            lo: 0,
            // write the hi byte first
            is_lo_byte: false,
        }
    }

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

    pub fn reset_to_hi_bit(&mut self) {
        self.is_lo_byte = false;
    }
}

impl Mem for Ppu {
    fn mem_read(&self, addr: u16) -> u8 {
        let chr_rom_range: Range<u16> = 0..0x2000;
        let vram_range: Range<u16> = 0x2000..0x3F00;

        let palettes: Range<u16> = 0x3F00..0x4000;
        let mirrors = 0x4000..=0xFFFF;

        // match addr {
        //     chr_rom_range => todo!("read from pattern tables"),
        //     vram_range => {
        //         todo!(""),
        //     }
        //     // TODO: mirroring?
        //     palettes => palettes[addr - 0x3F00],
        //     mirrors => todo!(""),
        // }

        todo!()
    }

    fn mem_write(&mut self, addr: u16, val: u8) {
        todo!()
    }
}

mod tests {
    use crate::assert_eq_hex;

    use super::*;

    #[test]
    fn test_addr_register_external_writes_and_increment() {
        let mut ar = AddrRegister::new();
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
        let mut ar = AddrRegister::new();
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
