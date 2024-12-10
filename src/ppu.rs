use bitflags::bitflags;

use crate::{
    rom::Mirroring,
    utility::{addr_from, split_addr},
};

#[derive(Default)]
pub struct PpuRegisters {
    /// Controller (0x2000) - instructs PPU on general logic flow
    control: ControlRegister,

    // Mask (0x2001) - instructs PPU how to render sprites and background
    // mask: u8,

    // Status (0x2002) - reports PPU status
    // status: u8,

    // Object Attribute Memory - the space responsible for sprites
    // oam_address: u8,
    // oam_data: u8,

    // Scroll (0x2005) - instructs PPU how to set a viewport
    // scroll: u8,
    /// Address (0x2006) - provides access to memory map available for PPU
    /// Data (0x2007) - provides access to the memory map available for PPU
    address: AddrRegister,
    // oam_dma: u8,
}

pub struct Ppu {
    /// CHR ROM (also called "pattern tables")
    chr_rom: Vec<u8>,
    /// VRAM (also called "name tables")
    vram: [u8; 2048],
    palettes: [u8; 32],
    #[allow(dead_code)]
    oam_data: [u8; 256],

    mirroring: Mirroring,

    registers: PpuRegisters,

    #[allow(dead_code)]
    nmi_interrupt: bool,
    read_data_buffer: u8,
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
        }
    }

    #[allow(dead_code)]
    pub fn is_nmi_interrupt_triggered(&self) -> bool {
        self.nmi_interrupt
    }

    pub fn write_to_addr(&mut self, data: u8) {
        self.registers.address.external_write(data);
    }

    pub fn write_to_ctrl(&mut self, data: u8) {
        // TODO: bugzmanov book sets ctrl.bits directly vs recreating .. I hit an error attempting that
        self.registers.control = ControlRegister::from_bits_truncate(data);
    }

    fn increment_vram_addr(&mut self) {
        self.registers
            .address
            .increment(self.registers.control.vram_increment_amount());
    }

    pub fn read_data(&mut self) -> u8 {
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
                    (Mirroring::Horizontal, 1) => base - 0x0400,
                    (Mirroring::Horizontal, 3) => base - 0x0C00,
                    (Mirroring::Vertical, 2) => base - 0x0800,
                    (Mirroring::Vertical, 3) => base - 0x0C00,
                    (Mirroring::FourScreen, _) => {
                        todo!("NYI. More info: https://www.nesdev.org/wiki/Mirroring#4-Screen")
                    }
                    _ => base,
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
}

bitflags! {
    // 7  bit  0
    // ---- ----
    // VPHB SINN
    // |||| ||||
    // |||| ||++- Base nametable address
    // |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
    // |||| |+--- VRAM address increment per CPU read/write of PPUDATA
    // |||| |     (0: add 1, going across; 1: add 32, going down)
    // |||| +---- Sprite pattern table address for 8x8 sprites
    // ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
    // |||+------ Background pattern table address (0: $0000; 1: $1000)
    // ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels – see PPU OAM#Byte 1)
    // |+-------- PPU master/slave select
    // |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
    // +--------- Vblank NMI enable (0: off, 1: on)
    #[derive(Default)]
    pub struct ControlRegister: u8 {
        const NAMETABLE_1 = 1 << 0;
        const NAMETABLE_2 = 1 << 1;
        const VRAM_INCREMENT = 1 << 2;
        const SPRITE_PATTERN_ADDR = 1 << 3;
        const BACKGROUND_PATTERN_ADDR = 1 << 4;
        const SPRITE_SIZE = 1 << 5;
        const MASTER_SLAVE_SELECT = 1 << 6;
        const VBLANK_NMI_ENABLE = 1 << 7;
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
struct AddrRegister {
    hi: u8,
    lo: u8,

    // This represents the `w` register, where false means w=0
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
