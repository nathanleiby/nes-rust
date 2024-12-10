use crate::{core::Mem, ppu::Ppu, rom::Rom};

const RAM: u16 = 0x0000;
const RAM_MIRROR_END: u16 = 0x2000;

const PPU: u16 = 0x2000;
const PPU_MIRROR_END: u16 = 0x4000;

pub const PRG_ROM_START: u16 = 0x8000;
const PRG_ROM_END: u16 = 0xFFFF;

pub struct Bus {
    cpu_vram: [u8; 0x800], // 2048
    rom: Rom,
    ppu: Ppu,
}

impl Bus {
    pub fn new(rom: Rom) -> Self {
        let ppu = Ppu::new(rom.chr_rom.clone(), rom.mirroring);

        Bus {
            cpu_vram: [0; 0x800],
            rom,
            ppu,
        }
    }

    fn read_prg_rom(&self, addr: u16) -> u8 {
        let mut idx = addr - PRG_ROM_START;

        // If the prg rom is 16KiB (not 32KiB), then we should mirror it
        if self.rom.prg_rom.len() == 0x4000 {
            idx %= 0x4000;
        }

        self.rom.prg_rom[idx as usize]
    }
}

impl Mem for Bus {
    fn mem_read(&mut self, addr: u16) -> u8 {
        if (RAM..RAM_MIRROR_END).contains(&addr) {
            let a = addr & 0b1110_0111_1111_1111;
            self.cpu_vram[a as usize]
        } else if (PPU..PPU_MIRROR_END).contains(&addr) {
            // The PPU exposes 8 registers. They are mirrored every 8 bytes in this range.
            let register_idx = addr % 8;
            match register_idx {
                0 | 1 | 3 | 5 | 6 => panic!(
                    "attempt to read from write-only PPU register: 0x200{}",
                    register_idx
                ),
                2 => self.ppu.read_from_status(),
                4 => self.ppu.read_from_oam_data(),
                7 => self.ppu.read_from_data(),
                8..=u16::MAX => panic!("invalid PPU register IDX: {}", register_idx),
            }
        } else if addr == 0x4016 {
            // 2.9	OAMDMA - Sprite DMA ($4014 write)
            panic!("attempt to read from write-only PPU register: 0x4016 (OAMDMA - Sprite DMA)");
        } else if (PRG_ROM_START..=PRG_ROM_END).contains(&addr) {
            self.read_prg_rom(addr)
        } else {
            0
        }
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        if (RAM..RAM_MIRROR_END).contains(&addr) {
            let a = addr & 0b1110_0111_1111_1111;
            self.cpu_vram[a as usize] = data
        } else if (PPU..PPU_MIRROR_END).contains(&addr) {
            // The PPU exposes 8 registers. They are mirrored every 8 bytes in this range.
            let register_idx = addr % 8;
            match register_idx {
                0 => self.ppu.write_to_ctrl(data),
                1 => self.ppu.write_to_mask(data),
                2 => panic!("attempt to write to read-only PPU register: 0x2002 (Status)",),
                3 => self.ppu.write_to_oam_address(data),
                4 => self.ppu.write_to_oam_data(data),
                5 => self.ppu.write_to_scroll_register(data),
                6 => self.ppu.write_to_addr(data),
                7 => self.ppu.write_to_data(data),
                8..=u16::MAX => panic!("invalid PPU register IDX: {}", register_idx),
            }
        } else if addr == 0x4016 {
            // 2.9	OAMDMA - Sprite DMA ($4014 write)
            panic!("attempt to read from write-only PPU register: 0x4016 (OAMDMA - Sprite DMA)");
        } else if (PRG_ROM_START..=PRG_ROM_END).contains(&addr) {
            panic!("attempt to write to ROM cartridge")
        } else {
            panic!("attempt to write to NYI section of memory")
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_mirroring() {
        let rom = Rom::new_test_rom(vec![]);
        let mut bus = Bus::new(rom);
        bus.cpu_vram[0] = 123;

        assert_eq!(bus.mem_read(0), 123);
        assert_eq!(bus.mem_read(0x1 << 11), 123);
        assert_eq!(bus.mem_read(0x1 << 12), 123);
        assert_eq!(bus.mem_read(0x1 << (11 + 0x1) << 12), 123);
    }

    #[test]
    fn test_write_mirroring() {
        let rom = Rom::new_test_rom(vec![]);
        let mut bus = Bus::new(rom);

        bus.mem_write(0, 1);
        assert_eq!(bus.cpu_vram[0], 1);
        bus.mem_write(0x1 << 11, 2);
        assert_eq!(bus.cpu_vram[0], 2);
        bus.mem_write(0x1 << 12, 3);
        assert_eq!(bus.cpu_vram[0], 3);
        bus.mem_write(0x1 << (11 + 0x1) << 12, 4);
        assert_eq!(bus.cpu_vram[0], 4);
    }
}
