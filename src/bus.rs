use crate::core::Mem;

const RAM: u16 = 0x0000;
const RAM_MIRROR_END: u16 = 0x2000;

const PPU: u16 = 0x2000;
const PPU_MIRROR_END: u16 = 0x4000;

pub struct Bus {
    cpu_vram: [u8; 0x800], // 2048

    // TODO: This was needed to get my tests working again after Ch4
    // https://bugzmanov.github.io/nes_ebook/chapter_4.html
    // Did the author have another approach? I couldn't find it in repo.
    // fallback: [u8; 0xffff],
    program_start1: u8,
    program_start2: u8,
}

impl Bus {
    pub fn new() -> Self {
        Bus {
            cpu_vram: [0; 0x800],
            // fallback: [0; 0xffff],
            program_start1: 0,
            program_start2: 0,
        }
    }
}

// TODO: More mappings..

// That RAM is accessible via [0x0000 … 0x2000] address space.

// Access to [0x2000 … 0x4020] is redirected to other available NES hardware modules: PPU, APU, GamePads, etc. (more on this later)

// Access to [0x4020 .. 0x6000] is a special space that different generations of cartridges used differently. It might be mapped to RAM, ROM, or nothing at all. The space is controlled by so-called mappers - special circuitry on a cartridge. We will ignore this space.

// Access to [0x6000 .. 0x8000] is reserved to a RAM space on a cartridge if a cartridge has one. It was used in games like Zelda for storing and retrieving the game state. We will ignore this space as well.

// Access to [0x8000 … 0xFFFF] is mapped to Program ROM (PRG ROM) space on a cartridge.

impl Mem for Bus {
    fn mem_read(&self, addr: u16) -> u8 {
        if (RAM..RAM_MIRROR_END).contains(&addr) {
            let a = addr & 0b1110_0111_1111_1111;
            self.cpu_vram[a as usize]
        } else if (PPU..PPU_MIRROR_END).contains(&addr) {
            todo!("PPU NYI")
        } else if addr == 0xFFFC {
            self.program_start1
        } else if addr == 0xFFFD {
            self.program_start2
        } else {
            // self.fallback[addr as usize]
            0
        }
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        if (RAM..RAM_MIRROR_END).contains(&addr) {
            let a = addr & 0b1110_0111_1111_1111;
            self.cpu_vram[a as usize] = data
        } else if (PPU..PPU_MIRROR_END).contains(&addr) {
            todo!("PPU NYI")
        } else if addr == 0xFFFC {
            self.program_start1 = data
        } else if addr == 0xFFFD {
            self.program_start2 = data
        } else {
            // self.fallback[addr as usize] = data
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_mirroring() {
        let mut bus = Bus::new();
        bus.cpu_vram[0] = 123;

        assert_eq!(bus.mem_read(0), 123);
        assert_eq!(bus.mem_read(0x1 << 11), 123);
        assert_eq!(bus.mem_read(0x1 << 12), 123);
        assert_eq!(bus.mem_read(0x1 << (11 + 0x1) << 12), 123);
    }

    #[test]
    fn test_write_mirroring() {
        let mut bus = Bus::new();

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
