use crate::core::CPU_START;

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Mirroring {
    FourScreen,
    Vertical,
    Horizontal,
}

#[derive(PartialEq, Eq, Debug)]
enum Mapper {
    Zero,
}

pub struct Rom {
    pub prg_rom: Vec<u8>,

    /// CHR ROM (also called "pattern tables"), which contains the visual graphics data of a game.
    pub chr_rom: Vec<u8>,

    #[allow(dead_code)]
    mapper: Mapper,

    pub mirroring: Mirroring,
}

const PRG_ROM_PAGE_SIZE: usize = 16384;
const CHR_ROM_PAGE_SIZE: usize = 8192;

impl Rom {
    pub fn new(bytes: &[u8]) -> Self {
        if bytes.len() < 16 {
            panic!("invalid rom: less than 16 bytes long")
        }
        let header = &bytes[0..16];

        // check if NES^Z is set
        let nes_file_prefix: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];
        if !header[0..4].eq(&nes_file_prefix) {
            panic!("invalid rom: lacks the 'NES^Z' prefix")
        }

        let prg_rom_banks = header[4];
        let chr_rom_banks = header[5];
        let control_byte_1 = header[6];
        let control_byte_2 = header[7];
        // let size_of_prg_ram_times_8kb = header[8];

        let has_trainer_bytes = control_byte_1 ^ (1 << 2) == 0;
        let is_four_screen = control_byte_1 & (1 << 3) > 0;
        let is_vertical_screen = control_byte_1 & 1 > 0;

        let lo = (control_byte_1 & 0b1111_0000) >> 4;
        let hi = control_byte_2 & 0b1111_0000;
        let rom_mapper_type = hi + lo;

        let mapper = match rom_mapper_type {
            0 => Mapper::Zero,
            _ => todo!(),
            // _ => Mapper::Zero, // TODO: Hack to get tiles viewing.. but should revert to TODO
        };

        let mirroring = match (is_four_screen, is_vertical_screen) {
            (true, _) => Mirroring::FourScreen,
            (false, true) => Mirroring::Vertical,
            (false, false) => Mirroring::Horizontal,
        };

        let prg_rom_len = (prg_rom_banks as usize) * PRG_ROM_PAGE_SIZE;
        let chr_rom_len = (chr_rom_banks as usize) * CHR_ROM_PAGE_SIZE;

        let header_size: usize = 16;
        let trainer_size = if has_trainer_bytes { 512 } else { 0 };
        let prg_rom_start = header_size + trainer_size;
        let prg_rom: Vec<u8> = bytes[prg_rom_start..prg_rom_start + prg_rom_len].to_vec();

        let chr_rom_start = prg_rom_start + prg_rom_len;
        let chr_rom = bytes[chr_rom_start..chr_rom_start + chr_rom_len].to_vec();

        Rom {
            prg_rom,
            chr_rom,
            mapper,
            mirroring,
        }
    }

    pub fn new_test_rom(vec: Vec<u8>) -> Rom {
        let mut prg_rom: Vec<u8> = [0; 0x8000].to_vec();
        let program_start = CPU_START;
        for (idx, b) in vec.iter().enumerate() {
            prg_rom[program_start + idx] = *b;
        }

        prg_rom[0xFFFC - 0x8000] = 0x00;
        prg_rom[0xFFFD - 0x8000] = 0x86;

        Self {
            prg_rom,
            chr_rom: vec![],
            mapper: Mapper::Zero,
            mirroring: Mirroring::Vertical,
        }
    }
}

#[cfg(test)]
mod test {
    use std::fs;

    use super::*;

    #[test]
    fn test_snake_rom_from_file() {
        let contents = fs::read("roms/snake.nes").unwrap();
        let rom = Rom::new(&contents);
        assert_eq!(rom.mapper, Mapper::Zero);
        assert_eq!(rom.mirroring, Mirroring::Vertical);
        assert_eq!(rom.prg_rom.len(), 2 * PRG_ROM_PAGE_SIZE);
        assert_eq!(rom.chr_rom.len(), 0);
    }
}
