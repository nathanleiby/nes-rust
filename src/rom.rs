use crate::cpu::CPU_START;

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
        // let size_of_prg_ram_times_8kb = header[8];

        let header = parse_rom_header(bytes);
        let header_size: usize = 16;
        match header {
            Header::INES {
                mapper,
                mirroring,
                prg_rom_len,
                chr_rom_len,
                trainer_size,
            } => {
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
            _ => todo!(),
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

enum Header {
    /// https://www.nesdev.org/wiki/INES
    INES {
        mapper: Mapper,
        mirroring: Mirroring,
        prg_rom_len: usize,
        chr_rom_len: usize,
        trainer_size: usize,
    },
    /// https://www.nesdev.org/wiki/NES_2.0
    NES2 {
        mapper: Mapper,
        mirroring: Mirroring,
        prg_rom_len: usize,
        chr_rom_len: usize,
        trainer_size: usize,
    },
}

fn parse_rom_header(bytes: &[u8]) -> Header {
    if bytes.len() < 16 {
        panic!("invalid rom: less than 16 bytes long")
    }
    let header = &bytes[0..16];

    // check if NES^Z is set
    let nes_file_prefix: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];
    if !header[0..4].eq(&nes_file_prefix) {
        panic!("invalid rom: lacks the 'NES^Z' prefix")
    }

    let ines_ver = (header[7] >> 2) & 0b11;
    if [0, 1].contains(&ines_ver) {
        let prg_rom_banks = header[4];
        let chr_rom_banks = header[5];
        let control_byte_1 = header[6];
        let control_byte_2 = header[7];

        let has_trainer_bytes = control_byte_1 ^ (1 << 2) == 0;
        let is_four_screen = control_byte_1 & (1 << 3) > 0;
        let is_vertical_screen = control_byte_1 & 1 > 0;

        let lo = (control_byte_1 & 0b1111_0000) >> 4;
        let hi = control_byte_2 & 0b1111_0000;
        let rom_mapper_type = hi | lo;

        let mapper = match rom_mapper_type {
            0 => Mapper::Zero,
            _ => todo!(),
            // _ => Mapper::Zero, // TODO: Hack to get tiles viewing.. but should revert to TODO. Why is mapper saying `0x40` (64) for some games even though it should be 0?
        };

        let mirroring = match (is_four_screen, is_vertical_screen) {
            (true, _) => Mirroring::FourScreen,
            (false, true) => Mirroring::Vertical,
            (false, false) => Mirroring::Horizontal,
        };

        let prg_rom_len = (prg_rom_banks as usize) * PRG_ROM_PAGE_SIZE;
        let chr_rom_len = (chr_rom_banks as usize) * CHR_ROM_PAGE_SIZE;

        let trainer_size = if has_trainer_bytes { 512 } else { 0 };
        Header::INES {
            mapper,
            mirroring,
            prg_rom_len,
            chr_rom_len,
            trainer_size,
        }
    } else if ines_ver == 2 {
        let prg_rom_lsb = header[4];
        let chr_rom_lsb = header[5];
        let prg_rom_msb = header[9] & 0b1111;
        let chr_rom_msb = (header[9] & 0b1111_0000) >> 4;

        let chr_rom_len = compute_rom_banks(chr_rom_msb, chr_rom_lsb);
        let prg_rom_len = compute_rom_banks(prg_rom_msb, prg_rom_lsb);

        let has_trainer_bytes = header[6] ^ (1 << 2) == 0;
        let is_four_screen = header[6] & (1 << 3) > 0;
        let is_vertical_screen = header[6] & 1 > 0;

        let mapper_0_to_3 = (header[6] & 0b1111_0000) >> 4;
        let mapper_4_to_7 = header[7] & 0b1111_0000;
        let mapper_8_to_11 = header[8] & 0b1111;
        let submapper_num = (header[8] & 0b1111_0000) >> 4;
        let rom_mapper_type = (mapper_8_to_11 as u16) << 8 + (mapper_4_to_7 | mapper_0_to_3) as u16;

        let mapper = match rom_mapper_type {
            0 => Mapper::Zero,
            _ => todo!(),
        };

        // Submapper 0 represents the default iNES behavour,
        if submapper_num != 0 {
            todo!("submapper != 0... need to handle non-default iNES behavior")
        }

        let mirroring = match (is_four_screen, is_vertical_screen) {
            (true, _) => Mirroring::FourScreen,
            (false, true) => Mirroring::Vertical,
            (false, false) => Mirroring::Horizontal,
        };

        let trainer_size = if has_trainer_bytes { 512 } else { 0 };
        Header::INES {
            mapper,
            mirroring,
            prg_rom_len,
            chr_rom_len,
            trainer_size,
        }
    } else {
        unimplemented!("unknown INES version: {:?}", ines_ver)
    }
}

fn parse_nes_20_rom() {
    //     Offset Meaning
    // --------------
    // 4      PRG-ROM size LSB
    // 5      CHR-ROM size LSB

    // 6      Flags 6
    //        D~7654 3210
    //          ---------
    //          NNNN FTBM
    //          |||| |||+-- Hard-wired nametable layout
    //          |||| |||     0: Vertical arrangement ("mirrored horizontally") or mapper-controlled
    //          |||| |||     1: Horizontal arrangement ("mirrored vertically")
    //          |||| ||+--- "Battery" and other non-volatile memory
    //          |||| ||      0: Not present
    //          |||| ||      1: Present
    //          |||| |+--- 512-byte Trainer
    //          |||| |      0: Not present
    //          |||| |      1: Present between Header and PRG-ROM data
    //          |||| +---- Alternative nametables
    //          ||||        0: No
    //          ||||        1: Yes
    //          ++++------ Mapper Number D3..D0

    // 7      Flags 7
    //        D~7654 3210
    //          ---------
    //          NNNN 10TT
    //          |||| ||++- Console type
    //          |||| ||     0: Nintendo Entertainment System/Family Computer
    //          |||| ||     1: Nintendo Vs. System
    //          |||| ||     2: Nintendo Playchoice 10
    //          |||| ||     3: Extended Console Type
    //          |||| ++--- NES 2.0 identifier
    //          ++++------ Mapper Number D7..D4

    // 8      Mapper MSB/Submapper
    //        D~7654 3210
    //          ---------
    //          SSSS NNNN
    //          |||| ++++- Mapper number D11..D8
    //          ++++------ Submapper number

    // 9      PRG-ROM/CHR-ROM size MSB
    //        D~7654 3210
    //          ---------
    //          CCCC PPPP
    //          |||| ++++- PRG-ROM size MSB
    //          ++++------ CHR-ROM size MSB

    // 10     PRG-RAM/EEPROM size
    //        D~7654 3210
    //          ---------
    //          pppp PPPP
    //          |||| ++++- PRG-RAM (volatile) shift count
    //          ++++------ PRG-NVRAM/EEPROM (non-volatile) shift count
    //        If the shift count is zero, there is no PRG-(NV)RAM.
    //        If the shift count is non-zero, the actual size is
    //        "64 << shift count" bytes, i.e. 8192 bytes for a shift count of 7.

    // 11     CHR-RAM size
    //        D~7654 3210
    //          ---------
    //          cccc CCCC
    //          |||| ++++- CHR-RAM size (volatile) shift count
    //          ++++------ CHR-NVRAM size (non-volatile) shift count
    //        If the shift count is zero, there is no CHR-(NV)RAM.
    //        If the shift count is non-zero, the actual size is
    //        "64 << shift count" bytes, i.e. 8192 bytes for a shift count of 7.

    // 12     CPU/PPU Timing
    //        D~7654 3210
    //          ---------
    //          .... ..VV
    //                 ++- CPU/PPU timing mode
    //                      0: RP2C02 ("NTSC NES")
    //                      1: RP2C07 ("Licensed PAL NES")
    //                      2: Multiple-region
    //                      3: UA6538 ("Dendy")

    // 13     When Byte 7 AND 3 =1: Vs. System Type
    //        D~7654 3210
    //          ---------
    //          MMMM PPPP
    //          |||| ++++- Vs. PPU Type
    //          ++++------ Vs. Hardware Type

    //        When Byte 7 AND 3 =3: Extended Console Type
    //        D~7654 3210
    //          ---------
    //          .... CCCC
    //               ++++- Extended Console Type

    // 14     Miscellaneous ROMs
    //        D~7654 3210
    //          ---------
    //          .... ..RR
    //                 ++- Number of miscellaneous ROMs present

    // 15     Default Expansion Device
    //        D~7654 3210
    //          ---------
    //          ..DD DDDD
    //            ++-++++- Default Expansion Device
}

/// Computes the ROM length in bytes, for a NES2.0 ROM
/// https://www.nesdev.org/wiki/NES_2.0#PRG-ROM_Area
/// https://www.nesdev.org/wiki/NES_2.0#CHR-ROM_Area
fn compute_rom_banks(msb: u8, lsb: u8) -> usize {
    if msb == 0xf {
        // // Multiplier, actual value is MM*2+1 (1,3,5,7)
        // let multiplier = (lsb & 0b11) * 2 + 1;
        // // Exponent (2^E), 0-63
        // let exponent = lsb >> 2;
        // 2_usize.pow(exponent as u32) * (multiplier as usize)
        unimplemented!(
            "not sure if roms use this special case. I see it's ignored in some emulators"
        )
    } else {
        ((msb as usize) << 8) + lsb as usize
    }
}

#[cfg(test)]
mod test {
    use std::fs;

    use super::*;

    #[test]
    fn test_compute_rom_len() {
        assert_eq!(compute_rom_banks(0, 0), 0);
        assert_eq!(compute_rom_banks(0, 0xAB), 0xAB);
        assert_eq!(compute_rom_banks(1, 0), 0x100);
        assert_eq!(compute_rom_banks(0xE, 0xFF), 0xEFF);
    }

    #[test]
    #[should_panic(expected = "not implemented")]
    fn test_unsupported_nes2_rom_banks_case() {
        compute_rom_banks(0xF, 0);
    }

    #[test]
    fn test_snake_rom_from_file() {
        let contents = fs::read("roms/snake.nes").unwrap();
        let rom = Rom::new(&contents);
        assert_eq!(rom.mapper, Mapper::Zero);
        assert_eq!(rom.mirroring, Mirroring::Vertical);
        assert_eq!(rom.prg_rom.len(), 2 * PRG_ROM_PAGE_SIZE);
        assert_eq!(rom.chr_rom.len(), 0);
    }

    // #[test]
    // fn test_ice_climber_header_from_file() {
    //     // let contents = fs::read("roms/local/ice-climber.nes").unwrap();
    //     let contents = fs::read("ice-climber.header.nes").unwrap();
    //     let rom = Rom::new(&contents);
    //     assert_eq!(rom.mapper, Mapper::Zero);
    //     assert_eq!(rom.mirroring, Mirroring::Horizontal);
    //     assert_eq!(rom.prg_rom.len(), PRG_ROM_PAGE_SIZE);
    //     assert_eq!(rom.chr_rom.len(), CHR_ROM_PAGE_SIZE);
    // }
}
