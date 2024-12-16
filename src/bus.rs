use crate::{apu::Apu, core::Mem, gamepad::GamepadRegister, ppu::Ppu, rom::Rom};

const RAM: u16 = 0x0000;
const RAM_MIRROR_END: u16 = 0x2000;

const PPU: u16 = 0x2000;
const PPU_MIRROR_END: u16 = 0x4000;

pub const PRG_ROM_START: u16 = 0x8000;
const PRG_ROM_END: u16 = 0xFFFF;

pub struct Bus<'call> {
    cpu_vram: [u8; 0x800], // 2048
    rom: Rom,
    ppu: Ppu,
    apu: Apu,
    gamepad1: GamepadRegister,
    gamepad2: GamepadRegister,
    cycles: usize,
    #[allow(clippy::type_complexity)]
    gameloop_callback: Box<dyn FnMut(&Ppu, &mut GamepadRegister, &mut GamepadRegister) + 'call>,
}

impl<'a> Bus<'a> {
    pub fn new(rom: Rom) -> Self {
        Bus::new_with_cb(rom, |_, _, _| {})
    }

    pub fn new_with_cb<'call, F>(rom: Rom, gameloop_callback: F) -> Self
    where
        F: FnMut(&Ppu, &mut GamepadRegister, &mut GamepadRegister) + 'call + 'a,
    {
        let ppu = Ppu::new(rom.chr_rom.clone(), rom.mirroring);
        let apu = Apu::new();

        Bus {
            cpu_vram: [0; 0x800],
            rom,
            ppu,
            apu,
            cycles: 0,
            gamepad1: GamepadRegister::new(),
            gamepad2: GamepadRegister::new(),
            gameloop_callback: Box::from(gameloop_callback),
        }
    }

    pub fn tick(&mut self, cycles: usize) {
        self.cycles += cycles;

        let should_rerender = self.ppu.tick(cycles * 3);
        if should_rerender {
            (self.gameloop_callback)(&self.ppu, &mut self.gamepad1, &mut self.gamepad2)
        }

        self.apu.tick_cpu_cycles(cycles);
    }

    pub fn poll_nmi_interrupt(&mut self) -> bool {
        self.ppu.poll_nmi_interrupt().is_some()
    }

    fn read_prg_rom(&self, addr: u16) -> u8 {
        let mut idx = addr - PRG_ROM_START;

        // If the prg rom is 16KiB (not 32KiB), then we should mirror it
        if self.rom.prg_rom.len() == 0x4000 {
            idx %= 0x4000;
        }

        self.rom.prg_rom[idx as usize]
    }

    /// returns (scanline, clock_cycles)
    pub(crate) fn get_ppu_tick_status(&self) -> (usize, usize) {
        self.ppu.get_tick_status()
    }
}

impl Mem for Bus<'_> {
    fn mem_read(&mut self, addr: u16) -> u8 {
        match addr {
            RAM..RAM_MIRROR_END => {
                let a = addr & 0b1110_0111_1111_1111;
                self.cpu_vram[a as usize]
            }
            PPU..PPU_MIRROR_END => {
                // The PPU exposes 8 registers. They are mirrored every 8 bytes in this range.
                let register_idx = addr % 8;
                match register_idx {
                    0 | 1 | 3 | 5 | 6 => panic!(
                        "attempt to read from write-only PPU register: 0x200{}",
                        register_idx
                    ),
                    // 0 | 1 | 3 | 5 | 6 => 0, // TODO
                    2 => self.ppu.read_from_status(),
                    4 => self.ppu.read_from_oam_data(),
                    7 => self.ppu.read_from_data(),
                    8..=u16::MAX => panic!("invalid PPU register IDX: {}", register_idx),
                }
            }
            0x4014 => {
                panic!("attempt to read from write-only PPU register: 0x4014 (OAMDMA - Sprite DMA)")
            }
            0x4000..=0x4013 | 0x4015 => self.apu.mem_read(addr),
            0x4016 => self.gamepad1.read(),
            0x4017 => self.gamepad2.read(),
            0x4018..0x4020 => 0, // APU and I/O functionality that is normally disabled
            0x4020..0x8000 => 0, // available for cartridge use
            PRG_ROM_START..=PRG_ROM_END => self.read_prg_rom(addr),
        }
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        match addr {
            RAM..RAM_MIRROR_END => {
                let a = addr & 0b1110_0111_1111_1111;
                self.cpu_vram[a as usize] = data;
            }
            PPU..PPU_MIRROR_END => {
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
            }
            0x4014 => {
                // 2.9	OAMDMA - Sprite DMA ($4014 write)
                // https://www.nesdev.org/wiki/PPU_programmer_reference#OAMDMA

                // data is treated as hi-byte of an addr
                let hi = (data as u16) << 8;

                // do 256 writes
                for i in 0..256 {
                    let data_to_copy = self.mem_read(hi + i);
                    self.ppu.write_to_oam_data(data_to_copy);
                }

                // TODO: Eventually figure out what CPU cycles need to be added for OAM DMA write
            }
            0x4000..=0x4013 | 0x4015 => self.apu.mem_write(addr, data),
            0x4016 => self.gamepad1.write(data),
            0x4017 => self.gamepad2.write(data),
            0x4018..0x4020 => (), // APU and I/O functionality that is normally disabled
            0x4020..0x8000 => (), // available for cartridge use
            PRG_ROM_START..=PRG_ROM_END => panic!("invalid write to ROM cartridge: {:04X}", addr),
        }
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
