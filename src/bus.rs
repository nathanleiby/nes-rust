use crate::{core::Mem, gamepad::GamepadRegister, ppu::Ppu, rom::Rom};

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

        Bus {
            cpu_vram: [0; 0x800],
            rom,
            ppu,
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
        if (RAM..RAM_MIRROR_END).contains(&addr) {
            let a = addr & 0b1110_0111_1111_1111;
            self.cpu_vram[a as usize]
        } else if (PPU..PPU_MIRROR_END).contains(&addr) {
            // The PPU exposes 8 registers. They are mirrored every 8 bytes in this range.
            let register_idx = addr % 8;
            match register_idx {
                // 0 | 1 | 3 | 5 | 6 => panic!(
                //     "attempt to read from write-only PPU register: 0x200{}",
                //     register_idx
                // ),
                0 | 1 | 3 | 5 | 6 => 0, // TODO
                2 => self.ppu.read_from_status(),
                4 => self.ppu.read_from_oam_data(),
                7 => self.ppu.read_from_data(),
                8..=u16::MAX => panic!("invalid PPU register IDX: {}", register_idx),
            }
        } else if addr == 0x4014 {
            panic!("attempt to read from write-only PPU register: 0x4014 (OAMDMA - Sprite DMA)");
        } else if (PRG_ROM_START..=PRG_ROM_END).contains(&addr) {
            self.read_prg_rom(addr)
        } else if addr == 0x4016 {
            self.gamepad1.read()
        } else if addr == 0x4017 {
            self.gamepad2.read()
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
        } else if addr == 0x4014 {
            // 2.9	OAMDMA - Sprite DMA ($4014 write)
            self.ppu.write_to_oam_data(data);
        } else if (PRG_ROM_START..=PRG_ROM_END).contains(&addr) {
            panic!("attempt to write to ROM cartridge")
        } else if [0x4000, 0x4001, 0x4002, 0x4003, 0x4004, 0x4015, 0x4017].contains(&addr) {
            // } else if [0x4000, 0x4001, 0x4002, 0x4003, 0x4004, 0x4015, 0x4017].contains(&addr) {
        } else if (0x4000..=0x4013).contains(&addr) || addr == 0x4015 {
            // todo!(
            //     "attempt to write to addr=0x{:04X}. This will be the APU, later!",
            //     addr
            // )
            // TODO
        } else if addr == 0x4016 {
            self.gamepad1.write(data);
        } else if addr == 0x4017 {
            self.gamepad2.write(data);
        } else {
            todo!("attempt to write to memory addr=0x{:04X}", addr)
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
