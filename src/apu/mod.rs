mod pulse_register;

use crate::core::Mem;

use pulse_register::PulseRegister;

/// Apu is the Audio Processing Unit
/// https://www.nesdev.org/wiki/APU
pub struct Apu {
    pulse1: PulseRegister,
    pulse2: PulseRegister,
    triangle: [u8; 4],
    noise: [u8; 4],
    dmc: [u8; 4],
    status: u8,
    frame_counter: u8,
}

impl Apu {
    pub fn new() -> Self {
        Self {
            pulse1: PulseRegister::new(),
            pulse2: PulseRegister::new(),
            triangle: [0; 4],
            noise: [0; 4],
            dmc: [0; 4],
            status: 0,
            frame_counter: 0,
        }
    }
}

// https://www.nesdev.org/wiki/APU_Length_Counter
// const LENGTH_TABLE: [u8; 32] = [
//     10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48, 20, 96, 22,
//     192, 24, 72, 26, 16, 28, 32, 30,
// ];

impl Mem for Apu {
    fn mem_read(&mut self, addr: u16) -> u8 {
        match addr {
            0x4000..=0x4013 | 0x4017 => panic!(
                "attempt to read from write only APU register: 0x{:04X}",
                addr
            ),
            0x04015 => self.status,
            _ => panic!("invalid lookup: 0x{:04X} is not in APU memory map", addr),
        }
    }

    fn mem_write(&mut self, addr: u16, val: u8) {
        let idx = (addr % 4) as usize;
        match addr {
            0x4000..=0x4003 => self.pulse1.write_data(idx, val),
            0x4004..=0x4007 => self.pulse2.write_data(idx, val),
            0x4008..=0x400B => self.triangle[idx] = val,
            0x400C..=0x400F => self.noise[idx] = val,
            0x4010..=0x4013 => self.dmc[idx] = val,
            0x04015 => self.status = val,
            0x04017 => self.frame_counter = val,
            _ => panic!("invalid lookup: 0x{:04X} is not in APU memory map", addr),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_apu_mem_writes() {
        let mut apu = Apu::new();
        for x in 0..=0x13 {
            apu.mem_write(0x4000 + x, 0xff);
        }

        apu.mem_write(0x4015, 123);
        assert_eq!(apu.status, 123);
        apu.mem_write(0x4017, 222);
        assert_eq!(apu.frame_counter, 222);
    }

    #[test]
    fn test_apu_mem_read_status() {
        let mut apu = Apu::new();
        let data = apu.mem_read(0x4015);
        assert_eq!(data, 0);

        apu.status = 123;
        let data = apu.mem_read(0x4015);
        assert_eq!(data, 123);
    }

    #[test]
    #[should_panic(expected = "attempt to read from write only APU register")]
    fn test_apu_invalid_mem_read_of_read_only_register_panics() {
        let mut apu = Apu::new();
        _ = apu.mem_read(0x4000);
    }

    #[test]
    #[should_panic(expected = "invalid lookup")]
    fn test_apu_out_of_bounds_mem_read_panics() {
        let mut apu = Apu::new();
        _ = apu.mem_read(0x4000 - 1);
    }

    #[test]
    #[should_panic(expected = "invalid lookup")]
    fn test_apu_out_of_bounds_mem_write_panics() {
        let mut apu = Apu::new();
        apu.mem_write(0x4000 - 1, 0);
    }
}
