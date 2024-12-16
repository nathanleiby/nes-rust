use crate::core::Mem;

use bitflags::bitflags;

/// Apu is the Audio Processing Unit
pub struct Apu {
    // pulse_register:
    // Pulse ($4000–$4007)
    pulse1: PulseRegister,
    pulse2: PulseRegister,
    // Triangle ($4008–$400B)
    // 2.4	Noise ($400C–$400F)
    // 2.5	DMC ($4010–$4013)
    // 2.5.1	Other Uses
    // 2.6	Status ($4015)
    // 2.7	Frame Counter ($4017)
    // 2.7.1	Length Counter
    length_counter: usize,
}

bitflags! {
    /// Pulse
    /// https://www.nesdev.org/wiki/APU_Sweep
    struct Pulse: u8 {
        /// Duty Cycle
        const D = 0b1100_0000;
        /// Envelope loop / length counter halt
        const L = 0b0010_0000;
        /// Constant volume/envelope flag
        const C = 0b0001_0000;
        /// Volume/envelope divider period
        const V = 0b0000_1111;
    }
}

bitflags! {
    /// APU Sweep
    /// https://www.nesdev.org/wiki/APU_Sweep
    struct Sweep: u8 {
        /// Enabled flag
        const E = 0b1000_0000;
        /// Period
        /// The divider's period is P + 1 half-frames
        const P = 0b0111_0000;
        /// Negate flag
        /// 0: add to period, sweeping toward lower frequencies
        /// 1: subtract from period, sweeping toward higher frequencies
        const N = 0b0000_1000;
        /// Shift count (number of bits).
        /// If SSS is 0, then behaves like E=0.
        const S = 0b0000_0111;
    }
}

/// https://www.nesdev.org/wiki/APU_Pulse
struct PulseRegister {
    data: [u8; 4],
}

/// https://www.nesdev.org/wiki/APU_Length_Counter
const LENGTH_TABLE: [u8; 32] = [
    10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48, 20, 96, 22,
    192, 24, 72, 26, 16, 28, 32, 30,
];

impl PulseRegister {
    fn duty_cycle(&self) -> u8 {
        let duty = (self.data[0] & Pulse::D.bits()) >> 6;
        assert!(duty < 4);
        duty
    }

    fn is_length_counter_halted(&self) -> bool {
        Pulse::from_bits_truncate(self.data[0]).contains(Pulse::L)
    }

    fn is_constant_volume(&self) -> bool {
        Pulse::from_bits_truncate(self.data[1]).contains(Pulse::C)
    }

    /// The sequencer is clocked by an 11-bit timer.
    /// The timer value t = HHHLLLLLLLL is formed by timer high and timer low.
    fn timer(&self) -> u16 {
        let low8 = self.data[2] as u16;
        let hi3 = ((self.data[3] & 0b0000_0111) as u16) << 8;
        hi3 + low8
    }

    /// Gives an index into the LENGTH_TABLE
    fn length_counter_load(&self) -> u8 {
        (self.data[3] & 0b1111_1000) >> 3
    }

    fn is_sweep_enabled(&self) -> bool {
        let ssc = self.sweep_shift_count();
        let enabled = Sweep::from_bits_truncate(self.data[1]).contains(Sweep::E);
        ssc > 0 && enabled
    }

    fn is_sweep_negated(&self) -> bool {
        Sweep::from_bits_truncate(self.data[1]).contains(Sweep::N)
    }

    fn sweep_period(&self) -> u8 {
        let s = Sweep::from_bits_truncate(self.data[1]);
        Sweep::P.intersection(s).bits() >> 4
    }

    fn sweep_shift_count(&self) -> u8 {
        let s = Sweep::from_bits_truncate(self.data[1]);
        Sweep::S.intersection(s).bits()
    }
}

// // https://www.nesdev.org/wiki/APU_registers

impl Mem for Apu {
    fn mem_read(&mut self, addr: u16) -> u8 {
        match addr {
            // They are write-only except $4015 which is read/write
            0x4000..=0x4013 | 0x4017 => panic!(
                "attempt to read from write only APU register: 0x{:04X}",
                addr
            ),
            0x04015 => todo!("read APU status"),
            _ => panic!("invalid lookup: 0x{:04X} is not in APU memory map", addr),
        }
    }

    fn mem_write(&mut self, addr: u16, val: u8) {
        match addr {
            0x4000..=0x4003 => self.pulse1.data[(0x4000 - addr) as usize] = val,
            0x4004..=0x4007 => self.pulse2.data[(0x4004 - addr) as usize] = val,
            // 0x4008..=0x400B => panic!("APU write-only register: triangle register"),
            // 0x400C..=0x400F => panic!("APU write-only register: noise register"),
            // 0x4010..=0x4013 => todo!("dmc"),
            // They are write-only except $4015 which is read/write
            // 0x04015 => todo!("status"),
            // 0x04017 => todo!("frame counter"),
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::PulseRegister;

    #[test]
    fn test_pulse_register_getters() {
        let mut p = PulseRegister { data: [0; 4] };
        assert_eq!(p.timer(), 0);
        assert_eq!(p.length_counter_load(), 0);
        p.data[2] = 0xff;
        p.data[3] = 0b0000_0111;
        assert_eq!(p.length_counter_load(), 0);
        assert_eq!(p.timer(), 2048 - 1);
        p.data[3] = 0xff;
        assert_eq!(p.length_counter_load(), 32 - 1);

        assert_eq!(p.duty_cycle(), 0);
        assert_eq!(p.is_constant_volume(), false);

        assert_eq!(p.is_length_counter_halted(), true);
    }
}
