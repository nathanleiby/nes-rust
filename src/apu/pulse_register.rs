use bitflags::bitflags;

bitflags! {
    /// PulseEnvelope
    /// https://www.nesdev.org/wiki/APU_Envelope
    struct PulseEnvelope: u8 {
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
    /// PulseSweep
    /// https://www.nesdev.org/wiki/APU_Sweep
    struct PulseSweep: u8 {
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
pub struct PulseRegister {
    data: [u8; 4],
    // TODO
    // length_counter: usize,
}

impl PulseRegister {
    pub(crate) fn new() -> Self {
        Self { data: [0; 4] }
    }

    pub(crate) fn write_data(&mut self, idx: usize, data: u8) {
        self.data[idx] = data;
    }

    // data[0]

    fn duty_cycle(&self) -> u8 {
        let duty = (self.data[0] & PulseEnvelope::D.bits()) >> 6;
        assert!(duty < 4);
        duty
    }

    fn is_length_counter_halted(&self) -> bool {
        PulseEnvelope::from_bits_truncate(self.data[0]).contains(PulseEnvelope::L)
    }

    fn is_constant_volume(&self) -> bool {
        PulseEnvelope::from_bits_truncate(self.data[0]).contains(PulseEnvelope::C)
    }

    fn envelope_period(&self) -> u8 {
        let pe = PulseEnvelope::from_bits_truncate(self.data[0]);
        PulseEnvelope::V.intersection(pe).bits()
    }

    // data[1]

    fn is_sweep_enabled(&self) -> bool {
        let ssc = self.sweep_shift_count();
        let enabled = PulseSweep::from_bits_truncate(self.data[1]).contains(PulseSweep::E);
        ssc > 0 && enabled
    }

    fn is_sweep_negated(&self) -> bool {
        PulseSweep::from_bits_truncate(self.data[1]).contains(PulseSweep::N)
    }

    fn sweep_period(&self) -> u8 {
        let s = PulseSweep::from_bits_truncate(self.data[1]);
        PulseSweep::P.intersection(s).bits() >> 4
    }

    fn sweep_shift_count(&self) -> u8 {
        let s = PulseSweep::from_bits_truncate(self.data[1]);
        PulseSweep::S.intersection(s).bits()
    }

    // data[2] and data[3]

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
}

#[cfg(test)]
mod tests {
    use super::PulseRegister;

    #[test]
    fn test_pulse_register_getters() {
        let mut p = PulseRegister::new();

        assert!(!p.is_length_counter_halted());
        assert_eq!(p.duty_cycle(), 0);
        assert!(!p.is_constant_volume());
        assert_eq!(p.envelope_period(), 0);
        p.data[0] = 0xff;
        assert!(p.is_length_counter_halted());
        assert_eq!(p.duty_cycle(), 3);
        assert!(p.is_constant_volume());
        assert_eq!(p.envelope_period(), 16 - 1);

        assert!(!p.is_sweep_enabled());
        assert!(!p.is_sweep_negated());
        assert_eq!(p.sweep_period(), 0);
        assert_eq!(p.sweep_shift_count(), 0);
        p.data[1] = 0xff;
        assert!(p.is_sweep_enabled());
        assert!(p.is_sweep_negated());
        assert_eq!(p.sweep_period(), 8 - 1);
        assert_eq!(p.sweep_shift_count(), 8 - 1);

        assert_eq!(p.timer(), 0);
        assert_eq!(p.length_counter_load(), 0);
        p.data[2] = 0xff;
        p.data[3] = 0xff;
        assert_eq!(p.timer(), 2048 - 1);
        assert_eq!(p.length_counter_load(), 32 - 1);
    }
}
