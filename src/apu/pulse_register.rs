use bitfields::bitfield;

/// https://www.nesdev.org/wiki/APU_Envelope
#[bitfield(u8)]
// #[derive(Copy, Clone)] // Attributes are passed to the struct.
struct PulseEnvelope {
    /// Duty Cycle
    #[bits(2)]
    d: u8,
    /// Envelope loop / length counter halt
    l: bool,
    /// Constant volume/envelope flag
    c: bool,
    /// Volume/envelope divider period
    #[bits(4)]
    v: u8,
}

/// PulseSweep
/// https://www.nesdev.org/wiki/APU_Sweep
#[bitfield(u8)]
// #[derive(Copy, Clone)] // Attributes are passed to the struct.
struct PulseSweep {
    /// Enabled flag
    e: bool,
    /// Period
    /// The divider's period is P + 1 half-frames
    #[bits(3)]
    p: u8,
    /// Negate flag
    /// 0: add to period, sweeping toward lower frequencies
    /// 1: subtract from period, sweeping toward higher frequencies
    n: bool,
    /// Shift count (number of bits).
    /// If SSS is 0, then behaves like E=0.
    #[bits(3)]
    s: u8,
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
        PulseEnvelope::from_bits(self.data[0]).d()
    }

    fn is_length_counter_halted(&self) -> bool {
        PulseEnvelope::from_bits(self.data[0]).l()
    }

    fn is_constant_volume(&self) -> bool {
        PulseEnvelope::from_bits(self.data[0]).c()
    }

    fn envelope_period(&self) -> u8 {
        PulseEnvelope::from_bits(self.data[0]).v()
    }

    // data[1]

    fn is_sweep_enabled(&self) -> bool {
        let s = PulseSweep::from_bits(self.data[1]);
        s.e() && s.s() > 0
    }

    fn is_sweep_negated(&self) -> bool {
        PulseSweep::from_bits(self.data[1]).n()
    }

    fn sweep_period(&self) -> u8 {
        PulseSweep::from_bits(self.data[1]).p()
    }

    fn sweep_shift_count(&self) -> u8 {
        PulseSweep::from_bits(self.data[1]).s()
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
