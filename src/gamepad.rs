use bitflags::bitflags;

bitflags! {
    /// Tracks the state of each button. (1 - pressed, 0 - released)
    pub struct GamepadButtons: u8 {
        const ButtonA = 1 << 0;
        const ButtonB = 1 << 1;
        const Select = 1 << 2;
        const Start = 1 << 3;
        const Up = 1 << 4;
        const Down = 1 << 5;
        const Left = 1 << 6;
        const Right = 1 << 7;
    }
}

struct GamepadRegister {
    current_idx: u8,

    button_status: GamepadButtons,
    /// strobe bit on - controller reports only status of the button A on every read
    /// strobe bit off - controller cycles through all buttons
    strobe: bool,
}

impl GamepadRegister {
    pub fn new() -> Self {
        GamepadRegister {
            current_idx: 0,
            strobe: false,
            button_status: GamepadButtons::from_bits_truncate(0),
        }
    }

    // returns the state of one button
    pub fn read(&mut self) -> u8 {
        // TOOD: handle strobe bit behavior

        if self.current_idx >= 8 {
            1
        } else {
            self.current_idx += 1;
            let is_current_button_pressed = self.button_status.bits() & (1 << self.current_idx) > 0;
            is_current_button_pressed as u8
        }
    }

    pub fn write(&mut self, data: u8) {
        self.strobe = data & 1 > 0;
        if self.strobe {
            self.current_idx = 0;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::gamepad::{GamepadButtons, GamepadRegister};

    #[test]
    fn test_strobe_bit() {
        let mut g = GamepadRegister::new();

        g.button_status.insert(GamepadButtons::ButtonA);

        assert_eq!(g.current_idx, 0);
        assert_eq!(1, g.read());
        assert_eq!(g.current_idx, 1);

        for i in 2..8 {
            assert_eq!(0, g.read());
            assert_eq!(g.current_idx, i);
        }

        for _ in 0..3 {
            assert_eq!(
                1,
                g.read(),
                "should continually return 1's until a strobe mode change"
            )
        }

        g.write(0);

        for _ in 0..3 {
            assert_eq!(
                1,
                g.read(),
                "should continually return 1's until a strobe mode change"
            )
        }

        g.write(1);

        assert_eq!(g.read(), 1);
        assert_eq!(g.read(), 0);
    }
}
