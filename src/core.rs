/// CPU (Central Processing Unit)
/// The NES uses  2A03, which is a modified version of the 6502 chip.
struct CPU {
    memory_map: [u8; 0xffff],

    /// program counter
    pc: u16,

    /// stack pointer
    sp: u8,

    /// accumulator
    a: u8,

    /// index register X
    x: u8,

    /// index register Y
    y: u8,

    /// processor status register
    ///This is a set of flags
    // TODO: explore flagset representation
    status: u8,

    /// total CPU cycles elapsed
    cycles: usize,
}

impl CPU {
    fn new() -> Self {
        CPU {
            memory_map: [0; 0xffff],
            pc: 0,
            sp: 0,
            a: 0,
            x: 0,
            y: 0,
            status: 0,
            cycles: 0,
        }
    }

    fn interpret(&mut self, ops: Vec<u8>) {
        self.pc = 0;

        loop {
            let op = ops[self.pc as usize];
            self.pc += 1;
            match op {
                // LDA
                0xA9 => {
                    let param = ops[self.pc as usize];
                    self.a = param;
                    self.pc += 1;

                    self.set_zero_and_negative_flags(self.a);
                }
                // BRK
                0x00 => {
                    return;
                }
                // TAX - transfer A to X
                0xAA => {
                    self.x = self.a;
                    self.set_zero_and_negative_flags(self.x);
                }
                // INX  - increment X
                0xE8 => {
                    self.x = self.x.wrapping_add(1);
                    self.set_zero_and_negative_flags(self.x)
                }
                _ => todo!(),
            }
        }
    }

    fn set_zero_and_negative_flags(&mut self, val: u8) {
        let z = val == 0;
        if z {
            // set the zero flag
            self.status |= 0b0000_0010;
        } else {
            self.status &= 0b1111_1101;
        }
        let n = (val & 0b1000_0000) > 0;
        if n {
            // set the negative flag
            self.status |= 0b1000_0000;
        } else {
            self.status &= 0b0111_1111;
        }
    }

    // fn set_status_flag(&mut self, )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_0xa0_lda_load_nonzero() {
        let mut cpu = CPU::new();
        cpu.interpret(vec![0xa9, 0x55, 0x00]);
        assert_eq!(cpu.a, 0x55);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[test]
    fn test_0xa9_lda_load_zero() {
        let mut cpu = CPU::new();
        cpu.interpret(vec![0xa9, 0x00, 0x00]);
        assert_eq!(cpu.a, 0x00);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.a = 123;
        cpu.interpret(vec![0xaa, 0x00]);
        assert_eq!(cpu.x, 123);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x_sets_zero_flag() {
        let mut cpu = CPU::new();
        cpu.a = 0;
        cpu.x = 123;
        cpu.interpret(vec![0xaa, 0x00]);
        assert_eq!(cpu.x, 0);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.interpret(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.x = 0xff;
        cpu.interpret(vec![0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.x, 1)
    }
}
