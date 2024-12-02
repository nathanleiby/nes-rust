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

#[derive(Debug)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectY,
    None,
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

    fn mem_read(&self, addr: u16) -> u8 {
        self.memory_map[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, val: u8) {
        self.memory_map[addr as usize] = val;
    }

    fn mem_read_u16(&self, addr: u16) -> u16 {
        // NES CPU uses Little-Endian addressing
        let lo = self.memory_map[addr as usize];
        let hi = self.memory_map[(addr + 1) as usize];
        (hi as u16) << 8 | lo as u16
    }

    /// used for the "index indirect" and "indirect indexed" lookups
    fn mem_read_zero_page_wrapping(&self, ptr: u8) -> u16 {
        let lo = self.mem_read(ptr as u16);
        let hi = self.mem_read(ptr.wrapping_add(1) as u16);
        (hi as u16) << 8 | (lo as u16)
    }

    fn mem_write_u16(&mut self, addr: u16, val: u16) {
        let hi = (val >> 8) as u8;
        let lo = (val & 0x00ff) as u8;
        self.mem_write(addr, lo);
        self.mem_write(addr + 1, hi);
    }

    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.pc,
            AddressingMode::ZeroPage => self.mem_read(self.pc) as u16,
            AddressingMode::ZeroPageX => self.mem_read(self.pc).wrapping_add(self.x) as u16,
            AddressingMode::ZeroPageY => self.mem_read(self.pc).wrapping_add(self.y) as u16,
            AddressingMode::Absolute => self.mem_read_u16(self.pc),
            AddressingMode::AbsoluteX => self.mem_read_u16(self.pc).wrapping_add(self.x as u16),
            AddressingMode::AbsoluteY => self.mem_read_u16(self.pc).wrapping_add(self.y as u16),
            // TODO: is basic Indirect supported
            AddressingMode::IndirectX => {
                // "Indexed indirect"
                let base = self.mem_read(self.pc);
                let target = base.wrapping_add(self.x); // indexed
                self.mem_read_zero_page_wrapping(target) // indirect
            }
            AddressingMode::IndirectY => {
                // "Indirect indexed"
                let base = self.mem_read(self.pc);
                let target = self.mem_read_zero_page_wrapping(base); // indirect
                target.wrapping_add(self.y as u16) // indexed
            }
            AddressingMode::None => panic!("mode {:?} is not supported", mode),
        }
    }

    // load method should load a program into PRG ROM space and save the reference to the code into 0xFFFC memory cell
    fn load(&mut self, program: Vec<u8>) {
        let start = 0x8000;
        self.memory_map[start..start + program.len()].copy_from_slice(&program);
        self.mem_write_u16(0xFFFC, start as u16);
    }

    fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    // reset method should restore the state of all registers, and initialize program_counter by the 2-byte value stored at 0xFFFC
    fn reset(&mut self) {
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.sp = 0;
        self.pc = self.mem_read_u16(0xFFFC);
    }

    fn run(&mut self) {
        loop {
            let op = self.mem_read(self.pc);
            self.pc += 1;
            match op {
                // LDA - load next byte into register A
                0xA9 => {
                    let param = self.mem_read(self.pc);
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
        cpu.load_and_run(vec![0xa9, 0x55, 0x00]);
        assert_eq!(cpu.a, 0x55);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[test]
    fn test_0xa9_lda_load_zero() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert_eq!(cpu.a, 0x00);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xaa, 0x00]);
        cpu.reset();
        cpu.a = 123;
        cpu.run();
        assert_eq!(cpu.x, 123);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x_sets_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xaa, 0x00]);
        cpu.reset();
        cpu.x = 0xff;
        cpu.a = 0;
        cpu.run();
        assert_eq!(cpu.x, 0);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xe8, 0xe8, 0x00]);
        cpu.reset();
        cpu.x = 0xff;
        cpu.run();

        assert_eq!(cpu.x, 1)
    }
}
