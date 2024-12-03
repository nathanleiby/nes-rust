/// CPU (Central Processing Unit)
/// The NES uses  2A03, which is a modified version of the 6502 chip.
pub struct CPU {
    memory: [u8; 0xffff],

    /// program counter
    pc: u16,

    /// stack pointer
    /// The stack lives in memory between $0100 and $01ff.
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

// TODO: restore this later. It's hard-coded to support Snake right now
// const CPU_START: u16 = 0x8000;
const CPU_START: usize = 0x0600;

impl CPU {
    pub fn new() -> Self {
        CPU {
            memory: [0; 0xffff],
            pc: 0,
            sp: 0xff,
            a: 0,
            x: 0,
            y: 0,
            status: 0,
            cycles: 0,
        }
    }

    fn stack_push(&mut self, val: u8) {
        println!("stack_push ... val={:?} ({:#04x})", val, val);
        self.mem_write(0x0100 + self.sp as u16, val);
        self.sp -= 1;
    }

    fn stack_push_u16(&mut self, val: u16) {
        let lo = (val & 0xff) as u8;
        let hi = (val >> 8) as u8;
        self.stack_push(hi);
        self.stack_push(lo);
    }

    fn stack_pop(&mut self) -> u8 {
        self.sp += 1;
        let val = self.mem_read(0x0100 + self.sp as u16);
        println!("stack_pop ... val={:?} ({:#04x})", val, val);
        val
    }

    fn stack_pop_u16(&mut self) -> u16 {
        println!("{:?}", self.memory[0x01ff]);
        println!("{:?}", self.memory[0x01fe]);
        println!("{:?}", self.memory[0x01fd]);
        let lo = self.stack_pop();
        let hi = self.stack_pop();
        let out = (hi as u16) << 8 | lo as u16;
        println!(
            "stack pop 16 .. hi = {:#06x}, lo = {:#06x}, out = {:#06x} to stack",
            lo, hi, out
        );
        out
    }

    pub fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    pub fn mem_write(&mut self, addr: u16, val: u8) {
        self.memory[addr as usize] = val;
    }

    fn mem_read_u16(&self, addr: u16) -> u16 {
        // NES CPU uses Little-Endian addressing
        let lo = self.memory[addr as usize];
        let hi = self.memory[(addr + 1) as usize];
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
    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[CPU_START..CPU_START + program.len()].copy_from_slice(&program);
        self.mem_write_u16(0xFFFC, CPU_START as u16);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    // reset method should restore the state of all registers, and initialize program_counter by the 2-byte value stored at 0xFFFC
    pub fn reset(&mut self) {
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.sp = 0xff;
        self.pc = self.mem_read_u16(0xFFFC);
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where
        F: FnMut(&mut CPU),
    {
        loop {
            callback(self);

            let op = self.mem_read(self.pc);
            println!("GOT OP: {:#06x}", op);
            self.pc += 1;
            match op {
                // LDA
                0xA9 => {
                    self.lda(&AddressingMode::Immediate);
                    self.pc += 1;
                }
                0xA5 => {
                    self.lda(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0xB5 => {
                    self.lda(&AddressingMode::ZeroPageX);
                    self.pc += 1;
                }
                0xAD => {
                    self.lda(&AddressingMode::Absolute);
                    self.pc += 2;
                }
                0xBD => {
                    self.lda(&AddressingMode::AbsoluteX);
                    self.pc += 2;
                }
                0xB9 => {
                    self.lda(&AddressingMode::AbsoluteY);
                    self.pc += 2;
                }
                0xA1 => {
                    self.lda(&AddressingMode::IndirectX);
                    self.pc += 1;
                }
                0xB1 => {
                    self.lda(&AddressingMode::IndirectY);
                    self.pc += 1;
                }

                // STA
                0x85 => {
                    self.sta(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0x95 => {
                    self.sta(&AddressingMode::ZeroPageX);
                    self.pc += 1;
                }
                0x8D => {
                    self.sta(&AddressingMode::Absolute);
                    self.pc += 2;
                }
                0x9D => {
                    self.sta(&AddressingMode::AbsoluteX);
                    self.pc += 2;
                }
                0x99 => {
                    self.sta(&AddressingMode::AbsoluteY);
                    self.pc += 2;
                }
                0x81 => {
                    self.sta(&AddressingMode::IndirectX);
                    self.pc += 1;
                }
                0x91 => {
                    self.sta(&AddressingMode::IndirectY);
                    self.pc += 1;
                }

                // ORA
                0x09 => {
                    self.ora(&AddressingMode::Immediate);
                    self.pc += 1;
                }
                0x05 => {
                    self.ora(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0x15 => {
                    self.ora(&AddressingMode::ZeroPageX);
                    self.pc += 1;
                }
                0x0D => {
                    self.ora(&AddressingMode::Absolute);
                    self.pc += 2;
                }
                0x1D => {
                    self.ora(&AddressingMode::AbsoluteX);
                    self.pc += 2;
                }
                0x19 => {
                    self.ora(&AddressingMode::AbsoluteY);
                    self.pc += 2;
                }
                0x01 => {
                    self.ora(&AddressingMode::IndirectX);
                    self.pc += 1;
                }
                0x11 => {
                    self.ora(&AddressingMode::IndirectY);
                    self.pc += 1;
                }

                // BRK
                0x00 => {
                    return;
                }

                // JSR
                0x20 => {
                    let jump_dest = self.get_operand_address(&AddressingMode::Absolute);
                    println!("jump dest={:?}", jump_dest);
                    println!("saving pc={:?} ({:#06x}) to stack", self.pc, self.pc);
                    self.stack_push_u16(self.pc);
                    self.pc = jump_dest;
                }

                // RTS
                0x60 => {
                    let addr = self.stack_pop_u16();

                    println!("return target ={:?} ({:#06x}) to stack", addr, addr);

                    self.pc = addr;
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
                _ => {
                    println!("Op {:#04x} not yet implemented", op);
                    todo!();
                }
            }
        }
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let param = self.mem_read(addr);
        self.a = param;
        self.set_zero_and_negative_flags(self.a);
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.a);
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

    /// ORA (bitwise OR with Accumulator)
    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let param = self.mem_read(addr);
        self.a |= param;

        self.set_zero_and_negative_flags(self.a);
    }

    // fn set_status_flag(&mut self, )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_0xa0_lda_immediate_nonzero() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x55, 0x00]);
        assert_eq!(cpu.a, 0x55);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[test]
    fn test_0xa9_lda_immediate_load_zero() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert_eq!(cpu.a, 0x00);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[test]
    fn test_lda_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);
        cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

        assert_eq!(cpu.a, 0x55);
    }

    #[test]
    fn test_lda_absolute() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x2001, 0x99);
        cpu.load_and_run(vec![0xad, 0x01, 0x20, 0x00]);

        assert_eq!(cpu.a, 0x99);
    }

    #[test]
    fn test_sta_zero_page() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x85, 0x10, 0x00]);
        cpu.reset();
        cpu.a = 123;
        cpu.run();

        assert_eq!(cpu.memory[0x10], 123);
    }

    #[test]
    fn test_sta_zero_page_x() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x95, 0x10, 0x00]);
        cpu.reset();
        cpu.x = 1;
        cpu.memory[0x11] = 0x20;
        cpu.a = 123;
        cpu.run();

        assert_eq!(cpu.memory[0x11], 123);
    }

    #[test]
    fn test_addressing_modes() {
        let pc = 0x11;
        for (mode, expected) in vec![
            (AddressingMode::Immediate, pc),
            (AddressingMode::ZeroPage, 0x22),
            (AddressingMode::ZeroPageX, 0x22 + 4),
            (AddressingMode::ZeroPageY, 0x22 + 2),
            (AddressingMode::Absolute, 0x3322),
            (AddressingMode::AbsoluteX, 0x3322 + 4),
            (AddressingMode::AbsoluteY, 0x3322 + 2),
            (AddressingMode::IndirectX, 0x11),
            (AddressingMode::IndirectY, 0x5533 + 2),
        ] {
            let mut cpu = CPU::new();
            cpu.pc = pc;
            cpu.memory[0x11] = 0x22;
            cpu.memory[0x12] = 0x33;
            cpu.x = 4;
            cpu.y = 2;

            // for IndirectX
            cpu.memory[0x22 + 4] = 0x11;

            // for IndirectY
            cpu.memory[0x22] = 0x33;
            cpu.memory[0x23] = 0x55;

            let actual = cpu.get_operand_address(&mode);
            println!("Testing addressing mode = {:?}", mode);
            assert_eq!(actual, expected);
        }
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

    #[test]
    fn test_jmp() {
        let mut cpu = CPU::new();
        let jump_dest: u16 = (CPU_START as u16) + 333;
        cpu.mem_write_u16((CPU_START as u16) + 1, jump_dest);
        cpu.load_and_run(vec![0x20]);

        // expect that you jump to jump_dest, then pc steps forward one more time while reading a BRK
        // (since everything is 0x00 BRK by default)
        assert_eq!(cpu.pc, (jump_dest + 1) as u16);
    }

    #[test]
    fn test_rts() {
        let mut cpu = CPU::new();

        let jmp_opcode = 0x20;
        let rts_opcode = 0x60;
        cpu.load(vec![jmp_opcode]);
        cpu.reset();

        // TODO: I'm not sure why the jump_dest matters here, but it seems to cause test to pass/fail
        // let jump_dest: u16 = (CPU_START as u16) + 5;
        let jump_dest: u16 = (CPU_START as u16) + 123;
        cpu.mem_write_u16((CPU_START as u16) + 1, jump_dest);

        cpu.mem_write(jump_dest, rts_opcode);

        cpu.run();
        // +4 =
        // [jmp, addr, addr+1, brk] .. and +1 as last pc+1 after brek
        assert_eq!(cpu.pc, (CPU_START as u16) + 4);
    }

    #[test]
    fn test_ora() {
        todo!()
        // assert_eq!(cpu.status, 0b0000_0000);
    }
}
