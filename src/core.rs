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
    /// This is a set of flags
    // TODO: explore flagset representation
    status: u8,

    /// total CPU cycles elapsed
    cycles: usize,
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Copy, Clone)]
enum Flag {
    Negative,
    Overflow,
    Break,
    Decimal,
    Interrupt,
    Zero,
    Carry,
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

    //
    // STACK
    //

    fn stack_push(&mut self, val: u8) {
        log::debug!("stack_push ... val={:?} ({:#04x})", val, val);
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
        log::debug!("stack_pop ... val={:?} ({:#04x})", val, val);
        val
    }

    fn stack_pop_u16(&mut self) -> u16 {
        let lo = self.stack_pop();
        let hi = self.stack_pop();
        let out = (hi as u16) << 8 | lo as u16;
        out
    }

    //
    // MEMORY
    //

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

    //
    // MAIN
    //

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
            println!("got op: {:#06x}", op);
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

                // LDX
                0xA2 => {
                    self.ldx(&AddressingMode::Immediate);
                    self.pc += 1;
                }
                0xA6 => {
                    self.ldx(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0xB6 => {
                    self.ldx(&AddressingMode::ZeroPageY);
                    self.pc += 1;
                }
                0xAE => {
                    self.ldx(&AddressingMode::Absolute);
                    self.pc += 2;
                }
                0xBE => {
                    self.ldx(&AddressingMode::AbsoluteY);
                    self.pc += 2;
                }

                // LDY
                0xA0 => {
                    self.ldy(&AddressingMode::Immediate);
                    self.pc += 1;
                }
                0xA4 => {
                    self.ldy(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0xB4 => {
                    self.ldy(&AddressingMode::ZeroPageX);
                    self.pc += 1;
                }
                0xAC => {
                    self.ldy(&AddressingMode::Absolute);
                    self.pc += 2;
                }
                0xBC => {
                    self.ldy(&AddressingMode::AbsoluteX);
                    self.pc += 2;
                }

                // LSR
                0x4A => {
                    self.lsr(&AddressingMode::None);
                    self.pc += 1;
                }
                0x46 => {
                    self.lsr(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0x56 => {
                    self.lsr(&AddressingMode::ZeroPageX);
                    self.pc += 1;
                }
                0x4E => {
                    self.lsr(&AddressingMode::Absolute);
                    self.pc += 2;
                }
                0x5E => {
                    self.lsr(&AddressingMode::AbsoluteX);
                    self.pc += 2;
                }

                // NOP
                0xEA => self.nop(),

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

                // AND
                0x29 => {
                    self.and(&AddressingMode::Immediate);
                    self.pc += 1;
                }
                0x25 => {
                    self.and(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0x35 => {
                    self.and(&AddressingMode::ZeroPageX);
                    self.pc += 1;
                }
                0x2D => {
                    self.and(&AddressingMode::Absolute);
                    self.pc += 2;
                }
                0x3D => {
                    self.and(&AddressingMode::AbsoluteX);
                    self.pc += 2;
                }
                0x39 => {
                    self.and(&AddressingMode::AbsoluteY);
                    self.pc += 2;
                }
                0x21 => {
                    self.and(&AddressingMode::IndirectX);
                    self.pc += 1;
                }
                0x31 => {
                    self.and(&AddressingMode::IndirectY);
                    self.pc += 1;
                }

                // ADC
                0x69 => {
                    self.adc(&AddressingMode::Immediate);
                    self.pc += 1;
                }
                0x65 => {
                    self.adc(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0x75 => {
                    self.adc(&AddressingMode::ZeroPageX);
                    self.pc += 1;
                }
                0x6D => {
                    self.adc(&AddressingMode::Absolute);
                    self.pc += 2;
                }
                0x7D => {
                    self.adc(&AddressingMode::AbsoluteX);
                    self.pc += 2;
                }
                0x79 => {
                    self.adc(&AddressingMode::AbsoluteY);
                    self.pc += 2;
                }
                0x61 => {
                    self.adc(&AddressingMode::IndirectX);
                    self.pc += 1;
                }
                0x71 => {
                    self.adc(&AddressingMode::IndirectY);
                    self.pc += 1;
                }

                // Branch Instructions
                0x10 => self.bpl(),
                0x30 => self.bmi(),
                0x50 => self.bvc(),
                0x70 => self.bvs(),
                0x90 => self.bcc(),
                0xB0 => self.bcs(),
                0xD0 => self.bne(),
                0xF0 => self.beq(),

                // CMP
                0xC9 => {
                    self.cmp(&AddressingMode::Immediate);
                    self.pc += 1;
                }
                0xC5 => {
                    self.cmp(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0xD5 => {
                    self.cmp(&AddressingMode::ZeroPageX);
                    self.pc += 1;
                }
                0xCD => {
                    self.cmp(&AddressingMode::Absolute);
                    self.pc += 2;
                }
                0xDD => {
                    self.cmp(&AddressingMode::AbsoluteX);
                    self.pc += 2;
                }
                0xD9 => {
                    self.cmp(&AddressingMode::AbsoluteY);
                    self.pc += 2;
                }
                0xC1 => {
                    self.cmp(&AddressingMode::IndirectX);
                    self.pc += 1;
                }
                0xD1 => {
                    self.cmp(&AddressingMode::IndirectY);
                    self.pc += 1;
                }

                // INC
                0xE6 => {
                    self.inc(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0xF6 => {
                    self.inc(&AddressingMode::ZeroPageX);
                    self.pc += 1;
                }
                0xEE => {
                    self.inc(&AddressingMode::Absolute);
                    self.pc += 2;
                }
                0xFE => {
                    self.inc(&AddressingMode::AbsoluteX);
                    self.pc += 2;
                }

                // BRK
                0x00 => return,

                // JSR
                0x20 => {
                    let jump_dest = self.get_operand_address(&AddressingMode::Absolute);
                    self.pc += 2;

                    self.stack_push_u16(self.pc);
                    self.pc = jump_dest;
                }

                // RTS
                0x60 => {
                    let addr = self.stack_pop_u16();
                    self.pc = addr;
                }

                // Register Instructions
                0xAA => self.tax(),
                0xE8 => self.inx(),
                // TAX (Transfer A to X)    $AA
                // TXA (Transfer X to A)    $8A
                // DEX (DEcrement X)        $CA
                // INX (INcrement X)        $E8
                // TAY (Transfer A to Y)    $A8
                // TYA (Transfer Y to A)    $98
                // DEY (DEcrement Y)        $88
                // INY (INcrement Y)        $C8

                // Flag (Processor Status) Instructions

                // CLC (CLear Carry)
                0x18 => self.update_flag(Flag::Carry, false),
                // SEC (SEt Carry)
                0x38 => self.update_flag(Flag::Carry, true),
                // CLI (CLear Interrupt)
                0x58 => self.update_flag(Flag::Interrupt, false),
                // SEI (SEt Interrupt)
                0x78 => self.update_flag(Flag::Interrupt, true),
                // CLV (CLear oVerflow)
                0xB8 => self.update_flag(Flag::Overflow, false),
                // CLD (CLear Decimal)
                0xD8 => self.update_flag(Flag::Decimal, false),
                // SED (SEt Decimal)
                0xF8 => self.update_flag(Flag::Decimal, true),

                _ => {
                    println!("Op {:#04x} not yet implemented", op);
                    todo!();
                }
            }
        }
    }

    //
    // INSTRUCTIONS
    // Order corresponds to spec here: http://www.6502.org/tutorials/6502opcodes.html
    //

    /// ADC (ADd with Carry)
    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let param = self.mem_read(addr);
        let (new_val, overflow) = self.a.overflowing_add(param);
        self.a = new_val;

        self.set_zero_and_negative_flags(new_val);
        self.update_flag(Flag::Carry, overflow);
        self.update_flag(Flag::Overflow, false); // TODO: not implemented
    }

    /// AND (bitwise AND with accumulator)
    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let param = self.mem_read(addr);

        self.a &= param;

        self.set_zero_and_negative_flags(self.a);
    }

    /// ASL (Arithmetic Shift Left)
    fn asl(&mut self, mode: &AddressingMode) {
        // Affects Flags: N Z C
        todo!()
    }

    /// Branch Instructions
    fn branch_on_flag(&mut self, flag: Flag, is_set: bool) {
        // TODO: this reads the operand from memory the same way as AddressingMode::Immediate...
        // though the docs call this "Relative" addressing due to its use as an offset/displacement.
        // Should I refactor to use an addressing mode?
        let displacement = self.mem_read(self.pc);
        self.pc += 1;

        if self.get_flag(flag) == is_set {
            self.pc += displacement as u16;
        }
    }

    fn bpl(&mut self) {
        self.branch_on_flag(Flag::Negative, false);
    }

    fn bmi(&mut self) {
        self.branch_on_flag(Flag::Negative, true);
    }

    fn bvc(&mut self) {
        self.branch_on_flag(Flag::Overflow, false);
    }

    fn bvs(&mut self) {
        self.branch_on_flag(Flag::Overflow, true);
    }

    fn bcc(&mut self) {
        self.branch_on_flag(Flag::Carry, false);
    }

    fn bcs(&mut self) {
        self.branch_on_flag(Flag::Carry, true);
    }

    fn bne(&mut self) {
        self.branch_on_flag(Flag::Zero, false);
    }

    fn beq(&mut self) {
        self.branch_on_flag(Flag::Zero, true);
    }

    /// BIT (test BITs)
    fn bit(&mut self, mode: &AddressingMode) {
        todo!()
    }

    /// CMP (CoMPare accumulator)
    fn cmp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let param = self.mem_read(addr);

        let gte = self.a >= param;
        self.update_flag(Flag::Carry, gte);

        let eq = self.a == param;
        self.update_flag(Flag::Zero, eq);

        let sign = self.a >= 0x80;
        self.update_flag(Flag::Negative, sign);
    }

    /// INC (INCrement memory)
    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let old_val = self.mem_read(addr);
        let new_val = old_val.wrapping_add(1);
        self.mem_write(addr, new_val);
        self.set_zero_and_negative_flags(new_val);
    }

    fn inx(&mut self) {
        self.x = self.x.wrapping_add(1);
        self.set_zero_and_negative_flags(self.x)
    }

    fn tax(&mut self) {
        self.x = self.a;
        self.set_zero_and_negative_flags(self.x);
    }

    /// LDA (LoaD Accumulator)
    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let param = self.mem_read(addr);

        self.a = param;
        self.set_zero_and_negative_flags(self.a);
    }

    /// LDX (LoaD X register)
    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let param = self.mem_read(addr);

        self.x = param;
        self.set_zero_and_negative_flags(self.a);
    }

    /// LDY (LoaD Y register)
    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let param = self.mem_read(addr);

        self.y = param;
        self.set_zero_and_negative_flags(self.a);
    }

    /// LSR (Logical Shift Right)
    fn lsr(&mut self, mode: &AddressingMode) {
        if mode == &AddressingMode::None {
            let old_val = self.a;
            let new_val = self.a >> 1;

            self.a = new_val;

            self.set_zero_and_negative_flags(new_val);
            self.update_flag(Flag::Carry, old_val & 0b0000_0001 > 0);
        } else {
            let addr = self.get_operand_address(mode);
            let old_val = self.mem_read(addr);
            let new_val = old_val >> 1;

            self.mem_write(addr, new_val);

            self.set_zero_and_negative_flags(new_val);
            self.update_flag(Flag::Carry, old_val & 0b0000_0001 > 0);
        }
    }

    /// NOP (No OPeration)
    fn nop(&mut self) {}

    /// ORA (bitwise OR with Accumulator)
    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let param = self.mem_read(addr);

        self.a |= param;

        self.set_zero_and_negative_flags(self.a);
    }

    /// STA (STore Accumulator)
    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);

        self.mem_write(addr, self.a);
    }

    //
    // STATUS FLAGS
    //

    fn set_zero_and_negative_flags(&mut self, val: u8) {
        let z = val == 0;
        self.update_flag(Flag::Zero, z);

        let n = (val & 0b1000_0000) > 0;
        self.update_flag(Flag::Negative, n);
    }

    fn update_flag(&mut self, flag: Flag, on: bool) {
        let base: u8 = 0b0000_0001;
        let shift = match flag {
            Flag::Carry => 0,
            Flag::Zero => 1,
            Flag::Interrupt => 2,
            Flag::Decimal => 3,
            Flag::Break => 4,
            Flag::Overflow => 6,
            Flag::Negative => 7,
        };

        let mask = base << shift;

        if on {
            self.status |= mask;
        } else {
            self.status &= mask ^ 0b1111_1111;
        }
    }

    fn get_flag(&self, flag: Flag) -> bool {
        let base: u8 = 0b0000_0001; // Carry Flag
        let shift = match flag {
            Flag::Carry => 0,
            Flag::Zero => 1,
            Flag::Interrupt => 2,
            Flag::Decimal => 3,
            Flag::Break => 4,
            Flag::Overflow => 6,
            Flag::Negative => 7,
        };

        let mask = base << shift;
        let val = self.status & mask;
        val != 0
    }
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
            println!("Testing addressing mode = {:?}", mode);
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

        let jump_dest: u16 = (CPU_START as u16) + 123;
        cpu.mem_write_u16((CPU_START as u16) + 1, jump_dest);

        cpu.mem_write(jump_dest, rts_opcode);

        cpu.run();
        // +4 =
        // [0    1     2       3  ]
        // [jmp, addr, addr+1, brk] .. and +1 as last pc+1 after brek
        assert_eq!(cpu.pc, (CPU_START as u16) + 4);
    }

    #[test]
    fn test_0x09_ora_immediate() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x09, 0x0f, 0x00]);
        cpu.reset();
        cpu.a = 0xf0;
        cpu.run();
        assert_eq!(cpu.a, 0xff);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[test]
    fn test_0x29_and_immediate() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x29, 0x0f, 0x00]);
        cpu.reset();
        cpu.a = 0xf0;
        cpu.run();
        assert_eq!(cpu.a, 0x00);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[test]
    fn test_flag_processor_status_instructions_set_flags() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0x38, 0x78, 0xF8]);
        assert_eq!(cpu.status, 0b0000_1101);
    }

    #[test]
    fn test_flag_processor_status_instructions_clear_flags() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x18, 0x58, 0xB8, 0xD8]);
        cpu.reset();
        cpu.status = 0xff;
        cpu.run();
        assert_eq!(cpu.status, 0b1011_0010);
    }

    #[test]
    fn test_0xee_inc_absolute() {
        let mut cpu = CPU::new();
        let to_inc = 0x01;
        let to_inc_addr = CPU_START as u16 + 4;
        let inc_param_addr = CPU_START as u16 + 1;
        cpu.load(vec![0xee, 0x00, 0x00, 0x00, to_inc]);
        cpu.reset();
        cpu.mem_write_u16(inc_param_addr, to_inc_addr);
        cpu.run();
        assert_eq!(cpu.mem_read(to_inc_addr as u16), 0x02);
    }

    #[test]
    fn test_0x69_adc_immediate() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0x69, 123]);
        assert_eq!(cpu.a, 123);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[test]
    fn test_0x69_adc_immediate_sets_carry_flag() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x69, 1]);
        cpu.reset();
        cpu.a = 255;
        cpu.run();
        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.get_flag(Flag::Zero), true);
        assert_eq!(cpu.get_flag(Flag::Carry), true);
    }

    #[test]
    fn test_branch_instructions() {
        let specs = vec![
            (0x10, Flag::Negative, false),
            (0x30, Flag::Negative, true),
            (0x50, Flag::Overflow, false),
            (0x70, Flag::Overflow, true),
            (0x90, Flag::Carry, false),
            (0xb0, Flag::Carry, true),
            (0xd0, Flag::Zero, false),
            (0xf0, Flag::Zero, true),
        ];
        for (branch_ins, flag, branch_if) in specs {
            let mut cpu = CPU::new();
            let displacement = 5;
            let program = vec![branch_ins, displacement];
            cpu.load(program.clone());
            cpu.reset();
            cpu.update_flag(flag, branch_if);
            cpu.run();
            let consumed_bpl_op = 2;
            let consumed_brk_op = 1;
            assert_eq!(
                cpu.pc,
                CPU_START as u16 + consumed_bpl_op + displacement as u16 + consumed_brk_op
            );

            let mut cpu = CPU::new();
            cpu.load(program);
            cpu.reset();
            cpu.update_flag(flag, !branch_if);
            cpu.run();
            assert_eq!(
                cpu.pc,
                CPU_START as u16 + consumed_bpl_op + 0 + consumed_brk_op
            );
        }
    }

    #[test]
    fn test_ldx() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa2, 123]);
        assert_eq!(cpu.x, 123);
    }

    #[test]
    fn test_ldy() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa0, 123]);
        assert_eq!(cpu.y, 123);
    }

    #[test]
    fn test_0x4a_lsr_shifts_accumulator() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x4a]);
        cpu.reset();
        cpu.a = 0b1000_1001;
        cpu.run();
        assert_eq!(cpu.a, 0b0100_0100);
        assert_eq!(cpu.get_flag(Flag::Carry), true);
    }

    #[test]
    fn test_0x4e_lsr_shifts_absolute() {
        let mut cpu = CPU::new();
        let to_lsr = 0b1000_1001;
        let to_lsr_addr = CPU_START as u16 + 4;
        let lsr_param_addr = CPU_START as u16 + 1;
        cpu.load(vec![0x4e, 0x00, 0x00, 0x00, to_lsr]);
        cpu.reset();
        cpu.mem_write_u16(lsr_param_addr, to_lsr_addr);
        cpu.run();
        assert_eq!(cpu.mem_read(to_lsr_addr as u16), 0b0100_0100);
        assert_eq!(cpu.get_flag(Flag::Carry), true);
    }

    #[test]
    fn test_0xea_nop() {
        let mut cpu = CPU::new();
        let program = vec![0xea, 0xea, 0xea, 0xea, 0x00];
        cpu.load_and_run(program.clone());
        let end = (CPU_START + program.len()) as u16;
        assert_eq!(cpu.pc, end);
    }
}
