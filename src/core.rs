use crate::bus::Bus;

/// CPU (Central Processing Unit)
/// The NES uses  2A03, which is a modified version of the 6502 chip.
pub struct CPU {
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

    /// Bus
    bus: Bus,
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
    Indirect,
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

pub trait Mem {
    fn mem_read(&self, addr: u16) -> u8;

    fn mem_write(&mut self, addr: u16, val: u8);

    fn mem_read_u16(&self, addr: u16) -> u16 {
        // NES CPU uses Little-Endian addressing
        let lo = self.mem_read(addr);
        let hi = self.mem_read(addr + 1);
        (hi as u16) << 8 | lo as u16
    }

    fn mem_write_u16(&mut self, addr: u16, val: u16) {
        let hi = (val >> 8) as u8;
        let lo = (val & 0x00ff) as u8;
        self.mem_write(addr, lo);
        self.mem_write(addr + 1, hi);
    }
}

impl Mem for CPU {
    fn mem_read(&self, addr: u16) -> u8 {
        self.bus.mem_read(addr)
    }

    fn mem_write(&mut self, addr: u16, val: u8) {
        self.bus.mem_write(addr, val);
    }
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            pc: 0,
            sp: 0xff,
            a: 0,
            x: 0,
            y: 0,
            status: 0,
            bus: Bus::new(),
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

    /// used for the "index indirect" and "indirect indexed" lookups
    fn mem_read_zero_page_wrapping(&self, ptr: u8) -> u16 {
        let lo = self.mem_read(ptr as u16);
        let hi = self.mem_read(ptr.wrapping_add(1) as u16);
        (hi as u16) << 8 | (lo as u16)
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
            AddressingMode::Indirect => {
                let base = self.mem_read(self.pc);
                let target = self.mem_read_zero_page_wrapping(base); // indirect
                target
            }
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
        for (i, val) in program.iter().enumerate() {
            self.mem_write((CPU_START + i) as u16, *val);
        }
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
            // TODO: Turn this into trace() fn
            println!(
                "pc={:#06x} (program idx={:03}) op={:#04x}",
                self.pc,
                // self.pc - CPU_START as u16,
                0,
                op
            );
            // println!(
            //     "\ta={:#06x} x={:#06x} y={:#06x} flags={:#010b}",
            //     self.a, self.x, self.y, self.status,
            // );
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
                    // BUG
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

                // ASL
                0x0A => {
                    self.asl(&AddressingMode::None);
                }
                0x06 => {
                    self.asl(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0x16 => {
                    self.asl(&AddressingMode::ZeroPageX);
                    self.pc += 1;
                }
                0x0E => {
                    self.asl(&AddressingMode::Absolute);
                    self.pc += 2;
                }
                0x1E => {
                    self.asl(&AddressingMode::AbsoluteX);
                    self.pc += 2;
                }

                // BIT
                0x24 => {
                    self.bit(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0x2C => {
                    self.bit(&AddressingMode::Absolute);
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

                // EOR
                0x49 => {
                    self.eor(&AddressingMode::Immediate);
                    self.pc += 1;
                }
                0x45 => {
                    self.eor(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0x55 => {
                    self.eor(&AddressingMode::ZeroPageX);
                    self.pc += 1;
                }
                0x4D => {
                    self.eor(&AddressingMode::Absolute);
                    self.pc += 2;
                }
                0x5D => {
                    self.eor(&AddressingMode::AbsoluteX);
                    self.pc += 2;
                }
                0x59 => {
                    self.eor(&AddressingMode::AbsoluteY);
                    self.pc += 2;
                }
                0x41 => {
                    self.eor(&AddressingMode::IndirectX);
                    self.pc += 1;
                }
                0x51 => {
                    self.eor(&AddressingMode::IndirectY);
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

                // CPX
                0xE0 => {
                    self.cpx(&AddressingMode::Immediate);
                    self.pc += 1;
                }
                0xE4 => {
                    self.cpx(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0xEC => {
                    self.cpx(&AddressingMode::Absolute);
                    self.pc += 2;
                }

                // CPY
                0xC0 => {
                    self.cpy(&AddressingMode::Immediate);
                    self.pc += 1;
                }
                0xC4 => {
                    self.cpy(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0xCC => {
                    self.cpy(&AddressingMode::Absolute);
                    self.pc += 2;
                }

                // DEC
                0xC6 => {
                    self.dec(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0xD6 => {
                    self.dec(&AddressingMode::ZeroPageX);
                    self.pc += 1;
                }
                0xCE => {
                    self.dec(&AddressingMode::Absolute);
                    self.pc += 2;
                }
                0xDE => {
                    self.dec(&AddressingMode::AbsoluteX);
                    self.pc += 2;
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
                    // +2 for consumed u16 destination
                    // -1 because the spec says so
                    self.stack_push_u16(self.pc + 2 - 1);
                    self.pc = jump_dest;
                }

                // JMP
                0x4C => {
                    let jump_dest = self.get_operand_address(&AddressingMode::Absolute);
                    self.pc = jump_dest;
                }

                0x6C => {
                    let jump_dest = self.get_operand_address(&AddressingMode::Indirect);
                    self.pc = jump_dest;
                }

                // ROL
                0x2A => {
                    self.rol(&AddressingMode::None);
                }
                0x26 => {
                    self.rol(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0x36 => {
                    self.rol(&AddressingMode::ZeroPageX);
                    self.pc += 1;
                }
                0x2E => {
                    self.rol(&AddressingMode::Absolute);
                    self.pc += 2;
                }
                0x3E => {
                    self.rol(&AddressingMode::AbsoluteX);
                    self.pc += 2;
                }

                // ROR
                0x6A => {
                    self.ror(&AddressingMode::None);
                }
                0x66 => {
                    self.ror(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0x76 => {
                    self.ror(&AddressingMode::ZeroPageX);
                    self.pc += 1;
                }
                0x6E => {
                    self.ror(&AddressingMode::Absolute);
                    self.pc += 2;
                }
                0x7E => {
                    self.ror(&AddressingMode::AbsoluteX);
                    self.pc += 2;
                }

                // RTI
                0x40 => self.rti(),

                // RTS
                0x60 => {
                    let addr = self.stack_pop_u16() + 1;
                    self.pc = addr;
                }

                // Register Instructions
                0xAA => self.tax(),
                0x8A => self.txa(),
                0xCA => self.dex(),
                0xE8 => self.inx(),
                0xA8 => self.tay(),
                0x98 => self.tya(),
                0x88 => self.dey(),
                0xC8 => self.iny(),

                // SBC
                0xE9 => {
                    self.sbc(&AddressingMode::Immediate);
                    self.pc += 1;
                }
                0xE5 => {
                    self.sbc(&AddressingMode::ZeroPage);
                    self.pc += 1;
                }
                0xF5 => {
                    self.sbc(&AddressingMode::ZeroPageX);
                    self.pc += 1;
                }
                0xED => {
                    self.sbc(&AddressingMode::Absolute);
                    self.pc += 2;
                }
                0xFD => {
                    self.sbc(&AddressingMode::AbsoluteX);
                    self.pc += 2;
                }
                0xF9 => {
                    self.sbc(&AddressingMode::AbsoluteY);
                    self.pc += 2;
                }
                0xE1 => {
                    self.sbc(&AddressingMode::IndirectX);
                    self.pc += 1;
                }
                0xF1 => {
                    self.sbc(&AddressingMode::IndirectY);
                    self.pc += 1;
                }

                // Flag (Processor Status) Instructions

                // CLC (CLear Carry)
                0x18 => self.set_flag(Flag::Carry, false),
                // SEC (SEt Carry)
                0x38 => self.set_flag(Flag::Carry, true),
                // CLI (CLear Interrupt)
                0x58 => self.set_flag(Flag::Interrupt, false),
                // SEI (SEt Interrupt)
                0x78 => self.set_flag(Flag::Interrupt, true),
                // CLV (CLear oVerflow)
                0xB8 => self.set_flag(Flag::Overflow, false),
                // CLD (CLear Decimal)
                0xD8 => self.set_flag(Flag::Decimal, false),
                // SED (SEt Decimal)
                0xF8 => self.set_flag(Flag::Decimal, true),

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
        self.set_flag(Flag::Carry, overflow);
        self.set_flag(Flag::Overflow, false); // TODO: not implemented -- fix it for snake to work?
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
        // I've overloaded the addressing mode idea to handle accumlator variant
        if mode == &AddressingMode::None {
            let old_val = self.a;
            let new_val = self.a << 1;

            self.a = new_val;

            self.set_zero_and_negative_flags(new_val);
            self.set_flag(Flag::Carry, old_val & 0b1000_0000 > 0);
        } else {
            let addr = self.get_operand_address(mode);
            let old_val = self.mem_read(addr);
            let new_val = old_val << 1;

            self.mem_write(addr, new_val);

            self.set_zero_and_negative_flags(new_val);
            self.set_flag(Flag::Carry, old_val & 0b1000_0000 > 0);
        }
    }

    // Branch Instructions //

    fn branch_on_flag(&mut self, flag: Flag, is_set: bool) {
        // TODO: this reads the operand from memory the same way as AddressingMode::Immediate...
        // though the docs call this "Relative" addressing due to its use as an offset/displacement.
        // Should I refactor to use an addressing mode?
        let displacement = self.mem_read(self.pc) as i8;

        self.pc += 1;

        println!("is_set={:?} displacement = {:?}", is_set, displacement);

        if self.get_flag(flag) == is_set {
            self.pc = (self.pc as isize + displacement as isize) as u16
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
        let addr = self.get_operand_address(mode);
        let param = self.mem_read(addr);

        let outcome = param & self.a;
        self.set_zero_and_negative_flags(outcome);
        self.set_flag(Flag::Overflow, 0b0100_000 & param > 0);
    }

    // Comparisons //

    fn compare(&mut self, mode: &AddressingMode, val: u8) {
        let addr = self.get_operand_address(mode);
        let param = self.mem_read(addr);

        let gte = val >= param;
        self.set_flag(Flag::Carry, gte);

        let eq = val == param;
        self.set_flag(Flag::Zero, eq);

        let sign = val >= 0x80;
        self.set_flag(Flag::Negative, sign);
    }

    /// CMP (CoMPare accumulator)
    fn cmp(&mut self, mode: &AddressingMode) {
        self.compare(mode, self.a);
    }

    /// CPX (ComPare X register)
    fn cpx(&mut self, mode: &AddressingMode) {
        self.compare(mode, self.x)
    }

    /// CPY (ComPare Y register)
    fn cpy(&mut self, mode: &AddressingMode) {
        self.compare(mode, self.y)
    }

    /// DEC (DECrement memory)
    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let old_val = self.mem_read(addr);
        let new_val = old_val.wrapping_sub(1);
        self.mem_write(addr, new_val);
        self.set_zero_and_negative_flags(new_val);
    }

    /// EOR (bitwise Exclusive OR)
    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let param = self.mem_read(addr);
        self.a ^= param;
        self.set_zero_and_negative_flags(self.a);
    }

    /// INC (INCrement memory)
    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let old_val = self.mem_read(addr);
        let new_val = old_val.wrapping_add(1);
        self.mem_write(addr, new_val);
        self.set_zero_and_negative_flags(new_val);
    }

    // Register instructions //

    /// TAX (Transfer A to X)
    fn tax(&mut self) {
        self.x = self.a;
        self.set_zero_and_negative_flags(self.x);
    }

    /// TXA (Transfer X to A)
    fn txa(&mut self) {
        self.a = self.x;
        self.set_zero_and_negative_flags(self.a);
    }

    /// DEX (DEcrement X)
    fn dex(&mut self) {
        self.x = self.x.wrapping_sub(1);
        self.set_zero_and_negative_flags(self.x);
    }

    /// INX (INcrement X)
    fn inx(&mut self) {
        self.x = self.x.wrapping_add(1);
        self.set_zero_and_negative_flags(self.x);
    }

    /// TAY (Transfer A to Y)
    fn tay(&mut self) {
        self.y = self.a;
        self.set_zero_and_negative_flags(self.y);
    }

    /// TYA (Transfer Y to A)
    fn tya(&mut self) {
        self.a = self.y;
        self.set_zero_and_negative_flags(self.a);
    }

    /// DEY (DEcrement Y)
    fn dey(&mut self) {
        self.y = self.y.wrapping_sub(1);
        self.set_zero_and_negative_flags(self.y);
    }
    /// INY (INcrement Y)
    fn iny(&mut self) {
        self.y = self.y.wrapping_add(1);
        self.set_zero_and_negative_flags(self.y);
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
        // I've overloaded the addressing mode idea to handle accumlator variant
        if mode == &AddressingMode::None {
            let old_val = self.a;
            let new_val = self.a >> 1;

            self.a = new_val;

            self.set_zero_and_negative_flags(new_val);
            self.set_flag(Flag::Carry, old_val & 0b0000_0001 > 0);
        } else {
            let addr = self.get_operand_address(mode);
            let old_val = self.mem_read(addr);
            let new_val = old_val >> 1;

            self.mem_write(addr, new_val);

            self.set_zero_and_negative_flags(new_val);
            self.set_flag(Flag::Carry, old_val & 0b0000_0001 > 0);
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

    /// ROL (ROtate Left)
    fn rol(&mut self, mode: &AddressingMode) {
        // I've overloaded the addressing mode idea to handle accumlator variant
        if mode == &AddressingMode::None {
            let old_val = self.a;
            let new_val = self.a << 1;

            self.a = new_val;

            self.set_zero_and_negative_flags(new_val);
            self.set_flag(Flag::Carry, old_val & 0b1000_0000 > 0);
        } else {
            let addr = self.get_operand_address(mode);
            let old_val = self.mem_read(addr);
            let new_val = old_val << 1;

            self.mem_write(addr, new_val);

            self.set_zero_and_negative_flags(new_val);
            self.set_flag(Flag::Carry, old_val & 0b1000_0000 > 0);
        }

        // // I've overloaded the addressing mode idea to handle accumlator variant
        // let old_val = if mode == &AddressingMode::None {
        //     self.a
        // } else {
        //     let addr = self.get_operand_address(mode);
        //     self.mem_read(addr)
        // };

        // let new_val = old_val << 1;

        // // set new val
        // if mode == &AddressingMode::None {
        //     self.a = new_val;
        // } else {
        //     self.mem_write(addr, new_val);
        // }

        // self.set_zero_and_negative_flags(new_val);
        // self.update_flag(Flag::Carry, old_val & 0b1000_0000 > 0);
    }

    /// ROR (ROtate Right)
    fn ror(&mut self, mode: &AddressingMode) {
        // I've overloaded the addressing mode idea to handle accumlator variant
        if mode == &AddressingMode::None {
            let old_val = self.a;
            let mut new_val = self.a >> 1;
            if self.get_flag(Flag::Carry) {
                new_val |= 0x1 << 7;
            }

            self.a = new_val;

            self.set_zero_and_negative_flags(new_val);
            self.set_flag(Flag::Carry, old_val & 0b0000_0001 > 0);
        } else {
            let addr = self.get_operand_address(mode);
            let old_val = self.mem_read(addr);
            let mut new_val = old_val >> 1;
            if self.get_flag(Flag::Carry) {
                new_val |= 0x1 << 7;
            }

            self.mem_write(addr, new_val);

            self.set_zero_and_negative_flags(new_val);
            self.set_flag(Flag::Carry, old_val & 0b0000_0001 > 0);
        }
    }

    /// RTI (ReTurn from Interrupt)
    fn rti(&mut self) {}

    /// SBC (SuBtract with Carry)
    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let param = self.mem_read(addr);
        let (new_val, overflow) = self.a.overflowing_sub(param);
        self.a = new_val;

        self.set_zero_and_negative_flags(new_val);
        if overflow {
            self.set_flag(Flag::Carry, false);
        }
        self.set_flag(Flag::Overflow, false); // TODO: not implemented -- fix it for snake to work?
    }

    /// STA (STore Accumulator)
    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);

        self.mem_write(addr, self.a);
    }

    /// STX (STore X register)
    fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);

        self.mem_write(addr, self.x);
    }

    /// STY (STore Y register)
    fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);

        self.mem_write(addr, self.y);
    }

    // Stack Instructions //

    /// TXS (Transfer X to Stack ptr)
    fn txs(&mut self) {
        self.sp = self.x;
        self.set_zero_and_negative_flags(self.sp);
    }

    /// TSX (Transfer Stack ptr to X)
    fn tsx(&mut self) {
        self.x = self.sp;
        self.set_zero_and_negative_flags(self.x);
    }

    /// PHA (PusH Accumulator)
    fn pha(&mut self) {
        self.stack_push(self.a)
    }

    /// PLA (PuLl Accumulator)
    fn pla(&mut self) {
        self.a = self.stack_pop();
        self.set_zero_and_negative_flags(self.a);
    }

    /// PHP (PusH Processor status)
    fn php(&mut self) {
        self.stack_push(self.status)
    }

    /// PLP (PuLl Processor status)
    fn plp(&mut self) {
        self.status = self.stack_pop()
    }

    //
    // STATUS FLAGS
    //

    fn set_zero_and_negative_flags(&mut self, val: u8) {
        let z = val == 0;
        self.set_flag(Flag::Zero, z);

        let n = (val & 0b1000_0000) > 0;
        self.set_flag(Flag::Negative, n);
    }

    fn set_flag(&mut self, flag: Flag, on: bool) {
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
        assert_eq!(cpu.mem_read(0x10), 0x55);
        cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

        assert_eq!(cpu.a, 0x55);
    }

    #[test]
    fn test_lda_absolute() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x1003, 0x99);
        cpu.load_and_run(vec![0xad, 0x03, 0x10, 0x00]);

        assert_eq!(cpu.a, 0x99);
    }

    #[test]
    fn test_sta_zero_page() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x85, 0x10, 0x00]);
        cpu.reset();
        cpu.a = 123;
        cpu.run();

        assert_eq!(cpu.mem_read(0x10), 123);
    }

    #[test]
    fn test_sta_zero_page_x() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x95, 0x10, 0x00]);
        cpu.reset();
        cpu.x = 1;
        cpu.mem_write(0x11, 0x20);
        cpu.a = 123;
        cpu.run();

        assert_eq!(cpu.mem_read(0x11), 123);
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
            cpu.mem_write(0x11, 0x22);
            cpu.mem_write(0x12, 0x33);
            cpu.x = 4;
            cpu.y = 2;

            // for IndirectX
            cpu.mem_write(0x22 + 4, 0x11);

            // for IndirectY
            cpu.mem_write(0x22, 0x33);
            cpu.mem_write(0x23, 0x55);

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
            cpu.set_flag(flag, branch_if);
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
            cpu.set_flag(flag, !branch_if);
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

    #[test]
    fn test_register_instructions_for_x() {
        let mut cpu = CPU::new();
        let tax = 0xaa;
        let txa = 0x8a;
        let dex = 0xca;
        let inx = 0xe8;
        cpu.load_and_run(vec![dex, dex, inx, inx, inx, txa, tax]);
        assert_eq!(cpu.x, 1);
        assert_eq!(cpu.a, 1);
    }

    #[test]
    fn test_register_instructions_for_y() {
        let tay = 0xa8;
        let tya = 0x98;
        let dey = 0x88;
        let iny = 0xc8;

        let mut cpu = CPU::new();
        cpu.load_and_run(vec![dey, dey, iny, iny, iny, tya, tay]);
        assert_eq!(cpu.y, 1);
        assert_eq!(cpu.a, 1);
    }

    #[test]
    fn test_compare_instructions() {
        enum Register {
            A,
            X,
            Y,
        }

        for (instruction, register) in [
            (0xC9, Register::A),
            (0xE0, Register::X),
            (0xC0, Register::Y),
        ] {
            // equal to
            let mut cpu = CPU::new();
            cpu.load_and_run(vec![instruction, 0x00]);
            assert_eq!(cpu.status, 0b0000_0011);

            // greater than
            let mut cpu = CPU::new();
            cpu.load_and_run(vec![instruction, 0x01]);
            assert_eq!(cpu.status, 0b0000_0000);

            // less than
            let mut cpu = CPU::new();
            cpu.load(vec![instruction, 0x01]);
            cpu.reset();
            match register {
                Register::A => cpu.a = 0x02,
                Register::X => cpu.x = 0x02,
                Register::Y => cpu.y = 0x02,
            }
            cpu.run();
            assert_eq!(cpu.status, 0b0000_0001);

            // ensure sets Negative flag, if applicable
            let mut cpu = CPU::new();
            cpu.load(vec![instruction, 0x81]);
            cpu.reset();
            match register {
                Register::A => cpu.a = 0x81,
                Register::X => cpu.x = 0x81,
                Register::Y => cpu.y = 0x81,
            }
            cpu.run();
            assert_eq!(cpu.status, 0b1000_0011);
        }
    }

    #[test]
    fn test_0xce_dec_absolute() {
        let mut cpu = CPU::new();
        let dec = 0xce;
        let to_dec = 3;
        let to_dec_addr = CPU_START as u16 + 4;
        let dec_param_addr = CPU_START as u16 + 1;
        cpu.load(vec![dec, 0x00, 0x00, 0x00, to_dec]);
        cpu.reset();
        cpu.mem_write_u16(dec_param_addr, to_dec_addr);
        cpu.run();
        assert_eq!(cpu.mem_read(to_dec_addr as u16), 2);
    }

    #[test]
    fn test_0x49_eor_immediate() {
        let mut cpu = CPU::new();
        let eor = 0x49;
        cpu.load(vec![eor, 0b1111_0000, 0x00]);
        cpu.reset();
        cpu.a = 0b0011_1100;
        cpu.run();
        assert_eq!(cpu.a, 0b1100_1100);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[test]
    fn test_0x0a_asl_shifts_accumulator() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x0a]);
        cpu.reset();
        cpu.a = 0b1000_1001;
        cpu.run();
        assert_eq!(cpu.a, 0b0001_0010);
        assert_eq!(cpu.get_flag(Flag::Carry), true);
    }

    #[test]
    fn test_0x0e_asl_shifts_absolute() {
        let mut cpu = CPU::new();
        let to_lsr = 0b1000_1001;
        let to_lsr_addr = CPU_START as u16 + 4;
        let lsr_param_addr = CPU_START as u16 + 1;
        cpu.load(vec![0x0e, 0x00, 0x00, 0x00, to_lsr]);
        cpu.reset();
        cpu.mem_write_u16(lsr_param_addr, to_lsr_addr);
        cpu.run();
        assert_eq!(cpu.mem_read(to_lsr_addr as u16), 0b0001_0010);
        assert_eq!(cpu.get_flag(Flag::Carry), true);
    }

    #[test]
    fn test_rol() {
        todo!()
    }

    #[test]
    fn test_ror() {
        todo!()
    }

    #[test]
    fn test_0x40_rti() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0x40]);

        todo!()
    }

    #[test]
    fn test_bit() {
        let mut cpu = CPU::new();
        let bit_absolute = 0x2C;
        cpu.mem_write(0x2001, 0b0010);
        cpu.reset();
        cpu.load(vec![bit_absolute, 0x01, 0x20, 0x00]);
        cpu.a = 0b0101;
        cpu.run();

        assert_eq!(cpu.get_flag(Flag::Zero), true);
        assert_eq!(cpu.get_flag(Flag::Negative), true);
        assert_eq!(cpu.get_flag(Flag::Overflow), true);
    }
}
