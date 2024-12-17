use std::env;

use crate::{
    bus::Bus,
    ops::{addressing_mode_to_size, is_official, lookup_opcode, OpName},
    rom::Rom,
    utility::{addr_from, is_addr_at_page_edge, is_page_cross},
};

/// CPU (Central Processing Unit)
/// The NES uses  2A03, which is a modified version of the 6502 chip.
pub struct Cpu<'a> {
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
    bus: Bus<'a>,

    /// CPU Cycles
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
    Indirect,
    Relative,
    Accumulator,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Flag {
    Carry, // 0th bit
    Zero,
    InterruptDisable,
    Decimal,
    Break,
    Break2,
    Overflow,
    Negative, // 7th bit
}

// TODO: restore this later. It's hard-coded to support Snake right now
// const CPU_START: u16 = 0x8000;
pub const CPU_START: usize = 0x0600;
const DEFAULT_STACK_POINTER: u8 = 0xfd;
const DEFAULT_STATUS: u8 = 0b100100;

pub trait Mem {
    fn mem_peek(&self, addr: u16) -> u8;

    fn mem_peek_u16(&self, addr: u16) -> u16 {
        // NES CPU uses Little-Endian addressing
        let lo = self.mem_peek(addr);
        let hi = self.mem_peek(addr + 1);
        (hi as u16) << 8 | lo as u16
    }

    fn mem_read(&mut self, addr: u16) -> u8;

    fn mem_write(&mut self, addr: u16, val: u8);

    fn mem_read_u16(&mut self, addr: u16) -> u16 {
        // NES CPU uses Little-Endian addressing
        let lo = self.mem_read(addr);
        let hi = self.mem_read(addr + 1);
        (hi as u16) << 8 | lo as u16
    }

    // TODO: I think this will be used later
    #[allow(dead_code)]
    fn mem_write_u16(&mut self, addr: u16, val: u16) {
        let hi = (val >> 8) as u8;
        let lo = (val & 0x00ff) as u8;
        self.mem_write(addr, lo);
        self.mem_write(addr + 1, hi);
    }
}

impl Mem for Cpu<'_> {
    /// Peek is non-mutating version of Read.
    ///
    /// Because peek is called on all addresses when tracing,
    /// by convention if it is used on a write-only register,
    /// we return 0
    fn mem_peek(&self, addr: u16) -> u8 {
        self.bus.mem_peek(addr)
    }

    fn mem_read(&mut self, addr: u16) -> u8 {
        self.bus.mem_read(addr)
    }

    fn mem_write(&mut self, addr: u16, val: u8) {
        self.bus.mem_write(addr, val);
    }
}

impl<'a, 'b: 'a> Cpu<'a> {
    pub fn new() -> Self {
        let rom = Rom::new_test_rom(vec![]);
        Cpu {
            pc: 0,
            sp: DEFAULT_STACK_POINTER,
            a: 0,
            x: 0,
            y: 0,
            status: DEFAULT_STATUS,
            bus: Bus::new(rom),
            cycles: 0,
        }
    }

    pub fn set_bus(&mut self, bus: Bus<'b>) {
        self.bus = bus
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

        (hi as u16) << 8 | lo as u16
    }

    //
    // MEMORY
    //

    /// used for the "index indirect" and "indirect indexed" lookups
    fn mem_read_zero_page_wrapping(&mut self, ptr: u8) -> (u16, bool) {
        let lo = self.mem_read(ptr as u16);
        let (hi_ptr, is_page_crossed) = ptr.overflowing_add(1);
        let hi = self.mem_read(hi_ptr as u16);

        let result = (hi as u16) << 8 | (lo as u16);

        (result, is_page_crossed)
    }

    /// returns (u16: operand addresss, bool: is_page_crossed)
    fn get_operand_address2(&mut self, mode: &AddressingMode, tick_on_page_cross: bool) -> u16 {
        let mut page_was_crossed = false;
        let addr = match mode {
            AddressingMode::Immediate => self.pc,
            AddressingMode::ZeroPage => self.mem_read(self.pc) as u16,
            AddressingMode::ZeroPageX => self.mem_read(self.pc).wrapping_add(self.x) as u16,
            AddressingMode::ZeroPageY => self.mem_read(self.pc).wrapping_add(self.y) as u16,
            AddressingMode::Absolute => self.mem_read_u16(self.pc),
            AddressingMode::AbsoluteX => {
                let base = self.mem_read_u16(self.pc);
                let result = base.wrapping_add(self.x as u16);

                if is_page_cross(base, result) {
                    page_was_crossed = true;
                }

                result
            }
            AddressingMode::AbsoluteY => {
                let base = self.mem_read_u16(self.pc);
                let result = base.wrapping_add(self.y as u16);

                if is_page_cross(base, result) {
                    page_was_crossed = true;
                }

                result
            }
            AddressingMode::Indirect => {
                let base = self.mem_read_u16(self.pc);

                if is_addr_at_page_edge(base) {
                    let first = self.mem_read(base);
                    // Intentionally wrap incorrectly, to recreate buggy behavior from original 6502
                    let second = self.mem_read(base & 0xff00);
                    ((second as u16) << 8) + first as u16
                } else {
                    self.mem_read_u16(base)
                }
            }
            AddressingMode::IndirectX => {
                // "Indexed indirect"
                let base = self.mem_read(self.pc);
                let target = base.wrapping_add(self.x); // indexed
                let (result, _) = self.mem_read_zero_page_wrapping(target); // indirect

                if is_addr_at_page_edge(result) {
                    page_was_crossed = true;
                }

                result
            }
            AddressingMode::IndirectY => {
                // "Indirect indexed"
                let base = self.mem_read(self.pc);
                let (target, _) = self.mem_read_zero_page_wrapping(base); // indirect
                let result = target.wrapping_add(self.y as u16); // indexed

                if is_addr_at_page_edge(base as u16) || is_addr_at_page_edge(target) {
                    page_was_crossed = true;
                }

                result
            }
            _ => panic!("mode {:?} is not supported", mode),
        };

        if tick_on_page_cross && page_was_crossed {
            self.tick(1)
        }

        addr
    }

    fn get_operand_address(&mut self, mode: &AddressingMode) -> u16 {
        self.get_operand_address2(mode, false)
    }

    //
    // MAIN
    //

    // Test Helpers

    fn _load_test_rom(&mut self, program: Vec<u8>) {
        let rom = Rom::new_test_rom(program);

        self.set_bus(Bus::new(rom));
    }

    fn _load_and_run(&mut self, program: Vec<u8>) {
        self._load_test_rom(program);
        self.reset();
        self._run();
    }

    fn _run(&mut self) {
        self.run_with_callback(|_| {});
    }

    // reset method should restore the state of all registers, and initialize program_counter by the 2-byte value stored at 0xFFFC
    pub fn reset(&mut self) {
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.sp = DEFAULT_STACK_POINTER;
        self.pc = self.mem_read_u16(0xFFFC);
        if env::var("NESTEST_HACK").is_ok() {
            self.pc = 0xC000; // TODO: why is it saying 0xC004 normally when running nestest.nes ??
        }
        self.status = DEFAULT_STATUS;
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where
        F: FnMut(&mut Cpu),
    {
        // This simulates the wait time for the PPU to start
        // TODO: I really hope this isn't it but could this cause problems for pacman.nes?
        // self.tick(7);

        loop {
            if self.bus.poll_nmi_interrupt() {
                self.interrupt_nmi();
            }

            callback(self);

            let opcode = self.mem_read(self.pc);
            self.pc += 1;

            let (name, cycles, mode) = lookup_opcode(opcode);
            let size = addressing_mode_to_size(&mode);

            let saved_pc = self.pc;
            match name {
                OpName::ADC => self.adc(&mode),
                OpName::AND => self.and(&mode),
                OpName::ASL => self.asl(&mode),
                OpName::BIT => self.bit(&mode),
                OpName::CMP => self.cmp(&mode),
                OpName::CPX => self.cpx(&mode),
                OpName::CPY => self.cpy(&mode),
                OpName::DCP => self.dcp(&mode),
                OpName::DEC => self.dec(&mode),
                OpName::EOR => self.eor(&mode),
                OpName::INC => self.inc(&mode),
                OpName::ISB => self.isb(&mode),
                OpName::LAX => {
                    self.lda(&mode);
                    self.tax();
                }
                OpName::LDA => self.lda(&mode),
                OpName::LDX => self.ldx(&mode),
                OpName::LDY => self.ldy(&mode),
                OpName::LSR => self.lsr(&mode),
                OpName::NOP => self.nop(&mode, cycles.1),
                OpName::ORA => self.ora(&mode),
                OpName::ROL => self.rol(&mode),
                OpName::ROR => self.ror(&mode),
                OpName::SAX => self.sax(&mode),
                OpName::SBC => self.sbc(&mode),
                OpName::SLO => self.slo(&mode),
                OpName::STA => self.sta(&mode),
                OpName::STX => self.stx(&mode),
                OpName::STY => self.sty(&mode),

                OpName::TXS => self.txs(),
                OpName::TSX => self.tsx(),
                OpName::PHA => self.pha(),
                OpName::PLA => self.pla(),
                OpName::PHP => self.php(),
                OpName::PLP => self.plp(),

                OpName::TAX => self.tax(),
                OpName::TXA => self.txa(),
                OpName::DEX => self.dex(),
                OpName::INX => self.inx(),
                OpName::TAY => self.tay(),
                OpName::TYA => self.tya(),
                OpName::DEY => self.dey(),
                OpName::INY => self.iny(),

                OpName::CLC => self.set_flag(Flag::Carry, false),
                OpName::SEC => self.set_flag(Flag::Carry, true),
                OpName::CLI => self.set_flag(Flag::InterruptDisable, false),
                OpName::SEI => self.set_flag(Flag::InterruptDisable, true),
                OpName::CLV => self.set_flag(Flag::Overflow, false),
                OpName::CLD => self.set_flag(Flag::Decimal, false),
                OpName::SED => self.set_flag(Flag::Decimal, true),

                OpName::BPL => self.bpl(),
                OpName::BMI => self.bmi(),
                OpName::BVC => self.bvc(),
                OpName::BVS => self.bvs(),
                OpName::BCC => self.bcc(),
                OpName::BCS => self.bcs(),
                OpName::BNE => self.bne(),
                OpName::BEQ => self.beq(),

                OpName::JSR => self.jsr(&mode),

                OpName::JMP => self.jmp(mode),

                OpName::BRK => {
                    // self.set_flag(Flag::Break, true);
                    return;
                }

                OpName::RTI => self.rti(),

                OpName::RTS => self.rts(),
                OpName::SRE => self.sre(&mode),
                OpName::RLA => self.rla(&mode),
                OpName::RRA => self.rra(&mode),
            }

            self.tick(cycles.0 as usize);

            // some operations modify the pc, like JMP. We shouldn't override that.
            if self.pc == saved_pc {
                self.pc += size - 1;
            }
        }
    }

    fn tick(&mut self, cycles: usize) {
        self.cycles += cycles;
        self.bus.tick(cycles);
    }

    fn jsr(&mut self, mode: &AddressingMode) {
        let jump_dest = self.get_operand_address(mode);
        // +2 for consumed u16 destination
        // -1 because the spec says so
        self.stack_push_u16(self.pc + 2 - 1);
        self.pc = jump_dest;
    }

    fn jmp(&mut self, mode: AddressingMode) {
        let jump_dest = self.get_operand_address(&mode);
        self.pc = jump_dest;
    }

    fn rts(&mut self) {
        let addr = self.stack_pop_u16() + 1;
        self.pc = addr;
    }

    //
    // INSTRUCTIONS
    // Order corresponds to spec here: http://www.6502.org/tutorials/6502opcodes.html
    //

    /// ADC (ADd with Carry)
    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let param = self.mem_read(addr);

        self.adc_helper(param);
    }

    fn adc_helper(&mut self, mem_value: u8) {
        let (mid, overflow) = self.a.overflowing_add(mem_value);

        let c = if self.get_flag(Flag::Carry) { 1 } else { 0 };
        let (result, overflow2) = mid.overflowing_add(c);

        self.set_zero_and_negative_flags(result);
        self.set_flag(Flag::Carry, overflow || overflow2);

        let v = (result ^ self.a) & (result ^ mem_value) & 0x80;
        self.set_flag(Flag::Overflow, v > 0);

        self.a = result;
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
        if mode == &AddressingMode::Accumulator {
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
        // displacement is read as a signed integer (2's complement)
        let displacement = self.mem_read(self.pc) as i8;

        if self.get_flag(flag) == is_set {
            let before = self.pc;
            let after = (self.pc as isize + 1 + displacement as isize) as u16;
            self.pc = after;

            // We set `before+2` so that we only check if we've moved to a DIFFERENT page given the branching scenario.
            let before_plus = before + 2;
            // This behavior is based on nestest.log line 1107
            if is_page_cross(before_plus, after) {
                self.tick(2);
            } else {
                self.tick(1);
            }
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
    /// used to test if one or more bits are set in a target memory location.
    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.set_flag(Flag::Zero, self.a & value == 0);
        self.set_flag(Flag::Negative, (1 << 7) & value > 0);
        self.set_flag(Flag::Overflow, (1 << 6) & value > 0);
    }

    // Comparisons //

    fn compare(&mut self, mode: &AddressingMode, val: u8) {
        let addr = self.get_operand_address(mode);
        let param = self.mem_read(addr);

        let (result, borrow) = val.overflowing_sub(param);

        self.set_flag(Flag::Carry, !borrow);

        self.set_zero_and_negative_flags(result);
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
        let addr = self.get_operand_address2(mode, true);
        let param = self.mem_read(addr);

        self.a = param;
        self.set_zero_and_negative_flags(self.a);
    }

    /// LDX (LoaD X register)
    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address2(mode, true);
        let param = self.mem_read(addr);

        self.x = param;
        self.set_zero_and_negative_flags(self.x);
    }

    /// LDY (LoaD Y register)
    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address2(mode, true);
        let param = self.mem_read(addr);

        self.y = param;
        self.set_zero_and_negative_flags(self.y);
    }

    /// LSR (Logical Shift Right)
    fn lsr(&mut self, mode: &AddressingMode) {
        if mode == &AddressingMode::Accumulator {
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
    fn nop(&mut self, mode: &AddressingMode, tick_on_page_cross: bool) {
        if tick_on_page_cross {
            // Run this for the side-effect of possibly ticking one cycle
            self.get_operand_address2(mode, tick_on_page_cross);
        }
    }

    /// ORA (bitwise OR with Accumulator)
    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let param = self.mem_read(addr);

        self.a |= param;

        self.set_zero_and_negative_flags(self.a);
    }

    /// ROL (ROtate Left)
    fn rol(&mut self, mode: &AddressingMode) {
        if mode == &AddressingMode::Accumulator {
            let old_val = self.a;
            let new_val = (self.a << 1) + self.get_flag(Flag::Carry) as u8;

            self.a = new_val;

            self.set_zero_and_negative_flags(new_val);
            self.set_flag(Flag::Carry, old_val & 0b1000_0000 > 0);
        } else {
            let addr = self.get_operand_address(mode);
            let old_val = self.mem_read(addr);
            let new_val = (old_val << 1) + self.get_flag(Flag::Carry) as u8;

            self.mem_write(addr, new_val);

            self.set_zero_and_negative_flags(new_val);
            self.set_flag(Flag::Carry, old_val & 0b1000_0000 > 0);
        }
    }

    /// ROR (ROtate Right)
    fn ror(&mut self, mode: &AddressingMode) {
        if mode == &AddressingMode::Accumulator {
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
    fn rti(&mut self) {
        self.status = self.stack_pop();
        self.pc = self.stack_pop_u16();
        self.set_flag(Flag::Break, false);
        self.set_flag(Flag::Break2, true);
    }

    /// SBC (SuBtract with Carry)
    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let param = self.mem_read(addr);

        self.adc_helper(255 - param);
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
        // The B flag and extra bit are both pushed as 1.
        self.stack_push(self.status | 0b0011_0000);
        // self.set_flag(Flag::Break, true);
    }

    /// PLP (PuLl Processor status)
    fn plp(&mut self) {
        let val = self.stack_pop();

        let ignore_b = val & 0b1100_1111; // ignore b from stack value
        self.status &= 0b0011_0000; // keep b if it was set before

        self.status |= ignore_b;

        // TODO: Note that the effect of changing I is delayed one instruction
        // because the flag is changed after IRQ is polled, delaying the effect until IRQ is polled in the next instruction like with CLI and SEI.

        // self.set_flag(Flag::Interrupt, true);
        // self.set_flag(Flag::Break, true);
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
            Flag::InterruptDisable => 2,
            Flag::Decimal => 3,
            Flag::Break => 4,
            Flag::Break2 => 5,
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
            Flag::InterruptDisable => 2,
            Flag::Decimal => 3,
            Flag::Break => 4,
            Flag::Break2 => 5,
            Flag::Overflow => 6,
            Flag::Negative => 7,
        };

        let mask = base << shift;
        let val = self.status & mask;
        val != 0
    }

    pub fn tracelite(&mut self) -> String {
        // format!("PC = {:04X}", self.pc)
        let code = self.mem_read(self.pc);
        let op = lookup_opcode(code);

        let cpu_cycles = self.cycles;
        let (ppu_frame, ppu_cycles) = self.bus.get_ppu_tick_status();

        format!(
            "pc={:04X} op={} CPU: {} .. PPU: {},{}",
            self.pc, op.0, cpu_cycles, ppu_frame, ppu_cycles
        )
    }

    fn mem_peek_zero_page_wrapping(&self, ptr: u8) -> (u16, bool) {
        let lo = self.mem_peek(ptr as u16);
        let (hi_ptr, is_page_crossed) = ptr.overflowing_add(1);
        let hi = self.mem_peek(hi_ptr as u16);

        let result = (hi as u16) << 8 | (lo as u16);

        (result, is_page_crossed)
    }

    pub fn trace(&self) -> String {
        // C000  4C F5 C5  JMP $C5F5                       A:00 X:00 Y:00 P:24 SP:FD PPU:  0, 21 CYC:7

        let cpu_cycles = self.cycles;
        let (ppu_frame, ppu_cycles) = self.bus.get_ppu_tick_status();

        let code = self.mem_peek(self.pc);
        let param1 = self.mem_peek(self.pc + 1);
        let param2 = self.mem_peek(self.pc + 2);

        //// Address syntax by mode looks like:

        let op = lookup_opcode(code);
        let (name, _, mode) = op;
        let size = addressing_mode_to_size(&mode);

        let tla = format!("{}{}", if is_official(code) { "" } else { "*" }, name);
        let addr_block = match mode {
            AddressingMode::Immediate => format!("#${:02X}", param1),
            AddressingMode::ZeroPage => {
                format!("${:02X} = {:02X}", param1, self.mem_peek(param1 as u16))
            }
            AddressingMode::ZeroPageX => {
                let offset = param1.wrapping_add(self.x);
                let data = self.mem_peek(offset as u16);
                format!("${:02X},X @ {:02X} = {:02X}", param1, offset, data)
            }
            AddressingMode::ZeroPageY => {
                let offset = param1.wrapping_add(self.y);
                let data = self.mem_peek(offset as u16);
                format!("${:02X},Y @ {:02X} = {:02X}", param1, offset, data)
            }
            AddressingMode::Absolute => {
                let hi = (param2 as u16) << 8;
                let addr: u16 = hi + (param1 as u16);
                match name {
                    OpName::JMP | OpName::JSR => format!("${:02X}{:02X}", param2, param1,),
                    _ => format!(
                        "${:02X}{:02X} = {:02X}",
                        param2,
                        param1,
                        self.mem_peek(addr)
                    ),
                }
            }
            AddressingMode::AbsoluteX => {
                let addr = addr_from(param1, param2).wrapping_add(self.x as u16);
                let data = self.mem_peek(addr);
                format!(
                    "${:02X}{:02X},X @ {:04X} = {:02X}",
                    param2, param1, addr, data
                )
            }
            AddressingMode::AbsoluteY => {
                let addr = addr_from(param1, param2).wrapping_add(self.y as u16);
                let data = self.mem_peek(addr);
                format!(
                    "${:02X}{:02X},Y @ {:04X} = {:02X}",
                    param2, param1, addr, data
                )
            }
            AddressingMode::IndirectX => {
                let indexed = param1.wrapping_add(self.x);
                let (indirect, _) = self.mem_peek_zero_page_wrapping(indexed);
                let data = self.mem_peek(indirect);

                format!(
                    "(${:02X},X) @ {:02X} = {:04X} = {:02X}",
                    param1, indexed, indirect, data
                )
            }
            AddressingMode::IndirectY => {
                let (indirect, _) = self.mem_peek_zero_page_wrapping(param1);
                let indexed = indirect.wrapping_add(self.y as u16);
                let data = self.mem_peek(indexed);
                format!(
                    "(${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                    param1, indirect, indexed, data
                )
            }
            AddressingMode::Relative => {
                format!(
                    "${:04X}",
                    (self.pc as isize + 2 + (param1 as i8) as isize) as u16
                )
            }
            AddressingMode::Accumulator => "A".to_string(),
            AddressingMode::Indirect => {
                // Copied from get_operand_address.
                // TODO: Figure out how to share things better here, given pc is off by 1
                let indirect = {
                    let base = self.mem_peek_u16(self.pc + 1);

                    let is_edge_of_page = base & 0x00ff == 0xff;

                    if is_edge_of_page {
                        let first = self.mem_peek(base);
                        // Intentionally wrap incorrectly, to recreate buggy behavior from original 6502
                        let second = self.mem_peek(base & 0xff00);
                        ((second as u16) << 8) + first as u16
                    } else {
                        self.mem_peek_u16(base)
                    }
                };

                format!("(${:02X}{:02X}) = {:04X}", param2, param1, indirect)
            }
            _ => "".to_string(),
        };

        format!(
            "{:04X}  {:02X} {} {} {:>4} {:<28}A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} PPU:{:>3},{:>3} CYC:{}",
            self.pc,
            code,
            if size >= 2 {
                format!("{:02X}", param1)
            } else {
                "  ".to_string()
            },
            if size >= 3 {
                format!("{:02X}", param2)
            } else {
                "  ".to_string()
            },
            tla, // TODO: opname sometimes has * prefix like *NOP
            addr_block,
            self.a,
            self.x,
            self.y,
            self.status,
            self.sp,
            ppu_frame,
            ppu_cycles % 341,
            cpu_cycles,
        )
        .to_string()
    }

    fn sax(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let val = self.x & self.a;

        self.mem_write(addr, val);
    }

    fn dcp(&mut self, mode: &AddressingMode) {
        self.dec(mode);
        self.cmp(mode);
    }

    fn isb(&mut self, mode: &AddressingMode) {
        self.inc(mode);
        self.sbc(mode);
    }

    fn rla(&mut self, mode: &AddressingMode) {
        self.rol(mode);
        self.and(mode);
    }

    fn rra(&mut self, mode: &AddressingMode) {
        self.ror(mode);
        self.adc(mode);
    }

    fn slo(&mut self, mode: &AddressingMode) {
        self.asl(mode);
        self.ora(mode);
    }

    fn sre(&mut self, mode: &AddressingMode) {
        self.lsr(mode);
        self.eor(mode);
    }

    fn interrupt_nmi(&mut self) {
        self.stack_push_u16(self.pc);
        // TODO: Might need to flip bits on the Break1 and Break2 flags
        self.stack_push(self.status);

        self.set_flag(Flag::InterruptDisable, true);

        self.bus.tick(2);

        self.pc = self.mem_read_u16(0xFFFA);
    }
}

#[cfg(test)]
mod tests {
    use std::{fs, io::BufRead, sync::Mutex};

    use crate::{assert_eq_bits, assert_eq_hex, bus::PRG_ROM_START};

    use super::*;

    #[test]
    fn test_0xa0_lda_immediate_nonzero() {
        let mut cpu = Cpu::new();
        cpu._load_test_rom(vec![0xa9, 0x55, 0x00]);
        cpu.reset();
        cpu._run();
        assert_eq!(cpu.a, 0x55);
        assert_eq!(cpu.status, 0b0010_0100);
    }

    #[test]
    fn test_0xa9_lda_immediate_load_zero() {
        let mut cpu = Cpu::new();
        cpu._load_and_run(vec![0xa9, 0x00, 0x00]);
        assert_eq!(cpu.a, 0x00);
        assert_eq!(cpu.status, 0b0010_0110);
    }

    #[test]
    fn test_0xa5_lda_from_memory() {
        let mut cpu = Cpu::new();
        cpu._load_test_rom(vec![0xa5, 0x10, 0x00]);
        cpu.reset();
        cpu.mem_write(0x10, 0x55);
        cpu._run();

        assert_eq!(cpu.a, 0x55);
    }

    #[test]
    fn test_0xad_lda_absolute() {
        let mut cpu = Cpu::new();
        cpu._load_test_rom(vec![0xad, 0x03, 0x10, 0x00]);
        cpu.reset();
        cpu.mem_write(0x1003, 0x99);
        cpu._run();

        assert_eq!(cpu.a, 0x99);
    }

    #[test]
    fn test_sta_zero_page() {
        let mut cpu = Cpu::new();
        let sta = 0x85;
        cpu._load_test_rom(vec![sta, 0x10, 0x00]);
        cpu.reset();
        cpu.a = 123;
        cpu._run();

        assert_eq!(cpu.mem_read(0x10), 123);
    }

    #[test]
    fn test_stx_zero_page() {
        let mut cpu = Cpu::new();
        let stx = 0x86;
        cpu._load_test_rom(vec![stx, 0x10, 0x00]);
        cpu.reset();
        cpu.x = 123;
        cpu._run();

        assert_eq!(cpu.mem_read(0x10), 123);
    }

    #[test]
    fn test_sty_zero_page() {
        let mut cpu = Cpu::new();
        let sty = 0x84;
        cpu._load_test_rom(vec![sty, 0x10, 0x00]);
        cpu.reset();
        cpu.y = 123;
        cpu._run();

        assert_eq!(cpu.mem_read(0x10), 123);
    }

    #[test]
    fn test_sta_zero_page_x() {
        let mut cpu = Cpu::new();
        cpu._load_test_rom(vec![0x95, 0x10, 0x00]);
        cpu.reset();
        cpu.x = 1;
        cpu.mem_write(0x11, 0x20);
        cpu.a = 123;
        cpu._run();

        assert_eq!(cpu.mem_read(0x11), 123);
    }

    #[test]
    fn test_addressing_modes() {
        let pc = 0x11;
        for (mode, expected) in [
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
            let mut cpu = Cpu::new();
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
        let mut cpu = Cpu::new();
        cpu._load_test_rom(vec![0xaa, 0x00]);
        cpu.reset();
        cpu.a = 123;
        cpu._run();
        assert_eq!(cpu.x, 123);
        assert_eq!(cpu.status, 0b0010_0100);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x_sets_zero_flag() {
        let mut cpu = Cpu::new();
        cpu._load_test_rom(vec![0xaa, 0x00]);
        cpu.reset();
        cpu.x = 0xff;
        cpu.a = 0;
        cpu._run();
        assert_eq!(cpu.x, 0);
        assert_eq!(cpu.status, 0b0010_0110);
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = Cpu::new();
        cpu._load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = Cpu::new();
        cpu._load_test_rom(vec![0xe8, 0xe8, 0x00]);
        cpu.reset();
        cpu.x = 0xff;
        cpu._run();

        assert_eq!(cpu.x, 1)
    }

    #[test]
    fn test_jsr() {
        let mut cpu = Cpu::new();
        let jump_dest: u16 = (PRG_ROM_START + CPU_START as u16) + 333;
        let lo = (jump_dest & 0xff) as u8;
        let hi = (jump_dest >> 8) as u8;
        cpu._load_and_run(vec![0x20, lo, hi]);

        // expect that you jump to jump_dest, then pc steps forward one more time while reading a BRK
        // (since everything is 0x00 BRK by default)
        assert_eq!(cpu.pc, jump_dest + 1);
    }

    #[test]
    fn test_jmp_indirect() {
        let mut cpu = Cpu::new();
        let jump_dest: u16 = 0;
        let mem_with_dest = 0x02A4;
        let lo = (mem_with_dest & 0xff) as u8;
        let hi = (mem_with_dest >> 8) as u8;
        let inst = 0x6C;
        cpu._load_test_rom(vec![inst, lo, hi]);
        cpu.reset();
        cpu.mem_write_u16(mem_with_dest, jump_dest);
        cpu._run();

        // expect that you jump to jump_dest, then pc steps forward one more time while reading a BRK
        // (since everything is 0x00 BRK by default)
        assert_eq!(cpu.pc, jump_dest + 1);
    }

    #[test]
    /// Check that we handle this behavior edge case:
    /// "INDIRECT JUMP MUST NEVER USE A VECTOR BEGINNING ON THE LAST BYTE OF A PAGE"
    /// See also: https://www.reddit.com/r/EmuDev/comments/fi29ah/6502_jump_indirect_error/
    fn test_jmp_indirect_end_of_page_edge_case() {
        let mut cpu = Cpu::new();

        let mem_with_dest = 0x03FF;
        let lo = (mem_with_dest & 0xff) as u8;
        let hi = (mem_with_dest >> 8) as u8;
        let inst = 0x6C;
        cpu._load_test_rom(vec![inst, lo, hi]);
        cpu.reset();
        cpu.mem_write(0x0300, 0x40);
        cpu.mem_write(0x03FF, 0x80);
        cpu.mem_write(0x0400, 0x50);
        cpu._run();

        // expect that you jump to jump_dest with the expected buggy wrapping behavior
        // then cpu.pc steps forward one more time while reading a BRK
        assert_eq_hex!(cpu.pc, 0x4080 + 1);
    }

    #[test]
    fn test_rts() {
        let mut cpu = Cpu::new();

        let jmp_opcode = 0x20;
        let rts_opcode = 0x60;

        let jump_dest: u16 = (CPU_START as u16) + 123;
        let lo = (jump_dest & 0xff) as u8;
        let hi = (jump_dest >> 8) as u8;
        cpu._load_test_rom(vec![jmp_opcode, lo, hi]);
        cpu.reset();
        cpu.mem_write(jump_dest, rts_opcode);

        cpu._run();
        // +4 =
        // [0    1     2       3  ]
        // [jmp, addr, addr+1, brk] .. and +1 as last pc+1 after brek
        assert_eq!(cpu.pc, (PRG_ROM_START + CPU_START as u16) + 4);
    }

    #[test]
    fn test_0x09_ora_immediate() {
        let mut cpu = Cpu::new();
        cpu._load_test_rom(vec![0x09, 0x0f, 0x00]);
        cpu.reset();
        cpu.a = 0xf0;
        cpu._run();
        assert_eq!(cpu.a, 0xff);
        assert!(cpu.get_flag(Flag::Negative));
    }

    #[test]
    fn test_0x29_and_immediate() {
        let mut cpu = Cpu::new();
        cpu._load_test_rom(vec![0x29, 0x0f, 0x00]);
        cpu.reset();
        cpu.a = 0xf0;
        cpu._run();
        assert_eq!(cpu.a, 0x00);
        assert!(cpu.get_flag(Flag::Zero));
    }

    #[test]
    fn test_flag_processor_status_instructions_set_flags() {
        let mut cpu = Cpu::new();
        cpu._load_test_rom(vec![0x38, 0x78, 0xF8]);
        cpu.reset();
        cpu.status = 0x00;
        cpu._run();
        assert_eq_bits!(cpu.status, 0b0000_1101);
    }

    #[test]
    fn test_flag_processor_status_instructions_clear_flags() {
        let mut cpu = Cpu::new();
        cpu._load_test_rom(vec![0x18, 0x58, 0xB8, 0xD8]);
        cpu.reset();
        cpu.status = 0xff;
        cpu._run();
        assert_eq!(cpu.status, 0b1011_0010);
    }

    #[test]
    fn test_0xee_inc_absolute() {
        let mut cpu = Cpu::new();

        let inst = 0xee;

        let target_val = 3;
        let target_address = 0x12; // needs to be in CPU

        let hi = (target_address >> 8) as u8;
        let lo = (target_address & 0x00ff) as u8;
        cpu._load_test_rom(vec![inst, lo, hi, 0x00]);
        cpu.mem_write(target_address, target_val);
        cpu.reset();
        cpu._run();
        assert_eq!(cpu.mem_read(target_address), 4);
    }

    #[test]
    fn test_0x69_adc_immediate() {
        let mut cpu = Cpu::new();
        cpu._load_and_run(vec![0x69, 123]);
        assert_eq!(cpu.a, 123);
        assert_eq!(cpu.status, 0b0010_0100);
    }

    #[test]
    fn test_0x69_adc_immediate_sets_carry_flag() {
        let mut cpu = Cpu::new();
        cpu._load_test_rom(vec![0x69, 1]);
        cpu.reset();
        cpu.a = 255;
        cpu._run();
        assert_eq!(cpu.a, 0);
        assert!(cpu.get_flag(Flag::Zero));
        assert!(cpu.get_flag(Flag::Carry));
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
            let mut cpu = Cpu::new();
            let displacement = 5;
            let program = vec![branch_ins, displacement];
            cpu._load_test_rom(program.clone());
            cpu.reset();
            cpu.set_flag(flag, branch_if);
            cpu._run();
            let consumed_bpl_op = 2;
            let consumed_brk_op = 1;
            assert_eq!(
                cpu.pc,
                PRG_ROM_START
                    + CPU_START as u16
                    + consumed_bpl_op
                    + displacement as u16
                    + consumed_brk_op
            );

            let mut cpu = Cpu::new();
            cpu._load_test_rom(program);
            cpu.reset();
            cpu.set_flag(flag, !branch_if);
            cpu._run();
            assert_eq!(
                cpu.pc,
                PRG_ROM_START + CPU_START as u16 + consumed_bpl_op + consumed_brk_op
            );
        }
    }

    #[test]
    fn test_ldx() {
        let mut cpu = Cpu::new();
        cpu._load_and_run(vec![0xa2, 123]);
        assert_eq!(cpu.x, 123);
    }

    #[test]
    fn test_ldy() {
        let mut cpu = Cpu::new();
        cpu._load_and_run(vec![0xa0, 123]);
        assert_eq!(cpu.y, 123);
    }

    #[test]
    fn test_0x4a_lsr_shifts_accumulator() {
        let mut cpu = Cpu::new();
        cpu._load_test_rom(vec![0x4a]);
        cpu.reset();
        cpu.a = 0b1000_1001;
        cpu._run();
        assert_eq!(cpu.a, 0b0100_0100);
        assert!(cpu.get_flag(Flag::Carry));
    }

    #[test]
    fn test_0x4e_lsr_shifts_absolute() {
        let mut cpu = Cpu::new();

        let target_val = 0b1000_1001;
        let target_address = 0x12; // needs to be in CPU

        let hi = (target_address >> 8) as u8;
        let lo = (target_address & 0x00ff) as u8;
        cpu._load_test_rom(vec![0x4e, lo, hi, 0x00]);
        cpu.mem_write(target_address, target_val);
        cpu.reset();
        cpu._run();
        assert_eq!(cpu.mem_read(target_address), 0b0100_0100);
        assert!(cpu.get_flag(Flag::Carry));
    }

    #[test]
    fn test_0xea_nop() {
        let mut cpu = Cpu::new();
        let program = vec![0xea, 0xea, 0xea, 0xea, 0x00];
        cpu._load_and_run(program.clone());
        let end = PRG_ROM_START + (CPU_START + program.len()) as u16;
        assert_eq!(cpu.pc, end);
    }

    #[test]
    fn test_register_instructions_for_x() {
        let mut cpu = Cpu::new();
        let tax = 0xaa;
        let txa = 0x8a;
        let dex = 0xca;
        let inx = 0xe8;
        cpu._load_and_run(vec![dex, dex, inx, inx, inx, txa, tax]);
        assert_eq!(cpu.x, 1);
        assert_eq!(cpu.a, 1);
    }

    #[test]
    fn test_register_instructions_for_y() {
        let tay = 0xa8;
        let tya = 0x98;
        let dey = 0x88;
        let iny = 0xc8;

        let mut cpu = Cpu::new();
        cpu._load_and_run(vec![dey, dey, iny, iny, iny, tya, tay]);
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
            let mut cpu = Cpu::new();
            cpu._load_and_run(vec![instruction, 0x00]);
            assert_eq_bits!(cpu.status, 0b0010_0111);

            // greater than
            let mut cpu = Cpu::new();
            cpu._load_and_run(vec![instruction, 0x01]);
            assert_eq_bits!(cpu.status, 0b1010_0100);

            // less than
            let mut cpu = Cpu::new();
            cpu._load_test_rom(vec![instruction, 0x01]);
            cpu.reset();
            match register {
                Register::A => cpu.a = 0x02,
                Register::X => cpu.x = 0x02,
                Register::Y => cpu.y = 0x02,
            }
            cpu._run();
            assert_eq_bits!(cpu.status, 0b0010_0101);
        }
    }

    #[test]
    fn test_0xce_dec_absolute() {
        let mut cpu = Cpu::new();

        let inst = 0xce;

        let target_val = 3;
        let target_address = 0x12; // needs to be in CPU

        let hi = (target_address >> 8) as u8;
        let lo = (target_address & 0x00ff) as u8;
        cpu._load_test_rom(vec![inst, lo, hi, 0x00]);
        cpu.mem_write(target_address, target_val);
        cpu.reset();
        cpu._run();
        assert_eq!(cpu.mem_read(target_address), 2);
    }

    #[test]
    fn test_0x49_eor_immediate() {
        let mut cpu = Cpu::new();
        let eor = 0x49;
        cpu._load_test_rom(vec![eor, 0b1111_0000, 0x00]);
        cpu.reset();
        cpu.a = 0b0011_1100;
        cpu._run();
        assert_eq!(cpu.a, 0b1100_1100);
        assert_eq_bits!(cpu.status, 0b1010_0100);
    }

    #[test]
    fn test_0x0a_asl_shifts_accumulator() {
        let mut cpu = Cpu::new();
        cpu._load_test_rom(vec![0x0a]);
        cpu.reset();
        cpu.a = 0b1000_1001;
        cpu._run();
        assert_eq!(cpu.a, 0b0001_0010);
        assert!(cpu.get_flag(Flag::Carry));
    }

    #[test]
    fn test_0x0e_asl_shifts_absolute() {
        let mut cpu = Cpu::new();

        let to_lsr = 0b1000_1001;
        let to_lsr_addr = 0x12; // needs to be in CPU

        let hi = (to_lsr_addr >> 8) as u8;
        let lo = (to_lsr_addr & 0x00ff) as u8;
        cpu._load_test_rom(vec![0x0e, lo, hi, 0x00]);
        cpu.mem_write(to_lsr_addr, to_lsr);
        cpu.reset();
        cpu._run();
        assert_eq!(cpu.mem_read(to_lsr_addr), 0b0001_0010);
        assert!(cpu.get_flag(Flag::Carry));
    }

    #[test]
    fn test_rol() {
        let rol_accum = 0x2A;

        let mut cpu = Cpu::new();
        cpu._load_test_rom(vec![rol_accum]);
        cpu.reset();

        cpu.set_flag(Flag::Carry, true);
        cpu.a = 0b1000_1000;
        cpu._run();

        assert_eq!(cpu.a, 0b0001_0001);
        assert!(cpu.get_flag(Flag::Carry));
    }

    #[test]
    fn test_ror() {
        let ror_accum = 0x6A;

        for (before_a, before_carry, after_a, after_carry) in [
            (0b0001_0001, true, 0b1000_1000, true),
            (0b0001_0000, true, 0b1000_1000, false),
        ] {
            let mut cpu = Cpu::new();
            cpu._load_test_rom(vec![ror_accum]);
            cpu.reset();

            cpu.a = before_a;
            cpu.set_flag(Flag::Carry, before_carry);
            cpu._run();

            assert_eq!(cpu.a, after_a);
            assert_eq!(cpu.get_flag(Flag::Carry), after_carry);
        }
    }

    #[test]
    fn test_0x40_rti() {
        let mut cpu = Cpu::new();
        cpu._load_test_rom(vec![0x40]);
        cpu.reset();

        let pc = 0x06FF;
        let status: u8 = 0b1010_1010;
        cpu.stack_push_u16(pc);
        cpu.stack_push(status);

        cpu._run();
        assert_eq!(cpu.pc, pc + 1);
        assert_eq!(cpu.status, status);
    }

    #[test]
    fn test_bit() {
        let mut cpu = Cpu::new();
        let bit_absolute = 0x2C;
        cpu._load_test_rom(vec![bit_absolute, 0x22, 0x11, 0x00]);
        cpu.reset();
        cpu.mem_write(0x1122, 0b1100_0000);
        cpu.a = 0b0000_1111;
        cpu._run();

        assert!(cpu.get_flag(Flag::Zero));
        assert!(cpu.get_flag(Flag::Negative));
        assert!(cpu.get_flag(Flag::Overflow));

        let mut cpu = Cpu::new();
        let bit_zeropage = 0x24;
        cpu._load_test_rom(vec![bit_zeropage, 0x01]);
        cpu.reset();
        cpu.mem_write(0x01, 0x40);
        cpu.status = 0x6D;
        cpu.a = 0x40;
        cpu._run();

        assert_eq_bits!(cpu.status, 0x6D);
    }

    #[test]
    fn test_format_trace() {
        let rom = Rom::new_test_rom(vec![]);

        let mut bus = Bus::new(rom);

        bus.mem_write(100, 0xa2);
        bus.mem_write(101, 0x01);
        bus.mem_write(102, 0xca);
        bus.mem_write(103, 0x88);
        bus.mem_write(104, 0x00);

        let mut cpu = Cpu::new();
        cpu.set_bus(bus);
        cpu.pc = 0x64;
        cpu.a = 1;
        cpu.x = 2;
        cpu.y = 3;
        let mut result: Vec<String> = vec![];
        cpu.run_with_callback(|cpu| {
            result.push(cpu.trace());
        });
        assert_eq!(
            "0064  A2 01     LDX #$01                        A:01 X:02 Y:03 P:24 SP:FD PPU:  0,  0 CYC:0",
            result[0]
        );
        assert_eq!(
            "0066  CA        DEX                             A:01 X:01 Y:03 P:24 SP:FD PPU:  0,  6 CYC:2",
            result[1]
        );
        assert_eq!(
            "0067  88        DEY                             A:01 X:00 Y:03 P:26 SP:FD PPU:  0, 12 CYC:4",
            result[2]
        );
    }

    #[test]
    fn test_format_mem_access() {
        let rom = Rom::new_test_rom(vec![]);
        let mut bus = Bus::new(rom);
        // ORA ($33), Y
        bus.mem_write(100, 0x11);
        bus.mem_write(101, 0x33);

        //data
        bus.mem_write(0x33, 0);
        bus.mem_write(0x34, 4);

        //target cell
        bus.mem_write(0x400, 0xAA);

        let mut cpu = Cpu::new();
        cpu.set_bus(bus);
        cpu.pc = 0x64;
        cpu.y = 0;
        let mut result: Vec<String> = vec![];
        cpu.run_with_callback(|cpu| {
            result.push(cpu.trace());
        });
        assert_eq!(
            "0064  11 33     ORA ($33),Y = 0400 @ 0400 = AA  A:00 X:00 Y:00 P:24 SP:FD PPU:  0,  0 CYC:0",
            result[0]
        );
    }

    #[test]
    fn test_nestest() {
        let max_known_good_line = 8980;
        // let max_known_good_line = 8981;

        let program = fs::read("roms/nestest.nes").unwrap();

        let mut cpu = Cpu::new();
        let rom = Rom::new(&program);
        cpu.set_bus(Bus::new(rom));
        cpu.reset();

        cpu.tick(7);

        cpu.pc = 0xC000; // TODO: fix this hacky workaround for nestest

        // execute it until crash, collecting trace data
        let mutex_cpu = Mutex::new(cpu);
        let result: Vec<String> = vec![];
        let mutex_result = Mutex::new(result);
        let _ = std::panic::catch_unwind(|| {
            mutex_cpu.lock().unwrap().run_with_callback(|cpu| {
                mutex_result.lock().unwrap().push(cpu.trace());
            })
        });

        let expected = fs::read("nestest.log").unwrap();

        let actual = match mutex_result.lock() {
            Ok(m) => m,
            Err(e) => e.into_inner(),
        };

        for (idx, e) in expected.lines().enumerate() {
            let line_num = idx + 1;
            if line_num > max_known_good_line {
                break;
            }

            let expected_line = e.unwrap();

            let current = if actual.len() > idx { &actual[idx] } else { "" };

            assert_eq!(
                current,
                expected_line,
                "First diff found on line = {:?}. In context the output was: \n\n'''\n{}\n{}\n'''\n",
                line_num,
                if idx == 0 { "n/a" } else { &actual[idx - 1] },
                current,
            );
        }
    }
}
