use core::fmt;

use crate::core::AddressingMode;

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, PartialEq, Eq)]
pub enum OpName {
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTI,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
    LAX,
    SAX,
    DCP,
    ISB,
    SLO,
    SRE,
    RLA,
    RRA,
}

impl fmt::Display for OpName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// Cycles (u8: number of cycles, bool: +1 if page crossed)
#[derive(PartialEq, Eq)]
pub struct Cycles(pub u8, pub bool);

pub type Op = (OpName, Cycles, AddressingMode);

pub fn lookup_opcode(code: u8) -> Op {
    match code {
        0x69 => (OpName::ADC, Cycles(2, false), AddressingMode::Immediate),
        0x65 => (OpName::ADC, Cycles(3, false), AddressingMode::ZeroPage),
        0x75 => (OpName::ADC, Cycles(4, false), AddressingMode::ZeroPageX),
        0x6D => (OpName::ADC, Cycles(4, false), AddressingMode::Absolute),
        0x7D => (OpName::ADC, Cycles(4, true), AddressingMode::AbsoluteX),
        0x79 => (OpName::ADC, Cycles(4, true), AddressingMode::AbsoluteY),
        0x61 => (OpName::ADC, Cycles(6, false), AddressingMode::IndirectX),
        0x71 => (OpName::ADC, Cycles(5, true), AddressingMode::IndirectY),

        0x29 => (OpName::AND, Cycles(2, false), AddressingMode::Immediate),
        0x25 => (OpName::AND, Cycles(3, false), AddressingMode::ZeroPage),
        0x35 => (OpName::AND, Cycles(4, false), AddressingMode::ZeroPageX),
        0x2D => (OpName::AND, Cycles(4, false), AddressingMode::Absolute),
        0x3D => (OpName::AND, Cycles(4, true), AddressingMode::AbsoluteX),
        0x39 => (OpName::AND, Cycles(4, true), AddressingMode::AbsoluteY),
        0x21 => (OpName::AND, Cycles(6, false), AddressingMode::IndirectX),
        0x31 => (OpName::AND, Cycles(5, true), AddressingMode::IndirectY),

        0x0A => (OpName::ASL, Cycles(2, false), AddressingMode::Accumulator),
        0x06 => (OpName::ASL, Cycles(5, false), AddressingMode::ZeroPage),
        0x16 => (OpName::ASL, Cycles(6, false), AddressingMode::ZeroPageX),
        0x0E => (OpName::ASL, Cycles(6, false), AddressingMode::Absolute),
        0x1E => (OpName::ASL, Cycles(7, false), AddressingMode::AbsoluteX),

        0x24 => (OpName::BIT, Cycles(3, false), AddressingMode::ZeroPage),
        0x2C => (OpName::BIT, Cycles(4, false), AddressingMode::Absolute),

        0xC9 => (OpName::CMP, Cycles(2, false), AddressingMode::Immediate),
        0xC5 => (OpName::CMP, Cycles(3, false), AddressingMode::ZeroPage),
        0xD5 => (OpName::CMP, Cycles(4, false), AddressingMode::ZeroPageX),
        0xCD => (OpName::CMP, Cycles(4, false), AddressingMode::Absolute),
        0xDD => (OpName::CMP, Cycles(4, true), AddressingMode::AbsoluteX),
        0xD9 => (OpName::CMP, Cycles(4, true), AddressingMode::AbsoluteY),
        0xC1 => (OpName::CMP, Cycles(6, false), AddressingMode::IndirectX),
        0xD1 => (OpName::CMP, Cycles(5, true), AddressingMode::IndirectY),

        0xE0 => (OpName::CPX, Cycles(2, false), AddressingMode::Immediate),
        0xE4 => (OpName::CPX, Cycles(3, false), AddressingMode::ZeroPage),
        0xEC => (OpName::CPX, Cycles(4, false), AddressingMode::Absolute),

        0xC0 => (OpName::CPY, Cycles(2, false), AddressingMode::Immediate),
        0xC4 => (OpName::CPY, Cycles(3, false), AddressingMode::ZeroPage),
        0xCC => (OpName::CPY, Cycles(4, false), AddressingMode::Absolute),

        0xC6 => (OpName::DEC, Cycles(5, false), AddressingMode::ZeroPage),
        0xD6 => (OpName::DEC, Cycles(6, false), AddressingMode::ZeroPageX),
        0xCE => (OpName::DEC, Cycles(6, false), AddressingMode::Absolute),
        0xDE => (OpName::DEC, Cycles(7, false), AddressingMode::AbsoluteX),

        0x49 => (OpName::EOR, Cycles(2, false), AddressingMode::Immediate),
        0x45 => (OpName::EOR, Cycles(3, false), AddressingMode::ZeroPage),
        0x55 => (OpName::EOR, Cycles(4, false), AddressingMode::ZeroPageX),
        0x4D => (OpName::EOR, Cycles(4, false), AddressingMode::Absolute),
        0x5D => (OpName::EOR, Cycles(4, true), AddressingMode::AbsoluteX),
        0x59 => (OpName::EOR, Cycles(4, true), AddressingMode::AbsoluteY),
        0x41 => (OpName::EOR, Cycles(6, false), AddressingMode::IndirectX),
        0x51 => (OpName::EOR, Cycles(5, true), AddressingMode::IndirectY),

        0xE6 => (OpName::INC, Cycles(5, false), AddressingMode::ZeroPage),
        0xF6 => (OpName::INC, Cycles(6, false), AddressingMode::ZeroPageX),
        0xEE => (OpName::INC, Cycles(6, false), AddressingMode::Absolute),
        0xFE => (OpName::INC, Cycles(7, false), AddressingMode::AbsoluteX),

        0xA7 => (OpName::LAX, Cycles(3, false), AddressingMode::ZeroPage),
        0xB7 => (OpName::LAX, Cycles(4, false), AddressingMode::ZeroPageY),
        0xAF => (OpName::LAX, Cycles(4, false), AddressingMode::Absolute),
        0xBF => (OpName::LAX, Cycles(4, true), AddressingMode::AbsoluteY),
        0xA3 => (OpName::LAX, Cycles(6, false), AddressingMode::IndirectX),
        0xB3 => (OpName::LAX, Cycles(5, true), AddressingMode::IndirectY),

        0xA9 => (OpName::LDA, Cycles(2, false), AddressingMode::Immediate),
        0xA5 => (OpName::LDA, Cycles(3, false), AddressingMode::ZeroPage),
        0xB5 => (OpName::LDA, Cycles(4, false), AddressingMode::ZeroPageX),
        0xAD => (OpName::LDA, Cycles(4, false), AddressingMode::Absolute),
        0xBD => (OpName::LDA, Cycles(4, true), AddressingMode::AbsoluteX),
        0xB9 => (OpName::LDA, Cycles(4, true), AddressingMode::AbsoluteY),
        0xA1 => (OpName::LDA, Cycles(6, false), AddressingMode::IndirectX),
        0xB1 => (OpName::LDA, Cycles(5, true), AddressingMode::IndirectY),

        0xA2 => (OpName::LDX, Cycles(2, false), AddressingMode::Immediate),
        0xA6 => (OpName::LDX, Cycles(3, false), AddressingMode::ZeroPage),
        0xB6 => (OpName::LDX, Cycles(4, false), AddressingMode::ZeroPageY),
        0xAE => (OpName::LDX, Cycles(4, false), AddressingMode::Absolute),
        0xBE => (OpName::LDX, Cycles(4, true), AddressingMode::AbsoluteY),

        0xA0 => (OpName::LDY, Cycles(2, false), AddressingMode::Immediate),
        0xA4 => (OpName::LDY, Cycles(3, false), AddressingMode::ZeroPage),
        0xB4 => (OpName::LDY, Cycles(4, false), AddressingMode::ZeroPageX),
        0xAC => (OpName::LDY, Cycles(4, false), AddressingMode::Absolute),
        0xBC => (OpName::LDY, Cycles(4, true), AddressingMode::AbsoluteX),

        0x4A => (OpName::LSR, Cycles(2, false), AddressingMode::Accumulator),
        0x46 => (OpName::LSR, Cycles(5, false), AddressingMode::ZeroPage),
        0x56 => (OpName::LSR, Cycles(6, false), AddressingMode::ZeroPageX),
        0x4E => (OpName::LSR, Cycles(6, false), AddressingMode::Absolute),
        0x5E => (OpName::LSR, Cycles(7, false), AddressingMode::AbsoluteX),

        0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xEA | 0xFA => {
            (OpName::NOP, Cycles(2, false), AddressingMode::None)
        }
        0x04 | 0x44 | 0x64 => (OpName::NOP, Cycles(3, false), AddressingMode::ZeroPage),
        0x14 | 0x34 | 0x54 | 0x74 | 0xD4 | 0xF4 => {
            (OpName::NOP, Cycles(4, false), AddressingMode::ZeroPageX)
        }
        0x80 | 0x82 | 0x89 | 0xC2 | 0xE2 => {
            (OpName::NOP, Cycles(2, false), AddressingMode::Immediate)
        }
        0x0C => (OpName::NOP, Cycles(4, false), AddressingMode::Absolute),
        0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => {
            (OpName::NOP, Cycles(4, true), AddressingMode::AbsoluteX)
        }

        0x09 => (OpName::ORA, Cycles(2, false), AddressingMode::Immediate),
        0x05 => (OpName::ORA, Cycles(3, false), AddressingMode::ZeroPage),
        0x15 => (OpName::ORA, Cycles(4, false), AddressingMode::ZeroPageX),
        0x0D => (OpName::ORA, Cycles(4, false), AddressingMode::Absolute),
        0x1D => (OpName::ORA, Cycles(4, true), AddressingMode::AbsoluteX),
        0x19 => (OpName::ORA, Cycles(4, true), AddressingMode::AbsoluteY),
        0x01 => (OpName::ORA, Cycles(6, false), AddressingMode::IndirectX),
        0x11 => (OpName::ORA, Cycles(5, true), AddressingMode::IndirectY),

        0x2A => (OpName::ROL, Cycles(2, false), AddressingMode::Accumulator),
        0x26 => (OpName::ROL, Cycles(5, false), AddressingMode::ZeroPage),
        0x36 => (OpName::ROL, Cycles(6, false), AddressingMode::ZeroPageX),
        0x2E => (OpName::ROL, Cycles(6, false), AddressingMode::Absolute),
        0x3E => (OpName::ROL, Cycles(7, false), AddressingMode::AbsoluteX),

        0x6A => (OpName::ROR, Cycles(2, false), AddressingMode::Accumulator),
        0x66 => (OpName::ROR, Cycles(5, false), AddressingMode::ZeroPage),
        0x76 => (OpName::ROR, Cycles(6, false), AddressingMode::ZeroPageX),
        0x6E => (OpName::ROR, Cycles(6, false), AddressingMode::Absolute),
        0x7E => (OpName::ROR, Cycles(7, false), AddressingMode::AbsoluteX),

        0xEB => (OpName::SBC, Cycles(2, false), AddressingMode::Immediate), // TODO: this is marked as an "illegal opcode" (https://www.oxyron.de/html/opcodes02.html)
        0xE9 => (OpName::SBC, Cycles(2, false), AddressingMode::Immediate),
        0xE5 => (OpName::SBC, Cycles(3, false), AddressingMode::ZeroPage),
        0xF5 => (OpName::SBC, Cycles(4, false), AddressingMode::ZeroPageX),
        0xED => (OpName::SBC, Cycles(4, false), AddressingMode::Absolute),
        0xFD => (OpName::SBC, Cycles(4, true), AddressingMode::AbsoluteX),
        0xF9 => (OpName::SBC, Cycles(4, true), AddressingMode::AbsoluteY),
        0xE1 => (OpName::SBC, Cycles(6, false), AddressingMode::IndirectX),
        0xF1 => (OpName::SBC, Cycles(5, true), AddressingMode::IndirectY),

        0x85 => (OpName::STA, Cycles(3, false), AddressingMode::ZeroPage),
        0x95 => (OpName::STA, Cycles(4, false), AddressingMode::ZeroPageX),
        0x8D => (OpName::STA, Cycles(4, false), AddressingMode::Absolute),
        0x9D => (OpName::STA, Cycles(5, false), AddressingMode::AbsoluteX),
        0x99 => (OpName::STA, Cycles(5, false), AddressingMode::AbsoluteY),
        0x81 => (OpName::STA, Cycles(6, false), AddressingMode::IndirectX),
        0x91 => (OpName::STA, Cycles(6, false), AddressingMode::IndirectY),

        0x86 => (OpName::STX, Cycles(3, false), AddressingMode::ZeroPage),
        0x96 => (OpName::STX, Cycles(4, false), AddressingMode::ZeroPageY),
        0x8E => (OpName::STX, Cycles(4, false), AddressingMode::Absolute),

        0x84 => (OpName::STY, Cycles(3, false), AddressingMode::ZeroPage),
        0x94 => (OpName::STY, Cycles(4, false), AddressingMode::ZeroPageX),
        0x8C => (OpName::STY, Cycles(4, false), AddressingMode::Absolute),

        // Stack Instructions
        0x9A => (OpName::TXS, Cycles(2, false), AddressingMode::None),
        0xBA => (OpName::TSX, Cycles(2, false), AddressingMode::None),
        0x48 => (OpName::PHA, Cycles(3, false), AddressingMode::None),
        0x68 => (OpName::PLA, Cycles(4, false), AddressingMode::None),
        0x08 => (OpName::PHP, Cycles(3, false), AddressingMode::None),
        0x28 => (OpName::PLP, Cycles(4, false), AddressingMode::None),

        // Register Instructions
        0xAA => (OpName::TAX, Cycles(2, false), AddressingMode::None),
        0x8A => (OpName::TXA, Cycles(2, false), AddressingMode::None),
        0xCA => (OpName::DEX, Cycles(2, false), AddressingMode::None),
        0xE8 => (OpName::INX, Cycles(2, false), AddressingMode::None),
        0xA8 => (OpName::TAY, Cycles(2, false), AddressingMode::None),
        0x98 => (OpName::TYA, Cycles(2, false), AddressingMode::None),
        0x88 => (OpName::DEY, Cycles(2, false), AddressingMode::None),
        0xC8 => (OpName::INY, Cycles(2, false), AddressingMode::None),

        // Flag (Processor Status) Instructions
        0x18 => (OpName::CLC, Cycles(2, false), AddressingMode::None),
        0x38 => (OpName::SEC, Cycles(2, false), AddressingMode::None),
        0x58 => (OpName::CLI, Cycles(2, false), AddressingMode::None),
        0x78 => (OpName::SEI, Cycles(2, false), AddressingMode::None),
        0xB8 => (OpName::CLV, Cycles(2, false), AddressingMode::None),
        0xD8 => (OpName::CLD, Cycles(2, false), AddressingMode::None),
        0xF8 => (OpName::SED, Cycles(2, false), AddressingMode::None),

        // Branching instructions
        // NOTE: These use "relative" addressing, where the next byte is read as a 2's complement signed integer
        // NOTE: For cycles, the branch ops have a non-standard bonus case... (+1 if branch succeeds, +2 if to a new page)
        0x10 => (OpName::BPL, Cycles(2, false), AddressingMode::Relative),
        0x30 => (OpName::BMI, Cycles(2, false), AddressingMode::Relative),
        0x50 => (OpName::BVC, Cycles(2, false), AddressingMode::Relative),
        0x70 => (OpName::BVS, Cycles(2, false), AddressingMode::Relative),
        0x90 => (OpName::BCC, Cycles(2, false), AddressingMode::Relative),
        0xB0 => (OpName::BCS, Cycles(2, false), AddressingMode::Relative),
        0xD0 => (OpName::BNE, Cycles(2, false), AddressingMode::Relative),
        0xF0 => (OpName::BEQ, Cycles(2, false), AddressingMode::Relative),

        0x00 => (OpName::BRK, Cycles(7, false), AddressingMode::None),
        0x40 => (OpName::RTI, Cycles(6, false), AddressingMode::None),
        0x60 => (OpName::RTS, Cycles(6, false), AddressingMode::None),

        0x20 => (OpName::JSR, Cycles(6, false), AddressingMode::Absolute),
        0x4C => (OpName::JMP, Cycles(3, false), AddressingMode::Absolute),
        0x6C => (OpName::JMP, Cycles(5, false), AddressingMode::Indirect),

        0x87 => (OpName::SAX, Cycles(3, false), AddressingMode::ZeroPage),
        0x97 => (OpName::SAX, Cycles(4, false), AddressingMode::ZeroPageY),
        0x83 => (OpName::SAX, Cycles(6, false), AddressingMode::IndirectX),
        0x8F => (OpName::SAX, Cycles(4, false), AddressingMode::Absolute),

        0xC7 => (OpName::DCP, Cycles(5, false), AddressingMode::ZeroPage),
        0xD7 => (OpName::DCP, Cycles(6, false), AddressingMode::ZeroPageX),
        0xCF => (OpName::DCP, Cycles(6, false), AddressingMode::Absolute),
        0xDF => (OpName::DCP, Cycles(7, false), AddressingMode::AbsoluteX),
        0xDB => (OpName::DCP, Cycles(7, false), AddressingMode::AbsoluteY),
        0xC3 => (OpName::DCP, Cycles(8, false), AddressingMode::IndirectX),
        0xD3 => (OpName::DCP, Cycles(8, false), AddressingMode::IndirectY),

        0xE7 => (OpName::ISB, Cycles(5, false), AddressingMode::ZeroPage),
        0xF7 => (OpName::ISB, Cycles(6, false), AddressingMode::ZeroPageX),
        0xEF => (OpName::ISB, Cycles(6, false), AddressingMode::Absolute),
        0xFF => (OpName::ISB, Cycles(7, false), AddressingMode::AbsoluteX),
        0xFB => (OpName::ISB, Cycles(7, false), AddressingMode::AbsoluteY),
        0xE3 => (OpName::ISB, Cycles(8, false), AddressingMode::IndirectX),
        0xF3 => (OpName::ISB, Cycles(8, false), AddressingMode::IndirectY),

        0x27 => (OpName::RLA, Cycles(5, false), AddressingMode::ZeroPage),
        0x37 => (OpName::RLA, Cycles(6, false), AddressingMode::ZeroPageX),
        0x2F => (OpName::RLA, Cycles(6, false), AddressingMode::Absolute),
        0x3F => (OpName::RLA, Cycles(7, false), AddressingMode::AbsoluteX),
        0x3B => (OpName::RLA, Cycles(7, false), AddressingMode::AbsoluteY),
        0x23 => (OpName::RLA, Cycles(8, false), AddressingMode::IndirectX),
        0x33 => (OpName::RLA, Cycles(8, false), AddressingMode::IndirectY),

        0x67 => (OpName::RRA, Cycles(5, false), AddressingMode::ZeroPage),
        0x77 => (OpName::RRA, Cycles(6, false), AddressingMode::ZeroPageX),
        0x6F => (OpName::RRA, Cycles(6, false), AddressingMode::Absolute),
        0x7F => (OpName::RRA, Cycles(7, false), AddressingMode::AbsoluteX),
        0x7B => (OpName::RRA, Cycles(7, false), AddressingMode::AbsoluteY),
        0x63 => (OpName::RRA, Cycles(8, false), AddressingMode::IndirectX),
        0x73 => (OpName::RRA, Cycles(8, false), AddressingMode::IndirectY),

        0x07 => (OpName::SLO, Cycles(5, false), AddressingMode::ZeroPage),
        0x17 => (OpName::SLO, Cycles(6, false), AddressingMode::ZeroPageX),
        0x0F => (OpName::SLO, Cycles(6, false), AddressingMode::Absolute),
        0x1F => (OpName::SLO, Cycles(7, false), AddressingMode::AbsoluteX),
        0x1B => (OpName::SLO, Cycles(7, false), AddressingMode::AbsoluteY),
        0x03 => (OpName::SLO, Cycles(8, false), AddressingMode::IndirectX),
        0x13 => (OpName::SLO, Cycles(8, false), AddressingMode::IndirectY),

        0x47 => (OpName::SRE, Cycles(5, false), AddressingMode::ZeroPage),
        0x57 => (OpName::SRE, Cycles(6, false), AddressingMode::ZeroPageX),
        0x4F => (OpName::SRE, Cycles(6, false), AddressingMode::Absolute),
        0x5F => (OpName::SRE, Cycles(7, false), AddressingMode::AbsoluteX),
        0x5B => (OpName::SRE, Cycles(7, false), AddressingMode::AbsoluteY),
        0x43 => (OpName::SRE, Cycles(8, false), AddressingMode::IndirectX),
        0x53 => (OpName::SRE, Cycles(8, false), AddressingMode::IndirectY),

        _ => todo!("unsupported opcode = {:02x}", code),
    }
}

pub fn addressing_mode_to_size(mode: &AddressingMode) -> u16 {
    match mode {
        AddressingMode::Immediate => 2,
        AddressingMode::ZeroPage => 2,
        AddressingMode::ZeroPageX => 2,
        AddressingMode::ZeroPageY => 2,
        AddressingMode::Absolute => 3,
        AddressingMode::AbsoluteX => 3,
        AddressingMode::AbsoluteY => 3,
        AddressingMode::IndirectX => 2,
        AddressingMode::IndirectY => 2,
        AddressingMode::None => 1,
        AddressingMode::Indirect => 3,
        AddressingMode::Relative => 2,
        AddressingMode::Accumulator => 1,
    }
}

pub fn is_official(code: u8) -> bool {
    let (name, _, _) = lookup_opcode(code);
    match name {
        OpName::DCP
        | OpName::ISB
        | OpName::RLA
        | OpName::RRA
        | OpName::SAX
        | OpName::SLO
        | OpName::SRE
        | OpName::LAX => false,
        OpName::NOP => code == 0xEA,
        OpName::SBC => ![0xEB].contains(&code),
        _ => true,
    }
}
