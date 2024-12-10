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

pub type Op = (OpName, AddressingMode);

pub fn lookup_opcode(code: u8) -> Op {
    match code {
        0x69 => (OpName::ADC, AddressingMode::Immediate),
        0x65 => (OpName::ADC, AddressingMode::ZeroPage),
        0x75 => (OpName::ADC, AddressingMode::ZeroPageX),
        0x6D => (OpName::ADC, AddressingMode::Absolute),
        0x7D => (OpName::ADC, AddressingMode::AbsoluteX),
        0x79 => (OpName::ADC, AddressingMode::AbsoluteY),
        0x61 => (OpName::ADC, AddressingMode::IndirectX),
        0x71 => (OpName::ADC, AddressingMode::IndirectY),

        0x29 => (OpName::AND, AddressingMode::Immediate),
        0x25 => (OpName::AND, AddressingMode::ZeroPage),
        0x35 => (OpName::AND, AddressingMode::ZeroPageX),
        0x2D => (OpName::AND, AddressingMode::Absolute),
        0x3D => (OpName::AND, AddressingMode::AbsoluteX),
        0x39 => (OpName::AND, AddressingMode::AbsoluteY),
        0x21 => (OpName::AND, AddressingMode::IndirectX),
        0x31 => (OpName::AND, AddressingMode::IndirectY),

        0x0A => (OpName::ASL, AddressingMode::Accumulator),
        0x06 => (OpName::ASL, AddressingMode::ZeroPage),
        0x16 => (OpName::ASL, AddressingMode::ZeroPageX),
        0x0E => (OpName::ASL, AddressingMode::Absolute),
        0x1E => (OpName::ASL, AddressingMode::AbsoluteX),

        0x24 => (OpName::BIT, AddressingMode::ZeroPage),
        0x2C => (OpName::BIT, AddressingMode::Absolute),

        0xC9 => (OpName::CMP, AddressingMode::Immediate),
        0xC5 => (OpName::CMP, AddressingMode::ZeroPage),
        0xD5 => (OpName::CMP, AddressingMode::ZeroPageX),
        0xCD => (OpName::CMP, AddressingMode::Absolute),
        0xDD => (OpName::CMP, AddressingMode::AbsoluteX),
        0xD9 => (OpName::CMP, AddressingMode::AbsoluteY),
        0xC1 => (OpName::CMP, AddressingMode::IndirectX),
        0xD1 => (OpName::CMP, AddressingMode::IndirectY),

        0xE0 => (OpName::CPX, AddressingMode::Immediate),
        0xE4 => (OpName::CPX, AddressingMode::ZeroPage),
        0xEC => (OpName::CPX, AddressingMode::Absolute),

        0xC0 => (OpName::CPY, AddressingMode::Immediate),
        0xC4 => (OpName::CPY, AddressingMode::ZeroPage),
        0xCC => (OpName::CPY, AddressingMode::Absolute),

        0xC6 => (OpName::DEC, AddressingMode::ZeroPage),
        0xD6 => (OpName::DEC, AddressingMode::ZeroPageX),
        0xCE => (OpName::DEC, AddressingMode::Absolute),
        0xDE => (OpName::DEC, AddressingMode::AbsoluteX),

        0x49 => (OpName::EOR, AddressingMode::Immediate),
        0x45 => (OpName::EOR, AddressingMode::ZeroPage),
        0x55 => (OpName::EOR, AddressingMode::ZeroPageX),
        0x4D => (OpName::EOR, AddressingMode::Absolute),
        0x5D => (OpName::EOR, AddressingMode::AbsoluteX),
        0x59 => (OpName::EOR, AddressingMode::AbsoluteY),
        0x41 => (OpName::EOR, AddressingMode::IndirectX),
        0x51 => (OpName::EOR, AddressingMode::IndirectY),

        0xE6 => (OpName::INC, AddressingMode::ZeroPage),
        0xF6 => (OpName::INC, AddressingMode::ZeroPageX),
        0xEE => (OpName::INC, AddressingMode::Absolute),
        0xFE => (OpName::INC, AddressingMode::AbsoluteX),

        0xA7 => (OpName::LAX, AddressingMode::ZeroPage),
        0xB7 => (OpName::LAX, AddressingMode::ZeroPageY),
        0xAF => (OpName::LAX, AddressingMode::Absolute),
        0xBF => (OpName::LAX, AddressingMode::AbsoluteY),
        0xA3 => (OpName::LAX, AddressingMode::IndirectX),
        0xB3 => (OpName::LAX, AddressingMode::IndirectY),

        0xA9 => (OpName::LDA, AddressingMode::Immediate),
        0xA5 => (OpName::LDA, AddressingMode::ZeroPage),
        0xB5 => (OpName::LDA, AddressingMode::ZeroPageX),
        0xAD => (OpName::LDA, AddressingMode::Absolute),
        0xBD => (OpName::LDA, AddressingMode::AbsoluteX),
        0xB9 => (OpName::LDA, AddressingMode::AbsoluteY),
        0xA1 => (OpName::LDA, AddressingMode::IndirectX),
        0xB1 => (OpName::LDA, AddressingMode::IndirectY),

        0xA2 => (OpName::LDX, AddressingMode::Immediate),
        0xA6 => (OpName::LDX, AddressingMode::ZeroPage),
        0xB6 => (OpName::LDX, AddressingMode::ZeroPageY),
        0xAE => (OpName::LDX, AddressingMode::Absolute),
        0xBE => (OpName::LDX, AddressingMode::AbsoluteY),

        0xA0 => (OpName::LDY, AddressingMode::Immediate),
        0xA4 => (OpName::LDY, AddressingMode::ZeroPage),
        0xB4 => (OpName::LDY, AddressingMode::ZeroPageX),
        0xAC => (OpName::LDY, AddressingMode::Absolute),
        0xBC => (OpName::LDY, AddressingMode::AbsoluteX),

        0x4A => (OpName::LSR, AddressingMode::Accumulator),
        0x46 => (OpName::LSR, AddressingMode::ZeroPage),
        0x56 => (OpName::LSR, AddressingMode::ZeroPageX),
        0x4E => (OpName::LSR, AddressingMode::Absolute),
        0x5E => (OpName::LSR, AddressingMode::AbsoluteX),

        0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xEA | 0xFA => (OpName::NOP, AddressingMode::None),
        0x04 | 0x44 | 0x64 => (OpName::NOP, AddressingMode::ZeroPage),
        0x14 | 0x34 | 0x54 | 0x74 | 0xD4 | 0xF4 => (OpName::NOP, AddressingMode::ZeroPageX),
        0x80 | 0x82 | 0x89 | 0xC2 | 0xE2 => (OpName::NOP, AddressingMode::Immediate),
        0x0C => (OpName::NOP, AddressingMode::Absolute),
        0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => (OpName::NOP, AddressingMode::AbsoluteX),

        0x09 => (OpName::ORA, AddressingMode::Immediate),
        0x05 => (OpName::ORA, AddressingMode::ZeroPage),
        0x15 => (OpName::ORA, AddressingMode::ZeroPageX),
        0x0D => (OpName::ORA, AddressingMode::Absolute),
        0x1D => (OpName::ORA, AddressingMode::AbsoluteX),
        0x19 => (OpName::ORA, AddressingMode::AbsoluteY),
        0x01 => (OpName::ORA, AddressingMode::IndirectX),
        0x11 => (OpName::ORA, AddressingMode::IndirectY),

        0x2A => (OpName::ROL, AddressingMode::Accumulator),
        0x26 => (OpName::ROL, AddressingMode::ZeroPage),
        0x36 => (OpName::ROL, AddressingMode::ZeroPageX),
        0x2E => (OpName::ROL, AddressingMode::Absolute),
        0x3E => (OpName::ROL, AddressingMode::AbsoluteX),

        0x6A => (OpName::ROR, AddressingMode::Accumulator),
        0x66 => (OpName::ROR, AddressingMode::ZeroPage),
        0x76 => (OpName::ROR, AddressingMode::ZeroPageX),
        0x6E => (OpName::ROR, AddressingMode::Absolute),
        0x7E => (OpName::ROR, AddressingMode::AbsoluteX),

        0xE9 => (OpName::SBC, AddressingMode::Immediate),
        0xEB => (OpName::SBC, AddressingMode::Immediate),
        0xE5 => (OpName::SBC, AddressingMode::ZeroPage),
        0xF5 => (OpName::SBC, AddressingMode::ZeroPageX),
        0xED => (OpName::SBC, AddressingMode::Absolute),
        0xFD => (OpName::SBC, AddressingMode::AbsoluteX),
        0xF9 => (OpName::SBC, AddressingMode::AbsoluteY),
        0xE1 => (OpName::SBC, AddressingMode::IndirectX),
        0xF1 => (OpName::SBC, AddressingMode::IndirectY),

        0x85 => (OpName::STA, AddressingMode::ZeroPage),
        0x95 => (OpName::STA, AddressingMode::ZeroPageX),
        0x8D => (OpName::STA, AddressingMode::Absolute),
        0x9D => (OpName::STA, AddressingMode::AbsoluteX),
        0x99 => (OpName::STA, AddressingMode::AbsoluteY),
        0x81 => (OpName::STA, AddressingMode::IndirectX),
        0x91 => (OpName::STA, AddressingMode::IndirectY),

        0x86 => (OpName::STX, AddressingMode::ZeroPage),
        0x96 => (OpName::STX, AddressingMode::ZeroPageY),
        0x8E => (OpName::STX, AddressingMode::Absolute),

        0x84 => (OpName::STY, AddressingMode::ZeroPage),
        0x94 => (OpName::STY, AddressingMode::ZeroPageX),
        0x8C => (OpName::STY, AddressingMode::Absolute),

        // Stack Instructions
        0x9A => (OpName::TXS, AddressingMode::None),
        0xBA => (OpName::TSX, AddressingMode::None),
        0x48 => (OpName::PHA, AddressingMode::None),
        0x68 => (OpName::PLA, AddressingMode::None),
        0x08 => (OpName::PHP, AddressingMode::None),
        0x28 => (OpName::PLP, AddressingMode::None),

        // Register Instructions
        0xAA => (OpName::TAX, AddressingMode::None),
        0x8A => (OpName::TXA, AddressingMode::None),
        0xCA => (OpName::DEX, AddressingMode::None),
        0xE8 => (OpName::INX, AddressingMode::None),
        0xA8 => (OpName::TAY, AddressingMode::None),
        0x98 => (OpName::TYA, AddressingMode::None),
        0x88 => (OpName::DEY, AddressingMode::None),
        0xC8 => (OpName::INY, AddressingMode::None),

        // Flag (Processor Status) Instructions
        0x18 => (OpName::CLC, AddressingMode::None),
        0x38 => (OpName::SEC, AddressingMode::None),
        0x58 => (OpName::CLI, AddressingMode::None),
        0x78 => (OpName::SEI, AddressingMode::None),
        0xB8 => (OpName::CLV, AddressingMode::None),
        0xD8 => (OpName::CLD, AddressingMode::None),
        0xF8 => (OpName::SED, AddressingMode::None),

        // Branching instructions
        // NOTE: These use "relative" addressing, where the next byte is read as a 2's complement signed integer
        0x10 => (OpName::BPL, AddressingMode::Relative),
        0x30 => (OpName::BMI, AddressingMode::Relative),
        0x50 => (OpName::BVC, AddressingMode::Relative),
        0x70 => (OpName::BVS, AddressingMode::Relative),
        0x90 => (OpName::BCC, AddressingMode::Relative),
        0xB0 => (OpName::BCS, AddressingMode::Relative),
        0xD0 => (OpName::BNE, AddressingMode::Relative),
        0xF0 => (OpName::BEQ, AddressingMode::Relative),

        0x00 => (OpName::BRK, AddressingMode::None),
        0x40 => (OpName::RTI, AddressingMode::None),
        0x60 => (OpName::RTS, AddressingMode::None),

        0x20 => (OpName::JSR, AddressingMode::Absolute),
        0x4C => (OpName::JMP, AddressingMode::Absolute),
        0x6C => (OpName::JMP, AddressingMode::Indirect),

        0x87 => (OpName::SAX, AddressingMode::ZeroPage),
        0x97 => (OpName::SAX, AddressingMode::ZeroPageY),
        0x83 => (OpName::SAX, AddressingMode::IndirectX),
        0x8F => (OpName::SAX, AddressingMode::Absolute),

        0xC7 => (OpName::DCP, AddressingMode::ZeroPage),
        0xD7 => (OpName::DCP, AddressingMode::ZeroPageX),
        0xCF => (OpName::DCP, AddressingMode::Absolute),
        0xDF => (OpName::DCP, AddressingMode::AbsoluteX),
        0xDB => (OpName::DCP, AddressingMode::AbsoluteY),
        0xC3 => (OpName::DCP, AddressingMode::IndirectX),
        0xD3 => (OpName::DCP, AddressingMode::IndirectY),

        0xE7 => (OpName::ISB, AddressingMode::ZeroPage),
        0xF7 => (OpName::ISB, AddressingMode::ZeroPageX),
        0xEF => (OpName::ISB, AddressingMode::Absolute),
        0xFF => (OpName::ISB, AddressingMode::AbsoluteX),
        0xFB => (OpName::ISB, AddressingMode::AbsoluteY),
        0xE3 => (OpName::ISB, AddressingMode::IndirectX),
        0xF3 => (OpName::ISB, AddressingMode::IndirectY),

        0x27 => (OpName::RLA, AddressingMode::ZeroPage),
        0x37 => (OpName::RLA, AddressingMode::ZeroPageX),
        0x2F => (OpName::RLA, AddressingMode::Absolute),
        0x3F => (OpName::RLA, AddressingMode::AbsoluteX),
        0x3B => (OpName::RLA, AddressingMode::AbsoluteY),
        0x23 => (OpName::RLA, AddressingMode::IndirectX),
        0x33 => (OpName::RLA, AddressingMode::IndirectY),

        0x67 => (OpName::RRA, AddressingMode::ZeroPage),
        0x77 => (OpName::RRA, AddressingMode::ZeroPageX),
        0x6F => (OpName::RRA, AddressingMode::Absolute),
        0x7F => (OpName::RRA, AddressingMode::AbsoluteX),
        0x7B => (OpName::RRA, AddressingMode::AbsoluteY),
        0x63 => (OpName::RRA, AddressingMode::IndirectX),
        0x73 => (OpName::RRA, AddressingMode::IndirectY),

        0x07 => (OpName::SLO, AddressingMode::ZeroPage),
        0x17 => (OpName::SLO, AddressingMode::ZeroPageX),
        0x0F => (OpName::SLO, AddressingMode::Absolute),
        0x1F => (OpName::SLO, AddressingMode::AbsoluteX),
        0x1B => (OpName::SLO, AddressingMode::AbsoluteY),
        0x03 => (OpName::SLO, AddressingMode::IndirectX),
        0x13 => (OpName::SLO, AddressingMode::IndirectY),

        0x47 => (OpName::SRE, AddressingMode::ZeroPage),
        0x57 => (OpName::SRE, AddressingMode::ZeroPageX),
        0x4F => (OpName::SRE, AddressingMode::Absolute),
        0x5F => (OpName::SRE, AddressingMode::AbsoluteX),
        0x5B => (OpName::SRE, AddressingMode::AbsoluteY),
        0x43 => (OpName::SRE, AddressingMode::IndirectX),
        0x53 => (OpName::SRE, AddressingMode::IndirectY),

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
    let (name, _) = lookup_opcode(code);
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
