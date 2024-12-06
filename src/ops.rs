use core::fmt;
use std::ops::Add;

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
}

impl fmt::Display for OpName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type Op = (OpName, u16, AddressingMode);

pub fn lookup_opcode(code: u8) -> Op {
    match code {
        0x69 => (OpName::ADC, 2, AddressingMode::Immediate),
        0x65 => (OpName::ADC, 2, AddressingMode::ZeroPage),
        0x75 => (OpName::ADC, 2, AddressingMode::ZeroPageX),
        0x6D => (OpName::ADC, 3, AddressingMode::Absolute),
        0x7D => (OpName::ADC, 3, AddressingMode::AbsoluteX),
        0x79 => (OpName::ADC, 3, AddressingMode::AbsoluteY),
        0x61 => (OpName::ADC, 2, AddressingMode::IndirectX),
        0x71 => (OpName::ADC, 2, AddressingMode::IndirectY),

        0x29 => (OpName::AND, 2, AddressingMode::Immediate),
        0x25 => (OpName::AND, 2, AddressingMode::ZeroPage),
        0x35 => (OpName::AND, 2, AddressingMode::ZeroPageX),
        0x2D => (OpName::AND, 3, AddressingMode::Absolute),
        0x3D => (OpName::AND, 3, AddressingMode::AbsoluteX),
        0x39 => (OpName::AND, 3, AddressingMode::AbsoluteY),
        0x21 => (OpName::AND, 2, AddressingMode::IndirectX),
        0x31 => (OpName::AND, 2, AddressingMode::IndirectY),

        0x0A => (OpName::ASL, 2, AddressingMode::None),
        0x06 => (OpName::ASL, 2, AddressingMode::ZeroPage),
        0x16 => (OpName::ASL, 2, AddressingMode::ZeroPageX),
        0x0E => (OpName::ASL, 3, AddressingMode::Absolute),
        0x1E => (OpName::ASL, 3, AddressingMode::AbsoluteX),

        0x24 => (OpName::BIT, 2, AddressingMode::ZeroPage),
        0x2C => (OpName::BIT, 3, AddressingMode::Absolute),

        0xC9 => (OpName::CMP, 2, AddressingMode::Immediate),
        0xC5 => (OpName::CMP, 2, AddressingMode::ZeroPage),
        0xD5 => (OpName::CMP, 2, AddressingMode::ZeroPageX),
        0xCD => (OpName::CMP, 3, AddressingMode::Absolute),
        0xDD => (OpName::CMP, 3, AddressingMode::AbsoluteX),
        0xD9 => (OpName::CMP, 3, AddressingMode::AbsoluteY),
        0xC1 => (OpName::CMP, 2, AddressingMode::IndirectX),
        0xD1 => (OpName::CMP, 2, AddressingMode::IndirectY),

        0xE0 => (OpName::CPX, 2, AddressingMode::Immediate),
        0xE4 => (OpName::CPX, 2, AddressingMode::ZeroPage),
        0xEC => (OpName::CPX, 3, AddressingMode::Absolute),

        0xC0 => (OpName::CPY, 2, AddressingMode::Immediate),
        0xC4 => (OpName::CPY, 2, AddressingMode::ZeroPage),
        0xCC => (OpName::CPY, 3, AddressingMode::Absolute),

        0xC6 => (OpName::DEC, 2, AddressingMode::ZeroPage),
        0xD6 => (OpName::DEC, 2, AddressingMode::ZeroPageX),
        0xCE => (OpName::DEC, 3, AddressingMode::Absolute),
        0xDE => (OpName::DEC, 3, AddressingMode::AbsoluteX),

        0x49 => (OpName::EOR, 2, AddressingMode::Immediate),
        0x45 => (OpName::EOR, 2, AddressingMode::ZeroPage),
        0x55 => (OpName::EOR, 2, AddressingMode::ZeroPageX),
        0x4D => (OpName::EOR, 3, AddressingMode::Absolute),
        0x5D => (OpName::EOR, 3, AddressingMode::AbsoluteX),
        0x59 => (OpName::EOR, 3, AddressingMode::AbsoluteY),
        0x41 => (OpName::EOR, 2, AddressingMode::IndirectX),
        0x51 => (OpName::EOR, 2, AddressingMode::IndirectY),

        0xE6 => (OpName::INC, 2, AddressingMode::ZeroPage),
        0xF6 => (OpName::INC, 2, AddressingMode::ZeroPageX),
        0xEE => (OpName::INC, 3, AddressingMode::Absolute),
        0xFE => (OpName::INC, 3, AddressingMode::AbsoluteX),

        0xA9 => (OpName::LDA, 2, AddressingMode::Immediate),
        0xA5 => (OpName::LDA, 2, AddressingMode::ZeroPage),
        0xB5 => (OpName::LDA, 2, AddressingMode::ZeroPageX),
        0xAD => (OpName::LDA, 3, AddressingMode::Absolute),
        0xBD => (OpName::LDA, 3, AddressingMode::AbsoluteX),
        0xB9 => (OpName::LDA, 3, AddressingMode::AbsoluteY),
        0xA1 => (OpName::LDA, 2, AddressingMode::IndirectX),
        0xB1 => (OpName::LDA, 2, AddressingMode::IndirectY),

        0xA2 => (OpName::LDX, 2, AddressingMode::Immediate),
        0xA6 => (OpName::LDX, 2, AddressingMode::ZeroPage),
        0xB6 => (OpName::LDX, 2, AddressingMode::ZeroPageY),
        0xAE => (OpName::LDX, 3, AddressingMode::Absolute),
        0xBE => (OpName::LDX, 3, AddressingMode::AbsoluteY),

        0xA0 => (OpName::LDY, 2, AddressingMode::Immediate),
        0xA4 => (OpName::LDY, 2, AddressingMode::ZeroPage),
        0xB4 => (OpName::LDY, 2, AddressingMode::ZeroPageX),
        0xAC => (OpName::LDY, 3, AddressingMode::Absolute),
        0xBC => (OpName::LDY, 3, AddressingMode::AbsoluteX),

        0x4A => (OpName::LSR, 2, AddressingMode::None),
        0x46 => (OpName::LSR, 2, AddressingMode::ZeroPage),
        0x56 => (OpName::LSR, 2, AddressingMode::ZeroPageX),
        0x4E => (OpName::LSR, 3, AddressingMode::Absolute),
        0x5E => (OpName::LSR, 3, AddressingMode::AbsoluteX),

        0xEA => (OpName::NOP, 1, AddressingMode::None),

        0x09 => (OpName::ORA, 2, AddressingMode::Immediate),
        0x05 => (OpName::ORA, 2, AddressingMode::ZeroPage),
        0x15 => (OpName::ORA, 2, AddressingMode::ZeroPageX),
        0x0D => (OpName::ORA, 3, AddressingMode::Absolute),
        0x1D => (OpName::ORA, 3, AddressingMode::AbsoluteX),
        0x19 => (OpName::ORA, 3, AddressingMode::AbsoluteY),
        0x01 => (OpName::ORA, 2, AddressingMode::IndirectX),
        0x11 => (OpName::ORA, 2, AddressingMode::IndirectY),

        0x2A => (OpName::ROL, 2, AddressingMode::None),
        0x26 => (OpName::ROL, 2, AddressingMode::ZeroPage),
        0x36 => (OpName::ROL, 2, AddressingMode::ZeroPageX),
        0x2E => (OpName::ROL, 3, AddressingMode::Absolute),
        0x3E => (OpName::ROL, 3, AddressingMode::AbsoluteX),

        0x6A => (OpName::ROR, 2, AddressingMode::None),
        0x66 => (OpName::ROR, 2, AddressingMode::ZeroPage),
        0x76 => (OpName::ROR, 2, AddressingMode::ZeroPageX),
        0x6E => (OpName::ROR, 3, AddressingMode::Absolute),
        0x7E => (OpName::ROR, 3, AddressingMode::AbsoluteX),

        0xE9 => (OpName::SBC, 2, AddressingMode::Immediate),
        0xE5 => (OpName::SBC, 2, AddressingMode::ZeroPage),
        0xF5 => (OpName::SBC, 2, AddressingMode::ZeroPageX),
        0xED => (OpName::SBC, 3, AddressingMode::Absolute),
        0xFD => (OpName::SBC, 3, AddressingMode::AbsoluteX),
        0xF9 => (OpName::SBC, 3, AddressingMode::AbsoluteY),
        0xE1 => (OpName::SBC, 2, AddressingMode::IndirectX),
        0xF1 => (OpName::SBC, 2, AddressingMode::IndirectY),

        0x85 => (OpName::STA, 2, AddressingMode::ZeroPage),
        0x95 => (OpName::STA, 2, AddressingMode::ZeroPageX),
        0x8D => (OpName::STA, 3, AddressingMode::Absolute),
        0x9D => (OpName::STA, 3, AddressingMode::AbsoluteX),
        0x99 => (OpName::STA, 3, AddressingMode::AbsoluteY),
        0x81 => (OpName::STA, 2, AddressingMode::IndirectX),
        0x91 => (OpName::STA, 2, AddressingMode::IndirectY),

        0x86 => (OpName::STX, 2, AddressingMode::ZeroPage),
        0x96 => (OpName::STX, 2, AddressingMode::ZeroPageX),
        0x8E => (OpName::STX, 3, AddressingMode::Absolute),

        0x84 => (OpName::STY, 2, AddressingMode::ZeroPage),
        0x94 => (OpName::STY, 2, AddressingMode::ZeroPageX),
        0x8C => (OpName::STY, 3, AddressingMode::Absolute),

        // Stack Instructions
        0x9A => (OpName::TXS, 1, AddressingMode::None),
        0xBA => (OpName::TSX, 1, AddressingMode::None),
        0x48 => (OpName::PHA, 1, AddressingMode::None),
        0x68 => (OpName::PLA, 1, AddressingMode::None),
        0x08 => (OpName::PHP, 1, AddressingMode::None),
        0x28 => (OpName::PLP, 1, AddressingMode::None),

        // Register Instructions
        0xAA => (OpName::TAX, 1, AddressingMode::None),
        0x8A => (OpName::TXA, 1, AddressingMode::None),
        0xCA => (OpName::DEX, 1, AddressingMode::None),
        0xE8 => (OpName::INX, 1, AddressingMode::None),
        0xA8 => (OpName::TAY, 1, AddressingMode::None),
        0x98 => (OpName::TYA, 1, AddressingMode::None),
        0x88 => (OpName::DEY, 1, AddressingMode::None),
        0xC8 => (OpName::INY, 1, AddressingMode::None),

        // Flag (Processor Status) Instructions
        0x18 => (OpName::CLC, 1, AddressingMode::None),
        0x38 => (OpName::SEC, 1, AddressingMode::None),
        0x58 => (OpName::CLI, 1, AddressingMode::None),
        0x78 => (OpName::SEI, 1, AddressingMode::None),
        0xB8 => (OpName::CLV, 1, AddressingMode::None),
        0xD8 => (OpName::CLD, 1, AddressingMode::None),
        0xF8 => (OpName::SED, 1, AddressingMode::None),

        // Branching instructions
        0x10 => (OpName::BPL, 1, AddressingMode::None),
        0x30 => (OpName::BMI, 1, AddressingMode::None),
        0x50 => (OpName::BVC, 1, AddressingMode::None),
        0x70 => (OpName::BVS, 1, AddressingMode::None),
        0x90 => (OpName::BCC, 1, AddressingMode::None),
        0xB0 => (OpName::BCS, 1, AddressingMode::None),
        0xD0 => (OpName::BNE, 1, AddressingMode::None),
        0xF0 => (OpName::BEQ, 1, AddressingMode::None),

        0x00 => (OpName::BRK, 1, AddressingMode::None),
        0x40 => (OpName::RTI, 1, AddressingMode::None),
        0x60 => (OpName::RTS, 1, AddressingMode::None),

        0x20 => (OpName::JSR, 3, AddressingMode::Absolute),
        0x4C => (OpName::JMP, 3, AddressingMode::Absolute),
        0x6C => (OpName::JMP, 3, AddressingMode::Indirect),

        _ => todo!("unsupported opcode = {:02x}", code),
    }
}
