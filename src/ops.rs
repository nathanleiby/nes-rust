use core::fmt;

use crate::core::{AddressingMode, Flag};

// TODO: Stringify these
#[derive(Debug, PartialEq, Eq)]
pub enum OpName {
    LDA,
    LDX,
    LDY,
    LSR,

    // TODO
    TODO,

    ASL,
    STA,
    STX,
    STY,
    BIT,
    NOP,
    TXS,
    TSX,
    PHA,
    PLA,
    PHP,
    PLP,
    ORA,
    // SetFlag(Flag, bool),
}

impl fmt::Display for OpName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            _ => write!(f, "{:?}", self),
        }
    }
}

pub type Op = (OpName, u16, AddressingMode);

pub fn lookup_opcode(code: u8) -> Option<Op> {
    let op = match code {
        // LDA
        0xA9 => (OpName::LDA, 2, AddressingMode::Immediate),
        0xA5 => (OpName::LDA, 2, AddressingMode::ZeroPage),
        0xB5 => (OpName::LDA, 2, AddressingMode::ZeroPageX),
        0xAD => (OpName::LDA, 3, AddressingMode::Absolute),
        0xBD => (OpName::LDA, 3, AddressingMode::AbsoluteX),
        0xB9 => (OpName::LDA, 3, AddressingMode::AbsoluteY),
        0xA1 => (OpName::LDA, 2, AddressingMode::IndirectX),
        0xB1 => (OpName::LDA, 2, AddressingMode::IndirectY),

        // LDX
        0xA2 => (OpName::LDX, 2, AddressingMode::Immediate),
        0xA6 => (OpName::LDX, 2, AddressingMode::ZeroPage),
        0xB6 => (OpName::LDX, 2, AddressingMode::ZeroPageY),
        0xAE => (OpName::LDX, 3, AddressingMode::Absolute),
        0xBE => (OpName::LDX, 3, AddressingMode::AbsoluteY),

        // LDY
        0xA0 => (OpName::LDY, 2, AddressingMode::Immediate),
        0xA4 => (OpName::LDY, 2, AddressingMode::ZeroPage),
        0xB4 => (OpName::LDY, 2, AddressingMode::ZeroPageX),
        0xAC => (OpName::LDY, 3, AddressingMode::Absolute),
        0xBC => (OpName::LDY, 3, AddressingMode::AbsoluteX),

        // LSR
        0x4A => (OpName::LSR, 2, AddressingMode::None),
        0x46 => (OpName::LSR, 2, AddressingMode::ZeroPage),
        0x56 => (OpName::LSR, 2, AddressingMode::ZeroPageX),
        0x4E => (OpName::LSR, 3, AddressingMode::Absolute),
        0x5E => (OpName::LSR, 3, AddressingMode::AbsoluteX),

        // ASL
        0x0A => (OpName::ASL, 2, AddressingMode::None),
        0x06 => (OpName::ASL, 2, AddressingMode::ZeroPage),
        0x16 => (OpName::ASL, 2, AddressingMode::ZeroPageX),
        0x0E => (OpName::ASL, 3, AddressingMode::Absolute),
        0x1E => (OpName::ASL, 3, AddressingMode::AbsoluteX),

        // STA
        0x85 => (OpName::STA, 2, AddressingMode::ZeroPage),
        0x95 => (OpName::STA, 2, AddressingMode::ZeroPageX),
        0x8D => (OpName::STA, 3, AddressingMode::Absolute),
        0x9D => (OpName::STA, 3, AddressingMode::AbsoluteX),
        0x99 => (OpName::STA, 3, AddressingMode::AbsoluteY),
        0x81 => (OpName::STA, 2, AddressingMode::IndirectX),
        0x91 => (OpName::STA, 2, AddressingMode::IndirectY),

        // STX
        0x86 => (OpName::STX, 2, AddressingMode::ZeroPage),
        0x96 => (OpName::STX, 2, AddressingMode::ZeroPageX),
        0x8E => (OpName::STX, 3, AddressingMode::Absolute),

        // STY
        0x84 => (OpName::STY, 2, AddressingMode::ZeroPage),
        0x94 => (OpName::STY, 2, AddressingMode::ZeroPageX),
        0x8C => (OpName::STY, 3, AddressingMode::Absolute),

        // BIT
        0x24 => (OpName::BIT, 2, AddressingMode::ZeroPage),
        0x2C => (OpName::BIT, 3, AddressingMode::Absolute),

        // NOP
        0xEA => (OpName::NOP, 1, AddressingMode::None),

        // Stack Instructions
        0x9A => (OpName::TXS, 1, AddressingMode::None),
        0xBA => (OpName::TSX, 1, AddressingMode::None),
        0x48 => (OpName::PHA, 1, AddressingMode::None),
        0x68 => (OpName::PLA, 1, AddressingMode::None),
        0x08 => (OpName::PHP, 1, AddressingMode::None),
        0x28 => (OpName::PLP, 1, AddressingMode::None),

        // ORA
        0x09 => (OpName::ORA, 2, AddressingMode::Immediate),
        0x05 => (OpName::ORA, 2, AddressingMode::ZeroPage),
        0x15 => (OpName::ORA, 2, AddressingMode::ZeroPageX),
        0x0D => (OpName::ORA, 3, AddressingMode::Absolute),
        0x1D => (OpName::ORA, 3, AddressingMode::AbsoluteX),
        0x19 => (OpName::ORA, 3, AddressingMode::AbsoluteY),
        0x01 => (OpName::ORA, 2, AddressingMode::IndirectX),
        0x11 => (OpName::ORA, 2, AddressingMode::IndirectY),

        _ => (OpName::TODO, 0, AddressingMode::None),
    };

    if op.0 == OpName::TODO {
        None
    } else {
        Some(op)
    }
}
