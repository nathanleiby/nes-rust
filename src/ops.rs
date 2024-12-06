use core::fmt;

use crate::core::AddressingMode;

// TODO: Stringify these
#[derive(Debug, PartialEq, Eq)]
pub enum OpName {
    LDA,

    TODO, // TODO
}

impl fmt::Display for OpName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub struct Op(pub OpName, pub u16, pub AddressingMode);

pub fn parse_opcode(code: u8) -> Option<Op> {
    let op = match code {
        0xA9 => Op(OpName::LDA, 2, AddressingMode::Immediate),
        0xA5 => Op(OpName::LDA, 2, AddressingMode::ZeroPage),
        0xB5 => Op(OpName::LDA, 2, AddressingMode::ZeroPageX),
        0xAD => Op(OpName::LDA, 3, AddressingMode::Absolute),
        0xBD => Op(OpName::LDA, 3, AddressingMode::AbsoluteX),
        0xB9 => Op(OpName::LDA, 3, AddressingMode::AbsoluteY),
        0xA1 => Op(OpName::LDA, 2, AddressingMode::IndirectX),
        0xB1 => Op(OpName::LDA, 2, AddressingMode::IndirectY),
        // 0xA1 => Op(OpName::LDA, 3, AddressingMode::AbsoluteY),
        // 0xB1 => Op(OpName::LDA, 3, AddressingMode::AbsoluteY),
        // 0xA2 => {
        //     self.ldx(&AddressingMode::Immediate);
        //     self.pc += 1;
        // }
        _ => Op(OpName::TODO, 0, AddressingMode::None),
    };

    if op.0 == OpName::TODO {
        None
    } else {
        Some(op)
    }
}
