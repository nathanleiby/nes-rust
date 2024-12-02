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
        }
    }

    fn interpret(&mut self, ops: Vec<u8>) {
        self.pc = 0;

        loop {
            let op = ops[self.pc as usize];
            match op {
                _ => todo!(),
            }
        }
    }
}

// enum CPUMemoryMap {
//     Ram = 0x0000,
//     IORegisters = 0x2000,
// }

// const CPU_START_RAM: u8 = 0x0000;
// const CPU_IO_REGISTERS: u8 = 0x2000;
// const CPU_EXPANSION_ROM: u8 = 0x4020;
// const CPU_SAVE_ROM: u8 = 0x6000;

/// PPU (Picture Processing Unit)
struct PPU {}

/// RAM
struct RAM {
    /// W-RAM is used by CPU
    w: [u8; 2048],
    /// V-RAM is used by PPU
    v: [u8; 2048],
}

/// Addr is a memory address
type Addr = u16;

/// APU (Audio Processing Unit): five-channel based sounds
struct APU {}

/// Cartridge
/// Carries st least two large ROM chips - the Character ROM (CHR ROM) and the Program ROM (PRG ROM).
struct Cartridge {
    chracterRom: [u8; 1],
    programRom: [u8; 1],
}

/// Gamepad represents which controls are currently pressed
struct Gamepad {
    buttons: [bool; 8],
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interpret() {
        assert_eq!(true, false);
    }
}
