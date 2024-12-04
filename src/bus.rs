struct Bus {
    cpu_vram: [u8; 0x800],
}

impl Bus {
    pub fn new() -> Self {
        Bus {
            cpu_vram: [0; 0x800],
        }
    }
    pub fn read(&self, addr: u16) -> u8 {
        // zero out the highest 2 bits if it receives a request in the range of [0x0000 … 0x2000]
        // top account for mirroring
        let read_addr = if (0x0000..=0x2000).contains(&addr) {
            addr & 0b1110_0111_1111_1111
        } else {
            addr
        };

        self.cpu_vram[read_addr as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_mirroring() {
        let mut bus = Bus::new();
        bus.cpu_vram[0] = 123;

        assert_eq!(bus.read(0), 123);
    }
}
