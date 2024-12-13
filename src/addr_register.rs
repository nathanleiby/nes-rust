use crate::utility::{addr_from, split_addr};

#[derive(Default)]
pub struct AddrRegister {
    hi: u8,
    lo: u8,

    // TODO: Actually controlled by w_register, which is shared with PpuScrollRegister
    is_lo_byte: bool,
}

const ADDR_REGISTER_MIRROR_MASK: u16 = 0b0011_1111_1111_1111; // 0x3FFF

impl AddrRegister {
    pub fn get(&self) -> u16 {
        addr_from(self.lo, self.hi)
    }

    fn set(&mut self, addr: u16) {
        (self.lo, self.hi) = split_addr(addr);
    }

    pub fn external_write(&mut self, data: u8) {
        if self.is_lo_byte {
            self.lo = data;
        } else {
            self.hi = data;
        }

        // mirror down addr above 0x3fff
        self.set(self.get() & ADDR_REGISTER_MIRROR_MASK);

        self.is_lo_byte = !self.is_lo_byte;
    }

    /// increment is called when there's a read/write to the corresponding `data` register
    // TODO: Should `inc` explicitly only support the valid args of 1 or 32?
    pub fn increment(&mut self, inc: u8) {
        let base = self.get();
        let updated = base.wrapping_add(inc as u16);

        self.set(updated & ADDR_REGISTER_MIRROR_MASK);
    }

    pub fn reset_latch(&mut self) {
        self.is_lo_byte = false;
    }
}

#[cfg(test)]
mod tests {

    use crate::assert_eq_hex;

    use super::*;

    #[test]
    fn test_addr_register_external_writes_and_increment() {
        let mut ar = AddrRegister::default();
        assert_eq_hex!(ar.get(), 0);

        // hi bit
        ar.external_write(0x12);
        assert_eq_hex!(ar.get(), 0x1200);

        // lo bit
        ar.external_write(0x34);
        assert_eq_hex!(ar.get(), 0x1234);

        ar.set(0x0123);
        assert_eq_hex!(ar.get(), 0x0123);

        ar.increment(1);
        assert_eq_hex!(ar.get(), 0x0124);

        ar.increment(0x20);
        assert_eq_hex!(ar.get(), 0x0144);

        ar.set(0x3FFF);
        assert_eq_hex!(ar.get(), 0x3FFF);

        ar.increment(1);
        assert_eq_hex!(ar.get(), 0, "increment should wrap around due to mirroring");
    }

    #[test]
    fn test_addr_register_special_cases() {
        let mut ar = AddrRegister::default();
        assert_eq_hex!(ar.get(), 0);

        ar.external_write(0x50);
        assert_eq_hex!(ar.get(), 0x1000, "hi bit should wrap due to mirroring");

        ar.external_write(0xFF);
        assert_eq_hex!(ar.get(), 0x10FF, "should modify lo bit");

        ar.external_write(0xBC);
        assert_eq_hex!(
            ar.get(),
            0x3CFF,
            "should be hi bit, wrapped due to mirroring"
        );

        ar.reset_latch();
        ar.external_write(0xAB);
        assert_eq_hex!(ar.get(), 0x2BFF, "should be hi bit again, after reset");
    }
}
