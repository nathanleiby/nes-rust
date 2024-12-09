pub fn addr_from(lo: u8, hi: u8) -> u16 {
    ((hi as u16) << 8) + lo as u16
}

pub fn split_addr(addr: u16) -> (u8, u8) {
    let lo = (addr & 0xff) as u8;
    let hi = (addr >> 8) as u8;
    (lo, hi)
}
