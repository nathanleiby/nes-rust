pub fn addr_from(lo: u8, hi: u8) -> u16 {
    ((hi as u16) << 8) + lo as u16
}

pub fn split_addr(addr: u16) -> (u8, u8) {
    let lo = (addr & 0xff) as u8;
    let hi = (addr >> 8) as u8;
    (lo, hi)
}

pub fn is_addr_at_page_edge(addr: u16) -> bool {
    addr & 0x00ff == 0xff
}

pub fn is_page_cross(addr1: u16, addr2: u16) -> bool {
    (addr1 & 0xFF00) != (addr2 & 0xFF00)
}
