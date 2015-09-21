pub struct Ppu;

impl Ppu {
    pub fn new() -> Ppu {
        Ppu
    }

    /// Load a PPU register ($2134 - $213f)
    pub fn load(&mut self, addr: u16) -> u8 {
        panic!("PPU register load unimplemented (${:04X})", addr)
    }

    /// Store a byte in a PPU register ($2100 - $2133)
    pub fn store(&mut self, addr: u16, value: u8) {
        trace!("PPU store: ${:02X} in ${:04X}", value, addr)
    }
}
