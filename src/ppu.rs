use cpu::AddressSpace;

pub struct Ppu;

impl Ppu {
    pub fn new() -> Ppu {
        Ppu
    }
}

impl AddressSpace for Ppu {
    fn load(&mut self, bank: u8, addr: u16) -> u8 {
        0
    }

    fn store(&mut self, bank: u8, addr: u16, value: u8) {
    }
}
