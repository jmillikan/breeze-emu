pub struct Ppu;

impl Ppu {
    pub fn new() -> Ppu {
        Ppu
    }

    pub fn load(&mut self, bank: u8, addr: u16) -> u8 {
        0
    }

    pub fn store(&mut self, bank: u8, addr: u16, value: u8) {
    }
}
