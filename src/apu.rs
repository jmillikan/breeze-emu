pub struct Apu;

impl Apu {
    pub fn new() -> Apu {
        Apu
    }

    /// Store a byte in an IO port (0-3)
    pub fn store(&mut self, port: u8, value: u8) {

    }

    /// Load a byte from an IO port (0-3)
    pub fn load(&mut self, port: u8) -> u8 {
        0
    }
}
