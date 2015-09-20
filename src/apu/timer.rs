//! APU timer implementation.

#[derive(Copy, Clone, Default, Debug)]
pub struct Timer {
    /// Divider of this timer
    pub div: u8,
    /// Current value of the counter. Limited to 4 bits (the upper 4 bits are always 0)
    pub val: u8,
    enabled: bool,
    /// Counted up at the same speed as the APU runs. When it reaches 128 (Timer 0/1) or 16
    /// (Timer 2), this is reset and stage1 is incremented.
    stage0: u8,
    /// Counted up at the timer's frequency. When this reaches `div`, it is reset and `val`
    /// (stage3) incremented. Since `div=0` is interpreted as 256, this is a `u16`.
    stage1: u16,
}

impl Timer {
    pub fn new() -> Timer { Timer::default() }

    /// Update the timer, applying a base-divisor (128 for Timer 0/1, 16 for Timer 2)
    pub fn update(&mut self, base_div: u8, cy: u8) {
        self.stage0 += cy;
        self.stage1 += self.stage0 as u16 / base_div as u16;
        self.stage0 %= base_div;

        let real_div: u16 = if self.div == 0 { 256 } else { self.div as u16 };
        self.val += (self.stage1 / real_div) as u8;
        self.stage1 %= real_div;
        self.val &= 0x0f;   // It's a 4-bit counter
    }

    pub fn set_enable(&mut self, enable: bool) {
        if !self.enabled && enable {
            self.stage1 = 0;
            self.val = 0;
        }

        self.enabled = enable;
    }
}
