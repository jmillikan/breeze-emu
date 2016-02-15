//! APU timer implementation.

#[derive(Copy, Clone, Debug)]
pub struct Timer {
    /// Divider of this timer
    pub div: u8,
    /// Current value of the counter. Limited to 4 bits (the upper 4 bits are always 0)
    pub val: u8,
    enabled: bool,
    /// Counted up at the same speed as the APU runs. When it reaches 128 (Timer 0/1) or 16
    /// (Timer 2), this is reset and stage2 is incremented.
    stage1: u8,
    /// Counted up at the timer's frequency. When this reaches `div`, it is reset and `val`
    /// (stage3) incremented. Since `div=0` is interpreted as 256, this is a `u16`.
    stage2: u16,
}

impl_save_state!(Timer { div, val, enabled, stage1, stage2 } ignore {});

impl Default for Timer {
    fn default() -> Self {
        Timer {
            div: 0,
            val: 0x0f,
            enabled: false,
            stage1: 0,
            stage2: 0,
        }
    }
}

impl Timer {
    pub fn new() -> Timer { Timer::default() }

    /// Update the timer, applying a base-divisor (128 for Timer 0/1, 16 for Timer 2)
    pub fn update(&mut self, base_div: u8, cy: u8) {
        self.stage1 += cy;
        if self.enabled { self.stage2 += self.stage1 as u16 / base_div as u16; }
        self.stage1 %= base_div;

        if self.enabled {
            let real_div: u16 = if self.div == 0 { 256 } else { self.div as u16 };
            self.val += (self.stage2 / real_div) as u8;
            self.stage2 %= real_div;
            self.val &= 0x0f;   // It's a 4-bit counter
        }
    }

    pub fn set_enable(&mut self, enable: bool) {
        if !self.enabled && enable {
            self.stage2 = 0;
            self.val = 0;
        }

        self.enabled = enable;
    }
}
