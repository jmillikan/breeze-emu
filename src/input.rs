//! Emulates the 4 controller ports (the SNES only has 2 controller ports, but I think each one
//! supports up to 2 controllers).

/// Controller input. `update` should be called once per frame (when entering V-Blank).
pub struct Input {
    sources: [Box<InputSource>; 4],
    states: [InputState; 4],
    /// Bit in the input state (where 0 = MSb, 15 = LSb) returned by next $4016/$4017 read resp.
    bitpos4016: u8,
    bitpos4017: u8,
}

impl Default for Input {
    /// Create the default input config. No controllers are attached.
    fn default() -> Input {
        Input {
            sources: [
                Box::new(DummyInput),
                Box::new(DummyInput),
                Box::new(DummyInput),
                Box::new(DummyInput)
            ],
            states: [InputState::default(); 4],
            bitpos4016: 0,
            bitpos4017: 0,
        }
    }
}

impl Input {
    /// Polls all controllers and stores their state
    pub fn update(&mut self) {
        for i in 0..4 {
            self.states[i] = self.sources[i].poll();
        }
    }

    /// Read from an input register
    pub fn load(&mut self, reg: u16) -> u8 {
        match reg {
            0x4016 => {
                let bitpos = self.bitpos4016;
                if bitpos == 16 { 1 } else {
                    // Read controller 1 and 3
                    let a = self.states[0].0 & (0x8000 >> bitpos as u16) != 0;
                    let c = self.states[2].0 & (0x8000 >> bitpos as u16) != 0;
                    self.bitpos4016 += 1;

                    (if c {0x02} else {0x00}) | (if a {0x01} else {0x00})
                }
            }
            0x4017 => {
                let bitpos = self.bitpos4017;
                if bitpos == 16 { 1 } else {
                    // Read controller 2 and 4
                    let a = self.states[1].0 & (0x8000 >> bitpos as u16) != 0;
                    let c = self.states[3].0 & (0x8000 >> bitpos as u16) != 0;
                    self.bitpos4017 += 1;

                    (if c {0x02} else {0x00}) | (if a {0x01} else {0x00})
                }
            }
            0x4218 ... 0x421f => {
                // "Full" read
                let controller = (reg - 0x4218) / 2;
                let hi = reg & 1 != 0;
                let state = &self.states[controller as usize];
                match hi {
                    false => state.0 as u8,
                    true => (state.0 >> 8) as u8,
                }
            }
            _ => panic!("${:04X} is not an input register", reg)
        }
    }

    /// Store to an input register. Will just latch the serial input.
    pub fn store(&mut self, reg: u16, val: u8) {
        if reg == 0x4016 {
            // No idea what this actually does, and no way to test it. Great! Let's just reset so
            // the MSb is read next time.
            assert!(val & 0xfe == val);
            self.bitpos4016 = 0;
            self.bitpos4017 = 0;
        } else {
            panic!("invalid input reg store to ${:04X}", reg);
        }
    }
}

/// State of a SNES joypad. The low byte can be read from `$4218`, the high byte from `$4219` (for
/// controller 1).
///
/// Bits:
/// `B Y Select Start Up Down Left Right - A X L R 0 0 0 0`
#[derive(Clone, Copy, Default)]
struct InputState(u16);

impl InputState {
    fn new() -> Self { Self::default() }

    fn set(&mut self, bit: u8, value: bool) -> &mut Self {
        match value {
            true => self.0 |= 1 << bit,
            false => self.0 &= !(1 << bit),
        };
        self
    }

    fn a(&mut self, pressed: bool) -> &mut Self { self.set(7, pressed) }
    fn b(&mut self, pressed: bool) -> &mut Self { self.set(15, pressed) }
    fn x(&mut self, pressed: bool) -> &mut Self { self.set(6, pressed) }
    fn y(&mut self, pressed: bool) -> &mut Self { self.set(14, pressed) }

    fn l(&mut self, pressed: bool) -> &mut Self { self.set(5, pressed) }
    fn r(&mut self, pressed: bool) -> &mut Self { self.set(4, pressed) }

    fn start(&mut self, pressed: bool) -> &mut Self { self.set(12, pressed) }
    fn select(&mut self, pressed: bool) -> &mut Self { self.set(13, pressed) }

    fn up(&mut self, pressed: bool) -> &mut Self { self.set(11, pressed) }
    fn down(&mut self, pressed: bool) -> &mut Self { self.set(10, pressed) }
    fn left(&mut self, pressed: bool) -> &mut Self { self.set(9, pressed) }
    fn right(&mut self, pressed: bool) -> &mut Self { self.set(8, pressed) }
}

/// Should be implemented for all input implementations (such as controller, keyboard, perhaps
/// stdin or something like that).
trait InputSource {
    fn poll(&mut self) -> InputState;
}

/// Dummy input for when a controller is not used. Buttons are never pressed.
struct DummyInput;
impl InputSource for DummyInput {
    fn poll(&mut self) -> InputState { InputState::new() }
}
