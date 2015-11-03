//! Emulates the controller ports `$4016 - $401f`

#![allow(dead_code)]    // FIXME: Implement an input frontend that uses this

use frontend::{InputSource};

/// Controller input management.
#[derive(Default)]
pub struct Input {
    sources: [Option<Box<InputSource>>; 4],
    states: [InputState; 4],
    /// Reset on frame render, set on lazy input update. Lazy input update is done the first time
    /// the game reads any controller state in the current frame and is supposed to improve input
    /// latency.
    updated_this_frame: bool,

    /// Bit in the input state (where 0 = MSb, 15 = LSb) returned by next $4016/$4017 read resp.
    bitpos4016: u8,
    bitpos4017: u8,
}

impl Input {
    /// Called on V-Blank. Ensures that `update` was called this frame, and enables lazy input
    /// update for the following frame.
    pub fn new_frame(&mut self) {
        if !self.updated_this_frame { self.update(); }
        self.updated_this_frame = false;
    }

    /// Polls all controllers and stores their state. Called lazily when the game reads from an
    /// input register.
    fn update(&mut self) {
        self.updated_this_frame = true;
        for i in 0..4 {
            if let Some(ref mut source) = self.sources[i] {
                self.states[i] = source.poll();
            }
        }
    }

    /// Read from an input register. Updates the controller state if this is the first load in this
    /// frame.
    pub fn load(&mut self, reg: u16) -> u8 {
        // Do we still need to fetch this frame's input data?
        if !self.updated_this_frame { self.update(); }

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
pub struct InputState(u16);

impl InputState {
    pub fn new() -> Self { Self::default() }

    fn set(&mut self, bit: u8, value: bool) -> &mut Self {
        match value {
            true => self.0 |= 1 << bit,
            false => self.0 &= !(1 << bit),
        };
        self
    }

    pub fn a(&mut self, pressed: bool) -> &mut Self      { self.set(7, pressed) }
    pub fn b(&mut self, pressed: bool) -> &mut Self      { self.set(15, pressed) }
    pub fn x(&mut self, pressed: bool) -> &mut Self      { self.set(6, pressed) }
    pub fn y(&mut self, pressed: bool) -> &mut Self      { self.set(14, pressed) }

    pub fn l(&mut self, pressed: bool) -> &mut Self      { self.set(5, pressed) }
    pub fn r(&mut self, pressed: bool) -> &mut Self      { self.set(4, pressed) }

    pub fn start(&mut self, pressed: bool) -> &mut Self  { self.set(12, pressed) }
    pub fn select(&mut self, pressed: bool) -> &mut Self { self.set(13, pressed) }

    pub fn up(&mut self, pressed: bool) -> &mut Self     { self.set(11, pressed) }
    pub fn down(&mut self, pressed: bool) -> &mut Self   { self.set(10, pressed) }
    pub fn left(&mut self, pressed: bool) -> &mut Self   { self.set(9, pressed) }
    pub fn right(&mut self, pressed: bool) -> &mut Self  { self.set(8, pressed) }
}
