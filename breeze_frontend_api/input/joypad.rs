//! The standard joypad. The most important peripheral. Can be plugged into the Multitap, but that
//! isn't yet implemented or reflected here.

// FIXME Allow configuring left+right/up+down behaviour

/// (C-like) Enum of all Joypad buttons.
///
/// Discriminants are the button's bit numbers in `JoypadState` (the highest number will be read
/// first).
pub enum JoypadButton {
    A = 7,
    B = 15,
    X = 6,
    Y = 14,
    L = 5,
    R = 4,
    Start = 12,
    Select = 13,
    Up = 11,
    Left = 9,
    Down = 10,
    Right = 8,
}

/// State of a SNES joypad.
///
/// Bits (`HIGH | LOW`):
/// `B Y Select Start Up Down Left Right | A X L R 0 0 0 0`
#[derive(Clone, Copy)]
pub struct JoypadState(u16);

impl JoypadState {
    /// Creates a new `InputState` with no buttons pressed
    pub fn new() -> Self { JoypadState(0) }

    /// Set a button's state
    pub fn set(&mut self, button: JoypadButton, pressed: bool) -> &mut Self {
        match pressed {
            true => self.0 |= 1 << button as u8,
            false => self.0 &= !(1 << button as u8),
        };
        self
    }
}


/// A frontend implementation of a joypad-like peripheral. Provides methods for querying joypad
/// buttons.
pub trait JoypadImpl {
    /// Called to "latch" the current joypad state.
    fn update_state(&mut self) -> JoypadState;
}

/// A standard SNES Joypad.
pub struct Joypad {
    imp: Box<JoypadImpl>,
    state: JoypadState,
}

impl Joypad {
    pub fn new(imp: Box<JoypadImpl>) -> Self {
        Joypad {
            imp: imp,
            state: JoypadState::new(),
        }
    }
}

impl super::ControllerPortAttachment for Joypad {
    fn set_latch(&mut self, latch: bool) {
        if latch {
            self.state = self.imp.update_state();
        }
    }

    fn read_bit(&mut self) -> (bool, bool) {
        // We read the highest bit...
        let status = self.state.0 & 0x8000 != 0;
        // Shift the "shift register" to the right
        self.state.0 <<= 1;
        // And shift a 1 bit into the other end
        self.state.0 |= 1;

        (status, false)
    }

    fn set_io_bit(&mut self, _iobit: bool) {}
    // FIXME: `IOBit` isn't connected. Does it read as 1 then?
    fn read_io_bit(&mut self) -> bool { true }

    fn needs_hv_latch_control(&self) -> bool { false }
    fn update_hv_latch(&mut self) -> bool { false }

    fn next_frame(&mut self) {}
}
