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
/// Bits (`HIGH | LOW`, returned on Data1 from high to low, or left to right):
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

    /// Reads a bit from the state, as if the state would be stored inside the joypads shift
    /// register. This shifts the state to the left and inserts a 1-bit at the right side.
    pub fn read_bit(&mut self) -> bool {
        // We read the highest bit...
        let status = self.0 & 0x8000 != 0;
        // Shift the "shift register" to the left
        self.0 <<= 1;
        // And shift a 1 bit into the other end
        self.0 |= 1;
        status
    }
}

/// Trait for joypad implementations.
///
/// This should be implemented by the frontend, and is a simple abstraction from the bit-level
/// trickery at the controller port made for joysticks.
pub trait JoypadImpl {
    /// Called to "latch" the current joypad state.
    ///
    /// This should check and return the current state of the joypad.
    fn update_state(&mut self) -> JoypadState;
}
