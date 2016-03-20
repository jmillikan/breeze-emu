//! Controller port abstraction layer
//!
//! This abstraction layer allows separating the bit-level operations performed by the CPU from the
//! higher-level operations that different types of peripherals must support.
//!
//! More specifically, this module defines all types of peripherals that can be plugged into the
//! controller ports, implements the low-level details of each peripheral, and invokes a few simple
//! methods provided by the frontend, which implements the actual input querying logic.
//!
//! This makes it very easy to implement new controller support in the frontend (for example,
//! `libinput` support), and yet is true to the hardware, since emulation is performed on a very low
//! level.

use frontend::input::joypad::{JoypadImpl, JoypadState};

/// Enumeration of things that can be plugged into a controller port.
pub enum Peripheral {
    /// The standard SNES joypad: A, B, X, Y, L, R, Start, Select, D-Pad
    Joypad {
        /// The actual implementation (provided by the frontend)
        imp: Box<JoypadImpl>,
        /// Current joypad state. When the latch is active, this is updated by asking the frontend
        /// for the current state.
        state: JoypadState,
    },

    // TODO: Mouse, Light Guns, etc.
}

use self::Peripheral::*;

/// Construction
impl Peripheral {
    /// Creates a new joypad peripheral using the given `JoypadImpl`.
    pub fn new_joypad(imp: Box<JoypadImpl>) -> Self {
        Joypad {
            imp: imp,
            state: JoypadState::new(),
        }
    }
}

/// CPU interface
impl Peripheral {
    /// Called with the value of the lowest bit written to `$4016`. When set to 1, the controller
    /// should latch its input (whatever that means is specific to the attached peripheral).
    ///
    /// Auto-joypad mode writes 1 and then 0 to the latch before reading data.
    pub fn set_latch(&mut self, latch: bool) {
        if latch {
            // FIXME This is flaky at best and doesn't emulate the hardware correctly: While latch
            // is active (not when writing to it), button input is written into the latch and the
            // clock is frozen, which means that the latch will return the first bit on every read.
            match *self {
                Joypad { ref mut imp, ref mut state } => {
                    *state = imp.update_state();
                }
            }
        }
    }

    /// Read a bit from the `Data1` and `Data2` lines. Called on serial reads either via Auto-Joypad
    /// mode or reads from `$4016`/`$4017`.
    ///
    /// Returns the bits on `Data1` and `Data2`, respectively. For Joypads, for example, `Data2`
    /// will always be `false`, since it's not connected.
    pub fn read_bit(&mut self) -> (bool, bool) {
        match *self {
            Joypad { ref mut state, .. } => {
                let bit = state.read_bit();

                // The Data2 line is always 0 (it's not used by single joypads)
                (bit, false)
            }
        }
    }

    /// Sets the bit written out to the `IOBit` line.
    ///
    /// This is called when the SNES writes to the highest 2 bits of `$4213`. (If these are set to
    /// 0, reads from `$4201` will always return 0. If these are set to 1, then reads from `$4201`
    /// will return whatever value is written to the respective `IOBit` lines.)
    pub fn set_io_bit(&mut self, _iobit: bool) {
        match *self {
            Joypad { .. } => {}
        }
    }

    /// Called on reads from `$4201` when the respective bit in `$4213` is set to 1 (if the bit in
    /// `$4213` is set to 0, all reads will return 0 and this method is not called).
    ///
    /// This should return the current status of the `IOBit` line.
    ///
    /// When using the `IOBit` line of port 2 to latch the PPU's H/V Counters, use
    /// `needs_hv_latch_control` and `update_hv_latch` *in addition* to this (the counters are
    /// latched when `IOBit` transitions from 1 to 0).
    pub fn read_io_bit(&mut self) -> bool {
        match *self {
            // FIXME: `IOBit` isn't connected. Does it read as true or false then?
            Joypad { .. } => true,
        }
    }

    /// This will be called on every pixel. When this method returns `true`, the PPU's H/V Counters
    /// will be latched.
    ///
    /// Note that the returned value is not returned on read from the I/O Port (`$4201`). You have
    /// to make sure that this method and `read_io_bit` return correct values.
    pub fn update_hv_latch(&mut self) -> bool {
        match *self {
            Joypad { .. } => false,
        }
    }

    /// Called once after every frame
    pub fn next_frame(&mut self) {
        match *self {
            Joypad { .. } => {},
        }
    }
}
