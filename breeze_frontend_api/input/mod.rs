//! Input handling and traits

pub mod bitstream;
pub mod joypad;

// FIXME Implement all of this

/// A device that can be attached to a controller port directly on the SNES (*not* to a port on a
/// multitap).
///
/// Attached peripherals are not saved in save states. This is something to be aware of, since
/// loading a save state while input is configured wrongly will lead to games not working. This is
/// done to allow switching the emulator frontend while still being able to load save states made in
/// other configurations.
pub trait ControllerPortAttachment {
    // This has no default methods to make the exact workings of peripherals explicit.

    /// Called with the value of the lowest bit written to `$4016`. When set to 1, the controller
    /// should latch its input (whatever that means is specific to the attached peripheral).
    ///
    /// Auto-joypad mode writes 1 and then 0 to the latch before reading data.
    fn set_latch(&mut self, latch: bool);

    /// Read a bit from the `Data1` and `Data2` lines. Called on serial reads either via Auto-Joypad
    /// mode or reads from `$4016`/`$4017`.
    ///
    /// Returns the bits on `Data1` and `Data2`, respectively. For Joypads, for example, `Data2`
    /// will always be `false`, since it's not connected.
    fn read_bit(&mut self) -> (bool, bool);

    /// Sets the bit written out to the `IOBit` line.
    ///
    /// This is called when the SNES writes to the highest 2 bits of `$4213`. (If these are set to
    /// 0, reads from `$4201` will always return 0. If these are set to 1, then reads from `$4201`
    /// will return whatever value is written to the respective `IOBit` lines.)
    fn set_io_bit(&mut self, iobit: bool);

    /// Called on reads from `$4201` when the respective bit in `$4213` is set to 1 (otherwise, all
    /// reads will return 0).
    ///
    /// This should return the current status of the `IOBit` line.
    ///
    /// When using the `IOBit` line of port 2 to latch the PPU's H/V Counters, use
    /// `needs_hv_latch_control` and `update_hv_latch` *in addition* to this (the counters are
    /// latched when `IOBit` transitions from 1 to 0).
    fn read_io_bit(&mut self) -> bool;

    /// Return whether this peripheral uses its `IOBit` line to latch the PPU H/V Counters. This is
    /// (probably) true for all light guns. Note that this feature only works for peripherals
    /// plugged into the second port, since its `IOBit` line (and bit 7 of `$4201`) is connected to
    /// the H/V latch line of the PPU.
    ///
    /// This is called once on initialization.
    fn needs_hv_latch_control(&self) -> bool;

    /// If `needs_hv_latch_control` returned `true`, this will be called on every pixel. When this
    /// method returns `true`, the PPU's H/V Counters will be latched.
    ///
    /// Note that the returned value is not returned on read from the I/O Port (`$4201`). You have
    /// to make sure that this method and `read_io_bit` return correct values.
    fn update_hv_latch(&mut self) -> bool;
    // FIXME: Maybe remove the above 2 methods and rely on `read_io_bit`?

    /// Called once after every frame
    fn next_frame(&mut self);
}

#[test]
fn cpa_is_obj_safe() {
    let _: &ControllerPortAttachment;
}
