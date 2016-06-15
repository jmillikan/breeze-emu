# Contributing

Do you want to contribute? Great! Emulator development is really fun, but can also be frustrating. If you have any questions, don't hesitate to ask!

Keep the following things in mind when digging in the source code:
* Unoptimized Rust is slow, if you want to actually use the emulator, compile it in release mode (`cargo run --release -- <path-to-rom>`)
* My code might be horribly broken and I might be terribly misunderstanding how something works, so please bear with me
* SNES documentation is often hard to find, incomplete or just plain wrong, so always check against other documents
* Please don't cheat by looking at other emulators (this is also questionable from a licensing perspective)

## Project structure

This project is split up into multiple crates for reusability and to make compile times better (with success, I might add):
* `spc700`: Contains the APU emulation (the SPC700 co-processor and the DSP).
* `wdc65816`: A reusable (but incomplete) WDC 65816 emulator, the CPU used in the SNES.
* `libsavestate`: A library for emulator save states. Provides a few traits, impls and macros to help implementing save states. They basically work like binary (de-)serialization, but can overwrite existing values (while leaving parts of them intact)
* `byte_array`: A small utility crate that provides a macro to allow easily implementing `[u8; N]` newtypes which implement several traits. These arrays are frequently used for the numerous types of memory inside the SNES.
* `breeze_core`: This is the heart of this project. This crate contains the PPU emulation, DMA routines, ROM loading code, and the main emulation coordination.
* `breeze_backend`: Contains traits used by `breeze_core`, which must be provided by the backend. Also contains a dummy implementation of these. Putting this in another crate allows parallel and independent compilation of `breeze_core` and `breeze_backends`.
* `breeze_backends`: Contains backend implementations. Currently, backends only handle controller input, rendering and window creation, but will eventually handle audio device access as well.
* `breeze`: A small CLI backend that invokes the main emulator. This is what you'll use to actually run this thing.
