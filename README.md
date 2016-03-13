# A Super Nintendo emulator written in Rust

[![Build Status](https://travis-ci.org/jonas-schievink/breeze-emu.svg?branch=master)](https://travis-ci.org/jonas-schievink/breeze-emu)

[API Documentation](http://jonas-schievink.github.io/breeze-emu/breeze_core/)

## Hacking

Do you want to contribute? Great! Emulator development is really fun, but can also be frustrating. If you have any questions, don't hesitate to ask!

Keep the following things in mind when digging in the source code:
* Unoptimized Rust is slow, if you want to actually use the emulator, compile it in release mode (`cargo run --release -- <path-to-rom>`)
* My code might be horribly broken and I might be terribly misunderstanding how something works, so please bear with me
