# A Super Nintendo emulator written in Rust

<sup>(**very** work in progress)

[![Build Status](https://travis-ci.org/jonas-schievink/breeze-emu.svg?branch=master)](https://travis-ci.org/jonas-schievink/breeze-emu)

[API Documentation](http://jonas-schievink.github.io/breeze-emu/breeze_core/)

## Building and Running

Breeze is very slow at the moment, so I recommend running it in `--release` mode:

    cargo run --release -- <path to rom>

Currently, only keyboard input is supported:

```
Q W          I O P
A S D  G H   K L
```
Maps to:
```
L ↑           Y X R
< ↓ > Sel Sta B A
```

## License

This project is licensed under either of

* MIT license ([LICENSE-MIT](LICENSE-MIT) or https://opensource.org/licenses/MIT)
* Apache License 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or https://www.apache.org/licenses/LICENSE-2.0)

at your option.

## Contributions

See [CONTRIBUTING.md](CONTRIBUTING.md) for more info.

Unless You explicitly state otherwise, any Contribution intentionally submitted for inclusion in the Work by You to the Licensor shall be dual licensed as above, without any additional terms or conditions.
