//! The Breeze render test runner

#[macro_use]
extern crate rustasm6502;
extern crate term;
extern crate png;

extern crate breeze_core;
extern crate breeze_frontends;

use breeze_frontends::frontend::test::TestRenderer;
use breeze_frontends::frontend::dummy::DummySink;
use breeze_core::rom::Rom;
use breeze_core::snes::Emulator;

use term::stdout as term_stdout;
use term::color;

use std::ascii::AsciiExt;
use std::io::{self, Write, Read};
use std::fs::File;
use std::process;
use std::env;
use std::iter;

/// The test's data sections are separate from the code, and we can't do relocations (well, we
/// could, but I don't want to write a feature-complete linker), so we'll place the data at this
/// constant address.
///
/// (this is the address as seen by the CPU, ie. the mapped address)
const DATA_ADDRESS: (u8, u16) = (1, 0x8000);

// These are provided to the tests, and can be referenced *in* the assembly! Macro magic ftw!
const DATA_ADDRESS_BANK: u8 = DATA_ADDRESS.0;
const DATA_ADDRESS_LO: u8 = DATA_ADDRESS.1 as u8;
const DATA_ADDRESS_HI: u8 = (DATA_ADDRESS.1 >> 8) as u8;

/// Test code is placed at this address (CPU view)
const CODE_ADDRESS: u16 = 0x8000;

/// Meta data for render tests
pub struct Test {
    /// Number of frames to run this test before comparing output
    frames: u32,
    #[allow(dead_code)] // unused, but useful for explanatory purposes
    description: &'static str,
    preparation: &'static [Preparation],
    /// Aux. data file, relative to the test.rs
    data_file: Option<&'static str>,
    /// Machine code to run (required)
    code: &'static [u8],
}

/// Preparation to be done by the test suite before running the test's assembly.
///
/// Tests can specify any number of these, and each preparation will result in setup code included
/// in the built ROM.
#[derive(Debug)]
pub enum Preparation {
    /// Sets up the stack and initializes system registers to sane default values
    SystemInit,
    /// Upload the included font to the PPU
    UploadFont,
}
// TODO ^ these are unimplemented

/// Describes how a test has failed
struct TestFailure;

macro_rules! include_tests {
    ( $( $test:ident, )+ ) => {
        mod tests {
            use super::Test;
            use super::{DATA_ADDRESS_BANK, DATA_ADDRESS_HI, DATA_ADDRESS_LO};
            use super::CODE_ADDRESS;

            $(
                // Okay, `include!` is awesome! (and a bit scary)
                #[allow(non_upper_case_globals)]
                pub static $test: Test = include!(concat!("tests/", stringify!($test), "/test.rs"));
            )+
        }

        static TESTS: &'static [(&'static str, &'static Test)] = &[ $(
            (stringify!($test), &tests::$test),
        )+ ];
    };
}

include_tests! {
    dummy,
}

/// Iterates over all files and directories in the test directory and returns `Err(())` if any of
/// them are not included as tests.
fn check_missed_tests() -> Result<(), ()> {
    use std::fs::read_dir;

    let mut error = false;
    for entry in read_dir("rendertest/tests").unwrap() {
        let entry = entry.unwrap();
        let file_name = entry.file_name();
        if TESTS.iter().find(|&&(name, _)| file_name == *name).is_none() {
            println!("{} not included as a render test", file_name.to_string_lossy());
            error = true;
        }
    }

    match error {
        true => Err(()),
        false => Ok(()),
    }
}

/// Flushes stdout
fn flush() {
    io::stdout().flush().unwrap();
}

/// Prints `ok` or `FAILED`, depending on the passed value. Does not print a trailing newline.
fn print_success(success: bool) {
    let mut term = term_stdout().unwrap();
    if success {
        term.fg(color::GREEN).unwrap();
        print!("ok");
    } else {
        term.fg(color::RED).unwrap();
        print!("FAILED");
    }
    term.reset().unwrap();
}

fn build_rom(name: &str, test: &Test) -> Vec<u8> {
    // Let's put all code at $8000 (in Bank 0), the first mapped ROM area
    // Since we can't cross bank boundaries, this leaves $8000 Bytes for out code, including setup
    // (32 KiB - should be plenty)

    let mut code = Vec::new();

    // Apply preparations first, since they need to run first
    for prep in test.preparation {
        match *prep {
            // TODO
            _ => panic!("unimplemented preparation: {:?}", prep),
        }
    }
    code.extend_from_slice(test.code);

    let mut data = Vec::new();

    // Read and add the test's data (if any)
    if let Some(file) = test.data_file {
        let mut file = File::open(format!("rendertest/tests/{}/{}", name, file)).unwrap();
        file.read_to_end(&mut data).unwrap();
    }

    // TODO some preparations might need to add data (they'll need a separate bank!)

    // Build a LoROM image (this is done so the ROM loading code is tested as well - we could also
    // open up the `Rom` struct and directly use that)

    // Build the header
    let mut header = Vec::with_capacity(32);

    // First 21 Bytes: Title (ASCII)
    header.extend(name.chars()
                      .map(|c| c.to_ascii_uppercase() as u8)
                      .chain(iter::repeat(' ' as u8))
                      .take(21));

    header.push(0);     // ROM makeup Byte - LoROM, no FastROM
    header.push(0);     // Chipset (none/don't care)
    header.push(6);     // ROM size - $400<<6 = 64K bytes
    header.push(0);     // Cart. RAM size - $400 bytes
    header.push(0);     // Vendor code
    header.push(0);
    header.push(0);     // Version
    header.push(0x55);  // Checksum (invalid)
    header.push(0x55);
    header.push(0xAA);  // Checksum complement
    header.push(0xAA);
    // Extended header (ignored):

    assert_eq!(header.len(), 32);
    assert!(code.len() < 0x8000 - 64, "code size too high");

    // Now we can put the image together
    // The header is located (for LoROM) at `0x8000 - 64`, preceded by code that will be mapped to
    // 0x8000+, followed by the extended header, the interrupt vectors, and the data section(s)
    // (in our case)
    let mut rom = code.into_iter()
                      .chain(iter::repeat(0))
                      .take(0x8000 - 64)
                      .chain(header.into_iter())
                      .chain(iter::repeat(0))
                      .take(0x8000)
                      .chain(data.into_iter().chain(iter::repeat(0)).take(0x8000))
                      .collect::<Vec<_>>();

    // Set the correct vectors (emulation mode)
    // RESET @ 0x8000
    rom[0x7ffc] = 0x00;
    rom[0x7ffd] = 0x80;
    // This should now be a valid, runnable 64K ROM image (minus the checksum)

    rom
}

/// Run a render test
///
/// To run a test, a few things need to happen in preparation: Test code and data is separated, so
/// we need to link them to produce a loadable ROM image. `DATA_ADDRESS` specifies the start of the
/// data section. It is used as a symbolic constant in the code (see docs around `DATA_ADDRESS`), so
/// we don't need to relocate.
///
/// The code also doesn't specify any interrupt vectors, which we'll do as well (defining custom
/// interrupt handlers isn't currently doable, but can be implemented quite nicely).
///
/// Later, tests might want to make use of some shared setup code, which would be inserted in here
/// too.
///
/// Relevant vectors:
/// * IRQB/BRK: 00FFFE,F
/// * RESETB: 00FFFC,D
/// * NMIB: 00FFFA,B
fn run_test(name: &str, test: &Test) -> Result<(), TestFailure> {
    let rom = build_rom(name, test);

    let rom = Rom::from_bytes(&rom).unwrap();
    let renderer = TestRenderer::new(test.frames);

    let mut emu = Emulator::new(rom, renderer, Box::new(DummySink::default()));
    emu.run();

    let mut exp_data = Vec::new();
    let mut exp_file = File::open(format!("rendertest/tests/{}/expected.png", name)).unwrap();
    exp_file.read_to_end(&mut exp_data).unwrap();
    let (info, mut reader) = png::Decoder::new(exp_data.as_slice()).read_info().unwrap();

    // sanity checks on image (should match what we test against)
    assert_eq!(info.color_type, png::ColorType::RGB);
    assert_eq!(info.bit_depth, png::BitDepth::Eight);
    assert_eq!(info.width, 256);    // FIXME use PPU's constants
    assert_eq!(info.height, 224);

    // decode expected image into `exp_frame` - given the above sanity checks, it should have the
    // same format the PPU emulation uses internally
    let mut exp_frame = vec![0; info.buffer_size()];
    reader.next_frame(&mut exp_frame).unwrap();

    let got_frame = emu.renderer.last_frame();
    // we assert that the lengths match, since that's an issue with the expected image file
    assert_eq!(got_frame.len(), exp_frame.len());

    if exp_frame == got_frame {
        Ok(())
    } else {
        Err(TestFailure)
    }
}

fn main() {
    if check_missed_tests().is_err() {
        process::exit(1);
    }

    // Enable verbose debug logging for the emulator
    // This is okay, since we capture output anyways
    if env::var_os("RUST_LOG").is_none() {
        env::set_var("RUST_LOG", "breeze=DEBUG");
    }

    println!("");
    print!("running {} rendering test", TESTS.len());
    if TESTS.len() != 1 { print!("s"); }
    println!("");
    let mut failed_names = Vec::new();
    for &(name, ref test) in TESTS {
        print!("test {} ... ", name);
        flush();

        let result = run_test(name, test);

        if result.is_err() {
            failed_names.push(name);
        }
        print_success(result.is_ok());
        println!("");
    }

    // TODO print test failures

    println!("");
    print!("test result: ");
    print_success(failed_names.is_empty());
    println!(". {} passed; {} failed", TESTS.len() - failed_names.len(), failed_names.len());
    println!("");
}
