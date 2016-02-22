//! The Breeze render test runner

#[macro_use]
extern crate rustasm6502;
extern crate term;
extern crate breeze_core;
extern crate breeze_frontends;

use term::stdout as term_stdout;
use term::color;

use std::io::{self, Write, Read};
use std::fs::File;
use std::process;

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

/// Meta data for render tests
pub struct Test {
    /// Number of frames to run this test before comparing output
    #[allow(dead_code)] // TODO actually run the tests
    frames: u64,
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
fn run_test(name: &'static str, test: &Test) -> Result<(), TestFailure> {
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

    Ok(())//unimplemented!()    // TODO
}

fn main() {
    if check_missed_tests().is_err() {
        process::exit(1);
    }

    println!("");
    println!("running {} rendering tests", TESTS.len());
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
