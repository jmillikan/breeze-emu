#![deny(warnings)]
#![deny(unused_import_braces, unused_qualifications, unused_extern_crates)]

#[macro_use] extern crate clap;
#[macro_use] #[no_link] extern crate lazy_static;
#[macro_use] extern crate log;
extern crate env_logger;

#[macro_use] extern crate libsavestate;
extern crate breeze_core as breeze;
extern crate breeze_frontends as frontends;

use std::env;
use std::fs::File;
use std::io::{BufReader, Read};

use libsavestate::SaveState;
use breeze::rom::Rom;
use breeze::snes::Snes;

fn main() {
    if env::var_os("RUST_LOG").is_none() {
        env::set_var("RUST_LOG", "breeze=INFO");
    }
    env_logger::init().unwrap();

    let args = clap_app!(breeze =>
        (version: option_env!("CARGO_PKG_VERSION").unwrap_or("<unknown version>"))
        (about: "SNES emulator")
        (@arg ROM_PATH: +required "The ROM file to execute")
        (@arg renderer: -R --renderer +takes_value "The renderer to use")
        (@arg savestate: --savestate +takes_value "The save state file to load")
        (@arg record: --record +takes_value "Record input to a text file")
        (@arg replay: --replay +takes_value "Replay a recording from a text file")
    ).get_matches();

    if args.value_of("record").is_some() && args.value_of("replay").is_some() {
        println!("`record` and `replay` may not be specified together!");
        return;
    }

    let renderer_name = args.value_of("renderer").unwrap_or(&*frontends::DEFAULT_RENDERER);

    let renderer_fn = match frontends::RENDERER_MAP.get(renderer_name) {
        None => {
            println!("error: unknown renderer: {}", renderer_name);
            // FIXME Print list of known renderers
            return
        }
        Some(&None) => {
            println!("error: renderer '{}' not compiled in", renderer_name);
            println!("(compile with `cargo build --features {}` to enable)", renderer_name);
            // NOTE: Make sure that renderer name always matches feature name!
            return
        }
        Some(&Some(renderer_fn)) => {
            info!("using {} renderer", renderer_name);
            renderer_fn
        }
    };

    let filename = args.value_of("ROM_PATH").unwrap();
    let mut file = File::open(&filename).unwrap();
    let mut buf = Vec::new();
    file.read_to_end(&mut buf).unwrap();

    let rom = Rom::from_bytes(&buf).unwrap();

    let mut snes = Snes::new(rom, renderer_fn());
    if let Some(record_file) = args.value_of("record") {
        snes.input_mut().start_recording(Box::new(File::create(record_file).unwrap()));
    }
    if let Some(replay_file) = args.value_of("replay") {
        snes.input_mut().start_replay(Box::new(BufReader::new(File::open(replay_file).unwrap())));
    }
    if let Some(filename) = args.value_of("savestate") {
        snes.restore_state(&mut File::open(filename).unwrap()).unwrap()
    }
    snes.run();
}
