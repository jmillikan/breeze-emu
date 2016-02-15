#![deny(warnings)]
#![deny(unused_import_braces, unused_qualifications)]

#[macro_use] extern crate log;
extern crate clap;
extern crate env_logger;

#[macro_use] extern crate libsavestate;
extern crate breeze_core as breeze;
extern crate breeze_frontends as frontends;
extern crate breeze_frontend_api as frontend_api;

use std::env;
use std::fs::File;
use std::io::{BufReader, Read};

use libsavestate::SaveState;
use breeze::rom::Rom;
use breeze::snes::Snes;
use breeze::input::Input;

// FIXME Replace this hack with input detection
#[cfg(feature = "sdl")]
fn attach_default_input(input: &mut Input) {
    let ports = input.unwrap_ports();
    ports.0 = Some(Box::new(::frontend_api::input::joypad::Joypad::new(Box::new(::frontends::frontend_sdl::KeyboardInput))));
}
#[cfg(not(feature = "sdl"))]
fn attach_default_input(_: &mut Input) {}

fn main() {
    if env::var_os("RUST_LOG").is_none() {
        env::set_var("RUST_LOG", "breeze=INFO");
    }
    env_logger::init().unwrap();

    let args = clap::App::new("breeze")
        .version(env!("CARGO_PKG_VERSION"))
        .about("SNES emulator")
        .arg(clap::Arg::with_name("rom")
            .required(true)
            .value_name("ROM_PATH")
            .takes_value(true)
            .help("The ROM file to execute"))
        .arg(clap::Arg::with_name("renderer")
            .short("R")
            .long("renderer")
            .takes_value(true)
            .help("The renderer to use"))
        .arg(clap::Arg::with_name("savestate")
            .long("savestate")
            .takes_value(true)
            .help("The save state file to load"))
        .arg(clap::Arg::with_name("record")
            .long("record")
            .takes_value(true)
            .help("Record input to a text file"))
        .arg(clap::Arg::with_name("replay")
            .long("replay")
            .takes_value(true)
            .help("Replay a recording from a text file"))
    .get_matches();

    if args.value_of("record").is_some() && args.value_of("replay").is_some() {
        println!("`record` and `replay` may not be specified together!");
        return;
    }

    let renderer_name = args.value_of("renderer").unwrap_or(&*frontends::DEFAULT_RENDERER);

    let renderer_fn = match frontends::RENDERER_MAP.get(renderer_name) {
        None => {
            println!("error: unknown renderer: {}", renderer_name);
            println!("{} renderers known:", frontends::RENDERER_MAP.len());
            for (name, opt_fn) in frontends::RENDERER_MAP.iter() {
                println!("\t{}\t{}", name, match *opt_fn {
                    Some(_) => "available",
                    None => "not compiled in",
                });
            }

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

    let filename = args.value_of("rom").unwrap();
    let mut file = File::open(&filename).unwrap();
    let mut buf = Vec::new();
    file.read_to_end(&mut buf).unwrap();

    let rom = Rom::from_bytes(&buf).unwrap();
    let mut renderer = renderer_fn();
    if let Some(title) = rom.get_title() {
        renderer.set_rom_title(title);
    }

    let mut snes = Snes::new(rom, renderer);
    attach_default_input(snes.input_mut());
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
