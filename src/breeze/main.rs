#![deny(warnings)]
#![deny(unused_import_braces, unused_qualifications)]

#[macro_use] extern crate log;
extern crate clap;
extern crate env_logger;

extern crate breeze_core;
extern crate breeze_backends;
extern crate breeze_backend;

mod input;

use input::attach_default_input;

use breeze_core::rom::Rom;
use breeze_core::snes::Emulator;
use breeze_core::save::SaveStateFormat;
use breeze_core::record::{RecordingFormat, create_recorder, create_replayer};
use breeze_backend::Renderer;

use clap::ArgMatches;

use std::env;
use std::error::Error;
use std::fs::File;
use std::io::{BufReader, Read};
use std::process;


fn process_args(args: &ArgMatches) -> Result<(), Box<Error>> {
    if args.value_of("record").is_some() && args.value_of("replay").is_some() {
        return Err("`record` and `replay` may not be specified together!".into());
    }

    let renderer_name = args.value_of("renderer").unwrap_or(&breeze_backends::DEFAULT_RENDERER);

    let renderer_fn = match breeze_backends::RENDERER_MAP.get(renderer_name) {
        None => {
            let mut message = format!("unknown renderer: {}\n", renderer_name);
            message.push_str(&format!("{} renderers known:\n",
                breeze_backends::RENDERER_MAP.len()));

            for (name, opt_fn) in breeze_backends::RENDERER_MAP.iter() {
                message.push_str(&format!("\t{}\t{}\n", name, match *opt_fn {
                    Some(_) => "available",
                    None => "not compiled in",
                }));
            }

            return Err(message.into());
        }
        Some(&None) => {
            let mut message = format!("renderer '{}' not compiled in", renderer_name);
            message.push_str(&format!("(compile with `cargo build --features {}` to enable)",
                renderer_name));
            // NOTE: Make sure that renderer name always matches feature name!

            return Err("exiting".into());
        }
        Some(&Some(renderer_fn)) => {
            renderer_fn
        }
    };

    let audio_name = args.value_of("audio").unwrap_or(&breeze_backends::DEFAULT_AUDIO);
    let audio_fn = match breeze_backends::AUDIO_MAP.get(audio_name) {
        None => {
            let mut message = format!("unknown audio sink: {}\n", audio_name);
            message.push_str(&format!("{} audio sinks known:\n", breeze_backends::AUDIO_MAP.len()));

            for (name, opt_fn) in breeze_backends::AUDIO_MAP.iter() {
                message.push_str(&format!("\t{}\t{}\n", name, match *opt_fn {
                    Some(_) => "available",
                    None => "not compiled in",
                }));
            }

            return Err(message.into());
        }
        Some(&None) => {
            let mut message = format!("audio backend '{0}' not compiled in\n", audio_name);
            message.push_str(&format!("(compile with `cargo build --features {0}` to enable)",
                audio_name));
            // NOTE: Make sure that audio sink name always matches feature name!

            return Err(message.into());
        }
        Some(&Some(audio_fn)) => {
            audio_fn
        }
    };

    // Load the ROM into memory
    let filename = args.value_of("rom").unwrap();
    let mut file = try!(File::open(&filename));
    let mut buf = Vec::new();
    try!(file.read_to_end(&mut buf));

    let rom = try!(Rom::from_bytes(&buf));

    // Create the backend parts
    info!("using {} renderer", renderer_name);
    let mut renderer = try!(renderer_fn());
    if let Some(title) = rom.get_title() {
        renderer.set_rom_title(title);
    }

    info!("using {} audio sink", audio_name);
    let audio = try!(audio_fn());

    // Put everything together in the emulator
    let mut emu = Emulator::new(rom, renderer, audio);
    attach_default_input(&mut emu.peripherals_mut().input, renderer_name);

    if let Some(record_file) = args.value_of("record") {
        let writer = Box::new(File::create(record_file).unwrap());
        let recorder = create_recorder(RecordingFormat::default(), writer, &emu.snes).unwrap();
        emu.peripherals_mut().input.start_recording(recorder);
    }
    if let Some(replay_file) = args.value_of("replay") {
        let reader = Box::new(BufReader::new(File::open(replay_file).unwrap()));
        let replayer = create_replayer(RecordingFormat::default(), reader, &emu.snes).unwrap();
        emu.peripherals_mut().input.start_replay(replayer);
    }
    if let Some(filename) = args.value_of("savestate") {
        let file = File::open(filename).unwrap();
        let mut bufrd = BufReader::new(file);
        emu.snes.restore_save_state(SaveStateFormat::default(), &mut bufrd).unwrap()
    }

    if cfg!(debug_assertions) && args.is_present("oneframe") {
        debug!("PPU H={}, V={}",
            emu.peripherals().ppu.h_counter(),
            emu.peripherals().ppu.v_counter());
        try!(emu.snes.render_frame(|_framebuf| Ok(vec![])));

        info!("frame rendered. pausing emulation.");

        // Keep rendering, but don't run emulation
        // Copy out the frame buffer because the damn borrow checker doesn't like it otherwise
        let framebuf = emu.peripherals().ppu.framebuf.clone();
        loop {
            let actions = try!(emu.renderer.render(&*framebuf));
            for a in actions {
                if emu.handle_action(a) { break }
            }
        }
    } else {
        // Run normally
        try!(emu.run());
    }

    Ok(())
}

fn main() {
    if env::var_os("RUST_LOG").is_none() {
        env::set_var("RUST_LOG", "breeze=INFO");
    }
    env_logger::init().unwrap();

    let mut app = clap::App::new("breeze")
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
        .arg(clap::Arg::with_name("audio")
            .short("A")
            .long("audio")
            .takes_value(true)
            .help("The audio backend to use"))
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
            .help("Replay a recording from a text file"));

    // Add debugging options
    if cfg!(debug_assertions) {
        app = app.arg(clap::Arg::with_name("oneframe")
            .long("oneframe")
            .help("Render a single frame, then pause"));
    }

    let args = app.get_matches();
    match process_args(&args) {
        Ok(()) => {},
        Err(e) => {
            // FIXME: Glium swallows useful information when using {} instead of {:?}
            // I should fix this upstream when I have time.
            debug!("error: {:?}", e);
            println!("error: {}", e);
            process::exit(1);
        }
    }
}
