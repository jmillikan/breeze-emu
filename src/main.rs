#![allow(dead_code)]    // FIXME Just for development

#[macro_use] extern crate lazy_static;
#[macro_use] extern crate log;
extern crate env_logger;
extern crate sdl2;
extern crate arrayvec;

#[cfg(feature = "glium")]
#[macro_use]
extern crate glium;

use std::env;
use std::fs::File;
use std::io::Read;

use rom::Rom;
use snes::Snes;

#[macro_use] mod byte_array;
#[macro_use] mod log_util;
mod apu;
mod cpu;
mod dma;
mod frontend;
mod input;
mod ppu;
mod rom;
mod snes;

fn main() {
    if env::var_os("RUST_LOG").is_none() {
        env::set_var("RUST_LOG", "sneeze=INFO");
    }
    env_logger::init().unwrap();

    let filename = std::env::args().skip(1).next().expect("no rom file specified");
    let mut file = File::open(&filename).unwrap();
    let mut buf = Vec::new();
    file.read_to_end(&mut buf).unwrap();

    let rom = Rom::from_bytes(&buf).unwrap();

    let renderer_name = &*frontend::DEFAULT_RENDERER;
    info!("using {} renderer", renderer_name);

    let renderer = frontend::RENDERER_MAP.get(renderer_name)
        .expect("default renderer has no entry in renderer map")()
        .expect("default renderer not compiled in");
    let mut snes = Snes::new(rom, renderer);
    snes.run();
}
