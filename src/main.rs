//#![allow(dead_code, unused_variables)]    // FIXME Just for development

#[macro_use] extern crate log;
extern crate env_logger;

use std::fs::File;
use std::io::Read;

use cpu::Cpu;
use rom::Rom;

mod cpu;
mod rom;

fn main() {
    env_logger::init().unwrap();

    let filename = std::env::args().skip(1).next().expect("no rom file passed to command");
    let mut file = File::open(&filename).unwrap();
    let mut buf = Vec::new();
    file.read_to_end(&mut buf).unwrap();

    let rom = Rom::load(&buf).unwrap();

    let mut cpu = Cpu::new(rom);
    cpu.run();
}
