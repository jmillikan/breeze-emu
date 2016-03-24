#![deny(warnings)]
#![deny(unused_import_braces, unused_qualifications, unused_extern_crates)]

#[macro_use] extern crate log;

#[macro_use] #[no_link] extern crate byte_array;
#[macro_use] extern crate libsavestate;
extern crate wdc65816 as cpu;
extern crate spc700 as apu;
extern crate breeze_frontend_api as frontend;

#[macro_use] mod log_util;
mod dma;
mod flexvec;
pub mod record;
pub mod ppu;
pub mod input;
pub mod rom;
pub mod save;
pub mod snes;
