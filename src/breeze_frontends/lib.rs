//! Contains the emulator frontend implementations.
//!
//! The "Frontend" is everything that sits between the emulator core and the user: Video output
//! (Window management), Input handling, Audio output.

// TODO We could easily do render tests with a custom `Renderer`, and even supply scripted input
// with an `InputSource`

#![deny(warnings)]
#![deny(unused_import_braces, unused_qualifications)]

extern crate breeze_frontend_api as frontend_api;

#[macro_use] #[no_link] extern crate lazy_static;
#[macro_use] extern crate log;

#[cfg(feature = "sdl2")]
extern crate sdl2;

#[cfg(feature = "glium")]
#[macro_use] extern crate glium;

#[cfg(feature = "cpal")]
extern crate cpal;

use frontend_api::{AudioSink, Renderer};

use std::collections::BTreeMap;
use std::error::Error;

pub type RendererMap = BTreeMap<&'static str, Option<fn() -> Result<Box<Renderer>, Box<Error>>>>;
pub type AudioMap = BTreeMap<&'static str, Option<fn() -> Result<Box<AudioSink>, Box<Error>>>>;

#[allow(dead_code)]
mod viewport;

pub mod frontend;

macro_rules! make_fn {
    ( $tr:ident: #[cfg($m:meta)] $name:ident :: $tyname:ident ) => {{
        #[cfg($m)]
        fn $name() -> Result<Box<$tr>, Box<Error>> {
            <frontend::$name::$tyname as $tr>::create().map(|r| Box::new(r) as Box<_>)
        }

        #[cfg(not($m))]
        fn $name() -> Result<Box<$tr>, Box<Error>> {
            unreachable!()
        }

        if cfg!($m) { Some($name as fn() -> Result<Box<$tr>, Box<Error>>) } else { None }
    }};
    ( $tr:ident: $name:ident :: $tyname:ident ) => {{
        fn $name() -> Result<Box<$tr>, Box<Error>> {
            <frontend::$name::$tyname as $tr>::create().map(|r| Box::new(r) as Box<_>)
        }

        Some($name as fn() -> Result<Box<$tr>, Box<Error>>)
    }};
}

lazy_static! {
    pub static ref RENDERER_MAP: RendererMap = {
        let mut map = RendererMap::new();
        map.insert("glium", make_fn!(Renderer: #[cfg(feature = "glium")] glium::GliumRenderer));
        map.insert("sdl", make_fn!(Renderer: #[cfg(feature = "sdl2")] sdl::SdlRenderer));
        map.insert("dummy", make_fn!(Renderer: dummy::DummyRenderer));
        map
    };

    pub static ref DEFAULT_RENDERER: &'static str = {
        if cfg!(feature = "glium") {
            "glium"
        } else if cfg!(feature = "sdl2") {
            "sdl"
        } else {
            "dummy" // let's hope nobody does this by accident
        }
    };

    pub static ref AUDIO_MAP: AudioMap = {
        let mut map = AudioMap::new();
        map.insert("cpal", make_fn!(AudioSink: #[cfg(feature = "cpal")] cpal::CpalAudio));
        map.insert("dummy", make_fn!(AudioSink: dummy::DummySink));
        map
    };

    pub static ref DEFAULT_AUDIO: &'static str = {
        if cfg!(feature = "cpal") {
            "cpal"
        } else {
            "dummy"
        }
    };
}
