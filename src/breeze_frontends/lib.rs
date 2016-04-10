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

pub type RendererMap = BTreeMap<&'static str, Option<fn() -> Box<Renderer>>>;
pub type AudioMap = BTreeMap<&'static str, Option<fn() -> Box<AudioSink>>>;

#[allow(dead_code)]
mod viewport;

pub mod frontend;

macro_rules! make_fn {
    ( $tr:ty: #[cfg($m:meta)] $name:ident :: $tyname:ident ) => {{
        #[cfg($m)]
        fn $name() -> Box<$tr> {
            Box::new(frontend::$name::$tyname::default())
        }

        #[cfg(not($m))]
        fn $name() -> Box<$tr> {
            unreachable!()
        }

        if cfg!($m) { Some($name as fn() -> Box<$tr>) } else { None }
    }};
    ( $tr:ty: $name:ident :: $tyname:ident ) => {{
        fn $name() -> Box<$tr> {
            Box::new(frontend::$name::$tyname::default())
        }

        Some($name as fn() -> Box<$tr>)
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
