//! Contains the emulator backend implementations.
//!
//! The "Backend" is everything that sits between the emulator core and the user: Video output
//! (Window management), Input handling, Audio output.

// TODO We could easily do render tests with a custom `Renderer`, and even supply scripted input
// with an `InputSource`

#![deny(warnings)]
#![deny(unused_import_braces, unused_qualifications)]

extern crate breeze_backend as backend_api;

#[macro_use] #[no_link] extern crate lazy_static;
#[macro_use] extern crate log;

#[cfg(feature = "glium")]
extern crate breeze_glium;

#[cfg(feature = "sdl")]
pub extern crate breeze_sdl;    // FIXME pub because of the input hack
#[cfg(feature = "sdl")]

use backend_api::{AudioSink, Renderer};
use backend_api::dummy::{DummyRenderer, DummySink};
pub use backend_api::viewport::{self, Viewport};

use std::collections::BTreeMap;
use std::error::Error;

pub type RendererMap = BTreeMap<&'static str, Option<fn() -> Result<Box<Renderer>, Box<Error>>>>;
pub type AudioMap = BTreeMap<&'static str, Option<fn() -> Result<Box<AudioSink>, Box<Error>>>>;

pub mod backend;

// FIXME I'd love to get rid of this... feels like too much magic.
macro_rules! make_fn {
    ( $tr:ident: #[cfg($m:meta)] $name:ident :: $tyname:ident ) => {{
        #[cfg($m)]
        fn $name() -> Result<Box<$tr>, Box<Error>> {
            <backend::$name::$tyname as $tr>::create().map(|r| Box::new(r) as Box<_>)
        }

        #[cfg(not($m))]
        fn $name() -> Result<Box<$tr>, Box<Error>> {
            unreachable!()
        }

        if cfg!($m) { Some($name as fn() -> Result<Box<$tr>, Box<Error>>) } else { None }
    }};
    ( $tr:ident: $name:ident :: $tyname:ident ) => {{
        fn $name() -> Result<Box<$tr>, Box<Error>> {
            <backend::$name::$tyname as $tr>::create().map(|r| Box::new(r) as Box<_>)
        }

        Some($name as fn() -> Result<Box<$tr>, Box<Error>>)
    }};
}

lazy_static! {
    pub static ref RENDERER_MAP: RendererMap = {
        fn make<R: Renderer + 'static>() -> Result<Box<Renderer>, Box<Error>> {
            R::create().map(|r| Box::new(r) as Box<_>)
        }

        type MapEntry = Option<fn() -> Result<Box<Renderer>, Box<Error>>>;

        // FIXME use cfg-if
        #[cfg(feature = "glium")]
        const BUILD_GLIUM: MapEntry = Some(make::<breeze_glium::GliumRenderer>);
        #[cfg(not(feature = "glium"))]
        const BUILD_GLIUM: MapEntry = None;

        #[cfg(feature = "sdl")]
        const BUILD_SDL: MapEntry = Some(make::<breeze_sdl::SdlRenderer>);
        #[cfg(not(feature = "sdl"))]
        const BUILD_SDL: MapEntry = None;

        let mut map = RendererMap::new();
        map.insert("glium", BUILD_GLIUM);
        map.insert("sdl", BUILD_SDL);
        map.insert("dummy", Some(make::<DummyRenderer>));
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
        fn make<A: AudioSink + 'static>() -> Result<Box<AudioSink>, Box<Error>> {
            A::create().map(|r| Box::new(r) as Box<_>)
        }

        let mut map = AudioMap::new();
        map.insert("cpal", make_fn!(AudioSink: #[cfg(feature = "cpal")] cpal::CpalAudio));
        map.insert("dummy", Some(make::<DummySink>));
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
