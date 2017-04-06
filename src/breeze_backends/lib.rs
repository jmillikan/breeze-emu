//! Contains the emulator backend implementations.
//!
//! The "Backend" is everything that sits between the emulator core and the user: Video output
//! (Window management), Input handling, Audio output.

// TODO We could easily do render tests with a custom `Renderer`, and even supply recorded input

#![deny(warnings)]
#![deny(unused_import_braces, unused_qualifications)]

extern crate breeze_backend;

#[macro_use] extern crate lazy_static;

#[cfg(feature = "glium")]
extern crate breeze_glium;

#[cfg(feature = "sdl")]
pub extern crate breeze_sdl;    // FIXME pub because of the input hack

#[cfg(feature = "cpal")]
extern crate breeze_cpal;

use breeze_backend::{AudioSink, Renderer};
use breeze_backend::dummy::{DummyRenderer, DummySink};
pub use breeze_backend::viewport::{self, Viewport};

use std::collections::BTreeMap;
use std::error::Error;

pub type RendererMap = BTreeMap<&'static str, Option<fn() -> Result<Box<Renderer>, Box<Error>>>>;
pub type AudioMap = BTreeMap<&'static str, Option<fn() -> Result<Box<AudioSink>, Box<Error>>>>;

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

        type MapEntry = Option<fn() -> Result<Box<AudioSink>, Box<Error>>>;

        #[cfg(feature = "cpal")]
        const BUILD_CPAL: MapEntry = Some(make::<breeze_cpal::CpalAudio>);
        #[cfg(not(feature = "cpal"))]
        const BUILD_CPAL: MapEntry = None;

        let mut map = AudioMap::new();
        map.insert("cpal", BUILD_CPAL);
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
