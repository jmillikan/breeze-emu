//! Contains the emulator frontend implementations.
//!
//! The "Frontend" is everything that sits between the emulator core and the user: Video output
//! (Window management), Input handling, Audio output.

// TODO We could easily do render tests with a custom `Renderer`, and even supply scripted input
// with an `InputSource`

extern crate breeze_frontend_api as frontend_api;

#[macro_use] #[no_link] extern crate lazy_static;
#[macro_use] extern crate log;

#[cfg(feature = "sdl2")]
extern crate sdl2;

#[cfg(feature = "glium")]
#[macro_use] extern crate glium;

pub use frontend_api::*;
pub use frontend_api::input::InputState;

use std::collections::HashMap;

mod frontend_dummy;
#[cfg(feature = "glium")]
mod frontend_glium;
#[cfg(feature = "sdl2")]
pub mod frontend_sdl;    // FIXME Make private after input handling is better

lazy_static! {
    pub static ref RENDERER_MAP: HashMap<&'static str, Option<fn() -> Box<Renderer>>> = {
        macro_rules! make_fn {
            ( #[cfg($m:meta)] $name:ident :: $tyname:ident ) => {{
                #[cfg($m)]
                fn $name() -> Box<Renderer> {
                    Box::new($name::$tyname::default())
                }

                #[cfg(not($m))]
                fn $name() -> Box<Renderer> {
                    unreachable!()
                }

                if cfg!($m) { Some($name as fn() -> Box<Renderer>) } else { None }
            }};
            ( $name:ident :: $tyname:ident ) => {{
                fn $name() -> Box<Renderer> {
                    Box::new($name::$tyname::default())
                }

                Some($name as fn() -> Box<Renderer>)
            }};
        }

        let mut map = HashMap::new();
        map.insert("glium", make_fn!(#[cfg(feature = "glium")] frontend_glium::GliumRenderer));
        map.insert("sdl", make_fn!(#[cfg(feature = "sdl2")] frontend_sdl::SdlRenderer));
        map.insert("dummy", make_fn!(frontend_dummy::DummyRenderer));
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
}
