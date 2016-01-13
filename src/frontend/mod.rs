//! Contains the emulator frontend implementations.
//!
//! The "Frontend" is everything that sits between the emulator core and the user: Video output
//! (Window management), Input handling, Audio output.

// TODO We could easily do render tests with a custom `Renderer`, and even supply scripted input
// with an `InputSource`

pub use breeze::frontend::*;
pub use breeze::input::InputState;

use std::collections::HashMap;

mod dummy;
#[cfg(feature = "glium")]
mod glium;
#[cfg(feature = "sdl2")]
pub mod sdl;    // FIXME Make private after input handling is better

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
        map.insert("glium", make_fn!(#[cfg(feature = "glium")] glium::GliumRenderer));
        map.insert("sdl", make_fn!(#[cfg(feature = "sdl2")] sdl::SdlRenderer));
        map.insert("dummy", make_fn!(dummy::DummyRenderer));
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
