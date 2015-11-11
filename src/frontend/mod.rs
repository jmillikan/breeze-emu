//! Contains the emulator frontend implementations.

// TODO We could easily do render tests with a custom `Renderer`, and even supply scripted input
// with an `InputSource`

use input::InputState;

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

/// An action that can be performed by the user, is detected by the frontend and executed by the
/// emulator core.
#[allow(dead_code)] // Variants may be dead, depending on which frontends are enabled
pub enum FrontendAction {
    /// Exit the emulator
    Exit,
    /// Create a save state
    SaveState,
    /// Restore the last save state
    LoadState,
}

/// Type returned by frontend methods on `Renderer` and `InputSource`.
#[derive(Default)]
pub struct FrontendResult<T> {
    pub result: T,
    /// An optional action to perform
    pub action: Option<FrontendAction>,
}

/// Trait for screen renderers. Once per frame, they are given the raw screen data produced by the
/// PPU and can then render this content in a frontend-specific way.
pub trait Renderer {
    /// Render a frame produced by the PPU. For optimal experience, the `Renderer` implementation
    /// should make sure that the frame is visible as soon as possible.
    ///
    /// The passed `frame_data` contains `RGB24` data: For each pixel, R, G and B values are stored
    /// in order, one byte per channel, then for the next pixel (left to right, top line to bottom
    /// line).
    ///
    /// The renderer is also responsible for timing: It should only return to the caller when
    /// another frame should be rendered. This also affects input latency: As soon as the `render`
    /// method returns, the input devices can be queried by the running program. This allows
    /// intricate timing mechanisms for better input latency and makes support for dynamic refresh
    /// easier. If the renderer returns immediately, the emulator will run at maximum speed.
    fn render(&mut self, frame_data: &[u8]) -> Option<FrontendAction>;
}

/// Should be implemented for all input implementations (such as controller, keyboard, perhaps
/// stdin or something like that).
pub trait InputSource {
    /// Poll the input state. For synchronization purposes, we guarantee that this method is called
    /// exactly once per frame. However, the exact time within a frame is left unspecified (we might
    /// want to call this function as late as possible to optimize input latency).
    fn poll(&mut self) -> FrontendResult<InputState>;
}
