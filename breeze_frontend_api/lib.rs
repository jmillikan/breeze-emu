#[macro_use] extern crate libsavestate;

pub mod input;
pub mod ppu;

use input::InputState;

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
