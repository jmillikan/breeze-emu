//! Dummy frontend that does nothing.

use super::{InputSource, Renderer};
use input::InputState;

/// Renderer that just does nothing.
pub struct DummyRenderer;

impl Renderer for DummyRenderer {
    fn render(&mut self, _frame_data: &[u8]) {}
}

/// Dummy input for when a controller is not used. Buttons are never pressed.
pub struct DummyInput;

impl InputSource for DummyInput {
    fn poll(&mut self) -> InputState { InputState::new() }
}
