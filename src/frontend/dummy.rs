//! Dummy frontend that does nothing.

use super::Renderer;

/// Renderer that just does nothing.
pub struct DummyRenderer;

impl Renderer for DummyRenderer {
    fn render(&mut self, _frame_data: &[u8]) {}
}
