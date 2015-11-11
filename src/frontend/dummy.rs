//! Dummy frontend that does nothing.

use super::*;

/// Renderer that just does nothing.
#[derive(Default)]
pub struct DummyRenderer;

impl Renderer for DummyRenderer {
    fn render(&mut self, _frame_data: &[u8]) -> Option<FrontendAction> { None }
}
