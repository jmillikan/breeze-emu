//! Dummy frontend that does nothing.

use frontend_api::{FrontendAction, Renderer};

/// Renderer that just does nothing.
#[derive(Default)]
pub struct DummyRenderer;

impl Renderer for DummyRenderer {
    fn render(&mut self, _frame_data: &[u8]) -> Option<FrontendAction> { None }
    fn set_rom_title(&mut self, _title: &str) {}
}
