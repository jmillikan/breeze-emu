//! Dummy frontend that does nothing.

use frontend_api::{FrontendAction, Renderer, AudioSink};

/// Renderer that just does nothing.
#[derive(Default)]
pub struct DummyRenderer;

impl Renderer for DummyRenderer {
    fn render(&mut self, _frame_data: &[u8]) -> Option<FrontendAction> { None }
    fn set_rom_title(&mut self, _title: &str) {}
}

/// Dummy audio sink with no output
#[derive(Default)]
pub struct DummySink;

impl AudioSink for DummySink {
    fn write(&mut self, _data: &[(i16, i16)]) {}
}
