//! Dummy renderer

use super::Renderer;

pub struct DummyRenderer;

impl Renderer for DummyRenderer {
    fn render(&mut self, frame_data: &[u8]) {}
}
