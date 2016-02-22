//! Render test frontend

use frontend_api::{FrontendAction, Renderer};

/// A testing renderer which saves the last frame rendered.
#[derive(Default)]
pub struct TestRenderer {
    last_frame: Option<Vec<u8>>,
}

impl TestRenderer {
    /// Returns the last frame rendered (as returned by the PPU), or `None` if no frames were
    /// rendered yet.
    pub fn last_frame(&self) -> Option<&[u8]> {
        self.last_frame
    }
}

impl Renderer for TestRenderer {
    fn render(&mut self, frame_data: &[u8]) -> Option<FrontendAction> {
        self.last_frame = Some(frame_data.to_vec());
    }
    fn set_rom_title(&mut self, _title: &str) {}
}
