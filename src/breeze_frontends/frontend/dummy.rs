//! Dummy frontend that does nothing.

use frontend_api::{FrontendAction, FrontendResult, Renderer, AudioSink};
use frontend_api::ppu::PixelData;

/// Renderer that just does nothing, apart from saving the PPU output for later use. This allows
/// users to extract single rendered frames without having to implement `Renderer`.
pub struct DummyRenderer {
    last_frame: Vec<u8>,
}

impl DummyRenderer {
    pub fn last_frame(&self) -> &[u8] {
        &self.last_frame
    }
}

impl Renderer for DummyRenderer {
    fn create() -> FrontendResult<Self> where Self: Sized {
        Ok(DummyRenderer {
            last_frame: Vec::new(),
        })
    }

    fn render(&mut self, frame: &[u8], _aux: &[PixelData]) -> FrontendResult<Vec<FrontendAction>> {
        self.last_frame.clear();
        self.last_frame.extend_from_slice(frame);
        Ok(vec![])
    }

    fn set_rom_title(&mut self, _title: &str) {}
}

/// Dummy audio sink with no output
pub struct DummySink;

impl AudioSink for DummySink {
    fn create() -> FrontendResult<Self> { Ok(DummySink) }
    fn write(&mut self, _data: &[(i16, i16)]) {}
}
