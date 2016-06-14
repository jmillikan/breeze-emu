//! Dummy frontend that does nothing.

use frontend_api::{FrontendAction, Renderer, AudioSink};

use std::error::Error;

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
    fn create() -> Result<Self, Box<Error>> where Self: Sized {
        Ok(DummyRenderer {
            last_frame: Vec::new(),
        })
    }

    fn render(&mut self, frame_data: &[u8]) -> Option<FrontendAction> {
        self.last_frame.clear();
        self.last_frame.extend_from_slice(frame_data);
        None
    }

    fn set_rom_title(&mut self, _title: &str) {}
}

/// Dummy audio sink with no output
pub struct DummySink;

impl AudioSink for DummySink {
    fn create() -> Result<Self, Box<Error>> { Ok(DummySink) }
    fn write(&mut self, _data: &[(i16, i16)]) {}
}
