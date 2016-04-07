//! Render test frontend

use frontend_api::{FrontendAction, Renderer};

/// A testing renderer which saves the last frame rendered.
pub struct TestRenderer {
    last_frame: Vec<u8>,
    frames_to_go: u32,
}

impl TestRenderer {
    /// Creates a new `TestRenderer` that will exit the emulator when the given number of frames are
    /// rendered.
    ///
    /// # Panics
    ///
    /// Panics when `total_frames` is 0.
    pub fn new(total_frames: u32) -> Self {
        assert!(total_frames > 0, "total_frames may not be 0");
        TestRenderer {
            last_frame: Vec::new(),
            frames_to_go: total_frames,
        }
    }

    /// Returns the last frame rendered (as returned by the PPU).
    ///
    /// Returns an empty slice if no frames have been rendered yet.
    pub fn last_frame(&self) -> &[u8] {
        &self.last_frame
    }
}

impl Renderer for TestRenderer {
    fn render(&mut self, frame_data: &[u8]) -> Option<FrontendAction> {
        self.last_frame.clear();
        self.last_frame.extend_from_slice(frame_data);
        self.frames_to_go -= 1;

        if self.frames_to_go == 0 {
            Some(FrontendAction::Exit)
        } else {
            None
        }
    }

    fn set_rom_title(&mut self, _title: &str) {}
}
