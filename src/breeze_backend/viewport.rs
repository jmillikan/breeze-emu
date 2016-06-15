//! Provides a utility method for calculating native viewport size when the window is resized.

use ppu::{SCREEN_WIDTH, SCREEN_HEIGHT};

/// A simple rectangle
pub struct Viewport {
    pub x: u32,
    pub y: u32,
    pub w: u32,
    pub h: u32,
}

impl Viewport {
    /// Calculates a viewport to use for a window of the given size.
    ///
    /// The returned viewport will have the native SNES aspect ratio and still fill the window on at
    /// least one axis. Basically, this calculates the black bars to apply to the window to make the
    /// center have the native SNES ratio.
    pub fn for_window_size(w: u32, h: u32) -> Self {
        // FIXME Not sure if floats are a good idea here
        let w = w as f32;
        let h = h as f32;

        const NATIVE_RATIO: f32 = SCREEN_WIDTH as f32 / SCREEN_HEIGHT as f32;
        let ratio = w / h;

        let view_w;
        let view_h;

        if ratio > NATIVE_RATIO {
            // Too wide
            view_h = h;
            view_w = h * NATIVE_RATIO;
        } else {
            // Too high
            view_w = w;
            view_h = w / NATIVE_RATIO;
        }

        let border_x = (w - view_w).round() as u32 / 2;
        let border_y = (h - view_h).round() as u32 / 2;
        let view_w = view_w.round() as u32;
        let view_h = view_h.round() as u32;

        Viewport {
            x: border_x as u32,
            y: border_y as u32,
            w: view_w,
            h: view_h,
        }
    }
}
