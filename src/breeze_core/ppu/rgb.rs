//! Defines RGB color types for use in the renderer

use std::cmp;

/// 5-bit per channel RGB value used by the SNES
#[derive(Debug, Copy, Clone)]
pub struct SnesRgb {
    r: u8,
    g: u8,
    b: u8,
}

impl SnesRgb {
    /// Creates a new `SnesRgb` instance from the given RGB values.
    ///
    /// When debug assertions are enabled, this will panic when any color value is outside the 5
    /// bit range. When assertions are disabled, the values will be truncated to their low 5 bits.
    pub fn new(r: u8, g: u8, b: u8) -> SnesRgb {
        debug_assert_eq!(r & 0b11111, r);
        debug_assert_eq!(g & 0b11111, g);
        debug_assert_eq!(b & 0b11111, b);

        SnesRgb {
            r: r & 0b11111,
            g: g & 0b11111,
            b: b & 0b11111,
        }
    }

    pub fn r(&self) -> u8 { self.r }
    pub fn g(&self) -> u8 { self.g }
    pub fn b(&self) -> u8 { self.b }

    /// Performs saturating addition of `self` and `other` per color.
    pub fn saturating_add(&self, other: &Self) -> Self {
        let r = cmp::min(self.r + other.r, 0b11111);
        let g = cmp::min(self.g + other.g, 0b11111);
        let b = cmp::min(self.b + other.b, 0b11111);

        SnesRgb::new(r, g, b)
    }

    /// Performs saturating subtraction of `self` and `other` per color.
    pub fn saturating_sub(&self, other: &Self) -> Self {
        let r = self.r.saturating_sub(other.r);
        let g = self.g.saturating_sub(other.g);
        let b = self.b.saturating_sub(other.b);

        SnesRgb::new(r, g, b)
    }

    /// Converts 5-bit RGB to 8-bit RGB, adjusting the color space
    ///
    /// The colors are adjusted as follows (http://wiki.superfamicom.org/snes/show/Palettes):
    /// ```text
    /// Rout = Rin << 3
    /// Gout = Gin << 3
    /// Bout = Bin << 3
    /// Rout += Rout / 32
    /// Gout += Gout / 32
    /// Bout += Bout / 32
    /// ```
    pub fn to_adjusted_rgb(&self) -> Rgb {
        // Convert to 8-bit per-channel RGB
        let mut rgb = Rgb {
            r: self.r() << 3,
            g: self.g() << 3,
            b: self.b() << 3,
        };

        // Adjust color range
        rgb.r += rgb.r / 32;
        rgb.g += rgb.g / 32;
        rgb.b += rgb.b / 32;

        rgb
    }
}

/// Standard 24-bit RGB color rendered to the frame buffer
#[derive(Debug, Copy, Clone)]
pub struct Rgb {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}
