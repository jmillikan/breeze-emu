//! CGRAM definitions and methods

use super::Rgb;

/// Color RAM size in Bytes
pub const CGRAM_SIZE: usize = 512;

byte_array!(pub Cgram[CGRAM_SIZE] with u16 indexing, save state please);

impl Cgram {
    /// Looks up a color index in the CGRAM and converts the stored 15-bit BGR color to 24-bit RGB
    /// (the 15-bit range is stretched to 24 bits).
    ///
    /// CGRAM contains 256 colors, so any index is valid.
    pub fn get_color(&self, index: u8) -> Rgb {
        let (r, g, b) = self.get_color_unadjusted(index);

        // Convert to RGB
        let mut rgb = Rgb { r: (r as u8) << 3, g: (g as u8) << 3, b: (b as u8) << 3 };

        // Adjust color range
        rgb.r += rgb.r / 32;
        rgb.g += rgb.g / 32;
        rgb.b += rgb.b / 32;

        rgb
    }

    /// Looks up a color in CGRAM and returns the RGB color values stored inside, without adjusting
    /// the color range to full RGB.
    pub fn get_color_unadjusted(&self, color: u8) -> (u8, u8, u8) {
        // -bbbbbgg gggrrrrr (16-bit big endian value! (high byte, high address first))
        let lo = self[color as u16 * 2] as u16;
        let hi = self[color as u16 * 2 + 1] as u16;
        let val = (hi << 8) | lo;

        // Extract components
        let b = (val & 0x7c00) >> 10;
        let g = (val & 0x03e0) >> 5;
        let r = val & 0x001f;

        (r as u8, g as u8, b as u8)
    }
}
