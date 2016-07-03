//! CGRAM definitions and methods

use super::SnesRgb;

/// Color RAM size in Bytes
pub const CGRAM_SIZE: usize = 512;

byte_array!(pub Cgram[CGRAM_SIZE] with u16 indexing, save state please);

impl Cgram {
    /// Looks up a color in CGRAM and returns the RGB color values stored inside, without adjusting
    /// the color range to full RGB.
    pub fn get_color(&self, color: u8) -> SnesRgb {
        // -bbbbbgg gggrrrrr (16-bit big endian value! (high byte, high address first))
        let val = self.get_color_raw(color);

        // Extract components
        let b = (val & 0x7c00) >> 10;
        let g = (val & 0x03e0) >> 5;
        let r = val & 0x001f;

        SnesRgb::new(r as u8, g as u8, b as u8)
    }

    /// Gets the raw, 16-bit (technically 15), color value stored at the given color index
    ///
    /// Colors are stored as BGR big-endian values. This method returns the BGR data in host-endian
    /// order.
    pub fn get_color_raw(&self, color: u8) -> u16 {
        // -bbbbbgg gggrrrrr (16-bit big endian value! (high byte, high address first))
        let lo = self[color as u16 * 2] as u16;
        let hi = self[color as u16 * 2 + 1] as u16;

        (hi << 8) | lo
    }

    /// Set a raw color value
    ///
    /// `-bbbbbgg gggrrrrr`
    ///
    /// The raw color is converted to big endian and stored as 2 CGRAM bytes.
    pub fn set_color_raw(&mut self, index: u8, raw: u16) {
        self[index as u16 * 2] = raw as u8;
        self[index as u16 * 2 + 1] = (raw >> 8) as u8;
    }
}
