//! PPU rendering code
//!
//! # Terminology
//!
//! * **Tile**/**Character**: A tile consists of 8x8 or 16x16 pixels worth of color indices and is
//!   what makes up both sprites and backgrounds. Sprites are always made up of 8x8 tiles, while
//!   backgrounds can use 16x16 pixel tiles by setting the appropriate bit in `BGMODE ($2105)`.
//! * **Background**: A background is a big, connected layer of tiles that can (only) be moved as a
//!   unit. Depending on the background mode, between 1 and 4 BG layers can be used, each with
//!   between 4 and 256 colors.
//! * **Sprite**: A sprite is an independently movable object up to 64x64 pixels in size. Sprites
//!   always use 8x8 tiles and 16 colors, while each sprite can use a different palette. Sprites can
//!   not be rotated (rotating objects in games are either prerendered or make use of BG mode 7,
//!   which allows full matrix transformations on the layer).
//! * **Name table**/**Character table**/**Character map**: These tables store tile data: Maps of
//!   color indices to use for the tile's pixels. Sprites and all background modes except mode 7
//!   store the color indices in "bitplanes", which makes their decoding tricky: A bitplane stores 1
//!   bit for every pixel of the tile. Two bitplanes are interleaved: Bitplane 0 is stored in the
//!   low byte, while bitplane 1 is stored in the high byte.
//! * **Tile map**: Backgrounds consist of 1, 2 or 4 tile maps, each storing data for 32x32 tiles.
//!   For each tile on the background, the tile map stores the tile's priority and H/V flip bits,
//!   the palette, and the tile number. If 16x16 tiles are enabled, each entry in the tile map
//!   stores data for a 16x16 tile consisting of 4 8x8 tiles: `TILE`, `TILE+1`, `TILE+16` and
//!   `TILE+17`, where `TILE` is the stored tile number.

use super::{Ppu, Rgb};

/// Rendering
impl Ppu {
    /// Get the configured sprite size in pixels. If `size_toggle` is `false`, gets the size of
    /// small sprites, otherwise gets the size of large sprites (OAM size bit set).
    pub fn obj_size(&self, size_toggle: bool) -> (u8, u8) {
        match self.obsel >> 5 & 0b111 {
            0b000 => if !size_toggle {(8,8)} else {(16,16)},
            0b001 => if !size_toggle {(8,8)} else {(32,32)},
            0b010 => if !size_toggle {(8,8)} else {(64,64)},
            0b011 => if !size_toggle {(16,16)} else {(32,32)},
            0b100 => if !size_toggle {(16,16)} else {(64,64)},
            0b101 => if !size_toggle {(32,32)} else {(64,64)},
            // FIXME Figure out if we want to support these:
            0b110 => if !size_toggle {(16,32)} else {(32,64)},
            0b111 => if !size_toggle {(16,32)} else {(32,32)},
            _ => unreachable!(),
        }
    }

    /// Returns the active BG mode (0-7).
    pub fn bg_mode(&self) -> u8 { self.bgmode & 0b111 }

    /// Looks up a color index in the CGRAM and converts the stored 15-bit BGR color to 24-bit RGB
    /// (the 15-bit range is stretched to 24 bits).
    pub fn lookup_color(&self, color: u8) -> Rgb {
        // FIXME Is this correct?
        // 16-bit big endian value! (high byte, high address first)
        // -bbbbbgg gggrrrrr
        let lo = self.cgram[color as u16 * 2] as u16;
        let hi = self.cgram[color as u16 * 2 + 1] as u16;
        let val = (hi << 8) | lo;

        // Extract components
        let b = (val & 0x7c00) >> 10;
        let g = (val & 0x03e0) >> 5;
        let r = val & 0x001f;

        // Convert to RGB
        let mut rgb = Rgb { r: (r as u8) << 3, g: (g as u8) << 3, b: (b as u8) << 3 };

        // Adjust color range
        rgb.r += rgb.r / 32;
        rgb.g += rgb.g / 32;
        rgb.b += rgb.b / 32;

        rgb
    }

    /// Returns the backdrop color used as a default color (with color math applied, if enabled).
    fn backdrop_color(&self) -> Rgb {
        // TODO: Color math
        self.lookup_color(0)
    }

    /// Main rendering entry point. Renders the current pixel and returns its color. Assumes that
    /// we're not in any blank mode.
    pub fn render_pixel(&mut self) -> Rgb {
        if self.x == 0 && self.scanline == 0 {
            trace!("New frame. BG mode {}, layers enabled: {:05b}, sprites are {:?} or {:?}",
                self.bg_mode(),
                self.tm & 0x1f,
                self.obj_size(false),
                self.obj_size(true));
        }

        if self.x == 0 {
            // Entered new scanline.
            self.collect_sprite_data_for_scanline();
        }

        macro_rules! e {
            ( $e:expr ) => ( $e );
        }

        // This macro gets the current pixel from a tile with given priority in the given layer.
        // If the pixel is non-transparent, it will return its RGB value (after applying color
        // math). If it is transparent, it will do nothing (ie. the code following this macro is
        // executed).
        macro_rules! try_layer {
            ( Sprites with priority $prio:tt ) => {
                if let Some(rgb) = self.maybe_draw_sprite_pixel(e!($prio)) {
                    return rgb
                }
            };
            ( BG $bg:tt tiles with priority $prio:tt ) => {
                if let Some(rgb) = self.lookup_bg_color(e!($bg), e!($prio)) {
                    return rgb
                }
            };
        }

        match self.bg_mode() {
            0 => {
                // I love macros <3
                try_layer!(Sprites with priority 3);
                try_layer!(BG 1 tiles with priority 1);
                try_layer!(BG 2 tiles with priority 1);
                try_layer!(Sprites with priority 2);
                try_layer!(BG 1 tiles with priority 0);
                try_layer!(BG 2 tiles with priority 0);
                try_layer!(Sprites with priority 1);
                try_layer!(BG 3 tiles with priority 1);
                try_layer!(BG 4 tiles with priority 1);
                try_layer!(Sprites with priority 0);
                try_layer!(BG 3 tiles with priority 0);
                try_layer!(BG 4 tiles with priority 0);
                self.backdrop_color()
            }
            1 => {
                if self.bgmode & 0x08 != 0 { try_layer!(BG 3 tiles with priority 1) }
                try_layer!(Sprites with priority 3);
                try_layer!(BG 1 tiles with priority 1);
                try_layer!(BG 2 tiles with priority 1);
                try_layer!(Sprites with priority 2);
                try_layer!(BG 1 tiles with priority 0);
                try_layer!(BG 2 tiles with priority 0);
                try_layer!(Sprites with priority 1);
                if self.bgmode & 0x08 == 0 { try_layer!(BG 3 tiles with priority 1) }
                try_layer!(Sprites with priority 0);
                try_layer!(BG 3 tiles with priority 0);
                self.backdrop_color()
            }
            2 ... 5 => {
                // FIXME Do the background priorities differ here?
                try_layer!(Sprites with priority 3);
                try_layer!(BG 1 tiles with priority 1);
                try_layer!(Sprites with priority 2);
                try_layer!(BG 2 tiles with priority 1);
                try_layer!(Sprites with priority 1);
                try_layer!(BG 1 tiles with priority 0);
                try_layer!(Sprites with priority 0);
                try_layer!(BG 2 tiles with priority 0);
                self.backdrop_color()
            }
            6 => {
                try_layer!(Sprites with priority 3);
                try_layer!(BG 1 tiles with priority 1);
                try_layer!(Sprites with priority 2);
                try_layer!(Sprites with priority 1);
                try_layer!(BG 1 tiles with priority 0);
                try_layer!(Sprites with priority 0);
                self.backdrop_color()
            }
            7 => panic!("NYI: BG mode 7"),
            _ => unreachable!(),
        }
    }

    /// Reads character data for a pixel and returns the palette index stored in the bitplanes.
    ///
    /// # Parameters
    /// * `bitplane_count`: Number of bitplanes (must be even)
    /// * `start_addr`: Address of the first bitplane (or the first 2)
    /// * `tile_size`: 8 or 16
    /// * `(x, y)`: Offset inside the tile (`0-7` or `0-15`, depending on the tile size)
    pub fn read_chr_entry(&self,
                      bitplane_count: u8,
                      start_addr: u16,
                      tile_size: u8,
                      (x, y): (u8, u8)) -> u8 {
        // 2 bitplanes are stored interleaved with each other, so there can only be an even number
        debug_assert!(bitplane_count & 1 == 0, "odd bitplane count");
        debug_assert!(x <= 7 || (x <= 15 && tile_size == 16), "invalid x value: {}", x);
        debug_assert!(y <= 7 || (y <= 15 && tile_size == 16), "invalid y value: {}", y);
        debug_assert!(tile_size == 8, "non-8x8 tiles unsupported"); // FIXME support 16x16 tiles
        let bitplane_pairs = bitplane_count >> 1;

        let mut palette_index = 0u8;
        for i in 0..bitplane_pairs {
            let bitplane_bits = self.read_2_bitplanes(
                start_addr + i as u16 * 16, // 16 Bytes per pair of bitplanes
                (x, y));
            palette_index = palette_index | (bitplane_bits << (2 * i));
        }

        palette_index
    }

    /// Reads 2 bits of the given coordinate within the bitplane's tile from 2 interleaved
    /// bitplanes.
    ///
    /// # Parameters
    /// * `bitplanes_start`: Start address of the bitplanes
    /// * `(x_off, y_off)`: Offset into the tile (`0-7`)
    fn read_2_bitplanes(&self, bitplanes_start: u16, (x_off, y_off): (u8, u8)) -> u8 {
        // FIXME Handle flipped tiles somewhere in here (or not in here)
        // Bit 0 in low bytes, bit 1 in high bytes
        let lo = self.vram[bitplanes_start + y_off as u16 * 2];
        let hi = self.vram[bitplanes_start + y_off as u16 * 2 + 1];
        // X values in a byte: 01234567
        let bit0 = (lo >> (7 - x_off)) & 1;
        let bit1 = (hi >> (7 - x_off)) & 1;

        (bit1 << 1) | bit0
    }
}
