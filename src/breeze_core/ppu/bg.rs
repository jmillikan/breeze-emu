//! Background layer rendering

use super::{Ppu, SnesRgb};

/// BG layer scanline cache.
///
/// This cache stores a prerendered scanline of all background layers. The cache is created lazily
/// (when BG layer pixels are looked up), so we will not waste time caching a disabled BG layer.
#[derive(Default)]
pub struct BgCache {
    layers: [BgLayerCache; 4],
}

/// Data that's stored in the BG layer caches for a single pixel
#[derive(Copy, Clone, Default)]
struct CachedPixel {
    // These are just copied from `TilemapEntry`.

    /// Tile priority bit (0-1)
    priority: u8,
    /// Precalculated color of the pixel (15-bit RGB). `None` = transparent.
    color: Option<SnesRgb>,
}

/// BG cache for a single layer
struct BgLayerCache {
    /// Whether this cache contains valid data. If `false`, the cache will be refreshed on next
    /// access.
    valid: bool,
    /// Stores the prerendered scanline
    scanline: [CachedPixel; super::SCREEN_WIDTH as usize],
}

impl Default for BgLayerCache {
    fn default() -> Self {
        BgLayerCache {
            valid: false,
            scanline: [CachedPixel::default(); super::SCREEN_WIDTH as usize],
        }
    }
}

impl BgLayerCache {
    /// Invalidates the cache of this layer, causing it to be rebuilt on next access.
    #[allow(dead_code)] // FIXME Use in the right locations
    fn invalidate(&mut self) {
        self.valid = false;
    }
}

impl BgCache {
    /// Invalidates the BG cache of all layers
    fn invalidate_all(&mut self) {
        self.layers[0].valid = false;
        self.layers[1].valid = false;
        self.layers[2].valid = false;
        self.layers[3].valid = false;
    }
}

/// Collected background settings
struct BgSettings {
    /// Mosaic pixel size (1-16). 1 = Normal pixels.
    /// FIXME: I think there's a difference between disabled and enabled with 1x1 mosaic size in
    /// some modes (highres presumably)
    #[allow(dead_code)] // FIXME NYI
    mosaic: u8,
    /// Tilemap word address in VRAM
    /// "Starting at the tilemap address, the first $800 bytes are for tilemap A. Then come the
    /// $800 bytes for B, then C then D."
    tilemap_word_addr: u16,
    /// When `true`, this BGs tilemaps are repeated sideways
    tilemap_mirror_h: bool,
    /// When `true`, this BGs tilemaps are repeated downwards
    tilemap_mirror_v: bool,
    /// If `true`, BG tiles are 16x16 pixels. If `false`, they are 8x8 pixels.
    tile_size_16: bool,
    /// Character Data start address in VRAM
    chr_addr: u16,
    /// Horizontal scroll offset. Moves the BG layer to the left by some number of pixels.
    hofs: u16,
    /// Vertical scroll offset. Moves the BG layer up by some number of pixels.
    vofs: u16,
}

/// Unpacked tilemap entry for internal (rendering) use.
///
/// A tilemap entry is 2 bytes large and contains informations about a single background layer tile.
struct TilemapEntry {
    /// Flip this tile vertically (flips top and down of the tile)
    vflip: bool,
    /// Flip horizontally (flips left and right side)
    hflip: bool,
    /// Priority bit (0-1)
    priority: u8,
    /// Tile palette (0-7)
    palette: u8,
    /// Index into the character/tile data, where the actual tile character data is stored in
    /// bitplanes (10 bits)
    tile_number: u16,
}

impl Ppu {
    /// Determines whether the given BG layer (1-4) is enabled
    fn bg_enabled(&self, bg: u8, subscreen: bool) -> bool {
        let reg = if subscreen { self.ts } else { self.tm };
        reg & (1 << (bg - 1)) != 0
    }

    /// Reads the tilemap entry at the given VRAM word address.
    ///     vhopppcc cccccccc (high, low)
    ///     v/h        = Vertical/Horizontal flip this tile.
    ///     o          = Tile priority.
    ///     ppp        = Tile palette base.
    ///     cccccccccc = Tile number.
    fn tilemap_entry(&self, word_address: u16) -> TilemapEntry {
        let byte_address = word_address << 1;
        let lo = self.vram[byte_address];
        let hi = self.vram[byte_address + 1];

        TilemapEntry {
            vflip: hi & 0x80 != 0,
            hflip: hi & 0x40 != 0,
            priority: (hi & 0x20) >> 5,
            palette: (hi & 0x1c) >> 2,
            tile_number: ((hi as u16 & 0x03) << 8) | lo as u16,
        }
    }

    /// Collects properties of a background layer
    fn bg_settings(&self, bg: u8) -> BgSettings {
        // The BGxSC register for our background layer
        let bgsc = match bg {
            1 => self.bg1sc,
            2 => self.bg2sc,
            3 => self.bg3sc,
            4 => self.bg4sc,
            _ => unreachable!(),
        };
        // Chr (Tileset, not Tilemap) start (word?) address >> 12
        let chr = match bg {
            1 => self.bg12nba & 0x0f,
            2 => (self.bg12nba & 0xf0) >> 4,
            3 => self.bg34nba & 0x0f,
            4 => (self.bg34nba & 0xf0) >> 4,
            _ => unreachable!(),
        };
        let (hofs, vofs) = match bg {
            1 => (self.bg1hofs, self.bg1vofs),
            2 => (self.bg2hofs, self.bg2vofs),
            3 => (self.bg3hofs, self.bg3vofs),
            4 => (self.bg4hofs, self.bg4vofs),
            _ => unreachable!(),
        };

        BgSettings {
            mosaic: if self.mosaic & (1 << (bg-1)) == 0 {
                1
            } else {
                ((self.mosaic & 0xf0) >> 4) + 1
            },
            tilemap_word_addr: ((bgsc as u16 & 0xfc) >> 2) << 10,
            tilemap_mirror_h: bgsc & 0b01 == 0, // inverted bit value
            tilemap_mirror_v: bgsc & 0b10 == 0, // inverted bit value
            tile_size_16: match self.bg_mode() {
                // "If the BG character size for BG1/BG2/BG3/BG4 bit is set, then the BG is made of
                // 16x16 tiles. Otherwise, 8x8 tiles are used. However, note that Modes 5 and 6
                // always use 16-pixel wide tiles, and Mode 7 always uses 8x8 tiles."
                5 | 6 => true,
                7 => false,
                _ => {
                    // BGMODE: `4321----` (`-` = not relevant here) - Use 16x16 tiles?
                    self.bgmode & (1 << (bg + 3)) != 0
                }
            },
            chr_addr: (chr as u16) << 12,
            hofs: hofs,
            vofs: vofs,
        }
    }

    /// Returns the number of color bits in the given BG layer in the current BG mode (2, 4, 7 or
    /// 8). To get the number of colors, use `1 << color_bits_for_bg`.
    ///
    /// Table of colors for BG layers (not what this function returns!). `X` denotes a BG for
    /// offset-per-tile data.
    /// ```text
    /// Mode    # Colors for BG
    ///          1   2   3   4
    /// ======---=---=---=---=
    /// 0        4   4   4   4
    /// 1       16  16   4   -
    /// 2       16  16   X   -
    /// 3      256  16   -   -
    /// 4      256   4   X   -
    /// 5       16   4   -   -
    /// 6       16   -   X   -
    /// 7      256   -   -   -
    /// 7EXTBG 256 128   -   -
    /// ```
    fn color_bits_for_bg(&self, bg: u8) -> u8 {
        match (self.bg_mode(), bg) {
            (0, _) => 2,
            (1, 1) |
            (1, 2) => 4,
            (1, 3) => 2,
            (2, _) => 4,
            (3, 1) => 8,
            (3, 2) => 4,
            (4, 1) => 8,
            (4, 2) => 2,
            (5, 1) => 4,
            (5, 2) => 2,
            (6, _) => 4,
            (7, _) => panic!("unreachable: color_count_for_bg for mode 7"),
            _ => unreachable!(),
        }
    }

    /// Calculates the palette base index for a tile in the given background layer. `palette_num`
    /// is the palette number stored in the tilemap entry (the 3 `p` bits).
    fn palette_base_for_bg_tile(&self, bg: u8, palette_num: u8) -> u8 {
        debug_assert!(bg >= 1 && bg <= 4);
        match (self.bg_mode(), bg) {
            (0, _) => palette_num * 4 + (bg - 1) * 32,
            (1, _) |
            (5, _) => palette_num * (1 << self.color_bits_for_bg(bg) as u8),
            (2, _) => palette_num * 16,
            (3, 1) => 0,
            (3, 2) => palette_num * 16,
            (4, 1) => 0,
            (4, 2) => palette_num * 4,
            (6, _) => palette_num * 16, // BG1 has 16 colors
            (7, _) => panic!("unreachable: palette_base_for_bg_tile for mode 7"),
            _ => unreachable!(),
        }
    }

    fn render_mode7_scanline(&mut self) {
        // TODO Figure out how to integrate EXTBG
        assert!(self.setini & 0x40 == 0, "NYI: Mode 7 EXTBG");

        // FIXME consider changing the type of `Ppu.m7a,...` to `i16`

        let vflip = self.m7sel & 0x02 != 0;
        let hflip = self.m7sel & 0x01 != 0;
        // 0/1: Wrap
        // 2: Transparent
        // 3: Fill with tile 0
        let screen_over = self.m7sel >> 6;

        let y = self.scanline;

        for x in self.x..super::SCREEN_WIDTH as u16 {
            // Code taken from http://problemkaputt.de/fullsnes.htm
            // FIXME: The above source also has a much faster way to render whole scanlines!
            let screen_x = x ^ if hflip { 0xff } else { 0x00 };
            let screen_y = y ^ if vflip { 0xff } else { 0x00 };

            let mut org_x = (self.m7hofs as i16 - self.m7x as i16) & !0x1c00;
            if org_x < 0 { org_x |= 0x1c00; }
            let mut org_y = (self.m7vofs as i16 - self.m7y as i16) & !0x1c00;
            if org_y < 0 { org_y |= 0x1c00; }

            let mut vram_x: i32 = ((self.m7a as i16 as i32 * org_x as i32) & !0x3f) + ((self.m7b as i16 as i32 * org_y as i32) & !0x3f) + self.m7x as i16 as i32 * 0x100;
            let mut vram_y: i32 = ((self.m7c as i16 as i32 * org_x as i32) & !0x3f) + ((self.m7d as i16 as i32 * org_y as i32) & !0x3f) + self.m7y as i16 as i32 * 0x100;
            vram_x += ((self.m7b as i16 as i32 * screen_y as i32) & !0x3f) + self.m7a as i16 as i32 * screen_x as i32;
            vram_y += ((self.m7d as i16 as i32 * screen_y as i32) & !0x3f) + self.m7c as i16 as i32 * screen_x as i32;

            let out_of_bounds = vram_x & (1 << 18) != 0 || vram_y & (1 << 18) != 0;
            let palette_index = match screen_over {
                2 if out_of_bounds => { // transparent
                    0
                },
                _ => {
                    let (tile_x, tile_y) = if screen_over == 3 && out_of_bounds {
                        (0, 0)  // 3 -> use tile 0
                    } else {
                        let tile_x: u16 = ((vram_x as u32 >> 11) & 0x7f) as u16;
                        let tile_y: u16 = ((vram_y as u32 >> 11) & 0x7f) as u16;
                        (tile_x, tile_y)
                    };

                    let off_x: u16 = (vram_x as u16 >> 8) & 0x07;
                    let off_y: u16 = (vram_y as u16 >> 8) & 0x07;

                    // Tilemap address for (7-bit) tile X/Y coordinates (BG1 is 128x128 tiles):
                    // `0yyyyyyy xxxxxxx0`
                    let tilemap_addr: u16 = (tile_y << 8) | (tile_x << 1);
                    // The "tilemap" in mode 7 just consists of "tile numbers" (or pixel addresses)
                    let tile_number = self.vram[tilemap_addr] as u16;

                    // The CHR address is calculated like this (where `t` is `tile_number` and `x` and `y`
                    // are pixel offsets inside the tile):
                    // `tttttttt tyyyxxx1`
                    let chr_addr = (tile_number << 7) | (off_y << 4) | (off_x << 1) | 1;
                    self.vram[chr_addr]
                },
            };

            let rgb = match palette_index {
                0 => None,
                _ => Some(self.cgram.get_color(palette_index)),
            };

            self.bg_cache.layers[0].scanline[x as usize] = CachedPixel {
                priority: 0,    // Ignored anyways
                color: rgb,
            };
        }
    }

    /// Render the current scanline of the given BG layer into its cache.
    ///
    /// We render starting at `self.x` (the pixel we actually need) until the end of the
    /// scanline. Note that this means that the `valid` flag is only relevant for the
    /// leftover part of the scanline, not the entire cached scanline.
    fn render_bg_scanline(&mut self, bg_num: u8) {
        // Apply BG scrolling and get the tile coordinates
        // FIXME Apply mosaic filter
        // FIXME Fix this: "Note that many games will set their vertical scroll values to -1 rather
        // than 0. This is because the SNES loads OBJ data for each scanline during the previous
        // scanline. The very first line, though, wouldn’t have any OBJ data loaded! So the SNES
        // doesn’t actually output scanline 0, although it does everything to render it. These
        // games want the first line of their tilemap to be the first line output, so they set
        // their VOFS registers in this manner. Note that an interlace screen needs -2 rather than
        // -1 to properly correct for the missing line 0 (and an emulator would need to add 2
        // instead of 1 to account for this)."
        // -> I guess we should just decrement the physical screen height by 1

        if self.bg_mode() == 7 {
            self.render_mode7_scanline();
            return;
        }

        let mut x = self.x;
        let y = self.scanline;
        let bg = self.bg_settings(bg_num);
        let tile_size = if bg.tile_size_16 { 16 } else { 8 };
        let (hofs, vofs) = (bg.hofs, bg.vofs);
        let (sx, sy) = (!bg.tilemap_mirror_h, !bg.tilemap_mirror_v);

        let color_bits = self.color_bits_for_bg(bg_num);
        if color_bits == 8 {
            // can use direct color mode
            debug_assert!(self.cgwsel & 0x01 == 0, "NYI: direct color mode");
        }

        let mut tile_x = x.wrapping_add(hofs) / tile_size as u16;
        let tile_y = y.wrapping_add(vofs) / tile_size as u16;
        let mut off_x = (x.wrapping_add(hofs) % tile_size as u16) as u8;
        let off_y = (y.wrapping_add(vofs) % tile_size as u16) as u8;

        while x < super::SCREEN_WIDTH as u16 {
            // Render current tile (`tile_x`) starting at `off_x` until the end of the tile,
            // then go to next tile and set `off_x = 0`

            // Calculate the VRAM word address, where the tilemap entry for our tile is stored
            let tilemap_entry_word_address =
                bg.tilemap_word_addr |
                ((tile_y & 0x1f) << 5) |
                (tile_x & 0x1f) |
                if sy {(tile_y & 0x20) << if sx {6} else {5}} else {0} |
                if sx {(tile_x & 0x20) << 5} else {0};
            let tilemap_entry = self.tilemap_entry(tilemap_entry_word_address);

            let bitplane_start_addr =
                (bg.chr_addr << 1) +
                (tilemap_entry.tile_number * 8 * color_bits as u16);   // 8 bytes per bitplane

            let palette_base = self.palette_base_for_bg_tile(bg_num, tilemap_entry.palette);

            while off_x < tile_size && x < super::SCREEN_WIDTH as u16 {
                let palette_index = self.read_chr_entry(color_bits,
                                                        bitplane_start_addr,
                                                        tile_size,
                                                        (off_x, off_y),
                                                        (tilemap_entry.vflip, tilemap_entry.hflip));

                let rgb = match palette_index {
                    0 => None,
                    _ => Some(self.cgram.get_color(palette_base + palette_index)),
                };

                self.bg_cache.layers[bg_num as usize - 1].scanline[x as usize] = CachedPixel {
                    priority: tilemap_entry.priority,
                    color: rgb,
                };
                x += 1;
                off_x += 1;
            }

            tile_x += 1;
            off_x = 0;
        }
    }

    /// Main entry point into the BG layer renderer.
    ///
    /// Lookup the color of the given background layer (1-4) at the current pixel, using the given
    /// priority (0-1) only. This will also scroll backgrounds accordingly.
    ///
    /// This may only be called with BG layer numbers which are actually valid in the current BG
    /// mode (the renderer code makes sure that this is the case).
    ///
    /// Returns `None` if the pixel is transparent, `Some(SnesRgb)` otherwise.
    pub fn lookup_bg_color(&mut self, bg_num: u8, prio: u8, subscreen: bool) -> Option<SnesRgb> {
        debug_assert!(bg_num >= 1 && bg_num <= 4);
        debug_assert!(prio == 0 || prio == 1);

        if !self.bg_enabled(bg_num, subscreen) {
            return None;
        }

        if self.x == 0 {
            // Before we draw the first pixel, make sure that we invalidate the cache so it is
            // rebuilt first.
            self.bg_cache.invalidate_all();
        }

        if !self.bg_cache.layers[bg_num as usize - 1].valid {
            // Call actual render code to render the scanline into the cache
            self.render_bg_scanline(bg_num);

            self.bg_cache.layers[bg_num as usize - 1].valid = true;
        }

        // Cache must be valid now, so we can access the pixel we need:
        let pixel = &self.bg_cache.layers[bg_num as usize - 1].scanline[self.x as usize];
        if pixel.priority == prio {
            pixel.color
        } else {
            None
        }
    }
}
