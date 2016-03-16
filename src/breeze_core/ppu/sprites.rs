//! Sprite rendering

use super::{Ppu, Rgb};

use arrayvec::ArrayVec;

/// Render state stored inside the `Ppu`. We use this to cache the visible sprites for each
/// scanline, just like the real PPU would.
pub struct SpriteRenderState {
    /// Caches a prerendered scanline of sprite pixels. This is rendered at the start of each
    /// scanline and contains, for each pixel on the scanline, the color of the OBJ (= sprite) layer
    /// and the priority of that pixel (`None` in case there's no opaque sprite pixel there).
    sprite_scanline: [(Option<(Rgb, u8)>); super::SCREEN_WIDTH as usize],
}

impl Default for SpriteRenderState {
    fn default() -> Self {
        SpriteRenderState {
            sprite_scanline: [None; super::SCREEN_WIDTH as usize],
        }
    }
}

/// Unpacked OAM entry for internal use.
struct OamEntry {
    /// First tile (0-255), needs to take name table selection bit into account
    tile: u8,
    /// Sprite's name table (0 or 1).
    name_table: u8,
    /// 9 bits, considered signed (-256 - 255)
    x: i16,
    y: u8,
    /// 0-3
    priority: u8,
    /// 0-7. The first palette entry is `128+ppp*16`.
    palette: u8,
    #[allow(dead_code)] // FIXME
    hflip: bool,
    #[allow(dead_code)]
    vflip: bool,
    size_toggle: bool,
}

/// Informations about a single tile of a sprite, needed for drawing.
struct SpriteTile {
    /// Address of character data for this tile
    chr_addr: u16,
    /// X position of the tile on the screen. Can be negative if the tile starts outside the screen.
    x: i16,
    /// Y position of the scanline inside the tile (0-7)
    /// FIXME Can we just store the pixel row that's on the scanline? (for each priority)
    y_off: u8,
    /// Priority of the sprite (0-3)
    priority: u8,
    /// Palette of the sprite (0-7)
    palette: u8,

    // FIXME hflip/vflip
}

impl Ppu {
    /// Returns the OAM entry of the given sprite. Always returns a valid entry if `index` is valid
    /// (0...127), panics otherwise.
    fn get_oam_entry(&self, index: u8) -> OamEntry {
        debug_assert!(index <= 127, "attempted to access sprite #{}", index);

        // OAM entry start address (low table)
        let start = index as u16 * 4;
        let mut x = self.oam[start] as u16;

        // vhoopppN
        let byte4 = self.oam[start + 3];
        let vflip = byte4 & 0x80 != 0;
        let hflip = byte4 & 0x40 != 0;
        let priority = (byte4 & 0x30) >> 4;
        let palette = (byte4 & 0x0e) >> 1;

        // Read the second table. Each byte contains information of 4 sprites (2 bits per sprite):
        // Bits 1/3/5/6 is the size-toggle bit, bits 0/2/4/6 is the MSb of the x coord
        let byte = self.oam[512 + index as u16 / 4];
        let index_in_byte = index & 0b11;
        let msb_mask = 1 << (index_in_byte * 2);
        let size_mask = 2 << (index_in_byte * 2);
        let size_toggle = byte & size_mask != 0;
        if byte & msb_mask != 0 {
            // MSb of `x` is set, so `x` is negative. Since `x` is a signed 9-bit value, we have to
            // sign-extend it to 16 bits by setting all bits starting from the MSb to 1.
            x = 0xff00 | x;
        }

        OamEntry {
            tile: self.oam[start + 2],
            name_table: byte4 & 1,
            x: x as i16,
            y: self.oam[start + 1],
            priority: priority,
            palette: palette,
            hflip: hflip,
            vflip: vflip,
            size_toggle: size_toggle,
        }
    }

    /// Collects visible sprites and sprite tiles for the current scanline.
    ///
    /// Called when rendering the first pixel on a scanline.
    pub fn collect_sprite_data_for_scanline(&mut self) {
        let first_sprite = if self.oamaddh & 0x80 == 0 {
            0
        } else {
            // Priority rotation enabled
            (self.oamaddl as u16 & 0xfe) >> 1
        };

        // Find the first 32 sprites on the current scanline (RANGE)
        // NB Priority is ignored for this step, it's only used for drawing, which isn't done here
        let mut visible_sprites = ArrayVec::<[_; 32]>::new();
        for i in first_sprite..first_sprite+128 {
            let entry = self.get_oam_entry((i & 0x7f) as u8);   // limit to 127 and wrap back around
            if self.sprite_on_scanline(&entry) {
                if let Some(_) = visible_sprites.push(entry) {
                    // FIXME: Sprite overflow. Set bit 6 of $213e.
                    break
                }
            }
        }

        // "Starting with the last sprite in Range, load up to 34 8x8 tiles (from left-to-right,
        // after flipping). If there are more than 34 tiles in Range, set bit 7 of $213e. Only
        // those tiles with -8 < X < 256 are counted."
        // A few notes:
        // * Sprite tiles are always 8x8 pixels
        // * Sprites do not have tile maps like BGs do
        // * "left-to-right" refers to how tiles of sprites are loaded, not the sprite order
        // * Tiles are loaded iff they are on the current scanline (and have `-8 < X < 256`)
        // FIXME Is this ^^ correct?

        // FIXME Use `ArrayVec<[_; 34]>` when it works
        let mut visible_tiles = array_vec![SpriteTile; 34];

        // Word address of first sprite character table
        let name_base: u16 = (self.obsel as u16 & 0b111) << 13;
        let name_select: u16 = (self.obsel as u16 >> 3) & 0b11;

        // TIME: Start at the last sprite found, load up to 34 8x8 tiles (for each sprite from left
        // to right, after taking flip bits of the sprite into account)
        'collect_tiles: for sprite in visible_sprites.iter().rev() {
            // How many tiles are there?
            let (sprite_w, _) = self.obj_size(sprite.size_toggle);
            let sprite_w_tiles = sprite_w / 8;
            // Offset into the sprite
            let sprite_y_off = self.scanline - sprite.y as u16;
            // Tile Y coordinate of the tile row we're interested in (tiles on the scanline)
            let y_tile = sprite_y_off / 8;
            // Y offset into the tile row
            let tile_y_off = (sprite_y_off % 8) as u8;

            // Calculate VRAM word address of first tile. Depends on base/name bits in `$2101`.
            let tile_start_word_addr =
                (name_base |
                ((sprite.tile as u16) << 4) |
                (sprite.name_table as u16 * ((name_select + 1) << 12))) & 0x7fff;
            let tile_start_addr = tile_start_word_addr * 2;

            // The character data for the first tile is stored at `tile_start_addr`, in the same
            // format as BG character data (bitplanes, etc.). Keep in mind that sprites do not have
            // tilemaps.
            // One 8x8 tile is 32 Bytes large (4 bits per pixel).
            // Tiles in a single (1 tile or 8 pixel high) row of the sprite are stored sequentially:
            // Tile coord (1,0) is stored directly behind (0,0), which is stored at
            // `tile_start_addr`.
            // Rows of tiles, however, are always stored 512 Bytes (or 16 tiles/128 pixels) apart:
            // If tile (0,0) is at address $0000, tile (0,1) is at $0200. This is independent of
            // the sprite size, which means that there are "holes" in the sprite character data,
            // which are used to store the data of other sprites.

            // Start address of the row of tiles on the scanline
            let y_row_start_addr = tile_start_addr.wrapping_add(512 * y_tile);

            // FIXME "Only those tiles with -8 < X < 256 are counted."
            // Add all tiles in this row to our tile list (left to right)
            for i in 0..sprite_w_tiles as i16 {
                if visible_tiles.len() < 34 {
                    visible_tiles.push(SpriteTile {
                        chr_addr: y_row_start_addr + 32 * i as u16,
                        x: sprite.x + 8 * i,
                        y_off: tile_y_off,
                        priority: sprite.priority,
                        palette: sprite.palette,
                    });
                } else {
                    // FIXME Set sprite tile overflow flag
                    break 'collect_tiles
                }
            }
        }

        // With all 34 visible sprite tiles collected, prerender them into a cache. We also store
        // the priority in the cache to correctly layer the OBJ layer between BGs. This also
        // emulates the "sprite priority quirk" (as it is known primarily on the NES, which seems to
        // share this behavior).
        // "Sprites with a lower index are always in front of sprites with a higher index."
        // Since the sprite list is already sorted by index, and the tile list is built up
        // backwards, we should be fine with iterating over all 34 (or less) tiles and rendering
        // them in order (overwriting what's already there).
        self.sprite_render_state.sprite_scanline = [None; super::SCREEN_WIDTH as usize];

        for tile in &visible_tiles {
            for x_off in 0u8..8 {
                let screen_x = tile.x + x_off as i16;
                if screen_x >= 0 && screen_x < super::SCREEN_WIDTH as i16 {
                    // on-screen pixel (can write to buffer)
                    let color = self.read_sprite_tile_pixel(tile, x_off);
                    let buffer = &mut self.sprite_render_state.sprite_scanline;
                    match color {
                        Some(rgb) => {
                            buffer[screen_x as usize] = Some((rgb, tile.priority));
                        }
                        None => {
                            // do nothing (don't overwrite visible pixels with transparent ones)
                        }
                    }
                }
            }
        }
    }

    /// Determines if the given sprite has any tiles on the current scanline
    fn sprite_on_scanline(&self, sprite: &OamEntry) -> bool {
        let (w, h) = self.obj_size(sprite.size_toggle);
        let (w, h) = (w as i16, h as u16);

        // "If any OBJ is at X=256 (or X=-256, same difference), consider it as being at X=0 when
        // considering Range and Time."
        // X=256 can not occur, since X is a signed 9-bit value (range is -256 - 255)
        let x = if sprite.x == -256 { 0 } else { sprite.x };
        let y = sprite.y as u16;

        // "Only those sprites with -size < X < 256 are considered in Range." (`size` is `w` here)
        // We don't check `X < 256`, since that cannot occur (X is a signed 9-bit integer)
        // A sprite moved past the right edge of the screen will wrap to `-256`, which is handled
        // by this check.
        if -w < x {
            if y <= self.scanline && y + h >= self.scanline {
                // Sprite is on scanline
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn read_sprite_tile_pixel(&self, tile: &SpriteTile, x_offset: u8) -> Option<Rgb> {
        debug_assert!(x_offset < 8);
        let rel_color = self.read_chr_entry(4,  // 16 colors
                                            tile.chr_addr,
                                            8,  // 8x8 tiles
                                            (x_offset as u8, tile.y_off));
        debug_assert!(rel_color < 16, "rel_color = {} (but is 4-bit!)", rel_color);

        // color index 0 is always transparent
        if rel_color == 0 { return None }

        let abs_color = 128 + tile.palette * 16 + rel_color;
        // FIXME Color math
        let rgb = self.lookup_color(abs_color);

        Some(rgb)
    }

    /// Returns the value of the current pixel on the sprite layer if it has the given priority
    /// (and `None` otherwise).
    pub fn maybe_draw_sprite_pixel(&self, prio: u8) -> Option<Rgb> {
        if self.tm & 0x10 == 0 { return None }  // OBJ layer disabled

        match self.sprite_render_state.sprite_scanline[self.x as usize] {
            None => None,   // No pixel or transparent pixel on OBJ layer
            Some((rgb, obj_prio)) => if obj_prio == prio {
                Some(rgb)
            } else {
                None
            },
        }
    }
}
