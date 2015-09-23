//! Emulates the Picture Processing Unit.
//!
//! Documentation mostly taken from http://emu-docs.org/Super%20NES/General/snesdoc.html

/// Physical screen width
pub const SCREEN_WIDTH: u16 = 256;
/// Physical screen height
pub const SCREEN_HEIGHT: u16 = 224;     // 224px for 60 Hz NTSC, 264 for 50 Hz PAL

/// The result of an `update` call. Either H-Blank or V-Blank might get entered, and IRQs can be
/// caused.
#[derive(Default)]
pub struct UpdateResult {
    /// `true` if the last `update` rendered the last visible pixel on the current scanline
    pub hblank: bool,
    /// `true` if the last (invisible) H-Blank pixel of the last visible scanline was rendered
    pub vblank: bool,
    /// `true` if the current V-Blank was just left (but no visible pixels were rendered). The next
    /// `update` call will render the first (visible) pixel of a new frame.
    pub new_frame: bool,
}

pub struct Ppu {
    /// Object Attribute Memory
    ///
    /// The first 512 Bytes contain 4 Bytes per sprite (for a maximum of 128 simultaneous on-screen
    /// sprites). The remaining 32 Bytes contain additional 2 bits per sprite.
    ///
    /// Layout of an OAM entry (4 Bytes) in the first 512 Bytes:
    /// `xxxxxxxx` - Low 8 bits of **X** coordinate of the sprite (top left corner?)
    /// `yyyyyyyy` - **Y** coordinate
    /// `tttttttt` - Starting **t**ile/character number (low 8 bits)
    /// `vhoopppt` - **V**ertical/**H**orizontal flip, Pri**o**rity bits, **P**alette number,
    ///     Bit 9 (most significant bit) of starting **t**ile number.
    ///
    /// Layout of a byte in the last 32 Bytes of OAM:
    /// `xsxsxsxs` - **S**ize toggle bit and most significant bit of **X** coordinate
    /// The low bits contain the information for sprites with low IDs.
    oam: [u8; 544],

    /// CGRAM - Stores the color palette
    ///
    /// There are 256 colors in the palette, each 15 bits (5 bits per color channel), represented
    /// by 2 Bytes of CGRAM. Layout:
    /// `?bbbbbgg` `gggrrrrr` (the `?`-bit is ignored)
    ///
    /// TODO LSB/MSB?
    cgram: [u8; 512],

    /// VRAM - Stores background maps and tile/character data
    ///
    /// The location of background maps can be selected with the registers `$2107-$210A`. An entry
    /// in these maps looks like this:
    /// `vhopppcc` `cccccccc`
    /// **V**ertical/**H**orizontal flip, Pri**o**rity bit, **P**alette number, **C**haracter/Tile
    /// starting number
    ///
    /// Character data locations are set with the registers `$210B` (BG1/2) and `$210C` (BG3/4).
    vram: [u8; 64 * 1024],

    /// Scanline counter
    ///
    /// "The SNES runs 1 scanline every 1364 master cycles, except in non-interlace mode scanline
    /// $f0 of every other frame (those with $213f.7=1) is only 1360 cycles. Frames are 262
    /// scanlines in non-interlace mode, while in interlace mode frames with $213f.7=0 are 263
    /// scanlines. V-Blank runs from either scanline $e1 or $f0 until the end of the frame."
    scanline: u16,

    /// Horizontal pixel counter
    ///
    /// Each call of the `update` method will advance this counter by exactly 1. Since the
    /// horizontal resolution of a frame is 256 pixels, H-Blank is started at `x=256`, or the 257th
    /// pixel. The last X coordinate in H-Blank is `x=339`. At the end of the `update` call (when
    /// `x=399`), `x` will be reset to 0 and `scanline` will be incremented.
    x: u16,
}

/// Unpacked OAM entry for internal use.
struct OamEntry {
    /// 0-511
    tile: u16,
    /// 0-511
    x: u16,
    y: u8,
    /// 0-3
    priority: u8,
    /// 0-7
    palette: u8,
    hflip: bool,
    vflip: bool,
    size_toggle: bool,
}

impl Ppu {
    pub fn new() -> Ppu {
        Ppu {
            oam: [0; 544],
            cgram: [0; 512],
            vram: [0; 64 * 1024],
            scanline: 0,
            x: 0,
        }
    }

    /// Load a PPU register ($2134 - $213f)
    pub fn load(&mut self, addr: u16) -> u8 {
        panic!("PPU register load unimplemented (${:04X})", addr)
    }

    /// Store a byte in a PPU register ($2100 - $2133)
    pub fn store(&mut self, addr: u16, value: u8) {
        trace!("PPU store: ${:02X} in ${:04X}", value, addr)
    }

    /// Runs the PPU for a bit.
    ///
    /// This will render exactly one pixel (when in V/H-Blank, the pixel counter will be
    /// incremented, but obviously nothing will be drawn).
    pub fn update(&mut self) -> (u8, UpdateResult) {
        // FIXME Does each pixel take *exactly* 4 master clock cycles?
        self.render_pixel();

        self.x += 1;
        let mut result = UpdateResult::default();
        match self.x {
            256 => {
                // H-Blank starts now!
                result.hblank = true;
            }
            340 => {
                // H-Blank ends now!
                self.x = 0;
                self.scanline += 1;
                match self.scanline {
                    SCREEN_HEIGHT => {
                        // V-Blank starts now!
                        result.vblank = true;
                    }
                    262 => {
                        // V-Blank ends now! The next `update` call will render the first visible
                        // pixel of a new frame.
                        result.new_frame = true;
                        self.scanline = 0;
                    }
                    _ => {}
                }
            }
            _ => {}
        }

        (4, result)
    }
}

/// Private methods
impl Ppu {
    fn in_h_blank(&self) -> bool { self.x >= 256 }
    fn in_v_blank(&self) -> bool { self.scanline >= SCREEN_HEIGHT }

    /// Renders the current pixel. If in H- or V-Blank, this does nothing.
    fn render_pixel(&mut self) {
    }
}
