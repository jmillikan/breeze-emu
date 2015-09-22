//! Emulates the Picture Processing Unit.
//!
//! Documentation mostly taken from http://emu-docs.org/Super%20NES/General/snesdoc.html

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
    ///     Bit 9 (most significant bit) of starting tile number.
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
}
