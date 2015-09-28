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
    /// FIXME LSB/MSB?
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
    /// pixel. The last X coordinate in H-Blank is `x=339`. At the end of the `update` call (during
    /// which `x=339`), `x` will be reset to 0 and `scanline` will be incremented.
    x: u16,

    /// `$2100` - Screen Display register
    /// `x---bbbb`
    /// * `x`: Force blank (F-Blank)
    /// * `b`: Brightness (0=black, 15=max)
    inidisp: u8,
    /// `$2101` - Object Size and Name Base Address
    /// `sssnnbbb`
    /// * `sss`: Object/Sprite size
    ///  * 000 =  8x8  and 16x16 sprites
    ///  * 001 =  8x8  and 32x32 sprites
    ///  * 010 =  8x8  and 64x64 sprites
    ///  * 011 = 16x16 and 32x32 sprites
    ///  * 100 = 16x16 and 64x64 sprites
    ///  * 101 = 32x32 and 64x64 sprites
    ///  * 110 = 16x32 and 32x64 sprites ('undocumented')
    ///  * 111 = 16x32 and 32x32 sprites ('undocumented')
    /// * `nn`: Name select - Offset between sprite/name tables
    /// * `bbb`: Address select of first sprite/name table
    obsel: u8,

    /// `$2102` Low byte of current OAM word address
    oamaddl: u8,
    /// `$2103` High bit (bit 9) of OAM word address and priority rotation bit
    /// `p------b`
    /// * `p`: If set, give priority to sprite `(OAMAddr&0xFE)>>1` (internal OAM address)
    /// * `b`: High bit of OAM word address
    oamaddh: u8,

    /// `$2115` Video Port Control (VRAM)
    /// `j---mmii`
    /// * `j`: Address increment mode
    ///  * 0 = increment after writing `$2118`/reading `$2139`
    ///  * 1 = increment after writing `$2119`/reading `$213a`
    /// * `m`: VRAM Address remapping
    ///  * 00 = None
    ///  * 01 = Remap addressing aaaaaaaaBBBccccc => aaaaaaaacccccBBB
    ///  * 10 = Remap addressing aaaaaaaBBBcccccc => aaaaaaaccccccBBB
    ///  * 11 = Remap addressing aaaaaaBBBccccccc => aaaaaacccccccBBB
    /// * `i`: Word address increment amount (00 => 1, 01 => 32, 10/11 => 128)
    vmain: u8,
    /// `$2116`/`$2117` Low/High byte of VRAM word address
    vmaddr: u16,
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
            inidisp: 0x80,  // F-Blank on by default
            obsel: 0,
            oamaddl: 0,
            oamaddh: 0,
            vmain: 0,
            vmaddr: 0,
        }
    }

    /// Load a PPU register (addresses `$2134` to `$213f`)
    pub fn load(&mut self, addr: u16) -> u8 {
        panic!("PPU register load unimplemented (${:04X})", addr)
    }

    /// Store a byte in a PPU register (addresses `$2100` - `$2133`)
    pub fn store(&mut self, addr: u16, value: u8) {
        trace_unique!("PPU store (first only): ${:02X} in ${:04X}", value, addr);

        match addr {
            0x2100 => self.inidisp = value,
            0x2101 => self.obsel = value,
            0x2102 => self.oamaddl = value,
            0x2103 => self.oamaddh = value,
            0x2115 => self.vmain = value,
            0x2116 => self.vmaddr = (self.vmaddr & 0xff00) | value as u16,
            0x2117 => self.vmaddr = ((value as u16) << 8) | self.vmaddr & 0xff,
            0x2118 => self.vram_store_low(value),
            0x2119 => self.vram_store_high(value),
            _ => panic!("invalid or unimplemented PPU store: ${:02X} to ${:04X}", value, addr),
        }
    }

    /// Runs the PPU for a bit.
    ///
    /// This will render exactly one pixel (when in H/V-Blank, the pixel counter will be
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
    fn force_blank(&self) -> bool { self.inidisp & 0x80 != 0 }
    fn brightness(&self) -> u8 { self.inidisp & 0xf }

    /// Get the configured sprite size in pixels
    fn obj_size(&self, alt: bool) -> (u8, u8) {
        match self.obsel & 0b111 {
            0b000 => if !alt {(8,8)} else {(16,16)},
            0b001 => if !alt {(8,8)} else {(32,32)},
            0b010 => if !alt {(8,8)} else {(64,64)},
            0b011 => if !alt {(16,16)} else {(32,32)},
            0b100 => if !alt {(16,16)} else {(64,64)},
            0b101 => if !alt {(32,32)} else {(64,64)},
            //0b110 => if !alt {(16,32)} else {(32,64)},
            //0b111 => if !alt {(16,32)} else {(32,32)},
            invalid => panic!("invalid sprite size selected: {:b} (OBSEL = ${:02X})",
                invalid, self.obsel)
        }
    }

    /// Get the value to increment the VRAM word address by
    fn vram_addr_increment(&self) -> u16 {
        match self.vmain & 0b11 {
            0b00 => 1,
            0b01 => 32,
            _ => 128,   // 0b10 | 0b11
        }
    }
    /// Translate a VRAM word address according to the address translation bits of `$2115`
    fn vram_translate_addr(&self, addr: u16) -> u16 {
        // * 00 = None
        // * 01 = Remap addressing aaaaaaaaBBBccccc => aaaaaaaacccccBBB
        // * 10 = Remap addressing aaaaaaaBBBcccccc => aaaaaaaccccccBBB
        // * 11 = Remap addressing aaaaaaBBBccccccc => aaaaaacccccccBBB
        let trans = (self.vmain & 0b1100) >> 2;
        match trans {
            0b00 => addr,
            0b01 => panic!("NYI: VRAM address translation"),   // FIXME
            0b10 => panic!("NYI: VRAM address translation"),
            0b11 => panic!("NYI: VRAM address translation"),
            _ => unreachable!(),
        }
    }
    /// Store to `$2118`. This writes the Byte to the current VRAM word address and increments it
    /// accordingly.
    fn vram_store_low(&mut self, data: u8) {
        let inc = if self.vmain & 0x80 != 0 { 0 } else { self.vram_addr_increment() };
        self.vram[self.vmaddr as usize * 2] = data;
        self.vmaddr += inc;
    }
    /// Store to `$2119`. This writes the Byte to the current VRAM word address + 1 and increments
    /// it accordingly.
    fn vram_store_high(&mut self, data: u8) {
        let inc = if self.vmain & 0x80 != 0 { self.vram_addr_increment() } else { 0 };
        self.vram[self.vmaddr as usize * 2 + 1] = data;
        self.vmaddr += inc;
    }

    /// Renders the current pixel. If in H/V/F-Blank, this does nothing.
    fn render_pixel(&mut self) {
    }
}
