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

const OAM_SIZE: usize = 544;
const CGRAM_SIZE: usize = 512;
const VRAM_SIZE: usize = 64 * 1024;
byte_array!(Oam[OAM_SIZE] with u16 indexing please);
byte_array!(Cgram[CGRAM_SIZE] with u16 indexing please);
byte_array!(Vram[VRAM_SIZE] with u16 indexing please);

#[derive(Default)]
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
    oam: Oam,

    /// CGRAM - Stores the color palette
    ///
    /// There are 256 colors in the palette, each 15 bits (5 bits per color channel), represented
    /// by 2 Bytes of CGRAM. Layout:
    /// `?bbbbbgg` `gggrrrrr` (the `?`-bit is ignored)
    ///
    /// FIXME LSB/MSB?
    cgram: Cgram,

    /// VRAM - Stores background maps and tile/character data
    ///
    /// The location of background maps can be selected with the registers `$2107-$210A`. An entry
    /// in these maps looks like this:
    /// `vhopppcc` `cccccccc`
    /// **V**ertical/**H**orizontal flip, Pri**o**rity bit, **P**alette number, **C**haracter/Tile
    /// starting number
    ///
    /// Character data locations are set with the registers `$210B` (BG1/2) and `$210C` (BG3/4).
    vram: Vram,

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

    /// `$2102` Low byte of current OAM word address ("reload value")
    oamaddl: u8,
    /// `$2103` High bit (bit 9) of OAM word address and priority rotation bit
    /// `p------b`
    /// * `p`: If set, give priority to sprite `(OAMAddr&0xFE)>>1` (internal OAM address)
    /// * `b`: High bit of OAM word address ("reload value")
    oamaddh: u8,
    /// Internal OAM address register (10 bit)
    oamaddr: u16,
    /// Byte written to the LSB of the current OAM address
    oam_lsb: u8,

    /// `$2106` Mosaic filter
    /// `xxxx4321`
    /// * `4321`: Enable mosaic filter for BG4/3/2/1
    /// * `xxxx`: Mosaic size in pixels (`0`: 1 pixel (default), `F`: 16 pixels)
    mosaic: u8,
    /// `$2107`-`$210a` BGx Tilemap Address and Size
    /// `aaaaaayx`
    /// * `a`: VRAM address is `aaaaaa << 10`
    /// * `y`: Vertical mirroring
    /// * `x`: Horizontal mirroring
    bg1sc: u8,
    bg2sc: u8,
    bg3sc: u8,
    bg4sc: u8,
    /// `$210b`/`$210c` BG Character Data Address
    /// `bbbbaaaa`
    /// * `b`: Base address for BG2/4 is `bbbb << 12`
    /// * `a`: Base address for BG1/3 is `bbbb << 12`
    bg12nba: u8,
    bg34nba: u8,

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
    /// `$211a` Mode 7 Settings
    /// `rc----xy`
    /// * `r`: Placing field size (`0` = 1024x1024, `1` = uhh... larger?)
    /// * `c`: 0 = Empty space is transparent, 1 = Fill empty space with character 0
    /// * `x`: Horizontal mirroring
    /// * `y`: Vertical mirroring
    m7sel: u8,

    /// `$2121` CGRAM word address (=color index)
    ///
    /// Automatically incremented after two writes to `$2122`.
    cgadd: u8,
    /// Store the low byte to write to the current CGRAM position after the high byte is written by
    /// the CPU (writes are always done in pairs - like the low 512 bytes of OAM).
    cg_low_buf: Option<u8>,

    /// `$212a` BG Window mask logic
    /// `44332211`
    ///
    /// `$212b` OBJ/Color Window mask logic
    /// `----ccoo`
    ///
    /// The 2 bits can select the boolean operator to apply:
    /// * `00 = OR`
    /// * `01 = AND`
    /// * `10 = XOR`
    /// * `11 = XNOR`
    wbglog: u8,
    wobjlog: u8,
    /// `$212c`/`$212d` Enable layers on main/sub screen
    /// `---o4321`
    /// OBJ layer, BG4/3/2/1
    tm: u8,
    ts: u8,
    /// `$212e`/`$212f` Enable window masking on main/sub screen
    /// `---o4321`
    tmw: u8,
    tsw: u8,

    /// `$2131` Color math
    /// `shbo4321`
    /// * `s`: 0 = Add, 1 = Subtract
    /// * `h`: Enable half-color math (the result of color math is divided by 2, in most cases)
    /// * `bo4321`: Enable color math on **B**ackdrop, **O**BJ, BG4/3/2/1
    cgadsub: u8,

    /// `$2133` Screen Mode/Video Select
    /// `se--poIi`
    /// * `s`: "External Sync" (should always be 0)
    /// * `e`: Allows enabling BG2 in mode 7
    /// * `p`: Pseudo-Hires
    /// * `o`: If set, 239 lines are rendered instead of 224. This changes the V-Blank timing.
    /// * `I`: OBJ interlace
    /// * `i`: Screen interlace. Doubles the effective screen height.
    setini: u8,
}

impl Ppu {
    pub fn new() -> Ppu { Ppu::default() }

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
            0x2102 => {
                self.oamaddl = value;
                self.update_oam_addr();
            }
            0x2103 => {
                self.oamaddh = value;
                self.update_oam_addr();
            }
            0x2104 => self.oam_store(value),
            0x2106 => self.mosaic = value,
            0x2107 => self.bg1sc = value,
            0x2108 => self.bg2sc = value,
            0x2109 => self.bg3sc = value,
            0x210a => self.bg4sc = value,
            0x210b => self.bg12nba = value,
            0x210c => self.bg34nba = value,
            0x2115 => self.vmain = value,
            0x2116 => self.vmaddr = (self.vmaddr & 0xff00) | value as u16,
            0x2117 => self.vmaddr = ((value as u16) << 8) | self.vmaddr & 0xff,
            0x2118 => self.vram_store_low(value),
            0x2119 => self.vram_store_high(value),
            0x211a => self.m7sel = value,
            0x2121 => {
                self.cgadd = value;
                self.cg_low_buf = None; // < FIXME: Is this correct?
            }
            0x2122 => match self.cg_low_buf {
                None => self.cg_low_buf = Some(value),
                Some(lo) => {
                    self.cgram[self.cgadd as u16 * 2] = lo;
                    self.cgram[self.cgadd as u16 * 2 + 1] = value;
                    self.cg_low_buf = None;
                    self.cgadd = self.cgadd.wrapping_add(1);
                }
            },
            0x212a => self.wbglog = value,
            0x212b => {
                if value & 0xf0 != 0 { panic!("invalid value for $212b: ${:02X}", value) }
                self.wobjlog = value;
            }
            0x212c => {
                if value & 0xe0 != 0 { panic!("invalid value for $212c: ${:02X}", value) }
                self.tm = value;
            }
            0x212d => {
                if value & 0xe0 != 0 { panic!("invalid value for $212d: ${:02X}", value) }
                self.ts = value;
            }
            0x212e => {
                if value & 0xe0 != 0 { panic!("invalid value for $212e: ${:02X}", value) }
                self.tmw = value;
            }
            0x212f => {
                if value & 0xe0 != 0 { panic!("invalid value for $212f: ${:02X}", value) }
                self.tsw = value;
            }
            0x2131 => self.cgadsub = value,
            0x2133 => {
                if value != 0 { panic!("NYI: $2133 != 0") }
                self.setini = value;
            }
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

/// Collected background settings
struct BgSettings {
    /// Mosaic pixel size. 1-16. 1 = Normal pixels.
    mosaic: u8,
    /// Tilemap address in VRAM
    tilemap_addr: u16,
    mirror_h: bool,
    mirror_v: bool,
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

    /// Update the internal OAM address register after a write to `$2102` or `$2103`
    fn update_oam_addr(&mut self) {
        self.oamaddr = (((self.oamaddh as u16 & 0x01) << 8) | self.oamaddl as u16) << 1;
    }
    fn oam_store(&mut self, val: u8) {
        if self.oamaddr < 0x200 {
            // Write to the first 512 Bytes
            if self.oamaddr & 0x01 == 0 {
                // Even address
                self.oam_lsb = val;
            } else {
                // Odd address
                // Address points to the MSB (=`val`) of the word we want to update
                self.oam[self.oamaddr - 1] = self.oam_lsb;
                self.oam[self.oamaddr] = val;
            }
        } else {
            // Write to 512-544
            self.oam[self.oamaddr] = val;
        }

        self.oamaddr = (self.oamaddr + 1) & 0x3f;   // reduce to 10 bits
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
        self.vram[self.vmaddr * 2] = data;
        self.vmaddr += inc;
    }
    /// Store to `$2119`. This writes the Byte to the current VRAM word address + 1 and increments
    /// it accordingly.
    fn vram_store_high(&mut self, data: u8) {
        let inc = if self.vmain & 0x80 != 0 { self.vram_addr_increment() } else { 0 };
        self.vram[self.vmaddr * 2 + 1] = data;
        self.vmaddr += inc;
    }

    /// Renders the current pixel. If in H/V/F-Blank, this does nothing.
    fn render_pixel(&mut self) {
        // TODO
    }
}
