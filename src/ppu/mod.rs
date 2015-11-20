//! Emulates the Picture Processing Unit.
//!
//! We emulate the NTSC console, which has a refresh rate of 60 Hz and a screen resolution of
//! 256x224 pixels by default.
//!
//! Documentation mostly taken from http://emu-docs.org/Super%20NES/General/snesdoc.html and
//! http://wiki.superfamicom.org/

mod rendering;

use self::rendering::RenderState;

/// Physical screen width
/// (this is the width of a field, or a half-frame)
pub const SCREEN_WIDTH: u32 = 256;
/// Physical screen height
/// (this is the height of a field, or a half-frame)
pub const SCREEN_HEIGHT: u32 = 224;     // 224px for 60 Hz NTSC, 264 for 50 Hz PAL

const OAM_SIZE: usize = 544;
const CGRAM_SIZE: usize = 512;
const VRAM_SIZE: usize = 64 * 1024;
const FRAME_BUF_SIZE: usize = SCREEN_WIDTH as usize * SCREEN_HEIGHT as usize * 3;
byte_array!(Oam[OAM_SIZE] with u16 indexing please);
byte_array!(Cgram[CGRAM_SIZE] with u16 indexing please);
byte_array!(Vram[VRAM_SIZE] with u16 indexing please);
byte_array!(pub FrameBuf[FRAME_BUF_SIZE]);

#[derive(Default)]
pub struct Ppu {
    /// PPU frame buffer. Contains raw RGB pixel data in `RGB24` format: The first byte is the red
    /// component of the pixel in the top left corner of the frame, the second byte is the green
    /// component and the third byte is the blue component. The fourth byte is then the red
    /// component of the second pixel (at coordinate `(1,0)`), and so on.
    ///
    /// FIXME The size can change depending on the PPU config, make sure all frames fit in
    /// FIXME How would this work in high resolution modes?
    pub framebuf: FrameBuf,

    /// Opaque state object used by the render code. This value may change between frames.
    render_state: RenderState,

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
    /// `sxsxsxsx` - **S**ize toggle bit and most significant bit of **X** coordinate
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

    /// `$2105` BG mode and character size
    /// `4321emmm`
    /// * `4321`: Use 16x16 tiles for BG**4**/**3**/**2**/**1** (if possible in this mode)
    /// * `e`: Mode 1 BG3 priority bit. If set, BG3 tiles with priority 1 have the highest
    ///   priority. If not set, these tiles have the third lowest priority (and end up between
    ///   sprites with priority 0 and 1).
    /// * `mmm`: BG mode
    bgmode: u8,
    /// `$2106` Mosaic filter
    /// `xxxx4321`
    /// * `4321`: Enable mosaic filter for BG4/3/2/1
    /// * `xxxx`: Mosaic size in pixels (`0`: 1 pixel (default), `F`: 16 pixels)
    mosaic: u8,
    /// `$2107`-`$210a` BGx Tilemap Address and Size
    /// `aaaaaayx`
    /// * `a`: VRAM address is `aaaaaa << 10`
    /// * `y`: Vertical mirroring off (when 0, the bottom tile maps are mirrored from the top)
    /// * `x`: Horizontal mirroring off (when 0, the right tile maps are mirrored from the left)
    ///
    /// The value of `yx` defines the number of 32x32 tilemaps used (1, 2 or 4) as well as their
    /// layout:
    ///     00  32x32   AA
    ///                 AA
    ///     01  64x32   AB
    ///                 AB
    ///     10  32x64   AA
    ///                 BB
    ///     11  64x64   AB
    ///                 CD
    /// FIXME Is this mirroring stuff correct? It's confusing as hell!
    bg1sc: u8,
    bg2sc: u8,
    bg3sc: u8,
    bg4sc: u8,
    /// `$210b` BG1/2 Character Data Address
    /// `bbbbaaaa`
    /// * `b`: Base address for BG2 is `bbbb << 12`
    /// * `a`: Base address for BG1 is `aaaa << 12`
    bg12nba: u8,
    /// `$210c` BG3/4 Character Data Address
    /// `bbbbaaaa`
    /// * `b`: Base address for BG4 is `bbbb << 12`
    /// * `a`: Base address for BG3 is `aaaa << 12`
    bg34nba: u8,

    /// `$210d` BG1 Horizontal Scroll
    /// BG1 offset is 10 bits
    bg1hofs: u16,
    /// `$210d` Mode 7 BG Horizontal Scroll
    /// 13 bits signed. Updated via the same register as `BG1HOFS`, but using the `m7_old`
    /// mechanism, which works differently than the `bg_old` mechanism below.
    m7hofs: u16,
    /// `$210e` BG1 Vertical Scroll
    bg1vofs: u16,
    /// `$210e` Mode 7 BG Vertical Scroll
    m7vofs: u16,
    /// `$210f` BG2 Horizontal Scroll
    bg2hofs: u16,
    /// `$2110` BG2 Vertical Scroll
    bg2vofs: u16,
    bg3hofs: u16,
    bg3vofs: u16,
    bg4hofs: u16,
    /// `$2114`
    bg4vofs: u16,
    /// BG registers are 16-bit "write-twice" registers. Their value is updated as follows
    /// (assuming `VAL` is the value written to the register):
    /// ```
    /// BGnHOFS = (VAL<<8) | (bg_old&~7) | ((BGnHOFS>>8)&7);
    /// bg_old = VAL;
    ///    or
    /// BGnVOFS = (VAL<<8) | bg_old;
    /// bg_old = VAL;
    /// ```
    bg_old: u8,
    /// Mode 7 16-bit "write-twice" registers are updated like this:
    /// ```
    /// m7_reg = new * 100h + m7_old
    /// m7_old = new
    /// ```
    m7_old: u8,

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
    /// `$211b` Mode 7 Matrix A
    ///
    /// The mode 7 matrix transformation works like this (where `SX/SY` are the current screen
    /// coordinates and `X/Y` the resulting playing field coordinates that will be looked up):
    /// ```
    /// [ X ]   [ A B ]   [ SX + M7HOFS - M7X ]   [ M7X ]
    /// [   ] = [     ] * [                   ] + [     ]
    /// [ Y ]   [ C D ]   [ SY + M7VOFS - M7Y ]   [ M7Y ]
    /// ```
    m7a: u16,
    /// `$211c` Mode 7 Matrix B
    m7b: u16,
    /// Last value written to `m7b` / `$211c`
    m7b_last: u8,
    /// `$211d` Mode 7 Matrix C
    m7c: u16,
    /// `$211e` Mode 7 Matrix D
    m7d: u16,
    /// `$211f` Mode 7 Center X
    m7x: u16,
    /// `$2120` Mode 7 Center Y
    m7y: u16,

    /// `$2121` CGRAM word address (=color index)
    ///
    /// Automatically incremented after two writes to `$2122`.
    cgadd: u8,
    /// Store the low byte to write to the current CGRAM position after the high byte is written by
    /// the CPU (writes are always done in pairs - like the low 512 bytes of OAM).
    /// FIXME: Is this correct?
    cg_low_buf: Option<u8>,

    /// `$2123` Window Mask Settings for BG1 and BG2
    /// `ABCDabcd`
    /// * `A`: Enable window 2 for BG2
    /// * `B`: Invert window 2 for BG2
    /// * `C`: Enable window 1 for BG2
    /// * `D`: Invert window 1 for BG2
    /// * `a`: Enable window 2 for BG1
    /// * `b`: Invert window 2 for BG1
    /// * `c`: Enable window 1 for BG1
    /// * `d`: Invert window 1 for BG1
    w12sel: u8,
    /// `$2124` Window Mask Settings for BG3 and BG4
    /// `ABCDabcd`
    /// * `A`: Enable window 2 for BG4
    /// * `B`: Invert window 2 for BG4
    /// * `C`: Enable window 1 for BG4
    /// * `D`: Invert window 1 for BG4
    /// * `a`: Enable window 2 for BG3
    /// * `b`: Invert window 2 for BG3
    /// * `c`: Enable window 1 for BG3
    /// * `d`: Invert window 1 for BG3
    w34sel: u8,
    /// `$2125` Window Mask Settings for OBJ and Color Window
    /// `ABCDabcd`
    /// * `A`: Enable window 2 for Color
    /// * `B`: Invert window 2 for Color
    /// * `C`: Enable window 1 for Color
    /// * `D`: Invert window 1 for Color
    /// * `a`: Enable window 2 for OBJ
    /// * `b`: Invert window 2 for OBJ
    /// * `c`: Enable window 1 for OBJ
    /// * `d`: Invert window 1 for OBJ
    wobjsel: u8,
    /// `$2126` Window 1 Left Position
    /// (this should really be called W1L)
    /// * `0`: Leftmost
    /// * `255`: Rightmost
    wh0: u8,
    /// `$2127` Window 1 Right Position
    wh1: u8,
    /// `$2128` Window 2 Left Position
    wh2: u8,
    /// `$2129` Window 2 Right Position
    wh3: u8,
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
    /// **O**BJ layer, BG**4**/**3**/**2**/**1**
    tm: u8,
    ts: u8,
    /// `$212e`/`$212f` Enable window masking on main/sub screen
    /// `---o4321`
    /// **O**BJ layer, BG**4**/**3**/**2**/**1**
    tmw: u8,
    tsw: u8,

    /// `$2130` Color Addition Select
    /// `ccmm--sd`
    /// * `cc`: Clip colors to black before math
    ///  * `00`: Never
    ///  * `01`: Outside Color Window
    ///  * `10`: Inside Color Window
    ///  * `11`: Always
    /// * `mm`: Prevent color math (same meaning as `cc`)
    /// * `s`: Add subscreen pixel instead of fixed color
    /// * `d`: Direct color mode for 256-color BGs
    cgwsel: u8,
    /// `$2131` Color math
    /// `shbo4321`
    /// * `s`: 0 = Add, 1 = Subtract
    /// * `h`: Enable half-color math (the result of color math is divided by 2, in most cases)
    /// * `bo4321`: Enable color math on **B**ackdrop, **O**BJ, BG4/3/2/1
    cgadsub: u8,

    /// `$2132` COLDATA: Fixed color data
    /// Each write can set 0-3 color planes (RGB), so we store them separately, which makes things
    /// easier.
    coldata_r: u8,
    coldata_g: u8,
    coldata_b: u8,

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

impl_save_state!(Ppu {
    oam, cgram, vram, inidisp, obsel, oamaddl, oamaddh, oamaddr, oam_lsb, bgmode, mosaic, bg1sc,
    bg2sc, bg3sc, bg4sc, bg12nba, bg34nba, bg1hofs, m7hofs, bg1vofs, m7vofs, bg2hofs, bg2vofs,
    bg3hofs, bg3vofs, bg4hofs, bg4vofs, bg_old, m7_old, vmain, vmaddr, m7sel, m7a, m7b, m7b_last,
    m7c, m7d, m7x, m7y, cgadd, cg_low_buf, w12sel, w34sel, wobjsel, wh0, wh1, wh2, wh3, wbglog,
    wobjlog, tm, ts, tmw, tsw, cgwsel, cgadsub, coldata_r, coldata_g, coldata_b, setini, scanline
} ignore {
    framebuf, render_state, x
});

#[derive(Debug)]
struct Rgb {
    r: u8,
    g: u8,
    b: u8,
}

impl Ppu {
    /// Load a PPU register (addresses `$2134` to `$213f`)
    pub fn load(&mut self, addr: u16) -> u8 {
        match addr {
            // `$2134` - `$2136`: Multiplication Result of `self.m7a * self.m7b_last`
            // MPYL - Low Byte
            0x2134 => (self.m7a as u32 * self.m7b_last as u32) as u8,
            // MPYM - Middle Byte
            0x2135 => ((self.m7a as u32 * self.m7b_last as u32) >> 8) as u8,
            // MPYH - High Byte
            0x2136 => ((self.m7a as u32 * self.m7b_last as u32) >> 16) as u8,
            _ => panic!("invalid PPU load from ${:04X}", addr),
        }
    }

    /// Store a byte in a PPU register (addresses `$2100` - `$2133`)
    pub fn store(&mut self, addr: u16, value: u8) {
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
            0x2105 => self.bgmode = value,
            0x2106 => self.mosaic = value,
            0x2107 => self.bg1sc = value,
            0x2108 => self.bg2sc = value,
            0x2109 => self.bg3sc = value,
            0x210a => self.bg4sc = value,
            0x210b => self.bg12nba = value,
            0x210c => self.bg34nba = value,
            0x210d | 0x210e => {
                self.bg_store(addr, value);
                self.m7_store(addr, value);
            }
            0x210f ... 0x2114 => self.bg_store(addr, value),
            0x2115 => self.vmain = value,
            0x2116 => self.vmaddr = (self.vmaddr & 0xff00) | value as u16,
            0x2117 => self.vmaddr = ((value as u16) << 8) | self.vmaddr & 0xff,
            0x2118 => self.vram_store_low(value),
            0x2119 => self.vram_store_high(value),
            0x211a => self.m7sel = value,
            0x211b ... 0x2120 => self.m7_store(addr, value),
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
            0x2123 => self.w12sel = value,
            0x2124 => self.w34sel = value,
            0x2125 => self.wobjsel = value,
            0x2126 => self.wh0 = value,
            0x2127 => self.wh1 = value,
            0x2128 => self.wh2 = value,
            0x2129 => self.wh3 = value,
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
            0x2130 => self.cgwsel = value,
            0x2131 => self.cgadsub = value,
            0x2132 => {
                let color = value & 0x1f;
                if value & 0x80 != 0 { self.coldata_b = color; }
                if value & 0x40 != 0 { self.coldata_g = color; }
                if value & 0x20 != 0 { self.coldata_r = color; }
            }
            0x2133 => {
                if value != 0 {
                    // FIXME When implementing this, we'll need to dynamically adjust the
                    // framebuffer size and various timings (needs `fn overscan() -> bool`).
                    panic!("NYI: $2133 != 0")
                }
                self.setini = value;
            }
            _ => panic!("invalid or unimplemented PPU store: ${:02X} to ${:04X}", value, addr),
        }
    }

    /// Runs the PPU for a bit.
    ///
    /// This will render exactly one pixel (when in H/V-Blank, the pixel counter will be
    /// incremented, but obviously nothing will be drawn).
    pub fn update(&mut self) -> u8 {
        if !self.in_h_blank() && !self.in_v_blank() {
            // This pixel is visible
            let pixel;
            if self.forced_blank() {
                pixel = Rgb {r: 0, g: 0, b: 0};
            } else {
                // "Normal" pixel
                pixel = self.render_pixel();
            }

            let x = self.x;
            let y = self.scanline;
            self.set_pixel(x, y, pixel);
        }

        self.x += 1;
        if self.x == 340 {
            // End of H-Blank
            self.x = 0;
            self.scanline += 1;
            if self.scanline == 262 {
                // V-Blank ends now. The next `update` call will render the first visible pixel of
                // a new frame.
                self.scanline = 0;
            }
        }

        // FIXME Not all pixels take 4 master cycles
        4
    }

    pub fn in_h_blank(&self) -> bool { self.x >= 256 }
    pub fn in_v_blank(&self) -> bool { self.scanline as u32 >= SCREEN_HEIGHT }
    pub fn forced_blank(&self) -> bool { self.inidisp & 0x80 != 0 }
    #[allow(dead_code)] // FIXME: Take this into account
    fn brightness(&self) -> u8 { self.inidisp & 0xf }
    pub fn h_counter(&self) -> u16 { self.x }
    pub fn v_counter(&self) -> u16 { self.scanline }

    fn set_pixel(&mut self, x: u16, y: u16, rgb: Rgb) {
        let start = (y as usize * SCREEN_WIDTH as usize + x as usize) * 3;
        self.framebuf[start] = rgb.r;
        self.framebuf[start+1] = rgb.g;
        self.framebuf[start+2] = rgb.b;
    }

    /// Store a byte to a "write-twice" `BGnxOFS` register
    fn bg_store(&mut self, addr: u16, val: u8) {
        let reg = match addr {
            0x210d => &mut self.bg1hofs,
            0x210e => &mut self.bg1vofs,
            0x210f => &mut self.bg2hofs,
            0x2110 => &mut self.bg2vofs,
            0x2111 => &mut self.bg3hofs,
            0x2112 => &mut self.bg3vofs,
            0x2113 => &mut self.bg4hofs,
            0x2114 => &mut self.bg4vofs,
            _ => panic!("invalid BG register ${:04X}", addr),
        };

        if addr & 1 != 0 {
            // Horizontal
            *reg = ((val as u16) << 8) | (self.bg_old as u16 & !7) | ((*reg >> 8) & 7);
        } else {
            // Vertical
            *reg = ((val as u16) << 8) | self.bg_old as u16;
        }

        self.bg_old = val;
    }

    /// Store a byte to a "write-twice" Mode 7 register
    fn m7_store(&mut self, addr: u16, val: u8) {
        let reg = match addr {
            0x210d => &mut self.m7hofs,
            0x210e => &mut self.m7vofs,
            0x211b => &mut self.m7a,
            0x211c => {
                self.m7b_last = val;
                &mut self.m7b
            },
            0x211d => &mut self.m7c,
            0x211e => &mut self.m7d,
            0x211f => &mut self.m7x,
            0x2120 => &mut self.m7y,
            _ => panic!("invalid Mode 7 write-twice register ${:04X}", addr),
        };

        *reg = (val as u16) << 8 | self.m7_old as u16;
        self.m7_old = val;
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
        if self.oamaddr == 544 { panic!() }
        self.oamaddr = (self.oamaddr + 1) & 0x3ff;   // reduce to 10 bits
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
        let inc = if self.vmain & 0x80 == 0 { self.vram_addr_increment() } else { 0 };
        let addr = self.vram_translate_addr(self.vmaddr * 2);
        self.vram[addr] = data;
        self.vmaddr += inc;
    }
    /// Store to `$2119`. This writes the Byte to the current VRAM word address + 1 and increments
    /// it accordingly.
    fn vram_store_high(&mut self, data: u8) {
        let inc = if self.vmain & 0x80 == 0 { 0 } else { self.vram_addr_increment() };
        let addr = self.vram_translate_addr(self.vmaddr * 2 + 1);
        self.vram[addr] = data;
        self.vmaddr += inc;
    }
}
