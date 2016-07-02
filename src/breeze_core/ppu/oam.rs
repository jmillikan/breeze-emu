//! OAM (Object Attribute Memory) representation and utilities

/// Object Attribute Memory size in Bytes
pub const OAM_SIZE: usize = 544;

byte_array!(pub Oam[OAM_SIZE] with u16 indexing, save state please);

/// Unpacked OAM entry for easier use
#[derive(Copy, Clone, Default)]
pub struct OamEntry {
    /// First tile (0-255), needs to take name table selection bit into account
    pub tile: u8,
    /// Sprite's name table (0 or 1).
    pub name_table: u8,
    /// 9 bits, considered signed (-256 - 255)
    pub x: i16,
    pub y: u8,
    /// 0-3
    pub priority: u8,
    /// 0-7. The first palette entry is `128+ppp*16`.
    pub palette: u8,
    pub hflip: bool,
    pub vflip: bool,
    pub size_toggle: bool,
}

impl Oam {
    /// Returns the OAM entry of the given sprite. Always returns a valid entry if `index` is valid
    /// (0...127), panics otherwise.
    pub fn get_sprite(&self, index: u8) -> OamEntry {
        debug_assert!(index <= 127, "attempted to access sprite #{}", index);

        // OAM entry start address (low table)
        let start = index as u16 * 4;
        let mut x = self[start] as u16;

        // vhoopppN
        let byte4 = self[start + 3];
        let vflip = byte4 & 0x80 != 0;
        let hflip = byte4 & 0x40 != 0;
        let priority = (byte4 & 0x30) >> 4;
        let palette = (byte4 & 0x0e) >> 1;

        // Read the second table. Each byte contains information of 4 sprites (2 bits per sprite):
        // Bits 1/3/5/6 is the size-toggle bit, bits 0/2/4/6 is the MSb of the x coord
        let byte = self[512 + index as u16 / 4];
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
            tile: self[start + 2],
            name_table: byte4 & 1,
            x: x as i16,
            y: self[start + 1],
            priority: priority,
            palette: palette,
            hflip: hflip,
            vflip: vflip,
            size_toggle: size_toggle,
        }
    }
}
