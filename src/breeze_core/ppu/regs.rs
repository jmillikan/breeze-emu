//! PPU Register accessor methods (meant for public API, subject to change)

// This will allow us to switch the representation (I'd like to try a simple array for the register
// values)

// FIXME Is this a good approach?

use super::Ppu;

macro_rules! fields_u8 {
    ( $($fld:ident)+ ) => {
        impl Ppu {
            $(
                pub fn $fld(&self) -> u8 { self.$fld }
            )+
        }
    };
}

macro_rules! fields_u16 {
    ( $($fld:ident)+ ) => {
        impl Ppu {
            $(
                pub fn $fld(&self) -> u16 { self.$fld }
            )+
        }
    };
}

fields_u8!(inidisp obsel bgmode mosaic bg1sc bg2sc bg3sc bg4sc bg12nba bg34nba vmain m7sel w12sel
    w34sel wobjsel wh0 wh1 wh2 wh3 wbglog wobjlog tm ts tmw tsw cgwsel cgadsub coldata_r coldata_g
    coldata_b setini);
fields_u16!(bg1hofs m7hofs bg1vofs m7vofs bg2hofs bg2vofs bg3hofs bg3vofs bg4hofs bg4vofs m7a m7b
    m7c m7d m7x m7y);
