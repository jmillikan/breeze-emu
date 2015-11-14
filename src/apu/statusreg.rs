use std::fmt;

pub struct StatusReg(pub u8);
const NEG_FLAG: u8         = 0x80;
const OVERFLOW_FLAG: u8    = 0x40;
const DIRECT_PAGE_FLAG: u8 = 0x20;
const HALF_CARRY_FLAG: u8  = 0x08;
const ZERO_FLAG: u8        = 0x02;
const CARRY_FLAG: u8       = 0x01;

impl_save_state_for_newtype!(StatusReg);

impl StatusReg {
    pub fn negative(&self) -> bool    { self.0 & NEG_FLAG != 0 }
    pub fn zero(&self) -> bool        { self.0 & ZERO_FLAG != 0 }
    pub fn direct_page(&self) -> bool { self.0 & DIRECT_PAGE_FLAG != 0 }
    pub fn carry(&self) -> bool       { self.0 & CARRY_FLAG != 0 }
    pub fn half_carry(&self) -> bool  { self.0 & HALF_CARRY_FLAG != 0 }
    pub fn overflow(&self) -> bool    { self.0 & OVERFLOW_FLAG != 0 }

    fn set(&mut self, flag: u8, v: bool) {
        match v {
            true => self.0 |= flag,
            false => self.0 &= !flag,
        }
    }

    pub fn set_negative(&mut self, v: bool)    { self.set(NEG_FLAG, v) }
    pub fn set_zero(&mut self, v: bool)        { self.set(ZERO_FLAG, v) }
    pub fn set_direct_page(&mut self, v: bool) { self.set(DIRECT_PAGE_FLAG, v) }
    pub fn set_carry(&mut self, v: bool)       { self.set(CARRY_FLAG, v) }
    pub fn set_half_carry(&mut self, v: bool)  { self.set(HALF_CARRY_FLAG, v) }
    pub fn set_overflow(&mut self, v: bool)    { self.set(OVERFLOW_FLAG, v) }

    pub fn set_nz(&mut self, val: u8) -> u8 {
        self.set_negative(val & 0x80 != 0);
        self.set_zero(val == 0);
        val
    }
}

impl fmt::Display for StatusReg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(f.write_str(if self.negative() { "N" } else { "-" }));
        try!(f.write_str(if self.overflow() { "V" } else { "-" }));
        try!(f.write_str(if self.direct_page() { "D" } else { "-" }));
        try!(f.write_str("x"));
        try!(f.write_str(if self.half_carry() { "H" } else { "-" }));
        try!(f.write_str("x"));
        try!(f.write_str(if self.zero() { "Z" } else { "-" }));
        try!(f.write_str(if self.carry() { "C" } else { "-" }));

        Ok(())
    }
}
