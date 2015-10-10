//! The SPC700's addressing modes

use super::Spc700;

use std::fmt;

/// An addressing mode of the SPC700.
///
/// As a safety measure, the `loadb` and `storeb` methods take the mode by value. We derive `Clone`
/// to make multiple uses explicit.
#[derive(Clone)]
pub enum AddressingMode {
    Immediate(u8),
    /// Direct Page, uses the Direct Page status bit to determine if page 0 or 1 should be accessed
    /// Address = `D + $ab`
    Direct(u8),
    /// Address = `D + $ab + X`
    DirectIndexedX(u8),
    /// Where X points to (in page 0, $00 - $ff)
    /// Address = `X`
    IndirectX,
    /// Fetch the word address at a direct address (this is the "indirect" part), then index the
    /// fetched address with Y.
    /// Address = `[D + $ab] + Y`
    IndirectIndexed(u8),
    /// Index direct address with X, then "indirect" by fetching the word address stored there.
    /// Address = `[D + $ab + X]`
    IndexedIndirect(u8),
    /// Fetch the target word address from an absolute address + X (`[$abcd+X]`)
    /// (only used for `JMP`)
    AbsIndexedIndirect(u16),
    /// Absolute address
    /// Address = `!abcd`
    Abs(u16),
    /// Address = `!abcd+X`
    AbsIndexedX(u16),
    /// Address = `!abcd+Y`
    AbsIndexedY(u16),
    /// Used for branch instructions
    Rel(i8),
    A,
    X,
    Y,
}

impl AddressingMode {
    pub fn loadb(self, spc: &mut Spc700) -> u8 {
        use self::AddressingMode::*;

        match self {
            Immediate(val) => val,
            A => spc.a,
            X => spc.x,
            Y => spc.y,
            _ => {
                let addr = self.address(spc);
                spc.load(addr)
            }
        }
    }

    /// Loads a word. Returns low and high byte.
    pub fn loadw(self, spc: &mut Spc700) -> (u8, u8) {
        use self::AddressingMode::*;

        let addr = self.address(spc);
        let addr2;  // address of second (high) byte
        if let Direct(_) = self {
            // Direct Page access will wrap in the page
            addr2 = (addr & 0xff00) | (addr as u8).wrapping_add(1) as u16;   // low byte wraps
        } else {
            // FIXME wrapping
            addr2 = addr + 1;
        }

        let lo = spc.load(addr);
        let hi = spc.load(addr2);
        (lo, hi)
    }

    pub fn storeb(self, spc: &mut Spc700, value: u8) {
        use self::AddressingMode::*;

        match self {
            // NB: Register stores always set NZ, make sure this is okay before using it!
            A => spc.a = spc.psw.set_nz(value),
            X => spc.x = spc.psw.set_nz(value),
            Y => spc.y = spc.psw.set_nz(value),
            _ => {
                let a = self.address(spc);
                spc.store(a, value);
            }
        }
    }

    pub fn storew(self, spc: &mut Spc700, (lo, hi): (u8, u8)) {
        use self::AddressingMode::*;

        let addr = self.address(spc);
        let addr2;  // address of second (high) byte
        if let Direct(_) = self {
            // Direct Page access will wrap in the page
            addr2 = (addr & 0xff00) | (addr as u8).wrapping_add(1) as u16;   // low byte wraps
        } else {
            // FIXME wrapping
            addr2 = addr + 1;
        }

        spc.store(addr, lo);
        spc.store(addr2, hi);
    }

    pub fn address(&self, spc: &mut Spc700) -> u16 {
        use self::AddressingMode::*;

        fn direct(offset: u16, dp: bool) -> u16 {
            offset + match dp {
                true => 0x100,
                false => 0,
            }
        }

        // FIXME wrapping is (intentionally) wrong here!
        match *self {
            Immediate(_) => panic!("attempted to get address of immediate"),
            A | X | Y => panic!("attempted to get address of register"),
            Direct(offset) => direct(offset as u16, spc.psw.direct_page()),
            DirectIndexedX(offset) => direct(offset as u16, spc.psw.direct_page()) + spc.x as u16,
            IndirectX => spc.x as u16,  // FIXME add direct page?
            IndirectIndexed(offset) => {
                // [d]+Y
                let addr_ptr = direct(offset as u16, spc.psw.direct_page());
                let addr = spc.loadw(addr_ptr) + spc.y as u16;
                addr
            }
            IndexedIndirect(offset) => {
                // [d+Y]
                let addr_ptr = direct(offset as u16, spc.psw.direct_page()) + spc.x as u16;
                let addr = spc.loadw(addr_ptr);
                addr
            }
            AbsIndexedIndirect(abs) => {
                let addr_ptr = abs + spc.x as u16;
                let addr = spc.loadw(addr_ptr);
                addr
            }
            Abs(addr) => addr,
            AbsIndexedX(addr) => addr + spc.x as u16,
            AbsIndexedY(addr) => addr + spc.y as u16,
            Rel(rel) => (spc.pc as i32 + rel as i32) as u16,
        }
    }
}

impl fmt::Display for AddressingMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::AddressingMode::*;

        match *self {
            A =>                       write!(f, "a"),
            X =>                       write!(f, "x"),
            Y =>                       write!(f, "y"),
            Immediate(val) =>          write!(f, "#${:02X}", val),
            Direct(offset) =>          write!(f, "${:02X}", offset),
            DirectIndexedX(offset) =>  write!(f, "${:02X}+X", offset),
            IndirectX =>               write!(f, "(X)"),
            IndirectIndexed(offset) => write!(f, "[${:02X}]+Y", offset),
            IndexedIndirect(offset) => write!(f, "[${:02X}+X]", offset),
            AbsIndexedIndirect(abs) => write!(f, "[!{:04X}+X]", abs),
            Abs(addr) =>               write!(f, "!{:04X}", addr),
            AbsIndexedX(addr) =>       write!(f, "!{:04X}+X", addr),
            AbsIndexedY(addr) =>       write!(f, "!{:04X}+Y", addr),
            Rel(rel) =>                write!(f, "{:+}", rel),
        }
    }
}
