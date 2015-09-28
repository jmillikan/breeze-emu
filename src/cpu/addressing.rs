//! Contains addressing mode definitions

use cpu::{Cpu, CPU_CYCLE};

use std::fmt;

/// As a safety measure, the load and store methods take the mode by value and consume it. Using
/// the same object twice requires an explicit `.clone()` (`Copy` isn't implemented).
#[derive(Clone)]
pub enum AddressingMode {
    Immediate(u16),
    Immediate8(u8),
    /// "Absolute-a"
    /// Access absolute offset in the current data bank
    /// (DBR, <val>)
    Absolute(u16),

    // "Absolute Indexed Indirect-(a,x)"

    /// "Absolute Indexed with X-a,x"
    /// (DBR, <val> + X)
    AbsIndexedX(u16),

    // "Absolute Indexed with Y-a,y"
    // "Absolute Indirect-(a)" (PC?)

    /// "Absolute Long Indexed With X-al,x" - Absolute Long + X
    /// (<val0>, <val1> + X)
    AbsLongIndexedX(u8, u16),

    /// "Absolute Long-al"
    /// Access absolute offset in the specified data bank (DBR is not changed)
    /// (<val0>, <val1>)
    AbsoluteLong(u8, u16),
    /// "Direct-d"
    /// <val> + direct page register in bank 0
    /// (0, D + <val>)
    Direct(u8),
    /// "Direct Indexed with X-d,x"
    /// (0, D + <val> + X)
    DirectIndexedX(u8),
    /// "Program Counter Relative-r"
    /// Used for jumps
    /// (PBR, PC + <val>)  [PC+<val> wraps inside the bank]
    Rel(i8),

    // "Direct Indirect Indexed-(d),y" - Indirect-Y
    // (DBR, D + <val> + Y)  [D+<val> wraps]

    /// "Direct Indirect Long-[d]"
    /// (bank, addr) := load(D + <val>)
    /// (bank, addr)
    IndirectLong(u8),

    /// "Direct Indirect Indexed Long/Long Indexed-[d],y"
    /// (bank, addr) := load(D + <val>)
    /// (bank, addr + Y)
    IndirectLongIdx(u8),
}

impl AddressingMode {
    /// Loads a byte from where this AM points to (or returns the immediate value)
    pub fn loadb(self, cpu: &mut Cpu) -> u8 {
        match self {
            AddressingMode::Immediate(val) => panic!("loadb on 16-bit immediate"),
            AddressingMode::Immediate8(val) => val,
            _ => {
                let (bank, addr) = self.address(cpu);
                cpu.loadb(bank, addr)
            }
        }
    }
    pub fn loadw(self, cpu: &mut Cpu) -> u16 {
        match self {
            AddressingMode::Immediate(val) => val,
            AddressingMode::Immediate8(val) => panic!("loadw on 8-bit immediate"),
            _ => {
                let (bank, addr) = self.address(cpu);
                cpu.loadw(bank, addr)
            }
        }
    }

    pub fn storeb(self, cpu: &mut Cpu, value: u8) {
        let (bank, addr) = self.address(cpu);
        cpu.storeb(bank, addr, value);
    }
    pub fn storew(self, cpu: &mut Cpu, value: u16) {
        let (bank, addr) = self.address(cpu);
        cpu.storew(bank, addr, value);
    }

    /// Computes the effective address as a bank-address-tuple. Panics if the addressing mode is
    /// immediate.
    pub fn address(&self, cpu: &mut Cpu) -> (u8, u16) {
        use self::AddressingMode::*;

        // FIXME is something here dependant on register sizes?
        // -> Yes, the cycle count. This causes bad timing, fix it!
        // FIXME Use next bank on some address overflows

        match *self {
            Absolute(addr) => {
                (cpu.dbr, addr)
            }
            AbsoluteLong(bank, addr) => {
                (bank, addr)
            }
            AbsLongIndexedX(bank, addr) => {
                if !cpu.p.small_index() { cpu.cy += CPU_CYCLE }
                let a = ((bank as u32) << 16) | addr as u32;
                let eff_addr = a + cpu.x as u32;
                assert!(eff_addr & 0xff000000 == 0, "address overflow");
                let bank = eff_addr >> 16;
                let addr = eff_addr as u16;
                (bank as u8, addr)
            }
            AbsIndexedX(offset) => {
                if !cpu.p.small_index() { cpu.cy += CPU_CYCLE }
                (cpu.dbr, offset + cpu.x)
            }
            Rel(rel) => {
                (cpu.pbr, (cpu.pc as i32 + rel as i32) as u16)
            }
            Direct(offset) => {
                if cpu.d & 0xff != 0 { cpu.cy += CPU_CYCLE }
                (0, cpu.d.wrapping_add(offset as u16))
            }
            DirectIndexedX(offset) => {
                if cpu.d & 0xff != 0 { cpu.cy += CPU_CYCLE }
                if !cpu.p.small_index() { cpu.cy += CPU_CYCLE }
                (0, cpu.d.wrapping_add(offset as u16).wrapping_add(cpu.x))
            }
            IndirectLong(offset) => {
                if cpu.d & 0xff != 0 { cpu.cy += CPU_CYCLE }
                let addr_ptr = cpu.d.wrapping_add(offset as u16);
                let lo = cpu.loadb(0, addr_ptr) as u16;
                let hi = cpu.loadb(0, addr_ptr + 1) as u16;
                let bank = cpu.loadb(0, addr_ptr + 2);
                (bank, (hi << 8) | lo)
            }
            IndirectLongIdx(offset) => {
                // "The 24-bit base address is pointed to by the sum of the second byte of the
                // instruction and the Direct Register. The effective address is this 24-bit base
                // address plus the Y Index Register."
                if cpu.d & 0xff != 0 { cpu.cy += CPU_CYCLE }
                if !cpu.p.small_index() { cpu.cy += CPU_CYCLE }

                let addr_ptr = cpu.d.wrapping_add(offset as u16);
                let lo = cpu.loadb(0, addr_ptr) as u32;
                let hi = cpu.loadb(0, addr_ptr + 1) as u32;
                let bank = cpu.loadb(0, addr_ptr + 2) as u32;
                let base_address = (bank << 16) | (hi << 8) | lo;
                let eff_addr = base_address + cpu.y as u32;
                assert!(eff_addr & 0xff000000 == 0, "address overflow");

                let bank = (eff_addr >> 16) as u8;
                let addr = eff_addr as u16;
                (bank, addr)
            }
            Immediate(_) | Immediate8(_) =>
                panic!("attempted to take the address of an immediate value (attempted store to \
                    immediate?)")
        }
    }
}

impl fmt::Display for AddressingMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::AddressingMode::*;

        match *self {
            Immediate(val) =>              write!(f, "#${:04X}", val),
            Immediate8(val) =>             write!(f, "#${:02X}", val),
            Absolute(addr) =>              write!(f, "${:04X}", addr),
            AbsoluteLong(bank, addr) =>    write!(f, "${:02X}:{:04X}", bank, addr),
            AbsLongIndexedX(bank, addr) => write!(f, "${:02X}:{:04X},x", bank, addr),
            AbsIndexedX(offset) =>         write!(f, "${:04X},x", offset),
            Rel(rel) =>                    write!(f, "{:+}", rel),
            Direct(offset) =>              write!(f, "${:02X}", offset),
            DirectIndexedX(offset) =>      write!(f, "${:02X},x", offset),
            IndirectLong(offset) =>        write!(f, "[${:02X}]", offset),
            IndirectLongIdx(offset) =>     write!(f, "[${:02X}],y", offset),
        }
    }
}
