//! Contains addressing mode definitions

use super::{Cpu, Mem};

use std::fmt;

/// Base trait for all types usable as addressing modes. Allows building the type from a `Cpu`.
///
/// The more fine-grained traits need to be implemented for this to be useful.
pub trait BuildAddrMode: fmt::Display {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self;
}

/// Trait for all addressing modes, including immediate addressing
pub trait ImmediateMode: BuildAddrMode {
    fn loadb<M: Mem>(&self, cpu: &mut Cpu<M>) -> u8;
    fn loadw<M: Mem>(&self, cpu: &mut Cpu<M>) -> u16;

    /// Small hack for instructions which act differently with immediate vs. other modes
    ///
    /// (Replace with specialization once stable)
    fn is_immediate() -> bool { true }
}

/// Trait for "real" addressing modes which compute an address and allow stores.
pub trait AddrMode: BuildAddrMode {
    fn address<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16);

    fn storeb<M: Mem>(&self, cpu: &mut Cpu<M>, value: u8) {
        let (bank, addr) = self.address(cpu);
        cpu.storeb(bank, addr, value);
    }

    fn storew<M: Mem>(&self, cpu: &mut Cpu<M>, value: u16) {
        let (bank, addr) = self.address(cpu);
        cpu.storew(bank, addr, value);
    }
}

impl<T: AddrMode> ImmediateMode for T {
    fn loadb<M: Mem>(&self, cpu: &mut Cpu<M>) -> u8 {
        let (bank, addr) = self.address(cpu);
        cpu.loadb(bank, addr)
    }

    fn loadw<M: Mem>(&self, cpu: &mut Cpu<M>) -> u16 {
        // FIXME Doesn't handle wrapping, which depends on emulation mode
        let (bank, addr) = self.address(cpu);
        cpu.loadw(bank, addr)
    }

    fn is_immediate() -> bool { false }
}

/// Program counter addressing modes
///
/// These are special addressing modes which do not handle values (loads/stores), but only compute
/// a jump target.
pub trait PcAddrMode: BuildAddrMode {
    /// Calculate the jump target address
    fn jump_target<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16);
}

/// The operand is the 16-bit value stored after the opcode
pub struct Immediate16(u16);

impl BuildAddrMode for Immediate16 {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        Immediate16(cpu.fetchw())
    }
}

impl ImmediateMode for Immediate16 {
    fn loadb<M: Mem>(&self, _cpu: &mut Cpu<M>) -> u8 {
        // One of the few things this overly complex trait hierarchy still doesn't solve:
        panic!("loadb on Immediate16 - doesn't make sense, probably a bug in the caller");
    }

    fn loadw<M: Mem>(&self, _cpu: &mut Cpu<M>) -> u16 { self.0 }
}

/// The operand is the 8-bit value stored after the opcode
pub struct Immediate8(u8);

impl BuildAddrMode for Immediate8 {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        Immediate8(cpu.fetchb())
    }
}

impl ImmediateMode for Immediate8 {
    fn loadb<M: Mem>(&self, _cpu: &mut Cpu<M>) -> u8 {
        self.0
    }

    fn loadw<M: Mem>(&self, _cpu: &mut Cpu<M>) -> u16 {
        panic!("loadw on Immediate8 - doesn't make sense, probably a bug in the caller");
    }
}

/// Either an 8- or 16-bit immediate, decided at runtime based on the configured accumulator size
pub enum ImmediateAcc {
    Small(u8),
    Large(u16),
}

impl BuildAddrMode for ImmediateAcc {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        if cpu.p.small_acc() {
            ImmediateAcc::Small(cpu.fetchb())
        } else {
            cpu.cy += 1;
            ImmediateAcc::Large(cpu.fetchw())
        }
    }
}

impl ImmediateMode for ImmediateAcc {
    fn loadb<M: Mem>(&self, _cpu: &mut Cpu<M>) -> u8 {
        match *self {
            ImmediateAcc::Small(val) => val,
            ImmediateAcc::Large(_) => {
                panic!("loadb on 16-bit ImmediateAcc");
            }
        }
    }

    fn loadw<M: Mem>(&self, _cpu: &mut Cpu<M>) -> u16 {
        match *self {
            ImmediateAcc::Small(_) => {
                panic!("loadw on 8-bit ImmediateAcc");
            }
            ImmediateAcc::Large(val) => val,
        }
    }
}

impl fmt::Display for ImmediateAcc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ImmediateAcc::Small(val) => write!(f, "#${:02X}", val),
            ImmediateAcc::Large(val) => write!(f, "#${:04X}", val),
        }
    }
}

/// Either an 8- or 16-bit immediate, decided at runtime based on the configured index register size
pub enum ImmediateIndex {
    Small(u8),
    Large(u16),
}

impl BuildAddrMode for ImmediateIndex {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        if cpu.p.small_index() {
            ImmediateIndex::Small(cpu.fetchb())
        } else {
            cpu.cy += 1;
            ImmediateIndex::Large(cpu.fetchw())
        }
    }
}

impl ImmediateMode for ImmediateIndex {
    fn loadb<M: Mem>(&self, _cpu: &mut Cpu<M>) -> u8 {
        match *self {
            ImmediateIndex::Small(val) => val,
            ImmediateIndex::Large(_) => {
                panic!("loadb on 16-bit ImmediateAcc");
            }
        }
    }

    fn loadw<M: Mem>(&self, _cpu: &mut Cpu<M>) -> u16 {
        match *self {
            ImmediateIndex::Small(_) => {
                panic!("loadw on 8-bit ImmediateAcc");
            }
            ImmediateIndex::Large(val) => val,
        }
    }
}

impl fmt::Display for ImmediateIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ImmediateIndex::Small(val) => write!(f, "#${:02X}", val),
            ImmediateIndex::Large(val) => write!(f, "#${:04X}", val),
        }
    }
}

/// "Program Counter Relative-r"
///
/// Used for branches. The jump target is computed by offsetting the program counter by the signed
/// 8-bit value stored after the current opcode.
///
/// (PBR, PC + <val>)  [PC+<val> wraps inside the bank]
pub struct PcRel(i8);

impl BuildAddrMode for PcRel {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        PcRel(cpu.fetchb() as i8)
    }
}

impl PcAddrMode for PcRel {
    fn jump_target<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        (cpu.pbr, (cpu.pc as i16).wrapping_add(self.0 as i16) as u16)
    }
}

/// "PC Relative Long-r"
///
/// Used for `PER` (Push Effective Relative Address) and `BRL` (Long branch).
///
/// (PBR, PC + <val>)
pub struct PcRelLong(i16);

impl BuildAddrMode for PcRelLong {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        PcRelLong(cpu.fetchw() as i16)
    }
}

impl PcAddrMode for PcRelLong {
    fn jump_target<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        (cpu.pbr, (cpu.pc as i16).wrapping_add(self.0) as u16)
    }
}

/// "Direct-d"
/// <val> + direct page register in bank 0
/// (0, D + <val>)
pub struct Direct(u8);

impl BuildAddrMode for Direct {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        Direct(cpu.fetchb())
    }
}

impl AddrMode for Direct {
    fn address<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        if cpu.d & 0xff != 0 { cpu.cy += 1 }
        (0, cpu.d.wrapping_add(self.0 as u16))
    }
}

/// "Direct Indexed with X-d,x"
/// (0, D + <val> + X)
pub struct DirectIndexedX(u8);

impl BuildAddrMode for DirectIndexedX {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        DirectIndexedX(cpu.fetchb())
    }
}

impl AddrMode for DirectIndexedX {
    fn address<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        if cpu.d & 0xff != 0 { cpu.cy += 1 }
        if !cpu.p.small_index() { cpu.cy += 1 }
        (0, cpu.d.wrapping_add(self.0 as u16).wrapping_add(cpu.x))
    }
}

/// "Direct Indexed with Y-d,y"
/// (0, D + <val> + Y)
pub struct DirectIndexedY(u8);

impl BuildAddrMode for DirectIndexedY {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        DirectIndexedY(cpu.fetchb())
    }
}

impl AddrMode for DirectIndexedY {
    fn address<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        if cpu.d & 0xff != 0 { cpu.cy += 1 }
        if !cpu.p.small_index() { cpu.cy += 1 }
        (0, cpu.d.wrapping_add(self.0 as u16).wrapping_add(cpu.y))
    }
}

/// "Direct Indexed Indirect-(d,x)" - Indirect-X
/// addr := load2(0, D + <val> + X)
/// (DBR, addr)
pub struct DirectIndexedIndirect(u8);

impl BuildAddrMode for DirectIndexedIndirect {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        DirectIndexedIndirect(cpu.fetchb())
    }
}

impl AddrMode for DirectIndexedIndirect {
    fn address<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        if cpu.d & 0xff != 0 { cpu.cy += 1 }
        let addr_ptr = cpu.d.wrapping_add(self.0 as u16).wrapping_add(cpu.x);
        let lo = cpu.loadb(0, addr_ptr) as u16;
        let hi = cpu.loadb(0, addr_ptr + 1) as u16;
        (cpu.dbr, (hi << 8) | lo)
    }
}

/// "Direct Indirect-(d)"
/// addr := load2(0, D + <val>)
/// (DBR, addr)
pub struct DirectIndirect(u8);

impl BuildAddrMode for DirectIndirect {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        DirectIndirect(cpu.fetchb())
    }
}

impl AddrMode for DirectIndirect {
    fn address<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        if cpu.d & 0xff != 0 { cpu.cy += 1 }
        let addr_ptr = cpu.d.wrapping_add(self.0 as u16);
        let lo = cpu.loadb(0, addr_ptr) as u16;
        let hi = cpu.loadb(0, addr_ptr + 1) as u16;
        (cpu.dbr, (hi << 8) | lo)
    }
}

// "Direct Indirect Indexed-(d),y" - Indirect-Y
// addr := load2(D + <val>)
// (DBR, addr + Y)  (NOTE: Wraps across data bank!)
pub struct DirectIndirectIndexed(u8);

impl BuildAddrMode for DirectIndirectIndexed {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        DirectIndirectIndexed(cpu.fetchb())
    }
}

impl AddrMode for DirectIndirectIndexed {
    fn address<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        if cpu.d & 0xff != 0 { cpu.cy += 1 }
        if !cpu.p.small_index() { cpu.cy += 1 }

        let addr_ptr = cpu.d.wrapping_add(self.0 as u16);
        let lo = cpu.loadb(0, addr_ptr) as u32;
        let hi = cpu.loadb(0, addr_ptr + 1) as u32;
        let base_address = ((cpu.dbr as u32) << 16) | (hi << 8) | lo;
        let eff_addr = base_address + cpu.y as u32;
        assert!(eff_addr & 0xff000000 == 0, "address overflow");

        let bank = (eff_addr >> 16) as u8;
        let addr = eff_addr as u16;
        (bank, addr)
    }
}

/// "Direct Indirect Long-[d]"
/// (bank, addr) := load3(0, D + <val>)
/// (bank, addr)
pub struct DirectIndirectLong(u8);

impl BuildAddrMode for DirectIndirectLong {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        DirectIndirectLong(cpu.fetchb())
    }
}

impl AddrMode for DirectIndirectLong {
    fn address<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        if cpu.d & 0xff != 0 { cpu.cy += 1 }
        let addr_ptr = cpu.d.wrapping_add(self.0 as u16);
        let lo = cpu.loadb(0, addr_ptr) as u16;
        let hi = cpu.loadb(0, addr_ptr + 1) as u16;
        let bank = cpu.loadb(0, addr_ptr + 2);
        (bank, (hi << 8) | lo)
    }
}

/// "Direct Indirect Long Indexed-[d],y" (or "Direct Indirect Indexed Long")
/// (bank, addr) := load3(0, D + <val>)
/// (bank, addr + Y)
pub struct DirectIndirectLongIdx(u8);

impl BuildAddrMode for DirectIndirectLongIdx {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        DirectIndirectLongIdx(cpu.fetchb())
    }
}

impl AddrMode for DirectIndirectLongIdx {
    fn address<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        // "The 24-bit base address is pointed to by the sum of the second byte of the
        // instruction and the Direct Register. The effective address is this 24-bit base
        // address plus the Y Index Register."
        if cpu.d & 0xff != 0 { cpu.cy += 1 }
        if !cpu.p.small_index() { cpu.cy += 1 }

        let addr_ptr = cpu.d.wrapping_add(self.0 as u16);
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
}

/// "Absolute-a"
/// Access absolute offset in the current data bank
/// (DBR, <val>)
pub struct Absolute(u16);

impl BuildAddrMode for Absolute {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        Absolute(cpu.fetchw())
    }
}

impl AddrMode for Absolute {
    fn address<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        (cpu.dbr, self.0)
    }
}

impl PcAddrMode for Absolute {
    fn jump_target<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        self.address(cpu)
    }
}

/// "Absolute Indexed with X-a,x"
/// (DBR, <val> + X)
pub struct AbsIndexedX(u16);

impl BuildAddrMode for AbsIndexedX {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        AbsIndexedX(cpu.fetchw())
    }
}

impl AddrMode for AbsIndexedX {
    fn address<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        if !cpu.p.small_index() { cpu.cy += 1; }
        (cpu.dbr, self.0 + cpu.x)
    }
}

/// "Absolute Indexed with Y-a,y"
/// (DBR, <val> + Y)
pub struct AbsIndexedY(u16);

impl BuildAddrMode for AbsIndexedY {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        AbsIndexedY(cpu.fetchw())
    }
}

impl AddrMode for AbsIndexedY {
    fn address<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        if !cpu.p.small_index() { cpu.cy += 1; }
        (cpu.dbr, self.0 + cpu.y)
    }
}

/// "Absolute Indexed Indirect-(a,x)"
/// addr := load2(PBR, <val> + X)
/// (PBR, addr)
pub struct AbsIndexedIndirect(u16);

impl BuildAddrMode for AbsIndexedIndirect {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        AbsIndexedIndirect(cpu.fetchw())
    }
}

impl AddrMode for AbsIndexedIndirect {
    fn address<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        let (x, pbr) = (cpu.x, cpu.pbr);
        let addr = cpu.loadw(pbr, self.0 + x);
        (pbr, addr)
    }
}

/// Can be used by JSR
impl PcAddrMode for AbsIndexedIndirect {
    fn jump_target<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        self.address(cpu)
    }
}

/// "Absolute Long Indexed With X-al,x" - Absolute Long + X
/// (<val0>, <val1> + X)
pub struct AbsLongIndexedX(u8, u16);

impl BuildAddrMode for AbsLongIndexedX {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        let bank = cpu.fetchb();
        let addr = cpu.fetchw();
        AbsLongIndexedX(bank, addr)
    }
}

impl AddrMode for AbsLongIndexedX {
    fn address<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        if !cpu.p.small_index() { cpu.cy += 1 }
        let a = ((self.0 as u32) << 16) | self.1 as u32;
        let eff_addr = a + cpu.x as u32;
        assert!(eff_addr & 0xff000000 == 0, "address overflow");
        let bank = eff_addr >> 16;
        let addr = eff_addr as u16;
        (bank as u8, addr)
    }
}

/// "Absolute Long-al"
/// Access absolute offset in the specified data bank (DBR is not changed)
/// (<val0>, <val1>)
pub struct AbsoluteLong(u8, u16);

impl BuildAddrMode for AbsoluteLong {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        let bank = cpu.fetchb();
        let addr = cpu.fetchw();
        AbsoluteLong(bank, addr)
    }
}

impl AddrMode for AbsoluteLong {
    fn address<M: Mem>(&self, _cpu: &mut Cpu<M>) -> (u8, u16) {
        (self.0, self.1)
    }
}

/// Absolute long addressing can be used by JSR (JSL)
impl PcAddrMode for AbsoluteLong {
    fn jump_target<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        self.address(cpu)
    }
}

/// "Absolute Indirect-(a)"
/// Used only by `jmp`.
/// addr := load2(0, <val>)
/// (PBR, addr)
pub struct AbsoluteIndirect(u16);

impl BuildAddrMode for AbsoluteIndirect {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        AbsoluteIndirect(cpu.fetchw())
    }
}

impl PcAddrMode for AbsoluteIndirect {
    fn jump_target<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        let addr = cpu.loadw(0, self.0);
        (cpu.pbr, addr)
    }
}

/// "Absolute Indirect Long-[a]"
///
/// Used only by `JML`.
///
/// (bank, addr) := load3(0, <val>)
/// (bank, addr)
pub struct AbsoluteIndirectLong(u16);

impl BuildAddrMode for AbsoluteIndirectLong {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        AbsoluteIndirectLong(cpu.fetchw())
    }
}

impl PcAddrMode for AbsoluteIndirectLong {
    fn jump_target<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        let addr = cpu.loadw(0, self.0);
        let bank = cpu.loadb(0, self.0 + 2);
        (bank, addr)
    }
}

/// "Stack Relative-d,s"
///
/// Unlike PC-relative addressing modes, this uses an unsigned offset added to the stack pointer.
/// Since the stack always is in bank 0, the DBR is ignored when accessing the data.
///
/// (0, SP + <val>)
pub struct StackRel(u8);

impl BuildAddrMode for StackRel {
    fn build<M: Mem>(cpu: &mut Cpu<M>) -> Self {
        StackRel(cpu.fetchb())
    }
}

impl AddrMode for StackRel {
    fn address<M: Mem>(&self, cpu: &mut Cpu<M>) -> (u8, u16) {
        let addr = cpu.s + self.0 as u16;
        (0, addr)
    }
}

// Printing

macro_rules! addr_display {
    ( Formatter: $f:ident; $( $t:ident ( $($p:pat),+ ) => $e:expr, )* ) => {
        $( impl fmt::Display for $t {
            fn fmt(&self, $f: &mut fmt::Formatter) -> fmt::Result {
                let $t ( $($p),+ ) = *self;
                $e
            }
        } )*
    };
}

addr_display! {
    Formatter: f;

    Immediate16(val) =>              write!(f, "#${:04X}", val),
    Immediate8(val) =>               write!(f, "#${:02X}", val),
    Absolute(addr) =>                write!(f, "${:04X}", addr),
    AbsoluteLong(bank, addr) =>      write!(f, "${:02X}:{:04X}", bank, addr),
    AbsLongIndexedX(bank, addr) =>   write!(f, "${:02X}:{:04X},x", bank, addr),
    AbsIndexedX(offset) =>           write!(f, "${:04X},x", offset),
    AbsIndexedY(offset) =>           write!(f, "${:04X},y", offset),
    AbsIndexedIndirect(addr) =>      write!(f, "(${:04X},x)", addr),
    AbsoluteIndirect(addr) =>        write!(f, "(${:04X})", addr),
    AbsoluteIndirectLong(addr) =>    write!(f, "[${:04X}]", addr),
    PcRel(rel) =>                    write!(f, "{:+}", rel),
    PcRelLong(rel_long) =>           write!(f, "{:+}", rel_long),
    Direct(offset) =>                write!(f, "${:02X}", offset),
    DirectIndexedX(offset) =>        write!(f, "${:02X},x", offset),
    DirectIndexedY(offset) =>        write!(f, "${:02X},y", offset),
    DirectIndexedIndirect(offset) => write!(f, "(${:02X},x)", offset),
    DirectIndirectIndexed(offset) => write!(f, "(${:02X}),y", offset),
    DirectIndirect(offset) =>        write!(f, "(${:02X})", offset),
    DirectIndirectLong(offset) =>    write!(f, "[${:02X}]", offset),
    DirectIndirectLongIdx(offset) => write!(f, "[${:02X}],y", offset),
    StackRel(offset) =>              write!(f, "${:02X},s", offset),
}
