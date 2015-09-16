//! 65816 emulator. Does not emulate internal memory-mapped registers (these are meant to be
//! provided via an implementation of `AddressSpace`).

use std::num::Wrapping as W;

pub type U8 = W<u8>;
pub type U16 = W<u16>;
pub type U32 = W<u32>;

/// Abstraction over memory operations executed by the CPU. If these operations access an unmapped
/// address, the methods in here will be used to perform the operation.
pub trait AddressSpace {
    /// Load a byte from the given address.
    fn load(&mut self, bank: u8, addr: u16) -> u8;

    /// Store a byte at the given address.
    fn store(&mut self, bank: u8, addr: u16, value: u8);
}

const NEG_FLAG: u8 = 0x80;
const OVERFLOW_FLAG: u8 = 0x40;
/// 1 = Accumulator is 8-bit (native mode only)
const SMALL_ACC_FLAG: u8 = 0x20;
/// 1 = Index registers X/Y are 8-bit (native mode only)
const SMALL_INDEX_FLAG: u8 = 0x10;
/// Emulation mode only (same bit as `SMALL_INDEX_FLAG`)
const BREAK_FLAG: u8 = 0x10;
const DEC_FLAG: u8 = 0x08;
/// 1 = IRQs disabled
const IRQ_FLAG: u8 = 0x04;
const ZERO_FLAG: u8 = 0x02;
const CARRY_FLAG: u8 = 0x01;
struct StatusReg(u8);

impl StatusReg {
    fn negative(&self) -> bool { self.0 & NEG_FLAG != 0 }
    fn overflow(&self) -> bool { self.0 & OVERFLOW_FLAG != 0 }
    fn zero(&self) -> bool { self.0 & ZERO_FLAG != 0}
    fn carry(&self) -> bool { self.0 & CARRY_FLAG != 0 }
    fn irq_disable(&self) -> bool { self.0 & IRQ_FLAG != 0 }

    fn set(&mut self, flag: u8, value: bool) {
        if value {
            self.0 &= !flag;
        } else {
            self.0 |= flag;
        }
    }

    fn set_negative(&mut self, value: bool) { self.set(NEG_FLAG, value) }
    fn set_overflow(&mut self, value: bool) { self.set(OVERFLOW_FLAG, value) }
    fn set_zero(&mut self, value: bool) { self.set(ZERO_FLAG, value) }
    fn set_carry(&mut self, value: bool) { self.set(CARRY_FLAG, value) }
    fn set_irq_disable(&mut self, value: bool) { self.set(IRQ_FLAG, value) }
}

// Emulation mode vectors
const IRQ_VEC8: u16 = 0xFFFE;
const RESET_VEC8: u16 = 0xFFFC;
const NMI_VEC8: u16 = 0xFFFA;
const ABORT_VEC8: u16 = 0xFFF8;
const COP_VEC8: u16 = 0xFFF4;

// Native mode vectors
const IRQ_VEC16: u16 = 0xFFEE;
const NMI_VEC16: u16 = 0xFFEA;
const ABORT_VEC16: u16 = 0xFFE8;
const BRK_VEC16: u16 = 0xFFE6;
const COP_VEC16: u16 = 0xFFE4;

pub struct Cpu<T: AddressSpace> {
    a: U16,
    x: U16,
    y: U16,
    /// Stack pointer
    s: U16,
    /// Data bank register. Bank for all memory accesses.
    dbr: U8,
    /// Direct register. Address offset for all instruction using "direct addressing" mode.
    d: U16,
    /// Program bank register. Opcodes are fetched from this bank.
    pbr: U8,
    /// Program counter. Note that PBR is not changed by the CPU, so code can not span multiple
    /// banks (without manual bank switching).
    pc: U16,
    p: StatusReg,

    mem: T,
}

impl<T: AddressSpace> Cpu<T> {
    /// Creates a new CPU and executes a reset. This will fetch the RESET vector from memory and
    /// put the CPU in emulation mode.
    pub fn new(mut mem: T) -> Cpu<T> {
        let pcl = mem.load(0, RESET_VEC8) as u16;
        let pch = mem.load(0, RESET_VEC8 + 1) as u16;
        let pc = (pch << 8) | pcl;
        debug!("RESET @ {:02X}", pc);

        Cpu {
            // Undefined according to datasheet
            a: W(0),
            x: W(0),
            y: W(0),
            // High byte set to 1 since we're now in emulation mode
            s: W(0x0100),
            // Initialized to 0
            dbr: W(0),
            d: W(0),
            pbr: W(0),
            // Read from RESET vector above
            pc: W(pc),
            // Acc and index regs start in 8-bit mode, IRQs disabled, CPU in emulation mode
            p: StatusReg(SMALL_ACC_FLAG | SMALL_INDEX_FLAG | IRQ_FLAG),

            mem: mem,
        }
    }

    /// Fetches the byte PC points at, then increments PC
    fn fetchb(&mut self) -> u8 {
        let b = self.mem.load(self.pbr.0, self.pc.0);
        self.pc = self.pc + W(1);
        b
    }

    /// Fetches a 16-bit word (little-endian) located at PC, by fetching 2 individual bytes
    fn fetchw(&mut self) -> u16 {
        let low = self.fetchb() as u16;
        let high = self.fetchb() as u16;
        (high << 8) | low
    }

    fn trace_op(&self, pc: u16, op: &str, am: Option<&AddressingMode>) {
        trace!("{:02X}:{:04X}  {} {}",
            self.pbr.0,
            pc,
            op,
            am.map(|am| am.format(self)).unwrap_or(String::new()));
    }

    /// FIXME Temporary function to test the CPU emulation
    pub fn run(&mut self) {
        let mut pc;
        macro_rules! instr {
            ( $name:ident ) => {{
                self.trace_op(pc, stringify!($name), None);
                self.$name()
            }};
            ( $name:ident $am:ident ) => {{
                let am = self.$am();
                self.trace_op(pc, stringify!($name), Some(&am));
                self.$name(am)
            }};
        }

        loop {
            pc = self.pc.0;
            let op = self.fetchb();

            match op {
                0x78 => instr!(sei),
                0x9C => instr!(stz absolute),
                _ => panic!("illegal opcode: {:02X}", op),
            }
        }
    }
}

enum AddressingMode {
    Absolute(u16),
}

impl AddressingMode {
    /// Loads a byte from where this AM points to (or returns the immediate value)
    fn loadb<T: AddressSpace>(&self, cpu: &mut Cpu<T>) -> u8 {
        match *self {
            AddressingMode::Absolute(addr) => {
                cpu.mem.load(cpu.dbr.0, addr)
            }
        }
    }

    fn storeb<T: AddressSpace>(&self, cpu: &mut Cpu<T>, value: u8) {
        match *self {
            AddressingMode::Absolute(addr) => {
                cpu.mem.store(cpu.dbr.0, addr, value)
            }
        }
    }

    /// Computes the effective address as a bank-address-tuple. Panics if the addressing mode is
    /// immediate.
    fn address<T: AddressSpace>(&self, cpu: &Cpu<T>) -> (u8, u16) {
        match *self {
            AddressingMode::Absolute(addr) => {
                (cpu.dbr.0, addr)
            }
        }
    }

    fn format<T: AddressSpace>(&self, cpu: &Cpu<T>) -> String {
        match *self {
            AddressingMode::Absolute(addr) => {
                format!("${:04X}", addr)
            }
        }
    }
}

/// Addressing mode construction
impl<T: AddressSpace> Cpu<T> {
    fn absolute(&mut self) -> AddressingMode {
        let addr = self.fetchw();
        AddressingMode::Absolute(addr)
    }
}

/// Opcode implementations
impl<T: AddressSpace> Cpu<T> {
    fn sei(&mut self) {
        self.p.set_irq_disable(true);
    }

    fn stz(&mut self, am: AddressingMode) {
        am.storeb(self, 0);
    }
}
