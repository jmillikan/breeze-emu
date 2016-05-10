//! 65816 emulator (work in progress)

#[macro_use] extern crate log;
#[macro_use] extern crate libsavestate;

use libsavestate::SaveState;

mod addressing;
mod statusreg;

use self::addressing::*;
use self::statusreg::StatusReg;

use std::fmt;

/// Trait for devices attached to the 65816's address/data bus
pub trait Mem {
    fn load(&mut self, bank: u8, addr: u16) -> u8;
    fn store(&mut self, bank: u8, addr: u16, value: u8);
}

// Emulation mode vectors
const IRQ_VEC8: u16 = 0xFFFE;
const RESET_VEC8: u16 = 0xFFFC;
const NMI_VEC8: u16 = 0xFFFA;
#[allow(dead_code)]
const ABORT_VEC8: u16 = 0xFFF8;
#[allow(dead_code)]
const COP_VEC8: u16 = 0xFFF4;

// Native mode vectors
const IRQ_VEC16: u16 = 0xFFEE;
const NMI_VEC16: u16 = 0xFFEA;
#[allow(dead_code)]
const ABORT_VEC16: u16 = 0xFFE8;
#[allow(dead_code)]
const BRK_VEC16: u16 = 0xFFE6;
#[allow(dead_code)]
const COP_VEC16: u16 = 0xFFE4;

pub struct Cpu<M: Mem> {
    pub a: u16,
    pub x: u16,
    pub y: u16,
    /// Stack pointer
    pub s: u16,
    /// Data bank register. Bank for all memory accesses.
    pub dbr: u8,
    /// Program bank register. Opcodes are fetched from this bank.
    pub pbr: u8,
    /// Direct (page) register. Address offset for all instruction using "direct addressing" mode.
    pub d: u16,
    /// Program counter. Note that PBR is not changed on pc overflow, so code can not span
    /// multiple banks (without `jml` or `jsr`).
    pub pc: u16,
    p: StatusReg,
    emulation: bool,
    /// Set to true when executing a WAI instruction. Stops the processor from dispatching further
    /// instructions until an interrupt is triggered.
    wai: bool,

    /// CPU clock cycle counter for the current instruction.
    cy: u16,

    pub trace: bool,
    pub mem: M,
}

// Needs an explicit impl because `Cpu` is generic over `M`.
impl<M: Mem + SaveState> SaveState for Cpu<M> {
    impl_save_state_fns!(Cpu {
        a, x, y, s, dbr, pbr, d, pc, p, emulation, wai, mem
    } ignore { cy, trace });
}

impl<M: Mem> Cpu<M> {
    /// Creates a new CPU and executes a reset. This will fetch the RESET vector from memory and
    /// put the CPU in emulation mode.
    pub fn new(mut mem: M) -> Cpu<M> {
        let pcl = mem.load(0, RESET_VEC8) as u16;
        let pch = mem.load(0, RESET_VEC8 + 1) as u16;
        let pc = (pch << 8) | pcl;

        Cpu {
            // Undefined according to datasheet
            a: 0,
            x: 0,
            y: 0,
            // High byte set to 1 since we're now in emulation mode
            s: 0x0100,
            // Initialized to 0
            dbr: 0,
            d: 0,
            pbr: 0,
            // Read from RESET vector above
            pc: pc,
            // Acc and index regs start in 8-bit mode, IRQs disabled, CPU in emulation mode
            p: StatusReg::new(),
            emulation: true,
            wai: false,
            cy: 0,
            trace: false,
            mem: mem,
        }
    }

    /// Load a byte from memory.
    fn loadb(&mut self, bank: u8, addr: u16) -> u8 {
        // FIXME Remove?
        self.mem.load(bank, addr)
    }
    fn loadw(&mut self, bank: u8, addr: u16) -> u16 {
        assert!(addr < 0xffff, "loadw on bank boundary");
        // ^ if this should be supported, make sure to fix the potential overflow below

        let lo = self.loadb(bank, addr) as u16;
        let hi = self.loadb(bank, addr + 1) as u16;
        (hi << 8) | lo
    }

    fn storeb(&mut self, bank: u8, addr: u16, value: u8) {
        // FIXME Remove?
        self.mem.store(bank, addr, value)
    }
    fn storew(&mut self, bank: u8, addr: u16, value: u16) {
        self.storeb(bank, addr, value as u8);
        if addr == 0xffff {
            self.storeb(bank + 1, 0, (value >> 8) as u8);
        } else {
            self.storeb(bank, addr + 1, (value >> 8) as u8);
        }
    }

    /// Fetches the byte PC points at, then increments PC
    fn fetchb(&mut self) -> u8 {
        let (pbr, pc) = (self.pbr, self.pc);
        let b = self.loadb(pbr, pc);
        self.pc += 1;
        b
    }

    /// Fetches a 16-bit word (little-endian) located at PC, by fetching 2 individual bytes
    fn fetchw(&mut self) -> u16 {
        let low = self.fetchb() as u16;
        let high = self.fetchb() as u16;
        (high << 8) | low
    }

    /// Pushes a byte onto the stack and decrements the stack pointer
    fn pushb(&mut self, value: u8) {
        let s = self.s;
        self.storeb(0, s, value);
        if self.emulation {
            // stack must stay in 0x01xx
            assert_eq!(self.s & 0xff00, 0x0100);
            let s = self.s as u8 - 1;
            self.s = (self.s & 0xff00) | s as u16;
        } else {
            self.s -= 1;
        }
    }

    fn pushw(&mut self, value: u16) {
        let hi = (value >> 8) as u8;
        let lo = value as u8;
        self.pushb(hi);
        self.pushb(lo);
    }

    fn popb(&mut self) -> u8 {
        if self.emulation {
            // stack must stay in 0x01xx
            assert_eq!(self.s & 0xff00, 0x0100);
            let s = self.s as u8 + 1;
            self.s = (self.s & 0xff00) | s as u16;
        } else {
            self.s += 1;
        }

        let s = self.s;
        self.loadb(0, s)
    }

    fn popw(&mut self) -> u16 {
        let lo = self.popb() as u16;
        let hi = self.popb() as u16;
        (hi << 8) | lo
    }

    /// Enters/exits emulation mode
    fn set_emulation(&mut self, value: bool) {
        // FIXME Should this set the DBR/PBR?
        if !self.emulation && value {
            // Enter emulation mode

            // Set high byte of stack ptr to 0x01 and set M/X bits to make A,X and Y 8-bit
            self.s = 0x0100 | (self.s & 0xff);
            self.p.set_small_acc(true);
            self.p.set_small_index(true);
            // "If the Index Select Bit (X) equals one, both registers will be 8 bits wide, and the
            // high byte is forced to zero"
            self.x &= 0xff;
            self.y &= 0xff;
        }

        self.emulation = value;
    }

    fn trace_op(&self, pc: u16, raw: u8, op: &str, am: Option<&fmt::Display>) {
        use log::LogLevel::Trace;
        if !log_enabled!(Trace) || !self.trace { return }

        let opstr = match am {
            Some(am) => format!("{} {}", op, am),
            None => format!("{}", op),
        };
        trace!("${:02X}:{:04X} {:02X}  {:14} a:{:04X} x:{:04X} y:{:04X} s:{:04X} d:{:04X} dbr:{:02X} emu:{} {}",
            self.pbr,
            pc,
            raw,
            opstr,
            self.a,
            self.x,
            self.y,
            self.s,
            self.d,
            self.dbr,
            self.emulation as u8,
            self.p,
        );
    }

    /// Executes a single opcode and returns the number of CPU clock cycles used.
    ///
    /// Note that in case a WAI instruction was executed, this will *not* execute anything and
    /// return 0. An interrupt has to be caused to resume work.
    #[inline(always)]   // <- This roughly doubles benchmark performance ¯\_(ツ)_/¯
    pub fn dispatch(&mut self) -> u16 {
        // CPU cycles each opcode takes (at the minimum).
        // This table assumes that fetching a byte takes 1 CPU cycle. The `Mem` implementor can add
        // additional wait state cycles externally.
        // (FIXME: Is the above correct? Critical for timing!)
        static CYCLE_TABLE: [u8; 256] = [
            7,6,7,4,5,3,5,6, 3,2,2,4,6,4,6,5,   // $00 - $0f
            2,5,5,7,5,4,6,6, 2,4,2,2,6,4,7,5,   // $10 - $1f
            6,6,8,4,3,3,5,6, 4,2,2,5,4,4,6,5,   // $20 - $2f
            2,5,5,7,4,4,6,6, 2,4,2,2,4,4,7,5,   // $30 - $3f
            7,6,2,4,7,3,5,6, 3,2,2,3,3,4,6,5,   // $40 - $4f
            2,5,5,7,7,4,6,6, 2,4,3,2,4,4,7,5,   // $50 - $5f
            7,6,6,4,3,3,5,6, 4,2,2,6,5,4,6,5,   // $60 - $6f
            2,5,5,7,4,4,6,6, 2,4,4,2,6,2,7,5,   // $70 - $7f
            2,6,3,4,3,3,3,2, 2,2,2,3,4,4,4,5,   // $80 - $8f
            2,6,5,7,4,4,4,6, 2,5,2,2,3,5,5,5,   // $90 - $9f
            2,6,2,4,3,3,3,6, 2,2,2,4,4,4,4,5,   // $a0 - $af
            2,5,5,7,4,4,4,6, 2,4,2,2,4,4,4,5,   // $b0 - $bf
            2,6,3,4,3,3,5,6, 2,2,2,3,4,4,6,5,   // $c0 - $cf
            2,5,5,7,6,4,6,6, 2,4,3,3,6,4,7,5,   // $d0 - $df
            2,6,3,4,3,3,5,6, 2,2,2,3,4,4,6,5,   // $e0 - $ef
            2,5,5,7,5,4,6,6, 2,4,4,2,6,4,7,5,   // $f0 - $ff
        ];

        // Still waiting for interrupt? Don't do any work.
        if self.wai { return 0; }

        let pc = self.pc;
        self.cy = 0;
        let op = self.fetchb();
        self.cy += CYCLE_TABLE[op as usize] as u16;

        macro_rules! instr {
            ( $name:ident ) => {{
                self.trace_op(pc, op, stringify!($name), None);
                self.$name()
            }};
            ( $name:ident $am:ident ) => {{
                let am: $am = $am::build(self);
                self.trace_op(pc, op, stringify!($name), Some(&am as &fmt::Display));
                self.$name(am)
            }};
        }

        match op {
            // Stack operations
            0x4b => instr!(phk),
            0x0b => instr!(phd),
            0x2b => instr!(pld),
            0x8b => instr!(phb),
            0xab => instr!(plb),
            0x08 => instr!(php),
            0x28 => instr!(plp),
            0x48 => instr!(pha),
            0x68 => instr!(pla),
            0xda => instr!(phx),
            0xfa => instr!(plx),
            0x5a => instr!(phy),
            0x7a => instr!(ply),
            0xf4 => instr!(pea Absolute),
            0x62 => instr!(per PcRelLong),

            // Processor status
            0x18 => instr!(clc),
            0x38 => instr!(sec),
            0x58 => instr!(cli),
            0x78 => instr!(sei),
            0xcb => instr!(wai),
            0xd8 => instr!(cld),
            0xf8 => instr!(sed),
            0xfb => instr!(xce),
            0xc2 => instr!(rep Immediate8),
            0xe2 => instr!(sep Immediate8),

            // Arithmetic
            0x0a => instr!(asl_a),
            0x06 => instr!(asl Direct),
            0x16 => instr!(asl DirectIndexedX),
            0x0e => instr!(asl Absolute),
            0x1e => instr!(asl AbsIndexedX),
            0x2a => instr!(rol_a),
            0x26 => instr!(rol Direct),
            0x2e => instr!(rol Absolute),
            0x3e => instr!(rol AbsIndexedX),
            0x36 => instr!(rol DirectIndexedX),
            0x4a => instr!(lsr_a),
            0x46 => instr!(lsr Direct),
            0x4e => instr!(lsr Absolute),
            0x56 => instr!(lsr DirectIndexedX),
            0x5e => instr!(lsr AbsIndexedX),
            0x66 => instr!(ror Direct),
            0x6a => instr!(ror_a),
            0x6e => instr!(ror Absolute),
            0x76 => instr!(ror DirectIndexedX),
            0x7e => instr!(ror AbsIndexedX),
            0x23 => instr!(and StackRel),
            0x25 => instr!(and Direct),
            0x21 => instr!(and DirectIndexedIndirect),
            0x29 => instr!(and ImmediateAcc),
            0x2d => instr!(and Absolute),
            0x3d => instr!(and AbsIndexedX),
            0x39 => instr!(and AbsIndexedY),
            0x2f => instr!(and AbsoluteLong),
            0x3f => instr!(and AbsLongIndexedX),
            0x03 => instr!(ora StackRel),
            0x05 => instr!(ora Direct),
            0x15 => instr!(ora DirectIndexedX),
            0x09 => instr!(ora ImmediateAcc),
            0x12 => instr!(ora DirectIndirect),
            0x07 => instr!(ora DirectIndirectLong),
            0x17 => instr!(ora DirectIndirectLongIdx),
            0x0d => instr!(ora Absolute),
            0x1d => instr!(ora AbsIndexedX),
            0x19 => instr!(ora AbsIndexedY),
            0x0f => instr!(ora AbsoluteLong),
            0x1f => instr!(ora AbsLongIndexedX),
            0x45 => instr!(eor Direct),
            0x55 => instr!(eor DirectIndexedX),
            0x49 => instr!(eor ImmediateAcc),
            0x4d => instr!(eor Absolute),
            0x5d => instr!(eor AbsIndexedX),
            0x59 => instr!(eor AbsIndexedY),
            0x4f => instr!(eor AbsoluteLong),
            0x5f => instr!(eor AbsLongIndexedX),
            0x65 => instr!(adc Direct),
            0x75 => instr!(adc DirectIndexedX),
            0x72 => instr!(adc DirectIndirect),
            0x71 => instr!(adc DirectIndirectIndexed),
            0x77 => instr!(adc DirectIndirectLongIdx),
            0x67 => instr!(adc DirectIndirectLong),
            0x69 => instr!(adc ImmediateAcc),
            0x6d => instr!(adc Absolute),
            0x7d => instr!(adc AbsIndexedX),
            0x79 => instr!(adc AbsIndexedY),
            0x6f => instr!(adc AbsoluteLong),
            0x7f => instr!(adc AbsLongIndexedX),
            0xe5 => instr!(sbc Direct),
            0xf5 => instr!(sbc DirectIndexedX),
            0xe9 => instr!(sbc ImmediateAcc),
            0xed => instr!(sbc Absolute),
            0xf9 => instr!(sbc AbsIndexedY),
            0xfd => instr!(sbc AbsIndexedX),
            0xef => instr!(sbc AbsoluteLong),
            0xff => instr!(sbc AbsLongIndexedX),
            0xe6 => instr!(inc Direct),
            0xf6 => instr!(inc DirectIndexedX),
            0xfe => instr!(inc AbsIndexedX),
            0xee => instr!(inc Absolute),
            0x1a => instr!(ina),
            0xe8 => instr!(inx),
            0xc8 => instr!(iny),
            0x3a => instr!(dea),
            0xc6 => instr!(dec Direct),
            0xd6 => instr!(dec DirectIndexedX),
            0xce => instr!(dec Absolute),
            0xde => instr!(dec AbsIndexedX),
            0xca => instr!(dex),
            0x88 => instr!(dey),

            // Register and memory transfers
            0x5b => instr!(tcd),
            0x7b => instr!(tdc),
            0x1b => instr!(tcs),
            0x3b => instr!(tsc),
            0xba => instr!(tsx),
            0xaa => instr!(tax),
            0xa8 => instr!(tay),
            0x8a => instr!(txa),
            0x9a => instr!(txs),
            0x9b => instr!(txy),
            0x98 => instr!(tya),
            0xbb => instr!(tyx),
            0xeb => instr!(xba),
            0x83 => instr!(sta StackRel),
            0x85 => instr!(sta Direct),
            0x95 => instr!(sta DirectIndexedX),
            0x92 => instr!(sta DirectIndirect),
            0x87 => instr!(sta DirectIndirectLong),
            0x97 => instr!(sta DirectIndirectLongIdx),
            0x8d => instr!(sta Absolute),
            0x8f => instr!(sta AbsoluteLong),
            0x9d => instr!(sta AbsIndexedX),
            0x99 => instr!(sta AbsIndexedY),
            0x9f => instr!(sta AbsLongIndexedX),
            0x86 => instr!(stx Direct),
            0x96 => instr!(stx DirectIndexedY),
            0x8e => instr!(stx Absolute),
            0x84 => instr!(sty Direct),
            0x94 => instr!(sty DirectIndexedY),
            0x8c => instr!(sty Absolute),
            0x64 => instr!(stz Direct),
            0x9c => instr!(stz Absolute),
            0x74 => instr!(stz DirectIndexedX),
            0x9e => instr!(stz AbsIndexedX),
            0xa3 => instr!(lda StackRel),
            0xa5 => instr!(lda Direct),
            0xb5 => instr!(lda DirectIndexedX),
            0xb1 => instr!(lda DirectIndirectIndexed),
            0xa9 => instr!(lda ImmediateAcc),
            0xb2 => instr!(lda DirectIndirect),
            0xa7 => instr!(lda DirectIndirectLong),
            0xb7 => instr!(lda DirectIndirectLongIdx),
            0xad => instr!(lda Absolute),
            0xbd => instr!(lda AbsIndexedX),
            0xb9 => instr!(lda AbsIndexedY),
            0xaf => instr!(lda AbsoluteLong),
            0xbf => instr!(lda AbsLongIndexedX),
            0xa6 => instr!(ldx Direct),
            0xb6 => instr!(ldx DirectIndexedY),
            0xa2 => instr!(ldx ImmediateIndex),
            0xae => instr!(ldx Absolute),
            0xbe => instr!(ldx AbsIndexedY),
            0xa4 => instr!(ldy Direct),
            0xb4 => instr!(ldy DirectIndexedX),
            0xa0 => instr!(ldy ImmediateIndex),
            0xac => instr!(ldy Absolute),
            0xbc => instr!(ldy AbsIndexedX),
            0x54 => instr!(mvn),    // FIXME These look bad in the trace, print src/dest banks!
            0x44 => instr!(mvp),

            // Bit operations
            0x24 => instr!(bit Direct),
            0x2c => instr!(bit Absolute),
            0x34 => instr!(bit DirectIndexedX),
            0x3c => instr!(bit AbsIndexedX),
            0x89 => instr!(bit ImmediateAcc),
            0x04 => instr!(tsb Direct),
            0x0c => instr!(tsb Absolute),
            0x14 => instr!(trb Direct),
            0x1c => instr!(trb Absolute),

            // Comparisons
            0xc9 => instr!(cmp ImmediateAcc),
            0xc5 => instr!(cmp Direct),
            0xd5 => instr!(cmp DirectIndexedX),
            0xcd => instr!(cmp Absolute),
            0xdd => instr!(cmp AbsIndexedX),
            0xd9 => instr!(cmp AbsIndexedY),
            0xcf => instr!(cmp AbsoluteLong),
            0xdf => instr!(cmp AbsLongIndexedX),
            0xd2 => instr!(cmp DirectIndirect),
            0xd1 => instr!(cmp DirectIndirectIndexed),
            0xd7 => instr!(cmp DirectIndirectLongIdx),
            0xe0 => instr!(cpx ImmediateIndex),
            0xe4 => instr!(cpx Direct),
            0xec => instr!(cpx Absolute),
            0xc0 => instr!(cpy ImmediateIndex),
            0xc4 => instr!(cpy Direct),
            0xcc => instr!(cpy Absolute),

            // Branches
            0x80 => instr!(bra PcRel),
            0x82 => instr!(bra PcRelLong),  // BRL
            0xf0 => instr!(beq PcRel),
            0xd0 => instr!(bne PcRel),
            0x10 => instr!(bpl PcRel),
            0x30 => instr!(bmi PcRel),
            0x50 => instr!(bvc PcRel),
            0x70 => instr!(bvs PcRel),
            0x90 => instr!(bcc PcRel),
            0xb0 => instr!(bcs PcRel),

            // Jumps, calls and returns
            0x4c => instr!(jmp Absolute),   // DBR is ignored
            0x5c => instr!(jml AbsoluteLong),
            0x6c => instr!(jmp AbsoluteIndirect),
            0x7c => instr!(jmp AbsIndexedIndirect),
            0xdc => instr!(jml AbsoluteIndirectLong),
            0x20 => instr!(jsr Absolute),
            0x22 => instr!(jsl AbsoluteLong),
            0xfc => instr!(jsr AbsIndexedIndirect),
            0x40 => instr!(rti),
            0x60 => instr!(rts),
            0x6b => instr!(rtl),

            0xea => instr!(nop),
            _ => {
                instr!(ill);
                panic!("illegal CPU opcode: ${:02X}", op);
            }
        }

        self.cy
    }

    /// Invokes the NMI handler.
    pub fn trigger_nmi(&mut self) {
        if self.emulation {
            self.interrupt(NMI_VEC8);
        } else {
            self.interrupt(NMI_VEC16);
        }
    }

    /// Invokes the IRQ handler if interrupts are enabled. Returns whether the interrupt was
    /// generated.
    pub fn trigger_irq(&mut self) -> bool {
        if !self.p.irq_disable() {
            false
        } else {
            if self.emulation {
                self.interrupt(IRQ_VEC8);
            } else {
                self.interrupt(IRQ_VEC16);
            }
            true
        }
    }

    /// Execute an IRQ sequence. This pushes PBR, PC and the processor status register P on the
    /// stack, sets the PBR to 0, loads the handler address from the given vector, and jumps to the
    /// handler.
    fn interrupt(&mut self, vector: u16) {
        self.wai = false;

        if !self.emulation {
            let pbr = self.pbr;
            self.pushb(pbr);
            self.pbr = 0;
        }

        let pc = self.pc;
        self.pushw(pc);
        let p = self.p.0;
        self.pushb(p);

        // Interrupts clear the decimal flag (http://www.6502.org/tutorials/decimal_mode.html)
        // ...but only in native mode
        if !self.emulation {
            self.p.set_decimal(false);
        }

        let handler = self.loadw(0, vector);
        self.pc = handler;
    }

    fn return_from_interrupt(&mut self) {
        let p = self.popb();
        self.p.0 = p;
        let pc = self.popw();
        self.pc = pc;

        if !self.emulation {
            let pbr = self.popb();
            self.pbr = pbr;
        }
    }

    /// Common method for all comparison opcodes. Compares `a` to `b` by effectively computing
    /// `a-b`. This method only works correctly for 16-bit values.
    ///
    /// The Z flag is set if both numbers are equal.
    /// The C flag will be set to `a >= b`.
    /// The N flag is set to the most significant bit of `a-b`.
    fn compare(&mut self, a: u16, b: u16) {
        self.p.set_zero(a == b);
        self.p.set_carry(a >= b);
        self.p.set_negative(a.wrapping_sub(b) & 0x8000 != 0);
    }
    /// Does the exact same thing as `compare`, but for 8-bit operands
    fn compare8(&mut self, a: u8, b: u8) {
        self.p.set_zero(a == b);
        self.p.set_carry(a >= b);
        self.p.set_negative(a.wrapping_sub(b) & 0x80 != 0);
    }

    /// Branch to an absolute address. This will overwrite the current program bank.
    fn branch(&mut self, target: (u8, u16)) {
        self.pbr = target.0;
        self.pc = target.1;
    }

    /// Changes the status register.
    fn set_p(&mut self, new: u8) {
        let small_idx = self.p.small_index();
        self.p.0 = new;
        if !small_idx && self.p.small_index() {
            // "If the Index Select Bit (X) equals one, both registers will be 8 bits wide, and the
            // high byte is forced to zero"
            self.x &= 0xff;
            self.y &= 0xff;
        }
    }
}

/// Opcode implementations
impl<M: Mem> Cpu<M> {
    /// Move Next (incrementing address). Copies C+1 (16-bit A) bytes from the address in X to the
    /// address in Y.
    fn mvn(&mut self) {
        let destbank = self.fetchb();
        let srcbank = self.fetchb();

        while self.a != 0xffff {
            let (x, y) = (self.x, self.y);
            let val = self.loadb(srcbank, x);
            self.storeb(destbank, y, val);

            self.x = self.x.wrapping_add(1);
            self.y = self.y.wrapping_add(1);
            self.a = self.a.wrapping_sub(1);
        }
    }

    /// Move Previous (decrementing address)
    fn mvp(&mut self) {
        let destbank = self.fetchb();
        let srcbank = self.fetchb();

        while self.a != 0xffff {
            let (x, y) = (self.x, self.y);
            let val = self.loadb(srcbank, x);
            self.storeb(destbank, y, val);

            self.x = self.x.wrapping_sub(1);
            self.y = self.y.wrapping_sub(1);
            self.a = self.a.wrapping_sub(1);
        }
    }

    /// Push Program Bank Register
    fn phk(&mut self) {
        let pbr = self.pbr;
        self.pushb(pbr);
    }
    /// Push Direct Page Register
    fn phd(&mut self) {
        let d = self.d;
        self.pushw(d);
    }
    /// Pull Direct Page Register
    fn pld(&mut self) {
        let d = self.popw();
        self.d = d;
    }
    /// Push Data Bank Register
    fn phb(&mut self) {
        let dbr = self.dbr;
        self.pushb(dbr);
    }
    /// Pop Data Bank Register
    fn plb(&mut self) {
        let dbr = self.popb();
        self.dbr = dbr;
    }
    /// Push Processor Status Register
    fn php(&mut self) {
        // Changes no flags
        let p = self.p.0;
        self.pushb(p);
    }
    /// Pull Processor Status Register
    fn plp(&mut self) {
        let p = self.popb();
        self.set_p(p);
    }
    /// Push A on the stack
    fn pha(&mut self) {
        // No flags modified
        if self.p.small_acc() {
            let a = self.a as u8;
            self.pushb(a);
        } else {
            let a = self.a;
            self.pushw(a);
            self.cy += 1;
        }
    }
    /// Pull Accumulator from stack
    fn pla(&mut self) {
        // Changes N and Z
        if self.p.small_acc() {
            let a = self.popb();
            self.a = (self.a & 0xff00) | self.p.set_nz_8(a) as u16;
        } else {
            let a = self.popw();
            self.a = self.p.set_nz(a);
            self.cy += 1;
        }
    }
    /// Push Index Register X
    fn phx(&mut self) {
        if self.p.small_index() {
            let val = self.x as u8;
            self.pushb(val);
        } else {
            let val = self.x;
            self.pushw(val);
            self.cy += 1;
        }
    }
    /// Pop Index Register X
    fn plx(&mut self) {
        // Changes N and Z
        if self.p.small_index() {
            let val = self.popb();
            self.x = self.p.set_nz_8(val) as u16;
        } else {
            let val = self.popw();
            self.x = self.p.set_nz(val);
            self.cy += 1;
        }
    }
    /// Push Index Register Y
    fn phy(&mut self) {
        if self.p.small_index() {
            let val = self.y as u8;
            self.pushb(val);
        } else {
            let val = self.y;
            self.pushw(val);
            self.cy += 1;
        }
    }
    /// Pop Index Register Y
    fn ply(&mut self) {
        // Changes N and Z
        if self.p.small_index() {
            let val = self.popb();
            self.y = self.p.set_nz_8(val) as u16;
        } else {
            let val = self.popw();
            self.y = self.p.set_nz(val);
            self.cy += 1;
        }
    }

    fn push_effective<AM: PcAddrMode>(&mut self, am: AM) {
        let (_, addr) = am.jump_target(self);
        self.pushw(addr);
    }
    /// Push Effective Absolute Address
    fn pea<AM: PcAddrMode>(&mut self, am: AM) {
        // Pushes the address (16-bit, no bank) onto the stack. This is equivalent of pushing the
        // 2 bytes following the opcode onto the stack.
        self.push_effective(am)
    }
    /// Push Effective PC-Relative Address
    fn per<AM: PcAddrMode>(&mut self, am: AM) {
        self.push_effective(am)
    }

    /// AND Accumulator with Memory (or immediate)
    fn and<AM: ImmediateMode>(&mut self, am: AM) {
        // Sets N and Z
        if self.p.small_acc() {
            let val = am.loadb(self);
            let res = self.a as u8 & val;
            self.p.set_nz_8(res);
            self.a = (self.a & 0xff00) | res as u16;
        } else {
            let val = am.loadw(self);
            let res = self.a & val;
            self.a = self.p.set_nz(res);
            self.cy += 1;
        }
    }
    /// OR Accumulator with Memory
    fn ora<AM: ImmediateMode>(&mut self, am: AM) {
        // Sets N and Z
        if self.p.small_acc() {
            let val = am.loadb(self);
            let res = self.a as u8 | val;
            self.p.set_nz_8(res);
            self.a = (self.a & 0xff00) | res as u16;
        } else {
            let val = am.loadw(self);
            let res = self.a | val;
            self.a = self.p.set_nz(res);
            self.cy += 1;
        }
    }
    /// Exclusive Or Accumulator with Memory
    fn eor<AM: ImmediateMode>(&mut self, am: AM) {
        // Sets N and Z
        if self.p.small_acc() {
            let val = am.loadb(self);
            let res = self.a as u8 ^ val;
            self.p.set_nz_8(res);
            self.a = (self.a & 0xff00) | res as u16;
        } else {
            let val = am.loadw(self);
            let res = self.a ^ val;
            self.a = self.p.set_nz(res);
            self.cy += 1;
        }
    }

    /// Add With Carry
    fn adc<AM: ImmediateMode>(&mut self, am: AM) {
        // Sets N, V, C and Z
        let c: u16 = if self.p.carry() { 1 } else { 0 };

        if self.p.small_acc() {
            let a = self.a & 0xff;
            let val = am.loadb(self) as u16;
            let mut res = if self.p.decimal() {
                let mut low = (a & 0xf) + (val & 0xf) + c;
                if low > 9 { low += 6; }

                (a & 0xf0) + (val & 0xf0) + (low & 0x0f) + if low > 0x0f { 0x10 } else { 0 }
            } else {
                a + val + c
            };
            self.p.set_overflow((a as u8 ^ val as u8) & 0x80 == 0 &&
                                (a as u8 ^ res as u8) & 0x80 == 0x80);
            if self.p.decimal() && res > 0x9f { res += 0x60; }
            self.p.set_carry(res > 255);

            self.a = (self.a & 0xff00) | self.p.set_nz_8(res as u8) as u16;
        } else {
            let val = am.loadw(self);
            let mut res: u32 = if self.p.decimal() {
                let mut res0 = (self.a & 0x000f) + (val & 0x000f) + c;
                if res0 > 0x0009 { res0 += 0x0006; }

                let mut res1 = (self.a & 0x00f0) + (val & 0x00f0) + (res0 & 0x000f) +
                               if res0 > 0x000f { 0x0010 } else { 0x0000 };
                if res1 > 0x009f { res1 += 0x0060; }

                let mut res2 = (self.a & 0x0f00) + (val & 0x0f00) + (res1 & 0x00ff) +
                               if res1 > 0x00ff { 0x0100 } else { 0x0000 };
                if res2 > 0x09ff { res2 += 0x0600; }

                (self.a as u32 & 0xf000) + (val as u32 & 0xf000) + (res2 as u32 & 0x0fff) +
                    if res2 > 0x0fff { 0x1000 } else { 0x0000 }
            } else {
                self.a as u32 + val as u32 + c as u32
            };
            self.p.set_overflow((self.a ^ val) & 0x8000 == 0 &&
                                (self.a ^ res as u16) & 0x8000 == 0x8000);
            if self.p.decimal() && res > 0x9fff { res += 0x6000; }
            self.p.set_carry(res > 65535);

            self.a = self.p.set_nz(res as u16);
            self.cy += 1;
        }
    }

    /// Subtract with Borrow from Accumulator
    fn sbc<AM: ImmediateMode>(&mut self, am: AM) {
        // Sets N, Z, C and V
        let c: i16 = if self.p.carry() { 1 } else { 0 };

        if self.p.small_acc() {
            let a = self.a as i16 & 0xff;
            let v = am.loadb(self) as i16 ^ 0xff;
            let mut res: i16 = if self.p.decimal() {
                let mut low: i16 = (a & 0x0f) + (v & 0x0f) + c;
                if low < 0x10 { low -= 6; }

                (a & 0xf0) + (v & 0xf0) + (low & 0x0f) + if low > 0x0f { 0x10 } else { 0x00 }
            } else {
                a + v + c
            };
            self.p.set_overflow((a & 0x80) == (v & 0x80) && (a & 0x80) != (res & 0x80));
            if self.p.decimal() && res < 0x100 { res -= 0x60; }
            self.p.set_carry(res > 255);

            self.a = (self.a & 0xff00) | self.p.set_nz_8(res as u8) as u16;
        } else {
            let a = self.a as i32;
            let v = am.loadw(self) as i32 ^ 0xffff;
            let mut res: i32 = if self.p.decimal() {
                let mut res0 = (a & 0x000f) + (v & 0x000f) + c as i32;
                if res0 < 0x0010 { res0 -= 0x0006; }

                let mut res1 = (a & 0x00f0) + (v & 0x00f0) + (res0 & 0x000f) +
                    if res0 > 0x000f { 0x10 } else { 0x00 };
                if res1 < 0x0100 { res1 -= 0x0060; }

                let mut res2 = (a & 0x0f00) + (v & 0x0f00) + (res1 & 0x00ff) +
                    if res1 > 0x00ff { 0x100 } else { 0x000 };
                if res2 < 0x1000 { res2 -= 0x0600; }

                (a as i32 & 0xf000) + (v as i32 & 0xf000) + (res2 as i32 & 0x0fff) +
                    if res2 > 0x0fff { 0x1000 } else { 0x0000 }
            } else {
                self.a as i32 + v as i32 + c as i32
            };
            self.p.set_overflow((self.a ^ res as u16) & 0x8000 != 0 && (self.a ^ v as u16) & 0x8000 == 0);
            if self.p.decimal() && res < 0x10000 { res -= 0x6000; }
            self.p.set_carry(res > 65535);

            self.a = self.p.set_nz(res as u16);
            self.cy += 1;
        }
    }

    /// Shift accumulator left by 1 bit
    fn asl_a(&mut self) {
        // Sets N, Z and C. The rightmost bit is filled with 0.
        if self.p.small_acc() {
            let a = self.a as u8;
            self.p.set_carry(self.a & 0x80 != 0);
            self.a = (self.a & 0xff00) | self.p.set_nz_8(a << 1) as u16;
        } else {
            self.p.set_carry(self.a & 0x8000 != 0);
            self.a = self.p.set_nz(self.a << 1);
        }
    }
    /// Arithmetic left-shift: Shift a memory location left by 1 bit (Read-Modify-Write)
    fn asl<AM: AddrMode>(&mut self, am: AM) {
        // Sets N, Z and C. The rightmost bit is filled with 0.
        let (bank, addr) = am.address(self);
        if self.p.small_acc() {
            let val = self.loadb(bank, addr);
            self.p.set_carry(val & 0x80 != 0);
            let res = self.p.set_nz_8(val << 1);
            self.storeb(bank, addr, res);
        } else {
            let val = self.loadw(bank, addr);
            self.p.set_carry(val & 0x8000 != 0);
            let res = self.p.set_nz(val << 1);
            self.storew(bank, addr, res);
            self.cy += 2;
        }
    }
    /// Rotate Accumulator Left
    fn rol_a(&mut self) {
        // Sets N, Z, and C. C is used to fill the rightmost bit.
        let c: u8 = if self.p.carry() { 1 } else { 0 };
        if self.p.small_acc() {
            let a = self.a as u8;
            self.p.set_carry(self.a & 0x80 != 0);
            let res = (a << 1) | c;
            self.a = (self.a & 0xff00) | self.p.set_nz_8(res) as u16;
        } else {
            self.p.set_carry(self.a & 0x8000 != 0);
            let res = (self.a << 1) | c as u16;
            self.a = self.p.set_nz(res);
            self.cy += 1;
        }
    }
    /// Rotate Memory Left
    fn rol<AM: AddrMode>(&mut self, am: AM) {
        // Sets N, Z, and C. C is used to fill the rightmost bit.
        let c: u8 = if self.p.carry() { 1 } else { 0 };
        if self.p.small_acc() {
            let a = am.loadb(self);
            self.p.set_carry(a & 0x80 != 0);
            let res = self.p.set_nz_8((a << 1) | c);
            am.storeb(self, res);
        } else {
            let a = am.loadw(self);
            self.p.set_carry(a & 0x8000 != 0);
            let res = self.p.set_nz((a << 1) | c as u16);
            am.storew(self, res);
            self.cy += 1;   // FIXME times 2?
        }
    }

    /// Logical Shift Accumulator Right
    fn lsr_a(&mut self) {
        // Sets N (always cleared), Z and C. The leftmost bit is filled with 0.
        if self.p.small_acc() {
            let a = self.a as u8;
            self.p.set_carry(self.a & 0x01 != 0);
            self.a = (self.a & 0xff00) | self.p.set_nz_8(a >> 1) as u16;
        } else {
            self.p.set_carry(self.a & 0x0001 != 0);
            self.a = self.p.set_nz(self.a >> 1);
        }
    }
    /// Logical Shift Right
    fn lsr<AM: AddrMode>(&mut self, am: AM) {
        // Sets N (always cleared), Z and C. The leftmost bit is filled with 0.
        if self.p.small_acc() {
            let a = am.loadb(self);
            self.p.set_carry(a & 0x01 != 0);
            let res = self.p.set_nz_8(a >> 1);
            am.storeb(self, res);
        } else {
            let a = am.loadw(self);
            self.p.set_carry(a & 0x0001 != 0);
            let res = self.p.set_nz(a >> 1);
            am.storew(self, res);
        }
    }
    /// Rotate accumulator right
    fn ror_a(&mut self) {
        // Sets N, Z, and C. C is used to fill the leftmost bit.
        let c: u8 = if self.p.carry() { 1 } else { 0 };
        if self.p.small_acc() {
            let val = self.a as u8;
            self.p.set_carry(val & 0x01 != 0);
            let res = self.p.set_nz_8((val >> 1) | (c << 7));
            self.a = (self.a & 0xff00) | res as u16;
        } else {
            let val = self.a;
            self.p.set_carry(val & 0x0001 != 0);
            self.a = self.p.set_nz((val >> 1) | ((c as u16) << 15));
            self.cy += 2;
        }
    }
    /// Rotate Memory Right
    fn ror<AM: AddrMode>(&mut self, am: AM) {
        // Sets N, Z, and C. C is used to fill the leftmost bit.
        // The `AddrMode` is used for both loading and storing the value (Read-Modify-Write
        // instruction)
        let c: u8 = if self.p.carry() { 1 } else { 0 };
        let (bank, addr) = am.address(self);
        if self.p.small_acc() {
            let val = self.loadb(bank, addr);
            self.p.set_carry(val & 0x01 != 0);
            let res = self.p.set_nz_8((val >> 1) | (c << 7));
            self.storeb(bank, addr, res);
        } else {
            let val = self.loadw(bank, addr);
            self.p.set_carry(val & 0x0001 != 0);
            let res = self.p.set_nz((val >> 1) | ((c as u16) << 15));
            self.storew(bank, addr, res);
            self.cy += 2;
        }
    }

    /// Exchange B with A (B is the MSB of the accumulator, A is the LSB)
    fn xba(&mut self) {
        // Changes N and Z: "The flags are changed based on the new value of the low byte, the A
        // accumulator (that is, on the former value of the high byte, the B accumulator), even in
        // sixteen-bit accumulator mode."
        let lo = self.a & 0xff;
        let hi = self.a >> 8;
        self.a = (lo << 8) | self.p.set_nz_8(hi as u8) as u16;
    }

    /// Transfer Accumulator to Index Register X
    fn tax(&mut self) {
        // Changes N and Z
        if self.p.small_index() {
            self.x = (self.x & 0xff00) | self.p.set_nz_8(self.a as u8) as u16;
        } else {
            self.x = self.p.set_nz(self.a);
        }
    }
    /// Transfer Accumulator to Index register Y
    fn tay(&mut self) {
        // Changes N and Z
        if self.p.small_index() {
            self.y = (self.y & 0xff00) | self.p.set_nz_8(self.a as u8) as u16;
        } else {
            self.y = self.p.set_nz(self.a);
        }
    }
    /// Transfer X to A
    fn txa(&mut self) {
        // Changes N and Z
        if self.p.small_acc() {
            self.a = (self.a & 0xff00) | self.p.set_nz_8(self.x as u8) as u16;
        } else {
            self.a = self.p.set_nz(self.x);
        }
    }
    /// Transfer X to S
    fn txs(&mut self) {
        // High Byte of X is 0 if X is 8-bit, we can just copy the whole X
        // Changes no flags
        if self.emulation {
            // "When in the Emulation mode, a 01 is forced into SH."
            self.s = 0x0100 | (self.x & 0xff);
        } else {
            self.s = self.x;
        }
    }
    /// Transfer X to Y
    fn txy(&mut self) {
        // Changes N and Z
        if self.p.small_index() {
            self.y = self.p.set_nz_8(self.x as u8) as u16;
        } else {
            self.y = self.p.set_nz(self.x);
        }
    }
    /// Transfer Index Register Y to Accumulator
    fn tya(&mut self) {
        // Changes N and Z
        if self.p.small_acc() {
            self.a = (self.a & 0xff00) | self.p.set_nz_8(self.y as u8) as u16;
        } else {
            self.a = self.p.set_nz(self.y);
        }
    }
    /// Transfer Y to X
    fn tyx(&mut self) {
        // Changes N and Z
        if self.p.small_index() {
            self.x = self.p.set_nz_8(self.y as u8) as u16;
        } else {
            self.x = self.p.set_nz(self.y);
        }
    }

    /// Increment memory location
    fn inc<AM: AddrMode>(&mut self, am: AM) {
        let (bank, addr) = am.address(self);
        if self.p.small_acc() {
            let res = self.loadb(bank, addr).wrapping_add(1);
            self.p.set_nz_8(res);
            self.storeb(bank, addr, res);
        } else {
            let res = self.loadw(bank, addr).wrapping_add(1);
            self.p.set_nz(res);
            self.storew(bank, addr, res);
        }
    }
    /// Increment accumulator
    fn ina(&mut self) {
        // Changes N and Z. Timing does not depend on accumulator size.
        if self.p.small_acc() {
            let res = self.p.set_nz_8((self.a as u8).wrapping_add(1));
            self.a = (self.a & 0xff00) | res as u16;
        } else {
            self.a = self.p.set_nz(self.a.wrapping_add(1));
        }
    }
    /// Increment Index Register X
    fn inx(&mut self) {
        // Changes N and Z. Timing does not depend on index register size.
        if self.p.small_index() {
            let res = self.p.set_nz_8((self.x as u8).wrapping_add(1));
            self.x = (self.x & 0xff00) | res as u16;
        } else {
            self.x = self.p.set_nz(self.x.wrapping_add(1));
        }
    }
    /// Increment Index Register Y
    fn iny(&mut self) {
        // Changes N and Z. Timing does not depend on index register size.
        if self.p.small_index() {
            let res = self.p.set_nz_8((self.y as u8).wrapping_add(1));
            self.y = (self.y & 0xff00) | res as u16;
        } else {
            self.y = self.p.set_nz(self.y.wrapping_add(1));
        }
    }
    /// Decrement Accumulator
    fn dea(&mut self) {
        // Changes N and Z. Timing does not depend on accumulator size.
        if self.p.small_acc() {
            let res = self.p.set_nz_8((self.a as u8).wrapping_sub(1));
            self.a = (self.a & 0xff00) | res as u16;
        } else {
            self.a = self.p.set_nz(self.a.wrapping_sub(1));
        }
    }
    /// Decrement memory location
    fn dec<AM: AddrMode>(&mut self, am: AM) {
        let (bank, addr) = am.address(self);
        if self.p.small_acc() {
            let res = self.loadb(bank, addr).wrapping_sub(1);
            self.p.set_nz_8(res);
            self.storeb(bank, addr, res);
        } else {
            let res = self.loadw(bank, addr).wrapping_sub(1);
            self.p.set_nz(res);
            self.storew(bank, addr, res);
        }
    }
    /// Decrement X
    fn dex(&mut self) {
        // Changes N and Z. Timing does not depend on index register size.
        // NB According to the datasheet, this writes the result to A, not X! But since this
        // doesn't make sense when looking at the way it's used, I'm going to ignore the datasheet
        if self.p.small_index() {
            let res = self.p.set_nz_8((self.x as u8).wrapping_sub(1));
            self.x = (self.x & 0xff00) | res as u16;
        } else {
            self.x = self.p.set_nz(self.x.wrapping_sub(1));
        }
    }
    /// Decrement Y
    fn dey(&mut self) {
        // Changes N and Z. Timing does not depend on index register size.
        if self.p.small_index() {
            let res = self.p.set_nz_8((self.y as u8).wrapping_sub(1));
            self.y = (self.y & 0xff00) | res as u16;
        } else {
            self.y = self.p.set_nz(self.y.wrapping_sub(1));
        }
    }

    /// Jump long. Changes the PBR.
    fn jml<AM: PcAddrMode>(&mut self, am: AM) {
        let a = am.jump_target(self);
        self.branch(a);
    }
    /// Jump inside current program bank
    fn jmp<AM: PcAddrMode>(&mut self, am: AM) {
        let (_, addr) = am.jump_target(self);
        self.pc = addr;
    }
    /// Branch always (inside current program bank, but this isn't checked)
    fn bra<AM: PcAddrMode>(&mut self, am: AM) {
        let a = am.jump_target(self);
        self.branch(a);
    }
    /// Branch if Plus (N = 0)
    fn bpl<AM: PcAddrMode>(&mut self, am: AM) {
        let a = am.jump_target(self);
        if !self.p.negative() {
            self.branch(a);
            self.cy += 1;
        }
    }
    /// Branch if Minus/Negative (N = 1)
    fn bmi<AM: PcAddrMode>(&mut self, am: AM) {
        let a = am.jump_target(self);
        if self.p.negative() {
            self.branch(a);
            self.cy += 1;
        }
    }
    /// Branch if Overflow Clear
    fn bvc<AM: PcAddrMode>(&mut self, am: AM) {
        let a = am.jump_target(self);
        if !self.p.overflow() {
            self.branch(a);
            self.cy += 1;
        }
    }
    /// Branch if Overflow Set
    fn bvs<AM: PcAddrMode>(&mut self, am: AM) {
        let a = am.jump_target(self);
        if self.p.overflow() {
            self.branch(a);
            self.cy += 1;
        }
    }
    /// Branch if carry clear
    fn bcc<AM: PcAddrMode>(&mut self, am: AM) {
        let a = am.jump_target(self);
        if !self.p.carry() {
            self.branch(a);
            self.cy += 1;
        }
    }
    /// Branch if carry set
    fn bcs<AM: PcAddrMode>(&mut self, am: AM) {
        let a = am.jump_target(self);
        if self.p.carry() {
            self.branch(a);
            self.cy += 1;
        }
    }
    /// Branch if Equal
    fn beq<AM: PcAddrMode>(&mut self, am: AM) {
        let a = am.jump_target(self);
        if self.p.zero() {
            self.branch(a);
            self.cy += 1;
        }
    }
    /// Branch if Not Equal (Branch if Z = 0)
    fn bne<AM: PcAddrMode>(&mut self, am: AM) {
        let a = am.jump_target(self);
        if !self.p.zero() {
            self.branch(a);
            self.cy += 1;
        }
    }

    /// Test memory bits against accumulator
    fn bit<AM: ImmediateMode>(&mut self, am: AM) {
        if self.p.small_acc() {
            let val = am.loadb(self);
            self.p.set_zero(val & self.a as u8 == 0);
            if !AM::is_immediate() {
                self.p.set_negative(val & 0x80 != 0);
                self.p.set_overflow(val & 0x40 != 0);
            }
        } else {
            let val = am.loadw(self);
            self.p.set_zero(val & self.a == 0);
            if !AM::is_immediate() {
                self.p.set_negative(val & 0x8000 != 0);
                self.p.set_overflow(val & 0x4000 != 0);
            }
            self.cy += 1;
        }
    }
    /// Test and set memory bits against accumulator
    fn tsb<AM: AddrMode>(&mut self, am: AM) {
        // Sets Z
        // FIXME Is this correct?
        if self.p.small_index() {
            let val = am.loadb(self);
            self.p.set_zero(val & self.a as u8 == 0);
            let res = val | self.a as u8;
            am.storeb(self, res);
        } else {
            let val = am.loadw(self);
            self.p.set_zero(val & self.a == 0);
            let res = val | self.a;
            am.storew(self, res);

            self.cy += 2;
        }
    }
    /// Test and reset memory bits against accumulator
    fn trb<AM: AddrMode>(&mut self, am: AM) {
        // Sets Z
        // FIXME Is this correct?
        if self.p.small_index() {
            let val = am.loadb(self);
            self.p.set_zero(val & self.a as u8 == 0);
            let res = val & !(self.a as u8);
            am.storeb(self, res);
        } else {
            let val = am.loadw(self);
            self.p.set_zero(val & self.a == 0);
            let res = val & !self.a;
            am.storew(self, res);

            self.cy += 2;
        }
    }

    /// Compare Accumulator with Memory
    fn cmp<AM: ImmediateMode>(&mut self, am: AM) {
        if self.p.small_acc() {
            let a = self.a as u8;
            let b = am.loadb(self);
            self.compare8(a, b);
        } else {
            let a = self.a;
            let b = am.loadw(self);
            self.compare(a, b);
            self.cy += 1;
        }
    }
    /// Compare Index Register X with Memory
    fn cpx<AM: ImmediateMode>(&mut self, am: AM) {
        if self.p.small_index() {
            let val = am.loadb(self);
            let x = self.x as u8;
            self.compare8(x, val);
        } else {
            let val = am.loadw(self);
            let x = self.x;
            self.compare(x, val);
            self.cy += 1;
        }
    }
    /// Compare Index Register Y with Memory
    fn cpy<AM: ImmediateMode>(&mut self, am: AM) {
        if self.p.small_index() {
            let val = am.loadb(self);
            let y = self.y as u8;
            self.compare8(y, val);
        } else {
            let val = am.loadw(self);
            let y = self.y;
            self.compare(y, val);
            self.cy += 1;
        }
    }

    /// Jump to Subroutine (with short address). Doesn't change PBR.
    ///
    /// "The address saved is the address of the last byte of the JSR instruction (the address of
    /// the last byte of the operand), not the address of the next instruction as is the case with
    /// some other processors.  The address is pushed onto the stack in standard 65x order – the
    /// low byte in the lower address, the high byte in the higher address – and done in standard
    /// 65x fashion – the first byte is stored at the location pointed to by the stack pointer, the
    /// stack pointer is decremented, the second byte is stored, and the stack pointer is
    /// decremented again."
    fn jsr<AM: PcAddrMode>(&mut self, am: AM) {
        // Changes no flags
        let pc = self.pc - 1;
        self.pushb((pc >> 8) as u8);
        self.pushb(pc as u8);

        self.pc = am.jump_target(self).1;
    }
    /// Long jump to subroutine. Additionally saves PBR on the stack and sets it to the bank
    /// returned by `am.address()`.
    fn jsl<AM: PcAddrMode>(&mut self, am: AM) {
        // Changes no flags
        let pbr = self.pbr;
        self.pushb(pbr);
        let pc = self.pc - 1;
        self.pushb((pc >> 8) as u8);
        self.pushb(pc as u8);

        let (pbr, pc) = am.jump_target(self);
        self.pbr = pbr;
        self.pc = pc;
    }
    /// Return from Interrupt
    fn rti(&mut self) { self.return_from_interrupt() }
    /// Return from Subroutine (Short - Like JSR)
    fn rts(&mut self) {
        let pcl = self.popb() as u16;
        let pch = self.popb() as u16;
        let pc = (pch << 8) | pcl;
        self.pc = pc + 1;   // +1 since the last byte of the JSR was saved
    }
    /// Return from Subroutine called with `jsl`.
    ///
    /// This also restores the PBR.
    fn rtl(&mut self) {
        let pcl = self.popb() as u16;
        let pch = self.popb() as u16;
        let pbr = self.popb();
        let pc = (pch << 8) | pcl;
        self.pbr = pbr;
        self.pc = pc + 1;   // +1 since the last byte of the JSR was saved
    }

    fn cli(&mut self) { self.p.set_irq_disable(false) }
    fn sei(&mut self) { self.p.set_irq_disable(true) }
    fn cld(&mut self) { self.p.set_decimal(false) }
    fn sed(&mut self) { self.p.set_decimal(true) }
    fn clc(&mut self) { self.p.set_carry(false) }
    fn sec(&mut self) { self.p.set_carry(true) }

    fn wai(&mut self) { self.wai = true; }

    /// Store 0 to memory
    fn stz<AM: AddrMode>(&mut self, am: AM) {
        if self.p.small_acc() {
            am.storeb(self, 0);
        } else {
            am.storew(self, 0);
            self.cy += 1;
        }
    }

    /// Load accumulator from memory
    fn lda<AM: ImmediateMode>(&mut self, am: AM) {
        // Changes N and Z
        if self.p.small_acc() {
            let val = am.loadb(self);
            self.a = (self.a & 0xff00) | self.p.set_nz_8(val) as u16;
        } else {
            let val = am.loadw(self);
            self.a = self.p.set_nz(val);
            self.cy += 1;
        }
    }
    /// Load X register from memory
    fn ldx<AM: ImmediateMode>(&mut self, am: AM) {
        // Changes N and Z
        if self.p.small_index() {
            let val = am.loadb(self);
            self.x = (self.x & 0xff00) | self.p.set_nz_8(val) as u16;
        } else {
            let val = am.loadw(self);
            self.x = self.p.set_nz(val);
            self.cy += 1;
        }
    }
    /// Load Y register from memory
    fn ldy<AM: ImmediateMode>(&mut self, am: AM) {
        // Changes N and Z
        if self.p.small_index() {
            let val = am.loadb(self);
            self.y = (self.y & 0xff00) | self.p.set_nz_8(val) as u16;
        } else {
            let val = am.loadw(self);
            self.y = self.p.set_nz(val);
            self.cy += 1;
        }
    }

    /// Store accumulator to memory
    fn sta<AM: AddrMode>(&mut self, am: AM) {
        // Changes no flags
        if self.p.small_acc() {
            let b = self.a as u8;
            am.storeb(self, b);
        } else {
            let w = self.a;
            am.storew(self, w);
            self.cy += 1;
        }
    }
    fn stx<AM: AddrMode>(&mut self, am: AM) {
        // Changes no flags
        if self.p.small_index() {
            let b = self.x as u8;
            am.storeb(self, b);
        } else {
            let w = self.x;
            am.storew(self, w);
            self.cy += 1;
        }
    }
    fn sty<AM: AddrMode>(&mut self, am: AM) {
        // Changes no flags
        if self.p.small_index() {
            let b = self.y as u8;
            am.storeb(self, b);
        } else {
            let w = self.y;
            am.storew(self, w);
            self.cy += 1;
        }
    }

    /// Exchange carry and emulation flags
    fn xce(&mut self) {
        let carry = self.p.carry();
        let e = self.emulation;
        self.p.set_carry(e);
        self.set_emulation(carry);
    }

    /// Reset status bits
    ///
    /// Clears the bits in the status register that are 1 in the argument (argument is interpreted
    /// as 8-bit)
    fn rep<AM: ImmediateMode>(&mut self, am: AM) {
        let p = self.p.0 & !am.loadb(self);
        self.set_p(p);
    }

    /// Set Processor Status Bits
    fn sep<AM: ImmediateMode>(&mut self, am: AM) {
        let p = self.p.0 | am.loadb(self);
        self.set_p(p);
    }

    /// Transfer 16-bit Accumulator to Direct Page Register
    fn tcd(&mut self) {
        self.d = self.p.set_nz(self.a);
    }
    /// Transfer Direct Page Register to 16-bit Accumulator
    fn tdc(&mut self) {
        self.a = self.p.set_nz(self.d);
    }
    /// Transfer 16-bit Accumulator to Stack Pointer
    fn tcs(&mut self) {
        if self.emulation {
            // "When in the Emulation mode, a 01 is forced into SH. In this case, the B Accumulator
            // will not be loaded into SH during a TCS instruction."
            // S = 16-bit A; B = High byte of S
            self.s = 0x0100 | (self.a & 0xff);
        } else {
            self.s = self.a;
        }
    }
    /// Transfer Stack Pointer to 16-bit Accumulator
    fn tsc(&mut self) {
        // Sets N and Z
        self.a = self.p.set_nz(self.s);
    }
    /// Transfer Stack Pointer to X Register
    fn tsx(&mut self) {
        // Sets N and Z
        if self.p.small_index() {
            self.x = self.p.set_nz_8(self.s as u8) as u16;
        } else {
            self.x = self.p.set_nz(self.s);
        }
    }

    fn nop(&mut self) {}
    fn ill(&mut self) {}
}
