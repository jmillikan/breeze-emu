//! 65816 emulator

mod addressing;
mod statusreg;

use self::addressing::AddressingMode;
use self::statusreg::StatusReg;

use snes::Peripherals;

/// Rudimentary memory access break points. Stores (bank, address)-tuples that cause a break on
/// read access.
const MEM_BREAK_LOAD: &'static [(u8, u16)] = &[
    (0x7f, 0x8000),
];
const MEM_BREAK_STORE: &'static [(u8, u16)] = &[
    //(0x00, 0x1ff),
    //(0x7f, 0x8000),
];

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

/// One CPU cycle = 6 master clock cycles
pub const CPU_CYCLE: u8 = 6;

pub struct Cpu {
    a: u16,
    x: u16,
    y: u16,
    /// Stack pointer
    s: u16,
    /// Data bank register. Bank for all memory accesses.
    dbr: u8,
    /// Program bank register. Opcodes are fetched from this bank.
    pbr: u8,
    /// Direct (page) register. Address offset for all instruction using "direct addressing" mode.
    d: u16,
    /// Program counter. Note that PBR is not changed by the CPU, so code can not span multiple
    /// banks (without manual bank switching).
    pc: u16,
    p: StatusReg,
    emulation: bool,

    /// Master clock cycle counter for the current instruction.
    cy: u8,

    pub trace: bool,
    pub mem: Peripherals,
}

impl Cpu {
    /// Creates a new CPU and executes a reset. This will fetch the RESET vector from memory and
    /// put the CPU in emulation mode.
    pub fn new(mut mem: Peripherals) -> Cpu {
        let pcl = mem.load(0, RESET_VEC8) as u16;
        let pch = mem.load(0, RESET_VEC8 + 1) as u16;
        let pc = (pch << 8) | pcl;
        debug!("RESET @ {:02X}", pc);

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
            cy: 0,
            trace: false,
            mem: mem,
        }
    }

    /// Adds the time needed to access the given memory location to the cycle counter.
    fn do_io_cycle(&mut self, bank: u8, addr: u16) {
        const FAST: u8 = 0;
        const SLOW: u8 = 2;
        const XSLOW: u8 = 6;

        self.cy += match bank {
            0x00 ... 0x3f => match addr {
                0x0000 ... 0x1fff | 0x6000 ... 0xffff => SLOW,
                0x4000 ... 0x41ff => XSLOW,
                _ => FAST,
            },
            0x40 ... 0x7f => SLOW,
            0x80 ... 0xbf => match addr {
                0x0000 ... 0x1fff | 0x6000 ... 0x7fff => SLOW,
                0x4000 ... 0x41ff => XSLOW,
                // FIXME Depends on bit 1 in $420d. Assume slow for now.
                0x8000 ... 0xffff => SLOW,
                _ => FAST
            },
            // FIXME Depends on bit 1 in $420d. Assume slow for now.
            0xc0 ... 0xff => SLOW,
            _ => FAST,
        }
    }

    /// Load a byte from memory. Will change the cycle counter according to the memory speed.
    fn loadb(&mut self, bank: u8, addr: u16) -> u8 {
        if MEM_BREAK_LOAD.iter().find(|&&(b, a)| bank == b && addr == a).is_some() {
            debug!("MEM-BREAK: Breakpoint triggered on load from ${:02X}:{:04X} (${:02X})",
                bank, addr, self.mem.load(bank, addr))
        }

        self.do_io_cycle(bank, addr);
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
        if MEM_BREAK_STORE.iter().find(|&&(b, a)| bank == b && addr == a).is_some() {
            debug!("MEM-BREAK: Breakpoint triggered on store of ${:02X} to ${:02X}:{:04X}",
                value, bank, addr)
        }

        self.do_io_cycle(bank, addr);
        self.mem.store(bank, addr, value)
    }
    fn storew(&mut self, bank: u8, addr: u16, value: u16) {
        assert!(addr < 0xffff, "storew on bank boundary");
        // ^ if this should be supported, make sure to fix the potential overflow below

        self.storeb(bank, addr, value as u8);
        self.storeb(bank, addr + 1, (value >> 8) as u8);
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

    fn trace_op(&self, pc: u16, raw: u8, op: &str, am: Option<&AddressingMode>) {
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

    /// Executes a single opcode and returns the number of master clock cycles used.
    pub fn dispatch(&mut self) -> u8 {
        // CPU cycles each opcode takes (at the minimum).
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

        let pc = self.pc;
        self.cy = 0;
        let op = self.fetchb();
        self.cy += CYCLE_TABLE[op as usize] * CPU_CYCLE + 4;
        // FIXME: The +4 is a timing correction. I'm not sure what causes the inaccuracy, but I
        // suspect the addressing mode / memory access timing is a bit off.

        macro_rules! instr {
            ( $name:ident ) => {{
                self.trace_op(pc, op, stringify!($name), None);
                self.$name()
            }};
            ( $name:ident $am:ident ) => {{
                let am = self.$am();
                self.trace_op(pc, op, stringify!($name), Some(&am));
                self.$name(am)
            }};
        }

        match op {
            // Stack operations
            0x4b => instr!(phk),
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

            // Processor status
            0x18 => instr!(clc),
            0x38 => instr!(sec),
            0x58 => instr!(cli),
            0x78 => instr!(sei),
            0xfb => instr!(xce),
            0xc2 => instr!(rep immediate8),
            0xe2 => instr!(sep immediate8),

            // Arithmetic
            0x0a => instr!(asl_a),
            0x06 => instr!(asl direct),
            0x16 => instr!(asl direct_indexed_x),
            0x0e => instr!(asl absolute),
            0x2a => instr!(rol_a),
            0x4a => instr!(lsr_a),
            0x7e => instr!(ror absolute_indexed_x),
            0x25 => instr!(and direct),
            0x29 => instr!(and immediate_acc),
            0x2f => instr!(and absolute_long),
            0x03 => instr!(ora stack_rel),
            0x05 => instr!(ora direct),
            0x07 => instr!(ora indirect_long),
            0x1d => instr!(ora absolute_indexed_x),
            0x65 => instr!(adc direct),
            0x69 => instr!(adc immediate_acc),
            0xe9 => instr!(sbc immediate_acc),
            0xe6 => instr!(inc direct),
            0xee => instr!(inc absolute),
            0x1a => instr!(ina),
            0xe8 => instr!(inx),
            0xc8 => instr!(iny),
            0xc6 => instr!(dec direct),
            0xce => instr!(dec absolute),
            0xca => instr!(dex),
            0x88 => instr!(dey),

            // Register and memory transfers
            0x5b => instr!(tcd),
            0x1b => instr!(tcs),
            0xaa => instr!(tax),
            0xa8 => instr!(tay),
            0x8a => instr!(txa),
            0x9b => instr!(txy),
            0x98 => instr!(tya),
            0xbb => instr!(tyx),
            0xeb => instr!(xba),
            0x85 => instr!(sta direct),
            0x97 => instr!(sta indirect_long_idx),
            0x8d => instr!(sta absolute),
            0x8f => instr!(sta absolute_long),
            0x9d => instr!(sta absolute_indexed_x),
            0x99 => instr!(sta absolute_indexed_y),
            0x9f => instr!(sta absolute_long_indexed_x),
            0x86 => instr!(stx direct),
            0x8e => instr!(stx absolute),
            0x84 => instr!(sty direct),
            0x8c => instr!(sty absolute),
            0x64 => instr!(stz direct),
            0x9c => instr!(stz absolute),
            0x74 => instr!(stz direct_indexed_x),
            0x9e => instr!(stz absolute_indexed_x),
            0xa5 => instr!(lda direct),
            0xa9 => instr!(lda immediate_acc),
            0xb2 => instr!(lda indirect),
            0xa7 => instr!(lda indirect_long),
            0xb7 => instr!(lda indirect_long_idx),
            0xad => instr!(lda absolute),
            0xaf => instr!(lda absolute_long),
            0xbd => instr!(lda absolute_indexed_x),
            0xb9 => instr!(lda absolute_indexed_y),
            0xa6 => instr!(ldx direct),
            0xa2 => instr!(ldx immediate_index),
            0xae => instr!(ldx absolute),
            0xbe => instr!(ldx absolute_indexed_y),
            0xa4 => instr!(ldy direct),
            0xa0 => instr!(ldy immediate_index),
            0xac => instr!(ldy absolute),
            0xbc => instr!(ldy absolute_indexed_x),

            // Comparisons and control flow
            0xc9 => instr!(cmp immediate_acc),
            0xc5 => instr!(cmp direct),
            0xcd => instr!(cmp absolute),
            0xcf => instr!(cmp absolute_long),
            0xe0 => instr!(cpx immediate_index),
            0xe4 => instr!(cpx direct),
            0xec => instr!(cpx absolute),
            0xc0 => instr!(cpy immediate_index),
            0xc4 => instr!(cpy direct),
            0xcc => instr!(cpy absolute),
            0x80 => instr!(bra rel),
            0xdc => instr!(bra indirect_long),
            0x4c => instr!(bra absolute),
            0xf0 => instr!(beq rel),
            0xd0 => instr!(bne rel),
            0x10 => instr!(bpl rel),
            0x30 => instr!(bmi rel),
            0x70 => instr!(bvs rel),
            0x90 => instr!(bcc rel),
            0xb0 => instr!(bcs rel),
            0x20 => instr!(jsr absolute),
            0x22 => instr!(jsl absolute_long),
            0x60 => instr!(rts),
            0x6b => instr!(rtl),

            _ => {
                instr!(ill);
                panic!("illegal CPU opcode: ${:02X}", op);
            }
        }

        self.cy
    }

    /// Immediately executes an IRQ sequence and jumps to the NMI handler.
    pub fn trigger_nmi(&mut self) {
        if self.emulation {
            // FIXME Do we still push the same 4 Bytes in emulation mode?
            self.interrupt(NMI_VEC8);
        } else {
            self.interrupt(NMI_VEC16);
        }
    }

    pub fn trigger_irq(&mut self) {
        if self.emulation {
            // FIXME Do we still push the same 4 Bytes in emulation mode?
            self.interrupt(IRQ_VEC8);
        } else {
            self.interrupt(IRQ_VEC16);
        }
    }

    /// Execute an IRQ sequence. This pushes PBR, PC and the processor status register P on the
    /// stack, sets the PBR to 0, loads the handler address from the given vector, and jumps to the
    /// handler.
    fn interrupt(&mut self, vector: u16) {
        let pbr = self.pbr;
        self.pushb(pbr);
        let pc = self.pc;
        self.pushw(pc);
        let p = self.p.0;
        self.pushb(p);

        self.pbr = 0;
        let handler = self.loadw(0, vector);
        self.pc = handler;
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
impl Cpu {
    /// Push Program Bank Register
    fn phk(&mut self) {
        let pbr = self.pbr;
        self.pushb(pbr);
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
            self.cy += CPU_CYCLE;
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
            self.cy += CPU_CYCLE;
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
            self.cy += CPU_CYCLE;
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
            self.cy += CPU_CYCLE;
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
            self.cy += CPU_CYCLE;
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
            self.cy += CPU_CYCLE;
        }
    }

    /// AND Accumulator with Memory (or immediate)
    fn and(&mut self, am: AddressingMode) {
        if self.p.small_acc() {
            let val = am.loadb(self);
            let res = self.a as u8 & val;
            self.p.set_nz_8(res);
            self.a = (self.a & 0xff00) | res as u16;
        } else {
            let val = am.loadw(self);
            let res = self.a & val;
            self.a = self.p.set_nz(res);
            self.cy += CPU_CYCLE;
        }
    }
    /// OR Accumulator with Memory
    fn ora(&mut self, am: AddressingMode) {
        if self.p.small_acc() {
            let val = am.loadb(self);
            let res = self.a as u8 | val;
            self.p.set_nz_8(res);
            self.a = (self.a & 0xff00) | res as u16;
        } else {
            let val = am.loadw(self);
            let res = self.a | val;
            self.a = self.p.set_nz(res);
            self.cy += CPU_CYCLE;
        }
    }

    /// Add With Carry
    fn adc(&mut self, am: AddressingMode) {
        // Sets N, V, C and Z
        // FIXME is this correct? double-check this!
        let c = if self.p.carry() { 1 } else { 0 };
        if self.p.small_acc() {
            let a = self.a as u8;
            let val = am.loadb(self);
            let res = a as u16 + val as u16 + c;
            self.p.set_carry(res > 255);
            let res = res as u8;
            self.p.set_overflow((a ^ val) & 0x80 == 0 && (a ^ res) & 0x80 == 0x80);

            self.a = (self.a & 0xff00) | self.p.set_nz_8(res) as u16;
        } else {
            let val = am.loadw(self);
            let res = self.a as u32 + val as u32 + c as u32;
            self.p.set_carry(res > 65535);
            let res = res as u16;
            self.p.set_overflow((self.a ^ val) & 0x8000 == 0 && (self.a ^ res) & 0x8000 == 0x8000);

            self.a = self.p.set_nz(res);
            self.cy += CPU_CYCLE;
        }
    }

    /// Subtract with Borrow from Accumulator
    fn sbc(&mut self, am: AddressingMode) {
        // Changes N, Z, C and V
        // FIXME Set V flag!
        let c = if self.p.carry() { 0 } else { 1 };
        if self.p.small_acc() {
            let a = self.a as u8;
            let v = am.loadb(self);
            let res = a as i16 - v as i16 - c;
            self.p.set_carry(res < 0);

            self.a = (self.a & 0xff00) | self.p.set_nz_8(res as u8) as u16;
        } else {
            let v = am.loadw(self);
            let res = self.a as i32 - v as i32 - c as i32;
            self.p.set_carry(res < 0);

            self.a = self.p.set_nz(res as u16);
            self.cy += CPU_CYCLE;
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
    fn asl(&mut self, am: AddressingMode) {
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
            self.cy += 2 * CPU_CYCLE;
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
            self.cy += CPU_CYCLE;
        }
    }

    /// Logical Shift Accumulator Right
    fn lsr_a(&mut self) {
        // Sets N (always cleared), Z and C. The leftmost bit is filled with 0.
        // FIXME New code, needs small review
        if self.p.small_acc() {
            let a = self.a as u8;
            self.p.set_carry(self.a & 0x80 != 0);
            self.a = (self.a & 0xff00) | self.p.set_nz_8(a >> 1) as u16;
        } else {
            self.p.set_carry(self.a & 0x8000 != 0);
            self.a = self.p.set_nz(self.a >> 1);
        }
    }
    /// Rotate Memory Right
    fn ror(&mut self, am: AddressingMode) {
        // Sets N, Z, and C. Memory width can be changed. C is used to fill the leftmost bit.
        // The `AddressingMode` is used for both loading and storing the value (Read-Modify-Write
        // instruction)
        let c: u8 = if self.p.carry() { 1 } else { 0 };
        let (bank, addr) = am.address(self);
        if self.p.small_acc() {
            let val = self.loadb(bank, addr);
            self.p.set_carry(val & 0x80 != 0);
            let res = self.p.set_nz_8((val >> 1) | (c << 7));
            self.storeb(bank, addr, res);
        } else {
            let val = self.loadw(bank, addr);
            self.p.set_carry(val & 0x8000 != 0);
            let res = self.p.set_nz((val >> 1) | ((c as u16) << 15));
            self.storew(bank, addr, res);
            self.cy += 2 * CPU_CYCLE;
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
        self.x = self.y;
    }

    /// Increment memory location
    fn inc(&mut self, am: AddressingMode) {
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
    /// Decrement memory location
    fn dec(&mut self, am: AddressingMode) {
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

    /// Branch always
    fn bra(&mut self, am: AddressingMode) {
        let a = am.address(self);
        self.branch(a);
    }
    /// Branch if Plus (N = 0)
    fn bpl(&mut self, am: AddressingMode) {
        let a = am.address(self);
        if !self.p.negative() {
            self.branch(a);
            self.cy += CPU_CYCLE;
        }
    }
    /// Branch if Minus/Negative (N = 1)
    fn bmi(&mut self, am: AddressingMode) {
        let a = am.address(self);
        if self.p.negative() {
            self.branch(a);
            self.cy += CPU_CYCLE;
        }
    }
    /// Branch if Overflow Set
    fn bvs(&mut self, am: AddressingMode) {
        let a = am.address(self);
        if self.p.overflow() {
            self.branch(a);
            self.cy += CPU_CYCLE;
        }
    }
    /// Branch if carry clear
    fn bcc(&mut self, am: AddressingMode) {
        let a = am.address(self);
        if self.p.carry() {
            self.branch(a);
            self.cy += CPU_CYCLE;
        }
    }
    /// Branch if carry set
    fn bcs(&mut self, am: AddressingMode) {
        let a = am.address(self);
        if !self.p.carry() {
            self.branch(a);
            self.cy += CPU_CYCLE;
        }
    }
    /// Branch if Equal
    fn beq(&mut self, am: AddressingMode) {
        let a = am.address(self);
        if self.p.zero() {
            self.branch(a);
            self.cy += CPU_CYCLE;
        }
    }
    /// Branch if Not Equal (Branch if Z = 0)
    fn bne(&mut self, am: AddressingMode) {
        let a = am.address(self);
        if !self.p.zero() {
            self.branch(a);
            self.cy += CPU_CYCLE;
        }
    }

    /// Compare Accumulator with Memory
    fn cmp(&mut self, am: AddressingMode) {
        if self.p.small_acc() {
            let a = self.a as u8;
            let b = am.loadb(self);
            self.compare8(a, b);
        } else {
            let a = self.a;
            let b = am.loadw(self);
            self.compare(a, b);
            self.cy += CPU_CYCLE;
        }
    }
    /// Compare Index Register X with Memory
    fn cpx(&mut self, am: AddressingMode) {
        if self.p.small_index() {
            let val = am.loadb(self);
            let x = self.x as u8;
            self.compare8(x, val);
        } else {
            let val = am.loadw(self);
            let x = self.x;
            self.compare(x, val);
            self.cy += CPU_CYCLE;
        }
    }
    /// Compare Index Register Y with Memory
    fn cpy(&mut self, am: AddressingMode) {
        if self.p.small_index() {
            let val = am.loadb(self);
            let y = self.y as u8;
            self.compare8(y, val);
        } else {
            let val = am.loadw(self);
            let y = self.y;
            self.compare(y, val);
            self.cy += CPU_CYCLE;
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
    fn jsr(&mut self, am: AddressingMode) {
        // Changes no flags
        let pc = self.pc - 1;
        self.pushb((pc >> 8) as u8);
        self.pushb(pc as u8);

        self.pc = am.address(self).1;
    }
    /// Long jump to subroutine. Additionally saves PBR on the stack and sets it to the bank
    /// returned by `am.address()`.
    fn jsl(&mut self, am: AddressingMode) {
        // Changes no flags
        let pbr = self.pbr;
        self.pushb(pbr);
        let pc = self.pc - 1;
        self.pushb((pc >> 8) as u8);
        self.pushb(pc as u8);

        let (pbr, pc) = am.address(self);
        self.pbr = pbr;
        self.pc = pc;
    }
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
    fn clc(&mut self) { self.p.set_carry(false); }
    fn sec(&mut self) { self.p.set_carry(true); }

    /// Store 0 to memory
    fn stz(&mut self, am: AddressingMode) {
        if self.p.small_acc() {
            am.storeb(self, 0);
        } else {
            am.storew(self, 0);
            self.cy += CPU_CYCLE;
        }
    }

    /// Load accumulator from memory
    fn lda(&mut self, am: AddressingMode) {
        // Changes N and Z
        if self.p.small_acc() {
            let val = am.loadb(self);
            self.a = (self.a & 0xff00) | self.p.set_nz_8(val) as u16;
        } else {
            let val = am.loadw(self);
            self.a = self.p.set_nz(val);
            self.cy += CPU_CYCLE;
        }
    }
    /// Load X register from memory
    fn ldx(&mut self, am: AddressingMode) {
        // Changes N and Z
        if self.p.small_index() {
            let val = am.loadb(self);
            self.x = (self.x & 0xff00) | self.p.set_nz_8(val) as u16;
        } else {
            let val = am.loadw(self);
            self.x = self.p.set_nz(val);
            self.cy += CPU_CYCLE;
        }
    }
    /// Load Y register from memory
    fn ldy(&mut self, am: AddressingMode) {
        // Changes N and Z
        if self.p.small_index() {
            let val = am.loadb(self);
            self.y = (self.y & 0xff00) | self.p.set_nz_8(val) as u16;
        } else {
            let val = am.loadw(self);
            self.y = self.p.set_nz(val);
            self.cy += CPU_CYCLE;
        }
    }

    /// Store accumulator to memory
    fn sta(&mut self, am: AddressingMode) {
        // Changes no flags
        if self.p.small_acc() {
            let b = self.a as u8;
            am.storeb(self, b);
        } else {
            let w = self.a;
            am.storew(self, w);
            self.cy += CPU_CYCLE;
        }
    }
    fn stx(&mut self, am: AddressingMode) {
        // Changes no flags
        if self.p.small_index() {
            let b = self.x as u8;
            am.storeb(self, b);
        } else {
            let w = self.x;
            am.storew(self, w);
            self.cy += CPU_CYCLE;
        }
    }
    fn sty(&mut self, am: AddressingMode) {
        // Changes no flags
        if self.p.small_index() {
            let b = self.y as u8;
            am.storeb(self, b);
        } else {
            let w = self.y;
            am.storew(self, w);
            self.cy += CPU_CYCLE;
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
    fn rep(&mut self, am: AddressingMode) {
        assert!(!self.emulation);
        let p = self.p.0 & !am.loadb(self);
        self.set_p(p);
    }

    /// Set Processor Status Bits
    fn sep(&mut self, am: AddressingMode) {
        assert!(!self.emulation);
        let p = self.p.0 | am.loadb(self);
        self.set_p(p);
    }

    /// Transfer 16-bit Accumulator to Direct Page Register
    fn tcd(&mut self) {
        self.d = self.p.set_nz(self.a);
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

    fn ill(&mut self) {}
}

/// Addressing mode construction
impl Cpu {
    fn indirect(&mut self) -> AddressingMode {
        AddressingMode::Indirect(self.fetchb())
    }
    fn indirect_long(&mut self) -> AddressingMode {
        AddressingMode::IndirectLong(self.fetchb())
    }
    fn indirect_long_idx(&mut self) -> AddressingMode {
        AddressingMode::IndirectLongIdx(self.fetchb())
    }
    fn absolute(&mut self) -> AddressingMode {
        AddressingMode::Absolute(self.fetchw())
    }
    fn absolute_long(&mut self) -> AddressingMode {
        let addr = self.fetchw();
        let bank = self.fetchb();
        AddressingMode::AbsoluteLong(bank, addr)
    }
    fn absolute_indexed_x(&mut self) -> AddressingMode {
        AddressingMode::AbsIndexedX(self.fetchw())
    }
    fn absolute_indexed_y(&mut self) -> AddressingMode {
        AddressingMode::AbsIndexedY(self.fetchw())
    }
    fn absolute_long_indexed_x(&mut self) -> AddressingMode {
        let addr = self.fetchw();
        let bank = self.fetchb();
        AddressingMode::AbsLongIndexedX(bank, addr)
    }
    fn rel(&mut self) -> AddressingMode {
        AddressingMode::Rel(self.fetchb() as i8)
    }
    fn stack_rel(&mut self) -> AddressingMode {
        AddressingMode::StackRel(self.fetchb())
    }
    fn direct(&mut self) -> AddressingMode {
        AddressingMode::Direct(self.fetchb())
    }
    fn direct_indexed_x(&mut self) -> AddressingMode {
        AddressingMode::DirectIndexedX(self.fetchb())
    }
    /// Immediate value with accumulator size
    fn immediate_acc(&mut self) -> AddressingMode {
        if self.p.small_acc() {
            AddressingMode::Immediate8(self.fetchb())
        } else {
            self.cy += CPU_CYCLE;
            AddressingMode::Immediate(self.fetchw())
        }
    }
    /// Immediate value with index register size
    fn immediate_index(&mut self) -> AddressingMode {
        if self.p.small_index() {
            AddressingMode::Immediate8(self.fetchb())
        } else {
            self.cy += CPU_CYCLE;
            AddressingMode::Immediate(self.fetchw())
        }
    }
    /// Immediate value, one byte
    fn immediate8(&mut self) -> AddressingMode {
        AddressingMode::Immediate8(self.fetchb())
    }
}
