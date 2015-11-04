//! Implements the Audio Processing Unit (APU)
//!
//! The APU consists of 64 KB shared RAM, the SPC700 and the DSP. It is almost fully independent
//! from the rest of the SNES.
//!
//! The SPC700 is an independent audio coprocessor. The main CPU can transmit a program and audio
//! data into the shared RAM and then execute it. The program can manipulate DSP registers and
//! specify samples to play.

mod addressing;
mod dsp;
mod ipl;
mod timer;

use self::addressing::AddressingMode;
use self::dsp::Dsp;
use self::ipl::IPL_ROM;
use self::timer::Timer;

pub type Apu = Spc700;


const RAM_SIZE: usize = 65536;
byte_array!(Ram[RAM_SIZE] with u16 indexing please);

const RESET_VEC: u16 = 0xFFFE;

/// The SPC700 processor used in the APU is an 8-bit processor with a 16-bit address space. It has
/// 64 KB of RAM shared with the DSP. The last 64 Bytes in its address space are mapped to the
/// "IPL ROM", which contains a small piece of startup code that allows the main CPU to transfer a
/// program to the APU (we just copy the IPL ROM into the RAM and make it read-write).
pub struct Spc700 {
    // 64KB of RAM
    // (this is not the address space, even though both are 64KB!)
    mem: Ram,

    /// $f2 - DSP address selection ($f3 - DSP data)
    reg_dsp_addr: u8,
    /// Values written to the IO Registers by the main CPU. The CPU will write values here. These
    /// are read by the SPC, the CPU reads directly from RAM, while the SPC writes to RAM.
    /// $f4 - $f7
    io_vals: [u8; 4],
    timers: [Timer; 3],

    dsp: Dsp,

    a: u8,
    x: u8,
    y: u8,
    sp: u8,
    pc: u16,
    psw: StatusReg,

    cy: u8,

    pub trace: bool,
}

impl Default for Spc700 {
    fn default() -> Self {
        const IPL_START: usize = RAM_SIZE - 64;

        let mut mem = Ram::default();
        for i in 0..64 {
            mem[IPL_START as u16 + i] = IPL_ROM[i as usize];
        }

        let pcl = mem[RESET_VEC] as u16;
        let pch = mem[RESET_VEC + 1] as u16;
        let pc = (pch << 8) | pcl;

        Spc700 {
            mem: mem,
            reg_dsp_addr: 0,
            io_vals: [0; 4],
            timers: [Timer::new(); 3],
            dsp: Dsp::new(),
            a: 0,
            x: 0,
            y: 0,
            sp: 0,
            pc: pc,
            psw: StatusReg(0),  // FIXME is 0 correct?
            cy: 0,
            trace: false,
        }
    }
}

// Public interface
impl Spc700 {
    pub fn new() -> Spc700 { Spc700::default() }

    /// Store a byte in an IO port (0-3)
    ///
    /// IO ports 0x2140... are mapped to internal registers 0xf4 - 0xf7
    pub fn store_port(&mut self, port: u8, value: u8) {
        debug_assert!(port < 4);
        self.io_vals[port as usize] = value;
    }

    /// Load a byte from an IO port (0-3)
    ///
    /// IO ports 0x2140... are mapped to internal registers 0xf4 - 0xf7
    pub fn read_port(&mut self, port: u8) -> u8 {
        debug_assert!(port < 4);
        let val = self.mem[0xf4 + port as u16];
        val
    }
}

struct StatusReg(u8);
const NEG_FLAG: u8         = 0x80;
const OVERFLOW_FLAG: u8    = 0x40;
const DIRECT_PAGE_FLAG: u8 = 0x20;
const HALF_CARRY_FLAG: u8  = 0x08;
const ZERO_FLAG: u8        = 0x02;
const CARRY_FLAG: u8       = 0x01;

impl StatusReg {
    fn negative(&self) -> bool    { self.0 & NEG_FLAG != 0 }
    fn zero(&self) -> bool        { self.0 & ZERO_FLAG != 0 }
    fn direct_page(&self) -> bool { self.0 & DIRECT_PAGE_FLAG != 0 }
    fn carry(&self) -> bool       { self.0 & CARRY_FLAG != 0 }
    #[allow(dead_code)] // FIXME
    fn half_carry(&self) -> bool  { self.0 & HALF_CARRY_FLAG != 0 }
    #[allow(dead_code)] // FIXME Nothing uses this?
    fn overflow(&self) -> bool    { self.0 & OVERFLOW_FLAG != 0 }

    fn set(&mut self, flag: u8, v: bool) {
        match v {
            true => self.0 |= flag,
            false => self.0 &= !flag,
        }
    }

    fn set_negative(&mut self, v: bool)    { self.set(NEG_FLAG, v) }
    fn set_zero(&mut self, v: bool)        { self.set(ZERO_FLAG, v) }
    fn set_direct_page(&mut self, v: bool) { self.set(DIRECT_PAGE_FLAG, v) }
    fn set_carry(&mut self, v: bool)       { self.set(CARRY_FLAG, v) }
    fn set_half_carry(&mut self, v: bool)  { self.set(HALF_CARRY_FLAG, v) }
    fn set_overflow(&mut self, v: bool)    { self.set(OVERFLOW_FLAG, v) }

    fn set_nz(&mut self, val: u8) -> u8 {
        self.set_negative(val & 0x80 != 0);
        self.set_zero(val == 0);
        val
    }
}

impl Spc700 {
    fn load(&mut self, addr: u16) -> u8 {
        match addr {
            0xf0 | 0xf1 | 0xfa ... 0xfc =>
                panic!("APU attempted read from write-only register ${:02X}", addr),
            0xf2 => self.reg_dsp_addr,
            0xf3 => self.dsp.load(self.reg_dsp_addr),
            0xf4 ... 0xf7 => self.io_vals[addr as usize - 0xf4],
            0xfd => {
                let val = self.timers[0].val;
                self.timers[0].val = 0;
                val
            }
            0xfe => {
                let val = self.timers[1].val;
                self.timers[1].val = 0;
                val
            }
            0xff => {
                let val = self.timers[2].val;
                self.timers[2].val = 0;
                val
            }
            // NB: $f8 and $f9 work like regular RAM
            _ => self.mem[addr],
        }
    }

    fn store(&mut self, addr: u16, val: u8) {
        match addr {
            0xf0 => {
                assert!(val == 0x0a,
                    "SPC wrote ${:02X} to testing register (as a safety measure, \
                     only $0a is allowed)", 0);
            }
            0xf1 => {
                trace!("APU control write: ${:02X}", val);
                self.timers[0].set_enable(val & 0x01 != 0);
                self.timers[1].set_enable(val & 0x02 != 0);
                self.timers[2].set_enable(val & 0x04 != 0);
                if val & 0x10 != 0 {
                    self.io_vals[0] = 0;
                    self.io_vals[1] = 0;
                }
                if val & 0x20 != 0 {
                    self.io_vals[2] = 0;
                    self.io_vals[3] = 0;
                }
                // FIXME bit 7 can toggle IPL ROM and RAM
            },
            0xf2 => self.reg_dsp_addr = val,
            0xf3 => self.dsp.store(self.reg_dsp_addr, val),
            0xfa => self.timers[0].div = val,
            0xfb => self.timers[1].div = val,
            0xfc => self.timers[2].div = val,
            0xfd ... 0xff => panic!("APU attempted to write to read-only register ${:04X}", addr),
            // NB: Stores to 0xf4 - 0xf9 are just sent to RAM
            _ => self.mem[addr] = val,
        }
    }

    fn loadw(&mut self, addr: u16) -> u16 {
        let lo = self.load(addr) as u16;
        let hi = self.load(addr + 1) as u16;
        (hi << 8) | lo
    }

    fn fetchb(&mut self) -> u8 {
        let pc = self.pc;
        self.pc += 1;

        self.load(pc)
    }

    fn fetchw(&mut self) -> u16 {
        let lo = self.fetchb() as u16;
        let hi = self.fetchb() as u16;
        (hi << 8) | lo
    }

    fn trace_op(&self, pc: u16, opstr: &str) {
        trace!("${:04X}    {:02X}  {:16} a:{:02X} x:{:02X} y:{:02X} sp:{:02X} psw:{:08b}",
            pc,
            self.mem[pc],
            opstr,
            self.a,
            self.x,
            self.y,
            self.sp,
            self.psw.0,
        );
    }

    pub fn dispatch(&mut self) -> u8 {
        use log::LogLevel::Trace;

        // Cond. branches: +2 cycles if branch is taken
        static CYCLE_TABLE: [u8; 256] = [
            2,8,4,5,3,4,3,6, 2,6,5,4,5,4,6,8,   // $00-$0f
            2,8,4,5,4,5,5,6, 5,5,6,5,2,2,4,6,   // $10-$1f
            2,8,4,5,3,4,3,6, 2,6,5,4,5,4,5,4,   // $20-$2f
            2,8,4,5,4,5,5,6, 5,5,6,5,2,2,3,8,   // $30-$3f
            2,8,4,5,3,4,3,6, 2,6,4,4,5,4,6,6,   // $40-$4f
            2,8,4,5,4,5,5,6, 5,5,4,5,2,2,4,3,   // $50-$5f
            2,8,4,5,3,4,3,6, 2,6,4,4,5,4,5,5,   // $60-$6f
            2,8,4,5,4,5,5,6, 5,5,5,5,2,2,3,6,   // $70-$7f
            2,8,4,5,3,4,3,6, 2,6,5,4,5,2,4,5,   // $80-$8f
            2,8,4,5,4,5,5,6, 5,5,5,5,2,2,12,5,  // $90-$9f
            3,8,4,5,3,4,3,6, 2,6,4,4,5,2,4,4,   // $a0-$af
            2,8,4,5,4,5,5,6, 5,5,5,5,2,2,3,4,   // $b0-$bf
            3,8,4,5,4,5,4,7, 2,5,6,4,5,2,4,9,   // $c0-$cf
            2,8,4,5,5,6,6,7, 4,5,5,5,2,2,6,3,   // $d0-$df
            2,8,4,5,3,4,3,6, 2,4,5,3,4,3,4,3,   // $e0-$ef (last one is SLEEP, unknown timing)
            2,8,4,5,4,5,5,6, 3,4,5,4,2,2,5,3,   // $f0-$ff (last one is STOP, unknown timing)
        ];

        let pc = self.pc;

        macro_rules! e {
            ($e:expr) => ($e)
        }
        macro_rules! instr {
            ( _ $name:ident ) => {{
                if log_enabled!(Trace) && self.trace {
                    self.trace_op(pc, stringify!($name));
                }
                self.$name()
            }};
            ( $s:tt $name:ident ) => {{
                if log_enabled!(Trace) && self.trace {
                    self.trace_op(pc, e!($s));
                }
                self.$name()
            }};
            ( _ $name:ident ($arg:tt) $am:ident ) => {{
                let am = self.$am();
                if log_enabled!(Trace) && self.trace {
                    self.trace_op(pc, &format!(concat!(stringify!($name), " {}.", $arg), am));
                }
                self.$name(e!($arg), am)
            }};
            ( $s:tt $name:ident ($arg:tt) $am:ident ) => {{
                let am = self.$am();
                if log_enabled!(Trace) && self.trace {
                    self.trace_op(pc, &format!(e!($s), am));
                }
                self.$name(e!($arg), am)
            }};
            ( _ $name:ident ($arg:tt) $am:ident $am2:ident ) => {{
                let am = self.$am();
                let am2 = self.$am2();
                if log_enabled!(Trace) && self.trace {
                    self.trace_op(pc, &format!(concat!(stringify!($name), " {}.", $arg, ", {}"), am, am2));
                }
                self.$name(e!($arg), am, am2)
            }};
            ( $s:tt $name:ident ($arg:tt) $am:ident $am2:ident ) => {{
                let am = self.$am();
                let am2 = self.$am2();
                if log_enabled!(Trace) && self.trace {
                    self.trace_op(pc, &format!(e!($s), am, am2));
                }
                self.$name(e!($arg), am, am2)
            }};
            ( _ $name:ident $am:ident ) => {{
                let am = self.$am();
                if log_enabled!(Trace) && self.trace {
                    self.trace_op(pc, &format!(concat!(stringify!($name), " {}"), am));
                }
                self.$name(am)
            }};
            ( $s:tt $name:ident $am:ident ) => {{
                let am = self.$am();
                if log_enabled!(Trace) && self.trace {
                    self.trace_op(pc, &format!(e!($s), am));
                }
                self.$name(am)
            }};
            ( _ $name:ident $am:ident $am2:ident ) => {{
                let am = self.$am();
                let am2 = self.$am2();
                if log_enabled!(Trace) && self.trace {
                    self.trace_op(pc, &format!(concat!(stringify!($name), " {1}, {0}"), am, am2));
                }
                self.$name(am, am2)
            }};
            ( $s:tt $name:ident $am:ident $am2:ident ) => {{
                let am = self.$am();
                let am2 = self.$am2();
                if log_enabled!(Trace) && self.trace {
                    self.trace_op(pc, &format!(e!($s), am, am2));
                }
                self.$name(am, am2)
            }};
        }

        let op = self.fetchb();
        self.cy = CYCLE_TABLE[op as usize];
        match op {
            // Processor status
            0x20 => instr!(_ clrp),
            0x60 => instr!(_ clrc),
            0x80 => instr!(_ setc),
            0xed => instr!(_ notc),

            // Arithmetic
            0x9c => instr!(_ dec a),
            0x1d => instr!(_ dec x),
            0xdc => instr!(_ dec y),
            0x8b => instr!(_ dec direct),
            0x9b => instr!(_ dec direct_indexed_x),
            0x8c => instr!(_ dec abs),
            0xbc => instr!(_ inc a),
            0x3d => instr!(_ inc x),
            0xfc => instr!(_ inc y),
            0xab => instr!(_ inc direct),
            0xac => instr!(_ inc abs),
            0x3a => instr!(_ incw direct),
            0x28 => instr!(_ and immediate a),
            0x26 => instr!(_ and indirect_x a),
            0x37 => instr!(_ and indirect_indexed a),
            0x27 => instr!(_ and indexed_indirect a),
            0x24 => instr!(_ and direct a),
            0x34 => instr!(_ and direct_indexed_x a),
            0x25 => instr!(_ and abs a),
            0x35 => instr!(_ and abs_indexed_x a),
            0x36 => instr!(_ and abs_indexed_y a),
            0x29 => instr!(_ and direct direct),
            0x38 => instr!(_ and immediate direct),
            //0x19 => instr!(_ or indirect_y indirect_x),   TODO
            0x08 => instr!(_ or immediate a),
            0x06 => instr!(_ or indirect_x a),
            //0x17 => instr!(_ or direct_indexed_y a),  TODO
            0x07 => instr!(_ or direct_indexed_x a),
            0x04 => instr!(_ or direct a),
            0x14 => instr!(_ or direct_indexed_x a),
            0x05 => instr!(_ or abs a),
            0x15 => instr!(_ or abs_indexed_x a),
            0x16 => instr!(_ or abs_indexed_y a),
            0x09 => instr!(_ or direct direct),
            0x18 => instr!(_ or immediate direct),
            0x48 => instr!(_ eor immediate a),
            0x44 => instr!(_ eor direct a),
            0x1c => instr!(_ asl a),
            0x0b => instr!(_ asl direct),
            0x1b => instr!(_ asl direct_indexed_x),
            0x0c => instr!(_ asl abs),
            0x5c => instr!(_ lsr a),
            0x4b => instr!(_ lsr direct),
            0x5b => instr!(_ lsr direct_indexed_x),
            0x4c => instr!(_ lsr abs),
            0x7c => instr!(_ ror a),
            0x6b => instr!(_ ror direct),
            0x7b => instr!(_ ror direct_indexed_x),
            0x6c => instr!(_ ror abs),
            //0x99 => instr!(_ adc indirect_y indirect_x),  TODO
            0x88 => instr!(_ adc immediate a),
            0x86 => instr!(_ adc indirect_x a),
            0x97 => instr!(_ adc indirect_indexed a),
            0x87 => instr!(_ adc indexed_indirect a),
            0x84 => instr!(_ adc direct a),
            0x94 => instr!(_ adc direct_indexed_x a),
            0x85 => instr!(_ adc abs a),
            0x95 => instr!(_ adc abs_indexed_x a),
            0x96 => instr!(_ adc abs_indexed_y a),
            0x89 => instr!(_ adc direct direct),
            0x98 => instr!(_ adc immediate direct),
            0x7a => instr!("addw ya, {}" addw direct),
            0xa8 => instr!(_ sbc immediate a),
            0xa4 => instr!(_ sbc direct a),
            0xb4 => instr!(_ sbc direct_indexed_x a),
            0xa9 => instr!(_ sbc direct direct),
            0xa6 => instr!(_ sbc indirect_x a),
            0xa5 => instr!(_ sbc abs a),
            0xb5 => instr!(_ sbc abs_indexed_x a),
            0xb6 => instr!(_ sbc abs_indexed_y a),
            0x9a => instr!("subw ya, {0}" subw direct),
            0xcf => instr!("mul ya" mul),
            0x9e => instr!("div ya, x" div),
            0x9f => instr!(_ xcn a),

            // Control flow and comparisons
            0x78 => instr!(_ cmp immediate direct),
            0x64 => instr!(_ cmp direct a),
            0x3e => instr!(_ cmp direct x),
            0x7e => instr!(_ cmp direct y),
            0x69 => instr!(_ cmp direct direct),
            0x68 => instr!(_ cmp immediate a),
            0xc8 => instr!(_ cmp immediate x),
            0xad => instr!(_ cmp immediate y),
            0x65 => instr!(_ cmp abs a),
            0x1e => instr!(_ cmp abs x),
            0x5e => instr!(_ cmp abs y),
            0x75 => instr!(_ cmp abs_indexed_x a),
            0x76 => instr!(_ cmp abs_indexed_y a),

            0xde => instr!("cbne {}, {}" cbne direct_indexed_x rel),
            0x2e => instr!("cbne {}, {}" cbne direct rel),
            0xfe => instr!("dbnz {}, {}" dbnz y rel),
            0x6e => instr!("dbnz {}, {}" dbnz direct rel),

            0x02 => instr!(_ set1(0) direct),
            0x22 => instr!(_ set1(1) direct),
            0x42 => instr!(_ set1(2) direct),
            0x62 => instr!(_ set1(3) direct),
            0x82 => instr!(_ set1(4) direct),
            0xa2 => instr!(_ set1(5) direct),
            0xc2 => instr!(_ set1(6) direct),
            0xe2 => instr!(_ set1(7) direct),
            0x12 => instr!(_ clr1(0) direct),
            0x32 => instr!(_ clr1(1) direct),
            0x52 => instr!(_ clr1(2) direct),
            0x72 => instr!(_ clr1(3) direct),
            0x92 => instr!(_ clr1(4) direct),
            0xb2 => instr!(_ clr1(5) direct),
            0xd2 => instr!(_ clr1(6) direct),
            0xf2 => instr!(_ clr1(7) direct),
            0x13 => instr!(_ bbc(0) direct rel),
            0x33 => instr!(_ bbc(1) direct rel),
            0x53 => instr!(_ bbc(2) direct rel),
            0x73 => instr!(_ bbc(3) direct rel),
            0x93 => instr!(_ bbc(4) direct rel),
            0xb3 => instr!(_ bbc(5) direct rel),
            0xd3 => instr!(_ bbc(6) direct rel),
            0xf3 => instr!(_ bbc(7) direct rel),

            0x5f => instr!("jmp {}" bra abs),                       // reuse `bra` fn
            0x1f => instr!("jmp {}" bra abs_indexed_indirect),      // reuse `bra` fn
            0x2f => instr!(_ bra rel),
            0xf0 => instr!(_ beq rel),
            0xd0 => instr!(_ bne rel),
            0xb0 => instr!(_ bcs rel),
            0x90 => instr!(_ bcc rel),
            0x30 => instr!(_ bmi rel),
            0x10 => instr!(_ bpl rel),

            0x3f => instr!(_ call abs),
            0x6f => instr!(_ ret),

            0x2d => instr!(_ push a),
            0x4d => instr!(_ push x),
            0x6d => instr!(_ push y),
            0xae => instr!(_ pop a),
            0xce => instr!(_ pop x),
            0xee => instr!(_ pop y),

            // "mov"
            // NB: For moves, "a x" means "mov x, a" or "a -> x"
            // NB: Moves into registers will always set N and Z
            0x8f => instr!(_ mov immediate direct),
            0xe8 => instr!(_ mov immediate a),
            0xcd => instr!(_ mov immediate x),
            0x8d => instr!(_ mov immediate y),
            0x5d => instr!(_ mov a x),
            0xfd => instr!(_ mov a y),
            0xc4 => instr!(_ mov a direct),
            0xd4 => instr!(_ mov a direct_indexed_x),
            0xc5 => instr!(_ mov a abs),
            0xd5 => instr!(_ mov a abs_indexed_x),
            0xd6 => instr!(_ mov a abs_indexed_y),
            0xc6 => instr!(_ mov a indirect_x),
            0xd7 => instr!(_ mov a indirect_indexed),
            0x7d => instr!(_ mov x a),
            0xd8 => instr!(_ mov x direct),
            0xd9 => instr!(_ mov x direct_indexed_x),
            0xc9 => instr!(_ mov x abs),
            0xdd => instr!(_ mov y a),
            0xcb => instr!(_ mov y direct),
            0xdb => instr!(_ mov y direct_indexed_x),
            0xcc => instr!(_ mov y abs),
            0xe4 => instr!(_ mov direct a),
            0xf8 => instr!(_ mov direct x),
            0xeb => instr!(_ mov direct y),
            0xf4 => instr!(_ mov direct_indexed_x a),
            0xe6 => instr!(_ mov indirect_x a),
            0xe7 => instr!(_ mov indexed_indirect a),
            0xf7 => instr!(_ mov indirect_indexed a),
            0xe5 => instr!(_ mov abs a),
            0xe9 => instr!(_ mov abs x),
            0xec => instr!(_ mov abs y),
            0xf5 => instr!(_ mov abs_indexed_x a),
            0xf6 => instr!(_ mov abs_indexed_y a),
            0xba => instr!("movw ya, {}" movw_l direct),
            0xda => instr!("movw {}, ya" movw_s direct),
            0xbd => instr!("mov sp, x" mov_sp_x),
            0xaf => instr!("mov (x++), a" mov_xinc),
            _ => {
                instr!(_ ill);
                panic!("illegal APU opcode: ${:02X}", op);
            }
        }

        self.timers[0].update(128, self.cy);
        self.timers[1].update(128, self.cy);
        self.timers[2].update(16, self.cy);
        self.cy
    }

    fn pushb(&mut self, b: u8) {
        let sp = 0x0100 | self.sp as u16;
        self.store(sp, b);
        // FIXME This wraps, but we'll let it crash
        self.sp -= 1;
    }

    /// Pushes the high byte, then the low byte
    fn pushw(&mut self, w: u16) {
        let lo = w as u8;
        let hi = (w >> 8) as u8;
        self.pushb(hi);
        self.pushb(lo);
    }

    fn popb(&mut self) -> u8 {
        self.sp += 1;
        let sp = 0x0100 | self.sp as u16;
        self.load(sp)
    }

    /// Pops the low byte, then the high byte
    fn popw(&mut self) -> u16 {
        let lo = self.popb() as u16;
        let hi = self.popb() as u16;
        (hi << 8) | lo
    }

    /// Performs a call: Pushed PCh and PCl onto the stack and sets PC to `addr`.
    fn call_addr(&mut self, addr: u16) {
        let pc = self.pc;
        self.pushw(pc);
        self.pc = addr;
    }
}

/// Opcode implementations
impl Spc700 {
    fn push(&mut self, am: AddressingMode) {
        let v = am.loadb(self);
        self.pushb(v);
    }
    fn pop(&mut self, dest: AddressingMode) {
        let v = self.popb();
        dest.storeb(self, v);
    }

    fn ret(&mut self) {
        let pc = self.popw();
        self.pc = pc;
    }
    fn call(&mut self, am: AddressingMode) {
        let addr = am.address(self);
        self.call_addr(addr);
    }

    /// Clear direct page bit
    fn clrp(&mut self) { self.psw.set_direct_page(false) }
    /// Clear carry
    fn clrc(&mut self) { self.psw.set_carry(false) }
    /// Set carry
    fn setc(&mut self) { self.psw.set_carry(true) }
    fn notc(&mut self) {
        let c = self.psw.carry();
        self.psw.set_carry(!c);
    }

    fn cmp(&mut self, a: AddressingMode, b: AddressingMode) {
        // Sets N, Z and C
        // FIXME check if the order is correct
        let a = a.loadb(self);
        let b = b.loadb(self);

        let diff = a.wrapping_sub(b);
        self.psw.set_nz(diff);
        self.psw.set_carry(diff & 0x80 != 0);
    }

    /// Set bit
    fn set1(&mut self, bit: u8, am: AddressingMode) {
        // Sets no flags
        let mut val = am.clone().loadb(self);
        val |= 1 << bit;
        am.storeb(self, val);
    }
    /// Clear bit
    fn clr1(&mut self, bit: u8, am: AddressingMode) {
        // Sets no flags
        let mut val = am.clone().loadb(self);
        val &= !(1 << bit);
        am.storeb(self, val);
    }
    /// Branch if bit clear
    fn bbc(&mut self, bit: u8, val: AddressingMode, addr: AddressingMode) {
        let val = val.loadb(self);
        let addr = addr.address(self);
        if val & (1 << bit) != 0 {
            self.pc = addr;
            self.cy += 2;
        }
    }
    /// Decrement and branch if zero
    fn dbnz(&mut self, val: AddressingMode, addr: AddressingMode) {
        let v = val.clone().loadb(self);
        let a = addr.address(self);
        let res = v.wrapping_sub(1);
        val.storeb(self, res);
        if res == 0 {
            self.pc = a;
            self.cy += 2;
        }
    }
    /// Compare and branch if not equal
    fn cbne(&mut self, cmp: AddressingMode, addr: AddressingMode) {
        let cmp = cmp.loadb(self);
        let a = addr.address(self);
        if cmp != self.a {
            self.pc = a;
            self.cy += 2;
        }
    }

    fn bra(&mut self, am: AddressingMode) {
        let a = am.address(self);
        self.pc = a;
    }
    fn beq(&mut self, am: AddressingMode) {
        let a = am.address(self);
        if self.psw.zero() {
            self.pc = a;
            self.cy += 2;
        }
    }
    fn bne(&mut self, am: AddressingMode) {
        let a = am.address(self);
        if !self.psw.zero() {
            self.pc = a;
            self.cy += 2;
        }
    }
    /// Branch if carry set
    fn bcs(&mut self, am: AddressingMode) {
        let a = am.address(self);
        if self.psw.carry() {
            self.pc = a;
            self.cy += 2;
        }
    }
    /// Branch if carry clear
    fn bcc(&mut self, am: AddressingMode) {
        let a = am.address(self);
        if !self.psw.carry() {
            self.pc = a;
            self.cy += 2;
        }
    }
    fn bmi(&mut self, am: AddressingMode) {
        let a = am.address(self);
        if self.psw.negative() {
            self.pc = a;
            self.cy += 2;
        }
    }
    fn bpl(&mut self, am: AddressingMode) {
        let a = am.address(self);
        if !self.psw.negative() {
            self.pc = a;
            self.cy += 2;
        }
    }

    /// Exchange nibbles of byte
    fn xcn(&mut self, am: AddressingMode) {
        // Sets N and Z
        let val = am.clone().loadb(self);
        let res = (val >> 4) | (val << 4);
        self.psw.set_nz(res);
        am.storeb(self, res);
    }
    /// `mul ya`: ya = y * a
    fn mul(&mut self) {
        // Sets N and Z. Y = High, A = Low.
        let res = (self.y as u16).wrapping_mul(self.a as u16);
        self.y = self.psw.set_nz((res >> 8) as u8);
        self.a = res as u8;
    }
    /// A=YA/X, Y=mod(YA,X)
    fn div(&mut self) {
        // Sets N, Z, V, H
        // FIXME Set V and H
        let ya = ((self.y as u16) << 8) | self.a as u16;
        self.a = self.psw.set_nz((ya / self.x as u16) as u8);
        self.y = (ya % self.x as u16) as u8;
    }
    fn adc(&mut self, src: AddressingMode, dest: AddressingMode) {
        // Sets N, V, H, Z and C
        let c = if self.psw.carry() { 1 } else { 0 };
        let a = dest.clone().loadb(self);
        let b = src.loadb(self);
        let res = (a as u16).wrapping_add(b as u16).wrapping_add(c as u16);
        self.psw.set_carry(res > 255);
        self.psw.set_half_carry(((a & 0x0f) + (b & 0x0f) + c) & 0xf0 != 0);
        let res = res as u8;
        self.psw.set_overflow((a ^ b) & 0x80 == 0 && (a ^ res) & 0x80 == 0x80);
        self.psw.set_nz(res);
        dest.storeb(self, res);
    }
    fn addw(&mut self, am: AddressingMode) {
        // Sets N, V, H, Z and C (H on high byte)
        // FIXME: Set H and check if this is correct
        // YA := YA + <byte> (Y = High, A = Low)
        let ya = ((self.y as u16) << 8) | self.a as u16;
        let val = am.loadb(self) as u16;
        let res = (ya as u32).wrapping_add(val as u32);
        self.psw.set_carry(res & 0xffff0000 != 0);
        let res = res as u16;
        self.psw.set_overflow((ya ^ val) & 0x8000 == 0 && (ya ^ res) & 0x8000 == 0x8000);
        self.psw.set_negative(res & 0x8000 != 0);
        self.psw.set_zero(res == 0);
        self.y = (res >> 8) as u8;
        self.a = res as u8;
    }
    fn sbc(&mut self, src: AddressingMode, dest: AddressingMode) {
        // Sets N, V, H, Z and C
        // FIXME Set H and V
        let c = if self.psw.carry() { 0 } else { 1 };
        let a = dest.clone().loadb(self) as u16;
        let b = src.loadb(self) as u16;
        // a-b = a+!b+1
        let res = a.wrapping_add(!b).wrapping_add(c);
        self.psw.set_carry(res > 255);
        self.psw.set_nz(res as u8);
        dest.storeb(self, res as u8);
    }
    /// Subtract from YA (Carry is set, but ignored for the operation)
    fn subw(&mut self, am: AddressingMode) {
        // Sets N, V, H (on high byte), Z, C
        // FIXME Set V and H
        let ya = ((self.y as u32) << 8) | self.a as u32;
        let sub = am.loadb(self) as u32;
        let res = ya.wrapping_add(!sub);
        self.psw.set_carry(res & 0xffff0000 != 0);
        self.psw.set_negative(res & 0x8000 != 0);
        self.psw.set_zero(res == 0);
        self.y = (ya >> 8) as u8;
        self.a = ya as u8;
    }
    fn and(&mut self, r: AddressingMode, l: AddressingMode) {
        // Sets N and Z
        // l := l & r
        let rb = r.loadb(self);
        let lb = l.clone().loadb(self);
        let res = self.psw.set_nz(lb & rb);
        l.storeb(self, res);
    }
    fn or(&mut self, r: AddressingMode, l: AddressingMode) {
        // Sets N and Z
        // l := l | r
        let rb = r.loadb(self);
        let lb = l.clone().loadb(self);
        let res = self.psw.set_nz(lb | rb);
        l.storeb(self, res);
    }
    /// Exclusive Or
    fn eor(&mut self, r: AddressingMode, l: AddressingMode) {
        // Sets N and Z
        // l := l ^ r
        let rb = r.loadb(self);
        let lb = l.clone().loadb(self);
        let res = self.psw.set_nz(lb ^ rb);
        l.storeb(self, res);
    }
    /// Left shift
    fn asl(&mut self, op: AddressingMode) {
        let val = op.clone().loadb(self);
        self.psw.set_carry(val & 0x80 != 0);
        let res = self.psw.set_nz(val << 1);
        op.storeb(self, res);
    }
    /// Right shift
    fn lsr(&mut self, op: AddressingMode) {
        let val = op.clone().loadb(self);
        self.psw.set_carry(val & 0x01 != 0);
        let res = self.psw.set_nz(val >> 1);
        op.storeb(self, res);
    }
    /// Rotate right
    fn ror(&mut self, op: AddressingMode) {
        let val = op.clone().loadb(self);
        let c = if self.psw.carry() { 0x80 } else { 0 };
        self.psw.set_carry(val & 0x01 != 0);
        let res = self.psw.set_nz((val >> 1) | c);
        op.storeb(self, res);
    }
    fn dec(&mut self, am: AddressingMode) {
        // Sets N and Z
        let val = am.clone().loadb(self);
        let res = self.psw.set_nz(val.wrapping_sub(1));
        am.storeb(self, res);
    }
    fn inc(&mut self, am: AddressingMode) {
        // Sets N and Z
        let val = am.clone().loadb(self);
        let res = self.psw.set_nz(val.wrapping_add(1));
        am.storeb(self, res);
    }
    fn incw(&mut self, am: AddressingMode) {
        // Sets N and Z
        let (lo, hi) = am.clone().loadw(self);
        let val = ((hi as u16) << 8) | lo as u16;
        let res = val.wrapping_add(1);
        am.storew(self, ((res >> 8) as u8, res as u8));
    }

    /// `mov (X++), A` - Move A to the address pointed to by X, then increment X
    fn mov_xinc(&mut self) {
        // No flags changed
        // FIXME Does this work with direct page?
        let addr = self.x as u16;
        let a = self.a;
        self.store(addr, a);
        self.x = self.x.wrapping_add(1);
    }
    /// movw-load. Fetches a word from the addressing mode and puts it into Y (high) and A (low)
    /// (`movw ya, {X}`)
    fn movw_l(&mut self, am: AddressingMode) {
        // FIXME Are the flags set right?
        let (lo, hi) = am.loadw(self);
        self.y = self.psw.set_nz(hi);
        self.a = lo;
    }
    /// movw-store. Stores Y (high) and A (low) at the given word address.
    /// (`movw {X}, ya`)
    fn movw_s(&mut self, am: AddressingMode) {
        // No flags modified, Reads the low byte first
        let y = self.y;
        let a = self.a;
        am.clone().loadb(self);
        am.storew(self, (a, y));
    }
    /// Copy a byte
    fn mov(&mut self, src: AddressingMode, dest: AddressingMode) {
        // No flags modified. If a register is written, the corresponding `AddressingMode` will set
        // the flags.
        let val = src.loadb(self);
        dest.storeb(self, val);
    }
    fn mov_sp_x(&mut self) {
        // No flags modified
        self.sp = self.x;
    }
    fn ill(&mut self) {}
}

/// Addressing mode construction
impl Spc700 {
    fn direct(&mut self) -> AddressingMode {
        AddressingMode::Direct(self.fetchb())
    }
    fn direct_indexed_x(&mut self) -> AddressingMode {
        AddressingMode::DirectIndexedX(self.fetchb())
    }
    fn indirect_x(&mut self) -> AddressingMode {
        AddressingMode::IndirectX
    }
    fn indirect_indexed(&mut self) -> AddressingMode {
        AddressingMode::IndirectIndexed(self.fetchb())
    }
    fn indexed_indirect(&mut self) -> AddressingMode {
        AddressingMode::IndexedIndirect(self.fetchb())
    }
    fn abs_indexed_indirect(&mut self) -> AddressingMode {
        AddressingMode::AbsIndexedIndirect(self.fetchw())
    }
    fn abs(&mut self) -> AddressingMode {
        AddressingMode::Abs(self.fetchw())
    }
    fn abs_indexed_x(&mut self) -> AddressingMode {
        AddressingMode::AbsIndexedX(self.fetchw())
    }
    fn abs_indexed_y(&mut self) -> AddressingMode {
        AddressingMode::AbsIndexedY(self.fetchw())
    }
    fn immediate(&mut self) -> AddressingMode {
        AddressingMode::Immediate(self.fetchb())
    }
    fn rel(&mut self) -> AddressingMode {
        AddressingMode::Rel(self.fetchb() as i8)
    }
    fn a(&mut self) -> AddressingMode {
        AddressingMode::A
    }
    fn x(&mut self) -> AddressingMode {
        AddressingMode::X
    }
    fn y(&mut self) -> AddressingMode {
        AddressingMode::Y
    }
}
