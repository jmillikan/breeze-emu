mod addressing;
mod dsp;
mod timer;

use self::addressing::AddressingMode;
use self::dsp::Dsp;
use self::timer::Timer;

pub type Apu = Spc700;


const RAM_SIZE: usize = 65536;
const RESET_VEC: u16 = 0xFFFE;

/// The SPC700 processor used in the APU is an 8-bit processor with a 16-bit address space. It has
/// 64 KB of RAM shared with the DSP. The last 64 Bytes in its address space are mapped to the
/// "IPL ROM", which contains a small piece of startup code that allows the main CPU to transfer a
/// program to the APU (we just copy the IPL ROM into the RAM and make it read-write).
pub struct Spc700 {
    // 64KB of RAM
    // (this is not the address space, even though both are 64KB!)
    mem: [u8; RAM_SIZE],

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

// Public interface
impl Spc700 {
    pub fn new() -> Spc700 {
        const IPL_START: usize = RAM_SIZE - 64;

        let mut mem = [0; RAM_SIZE as usize];
        for i in 0..64 {
            mem[IPL_START as usize + i] = IPL_ROM[i];
        }

        let pcl = mem[RESET_VEC as usize] as u16;
        let pch = mem[RESET_VEC as usize + 1] as u16;
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
            psw: StatusReg(0),  // FIXME is 0 correct`?
            cy: 0,
            trace: false,
        }
    }

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
        let val = self.mem[0xf4 + port as usize];
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
    fn half_carry(&self) -> bool  { self.0 & HALF_CARRY_FLAG != 0 }
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
            0xfd => self.timers[0].val,
            0xfe => self.timers[1].val,
            0xff => self.timers[2].val,
            // NB: $f8 and $f9 are regular RAM
            _ => self.mem[addr as usize],
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
            0xfd ... 0xff => panic!("APU attempted to write to read-only register ${:02X}", addr),
            // NB: Stores to 0xf4 - 0xf9 are just sent to RAM
            _ => self.mem[addr as usize] = val,
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
        trace!("{:04X}     {:16} a:{:02X} x:{:02X} y:{:02X} sp:{:02X} psw:{:08b}",
            pc,
            opstr,
            self.a,
            self.x,
            self.y,
            self.sp,
            self.psw.0,
        );
    }

    pub fn dispatch(&mut self) -> u8 {
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
            2,8,4,5,3,4,3,6, 2,4,5,3,4,3,4,4,   // $e0-$ef (last one is SLEEP, unknown timing)
            2,8,4,5,4,5,5,6, 3,4,5,4,2,2,4,4,   // $f0-$ff (last one is STOP, unknown timing)
        ];

        let pc = self.pc;

        macro_rules!e{($e:expr)=>($e)}
        macro_rules! instr {
            ( $name:ident $s:tt ) => {{
                use log::LogLevel::Trace;
                if log_enabled!(Trace) && self.trace {
                    self.trace_op(pc, e!($s));
                }
                self.$name()
            }};
            ( $name:ident $s:tt $am:ident ) => {{
                use log::LogLevel::Trace;
                let am = self.$am();
                if log_enabled!(Trace) && self.trace {
                    self.trace_op(pc, &format!(e!($s), am));
                }
                self.$name(am)
            }};
            ( $name:ident $s:tt $am:ident $am2:ident ) => {{
                use log::LogLevel::Trace;
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
            0x20 => instr!(clrp "clrp"),
            0x60 => instr!(clrc "clrc"),

            // Arithmetic
            0x1d => instr!(dec "dec {}" x),
            0x3d => instr!(inc "inx {}" x),
            0xfc => instr!(inc "inc {}" y),
            0xab => instr!(inc "inc {}" direct),
            0x84 => instr!(adc "adc {1}, {0}" direct a),
            0xcf => instr!(mul "mul ya"),

            // Control flow and comparisons
            0x78 => instr!(cmp "cmp {1}, {0}" immediate direct),
            0x7e => instr!(cmp "cmp {1}, {0}" direct y),
            0x68 => instr!(cmp "cmp {1}, {0}" immediate a),
            0xc8 => instr!(cmp "cmp {1}, {0}" immediate x),
            0xad => instr!(cmp "cmp {1}, {0}" immediate y),
            0x5e => instr!(cmp "cmp {1}, {0}" abs y),
            0x75 => instr!(cmp "cmp {1}, {0}" abs_indexed_x a),

            0xde => instr!(cbne "cbne {}, {}" indexed_indirect rel),

            0x1f => instr!(bra "jmp {}" abs_indexed_indirect),    // reuse `bra` fn
            0x2f => instr!(bra "bra {}" rel),
            0xf0 => instr!(beq "beq {}" rel),
            0xd0 => instr!(bne "bne {}" rel),
            0x90 => instr!(bcc "bcc {}" rel),
            0x30 => instr!(bmi "bmi {}" rel),
            0x10 => instr!(bpl "bpl {}" rel),

            0x3f => instr!(call "call {}" abs),
            0x6f => instr!(ret "ret"),

            0x2d => instr!(push "push {}" a),
            0x4d => instr!(push "push {}" x),
            0x6d => instr!(push "push {}" y),
            0xee => instr!(pop "pop {}" y),

            // "mov"
            // NB: For moves, "a x" means "mov x, a" or "a -> x"
            // NB: Moves into registers will always set N and Z
            0x8f => instr!(mov "mov {1}, {0}" immediate direct),
            0xe8 => instr!(mov "mov {1}, {0}" immediate a),
            0xcd => instr!(mov "mov {1}, {0}" immediate x),
            0x8d => instr!(mov "mov {1}, {0}" immediate y),
            0x5d => instr!(mov "mov {1}, {0}" a x),
            0xfd => instr!(mov "mov {1}, {0}" a y),
            0xc4 => instr!(mov "mov {1}, {0}" a direct),
            0xc5 => instr!(mov "mov {1}, {0}" a abs),
            0xd5 => instr!(mov "mov {1}, {0}" a abs_indexed_x),
            0xd6 => instr!(mov "mov {1}, {0}" a abs_indexed_y),
            0xc6 => instr!(mov "mov {1}, {0}" a indirect_x),
            0xd7 => instr!(mov "mov {1}, {0}" a indirect_indexed),
            0x7d => instr!(mov "mov {1}, {0}" x a),
            0xc9 => instr!(mov "mov {1}, {0}" x abs),
            0xdd => instr!(mov "mov {1}, {0}" y a),
            0xcb => instr!(mov "mov {1}, {0}" y direct),
            0xcc => instr!(mov "mov {1}, {0}" y abs),
            0xdb => instr!(mov "mov {1}, {0}" y indexed_indirect),
            0xe4 => instr!(mov "mov {1}, {0}" direct a),
            0xeb => instr!(mov "mov {1}, {0}" direct y),
            0xe5 => instr!(mov "mov {1}, {0}" abs a),
            0xec => instr!(mov "mov {1}, {0}" abs y),
            0xf5 => instr!(mov "mov {1}, {0}" abs_indexed_x a),
            0xf4 => instr!(mov "mov {1}, {0}" indexed_indirect a),
            0xba => instr!(movw_l "movw ya, {}" direct),
            0xda => instr!(movw_s "movw {}, ya" direct),
            0xbd => instr!(mov_sp_x "mov sp, x"),
            0xaf => instr!(mov_xinc "mov x++, a"),
            _ => {
                instr!(ill "ill");
                panic!("illegal APU opcode at ${:04X}: ${:02X}", pc, op);
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

    fn cmp(&mut self, a: AddressingMode, b: AddressingMode) {
        // Sets N, Z and C
        // FIXME check if the order is correct
        let a = a.loadb(self);
        let b = b.loadb(self);

        let diff = a.wrapping_sub(b);
        self.psw.set_nz(diff);
        self.psw.set_carry(diff & 0x80 != 0);
    }

    fn cbne(&mut self, cmp: AddressingMode, addr: AddressingMode) {
        let cmp = cmp.loadb(self);
        if cmp != self.a {
            let a = addr.address(self);
            self.pc = a;
            self.cy += 2;
        }
    }

    fn bra(&mut self, am: AddressingMode) {
        let a = am.address(self);
        self.pc = a;
    }
    fn beq(&mut self, am: AddressingMode) {
        if self.psw.zero() {
            let a = am.address(self);
            self.pc = a;
            self.cy += 2;
        }
    }
    fn bne(&mut self, am: AddressingMode) {
        if !self.psw.zero() {
            let a = am.address(self);
            self.pc = a;
            self.cy += 2;
        }
    }
    fn bcc(&mut self, am: AddressingMode) {
        if !self.psw.carry() {
            let a = am.address(self);
            self.pc = a;
            self.cy += 2;
        }
    }
    fn bmi(&mut self, am: AddressingMode) {
        if self.psw.negative() {
            let a = am.address(self);
            self.pc = a;
            self.cy += 2;
        }
    }
    fn bpl(&mut self, am: AddressingMode) {
        if !self.psw.negative() {
            let a = am.address(self);
            self.pc = a;
            self.cy += 2;
        }
    }

    /// `mul ya` - ya = y * a
    fn mul(&mut self) {
        // Sets N and Z. Y = High, A = Low.
        let res = self.y as u16 * self.a as u16;
        self.y = self.psw.set_nz((res >> 8) as u8);
        self.a = res as u8;
    }
    fn adc(&mut self, src: AddressingMode, dest: AddressingMode) {
        // Set N, V, H, Z and C
        let c = if self.psw.carry() { 1 } else { 0 };
        let a = dest.clone().loadb(self);
        let b = src.loadb(self);
        let res = a as u16 + b as u16 + c as u16;
        self.psw.set_carry(res > 255);
        self.psw.set_half_carry(((a & 0x0f) + (b & 0x0f) + c) & 0xf0 != 0);
        let res = res as u8;
        self.psw.set_overflow((a ^ b) & 0x80 == 0 && (a ^ res) & 0x80 == 0x80);
        self.psw.set_nz(res);
        dest.storeb(self, res);
    }
    fn dec(&mut self, am: AddressingMode) {
        let val = am.clone().loadb(self);
        am.storeb(self, val.wrapping_sub(1));
    }
    fn inc(&mut self, am: AddressingMode) {
        let val = am.clone().loadb(self);
        am.storeb(self, val.wrapping_add(1));
    }

    /// `mov (X++), A` - Move A to the address pointed to by X, then increment X
    fn mov_xinc(&mut self) {
        // No flags changed
        // FIXME Does this work with direct page?
        let addr = self.x as u16;
        let a = self.a;
        self.store(addr, a);
        self.x += 1;
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
        // No flags modified
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

static IPL_ROM: [u8; 64] = [
    // NOTE: mov operands are `dest, source`

    // Set up stack pointer at $01ef
    0xcd, 0xef,         // FFC0  mov x, #$ef
    0xbd,               // FFC2  mov sp, x
    // Fill memory at $00-$ef with $00 (also sets all registers to 0)
    0xe8, 0x00,         // FFC3  mov a, #$00
    0xc6,               // FFC5  mov (x), a       :fill_zero_page
    0x1d,               // FFC6  dec x
    0xd0, 0xfc,         // FFC7  bne $ffc5        -> fill_zero_page

    // Write 0xaabb to ports 0 and 1 (registers $f4 and $f5)
    // This is the "ready" signal. The main CPU will wait until it sees it.
    0x8f, 0xaa, 0xf4,   // FFC9  mov $f4, #$aa
    0x8f, 0xbb, 0xf5,   // FFCC  mov $f5, #$bb

    // Wait until $cc is written to port 0 (reg $f4)
    0x78, 0xcc, 0xf4,   // FFCF  cmp $f4, #$cc    :wait_start
    0xd0, 0xfb,         // FFD2  bne $ffcf        -> wait_start

    0x2f, 0x19,         // FFD4  bra $ffef        -> start
    //---------------

    // Wait until a non-zero value is in reg 0
    0xeb, 0xf4,         // FFD6  mov y, $f4       :recv
    0xd0, 0xfc,         // FFD8  bne $ffd6        -> recv

    0x7e, 0xf4,         // FFDA  cmp y, $f4       :loop
    0xd0, 0x0b,         // FFDC  bne $ffe9        -> lbl1
    0xe4, 0xf5,         // FFDE  mov a, $f5
    0xcb, 0xf4,         // FFE0  mov $f4, y
    0xd7, 0x00,         // FFE2  mov [$00]+y, a
    0xfc,               // FFE4  inc y
    0xd0, 0xf3,         // FFE5  bne $ffda        -> loop
    0xab, 0x01,         // FFE7  inc $01
    0x10, 0xef,         // FFE9  bpl $ffda        :lbl1   -> loop
    0x7e, 0xf4,         // FFEB  cmp y, $f4
    0x10, 0xeb,         // FFED  bpl $ffda        -> loop

    // Load reg 2 ($f6) into y and reg 3 ($f7) into a
    0xba, 0xf6,         // FFEF  movw ya, $f6     :start
    // Write Y and A in memory at $00 and $01
    0xda, 0x00,         // FFF1  movw $00, ya
    // Load reg 0 ($f4) into y, reg 1 ($f5) into a
    0xba, 0xf4,         // FFF3  movw ya, $f4
    // Write reg 1's value to reg 0
    0xc4, 0xf4,         // FFF5  mov $f4, a
    0xdd,               // FFF7  mov a, y
    0x5d,               // FFF8  mov x, a
    // If reg 1's value is not 0, start the transmission loop
    0xd0, 0xdb,         // FFF9  bne $ffd6 (-37)  -> recv

    // We're done, jump to the address we just received and stored in $0000 (x is 0 here)
    0x1f, 0x00, 0x00,   // FFFB  jmp [$0000+x]

    // reset vector is at 0xfffe and points to the start of the IPL ROM: 0xffc0
    0xc0, 0xff,
];