pub struct Apu {
    cpu: Spc700,
}

impl Apu {
    pub fn new() -> Apu {
        Apu {
            cpu: Spc700::new(),
        }
    }

    /// Store a byte in an IO port (0-3)
    ///
    /// IO ports are mapped to internal registers 0xf4 - 0xf7
    pub fn store(&mut self, port: u8, value: u8) {
        self.cpu.store(0xf4 + port as u16, value)
    }

    /// Load a byte from an IO port (0-3)
    ///
    /// IO ports are mapped to internal registers 0xf4 - 0xf7
    pub fn load(&mut self, port: u8) -> u8 {
        self.cpu.load(0xf4 + port as u16)
    }

    // FIXME temp. function
    pub fn tick(&mut self) {
        self.cpu.dispatch()
    }
}

/// The SPC700 processor used in the APU is an 8-bit processor with a 16-bit address space. It has
/// 64 KB of RAM. The last 64 Bytes in it's address space are mapped to the "IPL ROM", which
/// contains a small piece of startup code that allows the main CPU to transfer a program to the
/// APU (we just copy the IPL ROM into the RAM and make it read-write).
struct Spc700 {
    // 64KB of RAM (FIXME use a fixed-size array)
    // (this is not the address space!)
    mem: Vec<u8>,

    a: u8,
    x: u8,
    y: u8,
    sp: u8,
    pc: u16,
    psw: StatusReg,
}

const RESET_VEC: u16 = 0xFFFE;

// PSW - Program Status Word
struct StatusReg(u8);
const NEG_FLAG: u8         = 1 << 7;
const OVERFLOW_FLAG: u8    = 1 << 6;
const DIRECT_PAGE_FLAG: u8 = 1 << 5;
//
const HALF_CARRY_FLAG: u8  = 1 << 3;
//
const ZERO_FLAG: u8        = 1 << 1;
const CARRY_FLAG: u8       = 1 << 0;

impl StatusReg {
    fn negative(&self) -> bool    { self.0 & NEG_FLAG != 0 }
    fn zero(&self) -> bool        { self.0 & ZERO_FLAG != 0 }
    fn direct_page(&self) -> bool { self.0 & DIRECT_PAGE_FLAG != 0 }
    fn carry(&self) -> bool       { self.0 & CARRY_FLAG != 0 }

    fn set(&mut self, flag: u8, v: bool) {
        if v {
            self.0 |= flag;
        } else {
            self.0 &= !flag;
        }
    }

    fn set_negative(&mut self, v: bool) { self.set(NEG_FLAG, v) }
    fn set_zero(&mut self, v: bool)     { self.set(ZERO_FLAG, v) }
    fn set_carry(&mut self, v: bool)    { self.set(CARRY_FLAG, v) }

    fn set_nz(&mut self, val: u8) -> u8 {
        self.set_negative(val & 0x80 != 0);
        self.set_zero(val == 0);
        val
    }
}

impl Spc700 {
    fn new() -> Spc700 {
        const MEM_SIZE: usize = 65536;
        const IPL_START: usize = MEM_SIZE - 64;

        let mut mem = vec![0; MEM_SIZE as usize];
        for i in 0..64 {
            mem[IPL_START as usize + i] = IPL_ROM[i];
        }

        let pcl = mem[RESET_VEC as usize] as u16;
        let pch = mem[RESET_VEC as usize + 1] as u16;
        let pc = (pch << 8) | pcl;

        Spc700 {
            mem: mem,
            a: 0,
            x: 0,
            y: 0,
            sp: 0,
            pc: pc,
            psw: StatusReg(0),  // FIXME is 0 correct`?
        }
    }

    fn load(&mut self, addr: u16) -> u8 {
        self.mem[addr as usize]
    }

    fn store(&mut self, addr: u16, val: u8) {
        self.mem[addr as usize] = val;
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
        trace!("{:04X}     {:14} a:{:02X} x:{:02X} y:{:02X} sp:{:02X} psw:{:08b}",
            pc,
            opstr,
            self.a,
            self.x,
            self.y,
            self.sp,
            self.psw.0,
        );
    }

    // FIXME temporary function, executes a single opcode
    pub fn dispatch(&mut self) {
        let pc = self.pc;

        macro_rules!e{($e:expr)=>($e)}
        macro_rules! instr {
            ( $name:ident $s:tt ) => {{
                self.trace_op(pc, e!($s));
                self.$name()
            }};
            ( $name:ident $s:tt $am:ident ) => {{
                use log::LogLevel::Trace;
                let am = self.$am();
                if log_enabled!(Trace) {
                    let amfmt = am.format(self);
                    self.trace_op(pc, &format!(e!($s), amfmt));
                }
                self.$name(am)
            }};
            ( $name:ident $s:tt $am:ident $am2:ident ) => {{
                use log::LogLevel::Trace;
                let am = self.$am();
                let am2 = self.$am2();
                if log_enabled!(Trace) {
                    let amfmt = am.format(self);
                    let amfmt2 = am2.format(self);
                    self.trace_op(pc, &format!(e!($s), amfmt, amfmt2));
                }
                self.$name(am, am2)
            }};
        }

        let op = self.fetchb();
        match op {
            0x1d => instr!(dec_x "dec x"),
            0x1f => instr!(bra "jmp {}" absolute_x),    // reuse `bra` fn
            0x2f => instr!(bra "bra {}" rel),
            0x5d => instr!(mov_x_a "mov x, y"),
            0x78 => instr!(cmp "cmp {}, {}" immediate direct),
            0x8f => instr!(mov_sti "mov {1}, {0}" immediate direct),
            0xba => instr!(movw_l "movw ya, {}" direct),
            0xbd => instr!(mov_sp_x "mov sp, x"),
            0xc4 => instr!(mov_lda "mov a, {}" direct),
            0xc6 => instr!(mov_sta "mov {}, a" indirect_x),
            0xcd => instr!(mov_ldx "mov x, {}" immediate),
            0xd0 => instr!(bne "bne {}" rel),
            0xda => instr!(movw_s "movw {}, ya" direct),
            0xdd => instr!(mov_a_y "mov a, y"),
            0xe8 => instr!(mov_lda "mov a, {}" immediate),
            _ => {
                instr!(ill "ill");
                panic!("illegal APU opcode: {:02X}", op);
            }
        }
    }
}

/// Opcode implementations
impl Spc700 {
    /// movw-load. Fetches a word from the addressing mode and puts it into Y and A
    fn movw_l(&mut self, am: AddressingMode) {
        let (lo, hi) = am.loadw(self);
        self.y = lo;
        self.a = hi;
        self.psw.set_nz(hi);
    }

    /// movw-store. Stores Y/A at the given word address.
    fn movw_s(&mut self, am: AddressingMode) {
        // No flags modified
        let y = self.y;
        let a = self.a;
        am.storew(self, (y, a));
    }

    fn cmp(&mut self, a: AddressingMode, b: AddressingMode) {
        // FIXME check if the order is correct
        let a = a.loadb(self);
        let b = b.loadb(self);

        let diff = a.wrapping_sub(b);
        self.psw.set_nz(diff);
        self.psw.set_carry(diff & 0x80 != 0);
    }

    fn bra(&mut self, am: AddressingMode) {
        let a = am.address(self);
        self.pc = a;
    }

    fn bne(&mut self, am: AddressingMode) {
        if !self.psw.zero() {
            let a = am.address(self);
            self.pc = a;
        }
    }

    fn dec_x(&mut self) {
        self.x = self.psw.set_nz(self.x.wrapping_sub(1));
    }

    /// Copy a value (except registers)
    fn mov_sti(&mut self, src: AddressingMode, dest: AddressingMode) {
        // No flags modified
        let val = src.loadb(self);
        dest.storeb(self, val);
    }
    /// Load A (`mov a, {X}`)
    fn mov_lda(&mut self, am: AddressingMode) {
        let val = am.loadb(self);
        self.a = self.psw.set_nz(val);
    }
    /// Load x (`mov x, {X}`)
    fn mov_ldx(&mut self, am: AddressingMode) {
        let val = am.loadb(self);
        self.x = self.psw.set_nz(val);
    }
    /// Store A wherever (`mov {X}, a`)
    fn mov_sta(&mut self, am: AddressingMode) {
        // No flags modified
        let a = self.a;
        debug!("STORE {:02X}", a);
        am.storeb(self, a);
    }
    fn mov_a_y(&mut self) {
        self.a = self.psw.set_nz(self.y);
    }
    fn mov_x_a(&mut self) {
        self.x = self.psw.set_nz(self.a);
    }
    fn mov_sp_x(&mut self) {
        // No flags modified
        self.sp = self.x;
    }
    fn ill(&mut self) {}
}


enum AddressingMode {
    /// Direct Page, uses the Direct Page status bit to determine if page 0 or 1 should be accessed
    Direct(u8),
    /// Where X points to (in page 0, $00 - $ff)
    IndirectX,
    /// Absolute address + X (`[$abcd+X]`)
    AbsoluteX(u16),
    Immediate(u8),
    /// Used for branch instructions
    Rel(i8),
}

impl AddressingMode {
    fn loadb(self, spc: &mut Spc700) -> u8 {
        match self {
            AddressingMode::Immediate(val) => val,
            _ => {
                let addr = self.address(spc);
                spc.load(addr)
            }
        }
    }

    /// Loads a word. Returns low and high byte.
    fn loadw(self, spc: &mut Spc700) -> (u8, u8) {
        let a = self.address(spc);
        let lo = spc.load(a);
        let hi = spc.load((a & 0xff00) | (a as u8 + 1) as u16); // wrap low byte XXX
        (lo, hi)
    }

    fn storeb(self, spc: &mut Spc700, value: u8) {
        let a = self.address(spc);
        spc.store(a, value);
    }

    fn storew(self, spc: &mut Spc700, (lo, hi): (u8, u8)) {
        let a = self.address(spc);
        spc.store(a, lo);
        spc.store((a & 0xff00) | (a as u8 + 1) as u16, hi); // wrap low byte XXX
    }

    fn address(&self, spc: &Spc700) -> u16 {
        match *self {
            AddressingMode::Immediate(_) => panic!("attempted to get address of immediate"),
            AddressingMode::Direct(offset) => {
                if spc.psw.direct_page() {
                    offset as u16 + 0x100
                } else {
                    offset as u16
                }
            }
            AddressingMode::IndirectX => {
                spc.x as u16
            }
            AddressingMode::AbsoluteX(abs) => {
                // XXX wrapping?
                abs + spc.x as u16
            }
            AddressingMode::Rel(rel) => {
                // FIXME wrapping is wrong here!
                (spc.pc as i32 + rel as i32) as u16
            }
        }
    }

    fn format(&self, spc: &Spc700) -> String {
        match *self {
            AddressingMode::Direct(offset) => format!("${:02X}", offset),
            AddressingMode::IndirectX => format!("(X)"),
            AddressingMode::AbsoluteX(abs) => format!("[${:04X}+X]", abs),
            AddressingMode::Immediate(val) => format!("#${:02X}", val),
            AddressingMode::Rel(rel) => format!("{:+}", rel),
        }
    }
}

/// Addressing mode construction
impl Spc700 {
    fn direct(&mut self) -> AddressingMode {
        AddressingMode::Direct(self.fetchb())
    }
    fn indirect_x(&mut self) -> AddressingMode {
        AddressingMode::IndirectX
    }
    fn absolute_x(&mut self) -> AddressingMode {
        AddressingMode::AbsoluteX(self.fetchw())
    }
    fn immediate(&mut self) -> AddressingMode {
        AddressingMode::Immediate(self.fetchb())
    }
    fn rel(&mut self) -> AddressingMode {
        AddressingMode::Rel(self.fetchb() as i8)
    }
}

const IPL_ROM: [u8; 64] = [
    // NOTE: mov operands are `dest, source`

    // Set up stack pointer at $01ef
    0xcd, 0xef,         // FFC0  mov x, #$ef
    0xbd,               // FFC2  mov sp, x
    // Fill memory at $00-$ff with $00 (also sets all registers to 0)
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

    // We're done, jump to $0000 (x is $00 here)
    0x1f, 0x00, 0x00,   // FFFB  jmp [$0000+x]

    // reset vector is at 0xfffe and points to the start of the IPL ROM: 0xffc0
    0xc0, 0xff,
];
