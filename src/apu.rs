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
        warn!("NYI: APU IO register store (port {}, value ${:02X})", port, value);
    }

    /// Load a byte from an IO port (0-3)
    ///
    /// IO ports are mapped to internal registers 0xf4 - 0xf7
    pub fn load(&mut self, port: u8) -> u8 {
        warn!("NYI: APU IO register load (port {}); 0 will be returned", port);
        0
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
    fn negative(&self) -> bool { self.0 & NEG_FLAG != 0 }
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

    fn trace_op(&self, pc: u16, op: &str, am: Option<&AddressingMode>) {
        trace!("{:04X}     {} {:10} a:{:02X} x:{:02X} y:{:02X} sp:{:02X} psw:{:08b}",
            pc,
            op,
            am.map(|am| am.format(self)).unwrap_or(String::new()),
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

        let op = self.fetchb();
        match op {
            _ => {
                instr!(ill);
                panic!("illegal opcode: {:02X}", op);
            }
        }
    }
}

/// Opcode implementations
impl Spc700 {
    fn ill(&mut self) {}
}


enum AddressingMode {
    /// Direct Page, uses the Direct Page status bit to determine if page 0 or 1 should be accessed
    Direct(u8),
}

impl AddressingMode {
    fn format(&self, spc: &Spc700) -> String {
        match *self {
            AddressingMode::Direct(offset) => format!("${:02}", offset),
        }
    }
}

/// Addressing mode construction
impl Spc700 {
    fn direct(&mut self) -> AddressingMode {
        AddressingMode::Direct(self.fetchb())
    }
}

const IPL_ROM: [u8; 64] = [
    // NOTE: mov operands are `dest, source`

    // Set up stack pointer at $01ef
    0xcd, 0xef,         // mov x, #$ef
    0xbd,               // mov sp, x
    // Fill the stack with $00
    0xe8, 0x00,         // mov a, #$00
    0xc6,               // mov (x), a       :fill_stack
    0x1d,               // dec x
    0xd0, 0xfc,         // bne $0ba0        -> fill_stack

    // Write 0xaabb to ports 0 and 1 (registers $f4 and $f5)
    // This is the "ready" signal. The main CPU will wait until it sees it.
    0x8f, 0xaa, 0xf4,   // mov $f4, #$aa
    0x8f, 0xbb, 0xf5,   // mov $f5, #$bb

    // Wait until $cc is written to port 0 (reg $f4)
    0x78, 0xcc, 0xf4,   // cmp #$cc, #$f4    :wait_start
    0xd0, 0xfb,         // bne $0baa        -> wait_start

    0x2f, 0x19,         // bra $0bca        -> lbl0

    0xeb, 0xf4,         // mov y, $f4
    0xd0, 0xfc,         // bne $0bb1
    0x7e, 0xf4,         // cmp y, $f4
    0xd0, 0x0b,         // bne $0bc4
    0xe4, 0xf5,         // mov a, $f5
    0xcb, 0xf4,         // mov $f4, y
    0xd7, 0x00,         // mov [$00]+y, a
    0xfc,               // inc y
    0xd0, 0xf3,         // bne $0bb5
    0xab, 0x01,         // inc $01
    0x10, 0xef,         // bpl $0bb5
    0x7e, 0xf4,         // cmp y, $f4
    0x10, 0xeb,         // bpl $0bb5

    0xba, 0xf6,         // movw ya, $f6      :lbl0
    0xda, 0x00,         // movw $00, ya
    0xba, 0xf4,         // movw ya, $f4
    0xc4, 0xf4,         // mov $f4, a
    0xdd,               // mov a, y
    0x5d,               // mov x, a
    0xd0, 0xdb,         // bne $0bb1
    0x1f, 0x00, 0x00,   // jmp [$0000]+x

    // reset vector is at 0xfffe and points to the start of the IPL ROM: 0xffc0
    0xc0, 0xff,
];
