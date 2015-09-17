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
        const MEM_SIZE: u16 = !0;
        const IPL_START: u16 = MEM_SIZE - 64;
        let mut mem = vec![0; MEM_SIZE as usize];
        for i in 0..64 {
            mem[IPL_START as usize + i] = IPL_ROM[i];
        }

        Spc700 {
            mem: mem,
            a: 0,
            x: 0,
            y: 0,
            sp: 0,
            pc: RESET_VEC,
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
            _ => instr!(ill)
        }
    }
}

/// Opcode implementations
impl Spc700 {
    fn ill(&mut self) {
        panic!("illegal opcode")
    }
}

const IPL_ROM: [u8; 64] = [0; 64];

enum AddressingMode {

}

impl AddressingMode {
    fn format(&self, spc: &Spc700) -> String {
        match *self {

        }
    }
}

/// Addressing mode construction
impl Spc700 {

}
