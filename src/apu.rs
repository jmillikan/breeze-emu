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
        const MEM_SIZE: u16 = 64*1000;
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
        panic!()
    }

    fn store(&mut self, addr: u16, val: u8) {
        panic!()
    }

    // FIXME temporary function
    fn run(&mut self) {

    }
}

const IPL_ROM: [u8; 64] = [0; 64];
