//! Contains the `Snes` struct, which wields the combined power of this project.

use apu::Apu;
use cpu::Cpu;
use dma::*;
use frontend::{FrontendAction, Renderer};
use input::Input;
use log_util::LogOnPanic;
use ppu::Ppu;
use rom::Rom;
use savestate::SaveState;

use std::env;
use std::fs::File;

const WRAM_SIZE: usize = 128 * 1024;
byte_array!(Wram[WRAM_SIZE]);

/// Contains everything connected to the CPU via one of the two address buses. All memory accesses
/// will be directed through this (the CPU already takes access time into account).
pub struct Peripherals {
    apu: Apu,
    ppu: Ppu,
    rom: Rom,
    /// The 128 KB of working RAM of the SNES (separate from cartridge RAM)
    wram: Wram,
    input: Input,

    pub dma: [DmaChannel; 8],
    /// `$420c` - HDMAEN: HDMA enable flags
    /// (Note that general DMA doesn't have a register here, since all transactions are started
    /// immediately and the register can't be read)
    hdmaen: u8,
    /// `$4200` - NMITIMEN: Interrupt enable flags
    /// `n-xy---a`
    /// * `n`: Enable NMI on V-Blank
    /// * `x`: Enable IRQ on H-Counter match
    /// * `y`: Enable IRQ on V-Counter match
    /// * `a`: Enable auto-joypad read
    nmien: u8,
    /// `$4202` - WRMPYA: Multiplicand 1
    wrmpya: u8,
    /// `$4204`/`$4205` - WRDIVH/WRDIVL: Dividend
    wrdiv: u16,
    /// `$4216`/`$4217` - RDDIVH/RDDIVL: Unsigned Division Result (Quotient)
    rddiv: u16,
    /// `$4216`/`$4217` - RDMPYH/RDMPYL: Unsigned Division Remainder / Multiply Product
    rdmpy: u16,
    /// `$4207`/`$4208` - HTIMEL/HTIMEH: H Timer (9-bit value)
    htime: u16,
    /// `$4209`/`$420a` - VTIMEL/VTIMEH: V Timer (9-bit value)
    vtime: u16,
    /// `$4210` NMI flag and 5A22 Version (the version is constant)
    /// `n---vvvv`
    /// * `n`: `self.nmi`
    /// * `v`: Version
    nmi: bool,
    /// `$4211` TIMEUP - IRQ flag
    /// `i-------`
    /// * `i`: IRQ flag (cleared on read)
    irq: bool,

    /// Additional cycles spent doing IO (in master clock cycles). This is added to the cycle count
    /// returned by the CPU and then reset to 0.
    cy: u32,
}

impl_save_state!(Peripherals {
    apu, ppu, rom, wram, dma, hdmaen, nmien, wrmpya, wrdiv, rddiv, rdmpy, htime, vtime, nmi, irq, cy
} ignore {
    input
});

impl Peripherals {
    pub fn new(rom: Rom, input: Input) -> Peripherals {
        Peripherals {
            rom: rom,
            apu: Apu::default(),
            ppu: Ppu::default(),
            wram: Wram::default(),
            input: input,
            dma: [DmaChannel::default(); 8],
            hdmaen: 0x00,
            nmien: 0x00,
            wrmpya: 0,
            wrdiv: 0xffff,
            rddiv: 0,
            rdmpy: 0,
            htime: 0x1ff,
            vtime: 0x1ff,
            nmi: false,
            irq: false,
            cy: 0,
        }
    }

    pub fn load(&mut self, bank: u8, addr: u16) -> u8 {
        match bank {
            0x00 ... 0x3f | 0x80 ... 0xbf => match addr {
                // Mirror of first 8k of WRAM
                0x0000 ... 0x1fff => self.wram[addr as usize],
                // PPU
                0x2100 ... 0x2133 => panic!("read from write-only PPU register ${:04X}", addr),
                0x2134 ... 0x213f => self.ppu.load(addr),
                // APU IO registers
                0x2140 ... 0x217f => self.apu.read_port((addr & 0b11) as u8),
                0x4016 | 0x4017 => self.input.load(addr),
                0x4210 => {
                    const CPU_VERSION: u8 = 2;  // FIXME Is 2 okay in all cases? Does anyone care?
                    let nmi = if self.nmi { 0x80 } else { 0 };
                    self.nmi = false;   // Cleared on read
                    nmi | CPU_VERSION
                }
                0x4211 => {
                    let val = if self.irq { 0x80 } else { 0 };
                    self.irq = false;
                    val
                }
                // HVBJOY - PPU Status
                0x4212 => {
                    // `vh-----a`
                    // V-Blank, H-Blank, Auto-Joypad-Read in progress
                    // FIXME: Use exact timings and set `a`
                    (if self.ppu.in_v_blank() { 0x80 } else { 0 }) +
                    (if self.ppu.in_h_blank() { 0x40 } else { 0 })
                }
                // RDDIVL - Unsigned Division Result (Quotient) (lower 8bit)
                0x4214 => self.rddiv as u8,
                // RDDIVH - Unsigned Division Result (Quotient) (upper 8bit)
                0x4215 => (self.rddiv >> 8) as u8,
                // RDMPYL
                0x4216 => self.rdmpy as u8,
                // RDMPYH
                0x4217 => (self.rdmpy >> 8) as u8,
                // Input ports
                0x4218 ... 0x421f => self.input.load(addr),
                // DMA channels (0x43xr, where x is the channel and r is the channel register)
                0x4300 ... 0x43ff => self.dma[(addr as usize & 0x00f0) >> 4].load(addr as u8 & 0xf),
                0x8000 ... 0xffff => self.rom.load(bank, addr),
                _ => panic!("invalid/unimplemented load from ${:02X}:{:04X}", bank, addr)
            },
            // WRAM banks. The first 8k are mapped into the start of all banks.
            0x7e | 0x7f => self.wram[(bank as usize - 0x7e) * 65536 + addr as usize],
            0x40 ... 0x7d | 0xc0 ... 0xff => self.rom.load(bank, addr),
            _ => unreachable!(),    // Rust should know this!
        }
    }

    pub fn store(&mut self, bank: u8, addr: u16, value: u8) {
        match bank {
            0x00 ... 0x3f | 0x80 ... 0xbf => match addr {
                0x0000 ... 0x1fff => self.wram[addr as usize] = value,
                // PPU registers. Let it deal with the access.
                0x2100 ... 0x2133 => self.ppu.store(addr, value),
                0x2134 ... 0x2137 => warn!("invalid store: ${:02X} to ${:02X}:{:04X}", value, bank,
                    addr),
                0x2138 ... 0x213f => warn!("store to read-only PPU register ${:04X}", addr),
                // APU IO registers.
                0x2140 ... 0x217f => self.apu.store_port((addr & 0b11) as u8, value),
                0x2180 ... 0x2183 => once!(warn!("NYI: WRAM registers")),
                0x2184 ... 0x21ff => warn!("invalid store: ${:02X} to ${:02X}:{:04X}", value, bank,
                    addr),
                0x4016 => self.input.store(addr, value),
                0x4200 => {
                    // NMITIMEN - NMI/IRQ enable
                    // E-HV---J
                    // E: Enable NMI
                    // H: Enable IRQ on H-Counter
                    // V: Enable IRQ on V-Counter
                    // J: Enable Auto-Joypad-Read

                    // Check useless bits
                    if value & 0x4e != 0 { panic!("Invalid value for NMIEN: ${:02X}", value) }
                    self.nmien = value;
                }
                0x4202 => self.wrmpya = value,
                // WRMPYB: Performs multiplication on write
                0x4203 => self.rdmpy = self.wrmpya as u16 * value as u16,
                0x4204 => self.wrdiv = (self.wrdiv & 0xff00) | value as u16,
                0x4205 => self.wrdiv = ((value as u16) << 8) | (self.wrdiv & 0xff),
                // WRDIVB: Performs division on write
                0x4206 => {
                    self.rddiv = if value == 0 { 0xffff } else { self.wrdiv / value as u16 };
                    self.rdmpy = if value == 0 { value as u16 } else { self.wrdiv % value as u16 };
                }
                0x4207 => self.htime = (self.htime & 0xff00) | value as u16,
                0x4208 => {
                    assert!(value & 0x01 == value, "invalid value for $4207: ${:02X}", value);
                    self.htime = ((value as u16) << 8) | (self.htime & 0xff);
                }
                0x4209 => self.vtime = (self.vtime & 0xff00) | value as u16,
                0x420a => {
                    assert!(value & 0x01 == value, "invalid value for $4209: ${:02X}", value);
                    self.vtime = ((value as u16) << 8) | (self.vtime & 0xff);
                }
                // MDMAEN - Party enable
                0x420b => self.cy += do_dma(self, value),
                // HDMAEN - HDMA enable
                0x420c => self.hdmaen = value,
                // DMA channels (0x43xr, where x is the channel and r is the channel register)
                0x4300 ... 0x43ff => {
                    self.dma[(addr as usize & 0x00f0) >> 4].store(addr as u8 & 0xf, value);
                }
                0x8000 ... 0xffff => self.rom.store(bank, addr, value),
                _ => panic!("invalid store: ${:02X} to ${:02X}:{:04X}", value, bank, addr)
            },
            // WRAM main banks
            0x7e | 0x7f => self.wram[(bank as usize - 0x7e) * 65536 + addr as usize] = value,
            0x40 ... 0x7d | 0xc0 ... 0xff => self.rom.store(bank, addr, value),
            _ => unreachable!(),    // Rust should know this!
        }
    }

    fn nmi_enabled(&self) -> bool { self.nmien & 0x80 != 0 }
    fn v_irq_enabled(&self) -> bool { self.nmien & 0x10 != 0 }
    fn h_irq_enabled(&self) -> bool { self.nmien & 0x20 != 0 }
}

pub struct Snes {
    cpu: Cpu,
    renderer: Box<Renderer>,
}

impl_save_state!(Snes { cpu } ignore { renderer });

impl Snes {
    pub fn new(rom: Rom, renderer: Box<Renderer>) -> Snes {
        // FIXME Temporary hack to have any input at all working. Replace with autodetection.
        #[cfg(feature = "sdl2")]
        fn attach_default_input(input: &mut Input) {
            input.sources[0] = Some(Box::new(::frontend::sdl::KeyboardInput))
        }
        #[cfg(not(feature = "sdl2"))]
        fn attach_default_input(_: &mut Input) {}

        let mut input = Input::default();
        attach_default_input(&mut input);

        Snes {
            cpu: Cpu::new(Peripherals::new(rom, input)),
            renderer: renderer,
        }
    }

    /// Handles a `FrontendAction`. Returns `true` if the emulator should exit.
    fn handle_action(&mut self, action: FrontendAction) -> bool {
        match action {
            FrontendAction::Exit => return true,
            FrontendAction::SaveState => {
                let mut file = File::create("sneeze.sav").unwrap();
                self.save_state(&mut file).unwrap();
            }
            FrontendAction::LoadState => {
                let mut file = File::open("sneeze.sav").unwrap();
                self.restore_state(&mut file).unwrap();
            }
        }

        false
    }

    pub fn run(&mut self) {
        // Start tracing at this master cycle (`!0` by default, which practically disables tracing)
        let trace_start = env::var("SNEEZE_TRACE")
            .map(|string| string.parse().expect("invalid value for SNEEZE_TRACE"))
            .unwrap_or(!0);

        /// Approximated APU clock divider. It's actually somewhere around 20.9..., which is why we
        /// can't directly use `MASTER_CLOCK_FREQ / APU_CLOCK_FREQ` (it would round down, which
        /// might not be critical, but better safe than sorry).
        const APU_DIVIDER: i32 = 21;

        // Master cycle counter, used only for debugging atm
        let mut master_cy: u64 = 0;
        // Master clock cycles for the APU not yet accounted for (can be negative)
        let mut apu_master_cy_debt = 0;
        let mut ppu_master_cy_debt = 0;
        let working_cy = LogOnPanic::new("cycle count", 0);

        loop {
            if master_cy >= trace_start {
                self.cpu.trace = true;
                self.cpu.mem.apu.trace = true;
            }

            // Run a CPU instruction and calculate the master cycles elapsed
            let cpu_master_cy = self.cpu.dispatch() as i32 + self.cpu.mem.cy as i32;
            self.cpu.mem.cy = 0;
            master_cy += cpu_master_cy as u64;

            // Now we "owe" the other components a few cycles:
            apu_master_cy_debt += cpu_master_cy;
            ppu_master_cy_debt += cpu_master_cy;

            // Run all components until we no longer owe them:
            while apu_master_cy_debt > APU_DIVIDER {
                // (Since the APU uses lots of cycles to do stuff - lower clock rate and such - we
                // only run it if we owe it `APU_DIVIDER` master cycles - or one SPC700 cycle)
                let apu_master_cy = self.cpu.mem.apu.dispatch() as i32 * APU_DIVIDER;
                apu_master_cy_debt -= apu_master_cy;
            }
            while ppu_master_cy_debt > 0 {
                let cy = self.cpu.mem.ppu.update();
                ppu_master_cy_debt -= cy as i32;

                let (v, h) = (self.cpu.mem.ppu.v_counter(), self.cpu.mem.ppu.h_counter());
                match (v, h) {
                    (0, 0) => self.cpu.mem.nmi = false,
                    (0, 6) => {
                        self.cpu.mem.cy += init_hdma(&mut self.cpu.mem.dma, self.cpu.mem.hdmaen)
                    }
                    (0 ... 224, 278) => {
                        // FIXME: 224 or 239, depending on overscan
                        self.cpu.mem.cy += do_hdma(self.cpu.mem.hdmaen);
                    }
                    (224, 256) => {
                        // Last pixel in the current frame was rendered
                        if let Some(action) = self.renderer.render(&*self.cpu.mem.ppu.framebuf) {
                            if self.handle_action(action) { return }
                        }
                    }
                    (225, 0) => {
                        // V-Blank
                        self.cpu.mem.input.new_frame();
                        if self.cpu.mem.nmi_enabled() {
                            self.cpu.mem.nmi = true;
                            self.cpu.trigger_nmi();
                            // XXX Break to handle the NMI immediately. Let's hope we don't owe the PPU
                            // too many cycles.
                            break;
                        }
                    }
                    _ => {}
                }

                let cpu = &mut self.cpu;
                if cpu.mem.ppu.v_counter() == cpu.mem.vtime && cpu.mem.v_irq_enabled() {
                    cpu.mem.irq = true;
                    cpu.trigger_irq();
                    break;
                }
                if cpu.mem.ppu.h_counter() == cpu.mem.htime && cpu.mem.h_irq_enabled() {
                    cpu.mem.irq = true;
                    cpu.trigger_irq();
                    break;
                }
            }

            working_cy.set(master_cy);
        }
    }
}
