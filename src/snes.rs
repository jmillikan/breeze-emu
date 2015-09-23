//! Contains the `Snes` struct, which wields the combined power of this project.

use apu::Apu;
use cpu::Cpu;
use dma::DmaChannel;
use ppu::Ppu;
use rom::Rom;

const WRAM_SIZE: usize = 128 * 1024;

/// Contains everything connected to the CPU via one of the two address buses. All memory accesses
/// will be directed through this (the CPU already takes access time into account).
pub struct Peripherals {
    apu: Apu,
    ppu: Ppu,
    rom: Rom,
    /// The 128 KB of working RAM of the SNES (separate from cartridge RAM)
    wram: Vec<u8>,

    dma: [DmaChannel; 8],
    /// $420b - MDMAEN - Enable-bits of general-purpose DMA channels. Writing with any value that
    /// contains a set bit will start a DMA transfer immediately.
    dmaen: u8,
    hdmaen: u8,

    /// Additional cycles spent doing IO (in master clock cycles). This is reset before each CPU
    /// instruction and added to the cycle count returned by the CPU.
    cy: u16,
}

impl Peripherals {
    pub fn new(rom: Rom) -> Peripherals {
        Peripherals {
            rom: rom,
            apu: Apu::new(),
            ppu: Ppu::new(),
            dma: [DmaChannel::new(); 8],
            dmaen: 0x00,
            hdmaen: 0x00,
            wram: vec![0; WRAM_SIZE],
            cy: 0,
        }
    }

    pub fn load(&mut self, bank: u8, addr: u16) -> u8 {
        match bank {
            0x00 ... 0x3f => match addr {
                // Mirror of first 8k of WRAM
                0x0000 ... 0x1fff => self.wram[addr as usize],
                // PPU
                0x2100 ... 0x2133 => panic!("read from write-only PPU register ${:04X}", addr),
                0x2138 ... 0x213f => self.ppu.load(addr),
                // APU IO registers
                0x2140 ... 0x217f => self.apu.read_port((addr & 0b11) as u8),
                // DMA channels (0x43xr, where x is the channel and r is the channel register)
                0x4300 ... 0x43ff => self.dma[(addr as usize & 0x00f0) >> 4].load(addr as u8 & 0xf),
                _ => self.rom.loadb(bank, addr)
            },
            // WRAM banks. The first 8k are mapped into the start of all banks.
            0x7e | 0x7f => self.wram[(bank as usize - 0x7e) * 65536 + addr as usize],
            _ => self.rom.loadb(bank, addr)
        }
    }

    pub fn store(&mut self, bank: u8, addr: u16, value: u8) {
        match bank {
            0x00 ... 0x3f | 0x80 ... 0xbf => match addr {
                0x0000 ... 0x1fff => self.wram[addr as usize] = value,
                // PPU registers. Let it deal with the access.
                0x2100 ... 0x2133 => self.ppu.store(addr, value),
                0x2138 ... 0x213f => panic!("store to read-only PPU register ${:04X}", addr),
                // APU IO registers.
                0x2140 ... 0x217f => self.apu.store_port((addr & 0b11) as u8, value),
                0x2180 ... 0x2183 => panic!("NYI: WRAM registers"),
                0x4200 => {
                    // NMITIMEN - NMI/IRQ enable
                    // E-HV---J
                    // E: Enable NMI
                    // H: Enable IRQ on H-Counter
                    // V: Enable IRQ on V-Counter
                    // J: Enable Auto-Joypad-Read
                    if value & 0x80 != 0 { panic!("NYI: NMI") }
                    if value & 0x20 != 0 { panic!("NYI: IRQ-H") }
                    if value & 0x10 != 0 { panic!("NYI: IRQ-V") }
                    if value & 0x01 != 0 { panic!("NYI: Auto-Joypad-Read") }
                }
                0x420b => {
                    // MDMAEN - Party enable
                    if value != 0 { panic!("NYI: DMA") }
                    self.dmaen = value;
                }
                0x420c => {
                    // HDMAEN - HDMA enable
                    if value != 0 { panic!("NYI: HDMA") }
                    self.hdmaen = value;
                }
                // DMA channels (0x43xr, where x is the channel and r is the channel register)
                0x4300 ... 0x43ff =>
                    self.dma[(addr as usize & 0x00f0) >> 4].store(addr as u8 & 0xf, value),
                _ => self.rom.storeb(bank, addr, value)
            },
            // WRAM main banks
            0x7e | 0x7f => self.wram[(bank as usize - 0x7e) * 65536 + addr as usize] = value,
            _ => self.rom.storeb(bank, addr, value)
        }
    }
}

pub struct Snes {
    cpu: Cpu,
}

impl Snes {
    pub fn new(rom: Rom) -> Snes {
        Snes {
            cpu: Cpu::new(Peripherals::new(rom)),
        }
    }

    pub fn run(&mut self) {
        /// Exit after this number of master clock cycles
        const CY_LIMIT: u64 = 30_000_000;
        /// Start tracing at this master cycle (0 to trace everything)
        const TRACE_START: u64 = CY_LIMIT - 3_000;

        const MASTER_CLOCK_FREQ: i32 = 21_477_000;
        /// APU clock speed. On real hardware, this can vary quite a bit (I think it uses a ceramic
        /// resonator instead of a quartz).
        const APU_CLOCK_FREQ: i32 = 1_024_000;
        /// Approximated APU clock divider. It's actually somewhere around 20.9..., which is why we
        /// can't directly use `MASTER_CLOCK_FREQ / APU_CLOCK_FREQ` (it would round down, which
        /// might not be critical, but better safe than sorry).
        const APU_DIVIDER: i32 = 21;

        // Master cycle counter, used only for debugging atm
        let mut master_cy: u64 = 0;
        // Master clock cycles for the APU not yet accounted for (can be negative)
        let mut apu_master_cy_debt = 0;
        let mut ppu_master_cy_debt = 0;

        while master_cy < CY_LIMIT {
            if master_cy >= TRACE_START {
                self.cpu.trace = true;
                self.cpu.mem.apu.trace = true;
            }

            // Run a CPU instruction and calculate the master cycles elapsed
            self.cpu.mem.cy = 0;
            let cpu_master_cy = self.cpu.dispatch() as i32 + self.cpu.mem.cy as i32;
            master_cy += cpu_master_cy as u64;

            // Now we "owe" the other components a few cycles:
            apu_master_cy_debt += cpu_master_cy;
            ppu_master_cy_debt += cpu_master_cy;

            // Run all components until we no longer owe them:
            while apu_master_cy_debt > 0 {
                apu_master_cy_debt -= self.cpu.mem.apu.dispatch() as i32 * APU_DIVIDER;
            }
            while ppu_master_cy_debt > 0 {
                let (cy, result) = self.cpu.mem.ppu.update();
                ppu_master_cy_debt -= cy as i32;

                if result.hblank {
                    // TODO Do HDMA
                }
                if result.vblank {
                    // TODO Raise an NMI
                }
            }
        }

        info!("EXITING");
    }
}
