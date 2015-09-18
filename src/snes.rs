//! Contains the `Snes` struct, which wields the combined power of this project.

use apu::Apu;
use cpu::{Cpu, AddressSpace};
use ppu::Ppu;
use rom::Rom;

const WRAM_SIZE: usize = 128 * 1024;

/// Contains everything connected to the CPU via one of the two address buses.
struct Memory {
    apu: Apu,
    ppu: Ppu,
    rom: Rom,
    /// The 128 KB of working RAM of the SNES (separate from cartridge RAM)
    wram: Vec<u8>,
}

impl AddressSpace for Memory {
    fn load(&mut self, bank: u8, addr: u16) -> u8 {
        match bank {
            0x00 ... 0x3f => {
                match addr {
                    0x0000 ... 0x1fff => {
                        // Mirror of first 8k of WRAM
                        self.wram[addr as usize]
                    }
                    0x2140 ... 0x217f => {
                        // APU IO registers. The APU has 4 IO regs which are mirrored.
                        // 2140 => f4
                        // 2141 => f5
                        // 2142 => f6
                        // 2143 => f7
                        let reg = addr & 0b11;
                        self.apu.load(reg as u8)
                    }
                    _ => self.rom.load(bank, addr)
                }
            }
            0x7e | 0x7f => {
                // WRAM main banks
                self.wram[(bank as usize - 0x7e) * 65536 + addr as usize]
            }
            _ => self.rom.load(bank, addr)
        }
    }

    fn store(&mut self, bank: u8, addr: u16, value: u8) {
        match bank {
            0x00 ... 0x3f => {
                match addr {
                    0x0000 ... 0x1fff => {
                        self.wram[addr as usize] = value;
                    }
                    0x2100 ... 0x213f => {
                        // PPU registers. Let it deal with the access.
                        self.ppu.store(bank, addr, value)
                    }
                    0x2140 ... 0x217f => {
                        // APU IO registers. The APU has 4 IO regs which are mirrored.
                        // 2140 => f4
                        // 2141 => f5
                        // 2142 => f6
                        // 2143 => f7
                        let reg = addr & 0b11;
                        self.apu.store(reg as u8, value)
                    }
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
                    }
                    0x420c => {
                        // HDMAEN - HDMA enable
                        if value != 0 { panic!("NYI: HDMA") }
                    }
                    _ => self.rom.store(bank, addr, value)
                }
            }
            0x7e | 0x7f => {
                // WRAM main banks
                self.wram[(bank as usize - 0x7e) * 65536 + addr as usize] = value;
            }
            _ => self.rom.store(bank, addr, value)
        }
    }
}

pub struct Snes {
    cpu: Cpu<Memory>,
}

impl Snes {
    pub fn new(rom: Rom) -> Snes {
        Snes {
            cpu: Cpu::new(Memory {
                rom: rom,
                apu: Apu::new(),
                ppu: Ppu::new(),
                wram: vec![0; WRAM_SIZE],
            }),
        }
    }

    pub fn run(&mut self) {
        /// Start tracing at this opcode (0 to trace everything)
        const TRACE_START: u32 = 87000;
        /// Exit after this number of iterations
        const OP_LIMIT: u32 = 88000;

        let mut opcount: u32 = 0;

        loop {
            if opcount == TRACE_START {
                self.cpu.trace = true;
                self.cpu.mem.apu.cpu.trace = true;
            }

            self.cpu.dispatch();
            self.cpu.dispatch();
            self.cpu.dispatch();
            self.cpu.mem.apu.tick();

            opcount += 1;
            if opcount == OP_LIMIT { break }
        }

        info!("EXITING");
    }
}
