//! Contains the `Snes` struct, which wields the combined power of this project.

use apu::Apu;
use cpu::{Cpu, AddressSpace};
use ppu::Ppu;
use rom::Rom;

/// Contains everything connected to the CPU via one of the two address buses.
struct Memory {
    apu: Apu,
    ppu: Ppu,
    rom: Rom,
}

impl AddressSpace for Memory {
    fn load(&mut self, bank: u8, addr: u16) -> u8 {
        if bank == 0 {
            // FIXME are internal regs only in bank 0?
            match addr {
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
        } else {
            self.rom.load(bank, addr)
        }
    }

    fn store(&mut self, bank: u8, addr: u16, value: u8) {
        if bank == 0 {
            // FIXME are internal regs only in bank 0?
            match addr {
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
                    // E--HV---J
                    // E: Enable NMI
                    // H: Enable IRQ on H-Counter
                    // V: Enable IRQ on V-Counter
                    // J: Enable Auto-Joypad-Read
                    if value & 0x80 != 0 { panic!("NYI: NMI") }
                    if value & 0x10 != 0 { panic!("NYI: IRQ-H") }
                    if value & 0x08 != 0 { panic!("NYI: IRQ-V") }
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
        } else {
            self.rom.store(bank, addr, value)
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
            }),
        }
    }

    pub fn run(&mut self) {
        // this counts down until 0 and then exits
        // backstory: powershell isn't able to Ctrl+C the emulator once it runs. (i'm serious)
        let mut opcount = 1000;

        while opcount > 0 {
            self.cpu.dispatch();
            self.cpu.mem.apu.tick();

            opcount -= 1;
        }

        info!("EXITING");
    }
}
