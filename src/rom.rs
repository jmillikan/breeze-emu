//! ROM image loading code

use std::cmp;
use std::str;

/// The (decoded) SNES header
pub struct RomHeader {
    /// ASCII title, filled with spaces to 21 Bytes
    #[allow(dead_code)] // FIXME Use this or drop this
    title: [u8; 21],
    rom_size: u32,
    ram_size: u32,
    checksum: u16,
    rom_type: RomType,
}

#[derive(Debug, PartialEq, Eq)]
enum RomType {
    LoRom,
    HiRom,
}

impl RomHeader {
    /// Loads the ROM header from the given byte slice (must be exactly 64 bytes large).
    /// `rom_type` is the expected type of the ROM header, based on its location. It will be
    /// used for the actual type if the type inside the header mismatches. This is apparently
    /// required for some broken ROMs.
    fn load(bytes: &[u8], rom_type: RomType) -> Result<RomHeader, ()> {
        // The header size must be correct (the ROM loader won't pass a wrong size)
        assert_eq!(bytes.len(), 64);

        debug!("raw rom header: {:?}", bytes);

        // First check: Is the title valid ASCII?
        let mut title = [0; 21];
        for (i, c) in bytes[0..21].iter().enumerate() {
            match *c {
                0x20 ... 0x7E => {
                    title[i] = *c;
                }
                _ => {
                    debug!("title contains non-ascii bytes");
                    return Err(())
                }
            }
        }

        info!("loading '{}'", str::from_utf8(&title).unwrap().trim_right());

        // 21) ROM makeup byte
        // `---smmmm`
        // * `s`: Speed (0: SlowROM, 1: FastROM)
        // * `m`: Map mode
        //  * `0000`: LoROM
        //  * `0001`: HiROM
        //  * `0010`: LoROM + S-DD1
        //  * `0011`: LoROM + SA-1
        //  * `0101`: ExHiROM
        //  * `1010`: HiROM + SPC7110
        if bytes[21] & 0x10 == 0x10 {
            warn!("FastROM not yet implemented!");
        }

        let header_rom_type = match bytes[21] & 0x0f {
            0 => RomType::LoRom,
            1 => RomType::HiRom,
            t => {
                error!("unknown / unimplemented ROM type {}", t);
                return Err(())
            }
        };

        if header_rom_type == rom_type {
            info!("type: {:?}", rom_type);
        } else {
            warn!("expected rom type {:?}, got {:?} (assuming {:?})", rom_type, header_rom_type,
                rom_type);
        }

        // bytes[22] is the chipset info. For now, we don't care about that.
        debug!("chipset: 0x{:02X}", bytes[22]);

        let rom_size = 0x400 << bytes[23] as u32;
        let ram_size = 0x400 << bytes[24] as u32;
        debug!("ROM/RAM size values: {:02X} {:02X}", bytes[23], bytes[24]);
        info!("{} KB of ROM, {} KB of cartridge RAM", rom_size / 1024, ram_size / 1024);

        // bytes[25-26] is a vendor code (doesn't matter)
        debug!("vendor code: 0x{:02X}{:02X}", bytes[25], bytes[26]);
        // 27 = version (also doesn't matter for us)
        debug!("version: 0x{:02X}", bytes[27]);

        // Now it's getting a bit more interesting: The next byte is the checksum's complement
        // followed by the checksum itself. The checksum is the sum of all bytes in the ROM
        // (truncated to 16 bits). It can be validated by the loader.
        // 16 bit, little-endian.
        let check_inv = (bytes[29] as u16) << 8 | bytes[28] as u16;
        let checksum = (bytes[31] as u16) << 8 | bytes[30] as u16;
        if check_inv != !checksum {
            error!("checksum invalid: stored complement is {:04X}, stored checksum is {:04X}",
                check_inv, checksum);
            return Err(())
        }

        Ok(RomHeader {
            title: title,
            rom_size: rom_size,
            ram_size: ram_size,
            checksum: checksum,
            rom_type: rom_type,
        })
    }
}

/// A ROM image
pub struct Rom {
    header: RomHeader,
    ram: Vec<u8>,
    rom: Vec<u8>,
}

impl_save_state!(Rom { ram } ignore { header, rom });

impl Rom {
    /// Loads a ROM from raw data.
    pub fn from_bytes(mut bytes: &[u8]) -> Result<Rom, ()> {
        // FIXME Return a proper error!

        debug!("raw size: {} bytes ({:#X})", bytes.len(), bytes.len());

        // ROMs may begin with a 512 Bytes SMC header. It needs to go.
        match bytes.len() % 1024 {
            512 => {
                debug!("stripping SMC header");
                bytes = &bytes[512..];
            }
            0 => {},
            n => panic!("len() % 1024 == {} (expected 512 or 0)", n),
        }

        // Read the SNES header. It is located in a really stupid location which depends on whether
        // the ROM is HiROM or LoROM, so we must check both locations and pick the right one.
        // LoROM header is at 0x7FFF - 63, HiROM is at 0xFFFF - 63 (it is 64 Bytes large)

        // Test LoROM first (it's more common)
        let header = try!(RomHeader::load(&bytes[0x7FFF - 63..0x7FFF + 1], RomType::LoRom)
            .or_else(|_| RomHeader::load(&bytes[0xFFFF - 63..0xFFFF + 1], RomType::HiRom)));

        if bytes.len() != header.rom_size as usize {
            warn!("raw ROM is {} KB, but header specifies {} KB",
                bytes.len() / 1024, header.rom_size / 1024);
        }

        // Create the right amount of RAM...
        let ram = vec![0; header.ram_size as usize];
        // ...and copy the ROM
        let rom = bytes.iter().cloned().cycle()
            .take(cmp::max(header.rom_size as usize, bytes.len())).collect();

        // Calculate the ROM's checksum
        let mut checksum: u16 = 0;
        for &byte in &rom {
            checksum = checksum.wrapping_add(byte as u16);
        }

        debug!("computed checksum: ${:04X}", checksum);

        if header.checksum != checksum {
            warn!("incorrect checksum: computed ${:04X}, expected ${:04X}",
                checksum, header.checksum);
        }

        Ok(Rom {
            header: header,
            ram: ram,
            rom: rom,
        })
    }

    fn resolve_lorom(&mut self, bank: u8, addr: u16) -> &mut u8 {
        match addr {
            0x0000 ... 0x7fff => {
                // Cartridge RAM mapped to the low 32 KB
                // (there's other stuff here, but that's handled much earlier than we are called)
                match bank {
                    0x70 ... 0x7d => {
                        let a = (bank as u32 - 0x70) * 0x8000 + addr as u32;
                        self.ram.get_mut(a as usize).unwrap_or_else(|| out_of_ram_bounds(bank, addr, a))
                    }
                    0xfe ... 0xff => {
                        // last 64k of RAM
                        let start = self.ram.len() as u32 - 64 * 1024;
                        let a = start + (bank - 0xfe) as u32 * 0x8000 + addr as u32;
                        self.ram.get_mut(a as usize).unwrap_or_else(|| out_of_ram_bounds(bank, addr, a))
                    }
                    _ => {
                        // 0x40 ... 0x6f | 0x7e ... 0xfd
                        panic!("attempted to access unmapped address: ${:02X}:{:04X}", bank, addr);
                    }
                }
            },
            0x8000 ... 0xffff => match bank {
                // LoROM is mapped to the higher 8 pages
                0xfe => {
                    let a = 0x3f0000 + addr as u32 - 0x8000;
                    self.rom.get_mut(a as usize).unwrap_or_else(|| out_of_rom_bounds(bank, addr, a))
                }
                0xff => {
                    let a = 0x3f8000 + addr as u32 - 0x8000;
                    self.rom.get_mut(a as usize).unwrap_or_else(|| out_of_rom_bounds(bank, addr, a))
                }
                0x80 ... 0xfd | 0x00 ... 0x7d => {
                    // `& !0x80` because 0x80-0xFD mirrors 0x00-0x7D
                    let a = (bank as u32 & !0x80) * 0x8000 + addr as u32 - 0x8000;
                    self.rom.get_mut(a as usize).unwrap_or_else(|| out_of_rom_bounds(bank, addr, a))
                }
                _ => panic!("attempted to access unmapped address: ${:02X}:{:04X}", bank, addr)
            },
            _ => unreachable!()
        }
    }

    fn resolve_hirom(&mut self, bank: u8, addr: u16) -> &mut u8 {
        let addr = addr as usize;
        match bank {
            0x00 ... 0x3f | 0x80 ... 0xbf if addr >= 0x8000 => {
                &mut self.rom[(bank as usize & 0x3f) << 16 | addr]
            }
            0x20 ... 0x3f | 0xa0 ... 0xbf if addr >= 0x6000 && addr <= 0x7fff => {
                let a = ((bank as usize & 0x3f) - 0x20) * 0x2000 + (addr - 0x6000);
                &mut self.ram[a]
            }
            0x40 ... 0x7d | 0xc0 ... 0xfd => {
                &mut self.rom[((bank as usize & 0x7f) - 0x40) << 16 | addr]
            }
            0x7e ... 0x7f => unreachable!(),    // WRAM banks
            0xfe ... 0xff => {
                &mut self.rom[(bank as usize - 0xfe + 0x3e) << 16 | addr]
            }
            _ => panic!("attempted to access unmapped address: ${:02}:{:04X}", bank, addr),
        }
    }

    fn resolve_addr(&mut self, bank: u8, addr: u16) -> &mut u8 {
        match self.header.rom_type {
            RomType::LoRom => self.resolve_lorom(bank, addr),
            RomType::HiRom => self.resolve_hirom(bank, addr),
        }
    }
}

impl Rom {
    pub fn load(&mut self, bank: u8, addr: u16) -> u8 {
        *self.resolve_addr(bank, addr)
    }

    pub fn store(&mut self, bank: u8, addr: u16, value: u8) {
        if addr >= 0x8000 {
            warn!("writing ${:02X} to ROM address ${:02X}:{:04X}", value, bank, addr);
        }
        *self.resolve_addr(bank, addr) = value;
    }
}

fn out_of_ram_bounds(bank: u8, addr: u16, abs: u32) -> ! {
    panic!("RAM access out of bounds at {:02X}:{:04X} -> {:04X}",
        bank, addr, abs)
}

fn out_of_rom_bounds(bank: u8, addr: u16, abs: u32) -> ! {
    panic!("ROM access out of bounds at {:02X}:{:04X} -> {:06X}",
        bank, addr, abs)
}
