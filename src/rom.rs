//! ROM image loading code

use std::str;

use cpu::AddressSpace;

/// The (decoded) SNES header
#[derive(Debug)]
pub struct RomHeader {
    /// ASCII title
    title: [u8; 21],
    rom_size: u32,
    ram_size: u32,

    // (type is ignored, FastROM and HiROM are not yet supported)
}

impl RomHeader {
    /// Loads the ROM header from the given byte slice (must be exactly 64 bytes large). `checksum`
    /// is the sum of all bytes in the ROM image (truncated to a `u16`).
    fn load(bytes: &[u8], checksum: u16) -> Result<RomHeader, ()> {
        // The header size must be correct (the ROM loader won't pass a wrong size)
        assert_eq!(bytes.len(), 64);

        debug!("{:?}", bytes);

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

        debug!("'{}'", str::from_utf8(&title).unwrap());

        // "ROM makeup byte"
        let makeup = bytes[21];
        if makeup & 0b00110000 == 0b00110000 {
            error!("FastROM unsupported!");
            return Err(())
        }

        if makeup & 1 == 1 {
            error!("HiROM unsupported");
            return Err(())
        }

        // bytes[22] is the ROM type. I don't care.
        debug!("type: 0x{:02X}", bytes[22]);

        let rom_size = 0x400 << bytes[23] as u32;
        let ram_size = 0x400 << bytes[24] as u32;
        debug!("ROM/RAM size values: {:02X} {:02X}", bytes[23], bytes[24]);
        debug!("{} bytes of ROM, {} bytes of RAM", rom_size, ram_size);

        // bytes[25-26] is a vendor code (doesn't matter)
        debug!("vendor code: 0x{:02X}{:02X}", bytes[25], bytes[26]);
        // 27 = version (also doesn't matter for us)
        debug!("version: 0x{:02X}", bytes[27]);

        // Now it's getting a bit more interesting: The next byte is the checksum's complement
        // followed by the checksum itself. The checksum is the sum of all bytes in the ROM
        // (truncated to 16 bits).
        // 16 bit, little-endian.
        let check_inv = (bytes[29] as u16) << 8 | bytes[28] as u16;
        let check = (bytes[31] as u16) << 8 | bytes[30] as u16;
        if check_inv != !check {
            error!("checksum invalid: stored complement is {:04X}, stored checksum is {:04X}",
                check_inv, check);
            return Err(())
        }

        if check != checksum {
            error!("checksum invalid: stored checksum is {:04X}, computed checksum is {:04X}",
                check, checksum);
            return Err(())
        }

        Ok(RomHeader {
            title: title,
            rom_size: rom_size,
            ram_size: ram_size,
        })
    }

    pub fn title(&self) -> &str {
        str::from_utf8(&self.title).unwrap()
    }
}

/// A ROM image
#[derive(Debug)]
pub struct Rom {
    header: RomHeader,
    ram: Vec<u8>,
    rom: Vec<u8>,
}

impl Rom {
    /// Loads a ROM from raw data.
    pub fn load(mut bytes: &[u8]) -> Result<Rom, ()> {
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

        // Calculate the ROM's checksum (the header loader validates it)
        let mut checksum: u16 = 0;
        for &byte in bytes {
            checksum = checksum.wrapping_add(byte as u16);
        }

        debug!("computed checksum: {:04X}", checksum);

        // Read the SNES header. It is located in a really stupid location which depends on whether
        // the ROM is HiROM or LoROM, so we must check both locations and pick the right one.
        // LoROM header is at 0x7FFF - 63, HiROM is at 0xFFFF - 63 (it is 64 Bytes large)

        // Test LoROM first (it's more common)
        let header = try!(RomHeader::load(&bytes[0x7FFF - 63..0x7FFF + 1], checksum)
            .or_else(|_| RomHeader::load(&bytes[0xFFFF - 63..0xFFFF + 1], checksum)));

        // Create the right amount of RAM...
        let ram = vec![0; header.ram_size as usize];
        // ...and copy the ROM
        let mut rom = Vec::with_capacity(header.rom_size as usize);
        for &b in bytes.iter().take(header.rom_size as usize) {
            rom.push(b);
        }
        if rom.len() != header.rom_size as usize { panic!("ROM size mismatch") }

        Ok(Rom {
            header: header,
            ram: ram,
            rom: rom,
        })
    }

    fn resolve_addr(&mut self, bank: u8, addr: u16) -> &mut u8 {
        // FIXME this assumes LoROM, implement HiROM!

        // XXX I fear that this can cause the worst bugs, so make sure this is 100% correct!!!

        match addr {
            0x0000 ... 0x7fff => {
                // Cartridge RAM mapped to the low 8 pages
                // (there's other stuff here, but that's handled much earlier than we are called)
                match bank {
                    0x70 ... 0x7d => {
                        let a = bank as u32 * 0x8000 + addr as u32;
                        &mut self.ram[a as usize]
                    }
                    0xfe ... 0xff => {
                        // last 64k of RAM
                        let start = self.ram.len() - 64 * 1024;
                        let a = (bank - 0xfe) as u32 * 0x8000 + addr as u32;
                        &mut self.ram[start + a as usize]
                    }
                    _ => {
                        panic!("attempted to access unmapped address: {:02X}:{:04X}", bank, addr);
                    }
                }
            },
            0x8000 ... 0xffff => match bank {
                // LoROM is mapped to the higher 8 pages
                0xfe => {
                    let a = addr as u32 - 0x8000;
                    &mut self.rom[0x3f0000 + a as usize]
                }
                0xff => {
                    let a = addr as u32 - 0x8000;
                    &mut self.rom[0x3f8000 + a as usize]
                }
                _ => {
                    // `% 0x80` because 0x80-0xFD mirrors 0x00-0x7D
                    let a = (bank as u32 % 0x80) * 0x8000 + addr as u32 - 0x8000;
                    &mut self.rom[a as usize]
                }
            },
            _ => unreachable!()
        }
    }
}

impl AddressSpace for Rom {
    fn load(&mut self, bank: u8, addr: u16) -> u8 {
        *self.resolve_addr(bank, addr)
    }

    fn store(&mut self, bank: u8, addr: u16, value: u8) {
        *self.resolve_addr(bank, addr) = value;
    }
}
