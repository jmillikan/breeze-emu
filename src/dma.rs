//! Implements DMA (Direct Memory Access). DMA can be set up by the CPU and either immediately
//! stops the CPU and performs the transfer. HDMA can be set up to perform one transfer per H-Blank
//! per channel.
//!
//! HDMA needs to be coordinated with the PPU: HDMA needs to be initialized when a new frame starts
//! and transfers data after every scanline.

use snes::Peripherals;

/// The configuration of a DMA channel
#[derive(Clone, Copy)]
pub struct DmaChannel {
    /// $43x0 - DMA parameters
    /// ```
    /// da-ssttt
    /// d: Direction (0: A->B, 1: B->A)
    /// a: Use indirect HDMA table
    /// s: A-bus address increment (00: +1, 01/11: 0, 10: -1)
    /// t: See `TransferMode`
    /// ```
    params: u8,
    /// $43x2/$43x3 - Bus A address
    a_addr: u16,
    /// $43x4 - Bus A bank
    a_addr_bank: u8,
    /// $43x1 - The bus B ("PPU bus") address to access
    b_addr: u8,
    /// $43x5/43x6 - DMA size/HDMA indirect address
    ///
    /// DMA transfer size in bytes. A size of 0 will result in a transfer of 65536 Bytes. This is a
    /// hard limit: If the transfer mode would transfer more bytes, the transfer is stopped before.
    /// This value is decremented during DMA and will end up at 0 when DMA is complete.
    dma_size: u16,
    /// $43x7 - HDMA indirect address bank
    hdma_indirect_bank: u8,
}

use self::TransferMode::*;

/// Describes how a single DMA unit (max. 4 Bytes) is transferred
#[derive(Clone, Copy, Debug)]
pub enum TransferMode {
    /// Just writes a single byte
    OneOnce,
    /// Read one Byte and writes it twice
    OneTwice,
    /// Reads two bytes and writes them to the destination
    TwoOnce,
    /// Does `OneTwice` two times: Reads a byte, writes it twice, reads another byte, writes it
    /// twice.
    TwoTwice,
    /// Reads two Bytes, A and B. Writes A, B, A, B.
    TwoTwiceAlternate,
    /// Reads and writes 4 Bytes.
    FourOnce,
}

impl DmaChannel {
    pub fn new() -> DmaChannel {
        DmaChannel {
            params: 0xff,
            a_addr: 0xffff,
            a_addr_bank: 0xff,
            b_addr: 0xff,
            dma_size: 0xffff,
            hdma_indirect_bank: 0xff,
        }
    }

    /// Load from `$43xN`, where `x` is the number of this DMA channel, and `N` is passed as
    /// `addr`.
    pub fn load(&self, reg: u8) -> u8 {
        match reg {
            0x0 => self.params,
            0x1 => self.b_addr,
            0x2 => self.a_addr as u8,
            0x3 => (self.a_addr >> 8) as u8,
            0x4 => self.a_addr_bank,
            0x5 => self.dma_size as u8,
            0x6 => (self.dma_size >> 8) as u8,
            0x7 => self.hdma_indirect_bank,
            _ => panic!("invalid DMA channel register ${:02X}", reg),
        }
    }

    pub fn store(&mut self, reg: u8, val: u8) {
        match reg {
            0x0 => self.params = val,
            0x1 => self.b_addr = val,
            0x2 => self.a_addr = (self.a_addr & 0xff00) | val as u16,
            0x3 => self.a_addr = (self.a_addr & 0x00ff) | ((val as u16) << 8),
            0x4 => self.a_addr_bank = val,
            0x5 => self.dma_size = (self.dma_size & 0xff00) | val as u16,
            0x6 => self.dma_size = (self.dma_size & 0x00ff) | ((val as u16) << 8),
            0x7 => self.hdma_indirect_bank = val,
            _ => panic!("invalid DMA channel register ${:02X}", reg),
        }
    }

    /// Returns `true` if this channel is configured to read from address bus B and write to bus A.
    /// If this returns `false`, the opposite transfer direction is configured.
    fn write_to_a(&self) -> bool { self.params & 0x80 != 0 }

    /// Returns the address increment step for the A bus address. Can be `-1`, `0` or `1`.
    fn a_addr_increment(&self) -> i8 {
        match (self.params >> 3) & 0b11 {
            0b00 => 1,
            0b10 => -1,
            _ => 0,
        }
    }

    /// Returns the transfer size in bytes. Note that this limits the number of bytes, even if the
    /// transfer mode would transfer more bytes.
    fn transfer_size(&self) -> u32 {
        if self.dma_size == 0xffff { 65536 } else { self.dma_size as u32 }
    }

    fn transfer_mode(&self) -> TransferMode {
        match self.params & 0b111 {
            0 => OneOnce,
            1 => TwoOnce,
            2|6 => OneTwice,
            3|7 => TwoTwice,
            4 => FourOnce,
            5 => TwoTwiceAlternate,
            _ => panic!("invalid DMA transfer mode: ${:02X}", self.params & 0b111),
        }
    }
}

/// Perform a single DMA transaction according to `mode`. Reads and writes up to 4 bytes using the
/// given read/write functions.
fn dma_transaction<R, W>(p: &mut Peripherals,
                         mode: TransferMode,
                         read_byte: &mut R,
                         write_byte: &mut W)
                         where R: FnMut(&mut Peripherals) -> u8, W: FnMut(&mut Peripherals, u8) {
    match mode {
        /// Just reads and writes a single byte
        OneOnce => {
            let b = read_byte(p);
            write_byte(p, b);
        }
        /// Read one Byte and writes it twice
        OneTwice => {
            let b = read_byte(p);
            write_byte(p, b);
            write_byte(p, b);
        }
        /// Reads two bytes and writes them to the destination
        TwoOnce => {
            let b = read_byte(p);
            write_byte(p, b);
            let b = read_byte(p);
            write_byte(p, b);
        }
        /// Does `OneTwice` two times: Reads a byte, writes it twice, reads another byte,
        /// writes it twice.
        TwoTwice => {
            let b = read_byte(p);
            write_byte(p, b);
            write_byte(p, b);
            let b = read_byte(p);
            write_byte(p, b);
            write_byte(p, b);
        }
        /// Reads two Bytes, A and B. Writes A, B, A, B.
        TwoTwiceAlternate => {
            let a = read_byte(p);
            let b = read_byte(p);
            write_byte(p, a);
            write_byte(p, b);
            write_byte(p, a);
            write_byte(p, b);
        }
        /// Reads and writes 4 Bytes.
        FourOnce => {
            let b = read_byte(p);
            write_byte(p, b);
            let b = read_byte(p);
            write_byte(p, b);
            let b = read_byte(p);
            write_byte(p, b);
            let b = read_byte(p);
            write_byte(p, b);
        }
    }
}

/// Performs all DMA transactions enabled by the given bitmask. Returns the number of master cycles
/// spent.
pub fn do_dma(p: &mut Peripherals, channels: u8) -> u32 {
    use std::cell::Cell;

    if channels == 0 { return 0 }

    // FIXME: "Now, after the pause, wait 2-8 master cycles to reach a whole multiple of 8 master
    // cycles since reset."
    // (Since this is pretty unpredictable behaviour, nothing should rely on it - I hope)

    let mut dma_cy = 8; // 8 cycles overhead for any DMA transaction

    for i in 0..8 {
        if channels & (1 << i) != 0 {
            dma_cy += 8;    // 8 cycles per active channel

            let chan = p.dma[i];
            let write_to_a = chan.write_to_a();
            let mode = chan.transfer_mode();
            let bytes = Cell::new(chan.transfer_size());
            let a_addr_inc = chan.a_addr_increment();

            let src_bank = if write_to_a { 0 } else { chan.a_addr_bank };
            let mut src_addr = if write_to_a {
                0x2100 + chan.b_addr as u16
            } else {
                chan.a_addr
            };
            let dest_bank = if write_to_a { chan.a_addr_bank } else { 0 };
            let mut dest_addr = if write_to_a {
                chan.a_addr
            } else {
                0x2100 + chan.b_addr as u16
            };

            trace!("DMA on channel {} with {} bytes in mode {:?}, inc {} ({}), \
                from ${:02X}:{:04X} to ${:02X}:{:04X}",
                i, bytes.get(), mode, a_addr_inc, if write_to_a {"B->A"} else {"A->B"}, src_bank,
                src_addr, dest_bank, dest_addr);

            let mut read_byte = |p: &mut Peripherals| {
                let byte = p.load(src_bank, src_addr);
                if !write_to_a {
                    // adjust `src_addr`
                    src_addr = (src_addr as i32 + a_addr_inc as i32) as u16;
                }
                byte
            };
            let mut write_byte = |p: &mut Peripherals, byte| {
                if bytes.get() == 0 { return }
                p.store(dest_bank, dest_addr, byte);
                bytes.set(bytes.get() - 1);
                if write_to_a {
                    // adjust `dest_addr`
                    dest_addr = (dest_addr as i32 + a_addr_inc as i32) as u16;
                }
            };

            dma_cy += bytes.get() * 8;  // 8 master cycles per byte
            while bytes.get() > 0 {
                dma_transaction(p, mode, &mut read_byte, &mut write_byte);
            }

            p.dma[i].dma_size = 0;
        }
    }

    trace!("DMA completed after {} master clock cycles", dma_cy);

    dma_cy
}
