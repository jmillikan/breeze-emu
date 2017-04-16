//! Implements DMA (Direct Memory Access).
//!
//! The SNES provides 2 DMA mechanisms: General purpose DMA can be set up by the CPU by writing to
//! `$420B - MDMAEN`, which stops the CPU and performs the transfer on all enabled channels. HDMA
//! can be set up to perform one transfer per H-Blank per channel. There are 8 channels in total,
//! and some of them may be configured to be used for HDMA, while the other channels can be used for
//! DMA.
//!
//! HDMA needs to be coordinated with the PPU: It needs to be initialized when a new frame starts,
//! which is done by calling `init_hdma`, and can transfer data every scanline, which is done by
//! `do_hdma`. DMA is simpler: It is started by calling `do_dma` when the CPU writes to `$420B` and
//! doesn't need periodic callbacks.

use std::cell::Cell;

use snes::Peripherals;

use wdc65816::Mem;

/// The configuration of a DMA channel
#[derive(Clone, Copy)]
pub struct DmaChannel {
    /// `$43x0`: DMAPx - DMA parameters
    /// ```
    /// da-ssttt
    /// d: Direction (0: A->B, 1: B->A)
    /// a: Use indirect HDMA table (ignored for DMA)
    /// s: A-bus address increment (00: +1, 01/11: 0, 10: -1)
    /// t: See `TransferMode`
    /// ```
    params: u8,
    /// `$43x1`: BBADx - The bus B ("PPU bus") address to access
    b_addr: u8,
    /// `$43x2/$43x3`: A1TxL/A1TxH - Bus A address
    a_addr: u16,
    /// `$43x4`: A1Bx - Bus A bank
    a_addr_bank: u8,
    /// `$43x5/$43x6`: DASxL/DASxH - DMA size/HDMA indirect address
    ///
    /// DMA transfer size in bytes. A size of 0 will result in a transfer of 65536 Bytes. This is a
    /// hard limit: If the transfer mode would transfer more bytes, the transfer is stopped before.
    /// This value is decremented during DMA and will end up at 0 when DMA is complete.
    dma_size: u16,
    /// `$43x7`: DASBx - HDMA indirect address bank
    hdma_indirect_bank: u8,
    /// `$43x8/$43x9`: A2AxL/A2AxH
    hdma_addr: u16,
    /// `$43xA`: NTRLx
    /// `rlllllll`
    /// * `r`: Repeat (1: write every scanline, 0: write once)
    /// * `l`: Line counter
    hdma_flags: u8,
    hdma_do_transfer: bool,
}

impl_save_state!(DmaChannel { params, a_addr, a_addr_bank, b_addr, dma_size, hdma_indirect_bank,
                              hdma_addr, hdma_flags, hdma_do_transfer } ignore {});

impl Default for DmaChannel {
    fn default() -> Self {
        DmaChannel {
            params: 0xff,
            a_addr: 0xffff,
            a_addr_bank: 0xff,
            b_addr: 0xff,
            dma_size: 0xffff,
            hdma_indirect_bank: 0xff,
            hdma_addr: 0,
            hdma_flags: 0,
            hdma_do_transfer: false,
        }
    }
}

/// Describes how a single DMA unit (max. 4 Bytes) is transferred
///
/// ```text
/// Mode  Bytes              B-Bus 21xxh Address   ;Usage Examples...
/// 0  =  Transfer 1 byte    xx                    ;eg. for WRAM (port 2180h)
/// 1  =  Transfer 2 bytes   xx, xx+1              ;eg. for VRAM (port 2118h/19h)
/// 2  =  Transfer 2 bytes   xx, xx                ;eg. for OAM or CGRAM
/// 3  =  Transfer 4 bytes   xx, xx,   xx+1, xx+1  ;eg. for BGnxOFS, M7x
/// 4  =  Transfer 4 bytes   xx, xx+1, xx+2, xx+3  ;eg. for BGnSC, Window, APU..
/// 5  =  Transfer 4 bytes   xx, xx+1, xx,   xx+1  ;whatever purpose, VRAM maybe
/// 6  =  Transfer 2 bytes   xx, xx                ;same as mode 2
/// 7  =  Transfer 4 bytes   xx, xx,   xx+1, xx+1  ;same as mode 3
/// ```
#[derive(Clone, Copy, Debug)]
pub enum TransferMode {
    /// Mode 0: Just reads and writes a single byte
    Single,
    /// Mode 1: Transfer 2 bytes. B-Bus address is incremented for the second byte.
    TwoInc,
    /// Mode 2/6: Reads two bytes and writes them to the destination. B-Bus address stays the same
    /// for both bytes.
    TwoNoInc,
    /// Mode 3/7: Transfer 4 bytes and increment B-Bus address after the first 2 bytes.
    FourIncOnce,
    /// Mode 4: Transfer 4 bytes and increment B-Bus address after each.
    FourIncAlways,
    /// Mode 5: Transfer 4 bytes, use B-Bus address offsets `0, 1, 0, 1`.
    FourToggle,
}

use self::TransferMode::*;

impl DmaChannel {
    /// Load from `$43xN`, where `x` is the number of this DMA channel, and `N` is passed as
    /// `reg`.
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
            0x8 => self.hdma_addr as u8,
            0x9 => (self.hdma_addr >> 8) as u8,
            0xA => self.hdma_flags,
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
            0x8 => self.hdma_addr = (self.hdma_addr & 0xff00) | val as u16,
            0x9 => self.hdma_addr = (self.hdma_addr & 0x00ff) | ((val as u16) << 8),
            0xA => self.hdma_flags = val,
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
            0 => Single,
            1 => TwoInc,
            2|6 => TwoNoInc,
            3|7 => FourIncOnce,
            4 => FourIncAlways,
            5 => FourToggle,
            _ => unreachable!(),
        }
    }
}

/// Perform a single DMA transfer according to `mode`. Reads and writes up to 4 bytes using the
/// given read/write functions.
fn dma_transfer<R, W>(p: &mut Peripherals,
                      mode: TransferMode,
                      b_addr: u16,
                      read_byte: &mut R,
                      write_byte: &mut W)
    where R: FnMut(&mut Peripherals, u16) -> u8,
          W: FnMut(&mut Peripherals, u8, u16) {
    match mode {
        Single => {
            let b = read_byte(p, b_addr);
            write_byte(p, b, b_addr);
        }
        TwoInc => {
            let b = read_byte(p, b_addr);
            write_byte(p, b, b_addr);
            let b = read_byte(p, b_addr + 1);
            write_byte(p, b, b_addr + 1);
        }
        TwoNoInc => {
            let b = read_byte(p, b_addr);
            write_byte(p, b, b_addr);
            let b = read_byte(p, b_addr);
            write_byte(p, b, b_addr);
        }
        FourIncOnce => {
            let b = read_byte(p, b_addr);
            write_byte(p, b, b_addr);
            let b = read_byte(p, b_addr);
            write_byte(p, b, b_addr);

            let b = read_byte(p, b_addr + 1);
            write_byte(p, b, b_addr + 1);
            let b = read_byte(p, b_addr + 1);
            write_byte(p, b, b_addr + 1);
        }
        FourIncAlways => {
            let b = read_byte(p, b_addr);
            write_byte(p, b, b_addr);
            let b = read_byte(p, b_addr + 1);
            write_byte(p, b, b_addr + 1);
            let b = read_byte(p, b_addr + 2);
            write_byte(p, b, b_addr + 2);
            let b = read_byte(p, b_addr + 3);
            write_byte(p, b, b_addr + 3);
        }
        FourToggle => {
            let b = read_byte(p, b_addr);
            write_byte(p, b, b_addr);
            let b = read_byte(p, b_addr + 1);
            write_byte(p, b, b_addr + 1);
            let b = read_byte(p, b_addr);
            write_byte(p, b, b_addr);
            let b = read_byte(p, b_addr + 1);
            write_byte(p, b, b_addr + 1);
        }
    }
}

/// Performs all DMA transactions enabled by the given `channels` bitmask. Returns the number of
/// master cycles spent.
pub fn do_dma(p: &mut Peripherals, channels: u8) -> u32 {
    if channels == 0 { return 0 }

    // FIXME: "Now, after the pause, wait 2-8 master cycles to reach a whole multiple of 8 master
    // cycles since reset."
    // (Since this is pretty unpredictable behaviour, nothing should rely on it - I hope)
    // FIXME: do_io_cycle is interfering badly with (H)DMA, we should save and restore p.cy
    // (I think?). also do this in init_hdma.

    let mut dma_cy = 8; // 8 cycles overhead for any DMA transaction

    for i in 0..8 {
        if channels & (1 << i) != 0 {
            dma_cy += 8;    // 8 cycles per active channel

            let chan = p.dma[i];
            let write_to_a = chan.write_to_a();
            let mode = chan.transfer_mode();
            let bytes = Cell::new(chan.transfer_size());
            let a_bank = chan.a_addr_bank;
            
            let a_addr = Cell::new(chan.a_addr);
            let a_addr_inc = chan.a_addr_increment();
            let b_addr = 0x2100 + chan.b_addr as u16;

            trace!("DMA on channel {} with {} bytes in mode {:?}, inc {} ({}), \
                    A-Bus ${:02X}:{:04X}, B-Bus $00:{:04X}",
                   i, bytes.get(), mode, a_addr_inc, if write_to_a {"B->A"} else {"A->B"}, a_bank,
                   a_addr.get(), b_addr);

            // FIXME Decrement the channel's `dma_size` field
            let mut read_byte = |p: &mut Peripherals, b_addr| -> u8 {
                if bytes.get() == 0 { return 0; }
                let (src_bank, src_addr) = if write_to_a {
                    (0, b_addr)
                } else {
                    (a_bank, a_addr.get())
                };
                let byte = p.load(src_bank, src_addr);
                if !write_to_a {
                    // adjust `a_addr`
                    a_addr.set((a_addr.get() as i32 + a_addr_inc as i32) as u16);
                }
                byte
            };
            let mut write_byte = |p: &mut Peripherals, byte, b_addr| {
                if bytes.get() == 0 { return }
                let (dest_bank, dest_addr) = if write_to_a {
                    (a_bank, a_addr.get())
                } else {
                    (0, b_addr)
                };
                p.store(dest_bank, dest_addr, byte);
                bytes.set(bytes.get() - 1);
                if write_to_a {
                    // adjust `a_addr`
                    a_addr.set((a_addr.get() as i32 + a_addr_inc as i32) as u16);
                }
            };

            dma_cy += bytes.get() * 8;  // 8 master cycles per byte
            while bytes.get() > 0 {
                dma_transfer(p, mode, b_addr, &mut read_byte, &mut write_byte);
            }

            p.dma[i].dma_size = 0;
        }
    }

    trace!("DMA completed after {} master clock cycles", dma_cy);

    dma_cy
}

/// Refresh HDMA state for a new frame. This is called at V=0, H~6 and will set up some internal
/// registers for all active channels.
///
/// See http://wiki.superfamicom.org/snes/show/DMA+%26+HDMA for more info.
///
/// Returns the number of cycles the setup needed.
pub fn init_hdma(p: &mut Peripherals, channel_mask: u8) -> u32 {
    // "Overhead is ~18 master cycles, plus 8 master cycles for each channel set for direct HDMA and
    // 24 master cycles for each channel set for indirect HDMA."

    // FIXME: See do_dma about do_io_cycle affecting master cy
    let mut cy = 0; 

    for i in 0..8 {
        if channel_mask & (1 << i) != 0 {
            // Set address = aaddress
            p.dma[i].hdma_addr = p.dma[i].a_addr;

            // Get line count & repeat from table
            let (i_bank, i_addr) = (p.dma[i].a_addr_bank, p.dma[i].hdma_addr);
            let repeat_and_count = p.load(i_bank, i_addr);

            // and bump table address to first entry
            p.dma[i].hdma_addr += 1;

            // HDMA should terminate if .hdma_flags is 0 here or below.
            // Assuming the mechanism is just .hdma_flags itself, checked per scanline.
            p.dma[i].hdma_flags = repeat_and_count;

            cy += 8;

            // If indirect, load first value address and bump table address to next line count.
            if p.dma[i].params & 0x40 != 0 {
                let addr_low = p.load(i_bank, i_addr + 1);
                let addr_high = p.load(i_bank, i_addr + 2);

                p.dma[i].hdma_addr += 2;
                
                p.dma[i].dma_size = ((addr_high as u16) << 8) | (addr_low as u16);

                cy += 16;
            }

            p.dma[i].hdma_do_transfer = true;
        }
    }

    cy
}

/// Performs one H-Blank worth of HDMA transfers (at most 8, if all channels are enabled).
pub fn do_hdma(p: &mut Peripherals, channel_mask: u8) -> u32 {

    if channel_mask == 0 { return 0 }

    // FIXME: See do_dma about do_io_cycle affecting master cy
    let mut cy = 18;

    for i in 0..8 {
        if channel_mask & (1 << i) != 0 {
            cy += 8;
            
            // Halt HDMA if line count/repeat is 0.
            // This may be wrong.
            if p.dma[i].hdma_flags == 0 { continue; }

            let indirect = p.dma[i].params & 0x40 != 0;
            
            let chan = p.dma[i];
            let mode = chan.transfer_mode();

            // Need a new abstraction for this, assuming I understand it right.
            let a_bank = if indirect { chan.hdma_indirect_bank } else { chan.a_addr_bank };
            let a_addr = Cell::new(if indirect { chan.dma_size } else { chan.hdma_addr });

            let b_addr = 0x2100 + chan.b_addr as u16;

            // Each round is a full round, so no counting. A->B only.
            let mut read_byte = |p: &mut Peripherals, _| -> u8 {
                let byte = p.load(a_bank, a_addr.get());
                a_addr.set((a_addr.get() as i32 + 1 as i32) as u16);

                byte
            };
            
            let mut write_byte = |p: &mut Peripherals, byte, b_addr| {
                p.store(0, b_addr, byte);

                cy += 8;
            };

            if p.dma[i].hdma_do_transfer {
                dma_transfer(p, mode, b_addr, &mut read_byte, &mut write_byte);

                // ...and now .hdma_addr or .dma_size is behind a_addr, so catch up.
                if indirect {
                    p.dma[i].dma_size = a_addr.get();
                }
                else {
                    p.dma[i].hdma_addr = a_addr.get();
                }
            }

            // Decrement line counter
            p.dma[i].hdma_flags -= 1;

            // Set do_transfer to repeat bit
            p.dma[i].hdma_do_transfer = p.dma[i].hdma_flags & 0b10000000 != 0;

            // If line counter is 0...
            if p.dma[i].hdma_flags & 0b01111111 == 0 {
                let bank = p.dma[i].a_addr_bank;
                
                let addr = p.dma[i].hdma_addr;
                
                p.dma[i].hdma_flags = p.load(bank, addr);
                // Next scanline will not occur if hdma_flags == 0

                if indirect {
                    // Apparently, we do this even if hdma_flags == 0 and we're about to stop HDMA.
                    let low_byte = p.load(bank, addr + 1);
                    let high_byte = p.load(bank, addr + 2);
                    
                    p.dma[i].dma_size = ((high_byte as u16) << 8) | (low_byte as u16);

                    p.dma[i].hdma_addr += 3;
                }
                else {
                    p.dma[i].hdma_addr += 1;
                }

                p.dma[i].hdma_do_transfer = true;
            }
        }
    }

    cy
}
