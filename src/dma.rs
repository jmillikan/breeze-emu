//! Implements DMA (Direct Memory Access). DMA can be set up by the CPU and either immediately
//! stops the CPU and performs the transfer. HDMA can be set up to perform one transfer per H-Blank
//! per channel.
//!
//! HDMA needs to be coordinated with the PPU: HDMA needs to be initialized when a new frame starts
//! and transfers data after every scanline.

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
    /// $43x1 - The bus B ("PPU bus") address to access
    b_addr: u8,
    /// $43x2/$43x3 - Bus A address
    a_addr: u16,
    /// $43x4 - Bus A bank
    a_addr_bank: u8,
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
#[derive(Clone, Copy)]
pub enum TransferMode {
    /// Just writes a single byte
    OneOnce,
    /// Read one Byte and writes it twice
    OneTwice,
    /// Reads two bytes and writes them to the destination
    TwoOnce,
    /// Does `1Twice` two times: Reads a byte, writes it twice, reads another byte, writes it
    /// twice.
    TwoTwice,
    /// Reads two Bytes, A and B. Writes A, B, A, B.
    TwoTwiceAlternate,
    /// Reads and writes 4 Bytes.
    FourOnce,
}

impl TransferMode {
    /// Returns the `TransferMode` associated with the low 3 bits of the given value
    pub fn from_u8(val: u8) -> TransferMode {
        use self::TransferMode::*;

        debug_assert_eq!(val, val & 0b111);
        match val & 0b111 {
            0 => OneOnce,
            1 => TwoOnce,
            2|6 => OneTwice,
            3|7 => TwoTwice,
            4 => FourOnce,
            5 => TwoTwiceAlternate,
            _ => panic!("invalid DMA transfer mode: ${:02X}", val),
        }
    }
}

impl DmaChannel {
    pub fn new() -> DmaChannel {
        DmaChannel {
            params: 0xff,
            b_addr: 0xff,
            a_addr: 0xffff,
            a_addr_bank: 0xff,
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
}
