/// The configuration of a DMA channel
#[derive(Clone, Copy)]
pub struct DmaChannel {
    /// $43x1 - The bus B ("PPU bus") address to access
    b_addr: u8,
    /// $43x2
    a_addr_lo: u8,
    /// $43x3
    a_addr_hi: u8,
    /// $43x4
    a_addr_bank: u8,
    /// When `false`, memory will be read from bus A and written to bus B (to the PPU). When
    /// `true`, the transfer direction is reversed.
    read_ppu: bool,
    /// Use an indirect table for HDMA transfers.
    hdma_indirect: bool,
    /// Address step of A bus: -1, 0 or 1
    a_step: i8,
    mode: TransferMode,
    /// $43x5/43x6 - DMA size/HDMA indirect address low/high byte
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
enum TransferMode {
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

impl DmaChannel {
    pub fn new() -> DmaChannel {
        DmaChannel {
            b_addr: 0xff,
            a_addr_lo: 0xff,
            a_addr_hi: 0xff,
            a_addr_bank: 0xff,
            read_ppu: true,
            hdma_indirect: true,
            a_step: 0,
            mode: TwoTwice,
            dma_size: 0xffff,
            hdma_indirect_bank: 0xff,
        }
    }
}
