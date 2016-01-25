// FIXME: Unused and maybe not what we want (but hey - at least it's got a test!)

/// Stores up to 64 Bits in a "Stream" or queue
pub struct BitStream {
    bits: u64,
    len: u8,
}

impl BitStream {
    /// Constructs an empty `BitStream`
    pub fn new() -> Self {
        BitStream {
            bits: 0,
            len: 0,
        }
    }

    /// Returns the number of bits currently stored in this `BitStream`
    pub fn len(&self) -> u8 { self.len }

    /// Returns whether this `BitStream` is empty. Equivalent (but preferred) to `bs.len() == 0`.
    pub fn is_empty(&self) -> bool { self.len() == 0 }

    /// Put a bit into the `BitStream`, increasing its length by 1.
    ///
    /// # Panics
    ///
    /// This method panics when the `BitStream` is already full (already contains 64 Bits)
    pub fn put(&mut self, bit: bool) -> &mut Self {
        assert!(self.len < 64, "called put() on a full BitStream");

        let bitnum = (bit as u64) << self.len;
        self.bits |= bitnum;
        self.len += 1;

        self
    }

    /// Remove the oldest bit from the `BitStream`, decreasing its length by 1.
    ///
    /// # Panics
    ///
    /// This method panics when the `BitStream` is empty
    pub fn get(&mut self) -> bool {
        assert!(self.len > 0, "called get() on empty BitStream");

        self.len -= 1;
        let bitnum = self.bits & 1;
        self.bits >>= 1;

        bitnum != 0
    }
}

#[test]
fn test() {
    let mut bs = BitStream::new();
    bs.put(true);
    bs.put(false);
    bs.put(true);
    bs.put(true);
    bs.put(true);
    bs.put(false);
    bs.put(false);
    bs.put(false);

    assert_eq!(bs.get(), true);
    assert_eq!(bs.get(), false);
    assert_eq!(bs.get(), true);
    assert_eq!(bs.get(), true);
    assert_eq!(bs.get(), true);
    assert_eq!(bs.get(), false);
    assert_eq!(bs.get(), false);

    bs.put(true);
    bs.put(false);

    assert_eq!(bs.get(), false);

    assert_eq!(bs.get(), true);
    assert_eq!(bs.get(), false);
}
