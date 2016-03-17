//! Provides a flexible vector wrapper that uses an external slice for storage.
//!
//! This is an alternative to the fixed-size vectors provided by the `arrayvec` crate.

use std::ops::Deref;
use std::mem::replace;

/// A Vector using any slice for backing storage (passed in at creation time).
///
/// Changes to the vector are visible in the backing storage after the `FlexVec` is dropped.
///
/// This is essentially a less ergonomic but more flexible version of the `arrayvec` crate's
/// `ArrayVec` type: You have to crate the backing storage yourself, but `FlexVec` works with
/// fixed-length arrays of *any* length (unlike `ArrayVec`, which works with a fixed set of lengths,
/// since Rust doesn't (yet) have integer generics).
pub struct FlexVec<'a, T: 'a> {
    storage: &'a mut [T],
    len: usize,
}

impl<'a, T> FlexVec<'a, T> {
    /// Create a new `FlexVec`, using the given slice as backing storage for elements.
    ///
    /// The capacity of the vector equals the length of the slice, you have to make sure that the
    /// slice is large enough for all elements.
    pub fn new(storage: &'a mut [T]) -> Self {
        FlexVec {
            storage: storage,
            len: 0,
        }
    }

    /// Returns the number of elements currently stored in this `FlexVec`.
    #[allow(dead_code)] // currently unused
    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns the maximum number of elements that can be stored in this vector. This is equal to
    /// the length of the backing storage passed at creation of this `FlexVec`.
    pub fn capacity(&self) -> usize {
        self.storage.len()
    }

    /// Tries to append an element to the end of this vector.
    ///
    /// If the backing storage is already filled, returns `Err(elem)`.
    pub fn push(&mut self, elem: T) -> Result<(), T> {
        if self.len < self.capacity() {
            self.storage[self.len] = elem;
            self.len += 1;
            Ok(())
        } else {
            Err(elem)
        }
    }
}

impl<'a, T: 'a + Default> FlexVec<'a, T> {
    /// Removes and returns the last element in this vector.
    ///
    /// Returns `None` if the vector is empty.
    ///
    /// This operation is restricted to element types that implement `Default`, since the element's
    /// spot in the backing storage is replaced by a default value.
    #[allow(dead_code)] // currently unused
    pub fn pop(&mut self) -> Option<T> {
        if self.len > 0 {
            self.len -= 1;
            let elem = replace(&mut self.storage[self.len], T::default());
            Some(elem)
        } else {
            None
        }
    }
}

impl<'a, T> Deref for FlexVec<'a, T> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        &self.storage[..self.len]
    }
}

#[test]
fn basic() {
    const CAP: usize = 1;
    let mut storage = [0; CAP];

    {
        let mut s = FlexVec::new(&mut storage);
        assert_eq!(s.len(), 0);
        assert_eq!(s.capacity(), CAP);

        assert_eq!(s.push(123), Ok(()));
        assert_eq!(s.len(), 1);
        assert_eq!(s.push(42), Err(42));
        assert_eq!(s.pop(), Some(123));
        assert_eq!(s.len(), 0);
        assert_eq!(&*s, &[]);
    }
}
