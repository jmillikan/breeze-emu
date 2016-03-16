//! Utilities for `ArrayVec`

// FIXME: This should not need to exist

macro_rules! array_vec {
    ( $t:ty; $size:expr ) => {{
        struct Wrapper([$t; $size]);
        unsafe impl ::arrayvec::Array for Wrapper {
            type Item = $t;

            // I feel really bad implementing `#[doc(hidden)]` items, but Rust's poorly implemented
            // fixed-size arrays leave me no choice.
            type Index = u16; // :(

            fn as_ptr(&self) -> *const Self::Item { self.0.as_ptr() }
            fn as_mut_ptr(&mut self) -> *mut Self::Item { self.0.as_mut_ptr() }
            fn capacity() -> usize { $size }
        }

        // Make sure the `Index` type is correct:
        assert!($size < ::std::u16::MAX);

        ::arrayvec::ArrayVec::<Wrapper>::new()
    }};
}
