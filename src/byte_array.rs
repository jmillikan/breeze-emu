//! Utility for fixed-size `u8` arrays used as memory

macro_rules! impl_byte_array {
    ( $name:ident [$size:expr] ) => {
        impl ::std::default::Default for $name {
            fn default() -> Self { $name([0; $size]) }
        }
        impl ::std::ops::Deref for $name {
            type Target = [u8; $size];
            fn deref(&self) -> &[u8; $size] { &self.0 }
        }
        impl ::std::ops::DerefMut for $name {
            fn deref_mut(&mut self) -> &mut [u8; $size] { &mut self.0 }
        }
    };
}

/// Create a newtype wrapper for `[u8; $size]` that implements `Deref`, `DerefMut` and `Default`.
///
/// Just to make my life a little bit easier, `Index<u16>` and `IndexMut<u16>` can optionally be
/// implemented if you say please (this can save a few mildly annoying `usize` casts).
macro_rules! byte_array {
    ( $name:ident [$size:expr] ) => {
        struct $name([u8; $size]);
        impl_byte_array!($name[$size]);
    };
    ( pub $name:ident [$size:expr] ) => {
        pub struct $name([u8; $size]);
        impl_byte_array!($name[$size]);
    };
    ( $name:ident [$size:expr] with u16 indexing please) => {
        byte_array!($name[$size]);
        impl ::std::ops::Index<u16> for $name {
            type Output = u8;
            fn index(&self, index: u16) -> &u8 { &self.0[index as usize] }
        }
        impl ::std::ops::IndexMut<u16> for $name {
            fn index_mut(&mut self, index: u16) -> &mut u8 { &mut self.0[index as usize] }
        }
    };
    ( pub $name:ident [$size:expr] with u16 indexing please) => {
        byte_array!(pub $name[$size]);
        impl ::std::ops::Index<u16> for $name {
            type Output = u8;
            fn index(&self, index: u16) -> &u8 { &self.0[index as usize] }
        }
        impl ::std::ops::IndexMut<u16> for $name {
            fn index_mut(&mut self, index: u16) -> &mut u8 { &mut self.0[index as usize] }
        }
    };
}
