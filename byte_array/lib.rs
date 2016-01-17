//! Utility macro for creating convenient fixed-size `u8` arrays.
//!
//! # Example
//! ```
//! #[macro_use]
//! extern crate byte_array;
//!
//! byte_array!(MyArray[1024] with u16 indexing please);
//! byte_array!(AnotherArray[100]);
//!
//! fn main() {
//!     let mut array = MyArray::default();
//!     array[0u16] = 255;
//!     assert_eq!(array[0u16], 255);
//! }
//! ```

#![deny(warnings)]
#![deny(unused_import_braces, unused_qualifications, unused_extern_crates)]

#[macro_export]
#[doc = "hidden"]
macro_rules! impl_byte_array_extra {
    ( $name:ident | u16 indexing $($rest:tt)* ) => {
        impl ::std::ops::Index<u16> for $name {
            type Output = u8;
            fn index(&self, index: u16) -> &u8 { &self.0[index as usize] }
        }
        impl ::std::ops::IndexMut<u16> for $name {
            fn index_mut(&mut self, index: u16) -> &mut u8 { &mut self.0[index as usize] }
        }
        impl_byte_array_extra!( $name | $($rest)* );
    };
    ( $name:ident | save state $($rest:tt)* ) => {
        impl ::libsavestate::SaveState for $name {
            fn save_state<W: ::std::io::Write>(&self, w: &mut W) -> ::std::io::Result<()> {
                w.write_all(&self.0)
            }
            fn restore_state<R: ::std::io::Read>(&mut self, r: &mut R) -> ::std::io::Result<()> {
                ::libsavestate::read_exact(r, &mut self.0)
            }
        }
        impl_byte_array_extra!( $name | $($rest)* );
    };

    ( $name:ident | with $($rest:tt)+ ) => { impl_byte_array_extra!( $name | $($rest)* ); };
    ( $name:ident | , $($rest:tt)+ ) => { impl_byte_array_extra!( $name | $($rest)* ); };
    ( $name:ident | please ) => {};
    ( $name:ident | ) => {};
}

#[macro_export]
#[doc = "hidden"]
macro_rules! impl_byte_array {
    ( $name:ident [$size:expr] $($extra:tt)* ) => {
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
        impl_byte_array_extra!($name | $($extra)*);
    };
}

/// Create a newtype wrapper for `[u8; $size]` that implements `Deref`, `DerefMut` and `Default`.
///
/// Just to make my life a little bit easier, `Index<u16>` and `IndexMut<u16>` can optionally be
/// implemented if you say please (this can save a few mildly annoying `usize` casts).
#[macro_export]
macro_rules! byte_array {
    ( $name:ident [$size:expr] $($extra:tt)* ) => {
        struct $name([u8; $size]);
        impl_byte_array!($name[$size] $($extra)*);
    };
    ( pub $name:ident [$size:expr] $($extra:tt)* ) => {
        pub struct $name([u8; $size]);
        impl_byte_array!($name[$size] $($extra)*);
    };
}
