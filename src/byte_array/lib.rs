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
macro_rules! impl_array_extra {
    ( $name:ident, $t:ty | u16 indexing $($rest:tt)* ) => {
        impl ::std::ops::Index<u16> for $name {
            type Output = $t;
            fn index(&self, index: u16) -> &$t { &self.0[index as usize] }
        }
        impl ::std::ops::IndexMut<u16> for $name {
            fn index_mut(&mut self, index: u16) -> &mut $t { &mut self.0[index as usize] }
        }
        impl_array_extra!( $name, $t | $($rest)* );
    };
    ( $name:ident, $t:ty | save state $($rest:tt)* ) => {
        impl ::libsavestate::SaveState for $name {
            fn save_state<W: ::std::io::Write + ?Sized>(&self, w: &mut W) -> ::std::io::Result<()> {
                w.write_all(&self.0)
            }
            fn restore_state<R: ::std::io::Read + ?Sized>(&mut self, r: &mut R) -> ::std::io::Result<()> {
                ::libsavestate::read_exact(r, &mut self.0)
            }
        }
        impl_array_extra!( $name, $t | $($rest)* );
    };

    ( $name:ident, $t:ty | with $($rest:tt)+ ) => { impl_array_extra!( $name, $t | $($rest)* ); };
    ( $name:ident, $t:ty | , $($rest:tt)+ ) => { impl_array_extra!( $name, $t | $($rest)* ); };
    ( $name:ident, $t:ty | please ) => {};
    ( $name:ident, $t:ty | ) => {};
}

#[macro_export]
#[doc = "hidden"]
macro_rules! impl_array {
    ( $name:ident [$t:ty; $size:expr] $($extra:tt)* ) => {
        impl ::std::default::Default for $name {
            fn default() -> Self { $name([<$t as Default>::default(); $size]) }
        }
        impl ::std::clone::Clone for $name {
            fn clone(&self) -> Self {
                $name(self.0)
            }
        }
        impl ::std::ops::Deref for $name {
            type Target = [$t; $size];
            fn deref(&self) -> &[$t; $size] { &self.0 }
        }
        impl ::std::ops::DerefMut for $name {
            fn deref_mut(&mut self) -> &mut [$t; $size] { &mut self.0 }
        }
        impl_array_extra!($name, $t | $($extra)*);
    };
}

/// Create a newtype wrapper for `[$t; $size]` that implements `Deref`, `DerefMut` and `Default`.
///
/// Just to make my life a little bit easier, `Index<u16>` and `IndexMut<u16>` can optionally be
/// implemented if you say please (this can save a few mildly annoying `usize` casts).
#[macro_export]
macro_rules! make_array {
    ( $name:ident [$t:ty; $size:expr] $($extra:tt)* ) => {
        struct $name([$t; $size]);
        impl_array!($name[$t; $size] $($extra)*);
    };
    ( pub $name:ident [$t:ty; $size:expr] $($extra:tt)* ) => {
        pub struct $name([$t; $size]);
        impl_array!($name[$t; $size] $($extra)*);
    };
}

/// Create a newtype wrapper for `[u8; $size]` that implements `Deref`, `DerefMut` and `Default`.
///
/// Just to make my life a little bit easier, `Index<u16>` and `IndexMut<u16>` can optionally be
/// implemented if you say please (this can save a few mildly annoying `usize` casts).
#[macro_export]
macro_rules! byte_array {
    ( $name:ident [$size:expr] $($extra:tt)* ) => {
        make_array!($name [u8; $size] $($extra)*);
    };
    ( pub $name:ident [$size:expr] $($extra:tt)* ) => {
        make_array!(pub $name [u8; $size] $($extra)*);
    };
}
