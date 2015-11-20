//! Emulator Save States.
//!
//! A save state represents the emulator's state at the beginning of a frame. Save states can be
//! written to disk and later restored.

use std::io::{self, Read, Write};
use std::mem;
use std::slice;

/// Trait for types which can be serialized in a save state.
///
/// Note that this approach is a bit different from usual serialization: `restore_state` takes a
/// `&mut self` and overwrites parts of it with the saved state. This way, we neither save the
/// renderer, nor the ROM, nor the input sources, nor temporary data that is recomputed when we
/// render a new frame after loading a save state.
///
/// When implementing this trait, use an exhaustive let-binding for all fields on the `Self` type.
/// This way, any change that could potentially require a change in the `SaveState` implementation
/// to the type will cause a compilation error. By using the `impl_save_state` macro defined below,
/// this will be done automatically.
pub trait SaveState {
    fn save_state<W: Write>(&self, w: &mut W) -> io::Result<()>;
    fn restore_state<R: Read>(&mut self, r: &mut R) -> io::Result<()>;
}

/// FIXME Temporary copy of `std`s `Read::read_exact`. Remove once `read_exact` is stable.
pub fn read_exact<R: Read>(r: &mut R, mut buf: &mut [u8]) -> io::Result<()> {
    while !buf.is_empty() {
        match r.read(buf) {
            Ok(0) => break,
            Ok(n) => { let tmp = buf; buf = &mut tmp[n..]; }
            Err(ref e) if e.kind() == io::ErrorKind::Interrupted => {}
            Err(e) => return Err(e),
        }
    }
    if !buf.is_empty() {
        Err(io::Error::new(io::ErrorKind::Other, "failed to fill whole buffer"))
    } else {
        Ok(())
    }
}

/// Declares that a type can be safely transmuted into a byte slice of same length as the type's
/// size in Bytes and vice-versa.
///
/// This is true for all types which don't impose any restrictions on their raw content, so this
/// would not be safe to implement for types like `bool` or `char`. This is also obviously unsafe
/// for non-`#[repr(C)]`-structs.
unsafe trait TransmuteByteSafe {}

unsafe impl TransmuteByteSafe for u8 {}
unsafe impl TransmuteByteSafe for u16 {}
unsafe impl TransmuteByteSafe for u32 {}
unsafe impl TransmuteByteSafe for u64 {}
unsafe impl TransmuteByteSafe for usize {}
unsafe impl TransmuteByteSafe for i8 {}
unsafe impl TransmuteByteSafe for i16 {}
unsafe impl TransmuteByteSafe for i32 {}

/// Everything that can be transmuted to/from fixed-size byte slices can trivially implement
/// `SaveState`. This includes a large portion of the emulator state, since much of it consist of
/// primitive integer values.
impl<T: TransmuteByteSafe> SaveState for T {
    fn save_state<W: Write>(&self, w: &mut W) -> io::Result<()> {
        let bytes = unsafe {
            slice::from_raw_parts(self as *const _ as *const u8, mem::size_of::<Self>())
        };
        w.write_all(bytes)
    }

    fn restore_state<R: Read>(&mut self, r: &mut R) -> io::Result<()> {
        read_exact(r, unsafe {
            slice::from_raw_parts_mut(self as *mut _ as *mut u8, mem::size_of::<Self>())
        })
    }
}

/// `bool` will always be saved as a `1` or `0` byte.
impl SaveState for bool {
    fn save_state<W: Write>(&self, w: &mut W) -> io::Result<()> {
        w.write_all(&[if *self {1} else {0}])
    }

    fn restore_state<R: Read>(&mut self, r: &mut R) -> io::Result<()> {
        let mut val = [0xff];
        try!(read_exact(r, &mut val));

        match val[0] {
            0 => *self = false,
            1 => *self = true,
            _ => return Err(io::Error::new(io::ErrorKind::Other, "invalid byte value for bool")),
        }
        Ok(())
    }
}

/// `Option<T>` will either save/restore `true` followed by its contents, or `false` if it's `None`.
impl<T: SaveState + Default> SaveState for Option<T> {
    fn save_state<W: Write>(&self, w: &mut W) -> io::Result<()> {
        match *self {
            None => try!(false.save_state(w)),
            Some(ref t) => {
                try!(true.save_state(w));
                try!(t.save_state(w));
            }
        }
        Ok(())
    }

    fn restore_state<R: Read>(&mut self, r: &mut R) -> io::Result<()> {
        let mut val = [0xff];
        try!(read_exact(r, &mut val));

        match val[0] {
            0 => *self = None,
            1 => {
                let mut t = T::default();
                try!(t.restore_state(r));
                *self = Some(t);
            }
            // XXX Ugly, but works
            _ => return Err(io::Error::new(io::ErrorKind::Other, "invalid byte value for option")),
        }
        Ok(())
    }
}

/// **FIXME** The `SaveState` impl for slices of `T` assumes that it is only used for fixed-size
/// arrays, which can easily lead to bugs!
impl<T: SaveState> SaveState for [T] {
    fn save_state<W: Write>(&self, w: &mut W) -> io::Result<()> {
        for t in self {
            try!(t.save_state(w))
        }
        Ok(())
    }

    fn restore_state<R: Read>(&mut self, r: &mut R) -> io::Result<()> {
        // Assume `self` always has the same size
        for t in self {
            try!(t.restore_state(r));
        }
        Ok(())
    }
}

/// `Vec<T>`s `SaveState` impl will read/write the `Vec`s length first, followed by its contents.
impl<T: SaveState + Default> SaveState for Vec<T> {
    fn save_state<W: Write>(&self, w: &mut W) -> io::Result<()> {
        // Write the len first:
        try!(self.len().save_state(w));
        (&self[..]).save_state(w)
    }

    fn restore_state<R: Read>(&mut self, r: &mut R) -> io::Result<()> {
        let mut len = 0usize;
        try!(len.restore_state(r));
        self.clear();
        for _ in 0..len {
            let mut obj = T::default();
            try!(obj.restore_state(r));
            self.push(obj);
        }
        Ok(())
    }
}


/// Generates an impl of `SaveState` for a given type, saving/restoring a list of fields, and
/// ignoring a second list of fields.
///
/// To prevent bugs, these lists must contains all fields of the type exactly once or this won't
/// compile.
macro_rules! impl_save_state {
    ( $t:ident { $( $field:ident ),* } ignore { $( $ignore:ident ),* } ) => {
        impl ::savestate::SaveState for $t {
            fn save_state<W: ::std::io::Write>(&self, w: &mut W) -> ::std::io::Result<()> {
                let $t { $(ref $field),*, $(ref $ignore,)* } = *self;
                $(
                    try!($field.save_state(w));
                );*
                $(
                    let _ = $ignore;
                )*
                Ok(())
            }

            fn restore_state<R: ::std::io::Read>(&mut self, r: &mut R) -> ::std::io::Result<()> {
                let $t { $(ref mut $field),*, $(ref mut $ignore,)* } = *self;
                $(
                    try!($field.restore_state(r));
                );*
                $(
                    let _ = $ignore;
                )*
                Ok(())
            }
        }
    };
}

/// Generates a `SaveState` impl for a newtype wrapper. The type must be declared as
/// `SomeType(InnerType)` and `InnerType` must implement `SaveState`.
macro_rules! impl_save_state_for_newtype {
    ( $t:ident ) => {
        impl ::savestate::SaveState for $t {
            fn save_state<W: ::std::io::Write>(&self, w: &mut W) -> ::std::io::Result<()> {
                self.0.save_state(w)
            }

            fn restore_state<R: ::std::io::Read>(&mut self, r: &mut R) -> ::std::io::Result<()> {
                self.0.restore_state(r)
            }
        }
    };
}
