pub mod dummy;

#[doc(hidden)]  // just used by rendertest
pub mod test;
#[cfg(feature = "glium")]
pub mod glium;
#[cfg(feature = "sdl2")]
pub mod sdl;    // FIXME Make private after input handling is better
