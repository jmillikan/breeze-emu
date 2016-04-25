//! Input recording and replaying
//!
//! Contains submodules that implement specific recording formats.

mod custom;
mod smv;

use input::Ports;
use snes::Snes;

use std::io::{self, Write, BufRead, Seek};

#[derive(Debug)]
pub enum RecordingFormat {
    /// Custom RLE compressed format
    ///
    /// See the `custom` module for the implementation. (currently broken, don't use)
    Custom,

    /// The SMV format used by Snes9x
    ///
    /// This implements SMV version 4, used by Snes9x 1.51
    Smv,
}

impl Default for RecordingFormat {
    fn default() -> Self {
        RecordingFormat::Smv
    }
}

/// Trait for recording sources
///
/// This shouldn't be implemented manually
pub trait WriteSeek : Write + Seek {}

impl<T: Write + Seek> WriteSeek for T {}

// TODO: Implement methods that detect the `RecordingFormat` from a file extension or a `Read`
// instance (based on the header)

/// Trait for input recorders
pub trait Recorder {
    /// Create a new recorder, writing to the given writer
    fn new(writer: Box<WriteSeek>, snes: &Snes) -> io::Result<Self> where Self: Sized;

    /// Record the state of the peripherals attached to `ports`.
    ///
    /// Called right after input was latched. If the game doesn't latch input, we guarantee that
    /// this will still be called once per frame.
    fn record_frame(&mut self, ports: &Ports) -> io::Result<()>;
}

/// Trait for record replayers
pub trait Replayer {
    /// Create a new replayer, reading from the given buffered reader.
    fn new(reader: Box<BufRead>, snes: &Snes) -> io::Result<Self> where Self: Sized;

    /// Replay the next frame, updating the state of `ports`.
    ///
    /// Called when input is latched. If the game doesn't latch input, we guarantee that this will
    /// still be called once per frame.
    fn replay_frame(&mut self, ports: &mut Ports) -> io::Result<()>;
}

/// Create a recorder for a specified format.
pub fn create_recorder(format: RecordingFormat,
                       writer: Box<WriteSeek>,
                       snes: &Snes)
                       -> io::Result<Box<Recorder>> {
    debug!("creating recorder for {:?} format", format);
    Ok(match format {
        RecordingFormat::Custom => Box::new(try!(custom::Recorder::new(writer, snes))),
        RecordingFormat::Smv => Box::new(try!(smv::Recorder::new(writer, snes))),
    })
}

pub fn create_replayer(format: RecordingFormat,
                       reader: Box<BufRead>,
                       snes: &Snes)
                       -> io::Result<Box<Replayer>> {
    debug!("creating replayer for {:?} format", format);
    Ok(match format {
        RecordingFormat::Custom => Box::new(try!(custom::Replayer::new(reader, snes))),
        RecordingFormat::Smv => Box::new(try!(smv::Replayer::new(reader, snes))),
    })
}
