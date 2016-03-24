//! Input recording and replaying
//!
//! Contains submodules that implement specific recording formats.

mod custom;
mod smv;

use input::Ports;

use std::io::{self, Write, BufRead};

#[derive(Debug)]
pub enum RecordingFormat {
    /// Custom RLE compressed format
    ///
    /// See the `custom` module for the implementation. (currently broken, don't use)
    Custom,

    /// The SMV format used by Snes9x
    Smv,

    // TODO: maybe add a few other emulator formats
}

impl Default for RecordingFormat {
    fn default() -> Self {
        RecordingFormat::Custom
    }
}


// TODO: Implement methods that detect the `RecordingFormat` from a file extension or a `Read`
// instance (based on the header)

/// Trait for input recorders
pub trait Recorder {
    /// Create a new recorder, writing to the given writer
    fn new(writer: Box<Write>) -> Self where Self: Sized;

    /// Record the state of the peripherals attached to `ports`.
    ///
    /// Called right after input was latched. If the game doesn't latch input, we guarantee that
    /// this will still be called once per frame.
    fn record_frame(&mut self, ports: &Ports) -> io::Result<()>;
}

/// Trait for record replayers
pub trait Replayer {
    /// Create a new replayer, reading from the given buffered reader.
    fn new(reader: Box<BufRead>) -> Self where Self: Sized;

    /// Replay the next frame, updating the state of `ports`.
    ///
    /// Called when input is latched. If the game doesn't latch input, we guarantee that this will
    /// still be called once per frame.
    fn replay_frame(&mut self, ports: &mut Ports) -> io::Result<()>;
}

/// Create a recorder for a specified format.
pub fn create_recorder(format: RecordingFormat, writer: Box<Write>) -> Box<Recorder> {
    debug!("creating recorder for {:?} format", format);
    match format {
        RecordingFormat::Custom => Box::new(custom::Recorder::new(writer)),
        RecordingFormat::Smv => Box::new(smv::Recorder::new(writer)),
    }
}

pub fn create_replayer(format: RecordingFormat, reader: Box<BufRead>) -> Box<Replayer> {
    debug!("creating replayer for {:?} format", format);
    match format {
        RecordingFormat::Custom => Box::new(custom::Replayer::new(reader)),
        RecordingFormat::Smv => Box::new(smv::Replayer::new(reader)),
    }
}
