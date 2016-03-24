//! Custom RLE compressed recording format
//!
//! This works by pushing all bits read by the CPU onto bit vectors, and comparing the data read in
//! each frame with the data from the last frame.
//!
//! Each entry we write to the recording is prefixed by the number of frames after the previous
//! entry the new entry will be activated. This means that (in the general case) we only write
//! something if the input actually changed.
//!
//! Note that this compression isn't perfect and actually produces more data if input changes on
//! every frame, and it depends on the game, so a "malicious" game could make us use an arbitrary
//! amount of RAM by reading the ports over and over. We could probably just impose an arbitrary
//! limit to fix this.

#![allow(dead_code, unused_variables)]    // NYI

use input::Ports;

use std::io::{self, Write, BufRead};

/// Recorder for the custom recording format
pub struct Recorder {
    writer: Box<Write>,
}

impl super::Recorder for Recorder {
    fn new(writer: Box<Write>) -> Self {
        Recorder {
            writer: writer,
        }
    }

    fn record_frame(&mut self, ports: &Ports) -> io::Result<()> {
        unimplemented!()
    }
}

pub struct Replayer {
    reader: Box<BufRead>,
}

impl super::Replayer for Replayer {
    fn new(reader: Box<BufRead>) -> Self {
        Replayer {
            reader: reader,
        }
    }

    fn replay_frame(&mut self, ports: &mut Ports) -> io::Result<()> {
        unimplemented!()
    }
}
