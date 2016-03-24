//! Snes9x recording format

#![allow(dead_code, unused_variables)]  // NYI

use input::Ports;

use std::io::{self, Write, BufRead};

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
