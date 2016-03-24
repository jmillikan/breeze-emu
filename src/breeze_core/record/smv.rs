//! Snes9x recording format

// FIXME: It would be nice if we'd have a creation/init function that gets passed a reference to the
// emulator. That way, we could support starting a recording mid-emulation (via save states).

#![allow(dead_code, unused_variables)]  // NYI

use super::WriteSeek;
use input::Ports;
use snes::Snes;

use byteorder::{LittleEndian, WriteBytesExt};

use std::io::{self, Write, BufRead, SeekFrom};

pub struct Recorder {
    writer: Box<WriteSeek>,
    frames: u32,
}

impl super::Recorder for Recorder {
    fn new(mut writer: Box<WriteSeek>, snes: &Snes) -> io::Result<Self> {
        // Write SMV header
        try!(write!(writer, "SMV\x1A"));
        try!(writer.write_u32::<LittleEndian>(4));  // SMV Version
        try!(writer.write_u32::<LittleEndian>(0));  // uid (Unix timestamp in Snes9x)
        try!(writer.write_u32::<LittleEndian>(0));  // rerecord count (no idea what this does)
        try!(writer.write_u32::<LittleEndian>(0xdeadbeef));    // Number of frames
        // The actual number of frames is written when the recorder is dropped
        try!(writer.write_u8(0));  // controller mask (FIXME)
        try!(writer.write_u8(1));  // movie opts
        try!(writer.write_u8(0));  // sync opts
        try!(writer.write_u8(0));  // more (actual) sync opts
        try!(writer.write_u32::<LittleEndian>(0x40));  // offset to save state / cart. RAM image
        try!(writer.write_u32::<LittleEndian>(0x40 + 0x20000));  // offset to controller data

        // Write extended header (+32 Bytes)
        try!(writer.write_u32::<LittleEndian>(0xdeadbeef));  // num. of input samples

        // Controller info (for port 1 / port 2)
        // TODO

        Ok(Recorder {
            writer: writer,
            frames: 0,
        })
    }

    fn record_frame(&mut self, ports: &Ports) -> io::Result<()> {
        self.frames += 1;
        unimplemented!()
    }
}

impl Drop for Recorder {
    fn drop(&mut self) {
        // FIXME At least warn when this fails
        self.writer.seek(SeekFrom::Start(16)).ok();
        self.writer.write_u32::<LittleEndian>(self.frames).ok();
        self.writer.seek(SeekFrom::Start(32)).ok();
        // FIXME This should be the total number of input samples (might be higher when other
        // peripherals attached)
        self.writer.write_u32::<LittleEndian>(self.frames).ok();
    }
}

pub struct Replayer {
    reader: Box<BufRead>,
}

impl super::Replayer for Replayer {
    fn new(reader: Box<BufRead>, _snes: &Snes) -> io::Result<Self> {
        Ok(Replayer {
            reader: reader,
        })
    }

    fn replay_frame(&mut self, ports: &mut Ports) -> io::Result<()> {
        unimplemented!()
    }
}
