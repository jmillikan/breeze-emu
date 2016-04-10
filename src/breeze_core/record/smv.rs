//! Snes9x recording format

// FIXME: It would be nice if we'd have a creation/init function that gets passed a reference to the
// emulator. That way, we could support starting a recording mid-emulation (via save states).

#![allow(dead_code, unused_variables)]  // NYI

use super::WriteSeek;
use input::{Ports, Peripheral};
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
        let input = &snes.peripherals().input;
        fn get_controller_type(port: &Option<Peripheral>) -> u8 {
            match *port {
                None => 0,
                Some(Peripheral::Joypad {..}) => 1,
            }
        }

        // Controller type (port 1 / port 2)
        try!(writer.write_u8(get_controller_type(&input.ports.0)));
        try!(writer.write_u8(get_controller_type(&input.ports.1)));
        // Controller ID (port 1 / port 2) - This probably has to do with the multitap (hence why
        // it's 4 bytes per port)
        // FIXME: We'll just write -1 for unplugged everywhere
        for i in 0..8 {
            try!(writer.write_i8(-1));
        }
        try!(writer.write_all(&[0; 18]));       // 18 bytes reserved for future use

        // Now follows a cartridge RAM image. It's apparently supposed to be gzip compressed and
        // should decompress into 0x20000 bytes. Since we can't gzip shit currently, we'll have to
        // waste all that space.
        try!(writer.write_all(&[0; 0x20000]));   // empty SRAM image

        Ok(Recorder {
            writer: writer,
            frames: 0,
        })
    }

    fn record_frame(&mut self, ports: &Ports) -> io::Result<()> {
        // TODO Record input data
        self.frames += 1;
        unimplemented!()
    }
}

impl Drop for Recorder {
    fn drop(&mut self) {
        info!("finalizing SMV recording ({} frames)", self.frames);

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
