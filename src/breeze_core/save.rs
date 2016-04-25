//! Savestate writing and reading

use snes::Snes;

use libsavestate::SaveState;

use std::io::prelude::*;
use std::io::{self, BufWriter};

/// Enum of supported save state formats
pub enum SaveStateFormat {
    /// ZSNES V0.6 (WIP)
    Zsnes,
    /// Custom binary format (unspecified format, subject to change)
    Custom,
}

impl Default for SaveStateFormat {
    fn default() -> Self {
        SaveStateFormat::Custom
    }
}

impl Snes {
    /// Saves the current emulator state
    pub fn create_save_state(&self, format: SaveStateFormat, w: &mut Write) -> io::Result<()> {
        // Wrap the writer in a `BufWriter` so the caller can't forget it :)
        let mut bufw = BufWriter::new(w);
        match format {
            SaveStateFormat::Zsnes => self.save_zsnes(&mut bufw),
            SaveStateFormat::Custom => self.save_state(&mut bufw),
        }
    }

    pub fn restore_save_state(&mut self, format: SaveStateFormat, r: &mut BufRead) -> io::Result<()> {
        // FIXME Remove `format` parameter when autodetection is implemented (and return the detected type instead)
        match format {
            SaveStateFormat::Zsnes => self.load_zsnes(r),
            SaveStateFormat::Custom => self.restore_state(r),
        }
    }

    fn save_zsnes(&self, w: &mut Write) -> io::Result<()> {
        info!("writing ZSNES save state in .zst format");

        try!(write!(w, "ZSNES Save State File V0.6"));
        try!(w.write_all(&[26]));   // ASCII SUBstitute (note that reading can not rely
                                    // on the text preceding this)
        try!(w.write_all(&[60]));   // version #/100 (= 0.6)

        unimplemented!()
    }

    fn load_zsnes(&mut self, _r: &mut BufRead) -> io::Result<()> {
        unimplemented!()
    }
}
