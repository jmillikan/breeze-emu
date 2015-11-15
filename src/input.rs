//! Emulates the controller ports `$4016 - $401f`

#![allow(dead_code)]    // FIXME: Maybe remove sometime

use frontend::{FrontendAction, InputSource};

use std::error::Error;
use std::io::{self, BufRead, Read, Write};
use std::str;

/// Controller input management.
#[derive(Default)]
pub struct Input {
    pub sources: [Option<Box<InputSource>>; 4],
    states: [InputState; 4],
    /// Reset on frame render, set on lazy input update. Lazy input update is done the first time
    /// the game reads any controller state in the current frame and is supposed to improve input
    /// latency.
    updated_this_frame: bool,

    /// Bit in the input state (where 0 = MSb, 15 = LSb) returned by next $4016/$4017 read resp.
    bitpos4016: u8,
    bitpos4017: u8,

    recorder: Option<Recorder>,
    replayer: Option<Replayer>,
}

impl_save_state!(Input {
    states, updated_this_frame, bitpos4016, bitpos4017
} ignore { sources, recorder, replayer });

impl Input {
    /// Start recording input to a `Write` implementor, often a file.
    ///
    /// Every time input is updated (once a frame), the recorder will write to the given
    /// `Box<Write>`.
    pub fn start_recording(&mut self, w: Box<Write>) {
        assert!(self.recorder.is_none(), "already recording input");
        self.recorder = Some(Recorder::new(w));
    }

    pub fn start_replay(&mut self, r: Box<BufRead>) {
        assert!(self.replayer.is_none(), "already replaying input");
        self.replayer = Some(Replayer::new(r));
    }

    /// Called on V-Blank. Ensures that `update` was called this frame, and enables lazy input
    /// update for the following frame.
    pub fn new_frame(&mut self) {
        if !self.updated_this_frame { self.update(); }
        self.updated_this_frame = false;
    }

    /// Polls all controllers and stores their state. Called lazily when the game reads from an
    /// input register.
    fn update(&mut self) -> Vec<FrontendAction> {
        let mut actions = Vec::new();

        self.updated_this_frame = true;
        for i in 0..4 {
            if let Some(ref mut source) = self.sources[i] {
                let result = source.poll();
                self.states[i] = result.result;

                if let Some(action) = result.action { actions.push(action) }
            }
        }

        // Overwrite `self.states` if replaying
        if let Some(ref mut replayer) = self.replayer {
            self.states = replayer.update().unwrap();
            return actions;
        }

        // Update the recorder. This is a bit of a mess because we set it to `None` on errors
        // (borrow checker).
        let recorder = match self.recorder.take() {
            Some(mut rec) => {
                if let Err(e) = rec.update(&self.states) {
                    error!("recording error: {}", e);
                    None
                } else {
                    Some(rec)
                }
            }
            None => None
        };
        self.recorder = recorder;

        actions
    }

    /// Read from an input register. Updates the controller state if this is the first load in this
    /// frame.
    pub fn load(&mut self, reg: u16) -> u8 {
        // Do we still need to fetch this frame's input data?
        if !self.updated_this_frame { self.update(); }

        match reg {
            0x4016 => {
                let bitpos = self.bitpos4016;
                if bitpos == 16 { 1 } else {
                    // Read controller 1 and 3
                    let a = self.states[0].0 & (0x8000 >> bitpos as u16) != 0;
                    let c = self.states[2].0 & (0x8000 >> bitpos as u16) != 0;
                    self.bitpos4016 += 1;

                    (if c {0x02} else {0x00}) | (if a {0x01} else {0x00})
                }
            }
            0x4017 => {
                let bitpos = self.bitpos4017;
                if bitpos == 16 { 1 } else {
                    // Read controller 2 and 4
                    let a = self.states[1].0 & (0x8000 >> bitpos as u16) != 0;
                    let c = self.states[3].0 & (0x8000 >> bitpos as u16) != 0;
                    self.bitpos4017 += 1;

                    (if c {0x02} else {0x00}) | (if a {0x01} else {0x00})
                }
            }
            0x4218 ... 0x421f => {
                // "Full" read
                let controller = (reg - 0x4218) / 2;
                let hi = reg & 1 != 0;
                let state = &self.states[controller as usize];
                match hi {
                    false => state.0 as u8,
                    true => (state.0 >> 8) as u8,
                }
            }
            _ => panic!("${:04X} is not an input register", reg)
        }
    }

    /// Store to an input register. Will just latch the serial input.
    pub fn store(&mut self, reg: u16, val: u8) {
        if reg == 0x4016 {
            if val & 0x01 != 0 {
                // Strobe joypads
                self.bitpos4016 = 0;
                self.bitpos4017 = 0;
            }
        } else {
            panic!("invalid input reg store to ${:04X}", reg);
        }
    }
}

/// State of a SNES joypad. The low byte can be read from `$4218`, the high byte from `$4219` (for
/// controller 1).
///
/// Bits (`HIGH | LOW`):
/// `B Y Select Start Up Down Left Right | A X L R 0 0 0 0`
#[derive(Clone, Copy, Default)]
pub struct InputState(u16);

impl_save_state_for_newtype!(InputState);

const BIT_A: u8 = 7;
const BIT_B: u8 = 15;
const BIT_X: u8 = 6;
const BIT_Y: u8 = 14;

const BIT_L: u8 = 5;
const BIT_R: u8 = 4;

const BIT_START: u8 = 12;
const BIT_SELECT: u8 = 13;

const BIT_UP: u8 = 11;
const BIT_DOWN: u8 = 10;
const BIT_LEFT: u8 = 9;
const BIT_RIGHT: u8 = 8;

impl InputState {
    pub fn new() -> Self { Self::default() }

    fn set(&mut self, bit: u8, value: bool) -> &mut Self {
        match value {
            true => self.0 |= 1 << bit,
            false => self.0 &= !(1 << bit),
        };
        self
    }

    pub fn a(&mut self, pressed: bool) -> &mut Self      { self.set(BIT_A, pressed) }
    pub fn b(&mut self, pressed: bool) -> &mut Self      { self.set(BIT_B, pressed) }
    pub fn x(&mut self, pressed: bool) -> &mut Self      { self.set(BIT_X, pressed) }
    pub fn y(&mut self, pressed: bool) -> &mut Self      { self.set(BIT_Y, pressed) }

    pub fn l(&mut self, pressed: bool) -> &mut Self      { self.set(BIT_L, pressed) }
    pub fn r(&mut self, pressed: bool) -> &mut Self      { self.set(BIT_R, pressed) }

    pub fn start(&mut self, pressed: bool) -> &mut Self  { self.set(BIT_START, pressed) }
    pub fn select(&mut self, pressed: bool) -> &mut Self { self.set(BIT_SELECT, pressed) }

    pub fn up(&mut self, pressed: bool) -> &mut Self     { self.set(BIT_UP, pressed) }
    pub fn down(&mut self, pressed: bool) -> &mut Self   { self.set(BIT_DOWN, pressed) }
    pub fn left(&mut self, pressed: bool) -> &mut Self   { self.set(BIT_LEFT, pressed) }
    pub fn right(&mut self, pressed: bool) -> &mut Self  { self.set(BIT_RIGHT, pressed) }
}

struct Recorder {
    out: Box<Write>,
    last_state: [InputState; 4],
    frame: u64,
}

impl Recorder {
    fn new(out: Box<Write>) -> Self {
        Recorder {
            out: out,
            last_state: [InputState::new(); 4],
            frame: 0,
        }
    }

    /// Update the recording with input data for a new frame
    fn update(&mut self, new_state: &[InputState; 4]) -> io::Result<()> {
        const BIT_NAMES: &'static [&'static str] = &[
            "", "", "", "", "R", "L", "X", "A", ">", "<", "v", "^", "Start", "Select", "Y", "B"
        ];

        let mut changed = false;
        for i in 0..4 {
            let last = self.last_state[i].0;
            let new = new_state[i].0;
            if last != new {
                if !changed { try!(write!(self.out, "{}:", self.frame)); }
                changed = true;

                let diff = last ^ new;

                for bit in 4..16 {
                    if diff & (1 << bit) != 0 {
                        // Bit changed
                        if last & (1 << bit) != 0 {
                            // Released
                            try!(write!(self.out, "-{}/{};", i, BIT_NAMES[bit]));
                        } else {
                            // Pressed
                            try!(write!(self.out, "+{}/{};", i, BIT_NAMES[bit]));
                        }
                    }
                }
            }
        }

        if changed { try!(write!(self.out, "\n")); }

        self.frame += 1;
        self.last_state = *new_state;
        Ok(())
    }
}

struct Replayer {
    read: Box<BufRead>,
    last_state: [InputState; 4],
    frame: u64,
    /// Next frame with changed input
    next_frame: u64,
    /// Input change for `next_frame` (XOR)
    next_state: [u16; 4],
}

impl Replayer {
    fn new(read: Box<BufRead>) -> Self {
        Replayer {
            read: read,
            last_state: [InputState::new(); 4],
            frame: 0,
            next_frame: 0,
            next_state: [0; 4],
        }
    }

    fn read_next_line(&mut self) -> Result<(), Box<Error>> {
        // Now read and parse the next line
        let mut buf = Vec::new();
        try!(self.read.read_until(b':', &mut buf));
        if buf.is_empty() {
            // Replay ended
            self.next_frame = !0;
            return Ok(());
        }

        self.next_frame = try!(try!(str::from_utf8(&buf[..buf.len()-1])).parse());
        buf = Vec::new();

        try!(self.read.read_until(b'\n', &mut buf));
        let line = try!(str::from_utf8(&buf));
        for entry in line.split(';') {
            if entry == "\n" { break }    // last one is empty
            let polarity = entry.chars().next().unwrap();

            let polarity = match polarity {
                '+' => true,
                '-' => false,
                _ => return Err(Box::new(io::Error::new(io::ErrorKind::Other,
                    "invalid button polarity"))),
            };

            let mut split = entry[1..].split('/');

            let controller: u8 = split.next().unwrap().parse().unwrap();

            let bit = match split.next().unwrap() {
                "A" => BIT_A,
                "B" => BIT_B,
                "X" => BIT_X,
                "Y" => BIT_Y,
                "L" => BIT_L,
                "R" => BIT_R,
                "Start" => BIT_START,
                "Select" => BIT_SELECT,
                "^" => BIT_UP,
                "v" => BIT_DOWN,
                "<" => BIT_LEFT,
                ">" => BIT_RIGHT,
                _ => return Err(Box::new(io::Error::new(io::ErrorKind::Other,
                    "invalid button name"))),
            };

            // Build `next_state`
            let state = &mut self.next_state[controller as usize];
            match polarity {
                true => *state &= !(1 << bit),
                false => *state |= 1 << bit,
            }
        }

        Ok(())
    }

    /// Fetches the input for the next frame from the recording
    fn update(&mut self) -> Result<[InputState; 4], Box<Error>> {
        if self.frame == self.next_frame {
            // This frame needs changes, apply them:
            for i in 0..4 {
                self.last_state[i].0 = self.next_state[i];
            }

            try!(self.read_next_line());
        }
        self.frame += 1;

        Ok(self.last_state)
    }
}
