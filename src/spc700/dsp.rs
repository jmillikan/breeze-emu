//! Emulates the DSP used in the APU.

#![allow(dead_code)]    // FIXME Implement the DSP

#[derive(Copy, Clone, Default)]
struct Voice {
    // Registers

    /// $x0 - Left channel volume
    lvol: i8,
    /// $x1 - Right channel volume
    rvol: i8,
    /// $x2 (low)/x3 (high)  (14 bit)
    pitch: u16,
    /// $x4 - Source number (source directory table entry)
    source: u8,
    /// $x5 - VxADSR1
    adsr1: u8,
    /// $x6 - VxADSR2
    adsr2: u8,
    /// $x7 - VxGAIN
    gain: u8,
    /// $x8 - VxENVX - Read-only: Current envelope value (0-127)
    env: u8,
    /// $x9 - VxOUTX - Read-only: Upper 8bit of the current 15bit sample value (-128..+127)
    out: u8,
    /// $xf - 8-tap FIR filter coefficients
    fir: u8,
}

impl_save_state!(Voice { lvol, rvol, pitch, source, adsr1, adsr2, gain, env, out, fir } ignore {});

pub struct Dsp {
    voices: [Voice; 8],
    /// $0c - Left main volume
    lmvol: u8,
    /// $1c - Right main volume
    rmvol: u8,
    /// $2c - EVOLL: Left echo volume
    levol: u8,
    /// $3c - EVOLR: Right echo volume
    revol: u8,
    /// $4c - KON: Key on (1 bit per voice)
    ///
    /// "Writing 1 to the KON bit will set the envelope to 0, the state to
    /// Attack, and will start the channel from the beginning (see DIR and
    /// VxSRCN). Note that this happens even if the channel is already playing
    /// (which may cause a click/pop), and that there are 5 'empty' samples
    /// before envelope updates and BRR decoding actually begin."
    keyon: u8,
    /// $5c - KOFF: Key off (1 bit per voice)
    ///
    /// "Setting 1 to the KOFF bit will transition the voice to the Release
    /// state. Thus, the envelope will decrease by 8 every sample (regardless
    /// of the VxADSR and VxGAIN settings) until it reaches 0, where it will
    /// stay until the next KON."
    keyoff: u8,
    /// $6c - FLG: Reset, Mute, Echo-Write flags and Noise Clock
    flags: u8,
    /// $7c - ENDX: Voice end flags (1 bit per voice)
    endx: u8,
    /// $0d - EFB: Echo feedback
    efb: u8,
    /// $2d - PMON: Pitch modulation
    pmod: u8,
    /// $3d - NON: Noise enable
    noise: u8,
    /// $4d - Echo enable
    echo: u8,
    /// $5d - Sample table address (`* $100` for memory offset)
    srcdir: u8,
    /// $6d - ESA: Echo ring buffer start offset (`* $100` for memory offset)
    echo_buf: u8,
    /// $7d - EDL: Echo delay (ring buffer size) (4 bits only!)
    echo_delay: u8,
}

impl_save_state!(Dsp { voices, lmvol, rmvol, levol, revol, keyon, keyoff, flags, endx, efb, pmod,
    noise, echo, srcdir, echo_buf, echo_delay } ignore {});

impl Dsp {
    pub fn new() -> Dsp {
        Dsp {
            voices: [Voice::default(); 8],
            lmvol: 0,
            rmvol: 0,
            levol: 0,
            revol: 0,
            keyon: 0,
            keyoff: 0,
            flags: 0xe0,
            endx: 0xff,
            efb: 0,
            pmod: 0,
            noise: 0,
            echo: 0,
            srcdir: 0,
            echo_buf: 0,
            echo_delay: 0,
        }
    }

    /// Load a value from a DSP register
    pub fn load(&mut self, mut reg: u8) -> u8 {
        reg &= 0x7f;
        match reg {
            0x0c => self.lmvol,
            0x1c => self.rmvol,
            0x2c => self.levol,
            0x3c => self.revol,
            0x4c => self.keyon,
            0x5c => self.keyoff,
            0x6c => self.flags,
            0x7c => self.endx,
            0x0d => self.efb,
            0x2d => self.pmod,
            0x3d => self.noise,
            0x4d => self.echo,
            0x5d => self.srcdir,
            0x6d => self.echo_buf,
            0x7d => self.echo_delay,
            _ => {
                let voice = &self.voices[(reg >> 4) as usize];
                match reg & 0x0f {
                    0x00 => voice.lvol as u8,
                    0x01 => voice.rvol as u8,
                    0x02 => voice.pitch as u8,
                    0x03 => (voice.pitch >> 8) as u8,
                    0x04 => voice.source,
                    0x05 => voice.adsr1,
                    0x06 => voice.adsr2,
                    0x07 => voice.gain,
                    0x08 => voice.env,
                    0x09 => voice.out,
                    0x0f => voice.fir,
                    _ => panic!("invalid DSP read from ${:02X}", reg),
                }
            }
        }
    }

    /// Store a value in a DSP register
    pub fn store(&mut self, reg: u8, value: u8) {
        match reg {
            0x0c => self.lmvol = value,
            0x1c => self.rmvol = value,
            0x2c => self.levol = value,
            0x3c => self.revol = value,
            0x4c => self.keyon = value,
            0x5c => self.keyoff = value,
            0x6c => self.flags = value,
            0x7c => self.endx = value,
            0x0d => self.efb = value,
            0x2d => self.pmod = value,
            0x3d => self.noise = value,
            0x4d => self.echo = value,
            0x5d => self.srcdir = value,
            0x6d => self.echo_buf = value,
            0x7d => self.echo_delay = value,
            _ => {
                let voice = &mut self.voices[(reg >> 4) as usize];
                match reg & 0x0f {
                    0x00 => voice.lvol = value as i8,
                    0x01 => voice.rvol = value as i8,
                    0x02 => voice.pitch = (voice.pitch & 0xff00) | value as u16,
                    0x03 => voice.pitch = (voice.pitch & 0x00ff) | ((value as u16) << 8),
                    0x04 => voice.source = value,
                    0x05 => voice.adsr1 = value,
                    0x06 => voice.adsr2 = value,
                    0x07 => voice.gain = value,
                    0x08 => once!(warn!("ignoring write to envelope value")),
                    0x09 => once!(warn!("ignoring write to sample value")),
                    0x0f => voice.fir = value,
                    _ => panic!("invalid DSP write to ${:02X}", reg),
                }
            }
        }
    }
}

enum BrrLoop {
    /// Continue playing with the next BRR block
    Continue,
    /// Jump to the block's loop address and set the ENDx flag for this voice
    Loop,
    /// Jump to the loop address, set ENDx, enter `Release`, set env to $000
    Release,
}

struct BrrBlock {
    /// 0-12 where 0 = silent and 12 = loudest
    shift: u8,
    /// 0-3, 0 = no filter
    filter: u8,
}
