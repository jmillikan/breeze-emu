//! Emulates the DSP used in the APU.

#[derive(Copy, Clone, Default)]
struct Voice {
    /// $x0 - Left channel volume
    lvol: i8,
    /// $x1 - Right channel volume
    rvol: i8,
    /// $x2 (low)/x3 (high)  (14 bit)
    pitch: u16,
    /// $x4 - Source number (source directory table entry)
    source: u8,
    /// $x5
    adsr1: u8,
    /// $x6
    adsr2: u8,
    /// $x7
    gain: u8,
    /// $x8 - Read-only: Current envelope value
    env: u8,
    /// $x9 - Read-only: Waveform value after envelope multiplication, before volume multiplication
    out: u8,
    /// $xf - 8-tap FIR filter coefficients
    fir: u8,
}

pub struct Dsp {
    voices: [Voice; 8],
    /// $0c - Left main volume
    lmvol: u8,
    /// $1c - Right main volume
    rmvol: u8,
    /// $2c - Left echo volume
    levol: u8,
    /// $3c - Right echo volume
    revol: u8,
    /// $4c - Key on (1 bit per voice)
    keyon: u8,
    /// $5c - Key off (1 bit per voice)
    keyoff: u8,
    /// $6c
    flags: u8,
    /// $7c - Voice end flags (1 bit per voice)
    endx: u8,
    /// $0d - Echo feedback
    efb: u8,
    /// $2d - Pitch modulation
    pmod: u8,
    /// $3d - Noise enable
    noise: u8,
    /// $4d - Echo enable
    echo: u8,
    /// $5d - Sample table address (`* $100` for memory offset)
    srcdir: u8,
    /// $6d - Echo buffer start offset (`* $100` for memory offset)
    echo_buf: u8,
    /// $7d - Echo delay (4 bits only!)
    echo_delay: u8,
}

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
    pub fn store(&mut self, mut reg: u8, value: u8) {
        trace!("DSP STORE: ${:02X} to ${:02X}", value, reg);
        reg &= 0x7f;
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
                    0x08 => panic!("can't write to envelope value"),
                    0x09 => panic!("can't write to sample value"),
                    0x0f => voice.fir = value,
                    _ => panic!("invalid DSP write to ${:02X}", reg),
                }
            }
        }
    }
}
