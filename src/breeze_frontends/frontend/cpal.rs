//! CPAL (Cross-Platform Audio Library) audio frontend

extern crate cpal;

use frontend_api::{FrontendResult, AudioSink};

use self::cpal::{get_default_endpoint, Voice, SampleFormat, SamplesRate, UnknownTypeBuffer};

pub struct CpalAudio {
    voice: Voice,
}

impl AudioSink for CpalAudio {
    fn create() -> FrontendResult<Self> {
        let endpoint = match get_default_endpoint() {
            Some(ep) => ep,
            None => return Err("Failed to get default endpoint".into()),
        };
        let formats = try!(endpoint.get_supported_formats_list()).collect::<Vec<_>>();
        for fmt in &formats {
            debug!("supported format: {:?}", fmt);
        }

        let format = match formats.iter().find(|fmt| {
            fmt.data_type == SampleFormat::I16 && fmt.samples_rate == SamplesRate(32000) &&
            fmt.channels.len() == 2
        }) {
            Some(fmt) => fmt,
            None => return Err("no support for signed 16-bit data".into()),
        };

        info!("audio format: {:?}", format);

        let voice = try!(Voice::new(&endpoint, &format));

        Ok(CpalAudio {
            voice: voice,
        })
    }

    fn write(&mut self, mut data: &[(i16, i16)]) {
        while !data.is_empty() {
            match self.voice.append_data(data.len() * 2) {
                UnknownTypeBuffer::I16(mut buffer) => {
                    for out in buffer.chunks_mut(2) {
                        let (first, rest) = data.split_first().unwrap();
                        out[0] = first.0;
                        out[1] = first.1;
                        data = rest;
                    }
                },
                _ => unimplemented!(),
            }
        }
    }
}
