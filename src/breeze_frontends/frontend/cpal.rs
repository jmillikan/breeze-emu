//! CPAL (Cross-Platform Audio Library) audio frontend

use frontend_api::AudioSink;

use cpal::{get_default_endpoint, Voice, SampleFormat, SamplesRate, UnknownTypeBuffer};

pub struct CpalAudio {
    voice: Voice,
}

impl Default for CpalAudio {
    fn default() -> Self {
        // FIXME Don't panic (we need fallible creation)
        let endpoint = get_default_endpoint().expect("Failed to get default endpoint");
        let mut formats = endpoint.get_supported_formats_list().unwrap();
        let format = formats.find(|fmt| {
            fmt.data_type == SampleFormat::I16 && fmt.samples_rate == SamplesRate(32000) &&
            fmt.channels.len() == 2
        }).expect("no support for signed 16-bit data");

        info!("audio format: {:?}", format);

        let voice = Voice::new(&endpoint, &format).expect("Failed to create a channel");

        CpalAudio {
            voice: voice,
        }
    }
}

impl AudioSink for CpalAudio {
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
