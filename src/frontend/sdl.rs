//! Render to an SDL window

use ppu::{SCREEN_WIDTH, SCREEN_HEIGHT};

use sdl2::Sdl;
use sdl2::pixels::PixelFormatEnum;
use sdl2::render::{Renderer, Texture, TextureAccess};

pub struct SdlRenderer {
    renderer: Renderer<'static>,
    texture: Texture,
    sdl: Sdl,
}

impl SdlRenderer {
    /// Creates a new SDL renderer. This also creates a window.
    pub fn new(sdl: &Sdl) -> SdlRenderer {
        info!("initializing SDL renderer");

        // FIXME: Support linear filtering and nearest neighbor

        let video = sdl.video().unwrap();
        let window = video.window("sneeze", SCREEN_WIDTH as u32 * 3, SCREEN_HEIGHT as u32 * 3)
            .build().unwrap();
        let renderer = window.renderer().accelerated().build().unwrap();
        debug!("renderer: {:?}", renderer.info());
        let texture = renderer.create_texture(
            PixelFormatEnum::RGB24,
            TextureAccess::Static,
            (SCREEN_WIDTH as u32, SCREEN_HEIGHT as u32)).unwrap();

        SdlRenderer {
            renderer: renderer,
            texture: texture,
            sdl: sdl.clone(),
        }
    }
}

impl super::Renderer for SdlRenderer {
    fn render(&mut self, frame_data: &[u8]) {
        // FIXME How's render target support? Would save a copy.
        self.texture.update(None, frame_data, SCREEN_WIDTH as usize * 3).unwrap();
        self.renderer.clear();
        self.renderer.copy(&self.texture, None, None);
        self.renderer.present();

        // FIXME Temporary hack
        let mut pump = self.sdl.event_pump().unwrap();
        pump.pump_events();
        while let Some(_) = pump.poll_event() {}
    }
}
