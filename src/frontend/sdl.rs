//! Render to an SDL window

use ppu::{SCREEN_WIDTH, SCREEN_HEIGHT};

use sdl2::{EventPump, Sdl};
use sdl2::pixels::PixelFormatEnum;
use sdl2::render::{Renderer, Texture, TextureAccess};

use std::cell::RefCell;
use std::ops::Deref;
use std::process;

/// Takes care of SDL (mainly used for event management). Owns an `EventPump`, which makes it
/// unavailable for other code. Initialized when the emulator uses an SDL frontend.
struct SdlManager(Sdl, RefCell<EventPump>);

impl SdlManager {
    /// Updates all SDL-related state. Polls events and may terminate the process via
    /// `process::exit`. Should be called at least once per frame.
    fn update(&self) {
        use sdl2::event::Event::*;

        for event in self.1.borrow_mut().poll_iter() {
            match event {
                Quit {..} => {
                    info!("quit event -> exiting");
                    process::exit(0);
                }
                _ => {}
            }
        }
    }
}

impl Deref for SdlManager {
    type Target = Sdl;
    fn deref(&self) -> &Sdl { &self.0 }
}

thread_local! {
    static SDL: SdlManager = {
        let sdl = ::sdl2::init().unwrap();
        let pump = sdl.event_pump().unwrap();

        SdlManager(sdl, RefCell::new(pump))
    }
}

pub struct SdlRenderer {
    renderer: Renderer<'static>,
    texture: Texture,
}

impl Default for SdlRenderer {
    fn default() -> Self {
        info!("initializing SDL renderer");

        // FIXME: Support linear filtering and nearest neighbor

        SDL.with(|sdl| {
            let video = sdl.video().unwrap();
            let window = video.window("sneeze", SCREEN_WIDTH * 3, SCREEN_HEIGHT * 3)
                .build().unwrap();
            let renderer = window.renderer().accelerated().build().unwrap();
            debug!("renderer: {:?}", renderer.info());
            let texture = renderer.create_texture(
                PixelFormatEnum::RGB24,
                TextureAccess::Static,
                (SCREEN_WIDTH, SCREEN_HEIGHT)).unwrap();

            SdlRenderer {
                renderer: renderer,
                texture: texture,
            }
        })
    }
}

impl super::Renderer for SdlRenderer {
    fn render(&mut self, frame_data: &[u8]) {
        // FIXME Can this be done with fewer copies?
        self.texture.update(None, frame_data, SCREEN_WIDTH as usize * 3).unwrap();
        self.renderer.clear();
        self.renderer.copy(&self.texture, None, None);
        self.renderer.present();

        SDL.with(|sdl| sdl.update())
    }
}
