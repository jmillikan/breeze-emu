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
struct SdlManager {
    sdl: Sdl,
    event_pump: EventPump,
    resized_to: Option<(u32, u32)>,
}

impl SdlManager {
    /// Updates all SDL-related state. Polls events and may terminate the process via
    /// `process::exit`. Should be called at least once per frame.
    fn update(&mut self) {
        use sdl2::event::Event::*;
        use sdl2::event::WindowEventId;

        for event in self.event_pump.poll_iter() {
            match event {
                Quit {..} => {
                    info!("quit event -> exiting");
                    process::exit(0);
                }
                Window { win_event_id: WindowEventId::Resized, data1: w, data2: h, .. } => {
                    info!("window resized to {}x{}", w, h);
                    self.resized_to = Some((w as u32, h as u32));
                }
                _ => {}
            }
        }
    }

    fn resized(&mut self) -> Option<(u32, u32)> { self.resized_to.take() }
}

impl Deref for SdlManager {
    type Target = Sdl;
    fn deref(&self) -> &Sdl { &self.sdl }
}

thread_local! {
    static SDL: RefCell<SdlManager> = {
        let sdl = ::sdl2::init().unwrap();
        let pump = sdl.event_pump().unwrap();

        RefCell::new(SdlManager {
            sdl: sdl,
            event_pump: pump,
            resized_to: None,
        })
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

        SDL.with(|sdl_cell| {
            let sdl = sdl_cell.borrow_mut();
            let video = sdl.video().unwrap();
            let window = video.window("sneeze", SCREEN_WIDTH * 3, SCREEN_HEIGHT * 3)
                .resizable()
                .build().unwrap();
            let renderer = window.renderer().accelerated().build().unwrap();
            debug!("renderer: {:?}", renderer.info());
            let texture = renderer.create_texture(
                PixelFormatEnum::RGB24,
                TextureAccess::Static,
                (SCREEN_WIDTH, SCREEN_HEIGHT)).unwrap();

            let mut this = SdlRenderer {
                renderer: renderer,
                texture: texture,
            };
            this.resize_to(SCREEN_WIDTH, SCREEN_HEIGHT);

            this
        })
    }
}

impl super::Renderer for SdlRenderer {
    fn render(&mut self, frame_data: &[u8]) {
        if let Some((w, h)) = SDL.with(|sdl| sdl.borrow_mut().resized()) {
            self.resize_to(w, h)
        }

        // FIXME Can this be done with fewer copies?
        self.texture.update(None, frame_data, SCREEN_WIDTH as usize * 3).unwrap();
        self.renderer.clear();
        self.renderer.copy(&self.texture, None, None);
        self.renderer.present();

        SDL.with(|sdl| sdl.borrow_mut().update());
    }
}

impl SdlRenderer {
    /// Handle a window resize to `w, h`
    fn resize_to(&mut self, w: u32, h: u32) {
        use sdl2::rect::Rect;

        let w = w as f32;
        let h = h as f32;

        const NATIVE_RATIO: f32 = SCREEN_WIDTH as f32 / SCREEN_HEIGHT as f32;
        let ratio = w / h;

        let view_w;
        let view_h;

        if ratio > NATIVE_RATIO {
            // Too wide
            view_h = h;
            view_w = h * NATIVE_RATIO;
        } else {
            // Too high
            view_w = w;
            view_h = w / NATIVE_RATIO;
        }

        let border_x = (w - view_w).round() as u32 / 2;
        let border_y = (h - view_h).round() as u32 / 2;
        let view_w = view_w.round() as u32;
        let view_h = view_h.round() as u32;

        let viewport = Rect::new(border_x as i32, border_y as i32, view_w, view_h).unwrap();
        self.renderer.set_viewport(viewport);

        info!("window ratio is {:.2} (native: {:.2}), viewport {}x{}, border ({},{})",
            ratio, NATIVE_RATIO, view_w, view_h, border_x, border_y);
    }
}
