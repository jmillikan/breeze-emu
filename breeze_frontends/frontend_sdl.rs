//! Render to an SDL window

use frontend_api::FrontendAction;
use frontend_api::input::joypad::{JoypadImpl, JoypadState, JoypadButton};
use frontend_api::ppu::{SCREEN_WIDTH, SCREEN_HEIGHT};

use sdl2::{EventPump, Sdl};
use sdl2::event::WindowEventId;
use sdl2::keyboard::Scancode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::render::{Renderer, Texture, TextureAccess};
use sdl2::rect::Rect;

use std::cell::RefCell;
use std::ops::Deref;

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
    fn update(&mut self) -> Option<FrontendAction> {
        use sdl2::event::Event::*;

        for event in self.event_pump.poll_iter() {
            match event {
                Quit { .. } => {
                    info!("quit event -> exiting");
                    return Some(FrontendAction::Exit);
                }
                Window { win_event_id: WindowEventId::Resized, data1: w, data2: h, .. } => {
                    info!("window resized to {}x{}", w, h);
                    self.resized_to = Some((w as u32, h as u32));
                }
                KeyDown { scancode: Some(Scancode::F5), .. } => {
                    return Some(FrontendAction::SaveState);
                }
                KeyDown { scancode: Some(Scancode::F9), .. } => {
                    return Some(FrontendAction::LoadState);
                }
                _ => {}
            }
        }

        if self.event_pump.keyboard_state().is_scancode_pressed(Scancode::LCtrl) {
            info!("<waiting>");
            for event in self.event_pump.wait_iter() {
                match event {
                    KeyUp { scancode: Some(Scancode::LCtrl), .. } => { break }
                    _ => {}
                }
            }
            info!("<running>");
        }

        None
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
        // FIXME: Support linear filtering and nearest neighbor

        SDL.with(|sdl_cell| {
            let sdl = sdl_cell.borrow_mut();
            let video = sdl.video().unwrap();
            let window = video.window("breeze", SCREEN_WIDTH * 3, SCREEN_HEIGHT * 3)
                .resizable()
                .build().unwrap();
            let renderer = window.renderer().accelerated().build().unwrap();
            let texture = renderer.create_texture(
                PixelFormatEnum::RGB24,
                TextureAccess::Static,
                (SCREEN_WIDTH, SCREEN_HEIGHT)).unwrap();

            let mut this = SdlRenderer {
                renderer: renderer,
                texture: texture,
            };
            this.resize_to(SCREEN_WIDTH * 3, SCREEN_HEIGHT * 3);

            this
        })
    }
}

impl ::frontend_api::Renderer for SdlRenderer {
    fn render(&mut self, frame_data: &[u8]) -> Option<FrontendAction> {
        if let Some((w, h)) = SDL.with(|sdl| sdl.borrow_mut().resized()) {
            self.resize_to(w, h)
        }

        // FIXME Can this be done with fewer copies?
        self.texture.update(None, frame_data, SCREEN_WIDTH as usize * 3).unwrap();
        self.renderer.clear();
        self.renderer.copy(&self.texture, None, None);
        self.renderer.present();

        SDL.with(|sdl| sdl.borrow_mut().update())
    }

    fn set_rom_title(&mut self, title: &str) {
        match self.renderer.window_mut() {
            Some(win) => win.set_title(title),
            None => {},
        }
    }
}

impl SdlRenderer {
    /// Handle a window resize to `w, h`
    fn resize_to(&mut self, w: u32, h: u32) {
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

#[allow(dead_code)]
pub struct KeyboardInput;

macro_rules! impl_fn {
    ( $btn:ident = $key:ident ) => {
        fn $btn(&mut self) -> bool {
            SDL.with(|sdl_cell| {
                let sdl = sdl_cell.borrow();
                let state = sdl.event_pump.keyboard_state();
                state.is_scancode_pressed(::sdl2::keyboard::Scancode::$key)
            })
        }
    };
}

impl JoypadImpl for KeyboardInput {
    fn update_state(&mut self) -> JoypadState {
        use sdl2::keyboard::Scancode::*;

        SDL.with(|sdl_cell| {
            let mut joypad = JoypadState::new();
            {
                // Fetch input state
                let sdl = sdl_cell.borrow();
                let state = sdl.event_pump.keyboard_state();

                // These bindings somewhat resemble an actual SNES controller:
                // Q W           I O P
                // A S D   G H   K L
                // -------------------
                // L ↑           Y X R
                // < ↓ > Sel Sta B A

                if state.is_scancode_pressed(W) { joypad.set(JoypadButton::Up, true); }
                if state.is_scancode_pressed(A) { joypad.set(JoypadButton::Left, true); }
                if state.is_scancode_pressed(S) { joypad.set(JoypadButton::Down, true); }
                if state.is_scancode_pressed(D) { joypad.set(JoypadButton::Right, true); }

                if state.is_scancode_pressed(G) { joypad.set(JoypadButton::Select, true); }
                if state.is_scancode_pressed(H) { joypad.set(JoypadButton::Start, true); }

                if state.is_scancode_pressed(L) { joypad.set(JoypadButton::A, true); }
                if state.is_scancode_pressed(K) { joypad.set(JoypadButton::B, true); }
                if state.is_scancode_pressed(O) { joypad.set(JoypadButton::X, true); }
                if state.is_scancode_pressed(I) { joypad.set(JoypadButton::Y, true); }

                if state.is_scancode_pressed(P) { joypad.set(JoypadButton::R, true); }
                if state.is_scancode_pressed(Q) { joypad.set(JoypadButton::L, true); }
            }

            joypad
        })
    }
}
