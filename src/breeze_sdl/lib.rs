//! Render to an SDL window

#[macro_use] extern crate log;
extern crate breeze_backend;
extern crate sdl2;
extern crate libc;

use breeze_backend::{BackendAction, BackendResult};
use breeze_backend::input::joypad::{JoypadImpl, JoypadState, JoypadButton};
use breeze_backend::ppu::{SCREEN_WIDTH, SCREEN_HEIGHT};
use breeze_backend::viewport::Viewport;

use sdl2::{EventPump, Sdl};
use sdl2::event::WindowEventId;
use sdl2::keyboard::Scancode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::render::{Renderer, Texture, TextureAccess};
use sdl2::rect::Rect;

use std::cell::RefCell;
use std::error::Error;
use std::ops::Deref;

/// Signal handler saving/restoring when initializing SDL
mod signal {
    #[cfg(unix)]
    use super::libc::*;

    #[cfg(unix)]
    pub type SavedSignals = [sighandler_t; 2];
    #[cfg(not(unix))]
    pub type SavedSignals = ();

    #[cfg(unix)]
    pub fn save_handlers() -> SavedSignals {
        let sigint = unsafe { signal(SIGINT, SIG_DFL) };
        let sigterm = unsafe { signal(SIGTERM, SIG_DFL) };
        [sigint, sigterm]
    }

    #[cfg(not(unix))]
    pub fn save_handlers() -> SavedSignals {}

    #[cfg(unix)]
    pub fn restore_handlers(saved: SavedSignals) {
        unsafe {
            signal(SIGINT, saved[0]);
            signal(SIGTERM, saved[1]);
        }
    }

    #[cfg(not(unix))]
    pub fn restore_handlers(_: SavedSignals) {}
}

/// Takes care of SDL (mainly used for event management). Owns an `EventPump`, which makes it
/// unavailable for other code. Initialized when the emulator uses an SDL backend.
struct SdlManager {
    sdl: Sdl,
    event_pump: EventPump,
    resized_to: Option<(u32, u32)>,
}

impl SdlManager {
    /// Updates all SDL-related state. Polls the event loop.
    ///
    /// Should be called at least once per frame.
    fn update(&mut self) -> BackendResult<Vec<BackendAction>> {
        use self::sdl2::event::Event::*;

        for event in self.event_pump.poll_iter() {
            match event {
                Quit { .. } => {
                    info!("quit event -> exiting");
                    return Ok(vec![BackendAction::Exit]);
                }
                Window { win_event_id: WindowEventId::Resized, data1: w, data2: h, .. } => {
                    info!("window resized to {}x{}", w, h);
                    self.resized_to = Some((w as u32, h as u32));
                }
                KeyDown { scancode: Some(Scancode::F5), .. } => {
                    return Ok(vec![BackendAction::SaveState]);
                }
                KeyDown { scancode: Some(Scancode::F9), .. } => {
                    return Ok(vec![BackendAction::LoadState]);
                }
                _ => {}
            }
        }

        Ok(vec![])
    }

    fn resized(&mut self) -> Option<(u32, u32)> { self.resized_to.take() }
}

impl Deref for SdlManager {
    type Target = Sdl;
    fn deref(&self) -> &Sdl { &self.sdl }
}

thread_local! {
    static SDL: RefCell<SdlManager> = {
        let signals = signal::save_handlers();
        let sdl = sdl2::init().unwrap();
        let pump = sdl.event_pump().unwrap();
        signal::restore_handlers(signals);

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

impl ::breeze_backend::Renderer for SdlRenderer {
    fn create() -> Result<Self, Box<Error>> {
        SDL.with(|sdl_cell| {
            let sdl = sdl_cell.borrow_mut();
            let video = try!(sdl.video());
            let window = try!(video.window("breeze", SCREEN_WIDTH * 3, SCREEN_HEIGHT * 3)
                .resizable()
                .build());
            let renderer = try!(window.renderer()
                .accelerated()
                .present_vsync()
                .build());
            let texture = try!(renderer.create_texture(
                PixelFormatEnum::RGB24,
                TextureAccess::Static,
                SCREEN_WIDTH,
                SCREEN_HEIGHT).map_err(|e| format!("{:?}", e)));    // FIXME missing Error impl

            let mut this = SdlRenderer {
                renderer: renderer,
                texture: texture,
            };
            this.resize_to(SCREEN_WIDTH * 3, SCREEN_HEIGHT * 3);

            Ok(this)
        })
    }

    fn render(&mut self, frame_data: &[u8]) -> BackendResult<Vec<BackendAction>> {
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
        if let Some(win) = self.renderer.window_mut() {
            win.set_title(title).unwrap();
        }
    }
}

impl SdlRenderer {
    /// Handle a window resize to `w, h`
    fn resize_to(&mut self, w: u32, h: u32) {
        let Viewport { x, y, w, h } = Viewport::for_window_size(w, h);

        let viewport = Rect::new(x as i32, y as i32, w, h);
        self.renderer.set_viewport(Some(viewport));

        info!("viewport: ({}, {}); {}x{}", x, y, w, h);
    }
}

pub struct KeyboardInput;

impl JoypadImpl for KeyboardInput {
    fn update_state(&mut self) -> JoypadState {
        use self::sdl2::keyboard::Scancode::*;

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
