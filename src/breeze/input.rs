//! Temporary input support

use breeze_core::input::{Input, Peripheral};
use breeze_backend::input::joypad::JoypadImpl;

// FIXME(#11) Replace this hack with proper input detection

/// Attaches a suitable keyboard joypad implementation that will work with the renderer.
///
/// Keyboard handling is window-specific, and there is currently no cross-platform input handling
/// library that would allow us to avoid that restriction. Therefore we have to select the correct
/// implementation depending on the renderer (window) in use.
pub fn attach_default_input(input: &mut Input, renderer_name: &str) {
    fn none<T>() -> Option<T> { None }

    let joypad = match renderer_name {
        "sdl" => sdl_kbd_joypad,
        _ => none,
    }();

    if joypad.is_none() {
        warn!("no suitable keyboard joypad for '{}' found, input will not work", renderer_name);
    }

    input.ports.0 = joypad.map(Peripheral::new_joypad);
}

#[cfg(feature = "sdl")]
fn sdl_kbd_joypad() -> Option<Box<JoypadImpl>> {
    Some(Box::new(::breeze_backends::breeze_sdl::KeyboardInput))
}
#[cfg(not(feature = "sdl"))]
fn sdl_kbd_joypad() -> Option<Box<JoypadImpl>> {
    None
}
