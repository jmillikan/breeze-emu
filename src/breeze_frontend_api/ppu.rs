// FIXME Figure out how to do this properly and without duplication

/// Physical screen width
/// (this is the width of a field, or a half-frame)
pub const SCREEN_WIDTH: u32 = 256;
/// Physical screen height
/// (this is the height of a field, or a half-frame)
pub const SCREEN_HEIGHT: u32 = 224;     // 224px for 60 Hz NTSC, 264 for 50 Hz PAL

/// Additional and optional per-pixel info.
///
/// This contains additional internal per-pixel info so renderers can do cool stuff.
#[derive(Default, Clone, Copy)]
pub struct PixelData {
    /// Priority index. 0 = lowest priority = Backdrop.
    pub prio: u8,
}
