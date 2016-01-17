/// State of a SNES joypad. The low byte can be read from `$4218`, the high byte from `$4219` (for
/// controller 1).
///
/// Bits (`HIGH | LOW`):
/// `B Y Select Start Up Down Left Right | A X L R 0 0 0 0`
#[derive(Clone, Copy, Default)]
pub struct InputState(pub u16);

impl_save_state_for_newtype!(InputState);

pub const BIT_A: u8 = 7;
pub const BIT_B: u8 = 15;
pub const BIT_X: u8 = 6;
pub const BIT_Y: u8 = 14;

pub const BIT_L: u8 = 5;
pub const BIT_R: u8 = 4;

pub const BIT_START: u8 = 12;
pub const BIT_SELECT: u8 = 13;

pub const BIT_UP: u8 = 11;
pub const BIT_DOWN: u8 = 10;
pub const BIT_LEFT: u8 = 9;
pub const BIT_RIGHT: u8 = 8;

impl InputState {
    pub fn new() -> Self { Self::default() }

    fn set(&mut self, bit: u8, value: bool) -> &mut Self {
        match value {
            true => self.0 |= 1 << bit,
            false => self.0 &= !(1 << bit),
        };
        self
    }

    pub fn a(&mut self, pressed: bool) -> &mut Self      { self.set(BIT_A, pressed) }
    pub fn b(&mut self, pressed: bool) -> &mut Self      { self.set(BIT_B, pressed) }
    pub fn x(&mut self, pressed: bool) -> &mut Self      { self.set(BIT_X, pressed) }
    pub fn y(&mut self, pressed: bool) -> &mut Self      { self.set(BIT_Y, pressed) }

    pub fn l(&mut self, pressed: bool) -> &mut Self      { self.set(BIT_L, pressed) }
    pub fn r(&mut self, pressed: bool) -> &mut Self      { self.set(BIT_R, pressed) }

    pub fn start(&mut self, pressed: bool) -> &mut Self  { self.set(BIT_START, pressed) }
    pub fn select(&mut self, pressed: bool) -> &mut Self { self.set(BIT_SELECT, pressed) }

    pub fn up(&mut self, pressed: bool) -> &mut Self     { self.set(BIT_UP, pressed) }
    pub fn down(&mut self, pressed: bool) -> &mut Self   { self.set(BIT_DOWN, pressed) }
    pub fn left(&mut self, pressed: bool) -> &mut Self   { self.set(BIT_LEFT, pressed) }
    pub fn right(&mut self, pressed: bool) -> &mut Self  { self.set(BIT_RIGHT, pressed) }
}
