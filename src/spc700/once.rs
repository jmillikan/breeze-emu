/// Evaluates the given expression once (when first reached).
///
/// *NOTE*: This isn't particularly thread-safe, so 2 threads reaching the statement at the same
/// time may both run it.
macro_rules! once {
    ( $e:expr ) => {{
        static mut REACHED: bool = false;
        if unsafe { !REACHED } {
            unsafe {
                REACHED = true;
            }
            $e;
        }
    }}
}
