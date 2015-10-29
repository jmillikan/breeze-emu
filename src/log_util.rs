//! Logging utility macros

use std::cell::Cell;
use std::ops::Deref;
use std::fmt::Debug;
use std::thread::panicking;

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

/// Executes `trace!` if the formatted message wasn't logged by this macro before.
///
/// Note that each invocation is tracked seperately: If you use multiple `trace_unique!`
/// invocations, each might log the same message once.
macro_rules! trace_unique {
    ( $($args:expr),* ) => {
        if log_enabled!(::log::LogLevel::Trace) {
            use std::collections::HashSet;
            use std::cell::RefCell;
            thread_local!(
                static LOGGED: RefCell<HashSet<String>> = RefCell::new(HashSet::new())
            );

            let msg = format!($($args),*);
            LOGGED.with(|set| {
                let mut set = set.borrow_mut();
                if !set.contains(&msg) {
                    trace!("{}", msg);
                    set.insert(msg);
                }
            })
        }
    };
}

pub struct LogOnPanic<T: Copy + Debug>(Cell<T>);

impl<T: Copy + Debug> LogOnPanic<T> {
    pub fn new(t: T) -> Self { LogOnPanic(Cell::new(t)) }
}

impl<T: Copy + Debug> Deref for LogOnPanic<T> {
    type Target = Cell<T>;
    fn deref(&self) -> &Cell<T> { &self.0 }
}

impl<T: Copy + Debug> Drop for LogOnPanic<T> {
    fn drop(&mut self) {
        if panicking() {
            println!("LogOnPanic: {:?}", self.0.get())
        }
    }
}
