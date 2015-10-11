//! Logging utility macros

use std::cell::Cell;
use std::ops::Deref;
use std::fmt::Debug;
use std::thread::panicking;

/// Evaluates the given expression once (when first reached).
#[macro_export]
macro_rules! once {
    ( $e:expr ) => {{
        static mut REACHED = false;
        if unsafe { !REACHED } {
            unsafe {
                REACHED = true;
            }
            $e
        }
    }}
}

/// Executes `trace!` the first time this macro is evaluated, and does nothing if this is reached
/// again.
#[macro_export]
macro_rules! trace_once {
    ( $($args:expr),* ) => {{
        once!(trace!( $($args),* ));
    }};
}

/// Executes `trace!` if the formatted message wasn't logged by this macro before.
///
/// Note that each invocation is tracked seperately: If you use multiple `trace_unique!`
/// invocations, each might log the same message once.
#[macro_export]
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
