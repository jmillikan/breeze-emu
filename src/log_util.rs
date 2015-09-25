//! Logging utility macros

/// Executes `trace!` the first time this macro is evaluated, and does nothing if this is reached
/// again.
#[macro_export]
macro_rules! trace_once {
    ( $($args:expr),* ) => {{
        static mut REACHED = false;
        if unsafe { !REACHED } {
            unsafe {
                REACHED = true;
            }
            trace!( $($args),* );
        }
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
