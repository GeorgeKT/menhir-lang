use crate::ast::prefix;
use std::time::Instant;

pub fn time_operation<Op, R>(show_timing: bool, level: usize, op_name: &str, op: Op) -> R
where
    Op: Fn() -> R,
    R: Sized,
{
    let start_time = Instant::now();
    let r = op();

    let duration = Instant::now() - start_time;
    let us = duration.as_micros() % 1000;
    if show_timing {
        println!("{}{}: {}.{:03} ms", prefix(level), op_name, duration.as_millis(), us);
    }
    r
}

pub fn time_operation_mut<Op, R>(show_timing: bool, level: usize, op_name: &str, mut op: Op) -> R
where
    Op: FnMut() -> R,
    R: Sized,
{
    let start_time = Instant::now();
    let r = op();

    let duration = Instant::now() - start_time;
    let us = duration.as_micros() % 1000;
    if show_timing {
        println!("{}{}: {}.{:03} ms", prefix(level), op_name, duration.as_millis(), us);
    }
    r
}
