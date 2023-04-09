use ast::prefix;
use time::SteadyTime;

pub fn time_operation<Op, R>(level: usize, op_name: &str, op: Op) -> R
where
    Op: Fn() -> R,
    R: Sized,
{
    let start_time = SteadyTime::now();
    let r = op();

    let duration = SteadyTime::now() - start_time;
    let us = duration.num_microseconds().unwrap_or(0) % 1000;
    println!(
        "{}{}: {}.{:03} ms",
        prefix(level),
        op_name,
        duration.num_milliseconds(),
        us
    );
    r
}

pub fn time_operation_mut<Op, R>(level: usize, op_name: &str, mut op: Op) -> R
where
    Op: FnMut() -> R,
    R: Sized,
{
    let start_time = SteadyTime::now();
    let r = op();

    let duration = SteadyTime::now() - start_time;
    let us = duration.num_microseconds().unwrap_or(0) % 1000;
    println!(
        "{}{}: {}.{:03} ms",
        prefix(level),
        op_name,
        duration.num_milliseconds(),
        us
    );
    r
}
