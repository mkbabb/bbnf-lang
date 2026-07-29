#![allow(dead_code)]

use std::time::{Duration, Instant};

pub mod limits {
    use std::time::Duration;

    pub const JSON_PARSE: Duration = Duration::from_secs(1);
}

#[inline]
pub fn bench_with_timeout<I, R>(
    bencher: divan::Bencher,
    limit: Duration,
    body: impl Fn(I) -> R + Sync,
    setup_input: &I,
) where
    I: Clone + Sync,
{
    bencher
        .with_inputs(|| setup_input.clone())
        .bench_values(|input| {
            let started = Instant::now();
            let result = body(input);
            let elapsed = started.elapsed();
            if elapsed > limit {
                panic!(
                    "bench iteration exceeded wall-clock limit \
                     (iteration took {elapsed:?}, limit {limit:?})"
                );
            }
            result
        });
}
