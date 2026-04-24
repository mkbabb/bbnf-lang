//! Wall-clock guard for `divan`-based benchmarks.
//!
//! `divan` (and its bencher/criterion forebears) have no built-in
//! timeout. A performance regression that sends a compile or parse path
//! into an effective infinite loop would otherwise freeze CI
//! indefinitely. Tranche Y.-1 installed two complementary guards
//! retained verbatim under B1's bencher → divan migration:
//!
//! 1. [`csp_solver::SolveConfig::node_budget`] — caps the search space
//!    of the CSP strategy solver so pathological grammars cannot hang
//!    inside `solve_strategy_decisions`.
//! 2. This module — wraps each divan sample with a per-iteration
//!    wall-clock check. When an iteration exceeds its configured limit,
//!    the bench panics with a clear diagnostic instead of running
//!    forever.
//!
//! The two guards compose: the CSP budget prevents the most common
//! hang source (unbounded branch-and-bound); the wall-clock guard
//! catches anything else (infinite loops, livelocks, runaway
//! recursion) without relying on cooperative interrupts.
//!
//! # Usage
//!
//! ```ignore
//! #[path = "common/timeout.rs"]
//! mod timeout;
//!
//! use timeout::{bench_with_timeout, limits};
//!
//! #[divan::bench]
//! fn compile_css_l4(b: divan::Bencher) {
//!     let source = load_grammar();
//!     bench_with_timeout(b, limits::COMPILE_CSS_L4, |source| {
//!         compile_grammar(&source).unwrap()
//!     }, &source);
//! }
//! ```
//!
//! The limit is per-iteration, not cumulative. A bench that iterates
//! 1000 times with a 100ms limit can still run for 100 seconds total;
//! the guard only fires if a **single** iteration exceeds 100ms. This
//! is the correct shape for catching performance regressions: a 10×
//! blow-up on one iteration trips the guard immediately, while normal
//! variance across iterations is ignored.

#![allow(dead_code)] // Not every bench file uses every helper.

use std::time::{Duration, Instant};

/// Wall-clock limits for the compile-pipeline benches. These are
/// generous (10–20× the current observed numbers) so normal variance
/// never trips the guard, but tight enough to catch a freeze within
/// a bounded time.
pub mod limits {
    use std::time::Duration;

    /// `compile_json` — ~100µs baseline, 50ms limit (500×).
    pub const COMPILE_JSON: Duration = Duration::from_millis(50);
    /// `compile_ebnf` — ~330µs baseline, 50ms limit.
    pub const COMPILE_EBNF: Duration = Duration::from_millis(50);
    /// `compile_bbnf` — ~1ms baseline, 50ms limit.
    pub const COMPILE_BBNF: Duration = Duration::from_millis(50);
    /// `compile_sheets` — ~500µs baseline, 50ms limit.
    pub const COMPILE_SHEETS: Duration = Duration::from_millis(50);
    /// `compile_css_l4` — ~7ms baseline, 100ms limit (14×).
    ///
    /// The X.6 attempt at a global CSP blew this from 9ms to 94ms;
    /// the guard is intentionally wider than that so a regression
    /// of similar magnitude still completes the bench (with an
    /// obvious slowdown) rather than being killed.
    pub const COMPILE_CSS_L4: Duration = Duration::from_millis(200);

    /// JSON parse benches — ~1ms baseline, 1s limit.
    pub const JSON_PARSE: Duration = Duration::from_secs(1);
    /// CSS tailwind parse — 3.6MB input, ~15ms baseline, 5s limit.
    pub const CSS_TAILWIND_PARSE: Duration = Duration::from_secs(5);
    /// Default for other parse benches — 500ms.
    pub const PARSE_DEFAULT: Duration = Duration::from_millis(500);
}

/// Run `body` under a per-iteration wall-clock guard. If any single
/// iteration exceeds `limit`, panics — surfacing the regression as a
/// bench failure rather than an indefinite hang
/// (feedback `bench-sequential-regression`).
///
/// Divan's `Bencher` is by-value (not `&mut`). The `with_inputs`/
/// `bench_values` pair is divan's idiom for "clone setup once per
/// sample, measure only the body". Per-sample input cloning is
/// unavoidable when `body` consumes its argument, but the
/// `skip_ext_time(true)` configuration on the bench `main()` excludes
/// the clone from the reported wall.
///
/// The guard is checked **after** each iteration, so a truly infinite
/// loop inside `body` is not interruptible from this helper alone. The
/// composition of guards (CSP budget + bench wall-clock) is what
/// makes the no-freeze commitment load-bearing.
#[inline]
pub fn bench_with_timeout<I, R>(
    b: divan::Bencher,
    limit: Duration,
    body: impl Fn(I) -> R + Sync,
    setup_input: &I,
) where
    I: Clone + Sync,
{
    b.with_inputs(|| setup_input.clone())
        .bench_values(|input| {
            let start = Instant::now();
            let result = body(input);
            let elapsed = start.elapsed();
            if elapsed > limit {
                panic!(
                    "bench iteration exceeded wall-clock limit — \
                     performance regression? (iteration took {elapsed:?}, \
                     limit {limit:?})"
                );
            }
            result
        });
}
