//! Per-pass timing accumulator for the pipeline.
//!
//! Tranche AA.0 — when `BBNF_PIPELINE_REPORT=1` is set in the
//! environment, [`super::compile_ast_common`] wraps every pipeline pass
//! with [`PipelineTimer::span`] and, on exit, prints a CSV row to
//! stderr: `pass,elapsed_us` per pass, plus a final
//! `__total__,<us>` line. This is pure observability — the
//! `Instant::now()` calls are elided when the env var is unset so the
//! hot LSP path pays zero cost.
//!
//! Consumer documentation: every "+X%" claim in `post-AA.json` for a
//! compile-time phase must cite a `BBNF_PIPELINE_REPORT=1` CSV diff on
//! `compile_css_l4` or `compile_bbnf`.

use std::time::{Duration, Instant};

pub(super) struct PipelineTimer {
    enabled: bool,
    total: Instant,
    rows: Vec<(&'static str, Duration)>,
}

impl PipelineTimer {
    pub(super) fn new() -> Self {
        Self {
            enabled: std::env::var("BBNF_PIPELINE_REPORT").is_ok(),
            total: Instant::now(),
            rows: Vec::new(),
        }
    }

    /// Wrap a pass body, accumulating its wall-clock time when enabled.
    /// The `pass` argument is a `&'static str` pass name (matches the
    /// pipeline operation documented in `crates/ir/CLAUDE.md`).
    #[inline]
    pub(super) fn span<R>(&mut self, pass: &'static str, body: impl FnOnce() -> R) -> R {
        if !self.enabled {
            return body();
        }
        let start = Instant::now();
        let result = body();
        self.rows.push((pass, start.elapsed()));
        result
    }

    /// Emit the CSV report to stderr and consume the timer. Called at
    /// the end of `compile_ast_common`.
    pub(super) fn finish(self, grammar_label: &str) {
        if !self.enabled {
            return;
        }
        let total = self.total.elapsed();
        eprintln!("pipeline_report: grammar={}", grammar_label);
        eprintln!("  pass,elapsed_us");
        for (pass, dur) in &self.rows {
            eprintln!("  {},{}", pass, dur.as_micros());
        }
        eprintln!("  __total__,{}", total.as_micros());
    }
}
