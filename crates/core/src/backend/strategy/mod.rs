//! Strategy domain — per-node decision types consumed by the driver.
//!
//! `Alt` is the one node kind with a pre-solved strategy: the enum captures
//! the backend-agnostic decision of *what* to emit (as opposed to *how*,
//! which the per-target emitters handle). Pre-solving at `prepare_grammar`
//! time lets the driver dispatch on the resolved form instead of
//! re-detecting patterns inline. Consumers read it via `DriverState`.
//!
//! Isomorphic with the regex pipeline's `RegexStrategy`/`RegexTier`
//! — both pipelines share the same facts-to-strategy-to-emission shape.

pub mod alt_strategy;

pub use alt_strategy::AltStrategy;
