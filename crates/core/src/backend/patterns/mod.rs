//! Pattern detection: pure decision functions for IR structural
//! analysis.
//!
//! Identifies patterns in the IR (key-dispatch, delimiter-scan,
//! sep-by, flatten) and resolves type-driven decisions. Consumed by
//! the driver and emit codegen — single source of truth for all
//! structural classification.
//!
//! [`cache`] runs the detectors once per compile and hands the
//! driver NodeId-keyed lookup tables, eliminating per-Alt re-walks
//! at emission time (Tranche F).

pub mod cache;
pub mod decisions;
pub mod delim_scan;
pub mod key_dispatch;
