//! Pattern detection: pure decision functions for IR structural analysis.
//!
//! These functions identify patterns in the IR (key-dispatch, delimiter-scan,
//! sep-by, flatten) and resolve type-driven decisions. Consumed by the driver
//! and emit codegen — single source of truth for all structural classification.

pub mod decisions;
pub mod delim_scan;
pub mod key_dispatch;
