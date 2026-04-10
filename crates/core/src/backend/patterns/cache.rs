//! Legacy shim — pattern caches are now authoritative on `GrammarIR`.
//!
//! Pre-Tranche X, this module ran structural detection passes over the
//! IR to build `NodeId`-keyed lookup tables for the backend. Tranche
//! X.8a moved the detection upstream into
//! `bbnf_ir::passes::recognizers::{delim_scan,key_dispatch}`; the
//! resulting configs live on `GrammarIR::delim_scan_configs` /
//! `GrammarIR::key_dispatch_configs` directly.
//!
//! This module survives only as a cloning re-export shim for call
//! sites that still consume a per-`NodeId` `HashMap`, until Tranche
//! X.8h deletes `backend/patterns/` outright. The `solve_*` functions
//! are now thin clones of the sidecar maps — no structural detection
//! is performed here.

use std::collections::HashMap;

use bbnf_ir::dag::NodeId;
use bbnf_ir::{DelimScanConfig, GrammarIR};

// Re-export of the authoritative `KeyDispatchMatch` alias from
// `bbnf_ir`. Legacy downstream imports use
// `backend::patterns::cache::KeyDispatchMatch`.
pub use bbnf_ir::KeyDispatchMatch;

/// Read the authoritative delim-scan configuration map from `GrammarIR`.
///
/// Tranche X.8b: the backend previously called this during
/// `BackendPreparation::from_ir` to build a NodeId→config lookup
/// table. Now that `ir.delim_scan_configs` is the authoritative
/// source, this function is a simple clone to preserve the previous
/// ownership semantics.
pub fn solve_delim_scan_configs(ir: &GrammarIR) -> HashMap<NodeId, DelimScanConfig> {
    ir.delim_scan_configs.clone()
}

/// Read the authoritative key-dispatch configuration map from `GrammarIR`.
pub fn solve_key_dispatch_configs(ir: &GrammarIR) -> HashMap<NodeId, KeyDispatchMatch> {
    ir.key_dispatch_configs.clone()
}
