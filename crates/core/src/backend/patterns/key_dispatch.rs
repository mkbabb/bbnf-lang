//! Legacy shim — re-exports the upstream key-dispatch types from
//! `bbnf_ir`.
//!
//! The detection logic that used to live here moved to
//! `bbnf_ir::passes::recognizers::key_dispatch` in Tranche X.8a. The
//! authoritative configs live on `GrammarIR::key_dispatch_configs`;
//! every backend reads them from there. This file survives only as a
//! re-export shim for downstream imports until Tranche X.8h deletes
//! `backend/patterns/` outright.

pub use bbnf_ir::{
    DetectedBranch, KeyClass, KeyDispatchConfig, KeyDispatchMatch, key_class_regex_pattern,
};
