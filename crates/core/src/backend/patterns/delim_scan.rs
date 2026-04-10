//! Legacy shim — re-exports the upstream delim-scan type from
//! `bbnf_ir`.
//!
//! The detection logic that used to live here moved to
//! `bbnf_ir::passes::recognizers::delim_scan` in Tranche X.8a. The
//! authoritative configs live on `GrammarIR::delim_scan_configs`;
//! every backend reads them from there. This file survives only as a
//! re-export shim for downstream imports until Tranche X.8h deletes
//! `backend/patterns/` outright.

pub use bbnf_ir::DelimScanConfig;
