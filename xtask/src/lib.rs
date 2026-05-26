//! xtask library surface — exposes the regen module so integration
//! tests can exercise it without spawning a cargo subprocess.
//!
//! AZ-IV.W0.5 carved this lib out of what was previously a pure-bin
//! crate. The bin entrypoint at `src/main.rs` re-uses the same module;
//! `xtask/tests/metadata_fail_closed.rs` exercises the manifest
//! fail-closed surface directly via `xtask::regen::validate_grammar_features`.
//!
//! See `docs/tranches/AZ-IV/waves/W0.md` §AZ-IV.W0.5 for scope.

pub mod regen;
pub mod regen_css;
pub mod regen_simple_runtime;
