//! Backend infrastructure: compilation driver, shared types, and emission trait.
//!
//! Tranche Y.1 deleted `backend/patterns/` outright. `decisions.rs` —
//! the only non-shim file in that directory — moved to
//! `backend/types/decisions.rs`. Every other file was a re-export
//! shim over `bbnf_ir::{KeyClass, KeyDispatchConfig, ..}` added in
//! Tranche X.8a; backends now import those types directly from
//! `bbnf_ir::` or via the `backend::types::*` re-export surface.

pub mod driver;
mod emitter;
pub mod kernels;
pub mod prettify;
pub mod recognizer_plan;
pub mod rust;
pub mod strategy;
pub mod ts;
pub mod types;
mod util;
pub mod wasm;

pub use driver::analysis::{
    BackendAnalysis, BackendPreparation, EffectiveBackendConfig, PreparedGrammar, TypeAnalysis,
    prepare_grammar,
};
pub use emitter::Emitter;
pub use recognizer_plan::{
    EmitHint, RecognizerFamily, ScannerPlanRecord, plan_for_id, scanner_plan_for,
};
pub use types::*;
pub use util::unescape_literal;
