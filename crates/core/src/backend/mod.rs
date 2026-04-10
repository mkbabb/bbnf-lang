//! Backend infrastructure: compilation driver, pattern detection, and emission trait.

pub mod driver;
mod emitter;
pub mod kernels;
pub mod patterns;
pub mod prettify;
pub mod recognizer_plan;
pub mod rust;
pub mod strategy;
pub mod ts;
mod types;
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
