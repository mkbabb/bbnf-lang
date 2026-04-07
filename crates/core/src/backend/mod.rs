//! Backend infrastructure: shared analysis, compilation driver, and emission trait.

pub mod analysis;
pub mod decisions;
pub mod delim_scan;
pub mod driver;
pub mod driver_prettify;
pub mod key_dispatch;
pub mod prettify;
pub mod rust;
mod traits;
pub mod ts;
mod types;
mod util;
pub mod wasm;

pub use analysis::{
    BackendAnalysis, BackendPreparation, EffectiveBackendConfig, PreparedGrammar, TypeAnalysis,
    prepare_grammar,
};
pub use traits::Emitter;
pub use types::*;
pub use util::unescape_literal;
