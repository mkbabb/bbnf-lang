//! Backend preparation artifacts shared by codegen entrypoints.

pub mod aot;

pub use aot::{
    AotAnalysis, AotPreparation, EffectiveAotConfig, PreparedAotGrammar, TypeAnalysis,
    prepare_aot,
};
