//! Import resolution for BBNF grammars.
//!
//! Provides a `ModuleRegistry` that loads a graph of `.bbnf` files connected by
//! `@import` directives. Each file is parsed once, and imports are resolved to
//! produce a per-file namespace of visible rules.

mod errors;
mod loader;
mod registry;
mod resolve;

pub use errors::ImportError;
pub use loader::load_module_graph;
pub use registry::{ImportCycle, ModuleData, ModuleRegistry, ResolvedImport};
