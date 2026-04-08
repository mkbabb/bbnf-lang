//! CstSchema — first-class CST description for the grammar.
//!
//! Built once from `GrammarIR` after lowering. Captures variant shapes,
//! field roles, directive membership, and transparent-wrapper edges. All
//! CST consumers (accessors, walkers, directive extractors, lowering
//! scaffolds) generate from this schema instead of re-reading IR metadata.
//!
//! ## Layering
//!
//! - **`model`** — target-agnostic data types (`CstSchema`, `FieldRole`, …)
//! - **`build`** — `CstSchema::from_ir()` — role assignment from IR rule bodies
//! - **`emit`** — per-target codegen (currently `rust`; `ts`/`runtime` are placeholders)
//!
//! Frontend-owned: lives under `grammar/`, NOT under `backend/`. Backend
//! emitters consume this schema; they never re-derive CST shape from IR.

pub mod build;
pub mod emit;
pub mod model;

pub use model::*;
