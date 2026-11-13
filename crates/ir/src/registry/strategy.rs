//! AZ-I.W2-act.A — `EmitStrategy` — IR-level codegen-time substrate
//! selector.
//!
//! Per `audit/AUDIT-6-ARCHITECTURE.md` §4 + §8.1, the substrate-
//! selection decision is **backend-shared, not Rust-specific**: each
//! backend reads its own [`SubstrateBinding`] field of the
//! [`EmitStrategy`] (`rust` today; `ts` / `wasm` populated at
//! BA-host-bindings time).
//!
//! The Rust emitter writes the struct-builder parse-fn body for every
//! production grammar:
//!
//! - [`EmitStrategy::StructDirect`] — the AZ-I.W2 struct-builder path.
//!   The dispatcher writes typed compound / leaf records into a
//!   grammar-specific concrete `StructBuilder` (e.g. `JsonStructBuilder`)
//!   and returns the matching grammar-specific document type
//!   (e.g. `JsonDocument`).
//!
//! Selection happens at codegen time, not at runtime —
//! `feedback_no-orthogonal-codepaths` is in force: ONE codegen path
//! per grammar; the dispatch is data, not branches threaded through
//! every emitted shape body.
//!
//! # The binding is the caller's data
//!
//! sk-v27 LW3.COMP-5a (RULINGS 2026-11-13c COMP5A-S, QX-1 clause 6): the
//! caller supplies a grammar's builder and document paths with the
//! grammar (the regen entry's `builder` and `document`, carried on
//! `ParserAttributes`). No table keyed by the grammar's parser ident
//! resolves them, so renaming a grammar keeps its binding.

use crate::registry::StructRegistry;

/// Per-backend binding describing where the codegen-time substrate
/// resolves on a given backend.
///
/// Carries fully-qualified type-path strings the emitter splices into
/// the generated `parse()` body. The Rust backend consumes
/// [`SubstrateBinding::builder_path`] / [`SubstrateBinding::document_path`]
/// directly; the TS / WASM hosts (BA wave) read the same field
/// shapes, mapped per-binding to that backend's native module path.
///
/// # Field semantics
///
/// - `builder_path` — Rust path the emitter instantiates with
///   `<builder_path>::new()` (e.g.
///   `"crate::runtime::json::JsonStructBuilder"`). Generated code
///   lives inside the `bbnf` crate, so paths are rooted at `crate::`
///   (B5 retired the `extern crate self as bbnf` self-alias; an
///   absolute `::bbnf::` path does not resolve from inside the crate).
/// - `document_path` — Rust path the emitter returns from `parse()`
///   (e.g. `"crate::runtime::json::JsonDocument"`). Same lifetime
///   signature as the builder; per-grammar code threads `'p` for
///   arena-borrowed slices.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SubstrateBinding<'a> {
    pub builder_path: &'a str,
    pub document_path: &'a str,
}

/// The codegen-time substrate decision for one grammar.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EmitStrategy<'a> {
    StructDirect {
        rust: SubstrateBinding<'a>,
        ts: Option<SubstrateBinding<'a>>,
        wasm: Option<SubstrateBinding<'a>>,
    },
}

impl<'a> EmitStrategy<'a> {
    /// The struct-direct strategy over the caller-supplied Rust binding.
    ///
    /// Fail-closed (AZ-II.cutover.O4): struct-direct generation needs the
    /// `project_types` registry closure, so an empty registry panics
    /// before emission.
    pub fn struct_direct(rust: SubstrateBinding<'a>, registry: &StructRegistry) -> Self {
        if registry.is_empty() {
            panic!(
                "EmitStrategy::struct_direct: empty StructRegistry; StructDirect generation \
                 requires project_types registry closure"
            );
        }
        EmitStrategy::StructDirect {
            rust,
            ts: None,
            wasm: None,
        }
    }

    #[inline]
    pub fn is_struct_direct(&self) -> bool {
        matches!(self, EmitStrategy::StructDirect { .. })
    }
}
