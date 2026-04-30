//! Keyword-shape emitter — `parse_keyword_<grammar>_<rule>`.
//!
//! # Role — AW-V.W3.2
//!
//! Emits the per-grammar Keyword-shape parse function. Handles two
//! admitted sub-cases per W3.1's keyword detector:
//!
//! 1. **Single-literal body** — e.g. JSON's `null = "null" -> 0u8`.
//!    Emits a direct byte-sequence match + a Literal leaf push with
//!    the rule's `-> <value>` payload.
//! 2. **Alt of literal-led branches** — e.g. JSON's
//!    `bool = "true" -> true | "false" -> false`. Emits a byte-dispatch
//!    over the discriminator byte + per-branch match + literal leaf
//!    push carrying the branch-specific payload.
//!
//! The payload inference reads the rule's `-> <expr>` annotation from
//! the IR's `FnDescriptor` list indirectly (via the existing
//! `MapExpr::Const` / `MapExpr::BoolLit` paths). For JSON the two
//! known keyword payloads are:
//!
//! - `null = "null" -> 0u8` → [`PayloadData::InlineScalar(0u32)`]
//! - `bool = "true" -> true | "false" -> false` →
//!   [`PayloadData::InlineScalar(1u32)`] and
//!   [`PayloadData::InlineScalar(0u32)`] respectively (per
//!   [`NumberVisitor`]'s `bool(value as u32)` convention in
//!   `tape::visitor`).

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;

use bbnf_ir::registry::EmitStrategy;

mod payload;
mod struct_direct;

/// AZ-I.W2.RD / AZ-II.cutover.O4 — emit the per-grammar
/// Keyword-shape parse function for the resolved struct-builder
/// substrate. The body routes JSON's keyword-payload projection through
/// `builder.push_leaf_with_bool(value)` (for `bool` rules) and
/// `builder.push_leaf_with_unit()` (for `null`-marker rules) per the
/// `JsonStructBuilder` contract documented in
/// `crates/core/src/runtime/json/builder.rs`.
pub fn emit_parse_keyword(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    struct_direct::emit_parse_keyword_struct_direct(grammar_suffix, rule, ir, strategy)
}

/// Strip `Map` / `OptionalWhitespace` trivia wrappers.
pub(super) fn unwrap_trivia(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } => unwrap_trivia(inner.as_ref()),
        IrNode::OptionalWhitespace(inner) => unwrap_trivia(inner.as_ref()),
        _ => node,
    }
}
