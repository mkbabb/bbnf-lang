//! Operator-chain emission for the Rust backend.
//!
//! Tranche AC.2 tape-first. An operator chain `head + (op rhs)*`
//! is compiled as a head sub-parse followed by a Repeat-shaped
//! tail. Under tape-first the tail pushes a compound Repeat record
//! with one Seq child per link; the head is a sibling of the
//! Repeat at the owning rule's children run.
//!
//! The shared driver's legacy all-Span fast-path returned `None`
//! here to fall back to plain Seq emission; the tape-first path
//! keeps that fallback since the scratch-Vec allocator this method
//! previously drove has been deleted.

use bbnf_ir::TypeDesc;
use proc_macro2::TokenStream;

use super::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(super) fn emit_operator_chain_impl(
        &mut self,
        _head: TokenStream,
        _op: TokenStream,
        _rhs: TokenStream,
        _head_type: &TypeDesc,
        _link_elem_type: &TypeDesc,
        _ctx: &mut RustEmitCtx,
    ) -> Option<TokenStream> {
        // Operator-chain specialization is deferred under tape-
        // first. The driver falls back to emitting the shape as a
        // generic `Seq(head, Repeat(Seq(op, rhs)))`, which yields
        // identical tape records via the standard repeat/seq
        // paths. Returning `None` triggers the fallback.
        None
    }
}
