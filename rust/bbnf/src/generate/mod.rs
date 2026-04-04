//! Rust parser code generation from BBNF grammars.
//!
//! This module is a thin facade over the codegen emitters.

// ── Codegen modules ────────────────────────────────────────────────────────
pub mod regex;

// Backward-compat alias — existing callers that reference regex_classify.
pub use regex::classify as regex_classify;

// Rust codegen lives in backend::rust; re-export for backward-compat paths.
pub use crate::backend::rust as codegen;

pub use codegen::ir_enums;
pub use codegen::ir_types;
pub use codegen::ir_types::ParserAttributes;

use crate::backend::PreparedGrammar;
use quote::quote;

// ── Entry point ────────────────────────────────────────────────────────────

/// Generate all parser code from a prepared AOT bundle: enum, parser methods,
/// and optionally prettify.
pub fn generate_all(
    prepared: &PreparedGrammar,
    parser_attrs: &ParserAttributes,
    ident: &syn::Ident,
) -> proc_macro2::TokenStream {
    let ir = &prepared.ir;

    // ── Build the codegen context (type bridge, scratch types, etc.) ─────

    let mut ctx = ir_types::IrCodegenCtx::new(
        ir,
        ident,
        parser_attrs,
        prepared.prep.effective_prettify,
    );

    // Copy prep analysis into the codegen context.
    ctx.sp_method_rules = prepared.prep.analysis.sp_method_rules.clone();
    ctx.fused_number_rules = prepared.prep.analysis.fused_number_rules.clone();
    ctx.operator_chain_rules = prepared.prep.analysis.operator_chain_rules.clone();

    // ── Generate prettify methods first (injected into impl block) ───────

    let prettify_methods = if prepared.prep.effective_prettify {
        codegen::prettify::generate_monolithic_prettify(ir, &ctx)
    } else {
        quote! {}
    };

    // ── Shared-driver parse codegen ─────────────────────────────────────

    let analysis = crate::backend::analysis::BackendAnalysis::default();
    let call_strategies = crate::pipeline::compute_call_strategies(ir);
    let mut dstate = crate::backend::driver::DriverState::new(call_strategies);

    let mut emitter = crate::backend::rust::emitter_types::RustEmitter {
        enum_ident: ctx.enum_ident.clone(),
        effective_prettify: prepared.prep.effective_prettify,
        fused_number_rules: ctx.fused_number_rules.clone(),
        operator_chain_rules: ctx.operator_chain_rules.clone(),
        extra_impl_methods: prettify_methods,
    };
    let mut emit_ctx = crate::backend::rust::emitter_types::RustEmitCtx::new(&ctx);

    crate::backend::driver::compile_grammar(
        ir, &analysis, &mut dstate, &mut emitter, &mut emit_ctx,
    )
}
