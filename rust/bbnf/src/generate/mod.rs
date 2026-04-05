//! Rust parser code generation from BBNF grammars.

pub mod regex;
pub use regex::classify as regex_classify;
pub use crate::backend::rust as codegen;
pub use codegen::ir_enums;
pub use codegen::ir_types;
pub use codegen::ir_types::ParserAttributes;

use crate::backend::PreparedGrammar;
use crate::backend::rust::emitter_types::{RustEmitCtx, RustEmitter};

/// Generate all parser code from a prepared AOT bundle.
pub fn generate_all(
    prepared: &PreparedGrammar,
    parser_attrs: &ParserAttributes,
    ident: &syn::Ident,
) -> proc_macro2::TokenStream {
    let ir = &prepared.ir;

    let mut ir_ctx = ir_types::IrCodegenCtx::new(ir, ident, parser_attrs, prepared.prep.effective_prettify);
    ir_ctx.sp_method_rules = prepared.prep.analysis.sp_method_rules.clone();
    ir_ctx.fused_number_rules = prepared.prep.analysis.fused_number_rules.clone();
    ir_ctx.operator_chain_rules = prepared.prep.analysis.operator_chain_rules.clone();

    // Compute prettify methods via the existing monolithic prettify path.
    // Prettify uses its own IR walker (not Map-aware), so it stays monolithic.
    let prettify_methods = if prepared.prep.effective_prettify {
        codegen::prettify::generate_monolithic_prettify(ir, &ir_ctx)
    } else {
        proc_macro2::TokenStream::new()
    };

    // Create emitter and context.
    let enum_ident = ir_ctx.enum_ident.clone();
    let mut emitter = RustEmitter::new(enum_ident, prepared.prep.effective_prettify);
    emitter.fused_number_rules = prepared.prep.analysis.fused_number_rules.clone();
    emitter.operator_chain_rules = prepared.prep.analysis.operator_chain_rules.clone();
    emitter.extra_impl_methods = prettify_methods;

    let mut emit_ctx = RustEmitCtx::new(&ir_ctx);

    // Compute call strategies from the IR analysis.
    let call_strategies = crate::pipeline::compile::compute_call_strategies(ir);
    let mut dstate = crate::backend::driver::DriverState::new(call_strategies);

    // Use the shared driver.
    crate::backend::driver::compile_grammar(
        ir,
        &prepared.prep.analysis,
        &mut dstate,
        &mut emitter,
        &mut emit_ctx,
    )
}
