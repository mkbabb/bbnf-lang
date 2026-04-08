//! Rust parser code generation from BBNF grammars.
//!
//! Two independent codegen tracks run in sequence:
//!
//! - **Track 1 — frontend CST helpers**. `CstSchema::from_ir(ir)` →
//!   `grammar::schema::emit::rust::generate(schema, fused)` → CST helper
//!   methods (`children`, `span_text`, `identifier_text`, `walk_children`)
//!   + the visitor trait. Frontend-owned, derived purely from grammar shape.
//!
//! - **Track 2 — backend parser code**. `BackendPreparation::from_ir(ir)` →
//!   `backend::rust::emitter::generate(...)` → parser functions, type
//!   definitions, dispatch tables, etc. Backend-owned, target-specific.
//!
//! The two tracks have zero interdependence: track 1 reads the schema,
//! track 2 reads `GrammarIR + BackendPreparation`. The final TokenStream
//! is the concatenation of both outputs.

pub mod serialize;
pub mod regex;
pub use crate::backend::rust as codegen;
pub use codegen::ir_enums;
pub use codegen::ir_types;
pub use codegen::ir_types::ParserAttributes;

use quote::quote;

use crate::backend::PreparedGrammar;
use crate::backend::rust::emitter_types::{RustEmitCtx, RustEmitter};
use crate::grammar::schema::CstSchema;

/// Generate all parser code from a prepared AOT bundle.
///
/// Runs Track 1 (frontend CST helpers from `CstSchema`) and Track 2
/// (backend parser code from `GrammarIR + BackendPreparation`) and
/// concatenates their outputs.
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

    // Create emitter and context.
    let enum_ident = ir_ctx.enum_ident.clone();
    let mut emitter = RustEmitter::new(enum_ident.clone(), prepared.prep.effective_prettify);
    emitter.fused_number_rules = prepared.prep.analysis.fused_number_rules.clone();
    emitter.operator_chain_rules = prepared.prep.analysis.operator_chain_rules.clone();

    // Compute prettify methods via the driver/emitter path.
    if prepared.prep.effective_prettify {
        let mut prettify_ctx = RustEmitCtx::new(&ir_ctx);
        let prettify_methods = crate::backend::driver::prettify::compile_prettify_grammar(
            ir, &mut emitter, &mut prettify_ctx,
        );
        emitter.extra_impl_methods = prettify_methods;
    }

    // Generate Serializer-based methods when `#[parser(serialize)]` is set.
    if parser_attrs.serialize {
        let ser_tokens = serialize::generate_serialize_methods(ir, &ir_ctx);
        emitter.extra_impl_methods.extend(ser_tokens);
    }

    let mut emit_ctx = RustEmitCtx::new(&ir_ctx);

    let call_strategies = crate::pipeline::compile::compute_call_strategies(ir);
    let mut dstate = crate::backend::driver::DriverState::new(call_strategies);
    // Install pre-solved Alt strategies so compile_alt can skip re-detection
    // of patterns that were already classified during prepare_grammar.
    dstate.alt_strategies = prepared.prep.alt_strategies.clone();

    // Track 2 — backend parser code (rule functions, dispatch, type defs).
    let backend_output = crate::backend::driver::compile_grammar(
        ir,
        &prepared.prep.analysis,
        &mut dstate,
        &mut emitter,
        &mut emit_ctx,
    );

    // Track 1 — frontend CST helpers from CstSchema.
    let has_recovered = ir
        .rules
        .iter()
        .any(|r| r.meta.directives.recover.is_some())
        && !parser_attrs.skip_recover;
    let schema = CstSchema::from_ir(ir, enum_ident.to_string(), has_recovered);
    let schema_output = crate::grammar::schema::emit::rust::generate(
        &schema,
        &prepared.prep.analysis.fused_number_rules,
    );

    quote! {
        #backend_output
        #schema_output
    }
}
