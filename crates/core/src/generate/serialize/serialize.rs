//! Tape-first serialize emission.
//!
//! Under the tape-first architecture every rule's data lives on the
//! tape.  Per-rule `serialize_*` functions emit `span_text()` for
//! exact round-trip reproduction.  `__dispatch_serialize` routes on
//! `variant_idx()` to the correct per-rule function.  Alt-bodied
//! rules dispatch on the branch index to reconstruct constant-mapped
//! literals from the IR and delegate compound branches to their
//! child's `serialize_*` function via `child(0)`.

use bbnf_ir::{GrammarIR, IrRule};
use bbnf_ir::passes::MaterializationClass;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::rust::ir_types::IrCodegenCtx;

// ─── Public ──────────────────────────────────────────────────────

/// Generate the variant_idx match arms for `__dispatch_serialize`.
pub fn generate_dispatch_arms(ir: &GrammarIR, _ctx: &IrCodegenCtx) -> Vec<TokenStream> {
    let mut arms = Vec::new();
    for rule in &ir.rules {
        if rule.meta.is_transparent || !rule_pushes_tape_record(ir, rule) {
            continue;
        }
        let idx = (rule.id & 0xFF) as u8;
        let emit_fn = format_ident!("serialize_{}", ir.get_string(rule.name));
        arms.push(quote! { #idx => { Self::#emit_fn(__v, __ser); } });
    }
    arms
}

/// Check if a rule produces a tape record (MustTape or TapeSpanOnly).
pub fn rule_pushes_tape_record(ir: &GrammarIR, rule: &IrRule) -> bool {
    rule_materialization(ir, rule) != MaterializationClass::TransparentElide
}

fn rule_materialization(ir: &GrammarIR, rule: &IrRule) -> MaterializationClass {
    if rule.meta.preserve_identity {
        return MaterializationClass::MustTape;
    }
    if let Some(dag) = ir.dag.as_ref() {
        if let Some(node_id) = dag.node_for(&rule.body) {
            if let Some(class) = ir.materialization.get(&node_id) {
                return *class;
            }
        }
    }
    MaterializationClass::MustTape
}
