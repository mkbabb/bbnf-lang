//! Pratt emitter dispatch.
//!
//! AZ-II/O4 routes production Pratt emission to the StructDirect
//! implementation. The retired column-body strategy no longer has a
//! selectable `EmitStrategy` variant.

use bbnf_ir::registry::EmitStrategy;
use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;

/// Emit `pub fn parse_pratt_<grammar>_<rule>(input, p, state, builder)`.
pub fn emit_parse_pratt(
    strategy: &EmitStrategy,
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    super::struct_direct::emit_parse_pratt_struct_direct(grammar_suffix, rule, ir, strategy)
}
