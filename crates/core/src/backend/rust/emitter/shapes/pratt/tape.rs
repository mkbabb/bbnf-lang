//! Pratt emitter dispatch.
//!
//! AZ-II.cutover.O4 routes production Pratt emission to the
//! struct-builder implementation. The historical tape body was retired
//! with the strategy variant that selected it.

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;

use bbnf_ir::registry::EmitStrategy;

/// Emit `pub fn parse_pratt_<grammar>_<rule>(input, p, state, builder)`.
pub fn emit_parse_pratt(
    strategy: &EmitStrategy,
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    super::struct_direct::emit_parse_pratt_struct_direct(grammar_suffix, rule, ir, strategy)
}
