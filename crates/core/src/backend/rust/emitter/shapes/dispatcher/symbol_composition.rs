//! Per-grammar shape-fn ident composition.
//!
//! Both families share the `parse_<...>_<grammar>_<rule>` shape:
//!
//! - `parse_<grammar>_<root>`              — tape-path top-level dispatcher
//! - `parse_<shape>_<grammar>_<rule>`      — per-shape tape-path entry
//!
//! Both the dispatcher emitter (cross_shape) and the per-Ref routing
//! emitter (ref_call) compose their target idents through these
//! helpers so the symbol layout stays consistent across the whole
//! generated module.

use proc_macro2::Ident;
use quote::format_ident;

use super::super::sanitise_grammar;

/// Compose the dispatcher symbol for a (grammar, root-rule) pair —
/// `parse_<grammar>_<root>`. The emitted `parse()` in
/// `emit_grammar_impl` routes to this ident when the grammar has
/// full shape coverage.
pub fn dispatcher_fn_ident(grammar_ident_str: &str, root_rule: &str) -> Ident {
    let grammar = sanitise_grammar(grammar_ident_str);
    let root = sanitise_grammar(root_rule);
    format_ident!("parse_{}_{}", grammar, root)
}

/// Compose the per-shape fn ident for a rule — `parse_<shape>_<grammar>_<rule>`.
pub fn shape_fn_ident(shape: &str, grammar_suffix: &str, rule_name: &str) -> Ident {
    let rule = sanitise_grammar(rule_name);
    format_ident!("parse_{}_{}_{}", shape, grammar_suffix, rule)
}
