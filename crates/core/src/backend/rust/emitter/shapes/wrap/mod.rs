//! Wrap-shape emitter.
//!
//! Wrap rules are StructDirect dispatch nodes: transparent wrappers
//! delegate to the matched branch, and non-transparent wrappers open
//! their registered StructBuilder frame before dispatch. The former
//! column dispatcher was unreachable after `EmitStrategy` became
//! StructDirect-only and was deleted in AZ-II/O5.

use bbnf_ir::registry::EmitStrategy;
use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;

mod struct_direct;

/// Emit `pub fn parse_wrap_<grammar>_<rule>(input, p, state, builder)
/// -> Result<(), DtaError>` for the active StructDirect strategy.
pub fn emit_parse_wrap(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    match strategy {
        EmitStrategy::StructDirect { .. } => {
            struct_direct::emit_parse_wrap_struct_direct(grammar_suffix, rule, ir, strategy)
        }
    }
}

/// Peel Map / OptionalWhitespace wrappers to reach the structural Alt
/// / Ref body.
pub(super) fn unwrap_outer(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => unwrap_outer(inner),
        _ => node,
    }
}

/// Convert a [`ShapeTag`] into the shape-fn prefix. Returns `None`
/// when the tag is `None` (unclassified).
pub(super) fn shape_tag_name(
    tag: bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag,
) -> Option<&'static str> {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
    match tag {
        ShapeTag::Object => Some("object"),
        ShapeTag::Array => Some("array"),
        ShapeTag::String => Some("string"),
        ShapeTag::Number => Some("number"),
        ShapeTag::Keyword => Some("keyword"),
        ShapeTag::Scalar => Some("scalar"),
        ShapeTag::Pratt => Some("pratt"),
        ShapeTag::Unordered => Some("unordered"),
        ShapeTag::ArgList => Some("arglist"),
        ShapeTag::Flat => Some("flat"),
        ShapeTag::Wrap => Some("wrap"),
        ShapeTag::HRegex => Some("hregex"),
        ShapeTag::AltDispatch => Some("altdispatch"),
        ShapeTag::None => None,
    }
}
