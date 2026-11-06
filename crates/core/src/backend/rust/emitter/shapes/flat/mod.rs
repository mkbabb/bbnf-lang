//! Flat-shape emitter — `parse_flat_<grammar>_<rule>`.
//!
//! # Role — AW-V.W4-fix
//!
//! Emits per-grammar Flat-shape parse functions for typed
//! `Seq(head, body+)` rules. Canonical:
//!
//! - CSS 28 `*Decl` rules — e.g. `displayDecl = "display" , ":" ?w ,
//!   (value ?w) * , importantSuffix , ";"?` per
//!   `grammar/css/l4/properties.bbnf`.
//! - BBNF 7 `*_directive` rules — e.g. `import_directive = "@import"
//!   ?w , ( … ) , ( ";" | "." ) ?` per `grammar/bbnf/bbnf.bbnf`.
//! - CSS typed dimensions — `length`, `angle`, `time`, etc.
//! - CSS rule scaffolding — `qualifiedRule`, `mediaQuery`, etc.
//!
//! # Emission shape
//!
//! The emitted function:
//!
//! 1. Captures `span_lo` + `outer_child = mark_children()` for the
//!    outer Seq compound.
//! 2. Walks each flattened position of the rule body, emitting per
//!    position:
//!    - `Literal(sid)` → byte-match at `*p`, push `TapeKind::Literal`
//!      leaf with `variant_idx` inherited from the rule.
//!    - `Regex(sid)` / `Ref(rid)` / `Alt(…)` → delegate to the
//!      dispatcher's value-position routine (the walker's own state
//!      path). The dispatcher resolves each to its shape fn or falls
//!      back to the walker for unclassified rules.
//!    - `Repeat(inner, 0, 1)` → one optional iteration wrapped in a
//!      `Rule` compound (mirroring the walker's Repeat tape shape).
//!    - `OptionalWhitespace(inner)` → leading + trailing ws-skip.
//! 3. Closes the outer Seq compound with `push_compound(..Seq, ..)`.
//!
//! # Wire contract
//!
//! Per the walker-tape parity contract (W3 Object / Array pattern),
//! every structural IR production corresponds to one record. The
//! Flat emitter walks the Seq structure once and emits a matching
//! body. Positions the emitter cannot inline (complex Repeats,
//! recursive Refs) dispatch through the grammar's value-position
//! dispatcher.
//!
//! The emitter is gated behind `has_full_shape_coverage` in
//! [`super::emit_shapes_for_grammar`] — it compiles standalone for
//! shape-dispatch substrate tests but is not consumed on the hot
//! path until W4.2 / W4.3 wire per-grammar consumers.
//!
//! Module layout (B5.W3):
//! - [`struct_direct`]   — StructBuilder per-position emission

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;

use bbnf_ir::registry::EmitStrategy;

mod struct_direct;

/// Emit `pub fn parse_flat_<grammar>_<rule>(input, p, state,
/// builder) -> Result<(), DtaError>`.
///
/// # AZ-I.W2.RF — strategy dispatch
///
/// `strategy` is the codegen-time substrate selector resolved by
/// [`EmitStrategy::for_grammar`] in `shapes/mod.rs`. O4 emits the
/// AZ-I.W2 struct-direct body: resolve the rule layout, open a Flat
/// frame via `begin_compound(&__layout)`, walk positions through
/// [`struct_direct::emit_parse_flat_struct_direct`], and close via
/// `end_compound(handle)`.
pub fn emit_parse_flat(
    strategy: &EmitStrategy,
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    payload_planner: &super::PayloadPlanner<'_>,
) -> TokenStream {
    struct_direct::emit_parse_flat_struct_direct(
        grammar_suffix,
        rule,
        ir,
        strategy,
        payload_planner,
    )
}

// ─────────────────────────────────────────────────────────────────────
// Position collection
// ─────────────────────────────────────────────────────────────────────

/// A single flattened position in the rule body with leading/trailing
/// ws-trim markers inherited from enclosing `OptionalWhitespace`s.
#[derive(Clone)]
pub(super) struct PositionedNode<'a> {
    pub(super) node: &'a IrNode,
    pub(super) leading_ws: bool,
    pub(super) trailing_ws: bool,
}

/// Flatten a rule body into a list of positional nodes.
fn collect_positions<'a>(node: &'a IrNode) -> Vec<PositionedNode<'a>> {
    let mut out = Vec::new();
    walk_positions(node, false, false, &mut out);
    out
}

fn walk_positions<'a>(
    node: &'a IrNode,
    leading: bool,
    trailing: bool,
    out: &mut Vec<PositionedNode<'a>>,
) {
    match node {
        // Map, Skip, and Next are semantic payload boundaries, not
        // transparent structural trivia. Preserve each node so the
        // struct-direct position emitter can apply replacement/discard
        // rollback while still parsing every recognition operand.
        IrNode::Map { .. } | IrNode::Next(_, _) | IrNode::Skip(_, _) => {
            out.push(PositionedNode {
                node,
                leading_ws: leading,
                trailing_ws: trailing,
            });
        }
        IrNode::OptionalWhitespace(inner) => walk_positions(inner, true, true, out),
        IrNode::Seq(children) => {
            for child in children {
                walk_positions(child, leading, trailing, out);
            }
        }
        IrNode::Epsilon => {}
        _ => out.push(PositionedNode {
            node,
            leading_ws: leading,
            trailing_ws: trailing,
        }),
    }
}
