//! Wrap-shape emitter — `parse_wrap_<grammar>_<rule>`.
//!
//! # Role — AW-V.W4-fix
//!
//! Emits per-grammar Wrap-shape parse functions for transparent
//! `Alt(Ref, Ref, …)` dispatchers. The Wrap-shape rule emits NO
//! compound of its own — it's a pass-through that dispatches to the
//! chosen branch rule's shape fn.
//!
//! Canonical:
//! - JSON `value = object | array | string | number | bool | null` —
//!   byte-dispatch onto the 6 branch shape fns.
//! - CSS `color = colorMix | colorFn | hex | colorFunction |
//!   namedColor` — each branch is a Ref.
//! - Sheets `range_end = cell_ref | /\$?[A-Za-z]{1,3}/ | /\$?\d+/` —
//!   mixed Ref + Regex branches.
//! - BBNF `rhs = closure | alternation`.
//!
//! # Emission shape
//!
//! The emitted function performs a byte-dispatch on the first
//! non-whitespace byte and directly delegates to the chosen branch's
//! shape fn. No outer compound is pushed — the branch's own compound
//! carries the final record (walker parity: the DTA's ByteDispatch
//! state emits no compound either).
//!
//! For branches where a byte-prefix-dispatch is possible (the Ref's
//! target rule's FIRST byte set is disjoint from siblings), the arm
//! is a direct Literal-byte match to the chosen shape fn. For Regex
//! branches or overlapping-FIRST branches, the arm falls through to
//! the grammar's value-dispatcher (which handles the per-grammar
//! Alt-dispatch table).
//!
//! # Wire contract
//!
//! Walker-tape parity: the chosen branch's shape fn emits the tape
//! record carrying the rule's semantic payload. Wrap itself emits
//! nothing. The dispatcher-fallback path hooks the walker for any
//! branch whose shape fn isn't in the emitter's reach.

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::{dispatcher_fn_ident, shape_fn_ident};
use super::root_rule_name;

mod tape_dispatch;
mod visitor;

pub use visitor::emit_parse_wrap_visitor;

use tape_dispatch::emit_alt_tape_dispatch;

/// AX.W0a.2.s — Wrap rules whose branches accept whitespace as a
/// leading byte (CSS `combinator = /\s*>\s*/ | /\s*\+\s*/ |
/// /\s*~\s*/ | /\s+/`) cannot pre-skip at entry.
pub(super) fn wrap_rule_accepts_leading_ws(rule: &IrRule, ir: &GrammarIR) -> bool {
    accepts_leading_ws_bounded(&rule.body, ir, 3)
}

fn accepts_leading_ws_bounded(node: &IrNode, ir: &GrammarIR, budget: usize) -> bool {
    if budget == 0 {
        return false;
    }
    match node {
        IrNode::Literal(sid) => {
            let bytes = ir.strings[*sid as usize].as_bytes();
            bytes
                .first()
                .map(|&b| matches!(b, b' ' | b'\t' | b'\n' | b'\r' | 0x0C))
                .unwrap_or(false)
        }
        IrNode::Regex(sid) => {
            // Inspect the first byte set the regex matches. The HIR
            // analysis result lives in the rule's
            // `meta.first_set` for Ref targets, but for inline
            // Regex bodies we re-derive: pattern starts with `\s` /
            // ws-class / explicit ws byte → accepts ws.
            let pat = &ir.strings[*sid as usize];
            // Strip leading `(?...)` flags.
            let stripped = pat.trim_start_matches(|c: char| c != '(' && c != ')');
            let pat = if stripped.starts_with("(?") {
                if let Some(end) = stripped.find(')') {
                    &stripped[end + 1..]
                } else {
                    pat
                }
            } else {
                pat
            };
            // Most ws-leading regex patterns start with `\s`.
            pat.starts_with("\\s")
                || pat.starts_with(' ')
                || pat.starts_with('\t')
                || pat.starts_with('\n')
                || pat.starts_with('\r')
        }
        IrNode::Alt(branches, _) => branches
            .iter()
            .any(|b| accepts_leading_ws_bounded(&b.node, ir, budget - 1)),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            accepts_leading_ws_bounded(inner, ir, budget - 1)
        }
        IrNode::Ref(rid) => {
            let target = match ir.rules.iter().find(|r| r.id == *rid) {
                Some(r) => r,
                None => return false,
            };
            target.meta.first_set.iter().any(|b| {
                matches!(b, b' ' | b'\t' | b'\n' | b'\r' | 0x0C)
            })
        }
        _ => false,
    }
}

/// AX.W0a.2.q — derive the leaf kind for a Wrap rule's typed-Alt
/// branches based on the owning rule's `TypeDesc`. Mirrors the
/// Keyword-shape policy:
///
/// - `TypeDesc::U8` rules → `TapeKind::KvPair` (single-byte payload —
///   Sheets `compare_op` / `add_op` / `mul_op` context match).
/// - `TypeDesc::Bool` rules → `TapeKind::Span` (JSON `bool`, Sheets
///   `boolean`).
/// - Other typed rules → `TapeKind::Span` (conservative default;
///   HRegex f64/u32 payloads follow this branch).
pub(super) fn wrap_rule_leaf_kind(rule: &IrRule, ir: &GrammarIR) -> TokenStream {
    use bbnf_ir::TypeDesc;
    let ty = ir.types.iter().find_map(|(rid, t)| {
        if *rid == rule.id {
            Some(t.clone())
        } else {
            None
        }
    });
    match ty {
        Some(TypeDesc::U8) => quote! { crate::runtime::tape::TapeKind::KvPair },
        _ => quote! { crate::runtime::tape::TapeKind::Span },
    }
}

/// AX.W0a.2.q — extract the `u32` payload value from a Wrap Alt
/// branch's `Map { fn_id }` annotation, if present. Handles `IntLit`
/// and `BoolLit` MapExpr forms via the shared
/// [`super::keyword::alt_branch_payload_value`] helper (re-exported as
/// a module-local helper so wrap and keyword agree on encoding).
///
/// Returns `Some(token)` with the u32 literal when the branch is
/// Map-annotated with a scalar constant; `None` otherwise (structural
/// emission without typed payload).
pub(super) fn alt_branch_payload_value_for_wrap(
    branch: &bbnf_ir::AltBranch,
    ir: &GrammarIR,
) -> Option<TokenStream> {
    use bbnf_ir::{FnDescriptor, MapExpr};
    fn find_map_fn(node: &IrNode) -> Option<u32> {
        match node {
            IrNode::Map { fn_id, .. } => Some(*fn_id),
            IrNode::OptionalWhitespace(inner) => find_map_fn(inner),
            _ => None,
        }
    }
    let fn_id = find_map_fn(&branch.node)?;
    let fd = ir.fns.get(fn_id as usize)?;
    let FnDescriptor::Expr { expr, .. } = fd else {
        return None;
    };
    match expr {
        MapExpr::BoolLit(b) => {
            let v = if *b { 1u32 } else { 0u32 };
            Some(quote! { #v })
        }
        MapExpr::IntLit(n) => {
            let v = *n as u32;
            Some(quote! { #v })
        }
        _ => None,
    }
}

/// AY.W2.6 — whether a Wrap rule's outer `push_compound` is safe to
/// elide. The elision is sound when every Alt branch's shape-fn call
/// already emits a self-contained tape record (either a compound via
/// `push_compound` or a leaf via `push_leaf_with`), so the outer
/// Rule-compound merely double-wraps the branch's own record.
fn wrap_can_elide_compound(rule: &IrRule, ir: &GrammarIR) -> bool {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
    let body = unwrap_outer(&rule.body);
    let IrNode::Alt(branches, _) = body else {
        return false;
    };
    if branches.is_empty() {
        return false;
    }
    branches.iter().all(|b| {
        match unwrap_outer(&b.node) {
            IrNode::Ref(rid) => {
                let tag = ir.shape_assignments.get(*rid);
                !matches!(tag, ShapeTag::None)
            }
            IrNode::Regex(_) => true,
            _ => false,
        }
    })
}

/// Emit `pub fn parse_wrap_<grammar>_<rule>(input, p, state, builder)
/// -> Result<TapeOffset, DtaError>`.
pub fn emit_parse_wrap(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("wrap", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let rule_variant_idx = (rule.id & 0xFF) as u8;

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    let body = unwrap_outer(&rule.body);
    // AY.W2.6 — wrap-compound elision: when every Alt branch's call
    // already emits its own tape record, the outer Rule-compound is a
    // no-op wrapper that doubles the record count. Per AY.md prop 2 /
    // invariant 23 part 2, skip the outer compound emission for such
    // rules (JSON `value` is the canonical case).
    let elide_compound = wrap_can_elide_compound(rule, ir);
    let dispatch = match body {
        IrNode::Alt(branches, _) => emit_alt_tape_dispatch(
            branches,
            grammar_suffix,
            &dispatcher_ident,
            ir,
            rule_variant_idx,
            rule,
            elide_compound,
        ),
        IrNode::Ref(rid) => {
            // Non-Alt Wrap body — single Ref transparent alias. Resolve
            // the target shape at emission time and call directly.
            // AX.W0a.2.e — never delegate to `#dispatcher_ident` which
            // for non-Alt-rooted grammars loops through the root.
            let _ = dispatcher_ident;
            match super::dispatcher::emit_ref_call_tape(grammar_suffix, *rid, ir) {
                Some(call) => quote! { #call },
                None => quote! {
                    ::core::result::Result::Err(
                        crate::runtime::tape::DtaError::Syntax {
                            offset: *p as u32,
                            failing_state:
                                crate::runtime::tape::DtaStateId::NONE,
                            failing_rule:
                                crate::runtime::tape::DtaRuleId(u32::MAX),
                        },
                    )
                },
            }
        }
        _ => {
            // Any other non-Alt Wrap body is structurally unsupported
            // under `has_shape_dispatcher_entrypoint` admission; emit a
            // syntax error rather than infinite-loop via the dispatcher.
            let _ = dispatcher_ident;
            quote! {
                ::core::result::Result::Err(
                    crate::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state:
                            crate::runtime::tape::DtaStateId::NONE,
                        failing_rule:
                            crate::runtime::tape::DtaRuleId(u32::MAX),
                    },
                )
            }
        }
    };

    quote! {
        /// AW-V.W4-fix — per-grammar Wrap-shape parse function.
        ///
        /// Transparent dispatcher — skip leading ws, byte-dispatch
        /// to the chosen branch's shape fn, return that shape fn's
        /// offset unchanged. No outer compound emission; the
        /// branch's own shape fn owns the tape record.
        ///
        /// AX.W0a.2.f — compound; see `flat.rs` emission for the
        /// `#[inline]` downgrade rationale (LLVM inline-cycle
        /// collapse vs hard-requirement inliner abort).
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut crate::runtime::tape::Tape<()>,
        ) -> ::core::result::Result<
            crate::runtime::tape::TapeOffset,
            crate::runtime::tape::DtaError,
        > {
            #dispatch
        }
    }
}

/// Peel Map / OptionalWhitespace wrappers to reach the structural Alt
/// / Ref body.
pub(super) fn unwrap_outer(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            unwrap_outer(inner)
        }
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
