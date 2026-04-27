//! Per-position structural element emission used by the Shape-2 list
//! body, plus the value-Ref extraction helper shared with Shape 1.

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::quote;

use super::super::dispatcher::emit_ref_call_tape;

/// AX.W0a.2.f — emit tape-path code for a Repeat element body.
///
/// Walks the element structurally and emits per-position code:
///
/// - `Ref(rid)` → `emit_ref_call_tape` on the target's classified
///   shape fn. Classification is admission-guaranteed by
///   [`has_shape_dispatcher_entrypoint`]; unclassified targets would
///   have rejected the grammar before reaching this emission.
/// - `Alt(_, _)` / `Regex(_)` / `Negate(_)` / `Minus(_, _)` /
///   `TokenDispatch { .. }` → `inline::emit_inline_position_tape` —
///   byte-dispatch, regex-scan, guard, or TokenDispatch compound
///   directly, no recursion through `__value`.
/// - `Literal(sid)` → byte-match + `TapeKind::Literal` leaf push.
/// - `Seq(children)` / `Next(lhs, rhs)` / `Skip(lhs, rhs)` → emit each
///   child structurally in order.
/// - `OptionalWhitespace(inner)` / `Map { inner, .. }` → strip wrapper,
///   recurse on inner.
/// - `Repeat { inner, .. }` → emit the Repeat's inner positions inside
///   a bounded per-iter retry loop. Matches walker's `handle_repeat`
///   state machine: rollback `*p` on failure, terminate loop.
/// - `Epsilon` → emit nothing.
///
/// Note: this emitter is distinct from
/// [`super::super::inline::emit_inline_position_tape`] — the inline
/// emitter focuses on the five discrete positions (Alt / Regex /
/// Negate / Minus / TokenDispatch) a Flat/ArgList/Seq body might
/// encounter at a single position. The element emitter composes Seq /
/// Next / Skip / Literal / Ref handling around those positions so a
/// Repeat can iterate a composite element.
pub(super) fn emit_element_position_tape(
    node: &bbnf_ir::IrNode,
    variant_idx: u8,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    use bbnf_ir::IrNode;
    match node {
        IrNode::Ref(rid) => match emit_ref_call_tape(grammar_suffix, *rid, ir) {
            Some(call) => quote! { let _value_off = (#call)?; },
            None => {
                // Admission guarantees this Ref's target is classified;
                // hitting None here indicates a detector/admission
                // disagreement. Emit a syntax error to surface the bug
                // rather than a silently-broken parse.
                quote! {
                    return ::core::result::Result::Err(
                        crate::runtime::tape::DtaError::Syntax {
                            offset: *p as u32,
                            failing_state: crate::runtime::tape::DtaStateId::NONE,
                            failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                        },
                    );
                }
            }
        },
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            let len = bytes.len();
            let byte_lits: Vec<TokenStream> =
                bytes.iter().map(|b| quote! { #b }).collect();
            let var = variant_idx;
            quote! {
                {
                    let at = *p;
                    let end = at + #len;
                    if input.len() < end || input[at..end] != [#(#byte_lits),*] {
                        return ::core::result::Result::Err(
                            crate::runtime::tape::DtaError::Syntax {
                                offset: at as u32,
                                failing_state:
                                    crate::runtime::tape::DtaStateId::NONE,
                                failing_rule:
                                    crate::runtime::tape::DtaRuleId(u32::MAX),
                            },
                        );
                    }
                    *p = end;
                    let _ = builder.push_leaf_with(
                        crate::runtime::tape::TapeKind::Literal,
                        at as u32,
                        end as u32,
                        #var,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
                }
            }
        }
        IrNode::Alt(_, _) | IrNode::Regex(_) | IrNode::Negate(_)
        | IrNode::Minus(_, _) | IrNode::TokenDispatch { .. } => {
            super::super::inline::emit_inline_position_tape(
                node, variant_idx, support_mod, grammar_suffix, ir,
            )
        }
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            emit_element_position_tape(
                inner, variant_idx, support_mod, grammar_suffix, ir,
            )
        }
        IrNode::Seq(children) => {
            let parts: Vec<TokenStream> = children
                .iter()
                .map(|c| emit_element_position_tape(
                    c, variant_idx, support_mod, grammar_suffix, ir,
                ))
                .collect();
            quote! { #(#parts)* }
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            let l = emit_element_position_tape(
                lhs, variant_idx, support_mod, grammar_suffix, ir,
            );
            let r = emit_element_position_tape(
                rhs, variant_idx, support_mod, grammar_suffix, ir,
            );
            quote! { #l #r }
        }
        IrNode::Epsilon => quote! {},
        IrNode::Repeat { inner, .. } => {
            // Nested Repeat inside a Repeat element — uncommon but
            // legal (e.g. `((a b)* c)*`). Emit a bounded retry loop
            // whose body emits the inner positions. Iteration
            // terminates on body failure or zero-width progress.
            let body = emit_element_position_tape(
                inner, variant_idx, support_mod, grammar_suffix, ir,
            );
            quote! {
                loop {
                    let __inner_save_p = *p;
                    let __inner_result:
                        ::core::result::Result<(), crate::runtime::tape::DtaError>
                        = (|| {
                            #body
                            Ok(())
                        })();
                    match __inner_result {
                        Ok(()) => {
                            if *p == __inner_save_p {
                                break;
                            }
                        }
                        Err(_) => {
                            *p = __inner_save_p;
                            break;
                        }
                    }
                }
            }
        }
    }
}

/// Extract the value-position Ref target from an array rule body.
///
/// AW-V.W5.2 — the canonical JSON array body is
/// `"[" >> ((value << comma?)*)?w << "]"`, which lowers to
/// `Skip(Next("[", OW(Repeat(Skip(value, Repeat(comma, 0..=1))))), "]")`.
/// The value Ref sits inside the outer Repeat. The list-rule entry
/// variant (CSS `stylesheet = ruleList ?w`, BBNF `grammar = (item ?w)*`)
/// has a simpler shape: `Repeat(ref_or_alt, lo, hi)` with OW wrappers.
///
/// Strategy: walk the body, find the outer `Repeat`, then find the
/// first value-position Ref inside the iteration body.
pub(super) fn extract_array_value_ref(
    node: &bbnf_ir::IrNode,
    ir: &GrammarIR,
) -> Option<bbnf_ir::RuleId> {
    use bbnf_ir::IrNode;
    fn find_repeat_inner<'a>(n: &'a IrNode) -> Option<&'a IrNode> {
        match n {
            IrNode::Repeat { inner, .. } => Some(inner),
            IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
                find_repeat_inner(inner)
            }
            IrNode::Seq(children) => children.iter().find_map(find_repeat_inner),
            IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
                find_repeat_inner(lhs).or_else(|| find_repeat_inner(rhs))
            }
            _ => None,
        }
    }
    fn first_value_ref(n: &IrNode, ir: &GrammarIR) -> Option<bbnf_ir::RuleId> {
        // Punctuation-rule predicate: a rule whose body is a single literal.
        fn is_punct(rid: bbnf_ir::RuleId, ir: &GrammarIR) -> bool {
            let rule = match ir.rules.iter().find(|r| r.id == rid) {
                Some(r) => r,
                None => return false,
            };
            fn unwrap<'a>(n: &'a IrNode) -> &'a IrNode {
                match n {
                    IrNode::OptionalWhitespace(i) | IrNode::Map { inner: i, .. } => {
                        unwrap(i)
                    }
                    _ => n,
                }
            }
            matches!(unwrap(&rule.body), IrNode::Literal(_))
        }
        match n {
            IrNode::Ref(rid) => {
                if is_punct(*rid, ir) {
                    None
                } else {
                    Some(*rid)
                }
            }
            IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
                first_value_ref(inner, ir)
            }
            IrNode::Seq(children) => children.iter().find_map(|c| first_value_ref(c, ir)),
            IrNode::Skip(lhs, _) => first_value_ref(lhs, ir),
            IrNode::Next(lhs, rhs) => {
                first_value_ref(lhs, ir).or_else(|| first_value_ref(rhs, ir))
            }
            IrNode::Alt(branches, _) => {
                // For Alt-of-Refs at the value position (uncommon but
                // legal), route through the dispatcher — return None.
                // A single-Ref Alt could be unwrapped, but that's not the
                // canonical shape.
                let _ = branches;
                None
            }
            _ => None,
        }
    }
    let repeat_inner = find_repeat_inner(node)?;
    first_value_ref(repeat_inner, ir)
}
