//! Per-branch tape-path emission: dispatch-arm assembly, branch-body
//! emitters per IR shape (Ref / Literal / Regex / Seq), Seq position
//! flattening, first-byte-set computation, and trivia stripping.

use bbnf_ir::{AltBranch, GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::dispatcher::emit_ref_call_shape;
/// AZ-I.W2.RB — emit the dispatch body for an AltDispatch rule under
/// [`bbnf_ir::registry::EmitStrategy::StructDirect`].
///
/// Each branch attempt routes through the per-Ref struct-direct shape
/// fn (or — for non-Ref leaf branches — emits a typed `builder.push_*`
/// for the leaf payload) and records the matched branch tag via
/// `builder.push_branch_tag(idx)` before the per-shape body fires. No
/// `tape.*` calls are emitted under this path.
///
/// JSON does not exercise AltDispatch (its `value` rule is `Wrap`); this
/// path is reachable only when CSS L4 (W3) or Sheets (W2.B) flips a
/// rule with AltDispatch shape into StructDirect. Until then it remains
/// dead code at codegen but the emission contract holds (zero
/// `tape.push` per the W2-EMITTER-REWIRE plan §3 hard gate).
pub(super) fn emit_dispatch_arms_struct_direct(
    branches: &[AltBranch],
    grammar_suffix: &str,
    _rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let mut arms: Vec<TokenStream> = Vec::new();
    for (branch_idx, branch) in branches.iter().enumerate() {
        let idx_u32 = branch_idx as u32;
        let inner = unwrap_trivia(&branch.node);
        let body = match inner {
            IrNode::Ref(rid) => match emit_ref_call_shape(grammar_suffix, *rid, ir) {
                Some(call) => quote! {
                    {
                        let attempt_p = *p;
                        let attempt_builder = builder.checkpoint();
                        match #call {
                            Ok(_) => {
                                builder.push_branch_tag(#idx_u32);
                                builder.commit(attempt_builder);
                                break 'try_branches;
                            }
                            Err(_) => {
                                *p = attempt_p;
                                builder.rollback(attempt_builder);
                            }
                        }
                    }
                },
                None => quote! {},
            },
            // AZ-II.cutover.M Phase 3 — Literal-led Alt branches under
            // struct-direct emit the byte-comparison + push_leaf pair
            // mirroring `emit_literal_attempt`'s tape-path body, plus
            // the discriminator-recording `push_branch_tag(idx)` that
            // the AltDispatch frame's TaggedEnum layout consumes when
            // the branch fires. EBNF's `letter`, `digit`, `symbol`,
            // `terminator` and BNF's `terminal` are the canonical
            // Alt-of-literal AltDispatch rules; pre-cutover.M these
            // emitted empty placeholders, dropping every literal
            // candidate on the floor.
            IrNode::Literal(sid) => {
                let bytes = ir.get_string(*sid).as_bytes();
                let len = bytes.len();
                let byte_lits: Vec<TokenStream> = bytes.iter().map(|b| quote! { #b }).collect();
                quote! {
                    {
                        let at = *p;
                        let end = at + #len;
                        if input.len() >= end && input[at..end] == [#(#byte_lits),*] {
                            *p = end;
                            builder.push_leaf_with_unit();
                            builder.push_branch_tag(#idx_u32);
                            break 'try_branches;
                        }
                    }
                }
            }
            // Regex-led branches dispatch through the per-grammar
            // regex-scan adapter and push a unit leaf on match. Same
            // discriminator-recording shape as the Literal arm.
            IrNode::Regex(sid) => {
                let pattern = ir.get_string(*sid).to_string();
                let regex_scan_ident = super::super::super::dfa_codegen::regex_scan_adapter_ident(
                    &super::super::sanitise_grammar(grammar_suffix),
                );
                quote! {
                    {
                        if let ::core::option::Option::Some(match_len) =
                            #regex_scan_ident(#pattern, input, *p)
                        {
                            *p += match_len as usize;
                            builder.push_leaf_with_unit();
                            builder.push_branch_tag(#idx_u32);
                            break 'try_branches;
                        }
                    }
                }
            }
            // Seq branches (literal-chain or mixed). Pure literal
            // chains record one unit leaf for the whole match; mixed
            // structural seqs preserve their position-level children
            // inside the surrounding TaggedEnum compound. EBNF
            // `term = "(" S rhs S ")" | "[" ... | "{" ...` is the
            // canonical O2 consumer: the grouped-term branch is not a
            // leaf and must keep the nested `rhs` record while still
            // recording the owning branch tag.
            IrNode::Seq(_) | IrNode::Next(_, _) | IrNode::Skip(_, _) => {
                if seq_is_pure_literal_chain(inner) {
                    let mut positions: Vec<&IrNode> = Vec::new();
                    flatten(inner, &mut positions);
                    let lit_checks: Vec<TokenStream> = positions
                        .iter()
                        .filter_map(|pos| match pos {
                            IrNode::Literal(sid) => {
                                let bytes = ir.get_string(*sid).as_bytes();
                                let len = bytes.len();
                                let byte_lits: Vec<TokenStream> =
                                    bytes.iter().map(|b| quote! { #b }).collect();
                                Some(quote! {
                                    {
                                        let at = *p;
                                        let end = at + #len;
                                        if input.len() < end
                                            || input[at..end] != [#(#byte_lits),*]
                                        {
                                            return ::core::result::Result::Err(());
                                        }
                                        *p = end;
                                    }
                                })
                            }
                            _ => None,
                        })
                        .collect();
                    quote! {
                        {
                            let save_p = *p;
                            let attempt: ::core::result::Result<(), ()> = (|| {
                                #(#lit_checks)*
                                Ok(())
                            })();
                            match attempt {
                                Ok(()) => {
                                    builder.push_leaf_with_unit();
                                    builder.push_branch_tag(#idx_u32);
                                    break 'try_branches;
                                }
                                Err(()) => { *p = save_p; }
                            }
                        }
                    }
                } else {
                    let seq_body = super::super::inline::emit_seq_branch_structural_struct_direct(
                        inner,
                        &format_ident!("__shape_support_{}", grammar_suffix),
                        grammar_suffix,
                        ir,
                    );
                    quote! {
                        {
                            let attempt_p = *p;
                            let attempt_builder = builder.checkpoint();
                            let attempt: ::core::result::Result<
                                (),
                                crate::runtime::DtaError,
                            > = (|| {
                                #seq_body
                                ::core::result::Result::Ok(())
                            })();
                            match attempt {
                                ::core::result::Result::Ok(()) => {
                                    builder.push_branch_tag(#idx_u32);
                                    builder.commit(attempt_builder);
                                    break 'try_branches;
                                }
                                ::core::result::Result::Err(_) => {
                                    *p = attempt_p;
                                    builder.rollback(attempt_builder);
                                }
                            }
                        }
                    }
                }
            }
            _ => quote! {},
        };
        arms.push(body);
    }
    quote! {
        'try_branches: loop {
            #(#arms)*
            return Err(crate::runtime::DtaError::Syntax {
                offset: *p as u32,
            });
        }
    }
}

/// Returns `true` when every flattened position in `seq` is a
/// `Literal`, `Alt(of Literals)`, `Regex`, or `Epsilon` — the set
/// [`emit_seq_position`] handles without falling through to
/// `return Err(())`. Refs, Repeats, Negate, Minus, TokenDispatch
/// trip the structural path.
fn seq_is_pure_literal_chain(seq: &IrNode) -> bool {
    let mut positions: Vec<&IrNode> = Vec::new();
    flatten(seq, &mut positions);
    positions.iter().all(|pos| match unwrap_trivia(pos) {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => true,
        IrNode::Alt(branches, _) => branches
            .iter()
            .all(|b| matches!(unwrap_trivia(&b.node), IrNode::Literal(_))),
        _ => false,
    })
}

pub(super) fn emit_seq_position(node: &IrNode, ir: &GrammarIR) -> TokenStream {
    match unwrap_trivia(node) {
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            let len = bytes.len();
            let byte_lits: Vec<TokenStream> = bytes.iter().map(|b| quote! { #b }).collect();
            quote! {
                let at = *p;
                let end = at + #len;
                if input.len() < end || input[at..end] != [#(#byte_lits),*] {
                    return Err(());
                }
                *p = end;
            }
        }
        IrNode::Alt(branches, _) => {
            let alt_arms: Vec<TokenStream> = branches
                .iter()
                .filter_map(|b| match unwrap_trivia(&b.node) {
                    IrNode::Literal(sid) => {
                        let bytes = ir.get_string(*sid).as_bytes();
                        let len = bytes.len();
                        let byte_lits: Vec<TokenStream> =
                            bytes.iter().map(|byte| quote! { #byte }).collect();
                        Some(quote! {
                            if !alt_hit {
                                let at = *p;
                                let end = at + #len;
                                if input.len() >= end
                                    && input[at..end] == [#(#byte_lits),*]
                                {
                                    *p = end;
                                    alt_hit = true;
                                }
                            }
                        })
                    }
                    _ => None,
                })
                .collect();
            quote! {
                {
                    let mut alt_hit = false;
                    #(#alt_arms)*
                    if !alt_hit {
                        return Err(());
                    }
                }
            }
        }
        IrNode::Regex(_) => quote! {
            let at = *p;
            let mut q = at;
            while q < input.len() {
                let b = input[q];
                if b.is_ascii_alphanumeric() || b == b'_' {
                    q += 1;
                } else {
                    break;
                }
            }
            if q == at {
                return Err(());
            }
            *p = q;
        },
        IrNode::Epsilon => quote! {},
        _ => quote! { return Err(()); },
    }
}

pub(super) fn flatten<'a>(node: &'a IrNode, out: &mut Vec<&'a IrNode>) {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            flatten(inner, out);
        }
        IrNode::Seq(children) => {
            for c in children {
                flatten(c, out);
            }
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            flatten(lhs, out);
            flatten(rhs, out);
        }
        IrNode::Epsilon => {}
        other => out.push(other),
    }
}

/// Compute the first-byte set for a branch body. Returns an empty
/// Vec when the set is unbounded (Regex branches without
/// classification, Refs without `meta.first_set`).
pub(super) fn branch_first_bytes(node: &IrNode, ir: &GrammarIR) -> Vec<u8> {
    match unwrap_trivia(node) {
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            if bytes.is_empty() {
                Vec::new()
            } else {
                vec![bytes[0]]
            }
        }
        IrNode::Ref(rid) => {
            let target = match ir.rules.iter().find(|r| r.id == *rid) {
                Some(r) => r,
                None => return Vec::new(),
            };
            target.meta.first_set.iter().collect()
        }
        IrNode::Regex(_) => Vec::new(),
        IrNode::Seq(children) => children
            .first()
            .map(|c| branch_first_bytes(c, ir))
            .unwrap_or_default(),
        IrNode::Next(lhs, _) => branch_first_bytes(lhs, ir),
        IrNode::Skip(lhs, _) => branch_first_bytes(lhs, ir),
        IrNode::Alt(inner_branches, _) => {
            // Union of sub-branch first-byte sets.
            let mut out: std::collections::BTreeSet<u8> = Default::default();
            for b in inner_branches {
                for byte in branch_first_bytes(&b.node, ir) {
                    out.insert(byte);
                }
            }
            out.into_iter().collect()
        }
        _ => Vec::new(),
    }
}

/// Strip Map / OptionalWhitespace trivia.
pub(super) fn unwrap_trivia(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } => unwrap_trivia(inner.as_ref()),
        IrNode::OptionalWhitespace(inner) => unwrap_trivia(inner.as_ref()),
        _ => node,
    }
}
