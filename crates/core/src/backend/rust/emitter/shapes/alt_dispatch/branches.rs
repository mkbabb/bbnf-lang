//! Per-branch tape-path emission: dispatch-arm assembly, branch-body
//! emitters per IR shape (Ref / Literal / Regex / Seq), Seq position
//! flattening, first-byte-set computation, and trivia stripping.

use bbnf_ir::{AltBranch, GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::dispatcher::emit_ref_call_tape;
use super::payload::{
    alt_dispatch_branch_literal_payloads, alt_dispatch_rule_payload_spec,
    emit_typed_literal_payloads,
};

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
            IrNode::Ref(rid) => match emit_ref_call_tape(grammar_suffix, *rid, ir) {
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
                let byte_lits: Vec<TokenStream> =
                    bytes.iter().map(|b| quote! { #b }).collect();
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
            // Seq branches (literal-chain or mixed). Walk the
            // pure-literal sequence position-by-position; on full
            // match record one unit leaf for the entire seq plus the
            // branch tag. Mixed seqs fall through to the post-cap
            // typed-leaf emission lane.
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
                    quote! {}
                }
            }
            _ => quote! {},
        };
        arms.push(body);
    }
    quote! {
        'try_branches: loop {
            #(#arms)*
            return Err(crate::runtime::tape::DtaError::Syntax {
                offset: *p as u32,
                failing_state: crate::runtime::tape::DtaStateId::NONE,
                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
            });
        }
    }
}

/// Emit the dispatch body for an AltDispatch rule — collects per-
/// branch first-byte sets and emits a match over the first byte,
/// with fallback linear scan for overlapping sets.
pub(super) fn emit_dispatch_arms(
    branches: &[AltBranch],
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    // AX.W0a.2.q — typed-payload spec; `Some` iff the owning rule's
    // type is a known scalar (U8 / U32 / F64 / Bool). Drives per-branch
    // emission to write an arena payload of the matching width.
    let payload_spec = alt_dispatch_rule_payload_spec(rule, ir);
    let rule_variant_idx = (rule.id & 0xFF) as u8;

    // Precompute per-branch (first_bytes, body) pairs.
    let mut enumerated: Vec<(Vec<u8>, TokenStream)> = Vec::new();
    for branch in branches {
        let first_bytes = branch_first_bytes(&branch.node, ir);
        // AX.W0a.2.q — when the rule is typed AND the branch decomposes
        // into a literal-chain + scalar payload (including prefix-
        // factored Seq(Literal, Alt(Map{Literal,…})) forms), emit a
        // typed-payload attempt that writes the decoded scalar into
        // the arena. Branches that don't decompose (Ref / Regex /
        // non-scalar) route through the pre-W0a.2.q structural
        // emission.
        let body = match &payload_spec {
            Some((leaf_kind, payload_width)) => {
                let pairs = alt_dispatch_branch_literal_payloads(&branch.node, ir);
                if !pairs.is_empty() {
                    emit_typed_literal_payloads(
                        &pairs,
                        rule_variant_idx,
                        leaf_kind,
                        *payload_width,
                    )
                } else {
                    emit_branch_body(&branch.node, grammar_suffix, ir)
                }
            }
            None => emit_branch_body(&branch.node, grammar_suffix, ir),
        };
        enumerated.push((first_bytes, body));
    }

    // Use linear-attempt form: try each branch in order, rolling *p
    // back on failure. Rollback is span-only (no tape mutation undo
    // because Tape<R> has no child truncation; instead each
    // branch emits in a scoped closure and only commits on success).
    //
    // The match is keyed on `first` to skip branches whose first
    // byte doesn't match — O(1) dispatch in the common case with a
    // linear fallback for overlapping/regex branches.
    let mut per_byte_arms: std::collections::BTreeMap<u8, Vec<TokenStream>> =
        Default::default();
    let mut fallback_arms: Vec<TokenStream> = Vec::new();

    for (first_bytes, body) in &enumerated {
        if first_bytes.is_empty() || first_bytes.len() > 16 {
            // No predictable first byte (or too many) — add to
            // fallback chain. Attempt after specific-byte arms fail.
            fallback_arms.push(body.clone());
        } else {
            for &b in first_bytes {
                per_byte_arms.entry(b).or_default().push(body.clone());
            }
        }
    }

    // Emit the per-byte match. Each arm is a block that tries its
    // candidate branches one at a time; on success it breaks out of
    // the labelled block; on failure it falls through to the next
    // candidate, eventually the default.
    let byte_arms: Vec<TokenStream> = per_byte_arms
        .into_iter()
        .map(|(byte, bodies)| {
            let byte_lit = byte;
            quote! {
                #byte_lit => {
                    #(#bodies)*
                }
            }
        })
        .collect();

    // Default arm — try each fallback in order.
    quote! {
        let save_p = *p;
        // AY-II.W0.b — structural-save index for the outer emission
        // window; kept as a read-only columns.len() snapshot (not a
        // checkpoint for rollback — the outer's own begin_compound_post
        // will back-patch child_off once branch records land).
        let save_child = builder.position();
        let _ = save_p;
        let _ = save_child;
        'try_branches: loop {
            match first {
                #(#byte_arms)*
                _ => {}
            }
            #(#fallback_arms)*
            // B5.W6 — the outer alt_dispatch frame opened a
            // post-order children bracket via
            // `enter_post_order_children`; close it explicitly before
            // propagating the error so `current_depth` matches the
            // outer frame's depth.
            builder.exit_post_order_children();
            return Err(crate::runtime::tape::DtaError::Syntax {
                offset: *p as u32,
                failing_state: crate::runtime::tape::DtaStateId::NONE,
                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
            });
        }
    }
}

/// Emit the body of a single branch attempt. The body either
/// `break 'try_branches`es on success, or falls through so the next
/// candidate can try.
fn emit_branch_body(
    node: &IrNode,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let inner = unwrap_trivia(node);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    match inner {
        IrNode::Ref(rid) => match emit_ref_call_tape(grammar_suffix, *rid, ir) {
            Some(call) => quote! {
                {
                    let attempt_p = *p;
                    match #call {
                        Ok(_) => break 'try_branches,
                        Err(_) => { *p = attempt_p; }
                    }
                }
            },
            None => quote! {},
        },
        IrNode::Literal(sid) => emit_literal_attempt(*sid, ir),
        // AX.W0a.2.q — dispatch Regex branches through their actual
        // pattern via the per-grammar regex-scan adapter. Pre-W0a.2.q
        // emission used a hard-coded `[^\s;!}…]+` scanner that over-
        // consumed on byte sequences the rule's regex would reject
        // (Sheets `range_end`'s column-only `A:A` case where the
        // scanner swallowed the `:` delimiter). The pattern-aware
        // scan respects each branch's own regex — on failure the
        // attempt rolls back and the next candidate is tried.
        IrNode::Regex(sid) => emit_regex_pattern_attempt(*sid, grammar_suffix, ir),
        IrNode::Seq(_) | IrNode::Next(_, _) | IrNode::Skip(_, _) => {
            // AX.W0a.2.h — dispatch on Seq content. Pure literal
            // chains (prefix-tree factored keywords) keep the legacy
            // `emit_seq_attempt` emission (one Literal leaf covering
            // the whole match). Seqs with Refs / Regex / nested Alts
            // delegate to the inline structural emitter, producing
            // walker-parity records position-by-position.
            if seq_is_pure_literal_chain(inner) {
                emit_seq_attempt(inner, ir)
            } else {
                let body = super::super::inline::emit_seq_branch_structural_tape(
                    inner, &support_mod, grammar_suffix, ir,
                );
                quote! {
                    {
                        let attempt_p = *p;
                        let attempt_len = builder.position();
                        let attempt: ::core::result::Result<(), ()> = (|| {
                            #body
                            Ok(())
                        })();
                        match attempt {
                            Ok(_) => break 'try_branches,
                            Err(_) => {
                                *p = attempt_p;
                                builder.rollback_to(attempt_len);
                            }
                        }
                    }
                }
            }
        }
        _ => quote! {},
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
    positions.iter().all(|pos| {
        match unwrap_trivia(pos) {
            IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => true,
            IrNode::Alt(branches, _) => branches.iter().all(|b| {
                matches!(unwrap_trivia(&b.node), IrNode::Literal(_))
            }),
            _ => false,
        }
    })
}

/// Literal-branch attempt — byte-match and commit.
fn emit_literal_attempt(sid: u32, ir: &GrammarIR) -> TokenStream {
    let bytes = ir.get_string(sid).as_bytes();
    let len = bytes.len();
    let byte_lits: Vec<TokenStream> =
        bytes.iter().map(|b| quote! { #b }).collect();
    quote! {
        {
            let at = *p;
            let end = at + #len;
            if input.len() >= end && input[at..end] == [#(#byte_lits),*] {
                *p = end;
                let _ = builder.push_leaf(
                    crate::runtime::tape::TapeKind::Literal,
                    at as u32,
                    end as u32,
                    0,
                    0,
                );
                break 'try_branches;
            }
        }
    }
}

/// AX.W0a.2.q — Regex-branch attempt using the rule's actual regex
/// pattern via the per-grammar regex-scan adapter. The attempt block
/// saves `*p` + column length, runs the scan, and on a successful
/// match pushes a Span leaf + `break 'try_branches`. On regex-scan
/// failure, it leaves `*p` + columns untouched so the linear-try
/// loop's outer `return Err` at the end of `emit_dispatch_arms`
/// surfaces the syntax error at the correct offset.
fn emit_regex_pattern_attempt(
    sid: u32,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let pattern = ir.get_string(sid).to_string();
    let regex_scan_ident = super::super::super::dfa_codegen::regex_scan_adapter_ident(
        &super::super::sanitise_grammar(grammar_suffix),
    );
    quote! {
        {
            let span_lo = *p as u32;
            if let ::core::option::Option::Some(match_len) =
                #regex_scan_ident(#pattern, input, *p)
            {
                *p += match_len as usize;
                let _ = builder.push_leaf(
                    crate::runtime::tape::TapeKind::Span,
                    span_lo,
                    *p as u32,
                    0,
                    0,
                );
                break 'try_branches;
            }
        }
    }
}

/// Seq-branch attempt — flatten literal/alt/regex positions into a
/// byte-sequence match. Used for prefix-tree-factored keyword chains.
fn emit_seq_attempt(seq: &IrNode, ir: &GrammarIR) -> TokenStream {
    let mut positions: Vec<&IrNode> = Vec::new();
    flatten(seq, &mut positions);
    let per_position: Vec<TokenStream> = positions
        .iter()
        .map(|pos| emit_seq_position(pos, ir))
        .collect();
    quote! {
        {
            let save_p = *p;
            let attempt = (|| -> ::core::result::Result<(), ()> {
                #(#per_position)*
                Ok(())
            })();
            match attempt {
                Ok(_) => {
                    let seq_lo = save_p as u32;
                    let seq_hi = *p as u32;
                    let _ = builder.push_leaf(
                        crate::runtime::tape::TapeKind::Literal,
                        seq_lo,
                        seq_hi,
                        0,
                        0,
                    );
                    break 'try_branches;
                }
                Err(_) => { *p = save_p; }
            }
        }
    }
}

pub(super) fn emit_seq_position(node: &IrNode, ir: &GrammarIR) -> TokenStream {
    match unwrap_trivia(node) {
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            let len = bytes.len();
            let byte_lits: Vec<TokenStream> =
                bytes.iter().map(|b| quote! { #b }).collect();
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
