//! Inline Alt-position dispatch — tape and visitor paths.
//!
//! Each Alt position emits per-branch byte-dispatch over each branch's
//! first-byte set. Branches whose first-byte set is unbounded (Regex
//! without classification, Refs without `meta.first_set`) fall into
//! the linear fallback list — the dispatcher tries them in source
//! order after the byte arms have all missed.
//!
//! The walker-parity split splits on `Alt(branches, Some(_))` (the
//! `AltDispatch` variant — `IrState::ByteDispatch` in the walker, no
//! Alt compound push) versus `Alt(branches, None)` (the `AltLinear`
//! variant — pushes a `TapeKind::Alt` compound around the winning
//! branch's records).

use bbnf_ir::{AltBranch, GrammarIR, IrNode};
use proc_macro2::TokenStream;
use quote::quote;

use super::super::super::dfa_codegen::regex_scan_adapter_ident;
use super::super::dispatcher::emit_ref_call_shape;
use super::super::sanitise_grammar;
use super::branch_analysis::{branch_first_bytes, flatten, unwrap_trivia};
use super::structural_branch::emit_structural_branch_tape;

// ─────────────────────────────────────────────────────────────────────
// Alt — tape path.
// ─────────────────────────────────────────────────────────────────────

/// AX.W0a.2.g — walker-parity `ByteDispatch` emission for inline Alts
/// carrying a dispatch table. Transitions to the chosen branch without
/// pushing an Alt compound.
///
/// Matches the walker's `emit_byte_dispatch_arm` contract: on each
/// branch attempt, rollback `*p` and truncate columns on failure so
/// a partial push from an inner shape fn's fallible parse doesn't
/// leak into the next attempt.
pub(super) fn emit_alt_byte_dispatch_tape(
    branches: &[AltBranch],
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let mut enumerated: Vec<(Vec<u8>, TokenStream)> = Vec::with_capacity(branches.len());
    for branch in branches {
        let first_bytes = branch_first_bytes(&branch.node, ir);
        let body = emit_alt_branch_body_tape(&branch.node, support_mod, grammar_suffix, ir);
        enumerated.push((first_bytes, body));
    }

    let mut per_byte_arms: std::collections::BTreeMap<u8, Vec<TokenStream>> = Default::default();
    let mut fallback_arms: Vec<TokenStream> = Vec::new();

    for (first_bytes, body) in &enumerated {
        if first_bytes.is_empty() || first_bytes.len() > 16 {
            fallback_arms.push(body.clone());
        } else {
            for &b in first_bytes {
                per_byte_arms.entry(b).or_default().push(body.clone());
            }
        }
    }

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

    quote! {
        {
            // AX.W0a.2.g — ByteDispatch-style inline Alt position. No
            // Alt compound push (walker parity: ByteDispatch only
            // transitions to the chosen branch, leaving tape emission
            // to the branch body itself).
            let first = #support_mod::skip_space(input, p, state)
                .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;
            'try_branches: loop {
                match first {
                    #(#byte_arms)*
                    _ => {}
                }
                #(#fallback_arms)*
                return ::core::result::Result::Err(
                    crate::runtime::DtaError::Syntax {
                        offset: *p as u32,
                    },
                );
            }
        }
    }
}

/// Emit an inline Alt dispatch producing a `TapeKind::Alt` compound
/// wrapping the winning branch's records. Byte-dispatches over each
/// branch's first-byte set; falls back to linear retry when no arm
/// matches. The implementation composes
/// [`super::super::alt_dispatch::emit_dispatch_arms`]'s per-branch
/// projection with an outer Alt compound push / close.
pub(super) fn emit_alt_tape(
    branches: &[AltBranch],
    variant_idx: u8,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    // Precompute (first-byte-set, branch-body) pairs.
    let mut enumerated: Vec<(Vec<u8>, TokenStream)> = Vec::with_capacity(branches.len());
    for branch in branches {
        let first_bytes = branch_first_bytes(&branch.node, ir);
        let body = emit_alt_branch_body_tape(&branch.node, support_mod, grammar_suffix, ir);
        enumerated.push((first_bytes, body));
    }

    let mut per_byte_arms: std::collections::BTreeMap<u8, Vec<TokenStream>> = Default::default();
    let mut fallback_arms: Vec<TokenStream> = Vec::new();

    for (first_bytes, body) in &enumerated {
        if first_bytes.is_empty() || first_bytes.len() > 16 {
            fallback_arms.push(body.clone());
        } else {
            for &b in first_bytes {
                per_byte_arms.entry(b).or_default().push(body.clone());
            }
        }
    }

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

    let variant_lit = variant_idx;
    quote! {
        {
            // AX.W0a.2.d — inline Alt position, walker-parity
            // `TapeKind::Alt` compound + per-branch byte dispatch.
            let first = #support_mod::skip_space(input, p, state)
                .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;
            let alt_lo = *p as u32;
            // AY-II.W0.b — walker-parity post-order Alt compound. Capture
            // first-child index before branches emit; allocate the
            // compound row post-branches via begin_compound_post; close
            // immediately; override child_off.
            //
            // B5.W6 — bracket the post-order children scope so child
            // records stamp `frame_depth` at the correct (parent + 1)
            // depth at push time.
            let alt_child = builder.enter_post_order_children();
            'try_branches: loop {
                match first {
                    #(#byte_arms)*
                    _ => {}
                }
                #(#fallback_arms)*
                // B5.W6 — every branch failed; close the bracket
                // before propagating the error.
                builder.exit_post_order_children();
                return ::core::result::Result::Err(
                    crate::runtime::DtaError::Syntax {
                        offset: *p as u32,
                    },
                );
            }
            let alt_hi = *p as u32;
            let __alt_off = builder.begin_compound_post(
                ::tape::TapeKind::Alt,
                alt_lo,
                #variant_lit,
                0u8,
                0u16,
            );
            builder.end_compound_post_order(
                __alt_off,
                alt_hi,
                ::tape::TapeOffset(alt_child),
            );
        }
    }
}

/// Emit the body of a single Alt-branch tape-path attempt. The body
/// either `break 'try_branches`es on success or falls through to the
/// next candidate.
///
/// # Walker parity — per-branch rollback
///
/// Walker's `emit_alt_linear_arm` captures `cols_len_after_push` and on
/// branch failure calls `columns.truncate(cols_len_after_push)` so
/// rows pushed inside the failed branch do not leak into subsequent
/// attempts or the surrounding tape. Ref branches through shape fns
/// may push a compound + leaves on partial success before the shape
/// fn's own internal parse fails; without the truncation those rows
/// persist after the branch's `Err` is observed here. Every branch
/// attempt below saves `builder.position()` and truncates
/// back on failure, matching the walker's rollback semantics.
fn emit_alt_branch_body_tape(
    node: &IrNode,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let inner = unwrap_trivia(node);
    match inner {
        IrNode::Ref(rid) => match emit_ref_call_shape(grammar_suffix, *rid, ir) {
            Some(call) => quote! {
                {
                    let attempt_p = *p;
                    let attempt_len = builder.position();
                    match #call {
                        Ok(_) => break 'try_branches,
                        Err(_) => {
                            *p = attempt_p;
                            builder.rollback_to(attempt_len);
                        }
                    }
                }
            },
            None => quote! {},
        },
        IrNode::Literal(sid) => emit_literal_branch_tape(*sid, ir),
        IrNode::Regex(sid) => emit_regex_branch_tape(*sid, grammar_suffix, ir),
        IrNode::Seq(_) | IrNode::Next(_, _) | IrNode::Skip(_, _) => {
            // AX.W0a.2.h — Seq Alt-branch emission splits on content.
            // Pure literal-chain branches (prefix-tree factored keywords)
            // compress into one `TapeKind::Literal` span. Branches that
            // contain structural positions (Refs, inline Alts, Regex
            // scans, nested Seqs, etc.) must emit walker-parity records
            // position-by-position — this unblocks BBNF's
            // `import_directive` branch `import_items ?w , "from" ?w ,
            // import_path`, where both end positions are Refs.
            if seq_is_pure_literal_chain(inner) {
                emit_seq_branch_tape(inner, ir)
            } else {
                emit_structural_branch_tape(inner, support_mod, grammar_suffix, ir)
            }
        }
        // AY.W2.6b — Epsilon branches always succeed without consuming.
        // EBNF's `factor = term, S, ("?" | "*" | "+" | "-", S, term | ε)`
        // hits this arm after post-prefix-factoring. First-byte-set is
        // empty so the branch lands in the fallback path; emission must
        // `break 'try_branches` unconditionally to signal the Alt
        // succeeded on the zero-byte branch.
        IrNode::Epsilon => quote! { break 'try_branches; },
        _ => quote! {},
    }
}

/// Returns `true` when every flattened position in `seq` is a
/// `Literal`, `Alt(of Literals)`, `Regex`, or `Epsilon` — the set
/// [`emit_seq_position`] handles without falling through to
/// `return Err(())`. Refs, nested Alts with non-literal branches,
/// Repeats, Negate, Minus, TokenDispatch all trip the structural
/// path.
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

/// Literal-branch attempt — byte-match and commit. Matches the
/// walker's Literal state emission: `TapeKind::Literal` leaf with
/// variant = 0 (the owning Alt compound carries the rule's variant).
fn emit_literal_branch_tape(sid: u32, ir: &GrammarIR) -> TokenStream {
    let bytes = ir.get_string(sid).as_bytes();
    let len = bytes.len();
    let byte_lits: Vec<TokenStream> = bytes.iter().map(|b| quote! { #b }).collect();
    quote! {
        {
            let at = *p;
            let end = at + #len;
            if input.len() >= end && input[at..end] == [#(#byte_lits),*] {
                *p = end;
                let _ = builder.push_leaf(
                    ::tape::TapeKind::Literal,
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

/// Regex-branch attempt — pattern-aware scan via the per-grammar
/// `__regex_scan_<grammar>` adapter. Matches the walker's Regex state
/// emission: `TapeKind::Span` leaf, span-only payload.
///
/// AX.W0a.2.r — previously used a hard-coded non-whitespace scanner
/// (`[^\s;!}…]+`) that ignored the branch's actual pattern. For
/// Sheets `range_end = cell_ref | /\$?[A-Za-z]{1,3}/ | /\$?\d+/`
/// inlined into `range_ref = sheet_prefix?, range_end, ":", range_end`
/// and the input `=A:A`, the scanner did not stop at `:` and consumed
/// `A:A` wholesale, failing the subsequent `Literal(":")`. Now the
/// branch honours the IR's pattern SID via the grammar-specific
/// regex-scan adapter, mirroring the
/// [`super::super::alt_dispatch::emit_regex_pattern_attempt`] contract.
fn emit_regex_branch_tape(sid: u32, grammar_suffix: &str, ir: &GrammarIR) -> TokenStream {
    let pattern = ir.get_string(sid).to_string();
    let regex_scan_ident = regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));
    quote! {
        {
            let span_lo = *p as u32;
            if let ::core::option::Option::Some(match_len) =
                #regex_scan_ident(#pattern, input, *p)
            {
                *p += match_len as usize;
                let _ = builder.push_leaf(
                    ::tape::TapeKind::Span,
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
fn emit_seq_branch_tape(seq: &IrNode, ir: &GrammarIR) -> TokenStream {
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
                        ::tape::TapeKind::Literal,
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

fn emit_seq_position(node: &IrNode, ir: &GrammarIR) -> TokenStream {
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

// ─────────────────────────────────────────────────────────────────────
// Alt — visitor path.
// ─────────────────────────────────────────────────────────────────────

pub(super) fn emit_alt_visitor(
    branches: &[AltBranch],
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let mut enumerated: Vec<(Vec<u8>, TokenStream)> = Vec::with_capacity(branches.len());
    for branch in branches {
        let first_bytes = branch_first_bytes(&branch.node, ir);
        let body = emit_alt_branch_body_visitor(&branch.node, grammar_suffix, ir);
        enumerated.push((first_bytes, body));
    }

    let mut per_byte_arms: std::collections::BTreeMap<u8, Vec<TokenStream>> = Default::default();
    let mut fallback_arms: Vec<TokenStream> = Vec::new();

    for (first_bytes, body) in &enumerated {
        if first_bytes.is_empty() || first_bytes.len() > 16 {
            fallback_arms.push(body.clone());
        } else {
            for &b in first_bytes {
                per_byte_arms.entry(b).or_default().push(body.clone());
            }
        }
    }

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

    quote! {
        {
            let first = #support_mod::skip_space(input, p, state)
                .ok_or(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32, rule: None,
                })?;
            'try_branches: loop {
                match first {
                    #(#byte_arms)*
                    _ => {}
                }
                #(#fallback_arms)*
                return ::core::result::Result::Err(
                    crate::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    },
                );
            }
        }
    }
}

fn emit_alt_branch_body_visitor(
    node: &IrNode,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    use super::super::dispatcher::emit_ref_call_visitor;
    let inner = unwrap_trivia(node);
    match inner {
        IrNode::Ref(rid) => match emit_ref_call_visitor(grammar_suffix, *rid, ir) {
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
                        break 'try_branches;
                    }
                }
            }
        }
        IrNode::Regex(_) => quote! {
            {
                let at = *p;
                let mut q = at;
                while q < input.len() {
                    let b = input[q];
                    if b == b' ' || b == b'\t' || b == b'\n' || b == b'\r'
                        || b == b';' || b == b'}' || b == b'!'
                        || b == b',' || b == b'{' || b == b')'
                    {
                        break;
                    }
                    q += 1;
                }
                if q > at {
                    *p = q;
                    break 'try_branches;
                }
            }
        },
        IrNode::Seq(_) | IrNode::Next(_, _) | IrNode::Skip(_, _) => {
            emit_seq_branch_visitor(inner, ir)
        }
        _ => quote! {},
    }
}

fn emit_seq_branch_visitor(seq: &IrNode, ir: &GrammarIR) -> TokenStream {
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
                Ok(_) => break 'try_branches,
                Err(_) => { *p = save_p; }
            }
        }
    }
}
