//! Inline-position emitters — AX.W0a.2.d.
//!
//! # Role
//!
//! Every shape emitter (`flat`, `wrap`, `arglist`, `unordered`, `pratt`,
//! `array`, `object`, `scalar`) walks a rule body and encounters
//! structural positions — `IrNode::Literal`, `IrNode::Ref`,
//! `IrNode::Alt`, `IrNode::Regex`, `IrNode::Negate`, `IrNode::Minus`,
//! `IrNode::TokenDispatch`. Literal / Ref positions are directly
//! emitted: Literal byte-matches, Ref dispatches via
//! [`super::dispatcher::emit_ref_call_tape`] (per-Ref routing, W5.2).
//!
//! The remaining five — `Alt`, `Regex`, `Negate`, `Minus`,
//! `TokenDispatch` — historically fell back to the grammar's
//! `#dispatcher_ident` (aka `__value`). For JSON that dispatcher byte-
//! dispatches over value's Alt-of-Refs; for non-Alt-rooted grammars
//! (CSS `stylesheet`, Sheets `formula`, BBNF `grammar`) the dispatcher
//! IS the root's shape fn, so the fallback loops. Closing the W0a hard
//! gate #2 — "every grammar's `parse()` routes through the shape
//! dispatcher" — requires every inline position to emit node-specific
//! dispatch, no cross-rule recursion into the root.
//!
//! # Wire contract
//!
//! This module exports two entry points consumed by every shape
//! emitter's position-core walker:
//!
//! - [`emit_inline_position_tape`] — tape-path emission; produces the
//!   walker-identical record stream for the position (Alt compound +
//!   branch records for `IrNode::Alt`, `TapeKind::Span` leaf for
//!   `IrNode::Regex`, guard-only for `Negate` / `Minus`, TokenDispatch
//!   compound + matched-arm records for `IrNode::TokenDispatch`).
//! - [`emit_inline_position_visitor`] — visitor-path emission; mirrors
//!   the tape path structurally with visitor method calls replacing
//!   tape pushes.
//!
//! Both share per-branch first-byte computation, trivia stripping, and
//! the "try-each-branch" fallback (`'try_branches: loop { match first
//! { ... } }`) the AltDispatch emitter pioneered.
//!
//! # Walker parity
//!
//! Record shape per `IrNode`:
//!
//! - `Alt(branches, _)` — one `TapeKind::Alt` compound wrapping the
//!   winning branch's records. Branch selection via byte-dispatch over
//!   each branch's first-byte set (port of
//!   [`super::alt_dispatch::emit_dispatch_arms`]), falling back to
//!   linear retry on overlap. Ref branches call the target's shape fn
//!   via [`super::dispatcher::emit_ref_call_tape`]; Literal branches
//!   byte-match and push a `Literal` leaf; Regex branches scan the
//!   grammar's regex adapter and push a `Span` leaf; Seq branches
//!   match a flattened literal sequence. Mirrors the walker's
//!   `emit_alt_linear_arm`.
//!
//! - `Regex(pattern)` — one `TapeKind::Span` leaf covering the scan
//!   match. Scan via the per-grammar regex adapter (same entry point
//!   the HRegex emitter uses). Mirrors the walker's
//!   `emit_regex_arm` (sans PSI payload scheduling — inline positions
//!   don't carry host decoders; when they do, the rule carrying the
//!   Regex is classified as HRegex and doesn't hit this path).
//!
//! - `Negate(inner)` — guard-only. Try the inner's attempt; on success
//!   return `Err`. No tape record. Mirrors the walker's NotFollowedBy
//!   pattern.
//!
//! - `Minus(primary, excluded)` — first attempt the excluded pattern;
//!   on success fail with `Syntax`; otherwise attempt primary. Matches
//!   the walker's `emit_minus_arm`.
//!
//! - `TokenDispatch { token, arms, fallback }` — one
//!   `TapeKind::TokenDispatch` compound. Emit the token's Span leaf,
//!   then byte-dispatch over arm tokens with each arm emitting its
//!   `continuation`; on no match, emit the fallback.

use bbnf_ir::{AltBranch, GrammarIR, IrNode};
use proc_macro2::TokenStream;
use quote::quote;

use super::super::dfa_codegen::regex_scan_adapter_ident;
use super::dispatcher::emit_ref_call_tape;
use super::sanitise_grammar;

// ─────────────────────────────────────────────────────────────────────
// Tape-path entry point.
// ─────────────────────────────────────────────────────────────────────

/// Emit tape-path dispatch for an inline structural position (Alt /
/// Regex / Negate / Minus / TokenDispatch).
///
/// The emitted TokenStream is a block producing no expression value
/// (the caller threads it into a larger body). On error the block
/// propagates via `?` to the enclosing shape fn's Result signature.
///
/// `variant_idx` is inherited from the owning rule; every compound
/// push stamps it onto the outer record so downstream view accessors
/// see the owning rule's discriminant.
pub(super) fn emit_inline_position_tape(
    node: &IrNode,
    variant_idx: u8,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    match node {
        // AX.W0a.2.g — walker-parity split. The IR's `Alt(branches,
        // Some(AltDispatch))` variant lowers to `IrState::ByteDispatch`
        // in the walker, which transitions to the chosen branch WITHOUT
        // pushing an Alt compound — the branch's own records are the
        // only tape emission. Inline emission must match: no Alt
        // compound push when a dispatch table is present. The
        // `AltLinear` variant (no dispatch table) continues to push
        // the Alt compound to preserve its walker parity.
        IrNode::Alt(branches, Some(_)) => emit_alt_byte_dispatch_tape(
            branches, support_mod, grammar_suffix, ir,
        ),
        IrNode::Alt(branches, None) => {
            emit_alt_tape(branches, variant_idx, support_mod, grammar_suffix, ir)
        }
        IrNode::Regex(sid) => emit_regex_tape(*sid, variant_idx, grammar_suffix, ir),
        IrNode::Negate(inner) => emit_negate_tape(inner, support_mod, grammar_suffix, ir),
        IrNode::Minus(primary, excluded) => {
            emit_minus_tape(primary, excluded, variant_idx, support_mod, grammar_suffix, ir)
        }
        IrNode::TokenDispatch { token, arms, fallback } => emit_token_dispatch_tape(
            token,
            arms,
            fallback,
            variant_idx,
            support_mod,
            grammar_suffix,
            ir,
        ),
        _ => unreachable!(
            "emit_inline_position_tape called on non-dispatch node: \
             {:?}",
            std::mem::discriminant(node),
        ),
    }
}

/// AX.W0a.2.g — walker-parity `ByteDispatch` emission for inline Alts
/// carrying a dispatch table. Transitions to the chosen branch without
/// pushing an Alt compound.
///
/// Matches the walker's `emit_byte_dispatch_arm` contract: on each
/// branch attempt, rollback `*p` and truncate columns on failure so
/// a partial push from an inner shape fn's fallible parse doesn't
/// leak into the next attempt.
fn emit_alt_byte_dispatch_tape(
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

    let mut per_byte_arms: std::collections::BTreeMap<u8, Vec<TokenStream>> =
        Default::default();
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
                .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;
            'try_branches: loop {
                match first {
                    #(#byte_arms)*
                    _ => {}
                }
                #(#fallback_arms)*
                return ::core::result::Result::Err(
                    crate::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    },
                );
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// Visitor-path entry point.
// ─────────────────────────────────────────────────────────────────────

/// Visitor-path analog of [`emit_inline_position_tape`]. Emits the
/// same structural dispatch with visitor method calls replacing tape
/// pushes. Negate / Minus produce guard-only emission; Alt / Regex /
/// TokenDispatch emit matching dispatch that calls through to
/// visitor-path Ref calls.
pub(super) fn emit_inline_position_visitor(
    node: &IrNode,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    match node {
        IrNode::Alt(branches, _) => {
            emit_alt_visitor(branches, support_mod, grammar_suffix, ir)
        }
        IrNode::Regex(sid) => emit_regex_visitor(*sid, grammar_suffix, ir),
        IrNode::Negate(inner) => {
            emit_negate_visitor(inner, support_mod, grammar_suffix, ir)
        }
        IrNode::Minus(primary, excluded) => emit_minus_visitor(
            primary,
            excluded,
            support_mod,
            grammar_suffix,
            ir,
        ),
        IrNode::TokenDispatch { token, arms, fallback } => emit_token_dispatch_visitor(
            token,
            arms,
            fallback,
            support_mod,
            grammar_suffix,
            ir,
        ),
        _ => unreachable!(
            "emit_inline_position_visitor called on non-dispatch node: \
             {:?}",
            std::mem::discriminant(node),
        ),
    }
}

// ─────────────────────────────────────────────────────────────────────
// Alt — tape path.
// ─────────────────────────────────────────────────────────────────────

/// Emit an inline Alt dispatch producing a `TapeKind::Alt` compound
/// wrapping the winning branch's records. Byte-dispatches over each
/// branch's first-byte set; falls back to linear retry when no arm
/// matches. The implementation composes
/// [`super::alt_dispatch::emit_dispatch_arms`]'s per-branch
/// projection with an outer Alt compound push / close.
fn emit_alt_tape(
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

    let mut per_byte_arms: std::collections::BTreeMap<u8, Vec<TokenStream>> =
        Default::default();
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
                .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;
            let alt_lo = *p as u32;
            // AY-II.W0.b — walker-parity post-order Alt compound. Capture
            // first-child index before branches emit; allocate the
            // compound row post-branches via begin_compound; close
            // immediately; override child_off.
            let alt_child = builder.position();
            'try_branches: loop {
                match first {
                    #(#byte_arms)*
                    _ => {}
                }
                #(#fallback_arms)*
                return ::core::result::Result::Err(
                    crate::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    },
                );
            }
            let alt_hi = *p as u32;
            let __alt_off = builder.begin_compound(
                crate::runtime::tape::TapeKind::Alt,
                alt_lo,
                #variant_lit,
                0u8,
                0u8,
                0u16,
            );
            builder.end_compound_post_order(
                __alt_off,
                alt_hi,
                crate::runtime::tape::TapeOffset(alt_child),
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
/// attempt below saves `builder.columns_mut().len()` and truncates
/// back on failure, matching the walker's rollback semantics.
fn emit_alt_branch_body_tape(
    node: &IrNode,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let inner = unwrap_trivia(node);
    match inner {
        IrNode::Ref(rid) => match emit_ref_call_tape(grammar_suffix, *rid, ir) {
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

/// Emit a structural Seq Alt-branch attempt — one position per
/// child, recursing through the standard per-position tape emitter
/// (`emit_branch_position_core`), with full rollback on failure.
///
/// The emission mirrors Flat's `emit_tape_position_core` contract:
/// each Ref position calls its target's shape fn; each Literal /
/// Regex / Alt / inline position emits the walker-parity record
/// stream for that node. Unlike the pure-literal-chain path, the
/// records are NOT compressed into a single Literal leaf — the
/// branch's records land directly in the outer Alt compound.
///
/// AX.W0a.2.h — must preserve `OptionalWhitespace` trivia between
/// positions. `flatten()` strips OW wrappers (historical contract
/// for pure-literal-chain emission); the structural branch instead
/// descends Seq / Next / Skip / Map wrappers directly, keeping OW
/// as an `emit_branch_position_core` case so `skip_space` lands
/// between positions. Without this, `@import { a } from "foo"`'s
/// branch loses the space between `}` and `from` and rejects.
fn emit_structural_branch_tape(
    seq: &IrNode,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let body = emit_branch_position_core(seq, support_mod, grammar_suffix, ir);
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

/// AX.W0a.2.h — sibling entry for the Keyword emitter's Seq-branch
/// arm. Emits the raw structural body of a Seq (per-position tape
/// emission) without the `break 'try_branches` / attempt wrapper —
/// the Keyword emitter packages the result under its own success
/// semantics (replacing inner records with ONE Span leaf).
pub(super) fn emit_seq_branch_structural_tape(
    seq: &IrNode,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    emit_branch_position_core(seq, support_mod, grammar_suffix, ir)
}

/// Emit a single position inside a structural Seq Alt-branch attempt
/// closure. The closure returns `Err(())` on failure and `Ok(())` on
/// the terminating position; each per-position emission propagates
/// failures via `?` → `Err(())` conversion or early `return Err(())`.
///
/// Mirrors the walker's per-state lowering for the corresponding
/// `IrNode`, with rollback handled by the outer attempt wrapper.
///
/// AX.W0a.2.h — matches `node` directly (NOT `unwrap_trivia(node)`):
/// `OptionalWhitespace` must reach its dedicated arm to emit
/// `skip_space` bookends around the inner position. Stripping OW
/// here would silently drop the whitespace-between-positions
/// emission (bug observed on BBNF's `import_directive` structural
/// branch, where `import_items ?w "from" ?w import_path` lost the
/// skip_space between `import_items` and `"from"`).
fn emit_branch_position_core(
    node: &IrNode,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    match node {
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
                let _ = builder.push_leaf_with(
                    crate::runtime::tape::TapeKind::Literal,
                    at as u32,
                    end as u32,
                    0,
                    0,
                    crate::runtime::tape::PayloadData::None,
                );
            }
        }
        IrNode::Ref(rid) => match emit_ref_call_tape(grammar_suffix, *rid, ir) {
            Some(call) => quote! {
                if (#call).is_err() {
                    return Err(());
                }
            },
            None => quote! { return Err(()); },
        },
        IrNode::Regex(sid) => {
            let pattern = ir.get_string(*sid).to_string();
            let regex_scan_ident =
                regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));
            quote! {
                let span_lo = *p as u32;
                let Some(match_len) = #regex_scan_ident(#pattern, input, *p) else {
                    return Err(());
                };
                *p += match_len as usize;
                let span_hi = *p as u32;
                let _ = builder.push_leaf_with(
                    crate::runtime::tape::TapeKind::Span,
                    span_lo,
                    span_hi,
                    0,
                    0,
                    crate::runtime::tape::PayloadData::None,
                );
            }
        }
        IrNode::Epsilon => quote! {},
        IrNode::Seq(children) => {
            let inner: Vec<TokenStream> = children
                .iter()
                .map(|c| emit_branch_position_core(c, support_mod, grammar_suffix, ir))
                .collect();
            quote! { #(#inner)* }
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            let l = emit_branch_position_core(lhs, support_mod, grammar_suffix, ir);
            let r = emit_branch_position_core(rhs, support_mod, grammar_suffix, ir);
            quote! { #l #r }
        }
        IrNode::OptionalWhitespace(inner) => {
            let i = emit_branch_position_core(inner, support_mod, grammar_suffix, ir);
            quote! {
                let _ = #support_mod::skip_space(input, p, state);
                #i
                let _ = #support_mod::skip_space(input, p, state);
            }
        }
        IrNode::Map { inner, .. } => {
            emit_branch_position_core(inner, support_mod, grammar_suffix, ir)
        }
        IrNode::Repeat { inner, lo, hi } => {
            let inner_emit =
                emit_branch_position_core(inner, support_mod, grammar_suffix, ir);
            let lo_lit = *lo;
            let hi_is_finite = *hi != u32::MAX;
            let hi_lit = *hi;
            let bound_check = if hi_is_finite {
                quote! {
                    if iter_count >= #hi_lit as usize {
                        break;
                    }
                }
            } else {
                quote! {}
            };
            quote! {
                {
                    let mut iter_count: usize = 0;
                    loop {
                        #bound_check
                        let iter_p = *p;
                        let iter_len = builder.position();
                        let iter_res: ::core::result::Result<(), ()> = (|| {
                            #inner_emit
                            Ok(())
                        })();
                        if iter_res.is_err() || *p == iter_p {
                            *p = iter_p;
                            builder.rollback_to(iter_len);
                            break;
                        }
                        iter_count += 1;
                    }
                    if iter_count < #lo_lit as usize {
                        return Err(());
                    }
                }
            }
        }
        // AY.W2.6b — Negate / Minus / Alt / TokenDispatch can appear
        // at position level inside a Keyword-shape Seq branch. EBNF's
        // `terminal` rule body `"'" , character - "'" , { character -
        // "'" } , "'"` places Minus inline between literal positions;
        // the keyword detector correctly admits the branch on the
        // leading `'` / `"` byte. The per-position emission delegates
        // to the rule-level helpers, wrapped through an inner closure
        // that converts `DtaError` rejections into the attempt
        // closure's `Err(())`. Variant index is 0 at position level —
        // the owning rule's Alt / Keyword compound stamps the rule
        // discriminant on the outer record.
        IrNode::Alt(branches, Some(_)) => {
            let inner =
                emit_alt_byte_dispatch_tape(branches, support_mod, grammar_suffix, ir);
            wrap_dta_err_to_unit(inner)
        }
        IrNode::Alt(branches, None) => {
            let inner = emit_alt_tape(branches, 0, support_mod, grammar_suffix, ir);
            wrap_dta_err_to_unit(inner)
        }
        IrNode::Negate(inner_node) => {
            let inner =
                emit_negate_tape(inner_node, support_mod, grammar_suffix, ir);
            wrap_dta_err_to_unit(inner)
        }
        IrNode::Minus(primary, excluded) => {
            let inner = emit_minus_tape(
                primary,
                excluded,
                0,
                support_mod,
                grammar_suffix,
                ir,
            );
            wrap_dta_err_to_unit(inner)
        }
        IrNode::TokenDispatch { token, arms, fallback } => {
            let inner = emit_token_dispatch_tape(
                token,
                arms,
                fallback,
                0,
                support_mod,
                grammar_suffix,
                ir,
            );
            wrap_dta_err_to_unit(inner)
        }
    }
}

/// Wrap a rule-level tape-emit block (whose early-return uses
/// `DtaError`) inside an inner closure that converts any
/// `DtaError` rejection into the per-position attempt closure's
/// `Err(())`. Used by [`emit_branch_position_core`] to delegate the
/// Minus / Negate / Alt / TokenDispatch emit helpers without
/// duplicating ~300 LOC of emission logic.
///
/// The inner closure isolates the rule-level `return Err(DtaError)`
/// exits: on success the outer attempt continues with the records
/// already pushed to `builder`; on rejection the outer attempt
/// returns `Err(())`, and the caller (`emit_structural_branch_tape`
/// / `emit_seq_branch_structural_tape`) handles rollback of `*p` +
/// `builder.columns_mut().truncate(...)`.
fn wrap_dta_err_to_unit(rule_emit: TokenStream) -> TokenStream {
    quote! {
        {
            let __pos_attempt: ::core::result::Result<
                (),
                crate::runtime::tape::DtaError,
            > = (|| {
                #rule_emit
                ::core::result::Result::Ok(())
            })();
            if __pos_attempt.is_err() {
                return ::core::result::Result::Err(());
            }
        }
    }
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
/// [`super::alt_dispatch::emit_regex_pattern_attempt`] contract.
fn emit_regex_branch_tape(
    sid: u32,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
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

fn emit_seq_position(node: &IrNode, ir: &GrammarIR) -> TokenStream {
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

// ─────────────────────────────────────────────────────────────────────
// Alt — visitor path.
// ─────────────────────────────────────────────────────────────────────

fn emit_alt_visitor(
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

    let mut per_byte_arms: std::collections::BTreeMap<u8, Vec<TokenStream>> =
        Default::default();
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
    use super::dispatcher::emit_ref_call_visitor;
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
            let byte_lits: Vec<TokenStream> =
                bytes.iter().map(|b| quote! { #b }).collect();
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

// ─────────────────────────────────────────────────────────────────────
// Regex — tape + visitor paths.
// ─────────────────────────────────────────────────────────────────────

/// Emit an inline Regex scan producing a `TapeKind::Span` leaf.
/// Uses the per-grammar regex adapter shared with the HRegex emitter.
fn emit_regex_tape(
    pattern_sid: u32,
    variant_idx: u8,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let pattern = ir.get_string(pattern_sid);
    let pattern_lit = pattern.to_string();
    let regex_scan_ident = regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));
    let variant_lit = variant_idx;
    quote! {
        {
            let span_lo = *p as u32;
            let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                return ::core::result::Result::Err(
                    crate::runtime::tape::DtaError::Syntax {
                        offset: span_lo,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    },
                );
            };
            *p += match_len as usize;
            let span_hi = *p as u32;
            let _ = builder.push_leaf_with(
                crate::runtime::tape::TapeKind::Span,
                span_lo,
                span_hi,
                #variant_lit,
                0,
                crate::runtime::tape::PayloadData::None,
            );
        }
    }
}

fn emit_regex_visitor(
    pattern_sid: u32,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let pattern = ir.get_string(pattern_sid);
    let pattern_lit = pattern.to_string();
    let regex_scan_ident = regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));
    quote! {
        {
            let span_lo = *p;
            let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                return ::core::result::Result::Err(
                    crate::runtime::ParseErr::Syntax {
                        offset: span_lo as u32, rule: None,
                    },
                );
            };
            *p = span_lo + match_len as usize;
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// Negate / Minus — guard-only.
// ─────────────────────────────────────────────────────────────────────

/// `Negate(inner)` — try inner; on success, fail with Syntax. No tape
/// record pushed. Mirrors walker's NotFollowedBy. On inner failure,
/// preserve `*p` and continue.
fn emit_negate_tape(
    inner: &IrNode,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let inner_attempt = emit_guard_attempt_tape(inner, support_mod, grammar_suffix, ir);
    quote! {
        {
            let save_p = *p;
            let attempt: ::core::result::Result<(), ()> = (|| {
                #inner_attempt
                Ok(())
            })();
            *p = save_p;
            if attempt.is_ok() {
                return ::core::result::Result::Err(
                    crate::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    },
                );
            }
        }
    }
}

fn emit_negate_visitor(
    inner: &IrNode,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let inner_attempt =
        emit_guard_attempt_visitor(inner, support_mod, grammar_suffix, ir);
    quote! {
        {
            let save_p = *p;
            let attempt: ::core::result::Result<(), ()> = (|| {
                #inner_attempt
                Ok(())
            })();
            *p = save_p;
            if attempt.is_ok() {
                return ::core::result::Result::Err(
                    crate::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    },
                );
            }
        }
    }
}

/// `Minus(primary, excluded)` — first check the excluded pattern; if
/// it would succeed at `*p`, fail. Otherwise parse the primary,
/// emitting its records. Mirrors walker's `emit_minus_arm`.
fn emit_minus_tape(
    primary: &IrNode,
    excluded: &IrNode,
    variant_idx: u8,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let excluded_attempt =
        emit_guard_attempt_tape(excluded, support_mod, grammar_suffix, ir);
    let primary_emit =
        emit_primary_tape(primary, variant_idx, support_mod, grammar_suffix, ir);
    quote! {
        {
            let save_p = *p;
            let excluded_result: ::core::result::Result<(), ()> = (|| {
                #excluded_attempt
                Ok(())
            })();
            *p = save_p;
            if excluded_result.is_ok() {
                return ::core::result::Result::Err(
                    crate::runtime::tape::DtaError::Syntax {
                        offset: save_p as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    },
                );
            }
            #primary_emit
        }
    }
}

fn emit_minus_visitor(
    primary: &IrNode,
    excluded: &IrNode,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let excluded_attempt =
        emit_guard_attempt_visitor(excluded, support_mod, grammar_suffix, ir);
    let primary_emit =
        emit_primary_visitor(primary, support_mod, grammar_suffix, ir);
    quote! {
        {
            let save_p = *p;
            let excluded_result: ::core::result::Result<(), ()> = (|| {
                #excluded_attempt
                Ok(())
            })();
            *p = save_p;
            if excluded_result.is_ok() {
                return ::core::result::Result::Err(
                    crate::runtime::ParseErr::Syntax {
                        offset: save_p as u32, rule: None,
                    },
                );
            }
            #primary_emit
        }
    }
}

/// Emit a guard-mode attempt for a node — returns `Ok(())` on match,
/// `Err(())` on failure. Used by Negate / Minus. No tape records are
/// committed; the caller wraps this in a rewind block.
fn emit_guard_attempt_tape(
    node: &IrNode,
    _support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
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
        IrNode::Regex(sid) => {
            let pattern = ir.get_string(*sid).to_string();
            let regex_scan_ident =
                regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));
            quote! {
                let Some(match_len) = #regex_scan_ident(#pattern, input, *p) else {
                    return Err(());
                };
                *p += match_len as usize;
            }
        }
        IrNode::Ref(rid) => match emit_ref_call_tape(grammar_suffix, *rid, ir) {
            Some(call) => quote! {
                if (#call).is_err() {
                    return Err(());
                }
            },
            None => quote! { return Err(()); },
        },
        _ => quote! { return Err(()); },
    }
}

fn emit_guard_attempt_visitor(
    node: &IrNode,
    _support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    use super::dispatcher::emit_ref_call_visitor;
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
        IrNode::Regex(sid) => {
            let pattern = ir.get_string(*sid).to_string();
            let regex_scan_ident =
                regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));
            quote! {
                let Some(match_len) = #regex_scan_ident(#pattern, input, *p) else {
                    return Err(());
                };
                *p += match_len as usize;
            }
        }
        IrNode::Ref(rid) => match emit_ref_call_visitor(grammar_suffix, *rid, ir) {
            Some(call) => quote! {
                if (#call).is_err() {
                    return Err(());
                }
            },
            None => quote! { return Err(()); },
        },
        _ => quote! { return Err(()); },
    }
}

/// Emit the primary-side of a Minus — a full-record-producing inline
/// position. Delegates back through [`emit_inline_position_tape`] for
/// non-leaf nodes (Alt / Regex / …) or emits direct byte matches for
/// Literal / Ref leaves.
fn emit_primary_tape(
    node: &IrNode,
    variant_idx: u8,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    match unwrap_trivia(node) {
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            let len = bytes.len();
            let byte_lits: Vec<TokenStream> =
                bytes.iter().map(|b| quote! { #b }).collect();
            let variant_lit = variant_idx;
            quote! {
                let at = *p;
                let end = at + #len;
                if input.len() < end || input[at..end] != [#(#byte_lits),*] {
                    return ::core::result::Result::Err(
                        crate::runtime::tape::DtaError::Syntax {
                            offset: at as u32,
                            failing_state: crate::runtime::tape::DtaStateId::NONE,
                            failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                        },
                    );
                }
                *p = end;
                let _ = builder.push_leaf_with(
                    crate::runtime::tape::TapeKind::Literal,
                    at as u32,
                    end as u32,
                    #variant_lit,
                    0,
                    crate::runtime::tape::PayloadData::None,
                );
            }
        }
        IrNode::Ref(rid) => match emit_ref_call_tape(grammar_suffix, *rid, ir) {
            // Walker-parity: on Ref-call failure inside a Minus-primary
            // we are already in a failure-commit state (the caller
            // propagates `?`), so the enclosing rule will itself fail
            // and its caller's truncation takes effect. No per-site
            // truncation needed here because the failure is terminal
            // at this position.
            Some(call) => quote! { let _ = (#call)?; },
            None => quote! {
                return ::core::result::Result::Err(
                    crate::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    },
                );
            },
        },
        IrNode::Regex(sid) => emit_regex_tape(*sid, variant_idx, grammar_suffix, ir),
        inner @ (IrNode::Alt(_, _) | IrNode::Negate(_) | IrNode::Minus(_, _) | IrNode::TokenDispatch { .. }) => {
            emit_inline_position_tape(inner, variant_idx, support_mod, grammar_suffix, ir)
        }
        _ => quote! {},
    }
}

fn emit_primary_visitor(
    node: &IrNode,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    use super::dispatcher::emit_ref_call_visitor;
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
                    return ::core::result::Result::Err(
                        crate::runtime::ParseErr::Syntax {
                            offset: at as u32, rule: None,
                        },
                    );
                }
                *p = end;
            }
        }
        IrNode::Ref(rid) => match emit_ref_call_visitor(grammar_suffix, *rid, ir) {
            Some(call) => quote! { (#call)?; },
            None => quote! {
                return ::core::result::Result::Err(
                    crate::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    },
                );
            },
        },
        IrNode::Regex(sid) => emit_regex_visitor(*sid, grammar_suffix, ir),
        inner @ (IrNode::Alt(_, _) | IrNode::Negate(_) | IrNode::Minus(_, _) | IrNode::TokenDispatch { .. }) => {
            emit_inline_position_visitor(inner, support_mod, grammar_suffix, ir)
        }
        _ => quote! {},
    }
}

// ─────────────────────────────────────────────────────────────────────
// TokenDispatch.
// ─────────────────────────────────────────────────────────────────────

/// Emit `TokenDispatch { token, arms, fallback }` as a
/// `TapeKind::TokenDispatch` compound with the token's records
/// followed by the winning arm's continuation (or the fallback on no
/// match).
///
/// Dispatch semantics follow the VM interpreter:
///
/// 1. Parse `token` — capture the span via `save_p .. *p`.
/// 2. For each arm, test whether the span's bytes match any of the
///    arm's `patterns` (each a `StringId` keyword). If `guard_byte`
///    is set, also require `input[*p] == guard`.
/// 3. On match, emit the arm's continuation.
/// 4. On no arm match, emit the `fallback` continuation.
fn emit_token_dispatch_tape(
    token: &IrNode,
    arms: &[bbnf_ir::TokenDispatchArm],
    fallback: &IrNode,
    variant_idx: u8,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let token_emit =
        emit_primary_tape(token, variant_idx, support_mod, grammar_suffix, ir);

    let mut per_arm: Vec<TokenStream> = Vec::with_capacity(arms.len());
    for arm in arms {
        let cont = emit_primary_tape(
            &arm.continuation,
            variant_idx,
            support_mod,
            grammar_suffix,
            ir,
        );
        let pattern_literals: Vec<TokenStream> = arm
            .patterns
            .iter()
            .map(|sid| {
                let bytes = ir.get_string(*sid).as_bytes();
                let byte_lits: Vec<TokenStream> =
                    bytes.iter().map(|b| quote! { #b }).collect();
                quote! { &[#(#byte_lits),*][..] }
            })
            .collect();
        let guard_check = if let Some(g) = arm.guard_byte {
            quote! { && input.get(*p).copied() == ::core::option::Option::Some(#g) }
        } else {
            quote! {}
        };
        per_arm.push(quote! {
            if !td_match
                && (#(token_span == #pattern_literals)||*)
                #guard_check
            {
                #cont
                td_match = true;
            }
        });
    }

    let fallback_emit = emit_primary_tape(
        fallback,
        variant_idx,
        support_mod,
        grammar_suffix,
        ir,
    );
    let variant_lit = variant_idx;

    quote! {
        {
            let td_lo = *p as u32;
            // AY-II.W0.b — walker-parity post-order TokenDispatch
            // compound. Capture first-child index pre-emission;
            // allocate the compound row post-children; override
            // child_off to point back at first-child.
            let td_child = builder.position();
            let token_lo = *p;
            #token_emit
            let token_span: &[u8] = &input[token_lo..*p];
            let mut td_match = false;
            #(#per_arm)*
            if !td_match {
                #fallback_emit
            }
            let td_hi = *p as u32;
            let __td_off = builder.begin_compound(
                crate::runtime::tape::TapeKind::TokenDispatch,
                td_lo,
                #variant_lit,
                0u8,
                0u8,
                0u16,
            );
            builder.end_compound_post_order(
                __td_off,
                td_hi,
                crate::runtime::tape::TapeOffset(td_child),
            );
        }
    }
}

fn emit_token_dispatch_visitor(
    token: &IrNode,
    arms: &[bbnf_ir::TokenDispatchArm],
    fallback: &IrNode,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let token_emit = emit_primary_visitor(token, support_mod, grammar_suffix, ir);
    let mut per_arm: Vec<TokenStream> = Vec::with_capacity(arms.len());
    for arm in arms {
        let cont = emit_primary_visitor(
            &arm.continuation,
            support_mod,
            grammar_suffix,
            ir,
        );
        let pattern_literals: Vec<TokenStream> = arm
            .patterns
            .iter()
            .map(|sid| {
                let bytes = ir.get_string(*sid).as_bytes();
                let byte_lits: Vec<TokenStream> =
                    bytes.iter().map(|b| quote! { #b }).collect();
                quote! { &[#(#byte_lits),*][..] }
            })
            .collect();
        let guard_check = if let Some(g) = arm.guard_byte {
            quote! { && input.get(*p).copied() == ::core::option::Option::Some(#g) }
        } else {
            quote! {}
        };
        per_arm.push(quote! {
            if !td_match
                && (#(token_span == #pattern_literals)||*)
                #guard_check
            {
                #cont
                td_match = true;
            }
        });
    }
    let fallback_emit =
        emit_primary_visitor(fallback, support_mod, grammar_suffix, ir);
    quote! {
        {
            let token_lo = *p;
            #token_emit
            let token_span: &[u8] = &input[token_lo..*p];
            let mut td_match = false;
            #(#per_arm)*
            if !td_match {
                #fallback_emit
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// Helpers — first-byte projection, trivia stripping, Seq flattening.
// ─────────────────────────────────────────────────────────────────────

/// Compute the first-byte set for a branch body. Returns an empty
/// Vec when the set is unbounded (Regex branches without
/// classification, Refs without `meta.first_set`).
///
/// Mirrors [`super::alt_dispatch::branch_first_bytes`] verbatim so the
/// inline Alt dispatcher and the top-level AltDispatch emitter agree
/// on byte-set projection.
fn branch_first_bytes(node: &IrNode, ir: &GrammarIR) -> Vec<u8> {
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
fn unwrap_trivia(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } => unwrap_trivia(inner.as_ref()),
        IrNode::OptionalWhitespace(inner) => unwrap_trivia(inner.as_ref()),
        _ => node,
    }
}

/// Flatten a Seq / Next / Skip chain into its structural children.
/// Used to project a Seq-branch into a sequence of byte-match
/// positions for the prefix-tree-factored keyword pattern.
fn flatten<'a>(node: &'a IrNode, out: &mut Vec<&'a IrNode>) {
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

