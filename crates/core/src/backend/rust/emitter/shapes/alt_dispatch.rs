//! AltDispatch-shape emitter — `parse_altdispatch_<grammar>_<rule>`.
//!
//! # Role — AX.W0a.2.b
//!
//! Emits per-grammar AltDispatch-shape parse functions for
//! generalized `Alt` byte-dispatchers (Wrap's strict superset).
//! Handles the mixed-leaf Alt pattern CSS `value`, BBNF `type_name`,
//! CSS `keyframeStop`, EBNF `letter`, and similar rules use.
//!
//! # Emission shape
//!
//! Byte-dispatch on the first non-whitespace byte:
//!
//! - Computes each branch's first-byte set via the branch body's
//!   structural head (Literal's first byte, Regex's NFA start set,
//!   Ref target's rule `meta.first_set`).
//! - Emits one match-arm per branch, keyed on the branch's first-
//!   byte set; overlapping sets fall through to a default arm that
//!   tries each branch in Alt order.
//! - For `Ref` branches the arm calls the target's shape fn via
//!   `dispatcher::emit_ref_call_tape` — no `__value` indirection.
//! - For `Literal` branches the arm matches the literal's bytes
//!   inline and pushes a `Literal` leaf.
//! - For `Regex` branches the arm performs a canonical non-whitespace
//!   scan (CSS `/[^\s;!}]+/` fallback pattern).
//! - For `Seq` branches (prefix-tree-factored keyword chains) the
//!   arm matches the flattened literal sequence inline.
//!
//! # Walker-parity compound
//!
//! Each admitted branch emits a `Rule`-kind outer compound stamping
//! `variant_idx = rule.id & 0xFF` so downstream `TapeView` readers
//! see the same shape the walker's Alt frame produces. The per-
//! branch leaf / compound pushes land as children of that Rule.

use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
use bbnf_ir::{AltBranch, GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::{
    emit_ref_call_tape, emit_ref_call_visitor, shape_fn_ident, visitor_shape_fn_ident,
};

/// AX.W0a.2.q — pick the payload-writing routine for a typed-Alt
/// rule's branch based on the owning rule's type. Returns
/// `Some((leaf_kind_tokens, payload_width_bytes))` when the rule is
/// typed with a scalar payload CSS / Sheets / BBNF ship. `None` for
/// rules whose inner type is not a single scalar — those fall through
/// to the pre-W0a.2.q plain-literal emission.
///
/// The policy mirrors the Keyword emitter's `rule_keyword_leaf_kind`:
/// `U8` → KvPair, every other scalar → Span. Matches the walker's
/// Seq-promotion output so walker-parity readers (`typed_u8_payloads`,
/// `payload_scalar::<u32>`) observe identical record kinds whether
/// the rule routes through Keyword (literal-led single-byte discriminants)
/// or AltDispatch (mixed leaf Alt dispatchers).
fn alt_dispatch_rule_payload_spec(
    rule: &IrRule,
    ir: &GrammarIR,
) -> Option<(TokenStream, u32)> {
    use bbnf_ir::TypeDesc;
    let ty = ir.types.iter().find_map(|(rid, t)| {
        if *rid == rule.id {
            Some(t.clone())
        } else {
            None
        }
    })?;
    match ty {
        TypeDesc::U8 => Some((quote! { ::bbnf::runtime::tape::TapeKind::KvPair }, 1)),
        TypeDesc::U32 => Some((quote! { ::bbnf::runtime::tape::TapeKind::Span }, 4)),
        TypeDesc::F64 => Some((quote! { ::bbnf::runtime::tape::TapeKind::Span }, 8)),
        TypeDesc::Bool => Some((quote! { ::bbnf::runtime::tape::TapeKind::Span }, 1)),
        _ => None,
    }
}

/// Extract the `u64` payload value from a Map-annotated `IntLit` /
/// `BoolLit` expression, widened enough to cover u32 / f64 forms.
/// Returns the raw literal and a boolean indicating whether the value
/// is an f64 bit pattern (for `TypeDesc::F64` rules).
///
/// Handles the CSS / Sheets / BBNF forms:
///   - `MapExpr::IntLit(n)`  — typed `Nu8` / `Nu32` / `Ni64`.
///   - `MapExpr::BoolLit(b)` — typed `bool`.
///   - `MapExpr::FloatLit(f)` — typed `f64`.
fn extract_map_scalar(node: &IrNode, ir: &GrammarIR) -> Option<(u64, bool)> {
    use bbnf_ir::{FnDescriptor, MapExpr};
    fn find_map_fn(n: &IrNode) -> Option<u32> {
        match n {
            IrNode::Map { fn_id, .. } => Some(*fn_id),
            IrNode::OptionalWhitespace(inner) => find_map_fn(inner),
            _ => None,
        }
    }
    let fn_id = find_map_fn(node)?;
    let fd = ir.fns.get(fn_id as usize)?;
    let FnDescriptor::Expr { expr, .. } = fd else {
        return None;
    };
    match expr {
        MapExpr::IntLit(n) => Some((*n as u64, false)),
        MapExpr::BoolLit(b) => Some((if *b { 1 } else { 0 }, false)),
        MapExpr::FloatLit(f) => Some((f.to_bits(), true)),
        _ => None,
    }
}

/// Strip `Map` / `OptionalWhitespace` and descend `Seq` /
/// nested-`Alt` prefix-factored branches down to the terminal
/// `Literal`. Returns every admissible `(literal_bytes, payload,
/// is_f64)` tuple a factored branch body expands to — the Cartesian
/// product of the prefix with each nested Alt branch's suffix +
/// payload, recursing through arbitrarily deep prefix-factored
/// subtrees.
///
/// Shapes recognised (composed recursively):
///   - `Map { <literal-chain>, <scalar> }` — typed literal leaf.
///   - `Seq([<literal-prefix>, <tail>])` where `<tail>` is an Alt of
///     further admissible branches, or a further prefix-factored Seq.
///   - `Seq([<literal-a>, <literal-b>, …, <tail>])` — multi-position
///     prefix chain; every leading position must be a pure literal.
///
/// CSS `namedColor`'s post-factoring shape is arbitrarily deep —
/// `"dark" , <Alt(blue|red|slate, <slate, <blue|grey>>)>` produces
/// `Seq(Literal("d"), Alt(..., Seq(Literal("ark"), Seq(Literal("slate"),
/// Alt(..., Alt(blue, grey))))))`. The recursive descent below
/// collects the full Cartesian product so every branch's `u32`
/// discriminant reaches the tape.
fn alt_dispatch_branch_literal_payloads(
    node: &IrNode,
    ir: &GrammarIR,
) -> Vec<(Vec<u8>, u64, bool)> {
    use bbnf_ir::IrNode;
    match node {
        IrNode::Map { inner, .. } => {
            let Some((payload, is_f64)) = extract_map_scalar(node, ir) else {
                return Vec::new();
            };
            let Some(bytes) = flat_literal_bytes(inner, ir) else {
                return Vec::new();
            };
            if bytes.is_empty() {
                return Vec::new();
            }
            vec![(bytes, payload, is_f64)]
        }
        IrNode::OptionalWhitespace(inner) => {
            alt_dispatch_branch_literal_payloads(inner, ir)
        }
        IrNode::Seq(children) => {
            // Partition the Seq into a (possibly multi-position)
            // literal prefix + a tail. The tail is either a Map
            // (typed leaf), an Alt (further prefix-factoring), or
            // another Seq (deeper nesting).
            let substantive: Vec<&IrNode> = children
                .iter()
                .filter(|c| {
                    !matches!(
                        c,
                        IrNode::Epsilon | IrNode::OptionalWhitespace(_)
                    )
                })
                .collect();
            if substantive.len() < 2 {
                return Vec::new();
            }
            // Collect the literal prefix, stopping at the first non-
            // literal position. Every intervening literal chain
            // concatenates.
            let mut prefix_bytes = Vec::new();
            let mut tail_idx = 0;
            for (i, pos) in substantive.iter().enumerate() {
                if let Some(b) = flat_literal_bytes(pos, ir) {
                    prefix_bytes.extend(b);
                } else {
                    tail_idx = i;
                    break;
                }
            }
            if tail_idx == 0 {
                // Every position was literal — no typed-payload
                // leaf, the branch is just a plain-literal chain.
                return Vec::new();
            }
            // All positions after tail_idx must be structural; combine
            // them as a Seq of the remaining positions.
            let tail_positions: Vec<&IrNode> =
                substantive.iter().skip(tail_idx).copied().collect();
            if tail_positions.is_empty() {
                return Vec::new();
            }
            // Single tail position — descend directly.
            let tail_expansions = if tail_positions.len() == 1 {
                alt_dispatch_branch_literal_payloads(tail_positions[0], ir)
            } else {
                // Multiple tail positions — reconstruct a Seq and
                // recurse. The recursion handles arbitrary depth of
                // prefix + nested-Alt + further prefix chains.
                let reconstructed = IrNode::Seq(
                    tail_positions.iter().map(|n| (*n).clone()).collect(),
                );
                alt_dispatch_branch_literal_payloads(&reconstructed, ir)
            };
            if tail_expansions.is_empty() {
                return Vec::new();
            }
            let mut out = Vec::with_capacity(tail_expansions.len());
            for (tail_bytes, payload, is_f64) in tail_expansions {
                let mut combined = prefix_bytes.clone();
                combined.extend(tail_bytes);
                out.push((combined, payload, is_f64));
            }
            out
        }
        IrNode::Alt(branches, _) => {
            // Bare Alt — every sub-branch must be admissible. Fold
            // the results, failing the whole Alt if any sub-branch
            // is not typed-payload.
            let mut out = Vec::new();
            for b in branches {
                let sub = alt_dispatch_branch_literal_payloads(&b.node, ir);
                if sub.is_empty() {
                    return Vec::new();
                }
                out.extend(sub);
            }
            out
        }
        _ => Vec::new(),
    }
}

/// Flatten a pure-literal body into its byte sequence. Rejects on any
/// non-literal position (Ref / Regex / Alt / Repeat / …).
fn flat_literal_bytes(node: &IrNode, ir: &GrammarIR) -> Option<Vec<u8>> {
    use bbnf_ir::IrNode;
    match node {
        IrNode::Literal(sid) => Some(ir.get_string(*sid).as_bytes().to_vec()),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            flat_literal_bytes(inner, ir)
        }
        IrNode::Seq(children) => {
            let mut acc = Vec::new();
            for c in children {
                acc.extend(flat_literal_bytes(c, ir)?);
            }
            Some(acc)
        }
        IrNode::Next(l, r) | IrNode::Skip(l, r) => {
            let mut acc = flat_literal_bytes(l, ir)?;
            acc.extend(flat_literal_bytes(r, ir)?);
            Some(acc)
        }
        _ => None,
    }
}

/// Emit `pub fn parse_altdispatch_<grammar>_<rule>(input, p, state,
/// builder) -> Result<TapeOffset, DtaError>`.
pub fn emit_parse_alt_dispatch(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("altdispatch", grammar_suffix, rule_name);
    let variant_idx = (rule.id & 0xFF) as u8;
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let body = unwrap_trivia(&rule.body);
    let IrNode::Alt(branches, _) = body else {
        return quote! {};
    };

    // Build (first-byte-set, branch-arm-body) pairs.
    // AX.W0a.2.q — pass `rule` so typed-Alt Literal branches can emit
    // the owning rule's payload (CSS `namedColor` → u32; analogous U8
    // / F64 shapes). Untyped rules fall through to the pre-W0a.2.q
    // plain-literal emission.
    let dispatch_arms = emit_dispatch_arms(branches, grammar_suffix, rule, ir);

    quote! {
        /// AX.W0a.2.b — per-grammar AltDispatch-shape parse function.
        ///
        /// Generalized byte-dispatch over `Alt(leaf, leaf, …)` bodies.
        /// Each branch is a classified-Ref, a Literal, a Regex, or a
        /// leaf-only Seq (prefix-tree factoring). No recursion through
        /// `__value` — per-Ref routing emits direct shape-fn calls.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
        /// recursion rationale.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut, unused_assignments, unreachable_code)]
        pub fn #fn_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut ::bbnf::runtime::tape::TapeBuilder,
        ) -> ::core::result::Result<
            ::bbnf::runtime::tape::TapeOffset,
            ::bbnf::runtime::tape::DtaError,
        > {
            let first = #support_mod::skip_space(input, p, state)
                .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;
            let alt_lo = *p as u32;
            // AY-II.W0.b — alt compound is walker-parity POST-ORDER:
            // branch records emit first, then the outer Rule row lands
            // after them. Capture the first-child index before branch
            // emission; allocate the compound row at post-branch
            // position via begin_compound; close immediately; override
            // child_off to name the first branch record.
            let alt_child = builder.columns_mut().len() as u32;
            let _ = alt_child;
            #dispatch_arms
            let alt_hi = *p as u32;
            let off = builder.begin_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                alt_lo,
                #variant_idx,
                0u8,
                0u8,
                0u16,
            );
            builder.end_compound_post_order(
                off,
                alt_hi,
                ::bbnf::runtime::tape::TapeOffset(alt_child),
            );
            Ok(::bbnf::runtime::tape::TapeOffset(off))
        }
    }
}

/// Emit the dispatch body for an AltDispatch rule — collects per-
/// branch first-byte sets and emits a match over the first byte,
/// with fallback linear scan for overlapping sets.
fn emit_dispatch_arms(
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
    // because TapeBuilder has no child truncation; instead each
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
        // checkpoint for rollback — the outer's own begin_compound
        // will back-patch child_off once branch records land).
        let save_child = builder.columns_mut().len() as u32;
        let _ = save_p;
        let _ = save_child;
        'try_branches: loop {
            match first {
                #(#byte_arms)*
                _ => {}
            }
            #(#fallback_arms)*
            return Err(::bbnf::runtime::tape::DtaError::Syntax {
                offset: *p as u32,
                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
            });
        }
    }
}

/// AX.W0a.2.q — emit a typed-payload Literal-branch attempt for an
/// AltDispatch rule. Each `(literal_bytes, payload, is_f64)` entry
/// produces an inline byte-match that commits `*p`, writes the
/// payload bytes into the tape arena (LE for u32 / f64; single byte
/// for u8 / bool), and pushes a payload-carrying leaf. Longer literal
/// prefixes try first so factored-Alt branches (`"darkblue"` before
/// `"dark"`) commit to the exact-match form.
///
/// Width semantics:
///   1 byte  → `payload & 0xFF` as u8 (u8 / bool rules).
///   4 bytes → `payload as u32` little-endian (u32 rules — CSS
///             `namedColor` canonical case).
///   8 bytes → when `is_f64`, `f64::from_bits(payload)` then LE
///             bytes; otherwise `payload as u64` LE.
fn emit_typed_literal_payloads(
    pairs: &[(Vec<u8>, u64, bool)],
    rule_variant_idx: u8,
    leaf_kind: &TokenStream,
    payload_width: u32,
) -> TokenStream {
    // Longer prefixes first, stable on equal length.
    let mut ordered: Vec<_> = pairs.to_vec();
    ordered.sort_by(|a, b| b.0.len().cmp(&a.0.len()));
    let tries: Vec<TokenStream> = ordered
        .iter()
        .map(|(bytes, payload, is_f64)| {
            let len = bytes.len();
            let byte_lits: Vec<TokenStream> =
                bytes.iter().map(|b| quote! { #b }).collect();
            let arena_write = match payload_width {
                1 => {
                    let v = (*payload & 0xFF) as u8;
                    quote! {
                        builder.arena_mut().push(#v);
                    }
                }
                4 => {
                    let v = (*payload & 0xFFFFFFFF) as u32;
                    quote! {
                        builder.arena_mut().extend_from_slice(
                            &(#v as u32).to_le_bytes(),
                        );
                    }
                }
                8 if *is_f64 => {
                    // Restore the f64 bit pattern — the lifter stored
                    // `FloatLit(f).to_bits()` so `f64::from_bits` round-
                    // trips.
                    let bits = *payload;
                    quote! {
                        let __f: f64 = f64::from_bits(#bits);
                        builder.arena_mut().extend_from_slice(
                            &__f.to_le_bytes(),
                        );
                    }
                }
                8 => {
                    let v = *payload;
                    quote! {
                        builder.arena_mut().extend_from_slice(
                            &(#v as u64).to_le_bytes(),
                        );
                    }
                }
                _ => quote! {
                    // Unsupported payload width — no-op so the branch
                    // still commits structurally even if the decode
                    // is not emitted. Caller's predicate screened for
                    // supported widths; this arm is defensive.
                },
            };
            let width_lit = payload_width;
            quote! {
                {
                    let at = *p;
                    let end = at + #len;
                    if input.len() >= end
                        && input[at..end] == [#(#byte_lits),*]
                    {
                        *p = end;
                        let __arena_off: u32 = builder.arena_mut().len() as u32;
                        #arena_write
                        let _ = builder.push_leaf_with_arena_payload(
                            #leaf_kind,
                            at as u32,
                            end as u32,
                            #rule_variant_idx,
                            0u8,
                            __arena_off,
                            #width_lit,
                        );
                        break 'try_branches;
                    }
                }
            }
        })
        .collect();
    quote! {
        #(#tries)*
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
                let body = super::inline::emit_seq_branch_structural_tape(
                    inner, &support_mod, grammar_suffix, ir,
                );
                quote! {
                    {
                        let attempt_p = *p;
                        let attempt_len = builder.columns_mut().len() as u32;
                        let attempt: ::core::result::Result<(), ()> = (|| {
                            #body
                            Ok(())
                        })();
                        match attempt {
                            Ok(_) => break 'try_branches,
                            Err(_) => {
                                *p = attempt_p;
                                builder.columns_mut().rollback_to(attempt_len);
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
                    ::bbnf::runtime::tape::TapeKind::Literal,
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
    let regex_scan_ident = super::super::dfa_codegen::regex_scan_adapter_ident(
        &super::sanitise_grammar(grammar_suffix),
    );
    quote! {
        {
            let span_lo = *p as u32;
            if let ::core::option::Option::Some(match_len) =
                #regex_scan_ident(#pattern, input, *p)
            {
                *p += match_len as usize;
                let _ = builder.push_leaf(
                    ::bbnf::runtime::tape::TapeKind::Span,
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

/// Regex-branch attempt — canonical non-whitespace scan. Matches
/// CSS's `/[^\s;!}]+/` catch-all and similar fallback patterns.
/// Retained for fallback cases where no structural regex pattern is
/// attached to a branch (shouldn't occur under AltDispatch admission,
/// but kept as a defensive path).
#[allow(dead_code)]
fn emit_regex_attempt() -> TokenStream {
    quote! {
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
                let _ = builder.push_leaf(
                    ::bbnf::runtime::tape::TapeKind::Literal,
                    at as u32,
                    q as u32,
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
                        ::bbnf::runtime::tape::TapeKind::Literal,
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

/// Compute the first-byte set for a branch body. Returns an empty
/// Vec when the set is unbounded (Regex branches without
/// classification, Refs without `meta.first_set`).
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
fn unwrap_trivia(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } => unwrap_trivia(inner.as_ref()),
        IrNode::OptionalWhitespace(inner) => unwrap_trivia(inner.as_ref()),
        _ => node,
    }
}

// ─────────────────────────────────────────────────────────────────────
// Visitor-path AltDispatch emitter.
// ─────────────────────────────────────────────────────────────────────

/// Emit `pub fn parse_altdispatch_visitor_<grammar>_<rule><V>(input,
/// p, state, visitor) -> Result<(), ParseErr>`.
pub fn emit_parse_alt_dispatch_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("altdispatch", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let body = unwrap_trivia(&rule.body);
    let IrNode::Alt(branches, _) = body else {
        return quote! {};
    };

    let dispatch_arms = emit_dispatch_arms_visitor(branches, grammar_suffix, ir);

    quote! {
        /// AX.W0a.2.b — visitor-path AltDispatch-shape parse function.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]`.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut, unused_assignments, unreachable_code)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            visitor: &mut V,
        ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
        where
            V: ::bbnf::runtime::tape::ObjectVisitor
                + ::bbnf::runtime::tape::ArrayVisitor
                + ::bbnf::runtime::tape::StringVisitor
                + ::bbnf::runtime::tape::NumberVisitor
                + ::bbnf::runtime::tape::KeywordVisitor,
        {
            let first = #support_mod::skip_space(input, p, state)
                .ok_or(::bbnf::runtime::ParseErr::Syntax {
                    offset: *p as u32, rule: None,
                })?;
            #dispatch_arms
            Ok(())
        }
    }
}

fn emit_dispatch_arms_visitor(
    branches: &[AltBranch],
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let mut enumerated: Vec<(Vec<u8>, TokenStream)> = Vec::new();
    for branch in branches {
        let first_bytes = branch_first_bytes(&branch.node, ir);
        let body = emit_branch_body_visitor(&branch.node, grammar_suffix, ir);
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
        'try_branches: loop {
            match first {
                #(#byte_arms)*
                _ => {}
            }
            #(#fallback_arms)*
            return Err(::bbnf::runtime::ParseErr::Syntax {
                offset: *p as u32, rule: None,
            });
        }
    }
}

fn emit_branch_body_visitor(
    node: &IrNode,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
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
            emit_seq_attempt_visitor(inner, ir)
        }
        _ => quote! {},
    }
}

fn emit_seq_attempt_visitor(seq: &IrNode, ir: &GrammarIR) -> TokenStream {
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

/// Static assertion to preserve the ShapeTag import.
#[allow(dead_code)]
const _: fn(ShapeTag) -> bool = ShapeTag::is_w4_classified;
