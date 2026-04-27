//! AX.W0a.2.p — typed-payload Alt-branch emission (Class 2).
//!
//! Detects Alt branches whose every branch is `Map { inner:
//! literal-or-literal-chain, IntLit | BoolLit }` — the factored-Alt
//! shape Sheets `error_literal` and analogous CSS typed-discriminant
//! rules produce post-factoring — and emits a per-byte dispatch where
//! each branch commits to its literal prefix and writes the typed
//! byte payload via `push_leaf_with_arena_payload`. Matches the
//! walker's Alt+Literal layering so downstream view accessors see
//! the same record stream.

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::quote;

/// True iff every branch in `branches` carries a `Map { inner: literal
/// or literal-chain, IntLit | BoolLit }` annotation — the factored-Alt
/// shape Sheets `error_literal` and analogous CSS typed-discriminant
/// rules produce post-factoring.
///
/// Returns false when any branch is Ref-led, Regex-led, or structurally
/// incompatible with per-literal byte dispatch (the inline module's
/// linear-try fallback retains ownership of those cases).
pub(super) fn alt_branches_carry_typed_payloads(
    branches: &[bbnf_ir::AltBranch],
    ir: &GrammarIR,
) -> bool {
    if branches.is_empty() {
        return false;
    }
    branches
        .iter()
        .all(|b| !extract_branch_typed_payload(&b.node, ir).is_empty())
}

/// Extract every `(literal_bytes, payload_u32)` pair a Map-carrying
/// Alt-branch declares, including nested factored branches
/// (`Seq([Literal(prefix), Alt([Map{…}, Map{…}])])` expands to the
/// Cartesian product of `prefix` with each inner Map's suffix +
/// payload).
///
/// Recognized shapes (recursing through `Map` wrappers and descending
/// `Seq` / `Next` / `Skip` / nested `Alt` prefix-factored bodies):
///
/// - `Map { Literal(s), IntLit(n) }` — single-literal branch.
/// - `Map { Literal(s), BoolLit(b) }` — single-literal with bool.
/// - `Map { Seq([Literal(a), Literal(b), …]), IntLit(n) }` —
///   literal-chain branch.
/// - `Seq([Literal(prefix), Alt([Map{Literal(s1), IntLit(n1)}, …])])`
///   — factored branch emitted post-prefix-factoring (Sheets
///   `error_literal`'s `Seq("N", Alt("/A"→0, "ULL!"→4, "UM!"→6,
///   "AME?"→5))` pattern). Expands into four flat
///   `(literal_bytes, payload)` pairs.
///
/// Returns an empty vector when the branch is not structurally
/// eligible for the typed-payload byte-dispatch.
fn extract_branch_typed_payload(
    node: &bbnf_ir::IrNode,
    ir: &GrammarIR,
) -> Vec<(Vec<u8>, u32)> {
    use bbnf_ir::{FnDescriptor, IrNode, MapExpr};
    match node {
        IrNode::Map { inner, fn_id } => {
            let Some(fd) = ir.fns.get(*fn_id as usize) else {
                return Vec::new();
            };
            let FnDescriptor::Expr { expr, .. } = fd else {
                return Vec::new();
            };
            let payload = match expr {
                MapExpr::IntLit(n) => *n as u32,
                MapExpr::BoolLit(b) => {
                    if *b {
                        1u32
                    } else {
                        0u32
                    }
                }
                _ => return Vec::new(),
            };
            let Some(bytes) = branch_literal_bytes(inner, ir) else {
                return Vec::new();
            };
            if bytes.is_empty() {
                return Vec::new();
            }
            vec![(bytes, payload)]
        }
        IrNode::OptionalWhitespace(inner) => {
            extract_branch_typed_payload(inner, ir)
        }
        IrNode::Seq(children) => {
            // Factored prefix + Alt-of-Map branch. The canonical
            // shape is `Seq([Literal(prefix), Alt([Map{...}, ...])])`,
            // possibly interleaved with trivial `Epsilon` /
            // `OptionalWhitespace` positions the lifter admits at
            // prefix boundaries.
            let substantive: Vec<&IrNode> = children
                .iter()
                .filter(|c| {
                    !matches!(
                        c,
                        IrNode::Epsilon | IrNode::OptionalWhitespace(_)
                    )
                })
                .collect();
            if substantive.len() != 2 {
                return Vec::new();
            }
            // First position is a literal (or literal-chain) prefix.
            let Some(prefix) = branch_literal_bytes(substantive[0], ir) else {
                return Vec::new();
            };
            // Second position is an Alt whose branches each are
            // Map-annotated literals.
            let suffix_alt = substantive[1];
            let IrNode::Alt(suffix_branches, _) = suffix_alt else {
                return Vec::new();
            };
            let mut out = Vec::new();
            for suffix_branch in suffix_branches {
                let suffix_pairs =
                    extract_branch_typed_payload(&suffix_branch.node, ir);
                if suffix_pairs.is_empty() {
                    return Vec::new();
                }
                for (suffix_bytes, payload) in suffix_pairs {
                    let mut combined = prefix.clone();
                    combined.extend(suffix_bytes);
                    out.push((combined, payload));
                }
            }
            out
        }
        _ => Vec::new(),
    }
}

/// Collect a literal-chain's byte sequence from a flat Map-free
/// structural body. Descends `Seq` / `Next` / `Skip` /
/// `OptionalWhitespace` / `Map` (the inner Map the lifter leaves in
/// place after factoring); rejects when any position is non-literal
/// (Ref, Regex, Alt, Repeat, Negate, Minus, TokenDispatch).
fn branch_literal_bytes(
    node: &bbnf_ir::IrNode,
    ir: &GrammarIR,
) -> Option<Vec<u8>> {
    use bbnf_ir::IrNode;
    match node {
        IrNode::Literal(sid) => Some(ir.get_string(*sid).as_bytes().to_vec()),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            branch_literal_bytes(inner, ir)
        }
        IrNode::Seq(children) => {
            let mut acc = Vec::new();
            for c in children {
                acc.extend(branch_literal_bytes(c, ir)?);
            }
            Some(acc)
        }
        IrNode::Next(l, r) | IrNode::Skip(l, r) => {
            let mut acc = branch_literal_bytes(l, ir)?;
            acc.extend(branch_literal_bytes(r, ir)?);
            Some(acc)
        }
        _ => None,
    }
}

/// Emit a byte-dispatch Alt where each branch commits to its literal
/// prefix + writes the typed-byte payload via
/// `push_leaf_with_arena_payload`. The emitted block reads one outer
/// Alt compound around the chosen branch's leaf — matching the
/// walker's Alt+Literal layering so downstream view accessors see the
/// same record stream.
///
/// Branches whose first byte collides collapse into one match arm that
/// tries each longer prefix first, then shorter (mirrors the Keyword
/// emitter's prefix-length-descending admission).
pub(super) fn emit_alt_typed_payload_tape(
    branches: &[bbnf_ir::AltBranch],
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    use std::collections::BTreeMap;
    let _ = grammar_suffix;
    let mut by_first: BTreeMap<u8, Vec<(Vec<u8>, u32)>> = BTreeMap::new();
    for b in branches {
        let pairs = extract_branch_typed_payload(&b.node, ir);
        if pairs.is_empty() {
            return quote! {
                // Defensive fallback — the admission predicate
                // `alt_branches_carry_typed_payloads` screened this,
                // but emit nothing to keep codegen valid if a later
                // mutation violates it.
            };
        }
        for (bytes, payload) in pairs {
            let first = *bytes.first().unwrap_or(&0);
            by_first.entry(first).or_default().push((bytes, payload));
        }
    }
    let byte_arms: Vec<TokenStream> = by_first
        .iter()
        .map(|(first, group)| {
            // Longer prefixes first so the exact-match branch commits
            // before any prefix-only neighbour. Stable order preserves
            // grammar-declaration order within equal-length groups.
            let mut ordered: Vec<_> = group.clone();
            ordered.sort_by(|a, b| b.0.len().cmp(&a.0.len()));
            let tries: Vec<TokenStream> = ordered
                .iter()
                .map(|(bytes, payload)| {
                    let len = bytes.len();
                    let byte_lits: Vec<TokenStream> =
                        bytes.iter().map(|b| quote! { #b }).collect();
                    let payload_u32 = *payload;
                    quote! {
                        if input.len() >= *p + #len
                            && input[*p..*p + #len] == [#(#byte_lits),*]
                        {
                            let at = *p;
                            let end = at + #len;
                            *p = end;
                            let __arena_off: u32 =
                                builder.arena_mut().len() as u32;
                            builder.arena_mut().push((#payload_u32) as u8);
                            let _ = builder.push_leaf_with_arena_payload(
                                crate::runtime::tape::TapeKind::Span,
                                at as u32,
                                end as u32,
                                0u8,
                                0u8,
                                __arena_off,
                                1u32,
                            );
                            break 'try_branches;
                        }
                    }
                })
                .collect();
            let first_lit = *first;
            quote! {
                #first_lit => {
                    #(#tries)*
                }
            }
        })
        .collect();
    quote! {
        {
            let first = #support_mod::skip_space(input, p, state)
                .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;
            let alt_lo = *p as u32;
            // AY-II.W0.b — walker-parity post-order Alt compound.
            // B5.W6 — bracket the post-order children scope; the
            // compound row stamps via `begin_compound_post` and
            // `end_compound_post_order` absorbs the bracket bump.
            let alt_child = builder.enter_post_order_children();
            'try_branches: loop {
                match first {
                    #(#byte_arms)*
                    _ => {}
                }
                // B5.W6 — every byte-arm fell through; close the
                // bracket explicitly before propagating the error.
                builder.exit_post_order_children();
                return ::core::result::Result::Err(
                    crate::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    },
                );
            }
            let alt_hi = *p as u32;
            let __alt_off = builder.begin_compound_post(
                crate::runtime::tape::TapeKind::Alt,
                alt_lo,
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
