//! Typed-payload helpers for AltDispatch rules whose owning rule
//! carries a scalar `TypeDesc` (CSS `namedColor` → u32, BBNF / Sheets
//! analogues for u8 / f64 / bool). Keeps the prefix-factored Seq +
//! Alt traversal that recovers `(literal_bytes, scalar_payload)`
//! tuples plus the codegen that writes the payload bytes into the
//! arena and emits a payload-carrying leaf at branch commit time.

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::quote;

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
pub(super) fn alt_dispatch_rule_payload_spec(
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
        TypeDesc::U8 => Some((quote! { crate::runtime::tape::TapeKind::KvPair }, 1)),
        TypeDesc::U32 => Some((quote! { crate::runtime::tape::TapeKind::Span }, 4)),
        TypeDesc::F64 => Some((quote! { crate::runtime::tape::TapeKind::Span }, 8)),
        TypeDesc::Bool => Some((quote! { crate::runtime::tape::TapeKind::Span }, 1)),
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
pub(super) fn alt_dispatch_branch_literal_payloads(
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
pub(super) fn emit_typed_literal_payloads(
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
