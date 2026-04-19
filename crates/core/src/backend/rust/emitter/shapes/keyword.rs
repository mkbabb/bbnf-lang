//! Keyword-shape emitter — `parse_keyword_<grammar>_<rule>`.
//!
//! # Role — AW-V.W3.2
//!
//! Emits the per-grammar Keyword-shape parse function. Handles two
//! admitted sub-cases per W3.1's keyword detector:
//!
//! 1. **Single-literal body** — e.g. JSON's `null = "null" -> 0u8`.
//!    Emits a direct byte-sequence match + a Literal leaf push with
//!    the rule's `-> <value>` payload.
//! 2. **Alt of literal-led branches** — e.g. JSON's
//!    `bool = "true" -> true | "false" -> false`. Emits a byte-dispatch
//!    over the discriminator byte + per-branch match + literal leaf
//!    push carrying the branch-specific payload.
//!
//! The payload inference reads the rule's `-> <expr>` annotation from
//! the IR's `FnDescriptor` list indirectly (via the existing
//! `MapExpr::Const` / `MapExpr::BoolLit` paths). For JSON the two
//! known keyword payloads are:
//!
//! - `null = "null" -> 0u8` → [`PayloadData::InlineScalar(0u32)`]
//! - `bool = "true" -> true | "false" -> false` →
//!   [`PayloadData::InlineScalar(1u32)`] and
//!   [`PayloadData::InlineScalar(0u32)`] respectively (per
//!   [`NumberVisitor`]'s `bool(value as u32)` convention in
//!   `bbnf-tape::visitor`).

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::{
    emit_ref_call_tape, emit_ref_call_visitor, shape_fn_ident, visitor_shape_fn_ident,
};

/// Emit `pub fn parse_keyword_<grammar>_<rule>(input, p, first_byte,
/// state, builder) -> Result<TapeOffset, DtaError>`.
///
/// AX.W0a.2.g — `state: &mut ScanState` threaded through the signature
/// so Ref-led Alt branches can delegate to their target rule's shape
/// fn via [`emit_ref_call_tape`]. Single-literal arms ignore `state`
/// (no downstream skip_space); the Alt arm's Ref branches forward
/// `state` into the target fn call. Legacy single-literal + pure-
/// Literal Alt emission is byte-identical to the pre-W0a.2.g form
/// modulo the extra parameter.
pub fn emit_parse_keyword(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("keyword", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let variant_idx = (rule.id & 0xFF) as u8;
    let _ = ir;

    // Detect the two sub-cases by body shape.
    let body = unwrap_trivia(&rule.body);
    match body {
        IrNode::Literal(sid) => {
            // Single-literal body — emit a byte-sequence match.
            let bytes = ir.get_string(*sid).as_bytes();
            let len = bytes.len();
            // Default payload: the rule's `-> 0u8` annotation; when
            // the rule doesn't carry a `-> const` payload, fall back
            // to pushing an empty Literal leaf (no payload).
            let payload = literal_payload_for(rule, ir);
            // Payload: Aggregate(&[byte]) for 1-byte arena slot; the
            // reader's `payload_bytes(rec, 1)` pulls the byte back
            // out. For a missing annotation we default to 0.
            let payload_byte = match payload.as_ref() {
                Some(_) => {
                    // Cast the u32 literal to u8 for the 1-byte arena
                    // slot. `payload` is always 0 / 1 for bool/null.
                    quote! { #payload }
                }
                None => quote! { 0u32 },
            };
            let payload_push = quote! {
                ::bbnf::runtime::tape::PayloadData::Aggregate(&[(#payload_byte) as u8])
            };
            let byte_lits: Vec<TokenStream> = bytes
                .iter()
                .map(|b| {
                    let lit = *b;
                    quote! { #lit }
                })
                .collect();
            quote! {
                /// AW-V.W3.2 — per-grammar Keyword-shape parse function
                /// (single-literal body).
                ///
                /// AX.W0a.2.g — `state` parameter unused for single-
                /// literal form (no downstream ws-skip / Ref delegation);
                /// present so every `parse_keyword_<grammar>_<rule>`
                /// shares one signature across sub-cases.
                #[inline(always)]
                #[allow(non_snake_case, clippy::too_many_arguments)]
                pub fn #fn_ident(
                    input: &[u8],
                    p: &mut usize,
                    _first_byte: u8,
                    _state: &mut #support_mod::ScanState,
                    builder: &mut ::bbnf::runtime::tape::TapeBuilder,
                ) -> ::core::result::Result<
                    ::bbnf::runtime::tape::TapeOffset,
                    ::bbnf::runtime::tape::DtaError,
                > {
                    let at = *p;
                    let end = at + #len;
                    if input.len() < end || input[at..end] != [#(#byte_lits),*] {
                        return Err(::bbnf::runtime::tape::DtaError::Syntax {
                            offset: at as u32,
                            failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                            failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                        });
                    }
                    *p = end;
                    // Span kind + Aggregate(&[byte]) matches the
                    // existing walker's emission — the bench's
                    // `tape.payload_bytes(rec, 1)` reader consumes
                    // the 1-byte arena slot.
                    let off = builder.push_leaf_with(
                        ::bbnf::runtime::tape::TapeKind::Span,
                        at as u32,
                        end as u32,
                        #variant_idx,
                        0,
                        #payload_push,
                    );
                    Ok(off)
                }
            }
        }
        IrNode::Alt(branches, _) => {
            // AX.W0a.2.g — Alt arm admits both Literal-led and Ref-led
            // branches. The W3.1 keyword detector accepts
            // `leading_literal_rec(branch)` which follows Ref targets
            // through to their body's literal prefix (BBNF `directive`
            // canonical case). For each branch we emit per-first-byte
            // arms; arms share a first byte when multiple branches'
            // leading literals collide on byte 0 (BBNF: every directive
            // branch starts with `@`). Inside a shared arm, each branch
            // checks its full prefix (e.g. `@import`, `@recover`) before
            // committing to its call.
            use std::collections::BTreeMap;

            // Collect `(leading_literal_bytes, branch_ref_or_literal)`
            // for each branch. Literal-led branches carry `None` for
            // the target; Ref-led branches carry `Some(rid)` so the
            // emission dispatches to the target's shape fn via
            // `emit_ref_call_tape`.
            let per_branch: Vec<(Vec<u8>, Option<bbnf_ir::RuleId>, usize, &bbnf_ir::AltBranch)> =
                branches
                    .iter()
                    .enumerate()
                    .filter_map(|(branch_idx, branch)| {
                        let body = unwrap_trivia(&branch.node);
                        match body {
                            IrNode::Literal(sid) => {
                                let bytes = ir.get_string(*sid).as_bytes().to_vec();
                                if bytes.is_empty() {
                                    return None;
                                }
                                Some((bytes, None, branch_idx, branch))
                            }
                            IrNode::Ref(rid) => {
                                let bytes = leading_literal_bytes(body, ir)?;
                                if bytes.is_empty() {
                                    return None;
                                }
                                Some((bytes, Some(*rid), branch_idx, branch))
                            }
                            _ => None,
                        }
                    })
                    .collect();

            // Group per-branch entries by their first byte; each group
            // becomes one `#first => { ... }` match arm.
            let mut by_first: BTreeMap<u8, Vec<&(Vec<u8>, Option<bbnf_ir::RuleId>, usize, &bbnf_ir::AltBranch)>> =
                BTreeMap::new();
            for entry in &per_branch {
                by_first.entry(entry.0[0]).or_default().push(entry);
            }

            let arms: Vec<TokenStream> = by_first
                .iter()
                .map(|(first, group)| {
                    // For each branch in this first-byte group, emit a
                    // full-prefix check + commit. Branches are ordered
                    // by their original Alt position (stable per the
                    // per_branch build order).
                    let tries: Vec<TokenStream> = group
                        .iter()
                        .map(|(bytes, target_ref, branch_idx, branch)| {
                            let len = bytes.len();
                            let byte_lits: Vec<TokenStream> =
                                bytes.iter().map(|b| {
                                    let lit = *b;
                                    quote! { #lit }
                                }).collect();
                            if let Some(target_rid) = target_ref {
                                // Ref branch — prefix check then delegate to the
                                // target's shape fn. `emit_ref_call_tape`'s stream
                                // already assumes a classified target; admission
                                // guarantees that. The prefix check fires only
                                // when the ENTIRE leading literal matches, so the
                                // target's shape fn sees `*p` pointed at its own
                                // recognizable prefix.
                                let ref_call = emit_ref_call_tape(grammar_suffix, *target_rid, ir)
                                    .unwrap_or_else(|| quote! {
                                        ::core::result::Result::Err(
                                            ::bbnf::runtime::tape::DtaError::Syntax {
                                                offset: *p as u32,
                                                failing_state:
                                                    ::bbnf::runtime::tape::DtaStateId::NONE,
                                                failing_rule:
                                                    ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                                            },
                                        )
                                    });
                                let _ = (branch_idx, branch);
                                quote! {
                                    if input.len() >= *p + #len
                                        && input[*p..*p + #len] == [#(#byte_lits),*]
                                    {
                                        return (#ref_call);
                                    }
                                }
                            } else {
                                // Literal branch — emit the legacy
                                // byte-sequence-match + Literal leaf
                                // push with per-branch payload.
                                let branch_payload =
                                    alt_branch_payload(rule, branch, *branch_idx, ir);
                                // AW-V.W3-fix (cursor parity): walker
                                // emits meta_idx=0 for every leaf —
                                // `push_leaf_fused` packs `kind_meta =
                                // kind & 0x0F` with no meta_idx slot,
                                // and the Alt frame's `cursor` (branch
                                // index) is stamped into the COMPOUND's
                                // `flags` by close_compound, not into
                                // leaf meta_idx.
                                quote! {
                                    if input.len() >= *p + #len
                                        && input[*p..*p + #len] == [#(#byte_lits),*]
                                    {
                                        let at = *p;
                                        let end = at + #len;
                                        *p = end;
                                        let off = builder.push_leaf_with(
                                            ::bbnf::runtime::tape::TapeKind::Span,
                                            at as u32,
                                            end as u32,
                                            #variant_idx,
                                            0u8,
                                            #branch_payload,
                                        );
                                        return Ok(off);
                                    }
                                }
                            }
                        })
                        .collect();
                    quote! {
                        #first => {
                            #(#tries)*
                            return Err(::bbnf::runtime::tape::DtaError::Syntax {
                                offset: *p as u32,
                                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                            });
                        }
                    }
                })
                .collect();
            quote! {
                /// AW-V.W3.2 — per-grammar Keyword-shape parse function
                /// (Alt of literal-led or Ref-led branches).
                ///
                /// AX.W0a.2.g — admits Ref-led branches whose target
                /// resolves to a literal-prefix body (per `leading_
                /// literal_bytes`). For each first-byte group, each
                /// candidate's full prefix is checked before committing:
                /// Literal branches emit the legacy leaf push;
                /// Ref branches delegate to the target's shape fn via
                /// [`emit_ref_call_tape`], threading `state` through.
                #[inline(always)]
                #[allow(non_snake_case, clippy::too_many_arguments)]
                pub fn #fn_ident(
                    input: &[u8],
                    p: &mut usize,
                    first_byte: u8,
                    state: &mut #support_mod::ScanState,
                    builder: &mut ::bbnf::runtime::tape::TapeBuilder,
                ) -> ::core::result::Result<
                    ::bbnf::runtime::tape::TapeOffset,
                    ::bbnf::runtime::tape::DtaError,
                > {
                    let _ = state;
                    match first_byte {
                        #(#arms)*
                        _ => Err(::bbnf::runtime::tape::DtaError::Syntax {
                            offset: *p as u32,
                            failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                            failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                        }),
                    }
                }
            }
        }
        _ => quote! {},
    }
}

/// Extract a leading literal byte-sequence from a branch body.
/// Mirrors the recognizer-side `leading_literal_rec` walk so the emitter
/// can cheaply recover the same prefix the keyword detector admitted.
/// Handles Literal / Seq-prefix / Skip / Next / Map /
/// OptionalWhitespace / Ref (one-step), with a simple depth bound to
/// avoid cyclic Ref chains.
fn leading_literal_bytes(node: &IrNode, ir: &GrammarIR) -> Option<Vec<u8>> {
    fn rec(
        node: &IrNode,
        ir: &GrammarIR,
        depth: u32,
        visited: &mut std::collections::HashSet<bbnf_ir::RuleId>,
    ) -> Option<Vec<u8>> {
        if depth > 16 {
            return None;
        }
        match node {
            IrNode::Literal(sid) => Some(ir.get_string(*sid).as_bytes().to_vec()),
            IrNode::Seq(children) if !children.is_empty() => {
                rec(&children[0], ir, depth + 1, visited)
            }
            IrNode::Skip(a, _) | IrNode::Next(a, _) => rec(a, ir, depth + 1, visited),
            IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
                rec(inner, ir, depth + 1, visited)
            }
            IrNode::Ref(rid) => {
                if !visited.insert(*rid) {
                    return None;
                }
                let rule = ir.rules.iter().find(|r| r.id == *rid)?;
                rec(&rule.body, ir, depth + 1, visited)
            }
            _ => None,
        }
    }
    let mut visited = std::collections::HashSet::new();
    rec(node, ir, 0, &mut visited)
}

/// Extract the rule's `-> <const>` scalar payload if present. Returns
/// `Some(u32)` for `InlineScalar` form; `None` when the rule has no
/// `-> ` annotation or carries a non-scalar payload.
fn literal_payload_for(rule: &IrRule, ir: &GrammarIR) -> Option<TokenStream> {
    // Walk the rule body looking for Map { inner: Literal, fn_id }.
    fn find_map_fn(node: &IrNode) -> Option<u32> {
        match node {
            IrNode::Map { fn_id, .. } => Some(*fn_id),
            IrNode::OptionalWhitespace(inner) => find_map_fn(inner),
            _ => None,
        }
    }
    let fn_id = find_map_fn(&rule.body)?;
    let fn_desc = ir.fns.get(fn_id as usize)?;
    payload_from_fn(fn_desc, ir)
}

/// Per-branch payload for an Alt-of-literals. Branch index is passed
/// so we can discriminate (e.g. `true`→1, `false`→0).
fn alt_branch_payload(
    _rule: &IrRule,
    branch: &bbnf_ir::AltBranch,
    _branch_idx: usize,
    ir: &GrammarIR,
) -> TokenStream {
    // Walk the branch body for a Map { fn_id } annotation.
    fn find_map_fn(node: &IrNode) -> Option<u32> {
        match node {
            IrNode::Map { fn_id, .. } => Some(*fn_id),
            IrNode::OptionalWhitespace(inner) => find_map_fn(inner),
            _ => None,
        }
    }
    let payload = find_map_fn(&branch.node)
        .and_then(|fid| ir.fns.get(fid as usize))
        .and_then(|fd| payload_from_fn(fd, ir));
    let payload_byte = match payload {
        Some(value) => quote! { #value },
        None => quote! { 0u32 },
    };
    quote! {
        ::bbnf::runtime::tape::PayloadData::Aggregate(&[(#payload_byte) as u8])
    }
}

/// Extract a `u32` payload value from a `FnDescriptor` when possible.
///
/// Handles the simple cases W3.2 admits (single-literal + 2-branch
/// bool). More nuanced payload typing (F64 / U32 / Aggregate) is
/// out-of-scope for Keyword-shape (numbers route through
/// Number-shape; strings route through String-shape).
fn payload_from_fn(fn_desc: &bbnf_ir::FnDescriptor, ir: &GrammarIR) -> Option<TokenStream> {
    use bbnf_ir::{FnDescriptor, MapExpr};
    let FnDescriptor::Expr { expr, .. } = fn_desc else {
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
        MapExpr::StringLit(sid) => {
            // `"null" -> 0u8` can also lower as IntLit; if it lowers
            // as StringLit we conservatively pick 0.
            let _ = sid;
            let _ = ir;
            Some(quote! { 0u32 })
        }
        _ => None,
    }
}

/// Strip `Map` / `OptionalWhitespace` trivia wrappers.
fn unwrap_trivia(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } => unwrap_trivia(inner.as_ref()),
        IrNode::OptionalWhitespace(inner) => unwrap_trivia(inner.as_ref()),
        _ => node,
    }
}

// ─────────────────────────────────────────────────────────────────────
// AW-V.W3-bench-fix — visitor-path Keyword emitter.
//
// Mirrors the prototype's `expect_keyword` + `visitor.bool(...)` /
// `visitor.null()` invocations at the dispatcher arm. Two sub-cases:
//
// - `null = "null" -> 0u8` → `visitor.null()`
// - `bool = "true" -> true | "false" -> false` →
//   `visitor.bool(true)` / `visitor.bool(false)` per branch.
// ─────────────────────────────────────────────────────────────────────

/// Emit `pub fn parse_keyword_visitor_<grammar>_<rule><V: JsonVisitor>(
/// input, p, first_byte, state, visitor) -> Result<(), ParseErr>`.
///
/// AX.W0a.2.g — mirrors tape-path signature extension: `state` parameter
/// threaded through so Ref-led Alt branches can delegate to their
/// target's visitor-path shape fn via [`emit_ref_call_visitor`]. For
/// single-literal and pure-Literal Alt forms, `state` is unused — the
/// parameter is present only so the signature stays uniform across
/// sub-cases.
pub fn emit_parse_keyword_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("keyword", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let body = unwrap_trivia(&rule.body);
    match body {
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            let len = bytes.len();
            let byte_lits: Vec<TokenStream> = bytes
                .iter()
                .map(|b| {
                    let lit = *b;
                    quote! { #lit }
                })
                .collect();
            // For `null = "null"` the semantic is `visitor.null()`.
            // Single-literal keyword emit: if the literal is `null`
            // call `visitor.null()`; if it's `true` / `false` the
            // bool visitor method; otherwise default to `null()`
            // (best-effort — rules without a `-> const` lose the
            // discriminator).
            let literal_str = std::str::from_utf8(bytes).unwrap_or("");
            let emit = match literal_str {
                "true" => quote! {
                    visitor.bool(true).map_err(|_| ::bbnf::runtime::ParseErr::Syntax {
                        offset: at as u32, rule: None,
                    })
                },
                "false" => quote! {
                    visitor.bool(false).map_err(|_| ::bbnf::runtime::ParseErr::Syntax {
                        offset: at as u32, rule: None,
                    })
                },
                _ => quote! {
                    visitor.null().map_err(|_| ::bbnf::runtime::ParseErr::Syntax {
                        offset: at as u32, rule: None,
                    })
                },
            };
            quote! {
                /// AW-V.W3-bench-fix — visitor-path Keyword-shape parse
                /// function (single-literal body).
                ///
                /// AX.W0a.2.g — `state` parameter unused for single-
                /// literal form.
                #[inline(always)]
                #[allow(non_snake_case, clippy::too_many_arguments)]
                pub fn #fn_ident<V>(
                    input: &[u8],
                    p: &mut usize,
                    _first_byte: u8,
                    _state: &mut #support_mod::ScanState,
                    visitor: &mut V,
                ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
                where
                    V: ::bbnf::runtime::tape::KeywordVisitor
                       + ::bbnf::runtime::tape::ObjectVisitor
                       + ::bbnf::runtime::tape::ArrayVisitor
                       + ::bbnf::runtime::tape::StringVisitor
                       + ::bbnf::runtime::tape::NumberVisitor,
                {
                    let at = *p;
                    let end = at + #len;
                    if input.len() < end || input[at..end] != [#(#byte_lits),*] {
                        return Err(::bbnf::runtime::ParseErr::Syntax {
                            offset: at as u32, rule: None,
                        });
                    }
                    *p = end;
                    #emit
                }
            }
        }
        IrNode::Alt(branches, _) => {
            use std::collections::BTreeMap;

            // Collect `(leading_literal_bytes, branch_ref_or_literal)`
            // for each branch; mirrors the tape-path collection logic.
            let per_branch: Vec<(Vec<u8>, Option<bbnf_ir::RuleId>, usize, &bbnf_ir::AltBranch)> =
                branches
                    .iter()
                    .enumerate()
                    .filter_map(|(branch_idx, branch)| {
                        let body = unwrap_trivia(&branch.node);
                        match body {
                            IrNode::Literal(sid) => {
                                let bytes = ir.get_string(*sid).as_bytes().to_vec();
                                if bytes.is_empty() {
                                    return None;
                                }
                                Some((bytes, None, branch_idx, branch))
                            }
                            IrNode::Ref(rid) => {
                                let bytes = leading_literal_bytes(body, ir)?;
                                if bytes.is_empty() {
                                    return None;
                                }
                                Some((bytes, Some(*rid), branch_idx, branch))
                            }
                            _ => None,
                        }
                    })
                    .collect();

            let mut by_first: BTreeMap<u8, Vec<&(Vec<u8>, Option<bbnf_ir::RuleId>, usize, &bbnf_ir::AltBranch)>> =
                BTreeMap::new();
            for entry in &per_branch {
                by_first.entry(entry.0[0]).or_default().push(entry);
            }

            let arms: Vec<TokenStream> = by_first
                .iter()
                .map(|(first, group)| {
                    let tries: Vec<TokenStream> = group
                        .iter()
                        .map(|(bytes, target_ref, _branch_idx, _branch)| {
                            let len = bytes.len();
                            let byte_lits: Vec<TokenStream> =
                                bytes.iter().map(|b| {
                                    let lit = *b;
                                    quote! { #lit }
                                }).collect();
                            if let Some(target_rid) = target_ref {
                                let ref_call =
                                    emit_ref_call_visitor(grammar_suffix, *target_rid, ir)
                                        .unwrap_or_else(|| quote! {
                                            ::core::result::Result::Err(
                                                ::bbnf::runtime::ParseErr::Syntax {
                                                    offset: *p as u32, rule: None,
                                                },
                                            )
                                        });
                                quote! {
                                    if input.len() >= *p + #len
                                        && input[*p..*p + #len] == [#(#byte_lits),*]
                                    {
                                        return (#ref_call);
                                    }
                                }
                            } else {
                                let literal_str =
                                    std::str::from_utf8(bytes).unwrap_or("");
                                let emit = match literal_str {
                                    "true" => quote! {
                                        visitor.bool(true).map_err(
                                            |_| ::bbnf::runtime::ParseErr::Syntax {
                                                offset: at as u32, rule: None,
                                            })
                                    },
                                    "false" => quote! {
                                        visitor.bool(false).map_err(
                                            |_| ::bbnf::runtime::ParseErr::Syntax {
                                                offset: at as u32, rule: None,
                                            })
                                    },
                                    _ => quote! {
                                        visitor.null().map_err(
                                            |_| ::bbnf::runtime::ParseErr::Syntax {
                                                offset: at as u32, rule: None,
                                            })
                                    },
                                };
                                quote! {
                                    if input.len() >= *p + #len
                                        && input[*p..*p + #len] == [#(#byte_lits),*]
                                    {
                                        let at = *p;
                                        let end = at + #len;
                                        *p = end;
                                        return #emit;
                                    }
                                }
                            }
                        })
                        .collect();
                    quote! {
                        #first => {
                            #(#tries)*
                            return Err(::bbnf::runtime::ParseErr::Syntax {
                                offset: *p as u32, rule: None,
                            });
                        }
                    }
                })
                .collect();
            quote! {
                /// AW-V.W3-bench-fix — visitor-path Keyword-shape parse
                /// function (Alt of literal-led or Ref-led branches).
                ///
                /// AX.W0a.2.g — admits Ref-led branches; threads `state`
                /// for downstream visitor-path Ref calls.
                #[inline(always)]
                #[allow(non_snake_case, clippy::too_many_arguments)]
                pub fn #fn_ident<V>(
                    input: &[u8],
                    p: &mut usize,
                    first_byte: u8,
                    state: &mut #support_mod::ScanState,
                    visitor: &mut V,
                ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
                where
                    V: ::bbnf::runtime::tape::KeywordVisitor
                       + ::bbnf::runtime::tape::ObjectVisitor
                       + ::bbnf::runtime::tape::ArrayVisitor
                       + ::bbnf::runtime::tape::StringVisitor
                       + ::bbnf::runtime::tape::NumberVisitor,
                {
                    let _ = state;
                    match first_byte {
                        #(#arms)*
                        _ => Err(::bbnf::runtime::ParseErr::Syntax {
                            offset: *p as u32, rule: None,
                        }),
                    }
                }
            }
        }
        _ => quote! {},
    }
}
