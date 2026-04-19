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

    // AX.W0a.2.p — rule-type-driven leaf-kind selection.
    //
    // U8 scalar rules (`-> Nu8`) emit `TapeKind::KvPair` on typed
    // payloads so the walker-parity `Tuple([Span, U8])` reader
    // (CSS `dir_pseudo_*`, Sheets `compare_op`) sees the same kind
    // the walker would have emitted post-Seq-promotion. `Bool`
    // scalar rules (JSON `bool`) keep `TapeKind::Span` because
    // json_parity's `every_declared_leaf_reaches_the_tape` asserts
    // typed-bool leaves are Span-kinded. Payload-less branches keep
    // Span in both cases.
    let leaf_kind = rule_keyword_leaf_kind(rule, ir);

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
            // AX.W0a.2.p — payload lands via push_leaf_with_arena_payload
            // so the record carries TapeRec::PAYLOAD_IN_ARENA_BIT.
            // payload_u8 / payload_inline scalar readers slice the
            // arena at `arena_off`; payload_bytes(rec, 1) continues
            // to work unchanged. Pre-W0a.2.p the push_leaf_with
            // Aggregate path stored the byte in pay_agg but left the
            // bit clear, so the scalar readers mistook child_off for
            // a pay_narrow rank and returned junk.
            let (arena_emit, payload_width) = match payload.as_ref() {
                Some(_) => (
                    quote! {
                        let __arena_off: u32 = builder.arena_mut().len() as u32;
                        builder.arena_mut().push((#payload) as u8);
                    },
                    1u32,
                ),
                None => (quote! {}, 0u32),
            };
            let byte_lits: Vec<TokenStream> = bytes
                .iter()
                .map(|b| {
                    let lit = *b;
                    quote! { #lit }
                })
                .collect();
            let push_stmt = if payload_width == 0 {
                quote! {
                    let off = builder.push_leaf_with(
                        ::bbnf::runtime::tape::TapeKind::Span,
                        at as u32,
                        end as u32,
                        #variant_idx,
                        0,
                        ::bbnf::runtime::tape::PayloadData::None,
                    );
                }
            } else {
                // AX.W0a.2.p — single-literal Keyword with typed payload
                // lands on a Span leaf. The PAYLOAD_IN_ARENA_BIT fires
                // so payload_u8 / payload_inline scalar readers slice
                // the arena at `__arena_off`; payload_bytes(rec, 1)
                // continues to work unchanged. KvPair emission is
                // reserved for the Alt-of-literal-branches arm below
                // (walker-parity for typed-enum rules like CSS
                // `dirKeyword`, Sheets `add_op`); JSON's single-literal
                // `null = "null" -> 0u8` keeps Span per json_parity's
                // declared-leaf-reaches-the-tape contract.
                quote! {
                    let off = builder.push_leaf_with_arena_payload(
                        ::bbnf::runtime::tape::TapeKind::Span,
                        at as u32,
                        end as u32,
                        #variant_idx,
                        0,
                        __arena_off,
                        #payload_width,
                    );
                }
            };
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
                    #arena_emit
                    #push_stmt
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

            // BranchKind selects emission strategy per branch:
            //   Literal  — byte-match + Literal leaf push.
            //   Ref(rid) — prefix-check + delegate to target shape fn.
            //   Seq      — structural per-position emission + one
            //              Span leaf covering the whole match. BBNF
            //              `literal = "\"" , Regex , "\"" | ...`
            //              canonical case — Keyword-classified, each
            //              branch is a Seq with a leading literal plus
            //              inner positions.
            enum BranchKind<'a> {
                Literal,
                Ref(bbnf_ir::RuleId),
                Seq(&'a IrNode),
            }
            let per_branch: Vec<(Vec<u8>, BranchKind<'_>, usize, &bbnf_ir::AltBranch)> =
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
                                Some((bytes, BranchKind::Literal, branch_idx, branch))
                            }
                            IrNode::Ref(rid) => {
                                let bytes = leading_literal_bytes(body, ir)?;
                                if bytes.is_empty() {
                                    return None;
                                }
                                Some((bytes, BranchKind::Ref(*rid), branch_idx, branch))
                            }
                            IrNode::Seq(_)
                            | IrNode::Next(_, _)
                            | IrNode::Skip(_, _) => {
                                let bytes = leading_literal_bytes(body, ir)?;
                                if bytes.is_empty() {
                                    return None;
                                }
                                Some((bytes, BranchKind::Seq(body), branch_idx, branch))
                            }
                            _ => None,
                        }
                    })
                    .collect();

            // Group per-branch entries by their first byte; each group
            // becomes one `#first => { ... }` match arm.
            let mut by_first: BTreeMap<u8, Vec<&(Vec<u8>, BranchKind<'_>, usize, &bbnf_ir::AltBranch)>> =
                BTreeMap::new();
            for entry in &per_branch {
                by_first.entry(entry.0[0]).or_default().push(entry);
            }

            let arms: Vec<TokenStream> = by_first
                .iter()
                .map(|(first, group)| {
                    // For each branch in this first-byte group, emit a
                    // full-prefix check + commit.
                    //
                    // AX.W0a.2.p — branches are tried in descending
                    // prefix-length order so longer literal prefixes
                    // (`:is`, `:where`, `:dir`, ...) test before 1-byte
                    // fallback candidates (`":", nthFunctionName` /
                    // `":", selectorIdent`) that would false-positive on
                    // every `:` input. Equal-length prefixes retain
                    // their original Alt order for determinism.
                    //
                    // Ref branches use match + rollback on Err so a
                    // successful prefix-match that trips the delegated
                    // call's inner parse can hand control back to the
                    // next candidate (CSS `pseudoClass` — nthPseudo's
                    // 1-byte `:` prefix matches `:hover`, its inner
                    // nthFunctionName parse fails, control must fall
                    // through to simplePseudoClass. Pre-W0a.2.p's
                    // early-return lost the Err outside the rule).
                    let mut group_sorted: Vec<&(Vec<u8>, BranchKind<'_>, usize, &bbnf_ir::AltBranch)> =
                        group.iter().copied().collect();
                    group_sorted.sort_by_key(|entry| {
                        // Descending prefix length; ties broken by
                        // ascending Alt position for determinism.
                        (std::cmp::Reverse(entry.0.len()), entry.2)
                    });
                    let tries: Vec<TokenStream> = group_sorted
                        .iter()
                        .map(|(bytes, kind, branch_idx, branch)| {
                            let len = bytes.len();
                            let byte_lits: Vec<TokenStream> =
                                bytes.iter().map(|b| {
                                    let lit = *b;
                                    quote! { #lit }
                                }).collect();
                            match kind {
                                BranchKind::Ref(target_rid) => {
                                    // Ref branch — prefix check, delegate,
                                    // rollback-and-fallthrough on Err so
                                    // subsequent candidates in the same
                                    // first-byte group get their chance.
                                    let ref_call = emit_ref_call_tape(
                                        grammar_suffix, *target_rid, ir,
                                    ).unwrap_or_else(|| quote! {
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
                                            let __ref_save_p = *p;
                                            let __ref_save_cols =
                                                builder.columns_mut().len();
                                            match (#ref_call) {
                                                ::core::result::Result::Ok(__off) => {
                                                    return ::core::result::Result::Ok(__off);
                                                }
                                                ::core::result::Result::Err(_) => {
                                                    *p = __ref_save_p;
                                                    builder.columns_mut()
                                                        .truncate(__ref_save_cols);
                                                }
                                            }
                                        }
                                    }
                                }
                                BranchKind::Literal => {
                                    // Literal branch — byte-match + leaf.
                                    // AX.W0a.2.p — typed Alt-of-literal
                                    // payloads land on `leaf_kind`
                                    // (KvPair for U8 rules, Span for
                                    // Bool rules). The
                                    // PAYLOAD_IN_ARENA_BIT fires and
                                    // scalar readers slice the arena.
                                    let _ = branch_idx;
                                    let branch_emit = match
                                        alt_branch_payload_value(branch, ir)
                                    {
                                        Some(value) => quote! {
                                            if input.len() >= *p + #len
                                                && input[*p..*p + #len] == [#(#byte_lits),*]
                                            {
                                                let at = *p;
                                                let end = at + #len;
                                                *p = end;
                                                let __arena_off: u32 =
                                                    builder.arena_mut().len() as u32;
                                                builder.arena_mut().push((#value) as u8);
                                                let off = builder
                                                    .push_leaf_with_arena_payload(
                                                        #leaf_kind,
                                                        at as u32,
                                                        end as u32,
                                                        #variant_idx,
                                                        0u8,
                                                        __arena_off,
                                                        1u32,
                                                    );
                                                return Ok(off);
                                            }
                                        },
                                        None => quote! {
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
                                                    ::bbnf::runtime::tape::PayloadData::None,
                                                );
                                                return Ok(off);
                                            }
                                        },
                                    };
                                    branch_emit
                                }
                                BranchKind::Seq(seq_body) => {
                                    // Seq branch — structural per-position
                                    // emission. On prefix match, run each
                                    // position (Literal → byte-match; Regex →
                                    // adapter scan; Ref → delegate; OW →
                                    // skip_space; etc.) inside an attempt
                                    // closure. On success, push ONE Span leaf
                                    // covering the whole matched range
                                    // (`literal` rule's contract — emit one
                                    // record per matched literal, not one
                                    // per position).
                                    let inner_emit = super::inline::
                                        emit_seq_branch_structural_tape(
                                            seq_body,
                                            &support_mod,
                                            grammar_suffix,
                                            ir,
                                        );
                                    quote! {
                                        if input.len() >= *p + #len
                                            && input[*p..*p + #len] == [#(#byte_lits),*]
                                        {
                                            let span_lo = *p as u32;
                                            let seq_save_cols =
                                                builder.columns_mut().len();
                                            let seq_attempt:
                                                ::core::result::Result<(), ()> =
                                                (|| {
                                                    #inner_emit
                                                    Ok(())
                                                })();
                                            if seq_attempt.is_err() {
                                                *p = span_lo as usize;
                                                builder.columns_mut()
                                                    .truncate(seq_save_cols);
                                                return Err(
                                                    ::bbnf::runtime::tape::DtaError::Syntax {
                                                        offset: span_lo,
                                                        failing_state:
                                                            ::bbnf::runtime::tape::DtaStateId::NONE,
                                                        failing_rule:
                                                            ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                                                    },
                                                );
                                            }
                                            let span_hi = *p as u32;
                                            // Replace the structural records
                                            // pushed by the attempt with a
                                            // single Span leaf — mirrors the
                                            // walker's `Keyword` state that
                                            // emits one leaf per matched
                                            // literal.
                                            builder.columns_mut()
                                                .truncate(seq_save_cols);
                                            let off = builder.push_leaf_with(
                                                ::bbnf::runtime::tape::TapeKind::Span,
                                                span_lo,
                                                span_hi,
                                                #variant_idx,
                                                0u8,
                                                ::bbnf::runtime::tape::PayloadData::None,
                                            );
                                            return Ok(off);
                                        }
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
/// AX.W0a.2.p — extract the branch's typed-byte payload (if any) from
/// its `Map { fn_id }` annotation.
///
/// AX.W0a.2.p — choose the leaf `TapeKind` for a Keyword rule's
/// payload-bearing emission.
///
/// Policy:
///
/// - `TypeDesc::U8` scalar rules emit `TapeKind::KvPair`. Matches the
///   walker's Seq-promotion output for `Tuple([Span, U8])`-shaped
///   rules (CSS `dir_pseudo_*`, Sheets `compare_op` / `add_op` /
///   `mul_op` in parent context).
/// - Every other admissible type (chiefly `Bool`) emits
///   `TapeKind::Span`. JSON's `bool`/`null` rules ride this branch;
///   `json_parity::every_declared_leaf_reaches_the_tape` asserts
///   typed-bool leaves are Span-kinded.
///
/// Non-typed rules (no `->` annotation) never hit the payload-bearing
/// branch here; their emission stays on a payload-less Span leaf.
fn rule_keyword_leaf_kind(rule: &IrRule, ir: &GrammarIR) -> TokenStream {
    use bbnf_ir::TypeDesc;
    let ty = ir.types.iter().find_map(|(rid, t)| {
        if *rid == rule.id {
            Some(t.clone())
        } else {
            None
        }
    });
    match ty {
        Some(TypeDesc::U8) => quote! { ::bbnf::runtime::tape::TapeKind::KvPair },
        _ => quote! { ::bbnf::runtime::tape::TapeKind::Span },
    }
}

/// Returns the `u32` literal value the `-> Nu8` / `-> bool` produces,
/// or `None` when the branch has no payload. Callers emit the arena
/// staging + `push_leaf_with_arena_payload` when `Some`, and a raw
/// `push_leaf_with(.., PayloadData::None)` when `None`.
fn alt_branch_payload_value(
    branch: &bbnf_ir::AltBranch,
    ir: &GrammarIR,
) -> Option<TokenStream> {
    // Walk the branch body for a Map { fn_id } annotation. Descends
    // through OptionalWhitespace / Map chains; Seq branches take the
    // outermost Map — post-factoring the annotation rides the branch
    // root even when the branch body is a Seq prefix-factored down
    // to one residual.
    fn find_map_fn(node: &IrNode) -> Option<u32> {
        match node {
            IrNode::Map { fn_id, .. } => Some(*fn_id),
            IrNode::OptionalWhitespace(inner) => find_map_fn(inner),
            _ => None,
        }
    }
    find_map_fn(&branch.node)
        .and_then(|fid| ir.fns.get(fid as usize))
        .and_then(|fd| payload_from_fn(fd, ir))
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
