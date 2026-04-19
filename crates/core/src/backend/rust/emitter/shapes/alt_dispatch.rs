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
    let dispatch_arms = emit_dispatch_arms(branches, grammar_suffix, ir);

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
            let alt_child = builder.mark_children();
            #dispatch_arms
            let alt_hi = *p as u32;
            let off = builder.push_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                alt_child,
                alt_lo,
                alt_hi,
                #variant_idx,
                0,
            );
            Ok(off)
        }
    }
}

/// Emit the dispatch body for an AltDispatch rule — collects per-
/// branch first-byte sets and emits a match over the first byte,
/// with fallback linear scan for overlapping sets.
fn emit_dispatch_arms(
    branches: &[AltBranch],
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    // Precompute per-branch (first_bytes, body) pairs.
    let mut enumerated: Vec<(Vec<u8>, TokenStream)> = Vec::new();
    for branch in branches {
        let first_bytes = branch_first_bytes(&branch.node, ir);
        let body = emit_branch_body(&branch.node, grammar_suffix, ir);
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
        let save_child = builder.mark_children();
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

/// Emit the body of a single branch attempt. The body either
/// `break 'try_branches`es on success, or falls through so the next
/// candidate can try.
fn emit_branch_body(
    node: &IrNode,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let inner = unwrap_trivia(node);
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
        IrNode::Regex(_) => emit_regex_attempt(),
        IrNode::Seq(_) | IrNode::Next(_, _) | IrNode::Skip(_, _) => {
            emit_seq_attempt(inner, ir)
        }
        _ => quote! {},
    }
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

/// Regex-branch attempt — canonical non-whitespace scan. Matches
/// CSS's `/[^\s;!}]+/` catch-all and similar fallback patterns.
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
