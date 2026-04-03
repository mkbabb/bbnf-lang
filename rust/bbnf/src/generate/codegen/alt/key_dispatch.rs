//! Grammar-agnostic key-dispatch optimization for Alt patterns.
//!
//! Pattern: Alt where branches are `Seq(key, separator, ...)` with a shared
//! separator literal, and an optional regex catch-all fallback.
//!
//! Instead of trying all N branches sequentially, scan the key token once,
//! then dispatch on the consumed bytes via inline comparisons.
//!
//! Generalizes the former CSS-specific `ident_dispatch` to work with any
//! key type (identifiers, quoted strings, numbers) and any separator
//! (`:`, `=`, `->`, etc.), detected automatically from the grammar structure.

use bbnf_ir::{AltBranch, IrNode, TypeDesc};

use proc_macro2::TokenStream;
use quote::quote;

use super::super::ir_types::{self, IrCodegenCtx};
use super::{coerce_mono_branch, coerce_mono_branch_by_value};
use crate::generate::codegen::unescape_literal;
use crate::generate::codegen::{emit_mono_expr, MonoCtx};
use crate::generate::regex::classify::{classify_regex, RegexClass};
use crate::generate::regex::emit::scanner_plan;

// ── Helpers ────────────────────────────────────────────────────

/// Extract literal strings from a branch's leading node.
/// Returns None if the leading node isn't a pure literal or literal-Alt.
pub(in super::super) fn extract_leading_literals(
    node: &IrNode,
    ctx: &IrCodegenCtx<'_>,
) -> Option<Vec<String>> {
    match node {
        IrNode::Seq(children) if !children.is_empty() => {
            extract_leading_literals(&children[0], ctx)
        }
        IrNode::Ref(rule_id) => {
            let rule = ctx.ir.rules.iter().find(|r| r.id == *rule_id)?;
            extract_leading_literals(&rule.body, ctx)
        }
        IrNode::Literal(sid) => Some(vec![unescape_literal(ctx.ir.get_string(*sid))]),
        IrNode::Alt(branches, _) => {
            let mut lits = Vec::new();
            for b in branches {
                lits.extend(extract_leading_literals(&b.node, ctx)?);
            }
            Some(lits)
        }
        IrNode::Map { inner, .. } => extract_leading_literals(inner, ctx),
        IrNode::OptionalWhitespace(inner) => extract_leading_literals(inner, ctx),
        _ => None,
    }
}

/// Check if a node's leading position is a regex (catch-all pattern).
pub(in super::super) fn is_leading_regex(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> bool {
    match node {
        IrNode::Seq(children) if !children.is_empty() => is_leading_regex(&children[0], ctx),
        IrNode::Ref(rule_id) => {
            if let Some(rule) = ctx.ir.rules.iter().find(|r| r.id == *rule_id) {
                is_leading_regex(&rule.body, ctx)
            } else {
                false
            }
        }
        IrNode::Regex(_) => true,
        IrNode::Alt(branches, _) => branches.iter().any(|b| is_leading_regex(&b.node, ctx)),
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            is_leading_regex(inner, ctx)
        }
        _ => false,
    }
}

/// Extract the regex pattern string from a node's leading position.
fn extract_leading_regex_pattern<'a>(
    node: &'a IrNode,
    ctx: &'a IrCodegenCtx<'_>,
) -> Option<&'a str> {
    match node {
        IrNode::Seq(children) if !children.is_empty() => {
            extract_leading_regex_pattern(&children[0], ctx)
        }
        IrNode::Ref(rule_id) => {
            let rule = ctx.ir.rules.iter().find(|r| r.id == *rule_id)?;
            extract_leading_regex_pattern(&rule.body, ctx)
        }
        IrNode::Regex(sid) => Some(ctx.ir.get_string(*sid)),
        IrNode::Alt(branches, _) => branches
            .iter()
            .find_map(|b| extract_leading_regex_pattern(&b.node, ctx)),
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            extract_leading_regex_pattern(inner, ctx)
        }
        _ => None,
    }
}

// ── Key type classification ────────────────────────────────────

/// What kind of key scanner to use.
enum KeyClass {
    /// CSS/general identifiers: `[a-zA-Z_][\w-]*`
    Identifier,
    /// Quoted strings: `"..."` or `'...'`
    QuotedString { _quote_char: u8 },
}

/// Derive the key class from the fallback branch's leading regex pattern.
fn classify_fallback_key(fallback: &IrNode, ctx: &IrCodegenCtx<'_>) -> Option<KeyClass> {
    let pattern = extract_leading_regex_pattern(fallback, ctx)?;
    match classify_regex(pattern) {
        RegexClass::Identifier => Some(KeyClass::Identifier),
        RegexClass::QuotedString {
            quote_char,
            allows_escapes: _,
        } => Some(KeyClass::QuotedString {
            _quote_char: quote_char,
        }),
        // Numeric and unknown patterns: don't dispatch (not enough structure).
        _ => None,
    }
}

/// Validate that a key literal is consistent with the detected key class.
fn validate_key_for_class(key: &str, class: &KeyClass) -> bool {
    match class {
        KeyClass::Identifier => key
            .as_bytes()
            .first()
            .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_' || *b == b'-'),
        KeyClass::QuotedString { .. } => {
            // String keys are stored without quotes in the grammar.
            // All byte values are valid key content.
            !key.is_empty()
        }
    }
}

// ── Separator detection ────────────────────────────────────────

/// Detect a common separator shared by all key literals.
///
/// Checks two patterns:
/// 1. Fused suffix: all literals share a non-alphanumeric trailing byte
///    (e.g., `"color:"` → separator `":"`, key `"color"`)
/// 2. Seq element: all branches have the same Literal as their 2nd Seq child
///    (e.g., `Seq(Ref(props), Literal(":"), ...)`)
fn detect_separator<'a>(
    branch_literals: &[Vec<String>],
    branches: &[AltBranch],
    fallback_idx: Option<usize>,
    ctx: &IrCodegenCtx<'a>,
) -> Option<String> {
    // Strategy 1: check if all literals share a non-alphanumeric suffix.
    let all_lits: Vec<&str> = branch_literals
        .iter()
        .flat_map(|lits| lits.iter().map(|s| s.as_str()))
        .collect();
    if !all_lits.is_empty() {
        if let Some(&first) = all_lits.first() {
            if let Some(last_byte) = first.as_bytes().last() {
                if !last_byte.is_ascii_alphanumeric() && *last_byte != b'_' && *last_byte != b'-' {
                    let suffix = &first[first.len() - 1..];
                    if all_lits.iter().all(|lit| lit.ends_with(suffix)) {
                        return Some(suffix.to_string());
                    }
                }
            }
        }
    }

    // Strategy 2: check for a common 2nd Seq element across non-fallback branches.
    let check_count = if let Some(fb) = fallback_idx {
        fb
    } else {
        branches.len()
    };
    if check_count >= 2 {
        let sep = extract_seq_separator(&branches[0].node, ctx);
        if let Some(ref s) = sep {
            let all_match = branches[1..check_count]
                .iter()
                .all(|b| extract_seq_separator(&b.node, ctx).as_deref() == Some(s.as_str()));
            if all_match {
                return sep;
            }
        }
    }

    None
}

/// Extract the separator literal from a branch's Seq(key, separator, ...).
fn extract_seq_separator(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> Option<String> {
    match node {
        IrNode::Seq(children) if children.len() >= 2 => match &children[1] {
            IrNode::Literal(sid) => Some(unescape_literal(ctx.ir.get_string(*sid))),
            IrNode::OptionalWhitespace(inner) => match inner.as_ref() {
                IrNode::Literal(sid) => Some(unescape_literal(ctx.ir.get_string(*sid))),
                _ => None,
            },
            _ => None,
        },
        IrNode::Ref(rule_id) => {
            let rule = ctx.ir.rules.iter().find(|r| r.id == *rule_id)?;
            extract_seq_separator(&rule.body, ctx)
        }
        IrNode::OptionalWhitespace(inner) => extract_seq_separator(inner, ctx),
        _ => None,
    }
}

// ── Code emission ──────────────────────────────────────────────

/// Emit the scanner call for the detected key class.
fn emit_key_scanner(class: &KeyClass) -> TokenStream {
    match class {
        KeyClass::Identifier => scanner_plan::shared_ident_scanner().into_tokens(),
        KeyClass::QuotedString { .. } => scanner_plan::shared_quoted_string_scanner().into_tokens(),
    }
}

/// Strip the separator suffix from a key literal for byte comparison.
fn strip_key_suffix<'a>(lit: &'a str, separator: &Option<String>) -> &'a str {
    if let Some(sep) = separator {
        lit.strip_suffix(sep.as_str()).unwrap_or(lit)
    } else {
        lit
    }
}

/// Try to emit key-dispatch for a key-separator Alt pattern.
///
/// Detects: Alt where branches start with literal keys (optionally followed
/// by a common separator), with an optional regex catch-all fallback.
/// Emits: scan key once → byte-match → restore offset → call full branch.
pub(in super::super) fn try_emit_key_dispatch(
    branches: &[AltBranch],
    branch_tys: &[TypeDesc],
    needs_coercion: bool,
    local_sub_variants: &[Option<String>],
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> Option<TokenStream> {
    // Detect optional fallback (regex catch-all as last branch).
    let fallback_idx = if is_leading_regex(&branches[branches.len() - 1].node, ctx) {
        Some(branches.len() - 1)
    } else {
        None
    };

    let typed_end = fallback_idx.unwrap_or(branches.len());

    // Need enough branches to justify dispatch.
    if fallback_idx.is_some() && branches.len() < 3 {
        return None;
    }
    if fallback_idx.is_none() && branches.len() < 3 {
        return None;
    }

    // Determine key class from fallback's regex, or infer from literals.
    let key_class = if let Some(fb_idx) = fallback_idx {
        classify_fallback_key(&branches[fb_idx].node, ctx)?
    } else {
        // No fallback — try to infer from the literal branches.
        // Default to Identifier if all keys are ident-like.
        KeyClass::Identifier
    };

    // Extract leading literals from all typed branches.
    let mut branch_literals: Vec<Vec<String>> = Vec::new();
    for branch in &branches[..typed_end] {
        let lits = extract_leading_literals(&branch.node, ctx)?;
        if lits.is_empty() {
            return None;
        }
        // Validate all literals against the detected key class.
        for lit in &lits {
            let sep = detect_separator(&branch_literals, branches, fallback_idx, ctx);
            let name = strip_key_suffix(lit, &sep);
            if !validate_key_for_class(name, &key_class) {
                return None;
            }
        }
        branch_literals.push(lits);
    }

    // Detect common separator.
    let separator = detect_separator(&branch_literals, branches, fallback_idx, ctx);

    // Re-validate keys with the detected separator.
    for lits in &branch_literals {
        for lit in lits {
            let name = strip_key_suffix(lit, &separator);
            if !validate_key_for_class(name, &key_class) {
                return None;
            }
        }
    }

    // ── Emit code ──────────────────────────────────────────────

    let scanner = emit_key_scanner(&key_class);
    let cp_var = mctx.fresh("kd_cp");
    let mut if_arms: Vec<TokenStream> = Vec::new();

    for (idx, lits) in branch_literals.iter().enumerate() {
        let branch_expr = emit_mono_expr(&branches[idx].node, ctx, mctx, elide_box);
        let coerced = if needs_coercion {
            let sv_name = local_sub_variants.get(idx).and_then(|s| s.as_deref());
            if elide_box {
                coerce_mono_branch_by_value(branch_expr, &branch_tys[idx], sv_name, ctx)
            } else {
                coerce_mono_branch(branch_expr, &branch_tys[idx], sv_name, ctx)
            }
        } else {
            branch_expr
        };

        // Generate byte comparisons for each key in this branch's set.
        let comparisons: Vec<TokenStream> = lits
            .iter()
            .map(|lit| {
                let name = strip_key_suffix(lit, &separator);
                let key_bytes = match &key_class {
                    KeyClass::QuotedString { .. } => {
                        // For string keys, compare the content between quotes.
                        // __kd_bytes already has quotes stripped by the scanner.
                        name.as_bytes().to_vec()
                    }
                    KeyClass::Identifier => name.as_bytes().to_vec(),
                };
                let byte_lits: Vec<proc_macro2::Literal> = key_bytes
                    .iter()
                    .map(|b| proc_macro2::Literal::byte_character(*b))
                    .collect();
                let len = key_bytes.len();
                quote! { (__kd_len == #len && __kd_bytes == &[#(#byte_lits),*]) }
            })
            .collect();

        // Emit: if key matches, restore offset and run the full branch.
        if_arms.push(quote! {
            if #(#comparisons)||* {
                state.offset = #cp_var;
                return #coerced;
            }
        });
    }

    // Fallback branch (or None if no fallback).
    let fallback_code = if let Some(fb_idx) = fallback_idx {
        let fb_expr = emit_mono_expr(&branches[fb_idx].node, ctx, mctx, elide_box);
        let fb_coerced = if needs_coercion {
            let sv_name = local_sub_variants.get(fb_idx).and_then(|s| s.as_deref());
            if elide_box {
                coerce_mono_branch_by_value(fb_expr, &branch_tys[fb_idx], sv_name, ctx)
            } else {
                coerce_mono_branch(fb_expr, &branch_tys[fb_idx], sv_name, ctx)
            }
        } else {
            fb_expr
        };
        quote! {
            state.offset = #cp_var;
            #fb_coerced
        }
    } else {
        // No fallback: key didn't match any branch → Alt fails.
        quote! { None }
    };

    let return_type = if needs_coercion {
        if elide_box {
            ctx.enum_type.clone()
        } else {
            ctx.boxed_enum_type.clone()
        }
    } else if branch_tys[0] == TypeDesc::BoxedEnum {
        ctx.boxed_enum_type.clone()
    } else {
        ir_types::type_desc_to_syn(&branch_tys[0], ctx)
    };

    // Emit scanner-specific byte extraction.
    let byte_extraction = match &key_class {
        KeyClass::Identifier => quote! {
            let __kd_bytes = &state.src_bytes[__kd_s.start..__kd_s.end];
            let __kd_len = __kd_bytes.len();
        },
        KeyClass::QuotedString { .. } => quote! {
            // Strip quote delimiters for comparison.
            let __kd_bytes = &state.src_bytes[__kd_s.start + 1..__kd_s.end - 1];
            let __kd_len = __kd_bytes.len();
        },
    };

    Some(quote! {
        (|| -> Option<#return_type> {
            let #cp_var = state.offset;
            if let Some(ref __kd_s) = #scanner {
                #byte_extraction
                #(#if_arms)*
            }
            // No match or scanner failed — try fallback from original offset.
            #fallback_code
        })()
    })
}
