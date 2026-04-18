//! Wrap-shape emitter — `parse_wrap_<grammar>_<rule>`.
//!
//! # Role — AW-V.W4-fix
//!
//! Emits per-grammar Wrap-shape parse functions for transparent
//! `Alt(Ref, Ref, …)` dispatchers. The Wrap-shape rule emits NO
//! compound of its own — it's a pass-through that dispatches to the
//! chosen branch rule's shape fn.
//!
//! Canonical:
//! - JSON `value = object | array | string | number | bool | null` —
//!   byte-dispatch onto the 6 branch shape fns.
//! - CSS `color = colorMix | colorFn | hex | colorFunction |
//!   namedColor` — each branch is a Ref.
//! - Sheets `range_end = cell_ref | /\$?[A-Za-z]{1,3}/ | /\$?\d+/` —
//!   mixed Ref + Regex branches.
//! - BBNF `rhs = closure | alternation`.
//!
//! # Emission shape
//!
//! The emitted function performs a byte-dispatch on the first
//! non-whitespace byte and directly delegates to the chosen branch's
//! shape fn. No outer compound is pushed — the branch's own compound
//! carries the final record (walker parity: the DTA's ByteDispatch
//! state emits no compound either).
//!
//! For branches where a byte-prefix-dispatch is possible (the Ref's
//! target rule's FIRST byte set is disjoint from siblings), the arm
//! is a direct Literal-byte match to the chosen shape fn. For Regex
//! branches or overlapping-FIRST branches, the arm falls through to
//! the grammar's value-dispatcher (which handles the per-grammar
//! Alt-dispatch table).
//!
//! # Wire contract
//!
//! Walker-tape parity: the chosen branch's shape fn emits the tape
//! record carrying the rule's semantic payload. Wrap itself emits
//! nothing. The dispatcher-fallback path hooks the walker for any
//! branch whose shape fn isn't in the emitter's reach.

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::{
    dispatcher_fn_ident, shape_fn_ident, visitor_dispatcher_fn_ident, visitor_shape_fn_ident,
};
use super::root_rule_name;

/// Emit `pub fn parse_wrap_<grammar>_<rule>(input, p, state, builder)
/// -> Result<TapeOffset, DtaError>`.
pub fn emit_parse_wrap(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("wrap", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    let body = unwrap_outer(&rule.body);
    let dispatch = match body {
        IrNode::Alt(branches, _) => emit_alt_tape_dispatch(
            branches,
            grammar_suffix,
            &dispatcher_ident,
            ir,
        ),
        _ => {
            // Non-Alt Wrap body (transparent alias) — just call the
            // dispatcher and return.
            quote! {
                #dispatcher_ident(input, p, state, builder)
            }
        }
    };

    quote! {
        /// AW-V.W4-fix — per-grammar Wrap-shape parse function.
        ///
        /// Transparent dispatcher — skip leading ws, byte-dispatch
        /// to the chosen branch's shape fn, return that shape fn's
        /// offset unchanged. No outer compound emission; the
        /// branch's own shape fn owns the tape record.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut ::bbnf::runtime::tape::TapeBuilder,
        ) -> ::core::result::Result<
            ::bbnf::runtime::tape::TapeOffset,
            ::bbnf::runtime::tape::DtaError,
        > {
            #dispatch
        }
    }
}

/// Peel Map / OptionalWhitespace wrappers to reach the structural Alt
/// / Ref body.
fn unwrap_outer(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            unwrap_outer(inner)
        }
        _ => node,
    }
}

/// Emit the Alt-dispatch body for the Wrap tape-path emitter.
///
/// Each branch is a Ref or Regex. For Ref branches we look up the
/// target rule's shape tag and emit a direct call to the matching
/// shape fn. Non-admitting branches fall through the grammar's
/// value-dispatcher.
fn emit_alt_tape_dispatch(
    branches: &[bbnf_ir::AltBranch],
    grammar_suffix: &str,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> TokenStream {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;

    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    // Collect (first-byte, shape-fn-ident) pairs. When a Ref target
    // has a predictable first byte (its `meta.first_set` is a single-
    // byte set), we can emit a byte-dispatch arm. Otherwise we call
    // through the dispatcher.
    let mut ref_arms: Vec<TokenStream> = Vec::new();
    for branch in branches {
        let inner = unwrap_outer(&branch.node);
        let IrNode::Ref(rid) = inner else {
            // Regex branches — no single-byte dispatch; route via the
            // general dispatcher.
            continue;
        };
        let Some(target) = ir.rules.iter().find(|r| r.id == *rid) else {
            continue;
        };
        let tag = ir.shape_assignments.get(*rid);
        let shape_name = shape_tag_name(tag);
        if shape_name.is_none() {
            continue;
        }
        let shape_name = shape_name.unwrap();
        let target_fn = shape_fn_ident(shape_name, grammar_suffix, ir.get_string(target.name));

        // Per-branch first-byte dispatch — extract from the rule's
        // `meta.first_set` when it's a single-byte set.
        let first_bytes: Vec<u8> = target
            .meta
            .first_set
            .iter()
            .collect();
        if first_bytes.is_empty() || first_bytes.len() > 16 {
            continue;
        }
        let byte_pats: Vec<TokenStream> =
            first_bytes.iter().map(|b| quote! { #b }).collect();
        // Shape-fn signature differs per shape — Number / Keyword take
        // `(input, p, first, builder)`; others take `(input, p, state,
        // builder)`.
        let call = match tag {
            ShapeTag::Number | ShapeTag::Keyword => quote! {
                #target_fn(input, p, first, builder)
            },
            _ => quote! {
                #target_fn(input, p, state, builder)
            },
        };
        ref_arms.push(quote! {
            #(#byte_pats)|* => #call,
        });
    }

    if ref_arms.is_empty() {
        // No byte-dispatchable branches — route entirely through the
        // grammar's value-dispatcher.
        return quote! {
            let _ = #support_mod::skip_space(input, p, state);
            #dispatcher_ident(input, p, state, builder)
        };
    }

    let fallback = quote! {
        _ => #dispatcher_ident(input, p, state, builder),
    };

    quote! {
        let first = #support_mod::skip_space(input, p, state)
            .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        match first {
            #(#ref_arms)*
            #fallback
        }
    }
}

/// Convert a [`ShapeTag`] into the shape-fn prefix. Returns `None`
/// when the tag is `None` (unclassified).
fn shape_tag_name(
    tag: bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag,
) -> Option<&'static str> {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
    match tag {
        ShapeTag::Object => Some("object"),
        ShapeTag::Array => Some("array"),
        ShapeTag::String => Some("string"),
        ShapeTag::Number => Some("number"),
        ShapeTag::Keyword => Some("keyword"),
        ShapeTag::Scalar => Some("scalar"),
        ShapeTag::Pratt => Some("pratt"),
        ShapeTag::Unordered => Some("unordered"),
        ShapeTag::ArgList => Some("arglist"),
        ShapeTag::Flat => Some("flat"),
        ShapeTag::Wrap => Some("wrap"),
        ShapeTag::HRegex => Some("hregex"),
        ShapeTag::None => None,
    }
}

// ─────────────────────────────────────────────────────────────────────
// AW-V.W4-fix — visitor-path Wrap emitter.
// ─────────────────────────────────────────────────────────────────────

/// Emit `pub fn parse_wrap_visitor_<grammar>_<rule><V>(input, p,
/// state, visitor) -> Result<(), ParseErr>`.
pub fn emit_parse_wrap_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("wrap", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = visitor_dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    let body = unwrap_outer(&rule.body);
    let dispatch = match body {
        IrNode::Alt(branches, _) => emit_alt_visitor_dispatch(
            branches,
            grammar_suffix,
            &dispatcher_ident,
            ir,
        ),
        _ => quote! {
            #dispatcher_ident(input, p, state, visitor)
        },
    };

    quote! {
        /// AW-V.W4-fix — visitor-path Wrap-shape parse function.
        ///
        /// Transparent dispatcher — skip leading ws, byte-dispatch to
        /// the chosen branch's visitor-path shape fn. No visitor event
        /// fires here; the chosen branch's visitor fn owns the event
        /// emission.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
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
            #dispatch
        }
    }
}

/// Emit the visitor-path Alt-dispatch body.
fn emit_alt_visitor_dispatch(
    branches: &[bbnf_ir::AltBranch],
    grammar_suffix: &str,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> TokenStream {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;

    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let mut ref_arms: Vec<TokenStream> = Vec::new();
    for branch in branches {
        let inner = unwrap_outer(&branch.node);
        let IrNode::Ref(rid) = inner else { continue };
        let Some(target) = ir.rules.iter().find(|r| r.id == *rid) else {
            continue;
        };
        let tag = ir.shape_assignments.get(*rid);
        let shape_name = shape_tag_name(tag);
        let Some(shape_name) = shape_name else { continue };
        let target_fn =
            visitor_shape_fn_ident(shape_name, grammar_suffix, ir.get_string(target.name));
        let first_bytes: Vec<u8> =
            target.meta.first_set.iter().collect();
        if first_bytes.is_empty() || first_bytes.len() > 16 {
            continue;
        }
        let byte_pats: Vec<TokenStream> =
            first_bytes.iter().map(|b| quote! { #b }).collect();
        let call = match tag {
            ShapeTag::Number | ShapeTag::Keyword => quote! {
                #target_fn(input, p, first, visitor)
            },
            ShapeTag::String => quote! {
                #target_fn(input, p, state, visitor, /*is_key=*/ false)
            },
            _ => quote! {
                #target_fn(input, p, state, visitor)
            },
        };
        ref_arms.push(quote! {
            #(#byte_pats)|* => #call,
        });
    }

    if ref_arms.is_empty() {
        return quote! {
            let _ = #support_mod::skip_space(input, p, state);
            #dispatcher_ident(input, p, state, visitor)
        };
    }

    let fallback = quote! {
        _ => #dispatcher_ident(input, p, state, visitor),
    };

    quote! {
        let first = #support_mod::skip_space(input, p, state)
            .ok_or(::bbnf::runtime::ParseErr::Syntax {
                offset: *p as u32, rule: None,
            })?;
        match first {
            #(#ref_arms)*
            #fallback
        }
    }
}
