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
        IrNode::Ref(rid) => {
            // Non-Alt Wrap body — single Ref transparent alias. Resolve
            // the target shape at emission time and call directly.
            // AX.W0a.2.e — never delegate to `#dispatcher_ident` which
            // for non-Alt-rooted grammars loops through the root.
            let _ = dispatcher_ident;
            match super::dispatcher::emit_ref_call_tape(grammar_suffix, *rid, ir) {
                Some(call) => quote! { #call },
                None => quote! {
                    ::core::result::Result::Err(
                        ::bbnf::runtime::tape::DtaError::Syntax {
                            offset: *p as u32,
                            failing_state:
                                ::bbnf::runtime::tape::DtaStateId::NONE,
                            failing_rule:
                                ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                        },
                    )
                },
            }
        }
        _ => {
            // Any other non-Alt Wrap body is structurally unsupported
            // under `has_shape_dispatcher_entrypoint` admission; emit a
            // syntax error rather than infinite-loop via the dispatcher.
            let _ = dispatcher_ident;
            quote! {
                ::core::result::Result::Err(
                    ::bbnf::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state:
                            ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule:
                            ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    },
                )
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
        ///
        /// AX.W0a.2.f — compound; see `flat.rs` emission for the
        /// `#[inline]` downgrade rationale (LLVM inline-cycle
        /// collapse vs hard-requirement inliner abort).
        #[inline]
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
/// shape fn. The emission partitions branches into two sets:
///
/// 1. **Byte-dispatch branches** — Ref-to-classified whose target
///    carries a bounded first-byte set (≤ 16 bytes). These emit as
///    match arms keyed on the byte patterns.
///
/// 2. **Linear-try fallback branches** — Regex branches, Refs with
///    unbounded/large first sets, or overlapping first sets. These
///    emit as sequential attempts that roll back `*p` + tape columns
///    on failure, breaking out on first success.
///
/// The pre-AX.W0a.2.e emission delegated the fallback set to the
/// grammar's `#dispatcher_ident` (`__value`). For non-Alt-rooted
/// grammars that dispatcher IS the root shape fn, so the fallback
/// loops through the root unbounded. The linear-try rewrite
/// eliminates the cycle while preserving byte-dispatch performance
/// for the common disjoint-FIRST case (JSON `value`).
fn emit_alt_tape_dispatch(
    branches: &[bbnf_ir::AltBranch],
    grammar_suffix: &str,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> TokenStream {
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let _ = dispatcher_ident; // retained for trait-level compat; not emitted.

    // Partition branches into byte-dispatch and linear-try sets. The
    // split mirrors `alt_dispatch.rs`'s treatment: a branch whose
    // first-byte set is bounded contributes a per-byte entry; every
    // other branch contributes an in-order linear-try body.
    //
    // Branches that share a first byte (e.g. BBNF's `comment` and
    // `big_comment` both starting with `/`) must collapse into ONE
    // match arm that linear-tries each in grammar order; otherwise
    // Rust's `match` would only take the first arm and later
    // same-byte branches become unreachable.
    let mut per_byte: std::collections::BTreeMap<u8, Vec<TokenStream>> = Default::default();
    let mut linear_arms: Vec<TokenStream> = Vec::new();

    for branch in branches {
        let inner = unwrap_outer(&branch.node);
        let body_call = emit_wrap_branch_call_tape(inner, grammar_suffix, ir);
        let Some((call, first_bytes)) = body_call else {
            // Branch is not emitter-routable (unclassified or structurally
            // unsupported). Skip — under `has_shape_dispatcher_entrypoint`
            // admission every reachable Ref target is classified, so the
            // only remaining unroutable branches are structurally malformed
            // grammars which reject during classification.
            continue;
        };
        let linear_body = emit_wrap_linear_body_tape(&call);
        if first_bytes.is_empty() {
            linear_arms.push(linear_body);
        } else {
            for b in first_bytes {
                per_byte.entry(b).or_default().push(linear_body.clone());
            }
        }
    }

    let byte_arms: Vec<TokenStream> = per_byte
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
        let first = #support_mod::skip_space(input, p, state)
            .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        'try_branches: loop {
            match first {
                #(#byte_arms)*
                _ => {}
            }
            #(#linear_arms)*
            return ::core::result::Result::Err(
                ::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                },
            );
        }
        // Reaching here means one branch succeeded; its call already
        // produced the tape record + advanced `*p`. Return an
        // unspecified offset placeholder — Wrap is transparent so
        // callers ignore the returned offset (they read the last-pushed
        // compound from the tape directly).
        Ok(::bbnf::runtime::tape::TapeOffset::NONE)
    }
}

/// Emit the guts of a Wrap-branch attempt: save `*p` + column length,
/// invoke `call`, on failure restore both + fall through; on success
/// `break 'try_branches`.
fn emit_wrap_linear_body_tape(call: &TokenStream) -> TokenStream {
    quote! {
        {
            let attempt_p = *p;
            let attempt_len = builder.columns_mut().len();
            match #call {
                Ok(_) => break 'try_branches,
                Err(_) => {
                    *p = attempt_p;
                    builder.columns_mut().truncate(attempt_len);
                }
            }
        }
    }
}

/// Resolve a Wrap-branch body to its shape-fn call + first-byte set.
/// Returns `(call_tokens, first_bytes)` or `None` when the branch is
/// structurally ineligible. An empty `first_bytes` denotes an
/// unbounded first set — the branch routes as a linear-try fallback.
fn emit_wrap_branch_call_tape(
    inner: &IrNode,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> Option<(TokenStream, Vec<u8>)> {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
    match inner {
        IrNode::Ref(rid) => {
            let target = ir.rules.iter().find(|r| r.id == *rid)?;
            let tag = ir.shape_assignments.get(*rid);
            let shape_name = shape_tag_name(tag)?;
            let target_fn =
                shape_fn_ident(shape_name, grammar_suffix, ir.get_string(target.name));
            // AX.W0a.2.g — Keyword fn signature extended with `state`.
            let call = match tag {
                ShapeTag::Number => quote! {
                    #target_fn(input, p, first, builder)
                },
                ShapeTag::Keyword => quote! {
                    #target_fn(input, p, first, state, builder)
                },
                _ => quote! {
                    #target_fn(input, p, state, builder)
                },
            };
            let first_bytes: Vec<u8> = target.meta.first_set.iter().collect();
            if first_bytes.len() > 16 {
                Some((call, Vec::new()))
            } else {
                Some((call, first_bytes))
            }
        }
        IrNode::Regex(sid) => {
            // Inline regex scan — emit via the per-grammar adapter the
            // inline module uses. No shape fn; wrap the scan directly.
            let pattern = ir.get_string(*sid).to_string();
            let regex_scan_ident = super::super::dta_walker::regex_scan_adapter_ident(
                &super::sanitise_grammar(grammar_suffix),
            );
            let call = quote! {
                {
                    let span_lo = *p as u32;
                    match #regex_scan_ident(#pattern, input, *p) {
                        ::core::option::Option::Some(len) => {
                            *p += len as usize;
                            let _ = builder.push_leaf_with(
                                ::bbnf::runtime::tape::TapeKind::Span,
                                span_lo,
                                *p as u32,
                                0,
                                0,
                                ::bbnf::runtime::tape::PayloadData::None,
                            );
                            ::core::result::Result::<
                                ::bbnf::runtime::tape::TapeOffset,
                                ::bbnf::runtime::tape::DtaError,
                            >::Ok(::bbnf::runtime::tape::TapeOffset::NONE)
                        }
                        ::core::option::Option::None => {
                            ::core::result::Result::Err(
                                ::bbnf::runtime::tape::DtaError::Syntax {
                                    offset: span_lo,
                                    failing_state:
                                        ::bbnf::runtime::tape::DtaStateId::NONE,
                                    failing_rule:
                                        ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                                },
                            )
                        }
                    }
                }
            };
            // Regex branches have no bounded first-byte set at emit time
            // (pattern-first-byte analysis would require NFA inspection);
            // route as linear-try fallback.
            Some((call, Vec::new()))
        }
        _ => None,
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
        ShapeTag::AltDispatch => Some("altdispatch"),
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
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
        /// recursion rationale.
        #[inline]
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
        // AX.W0a.2.g — visitor-path Keyword signature extended with
        // `state` (see tape-path call).
        let call = match tag {
            ShapeTag::Number => quote! {
                #target_fn(input, p, first, visitor)
            },
            ShapeTag::Keyword => quote! {
                #target_fn(input, p, first, state, visitor)
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
