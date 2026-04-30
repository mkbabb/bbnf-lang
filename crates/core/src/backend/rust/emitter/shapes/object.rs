//! Object-shape emitter — `parse_object_<grammar>_<rule>`.
//!
//! # Role — AW-V.W3.2
//!
//! Emits walker-tape-identical code for the canonical JSON object rule:
//!
//! ```text
//! object = "{" >> ((pair << comma?)*)?w << "}"
//! pair   = string, colon >> value
//! ```
//!
//! The emitted tape mirrors the walker's structural compound tree
//! exactly — the only difference is that dispatch is inlined (no
//! `dispatch_one`, no `try_branch`) while the record stream matches
//! byte-for-byte.
//!
//! # Emitted tape shape (for `{"k":v}`)
//!
//! ```text
//! [ 0] Seq     variant=<object_id>  span=0..N                                <- object outer Seq
//! [ 1] Seq     variant=0            span=0..N-1 has_children                 <- Next("{" , rest)
//! [ 2] Literal variant=0            span=0..1                                <- "{"
//! [ 3] Seq     variant=0            has_children                             <- OptionalWhitespace
//! [ 4] Rule    variant=0            has_children                             <- Repeat
//! [ 5] Seq     variant=0            has_children                             <- per-iter Skip(pair, Repeat(,?))
//! [ 6] Seq     variant=<pair_id>    has_children                             <- pair (Seq)
//! [ 7] ... string records ...                                                <- Ref(string)
//! [ .] Seq     variant=0            has_children                             <- Next(OptionalWhitespace(":"), Ref(value))
//! [ .] Seq     variant=0            has_children                             <- OptionalWhitespace(":")
//! [ .] Literal variant=0                                                     <- ":"
//! [ .] ... value records ...                                                 <- Ref(value)
//! [ .] Rule    variant=0            has_children                             <- Repeat(,?)
//! [ .] ... optional "," records ...
//! [ .] Literal variant=0                                                     <- "}"
//! ```

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::{
    dispatcher_fn_ident, emit_ref_call_shape, emit_ref_call_visitor, shape_fn_ident,
    visitor_dispatcher_fn_ident, visitor_shape_fn_ident,
};
use super::root_rule_name;
use super::substrate::{builder_ty_elided, builder_ty_with_lifetime};
use bbnf_ir::registry::EmitStrategy;

/// Emit the `parse_object_<grammar>_<rule>` body for the resolved
/// struct-builder substrate.
///
/// AZ-I.W2.RB — strategy match is at codegen time, not at runtime: the
/// emitter produces ONE function body per `(grammar, rule)` and selects
/// it on `strategy`. Per `feedback_no-orthogonal-codepaths` there is no
/// runtime conditional within a single emitted parse fn.
pub fn emit_parse_object(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    emit_parse_object_struct_direct(grammar_suffix, rule, ir, strategy)
}

/// AZ-I.W2.RB — struct-direct body for the Object shape.
///
/// Emits a `parse_object_<grammar>_<rule>` whose body drives
/// `JsonStructBuilder` directly: `begin_compound(&__layout)` opens the
/// object frame, the parse loop alternates `push_leaf_with_str(key)` /
/// recursive value calls, and `end_compound(handle)` closes it. The
/// function takes `&mut crate::runtime::JsonStructBuilder<'p>` instead
/// of `&mut Tape<()>` and returns `Result<(), DtaError>` (no
/// unit — the builder owns the in-flight stack and finalises into
/// `JsonDocument` at the call boundary in `parse()`).
///
/// The `__layout` lookup is asserted (not fall-back). Per the W2-EMITTER-
/// REWIRE plan §1, `for_grammar` GUARANTEES the layout exists for every
/// rule when the strategy is `StructDirect`; missing layout is a
/// W1.A invariant violation, not a runtime case to recover from.
fn emit_parse_object_struct_direct(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("object", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let rule_id_lit = rule.id;
    let rule_name_lit = rule_name.to_string();
    let p_lt = format_ident!("p");
    let builder_ty = builder_ty_with_lifetime(strategy, &p_lt);
    let _builder_ty_e = builder_ty_elided(strategy);

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    let (string_fn, _string_variant, _pair_variant, value_ref) =
        resolve_pair_context(grammar_suffix, ir);

    // Per-Ref direct value call (when classified) — same routing as the
    // tape path; targets are the per-shape struct-direct fns the sibling
    // emitters land. On struct-direct, all Ref calls take the
    // JsonStructBuilder argument unchanged from this fn's `builder`.
    let value_call = value_ref
        .and_then(|rid| emit_ref_call_shape(grammar_suffix, rid, ir))
        .map(|call| quote! { (#call)?; })
        .unwrap_or_else(|| {
            quote! {
                #dispatcher_ident(input, p, state, builder)?;
            }
        });

    quote! {
        /// AZ-I.W2.RB — per-grammar Object-shape parse function,
        /// **struct-direct body**. Targets [`JsonStructBuilder`].
        ///
        /// Walker-tape compound emission is replaced by typed
        /// `begin_compound` / `end_compound` calls against the in-flight
        /// frame stack. Per-element pushes (string keys + value
        /// dispatch) land directly on the topmost open frame.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident<'p>(
            input: &'p [u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty,
        ) -> ::core::result::Result<(), crate::runtime::tape::DtaError> {
            use crate::runtime::builder::StructBuilder;

            if input.get(*p).copied() != Some(b'{') {
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }

            // AZ-I.W2.RB — open the object compound. Inline layout
            // literal mirrors W2.RD/RF's pattern; JsonStructBuilder's
            // `begin_compound` consults `(kind, rule_name)` to route
            // OpenFrame::Object.
            let __layout: ::bbnf_ir::registry::StructLayout =
                ::bbnf_ir::registry::StructLayout {
                    rule_id: #rule_id_lit as ::bbnf_ir::RuleId,
                    rule_name: ::std::string::String::from(#rule_name_lit),
                    kind: ::bbnf_ir::registry::LayoutKind::Struct,
                    rule_type: ::bbnf_ir::TypeDesc::Span,
                    fields: ::std::vec::Vec::new(),
                };
            let __handle = builder.begin_compound(&__layout);

            *p += 1;
            let _ = #support_mod::skip_space(input, p, state);

            if input.get(*p).copied() == Some(b'}') {
                *p += 1;
                builder.end_compound(__handle);
                return Ok(());
            }

            loop {
                // Key: string shape fn pushes the decoded slice.
                if input.get(*p).copied() != Some(b'"') {
                    return Err(crate::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    });
                }
                #string_fn(input, p, state, builder)?;

                // Colon.
                let _ = #support_mod::skip_space(input, p, state);
                if input.get(*p).copied() != Some(b':') {
                    return Err(crate::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    });
                }
                *p += 1;
                let _ = #support_mod::skip_space(input, p, state);

                // Value dispatch — same per-Ref routing as the tape body.
                #value_call

                // Comma / close.
                let _ = #support_mod::skip_space(input, p, state);
                match input.get(*p).copied() {
                    Some(b',') => {
                        *p += 1;
                        let _ = #support_mod::skip_space(input, p, state);
                    }
                    Some(b'}') => {
                        *p += 1;
                        builder.end_compound(__handle);
                        return Ok(());
                    }
                    _ => return Err(crate::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    }),
                }
            }
        }
    }
}

/// Resolve the grammar's String-shape rule (for key parsing), the
/// pair rule's variant_idx, and the pair rule's value-position Ref
/// target. Returns `(string_fn_ident, string_variant_idx,
/// pair_variant_idx, value_ref)`.
///
/// The pair rule is the one whose body is `Seq(Ref(string), ...)`; per
/// JSON's canonical shape `pair = string, colon >> value`, walking the
/// rule list for a Seq-bodied rule with a Ref-to-string head yields it.
///
/// AW-V.W5.2 — `value_ref` is the pair rule's value-position Ref
/// target, extracted from the pair body's post-colon recursion. Used
/// by the Object emitter to emit a direct call to that target's shape
/// fn (per-Ref routing).
fn resolve_pair_context(
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> (proc_macro2::Ident, u8, u8, Option<bbnf_ir::RuleId>) {
    use bbnf_ir::IrNode;
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;

    // Find the String-shape rule.
    let string_rule = ir
        .rules
        .iter()
        .find(|r| matches!(ir.shape_assignments.get(r.id), ShapeTag::String))
        .expect("object-shape admission requires a String-shape sibling rule");
    let string_name = ir.get_string(string_rule.name);
    let string_fn = shape_fn_ident("string", grammar_suffix, string_name);
    let string_variant = (string_rule.id & 0xFF) as u8;

    // Find the pair rule — a Seq whose first child is Ref(string_rule_id).
    let pair_rule = ir.rules.iter().find(|r| match &r.body {
        IrNode::Seq(children) => children
            .first()
            .map(|c| matches!(c, IrNode::Ref(rid) if *rid == string_rule.id))
            .unwrap_or(false),
        _ => false,
    });
    let pair_variant = pair_rule.map(|r| (r.id & 0xFF) as u8).unwrap_or(0);

    // AW-V.W5.2 — extract the value-position Ref target from the pair
    // body. Canonical JSON pair body:
    //   Seq[Ref(string), Next(Ref(colon), Ref(value))]
    // Walk to find the non-string, non-colon Ref — that's the value.
    let value_ref =
        pair_rule.and_then(|pair| extract_value_ref_from_pair_body(&pair.body, string_rule.id, ir));

    (string_fn, string_variant, pair_variant, value_ref)
}

/// Extract the value-position Ref target from a pair rule body.
/// Skips the string-key Ref (head) and colon-related nodes; returns
/// the first Ref whose target is NOT the string rule and NOT a
/// punctuation rule (a rule whose body is a single-char Literal).
fn extract_value_ref_from_pair_body(
    node: &bbnf_ir::IrNode,
    string_rid: bbnf_ir::RuleId,
    ir: &GrammarIR,
) -> Option<bbnf_ir::RuleId> {
    use bbnf_ir::IrNode;
    fn is_punctuation_rule(rid: bbnf_ir::RuleId, ir: &GrammarIR) -> bool {
        // A "punctuation" rule body is a single literal (colon ":" ?w,
        // comma "," ?w). Unwrap OW + Map to reach the core node.
        let rule = match ir.rules.iter().find(|r| r.id == rid) {
            Some(r) => r,
            None => return false,
        };
        fn unwrap<'a>(n: &'a IrNode) -> &'a IrNode {
            match n {
                IrNode::OptionalWhitespace(i) | IrNode::Map { inner: i, .. } => unwrap(i),
                _ => n,
            }
        }
        matches!(unwrap(&rule.body), IrNode::Literal(_))
    }
    fn walk(node: &IrNode, string_rid: bbnf_ir::RuleId, ir: &GrammarIR) -> Option<bbnf_ir::RuleId> {
        match node {
            IrNode::Ref(rid) => {
                if *rid == string_rid {
                    return None;
                }
                if is_punctuation_rule(*rid, ir) {
                    return None;
                }
                Some(*rid)
            }
            IrNode::Seq(children) => {
                for c in children {
                    if let Some(v) = walk(c, string_rid, ir) {
                        return Some(v);
                    }
                }
                None
            }
            IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
                walk(lhs, string_rid, ir).or_else(|| walk(rhs, string_rid, ir))
            }
            IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
                walk(inner, string_rid, ir)
            }
            _ => None,
        }
    }
    walk(node, string_rid, ir)
}

// ─────────────────────────────────────────────────────────────────────
// AW-V.W3-bench-fix — visitor-path Object emitter.
//
// Mirrors the prototype's `json_prototype::parse_object::<V>`
// (crates/json-prototype/src/lib.rs:258). Bypasses the tape
// entirely: `visitor.begin_object()` / `visitor.key()` /
// `visitor.end_object()` replace the compound + leaf record pushes
// the tape-path emits.
// ─────────────────────────────────────────────────────────────────────

/// Resolve the grammar's String-shape visitor-path fn ident for key
/// parsing.
fn resolve_visitor_pair_context(grammar_suffix: &str, ir: &GrammarIR) -> proc_macro2::Ident {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;

    let string_rule = ir
        .rules
        .iter()
        .find(|r| matches!(ir.shape_assignments.get(r.id), ShapeTag::String))
        .expect("object-shape admission requires a String-shape sibling rule");
    let string_name = ir.get_string(string_rule.name);
    visitor_shape_fn_ident("string", grammar_suffix, string_name)
}

/// Emit `pub fn parse_object_visitor_<grammar>_<rule><V: JsonVisitor>(...)
/// -> Result<(), ParseErr>`.
pub fn emit_parse_object_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("object", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = visitor_dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    let string_fn = resolve_visitor_pair_context(grammar_suffix, ir);

    // AW-V.W5.2 — resolve value-position Ref for visitor path.
    let (_, _, _, value_ref) = resolve_pair_context(grammar_suffix, ir);
    let value_call = value_ref
        .and_then(|rid| emit_ref_call_visitor(grammar_suffix, rid, ir))
        .map(|call| quote! { (#call)?; })
        .unwrap_or_else(|| {
            quote! {
                #dispatcher_ident(input, p, state, visitor)?;
            }
        });

    quote! {
        /// AW-V.W3-bench-fix — visitor-path Object-shape parse function.
        ///
        /// Mirrors `json_prototype::parse_object::<V>`. Bypasses
        /// the tape entirely; visitor method calls drive materialisation.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]`.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            visitor: &mut V,
        ) -> ::core::result::Result<(), crate::runtime::ParseErr>
        where
            V: crate::runtime::tape::ObjectVisitor
                + crate::runtime::tape::ArrayVisitor
                + crate::runtime::tape::StringVisitor
                + crate::runtime::tape::NumberVisitor
                + crate::runtime::tape::KeywordVisitor,
        {
            let begin_at = *p;
            if input.get(*p).copied() != Some(b'{') {
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: begin_at as u32, rule: None,
                });
            }
            *p += 1;
            visitor.begin_object().map_err(|_| crate::runtime::ParseErr::Syntax {
                offset: begin_at as u32, rule: None,
            })?;
            let mut cur = #support_mod::skip_space(input, p, state);
            if cur == Some(b'}') {
                *p += 1;
                return visitor.end_object().map_err(|_| crate::runtime::ParseErr::Syntax {
                    offset: *p as u32, rule: None,
                });
            }
            loop {
                if cur != Some(b'"') {
                    return Err(crate::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    });
                }
                #string_fn(input, p, state, visitor, /*is_key=*/ true)?;
                if #support_mod::skip_space(input, p, state) != Some(b':') {
                    return Err(crate::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    });
                }
                *p += 1;
                let _ = #support_mod::skip_space(input, p, state);
                // AW-V.W5.2 — per-Ref direct call when classified.
                #value_call
                match #support_mod::skip_space(input, p, state) {
                    Some(b'}') => {
                        *p += 1;
                        return visitor.end_object().map_err(|_| crate::runtime::ParseErr::Syntax {
                            offset: *p as u32, rule: None,
                        });
                    }
                    Some(b',') => {
                        *p += 1;
                        cur = #support_mod::skip_space(input, p, state);
                    }
                    _ => return Err(crate::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    }),
                }
            }
        }
    }
}
