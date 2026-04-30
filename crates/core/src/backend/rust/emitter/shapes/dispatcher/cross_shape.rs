//! Top-level dispatcher fns + Alt-byte-dispatch body emission.
//!
//! The dispatcher follows the JSON-shaped layout: skip leading
//! whitespace, dispatch on the next byte to the appropriate per-shape
//! fn, and carry a sibling Alt-body emitter for root rules whose body
//! is `Alt(Ref, Ref, ...)` of classified branches.

use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::parse_str;

use super::super::{has_shape_dispatch, root_rule_name};
use super::shape_tag_name;
use super::symbol_composition::{dispatcher_fn_ident, shape_fn_ident};
use bbnf_ir::registry::EmitStrategy;

/// Build the dispatcher's `builder: &mut <Type>` parameter for the
/// active emit strategy. StructDirect grammars carry the per-grammar
/// concrete struct-builder declared in the strategy variant.
fn dispatcher_builder_type(strategy: &EmitStrategy) -> TokenStream {
    match strategy {
        EmitStrategy::StructDirect { rust, .. } => {
            let path: syn::Path = parse_str(rust.builder_path).unwrap_or_else(|_| {
                panic!(
                    "invalid rust.builder_path in EmitStrategy: {}",
                    rust.builder_path,
                )
            });
            // Bind the builder's type parameter to `'p` so the dispatcher's
            // input lifetime threads to per-shape calls (which all expect
            // `&mut <Builder><'p>` matching `input: &'p [u8]`).
            quote! { #path<'p> }
        }
    }
}

/// Emit the dispatcher fn — `parse_<grammar>_<root>`.
///
/// Dispatches the next non-whitespace byte to the appropriate
/// shape-specific function. Routes through the rule's Alt structure
/// when the root rule is itself an Alt (JSON's `value =
/// object | array | string | number | bool | null` pattern); when the
/// root is a single-shape rule (e.g. a top-level Array), the
/// dispatcher emits a thin delegator.
pub fn emit_dispatcher(
    grammar_suffix: &str,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    let builder_ty = dispatcher_builder_type(strategy);
    let (lifetime_params, input_lifetime) = (quote! { <'p> }, quote! { &'p });
    let Some(root_name) = root_rule_name(ir) else {
        return quote! {};
    };
    let entry = ir.entry;
    let Some(entry_rule) = ir.rules.iter().find(|r| r.id == entry) else {
        return quote! {};
    };

    let dispatcher_ident = dispatcher_fn_ident(grammar_suffix, &root_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    // Decide the dispatcher strategy based on the root rule's shape.
    // If the root is Alt-bodied (JSON's `value`) we emit a byte-
    // dispatch over the Alt branches. If the root is itself a shape
    // (e.g. a top-level Object), we emit a delegator to that shape fn.
    let root_tag = ir.shape_assignments.get(entry);

    let dispatch_body = if matches!(&entry_rule.body, bbnf_ir::IrNode::Alt(_, _))
        && has_shape_dispatch(ir)
    {
        // Root body is an Alt — enumerate branches and emit per-branch
        // byte-dispatch arms targeting each Ref's shape fn. Both an
        // unclassified root (pre-W4) and a Wrap-classified root (W4+)
        // take this path when the body is Alt-shaped.
        emit_alt_dispatch_body(grammar_suffix, entry_rule, ir)
    } else if root_tag.is_classified() {
        // AW-V.W4-activation — root is itself a W3 or W4 shape. The
        // dispatcher delegates directly to `parse_<shape>_<grammar>_<root>`.
        // Shape-fn arg shapes:
        //   - Number / Keyword take `first_byte` — the dispatcher peeks
        //     first non-ws byte, passes it in.
        //   - Object / Array / String / Scalar / Pratt / Unordered /
        //     ArgList / Flat / Wrap / HRegex take `(input, p, state,
        //     builder)` — the dispatcher skips leading ws and delegates.
        let shape_name = shape_tag_name(root_tag);
        let target_ident = shape_fn_ident(shape_name, grammar_suffix, &root_name);
        match root_tag {
            // AX.W0a.2.g — Keyword signature extended with `state`.
            // Number stays at `(input, p, first, builder)`; Keyword now
            // takes `(input, p, first, state, builder)`. The split
            // mirrors the Ref-call emitter's per-shape switch.
            ShapeTag::Number => quote! {
                let first = #support_mod::skip_space(input, p, state)
                    .ok_or(crate::runtime::DtaError::UnexpectedEnd { offset: *p as u32 })?;
                #target_ident(input, p, first, builder)
            },
            ShapeTag::Keyword => quote! {
                let first = #support_mod::skip_space(input, p, state)
                    .ok_or(crate::runtime::DtaError::UnexpectedEnd { offset: *p as u32 })?;
                #target_ident(input, p, first, state, builder)
            },
            _ => quote! {
                let _ = #support_mod::skip_space(input, p, state);
                #target_ident(input, p, state, builder)
            },
        }
    } else if matches!(root_tag, ShapeTag::None) && has_shape_dispatch(ir) {
        // Root unclassified but grammar has classified rules — use the
        // legacy Alt-dispatch body (pre-W4 pattern preserved for
        // transitional grammars where the root is a transparent alias).
        emit_alt_dispatch_body(grammar_suffix, entry_rule, ir)
    } else {
        // No shape coverage — shouldn't reach here (caller gates
        // dispatcher emission); emit a stub for safety.
        quote! {
            Err(crate::runtime::DtaError::InvalidState {
                state: 0,
            })
        }
    };

    // Dispatcher — for JSON's `value = object | array | string | number |
    // bool | null` Alt-dispatch pattern, this maps to a ByteDispatch
    // state at the DTA level. ByteDispatch pushes NO compound; it simply
    // transitions to the chosen branch rule's entry state. The
    // `pending_variant_idx` stamped by the Ref into `value` is then
    // overwritten by the target rule's own Ref-set stamp (e.g. array's
    // Ref sets variant=4, which lands on array's Seq compound push).
    //
    // Therefore the shape dispatcher emits NO outer wrap — it directly
    // delegates to the chosen shape fn. Both the root call site (from
    // `parse()`) and the non-root call site (from Object / Array value-
    // position recursion) share the same dispatch body; the walker
    // likewise does not differentiate between root and non-root value
    // positions (ByteDispatch's transition is the same either way).
    //
    // `#nonroot_ident` retained as an alias for backwards symbol
    // compatibility with the per-shape emitters that reference it —
    // both idents point at the same body.
    let nonroot_ident = format_ident!("{}__value", dispatcher_ident);
    let _ = entry;

    quote! {
        /// AW-V.W3.2 — top-level shape dispatcher.
        ///
        /// Mirrors the walker's `value` rule ByteDispatch: skip leading
        /// whitespace, dispatch on the first byte to the chosen branch
        /// shape fn, return unit after the chosen shape succeeds. No outer Rule /
        /// Alt compound is pushed — the DTA's ByteDispatch state for
        /// `value` emits no compound either, and the target rule's Ref
        /// overwrites any `pending_variant_idx` en route, so the chosen
        /// rule's own compound carries the final root variant.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
        /// recursion rationale.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #dispatcher_ident #lifetime_params(
            input: #input_lifetime [u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            #nonroot_ident(input, p, state, builder)
        }

        /// AW-V.W3.2 — value-position shape dispatcher. Called both at
        /// the grammar root and from Object / Array compound bodies.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]`.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #nonroot_ident #lifetime_params(
            input: #input_lifetime [u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            #dispatch_body
        }
    }
}

/// Emit the Alt-dispatch body for the root rule — byte-matches the
/// next non-whitespace byte and calls the corresponding branch shape
/// fn. Mirrors `json_prototype::parse_value`'s 6-arm match.
pub(super) fn emit_alt_dispatch_body(
    grammar_suffix: &str,
    root_rule: &bbnf_ir::IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    use bbnf_ir::IrNode;

    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    // Walk the Alt branches (or single body) and collect per-branch
    // (first-byte(s), shape-fn-ident) pairs.
    let branches = match &root_rule.body {
        IrNode::Alt(bs, _) => bs.as_slice(),
        _ => {
            // Single body — unreachable via root_tag guard above.
            return quote! {
                Err(crate::runtime::DtaError::InvalidState {
                state: 0,
            })
            };
        }
    };

    let mut object_fn: Option<proc_macro2::Ident> = None;
    let mut array_fn: Option<proc_macro2::Ident> = None;
    let mut string_fn: Option<proc_macro2::Ident> = None;
    let mut number_fn: Option<proc_macro2::Ident> = None;
    let mut keyword_bool_fn: Option<proc_macro2::Ident> = None;
    let mut keyword_null_fn: Option<proc_macro2::Ident> = None;

    for branch in branches {
        let IrNode::Ref(rid) = &branch.node else {
            continue;
        };
        let Some(rule) = ir.rules.iter().find(|r| r.id == *rid) else {
            continue;
        };
        let name = ir.get_string(rule.name);
        let tag = ir.shape_assignments.get(*rid);
        match tag {
            ShapeTag::Object => {
                object_fn = Some(shape_fn_ident("object", grammar_suffix, name));
            }
            ShapeTag::Array => {
                array_fn = Some(shape_fn_ident("array", grammar_suffix, name));
            }
            ShapeTag::String => {
                string_fn = Some(shape_fn_ident("string", grammar_suffix, name));
            }
            ShapeTag::Number => {
                number_fn = Some(shape_fn_ident("number", grammar_suffix, name));
            }
            ShapeTag::Keyword => {
                // Distinguish bool (two branches) from null (one
                // branch) via the rule's body shape.
                let is_null = rule_is_single_null_keyword(rule, ir);
                if is_null {
                    keyword_null_fn = Some(shape_fn_ident("keyword", grammar_suffix, name));
                } else {
                    keyword_bool_fn = Some(shape_fn_ident("keyword", grammar_suffix, name));
                }
            }
            _ => {}
        }
    }

    // Emit the arms, gating each on whether the branch shape fn
    // resolved. Missing branches fall into the default error arm.
    let object_arm = object_fn
        .as_ref()
        .map(|f| quote! { b'{' => { #f(input, p, state, builder) } })
        .unwrap_or_else(|| quote! {});
    let array_arm = array_fn
        .as_ref()
        .map(|f| quote! { b'[' => { #f(input, p, state, builder) } })
        .unwrap_or_else(|| quote! {});
    let string_arm = string_fn
        .as_ref()
        .map(|f| quote! { b'"' => { #f(input, p, state, builder) } })
        .unwrap_or_else(|| quote! {});
    let number_arm = number_fn
        .as_ref()
        .map(|f| quote! { b'-' | b'0'..=b'9' => { #f(input, p, first, builder) } })
        .unwrap_or_else(|| quote! {});
    // AX.W0a.2.g — Keyword fn signature extended with `state: &mut
    // ScanState` so Ref-led Alt branches can delegate via
    // `emit_ref_call_shape`. Threading `state` here is a no-op for the
    // JSON true_arm / null_arm single-literal forms (they ignore the
    // argument via `_state`), and carries the Ref-branch delegation
    // path for grammars that admit Ref-led Keyword branches.
    let true_arm = keyword_bool_fn
        .as_ref()
        .map(|f| quote! { b't' | b'f' => { #f(input, p, first, state, builder) } })
        .unwrap_or_else(|| quote! {});
    let null_arm = keyword_null_fn
        .as_ref()
        .map(|f| quote! { b'n' => { #f(input, p, first, state, builder) } })
        .unwrap_or_else(|| quote! {});

    quote! {
        let first = #support_mod::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd { offset: *p as u32 })?;
        let __result = match first {
            #object_arm
            #array_arm
            #string_arm
            #number_arm
            #true_arm
            #null_arm
            c => {
                return ::core::result::Result::Err(
                    crate::runtime::DtaError::Syntax {
                        offset: *p as u32,
                    }
                );
            }
        };
        __result
    }
}

/// Predicate: is `rule`'s body a single literal matching `null`?
fn rule_is_single_null_keyword(rule: &bbnf_ir::IrRule, ir: &GrammarIR) -> bool {
    use bbnf_ir::IrNode;
    // Walk through Map / OptionalWhitespace wrappers.
    fn unwrap(node: &IrNode) -> &IrNode {
        match node {
            IrNode::Map { inner, .. } => unwrap(inner.as_ref()),
            IrNode::OptionalWhitespace(inner) => unwrap(inner.as_ref()),
            _ => node,
        }
    }
    matches!(unwrap(&rule.body), IrNode::Literal(sid)
        if ir.get_string(*sid) == "null")
}
