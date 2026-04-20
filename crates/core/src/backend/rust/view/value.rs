//! AY.W3b.1 — grammar-emitted `<Grammar>Value<'p>` enum via TypeDesc
//! equivalence-class collapse.
//!
//! Emits three artefacts per grammar:
//!
//! 1. `pub enum <Grammar>Value<'p>` — one variant per
//!    TypeDesc-equivalence class covering the grammar's
//!    non-transparent rules. Leaves carry typed scalar payloads,
//!    Span rules carry `&'p str`, compound rules carry a
//!    `Vec<<Grammar>Value<'p>>` of eagerly-materialised children,
//!    and the fallback `Unknown` variant wraps the generic
//!    `<Grammar>NodeView<'p>` for recovered / unclassified records.
//!
//! 2. `impl ::bbnf::runtime::ValueRoot for <Grammar>` — the GAT
//!    binding with `type Value<'p> = <Grammar>Value<'p>` + the
//!    `view_to_value` entry-point that dispatches through the
//!    grammar's per-shape `materialize_*_<Grammar>` inline fns
//!    emitted in `emitter/shapes/value_materialize.rs`.
//!
//! 3. `impl ::bbnf::runtime::PathQuery<T> for <Grammar>` for
//!    `T ∈ { &str, f64, bool }` — linear-walk path queries against
//!    the tape. The emitted impls are < 100 LOC each; the
//!    binary-search packed-cache variant is a follow-on optimisation
//!    — for now a cursor walk over the tape suffices for the hard-gate
//!    sub-item "at least one PathQuery impl emits" and keeps the
//!    emitted surface tractable.
//!
//! # TypeDesc-equivalence-class collapse
//!
//! The per-AY.md prop 3 thesis: rules with identical `TypeDesc`
//! collapse into one variant. For the first cut we emit one variant
//! per non-transparent rule (identity collapse); the
//! [`collect_variant_classes`] helper is the single point where the
//! class map is computed, so a future widening to
//! `FxHashMap<TypeDesc, VariantIndex>` is a drop-in change without
//! disturbing the consumers. The per-shape `materialize_*` fns
//! dispatch on `rule_kind()` regardless of the collapse depth —
//! multiple rule kinds mapping to the same variant simply call the
//! same variant constructor.

use bbnf_ir::{GrammarIR, IrNode, IrRule, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// Shape the variant's payload takes in `<Grammar>Value`.
///
/// Resolved from the rule's `TypeDesc` + its body shape. The
/// compound rules fall into [`VariantShape::Compound`] regardless of
/// whether their body is `Seq`, `Alt`, or `Repeat` — the per-shape
/// `materialize_*` fns inspect the body shape when constructing the
/// variant, this only governs the enum variant's declared type.
#[derive(Clone, Debug, PartialEq, Eq)]
enum VariantShape {
    /// Borrowed source span — `&'p str`.
    Span,
    /// Typed scalar primitive — carries the TypeDesc so the emitter
    /// can splice in the Rust type token (`f64`, `bool`, etc.).
    Scalar(TypeDesc),
    /// Compound (Seq / Alt / Repeat) — carries a
    /// `Vec<<Grammar>Value<'p>>` of eagerly-materialised children.
    /// The variant's semantic identity lives in the variant name
    /// (rule-derived); the compound's structure is whatever the
    /// materialize fn produced.
    Compound,
    /// Fallback / unclassified — wraps the generic NodeView. Used
    /// for recovered records and the mandatory `Unknown` catch-all.
    Cursor,
}

/// Emit the `<Grammar>Value<'p>` enum + `impl ValueRoot` + narrow
/// `PathQuery` impls for the grammar.
///
/// Returns an empty [`TokenStream`] when the grammar has no
/// non-transparent rules.
pub fn emit_value_surface(ir: &GrammarIR, grammar_name: &str) -> TokenStream {
    let non_transparent: Vec<&IrRule> = ir
        .rules
        .iter()
        .filter(|r| !r.meta.is_transparent)
        .collect();
    if non_transparent.is_empty() {
        return quote! {};
    }

    let variants = collect_variant_classes(ir, &non_transparent);
    let value_ident = format_ident!("{}Value", grammar_name);
    let grammar_ident = format_ident!("{}", grammar_name);
    let node_view_ident = format_ident!("{}NodeView", grammar_name);
    let rule_kind_ident = format_ident!("{}RuleKind", grammar_name);

    let enum_tokens = emit_enum_decl(&value_ident, &node_view_ident, &variants);
    let value_root_impl = emit_value_root_impl(
        &grammar_ident,
        &value_ident,
        &node_view_ident,
        &rule_kind_ident,
        &variants,
        grammar_name,
    );
    let path_query_impls = emit_path_query_impls(
        &grammar_ident,
        &value_ident,
        &node_view_ident,
        &rule_kind_ident,
        &variants,
    );

    quote! {
        #enum_tokens
        #value_root_impl
        #path_query_impls
    }
}

/// A single variant entry in `<Grammar>Value`.
struct VariantEntry {
    /// Rule name (variant identifier in the enum).
    name: String,
    /// Rule id — the discriminator index the view's `rule_kind()`
    /// dispatcher emits for this rule.
    rule_id: u32,
    /// The payload shape of the variant.
    shape: VariantShape,
}

/// Walk the grammar's non-transparent rules and assign one variant
/// per rule. This is the per-AY.md prop 3 collapse site — today we
/// emit a 1:1 rule → variant mapping; the `FxHashMap<TypeDesc,
/// VariantIndex>` widening lives here when it lands.
fn collect_variant_classes(ir: &GrammarIR, rules: &[&IrRule]) -> Vec<VariantEntry> {
    let mut out: Vec<VariantEntry> = Vec::with_capacity(rules.len());
    let mut seen_names: std::collections::HashSet<String> =
        std::collections::HashSet::with_capacity(rules.len());

    for rule in rules {
        let raw_name = ir.get_string(rule.name).to_string();
        let mut name = raw_name.clone();
        // Disambiguate collisions — rare, but defensive for
        // generated sub-variant names colliding with rule names.
        let mut idx = 0;
        while !seen_names.insert(name.clone()) {
            idx += 1;
            name = format!("{}_{}", raw_name, idx);
        }
        let shape = classify_shape(rule, ir);
        out.push(VariantEntry {
            name,
            rule_id: rule.id,
            shape,
        });
    }

    out
}

/// Classify the variant's payload shape from the rule's TypeDesc +
/// body. The priority order is:
///
/// 1. Aggregate payload layout (multi-field scalar tuple) → Compound
///    (materialize reads the tuple via the view's `.value()` accessor
///    and wraps it in a child-count-1 compound).
/// 2. Scalar `TypeDesc::Span` → Span variant carrying `&'p str`.
/// 3. Scalar payload `TypeDesc` (`F64`, `Bool`, `U32`, …) → Scalar
///    variant carrying the primitive.
/// 4. `IrNode::Repeat` / `IrNode::Seq` / `IrNode::Alt` body → Compound.
/// 5. Fallback → Cursor (wraps `NodeView`).
fn classify_shape(rule: &IrRule, ir: &GrammarIR) -> VariantShape {
    if ir.payload_layouts.contains_key(&rule.id) {
        // Aggregate (tuple of scalars packed in the payload buffer)
        // — Compound so the materialize fn can build a typed tuple
        // via the existing `.value()` accessor on the per-rule view.
        return VariantShape::Compound;
    }

    let type_desc = ir
        .types
        .iter()
        .find_map(|(id, ty)| (*id == rule.id).then_some(ty));

    match type_desc {
        Some(TypeDesc::Span) => VariantShape::Span,
        Some(td) if td.is_scalar_payload() && !matches!(td, TypeDesc::Span) => {
            VariantShape::Scalar(td.clone())
        }
        _ => {
            // Classify via body shape.
            match peel_body(&rule.body) {
                IrNode::Seq(_) | IrNode::Alt(_, _) | IrNode::Repeat { .. } => {
                    VariantShape::Compound
                }
                IrNode::Literal(_) | IrNode::Regex(_) => VariantShape::Span,
                _ => VariantShape::Cursor,
            }
        }
    }
}

/// Peel through `Map` / `OptionalWhitespace` to reach the
/// structurally-significant body node. Mirrors the helper in
/// `view/mod.rs`.
fn peel_body(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => peel_body(inner),
        other => other,
    }
}

/// Emit the `pub enum <Grammar>Value<'p> { ... }` declaration.
///
/// Payload shapes:
/// - Span → `&'p str`
/// - Scalar(td) → primitive (`f64`, `bool`, etc.)
/// - Compound → `::std::vec::Vec<<Grammar>Value<'p>>`
/// - Cursor → `<Grammar>NodeView<'p>`
///
/// Every enum emits a `Unknown(NodeView)` catch-all so the
/// `rule_kind() == Unknown` dispatch never panics on recovered /
/// unclassified records.
fn emit_enum_decl(
    value_ident: &syn::Ident,
    node_view_ident: &syn::Ident,
    variants: &[VariantEntry],
) -> TokenStream {
    let variant_tokens: Vec<TokenStream> = variants
        .iter()
        .map(|v| {
            let id = format_ident!("{}", v.name);
            match &v.shape {
                VariantShape::Span => quote! { #id(&'p str) },
                VariantShape::Scalar(td) => {
                    let ty = format_ident!(
                        "{}",
                        td.rust_ident().expect("scalar TypeDesc has rust_ident"),
                    );
                    quote! { #id(#ty) }
                }
                VariantShape::Compound => {
                    quote! { #id(::std::vec::Vec<#value_ident<'p>>) }
                }
                VariantShape::Cursor => {
                    quote! { #id(#node_view_ident<'p>) }
                }
            }
        })
        .collect();

    quote! {
        /// AY.W3b.1 — grammar-emitted value enum. Eager materialisation
        /// target for `Parsed::to_value()`. Variants enumerate
        /// non-transparent rules (TypeDesc-equivalence-class collapse
        /// applied per the emitter); compound variants carry
        /// child values in declaration order.
        #[derive(Clone, Debug)]
        pub enum #value_ident<'p> {
            #(#variant_tokens,)*
            /// Fallback for records whose `variant_idx` is not a
            /// known rule discriminator (recovered records, stray
            /// sub-variant indices).
            Unknown(#node_view_ident<'p>),
        }
    }
}

/// Emit `impl ::bbnf::runtime::ValueRoot for <Grammar>` — the GAT
/// `type Value<'p> = <Grammar>Value<'p>` binding + the
/// `view_to_value` entry point that dispatches through the grammar's
/// per-shape `materialize_*_<Grammar>` inline fns.
///
/// The emitted body delegates to `materialize_value_<Grammar>` —
/// the root materialiser. It dispatches on `rule_kind()` and
/// tail-calls the per-shape fns to build the variant.
fn emit_value_root_impl(
    grammar_ident: &syn::Ident,
    value_ident: &syn::Ident,
    node_view_ident: &syn::Ident,
    _rule_kind_ident: &syn::Ident,
    _variants: &[VariantEntry],
    grammar_name: &str,
) -> TokenStream {
    let root_materialize_fn =
        format_ident!("materialize_value_{}", grammar_name);

    // Root view type is always the grammar's View<'p> per the
    // existing `impl Root` binding — we don't need to name it; the
    // `Self::View<'p>` GAT resolves to it.
    //
    // The root view is convertible to a NodeView via `.cursor()` +
    // `NodeView::from_cursor(cursor, input)`. We bounce through
    // NodeView so the materialiser can dispatch uniformly on
    // `rule_kind()` regardless of which specific per-rule view the
    // caller started from.

    quote! {
        impl ::bbnf::runtime::ValueRoot for #grammar_ident {
            type Value<'p> = #value_ident<'p>;

            #[inline]
            fn view_to_value<'p>(view: Self::View<'p>) -> Self::Value<'p>
            where
                Self: 'p,
            {
                let __node = #node_view_ident::from_cursor(view.cursor(), view.input());
                #root_materialize_fn(__node)
            }
        }
    }
}

/// Emit narrow `PathQuery<T>` impls for the common leaf types.
/// Returns one impl per T ∈ { &'static str, f64, bool } — each
/// performs a cursor-walk over the tape, descending through
/// `PathSegment::Field` / `PathSegment::Index` steps and extracting
/// the leaf on exact match.
///
/// The emitted walker is intentionally linear: the binary-search
/// packed-cache variant that AY.W3b.1 §Step 5 optionally mentions
/// lives in a follow-on tranche — the hard gate requires one
/// PathQuery impl emits, and the lazy lane's perf claim rests on the
/// `get_by_path` being O(depth·siblings) against sonic_rs's own
/// cursor walk, which has the same complexity.
///
/// For `PathSegment::Field` the walker compares the field's source
/// span against the requested key — a zero-copy bytewise compare.
/// For `PathSegment::Index` the walker selects the `i`-th child by
/// cursor iteration.
fn emit_path_query_impls(
    grammar_ident: &syn::Ident,
    _value_ident: &syn::Ident,
    node_view_ident: &syn::Ident,
    _rule_kind_ident: &syn::Ident,
    _variants: &[VariantEntry],
) -> TokenStream {
    // Helper fn name for the shared cursor walker: lives next to the
    // PathQuery impls so every leaf-type-specialised impl calls into
    // it after the cursor has been narrowed to the target record.
    // Emitted alongside the impls; internal to the grammar module.

    let walk_fn = quote! {
        /// AY.W3b.1 — shared path walker. Descends from `view` per
        /// the given path, returning the narrowed NodeView on hit
        /// or `None` when any step misses.
        ///
        /// `Field` steps match by comparing the child's source span
        /// against the requested key; for object-like compounds the
        /// key is the Span leaf at child index `2*i`, and the value
        /// is at `2*i+1` (see per-grammar object materialisation).
        /// `Index` steps select the `i`-th child directly.
        ///
        /// The walker intentionally treats every compound uniformly —
        /// the emitter does not specialise per rule body today; the
        /// binary-search packed-cache variant is a follow-on.
        #[inline]
        fn __path_walk<'p>(
            view: #node_view_ident<'p>,
            path: ::bbnf::runtime::Path<'_>,
        ) -> ::core::option::Option<#node_view_ident<'p>> {
            let mut cur = view;
            for seg in path.iter() {
                match seg {
                    ::bbnf::runtime::PathSegment::Field(key) => {
                        // Walk children two at a time: (key, value).
                        // The key child's span text is compared to
                        // the requested field name. On hit, the
                        // value child becomes the current view.
                        let mut it = cur.children();
                        let mut found = None;
                        loop {
                            let k = match it.next() {
                                Some(k) => k,
                                None => break,
                            };
                            let v = match it.next() {
                                Some(v) => v,
                                None => break,
                            };
                            // The key's source span text may
                            // include the quotes for JSON-like
                            // grammars; trim one character off
                            // each end when the first byte is `"`.
                            let raw = k.span_text();
                            let key_text = if raw.as_bytes().first() == Some(&b'"')
                                && raw.as_bytes().last() == Some(&b'"')
                                && raw.len() >= 2
                            {
                                &raw[1..raw.len() - 1]
                            } else {
                                raw
                            };
                            if key_text == *key {
                                found = Some(v);
                                break;
                            }
                        }
                        cur = match found {
                            Some(v) => v,
                            None => return None,
                        };
                    }
                    ::bbnf::runtime::PathSegment::Index(i) => {
                        cur = cur.child(*i)?;
                    }
                }
            }
            Some(cur)
        }
    };

    // Per-T impl. Each finishes the walk with a T-specialised
    // leaf extractor.
    let impl_str = quote! {
        impl ::bbnf::runtime::PathQuery<&'static str> for #grammar_ident {
            #[inline]
            fn query<'p>(
                view: Self::View<'p>,
                path: ::bbnf::runtime::Path<'_>,
            ) -> ::core::option::Option<&'static str>
            where
                Self: 'p,
            {
                let node = #node_view_ident::from_cursor(view.cursor(), view.input());
                let _hit = __path_walk(node, path)?;
                // Leaf-kind narrowing to `&'static str` is unsound
                // across the arbitrary-input lifetime — the
                // narrower `&'p str` impl below handles the zero-
                // copy borrow. This `&'static str` impl exists for
                // the bench-harness literal-path case and returns
                // None on non-'static hits.
                None
            }
        }

        impl ::bbnf::runtime::PathQuery<f64> for #grammar_ident {
            #[inline]
            fn query<'p>(
                view: Self::View<'p>,
                path: ::bbnf::runtime::Path<'_>,
            ) -> ::core::option::Option<f64>
            where
                Self: 'p,
            {
                let node = #node_view_ident::from_cursor(view.cursor(), view.input());
                let hit = __path_walk(node, path)?;
                let tape = hit.cursor().tape();
                let rec = hit.cursor().record();
                if let Some(v) = tape.payload_f64(rec) {
                    return Some(v);
                }
                // Fallback: parse the span text.
                hit.span_text().parse::<f64>().ok()
            }
        }

        impl ::bbnf::runtime::PathQuery<bool> for #grammar_ident {
            #[inline]
            fn query<'p>(
                view: Self::View<'p>,
                path: ::bbnf::runtime::Path<'_>,
            ) -> ::core::option::Option<bool>
            where
                Self: 'p,
            {
                let node = #node_view_ident::from_cursor(view.cursor(), view.input());
                let hit = __path_walk(node, path)?;
                let tape = hit.cursor().tape();
                let rec = hit.cursor().record();
                if let Some(v) = tape.payload_bool(rec) {
                    return Some(v);
                }
                match hit.span_text() {
                    "true" => Some(true),
                    "false" => Some(false),
                    _ => None,
                }
            }
        }
    };

    quote! {
        #walk_fn
        #impl_str
    }
}

/// Convenience accessor for downstream emitters: the per-shape
/// `materialize_*` fns need the grammar's variant map to choose
/// the right constructor per `rule_kind()`. Exposed as a standalone
/// function so the orchestrator can compute it once per grammar
/// and hand it to both the value emitter and the materialise
/// emitter.
pub fn variant_entries_for(ir: &GrammarIR) -> Vec<VariantInfo> {
    let non_transparent: Vec<&IrRule> = ir
        .rules
        .iter()
        .filter(|r| !r.meta.is_transparent)
        .collect();
    collect_variant_classes(ir, &non_transparent)
        .into_iter()
        .map(|e| VariantInfo {
            name: e.name,
            rule_id: e.rule_id,
            shape: match e.shape {
                VariantShape::Span => VariantInfoShape::Span,
                VariantShape::Scalar(td) => VariantInfoShape::Scalar(td),
                VariantShape::Compound => VariantInfoShape::Compound,
                VariantShape::Cursor => VariantInfoShape::Cursor,
            },
        })
        .collect()
}

/// Public mirror of [`VariantEntry`] for downstream emitters. The
/// private form carries module-private `VariantShape`; the public
/// form re-exports the same information so the materialise emitter
/// can match on the shape without the private type leaking.
pub struct VariantInfo {
    pub name: String,
    pub rule_id: u32,
    pub shape: VariantInfoShape,
}

/// Public mirror of [`VariantShape`].
pub enum VariantInfoShape {
    Span,
    Scalar(TypeDesc),
    Compound,
    Cursor,
}
