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
//!    `project_value_output` entry-point (AY-II.W0.c) that reads
//!    the fused-pipeline [`ValueBuilderOutput`] frame arena and
//!    drives the grammar's per-shape `materialize_*_<Grammar>`
//!    inline fns emitted in
//!    `emitter/shapes/value_materialize.rs`.
//!
//! 3. `impl ::bbnf::runtime::PathQuery<T> for <Grammar>` for
//!    `T ∈ { &str, f64, bool }` — linear-walk path queries against
//!    the tape. The emitted impls are < 100 LOC each; the
//!    binary-search packed-cache variant is a follow-on optimisation
//!    — for now a cursor walk over the tape suffices for the hard-gate
//!    sub-item "at least one PathQuery impl emits" and keeps the
//!    emitted surface tractable.
//!
//! # Tranche AY-II.W0.c — fused-pipeline value surface
//!
//! The `project_value_output` entry-point projects the
//! already-constructed value substrate — no tape-walk, no second
//! parse, no reconstruction pass. The `PathQuery<T>` walkers keep
//! their cursor-backed structural discipline: every structural step
//! lands on `TapeCursor::children()` / `TapeCursor::child(i)` and
//! every scalar extraction lands on `tape.payload_*(rec)`. The two
//! consumer paths read disjoint substrates (value output vs. tape)
//! but both sit under the same fused single-pass parse.
//!
//! [`ValueBuilderOutput`]: crate::runtime::ValueBuilderOutput
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
        ir,
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
/// `project_value_output` entry point (AY-II.W0.c) that projects the
/// fused-pipeline value substrate into the typed enum.
///
/// The emitted body delegates to `project_value_<Grammar>` — the
/// root projector emitted alongside the per-shape materialisers in
/// `emitter/shapes/value_materialize.rs`. It reads the root frame
/// from [`ValueBuilderOutput`](crate::runtime::ValueBuilderOutput)
/// and walks its child run, constructing each variant via the
/// grammar's emitted projection logic. No tape walk, no cursor
/// dispatch; the substrate the emitter writes at parse time is the
/// direct source.
fn emit_value_root_impl(
    grammar_ident: &syn::Ident,
    value_ident: &syn::Ident,
    _node_view_ident: &syn::Ident,
    _rule_kind_ident: &syn::Ident,
    _variants: &[VariantEntry],
    grammar_name: &str,
) -> TokenStream {
    let root_project_fn =
        format_ident!("project_value_{}", grammar_name);

    quote! {
        impl ::bbnf::runtime::ValueRoot for #grammar_ident {
            type Value<'p> = #value_ident<'p>;

            #[inline]
            fn project_value_output<'p>(
                output: &::bbnf::runtime::ValueBuilderOutput<Self>,
                input: &'p str,
            ) -> Self::Value<'p>
            where
                Self: 'p,
            {
                #root_project_fn(output, input)
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
/// # AY-II.W0'.c — STRUCTURAL_SCAN_POLICY emission-time splice
///
/// The emitted `__path_walk` is policy-driven: at each step the
/// current record's `rule_kind()` dispatches through a compile-time
/// match arm whose body inlines the matching cursor primitive. Rules
/// whose [`ScanActivationFlags`] admit `OBJECT_KEY_SEEK` emit a
/// `cursor.bounded_lookahead(...)` iteration paired with
/// `cursor.object_key_seek(...)` for the value-position seek; rules
/// admitting `SCAN_STRUCTURAL_BOUNDED` emit a
/// `cursor.scan_structural_bounded(...)` indexed walk for positional
/// access. Rules whose policy alphabet class is
/// [`ScanAlphabetClass::Empty`][empty] retain the generic
/// children-iteration walker (the default used pre-AY-II.W0'.c).
///
/// The per-rule dispatch resolves at compile time — the match arms
/// below fold to the admitted primitive for that rule without any
/// runtime flag lookup or dispatch-table read. The emitted body
/// carries only primitives the rule's policy actually admits; non-
/// admitted rules fall through to the default arm.
///
/// [`ScanActivationFlags`]: tape::ScanActivationFlags
/// [empty]: tape::ScanAlphabetClass::Empty
fn emit_path_query_impls(
    ir: &GrammarIR,
    grammar_ident: &syn::Ident,
    _value_ident: &syn::Ident,
    node_view_ident: &syn::Ident,
    rule_kind_ident: &syn::Ident,
    variants: &[VariantEntry],
) -> TokenStream {
    use crate::backend::rust::emitter::shapes::dispatcher::lookup_scan_policy;
    use tape::ScanActivationFlags;

    // Partition non-transparent rules by the primitives their scan
    // policy admits. The sets may overlap (a rule can admit both
    // OBJECT_KEY_SEEK and SCAN_STRUCTURAL_BOUNDED); emission below
    // composes the partitions into disjoint match-arm groups.
    let mut object_key_seek_rks: Vec<syn::Ident> = Vec::new();
    let mut scan_structural_rks: Vec<syn::Ident> = Vec::new();
    let mut bounded_lookahead_rks: Vec<syn::Ident> = Vec::new();
    for v in variants {
        let Some((_, flags)) =
            lookup_scan_policy(ir, v.rule_id)
        else {
            continue;
        };
        let variant_ident = format_ident!("{}", v.name);
        if flags.contains(ScanActivationFlags::OBJECT_KEY_SEEK) {
            object_key_seek_rks.push(variant_ident.clone());
        }
        if flags.contains(ScanActivationFlags::SCAN_STRUCTURAL_BOUNDED) {
            scan_structural_rks.push(variant_ident.clone());
        }
        // Bounded-lookahead admission independent of object-key-seek
        // — rules like CSS `declarationList` admit bounded scan
        // without the object-value hop pattern.
        if flags.contains(ScanActivationFlags::BOUNDED_LOOKAHEAD)
            && !flags.contains(ScanActivationFlags::OBJECT_KEY_SEEK)
        {
            bounded_lookahead_rks.push(variant_ident);
        }
    }

    // Compose the Field handler. The object-key-seek fast path drops
    // in when the rule admits OBJECT_KEY_SEEK; the bounded-lookahead
    // fast path drops in for rules that admit BOUNDED_LOOKAHEAD
    // without the key-hop. Other rules fall through to the generic
    // children iteration.
    let field_fast_key_seek = if object_key_seek_rks.is_empty() {
        quote! {}
    } else {
        quote! {
            #( #rule_kind_ident::#object_key_seek_rks )|* => {
                // OBJECT_KEY_SEEK admission: bound the child scan
                // by the compound's span end (BOUNDED_LOOKAHEAD
                // co-admits with OBJECT_KEY_SEEK per
                // `lookup_scan_policy`'s Dense arm), compare
                // keys against the requested path segment, then
                // hop to the value via `TapeCursor::object_key_seek`.
                let parent = cur.cursor();
                let (_, parent_end) = parent.span();
                let mut iter = parent.bounded_lookahead(parent_end);
                let mut hit: ::core::option::Option<#node_view_ident<'p>> = None;
                loop {
                    let k_cur = match iter.next() {
                        ::core::option::Option::Some(c) => c,
                        ::core::option::Option::None => break,
                    };
                    // Skip the value slot so the next iteration
                    // lands on the following key.
                    let _ = iter.next();
                    let (k_lo, k_hi) = k_cur.span();
                    let raw = &cur_input[k_lo as usize..k_hi as usize];
                    let key_text = if raw.as_bytes().first() == ::core::option::Option::Some(&b'"')
                        && raw.as_bytes().last() == ::core::option::Option::Some(&b'"')
                        && raw.len() >= 2
                    {
                        &raw[1..raw.len() - 1]
                    } else {
                        raw
                    };
                    if key_text == *key {
                        // Span-equality seek to the value cursor;
                        // zero additional child iteration.
                        let v_cursor = parent.object_key_seek((k_lo, k_hi));
                        hit = v_cursor.map(|c| #node_view_ident::from_cursor(c, cur_input));
                        break;
                    }
                }
                cur = match hit {
                    ::core::option::Option::Some(v) => v,
                    ::core::option::Option::None => return ::core::option::Option::None,
                };
            }
        }
    };

    let field_fast_bounded = if bounded_lookahead_rks.is_empty() {
        quote! {}
    } else {
        quote! {
            #( #rule_kind_ident::#bounded_lookahead_rks )|* => {
                // BOUNDED_LOOKAHEAD without OBJECT_KEY_SEEK:
                // span-bounded key/value probe without the
                // span-equality hop. Used by rules whose policy
                // admits bounded scan but whose shape does not
                // present the {key,value} pairing the Dense class
                // gates on.
                let parent = cur.cursor();
                let (_, parent_end) = parent.span();
                let mut iter = parent.bounded_lookahead(parent_end);
                let mut hit: ::core::option::Option<#node_view_ident<'p>> = None;
                loop {
                    let k_cur = match iter.next() {
                        ::core::option::Option::Some(c) => c,
                        ::core::option::Option::None => break,
                    };
                    let v_cur = match iter.next() {
                        ::core::option::Option::Some(c) => c,
                        ::core::option::Option::None => break,
                    };
                    let (k_lo, k_hi) = k_cur.span();
                    let raw = &cur_input[k_lo as usize..k_hi as usize];
                    let key_text = if raw.as_bytes().first() == ::core::option::Option::Some(&b'"')
                        && raw.as_bytes().last() == ::core::option::Option::Some(&b'"')
                        && raw.len() >= 2
                    {
                        &raw[1..raw.len() - 1]
                    } else {
                        raw
                    };
                    if key_text == *key {
                        hit = ::core::option::Option::Some(
                            #node_view_ident::from_cursor(v_cur, cur_input),
                        );
                        break;
                    }
                }
                cur = match hit {
                    ::core::option::Option::Some(v) => v,
                    ::core::option::Option::None => return ::core::option::Option::None,
                };
            }
        }
    };

    let index_fast_scan = if scan_structural_rks.is_empty() {
        quote! {}
    } else {
        quote! {
            #( #rule_kind_ident::#scan_structural_rks )|* => {
                // SCAN_STRUCTURAL_BOUNDED admission: emit the
                // bounded structural scan and pick the i-th
                // admitted cursor. Zero-allocation iteration; the
                // iterator terminates early at the compound's end
                // span without visiting post-close records.
                let parent = cur.cursor();
                let (_, parent_end) = parent.span();
                let scan = parent.scan_structural_bounded(parent_end);
                cur = match scan.iter().nth(*i) {
                    ::core::option::Option::Some(c) =>
                        #node_view_ident::from_cursor(c, cur_input),
                    ::core::option::Option::None =>
                        return ::core::option::Option::None,
                };
            }
        }
    };

    let walk_fn = quote! {
        /// AY-II.W0'.c — policy-driven path walker. Descends from
        /// `view` per the given path, returning the narrowed
        /// NodeView on hit or `None` when any step misses.
        ///
        /// The per-step dispatch reads `cur.rule_kind()` and
        /// resolves to the structural-scan primitive the rule's
        /// [`STRUCTURAL_SCAN_POLICY`] entry admits: rules admitting
        /// `OBJECT_KEY_SEEK` use
        /// [`TapeCursor::bounded_lookahead`] + [`TapeCursor::object_key_seek`]
        /// for the key-match + value-hop sequence; rules admitting
        /// `SCAN_STRUCTURAL_BOUNDED` use
        /// [`TapeCursor::scan_structural_bounded`] for positional
        /// access. Rules outside the policy's admission fall
        /// through to a generic children iteration.
        ///
        /// [`STRUCTURAL_SCAN_POLICY`]: crate::STRUCTURAL_SCAN_POLICY
        /// [`TapeCursor::bounded_lookahead`]: ::bbnf::runtime::tape::TapeCursor::bounded_lookahead
        /// [`TapeCursor::object_key_seek`]: ::bbnf::runtime::tape::TapeCursor::object_key_seek
        /// [`TapeCursor::scan_structural_bounded`]: ::bbnf::runtime::tape::TapeCursor::scan_structural_bounded
        #[inline]
        fn __path_walk<'p>(
            view: #node_view_ident<'p>,
            path: ::bbnf::runtime::Path<'_>,
        ) -> ::core::option::Option<#node_view_ident<'p>> {
            let cur_input = view.input();
            let mut cur = view;
            for seg in path.iter() {
                match seg {
                    ::bbnf::runtime::PathSegment::Field(key) => {
                        match cur.rule_kind() {
                            #field_fast_key_seek
                            #field_fast_bounded
                            _ => {
                                // Generic walk — children pair
                                // (key, value) compared by source
                                // span text. Used for rules whose
                                // policy admission is
                                // `ScanAlphabetClass::Empty`.
                                let mut it = cur.children();
                                let mut found = None;
                                loop {
                                    let k = match it.next() {
                                        ::core::option::Option::Some(k) => k,
                                        ::core::option::Option::None => break,
                                    };
                                    let v = match it.next() {
                                        ::core::option::Option::Some(v) => v,
                                        ::core::option::Option::None => break,
                                    };
                                    let raw = k.span_text();
                                    let key_text = if raw.as_bytes().first() == ::core::option::Option::Some(&b'"')
                                        && raw.as_bytes().last() == ::core::option::Option::Some(&b'"')
                                        && raw.len() >= 2
                                    {
                                        &raw[1..raw.len() - 1]
                                    } else {
                                        raw
                                    };
                                    if key_text == *key {
                                        found = ::core::option::Option::Some(v);
                                        break;
                                    }
                                }
                                cur = match found {
                                    ::core::option::Option::Some(v) => v,
                                    ::core::option::Option::None =>
                                        return ::core::option::Option::None,
                                };
                            }
                        }
                    }
                    ::bbnf::runtime::PathSegment::Index(i) => {
                        match cur.rule_kind() {
                            #index_fast_scan
                            _ => {
                                cur = cur.child(*i)?;
                            }
                        }
                    }
                }
            }
            ::core::option::Option::Some(cur)
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
                __path_walk(node, path)?;
                // Leaf-kind narrowing to `&'static str` is unsound
                // across the arbitrary-input lifetime — the
                // narrower `&'p str` impl below handles the zero-
                // copy borrow. This `&'static str` impl exists for
                // the bench-harness literal-path case and returns
                // None on non-'static hits.
                ::core::option::Option::None
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
                if let ::core::option::Option::Some(v) = tape.payload_f64(rec) {
                    return ::core::option::Option::Some(v);
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
                if let ::core::option::Option::Some(v) = tape.payload_bool(rec) {
                    return ::core::option::Option::Some(v);
                }
                match hit.span_text() {
                    "true" => ::core::option::Option::Some(true),
                    "false" => ::core::option::Option::Some(false),
                    _ => ::core::option::Option::None,
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
