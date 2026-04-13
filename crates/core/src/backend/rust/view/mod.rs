//! Per-rule tape-view codegen for the Rust backend.
//!
//! Tranche AC.2 landing surface. For every rule in the IR, this
//! module emits a `<Rule>View<'p>` wrapper struct that holds a
//! [`::bbnf::runtime::tape::TapeCursor`] plus a borrow of the
//! original input string. The `input` borrow lets view accessors
//! lazily materialize scalar projections (numeric conversion, hex
//! conversion, constant lookups, etc.) from the source bytes —
//! under AC.2 scalar projection runs at view-read time rather
//! than at parse time.
//!
//! Universal accessors exposed on every generated view:
//!
//! - `.kind()` — the `TapeKind` discriminator of the record.
//! - `.span()` — the source span `(lo, hi)` as a byte range.
//! - `.span_text()` — the source substring for the span.
//! - `.input()` — the full input string borrow.
//! - `.variant_idx()` — rule identity discriminator (`rule.id`).
//!   Always identifies which rule produced the record.
//! - `.meta_idx()` — branch identity for Alt-bodied rules (which
//!   branch was chosen); `0` for non-Alt rules.
//! - `.children()` — iterator of child cursors, for `MustTape`
//!   compound records (Seq/Alt/Repeat/Rule).
//! - `.child(i)` — the i-th direct child, for indexed access.
//! - `.is_recovered()` — true iff the record was pushed from an
//!   `@recover` arm.
//!
//! Typed per-kind accessors (Tranche AI.5) supplement the universals:
//!
//! - **Leaves** (`leaves.rs`): `.text()`, `.as_f64()`, `.as_u32()`,
//!   `.byte_range()` based on the rule's `TypeDesc`.
//! - **Seq** (`seq.rs`): `.child_N()` positional + named accessors
//!   derived from `Ref` targets.
//! - **Alt** (`alt.rs`): `.as_<variant>()`, `.is_<variant>()`,
//!   `.chosen()` discriminated branch accessors.
//! - **Repeat** (`repeat.rs`): `.iter()`, `.len()`, `.is_empty()`,
//!   `.get(i)` typed iteration.
//! - **Grammar** (`grammar.rs`): root-rule name binding on the
//!   grammar marker struct.
//!
//! The top-level grammar-view binding ties the grammar marker
//! struct's [`::bbnf::runtime::Root`] GAT `type View<'p>` to the
//! root rule's view type. Generated `Parsed<Grammar>::view(&self)`
//! calls the `Root::make_view` constructor to lend the root view
//! with a `&self` lifetime.
//!
//! # Ownership of generation
//!
//! - `generate_views` is the public entry point the backend calls
//!   to emit all per-rule view types and the top-level `Root`
//!   binding as one `TokenStream`.
//! - The per-kind sibling files emit typed accessors driven by
//!   the rule's body shape (`IrNode` variant) and `TypeDesc`.
//! - `TransparentElide` rules never get a view type: they are
//!   inlined at every call site during parser emission, so the
//!   view layer never sees them.

use bbnf_ir::{GrammarIR, IrNode, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::rust::ir_types::IrCodegenCtx;

mod alt;
mod grammar;
mod leaves;
mod repeat;
mod seq;

/// Generate the full view-type surface for a grammar.
///
/// Returns a `TokenStream` that, when spliced into the generated
/// parser module, defines:
///
/// - One `<Rule>View<'p>` struct per non-transparent rule,
///   wrapping a `TapeCursor<'p>` + `input: &'p str` with
///   universal accessors.
/// - One `impl ::bbnf::runtime::Root for <Grammar>` binding that
///   ties the grammar marker struct's `type View<'p>` GAT to the
///   root rule's view type.
///
/// Transparent rules are skipped — they are inlined at every
/// call site and never materialize a tape record.
///
/// # Root-rule selection
///
/// The root rule is the first non-transparent rule in the IR's
/// declaration order. BBNF grammars put the entry point first by
/// convention (the bootstrap enforces this), so this matches the
/// user's intent without needing a separate annotation.
pub fn generate_views(ir: &GrammarIR, ctx: &IrCodegenCtx<'_>) -> TokenStream {
    // Pre-compute the RuleKind enum shape + the per-rule
    // variant_idx dispatch table. RuleKind includes both the
    // rule-level variants AND the heterogeneous-alt sub-variants
    // so CST consumers that dispatch on sub-variant coercion (e.g.
    // `term_0`, `term_1`, `value_atom_0`) can match against the
    // same enum the view layer exposes.
    //
    // Numbering convention: rule variants take positions 0..R,
    // sub-variants take positions R..R+S, in the order they appear
    // in `ir.rules` walked for rule-variants then again for
    // sub-variants. The emitted `variant_idx` the rule function's
    // epilogue writes matches the rule position; sub-variant
    // positions are reserved in the enum but emitted at parse time
    // only when the driver explicitly stamps a coercion idx.

    let grammar_name = ctx.ident.to_string();
    let node_view_ident = format_ident!("{}NodeView", grammar_name);
    let rule_kind_ident = format_ident!("{}RuleKind", grammar_name);

    // Non-transparent rules in declaration order (used for emit
    // AND for the RuleKind variant list).
    let non_transparent_rules: Vec<&bbnf_ir::IrRule> = ir
        .rules
        .iter()
        .filter(|r| !r.meta.is_transparent)
        .collect();

    let mut rule_kind_variants: Vec<TokenStream> = Vec::new();
    let mut rule_kind_dispatch: Vec<TokenStream> = Vec::new();

    // Rule-level variants: use `rule.id` as the discriminator —
    // this is the value the emitter's `emit_rule_function_impl`
    // stamps into the compound record's variant_idx via
    // `rule.id as u8`, so the view-side dispatch must match
    // exactly. Rule ids are stable across IR passes because the
    // pipeline preserves them through inlining / reordering.
    for rule in &non_transparent_rules {
        let name = ir.get_string(rule.name);
        let ident = format_ident!("{}", name);
        let idx_lit = (rule.id & 0xFF) as u8;
        rule_kind_variants.push(quote! { #ident });
        rule_kind_dispatch.push(quote! { #idx_lit => #rule_kind_ident::#ident, });
    }
    // Sub-variants from heterogeneous alts. These are written
    // into the tape by the driver's coerce-branch path and take
    // the NEXT available variant_idx values after the rule
    // range (the emitter uses `rule.id` for rules and separate
    // discriminators for sub-variants; the dispatch table here
    // lists them with sequentially-assigned positions above the
    // max rule id).
    let max_rule_id = non_transparent_rules
        .iter()
        .map(|r| r.id as u16)
        .max()
        .unwrap_or(0);
    let mut next_sub_idx: u16 = max_rule_id + 1;
    let mut seen_sub: std::collections::HashSet<String> = Default::default();
    for rule in &ir.rules {
        for sv in &rule.meta.sub_variants {
            let sv_name = ir.get_string(sv.variant_name).to_string();
            if !seen_sub.insert(sv_name.clone()) {
                continue;
            }
            let ident = format_ident!("{}", sv_name);
            let idx_lit = (next_sub_idx & 0xFF) as u8;
            rule_kind_variants.push(quote! { #ident });
            rule_kind_dispatch.push(quote! { #idx_lit => #rule_kind_ident::#ident, });
            next_sub_idx = next_sub_idx.saturating_add(1);
        }
    }

    // Shared per-view accessor block. Used by every per-rule view
    // AND by the generic NodeView. The `children()` / `child(i)`
    // iterators yield `NodeView`s so CST consumers can walk the
    // tape tree polymorphically without knowing per-rule view
    // type names.
    let rule_kind_ident_for_closure = rule_kind_ident.clone();
    let node_view_ident_for_closure = node_view_ident.clone();
    let emit_common_accessors = |view_ident: &syn::Ident| -> TokenStream {
        let rk = &rule_kind_ident_for_closure;
        let nv = &node_view_ident_for_closure;
        let dispatch = &rule_kind_dispatch;
        quote! {
            #[allow(dead_code)]
            impl<'p> #view_ident<'p> {
                #[inline]
                pub fn new(
                    tape: &'p ::bbnf::runtime::tape::Tape,
                    input: &'p str,
                    offset: ::bbnf::runtime::tape::TapeOffset,
                ) -> Self {
                    Self {
                        cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                        input,
                    }
                }

                #[inline]
                pub fn from_cursor(
                    cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
                    input: &'p str,
                ) -> Self {
                    Self { cursor, input }
                }

                #[inline]
                pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
                    self.cursor
                }

                #[inline]
                pub fn input(&self) -> &'p str { self.input }

                #[inline]
                pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
                    self.cursor.kind()
                }

                #[inline]
                pub fn span(&self) -> (u32, u32) { self.cursor.span() }

                #[inline]
                pub fn span_text(&self) -> &'p str {
                    let (lo, hi) = self.cursor.span();
                    &self.input[lo as usize..hi as usize]
                }

                #[inline]
                pub fn variant_idx(&self) -> u8 { self.cursor.variant_idx() }

                /// Dispatch on `variant_idx` to identify which rule
                /// (or sub-variant) produced this record.
                #[inline]
                pub fn rule_kind(&self) -> #rk {
                    match self.variant_idx() {
                        #(#dispatch)*
                        _ => #rk::Unknown,
                    }
                }

                /// Iterator over direct children as `NodeView`s.
                #[inline]
                pub fn children(&self) -> impl ::core::iter::Iterator<Item = #nv<'p>> + 'p {
                    let input = self.input;
                    self.cursor.children().map(move |c| #nv::from_cursor(c, input))
                }

                /// The i-th direct child as a `NodeView`, if present.
                #[inline]
                pub fn child(&self, i: usize) -> ::core::option::Option<#nv<'p>> {
                    self.cursor.child(i).map(|c| #nv::from_cursor(c, self.input))
                }

                #[inline]
                pub fn is_recovered(&self) -> bool {
                    self.cursor.kind().is_recovered()
                }

                /// Source-byte span as a `parse_that::Span<'p>` slice.
                /// Used by CST consumers that historically held
                /// `parse_that::Span` references (`RuleEntry::name_span`,
                /// `ImportedName::span`) alongside the view.
                #[inline]
                pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
                    let (lo, hi) = self.cursor.span();
                    ::parse_that::Span::new(lo as usize, hi as usize, self.input)
                }
            }
        }
    };

    let grammar_name_str = grammar_name.as_str();
    let mut rule_views: Vec<TokenStream> = Vec::new();

    for rule in &non_transparent_rules {
        let name = ir.get_string(rule.name);
        let view_ident = format_ident!("{}View", name);
        let accessors = emit_common_accessors(&view_ident);

        // Per-kind typed accessors driven by the rule's body shape
        // and TypeDesc. These supplement the universal accessor set.
        let typed_accessors = emit_typed_accessors(rule, name, ir, grammar_name_str);

        rule_views.push(quote! {
            /// Generated view over a tape record produced by this rule.
            #[derive(Clone, Copy, Debug)]
            #[allow(non_camel_case_types)]
            pub struct #view_ident<'p> {
                cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
                input: &'p str,
            }

            #accessors
            #typed_accessors
        });
    }

    // Generic NodeView — a single wrapper type that carries any
    // cursor + input borrow. CST consumers (lowering, graph
    // traversal, pipeline directives) use `NodeView` with
    // `rule_kind()` dispatch rather than per-rule view types.
    let node_view_accessors = emit_common_accessors(&node_view_ident);
    let node_view_decl = quote! {
        /// Generic node view over any tape record for this grammar.
        #[derive(Clone, Copy, Debug)]
        #[allow(non_camel_case_types)]
        pub struct #node_view_ident<'p> {
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        }

        /// Rule-identity discriminator for `NodeView::rule_kind`
        /// and per-rule view `rule_kind` accessors. One variant
        /// per non-transparent rule (in declaration order),
        /// followed by one variant per distinct sub-variant name
        /// from heterogeneous alt coercion, plus a fallback
        /// `Unknown` for records the discriminator table does
        /// not cover (leaf spans, alt branch indices, etc.).
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        #[allow(non_camel_case_types)]
        pub enum #rule_kind_ident {
            #(#rule_kind_variants,)*
            /// Fallback for records whose variant_idx is not a
            /// known rule- or sub-variant discriminator.
            Unknown,
        }

        #node_view_accessors
    };

    // Pick the entry-point rule from `ir.entry` (preserved through
    // every IR pass). Fall back to the first non-transparent rule
    // only if entry somehow resolves to a transparent rule (defensive;
    // `ir.entry` is set at lowering time and preserved thereafter).
    let root_rule_name = {
        let entry_rule = ir
            .rules
            .iter()
            .find(|r| r.id == ir.entry && !r.meta.is_transparent);
        if let Some(rule) = entry_rule {
            Some(ir.get_string(rule.name))
        } else {
            ir.rules
                .iter()
                .find(|r| !r.meta.is_transparent)
                .map(|r| ir.get_string(r.name))
        }
    };

    let grammar_ident = ctx.ident;
    let root_binding = if let Some(root_name) = root_rule_name {
        let root_view_ident = format_ident!("{}View", root_name);
        quote! {
            impl ::bbnf::runtime::Root for #grammar_ident {
                type View<'p> = #root_view_ident<'p>;

                #[inline]
                fn make_view<'p>(
                    tape: &'p ::bbnf::runtime::tape::Tape,
                    input: &'p str,
                    root: ::bbnf::runtime::tape::TapeOffset,
                ) -> Self::View<'p> {
                    #root_view_ident::new(tape, input, root)
                }
            }
        }
    } else {
        // Empty grammar (all-transparent) — no binding emitted.
        quote! {}
    };

    // Grammar-level root helpers (rule name constant, etc.).
    let grammar_root_helpers = grammar::emit_grammar_root_helpers(ir, ctx);

    quote! {
        #(#rule_views)*
        #node_view_decl
        #root_binding
        #grammar_root_helpers
    }
}

/// Dispatch to the appropriate per-kind typed accessor generator
/// based on the rule's body shape.
///
/// Peels through outer wrappers (Map, OptionalWhitespace) to find
/// the structurally significant node, then delegates to:
/// - `leaves::emit_leaf_accessors` for Literal, Regex, Epsilon, Ref
/// - `seq::emit_seq_accessors` for Seq
/// - `alt::emit_alt_accessors` for Alt
/// - `repeat::emit_repeat_accessors` for Repeat
///
/// AQ.6.B exception: a rule with an aggregate `PayloadLayout`
/// always lands in `leaves::emit_leaf_accessors`. The rule's tape
/// record is a `push_leaf_with_aggregate` leaf (no children) even
/// though its IR body is structurally a Seq, so the per-field
/// reader belongs on the leaf accessor surface.
fn emit_typed_accessors(
    rule: &bbnf_ir::IrRule,
    rule_name: &str,
    ir: &GrammarIR,
    grammar_name: &str,
) -> TokenStream {
    // Look up the rule's TypeDesc from `ir.types`.
    let type_desc = ir
        .types
        .iter()
        .find_map(|(id, ty)| (*id == rule.id).then_some(ty));

    // AQ.6.B: aggregate-payload rules go straight to the leaf
    // emitter, which generates the typed `.value()` reader from the
    // packed bytes.
    if let Some(layout) = ir.payload_layouts.get(&rule.id) {
        return leaves::emit_aggregate_accessors(rule, rule_name, layout, type_desc);
    }

    // Peel the body through Map/OptionalWhitespace to find the
    // structurally meaningful shape.
    let body = peel_body(&rule.body);

    match body {
        IrNode::Seq(_) => seq::emit_seq_accessors(rule, rule_name, ir, grammar_name),
        IrNode::Alt(_, _) => alt::emit_alt_accessors(rule, rule_name, ir, grammar_name),
        IrNode::Repeat { .. } => {
            repeat::emit_repeat_accessors(rule, rule_name, ir, grammar_name)
        }
        // Leaves: Literal, Regex, Epsilon, Ref, and any body that
        // doesn't match the compound shapes above.
        _ => {
            if let Some(td) = type_desc {
                leaves::emit_leaf_accessors(rule, rule_name, td)
            } else {
                // Default to Span for rules without a TypeDesc entry.
                leaves::emit_leaf_accessors(rule, rule_name, &TypeDesc::Span)
            }
        }
    }
}

/// Peel through Map, OptionalWhitespace, and other transparent
/// wrappers to expose the structurally significant body node.
fn peel_body(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => peel_body(inner),
        other => other,
    }
}
