//! AZ-IV.W3.3 — Path-plan emitter (codegen-side).
//!
//! Emits a per-grammar `<GRAMMAR>_PATH_PLAN` static array of
//! `(RuleId, SegmentKind, Decision)` triples that the lazy bail-out
//! parse executor (W3.1) consults to decide whether a sub-rule's body
//! must be `ParseFully`-evaluated, partially evaluated up to a
//! `ParseUntil(child_idx)` cut, or `Skip`-elided when its bytes are
//! provably outside the path's reach.
//!
//! ## Grammar generality
//!
//! Per AZ-IV §Invariants #2 and §Hard Gates 17, this emitter walks
//! [`StructRegistry`] facts only — never matches on rule names, parser
//! idents, or any literal grammar-overfit keying. The plan's shape
//! derives from `LayoutKind` (the projected layout discriminator) and
//! `StructField::source` (the IR provenance tag); both are facts the
//! `project_types` CSP solver computes from grammar data, not from a
//! per-grammar lookup table. Adding a new grammar to the manifest
//! produces a path plan with zero changes to this module.
//!
//! ## Output surface
//!
//! For a grammar with sanitised ident `<GRAMMAR>` (the
//! `parser_struct_ident` after `sanitise_grammar` runs), the emitted
//! module exports:
//!
//! ```text
//! pub mod __path_plan {
//!     #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
//!     pub enum SegmentKind { Field, Index, Wildcard, VariantName }
//!     #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
//!     pub enum Decision { ParseFully, ParseUntil(u32), Skip }
//!     pub struct PathPlanEntry {
//!         pub rule_id: u32,
//!         pub segment_kind: SegmentKind,
//!         pub field_index: u32, // u32::MAX when irrelevant (Wildcard / Skip)
//!         pub decision: Decision,
//!     }
//!     pub static PATH_PLAN: &[PathPlanEntry] = &[ ... ];
//! }
//! ```
//!
//! The runtime executor (W3.1) re-exports these types from a shared
//! `crate::path::path_plan` surface; until W3.1 lands, every grammar's
//! file carries its own type definitions so the static compiles
//! standalone. The cherry-pick after W3.1 lands replaces the local
//! definitions with `pub use crate::path::path_plan::*;` re-exports —
//! the `PATH_PLAN` static reference shape stays byte-identical.
//!
//! ## Determinism
//!
//! [`StructRegistry`] iterates by `RuleId`; per-layout iteration walks
//! `StructLayout::fields` in declaration order. Both orderings are
//! `BTreeMap`-stable / `Vec`-stable, so the same `(GrammarIR, manifest)`
//! pair always produces the same `PATH_PLAN` byte-for-byte.

use bbnf_ir::registry::{FieldSource, LayoutKind, StructField, StructLayout, StructRegistry};
use bbnf_ir::{GrammarIR, RuleId};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};

/// Decision a [`PathCursor`] makes when descending into a rule's
/// generated parse function.
///
/// Mirrors the runtime enum the W3.1 executor consults; emitted as a
/// local definition until the W3.1 cherry-pick replaces the per-file
/// definitions with re-exports from `crate::path::path_plan`.
///
/// `Skip` is part of the plan vocabulary even when the present
/// codegen rules never produce it: the W3.1 executor consumes the
/// full alphabet, and W3.4 negative-fixture tests construct `Skip`
/// rows directly to exercise the lazy-error-elision contract. The
/// `dead_code` allow keeps the variant in the source-of-truth enum.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[allow(dead_code)]
enum Decision {
    /// Parse the rule's full body — the path either lands inside this
    /// rule or descends through one of its children whose subtree the
    /// plan does not yet partition.
    ParseFully,
    /// Parse only the prefix of the rule's body up to and including the
    /// child at the given branch / position index. Sibling children
    /// after the cut may be skipped.
    ParseUntil(u32),
    /// Skip the rule's body entirely — the path's reach proves the
    /// rule's bytes are not visited.
    Skip,
}

/// One emitted plan entry: `(rule_id, segment_kind, field_index,
/// decision)`.
struct PlanRow {
    rule_id: RuleId,
    segment_kind: SegmentKindTag,
    /// Branch / position index when the decision is
    /// [`Decision::ParseUntil`]; `u32::MAX` otherwise.
    field_index: u32,
    decision: Decision,
}

/// Discriminator mirroring [`crate::path::PathSegment`]'s alphabet.
///
/// Emitted alongside the plan; the runtime executor matches on this
/// enum when advancing its cursor against a path segment.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum SegmentKindTag {
    Field,
    Index,
    Wildcard,
    VariantName,
}

/// Build the per-rule plan rows for a single registered layout.
///
/// The mechanism is grammar-general:
///
/// - [`LayoutKind::Struct`] — every [`FieldSource::SeqPosition`] field
///   contributes a `(Field, position) → ParseUntil(position)` row plus
///   an `(Index, position) → ParseUntil(position)` row; every
///   [`FieldSource::RepeatElement`] contributes a `Wildcard →
///   ParseFully` row (the rule's own body iterates).
/// - [`LayoutKind::TaggedEnum`] — every [`FieldSource::BranchTag`]
///   field contributes a `VariantName → ParseUntil(branch_index)` row;
///   the wildcard row is `ParseFully` (the executor falls back when
///   the variant doesn't match).
/// - [`LayoutKind::UntaggedEnum`] — emits a single `Wildcard →
///   ParseFully` row; variant selection collapses at projection time
///   so the plan does not need per-branch entries.
/// - [`LayoutKind::NewtypeWrapper`] — single `Wildcard → ParseFully`
///   row; the wrapper has exactly one inner field.
///
/// Field rows for [`FieldSource::TypedLeaf`] / [`FieldSource::RuleReference`]
/// route under their parent layout's discriminator: typed leaves are
/// the rule's own scalar payload (no further descent), and rule
/// references are followed through the registry by the executor when
/// it walks through the path.
fn rows_for_layout(layout: &StructLayout) -> Vec<PlanRow> {
    let mut rows = Vec::new();

    match layout.kind {
        LayoutKind::Struct => {
            for field in layout.fields() {
                rows.extend(rows_for_struct_field(layout.rule_id, field));
            }
            rows.push(PlanRow {
                rule_id: layout.rule_id,
                segment_kind: SegmentKindTag::Wildcard,
                field_index: u32::MAX,
                decision: Decision::ParseFully,
            });
        }
        LayoutKind::TaggedEnum => {
            for field in layout.fields() {
                rows.extend(rows_for_tagged_branch(layout.rule_id, field));
            }
            rows.push(PlanRow {
                rule_id: layout.rule_id,
                segment_kind: SegmentKindTag::Wildcard,
                field_index: u32::MAX,
                decision: Decision::ParseFully,
            });
        }
        LayoutKind::UntaggedEnum | LayoutKind::NewtypeWrapper => {
            rows.push(PlanRow {
                rule_id: layout.rule_id,
                segment_kind: SegmentKindTag::Wildcard,
                field_index: u32::MAX,
                decision: Decision::ParseFully,
            });
        }
    }

    rows
}

/// Per-field rows for a `Struct` layout. The function consults
/// `StructField::source` to project the field's IR provenance into a
/// path-plan decision; the keying is the provenance tag, not the field
/// name.
fn rows_for_struct_field(rule_id: RuleId, field: &StructField) -> Vec<PlanRow> {
    match field.source {
        FieldSource::SeqPosition { position } => vec![
            PlanRow {
                rule_id,
                segment_kind: SegmentKindTag::Field,
                field_index: position,
                decision: Decision::ParseUntil(position),
            },
            PlanRow {
                rule_id,
                segment_kind: SegmentKindTag::Index,
                field_index: position,
                decision: Decision::ParseUntil(position),
            },
        ],
        FieldSource::RepeatElement => vec![PlanRow {
            rule_id,
            segment_kind: SegmentKindTag::Wildcard,
            field_index: u32::MAX,
            decision: Decision::ParseFully,
        }],
        FieldSource::TypedLeaf | FieldSource::RuleReference { .. } => Vec::new(),
        FieldSource::BranchTag { .. } => Vec::new(),
    }
}

/// Per-branch rows for a `TaggedEnum` layout.
fn rows_for_tagged_branch(rule_id: RuleId, field: &StructField) -> Vec<PlanRow> {
    match field.source {
        FieldSource::BranchTag { branch_index } => vec![PlanRow {
            rule_id,
            segment_kind: SegmentKindTag::VariantName,
            field_index: branch_index,
            decision: Decision::ParseUntil(branch_index),
        }],
        _ => Vec::new(),
    }
}

/// Project the registry into a flat plan-row vector. Iterates layouts
/// in `RuleId` order; within each layout, fields walk declaration
/// order. The same `(grammar, manifest)` pair always yields the same
/// row sequence.
fn collect_plan_rows(registry: &StructRegistry) -> Vec<PlanRow> {
    let mut rows = Vec::new();
    for layout in registry.iter() {
        rows.extend(rows_for_layout(layout));
    }
    rows
}

/// Emit the local `__path_plan` module definition + the `PATH_PLAN`
/// static for the given grammar.
///
/// `grammar_ident_str` is the parser-struct identifier (after
/// `sanitise_grammar` — i.e. the same key the existing emitters use to
/// scope per-grammar statics like `PRECEDENCE_LUT`). The emitted
/// module is private to the per-grammar generated module; the
/// executor reaches it via the parser-ident-scoped path.
pub fn emit_path_plan(grammar_ident_str: &str, ir: &GrammarIR) -> TokenStream {
    let _ = grammar_ident_str;
    let registry = &ir.struct_registry;
    let rows = collect_plan_rows(registry);

    let row_tokens: Vec<TokenStream> = rows.iter().map(emit_row).collect();

    let row_count = row_tokens.len();
    let row_count_lit = Literal::usize_unsuffixed(row_count);

    let plan_mod_ident = format_ident!("__path_plan");

    quote! {
        /// AZ-IV.W3.3 — codegen-emitted lazy-parse path plan.
        ///
        /// The static `PATH_PLAN` carries one row per `(rule, segment
        /// kind)` decision the executor consults. The runtime cursor
        /// linearly searches the static for a matching `(rule_id,
        /// segment_kind)` pair and applies the recorded decision; a
        /// missing match falls back to `ParseFully` at the executor
        /// surface.
        ///
        /// W3.1's executor cherry-pick re-exports the types from
        /// `crate::path::path_plan`; until then the local module
        /// definitions keep this generated file compilable in
        /// isolation per the AZ-IV.W0 regen-discipline contract.
        #[allow(dead_code)]
        pub mod #plan_mod_ident {
            #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
            pub enum SegmentKind {
                Field,
                Index,
                Wildcard,
                VariantName,
            }

            #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
            pub enum Decision {
                ParseFully,
                ParseUntil(u32),
                Skip,
            }

            #[derive(Clone, Copy, Debug)]
            pub struct PathPlanEntry {
                pub rule_id: u32,
                pub segment_kind: SegmentKind,
                /// Branch / position index when the decision is
                /// `ParseUntil`; `u32::MAX` otherwise.
                pub field_index: u32,
                pub decision: Decision,
            }

            pub const PATH_PLAN_LEN: usize = #row_count_lit;
            pub static PATH_PLAN: &[PathPlanEntry; #row_count_lit] = &[
                #(#row_tokens),*
            ];

            /// Linear search the plan for the first `(rule_id,
            /// segment_kind)` match. The W3.1 executor consults this
            /// fn through its cursor; `None` = fall back to
            /// `ParseFully` at the executor surface.
            #[inline]
            pub fn lookup(rule_id: u32, segment_kind: SegmentKind) -> ::core::option::Option<&'static PathPlanEntry> {
                let mut i = 0usize;
                while i < PATH_PLAN.len() {
                    let entry = &PATH_PLAN[i];
                    if entry.rule_id == rule_id && entry.segment_kind as u8 == segment_kind as u8 {
                        return ::core::option::Option::Some(entry);
                    }
                    i += 1;
                }
                ::core::option::Option::None
            }
        }
    }
}

/// Emit one `PathPlanEntry` literal.
fn emit_row(row: &PlanRow) -> TokenStream {
    let rule_id = Literal::u32_unsuffixed(row.rule_id);
    let field_index = Literal::u32_unsuffixed(row.field_index);
    let segment_kind = emit_segment_kind(row.segment_kind);
    let decision = emit_decision(row.decision);
    quote! {
        PathPlanEntry {
            rule_id: #rule_id,
            segment_kind: #segment_kind,
            field_index: #field_index,
            decision: #decision,
        }
    }
}

fn emit_segment_kind(kind: SegmentKindTag) -> TokenStream {
    match kind {
        SegmentKindTag::Field => quote! { SegmentKind::Field },
        SegmentKindTag::Index => quote! { SegmentKind::Index },
        SegmentKindTag::Wildcard => quote! { SegmentKind::Wildcard },
        SegmentKindTag::VariantName => quote! { SegmentKind::VariantName },
    }
}

fn emit_decision(decision: Decision) -> TokenStream {
    match decision {
        Decision::ParseFully => quote! { Decision::ParseFully },
        Decision::ParseUntil(idx) => {
            let lit = Literal::u32_unsuffixed(idx);
            quote! { Decision::ParseUntil(#lit) }
        }
        Decision::Skip => quote! { Decision::Skip },
    }
}
