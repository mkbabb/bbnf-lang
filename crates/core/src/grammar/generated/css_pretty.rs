//! AUTO-GENERATED from `[workspace.metadata.bbnf.grammars]` — do not edit manually.
//! Regenerate: cargo xtask regen --grammar css_pretty

#![allow(
    dead_code,
    unused_variables,
    unused_mut,
    unused_parens,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    clippy::all
)]

use ::parse_that::*;

pub struct CssPrettyParser;
mod __cssprettyparser_emit_impl {
    #![allow(
        dead_code,
        unused_variables,
        unused_mut,
        unused_parens,
        unused_assignments,
        non_camel_case_types,
        non_snake_case,
        non_upper_case_globals,
        clippy::all,
    )]
    use super::*;
    use ::parse_that::*;
    pub const GRAMMAR_CssPrettyParser: [&'static str; 1usize] = [
        include_str!(
            concat!(env!("CARGO_MANIFEST_DIR"), "/../../grammar/css/pretty.bbnf")
        ),
    ];
    static __GRAMMAR_STRUCTURAL_ALPHABET: [u8; 6usize] = [33, 44, 58, 59, 123, 125];
    pub const GRAMMAR_STRUCTURAL_ALPHABET: &[u8] = &__GRAMMAR_STRUCTURAL_ALPHABET;
    pub const GRAMMAR_STRUCTURAL_DIGRAPHS: &[(u8, u8)] = &[];
    pub const GRAMMAR_STRUCTURAL_DIGRAPH_MASK: [u64; 4] = [0, 0, 0, 0];
    pub const GRAMMAR_STRUCTURAL_QUOTE_CLASSES: &[u8] = &[];
    /// Grammar-local Pratt operator metadata.
    ///
    /// The dense LUT carries precedence, associativity, arity, and
    /// the two-byte flag. This sparse slice only carries the data
    /// needed to resolve ambiguous first bytes and stamp the
    /// grammar's operator discriminant.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct PrattEntry {
        pub byte: u8,
        pub second_byte: ::core::option::Option<u8>,
        pub op_discriminant: u8,
    }
    /// AW-III.W6.5 — aggregate dense Pratt precedence LUT.
    ///
    /// Union of every Pratt rule's packed LUT (last-write-wins
    /// per byte). See
    /// `bbnf::backend::rust::emitter::precedence` for the bit
    /// layout.
    pub const PRECEDENCE_LUT: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
    ];
    /// AW-III.W6.5 — aggregate sparse Pratt metadata slice.
    ///
    /// Flat union of every rule's mined operator entries.
    pub const PRECEDENCE_ENTRIES: &[PrattEntry] = &[];
    /// AW-III.W6.5 — total mined operator count for this
    /// grammar. Non-zero iff the lift admitted ≥ 1 chain OR the
    /// shape classifier admitted ≥ 1 single-rung Pratt rule.
    pub const PRECEDENCE_OPERATOR_COUNT: usize = 0usize;
    /// AZ-IV.W3.3 — codegen-emitted lazy-parse path plan.
    ///
    /// The static `PATH_PLAN` carries one row per `(rule, segment
    /// kind)` decision the executor consults. The runtime cursor
    /// linearly searches the static for a matching `(rule_id,
    /// segment_kind)` pair and applies the recorded decision; a
    /// missing match falls back to `ParseFully` at the executor
    /// surface.
    ///
    /// `SegmentKind` and `Decision` re-export from
    /// `crate::path::cursor` — the runtime executor's canonical
    /// alphabet — so the plan rows and the cursor's decision
    /// vocabulary stay byte-identical without duplication.
    #[allow(dead_code)]
    pub mod __path_plan {
        pub use crate::path::cursor::{Decision, SegmentKind};
        #[derive(Clone, Copy, Debug)]
        pub struct PathPlanEntry {
            pub rule_id: u32,
            pub segment_kind: SegmentKind,
            /// Branch / position index when the decision is
            /// `ParseUntil`; `u32::MAX` otherwise.
            pub field_index: u32,
            pub decision: Decision,
        }
        pub const PATH_PLAN_LEN: usize = 82;
        pub static PATH_PLAN: &[PathPlanEntry; 82] = &[
            PathPlanEntry {
                rule_id: 0,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 0,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 0,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 0,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 0,
                segment_kind: SegmentKind::Field,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 0,
                segment_kind: SegmentKind::Index,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 0,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 1,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 1,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 1,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 1,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 1,
                segment_kind: SegmentKind::Field,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 1,
                segment_kind: SegmentKind::Index,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 1,
                segment_kind: SegmentKind::Field,
                field_index: 3,
                decision: Decision::ParseUntil(3),
            },
            PathPlanEntry {
                rule_id: 1,
                segment_kind: SegmentKind::Index,
                field_index: 3,
                decision: Decision::ParseUntil(3),
            },
            PathPlanEntry {
                rule_id: 1,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 2,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 2,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 2,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 2,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 2,
                segment_kind: SegmentKind::Field,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 2,
                segment_kind: SegmentKind::Index,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 2,
                segment_kind: SegmentKind::Field,
                field_index: 3,
                decision: Decision::ParseUntil(3),
            },
            PathPlanEntry {
                rule_id: 2,
                segment_kind: SegmentKind::Index,
                field_index: 3,
                decision: Decision::ParseUntil(3),
            },
            PathPlanEntry {
                rule_id: 2,
                segment_kind: SegmentKind::Field,
                field_index: 4,
                decision: Decision::ParseUntil(4),
            },
            PathPlanEntry {
                rule_id: 2,
                segment_kind: SegmentKind::Index,
                field_index: 4,
                decision: Decision::ParseUntil(4),
            },
            PathPlanEntry {
                rule_id: 2,
                segment_kind: SegmentKind::Field,
                field_index: 5,
                decision: Decision::ParseUntil(5),
            },
            PathPlanEntry {
                rule_id: 2,
                segment_kind: SegmentKind::Index,
                field_index: 5,
                decision: Decision::ParseUntil(5),
            },
            PathPlanEntry {
                rule_id: 2,
                segment_kind: SegmentKind::Field,
                field_index: 6,
                decision: Decision::ParseUntil(6),
            },
            PathPlanEntry {
                rule_id: 2,
                segment_kind: SegmentKind::Index,
                field_index: 6,
                decision: Decision::ParseUntil(6),
            },
            PathPlanEntry {
                rule_id: 2,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 3,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 3,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 3,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 3,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 3,
                segment_kind: SegmentKind::Field,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 3,
                segment_kind: SegmentKind::Index,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 3,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 4,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 4,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 4,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 4,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 4,
                segment_kind: SegmentKind::Field,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 4,
                segment_kind: SegmentKind::Index,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 4,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 5,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 5,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 5,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 5,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 5,
                segment_kind: SegmentKind::Field,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 5,
                segment_kind: SegmentKind::Index,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 5,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 6,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 6,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 6,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 6,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 6,
                segment_kind: SegmentKind::Field,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 6,
                segment_kind: SegmentKind::Index,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 6,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 7,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 7,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 7,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 7,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 7,
                segment_kind: SegmentKind::Field,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 7,
                segment_kind: SegmentKind::Index,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 7,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 8,
                segment_kind: SegmentKind::VariantName,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 8,
                segment_kind: SegmentKind::VariantName,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 8,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 9,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 10,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 10,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 11,
                segment_kind: SegmentKind::VariantName,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 11,
                segment_kind: SegmentKind::VariantName,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 11,
                segment_kind: SegmentKind::VariantName,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 11,
                segment_kind: SegmentKind::VariantName,
                field_index: 3,
                decision: Decision::ParseUntil(3),
            },
            PathPlanEntry {
                rule_id: 11,
                segment_kind: SegmentKind::VariantName,
                field_index: 4,
                decision: Decision::ParseUntil(4),
            },
            PathPlanEntry {
                rule_id: 11,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 12,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 13,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 13,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 14,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
        ];
        /// Linear search the plan for the first `(rule_id,
        /// segment_kind)` match. The W3.1 executor consults this
        /// fn through its cursor; `None` = fall back to
        /// `ParseFully` at the executor surface.
        #[inline]
        pub fn lookup(
            rule_id: u32,
            segment_kind: SegmentKind,
        ) -> ::core::option::Option<&'static PathPlanEntry> {
            let mut i = 0usize;
            while i < PATH_PLAN.len() {
                let entry = &PATH_PLAN[i];
                if entry.rule_id == rule_id
                    && entry.segment_kind as u8 == segment_kind as u8
                {
                    return ::core::option::Option::Some(entry);
                }
                i += 1;
            }
            ::core::option::Option::None
        }
    }
    /// AZ-IV.W5 T4 — codegen-emitted production registry.
    ///
    /// `LazyLock<StructRegistry>` projected from the IR's
    /// `project_types` closure at `cargo xtask regen` time. The
    /// `bbnf-path` proc-macro consumes this static through the
    /// per-marker resolver in `crates/bbnf-path/src/registry.rs`; the
    /// runtime audit pass + emitter consume the same registry shape
    /// from `GrammarIR::struct_registry`.
    ///
    /// `Deref` on `LazyLock` makes the registry's accessor surface
    /// available without explicit deref — `REGISTRY.layout(rule_id)`
    /// reads through the deref impl directly.
    pub static REGISTRY: ::std::sync::LazyLock<::bbnf_ir::registry::StructRegistry> = ::std::sync::LazyLock::new(||
    {
        let mut __registry = ::bbnf_ir::registry::StructRegistry::new();
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 0u32,
                rule_name: ::std::string::String::from("important"),
                kind: ::bbnf_ir::registry::LayoutKind::Struct,
                rule_type: ::bbnf_ir::TypeDesc::Span,
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_0"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 0u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_1"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 1u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_2"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 2u32, }, }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 1u32,
                rule_name: ::std::string::String::from("importRule"),
                kind: ::bbnf_ir::registry::LayoutKind::Struct,
                rule_type: ::bbnf_ir::TypeDesc::Span,
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_0"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 0u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_1"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 1u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_2"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 2u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_3"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 3u32, }, }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 2u32,
                rule_name: ::std::string::String::from("declaration"),
                kind: ::bbnf_ir::registry::LayoutKind::Struct,
                rule_type: ::bbnf_ir::TypeDesc::Tuple(
                    ::std::vec![
                        ::bbnf_ir::TypeDesc::Span, ::bbnf_ir::TypeDesc::Span,
                        ::bbnf_ir::TypeDesc::Span, ::bbnf_ir::TypeDesc::Span,
                        ::bbnf_ir::TypeDesc::Span, ::bbnf_ir::TypeDesc::Span,
                        ::bbnf_ir::TypeDesc::Span
                    ],
                ),
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_0"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 0u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_1"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 1u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_2"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 2u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_3"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 3u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_4"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 4u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_5"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 5u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_6"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 6u32, }, }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 3u32,
                rule_name: ::std::string::String::from("genericAtRule"),
                kind: ::bbnf_ir::registry::LayoutKind::Struct,
                rule_type: ::bbnf_ir::TypeDesc::Tuple(
                    ::std::vec![
                        ::bbnf_ir::TypeDesc::Span, ::bbnf_ir::TypeDesc::BoxedEnum
                    ],
                ),
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_0"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 0u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_1"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 1u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("atRuleBody"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 2u32, }, }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 4u32,
                rule_name: ::std::string::String::from("qualifiedRule"),
                kind: ::bbnf_ir::registry::LayoutKind::Struct,
                rule_type: ::bbnf_ir::TypeDesc::Tuple(
                    ::std::vec![
                        ::bbnf_ir::TypeDesc::Span, ::bbnf_ir::TypeDesc::BoxedEnum
                    ],
                ),
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_0"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 0u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_1"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 1u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("ruleBlock"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 2u32, }, }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 5u32,
                rule_name: ::std::string::String::from("mediaRule"),
                kind: ::bbnf_ir::registry::LayoutKind::Struct,
                rule_type: ::bbnf_ir::TypeDesc::Tuple(
                    ::std::vec![
                        ::bbnf_ir::TypeDesc::Span, ::bbnf_ir::TypeDesc::BoxedEnum
                    ],
                ),
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_0"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 0u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_1"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 1u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("ruleBlock"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 2u32, }, }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 6u32,
                rule_name: ::std::string::String::from("supportsRule"),
                kind: ::bbnf_ir::registry::LayoutKind::Struct,
                rule_type: ::bbnf_ir::TypeDesc::Tuple(
                    ::std::vec![
                        ::bbnf_ir::TypeDesc::Span, ::bbnf_ir::TypeDesc::BoxedEnum
                    ],
                ),
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_0"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 0u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_1"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 1u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("ruleBlock"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 2u32, }, }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 7u32,
                rule_name: ::std::string::String::from("fontFaceRule"),
                kind: ::bbnf_ir::registry::LayoutKind::Struct,
                rule_type: ::bbnf_ir::TypeDesc::Tuple(
                    ::std::vec![
                        ::bbnf_ir::TypeDesc::Span, ::bbnf_ir::TypeDesc::BoxedEnum
                    ],
                ),
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_0"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 0u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_1"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 1u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("ruleBlock"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 2u32, }, }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 8u32,
                rule_name: ::std::string::String::from("atRuleBody"),
                kind: ::bbnf_ir::registry::LayoutKind::TaggedEnum,
                rule_type: ::bbnf_ir::TypeDesc::HeterogeneousAltJoin(
                    ::std::vec![
                        ::bbnf_ir::TypeDesc::Span, ::bbnf_ir::TypeDesc::BoxedEnum
                    ],
                ),
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("43"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::BranchTag { branch_index : 0u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("branch_1"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::BranchTag { branch_index : 1u32, },
                    }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 9u32,
                rule_name: ::std::string::String::from("ruleBlock"),
                kind: ::bbnf_ir::registry::LayoutKind::Struct,
                rule_type: ::bbnf_ir::TypeDesc::BoxedEnum,
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("blockContent"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::RuleReference { target_rule :
                    10u32, }, }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 10u32,
                rule_name: ::std::string::String::from("blockContent"),
                kind: ::bbnf_ir::registry::LayoutKind::Struct,
                rule_type: ::bbnf_ir::TypeDesc::Vec(
                    ::std::boxed::Box::new(::bbnf_ir::TypeDesc::Enum),
                ),
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("element"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::RepeatElement, }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 11u32,
                rule_name: ::std::string::String::from("atRule"),
                kind: ::bbnf_ir::registry::LayoutKind::TaggedEnum,
                rule_type: ::bbnf_ir::TypeDesc::HeterogeneousAltJoin(
                    ::std::vec![
                        ::bbnf_ir::TypeDesc::BoxedEnum, ::bbnf_ir::TypeDesc::Span
                    ],
                ),
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("branch_0"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::BranchTag { branch_index : 0u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("branch_1"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::BranchTag { branch_index : 1u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("branch_2"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::BranchTag { branch_index : 2u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("branch_3"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::BranchTag { branch_index : 3u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("branch_4"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::BranchTag { branch_index : 4u32, },
                    }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 12u32,
                rule_name: ::std::string::String::from("ruleItem"),
                kind: ::bbnf_ir::registry::LayoutKind::UntaggedEnum,
                rule_type: ::bbnf_ir::TypeDesc::BoxedEnum,
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("branch_0"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::BranchTag { branch_index : 0u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("branch_1"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::BranchTag { branch_index : 1u32, },
                    }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 13u32,
                rule_name: ::std::string::String::from("ruleList"),
                kind: ::bbnf_ir::registry::LayoutKind::Struct,
                rule_type: ::bbnf_ir::TypeDesc::Vec(
                    ::std::boxed::Box::new(::bbnf_ir::TypeDesc::Enum),
                ),
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("element"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::RepeatElement, }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 14u32,
                rule_name: ::std::string::String::from("stylesheet"),
                kind: ::bbnf_ir::registry::LayoutKind::Struct,
                rule_type: ::bbnf_ir::TypeDesc::BoxedEnum,
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("ruleList"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::RuleReference { target_rule :
                    13u32, }, }
                ],
            });
        __registry
    });
    /// AZ-IV.W5 T4 — entry-rule name for the per-grammar registry.
    ///
    /// `bbnf-path` resolves the document-root rule through this const;
    /// the value is the first non-transparent rule's name (typically
    /// the start production of the grammar's top-level production).
    pub const REGISTRY_ENTRY_RULE: &str = "stylesheet";
    static __DTA_REGEX_1: &str = "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*";
    static __DTA_REGEX_5: &str = "[^;{}!,]+";
    static __DTA_REGEX_13: &str = "[a-zA-Z_][\\w-]*|--[\\w-]+|-[a-zA-Z][\\w-]*";
    static __DTA_REGEX_32: &str = "@[a-zA-Z][\\w-]*";
    static __DTA_REGEX_33: &str = "[^;{}]*";
    static __DTA_REGEX_36: &str = "[^{};]+";
    static __DTA_REGEX_41: &str = "[^{]+";
    /// AY.W4.3 — per-pattern (LAST-byte-set lo, hi) packed
    /// `CharSet128` tuples. `(0, 0)` means narrowing is
    /// disabled for that pattern (suffix not deterministic).
    ///
    /// The adapter consults this when invoked: if the pattern's
    /// entry is non-zero AND the input slice from `pos` does not
    /// contain any byte in the LAST set, the regex cannot
    /// complete a match — skip the DFA walk entirely.
    #[allow(dead_code)]
    pub(crate) const __REGEX_LAST_BYTE_SET_CssPrettyParser: [(u64, u64); 7] = [
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
    ];
    #[inline]
    #[cold]
    fn __regex_scan_CssPrettyParser(
        pattern: &str,
        input: &[u8],
        pos: usize,
    ) -> ::core::option::Option<u32> {
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_1.as_ptr())
            || pattern == __DTA_REGEX_1
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_CssPrettyParser[0];
                if (__lb_lo | __lb_hi) != 0 {
                    let __scan_end = (pos + 256).min(input.len());
                    let __slice = &input[pos..__scan_end];
                    let mut __found = false;
                    for &__b in __slice {
                        let __test = if __b < 64 {
                            (__lb_lo >> __b) & 1
                        } else if __b < 128 {
                            (__lb_hi >> (__b - 64)) & 1
                        } else {
                            0
                        };
                        if __test != 0 {
                            __found = true;
                            break;
                        }
                    }
                    if !__found && __scan_end == input.len() {
                        return ::core::option::Option::None;
                    }
                }
            }
            return '__dfa: {
                let mut __dfa_state: u32 = 0;
                let mut __dfa_p: usize = pos;
                let mut __dfa_last_match: ::core::option::Option<u32> = ::core::option::Option::Some(
                    pos as u32,
                );
                loop {
                    let b = match input.get(__dfa_p) {
                        ::core::option::Option::Some(&b) => b,
                        ::core::option::Option::None => break,
                    };
                    match __dfa_state {
                        0 => {
                            match b {
                                9 | 10 | 11 | 12 | 13 | 32 => __dfa_state = 0,
                                47 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                42 => __dfa_state = 3,
                                _ => break,
                            }
                        }
                        2 => {
                            match b {
                                9 | 10 | 11 | 12 | 13 | 32 => __dfa_state = 2,
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 14 | 15 | 16 | 17 | 18
                                | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30
                                | 31 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 | 41 | 43 | 44
                                | 45 | 46 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57
                                | 58 | 59 | 60 | 61 | 62 | 63 | 64 | 65 | 66 | 67 | 68 | 69
                                | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77 | 78 | 79 | 80 | 81
                                | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89 | 90 | 91 | 92 | 93
                                | 94 | 95 | 96 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104
                                | 105 | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114
                                | 115 | 116 | 117 | 118 | 119 | 120 | 121 | 122 | 123 | 124
                                | 125 | 126 | 127 | 128 | 129 | 130 | 131 | 132 | 133 | 134
                                | 135 | 136 | 137 | 138 | 139 | 140 | 141 | 142 | 143 | 144
                                | 145 | 146 | 147 | 148 | 149 | 150 | 151 | 152 | 153 | 154
                                | 155 | 156 | 157 | 158 | 159 | 160 | 161 | 162 | 163 | 164
                                | 165 | 166 | 167 | 168 | 169 | 170 | 171 | 172 | 173 | 174
                                | 175 | 176 | 177 | 178 | 179 | 180 | 181 | 182 | 183 | 184
                                | 185 | 186 | 187 | 188 | 189 | 190 | 191 | 192 | 193 | 194
                                | 195 | 196 | 197 | 198 | 199 | 200 | 201 | 202 | 203 | 204
                                | 205 | 206 | 207 | 208 | 209 | 210 | 211 | 212 | 213 | 214
                                | 215 | 216 | 217 | 218 | 219 | 220 | 221 | 222 | 223 | 224
                                | 225 | 226 | 227 | 228 | 229 | 230 | 231 | 232 | 233 | 234
                                | 235 | 236 | 237 | 238 | 239 | 240 | 241 | 242 | 243 | 244
                                | 245 | 246 | 247 | 248 | 249 | 250 | 251 | 252 | 253 | 254
                                | 255 => __dfa_state = 3,
                                42 => __dfa_state = 4,
                                47 => __dfa_state = 6,
                                _ => break,
                            }
                        }
                        3 => {
                            match b {
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50
                                | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62
                                | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74
                                | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86
                                | 87 | 88 | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98
                                | 99 | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108
                                | 109 | 110 | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118
                                | 119 | 120 | 121 | 122 | 123 | 124 | 125 | 126 | 127 | 128
                                | 129 | 130 | 131 | 132 | 133 | 134 | 135 | 136 | 137 | 138
                                | 139 | 140 | 141 | 142 | 143 | 144 | 145 | 146 | 147 | 148
                                | 149 | 150 | 151 | 152 | 153 | 154 | 155 | 156 | 157 | 158
                                | 159 | 160 | 161 | 162 | 163 | 164 | 165 | 166 | 167 | 168
                                | 169 | 170 | 171 | 172 | 173 | 174 | 175 | 176 | 177 | 178
                                | 179 | 180 | 181 | 182 | 183 | 184 | 185 | 186 | 187 | 188
                                | 189 | 190 | 191 | 192 | 193 | 194 | 195 | 196 | 197 | 198
                                | 199 | 200 | 201 | 202 | 203 | 204 | 205 | 206 | 207 | 208
                                | 209 | 210 | 211 | 212 | 213 | 214 | 215 | 216 | 217 | 218
                                | 219 | 220 | 221 | 222 | 223 | 224 | 225 | 226 | 227 | 228
                                | 229 | 230 | 231 | 232 | 233 | 234 | 235 | 236 | 237 | 238
                                | 239 | 240 | 241 | 242 | 243 | 244 | 245 | 246 | 247 | 248
                                | 249 | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 3,
                                42 => __dfa_state = 4,
                                _ => break,
                            }
                        }
                        4 => {
                            match b {
                                47 => __dfa_state = 0,
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 43 | 44 | 45 | 46 | 48 | 49 | 50 | 51
                                | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63
                                | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75
                                | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87
                                | 88 | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98 | 99
                                | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108 | 109
                                | 110 | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118 | 119
                                | 120 | 121 | 122 | 123 | 124 | 125 | 126 | 127 | 128 | 129
                                | 130 | 131 | 132 | 133 | 134 | 135 | 136 | 137 | 138 | 139
                                | 140 | 141 | 142 | 143 | 144 | 145 | 146 | 147 | 148 | 149
                                | 150 | 151 | 152 | 153 | 154 | 155 | 156 | 157 | 158 | 159
                                | 160 | 161 | 162 | 163 | 164 | 165 | 166 | 167 | 168 | 169
                                | 170 | 171 | 172 | 173 | 174 | 175 | 176 | 177 | 178 | 179
                                | 180 | 181 | 182 | 183 | 184 | 185 | 186 | 187 | 188 | 189
                                | 190 | 191 | 192 | 193 | 194 | 195 | 196 | 197 | 198 | 199
                                | 200 | 201 | 202 | 203 | 204 | 205 | 206 | 207 | 208 | 209
                                | 210 | 211 | 212 | 213 | 214 | 215 | 216 | 217 | 218 | 219
                                | 220 | 221 | 222 | 223 | 224 | 225 | 226 | 227 | 228 | 229
                                | 230 | 231 | 232 | 233 | 234 | 235 | 236 | 237 | 238 | 239
                                | 240 | 241 | 242 | 243 | 244 | 245 | 246 | 247 | 248 | 249
                                | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 3,
                                42 => __dfa_state = 5,
                                _ => break,
                            }
                        }
                        5 => {
                            match b {
                                47 => __dfa_state = 2,
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 43 | 44 | 45 | 46 | 48 | 49 | 50 | 51
                                | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63
                                | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75
                                | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87
                                | 88 | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98 | 99
                                | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108 | 109
                                | 110 | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118 | 119
                                | 120 | 121 | 122 | 123 | 124 | 125 | 126 | 127 | 128 | 129
                                | 130 | 131 | 132 | 133 | 134 | 135 | 136 | 137 | 138 | 139
                                | 140 | 141 | 142 | 143 | 144 | 145 | 146 | 147 | 148 | 149
                                | 150 | 151 | 152 | 153 | 154 | 155 | 156 | 157 | 158 | 159
                                | 160 | 161 | 162 | 163 | 164 | 165 | 166 | 167 | 168 | 169
                                | 170 | 171 | 172 | 173 | 174 | 175 | 176 | 177 | 178 | 179
                                | 180 | 181 | 182 | 183 | 184 | 185 | 186 | 187 | 188 | 189
                                | 190 | 191 | 192 | 193 | 194 | 195 | 196 | 197 | 198 | 199
                                | 200 | 201 | 202 | 203 | 204 | 205 | 206 | 207 | 208 | 209
                                | 210 | 211 | 212 | 213 | 214 | 215 | 216 | 217 | 218 | 219
                                | 220 | 221 | 222 | 223 | 224 | 225 | 226 | 227 | 228 | 229
                                | 230 | 231 | 232 | 233 | 234 | 235 | 236 | 237 | 238 | 239
                                | 240 | 241 | 242 | 243 | 244 | 245 | 246 | 247 | 248 | 249
                                | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 3,
                                42 => __dfa_state = 5,
                                _ => break,
                            }
                        }
                        6 => {
                            match b {
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50
                                | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62
                                | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74
                                | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86
                                | 87 | 88 | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98
                                | 99 | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108
                                | 109 | 110 | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118
                                | 119 | 120 | 121 | 122 | 123 | 124 | 125 | 126 | 127 | 128
                                | 129 | 130 | 131 | 132 | 133 | 134 | 135 | 136 | 137 | 138
                                | 139 | 140 | 141 | 142 | 143 | 144 | 145 | 146 | 147 | 148
                                | 149 | 150 | 151 | 152 | 153 | 154 | 155 | 156 | 157 | 158
                                | 159 | 160 | 161 | 162 | 163 | 164 | 165 | 166 | 167 | 168
                                | 169 | 170 | 171 | 172 | 173 | 174 | 175 | 176 | 177 | 178
                                | 179 | 180 | 181 | 182 | 183 | 184 | 185 | 186 | 187 | 188
                                | 189 | 190 | 191 | 192 | 193 | 194 | 195 | 196 | 197 | 198
                                | 199 | 200 | 201 | 202 | 203 | 204 | 205 | 206 | 207 | 208
                                | 209 | 210 | 211 | 212 | 213 | 214 | 215 | 216 | 217 | 218
                                | 219 | 220 | 221 | 222 | 223 | 224 | 225 | 226 | 227 | 228
                                | 229 | 230 | 231 | 232 | 233 | 234 | 235 | 236 | 237 | 238
                                | 239 | 240 | 241 | 242 | 243 | 244 | 245 | 246 | 247 | 248
                                | 249 | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 3,
                                42 => __dfa_state = 5,
                                _ => break,
                            }
                        }
                        _ => unsafe { ::core::hint::unreachable_unchecked() }
                    }
                    __dfa_p += 1;
                    match __dfa_state {
                        0 | 2 => {
                            __dfa_last_match = ::core::option::Option::Some(
                                __dfa_p as u32,
                            );
                        }
                        _ => {}
                    }
                }
                break '__dfa __dfa_last_match.map(|end| end - pos as u32);
            };
        }
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_5.as_ptr())
            || pattern == __DTA_REGEX_5
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_CssPrettyParser[1];
                if (__lb_lo | __lb_hi) != 0 {
                    let __scan_end = (pos + 256).min(input.len());
                    let __slice = &input[pos..__scan_end];
                    let mut __found = false;
                    for &__b in __slice {
                        let __test = if __b < 64 {
                            (__lb_lo >> __b) & 1
                        } else if __b < 128 {
                            (__lb_hi >> (__b - 64)) & 1
                        } else {
                            0
                        };
                        if __test != 0 {
                            __found = true;
                            break;
                        }
                    }
                    if !__found && __scan_end == input.len() {
                        return ::core::option::Option::None;
                    }
                }
            }
            return '__dfa: {
                let mut __dfa_state: u32 = 0;
                let mut __dfa_p: usize = pos;
                let mut __dfa_last_match: ::core::option::Option<u32> = ::core::option::Option::None;
                loop {
                    let b = match input.get(__dfa_p) {
                        ::core::option::Option::Some(&b) => b,
                        ::core::option::Option::None => break,
                    };
                    match __dfa_state {
                        0 => {
                            match b {
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 34 | 35 | 36 | 37 | 38
                                | 39 | 40 | 41 | 42 | 43 | 45 | 46 | 47 | 48 | 49 | 50 | 51
                                | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 60 | 61 | 62 | 63 | 64
                                | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76
                                | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88
                                | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98 | 99 | 100
                                | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108 | 109 | 110
                                | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118 | 119 | 120
                                | 121 | 122 | 124 | 126 | 127 | 128 | 129 | 130 | 131 | 132
                                | 133 | 134 | 135 | 136 | 137 | 138 | 139 | 140 | 141 | 142
                                | 143 | 144 | 145 | 146 | 147 | 148 | 149 | 150 | 151 | 152
                                | 153 | 154 | 155 | 156 | 157 | 158 | 159 | 160 | 161 | 162
                                | 163 | 164 | 165 | 166 | 167 | 168 | 169 | 170 | 171 | 172
                                | 173 | 174 | 175 | 176 | 177 | 178 | 179 | 180 | 181 | 182
                                | 183 | 184 | 185 | 186 | 187 | 188 | 189 | 190 | 191 | 192
                                | 193 | 194 | 195 | 196 | 197 | 198 | 199 | 200 | 201 | 202
                                | 203 | 204 | 205 | 206 | 207 | 208 | 209 | 210 | 211 | 212
                                | 213 | 214 | 215 | 216 | 217 | 218 | 219 | 220 | 221 | 222
                                | 223 | 224 | 225 | 226 | 227 | 228 | 229 | 230 | 231 | 232
                                | 233 | 234 | 235 | 236 | 237 | 238 | 239 | 240 | 241 | 242
                                | 243 | 244 | 245 | 246 | 247 | 248 | 249 | 250 | 251 | 252
                                | 253 | 254 | 255 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 34 | 35 | 36 | 37 | 38
                                | 39 | 40 | 41 | 42 | 43 | 45 | 46 | 47 | 48 | 49 | 50 | 51
                                | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 60 | 61 | 62 | 63 | 64
                                | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76
                                | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88
                                | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98 | 99 | 100
                                | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108 | 109 | 110
                                | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118 | 119 | 120
                                | 121 | 122 | 124 | 126 | 127 | 128 | 129 | 130 | 131 | 132
                                | 133 | 134 | 135 | 136 | 137 | 138 | 139 | 140 | 141 | 142
                                | 143 | 144 | 145 | 146 | 147 | 148 | 149 | 150 | 151 | 152
                                | 153 | 154 | 155 | 156 | 157 | 158 | 159 | 160 | 161 | 162
                                | 163 | 164 | 165 | 166 | 167 | 168 | 169 | 170 | 171 | 172
                                | 173 | 174 | 175 | 176 | 177 | 178 | 179 | 180 | 181 | 182
                                | 183 | 184 | 185 | 186 | 187 | 188 | 189 | 190 | 191 | 192
                                | 193 | 194 | 195 | 196 | 197 | 198 | 199 | 200 | 201 | 202
                                | 203 | 204 | 205 | 206 | 207 | 208 | 209 | 210 | 211 | 212
                                | 213 | 214 | 215 | 216 | 217 | 218 | 219 | 220 | 221 | 222
                                | 223 | 224 | 225 | 226 | 227 | 228 | 229 | 230 | 231 | 232
                                | 233 | 234 | 235 | 236 | 237 | 238 | 239 | 240 | 241 | 242
                                | 243 | 244 | 245 | 246 | 247 | 248 | 249 | 250 | 251 | 252
                                | 253 | 254 | 255 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        _ => unsafe { ::core::hint::unreachable_unchecked() }
                    }
                    __dfa_p += 1;
                    match __dfa_state {
                        1 => {
                            __dfa_last_match = ::core::option::Option::Some(
                                __dfa_p as u32,
                            );
                        }
                        _ => {}
                    }
                }
                break '__dfa __dfa_last_match.map(|end| end - pos as u32);
            };
        }
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_13.as_ptr())
            || pattern == __DTA_REGEX_13
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_CssPrettyParser[2];
                if (__lb_lo | __lb_hi) != 0 {
                    let __scan_end = (pos + 256).min(input.len());
                    let __slice = &input[pos..__scan_end];
                    let mut __found = false;
                    for &__b in __slice {
                        let __test = if __b < 64 {
                            (__lb_lo >> __b) & 1
                        } else if __b < 128 {
                            (__lb_hi >> (__b - 64)) & 1
                        } else {
                            0
                        };
                        if __test != 0 {
                            __found = true;
                            break;
                        }
                    }
                    if !__found && __scan_end == input.len() {
                        return ::core::option::Option::None;
                    }
                }
            }
            return '__dfa: {
                let mut __dfa_state: u32 = 0;
                let mut __dfa_p: usize = pos;
                let mut __dfa_last_match: ::core::option::Option<u32> = ::core::option::Option::None;
                loop {
                    let b = match input.get(__dfa_p) {
                        ::core::option::Option::Some(&b) => b,
                        ::core::option::Option::None => break,
                    };
                    match __dfa_state {
                        0 => {
                            match b {
                                65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76
                                | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88
                                | 89 | 90 | 95 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104
                                | 105 | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114
                                | 115 | 116 | 117 | 118 | 119 | 120 | 121 | 122 => {
                                    __dfa_state = 1;
                                }
                                45 => __dfa_state = 3,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                45 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 65
                                | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77
                                | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89
                                | 90 | 95 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
                                | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115
                                | 116 | 117 | 118 | 119 | 120 | 121 | 122 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        2 => {
                            match b {
                                45 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 65
                                | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77
                                | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89
                                | 90 | 95 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
                                | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115
                                | 116 | 117 | 118 | 119 | 120 | 121 | 122 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        3 => {
                            match b {
                                65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76
                                | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88
                                | 89 | 90 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
                                | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115
                                | 116 | 117 | 118 | 119 | 120 | 121 | 122 => __dfa_state = 1,
                                45 => __dfa_state = 2,
                                _ => break,
                            }
                        }
                        _ => unsafe { ::core::hint::unreachable_unchecked() }
                    }
                    __dfa_p += 1;
                    match __dfa_state {
                        1 => {
                            __dfa_last_match = ::core::option::Option::Some(
                                __dfa_p as u32,
                            );
                        }
                        _ => {}
                    }
                }
                break '__dfa __dfa_last_match.map(|end| end - pos as u32);
            };
        }
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_32.as_ptr())
            || pattern == __DTA_REGEX_32
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_CssPrettyParser[3];
                if (__lb_lo | __lb_hi) != 0 {
                    let __scan_end = (pos + 256).min(input.len());
                    let __slice = &input[pos..__scan_end];
                    let mut __found = false;
                    for &__b in __slice {
                        let __test = if __b < 64 {
                            (__lb_lo >> __b) & 1
                        } else if __b < 128 {
                            (__lb_hi >> (__b - 64)) & 1
                        } else {
                            0
                        };
                        if __test != 0 {
                            __found = true;
                            break;
                        }
                    }
                    if !__found && __scan_end == input.len() {
                        return ::core::option::Option::None;
                    }
                }
            }
            return '__dfa: {
                let mut __dfa_state: u32 = 0;
                let mut __dfa_p: usize = pos;
                let mut __dfa_last_match: ::core::option::Option<u32> = ::core::option::Option::None;
                loop {
                    let b = match input.get(__dfa_p) {
                        ::core::option::Option::Some(&b) => b,
                        ::core::option::Option::None => break,
                    };
                    match __dfa_state {
                        0 => {
                            match b {
                                64 => __dfa_state = 2,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                45 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 65
                                | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77
                                | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89
                                | 90 | 95 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
                                | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115
                                | 116 | 117 | 118 | 119 | 120 | 121 | 122 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        2 => {
                            match b {
                                65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76
                                | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88
                                | 89 | 90 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
                                | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115
                                | 116 | 117 | 118 | 119 | 120 | 121 | 122 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        _ => unsafe { ::core::hint::unreachable_unchecked() }
                    }
                    __dfa_p += 1;
                    match __dfa_state {
                        1 => {
                            __dfa_last_match = ::core::option::Option::Some(
                                __dfa_p as u32,
                            );
                        }
                        _ => {}
                    }
                }
                break '__dfa __dfa_last_match.map(|end| end - pos as u32);
            };
        }
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_33.as_ptr())
            || pattern == __DTA_REGEX_33
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_CssPrettyParser[4];
                if (__lb_lo | __lb_hi) != 0 {
                    let __scan_end = (pos + 256).min(input.len());
                    let __slice = &input[pos..__scan_end];
                    let mut __found = false;
                    for &__b in __slice {
                        let __test = if __b < 64 {
                            (__lb_lo >> __b) & 1
                        } else if __b < 128 {
                            (__lb_hi >> (__b - 64)) & 1
                        } else {
                            0
                        };
                        if __test != 0 {
                            __found = true;
                            break;
                        }
                    }
                    if !__found && __scan_end == input.len() {
                        return ::core::option::Option::None;
                    }
                }
            }
            return '__dfa: {
                let mut __dfa_state: u32 = 0;
                let mut __dfa_p: usize = pos;
                let mut __dfa_last_match: ::core::option::Option<u32> = ::core::option::Option::Some(
                    pos as u32,
                );
                loop {
                    let b = match input.get(__dfa_p) {
                        ::core::option::Option::Some(&b) => b,
                        ::core::option::Option::None => break,
                    };
                    match __dfa_state {
                        0 => {
                            match b {
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49
                                | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 60 | 61 | 62
                                | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74
                                | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86
                                | 87 | 88 | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98
                                | 99 | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108
                                | 109 | 110 | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118
                                | 119 | 120 | 121 | 122 | 124 | 126 | 127 | 128 | 129 | 130
                                | 131 | 132 | 133 | 134 | 135 | 136 | 137 | 138 | 139 | 140
                                | 141 | 142 | 143 | 144 | 145 | 146 | 147 | 148 | 149 | 150
                                | 151 | 152 | 153 | 154 | 155 | 156 | 157 | 158 | 159 | 160
                                | 161 | 162 | 163 | 164 | 165 | 166 | 167 | 168 | 169 | 170
                                | 171 | 172 | 173 | 174 | 175 | 176 | 177 | 178 | 179 | 180
                                | 181 | 182 | 183 | 184 | 185 | 186 | 187 | 188 | 189 | 190
                                | 191 | 192 | 193 | 194 | 195 | 196 | 197 | 198 | 199 | 200
                                | 201 | 202 | 203 | 204 | 205 | 206 | 207 | 208 | 209 | 210
                                | 211 | 212 | 213 | 214 | 215 | 216 | 217 | 218 | 219 | 220
                                | 221 | 222 | 223 | 224 | 225 | 226 | 227 | 228 | 229 | 230
                                | 231 | 232 | 233 | 234 | 235 | 236 | 237 | 238 | 239 | 240
                                | 241 | 242 | 243 | 244 | 245 | 246 | 247 | 248 | 249 | 250
                                | 251 | 252 | 253 | 254 | 255 => __dfa_state = 0,
                                _ => break,
                            }
                        }
                        _ => unsafe { ::core::hint::unreachable_unchecked() }
                    }
                    __dfa_p += 1;
                    match __dfa_state {
                        0 => {
                            __dfa_last_match = ::core::option::Option::Some(
                                __dfa_p as u32,
                            );
                        }
                        _ => {}
                    }
                }
                break '__dfa __dfa_last_match.map(|end| end - pos as u32);
            };
        }
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_36.as_ptr())
            || pattern == __DTA_REGEX_36
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_CssPrettyParser[5];
                if (__lb_lo | __lb_hi) != 0 {
                    let __scan_end = (pos + 256).min(input.len());
                    let __slice = &input[pos..__scan_end];
                    let mut __found = false;
                    for &__b in __slice {
                        let __test = if __b < 64 {
                            (__lb_lo >> __b) & 1
                        } else if __b < 128 {
                            (__lb_hi >> (__b - 64)) & 1
                        } else {
                            0
                        };
                        if __test != 0 {
                            __found = true;
                            break;
                        }
                    }
                    if !__found && __scan_end == input.len() {
                        return ::core::option::Option::None;
                    }
                }
            }
            return '__dfa: {
                let mut __dfa_state: u32 = 0;
                let mut __dfa_p: usize = pos;
                let mut __dfa_last_match: ::core::option::Option<u32> = ::core::option::Option::None;
                loop {
                    let b = match input.get(__dfa_p) {
                        ::core::option::Option::Some(&b) => b,
                        ::core::option::Option::None => break,
                    };
                    match __dfa_state {
                        0 => {
                            match b {
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49
                                | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 60 | 61 | 62
                                | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74
                                | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86
                                | 87 | 88 | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98
                                | 99 | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108
                                | 109 | 110 | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118
                                | 119 | 120 | 121 | 122 | 124 | 126 | 127 | 128 | 129 | 130
                                | 131 | 132 | 133 | 134 | 135 | 136 | 137 | 138 | 139 | 140
                                | 141 | 142 | 143 | 144 | 145 | 146 | 147 | 148 | 149 | 150
                                | 151 | 152 | 153 | 154 | 155 | 156 | 157 | 158 | 159 | 160
                                | 161 | 162 | 163 | 164 | 165 | 166 | 167 | 168 | 169 | 170
                                | 171 | 172 | 173 | 174 | 175 | 176 | 177 | 178 | 179 | 180
                                | 181 | 182 | 183 | 184 | 185 | 186 | 187 | 188 | 189 | 190
                                | 191 | 192 | 193 | 194 | 195 | 196 | 197 | 198 | 199 | 200
                                | 201 | 202 | 203 | 204 | 205 | 206 | 207 | 208 | 209 | 210
                                | 211 | 212 | 213 | 214 | 215 | 216 | 217 | 218 | 219 | 220
                                | 221 | 222 | 223 | 224 | 225 | 226 | 227 | 228 | 229 | 230
                                | 231 | 232 | 233 | 234 | 235 | 236 | 237 | 238 | 239 | 240
                                | 241 | 242 | 243 | 244 | 245 | 246 | 247 | 248 | 249 | 250
                                | 251 | 252 | 253 | 254 | 255 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49
                                | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 60 | 61 | 62
                                | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74
                                | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86
                                | 87 | 88 | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98
                                | 99 | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108
                                | 109 | 110 | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118
                                | 119 | 120 | 121 | 122 | 124 | 126 | 127 | 128 | 129 | 130
                                | 131 | 132 | 133 | 134 | 135 | 136 | 137 | 138 | 139 | 140
                                | 141 | 142 | 143 | 144 | 145 | 146 | 147 | 148 | 149 | 150
                                | 151 | 152 | 153 | 154 | 155 | 156 | 157 | 158 | 159 | 160
                                | 161 | 162 | 163 | 164 | 165 | 166 | 167 | 168 | 169 | 170
                                | 171 | 172 | 173 | 174 | 175 | 176 | 177 | 178 | 179 | 180
                                | 181 | 182 | 183 | 184 | 185 | 186 | 187 | 188 | 189 | 190
                                | 191 | 192 | 193 | 194 | 195 | 196 | 197 | 198 | 199 | 200
                                | 201 | 202 | 203 | 204 | 205 | 206 | 207 | 208 | 209 | 210
                                | 211 | 212 | 213 | 214 | 215 | 216 | 217 | 218 | 219 | 220
                                | 221 | 222 | 223 | 224 | 225 | 226 | 227 | 228 | 229 | 230
                                | 231 | 232 | 233 | 234 | 235 | 236 | 237 | 238 | 239 | 240
                                | 241 | 242 | 243 | 244 | 245 | 246 | 247 | 248 | 249 | 250
                                | 251 | 252 | 253 | 254 | 255 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        _ => unsafe { ::core::hint::unreachable_unchecked() }
                    }
                    __dfa_p += 1;
                    match __dfa_state {
                        1 => {
                            __dfa_last_match = ::core::option::Option::Some(
                                __dfa_p as u32,
                            );
                        }
                        _ => {}
                    }
                }
                break '__dfa __dfa_last_match.map(|end| end - pos as u32);
            };
        }
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_41.as_ptr())
            || pattern == __DTA_REGEX_41
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_CssPrettyParser[6];
                if (__lb_lo | __lb_hi) != 0 {
                    let __scan_end = (pos + 256).min(input.len());
                    let __slice = &input[pos..__scan_end];
                    let mut __found = false;
                    for &__b in __slice {
                        let __test = if __b < 64 {
                            (__lb_lo >> __b) & 1
                        } else if __b < 128 {
                            (__lb_hi >> (__b - 64)) & 1
                        } else {
                            0
                        };
                        if __test != 0 {
                            __found = true;
                            break;
                        }
                    }
                    if !__found && __scan_end == input.len() {
                        return ::core::option::Option::None;
                    }
                }
            }
            return '__dfa: {
                let mut __dfa_state: u32 = 0;
                let mut __dfa_p: usize = pos;
                let mut __dfa_last_match: ::core::option::Option<u32> = ::core::option::Option::None;
                loop {
                    let b = match input.get(__dfa_p) {
                        ::core::option::Option::Some(&b) => b,
                        ::core::option::Option::None => break,
                    };
                    match __dfa_state {
                        0 => {
                            match b {
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49
                                | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61
                                | 62 | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73
                                | 74 | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85
                                | 86 | 87 | 88 | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97
                                | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107
                                | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115 | 116 | 117
                                | 118 | 119 | 120 | 121 | 122 | 124 | 125 | 126 | 127 | 128
                                | 129 | 130 | 131 | 132 | 133 | 134 | 135 | 136 | 137 | 138
                                | 139 | 140 | 141 | 142 | 143 | 144 | 145 | 146 | 147 | 148
                                | 149 | 150 | 151 | 152 | 153 | 154 | 155 | 156 | 157 | 158
                                | 159 | 160 | 161 | 162 | 163 | 164 | 165 | 166 | 167 | 168
                                | 169 | 170 | 171 | 172 | 173 | 174 | 175 | 176 | 177 | 178
                                | 179 | 180 | 181 | 182 | 183 | 184 | 185 | 186 | 187 | 188
                                | 189 | 190 | 191 | 192 | 193 | 194 | 195 | 196 | 197 | 198
                                | 199 | 200 | 201 | 202 | 203 | 204 | 205 | 206 | 207 | 208
                                | 209 | 210 | 211 | 212 | 213 | 214 | 215 | 216 | 217 | 218
                                | 219 | 220 | 221 | 222 | 223 | 224 | 225 | 226 | 227 | 228
                                | 229 | 230 | 231 | 232 | 233 | 234 | 235 | 236 | 237 | 238
                                | 239 | 240 | 241 | 242 | 243 | 244 | 245 | 246 | 247 | 248
                                | 249 | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49
                                | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61
                                | 62 | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73
                                | 74 | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85
                                | 86 | 87 | 88 | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97
                                | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107
                                | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115 | 116 | 117
                                | 118 | 119 | 120 | 121 | 122 | 124 | 125 | 126 | 127 | 128
                                | 129 | 130 | 131 | 132 | 133 | 134 | 135 | 136 | 137 | 138
                                | 139 | 140 | 141 | 142 | 143 | 144 | 145 | 146 | 147 | 148
                                | 149 | 150 | 151 | 152 | 153 | 154 | 155 | 156 | 157 | 158
                                | 159 | 160 | 161 | 162 | 163 | 164 | 165 | 166 | 167 | 168
                                | 169 | 170 | 171 | 172 | 173 | 174 | 175 | 176 | 177 | 178
                                | 179 | 180 | 181 | 182 | 183 | 184 | 185 | 186 | 187 | 188
                                | 189 | 190 | 191 | 192 | 193 | 194 | 195 | 196 | 197 | 198
                                | 199 | 200 | 201 | 202 | 203 | 204 | 205 | 206 | 207 | 208
                                | 209 | 210 | 211 | 212 | 213 | 214 | 215 | 216 | 217 | 218
                                | 219 | 220 | 221 | 222 | 223 | 224 | 225 | 226 | 227 | 228
                                | 229 | 230 | 231 | 232 | 233 | 234 | 235 | 236 | 237 | 238
                                | 239 | 240 | 241 | 242 | 243 | 244 | 245 | 246 | 247 | 248
                                | 249 | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        _ => unsafe { ::core::hint::unreachable_unchecked() }
                    }
                    __dfa_p += 1;
                    match __dfa_state {
                        1 => {
                            __dfa_last_match = ::core::option::Option::Some(
                                __dfa_p as u32,
                            );
                        }
                        _ => {}
                    }
                }
                break '__dfa __dfa_last_match.map(|end| end - pos as u32);
            };
        }
        ::core::option::Option::None
    }
    /// AW-V.W3.2 — per-grammar shape-dispatch support.
    ///
    /// Inlined by every `parse_<shape>_<grammar>_<rule>` emitted
    /// sibling; carries the SIMD whitespace bitmap cache + the
    /// quoted-string scanner primitive. The module is private to
    /// the generated code — downstream consumers route through the
    /// top-level `parse_<grammar>_<root>` which inlines every
    /// helper under workspace LTO.
    #[allow(dead_code, non_snake_case)]
    pub(crate) mod __shape_support_CssPrettyParser {
        /// Per-parse SIMD scratch — 64-byte whitespace-bitmap
        /// cache mirroring `json-prototype::simd::ScanState`.
        ///
        /// AY.W4.3 — for grammars whose `structural_alphabet` is
        /// non-empty, ScanState additionally carries a lazy
        /// `OnceCell<StructuralIndex>` consumed by CTNS-style
        /// probes. Lazy-init keeps the O(N) scan cost amortised
        /// rather than paid eagerly at parse entry — see
        /// `AYW1-twitter-regression-diag` for the eager-init
        /// regression that motivates the OnceCell discipline.
        #[derive(Debug, Default)]
        pub struct ScanState {
            pub(crate) nospace_bits: u64,
            pub(crate) nospace_start: isize,
            /// AY.W4.3 — lazy structural-byte index. Populated on
            /// first consumer query via `ensure_structural_index`;
            /// `OnceCell` discipline keeps the O(N) scan cost
            /// amortised across the parse rather than paid eagerly
            /// at parse-entry (AY.W1-fix demonstrated eager scans
            /// regress JSON twitter -64%).
            pub(crate) structural_index: ::core::cell::OnceCell<
                ::simd_scan::StructuralIndex,
            >,
        }
        impl ScanState {
            #[inline]
            pub fn new() -> Self {
                Self {
                    nospace_bits: 0,
                    nospace_start: -1,
                    structural_index: ::core::cell::OnceCell::new(),
                }
            }
        }
        /// AY.W4.3 — lazy-init the per-parse structural index
        /// against the grammar's mined structural alphabet.
        /// Idempotent; consumers may call freely.
        #[inline]
        pub(crate) fn ensure_structural_index<'a>(
            state: &'a mut ScanState,
            input: &[u8],
        ) -> &'a ::simd_scan::StructuralIndex {
            state
                .structural_index
                .get_or_init(|| {
                    let alphabet = ::simd_scan::StructuralAlphabet {
                        singletons: super::GRAMMAR_STRUCTURAL_ALPHABET,
                        digraph_mask: super::GRAMMAR_STRUCTURAL_DIGRAPH_MASK,
                        digraph_pairs: super::GRAMMAR_STRUCTURAL_DIGRAPHS,
                        quote_classes: super::GRAMMAR_STRUCTURAL_QUOTE_CLASSES,
                    };
                    ::simd_scan::scan_structural(input, &alphabet)
                })
        }
        /// Skip whitespace AND `/* ... */` block comments at `*p`,
        /// returning the first non-whitespace, non-comment byte
        /// (or `None` on EOF). Hot-path fast-exit when the next
        /// byte is neither whitespace nor `/`.
        ///
        /// When the next byte is `/`: if followed by `*`, enter
        /// the slow path to consume the comment body; otherwise
        /// return `Some(b'/')` — a bare `/` is a semantic byte.
        #[inline(always)]
        pub fn skip_space(
            input: &[u8],
            p: &mut usize,
            state: &mut ScanState,
        ) -> Option<u8> {
            match input.get(*p) {
                Some(
                    &b,
                ) if b != b' ' && b != b'\t' && b != b'\n' && b != b'\r' && b != b'/' => {
                    Some(b)
                }
                Some(&b'/') if input.get(*p + 1) != Some(&b'*') => Some(b'/'),
                None => None,
                _ => {
                    skip_space_slow(input, p, state);
                    input.get(*p).copied()
                }
            }
        }
        /// Advance `*p` past ASCII whitespace AND `/* ... */` block
        /// comments. The bitmap cache accelerates pure-whitespace
        /// runs; comment detection runs on every iteration where
        /// `*p` points at `/`.
        ///
        /// AY.W4.3 — opens with a CTNS-style structural-index probe
        /// when the grammar mines a non-empty alphabet. On long
        /// whitespace runs that don't intersect comment openers,
        /// the probe jumps directly to the next structural byte
        /// instead of iterating SIMD stripes.
        #[inline(always)]
        pub(crate) fn skip_space_slow(
            input: &[u8],
            p: &mut usize,
            state: &mut ScanState,
        ) {
            loop {
                let cache_base = state.nospace_start;
                if cache_base >= 0 && (*p as isize) >= cache_base {
                    let rel = *p - cache_base as usize;
                    if rel < 64 {
                        let masked = state.nospace_bits & (!0u64 << rel);
                        if masked != 0 {
                            let bit = masked.trailing_zeros() as usize;
                            *p = cache_base as usize + bit;
                        } else {
                            *p = cache_base as usize + 64;
                            continue;
                        }
                    }
                }
                if *p >= input.len() {
                    return;
                }
                if *p + 64 > input.len() {
                    while let Some(&b) = input.get(*p) {
                        if b != b' ' && b != b'\t' && b != b'\n' && b != b'\r' {
                            break;
                        }
                        *p += 1;
                    }
                } else {
                    let stripe = unsafe {
                        ::core::slice::from_raw_parts(input.as_ptr().add(*p), 64)
                    };
                    let mask = nospace_bitmap_64(stripe);
                    state.nospace_bits = mask;
                    state.nospace_start = *p as isize;
                    if mask != 0 {
                        let bit = mask.trailing_zeros() as usize;
                        *p += bit;
                    } else {
                        *p += 64;
                        continue;
                    }
                }
                if input.get(*p) == Some(&b'/') && input.get(*p + 1) == Some(&b'*') {
                    *p += 2;
                    let len = input.len();
                    loop {
                        if *p + 1 >= len {
                            *p = len;
                            state.nospace_start = -1;
                            return;
                        }
                        let slice = unsafe { input.get_unchecked(*p..len) };
                        match slice.iter().position(|&b| b == b'*') {
                            None => {
                                *p = len;
                                state.nospace_start = -1;
                                return;
                            }
                            Some(rel) => {
                                *p += rel + 1;
                                if input.get(*p) == Some(&b'/') {
                                    *p += 1;
                                    break;
                                }
                            }
                        }
                    }
                    state.nospace_start = -1;
                    continue;
                }
                return;
            }
        }
        /// Compute the 64-bit "non-whitespace" bitmap for a 64-byte
        /// stripe. Bit `i` is `1` iff `stripe[i]` is NOT in
        /// `{b' ', b'\t', b'\n', b'\r'}`.
        #[inline(always)]
        pub(crate) fn nospace_bitmap_64(stripe: &[u8]) -> u64 {
            #[cfg(target_arch = "aarch64")]
            unsafe {
                return nospace_bitmap_64_neon(stripe);
            }
            #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
            unsafe {
                return nospace_bitmap_64_avx2(stripe);
            }
            #[allow(unreachable_code)] nospace_bitmap_64_scalar(stripe)
        }
        #[cfg(target_arch = "aarch64")]
        #[inline(always)]
        unsafe fn nospace_bitmap_64_neon(stripe: &[u8]) -> u64 {
            use core::arch::aarch64::*;
            unsafe {
                let ptr = stripe.as_ptr();
                let space = vdupq_n_u8(b' ');
                let tab = vdupq_n_u8(b'\t');
                let nl = vdupq_n_u8(b'\n');
                let cr = vdupq_n_u8(b'\r');
                let bits_lo: [u8; 16] = [
                    1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128,
                ];
                let bit_vec = vld1q_u8(bits_lo.as_ptr());
                let m0 = chunk_ns_mask16(ptr, 0, space, tab, nl, cr, bit_vec);
                let m1 = chunk_ns_mask16(ptr, 16, space, tab, nl, cr, bit_vec);
                let m2 = chunk_ns_mask16(ptr, 32, space, tab, nl, cr, bit_vec);
                let m3 = chunk_ns_mask16(ptr, 48, space, tab, nl, cr, bit_vec);
                (m0 as u64) | ((m1 as u64) << 16) | ((m2 as u64) << 32)
                    | ((m3 as u64) << 48)
            }
        }
        #[cfg(target_arch = "aarch64")]
        #[inline(always)]
        unsafe fn chunk_ns_mask16(
            ptr: *const u8,
            off: usize,
            space: core::arch::aarch64::uint8x16_t,
            tab: core::arch::aarch64::uint8x16_t,
            nl: core::arch::aarch64::uint8x16_t,
            cr: core::arch::aarch64::uint8x16_t,
            bit_vec: core::arch::aarch64::uint8x16_t,
        ) -> u16 {
            use core::arch::aarch64::*;
            unsafe {
                let chunk = vld1q_u8(ptr.add(off));
                let ws = vorrq_u8(
                    vorrq_u8(vceqq_u8(chunk, space), vceqq_u8(chunk, tab)),
                    vorrq_u8(vceqq_u8(chunk, nl), vceqq_u8(chunk, cr)),
                );
                let ns = vmvnq_u8(ws);
                let weighted = vandq_u8(ns, bit_vec);
                let low = vaddv_u8(vget_low_u8(weighted)) as u16;
                let high = vaddv_u8(vget_high_u8(weighted)) as u16;
                low | (high << 8)
            }
        }
        #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
        #[inline(always)]
        unsafe fn nospace_bitmap_64_avx2(stripe: &[u8]) -> u64 {
            use core::arch::x86_64::*;
            unsafe {
                let ptr = stripe.as_ptr();
                let space = _mm256_set1_epi8(b' ' as i8);
                let tab = _mm256_set1_epi8(b'\t' as i8);
                let nl = _mm256_set1_epi8(b'\n' as i8);
                let cr = _mm256_set1_epi8(b'\r' as i8);
                let mut out = 0u64;
                for i in 0..2 {
                    let v = _mm256_loadu_si256(ptr.add(i * 32) as *const __m256i);
                    let ws = _mm256_or_si256(
                        _mm256_or_si256(
                            _mm256_cmpeq_epi8(v, space),
                            _mm256_cmpeq_epi8(v, tab),
                        ),
                        _mm256_or_si256(
                            _mm256_cmpeq_epi8(v, nl),
                            _mm256_cmpeq_epi8(v, cr),
                        ),
                    );
                    let ws_mask = _mm256_movemask_epi8(ws) as u32;
                    let ns_mask = !ws_mask as u64;
                    out |= (ns_mask & 0xFFFF_FFFF) << (i * 32);
                }
                out
            }
        }
        #[inline(always)]
        pub(crate) fn nospace_bitmap_64_scalar(stripe: &[u8]) -> u64 {
            let mut out = 0u64;
            for (i, &b) in stripe.iter().enumerate() {
                if b != b' ' && b != b'\t' && b != b'\n' && b != b'\r' {
                    out |= 1u64 << i;
                }
            }
            out
        }
        /// Find the first `b'"'` or `b'\\'` byte in `bytes`.
        /// Mirrors `json-prototype::simd::first_quote_or_backslash`.
        #[inline(always)]
        pub fn first_quote_or_backslash(bytes: &[u8]) -> Option<(usize, u8)> {
            #[cfg(target_arch = "aarch64")]
            unsafe {
                return first_quote_or_backslash_neon(bytes);
            }
            #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
            unsafe {
                return first_quote_or_backslash_avx2(bytes);
            }
            #[allow(unreachable_code)] first_quote_or_backslash_scalar(bytes)
        }
        #[cfg(target_arch = "aarch64")]
        #[inline(always)]
        unsafe fn first_quote_or_backslash_neon(bytes: &[u8]) -> Option<(usize, u8)> {
            use core::arch::aarch64::*;
            unsafe {
                let quote = vdupq_n_u8(b'"');
                let backslash = vdupq_n_u8(b'\\');
                let ptr = bytes.as_ptr();
                let len = bytes.len();
                let mut i = 0usize;
                while i + 16 <= len {
                    let v = vld1q_u8(ptr.add(i));
                    let hit = vorrq_u8(vceqq_u8(v, quote), vceqq_u8(v, backslash));
                    let packed = vshrn_n_u16::<4>(vreinterpretq_u16_u8(hit));
                    let bits = vget_lane_u64::<0>(vreinterpret_u64_u8(packed));
                    if bits != 0 {
                        let off = (bits.trailing_zeros() >> 2) as usize;
                        let byte = *ptr.add(i + off);
                        return Some((i + off, byte));
                    }
                    i += 16;
                }
                while i < len {
                    let b = *ptr.add(i);
                    if b == b'"' || b == b'\\' {
                        return Some((i, b));
                    }
                    i += 1;
                }
                None
            }
        }
        #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
        #[inline(always)]
        unsafe fn first_quote_or_backslash_avx2(bytes: &[u8]) -> Option<(usize, u8)> {
            use core::arch::x86_64::*;
            unsafe {
                let quote = _mm256_set1_epi8(b'"' as i8);
                let backslash = _mm256_set1_epi8(b'\\' as i8);
                let ptr = bytes.as_ptr();
                let len = bytes.len();
                let mut i = 0usize;
                while i + 32 <= len {
                    let v = _mm256_loadu_si256(ptr.add(i) as *const __m256i);
                    let hit = _mm256_or_si256(
                        _mm256_cmpeq_epi8(v, quote),
                        _mm256_cmpeq_epi8(v, backslash),
                    );
                    let mask = _mm256_movemask_epi8(hit) as u32;
                    if mask != 0 {
                        let off = mask.trailing_zeros() as usize;
                        return Some((i + off, *ptr.add(i + off)));
                    }
                    i += 32;
                }
                while i < len {
                    let b = *ptr.add(i);
                    if b == b'"' || b == b'\\' {
                        return Some((i, b));
                    }
                    i += 1;
                }
                None
            }
        }
        #[inline(always)]
        pub(crate) fn first_quote_or_backslash_scalar(
            bytes: &[u8],
        ) -> Option<(usize, u8)> {
            for (i, &b) in bytes.iter().enumerate() {
                if b == b'"' || b == b'\\' {
                    return Some((i, b));
                }
            }
            None
        }
        /// Map a byte into one of six arms: object `{` → 0,
        /// array `[` → 1, string `"` → 2, digit/`-` (number) → 3,
        /// keyword-led `t` / `f` / `n` → 4, else → 5.
        ///
        /// The emitter's dispatcher inlines this to compile-time
        /// byte matches; kept here as a reference helper for tests.
        #[inline(always)]
        pub(crate) fn shape_byte_arm(b: u8) -> u8 {
            match b {
                b'{' => 0,
                b'[' => 1,
                b'"' => 2,
                b'-' | b'0'..=b'9' => 3,
                b't' | b'f' | b'n' => 4,
                _ => 5,
            }
        }
        /// Expect an exact keyword sequence at `*p` and advance
        /// past it on match.
        #[inline(always)]
        pub fn expect_keyword(input: &[u8], p: &mut usize, word: &[u8]) -> bool {
            let at = *p;
            let end = at + word.len();
            if input.len() < end || &input[at..end] != word {
                return false;
            }
            *p = end;
            true
        }
        /// AZ-IV.W3-DYNAMIC — byte-balanced value skip for the
        /// lazy bail-out parser's mismatched-key fast path.
        ///
        /// Advances `*p` past one structural value (object,
        /// array, string, number, true / false / null,
        /// identifier-shaped scalar) without producing any
        /// builder push. The scan is a forward state machine:
        ///
        /// - `{` / `[` — track open/close depth (treating bytes
        ///   inside `"…"` strings as opaque) and stop at depth
        ///   zero with the matching close.
        /// - `"` — scan to the next unescaped `"`.
        /// - everything else — read until the next structural
        ///   delimiter (`,` `}` `]` whitespace).
        ///
        /// Returns `Err` only on premature EOF inside an
        /// unterminated string or compound; the lazy-error-
        /// elision contract ensures the caller never propagates
        /// that error.
        #[inline]
        pub fn byte_skip_value(
            input: &[u8],
            p: &mut usize,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            let start = *p;
            let first = match input.get(start).copied() {
                Some(b) => b,
                None => {
                    return Err(crate::runtime::DtaError::UnexpectedEnd {
                        offset: start as u32,
                    });
                }
            };
            match first {
                b'{' | b'[' => byte_skip_balanced(input, p),
                b'"' => byte_skip_string(input, p),
                _ => byte_skip_scalar(input, p),
            }
        }
        /// AZ-IV.W3-DYNAMIC — balanced-compound skip. Honours
        /// `"` strings (with `\"` escapes) so `}` / `]` bytes
        /// inside string literals do not falsely close.
        #[inline]
        fn byte_skip_balanced(
            input: &[u8],
            p: &mut usize,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            let start = *p;
            let mut depth: u32 = 0;
            let mut i = start;
            while let Some(&b) = input.get(i) {
                match b {
                    b'{' | b'[' => depth = depth.saturating_add(1),
                    b'}' | b']' => {
                        if depth <= 1 {
                            *p = i + 1;
                            return Ok(());
                        }
                        depth -= 1;
                    }
                    b'"' => {
                        i += 1;
                        while let Some(&sb) = input.get(i) {
                            if sb == b'\\' {
                                i += 2;
                                continue;
                            }
                            if sb == b'"' {
                                break;
                            }
                            i += 1;
                        }
                        if input.get(i).is_none() {
                            return Err(crate::runtime::DtaError::UnexpectedEnd {
                                offset: start as u32,
                            });
                        }
                    }
                    _ => {}
                }
                i += 1;
            }
            Err(crate::runtime::DtaError::UnexpectedEnd {
                offset: start as u32,
            })
        }
        /// AZ-IV.W3-DYNAMIC — quoted-string skip. Advances past
        /// the closing `"` honouring `\"` and `\\` escapes.
        #[inline]
        fn byte_skip_string(
            input: &[u8],
            p: &mut usize,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            let start = *p;
            let mut i = start + 1;
            while let Some(&b) = input.get(i) {
                if b == b'\\' {
                    i += 2;
                    continue;
                }
                if b == b'"' {
                    *p = i + 1;
                    return Ok(());
                }
                i += 1;
            }
            Err(crate::runtime::DtaError::UnexpectedEnd {
                offset: start as u32,
            })
        }
        /// AZ-IV.W3-DYNAMIC — scalar skip. Advances past
        /// non-structural bytes until a delimiter (`,` `}` `]`
        /// whitespace) or EOF.
        #[inline]
        fn byte_skip_scalar(
            input: &[u8],
            p: &mut usize,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            let mut i = *p;
            while let Some(&b) = input.get(i) {
                match b {
                    b',' | b'}' | b']' | b' ' | b'\t' | b'\n' | b'\r' => break,
                    _ => i += 1,
                }
            }
            *p = i;
            Ok(())
        }
    }
    /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / CSS L4 / Sheets per the
    /// resolver's `SubstrateBinding`).
    ///
    /// Compound emission lands as typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    ///
    /// AZ-III.W2.4.r — content-only bodies (no Ref /
    /// TokenDispatch in the IR) capture `*p` before and after
    /// the per-position emission and push one synthetic Span
    /// leaf carrying the consumed source slice; this restores
    /// the contract `bootstrap_parser` met for `regex` /
    /// `literal` / `comment` / `big_comment` / `import_path`
    /// (all flat-shape rules whose grammar projection is
    /// `-> Span` or whose host walker reads via `byte_span()`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_CssPrettyParser_important<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::css_pretty::CssPrettyStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(0u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __span_lo: usize = *p;
        let __compound_start: u32 = *p as u32;
        static __IMPORTANT_LAYOUT: ::std::sync::LazyLock<
            ::bbnf_ir::registry::StructLayout,
        > = ::std::sync::LazyLock::new(|| ::bbnf_ir::registry::StructLayout {
            rule_id: 0u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("important"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        });
        let __important_handle = <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &*__IMPORTANT_LAYOUT,
        );
        <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [33u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
            }
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                let at = *p;
                let end = at + 9usize;
                if input.len() < end
                    || input[at..end]
                        != [105u8, 109u8, 112u8, 111u8, 114u8, 116u8, 97u8, 110u8, 116u8]
                {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                let __span_hi: usize = *p;
                let __span_slice: &str = ::core::str::from_utf8(
                        &input[__span_lo..__span_hi],
                    )
                    .unwrap_or("");
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_leaf_with_str(
                    builder,
                    __span_slice,
                );
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __important_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / CSS L4 / Sheets per the
    /// resolver's `SubstrateBinding`).
    ///
    /// Compound emission lands as typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    ///
    /// AZ-III.W2.4.r — content-only bodies (no Ref /
    /// TokenDispatch in the IR) capture `*p` before and after
    /// the per-position emission and push one synthetic Span
    /// leaf carrying the consumed source slice; this restores
    /// the contract `bootstrap_parser` met for `regex` /
    /// `literal` / `comment` / `big_comment` / `import_path`
    /// (all flat-shape rules whose grammar projection is
    /// `-> Span` or whose host walker reads via `byte_span()`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_CssPrettyParser_importRule<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::css_pretty::CssPrettyStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(1u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __span_lo: usize = *p;
        let __compound_start: u32 = *p as u32;
        static __IMPORTRULE_LAYOUT: ::std::sync::LazyLock<
            ::bbnf_ir::registry::StructLayout,
        > = ::std::sync::LazyLock::new(|| ::bbnf_ir::registry::StructLayout {
            rule_id: 1u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("importRule"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        });
        let __importRule_handle = <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &*__IMPORTRULE_LAYOUT,
        );
        <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let at = *p;
                let end = at + 7usize;
                if input.len() < end
                    || input[at..end] != [64u8, 105u8, 109u8, 112u8, 111u8, 114u8, 116u8]
                {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
            }
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "[^;{}!,]+",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                {
                    let mut __iter_count: u32 = 0;
                    loop {
                        if __iter_count >= 4294967295u32 {
                            break;
                        }
                        let __iter_save_p = *p;
                        if input.get(*p).is_none() {
                            break;
                        }
                        let __iter_builder_checkpoint = builder.checkpoint();
                        let __iter_result: ::core::result::Result<
                            (),
                            crate::runtime::DtaError,
                        > = (|| {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [44u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                            {
                                let __scan_start = *p;
                                let Some(match_len) = __regex_scan_CssPrettyParser(
                                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
                                    input,
                                    *p,
                                ) else {
                                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                        offset: __scan_start as u32,
                                    });
                                };
                                *p += match_len as usize;
                            }
                            {
                                let __scan_start = *p;
                                let Some(match_len) = __regex_scan_CssPrettyParser(
                                    "[^;{}!,]+",
                                    input,
                                    *p,
                                ) else {
                                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                        offset: __scan_start as u32,
                                    });
                                };
                                *p += match_len as usize;
                            }
                            Ok(())
                        })();
                        match __iter_result {
                            Ok(()) => {
                                if *p == __iter_save_p {
                                    builder.rollback(__iter_builder_checkpoint);
                                    break;
                                }
                                builder.commit(__iter_builder_checkpoint);
                                __iter_count += 1;
                            }
                            Err(_) => {
                                *p = __iter_save_p;
                                builder.rollback(__iter_builder_checkpoint);
                                break;
                            }
                        }
                    }
                    if __iter_count < 0u32 {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: *p as u32,
                        });
                    }
                }
            }
            {
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [59u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                let __span_hi: usize = *p;
                let __span_slice: &str = ::core::str::from_utf8(
                        &input[__span_lo..__span_hi],
                    )
                    .unwrap_or("");
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_leaf_with_str(
                    builder,
                    __span_slice,
                );
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __importRule_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / CSS L4 / Sheets per the
    /// resolver's `SubstrateBinding`).
    ///
    /// Compound emission lands as typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    ///
    /// AZ-III.W2.4.r — content-only bodies (no Ref /
    /// TokenDispatch in the IR) capture `*p` before and after
    /// the per-position emission and push one synthetic Span
    /// leaf carrying the consumed source slice; this restores
    /// the contract `bootstrap_parser` met for `regex` /
    /// `literal` / `comment` / `big_comment` / `import_path`
    /// (all flat-shape rules whose grammar projection is
    /// `-> Span` or whose host walker reads via `byte_span()`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_CssPrettyParser_declaration<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::css_pretty::CssPrettyStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(2u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        static __DECLARATION_LAYOUT: ::std::sync::LazyLock<
            ::bbnf_ir::registry::StructLayout,
        > = ::std::sync::LazyLock::new(|| ::bbnf_ir::registry::StructLayout {
            rule_id: 2u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("declaration"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        });
        let __declaration_handle = <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &*__DECLARATION_LAYOUT,
        );
        <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "[a-zA-Z_][\\w-]*|--[\\w-]+|-[a-zA-Z][\\w-]*",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [58u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
            }
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                {
                    let mut __iter_count: u32 = 0;
                    loop {
                        if __iter_count >= 1u32 {
                            break;
                        }
                        let __iter_save_p = *p;
                        if input.get(*p).is_none() {
                            break;
                        }
                        let __iter_builder_checkpoint = builder.checkpoint();
                        let __iter_result: ::core::result::Result<
                            (),
                            crate::runtime::DtaError,
                        > = (|| {
                            {
                                let __scan_start = *p;
                                let Some(match_len) = __regex_scan_CssPrettyParser(
                                    "[^;{}!,]+",
                                    input,
                                    *p,
                                ) else {
                                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                        offset: __scan_start as u32,
                                    });
                                };
                                *p += match_len as usize;
                            }
                            {
                                let mut __iter_count: u32 = 0;
                                loop {
                                    if __iter_count >= 4294967295u32 {
                                        break;
                                    }
                                    let __iter_save_p = *p;
                                    if input.get(*p).is_none() {
                                        break;
                                    }
                                    let __iter_builder_checkpoint = builder.checkpoint();
                                    let __iter_result: ::core::result::Result<
                                        (),
                                        crate::runtime::DtaError,
                                    > = (|| {
                                        let at = *p;
                                        let end = at + 1usize;
                                        if input.len() < end || input[at..end] != [44u8] {
                                            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                                offset: at as u32,
                                            });
                                        }
                                        *p = end;
                                        {
                                            let __scan_start = *p;
                                            let Some(match_len) = __regex_scan_CssPrettyParser(
                                                "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
                                                input,
                                                *p,
                                            ) else {
                                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                                    offset: __scan_start as u32,
                                                });
                                            };
                                            *p += match_len as usize;
                                        }
                                        {
                                            let __scan_start = *p;
                                            let Some(match_len) = __regex_scan_CssPrettyParser(
                                                "[^;{}!,]+",
                                                input,
                                                *p,
                                            ) else {
                                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                                    offset: __scan_start as u32,
                                                });
                                            };
                                            *p += match_len as usize;
                                        }
                                        Ok(())
                                    })();
                                    match __iter_result {
                                        Ok(()) => {
                                            if *p == __iter_save_p {
                                                builder.rollback(__iter_builder_checkpoint);
                                                break;
                                            }
                                            builder.commit(__iter_builder_checkpoint);
                                            __iter_count += 1;
                                        }
                                        Err(_) => {
                                            *p = __iter_save_p;
                                            builder.rollback(__iter_builder_checkpoint);
                                            break;
                                        }
                                    }
                                }
                                if __iter_count < 0u32 {
                                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                        offset: *p as u32,
                                    });
                                }
                            }
                            Ok(())
                        })();
                        match __iter_result {
                            Ok(()) => {
                                if *p == __iter_save_p {
                                    builder.rollback(__iter_builder_checkpoint);
                                    break;
                                }
                                builder.commit(__iter_builder_checkpoint);
                                __iter_count += 1;
                            }
                            Err(_) => {
                                *p = __iter_save_p;
                                builder.rollback(__iter_builder_checkpoint);
                                break;
                            }
                        }
                    }
                    if __iter_count < 0u32 {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: *p as u32,
                        });
                    }
                }
            }
            {
                {
                    let mut __iter_count: u32 = 0;
                    loop {
                        if __iter_count >= 1u32 {
                            break;
                        }
                        let __iter_save_p = *p;
                        if input.get(*p).is_none() {
                            break;
                        }
                        let __iter_builder_checkpoint = builder.checkpoint();
                        let __iter_result: ::core::result::Result<
                            (),
                            crate::runtime::DtaError,
                        > = (|| {
                            let _ = ({
                                let _ = __shape_support_CssPrettyParser::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_flat_CssPrettyParser_important(
                                    input,
                                    p,
                                    state,
                                    builder,
                                    cursor,
                                )
                            })?;
                            Ok(())
                        })();
                        match __iter_result {
                            Ok(()) => {
                                if *p == __iter_save_p {
                                    builder.rollback(__iter_builder_checkpoint);
                                    break;
                                }
                                builder.commit(__iter_builder_checkpoint);
                                __iter_count += 1;
                            }
                            Err(_) => {
                                *p = __iter_save_p;
                                builder.rollback(__iter_builder_checkpoint);
                                break;
                            }
                        }
                    }
                    if __iter_count < 0u32 {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: *p as u32,
                        });
                    }
                }
            }
            {
                {
                    let mut __iter_count: u32 = 0;
                    loop {
                        if __iter_count >= 1u32 {
                            break;
                        }
                        let __iter_save_p = *p;
                        if input.get(*p).is_none() {
                            break;
                        }
                        let __iter_builder_checkpoint = builder.checkpoint();
                        let __iter_result: ::core::result::Result<
                            (),
                            crate::runtime::DtaError,
                        > = (|| {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [59u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                            Ok(())
                        })();
                        match __iter_result {
                            Ok(()) => {
                                if *p == __iter_save_p {
                                    builder.rollback(__iter_builder_checkpoint);
                                    break;
                                }
                                builder.commit(__iter_builder_checkpoint);
                                __iter_count += 1;
                            }
                            Err(_) => {
                                *p = __iter_save_p;
                                builder.rollback(__iter_builder_checkpoint);
                                break;
                            }
                        }
                    }
                    if __iter_count < 0u32 {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: *p as u32,
                        });
                    }
                }
            }
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __declaration_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / CSS L4 / Sheets per the
    /// resolver's `SubstrateBinding`).
    ///
    /// Compound emission lands as typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    ///
    /// AZ-III.W2.4.r — content-only bodies (no Ref /
    /// TokenDispatch in the IR) capture `*p` before and after
    /// the per-position emission and push one synthetic Span
    /// leaf carrying the consumed source slice; this restores
    /// the contract `bootstrap_parser` met for `regex` /
    /// `literal` / `comment` / `big_comment` / `import_path`
    /// (all flat-shape rules whose grammar projection is
    /// `-> Span` or whose host walker reads via `byte_span()`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_CssPrettyParser_genericAtRule<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::css_pretty::CssPrettyStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(3u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        static __GENERICATRULE_LAYOUT: ::std::sync::LazyLock<
            ::bbnf_ir::registry::StructLayout,
        > = ::std::sync::LazyLock::new(|| ::bbnf_ir::registry::StructLayout {
            rule_id: 3u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("genericAtRule"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        });
        let __genericAtRule_handle = <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &*__GENERICATRULE_LAYOUT,
        );
        <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "@[a-zA-Z][\\w-]*",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "[^;{}]*",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                let _ = ({
                    let __first = __shape_support_CssPrettyParser::skip_space(
                            input,
                            p,
                            state,
                        )
                        .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                            offset: *p as u32,
                        })?;
                    parse_keyword_CssPrettyParser_atRuleBody(
                        input,
                        p,
                        __first,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __genericAtRule_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / CSS L4 / Sheets per the
    /// resolver's `SubstrateBinding`).
    ///
    /// Compound emission lands as typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    ///
    /// AZ-III.W2.4.r — content-only bodies (no Ref /
    /// TokenDispatch in the IR) capture `*p` before and after
    /// the per-position emission and push one synthetic Span
    /// leaf carrying the consumed source slice; this restores
    /// the contract `bootstrap_parser` met for `regex` /
    /// `literal` / `comment` / `big_comment` / `import_path`
    /// (all flat-shape rules whose grammar projection is
    /// `-> Span` or whose host walker reads via `byte_span()`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_CssPrettyParser_qualifiedRule<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::css_pretty::CssPrettyStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(4u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        static __QUALIFIEDRULE_LAYOUT: ::std::sync::LazyLock<
            ::bbnf_ir::registry::StructLayout,
        > = ::std::sync::LazyLock::new(|| ::bbnf_ir::registry::StructLayout {
            rule_id: 4u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("qualifiedRule"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        });
        let __qualifiedRule_handle = <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &*__QUALIFIEDRULE_LAYOUT,
        );
        <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "[^{};]+",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                let _ = ({
                    let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                    parse_flat_CssPrettyParser_ruleBlock(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __qualifiedRule_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / CSS L4 / Sheets per the
    /// resolver's `SubstrateBinding`).
    ///
    /// Compound emission lands as typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    ///
    /// AZ-III.W2.4.r — content-only bodies (no Ref /
    /// TokenDispatch in the IR) capture `*p` before and after
    /// the per-position emission and push one synthetic Span
    /// leaf carrying the consumed source slice; this restores
    /// the contract `bootstrap_parser` met for `regex` /
    /// `literal` / `comment` / `big_comment` / `import_path`
    /// (all flat-shape rules whose grammar projection is
    /// `-> Span` or whose host walker reads via `byte_span()`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_CssPrettyParser_mediaRule<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::css_pretty::CssPrettyStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(5u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        static __MEDIARULE_LAYOUT: ::std::sync::LazyLock<
            ::bbnf_ir::registry::StructLayout,
        > = ::std::sync::LazyLock::new(|| ::bbnf_ir::registry::StructLayout {
            rule_id: 5u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("mediaRule"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        });
        let __mediaRule_handle = <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &*__MEDIARULE_LAYOUT,
        );
        <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let at = *p;
                let end = at + 6usize;
                if input.len() < end
                    || input[at..end] != [64u8, 109u8, 101u8, 100u8, 105u8, 97u8]
                {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
            }
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "[^{]+",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                let _ = ({
                    let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                    parse_flat_CssPrettyParser_ruleBlock(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __mediaRule_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / CSS L4 / Sheets per the
    /// resolver's `SubstrateBinding`).
    ///
    /// Compound emission lands as typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    ///
    /// AZ-III.W2.4.r — content-only bodies (no Ref /
    /// TokenDispatch in the IR) capture `*p` before and after
    /// the per-position emission and push one synthetic Span
    /// leaf carrying the consumed source slice; this restores
    /// the contract `bootstrap_parser` met for `regex` /
    /// `literal` / `comment` / `big_comment` / `import_path`
    /// (all flat-shape rules whose grammar projection is
    /// `-> Span` or whose host walker reads via `byte_span()`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_CssPrettyParser_supportsRule<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::css_pretty::CssPrettyStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(6u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        static __SUPPORTSRULE_LAYOUT: ::std::sync::LazyLock<
            ::bbnf_ir::registry::StructLayout,
        > = ::std::sync::LazyLock::new(|| ::bbnf_ir::registry::StructLayout {
            rule_id: 6u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("supportsRule"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        });
        let __supportsRule_handle = <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &*__SUPPORTSRULE_LAYOUT,
        );
        <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let at = *p;
                let end = at + 9usize;
                if input.len() < end
                    || input[at..end]
                        != [64u8, 115u8, 117u8, 112u8, 112u8, 111u8, 114u8, 116u8, 115u8]
                {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
            }
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "[^{]+",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                let _ = ({
                    let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                    parse_flat_CssPrettyParser_ruleBlock(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __supportsRule_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / CSS L4 / Sheets per the
    /// resolver's `SubstrateBinding`).
    ///
    /// Compound emission lands as typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    ///
    /// AZ-III.W2.4.r — content-only bodies (no Ref /
    /// TokenDispatch in the IR) capture `*p` before and after
    /// the per-position emission and push one synthetic Span
    /// leaf carrying the consumed source slice; this restores
    /// the contract `bootstrap_parser` met for `regex` /
    /// `literal` / `comment` / `big_comment` / `import_path`
    /// (all flat-shape rules whose grammar projection is
    /// `-> Span` or whose host walker reads via `byte_span()`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_CssPrettyParser_fontFaceRule<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::css_pretty::CssPrettyStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(7u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        static __FONTFACERULE_LAYOUT: ::std::sync::LazyLock<
            ::bbnf_ir::registry::StructLayout,
        > = ::std::sync::LazyLock::new(|| ::bbnf_ir::registry::StructLayout {
            rule_id: 7u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("fontFaceRule"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        });
        let __fontFaceRule_handle = <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &*__FONTFACERULE_LAYOUT,
        );
        <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let at = *p;
                let end = at + 10usize;
                if input.len() < end
                    || input[at..end]
                        != [
                            64u8, 102u8, 111u8, 110u8, 116u8, 45u8, 102u8, 97u8, 99u8,
                            101u8,
                        ]
                {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
            }
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                let _ = ({
                    let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                    parse_flat_CssPrettyParser_ruleBlock(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __fontFaceRule_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RD — struct-direct Keyword-shape parse fn
    /// (Alt of literal-led, Ref-led, or Seq-led branches).
    ///
    /// Literal branches push leaves through
    /// `builder.push_leaf_with_bool` (TypeDesc::Bool) or
    /// `builder.push_leaf_with_unit` (TypeDesc::U8 /
    /// untyped). Ref branches delegate to the target shape
    /// fn so the target writes directly into the same
    /// builder. Returns unit for StructDirect composition.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_keyword_CssPrettyParser_atRuleBody<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::css_pretty::CssPrettyStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        let _ = state;
        let _ = cursor;
        match first_byte {
            59u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [59u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder
                        .push_leaf_with_str(unsafe {
                            ::core::str::from_utf8_unchecked(&input[at..end])
                        });
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            123u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [123u8] {
                    let __ref_save_p = *p;
                    let __ref_builder_checkpoint = builder.checkpoint();
                    match ({
                        let _ = __shape_support_CssPrettyParser::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_flat_CssPrettyParser_ruleBlock(
                            input,
                            p,
                            state,
                            builder,
                            cursor,
                        )
                    }) {
                        ::core::result::Result::Ok(__off) => {
                            builder.commit(__ref_builder_checkpoint);
                            return ::core::result::Result::Ok(__off);
                        }
                        ::core::result::Result::Err(_) => {
                            *p = __ref_save_p;
                            builder.rollback(__ref_builder_checkpoint);
                        }
                    }
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            _ => {
                ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                })
            }
        }
    }
    /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / CSS L4 / Sheets per the
    /// resolver's `SubstrateBinding`).
    ///
    /// Compound emission lands as typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    ///
    /// AZ-III.W2.4.r — content-only bodies (no Ref /
    /// TokenDispatch in the IR) capture `*p` before and after
    /// the per-position emission and push one synthetic Span
    /// leaf carrying the consumed source slice; this restores
    /// the contract `bootstrap_parser` met for `regex` /
    /// `literal` / `comment` / `big_comment` / `import_path`
    /// (all flat-shape rules whose grammar projection is
    /// `-> Span` or whose host walker reads via `byte_span()`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_CssPrettyParser_ruleBlock<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::css_pretty::CssPrettyStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(9u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        static __RULEBLOCK_LAYOUT: ::std::sync::LazyLock<
            ::bbnf_ir::registry::StructLayout,
        > = ::std::sync::LazyLock::new(|| ::bbnf_ir::registry::StructLayout {
            rule_id: 9u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("ruleBlock"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        });
        let __ruleBlock_handle = <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &*__RULEBLOCK_LAYOUT,
        );
        <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [123u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
            }
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                let _ = ({
                    let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                    parse_flat_CssPrettyParser_blockContent(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [125u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __ruleBlock_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / CSS L4 / Sheets per the
    /// resolver's `SubstrateBinding`).
    ///
    /// Compound emission lands as typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    ///
    /// AZ-III.W2.4.r — content-only bodies (no Ref /
    /// TokenDispatch in the IR) capture `*p` before and after
    /// the per-position emission and push one synthetic Span
    /// leaf carrying the consumed source slice; this restores
    /// the contract `bootstrap_parser` met for `regex` /
    /// `literal` / `comment` / `big_comment` / `import_path`
    /// (all flat-shape rules whose grammar projection is
    /// `-> Span` or whose host walker reads via `byte_span()`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_CssPrettyParser_blockContent<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::css_pretty::CssPrettyStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(10u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        static __BLOCKCONTENT_LAYOUT: ::std::sync::LazyLock<
            ::bbnf_ir::registry::StructLayout,
        > = ::std::sync::LazyLock::new(|| ::bbnf_ir::registry::StructLayout {
            rule_id: 10u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("blockContent"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        });
        let __blockContent_handle = <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &*__BLOCKCONTENT_LAYOUT,
        );
        <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                {
                    let mut __iter_count: u32 = 0;
                    loop {
                        if __iter_count >= 4294967295u32 {
                            break;
                        }
                        let __iter_save_p = *p;
                        if input.get(*p).is_none() {
                            break;
                        }
                        let __iter_builder_checkpoint = builder.checkpoint();
                        let __iter_result: ::core::result::Result<
                            (),
                            crate::runtime::DtaError,
                        > = (|| {
                            {
                                let __scan_start = *p;
                                let Some(match_len) = __regex_scan_CssPrettyParser(
                                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
                                    input,
                                    *p,
                                ) else {
                                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                        offset: __scan_start as u32,
                                    });
                                };
                                *p += match_len as usize;
                            }
                            'try_branches: loop {
                                {
                                    let __alt_save_p = *p;
                                    let __alt_builder_checkpoint = builder.checkpoint();
                                    let __alt_result: ::core::result::Result<
                                        (),
                                        crate::runtime::DtaError,
                                    > = (|| {
                                        let _ = ({
                                            let _ = __shape_support_CssPrettyParser::skip_space(
                                                input,
                                                p,
                                                state,
                                            );
                                            parse_flat_CssPrettyParser_declaration(
                                                input,
                                                p,
                                                state,
                                                builder,
                                                cursor,
                                            )
                                        })?;
                                        Ok(())
                                    })();
                                    match __alt_result {
                                        Ok(()) => {
                                            builder.commit(__alt_builder_checkpoint);
                                            break 'try_branches;
                                        }
                                        Err(_) => {
                                            *p = __alt_save_p;
                                            builder.rollback(__alt_builder_checkpoint);
                                        }
                                    }
                                }
                                {
                                    let __alt_save_p = *p;
                                    let __alt_builder_checkpoint = builder.checkpoint();
                                    let __alt_result: ::core::result::Result<
                                        (),
                                        crate::runtime::DtaError,
                                    > = (|| {
                                        let _ = ({
                                            parse_wrap_CssPrettyParser_ruleItem(
                                                input,
                                                p,
                                                state,
                                                builder,
                                                cursor,
                                            )
                                        })?;
                                        Ok(())
                                    })();
                                    match __alt_result {
                                        Ok(()) => {
                                            builder.commit(__alt_builder_checkpoint);
                                            break 'try_branches;
                                        }
                                        Err(_) => {
                                            *p = __alt_save_p;
                                            builder.rollback(__alt_builder_checkpoint);
                                        }
                                    }
                                }
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: *p as u32,
                                });
                            }
                            Ok(())
                        })();
                        match __iter_result {
                            Ok(()) => {
                                if *p == __iter_save_p {
                                    builder.rollback(__iter_builder_checkpoint);
                                    break;
                                }
                                builder.commit(__iter_builder_checkpoint);
                                __iter_count += 1;
                            }
                            Err(_) => {
                                *p = __iter_save_p;
                                builder.rollback(__iter_builder_checkpoint);
                                break;
                            }
                        }
                    }
                    if __iter_count < 0u32 {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: *p as u32,
                        });
                    }
                }
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __blockContent_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RD — struct-direct Wrap-shape parse function.
    ///
    /// Opens a Wrap frame on the builder, dispatches to the matched
    /// branch's shape fn (which carries its own
    /// begin_compound/end_compound for compound branches and the
    /// matching push_leaf_with_* for scalar branches), stamps the
    /// chosen branch index via push_branch_tag, then closes the
    /// Wrap frame. Mirrors `JsonStructBuilder::OpenFrame::Wrap`'s
    /// forward-the-single-child semantics.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AZ-IV.W3.6 — Cursor-threaded. The Alt-dispatch branch
    /// selector consults `cursor.decide(rule_id)` so a
    /// `Decision::ParseUntil(idx)` returned by the path plan
    /// means the targeted variant index is preserved by the
    /// linear-try fallback (the byte-dispatch arms are still
    /// the prefilter; the cursor's decision is forwarded into
    /// the inner Refs as the descent proceeds).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_wrap_CssPrettyParser_atRule<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::css_pretty::CssPrettyStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let _ = cursor.decide(11u32 as u32);
        let first = __shape_support_CssPrettyParser::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        'try_branches: loop {
            match first {
                64u8 => {
                    {
                        let attempt_p = *p;
                        let attempt_builder = builder.checkpoint();
                        match parse_flat_CssPrettyParser_mediaRule(
                            input,
                            p,
                            state,
                            builder,
                            cursor,
                        ) {
                            ::core::result::Result::Ok(_) => {
                                builder.commit(attempt_builder);
                                break 'try_branches;
                            }
                            ::core::result::Result::Err(_) => {
                                *p = attempt_p;
                                builder.rollback(attempt_builder);
                            }
                        }
                    }
                    {
                        let attempt_p = *p;
                        let attempt_builder = builder.checkpoint();
                        match parse_flat_CssPrettyParser_supportsRule(
                            input,
                            p,
                            state,
                            builder,
                            cursor,
                        ) {
                            ::core::result::Result::Ok(_) => {
                                builder.commit(attempt_builder);
                                break 'try_branches;
                            }
                            ::core::result::Result::Err(_) => {
                                *p = attempt_p;
                                builder.rollback(attempt_builder);
                            }
                        }
                    }
                    {
                        let attempt_p = *p;
                        let attempt_builder = builder.checkpoint();
                        match parse_flat_CssPrettyParser_fontFaceRule(
                            input,
                            p,
                            state,
                            builder,
                            cursor,
                        ) {
                            ::core::result::Result::Ok(_) => {
                                builder.commit(attempt_builder);
                                break 'try_branches;
                            }
                            ::core::result::Result::Err(_) => {
                                *p = attempt_p;
                                builder.rollback(attempt_builder);
                            }
                        }
                    }
                    {
                        let attempt_p = *p;
                        let attempt_builder = builder.checkpoint();
                        match parse_flat_CssPrettyParser_importRule(
                            input,
                            p,
                            state,
                            builder,
                            cursor,
                        ) {
                            ::core::result::Result::Ok(_) => {
                                builder.commit(attempt_builder);
                                break 'try_branches;
                            }
                            ::core::result::Result::Err(_) => {
                                *p = attempt_p;
                                builder.rollback(attempt_builder);
                            }
                        }
                    }
                    {
                        let attempt_p = *p;
                        let attempt_builder = builder.checkpoint();
                        match parse_flat_CssPrettyParser_genericAtRule(
                            input,
                            p,
                            state,
                            builder,
                            cursor,
                        ) {
                            ::core::result::Result::Ok(_) => {
                                builder.commit(attempt_builder);
                                break 'try_branches;
                            }
                            ::core::result::Result::Err(_) => {
                                *p = attempt_p;
                                builder.rollback(attempt_builder);
                            }
                        }
                    }
                }
                _ => {}
            }
            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                offset: *p as u32,
            });
        }
        ::core::result::Result::Ok(())
    }
    /// AZ-I.W2.RD — struct-direct Wrap-shape parse function.
    ///
    /// Opens a Wrap frame on the builder, dispatches to the matched
    /// branch's shape fn (which carries its own
    /// begin_compound/end_compound for compound branches and the
    /// matching push_leaf_with_* for scalar branches), stamps the
    /// chosen branch index via push_branch_tag, then closes the
    /// Wrap frame. Mirrors `JsonStructBuilder::OpenFrame::Wrap`'s
    /// forward-the-single-child semantics.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AZ-IV.W3.6 — Cursor-threaded. The Alt-dispatch branch
    /// selector consults `cursor.decide(rule_id)` so a
    /// `Decision::ParseUntil(idx)` returned by the path plan
    /// means the targeted variant index is preserved by the
    /// linear-try fallback (the byte-dispatch arms are still
    /// the prefilter; the cursor's decision is forwarded into
    /// the inner Refs as the descent proceeds).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_wrap_CssPrettyParser_ruleItem<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::css_pretty::CssPrettyStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let _ = cursor.decide(12u32 as u32);
        let first = __shape_support_CssPrettyParser::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        'try_branches: loop {
            match first {
                _ => {}
            }
            {
                let attempt_p = *p;
                let attempt_builder = builder.checkpoint();
                match parse_flat_CssPrettyParser_qualifiedRule(
                    input,
                    p,
                    state,
                    builder,
                    cursor,
                ) {
                    ::core::result::Result::Ok(_) => {
                        builder.commit(attempt_builder);
                        break 'try_branches;
                    }
                    ::core::result::Result::Err(_) => {
                        *p = attempt_p;
                        builder.rollback(attempt_builder);
                    }
                }
            }
            {
                let attempt_p = *p;
                let attempt_builder = builder.checkpoint();
                match parse_wrap_CssPrettyParser_atRule(
                    input,
                    p,
                    state,
                    builder,
                    cursor,
                ) {
                    ::core::result::Result::Ok(_) => {
                        builder.commit(attempt_builder);
                        break 'try_branches;
                    }
                    ::core::result::Result::Err(_) => {
                        *p = attempt_p;
                        builder.rollback(attempt_builder);
                    }
                }
            }
            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                offset: *p as u32,
            });
        }
        ::core::result::Result::Ok(())
    }
    /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / CSS L4 / Sheets per the
    /// resolver's `SubstrateBinding`).
    ///
    /// Compound emission lands as typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    ///
    /// AZ-III.W2.4.r — content-only bodies (no Ref /
    /// TokenDispatch in the IR) capture `*p` before and after
    /// the per-position emission and push one synthetic Span
    /// leaf carrying the consumed source slice; this restores
    /// the contract `bootstrap_parser` met for `regex` /
    /// `literal` / `comment` / `big_comment` / `import_path`
    /// (all flat-shape rules whose grammar projection is
    /// `-> Span` or whose host walker reads via `byte_span()`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_CssPrettyParser_ruleList<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::css_pretty::CssPrettyStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(13u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        static __RULELIST_LAYOUT: ::std::sync::LazyLock<
            ::bbnf_ir::registry::StructLayout,
        > = ::std::sync::LazyLock::new(|| ::bbnf_ir::registry::StructLayout {
            rule_id: 13u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("ruleList"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        });
        let __ruleList_handle = <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &*__RULELIST_LAYOUT,
        );
        <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                {
                    let mut __iter_count: u32 = 0;
                    loop {
                        if __iter_count >= 4294967295u32 {
                            break;
                        }
                        let __iter_save_p = *p;
                        if input.get(*p).is_none() {
                            break;
                        }
                        let __iter_builder_checkpoint = builder.checkpoint();
                        let __iter_result: ::core::result::Result<
                            (),
                            crate::runtime::DtaError,
                        > = (|| {
                            {
                                let __scan_start = *p;
                                let Some(match_len) = __regex_scan_CssPrettyParser(
                                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
                                    input,
                                    *p,
                                ) else {
                                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                        offset: __scan_start as u32,
                                    });
                                };
                                *p += match_len as usize;
                            }
                            let _ = ({
                                parse_wrap_CssPrettyParser_ruleItem(
                                    input,
                                    p,
                                    state,
                                    builder,
                                    cursor,
                                )
                            })?;
                            Ok(())
                        })();
                        match __iter_result {
                            Ok(()) => {
                                if *p == __iter_save_p {
                                    builder.rollback(__iter_builder_checkpoint);
                                    break;
                                }
                                builder.commit(__iter_builder_checkpoint);
                                __iter_count += 1;
                            }
                            Err(_) => {
                                *p = __iter_save_p;
                                builder.rollback(__iter_builder_checkpoint);
                                break;
                            }
                        }
                    }
                    if __iter_count < 0u32 {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: *p as u32,
                        });
                    }
                }
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __ruleList_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / CSS L4 / Sheets per the
    /// resolver's `SubstrateBinding`).
    ///
    /// Compound emission lands as typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    ///
    /// AZ-III.W2.4.r — content-only bodies (no Ref /
    /// TokenDispatch in the IR) capture `*p` before and after
    /// the per-position emission and push one synthetic Span
    /// leaf carrying the consumed source slice; this restores
    /// the contract `bootstrap_parser` met for `regex` /
    /// `literal` / `comment` / `big_comment` / `import_path`
    /// (all flat-shape rules whose grammar projection is
    /// `-> Span` or whose host walker reads via `byte_span()`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_CssPrettyParser_stylesheet<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::css_pretty::CssPrettyStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(14u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        static __STYLESHEET_LAYOUT: ::std::sync::LazyLock<
            ::bbnf_ir::registry::StructLayout,
        > = ::std::sync::LazyLock::new(|| ::bbnf_ir::registry::StructLayout {
            rule_id: 14u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("stylesheet"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        });
        let __stylesheet_handle = <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &*__STYLESHEET_LAYOUT,
        );
        <crate::runtime::css_pretty::CssPrettyStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                let _ = ({
                    let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                    parse_flat_CssPrettyParser_ruleList(input, p, state, builder, cursor)
                })?;
            }
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::css_pretty::CssPrettyStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __stylesheet_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
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
    pub fn parse_CssPrettyParser_stylesheet<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::css_pretty::CssPrettyStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        parse_CssPrettyParser_stylesheet__value(input, p, state, builder, cursor)
    }
    /// AW-V.W3.2 — value-position shape dispatcher. Called both at
    /// the grammar root and from Object / Array compound bodies.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_CssPrettyParser_stylesheet__value<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::css_pretty::CssPrettyStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
        let _ = cursor.decide(14u32);
        parse_flat_CssPrettyParser_stylesheet(input, p, state, builder, cursor)
    }
    impl CssPrettyParser {
        fn __important_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'!') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'!');
                    };
                    {
                        let __start = state.offset;
                        if {
                            let __start = state.offset;
                            let __result: Option<()> = (|| {
                                {
                                    let mut __rep_count: u32 = 0;
                                    loop {
                                        let __save = state.offset;
                                        let __ok = (|| -> Option<()> {
                                            {
                                                let __save_dispatch = state.offset;
                                                let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                match __dispatch_b {
                                                    b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' => {
                                                        let __ok = (|| -> Option<()> {
                                                            {
                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                if !(__b.is_ascii_whitespace()) {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                            }
                                                            Some(())
                                                        })();
                                                        if __ok.is_none() {
                                                            state.offset = __save_dispatch;
                                                            return None;
                                                        }
                                                    }
                                                    b'/' => {
                                                        let __ok = (|| -> Option<()> {
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            {
                                                                let __end = state.src_bytes.len();
                                                                let mut __pos = state.offset;
                                                                while __pos < __end {
                                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                    if !(__b == b'*') {
                                                                        __pos += 1;
                                                                    } else {
                                                                        break;
                                                                    }
                                                                }
                                                                state.offset = __pos;
                                                            }
                                                            {
                                                                let mut __rep_count: u32 = 0;
                                                                loop {
                                                                    let __save = state.offset;
                                                                    let __ok = (|| -> Option<()> {
                                                                        {
                                                                            let mut __rep_count: u32 = 0;
                                                                            loop {
                                                                                let __save = state.offset;
                                                                                let __ok = (|| -> Option<()> {
                                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                    {
                                                                                        return None;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                    Some(())
                                                                                })();
                                                                                if __ok.is_none() {
                                                                                    state.offset = __save;
                                                                                    break;
                                                                                }
                                                                                if state.offset == __save {
                                                                                    break;
                                                                                }
                                                                                __rep_count += 1;
                                                                            }
                                                                            if __rep_count < 1 {
                                                                                return None;
                                                                            }
                                                                        }
                                                                        {
                                                                            let __b = *state.src_bytes.get(state.offset)?;
                                                                            if !(!(__b == b'/')) {
                                                                                return None;
                                                                            }
                                                                            state.offset += 1;
                                                                        }
                                                                        {
                                                                            let __end = state.src_bytes.len();
                                                                            let mut __pos = state.offset;
                                                                            while __pos < __end {
                                                                                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                if !(__b == b'*') {
                                                                                    __pos += 1;
                                                                                } else {
                                                                                    break;
                                                                                }
                                                                            }
                                                                            state.offset = __pos;
                                                                        }
                                                                        Some(())
                                                                    })();
                                                                    if __ok.is_none() {
                                                                        state.offset = __save;
                                                                        break;
                                                                    }
                                                                    if state.offset == __save {
                                                                        break;
                                                                    }
                                                                    __rep_count += 1;
                                                                }
                                                            }
                                                            {
                                                                let mut __rep_count: u32 = 0;
                                                                loop {
                                                                    let __save = state.offset;
                                                                    let __ok = (|| -> Option<()> {
                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                        {
                                                                            return None;
                                                                        }
                                                                        state.offset += 1;
                                                                        Some(())
                                                                    })();
                                                                    if __ok.is_none() {
                                                                        state.offset = __save;
                                                                        break;
                                                                    }
                                                                    if state.offset == __save {
                                                                        break;
                                                                    }
                                                                    __rep_count += 1;
                                                                }
                                                                if __rep_count < 1 {
                                                                    return None;
                                                                }
                                                            }
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            Some(())
                                                        })();
                                                        if __ok.is_none() {
                                                            state.offset = __save_dispatch;
                                                            return None;
                                                        }
                                                    }
                                                    _ => {
                                                        return None;
                                                    }
                                                }
                                            }
                                            Some(())
                                        })();
                                        if __ok.is_none() {
                                            state.offset = __save;
                                            break;
                                        }
                                        if state.offset == __save {
                                            break;
                                        }
                                        __rep_count += 1;
                                    }
                                }
                                Some(())
                            })();
                            if __result.is_some() {
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                state.offset = __start;
                                None
                            }
                        }
                            .is_none()
                        {
                            return false;
                        }
                        let __matched = &state.src[__start..state.offset];
                        if !__matched.is_empty() {
                            __builder.text(__matched);
                        }
                    };
                    {
                        let __s = "important";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 9usize => s,
                            _ => return false,
                        };
                        if &__slc[..9usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 9usize]);
                        state.offset += 9usize;
                    };
                };
                true
            }
        }
        pub fn important_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__important_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __importRule_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __s = "@import";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 7usize => s,
                            _ => return false,
                        };
                        if &__slc[..7usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 7usize]);
                        state.offset += 7usize;
                    };
                    {
                        let __start = state.offset;
                        if {
                            let __start = state.offset;
                            loop {
                                ::parse_that::scan_ws_block_comments(state);
                                if state.offset >= state.src_bytes.len() {
                                    break;
                                }
                                let __b = state.src_bytes[state.offset];
                                if __b == b'!' || __b == b',' || __b == b';' || __b == b'{'
                                    || __b == b'}'
                                {
                                    break;
                                }
                                state.offset += 1;
                            }
                            if state.offset > __start {
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            }
                        }
                            .is_none()
                        {
                            return false;
                        }
                        let __matched = &state.src[__start..state.offset];
                        if !__matched.is_empty() {
                            __builder.text(__matched);
                        }
                    };
                    {
                        let mut __rep_count2 = 0usize;
                        while __rep_count2 < 4294967295 {
                            let __rep_cp3 = state.offset;
                            if !{
                                let __pretty_cp0 = state.offset;
                                let __pretty_bcp1 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b',');
                                        };
                                        {
                                            let __start = state.offset;
                                            if {
                                                let __start = state.offset;
                                                let __result: Option<()> = (|| {
                                                    {
                                                        let mut __rep_count: u32 = 0;
                                                        loop {
                                                            let __save = state.offset;
                                                            let __ok = (|| -> Option<()> {
                                                                {
                                                                    let __save_dispatch = state.offset;
                                                                    let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                                    match __dispatch_b {
                                                                        b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' => {
                                                                            let __ok = (|| -> Option<()> {
                                                                                {
                                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                                    if !(__b.is_ascii_whitespace()) {
                                                                                        return None;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                }
                                                                                Some(())
                                                                            })();
                                                                            if __ok.is_none() {
                                                                                state.offset = __save_dispatch;
                                                                                return None;
                                                                            }
                                                                        }
                                                                        b'/' => {
                                                                            let __ok = (|| -> Option<()> {
                                                                                if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                                {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                                if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                                {
                                                                                    let __end = state.src_bytes.len();
                                                                                    let mut __pos = state.offset;
                                                                                    while __pos < __end {
                                                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                        if !(__b == b'*') {
                                                                                            __pos += 1;
                                                                                        } else {
                                                                                            break;
                                                                                        }
                                                                                    }
                                                                                    state.offset = __pos;
                                                                                }
                                                                                {
                                                                                    let mut __rep_count: u32 = 0;
                                                                                    loop {
                                                                                        let __save = state.offset;
                                                                                        let __ok = (|| -> Option<()> {
                                                                                            {
                                                                                                let mut __rep_count: u32 = 0;
                                                                                                loop {
                                                                                                    let __save = state.offset;
                                                                                                    let __ok = (|| -> Option<()> {
                                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                                        {
                                                                                                            return None;
                                                                                                        }
                                                                                                        state.offset += 1;
                                                                                                        Some(())
                                                                                                    })();
                                                                                                    if __ok.is_none() {
                                                                                                        state.offset = __save;
                                                                                                        break;
                                                                                                    }
                                                                                                    if state.offset == __save {
                                                                                                        break;
                                                                                                    }
                                                                                                    __rep_count += 1;
                                                                                                }
                                                                                                if __rep_count < 1 {
                                                                                                    return None;
                                                                                                }
                                                                                            }
                                                                                            {
                                                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                                                if !(!(__b == b'/')) {
                                                                                                    return None;
                                                                                                }
                                                                                                state.offset += 1;
                                                                                            }
                                                                                            {
                                                                                                let __end = state.src_bytes.len();
                                                                                                let mut __pos = state.offset;
                                                                                                while __pos < __end {
                                                                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                                    if !(__b == b'*') {
                                                                                                        __pos += 1;
                                                                                                    } else {
                                                                                                        break;
                                                                                                    }
                                                                                                }
                                                                                                state.offset = __pos;
                                                                                            }
                                                                                            Some(())
                                                                                        })();
                                                                                        if __ok.is_none() {
                                                                                            state.offset = __save;
                                                                                            break;
                                                                                        }
                                                                                        if state.offset == __save {
                                                                                            break;
                                                                                        }
                                                                                        __rep_count += 1;
                                                                                    }
                                                                                }
                                                                                {
                                                                                    let mut __rep_count: u32 = 0;
                                                                                    loop {
                                                                                        let __save = state.offset;
                                                                                        let __ok = (|| -> Option<()> {
                                                                                            if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                            {
                                                                                                return None;
                                                                                            }
                                                                                            state.offset += 1;
                                                                                            Some(())
                                                                                        })();
                                                                                        if __ok.is_none() {
                                                                                            state.offset = __save;
                                                                                            break;
                                                                                        }
                                                                                        if state.offset == __save {
                                                                                            break;
                                                                                        }
                                                                                        __rep_count += 1;
                                                                                    }
                                                                                    if __rep_count < 1 {
                                                                                        return None;
                                                                                    }
                                                                                }
                                                                                if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                                {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                                Some(())
                                                                            })();
                                                                            if __ok.is_none() {
                                                                                state.offset = __save_dispatch;
                                                                                return None;
                                                                            }
                                                                        }
                                                                        _ => {
                                                                            return None;
                                                                        }
                                                                    }
                                                                }
                                                                Some(())
                                                            })();
                                                            if __ok.is_none() {
                                                                state.offset = __save;
                                                                break;
                                                            }
                                                            if state.offset == __save {
                                                                break;
                                                            }
                                                            __rep_count += 1;
                                                        }
                                                    }
                                                    Some(())
                                                })();
                                                if __result.is_some() {
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    state.offset = __start;
                                                    None
                                                }
                                            }
                                                .is_none()
                                            {
                                                return false;
                                            }
                                            let __matched = &state.src[__start..state.offset];
                                            if !__matched.is_empty() {
                                                __builder.text(__matched);
                                            }
                                        };
                                        {
                                            let __start = state.offset;
                                            if {
                                                let __start = state.offset;
                                                loop {
                                                    ::parse_that::scan_ws_block_comments(state);
                                                    if state.offset >= state.src_bytes.len() {
                                                        break;
                                                    }
                                                    let __b = state.src_bytes[state.offset];
                                                    if __b == b'!' || __b == b',' || __b == b';' || __b == b'{'
                                                        || __b == b'}'
                                                    {
                                                        break;
                                                    }
                                                    state.offset += 1;
                                                }
                                                if state.offset > __start {
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    None
                                                }
                                            }
                                                .is_none()
                                            {
                                                return false;
                                            }
                                            let __matched = &state.src[__start..state.offset];
                                            if !__matched.is_empty() {
                                                __builder.text(__matched);
                                            }
                                        };
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp0;
                                    __builder.restore(__pretty_bcp1);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp3;
                                break;
                            }
                            if state.offset == __rep_cp3 {
                                break;
                            }
                            __rep_count2 += 1;
                        }
                    };
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b';') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b';');
                    };
                };
                true
            }
        }
        pub fn importRule_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__importRule_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __declaration_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            let __pretty_ok = {
                {
                    {
                        {
                            let __start = state.offset;
                            if {
                                let __start = state.offset;
                                let __result: Option<()> = (|| {
                                    {
                                        let __save_alt = state.offset;
                                        let __alt_ok = (|| -> Option<()> {
                                            {
                                                let __b = *state.src_bytes.get(state.offset)?;
                                                if !(((__b >= b'A' && __b <= b'Z') || __b == b'_'
                                                    || (__b >= b'a' && __b <= b'z')))
                                                {
                                                    return None;
                                                }
                                                state.offset += 1;
                                            }
                                            {
                                                let __end = state.src_bytes.len();
                                                let mut __pos = state.offset;
                                                while __pos < __end {
                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                    if (__b == b'-' || (__b >= b'0' && __b <= b'9')
                                                        || (__b >= b'A' && __b <= b'Z') || __b == b'_'
                                                        || (__b >= b'a' && __b <= b'z'))
                                                    {
                                                        __pos += 1;
                                                    } else {
                                                        break;
                                                    }
                                                }
                                                state.offset = __pos;
                                            }
                                            Some(())
                                        })();
                                        let __alt_ok = if __alt_ok.is_none() {
                                            state.offset = __save_alt;
                                            (|| -> Option<()> {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                                {
                                                    return None;
                                                }
                                                state.offset += 1;
                                                if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                                {
                                                    return None;
                                                }
                                                state.offset += 1;
                                                {
                                                    let __loop_start = state.offset;
                                                    let __end = state.src_bytes.len();
                                                    let mut __pos = state.offset;
                                                    while __pos < __end {
                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                        if (__b == b'-' || (__b >= b'0' && __b <= b'9')
                                                            || (__b >= b'A' && __b <= b'Z') || __b == b'_'
                                                            || (__b >= b'a' && __b <= b'z'))
                                                        {
                                                            __pos += 1;
                                                        } else {
                                                            break;
                                                        }
                                                    }
                                                    if __pos < __loop_start + 1 as usize {
                                                        return None;
                                                    }
                                                    state.offset = __pos;
                                                }
                                                Some(())
                                            })()
                                        } else {
                                            __alt_ok
                                        };
                                        let __alt_ok = if __alt_ok.is_none() {
                                            state.offset = __save_alt;
                                            (|| -> Option<()> {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                                {
                                                    return None;
                                                }
                                                state.offset += 1;
                                                {
                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                    if !(__b.is_ascii_alphabetic()) {
                                                        return None;
                                                    }
                                                    state.offset += 1;
                                                }
                                                {
                                                    let __end = state.src_bytes.len();
                                                    let mut __pos = state.offset;
                                                    while __pos < __end {
                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                        if (__b == b'-' || (__b >= b'0' && __b <= b'9')
                                                            || (__b >= b'A' && __b <= b'Z') || __b == b'_'
                                                            || (__b >= b'a' && __b <= b'z'))
                                                        {
                                                            __pos += 1;
                                                        } else {
                                                            break;
                                                        }
                                                    }
                                                    state.offset = __pos;
                                                }
                                                Some(())
                                            })()
                                        } else {
                                            __alt_ok
                                        };
                                        if __alt_ok.is_none() {
                                            return None;
                                        }
                                    }
                                    Some(())
                                })();
                                if __result.is_some() && state.offset > __start {
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    state.offset = __start;
                                    None
                                }
                            }
                                .is_none()
                            {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        {
                            let __start = state.offset;
                            if {
                                let __start = state.offset;
                                let __result: Option<()> = (|| {
                                    {
                                        let mut __rep_count: u32 = 0;
                                        loop {
                                            let __save = state.offset;
                                            let __ok = (|| -> Option<()> {
                                                {
                                                    let __save_dispatch = state.offset;
                                                    let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                    match __dispatch_b {
                                                        b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' => {
                                                            let __ok = (|| -> Option<()> {
                                                                {
                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                    if !(__b.is_ascii_whitespace()) {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                }
                                                                Some(())
                                                            })();
                                                            if __ok.is_none() {
                                                                state.offset = __save_dispatch;
                                                                return None;
                                                            }
                                                        }
                                                        b'/' => {
                                                            let __ok = (|| -> Option<()> {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                                {
                                                                    let __end = state.src_bytes.len();
                                                                    let mut __pos = state.offset;
                                                                    while __pos < __end {
                                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                        if !(__b == b'*') {
                                                                            __pos += 1;
                                                                        } else {
                                                                            break;
                                                                        }
                                                                    }
                                                                    state.offset = __pos;
                                                                }
                                                                {
                                                                    let mut __rep_count: u32 = 0;
                                                                    loop {
                                                                        let __save = state.offset;
                                                                        let __ok = (|| -> Option<()> {
                                                                            {
                                                                                let mut __rep_count: u32 = 0;
                                                                                loop {
                                                                                    let __save = state.offset;
                                                                                    let __ok = (|| -> Option<()> {
                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                        {
                                                                                            return None;
                                                                                        }
                                                                                        state.offset += 1;
                                                                                        Some(())
                                                                                    })();
                                                                                    if __ok.is_none() {
                                                                                        state.offset = __save;
                                                                                        break;
                                                                                    }
                                                                                    if state.offset == __save {
                                                                                        break;
                                                                                    }
                                                                                    __rep_count += 1;
                                                                                }
                                                                                if __rep_count < 1 {
                                                                                    return None;
                                                                                }
                                                                            }
                                                                            {
                                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                                if !(!(__b == b'/')) {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                            }
                                                                            {
                                                                                let __end = state.src_bytes.len();
                                                                                let mut __pos = state.offset;
                                                                                while __pos < __end {
                                                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                    if !(__b == b'*') {
                                                                                        __pos += 1;
                                                                                    } else {
                                                                                        break;
                                                                                    }
                                                                                }
                                                                                state.offset = __pos;
                                                                            }
                                                                            Some(())
                                                                        })();
                                                                        if __ok.is_none() {
                                                                            state.offset = __save;
                                                                            break;
                                                                        }
                                                                        if state.offset == __save {
                                                                            break;
                                                                        }
                                                                        __rep_count += 1;
                                                                    }
                                                                }
                                                                {
                                                                    let mut __rep_count: u32 = 0;
                                                                    loop {
                                                                        let __save = state.offset;
                                                                        let __ok = (|| -> Option<()> {
                                                                            if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                            {
                                                                                return None;
                                                                            }
                                                                            state.offset += 1;
                                                                            Some(())
                                                                        })();
                                                                        if __ok.is_none() {
                                                                            state.offset = __save;
                                                                            break;
                                                                        }
                                                                        if state.offset == __save {
                                                                            break;
                                                                        }
                                                                        __rep_count += 1;
                                                                    }
                                                                    if __rep_count < 1 {
                                                                        return None;
                                                                    }
                                                                }
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                                Some(())
                                                            })();
                                                            if __ok.is_none() {
                                                                state.offset = __save_dispatch;
                                                                return None;
                                                            }
                                                        }
                                                        _ => {
                                                            return None;
                                                        }
                                                    }
                                                }
                                                Some(())
                                            })();
                                            if __ok.is_none() {
                                                state.offset = __save;
                                                break;
                                            }
                                            if state.offset == __save {
                                                break;
                                            }
                                            __rep_count += 1;
                                        }
                                    }
                                    Some(())
                                })();
                                if __result.is_some() {
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    state.offset = __start;
                                    None
                                }
                            }
                                .is_none()
                            {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b':') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b':');
                        };
                        {
                            let __start = state.offset;
                            if {
                                let __start = state.offset;
                                let __result: Option<()> = (|| {
                                    {
                                        let mut __rep_count: u32 = 0;
                                        loop {
                                            let __save = state.offset;
                                            let __ok = (|| -> Option<()> {
                                                {
                                                    let __save_dispatch = state.offset;
                                                    let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                    match __dispatch_b {
                                                        b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' => {
                                                            let __ok = (|| -> Option<()> {
                                                                {
                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                    if !(__b.is_ascii_whitespace()) {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                }
                                                                Some(())
                                                            })();
                                                            if __ok.is_none() {
                                                                state.offset = __save_dispatch;
                                                                return None;
                                                            }
                                                        }
                                                        b'/' => {
                                                            let __ok = (|| -> Option<()> {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                                {
                                                                    let __end = state.src_bytes.len();
                                                                    let mut __pos = state.offset;
                                                                    while __pos < __end {
                                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                        if !(__b == b'*') {
                                                                            __pos += 1;
                                                                        } else {
                                                                            break;
                                                                        }
                                                                    }
                                                                    state.offset = __pos;
                                                                }
                                                                {
                                                                    let mut __rep_count: u32 = 0;
                                                                    loop {
                                                                        let __save = state.offset;
                                                                        let __ok = (|| -> Option<()> {
                                                                            {
                                                                                let mut __rep_count: u32 = 0;
                                                                                loop {
                                                                                    let __save = state.offset;
                                                                                    let __ok = (|| -> Option<()> {
                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                        {
                                                                                            return None;
                                                                                        }
                                                                                        state.offset += 1;
                                                                                        Some(())
                                                                                    })();
                                                                                    if __ok.is_none() {
                                                                                        state.offset = __save;
                                                                                        break;
                                                                                    }
                                                                                    if state.offset == __save {
                                                                                        break;
                                                                                    }
                                                                                    __rep_count += 1;
                                                                                }
                                                                                if __rep_count < 1 {
                                                                                    return None;
                                                                                }
                                                                            }
                                                                            {
                                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                                if !(!(__b == b'/')) {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                            }
                                                                            {
                                                                                let __end = state.src_bytes.len();
                                                                                let mut __pos = state.offset;
                                                                                while __pos < __end {
                                                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                    if !(__b == b'*') {
                                                                                        __pos += 1;
                                                                                    } else {
                                                                                        break;
                                                                                    }
                                                                                }
                                                                                state.offset = __pos;
                                                                            }
                                                                            Some(())
                                                                        })();
                                                                        if __ok.is_none() {
                                                                            state.offset = __save;
                                                                            break;
                                                                        }
                                                                        if state.offset == __save {
                                                                            break;
                                                                        }
                                                                        __rep_count += 1;
                                                                    }
                                                                }
                                                                {
                                                                    let mut __rep_count: u32 = 0;
                                                                    loop {
                                                                        let __save = state.offset;
                                                                        let __ok = (|| -> Option<()> {
                                                                            if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                            {
                                                                                return None;
                                                                            }
                                                                            state.offset += 1;
                                                                            Some(())
                                                                        })();
                                                                        if __ok.is_none() {
                                                                            state.offset = __save;
                                                                            break;
                                                                        }
                                                                        if state.offset == __save {
                                                                            break;
                                                                        }
                                                                        __rep_count += 1;
                                                                    }
                                                                    if __rep_count < 1 {
                                                                        return None;
                                                                    }
                                                                }
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                                Some(())
                                                            })();
                                                            if __ok.is_none() {
                                                                state.offset = __save_dispatch;
                                                                return None;
                                                            }
                                                        }
                                                        _ => {
                                                            return None;
                                                        }
                                                    }
                                                }
                                                Some(())
                                            })();
                                            if __ok.is_none() {
                                                state.offset = __save;
                                                break;
                                            }
                                            if state.offset == __save {
                                                break;
                                            }
                                            __rep_count += 1;
                                        }
                                    }
                                    Some(())
                                })();
                                if __result.is_some() {
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    state.offset = __start;
                                    None
                                }
                            }
                                .is_none()
                            {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        {
                            let _ = {
                                let __pretty_cp8 = state.offset;
                                let __pretty_bcp9 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            let __start = state.offset;
                                            if {
                                                let __start = state.offset;
                                                loop {
                                                    ::parse_that::scan_ws_block_comments(state);
                                                    if state.offset >= state.src_bytes.len() {
                                                        break;
                                                    }
                                                    let __b = state.src_bytes[state.offset];
                                                    if __b == b'!' || __b == b',' || __b == b';' || __b == b'{'
                                                        || __b == b'}'
                                                    {
                                                        break;
                                                    }
                                                    state.offset += 1;
                                                }
                                                if state.offset > __start {
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    None
                                                }
                                            }
                                                .is_none()
                                            {
                                                return false;
                                            }
                                            let __matched = &state.src[__start..state.offset];
                                            if !__matched.is_empty() {
                                                __builder.text(__matched);
                                            }
                                        };
                                        {
                                            let mut __rep_count6 = 0usize;
                                            while __rep_count6 < 4294967295 {
                                                let __rep_cp7 = state.offset;
                                                if !{
                                                    let __pretty_cp4 = state.offset;
                                                    let __pretty_bcp5 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        {
                                                            {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                {
                                                                    return false;
                                                                }
                                                                state.offset += 1;
                                                                __builder.char(b',');
                                                            };
                                                            {
                                                                let __start = state.offset;
                                                                if {
                                                                    let __start = state.offset;
                                                                    let __result: Option<()> = (|| {
                                                                        {
                                                                            let mut __rep_count: u32 = 0;
                                                                            loop {
                                                                                let __save = state.offset;
                                                                                let __ok = (|| -> Option<()> {
                                                                                    {
                                                                                        let __save_dispatch = state.offset;
                                                                                        let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                                                        match __dispatch_b {
                                                                                            b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' => {
                                                                                                let __ok = (|| -> Option<()> {
                                                                                                    {
                                                                                                        let __b = *state.src_bytes.get(state.offset)?;
                                                                                                        if !(__b.is_ascii_whitespace()) {
                                                                                                            return None;
                                                                                                        }
                                                                                                        state.offset += 1;
                                                                                                    }
                                                                                                    Some(())
                                                                                                })();
                                                                                                if __ok.is_none() {
                                                                                                    state.offset = __save_dispatch;
                                                                                                    return None;
                                                                                                }
                                                                                            }
                                                                                            b'/' => {
                                                                                                let __ok = (|| -> Option<()> {
                                                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                                                    {
                                                                                                        return None;
                                                                                                    }
                                                                                                    state.offset += 1;
                                                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                                    {
                                                                                                        return None;
                                                                                                    }
                                                                                                    state.offset += 1;
                                                                                                    {
                                                                                                        let __end = state.src_bytes.len();
                                                                                                        let mut __pos = state.offset;
                                                                                                        while __pos < __end {
                                                                                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                                            if !(__b == b'*') {
                                                                                                                __pos += 1;
                                                                                                            } else {
                                                                                                                break;
                                                                                                            }
                                                                                                        }
                                                                                                        state.offset = __pos;
                                                                                                    }
                                                                                                    {
                                                                                                        let mut __rep_count: u32 = 0;
                                                                                                        loop {
                                                                                                            let __save = state.offset;
                                                                                                            let __ok = (|| -> Option<()> {
                                                                                                                {
                                                                                                                    let mut __rep_count: u32 = 0;
                                                                                                                    loop {
                                                                                                                        let __save = state.offset;
                                                                                                                        let __ok = (|| -> Option<()> {
                                                                                                                            if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                                                            {
                                                                                                                                return None;
                                                                                                                            }
                                                                                                                            state.offset += 1;
                                                                                                                            Some(())
                                                                                                                        })();
                                                                                                                        if __ok.is_none() {
                                                                                                                            state.offset = __save;
                                                                                                                            break;
                                                                                                                        }
                                                                                                                        if state.offset == __save {
                                                                                                                            break;
                                                                                                                        }
                                                                                                                        __rep_count += 1;
                                                                                                                    }
                                                                                                                    if __rep_count < 1 {
                                                                                                                        return None;
                                                                                                                    }
                                                                                                                }
                                                                                                                {
                                                                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                                                                    if !(!(__b == b'/')) {
                                                                                                                        return None;
                                                                                                                    }
                                                                                                                    state.offset += 1;
                                                                                                                }
                                                                                                                {
                                                                                                                    let __end = state.src_bytes.len();
                                                                                                                    let mut __pos = state.offset;
                                                                                                                    while __pos < __end {
                                                                                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                                                        if !(__b == b'*') {
                                                                                                                            __pos += 1;
                                                                                                                        } else {
                                                                                                                            break;
                                                                                                                        }
                                                                                                                    }
                                                                                                                    state.offset = __pos;
                                                                                                                }
                                                                                                                Some(())
                                                                                                            })();
                                                                                                            if __ok.is_none() {
                                                                                                                state.offset = __save;
                                                                                                                break;
                                                                                                            }
                                                                                                            if state.offset == __save {
                                                                                                                break;
                                                                                                            }
                                                                                                            __rep_count += 1;
                                                                                                        }
                                                                                                    }
                                                                                                    {
                                                                                                        let mut __rep_count: u32 = 0;
                                                                                                        loop {
                                                                                                            let __save = state.offset;
                                                                                                            let __ok = (|| -> Option<()> {
                                                                                                                if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                                                {
                                                                                                                    return None;
                                                                                                                }
                                                                                                                state.offset += 1;
                                                                                                                Some(())
                                                                                                            })();
                                                                                                            if __ok.is_none() {
                                                                                                                state.offset = __save;
                                                                                                                break;
                                                                                                            }
                                                                                                            if state.offset == __save {
                                                                                                                break;
                                                                                                            }
                                                                                                            __rep_count += 1;
                                                                                                        }
                                                                                                        if __rep_count < 1 {
                                                                                                            return None;
                                                                                                        }
                                                                                                    }
                                                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                                                    {
                                                                                                        return None;
                                                                                                    }
                                                                                                    state.offset += 1;
                                                                                                    Some(())
                                                                                                })();
                                                                                                if __ok.is_none() {
                                                                                                    state.offset = __save_dispatch;
                                                                                                    return None;
                                                                                                }
                                                                                            }
                                                                                            _ => {
                                                                                                return None;
                                                                                            }
                                                                                        }
                                                                                    }
                                                                                    Some(())
                                                                                })();
                                                                                if __ok.is_none() {
                                                                                    state.offset = __save;
                                                                                    break;
                                                                                }
                                                                                if state.offset == __save {
                                                                                    break;
                                                                                }
                                                                                __rep_count += 1;
                                                                            }
                                                                        }
                                                                        Some(())
                                                                    })();
                                                                    if __result.is_some() {
                                                                        Some(
                                                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                                                        )
                                                                    } else {
                                                                        state.offset = __start;
                                                                        None
                                                                    }
                                                                }
                                                                    .is_none()
                                                                {
                                                                    return false;
                                                                }
                                                                let __matched = &state.src[__start..state.offset];
                                                                if !__matched.is_empty() {
                                                                    __builder.text(__matched);
                                                                }
                                                            };
                                                            {
                                                                let __start = state.offset;
                                                                if {
                                                                    let __start = state.offset;
                                                                    loop {
                                                                        ::parse_that::scan_ws_block_comments(state);
                                                                        if state.offset >= state.src_bytes.len() {
                                                                            break;
                                                                        }
                                                                        let __b = state.src_bytes[state.offset];
                                                                        if __b == b'!' || __b == b',' || __b == b';' || __b == b'{'
                                                                            || __b == b'}'
                                                                        {
                                                                            break;
                                                                        }
                                                                        state.offset += 1;
                                                                    }
                                                                    if state.offset > __start {
                                                                        Some(
                                                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                                                        )
                                                                    } else {
                                                                        None
                                                                    }
                                                                }
                                                                    .is_none()
                                                                {
                                                                    return false;
                                                                }
                                                                let __matched = &state.src[__start..state.offset];
                                                                if !__matched.is_empty() {
                                                                    __builder.text(__matched);
                                                                }
                                                            };
                                                        };
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp4;
                                                        __builder.restore(__pretty_bcp5);
                                                    }
                                                    __ok
                                                } {
                                                    state.offset = __rep_cp7;
                                                    break;
                                                }
                                                if state.offset == __rep_cp7 {
                                                    break;
                                                }
                                                __rep_count6 += 1;
                                            }
                                        };
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp8;
                                    __builder.restore(__pretty_bcp9);
                                }
                                __ok
                            };
                            true
                        };
                        {
                            let _ = {
                                let __pretty_cp10 = state.offset;
                                let __pretty_bcp11 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'!')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'!');
                                        };
                                        {
                                            let __start = state.offset;
                                            if {
                                                let __start = state.offset;
                                                let __result: Option<()> = (|| {
                                                    {
                                                        let mut __rep_count: u32 = 0;
                                                        loop {
                                                            let __save = state.offset;
                                                            let __ok = (|| -> Option<()> {
                                                                {
                                                                    let __save_dispatch = state.offset;
                                                                    let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                                    match __dispatch_b {
                                                                        b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' => {
                                                                            let __ok = (|| -> Option<()> {
                                                                                {
                                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                                    if !(__b.is_ascii_whitespace()) {
                                                                                        return None;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                }
                                                                                Some(())
                                                                            })();
                                                                            if __ok.is_none() {
                                                                                state.offset = __save_dispatch;
                                                                                return None;
                                                                            }
                                                                        }
                                                                        b'/' => {
                                                                            let __ok = (|| -> Option<()> {
                                                                                if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                                {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                                if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                                {
                                                                                    let __end = state.src_bytes.len();
                                                                                    let mut __pos = state.offset;
                                                                                    while __pos < __end {
                                                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                        if !(__b == b'*') {
                                                                                            __pos += 1;
                                                                                        } else {
                                                                                            break;
                                                                                        }
                                                                                    }
                                                                                    state.offset = __pos;
                                                                                }
                                                                                {
                                                                                    let mut __rep_count: u32 = 0;
                                                                                    loop {
                                                                                        let __save = state.offset;
                                                                                        let __ok = (|| -> Option<()> {
                                                                                            {
                                                                                                let mut __rep_count: u32 = 0;
                                                                                                loop {
                                                                                                    let __save = state.offset;
                                                                                                    let __ok = (|| -> Option<()> {
                                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                                        {
                                                                                                            return None;
                                                                                                        }
                                                                                                        state.offset += 1;
                                                                                                        Some(())
                                                                                                    })();
                                                                                                    if __ok.is_none() {
                                                                                                        state.offset = __save;
                                                                                                        break;
                                                                                                    }
                                                                                                    if state.offset == __save {
                                                                                                        break;
                                                                                                    }
                                                                                                    __rep_count += 1;
                                                                                                }
                                                                                                if __rep_count < 1 {
                                                                                                    return None;
                                                                                                }
                                                                                            }
                                                                                            {
                                                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                                                if !(!(__b == b'/')) {
                                                                                                    return None;
                                                                                                }
                                                                                                state.offset += 1;
                                                                                            }
                                                                                            {
                                                                                                let __end = state.src_bytes.len();
                                                                                                let mut __pos = state.offset;
                                                                                                while __pos < __end {
                                                                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                                    if !(__b == b'*') {
                                                                                                        __pos += 1;
                                                                                                    } else {
                                                                                                        break;
                                                                                                    }
                                                                                                }
                                                                                                state.offset = __pos;
                                                                                            }
                                                                                            Some(())
                                                                                        })();
                                                                                        if __ok.is_none() {
                                                                                            state.offset = __save;
                                                                                            break;
                                                                                        }
                                                                                        if state.offset == __save {
                                                                                            break;
                                                                                        }
                                                                                        __rep_count += 1;
                                                                                    }
                                                                                }
                                                                                {
                                                                                    let mut __rep_count: u32 = 0;
                                                                                    loop {
                                                                                        let __save = state.offset;
                                                                                        let __ok = (|| -> Option<()> {
                                                                                            if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                            {
                                                                                                return None;
                                                                                            }
                                                                                            state.offset += 1;
                                                                                            Some(())
                                                                                        })();
                                                                                        if __ok.is_none() {
                                                                                            state.offset = __save;
                                                                                            break;
                                                                                        }
                                                                                        if state.offset == __save {
                                                                                            break;
                                                                                        }
                                                                                        __rep_count += 1;
                                                                                    }
                                                                                    if __rep_count < 1 {
                                                                                        return None;
                                                                                    }
                                                                                }
                                                                                if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                                {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                                Some(())
                                                                            })();
                                                                            if __ok.is_none() {
                                                                                state.offset = __save_dispatch;
                                                                                return None;
                                                                            }
                                                                        }
                                                                        _ => {
                                                                            return None;
                                                                        }
                                                                    }
                                                                }
                                                                Some(())
                                                            })();
                                                            if __ok.is_none() {
                                                                state.offset = __save;
                                                                break;
                                                            }
                                                            if state.offset == __save {
                                                                break;
                                                            }
                                                            __rep_count += 1;
                                                        }
                                                    }
                                                    Some(())
                                                })();
                                                if __result.is_some() {
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    state.offset = __start;
                                                    None
                                                }
                                            }
                                                .is_none()
                                            {
                                                return false;
                                            }
                                            let __matched = &state.src[__start..state.offset];
                                            if !__matched.is_empty() {
                                                __builder.text(__matched);
                                            }
                                        };
                                        {
                                            let __s = "important";
                                            let __bytes = __s.as_bytes();
                                            let __slc = match state.src_bytes.get(state.offset..) {
                                                Some(s) if s.len() >= 9usize => s,
                                                _ => return false,
                                            };
                                            if &__slc[..9usize] != __bytes {
                                                return false;
                                            }
                                            __builder
                                                .text(&state.src[state.offset..state.offset + 9usize]);
                                            state.offset += 9usize;
                                        };
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp10;
                                    __builder.restore(__pretty_bcp11);
                                }
                                __ok
                            };
                            true
                        };
                        {
                            {
                                let _ = {
                                    let __pretty_cp12 = state.offset;
                                    let __pretty_bcp13 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b';')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b';');
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp12;
                                        __builder.restore(__pretty_bcp13);
                                    }
                                    __ok
                                };
                                true
                            };
                            {
                                let __start = state.offset;
                                if {
                                    let __start = state.offset;
                                    let __result: Option<()> = (|| {
                                        {
                                            let mut __rep_count: u32 = 0;
                                            loop {
                                                let __save = state.offset;
                                                let __ok = (|| -> Option<()> {
                                                    {
                                                        let __save_dispatch = state.offset;
                                                        let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                        match __dispatch_b {
                                                            b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' => {
                                                                let __ok = (|| -> Option<()> {
                                                                    {
                                                                        let __b = *state.src_bytes.get(state.offset)?;
                                                                        if !(__b.is_ascii_whitespace()) {
                                                                            return None;
                                                                        }
                                                                        state.offset += 1;
                                                                    }
                                                                    Some(())
                                                                })();
                                                                if __ok.is_none() {
                                                                    state.offset = __save_dispatch;
                                                                    return None;
                                                                }
                                                            }
                                                            b'/' => {
                                                                let __ok = (|| -> Option<()> {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                    {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                    {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                    {
                                                                        let __end = state.src_bytes.len();
                                                                        let mut __pos = state.offset;
                                                                        while __pos < __end {
                                                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                            if !(__b == b'*') {
                                                                                __pos += 1;
                                                                            } else {
                                                                                break;
                                                                            }
                                                                        }
                                                                        state.offset = __pos;
                                                                    }
                                                                    {
                                                                        let mut __rep_count: u32 = 0;
                                                                        loop {
                                                                            let __save = state.offset;
                                                                            let __ok = (|| -> Option<()> {
                                                                                {
                                                                                    let mut __rep_count: u32 = 0;
                                                                                    loop {
                                                                                        let __save = state.offset;
                                                                                        let __ok = (|| -> Option<()> {
                                                                                            if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                            {
                                                                                                return None;
                                                                                            }
                                                                                            state.offset += 1;
                                                                                            Some(())
                                                                                        })();
                                                                                        if __ok.is_none() {
                                                                                            state.offset = __save;
                                                                                            break;
                                                                                        }
                                                                                        if state.offset == __save {
                                                                                            break;
                                                                                        }
                                                                                        __rep_count += 1;
                                                                                    }
                                                                                    if __rep_count < 1 {
                                                                                        return None;
                                                                                    }
                                                                                }
                                                                                {
                                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                                    if !(!(__b == b'/')) {
                                                                                        return None;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                }
                                                                                {
                                                                                    let __end = state.src_bytes.len();
                                                                                    let mut __pos = state.offset;
                                                                                    while __pos < __end {
                                                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                        if !(__b == b'*') {
                                                                                            __pos += 1;
                                                                                        } else {
                                                                                            break;
                                                                                        }
                                                                                    }
                                                                                    state.offset = __pos;
                                                                                }
                                                                                Some(())
                                                                            })();
                                                                            if __ok.is_none() {
                                                                                state.offset = __save;
                                                                                break;
                                                                            }
                                                                            if state.offset == __save {
                                                                                break;
                                                                            }
                                                                            __rep_count += 1;
                                                                        }
                                                                    }
                                                                    {
                                                                        let mut __rep_count: u32 = 0;
                                                                        loop {
                                                                            let __save = state.offset;
                                                                            let __ok = (|| -> Option<()> {
                                                                                if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                                Some(())
                                                                            })();
                                                                            if __ok.is_none() {
                                                                                state.offset = __save;
                                                                                break;
                                                                            }
                                                                            if state.offset == __save {
                                                                                break;
                                                                            }
                                                                            __rep_count += 1;
                                                                        }
                                                                        if __rep_count < 1 {
                                                                            return None;
                                                                        }
                                                                    }
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                    {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                    Some(())
                                                                })();
                                                                if __ok.is_none() {
                                                                    state.offset = __save_dispatch;
                                                                    return None;
                                                                }
                                                            }
                                                            _ => {
                                                                return None;
                                                            }
                                                        }
                                                    }
                                                    Some(())
                                                })();
                                                if __ok.is_none() {
                                                    state.offset = __save;
                                                    break;
                                                }
                                                if state.offset == __save {
                                                    break;
                                                }
                                                __rep_count += 1;
                                            }
                                        }
                                        Some(())
                                    })();
                                    if __result.is_some() {
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        state.offset = __start;
                                        None
                                    }
                                }
                                    .is_none()
                                {
                                    return false;
                                }
                                let __matched = &state.src[__start..state.offset];
                                if !__matched.is_empty() {
                                    __builder.text(__matched);
                                }
                            };
                        };
                    };
                    true
                }
            };
            __builder.group_close();
            __pretty_ok
        }
        pub fn declaration_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__declaration_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __genericAtRule_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __start = state.offset;
                        if {
                            let __start = state.offset;
                            let __result: Option<()> = (|| {
                                if state.src_bytes.get(state.offset).copied() != Some(b'@')
                                {
                                    return None;
                                }
                                state.offset += 1;
                                {
                                    let __b = *state.src_bytes.get(state.offset)?;
                                    if !(__b.is_ascii_alphabetic()) {
                                        return None;
                                    }
                                    state.offset += 1;
                                }
                                {
                                    let __end = state.src_bytes.len();
                                    let mut __pos = state.offset;
                                    while __pos < __end {
                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                        if (__b == b'-' || (__b >= b'0' && __b <= b'9')
                                            || (__b >= b'A' && __b <= b'Z') || __b == b'_'
                                            || (__b >= b'a' && __b <= b'z'))
                                        {
                                            __pos += 1;
                                        } else {
                                            break;
                                        }
                                    }
                                    state.offset = __pos;
                                }
                                Some(())
                            })();
                            if __result.is_some() && state.offset > __start {
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                state.offset = __start;
                                None
                            }
                        }
                            .is_none()
                        {
                            return false;
                        }
                        let __matched = &state.src[__start..state.offset];
                        if !__matched.is_empty() {
                            __builder.text(__matched);
                        }
                    };
                    {
                        let __start = state.offset;
                        if {
                            let __start = state.offset;
                            loop {
                                ::parse_that::scan_ws_block_comments(state);
                                if state.offset >= state.src_bytes.len() {
                                    break;
                                }
                                let __b = state.src_bytes[state.offset];
                                if __b == b';' || __b == b'{' || __b == b'}' {
                                    break;
                                }
                                state.offset += 1;
                            }
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        }
                            .is_none()
                        {
                            return false;
                        }
                        let __matched = &state.src[__start..state.offset];
                        if !__matched.is_empty() {
                            __builder.text(__matched);
                        }
                    };
                    if !Self::__atRuleBody_prettify(state, __builder) {
                        return false;
                    }
                };
                true
            }
        }
        pub fn genericAtRule_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__genericAtRule_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __qualifiedRule_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            __builder.indent_open();
            let __pretty_ok = {
                {
                    {
                        {
                            let __start = state.offset;
                            if {
                                let __start = state.offset;
                                loop {
                                    ::parse_that::scan_ws_block_comments(state);
                                    if state.offset >= state.src_bytes.len() {
                                        break;
                                    }
                                    let __b = state.src_bytes[state.offset];
                                    if __b == b';' || __b == b'{' || __b == b'}' {
                                        break;
                                    }
                                    state.offset += 1;
                                }
                                if state.offset > __start {
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                }
                            }
                                .is_none()
                            {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        {
                            let __start = state.offset;
                            if {
                                let __start = state.offset;
                                let __result: Option<()> = (|| {
                                    {
                                        let mut __rep_count: u32 = 0;
                                        loop {
                                            let __save = state.offset;
                                            let __ok = (|| -> Option<()> {
                                                {
                                                    let __save_dispatch = state.offset;
                                                    let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                    match __dispatch_b {
                                                        b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' => {
                                                            let __ok = (|| -> Option<()> {
                                                                {
                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                    if !(__b.is_ascii_whitespace()) {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                }
                                                                Some(())
                                                            })();
                                                            if __ok.is_none() {
                                                                state.offset = __save_dispatch;
                                                                return None;
                                                            }
                                                        }
                                                        b'/' => {
                                                            let __ok = (|| -> Option<()> {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                                {
                                                                    let __end = state.src_bytes.len();
                                                                    let mut __pos = state.offset;
                                                                    while __pos < __end {
                                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                        if !(__b == b'*') {
                                                                            __pos += 1;
                                                                        } else {
                                                                            break;
                                                                        }
                                                                    }
                                                                    state.offset = __pos;
                                                                }
                                                                {
                                                                    let mut __rep_count: u32 = 0;
                                                                    loop {
                                                                        let __save = state.offset;
                                                                        let __ok = (|| -> Option<()> {
                                                                            {
                                                                                let mut __rep_count: u32 = 0;
                                                                                loop {
                                                                                    let __save = state.offset;
                                                                                    let __ok = (|| -> Option<()> {
                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                        {
                                                                                            return None;
                                                                                        }
                                                                                        state.offset += 1;
                                                                                        Some(())
                                                                                    })();
                                                                                    if __ok.is_none() {
                                                                                        state.offset = __save;
                                                                                        break;
                                                                                    }
                                                                                    if state.offset == __save {
                                                                                        break;
                                                                                    }
                                                                                    __rep_count += 1;
                                                                                }
                                                                                if __rep_count < 1 {
                                                                                    return None;
                                                                                }
                                                                            }
                                                                            {
                                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                                if !(!(__b == b'/')) {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                            }
                                                                            {
                                                                                let __end = state.src_bytes.len();
                                                                                let mut __pos = state.offset;
                                                                                while __pos < __end {
                                                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                    if !(__b == b'*') {
                                                                                        __pos += 1;
                                                                                    } else {
                                                                                        break;
                                                                                    }
                                                                                }
                                                                                state.offset = __pos;
                                                                            }
                                                                            Some(())
                                                                        })();
                                                                        if __ok.is_none() {
                                                                            state.offset = __save;
                                                                            break;
                                                                        }
                                                                        if state.offset == __save {
                                                                            break;
                                                                        }
                                                                        __rep_count += 1;
                                                                    }
                                                                }
                                                                {
                                                                    let mut __rep_count: u32 = 0;
                                                                    loop {
                                                                        let __save = state.offset;
                                                                        let __ok = (|| -> Option<()> {
                                                                            if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                            {
                                                                                return None;
                                                                            }
                                                                            state.offset += 1;
                                                                            Some(())
                                                                        })();
                                                                        if __ok.is_none() {
                                                                            state.offset = __save;
                                                                            break;
                                                                        }
                                                                        if state.offset == __save {
                                                                            break;
                                                                        }
                                                                        __rep_count += 1;
                                                                    }
                                                                    if __rep_count < 1 {
                                                                        return None;
                                                                    }
                                                                }
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                                Some(())
                                                            })();
                                                            if __ok.is_none() {
                                                                state.offset = __save_dispatch;
                                                                return None;
                                                            }
                                                        }
                                                        _ => {
                                                            return None;
                                                        }
                                                    }
                                                }
                                                Some(())
                                            })();
                                            if __ok.is_none() {
                                                state.offset = __save;
                                                break;
                                            }
                                            if state.offset == __save {
                                                break;
                                            }
                                            __rep_count += 1;
                                        }
                                    }
                                    Some(())
                                })();
                                if __result.is_some() {
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    state.offset = __start;
                                    None
                                }
                            }
                                .is_none()
                            {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        if !Self::__ruleBlock_prettify(state, __builder) {
                            return false;
                        }
                    };
                    true
                }
            };
            __builder.indent_close();
            __builder.group_close();
            __pretty_ok
        }
        pub fn qualifiedRule_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__qualifiedRule_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __mediaRule_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __s = "@media";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 6usize => s,
                            _ => return false,
                        };
                        if &__slc[..6usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 6usize]);
                        state.offset += 6usize;
                    };
                    {
                        let __start = state.offset;
                        if {
                            let __start = state.offset;
                            loop {
                                ::parse_that::scan_ws_block_comments(state);
                                if state.offset >= state.src_bytes.len() {
                                    break;
                                }
                                let __b = state.src_bytes[state.offset];
                                if __b == b'{' {
                                    break;
                                }
                                state.offset += 1;
                            }
                            if state.offset > __start {
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            }
                        }
                            .is_none()
                        {
                            return false;
                        }
                        let __matched = &state.src[__start..state.offset];
                        if !__matched.is_empty() {
                            __builder.text(__matched);
                        }
                    };
                    if !Self::__ruleBlock_prettify(state, __builder) {
                        return false;
                    }
                };
                true
            }
        }
        pub fn mediaRule_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__mediaRule_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __supportsRule_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __s = "@supports";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 9usize => s,
                            _ => return false,
                        };
                        if &__slc[..9usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 9usize]);
                        state.offset += 9usize;
                    };
                    {
                        let __start = state.offset;
                        if {
                            let __start = state.offset;
                            loop {
                                ::parse_that::scan_ws_block_comments(state);
                                if state.offset >= state.src_bytes.len() {
                                    break;
                                }
                                let __b = state.src_bytes[state.offset];
                                if __b == b'{' {
                                    break;
                                }
                                state.offset += 1;
                            }
                            if state.offset > __start {
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            }
                        }
                            .is_none()
                        {
                            return false;
                        }
                        let __matched = &state.src[__start..state.offset];
                        if !__matched.is_empty() {
                            __builder.text(__matched);
                        }
                    };
                    if !Self::__ruleBlock_prettify(state, __builder) {
                        return false;
                    }
                };
                true
            }
        }
        pub fn supportsRule_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__supportsRule_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __fontFaceRule_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __s = "@font-face";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 10usize => s,
                            _ => return false,
                        };
                        if &__slc[..10usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 10usize]);
                        state.offset += 10usize;
                    };
                    {
                        let __start = state.offset;
                        if {
                            let __start = state.offset;
                            let __result: Option<()> = (|| {
                                {
                                    let mut __rep_count: u32 = 0;
                                    loop {
                                        let __save = state.offset;
                                        let __ok = (|| -> Option<()> {
                                            {
                                                let __save_dispatch = state.offset;
                                                let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                match __dispatch_b {
                                                    b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' => {
                                                        let __ok = (|| -> Option<()> {
                                                            {
                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                if !(__b.is_ascii_whitespace()) {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                            }
                                                            Some(())
                                                        })();
                                                        if __ok.is_none() {
                                                            state.offset = __save_dispatch;
                                                            return None;
                                                        }
                                                    }
                                                    b'/' => {
                                                        let __ok = (|| -> Option<()> {
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            {
                                                                let __end = state.src_bytes.len();
                                                                let mut __pos = state.offset;
                                                                while __pos < __end {
                                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                    if !(__b == b'*') {
                                                                        __pos += 1;
                                                                    } else {
                                                                        break;
                                                                    }
                                                                }
                                                                state.offset = __pos;
                                                            }
                                                            {
                                                                let mut __rep_count: u32 = 0;
                                                                loop {
                                                                    let __save = state.offset;
                                                                    let __ok = (|| -> Option<()> {
                                                                        {
                                                                            let mut __rep_count: u32 = 0;
                                                                            loop {
                                                                                let __save = state.offset;
                                                                                let __ok = (|| -> Option<()> {
                                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                    {
                                                                                        return None;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                    Some(())
                                                                                })();
                                                                                if __ok.is_none() {
                                                                                    state.offset = __save;
                                                                                    break;
                                                                                }
                                                                                if state.offset == __save {
                                                                                    break;
                                                                                }
                                                                                __rep_count += 1;
                                                                            }
                                                                            if __rep_count < 1 {
                                                                                return None;
                                                                            }
                                                                        }
                                                                        {
                                                                            let __b = *state.src_bytes.get(state.offset)?;
                                                                            if !(!(__b == b'/')) {
                                                                                return None;
                                                                            }
                                                                            state.offset += 1;
                                                                        }
                                                                        {
                                                                            let __end = state.src_bytes.len();
                                                                            let mut __pos = state.offset;
                                                                            while __pos < __end {
                                                                                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                if !(__b == b'*') {
                                                                                    __pos += 1;
                                                                                } else {
                                                                                    break;
                                                                                }
                                                                            }
                                                                            state.offset = __pos;
                                                                        }
                                                                        Some(())
                                                                    })();
                                                                    if __ok.is_none() {
                                                                        state.offset = __save;
                                                                        break;
                                                                    }
                                                                    if state.offset == __save {
                                                                        break;
                                                                    }
                                                                    __rep_count += 1;
                                                                }
                                                            }
                                                            {
                                                                let mut __rep_count: u32 = 0;
                                                                loop {
                                                                    let __save = state.offset;
                                                                    let __ok = (|| -> Option<()> {
                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                        {
                                                                            return None;
                                                                        }
                                                                        state.offset += 1;
                                                                        Some(())
                                                                    })();
                                                                    if __ok.is_none() {
                                                                        state.offset = __save;
                                                                        break;
                                                                    }
                                                                    if state.offset == __save {
                                                                        break;
                                                                    }
                                                                    __rep_count += 1;
                                                                }
                                                                if __rep_count < 1 {
                                                                    return None;
                                                                }
                                                            }
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            Some(())
                                                        })();
                                                        if __ok.is_none() {
                                                            state.offset = __save_dispatch;
                                                            return None;
                                                        }
                                                    }
                                                    _ => {
                                                        return None;
                                                    }
                                                }
                                            }
                                            Some(())
                                        })();
                                        if __ok.is_none() {
                                            state.offset = __save;
                                            break;
                                        }
                                        if state.offset == __save {
                                            break;
                                        }
                                        __rep_count += 1;
                                    }
                                }
                                Some(())
                            })();
                            if __result.is_some() {
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                state.offset = __start;
                                None
                            }
                        }
                            .is_none()
                        {
                            return false;
                        }
                        let __matched = &state.src[__start..state.offset];
                        if !__matched.is_empty() {
                            __builder.text(__matched);
                        }
                    };
                    if !Self::__ruleBlock_prettify(state, __builder) {
                        return false;
                    }
                };
                true
            }
        }
        pub fn fontFaceRule_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__fontFaceRule_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __atRuleBody_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __byte = match state.src_bytes.get(state.offset) {
                        Some(&b) => b,
                        None => return false,
                    };
                    match __byte {
                        b';' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b';')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b';');
                            };
                        }
                        b'{' => {
                            if !Self::__ruleBlock_prettify(state, __builder) {
                                return false;
                            }
                        }
                        _ => {
                            return false;
                        }
                    }
                };
                true
            }
        }
        pub fn atRuleBody_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__atRuleBody_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __ruleBlock_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'{') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'{');
                        };
                        {
                            {
                                {
                                    let __start = state.offset;
                                    if {
                                        let __start = state.offset;
                                        let __result: Option<()> = (|| {
                                            {
                                                let mut __rep_count: u32 = 0;
                                                loop {
                                                    let __save = state.offset;
                                                    let __ok = (|| -> Option<()> {
                                                        {
                                                            let __save_dispatch = state.offset;
                                                            let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                            match __dispatch_b {
                                                                b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' => {
                                                                    let __ok = (|| -> Option<()> {
                                                                        {
                                                                            let __b = *state.src_bytes.get(state.offset)?;
                                                                            if !(__b.is_ascii_whitespace()) {
                                                                                return None;
                                                                            }
                                                                            state.offset += 1;
                                                                        }
                                                                        Some(())
                                                                    })();
                                                                    if __ok.is_none() {
                                                                        state.offset = __save_dispatch;
                                                                        return None;
                                                                    }
                                                                }
                                                                b'/' => {
                                                                    let __ok = (|| -> Option<()> {
                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                        {
                                                                            return None;
                                                                        }
                                                                        state.offset += 1;
                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                        {
                                                                            return None;
                                                                        }
                                                                        state.offset += 1;
                                                                        {
                                                                            let __end = state.src_bytes.len();
                                                                            let mut __pos = state.offset;
                                                                            while __pos < __end {
                                                                                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                if !(__b == b'*') {
                                                                                    __pos += 1;
                                                                                } else {
                                                                                    break;
                                                                                }
                                                                            }
                                                                            state.offset = __pos;
                                                                        }
                                                                        {
                                                                            let mut __rep_count: u32 = 0;
                                                                            loop {
                                                                                let __save = state.offset;
                                                                                let __ok = (|| -> Option<()> {
                                                                                    {
                                                                                        let mut __rep_count: u32 = 0;
                                                                                        loop {
                                                                                            let __save = state.offset;
                                                                                            let __ok = (|| -> Option<()> {
                                                                                                if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                                {
                                                                                                    return None;
                                                                                                }
                                                                                                state.offset += 1;
                                                                                                Some(())
                                                                                            })();
                                                                                            if __ok.is_none() {
                                                                                                state.offset = __save;
                                                                                                break;
                                                                                            }
                                                                                            if state.offset == __save {
                                                                                                break;
                                                                                            }
                                                                                            __rep_count += 1;
                                                                                        }
                                                                                        if __rep_count < 1 {
                                                                                            return None;
                                                                                        }
                                                                                    }
                                                                                    {
                                                                                        let __b = *state.src_bytes.get(state.offset)?;
                                                                                        if !(!(__b == b'/')) {
                                                                                            return None;
                                                                                        }
                                                                                        state.offset += 1;
                                                                                    }
                                                                                    {
                                                                                        let __end = state.src_bytes.len();
                                                                                        let mut __pos = state.offset;
                                                                                        while __pos < __end {
                                                                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                            if !(__b == b'*') {
                                                                                                __pos += 1;
                                                                                            } else {
                                                                                                break;
                                                                                            }
                                                                                        }
                                                                                        state.offset = __pos;
                                                                                    }
                                                                                    Some(())
                                                                                })();
                                                                                if __ok.is_none() {
                                                                                    state.offset = __save;
                                                                                    break;
                                                                                }
                                                                                if state.offset == __save {
                                                                                    break;
                                                                                }
                                                                                __rep_count += 1;
                                                                            }
                                                                        }
                                                                        {
                                                                            let mut __rep_count: u32 = 0;
                                                                            loop {
                                                                                let __save = state.offset;
                                                                                let __ok = (|| -> Option<()> {
                                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                    {
                                                                                        return None;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                    Some(())
                                                                                })();
                                                                                if __ok.is_none() {
                                                                                    state.offset = __save;
                                                                                    break;
                                                                                }
                                                                                if state.offset == __save {
                                                                                    break;
                                                                                }
                                                                                __rep_count += 1;
                                                                            }
                                                                            if __rep_count < 1 {
                                                                                return None;
                                                                            }
                                                                        }
                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                        {
                                                                            return None;
                                                                        }
                                                                        state.offset += 1;
                                                                        Some(())
                                                                    })();
                                                                    if __ok.is_none() {
                                                                        state.offset = __save_dispatch;
                                                                        return None;
                                                                    }
                                                                }
                                                                _ => {
                                                                    return None;
                                                                }
                                                            }
                                                        }
                                                        Some(())
                                                    })();
                                                    if __ok.is_none() {
                                                        state.offset = __save;
                                                        break;
                                                    }
                                                    if state.offset == __save {
                                                        break;
                                                    }
                                                    __rep_count += 1;
                                                }
                                            }
                                            Some(())
                                        })();
                                        if __result.is_some() {
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            state.offset = __start;
                                            None
                                        }
                                    }
                                        .is_none()
                                    {
                                        return false;
                                    }
                                    let __matched = &state.src[__start..state.offset];
                                    if !__matched.is_empty() {
                                        __builder.text(__matched);
                                    }
                                };
                                if !Self::__blockContent_prettify(state, __builder) {
                                    return false;
                                }
                            };
                            {
                                let __start = state.offset;
                                if {
                                    let __start = state.offset;
                                    let __result: Option<()> = (|| {
                                        {
                                            let mut __rep_count: u32 = 0;
                                            loop {
                                                let __save = state.offset;
                                                let __ok = (|| -> Option<()> {
                                                    {
                                                        let __save_dispatch = state.offset;
                                                        let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                        match __dispatch_b {
                                                            b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' => {
                                                                let __ok = (|| -> Option<()> {
                                                                    {
                                                                        let __b = *state.src_bytes.get(state.offset)?;
                                                                        if !(__b.is_ascii_whitespace()) {
                                                                            return None;
                                                                        }
                                                                        state.offset += 1;
                                                                    }
                                                                    Some(())
                                                                })();
                                                                if __ok.is_none() {
                                                                    state.offset = __save_dispatch;
                                                                    return None;
                                                                }
                                                            }
                                                            b'/' => {
                                                                let __ok = (|| -> Option<()> {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                    {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                    {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                    {
                                                                        let __end = state.src_bytes.len();
                                                                        let mut __pos = state.offset;
                                                                        while __pos < __end {
                                                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                            if !(__b == b'*') {
                                                                                __pos += 1;
                                                                            } else {
                                                                                break;
                                                                            }
                                                                        }
                                                                        state.offset = __pos;
                                                                    }
                                                                    {
                                                                        let mut __rep_count: u32 = 0;
                                                                        loop {
                                                                            let __save = state.offset;
                                                                            let __ok = (|| -> Option<()> {
                                                                                {
                                                                                    let mut __rep_count: u32 = 0;
                                                                                    loop {
                                                                                        let __save = state.offset;
                                                                                        let __ok = (|| -> Option<()> {
                                                                                            if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                            {
                                                                                                return None;
                                                                                            }
                                                                                            state.offset += 1;
                                                                                            Some(())
                                                                                        })();
                                                                                        if __ok.is_none() {
                                                                                            state.offset = __save;
                                                                                            break;
                                                                                        }
                                                                                        if state.offset == __save {
                                                                                            break;
                                                                                        }
                                                                                        __rep_count += 1;
                                                                                    }
                                                                                    if __rep_count < 1 {
                                                                                        return None;
                                                                                    }
                                                                                }
                                                                                {
                                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                                    if !(!(__b == b'/')) {
                                                                                        return None;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                }
                                                                                {
                                                                                    let __end = state.src_bytes.len();
                                                                                    let mut __pos = state.offset;
                                                                                    while __pos < __end {
                                                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                        if !(__b == b'*') {
                                                                                            __pos += 1;
                                                                                        } else {
                                                                                            break;
                                                                                        }
                                                                                    }
                                                                                    state.offset = __pos;
                                                                                }
                                                                                Some(())
                                                                            })();
                                                                            if __ok.is_none() {
                                                                                state.offset = __save;
                                                                                break;
                                                                            }
                                                                            if state.offset == __save {
                                                                                break;
                                                                            }
                                                                            __rep_count += 1;
                                                                        }
                                                                    }
                                                                    {
                                                                        let mut __rep_count: u32 = 0;
                                                                        loop {
                                                                            let __save = state.offset;
                                                                            let __ok = (|| -> Option<()> {
                                                                                if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                                Some(())
                                                                            })();
                                                                            if __ok.is_none() {
                                                                                state.offset = __save;
                                                                                break;
                                                                            }
                                                                            if state.offset == __save {
                                                                                break;
                                                                            }
                                                                            __rep_count += 1;
                                                                        }
                                                                        if __rep_count < 1 {
                                                                            return None;
                                                                        }
                                                                    }
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                    {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                    Some(())
                                                                })();
                                                                if __ok.is_none() {
                                                                    state.offset = __save_dispatch;
                                                                    return None;
                                                                }
                                                            }
                                                            _ => {
                                                                return None;
                                                            }
                                                        }
                                                    }
                                                    Some(())
                                                })();
                                                if __ok.is_none() {
                                                    state.offset = __save;
                                                    break;
                                                }
                                                if state.offset == __save {
                                                    break;
                                                }
                                                __rep_count += 1;
                                            }
                                        }
                                        Some(())
                                    })();
                                    if __result.is_some() {
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        state.offset = __start;
                                        None
                                    }
                                }
                                    .is_none()
                                {
                                    return false;
                                }
                                let __matched = &state.src[__start..state.offset];
                                if !__matched.is_empty() {
                                    __builder.text(__matched);
                                }
                            };
                        };
                    };
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'}') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'}');
                    };
                };
                true
            }
        }
        pub fn ruleBlock_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__ruleBlock_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __blockContent_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.indent_open();
            __builder.hardline();
            let __pretty_ok = {
                {
                    {
                        let mut __rep_count17 = 0usize;
                        while __rep_count17 < 4294967295 {
                            let __rep_cp18 = state.offset;
                            let __iter_cp = if __rep_count17 > 0 {
                                Some(__builder.checkpoint())
                            } else {
                                None
                            };
                            if __rep_count17 > 0 {
                                __builder.hardline();
                            }
                            if !{
                                let __pretty_cp16 = state.offset;
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            let __start = state.offset;
                                            if {
                                                let __start = state.offset;
                                                let __result: Option<()> = (|| {
                                                    {
                                                        let mut __rep_count: u32 = 0;
                                                        loop {
                                                            let __save = state.offset;
                                                            let __ok = (|| -> Option<()> {
                                                                {
                                                                    let __save_dispatch = state.offset;
                                                                    let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                                    match __dispatch_b {
                                                                        b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' => {
                                                                            let __ok = (|| -> Option<()> {
                                                                                {
                                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                                    if !(__b.is_ascii_whitespace()) {
                                                                                        return None;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                }
                                                                                Some(())
                                                                            })();
                                                                            if __ok.is_none() {
                                                                                state.offset = __save_dispatch;
                                                                                return None;
                                                                            }
                                                                        }
                                                                        b'/' => {
                                                                            let __ok = (|| -> Option<()> {
                                                                                if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                                {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                                if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                                {
                                                                                    let __end = state.src_bytes.len();
                                                                                    let mut __pos = state.offset;
                                                                                    while __pos < __end {
                                                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                        if !(__b == b'*') {
                                                                                            __pos += 1;
                                                                                        } else {
                                                                                            break;
                                                                                        }
                                                                                    }
                                                                                    state.offset = __pos;
                                                                                }
                                                                                {
                                                                                    let mut __rep_count: u32 = 0;
                                                                                    loop {
                                                                                        let __save = state.offset;
                                                                                        let __ok = (|| -> Option<()> {
                                                                                            {
                                                                                                let mut __rep_count: u32 = 0;
                                                                                                loop {
                                                                                                    let __save = state.offset;
                                                                                                    let __ok = (|| -> Option<()> {
                                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                                        {
                                                                                                            return None;
                                                                                                        }
                                                                                                        state.offset += 1;
                                                                                                        Some(())
                                                                                                    })();
                                                                                                    if __ok.is_none() {
                                                                                                        state.offset = __save;
                                                                                                        break;
                                                                                                    }
                                                                                                    if state.offset == __save {
                                                                                                        break;
                                                                                                    }
                                                                                                    __rep_count += 1;
                                                                                                }
                                                                                                if __rep_count < 1 {
                                                                                                    return None;
                                                                                                }
                                                                                            }
                                                                                            {
                                                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                                                if !(!(__b == b'/')) {
                                                                                                    return None;
                                                                                                }
                                                                                                state.offset += 1;
                                                                                            }
                                                                                            {
                                                                                                let __end = state.src_bytes.len();
                                                                                                let mut __pos = state.offset;
                                                                                                while __pos < __end {
                                                                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                                    if !(__b == b'*') {
                                                                                                        __pos += 1;
                                                                                                    } else {
                                                                                                        break;
                                                                                                    }
                                                                                                }
                                                                                                state.offset = __pos;
                                                                                            }
                                                                                            Some(())
                                                                                        })();
                                                                                        if __ok.is_none() {
                                                                                            state.offset = __save;
                                                                                            break;
                                                                                        }
                                                                                        if state.offset == __save {
                                                                                            break;
                                                                                        }
                                                                                        __rep_count += 1;
                                                                                    }
                                                                                }
                                                                                {
                                                                                    let mut __rep_count: u32 = 0;
                                                                                    loop {
                                                                                        let __save = state.offset;
                                                                                        let __ok = (|| -> Option<()> {
                                                                                            if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                            {
                                                                                                return None;
                                                                                            }
                                                                                            state.offset += 1;
                                                                                            Some(())
                                                                                        })();
                                                                                        if __ok.is_none() {
                                                                                            state.offset = __save;
                                                                                            break;
                                                                                        }
                                                                                        if state.offset == __save {
                                                                                            break;
                                                                                        }
                                                                                        __rep_count += 1;
                                                                                    }
                                                                                    if __rep_count < 1 {
                                                                                        return None;
                                                                                    }
                                                                                }
                                                                                if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                                {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                                Some(())
                                                                            })();
                                                                            if __ok.is_none() {
                                                                                state.offset = __save_dispatch;
                                                                                return None;
                                                                            }
                                                                        }
                                                                        _ => {
                                                                            return None;
                                                                        }
                                                                    }
                                                                }
                                                                Some(())
                                                            })();
                                                            if __ok.is_none() {
                                                                state.offset = __save;
                                                                break;
                                                            }
                                                            if state.offset == __save {
                                                                break;
                                                            }
                                                            __rep_count += 1;
                                                        }
                                                    }
                                                    Some(())
                                                })();
                                                if __result.is_some() {
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    state.offset = __start;
                                                    None
                                                }
                                            }
                                                .is_none()
                                            {
                                                return false;
                                            }
                                            let __matched = &state.src[__start..state.offset];
                                            if !__matched.is_empty() {
                                                __builder.text(__matched);
                                            }
                                        };
                                        {
                                            if !{
                                                let __pretty_cp14 = state.offset;
                                                let __pretty_bcp15 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    if !Self::__declaration_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp14;
                                                    __builder.restore(__pretty_bcp15);
                                                }
                                                __ok
                                            } {
                                                if !Self::__ruleItem_prettify(state, __builder) {
                                                    return false;
                                                }
                                            }
                                        };
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp16;
                                }
                                __ok
                            } {
                                state.offset = __rep_cp18;
                                if let Some(__bcp) = __iter_cp {
                                    __builder.restore(__bcp);
                                }
                                break;
                            }
                            if state.offset == __rep_cp18 {
                                if let Some(__bcp) = __iter_cp {
                                    __builder.restore(__bcp);
                                }
                                break;
                            }
                            __rep_count17 += 1;
                        }
                    };
                    true
                }
            };
            __builder.indent_close();
            __builder.hardline();
            __pretty_ok
        }
        pub fn blockContent_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__blockContent_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __atRule_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            __builder.indent_open();
            let __pretty_ok = {
                {
                    {
                        if !{
                            let __pretty_cp27 = state.offset;
                            let __pretty_bcp28 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                if !Self::__mediaRule_prettify(state, __builder) {
                                    return false;
                                }
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp27;
                                __builder.restore(__pretty_bcp28);
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp25 = state.offset;
                                    let __pretty_bcp26 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        if !Self::__supportsRule_prettify(state, __builder) {
                                            return false;
                                        }
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp25;
                                        __builder.restore(__pretty_bcp26);
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp23 = state.offset;
                                            let __pretty_bcp24 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                if !Self::__fontFaceRule_prettify(state, __builder) {
                                                    return false;
                                                }
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp23;
                                                __builder.restore(__pretty_bcp24);
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp21 = state.offset;
                                                    let __pretty_bcp22 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        if !Self::__importRule_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp21;
                                                        __builder.restore(__pretty_bcp22);
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp19 = state.offset;
                                                            let __pretty_bcp20 = __builder.checkpoint();
                                                            let __ok = (|| -> bool {
                                                                if !Self::__genericAtRule_prettify(state, __builder) {
                                                                    return false;
                                                                }
                                                                true
                                                            })();
                                                            if !__ok {
                                                                state.offset = __pretty_cp19;
                                                                __builder.restore(__pretty_bcp20);
                                                            }
                                                            __ok
                                                        } {
                                                            return false;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    };
                    true
                }
            };
            __builder.indent_close();
            __builder.group_close();
            __pretty_ok
        }
        pub fn atRule_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__atRule_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __ruleItem_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp29 = state.offset;
                        let __pretty_bcp30 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__qualifiedRule_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp29;
                            __builder.restore(__pretty_bcp30);
                        }
                        __ok
                    } {
                        if !Self::__atRule_prettify(state, __builder) {
                            return false;
                        }
                    }
                };
                true
            }
        }
        pub fn ruleItem_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__ruleItem_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __ruleList_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let mut __rep_count32 = 0usize;
                    while __rep_count32 < 4294967295 {
                        let __rep_cp33 = state.offset;
                        let __iter_cp = if __rep_count32 > 0 {
                            Some(__builder.checkpoint())
                        } else {
                            None
                        };
                        if __rep_count32 > 0 {
                            __builder.hardline();
                        }
                        if !{
                            let __pretty_cp31 = state.offset;
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __start = state.offset;
                                        if {
                                            let __start = state.offset;
                                            let __result: Option<()> = (|| {
                                                {
                                                    let mut __rep_count: u32 = 0;
                                                    loop {
                                                        let __save = state.offset;
                                                        let __ok = (|| -> Option<()> {
                                                            {
                                                                let __save_dispatch = state.offset;
                                                                let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                                match __dispatch_b {
                                                                    b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' => {
                                                                        let __ok = (|| -> Option<()> {
                                                                            {
                                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                                if !(__b.is_ascii_whitespace()) {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                            }
                                                                            Some(())
                                                                        })();
                                                                        if __ok.is_none() {
                                                                            state.offset = __save_dispatch;
                                                                            return None;
                                                                        }
                                                                    }
                                                                    b'/' => {
                                                                        let __ok = (|| -> Option<()> {
                                                                            if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                            {
                                                                                return None;
                                                                            }
                                                                            state.offset += 1;
                                                                            if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                            {
                                                                                return None;
                                                                            }
                                                                            state.offset += 1;
                                                                            {
                                                                                let __end = state.src_bytes.len();
                                                                                let mut __pos = state.offset;
                                                                                while __pos < __end {
                                                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                    if !(__b == b'*') {
                                                                                        __pos += 1;
                                                                                    } else {
                                                                                        break;
                                                                                    }
                                                                                }
                                                                                state.offset = __pos;
                                                                            }
                                                                            {
                                                                                let mut __rep_count: u32 = 0;
                                                                                loop {
                                                                                    let __save = state.offset;
                                                                                    let __ok = (|| -> Option<()> {
                                                                                        {
                                                                                            let mut __rep_count: u32 = 0;
                                                                                            loop {
                                                                                                let __save = state.offset;
                                                                                                let __ok = (|| -> Option<()> {
                                                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                                    {
                                                                                                        return None;
                                                                                                    }
                                                                                                    state.offset += 1;
                                                                                                    Some(())
                                                                                                })();
                                                                                                if __ok.is_none() {
                                                                                                    state.offset = __save;
                                                                                                    break;
                                                                                                }
                                                                                                if state.offset == __save {
                                                                                                    break;
                                                                                                }
                                                                                                __rep_count += 1;
                                                                                            }
                                                                                            if __rep_count < 1 {
                                                                                                return None;
                                                                                            }
                                                                                        }
                                                                                        {
                                                                                            let __b = *state.src_bytes.get(state.offset)?;
                                                                                            if !(!(__b == b'/')) {
                                                                                                return None;
                                                                                            }
                                                                                            state.offset += 1;
                                                                                        }
                                                                                        {
                                                                                            let __end = state.src_bytes.len();
                                                                                            let mut __pos = state.offset;
                                                                                            while __pos < __end {
                                                                                                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                                if !(__b == b'*') {
                                                                                                    __pos += 1;
                                                                                                } else {
                                                                                                    break;
                                                                                                }
                                                                                            }
                                                                                            state.offset = __pos;
                                                                                        }
                                                                                        Some(())
                                                                                    })();
                                                                                    if __ok.is_none() {
                                                                                        state.offset = __save;
                                                                                        break;
                                                                                    }
                                                                                    if state.offset == __save {
                                                                                        break;
                                                                                    }
                                                                                    __rep_count += 1;
                                                                                }
                                                                            }
                                                                            {
                                                                                let mut __rep_count: u32 = 0;
                                                                                loop {
                                                                                    let __save = state.offset;
                                                                                    let __ok = (|| -> Option<()> {
                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                        {
                                                                                            return None;
                                                                                        }
                                                                                        state.offset += 1;
                                                                                        Some(())
                                                                                    })();
                                                                                    if __ok.is_none() {
                                                                                        state.offset = __save;
                                                                                        break;
                                                                                    }
                                                                                    if state.offset == __save {
                                                                                        break;
                                                                                    }
                                                                                    __rep_count += 1;
                                                                                }
                                                                                if __rep_count < 1 {
                                                                                    return None;
                                                                                }
                                                                            }
                                                                            if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                            {
                                                                                return None;
                                                                            }
                                                                            state.offset += 1;
                                                                            Some(())
                                                                        })();
                                                                        if __ok.is_none() {
                                                                            state.offset = __save_dispatch;
                                                                            return None;
                                                                        }
                                                                    }
                                                                    _ => {
                                                                        return None;
                                                                    }
                                                                }
                                                            }
                                                            Some(())
                                                        })();
                                                        if __ok.is_none() {
                                                            state.offset = __save;
                                                            break;
                                                        }
                                                        if state.offset == __save {
                                                            break;
                                                        }
                                                        __rep_count += 1;
                                                    }
                                                }
                                                Some(())
                                            })();
                                            if __result.is_some() {
                                                Some(
                                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                                )
                                            } else {
                                                state.offset = __start;
                                                None
                                            }
                                        }
                                            .is_none()
                                        {
                                            return false;
                                        }
                                        let __matched = &state.src[__start..state.offset];
                                        if !__matched.is_empty() {
                                            __builder.text(__matched);
                                        }
                                    };
                                    if !Self::__ruleItem_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp31;
                            }
                            __ok
                        } {
                            state.offset = __rep_cp33;
                            if let Some(__bcp) = __iter_cp {
                                __builder.restore(__bcp);
                            }
                            break;
                        }
                        if state.offset == __rep_cp33 {
                            if let Some(__bcp) = __iter_cp {
                                __builder.restore(__bcp);
                            }
                            break;
                        }
                        __rep_count32 += 1;
                    }
                };
                true
            }
        }
        pub fn ruleList_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__ruleList_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __stylesheet_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        {
                            let __start = state.offset;
                            if {
                                let __start = state.offset;
                                let __result: Option<()> = (|| {
                                    {
                                        let mut __rep_count: u32 = 0;
                                        loop {
                                            let __save = state.offset;
                                            let __ok = (|| -> Option<()> {
                                                {
                                                    let __save_dispatch = state.offset;
                                                    let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                    match __dispatch_b {
                                                        b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' => {
                                                            let __ok = (|| -> Option<()> {
                                                                {
                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                    if !(__b.is_ascii_whitespace()) {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                }
                                                                Some(())
                                                            })();
                                                            if __ok.is_none() {
                                                                state.offset = __save_dispatch;
                                                                return None;
                                                            }
                                                        }
                                                        b'/' => {
                                                            let __ok = (|| -> Option<()> {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                                {
                                                                    let __end = state.src_bytes.len();
                                                                    let mut __pos = state.offset;
                                                                    while __pos < __end {
                                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                        if !(__b == b'*') {
                                                                            __pos += 1;
                                                                        } else {
                                                                            break;
                                                                        }
                                                                    }
                                                                    state.offset = __pos;
                                                                }
                                                                {
                                                                    let mut __rep_count: u32 = 0;
                                                                    loop {
                                                                        let __save = state.offset;
                                                                        let __ok = (|| -> Option<()> {
                                                                            {
                                                                                let mut __rep_count: u32 = 0;
                                                                                loop {
                                                                                    let __save = state.offset;
                                                                                    let __ok = (|| -> Option<()> {
                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                        {
                                                                                            return None;
                                                                                        }
                                                                                        state.offset += 1;
                                                                                        Some(())
                                                                                    })();
                                                                                    if __ok.is_none() {
                                                                                        state.offset = __save;
                                                                                        break;
                                                                                    }
                                                                                    if state.offset == __save {
                                                                                        break;
                                                                                    }
                                                                                    __rep_count += 1;
                                                                                }
                                                                                if __rep_count < 1 {
                                                                                    return None;
                                                                                }
                                                                            }
                                                                            {
                                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                                if !(!(__b == b'/')) {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                            }
                                                                            {
                                                                                let __end = state.src_bytes.len();
                                                                                let mut __pos = state.offset;
                                                                                while __pos < __end {
                                                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                    if !(__b == b'*') {
                                                                                        __pos += 1;
                                                                                    } else {
                                                                                        break;
                                                                                    }
                                                                                }
                                                                                state.offset = __pos;
                                                                            }
                                                                            Some(())
                                                                        })();
                                                                        if __ok.is_none() {
                                                                            state.offset = __save;
                                                                            break;
                                                                        }
                                                                        if state.offset == __save {
                                                                            break;
                                                                        }
                                                                        __rep_count += 1;
                                                                    }
                                                                }
                                                                {
                                                                    let mut __rep_count: u32 = 0;
                                                                    loop {
                                                                        let __save = state.offset;
                                                                        let __ok = (|| -> Option<()> {
                                                                            if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                            {
                                                                                return None;
                                                                            }
                                                                            state.offset += 1;
                                                                            Some(())
                                                                        })();
                                                                        if __ok.is_none() {
                                                                            state.offset = __save;
                                                                            break;
                                                                        }
                                                                        if state.offset == __save {
                                                                            break;
                                                                        }
                                                                        __rep_count += 1;
                                                                    }
                                                                    if __rep_count < 1 {
                                                                        return None;
                                                                    }
                                                                }
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                                {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                                Some(())
                                                            })();
                                                            if __ok.is_none() {
                                                                state.offset = __save_dispatch;
                                                                return None;
                                                            }
                                                        }
                                                        _ => {
                                                            return None;
                                                        }
                                                    }
                                                }
                                                Some(())
                                            })();
                                            if __ok.is_none() {
                                                state.offset = __save;
                                                break;
                                            }
                                            if state.offset == __save {
                                                break;
                                            }
                                            __rep_count += 1;
                                        }
                                    }
                                    Some(())
                                })();
                                if __result.is_some() {
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    state.offset = __start;
                                    None
                                }
                            }
                                .is_none()
                            {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        if !Self::__ruleList_prettify(state, __builder) {
                            return false;
                        }
                    };
                    {
                        let __start = state.offset;
                        if {
                            let __start = state.offset;
                            let __result: Option<()> = (|| {
                                {
                                    let mut __rep_count: u32 = 0;
                                    loop {
                                        let __save = state.offset;
                                        let __ok = (|| -> Option<()> {
                                            {
                                                let __save_dispatch = state.offset;
                                                let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                match __dispatch_b {
                                                    b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' => {
                                                        let __ok = (|| -> Option<()> {
                                                            {
                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                if !(__b.is_ascii_whitespace()) {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                            }
                                                            Some(())
                                                        })();
                                                        if __ok.is_none() {
                                                            state.offset = __save_dispatch;
                                                            return None;
                                                        }
                                                    }
                                                    b'/' => {
                                                        let __ok = (|| -> Option<()> {
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            {
                                                                let __end = state.src_bytes.len();
                                                                let mut __pos = state.offset;
                                                                while __pos < __end {
                                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                    if !(__b == b'*') {
                                                                        __pos += 1;
                                                                    } else {
                                                                        break;
                                                                    }
                                                                }
                                                                state.offset = __pos;
                                                            }
                                                            {
                                                                let mut __rep_count: u32 = 0;
                                                                loop {
                                                                    let __save = state.offset;
                                                                    let __ok = (|| -> Option<()> {
                                                                        {
                                                                            let mut __rep_count: u32 = 0;
                                                                            loop {
                                                                                let __save = state.offset;
                                                                                let __ok = (|| -> Option<()> {
                                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                                    {
                                                                                        return None;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                    Some(())
                                                                                })();
                                                                                if __ok.is_none() {
                                                                                    state.offset = __save;
                                                                                    break;
                                                                                }
                                                                                if state.offset == __save {
                                                                                    break;
                                                                                }
                                                                                __rep_count += 1;
                                                                            }
                                                                            if __rep_count < 1 {
                                                                                return None;
                                                                            }
                                                                        }
                                                                        {
                                                                            let __b = *state.src_bytes.get(state.offset)?;
                                                                            if !(!(__b == b'/')) {
                                                                                return None;
                                                                            }
                                                                            state.offset += 1;
                                                                        }
                                                                        {
                                                                            let __end = state.src_bytes.len();
                                                                            let mut __pos = state.offset;
                                                                            while __pos < __end {
                                                                                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                if !(__b == b'*') {
                                                                                    __pos += 1;
                                                                                } else {
                                                                                    break;
                                                                                }
                                                                            }
                                                                            state.offset = __pos;
                                                                        }
                                                                        Some(())
                                                                    })();
                                                                    if __ok.is_none() {
                                                                        state.offset = __save;
                                                                        break;
                                                                    }
                                                                    if state.offset == __save {
                                                                        break;
                                                                    }
                                                                    __rep_count += 1;
                                                                }
                                                            }
                                                            {
                                                                let mut __rep_count: u32 = 0;
                                                                loop {
                                                                    let __save = state.offset;
                                                                    let __ok = (|| -> Option<()> {
                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                                        {
                                                                            return None;
                                                                        }
                                                                        state.offset += 1;
                                                                        Some(())
                                                                    })();
                                                                    if __ok.is_none() {
                                                                        state.offset = __save;
                                                                        break;
                                                                    }
                                                                    if state.offset == __save {
                                                                        break;
                                                                    }
                                                                    __rep_count += 1;
                                                                }
                                                                if __rep_count < 1 {
                                                                    return None;
                                                                }
                                                            }
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            Some(())
                                                        })();
                                                        if __ok.is_none() {
                                                            state.offset = __save_dispatch;
                                                            return None;
                                                        }
                                                    }
                                                    _ => {
                                                        return None;
                                                    }
                                                }
                                            }
                                            Some(())
                                        })();
                                        if __ok.is_none() {
                                            state.offset = __save;
                                            break;
                                        }
                                        if state.offset == __save {
                                            break;
                                        }
                                        __rep_count += 1;
                                    }
                                }
                                Some(())
                            })();
                            if __result.is_some() {
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                state.offset = __start;
                                None
                            }
                        }
                            .is_none()
                        {
                            return false;
                        }
                        let __matched = &state.src[__start..state.offset];
                        if !__matched.is_empty() {
                            __builder.text(__matched);
                        }
                    };
                };
                true
            }
        }
        pub fn stylesheet_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__stylesheet_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        /// Parse an input string and return the grammar-specific
        /// document that owns the StructDirect runtime arena.
        pub fn parse(
            input: &str,
        ) -> ::core::result::Result<
            crate::runtime::css_pretty::CssPrettyDocument<'_>,
            crate::runtime::ParseErr,
        > {
            let __input_bytes = input.as_bytes();
            let mut state = __shape_support_CssPrettyParser::ScanState::new();
            let mut builder = crate::runtime::css_pretty::CssPrettyStructBuilder::new();
            crate::runtime::builder::StructBuilder::bind_input(
                &mut builder,
                __input_bytes,
            );
            static __EAGER_EMPTY_PATH: ::std::sync::LazyLock<
                crate::path::ir::TypedPath<crate::path::markers::Json, &'static str>,
            > = ::std::sync::LazyLock::new(|| {
                crate::path::ir::TypedPath::from_owned(::std::vec::Vec::new())
            });
            let mut __eager_cursor: crate::path::cursor::PathCursor<
                'static,
                crate::path::ir::TypedPath<crate::path::markers::Json, &'static str>,
            > = crate::path::cursor::PathCursor::new(
                &*__EAGER_EMPTY_PATH,
                |_rid, _kind, _idx| crate::path::cursor::Decision::ParseFully,
            );
            {
                let mut pos: usize = 0;
                parse_CssPrettyParser_stylesheet(
                        __input_bytes,
                        &mut pos,
                        &mut state,
                        &mut builder,
                        &mut __eager_cursor,
                    )
                    .map_err(|e| match e {
                        crate::runtime::DtaError::Syntax { offset } => {
                            crate::runtime::ParseErr::Syntax {
                                offset,
                                rule: None,
                            }
                        }
                        crate::runtime::DtaError::UnexpectedEnd { offset } => {
                            crate::runtime::ParseErr::Syntax {
                                offset,
                                rule: None,
                            }
                        }
                        crate::runtime::DtaError::InvalidState { .. } => {
                            crate::runtime::ParseErr::Syntax {
                                offset: 0,
                                rule: None,
                            }
                        }
                    })?;
                let _ = __shape_support_CssPrettyParser::skip_space(
                    __input_bytes,
                    &mut pos,
                    &mut state,
                );
                if pos != input.len() {
                    return Err(crate::runtime::ParseErr::Syntax {
                        offset: pos as u32,
                        rule: None,
                    });
                }
            }
            ::core::result::Result::Ok(builder.finalise(input))
        }
    }
}
pub use __cssprettyparser_emit_impl::*;
