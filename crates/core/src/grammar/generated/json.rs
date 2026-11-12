//! AUTO-GENERATED from `[workspace.metadata.bbnf.grammars]` — do not edit manually.
//! Regenerate: cargo xtask regen --grammar json

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

pub struct JsonParser;
mod __jsonparser_emit_impl {
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
    pub const GRAMMAR_JsonParser: [&'static str; 1usize] = [
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/grammar/json/json.bbnf")),
    ];
    static __GRAMMAR_STRUCTURAL_ALPHABET: [u8; 6usize] = [44, 58, 91, 93, 123, 125];
    static __GRAMMAR_STRUCTURAL_QUOTE_CLASSES: [u8; 1usize] = [34];
    pub const GRAMMAR_STRUCTURAL_ALPHABET: &[u8] = &__GRAMMAR_STRUCTURAL_ALPHABET;
    pub const GRAMMAR_STRUCTURAL_DIGRAPHS: &[(u8, u8)] = &[];
    pub const GRAMMAR_STRUCTURAL_DIGRAPH_MASK: [u64; 4] = [0, 0, 0, 0];
    pub const GRAMMAR_STRUCTURAL_QUOTE_CLASSES: &[u8] = &__GRAMMAR_STRUCTURAL_QUOTE_CLASSES;
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
        pub const PATH_PLAN_LEN: usize = 22;
        pub static PATH_PLAN: &[PathPlanEntry; 22] = &[
            PathPlanEntry {
                rule_id: 0,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 1,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 2,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 3,
                segment_kind: SegmentKind::Index,
                field_index: 4294967295,
                decision: Decision::Skip,
            },
            PathPlanEntry {
                rule_id: 3,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 4,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 4,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 5,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
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
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 7,
                segment_kind: SegmentKind::VariantName,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 7,
                segment_kind: SegmentKind::VariantName,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 7,
                segment_kind: SegmentKind::VariantName,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 7,
                segment_kind: SegmentKind::VariantName,
                field_index: 3,
                decision: Decision::ParseUntil(3),
            },
            PathPlanEntry {
                rule_id: 7,
                segment_kind: SegmentKind::VariantName,
                field_index: 4,
                decision: Decision::ParseUntil(4),
            },
            PathPlanEntry {
                rule_id: 7,
                segment_kind: SegmentKind::VariantName,
                field_index: 5,
                decision: Decision::ParseUntil(5),
            },
            PathPlanEntry {
                rule_id: 7,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 8,
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
                rule_name: ::std::string::String::from("null"),
                kind: ::bbnf_ir::registry::LayoutKind::NewtypeWrapper,
                rule_type: ::bbnf_ir::TypeDesc::Span,
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("value"), type_desc :
                    ::bbnf_ir::TypeDesc::Span, source :
                    ::bbnf_ir::registry::FieldSource::TypedLeaf, }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 1u32,
                rule_name: ::std::string::String::from("bool"),
                kind: ::bbnf_ir::registry::LayoutKind::UntaggedEnum,
                rule_type: ::bbnf_ir::TypeDesc::Bool,
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("branch_0"), type_desc :
                    ::bbnf_ir::TypeDesc::Bool, source :
                    ::bbnf_ir::registry::FieldSource::BranchTag { branch_index : 0u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("branch_1"), type_desc :
                    ::bbnf_ir::TypeDesc::Bool, source :
                    ::bbnf_ir::registry::FieldSource::BranchTag { branch_index : 1u32, },
                    }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 2u32,
                rule_name: ::std::string::String::from("number"),
                kind: ::bbnf_ir::registry::LayoutKind::NewtypeWrapper,
                rule_type: ::bbnf_ir::TypeDesc::F64,
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("value"), type_desc :
                    ::bbnf_ir::TypeDesc::F64, source :
                    ::bbnf_ir::registry::FieldSource::TypedLeaf, }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 3u32,
                rule_name: ::std::string::String::from("string"),
                kind: ::bbnf_ir::registry::LayoutKind::Struct,
                rule_type: ::bbnf_ir::TypeDesc::Named(12u32),
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("value"), type_desc :
                    ::bbnf_ir::TypeDesc::Named(12u32), source :
                    ::bbnf_ir::registry::FieldSource::TypedLeaf, }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 4u32,
                rule_name: ::std::string::String::from("object"),
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
                rule_id: 5u32,
                rule_name: ::std::string::String::from("array"),
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
                rule_id: 6u32,
                rule_name: ::std::string::String::from("pair"),
                kind: ::bbnf_ir::registry::LayoutKind::Struct,
                rule_type: ::bbnf_ir::TypeDesc::Tuple(
                    ::std::vec![
                        ::bbnf_ir::TypeDesc::BoxedEnum, ::bbnf_ir::TypeDesc::BoxedEnum
                    ],
                ),
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("string"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 0u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("field_1"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::SeqPosition { position : 1u32, }, }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 7u32,
                rule_name: ::std::string::String::from("value"),
                kind: ::bbnf_ir::registry::LayoutKind::TaggedEnum,
                rule_type: ::bbnf_ir::TypeDesc::HeterogeneousAltJoin(
                    ::std::vec![
                        ::bbnf_ir::TypeDesc::BoxedEnum, ::bbnf_ir::TypeDesc::Span,
                        ::bbnf_ir::TypeDesc::Bool, ::bbnf_ir::TypeDesc::F64
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
                    ::bbnf_ir::TypeDesc::Bool, source :
                    ::bbnf_ir::registry::FieldSource::BranchTag { branch_index : 4u32, },
                    }, ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("branch_5"), type_desc :
                    ::bbnf_ir::TypeDesc::F64, source :
                    ::bbnf_ir::registry::FieldSource::BranchTag { branch_index : 5u32, },
                    }
                ],
            });
        __registry
            .insert(::bbnf_ir::registry::StructLayout {
                rule_id: 8u32,
                rule_name: ::std::string::String::from("json"),
                kind: ::bbnf_ir::registry::LayoutKind::Struct,
                rule_type: ::bbnf_ir::TypeDesc::BoxedEnum,
                fields: ::std::vec![
                    ::bbnf_ir::registry::StructField { name :
                    ::std::string::String::from("value"), type_desc :
                    ::bbnf_ir::TypeDesc::BoxedEnum, source :
                    ::bbnf_ir::registry::FieldSource::RuleReference { target_rule : 7u32,
                    }, }
                ],
            });
        __registry
    });
    /// AZ-IV.W5 T4 — entry-rule name for the per-grammar registry.
    ///
    /// `bbnf-path` resolves the document-root rule through this const;
    /// the value is the first non-transparent rule's name (typically
    /// the start production of the grammar's top-level production).
    pub const REGISTRY_ENTRY_RULE: &str = "json";
    static __DTA_REGEX_4: &str = "-?(0|[1-9]\\d*)(\\.\\d+)?([eE][+-]?\\d+)?";
    static __DTA_REGEX_5: &str = "\"(?:[^\"\\\\]|\\\\(?:[\"\\\\\\/bfnrt]|u[0-9a-fA-F]{4}))*\"";
    static __DTA_REGEX_48: &str = "[ \\t\\n\\r]*";
    /// AY.W4.3 — hoisted DFA byte-class equivalence table.
    /// Consumed by `emit_dfa_body_table_driven` emitted in
    /// the same translation unit; AY-II.W0'.c retired the
    /// `#[allow(dead_code)]` marker — the emission pairs the
    /// tables with their consumer 1:1 at
    /// `emit_regex_scan_adapter`'s `state_count >=
    /// DFA_HOIST_MIN_STATES` branch.
    pub(crate) const __DFA_CLASSES_JsonParser_0: [u8; 256] = [
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 10, 10, 10, 10, 10, 10, 10, 10,
        10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
        10, 10, 10, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
        12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
        12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
        12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
        12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
        12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
        12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
        12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
    ];
    /// AY.W4.3 — hoisted DFA flat transition table
    /// (state * num_classes + class -> target_state |
    /// 0xFF=DEAD). Consumed via the same hoist-branch pairing
    /// as the byte-class table above.
    pub(crate) const __DFA_TRANS_JsonParser_0: [u8; 156] = [
        255, 255, 255, 1, 255, 255, 2, 3, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 2, 3, 255, 255, 255, 255, 255, 255, 255, 255, 255, 4, 255, 255, 255,
        255, 5, 255, 5, 255, 255, 255, 255, 255, 4, 255, 6, 6, 255, 5, 255, 5, 255, 255,
        255, 255, 255, 255, 255, 7, 7, 255, 255, 255, 255, 255, 255, 8, 255, 8, 255, 255,
        9, 9, 255, 255, 255, 255, 255, 255, 255, 255, 255, 4, 255, 6, 6, 255, 5, 255, 5,
        255, 255, 255, 255, 255, 255, 255, 10, 10, 255, 5, 255, 5, 255, 255, 255, 255,
        255, 255, 255, 9, 9, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 11,
        11, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 10, 10, 255, 5, 255,
        5, 255, 255, 255, 255, 255, 255, 255, 11, 11, 255, 255, 255, 255, 255,
    ];
    /// AY.W4.3 — hoisted DFA accept-state bitset. Consumed
    /// via the same hoist-branch pairing as the byte-class
    /// table above.
    pub(crate) const __DFA_ACCEPT_JsonParser_0: [u64; 1] = [3788];
    /// AY.W4.3 — hoisted DFA byte-class equivalence table.
    /// Consumed by `emit_dfa_body_table_driven` emitted in
    /// the same translation unit; AY-II.W0'.c retired the
    /// `#[allow(dead_code)]` marker — the emission pairs the
    /// tables with their consumer 1:1 at
    /// `emit_regex_scan_adapter`'s `state_count >=
    /// DFA_HOIST_MIN_STATES` branch.
    pub(crate) const __DFA_CLASSES_JsonParser_1: [u8; 256] = [
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 4, 4, 4, 4, 4, 4,
        4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 9, 9, 9, 9, 10, 11, 12, 12, 12, 13, 14, 14,
        14, 14, 14, 14, 14, 15, 16, 16, 16, 17, 18, 19, 20, 21, 21, 21, 21, 21, 21, 21,
        21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 23,
        23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24,
        24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
        24, 24, 24, 24, 24, 24, 24, 25, 25, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
        26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 27,
        28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 29, 30, 30, 31, 32, 32, 32, 33,
        34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
    ];
    /// AY.W4.3 — hoisted DFA flat transition table
    /// (state * num_classes + class -> target_state |
    /// 0xFF=DEAD). Consumed via the same hoist-branch pairing
    /// as the byte-class table above.
    pub(crate) const __DFA_TRANS_JsonParser_1: [u8; 525] = [
        255, 1, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 1, 2, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        255, 255, 255, 255, 4, 5, 6, 7, 6, 8, 9, 10, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 1, 255, 1,
        255, 255, 255, 255, 1, 255, 255, 1, 255, 1, 255, 1, 255, 1, 255, 1, 11, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 1, 1, 1, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 4, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 4, 4, 4, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 4, 4, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 6, 6, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 6, 6, 6, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 6,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        12, 255, 12, 255, 255, 255, 12, 12, 12, 12, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 13, 255, 13, 255, 255, 255, 13, 13, 13, 13, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 14, 255, 14, 255, 255, 255, 14, 14, 14, 14, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 1, 255, 1, 255, 255, 255, 1, 1, 1, 1, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255,
    ];
    /// AY.W4.3 — hoisted DFA accept-state bitset. Consumed
    /// via the same hoist-branch pairing as the byte-class
    /// table above.
    pub(crate) const __DFA_ACCEPT_JsonParser_1: [u64; 1] = [4];
    #[inline]
    #[cold]
    fn __regex_scan_JsonParser(
        pattern: &str,
        input: &[u8],
        pos: usize,
    ) -> ::core::option::Option<u32> {
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_4.as_ptr())
            || pattern == __DTA_REGEX_4
        {
            return '__dfa: {
                let mut __dfa_state: u8 = 0;
                let mut __dfa_p: usize = pos;
                let mut __dfa_last_match: ::core::option::Option<u32> = ::core::option::Option::None;
                let __end = input.len();
                while __dfa_p < __end {
                    let __b = unsafe { *input.get_unchecked(__dfa_p) };
                    let __c = unsafe {
                        *__DFA_CLASSES_JsonParser_0.get_unchecked(__b as usize)
                    };
                    let __next = unsafe {
                        *__DFA_TRANS_JsonParser_0
                            .get_unchecked(__dfa_state as usize * 13 + __c as usize)
                    };
                    if __next == 0xFF {
                        break;
                    }
                    __dfa_state = __next;
                    __dfa_p += 1;
                    if (__DFA_ACCEPT_JsonParser_0[__dfa_state as usize / 64]
                        >> (__dfa_state as usize % 64)) & 1 != 0
                    {
                        __dfa_last_match = ::core::option::Option::Some(__dfa_p as u32);
                    }
                }
                break '__dfa __dfa_last_match.map(|end| end - pos as u32);
            };
        }
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_5.as_ptr())
            || pattern == __DTA_REGEX_5
        {
            return '__dfa: {
                let mut __dfa_state: u8 = 0;
                let mut __dfa_p: usize = pos;
                let mut __dfa_last_match: ::core::option::Option<u32> = ::core::option::Option::None;
                let __end = input.len();
                while __dfa_p < __end {
                    let __b = unsafe { *input.get_unchecked(__dfa_p) };
                    let __c = unsafe {
                        *__DFA_CLASSES_JsonParser_1.get_unchecked(__b as usize)
                    };
                    let __next = unsafe {
                        *__DFA_TRANS_JsonParser_1
                            .get_unchecked(__dfa_state as usize * 35 + __c as usize)
                    };
                    if __next == 0xFF {
                        break;
                    }
                    __dfa_state = __next;
                    __dfa_p += 1;
                    if (__DFA_ACCEPT_JsonParser_1[__dfa_state as usize / 64]
                        >> (__dfa_state as usize % 64)) & 1 != 0
                    {
                        __dfa_last_match = ::core::option::Option::Some(__dfa_p as u32);
                    }
                }
                break '__dfa __dfa_last_match.map(|end| end - pos as u32);
            };
        }
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_48.as_ptr())
            || pattern == __DTA_REGEX_48
        {
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
                                9 | 10 | 13 | 32 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                9 | 10 | 13 | 32 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        _ => unsafe { ::core::hint::unreachable_unchecked() }
                    }
                    __dfa_p += 1;
                    match __dfa_state {
                        0 | 1 => {
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
    #[inline(never)]
    #[cold]
    #[allow(non_snake_case)]
    fn parse_number_fallback(bytes: &[u8]) -> f64 {
        let s = unsafe { ::core::str::from_utf8_unchecked(bytes) };
        s.parse::<f64>().unwrap_or(f64::NAN)
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
    pub(crate) mod __shape_support_JsonParser {
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
                ::parse_that::scan::StructuralIndex,
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
        ) -> &'a ::parse_that::scan::StructuralIndex {
            state
                .structural_index
                .get_or_init(|| {
                    let alphabet = ::parse_that::scan::StructuralAlphabet {
                        singletons: super::GRAMMAR_STRUCTURAL_ALPHABET,
                        digraph_mask: super::GRAMMAR_STRUCTURAL_DIGRAPH_MASK,
                        digraph_pairs: super::GRAMMAR_STRUCTURAL_DIGRAPHS,
                        quote_classes: super::GRAMMAR_STRUCTURAL_QUOTE_CLASSES,
                    };
                    ::parse_that::scan::scan_structural(input, &alphabet)
                })
        }
        /// Skip JSON whitespace at `*p`, returning the first
        /// non-whitespace byte (or `None` on EOF). Hot-path fast-
        /// exit when the next byte is non-whitespace.
        #[inline(always)]
        pub fn skip_space(
            input: &[u8],
            p: &mut usize,
            state: &mut ScanState,
        ) -> Option<u8> {
            match input.get(*p) {
                Some(&b) if b != b' ' && b != b'\t' && b != b'\n' && b != b'\r' => {
                    Some(b)
                }
                None => None,
                _ => {
                    skip_space_slow(input, p, state);
                    input.get(*p).copied()
                }
            }
        }
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
                            return;
                        }
                        *p = cache_base as usize + 64;
                    }
                }
                if *p + 64 > input.len() {
                    while let Some(&b) = input.get(*p) {
                        if b != b' ' && b != b'\t' && b != b'\n' && b != b'\r' {
                            return;
                        }
                        *p += 1;
                    }
                    return;
                }
                let stripe = unsafe {
                    ::core::slice::from_raw_parts(input.as_ptr().add(*p), 64)
                };
                let mask = nospace_bitmap_64(stripe);
                state.nospace_bits = mask;
                state.nospace_start = *p as isize;
                if mask != 0 {
                    let bit = mask.trailing_zeros() as usize;
                    *p += bit;
                    return;
                }
                *p += 64;
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
    /// AZ-I.W2.RD — struct-direct Keyword-shape parse fn
    /// (single-literal body).
    ///
    /// Matches the literal byte sequence and routes the
    /// rule's projected payload through the `StructBuilder`
    /// trait surface. Returns unit on success
    /// for compositional uniformity with the tape-path
    /// emission; the offset is unused by struct-direct
    /// callers (the dispatcher discards `Ok(_)` payloads).
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_keyword_JsonParser_null<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        _first_byte: u8,
        _state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'_>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        let _ = cursor;
        let at = *p;
        let end = at + 4usize;
        if input.len() < end || input[at..end] != [110u8, 117u8, 108u8, 108u8] {
            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                offset: at as u32,
            });
        }
        *p = end;
        builder.push_leaf_with_unit();
        ::core::result::Result::Ok(())
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
    pub fn parse_keyword_JsonParser_bool<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        let _ = state;
        let _ = cursor;
        match first_byte {
            102u8 => {
                if input.len() >= *p + 5usize
                    && input[*p..*p + 5usize] == [102u8, 97u8, 108u8, 115u8, 101u8]
                {
                    let at = *p;
                    let end = at + 5usize;
                    *p = end;
                    builder.push_leaf_with_bool(false);
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            116u8 => {
                if input.len() >= *p + 4usize
                    && input[*p..*p + 4usize] == [116u8, 114u8, 117u8, 101u8]
                {
                    let at = *p;
                    let end = at + 4usize;
                    *p = end;
                    builder.push_leaf_with_bool(true);
                    return ::core::result::Result::Ok(());
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
    /// AZ-I.W2.RC — per-grammar Number-shape parse function
    /// (struct-direct substrate).
    ///
    /// Mirrors `json_prototype::number::parse_number_body` for the
    /// scan body; the trailing leaf push routes through
    /// `builder.push_leaf_with_f64(value)` against the
    /// grammar-specific concrete `StructBuilder` impl. `first_byte`
    /// is the byte the dispatcher already matched; passing it
    /// avoids a redundant re-read for the sign check.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_number_JsonParser_number<'p>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        const POW10_U64: [u64; 17] = [
            1, 10, 100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000,
            1_000_000_000, 10_000_000_000, 100_000_000_000, 1_000_000_000_000,
            10_000_000_000_000, 100_000_000_000_000, 1_000_000_000_000_000,
            10_000_000_000_000_000,
        ];
        let _ = POW10_U64;
        let start = *p;
        let len = input.len();
        let negative = first_byte == b'-';
        if negative {
            *p += 1;
        }
        let int_start = *p;
        let mut mantissa: u64 = 0;
        let mut many_digits = false;
        if input.get(*p) == Some(&b'0') {
            *p += 1;
        } else {
            while *p < len {
                let b = input[*p];
                if b.is_ascii_digit() {
                    mantissa = mantissa.wrapping_mul(10).wrapping_add((b - b'0') as u64);
                    *p += 1;
                } else {
                    break;
                }
            }
        }
        if *p == int_start {
            return Err(crate::runtime::DtaError::Syntax {
                offset: start as u32,
            });
        }
        let int_digit_count = *p - int_start;
        if int_digit_count > 19 {
            many_digits = true;
        }
        let mut fractional_digit_count: i64 = 0;
        if input.get(*p) == Some(&b'.')
            && input.get(*p + 1).is_some_and(u8::is_ascii_digit)
        {
            *p += 1;
            let frac_start = *p;
            while *p < len {
                let b = input[*p];
                if b.is_ascii_digit() {
                    mantissa = mantissa.wrapping_mul(10).wrapping_add((b - b'0') as u64);
                    *p += 1;
                } else {
                    break;
                }
            }
            fractional_digit_count = (*p - frac_start) as i64;
            if int_digit_count as i64 + fractional_digit_count > 19 {
                many_digits = true;
            }
        }
        let mut exponent: i64 = -fractional_digit_count;
        let exp_byte = input.get(*p).copied();
        if exp_byte == Some(b'e') || exp_byte == Some(b'E') {
            let (exp_negative, digits_at) = match input.get(*p + 1) {
                Some(b'+') => (false, *p + 2),
                Some(b'-') => (true, *p + 2),
                _ => (false, *p + 1),
            };
            if input.get(digits_at).is_some_and(u8::is_ascii_digit) {
                *p = digits_at;
                let mut exp_val: i64 = 0;
                while *p < len {
                    let b = input[*p];
                    if b.is_ascii_digit() {
                        exp_val = exp_val
                            .saturating_mul(10)
                            .saturating_add((b - b'0') as i64);
                        *p += 1;
                    } else {
                        break;
                    }
                }
                exponent += if exp_negative { -exp_val } else { exp_val };
            }
        }
        let end = *p;
        let bytes = &input[start..end];
        let value = if many_digits {
            parse_number_fallback(bytes)
        } else {
            match ::parse_that::parsers::eisel_lemire::compute_f64(
                exponent,
                mantissa,
                negative,
            ) {
                Some(v) => v,
                None => parse_number_fallback(bytes),
            }
        };
        builder.push_leaf_with_f64(value);
        Ok(())
    }
    /// AZ-I.W2.RC — per-grammar String-shape parse function
    /// (struct-direct substrate).
    ///
    /// `"` must NOT be consumed by the caller — this
    /// function reads it, scans for the closing quote, and
    /// pushes a `&'p str` leaf via the builder. The borrow
    /// path slices the input directly; the cold escape
    /// path decodes into the builder's arena and emits
    /// the decoded bytes via the same `push_leaf_with_str`
    /// surface.
    ///
    /// AZ-IV.W3.6 — Cursor parameter is threaded for signature
    /// uniformity; string is a leaf (no recursion), so the
    /// cursor is not consulted in the body.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_string_JsonParser_string<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        _state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        let open = *p;
        if input.get(open).copied() != Some(b'"') {
            return Err(crate::runtime::DtaError::Syntax {
                offset: open as u32,
            });
        }
        let body_start = open + 1;
        let tail = match input.get(body_start..) {
            Some(t) => t,
            None => {
                return Err(crate::runtime::DtaError::UnexpectedEnd {
                    offset: open as u32,
                });
            }
        };
        match __shape_support_JsonParser::first_quote_or_backslash(tail) {
            Some((off, b'"')) => {
                let end = body_start + off;
                *p = end + 1;
                let body: &'p str = unsafe {
                    ::core::str::from_utf8_unchecked(&input[body_start..end])
                };
                builder.push_leaf_with_str(body);
                Ok(())
            }
            Some((_off, b'\\')) => {
                let mut buf: Vec<u8> = Vec::with_capacity(
                    input.len().saturating_sub(open + 1),
                );
                match ::parse_that::parsers::scan::decode_json_string_to_arena(
                    input,
                    open,
                    &mut buf,
                ) {
                    Some(
                        (
                            ::parse_that::parsers::scan::StringPayload::Owned { .. },
                            end_pos,
                        ),
                    ) => {
                        *p = end_pos;
                        let bytes: Box<[u8]> = buf.into_boxed_slice();
                        let leaked: &'static [u8] = Box::leak(bytes);
                        let leaked_str: &'static str = unsafe {
                            ::core::str::from_utf8_unchecked(leaked)
                        };
                        builder.push_leaf_with_str(leaked_str);
                        Ok(())
                    }
                    Some(
                        (
                            ::parse_that::parsers::scan::StringPayload::Borrowed {
                                start,
                                end,
                            },
                            end_pos,
                        ),
                    ) => {
                        *p = end_pos;
                        let body: &'p str = unsafe {
                            ::core::str::from_utf8_unchecked(
                                &input[start as usize..end as usize],
                            )
                        };
                        builder.push_leaf_with_str(body);
                        Ok(())
                    }
                    None => {
                        Err(crate::runtime::DtaError::Syntax {
                            offset: open as u32,
                        })
                    }
                }
            }
            Some(_) => unreachable!(),
            None => {
                Err(crate::runtime::DtaError::UnexpectedEnd {
                    offset: open as u32,
                })
            }
        }
    }
    /// AZ-I.W2.RB — per-grammar Object-shape parse function,
    /// **struct-direct body**. Targets [`JsonStructBuilder`].
    ///
    /// Compound emission lands as typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-element pushes (string keys + value
    /// dispatch) land directly on the topmost open frame.
    ///
    /// AZ-IV.W3.6 — Cursor-threaded. Each loop iteration consults
    /// `cursor.decide(rule_id) -> Decision` so the lazy bail-out
    /// parse can break after `ParseUntil(idx)` reaches the targeted
    /// child. Eager parses pass an always-`ParseFully` cursor,
    /// which makes the consult a no-op against the pre-W3.6 body.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_object_JsonParser_object<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder;
        use crate::path::cursor::Decision as __Decision;
        if input.get(*p).copied() != Some(b'{') {
            return Err(crate::runtime::DtaError::Syntax {
                offset: *p as u32,
            });
        }
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 4u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("object"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __handle = builder.begin_compound(&__layout);
        *p += 1;
        let _ = __shape_support_JsonParser::skip_space(input, p, state);
        if input.get(*p).copied() == Some(b'}') {
            *p += 1;
            builder.end_compound(__handle);
            return Ok(());
        }
        let __decision: __Decision = cursor.decide(4u32 as u32);
        let mut __iter_idx: u32 = 0;
        loop {
            let __seg_kind_pre = cursor.current_kind();
            let __is_field_seg = matches!(
                __seg_kind_pre, crate ::path::cursor::SegmentKind::Field,
            );
            let __key_save_p = *p;
            let __key_checkpoint = if __is_field_seg {
                Some(builder.checkpoint())
            } else {
                None
            };
            if input.get(*p).copied() != Some(b'"') {
                return Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            parse_string_JsonParser_string(input, p, state, builder, cursor)?;
            let _ = __shape_support_JsonParser::skip_space(input, p, state);
            if input.get(*p).copied() != Some(b':') {
                return Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            *p += 1;
            let _ = __shape_support_JsonParser::skip_space(input, p, state);
            let __matched: bool = if let Some(_cp) = &__key_checkpoint {
                let mut __close = __key_save_p + 1;
                while __close < input.len() {
                    let b = input[__close];
                    if b == b'\\' {
                        __close += 2;
                        continue;
                    }
                    if b == b'"' {
                        break;
                    }
                    __close += 1;
                }
                let __key_bytes: &'p [u8] = if __close > __key_save_p + 1 {
                    &input[__key_save_p + 1..__close]
                } else {
                    &[]
                };
                let __parsed_key: &'p str = unsafe {
                    ::core::str::from_utf8_unchecked(__key_bytes)
                };
                cursor.match_field(__parsed_key)
            } else {
                false
            };
            if __is_field_seg && !__matched {
                if let Some(__cp) = __key_checkpoint {
                    builder.rollback(__cp);
                }
                __shape_support_JsonParser::byte_skip_value(input, p)?;
            } else {
                ({
                    let _ = __shape_support_JsonParser::skip_space(input, p, state);
                    parse_wrap_JsonParser_value(input, p, state, builder, cursor)
                })?;
            }
            let _ = __shape_support_JsonParser::skip_space(input, p, state);
            match input.get(*p).copied() {
                Some(b',') => {
                    *p += 1;
                    let _ = __shape_support_JsonParser::skip_space(input, p, state);
                }
                Some(b'}') => {
                    *p += 1;
                    builder.end_compound(__handle);
                    return Ok(());
                }
                _ => {
                    if __matched {
                        builder.end_compound(__handle);
                        return Ok(());
                    }
                    return Err(crate::runtime::DtaError::Syntax {
                        offset: *p as u32,
                    });
                }
            }
            if __matched {
                builder.end_compound(__handle);
                return Ok(());
            }
            __iter_idx = __iter_idx.saturating_add(1);
            if let __Decision::ParseUntil(__cut) = __decision
                && __iter_idx as u32 > __cut as u32
            {
                builder.end_compound(__handle);
                return Ok(());
            }
        }
    }
    /// AZ-I.W2.RB — per-grammar Array-shape parse function,
    /// **struct-direct body** (Shape 1 — wrapped homogeneous repeat).
    ///
    /// AZ-IV.W3.6 — Cursor-threaded. The element loop consults
    /// `cursor.decide(rule_id) -> Decision` so the lazy bail-out
    /// parse can break after `ParseUntil(idx)` reaches the targeted
    /// element index. Eager parses pass an always-`ParseFully`
    /// cursor; the consult is a no-op against the pre-W3.6 body.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_array_JsonParser_array<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder;
        use crate::path::cursor::Decision as __Decision;
        if input.get(*p).copied() != Some(b'[') {
            return Err(crate::runtime::DtaError::Syntax {
                offset: *p as u32,
            });
        }
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 5u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("array"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __array_checkpoint = builder.checkpoint();
        let __handle = builder.begin_compound(&__layout);
        let __decision: __Decision = cursor.decide(5u32 as u32);
        let mut __elem_idx: u32 = 0;
        let __array_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            *p += 1;
            let _ = __shape_support_JsonParser::skip_space(input, p, state);
            if input.get(*p).copied() == Some(b']') {
                *p += 1;
                return Ok(());
            }
            loop {
                let __seg_kind = cursor.current_kind();
                let __is_index_seg = matches!(
                    __seg_kind, crate ::path::cursor::SegmentKind::Index,
                );
                let __matched: bool = if __is_index_seg {
                    cursor.match_index(__elem_idx as usize)
                } else {
                    false
                };
                if __is_index_seg && !__matched {
                    __shape_support_JsonParser::byte_skip_value(input, p)?;
                } else {
                    ({
                        let _ = __shape_support_JsonParser::skip_space(input, p, state);
                        parse_wrap_JsonParser_value(input, p, state, builder, cursor)
                    })?;
                }
                if __matched {
                    return Ok(());
                }
                if let __Decision::ParseUntil(__cut) = __decision
                    && __elem_idx as u32 >= __cut as u32
                {
                    return Ok(());
                }
                __elem_idx = __elem_idx.saturating_add(1);
                let _ = __shape_support_JsonParser::skip_space(input, p, state);
                match input.get(*p).copied() {
                    Some(b',') => {
                        *p += 1;
                        let _ = __shape_support_JsonParser::skip_space(input, p, state);
                    }
                    Some(b']') => {
                        *p += 1;
                        return Ok(());
                    }
                    _ => {
                        return Err(crate::runtime::DtaError::Syntax {
                            offset: *p as u32,
                        });
                    }
                }
            }
        })();
        match __array_result {
            Ok(()) => {
                builder.end_compound(__handle);
                Ok(())
            }
            Err(__err) => {
                builder.rollback(__array_checkpoint);
                Err(__err)
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
    pub fn parse_flat_JsonParser_pair<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
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
        static __PAIR_LAYOUT: ::std::sync::LazyLock<::bbnf_ir::registry::StructLayout> = ::std::sync::LazyLock::new(||
        ::bbnf_ir::registry::StructLayout {
            rule_id: 6u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("pair"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        });
        let __pair_handle = <crate::runtime::json::JsonStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &*__PAIR_LAYOUT);
        <crate::runtime::json::JsonStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = ({
                    let _ = __shape_support_JsonParser::skip_space(input, p, state);
                    parse_string_JsonParser_string(input, p, state, builder, cursor)
                })?;
            }
            {
                let _ = __shape_support_JsonParser::skip_space(input, p, state);
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [58u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
                let _ = __shape_support_JsonParser::skip_space(input, p, state);
            }
            {
                let _ = ({
                    let _ = __shape_support_JsonParser::skip_space(input, p, state);
                    parse_wrap_JsonParser_value(input, p, state, builder, cursor)
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::json::JsonStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::json::JsonStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __pair_handle,
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
    pub fn parse_wrap_JsonParser_value<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let _ = cursor.decide(7u32 as u32);
        let first = __shape_support_JsonParser::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        'try_branches: loop {
            match first {
                34u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_string_JsonParser_string(
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
                45u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
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
                48u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
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
                49u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
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
                50u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
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
                51u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
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
                52u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
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
                53u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
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
                54u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
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
                55u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
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
                56u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
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
                57u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
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
                91u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_array_JsonParser_array(
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
                102u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_keyword_JsonParser_bool(
                        input,
                        p,
                        first,
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
                110u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_keyword_JsonParser_null(
                        input,
                        p,
                        first,
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
                116u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_keyword_JsonParser_bool(
                        input,
                        p,
                        first,
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
                123u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_object_JsonParser_object(
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
                _ => {}
            }
            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                offset: *p as u32,
            });
        }
        ::core::result::Result::Ok(())
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
    pub fn parse_JsonParser_json<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        parse_JsonParser_json__value(input, p, state, builder, cursor)
    }
    /// AW-V.W3.2 — value-position shape dispatcher. Called both at
    /// the grammar root and from Object / Array compound bodies.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_JsonParser_json__value<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let first = __shape_support_JsonParser::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        let __result = match first {
            b'{' => parse_object_JsonParser_object(input, p, state, builder, cursor),
            b'[' => parse_array_JsonParser_array(input, p, state, builder, cursor),
            b'"' => parse_string_JsonParser_string(input, p, state, builder, cursor),
            b'-' | b'0'..=b'9' => {
                parse_number_JsonParser_number(input, p, first, builder)
            }
            b't' | b'f' => {
                parse_keyword_JsonParser_bool(input, p, first, state, builder, cursor)
            }
            b'n' => {
                parse_keyword_JsonParser_null(input, p, first, state, builder, cursor)
            }
            c => {
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
        };
        __result
    }
    impl JsonParser {
        fn __null_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __s = "null";
                    let __bytes = __s.as_bytes();
                    let __slc = match state.src_bytes.get(state.offset..) {
                        Some(s) if s.len() >= 4usize => s,
                        _ => return false,
                    };
                    if &__slc[..4usize] != __bytes {
                        return false;
                    }
                    __builder.text(&state.src[state.offset..state.offset + 4usize]);
                    state.offset += 4usize;
                };
                true
            }
        }
        pub fn null_prettify<'a>() -> Parser<'a, ::pprint::Doc<'a>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__null_prettify(state, &mut __builder) {
                    return None;
                }
                __builder.finish().ok()
            })
        }
        fn __bool_prettify<'a>(
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
                        b'f' => {
                            {
                                let __s = "false";
                                let __bytes = __s.as_bytes();
                                let __slc = match state.src_bytes.get(state.offset..) {
                                    Some(s) if s.len() >= 5usize => s,
                                    _ => return false,
                                };
                                if &__slc[..5usize] != __bytes {
                                    return false;
                                }
                                __builder
                                    .text(&state.src[state.offset..state.offset + 5usize]);
                                state.offset += 5usize;
                            };
                        }
                        b't' => {
                            {
                                let __s = "true";
                                let __bytes = __s.as_bytes();
                                let __slc = match state.src_bytes.get(state.offset..) {
                                    Some(s) if s.len() >= 4usize => s,
                                    _ => return false,
                                };
                                if &__slc[..4usize] != __bytes {
                                    return false;
                                }
                                __builder
                                    .text(&state.src[state.offset..state.offset + 4usize]);
                                state.offset += 4usize;
                            };
                        }
                        _ => {
                            return false;
                        }
                    }
                };
                true
            }
        }
        pub fn bool_prettify<'a>() -> Parser<'a, ::pprint::Doc<'a>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__bool_prettify(state, &mut __builder) {
                    return None;
                }
                __builder.finish().ok()
            })
        }
        fn __number_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __start = state.offset;
                    if {
                        let __start = state.offset;
                        let __result: Option<()> = (|| {
                            {
                                let __save = state.offset;
                                let __ok = (|| -> Option<()> {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                    {
                                        return None;
                                    }
                                    state.offset += 1;
                                    Some(())
                                })();
                                if __ok.is_none() {
                                    state.offset = __save;
                                }
                            }
                            {
                                let __save_dispatch = state.offset;
                                let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                match __dispatch_b {
                                    b'0' => {
                                        let __ok = (|| -> Option<()> {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'0')
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
                                    b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8'
                                    | b'9' => {
                                        let __ok = (|| -> Option<()> {
                                            {
                                                let __b = *state.src_bytes.get(state.offset)?;
                                                if !((__b >= b'1' && __b <= b'9')) {
                                                    return None;
                                                }
                                                state.offset += 1;
                                            }
                                            {
                                                let _ = ::parse_that::scan_digits_star_mut(state);
                                            }
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
                            {
                                let __save = state.offset;
                                let __ok = (|| -> Option<()> {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                    {
                                        return None;
                                    }
                                    state.offset += 1;
                                    {
                                        if ::parse_that::scan_digits_mut(state).is_none() {
                                            return None;
                                        }
                                    }
                                    Some(())
                                })();
                                if __ok.is_none() {
                                    state.offset = __save;
                                }
                            }
                            {
                                let __save = state.offset;
                                let __ok = (|| -> Option<()> {
                                    {
                                        let __b = *state.src_bytes.get(state.offset)?;
                                        if !((__b == b'E' || __b == b'e')) {
                                            return None;
                                        }
                                        state.offset += 1;
                                    }
                                    {
                                        let __save = state.offset;
                                        let __ok = (|| -> Option<()> {
                                            {
                                                let __b = *state.src_bytes.get(state.offset)?;
                                                if !((__b == b'+' || __b == b'-')) {
                                                    return None;
                                                }
                                                state.offset += 1;
                                            }
                                            Some(())
                                        })();
                                        if __ok.is_none() {
                                            state.offset = __save;
                                        }
                                    }
                                    {
                                        if ::parse_that::scan_digits_mut(state).is_none() {
                                            return None;
                                        }
                                    }
                                    Some(())
                                })();
                                if __ok.is_none() {
                                    state.offset = __save;
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
                true
            }
        }
        pub fn number_prettify<'a>() -> Parser<'a, ::pprint::Doc<'a>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__number_prettify(state, &mut __builder) {
                    return None;
                }
                __builder.finish().ok()
            })
        }
        fn __string_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __start = state.offset;
                    if {
                        let __start = state.offset;
                        let __result: Option<()> = (|| {
                            if state.src_bytes.get(state.offset).copied() != Some(b'"') {
                                return None;
                            }
                            state.offset += 1;
                            {
                                let mut __rep_count: u32 = 0;
                                loop {
                                    let __save = state.offset;
                                    let __ok = (|| -> Option<()> {
                                        {
                                            let __save_alt = state.offset;
                                            let __alt_ok = (|| -> Option<()> {
                                                {
                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                    if !(!((__b == b'"' || __b == b'\\'))) {
                                                        return None;
                                                    }
                                                    state.offset += 1;
                                                }
                                                Some(())
                                            })();
                                            let __alt_ok = if __alt_ok.is_none() {
                                                state.offset = __save_alt;
                                                (|| -> Option<()> {
                                                    if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                    {
                                                        return None;
                                                    }
                                                    state.offset += 1;
                                                    {
                                                        let __save_dispatch = state.offset;
                                                        let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                        match __dispatch_b {
                                                            b'"' | b'/' | b'\\' | b'b' | b'f' | b'n' | b'r' | b't' => {
                                                                let __ok = (|| -> Option<()> {
                                                                    {
                                                                        let __b = *state.src_bytes.get(state.offset)?;
                                                                        if !((__b == b'"' || __b == b'/' || __b == b'\\'
                                                                            || __b == b'b' || __b == b'f' || __b == b'n' || __b == b'r'
                                                                            || __b == b't'))
                                                                        {
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
                                                            b'u' => {
                                                                let __ok = (|| -> Option<()> {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'u')
                                                                    {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                    {
                                                                        let __end = state.src_bytes.len();
                                                                        let mut __pos = state.offset;
                                                                        let mut __count: u32 = 0;
                                                                        while __pos < __end && __count < 4 {
                                                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                            if __b.is_ascii_hexdigit() {
                                                                                __pos += 1;
                                                                                __count += 1;
                                                                            } else {
                                                                                break;
                                                                            }
                                                                        }
                                                                        if __count < 4 {
                                                                            return None;
                                                                        }
                                                                        state.offset = __pos;
                                                                    }
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
                            if state.src_bytes.get(state.offset).copied() != Some(b'"') {
                                return None;
                            }
                            state.offset += 1;
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
                true
            }
        }
        pub fn string_prettify<'a>() -> Parser<'a, ::pprint::Doc<'a>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__string_prettify(state, &mut __builder) {
                    return None;
                }
                __builder.finish().ok()
            })
        }
        fn __object_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            let __pretty_ok = (|| -> bool {
                {
                    {
                        {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'{')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'{');
                            };
                            {
                                if !{
                                    let __pretty_cp11 = state.offset;
                                    let __pretty_bcp12 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            let __ows9 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder.text_inline_ws(&state.src[__ows9..state.offset]);
                                            {
                                                let _ = {
                                                    let __pretty_cp7 = state.offset;
                                                    let __pretty_bcp8 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        {
                                                            if !Self::__pair_prettify(state, __builder) {
                                                                return false;
                                                            }
                                                            {
                                                                let mut __rep_count5 = 0usize;
                                                                while __rep_count5 < 4294967295 {
                                                                    let __rep_cp6 = state.offset;
                                                                    if !{
                                                                        let __pretty_cp3 = state.offset;
                                                                        let __pretty_bcp4 = __builder.checkpoint();
                                                                        let __ok = (|| -> bool {
                                                                            {
                                                                                {
                                                                                    let __ows0 = state.offset;
                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                    let __ows1 = state.offset;
                                                                                    {
                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                                        {
                                                                                            return false;
                                                                                        }
                                                                                        state.offset += 1;
                                                                                        __builder.char(b',');
                                                                                    };
                                                                                    __builder.text_inline_ws(&state.src[__ows0..__ows1]);
                                                                                    let __ows2 = state.offset;
                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                    __builder.text_inline_ws(&state.src[__ows2..state.offset]);
                                                                                };
                                                                                if !Self::__pair_prettify(state, __builder) {
                                                                                    return false;
                                                                                }
                                                                            };
                                                                            true
                                                                        })();
                                                                        if !__ok {
                                                                            state.offset = __pretty_cp3;
                                                                            __builder.restore(__pretty_bcp4);
                                                                        }
                                                                        __ok
                                                                    } {
                                                                        state.offset = __rep_cp6;
                                                                        break;
                                                                    }
                                                                    if state.offset == __rep_cp6 {
                                                                        break;
                                                                    }
                                                                    __rep_count5 += 1;
                                                                }
                                                            };
                                                        };
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp7;
                                                        __builder.restore(__pretty_bcp8);
                                                    }
                                                    __ok
                                                };
                                                true
                                            };
                                            let __ows10 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder.text_inline_ws(&state.src[__ows10..state.offset]);
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp11;
                                        __builder.restore(__pretty_bcp12);
                                    }
                                    __ok
                                } {
                                    return false;
                                }
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
            })();
            __builder.group_close();
            __pretty_ok
        }
        pub fn object_prettify<'a>() -> Parser<'a, ::pprint::Doc<'a>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__object_prettify(state, &mut __builder) {
                    return None;
                }
                __builder.finish().ok()
            })
        }
        fn __array_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            let __pretty_ok = (|| -> bool {
                {
                    {
                        {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'[')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'[');
                            };
                            {
                                if !{
                                    let __pretty_cp24 = state.offset;
                                    let __pretty_bcp25 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            let __ows22 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder.text_inline_ws(&state.src[__ows22..state.offset]);
                                            {
                                                let _ = {
                                                    let __pretty_cp20 = state.offset;
                                                    let __pretty_bcp21 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        {
                                                            if !Self::__value_prettify(state, __builder) {
                                                                return false;
                                                            }
                                                            {
                                                                let mut __rep_count18 = 0usize;
                                                                while __rep_count18 < 4294967295 {
                                                                    let __rep_cp19 = state.offset;
                                                                    if !{
                                                                        let __pretty_cp16 = state.offset;
                                                                        let __pretty_bcp17 = __builder.checkpoint();
                                                                        let __ok = (|| -> bool {
                                                                            {
                                                                                {
                                                                                    let __ows13 = state.offset;
                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                    let __ows14 = state.offset;
                                                                                    {
                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                                        {
                                                                                            return false;
                                                                                        }
                                                                                        state.offset += 1;
                                                                                        __builder.char(b',');
                                                                                    };
                                                                                    __builder.text_inline_ws(&state.src[__ows13..__ows14]);
                                                                                    let __ows15 = state.offset;
                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                    __builder.text_inline_ws(&state.src[__ows15..state.offset]);
                                                                                };
                                                                                if !Self::__value_prettify(state, __builder) {
                                                                                    return false;
                                                                                }
                                                                            };
                                                                            true
                                                                        })();
                                                                        if !__ok {
                                                                            state.offset = __pretty_cp16;
                                                                            __builder.restore(__pretty_bcp17);
                                                                        }
                                                                        __ok
                                                                    } {
                                                                        state.offset = __rep_cp19;
                                                                        break;
                                                                    }
                                                                    if state.offset == __rep_cp19 {
                                                                        break;
                                                                    }
                                                                    __rep_count18 += 1;
                                                                }
                                                            };
                                                        };
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp20;
                                                        __builder.restore(__pretty_bcp21);
                                                    }
                                                    __ok
                                                };
                                                true
                                            };
                                            let __ows23 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder.text_inline_ws(&state.src[__ows23..state.offset]);
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp24;
                                        __builder.restore(__pretty_bcp25);
                                    }
                                    __ok
                                } {
                                    return false;
                                }
                            };
                        };
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b']') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b']');
                        };
                    };
                    true
                }
            })();
            __builder.group_close();
            __pretty_ok
        }
        pub fn array_prettify<'a>() -> Parser<'a, ::pprint::Doc<'a>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__array_prettify(state, &mut __builder) {
                    return None;
                }
                __builder.finish().ok()
            })
        }
        fn __pair_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            let __pretty_ok = (|| -> bool {
                {
                    {
                        {
                            let __start = state.offset;
                            if {
                                let __start = state.offset;
                                let __result: Option<()> = (|| {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'"')
                                    {
                                        return None;
                                    }
                                    state.offset += 1;
                                    {
                                        let mut __rep_count: u32 = 0;
                                        loop {
                                            let __save = state.offset;
                                            let __ok = (|| -> Option<()> {
                                                {
                                                    let __save_alt = state.offset;
                                                    let __alt_ok = (|| -> Option<()> {
                                                        {
                                                            let __b = *state.src_bytes.get(state.offset)?;
                                                            if !(!((__b == b'"' || __b == b'\\'))) {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                        }
                                                        Some(())
                                                    })();
                                                    let __alt_ok = if __alt_ok.is_none() {
                                                        state.offset = __save_alt;
                                                        (|| -> Option<()> {
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            {
                                                                let __save_dispatch = state.offset;
                                                                let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                                match __dispatch_b {
                                                                    b'"' | b'/' | b'\\' | b'b' | b'f' | b'n' | b'r' | b't' => {
                                                                        let __ok = (|| -> Option<()> {
                                                                            {
                                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                                if !((__b == b'"' || __b == b'/' || __b == b'\\'
                                                                                    || __b == b'b' || __b == b'f' || __b == b'n' || __b == b'r'
                                                                                    || __b == b't'))
                                                                                {
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
                                                                    b'u' => {
                                                                        let __ok = (|| -> Option<()> {
                                                                            if state.src_bytes.get(state.offset).copied() != Some(b'u')
                                                                            {
                                                                                return None;
                                                                            }
                                                                            state.offset += 1;
                                                                            {
                                                                                let __end = state.src_bytes.len();
                                                                                let mut __pos = state.offset;
                                                                                let mut __count: u32 = 0;
                                                                                while __pos < __end && __count < 4 {
                                                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                    if __b.is_ascii_hexdigit() {
                                                                                        __pos += 1;
                                                                                        __count += 1;
                                                                                    } else {
                                                                                        break;
                                                                                    }
                                                                                }
                                                                                if __count < 4 {
                                                                                    return None;
                                                                                }
                                                                                state.offset = __pos;
                                                                            }
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
                                    if state.src_bytes.get(state.offset).copied() != Some(b'"')
                                    {
                                        return None;
                                    }
                                    state.offset += 1;
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
                            {
                                let __ows26 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                let __ows27 = state.offset;
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b':')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b':');
                                };
                                __builder.text_inline_ws(&state.src[__ows26..__ows27]);
                                let __ows28 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows28..state.offset]);
                            };
                            if !Self::__value_prettify(state, __builder) {
                                return false;
                            }
                        };
                    };
                    true
                }
            })();
            __builder.group_close();
            __pretty_ok
        }
        pub fn pair_prettify<'a>() -> Parser<'a, ::pprint::Doc<'a>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__pair_prettify(state, &mut __builder) {
                    return None;
                }
                __builder.finish().ok()
            })
        }
        fn __value_prettify<'a>(
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
                        b'{' => {
                            if !Self::__object_prettify(state, __builder) {
                                return false;
                            }
                        }
                        b'[' => {
                            if !Self::__array_prettify(state, __builder) {
                                return false;
                            }
                        }
                        b'"' => {
                            {
                                let __start = state.offset;
                                if {
                                    let __start = state.offset;
                                    let __result: Option<()> = (|| {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'"')
                                        {
                                            return None;
                                        }
                                        state.offset += 1;
                                        {
                                            let mut __rep_count: u32 = 0;
                                            loop {
                                                let __save = state.offset;
                                                let __ok = (|| -> Option<()> {
                                                    {
                                                        let __save_alt = state.offset;
                                                        let __alt_ok = (|| -> Option<()> {
                                                            {
                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                if !(!((__b == b'"' || __b == b'\\'))) {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                            }
                                                            Some(())
                                                        })();
                                                        let __alt_ok = if __alt_ok.is_none() {
                                                            state.offset = __save_alt;
                                                            (|| -> Option<()> {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                                {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                                {
                                                                    let __save_dispatch = state.offset;
                                                                    let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                                    match __dispatch_b {
                                                                        b'"' | b'/' | b'\\' | b'b' | b'f' | b'n' | b'r' | b't' => {
                                                                            let __ok = (|| -> Option<()> {
                                                                                {
                                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                                    if !((__b == b'"' || __b == b'/' || __b == b'\\'
                                                                                        || __b == b'b' || __b == b'f' || __b == b'n' || __b == b'r'
                                                                                        || __b == b't'))
                                                                                    {
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
                                                                        b'u' => {
                                                                            let __ok = (|| -> Option<()> {
                                                                                if state.src_bytes.get(state.offset).copied() != Some(b'u')
                                                                                {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                                {
                                                                                    let __end = state.src_bytes.len();
                                                                                    let mut __pos = state.offset;
                                                                                    let mut __count: u32 = 0;
                                                                                    while __pos < __end && __count < 4 {
                                                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                                        if __b.is_ascii_hexdigit() {
                                                                                            __pos += 1;
                                                                                            __count += 1;
                                                                                        } else {
                                                                                            break;
                                                                                        }
                                                                                    }
                                                                                    if __count < 4 {
                                                                                        return None;
                                                                                    }
                                                                                    state.offset = __pos;
                                                                                }
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
                                        if state.src_bytes.get(state.offset).copied() != Some(b'"')
                                        {
                                            return None;
                                        }
                                        state.offset += 1;
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
                        }
                        b'n' => {
                            {
                                let __s = "null";
                                let __bytes = __s.as_bytes();
                                let __slc = match state.src_bytes.get(state.offset..) {
                                    Some(s) if s.len() >= 4usize => s,
                                    _ => return false,
                                };
                                if &__slc[..4usize] != __bytes {
                                    return false;
                                }
                                __builder
                                    .text(&state.src[state.offset..state.offset + 4usize]);
                                state.offset += 4usize;
                            };
                        }
                        b'f' | b't' => {
                            if !Self::__bool_prettify(state, __builder) {
                                return false;
                            }
                        }
                        b'-' | b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7'
                        | b'8' | b'9' => {
                            {
                                let __start = state.offset;
                                if {
                                    let __start = state.offset;
                                    let __result: Option<()> = (|| {
                                        {
                                            let __save = state.offset;
                                            let __ok = (|| -> Option<()> {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                                {
                                                    return None;
                                                }
                                                state.offset += 1;
                                                Some(())
                                            })();
                                            if __ok.is_none() {
                                                state.offset = __save;
                                            }
                                        }
                                        {
                                            let __save_dispatch = state.offset;
                                            let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                            match __dispatch_b {
                                                b'0' => {
                                                    let __ok = (|| -> Option<()> {
                                                        if state.src_bytes.get(state.offset).copied() != Some(b'0')
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
                                                b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8'
                                                | b'9' => {
                                                    let __ok = (|| -> Option<()> {
                                                        {
                                                            let __b = *state.src_bytes.get(state.offset)?;
                                                            if !((__b >= b'1' && __b <= b'9')) {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                        }
                                                        {
                                                            let _ = ::parse_that::scan_digits_star_mut(state);
                                                        }
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
                                        {
                                            let __save = state.offset;
                                            let __ok = (|| -> Option<()> {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                                {
                                                    return None;
                                                }
                                                state.offset += 1;
                                                {
                                                    if ::parse_that::scan_digits_mut(state).is_none() {
                                                        return None;
                                                    }
                                                }
                                                Some(())
                                            })();
                                            if __ok.is_none() {
                                                state.offset = __save;
                                            }
                                        }
                                        {
                                            let __save = state.offset;
                                            let __ok = (|| -> Option<()> {
                                                {
                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                    if !((__b == b'E' || __b == b'e')) {
                                                        return None;
                                                    }
                                                    state.offset += 1;
                                                }
                                                {
                                                    let __save = state.offset;
                                                    let __ok = (|| -> Option<()> {
                                                        {
                                                            let __b = *state.src_bytes.get(state.offset)?;
                                                            if !((__b == b'+' || __b == b'-')) {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                        }
                                                        Some(())
                                                    })();
                                                    if __ok.is_none() {
                                                        state.offset = __save;
                                                    }
                                                }
                                                {
                                                    if ::parse_that::scan_digits_mut(state).is_none() {
                                                        return None;
                                                    }
                                                }
                                                Some(())
                                            })();
                                            if __ok.is_none() {
                                                state.offset = __save;
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
                        }
                        _ => {
                            return false;
                        }
                    }
                };
                true
            }
        }
        pub fn value_prettify<'a>() -> Parser<'a, ::pprint::Doc<'a>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__value_prettify(state, &mut __builder) {
                    return None;
                }
                __builder.finish().ok()
            })
        }
        fn __json_prettify<'a>(
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
                                let __end = state.src_bytes.len();
                                let mut __pos = __start;
                                while __pos < __end {
                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                    if (__b >= b'\t' && __b <= b'\n') || __b == b'\r'
                                        || __b == b' '
                                    {
                                        __pos += 1;
                                    } else {
                                        break;
                                    }
                                }
                                state.offset = __pos;
                                Some(::parse_that::Span::new(__start, __pos, state.src))
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
                        if !Self::__value_prettify(state, __builder) {
                            return false;
                        }
                    };
                    {
                        let __start = state.offset;
                        if {
                            let __start = state.offset;
                            let __end = state.src_bytes.len();
                            let mut __pos = __start;
                            while __pos < __end {
                                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                if (__b >= b'\t' && __b <= b'\n') || __b == b'\r'
                                    || __b == b' '
                                {
                                    __pos += 1;
                                } else {
                                    break;
                                }
                            }
                            state.offset = __pos;
                            Some(::parse_that::Span::new(__start, __pos, state.src))
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
        pub fn json_prettify<'a>() -> Parser<'a, ::pprint::Doc<'a>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__json_prettify(state, &mut __builder) {
                    return None;
                }
                __builder.finish().ok()
            })
        }
        /// Parse an input string and return the grammar-specific
        /// document that owns the StructDirect runtime arena.
        pub fn parse(
            input: &str,
        ) -> ::core::result::Result<
            crate::runtime::json::JsonDocument<'_>,
            crate::runtime::ParseErr,
        > {
            let __input_bytes = input.as_bytes();
            let mut state = __shape_support_JsonParser::ScanState::new();
            let mut builder = crate::runtime::json::JsonStructBuilder::new();
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
                parse_JsonParser_json(
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
                let _ = __shape_support_JsonParser::skip_space(
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
pub use __jsonparser_emit_impl::*;
