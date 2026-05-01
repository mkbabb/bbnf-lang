//! IR transformation passes.
//!
//! Each pass is an independent function `GrammarIR → GrammarIR` (or `&mut GrammarIR`).
//! Passes can be composed in any order (though some orderings are more efficient).

pub mod audit;
pub mod context;
pub mod csp_domains;
pub mod csp_strategy;
pub mod facts;
pub mod inspect;
pub mod lr;
pub mod materialization;
pub mod metadata;
pub mod patterns;
pub mod payload;
pub mod prefix;
pub mod profile;
pub mod recognizers;
pub mod regex_info;
pub mod sets;
pub mod span;
pub mod transform;
pub mod types;

// NOTE: The `regex` submodule (simplify_regex_algebra + merge_regex_alts)
// was deleted in Tranche H-7. Every rewrite it performed is now handled
// by the retained grammar-tier e-graph rules
// (DeduplicateAltBranches, SupersetAbsorbAlt, UnionMergeAlt,
// FuseAltRegexBranches in crate::egraph::rules::regex) plus the HIR
// e-graph saturation in bbnf-regex. See commit message for details.

pub use audit::{
    AbsentRegistryProbe, AuditCoverageReport, GrammarAuditTag, GrammarCoverage, MarkerStatus,
    MissingMarker, PayloadLayoutsProbe, PendingMarker, StructRegistryProbe, audit_payload_coverage,
    is_typed_arrow_fn, write_coverage_report,
};
pub use csp_strategy::{
    AltMode, RecognizerDecision, RecognizerDecisionMap, RegexEngine, StrategyDomain, StrategyValue,
    WrapMode,
    components::{GrammarComponents, UnionFind, partition_by_call_graph},
    extract_regex_engine_decisions, solve_grammar_components,
};
pub use facts::FactAuthority;
pub use lr::{eliminate_direct_lr, eliminate_indirect_lr};
pub use materialization::{MaterializationClass, classify_materialization, mat_join};
pub use metadata::{compute_aliases, compute_transparent, has_named_return_type};
pub use payload::{
    LARGE_PAYLOAD_MAX, MAX_PAYLOAD_BYTES, NamedTypeResolver, NullResolver, PayloadField,
    PayloadLayout, compute_payload_layouts, compute_payload_layouts_with_resolver,
    is_kv_pair_shape, plan_layout, plan_layout_with_cap, scalar_range_includes_sentinel,
};
pub use prefix::factor_common_prefixes;
pub use profile::GrammarProfile;
pub use recognizers::dta::{
    Associativity, CounterOptional, DtaBuilder, DtaState, DtaTable, FrameKind, LiteralPayload,
    PrecedenceEntry, PrecedenceTable, RegexPayloadKind, SeqPromote, StateId, lift_dta,
};
pub use recognizers::mine_list_rules;
pub use recognizers::mine_recognizers;
pub use recognizers::operator_chain::{
    OperatorArity, OperatorChainEntry, OperatorChainFacts, OperatorChainRule,
    collect_operator_chains,
};
pub use regex_info::compute_regex_info;
pub use sets::{
    PushFingerprint, compute_first_sets, compute_follow_sets, compute_push_fingerprint,
    compute_rule_deps, compute_scc, factor_regex_with_lookahead, generate_dispatch_tables,
    sort_alt_branches,
};
pub use span::{compute_sp_method_rules, refine_span_eligibility};
pub use transform::{
    canonicalize_aliases, eliminate_epsilon, fuse_single_use, fuse_token_dispatch, inline_acyclic,
    merge_literals, prune_unreachable,
};
pub use types::project_types;
pub use types::registry::populate_struct_registry;
pub use types::{TypeMap, try_flatten_pair};
