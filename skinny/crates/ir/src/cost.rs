use crate::{BackendShape, RuleId};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CostFacts {
    pub rule_id: RuleId,
    pub chosen: BackendShape,
    pub rationale: ShapeRationale,
    pub rejected: Vec<RejectedAlternative>,
    pub priority_fired: PriorityStep,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub capacity_policy: Option<CapacityPolicy>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub active_cost: Option<ActiveCostFacts>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub decision_csp: Option<DecisionCspFacts>,
}

impl CostFacts {
    pub fn projection(
        rule_id: RuleId,
        chosen: BackendShape,
        rationale: ShapeRationale,
        priority_fired: PriorityStep,
    ) -> Self {
        let rejected = all_backend_shapes()
            .iter()
            .copied()
            .filter(|shape| *shape != chosen)
            .map(|shape| RejectedAlternative {
                shape,
                reason: RejectionReason::PreconditionUnmet,
                evidence: Vec::new(),
            })
            .collect();
        Self {
            rule_id,
            chosen,
            rationale,
            rejected,
            priority_fired,
            capacity_policy: None,
            active_cost: None,
            decision_csp: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ActiveCostFacts {
    pub schema_version: String,
    pub cost_formula_version: String,
    pub egraph_language_status: String,
    pub rewrite_set_id: String,
    pub candidate_total_count: u32,
    pub candidate_hard_pruned_count: u32,
    pub candidate_ranked_count: u32,
    pub candidate_stale_count: u32,
    pub candidate_cost_stale_rate_bps: u32,
    pub selected_candidate_id: String,
    pub selected_shape: BackendShape,
    pub selected_cost_freshness: String,
    pub selected_rule_id: RuleId,
    pub egraph_node_count: u32,
    pub egraph_eclass_count: u32,
    pub egraph_iteration_count: u32,
    pub egraph_memory_peak_bytes: u64,
    pub egraph_budget_status: String,
    pub determinism_replay_status: String,
    pub rewrite_order_replay_count: u32,
    pub rewrite_order_variance_pct: u32,
    pub selection_trace_hash: String,
    pub generated_selection_path: String,
    pub same_wave_consumer_path: String,
    pub cascade_fallback_status: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DecisionCspFacts {
    pub schema_version: String,
    pub csp_solver_source: String,
    pub csp_solver_version: String,
    pub csp_status: String,
    pub csp_variable_count: u32,
    pub csp_constraint_count: u32,
    pub csp_objective_count: u32,
    pub csp_named_grammars: Vec<String>,
    pub csp_solve_us: u64,
    pub csp_timeout_ms: u64,
    pub csp_node_budget: u64,
    pub csp_nodes_explored: u64,
    pub csp_budget_status: String,
    pub selected_rule_count: u32,
    pub selected_candidate_id: String,
    pub selected_shape: BackendShape,
    pub parity_constraint_status: String,
    pub recognizer_constraint_status: String,
    pub substrate_constraint_status: String,
    pub simd_constraint_status: String,
    pub capacity_constraint_status: String,
    pub resolver_output_piping: String,
    pub fused_solver_status: String,
    pub generated_selection_path: String,
    pub compile_consumer_path: String,
    pub same_wave_consumer_path: String,
    pub same_wave_consumer_class: String,
    pub cascade_retirement_status: String,
    pub choose_backend_shape_status: String,
    pub priority_table_status: String,
    pub p1_p8_fallback_status: String,
    pub legacy_cascade_admission_status: String,
    pub priority_data_role: String,
    pub priority_hard_prune_status: String,
    pub priority_objective_status: String,
    pub fallback_invoked: bool,
    pub compat_fallback_status: String,
    pub static_css_provider_status: String,
    pub json_sink_only_status: String,
    pub generated_runtime_diff_status: String,
    pub row_move_toward_sota_status: String,
    pub block_id: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ShapeRationale {
    FirstSetDisjoint,
    FirstSetOverlap,
    ErrorRecoveryRequired,
    HostFnParseTime,
    LayoutScopeWide,
    DirectBuildNoConsumer,
    CollapsedStageAdmissible,
    EventTapeAltDensity,
    DefaultOffsetTape,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PriorityStep {
    P1EagerForced,
    P2SinkOnlyConsumer,
    P3CollapsedStage,
    P4EventTapeAltDensity,
    P5OffsetTapeDispatchable,
    P6OffsetTapeSpeculative,
    P7OffsetTapeDefault,
    P8EagerFallback,
}

impl PriorityStep {
    pub const ALL: [Self; 8] = [
        Self::P1EagerForced,
        Self::P2SinkOnlyConsumer,
        Self::P3CollapsedStage,
        Self::P4EventTapeAltDensity,
        Self::P5OffsetTapeDispatchable,
        Self::P6OffsetTapeSpeculative,
        Self::P7OffsetTapeDefault,
        Self::P8EagerFallback,
    ];
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RejectedAlternative {
    pub shape: BackendShape,
    pub reason: RejectionReason,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub evidence: Vec<Measurement>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum RejectionReason {
    PreconditionUnmet,
    InferiorObjective,
    AuthorWaiverAbsent,
    PreviouslyRegressed,
    ConsumerMismatch,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Measurement {
    pub workload: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub throughput_mbps_x1000: Option<u64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub cycles_per_byte_x1000: Option<u64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub hot_leaf_count: Option<u64>,
    pub source: EvidenceSource,
    pub source_ref: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum EvidenceSource {
    BenchProbe,
    RedressBackfill,
    AuthorDeclared,
    StaticAnalysis,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct CapacityPolicy {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tiny_string_cap: Option<u8>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub container_initial_capacity: Option<u16>,
}

pub const fn all_backend_shapes() -> [BackendShape; 5] {
    [
        BackendShape::EagerTape,
        BackendShape::OffsetTape,
        BackendShape::EventTape,
        BackendShape::SinkOnly,
        BackendShape::CollapsedStage,
    ]
}
