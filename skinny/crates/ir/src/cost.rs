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
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
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
