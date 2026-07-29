use bbnf_regex::RegexTablePlan;
use serde::Serialize;

use super::analysis::AnalysisFacts;
use super::budget::CompileOptions;
use super::diagnostic::{Diagnostic, DiagnosticCode, StageResult};
use super::source::Revision;
use super::syntax::{
    FieldId, NodeKind, PlanNodeId, Program, RecordSlot, RuleId, TransformId, TypeId,
};
use super::typecheck::{SemanticType, TypeSolution};

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct RegexPlanData {
    byte_classes: Vec<u8>,
    transitions: Vec<u32>,
    accepting: Vec<bool>,
    state_count: u32,
    class_count: u16,
    start_state: u32,
}

impl RegexPlanData {
    pub fn byte_classes(&self) -> &[u8] {
        &self.byte_classes
    }

    pub fn transitions(&self) -> &[u32] {
        &self.transitions
    }

    pub fn accepting(&self) -> &[bool] {
        &self.accepting
    }

    pub const fn state_count(&self) -> u32 {
        self.state_count
    }

    pub const fn class_count(&self) -> u16 {
        self.class_count
    }

    pub const fn start_state(&self) -> u32 {
        self.start_state
    }
}

impl From<RegexTablePlan> for RegexPlanData {
    fn from(plan: RegexTablePlan) -> Self {
        Self {
            byte_classes: plan.byte_classes().to_vec(),
            transitions: plan.transitions().to_vec(),
            accepting: plan.accepting().to_vec(),
            state_count: plan.state_count(),
            class_count: plan.class_count(),
            start_state: plan.start_state(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[non_exhaustive]
pub enum PlanRecordSlot {
    Field { field: FieldId, child: PlanNodeId },
    Discard { child: PlanNodeId },
}

impl PlanRecordSlot {
    pub const fn child(&self) -> PlanNodeId {
        match self {
            Self::Field { child, .. } | Self::Discard { child } => *child,
        }
    }

    pub const fn field(&self) -> Option<FieldId> {
        match self {
            Self::Field { field, .. } => Some(*field),
            Self::Discard { .. } => None,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[non_exhaustive]
pub enum PlanOp {
    Epsilon,
    Literal(Vec<u8>),
    Regex(usize),
    RuleRef(RuleId),
    Sequence(Vec<PlanNodeId>),
    Choice(Vec<PlanNodeId>),
    Repeat {
        child: PlanNodeId,
        minimum: u32,
        maximum: Option<u32>,
    },
    Record {
        owner: RuleId,
        slots: Vec<PlanRecordSlot>,
    },
    Transform {
        child: PlanNodeId,
        transform: TransformId,
        name: String,
    },
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct PlanNode {
    id: PlanNodeId,
    op: PlanOp,
    semantic_type: TypeId,
}

impl PlanNode {
    pub const fn id(&self) -> PlanNodeId {
        self.id
    }

    pub const fn op(&self) -> &PlanOp {
        &self.op
    }

    pub const fn semantic_type(&self) -> TypeId {
        self.semantic_type
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct LoweredRule {
    id: RuleId,
    root: PlanNodeId,
    name: String,
    named_type: TypeId,
}

impl LoweredRule {
    pub const fn id(&self) -> RuleId {
        self.id
    }

    pub const fn root(&self) -> PlanNodeId {
        self.root
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub const fn named_type(&self) -> TypeId {
        self.named_type
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct LoweredField {
    id: FieldId,
    owner: RuleId,
    slot: u32,
    child: PlanNodeId,
    name: String,
    semantic_type: TypeId,
}

impl LoweredField {
    pub const fn id(&self) -> FieldId {
        self.id
    }

    pub const fn owner(&self) -> RuleId {
        self.owner
    }

    pub const fn slot(&self) -> u32 {
        self.slot
    }

    pub const fn child(&self) -> PlanNodeId {
        self.child
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub const fn semantic_type(&self) -> TypeId {
        self.semantic_type
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct LoweringPlan {
    revision: Revision,
    nodes: Vec<PlanNode>,
    rules: Vec<LoweredRule>,
    semantic_types: Vec<SemanticType>,
    fields: Vec<LoweredField>,
    regexes: Vec<RegexPlanData>,
    entry: RuleId,
}

impl LoweringPlan {
    pub(crate) fn build(
        program: &Program,
        types: &TypeSolution,
        facts: &AnalysisFacts,
        options: CompileOptions,
    ) -> StageResult<Self> {
        if program.revision() != types.revision() || program.revision() != facts.revision() {
            return StageResult::rejected(Diagnostic::new(
                DiagnosticCode::RevisionMismatch,
                0,
                0,
                "lowering inputs carry different revisions",
            ));
        }

        let mut regexes = Vec::new();
        let mut nodes = Vec::with_capacity(program.nodes().len());
        for node in program.nodes() {
            let id = PlanNodeId::checked(node.id().index())
                .expect("program node identity is representable");
            let op = match node.kind() {
                NodeKind::Epsilon => PlanOp::Epsilon,
                NodeKind::Literal(bytes) => PlanOp::Literal(bytes.clone()),
                NodeKind::Regex(pattern) => {
                    let plan = match RegexTablePlan::compile_with(
                        pattern,
                        options.regex.table_options(),
                    ) {
                        Ok(plan) => plan,
                        Err(error) => {
                            return StageResult::rejected(Diagnostic::new(
                                DiagnosticCode::Regex,
                                node.span().start() as usize,
                                node.span().end() as usize,
                                error.to_string(),
                            ));
                        }
                    };
                    let index = regexes.len();
                    regexes.push(plan.into());
                    PlanOp::Regex(index)
                }
                NodeKind::RuleRef(rule) => PlanOp::RuleRef(*rule),
                NodeKind::Sequence(children) => PlanOp::Sequence(
                    children
                        .iter()
                        .map(|child| {
                            PlanNodeId::checked(child.index())
                                .expect("program node identity is representable")
                        })
                        .collect(),
                ),
                NodeKind::Choice(children) => PlanOp::Choice(
                    children
                        .iter()
                        .map(|child| {
                            PlanNodeId::checked(child.index())
                                .expect("program node identity is representable")
                        })
                        .collect(),
                ),
                NodeKind::Repeat {
                    child,
                    minimum,
                    maximum,
                } => PlanOp::Repeat {
                    child: PlanNodeId::checked(child.index())
                        .expect("program node identity is representable"),
                    minimum: *minimum,
                    maximum: *maximum,
                },
                NodeKind::Record { owner, slots } => PlanOp::Record {
                    owner: *owner,
                    slots: slots
                        .iter()
                        .map(|slot| match slot {
                            RecordSlot::Field { field, child } => PlanRecordSlot::Field {
                                field: *field,
                                child: PlanNodeId::checked(child.index())
                                    .expect("program node identity is representable"),
                            },
                            RecordSlot::Discard { child } => PlanRecordSlot::Discard {
                                child: PlanNodeId::checked(child.index())
                                    .expect("program node identity is representable"),
                            },
                        })
                        .collect(),
                },
                NodeKind::Transform {
                    child,
                    transform,
                    name,
                } => PlanOp::Transform {
                    child: PlanNodeId::checked(child.index())
                        .expect("program node identity is representable"),
                    transform: *transform,
                    name: name.clone(),
                },
            };
            nodes.push(PlanNode {
                id,
                op,
                semantic_type: types.node_type_required(node.id()),
            });
        }

        let rules = program
            .rules()
            .iter()
            .map(|rule| LoweredRule {
                id: rule.id(),
                root: PlanNodeId::checked(rule.root().index())
                    .expect("program node identity is representable"),
                name: rule.name().to_owned(),
                named_type: types.rule_type_required(rule.id()),
            })
            .collect::<Vec<_>>();
        let fields = program
            .fields()
            .iter()
            .map(|field| LoweredField {
                id: field.id(),
                owner: field.owner(),
                slot: field.slot(),
                child: PlanNodeId::checked(field.child().index())
                    .expect("program node identity is representable"),
                name: field.name().to_owned(),
                semantic_type: types.node_type_required(field.child()),
            })
            .collect();

        StageResult::accepted(Self {
            revision: program.revision(),
            nodes,
            rules,
            semantic_types: types.arena().to_vec(),
            fields,
            regexes,
            entry: program.entry(),
        })
    }

    pub const fn revision(&self) -> Revision {
        self.revision
    }

    pub fn nodes(&self) -> &[PlanNode] {
        &self.nodes
    }

    pub fn node(&self, id: PlanNodeId) -> Option<&PlanNode> {
        self.nodes.get(id.index())
    }

    pub fn rules(&self) -> &[LoweredRule] {
        &self.rules
    }

    pub fn semantic_types(&self) -> &[SemanticType] {
        &self.semantic_types
    }

    pub fn fields(&self) -> &[LoweredField] {
        &self.fields
    }

    pub fn regexes(&self) -> &[RegexPlanData] {
        &self.regexes
    }

    pub const fn entry(&self) -> RuleId {
        self.entry
    }
}
