use bbnf_regex::RegexTablePlan;
use serde::{Deserialize, Serialize};

use super::analysis::AnalysisFacts;
use super::budget::CompileOptions;
use super::diagnostic::{Diagnostic, DiagnosticCode, StageResult};
use super::source::Revision;
use super::syntax::{NodeKind, PlanNodeId, Program, RuleId, TransformId};
use super::typecheck::TypeSolution;

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
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

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
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
    Transform {
        child: PlanNodeId,
        transform: TransformId,
        name: String,
    },
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct PlanNode {
    id: PlanNodeId,
    op: PlanOp,
}

impl PlanNode {
    pub const fn id(&self) -> PlanNodeId {
        self.id
    }

    pub const fn op(&self) -> &PlanOp {
        &self.op
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct LoweredRule {
    id: RuleId,
    root: PlanNodeId,
    name: String,
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
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct LoweringPlan {
    revision: Revision,
    nodes: Vec<PlanNode>,
    rules: Vec<LoweredRule>,
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
            nodes.push(PlanNode { id, op });
        }

        let rules = program
            .rules()
            .iter()
            .map(|rule| LoweredRule {
                id: rule.id(),
                root: PlanNodeId::checked(rule.root().index())
                    .expect("program node identity is representable"),
                name: rule.name().to_owned(),
            })
            .collect();

        StageResult::accepted(Self {
            revision: program.revision(),
            nodes,
            rules,
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

    pub fn node(&self, id: PlanNodeId) -> &PlanNode {
        &self.nodes[id.index()]
    }

    pub fn rules(&self) -> &[LoweredRule] {
        &self.rules
    }

    pub fn regexes(&self) -> &[RegexPlanData] {
        &self.regexes
    }

    pub const fn entry(&self) -> RuleId {
        self.entry
    }
}
