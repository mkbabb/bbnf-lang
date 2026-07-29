use std::ops::Range;

use bbnf_regex::RegexTablePlan;
use serde::Serialize;

use super::analysis::AnalysisFacts;
use super::budget::CompileOptions;
use super::diagnostic::{Diagnostic, DiagnosticCode, StageResult};
use super::source::Revision;
use super::syntax::{
    FieldId, FieldIx, NodeKindData, PlanNodeId, PlanNodeIds, PlanNodeIx, Program, RecordSlotData,
    RuleId, RuleIx, TransformId, TransformIx, TypeId, TypeIx,
};
use super::typecheck::{SemanticTypeData, SemanticTypes, TypeSolution};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LoweredRegex {
    byte_classes: Vec<u8>,
    transitions: Vec<u32>,
    accepting: Vec<bool>,
    state_count: u32,
    class_count: u16,
    start_state: u32,
}

impl LoweredRegex {
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

impl From<RegexTablePlan> for LoweredRegex {
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
pub(crate) enum PlanRecordSlotData {
    Field { field: FieldIx, child: PlanNodeIx },
    Discard { child: PlanNodeIx },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum PlanRecordSlot {
    Field { field: FieldId, child: PlanNodeId },
    Discard { child: PlanNodeId },
}

impl PlanRecordSlot {
    pub const fn child(self) -> PlanNodeId {
        match self {
            Self::Field { child, .. } | Self::Discard { child } => child,
        }
    }

    pub const fn field(self) -> Option<FieldId> {
        match self {
            Self::Field { field, .. } => Some(field),
            Self::Discard { .. } => None,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlanRecordSlots<'a> {
    revision: &'a Revision,
    slots: &'a [PlanRecordSlotData],
    remaining: Range<usize>,
}

impl<'a> PlanRecordSlots<'a> {
    fn new(revision: &'a Revision, slots: &'a [PlanRecordSlotData]) -> Self {
        Self {
            revision,
            slots,
            remaining: 0..slots.len(),
        }
    }

    pub fn get(&self, index: usize) -> Option<PlanRecordSlot> {
        match self.slots.get(index)? {
            PlanRecordSlotData::Field { field, child } => Some(PlanRecordSlot::Field {
                field: FieldId::bind(*self.revision, *field),
                child: PlanNodeId::bind(*self.revision, *child),
            }),
            PlanRecordSlotData::Discard { child } => Some(PlanRecordSlot::Discard {
                child: PlanNodeId::bind(*self.revision, *child),
            }),
        }
    }

    pub fn iter(&self) -> Self {
        Self::new(self.revision, self.slots)
    }

    pub fn is_empty(&self) -> bool {
        self.remaining.start >= self.remaining.end
    }
}

impl Iterator for PlanRecordSlots<'_> {
    type Item = PlanRecordSlot;

    fn next(&mut self) -> Option<Self::Item> {
        self.remaining.next().and_then(|index| self.get(index))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.remaining.size_hint()
    }
}

impl DoubleEndedIterator for PlanRecordSlots<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.remaining.next_back().and_then(|index| self.get(index))
    }
}

impl ExactSizeIterator for PlanRecordSlots<'_> {}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) enum PlanOpData {
    Epsilon,
    Literal(Vec<u8>),
    Regex(usize),
    RuleRef(RuleIx),
    Sequence(Vec<PlanNodeIx>),
    Choice(Vec<PlanNodeIx>),
    Repeat {
        child: PlanNodeIx,
        minimum: u32,
        maximum: Option<u32>,
    },
    Record {
        owner: RuleIx,
        slots: Vec<PlanRecordSlotData>,
    },
    Transform {
        child: PlanNodeIx,
        transform: TransformIx,
        name: String,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum PlanOp<'a> {
    Epsilon,
    Literal(&'a [u8]),
    Regex(usize),
    RuleRef(RuleId),
    Sequence(PlanNodeIds<'a>),
    Choice(PlanNodeIds<'a>),
    Repeat {
        child: PlanNodeId,
        minimum: u32,
        maximum: Option<u32>,
    },
    Record {
        owner: RuleId,
        slots: PlanRecordSlots<'a>,
    },
    Transform {
        child: PlanNodeId,
        transform: TransformId,
        name: &'a str,
    },
}

fn plan_op_view<'a>(revision: &'a Revision, data: &'a PlanOpData) -> PlanOp<'a> {
    match data {
        PlanOpData::Epsilon => PlanOp::Epsilon,
        PlanOpData::Literal(bytes) => PlanOp::Literal(bytes),
        PlanOpData::Regex(index) => PlanOp::Regex(*index),
        PlanOpData::RuleRef(rule) => PlanOp::RuleRef(RuleId::bind(*revision, *rule)),
        PlanOpData::Sequence(children) => PlanOp::Sequence(PlanNodeIds::new(revision, children)),
        PlanOpData::Choice(children) => PlanOp::Choice(PlanNodeIds::new(revision, children)),
        PlanOpData::Repeat {
            child,
            minimum,
            maximum,
        } => PlanOp::Repeat {
            child: PlanNodeId::bind(*revision, *child),
            minimum: *minimum,
            maximum: *maximum,
        },
        PlanOpData::Record { owner, slots } => PlanOp::Record {
            owner: RuleId::bind(*revision, *owner),
            slots: PlanRecordSlots::new(revision, slots),
        },
        PlanOpData::Transform {
            child,
            transform,
            name,
        } => PlanOp::Transform {
            child: PlanNodeId::bind(*revision, *child),
            transform: TransformId::bind(*revision, *transform),
            name,
        },
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct PlanNodeData {
    id: PlanNodeIx,
    op: PlanOpData,
    semantic_type: TypeIx,
}

#[derive(Clone, Copy, Debug)]
pub struct PlanNode<'a> {
    revision: &'a Revision,
    data: &'a PlanNodeData,
}

impl<'a> PlanNode<'a> {
    pub const fn id(self) -> PlanNodeId {
        PlanNodeId::bind(*self.revision, self.data.id)
    }

    pub fn op(self) -> PlanOp<'a> {
        plan_op_view(self.revision, &self.data.op)
    }

    pub const fn semantic_type(self) -> TypeId {
        TypeId::bind(*self.revision, self.data.semantic_type)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct LoweredRuleData {
    id: RuleIx,
    root: PlanNodeIx,
    name: String,
    named_type: TypeIx,
}

#[derive(Clone, Copy, Debug)]
pub struct LoweredRule<'a> {
    revision: &'a Revision,
    data: &'a LoweredRuleData,
}

impl<'a> LoweredRule<'a> {
    pub const fn id(self) -> RuleId {
        RuleId::bind(*self.revision, self.data.id)
    }

    pub const fn root(self) -> PlanNodeId {
        PlanNodeId::bind(*self.revision, self.data.root)
    }

    pub fn name(self) -> &'a str {
        &self.data.name
    }

    pub const fn named_type(self) -> TypeId {
        TypeId::bind(*self.revision, self.data.named_type)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct LoweredFieldData {
    id: FieldIx,
    owner: RuleIx,
    slot: u32,
    child: PlanNodeIx,
    name: String,
    semantic_type: TypeIx,
}

#[derive(Clone, Copy, Debug)]
pub struct LoweredField<'a> {
    revision: &'a Revision,
    data: &'a LoweredFieldData,
}

impl<'a> LoweredField<'a> {
    pub const fn id(self) -> FieldId {
        FieldId::bind(*self.revision, self.data.id)
    }

    pub const fn owner(self) -> RuleId {
        RuleId::bind(*self.revision, self.data.owner)
    }

    pub const fn slot(self) -> u32 {
        self.data.slot
    }

    pub const fn child(self) -> PlanNodeId {
        PlanNodeId::bind(*self.revision, self.data.child)
    }

    pub fn name(self) -> &'a str {
        &self.data.name
    }

    pub const fn semantic_type(self) -> TypeId {
        TypeId::bind(*self.revision, self.data.semantic_type)
    }
}

macro_rules! lowering_rows {
    ($name:ident, $view:ident, $data:ident) => {
        #[derive(Clone, Debug)]
        pub struct $name<'a> {
            revision: &'a Revision,
            rows: &'a [$data],
            remaining: Range<usize>,
        }

        impl<'a> $name<'a> {
            fn new(revision: &'a Revision, rows: &'a [$data]) -> Self {
                Self {
                    revision,
                    rows,
                    remaining: 0..rows.len(),
                }
            }

            pub fn get(&self, index: usize) -> Option<$view<'a>> {
                self.rows.get(index).map(|data| $view {
                    revision: self.revision,
                    data,
                })
            }

            pub fn first(&self) -> Option<$view<'a>> {
                self.get(0)
            }

            pub fn last(&self) -> Option<$view<'a>> {
                self.rows.last().map(|data| $view {
                    revision: self.revision,
                    data,
                })
            }

            pub fn iter(&self) -> Self {
                Self::new(self.revision, self.rows)
            }

            pub fn is_empty(&self) -> bool {
                self.remaining.start >= self.remaining.end
            }
        }

        impl<'a> Iterator for $name<'a> {
            type Item = $view<'a>;

            fn next(&mut self) -> Option<Self::Item> {
                let index = self.remaining.next()?;
                self.get(index)
            }

            fn size_hint(&self) -> (usize, Option<usize>) {
                self.remaining.size_hint()
            }
        }

        impl DoubleEndedIterator for $name<'_> {
            fn next_back(&mut self) -> Option<Self::Item> {
                let index = self.remaining.next_back()?;
                self.get(index)
            }
        }

        impl ExactSizeIterator for $name<'_> {}
    };
}

lowering_rows!(PlanNodes, PlanNode, PlanNodeData);
lowering_rows!(LoweredRules, LoweredRule, LoweredRuleData);
lowering_rows!(LoweredFields, LoweredField, LoweredFieldData);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LoweringPlan {
    revision: Revision,
    nodes: Vec<PlanNodeData>,
    rules: Vec<LoweredRuleData>,
    semantic_types: Vec<SemanticTypeData>,
    fields: Vec<LoweredFieldData>,
    regexes: Vec<LoweredRegex>,
    entry: RuleIx,
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
        let mut nodes = Vec::with_capacity(program.node_data_rows().len());
        for node in program.node_data_rows() {
            let id = PlanNodeIx::checked(node.id().index())
                .expect("program node ordinal is representable");
            let op = match node.kind() {
                NodeKindData::Epsilon => PlanOpData::Epsilon,
                NodeKindData::Literal(bytes) => PlanOpData::Literal(bytes.clone()),
                NodeKindData::Regex(pattern) => {
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
                    PlanOpData::Regex(index)
                }
                NodeKindData::RuleRef(rule) => PlanOpData::RuleRef(*rule),
                NodeKindData::Sequence(children) => PlanOpData::Sequence(
                    children
                        .iter()
                        .map(|child| {
                            PlanNodeIx::checked(child.index())
                                .expect("program node ordinal is representable")
                        })
                        .collect(),
                ),
                NodeKindData::Choice(children) => PlanOpData::Choice(
                    children
                        .iter()
                        .map(|child| {
                            PlanNodeIx::checked(child.index())
                                .expect("program node ordinal is representable")
                        })
                        .collect(),
                ),
                NodeKindData::Repeat {
                    child,
                    minimum,
                    maximum,
                } => PlanOpData::Repeat {
                    child: PlanNodeIx::checked(child.index())
                        .expect("program node ordinal is representable"),
                    minimum: *minimum,
                    maximum: *maximum,
                },
                NodeKindData::Record { owner, slots } => PlanOpData::Record {
                    owner: *owner,
                    slots: slots
                        .iter()
                        .map(|slot| match slot {
                            RecordSlotData::Field { field, child } => PlanRecordSlotData::Field {
                                field: *field,
                                child: PlanNodeIx::checked(child.index())
                                    .expect("program node ordinal is representable"),
                            },
                            RecordSlotData::Discard { child } => PlanRecordSlotData::Discard {
                                child: PlanNodeIx::checked(child.index())
                                    .expect("program node ordinal is representable"),
                            },
                        })
                        .collect(),
                },
                NodeKindData::Transform {
                    child,
                    transform,
                    name,
                } => PlanOpData::Transform {
                    child: PlanNodeIx::checked(child.index())
                        .expect("program node ordinal is representable"),
                    transform: *transform,
                    name: name.clone(),
                },
            };
            nodes.push(PlanNodeData {
                id,
                op,
                semantic_type: types.node_type_local(node.id()),
            });
        }

        let rules = program
            .rule_data_rows()
            .iter()
            .map(|rule| LoweredRuleData {
                id: rule.id(),
                root: PlanNodeIx::checked(rule.root().index())
                    .expect("program node ordinal is representable"),
                name: rule.name().to_owned(),
                named_type: types.rule_type_local(rule.id()),
            })
            .collect::<Vec<_>>();
        let fields = program
            .field_data_rows()
            .iter()
            .map(|field| LoweredFieldData {
                id: field.id(),
                owner: field.owner(),
                slot: field.slot(),
                child: PlanNodeIx::checked(field.child().index())
                    .expect("program node ordinal is representable"),
                name: field.name().to_owned(),
                semantic_type: types.node_type_local(field.child()),
            })
            .collect();

        StageResult::accepted(Self {
            revision: program.revision(),
            nodes,
            rules,
            semantic_types: types.arena_data().to_vec(),
            fields,
            regexes,
            entry: program.entry_local(),
        })
    }

    pub const fn revision(&self) -> Revision {
        self.revision
    }

    pub fn nodes(&self) -> PlanNodes<'_> {
        PlanNodes::new(&self.revision, &self.nodes)
    }

    pub fn node(&self, id: PlanNodeId) -> Option<PlanNode<'_>> {
        (id.revision() == self.revision)
            .then(|| {
                self.nodes.get(id.local().index()).map(|data| PlanNode {
                    revision: &self.revision,
                    data,
                })
            })
            .flatten()
    }

    pub fn rules(&self) -> LoweredRules<'_> {
        LoweredRules::new(&self.revision, &self.rules)
    }

    pub fn semantic_types(&self) -> SemanticTypes<'_> {
        SemanticTypes::new(&self.revision, &self.semantic_types)
    }

    pub fn fields(&self) -> LoweredFields<'_> {
        LoweredFields::new(&self.revision, &self.fields)
    }

    pub fn regexes(&self) -> &[LoweredRegex] {
        &self.regexes
    }

    pub const fn entry(&self) -> RuleId {
        RuleId::bind(self.revision, self.entry)
    }

    pub(crate) fn node_data_rows(&self) -> &[PlanNodeData] {
        &self.nodes
    }

    pub(crate) fn rule_data_rows(&self) -> &[LoweredRuleData] {
        &self.rules
    }

    pub(super) fn replay_value(&self) -> LoweringPlanReplay<'_> {
        LoweringPlanReplay {
            revision: self.revision,
            nodes: &self.nodes,
            rules: &self.rules,
            semantic_types: &self.semantic_types,
            fields: &self.fields,
            regexes: RegexPlansReplay(&self.regexes),
            entry: self.entry,
        }
    }
}

#[derive(Serialize)]
pub(super) struct LoweringPlanReplay<'a> {
    revision: Revision,
    nodes: &'a [PlanNodeData],
    rules: &'a [LoweredRuleData],
    semantic_types: &'a [SemanticTypeData],
    fields: &'a [LoweredFieldData],
    regexes: RegexPlansReplay<'a>,
    entry: RuleIx,
}

struct RegexPlansReplay<'a>(&'a [LoweredRegex]);

impl Serialize for RegexPlansReplay<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        #[derive(Serialize)]
        struct Regex<'a> {
            byte_classes: &'a [u8],
            transitions: &'a [u32],
            accepting: &'a [bool],
            state_count: u32,
            class_count: u16,
            start_state: u32,
        }

        use serde::ser::SerializeSeq;
        let mut sequence = serializer.serialize_seq(Some(self.0.len()))?;
        for regex in self.0 {
            sequence.serialize_element(&Regex {
                byte_classes: &regex.byte_classes,
                transitions: &regex.transitions,
                accepting: &regex.accepting,
                state_count: regex.state_count,
                class_count: regex.class_count,
                start_state: regex.start_state,
            })?;
        }
        sequence.end()
    }
}

impl PlanNodeData {
    pub(crate) const fn id(&self) -> PlanNodeIx {
        self.id
    }

    pub(crate) const fn op(&self) -> &PlanOpData {
        &self.op
    }
}

impl LoweredRuleData {
    pub(crate) const fn id(&self) -> RuleIx {
        self.id
    }

    pub(crate) const fn root(&self) -> PlanNodeIx {
        self.root
    }
}

#[cfg(test)]
mod representation_laws {
    use std::mem::size_of;

    use super::*;

    #[test]
    fn lowering_rows_keep_local_ordinals_and_pre_branding_widths() {
        assert_eq!(size_of::<PlanNodeIx>(), 4);
        assert_eq!(size_of::<Option<PlanNodeIx>>(), 8);
        assert!(size_of::<PlanRecordSlotData>() <= 12);
        assert!(size_of::<PlanNodeData>() <= 48);
        assert!(size_of::<LoweredRuleData>() <= 40);
        assert!(size_of::<LoweredFieldData>() <= 48);
        assert!(size_of::<PlanOpData>() <= 40);
    }
}
