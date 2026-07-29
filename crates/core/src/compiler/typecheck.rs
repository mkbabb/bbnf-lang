use std::collections::{BTreeMap, BTreeSet};
use std::ops::Range;

use serde::ser::SerializeSeq;
use serde::{Serialize, Serializer};

use super::dependency::DependencySolution;
use super::diagnostic::{Diagnostic, DiagnosticCode, StageResult};
use super::source::Revision;
use super::syntax::{
    FieldId, FieldIx, NodeId, NodeIds, NodeIx, NodeKindData, Program, ProjectionKeep,
    RecordSlotData, RuleId, RuleIx, TransformId, TransformIx, TypeId, TypeIds, TypeIx,
};

/// Compact semantic witness stored in the type arena.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize)]
pub(crate) enum SemanticTypeData {
    Unit,
    Span,
    Named(RuleIx),
    Product(Vec<TypeIx>),
    Sum(Vec<TypeIx>),
    Option(TypeIx),
    List(TypeIx),
    Record {
        owner: RuleIx,
        fields: Vec<RecordFieldTypeData>,
    },
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize)]
pub(crate) struct RecordFieldTypeData {
    field: FieldIx,
    value: TypeIx,
}

/// Public, carrier-aware view of one semantic witness.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum SemanticType<'a> {
    Unit,
    Span,
    Named(RuleId),
    Product(TypeIds<'a>),
    Sum(TypeIds<'a>),
    Option(TypeId),
    List(TypeId),
    Record {
        owner: RuleId,
        fields: RecordFieldTypes<'a>,
    },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RecordFieldType {
    field: FieldId,
    value: TypeId,
}

impl RecordFieldType {
    pub const fn field(self) -> FieldId {
        self.field
    }

    pub const fn value(self) -> TypeId {
        self.value
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RecordFieldTypes<'a> {
    revision: &'a Revision,
    fields: &'a [RecordFieldTypeData],
    remaining: Range<usize>,
}

impl<'a> RecordFieldTypes<'a> {
    fn new(revision: &'a Revision, fields: &'a [RecordFieldTypeData]) -> Self {
        Self {
            revision,
            fields,
            remaining: 0..fields.len(),
        }
    }

    pub fn get(&self, index: usize) -> Option<RecordFieldType> {
        let field = self.fields.get(index)?;
        Some(RecordFieldType {
            field: FieldId::bind(*self.revision, field.field),
            value: TypeId::bind(*self.revision, field.value),
        })
    }

    pub fn iter(&self) -> Self {
        Self::new(self.revision, self.fields)
    }

    pub fn is_empty(&self) -> bool {
        self.remaining.start >= self.remaining.end
    }
}

impl Iterator for RecordFieldTypes<'_> {
    type Item = RecordFieldType;

    fn next(&mut self) -> Option<Self::Item> {
        self.remaining.next().and_then(|index| self.get(index))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.remaining.size_hint()
    }
}

impl DoubleEndedIterator for RecordFieldTypes<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.remaining.next_back().and_then(|index| self.get(index))
    }
}

impl ExactSizeIterator for RecordFieldTypes<'_> {}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) enum RecordWitnessSlotData {
    Field { field: FieldIx, child: NodeIx },
    Discard { child: NodeIx },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum RecordWitnessSlot {
    Field { field: FieldId, child: NodeId },
    Discard { child: NodeId },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RecordWitnessSlots<'a> {
    revision: &'a Revision,
    slots: &'a [RecordWitnessSlotData],
    remaining: Range<usize>,
}

impl<'a> RecordWitnessSlots<'a> {
    fn new(revision: &'a Revision, slots: &'a [RecordWitnessSlotData]) -> Self {
        Self {
            revision,
            slots,
            remaining: 0..slots.len(),
        }
    }

    pub fn get(&self, index: usize) -> Option<RecordWitnessSlot> {
        match self.slots.get(index)? {
            RecordWitnessSlotData::Field { field, child } => Some(RecordWitnessSlot::Field {
                field: FieldId::bind(*self.revision, *field),
                child: NodeId::bind(*self.revision, *child),
            }),
            RecordWitnessSlotData::Discard { child } => Some(RecordWitnessSlot::Discard {
                child: NodeId::bind(*self.revision, *child),
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

impl Iterator for RecordWitnessSlots<'_> {
    type Item = RecordWitnessSlot;

    fn next(&mut self) -> Option<Self::Item> {
        self.remaining.next().and_then(|index| self.get(index))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.remaining.size_hint()
    }
}

impl DoubleEndedIterator for RecordWitnessSlots<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.remaining.next_back().and_then(|index| self.get(index))
    }
}

impl ExactSizeIterator for RecordWitnessSlots<'_> {}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) enum InferenceRuleData {
    EpsilonUnit,
    TerminalSpan,
    ReferenceNamed {
        rule: RuleIx,
    },
    SequenceProduct {
        children: Vec<NodeIx>,
    },
    ChoiceSum {
        children: Vec<NodeIx>,
    },
    Optional {
        child: NodeIx,
    },
    RepetitionList {
        child: NodeIx,
    },
    RecordProjection {
        owner: RuleIx,
        slots: Vec<RecordWitnessSlotData>,
    },
    TransformPreserves {
        child: NodeIx,
        transform: TransformIx,
    },
    Projection {
        left: NodeIx,
        right: NodeIx,
        keep: ProjectionKeep,
    },
    Difference {
        include: NodeIx,
        exclude: NodeIx,
    },
    Trim {
        child: NodeIx,
    },
    Pratt {
        atom: NodeIx,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum InferenceRule<'a> {
    EpsilonUnit,
    TerminalSpan,
    ReferenceNamed {
        rule: RuleId,
    },
    SequenceProduct {
        children: NodeIds<'a>,
    },
    ChoiceSum {
        children: NodeIds<'a>,
    },
    Optional {
        child: NodeId,
    },
    RepetitionList {
        child: NodeId,
    },
    RecordProjection {
        owner: RuleId,
        slots: RecordWitnessSlots<'a>,
    },
    TransformPreserves {
        child: NodeId,
        transform: TransformId,
    },
    Projection {
        left: NodeId,
        right: NodeId,
        keep: ProjectionKeep,
    },
    Difference {
        include: NodeId,
        exclude: NodeId,
    },
    Trim {
        child: NodeId,
    },
    Pratt {
        atom: NodeId,
    },
}

fn inference_view<'a>(revision: &'a Revision, data: &'a InferenceRuleData) -> InferenceRule<'a> {
    match data {
        InferenceRuleData::EpsilonUnit => InferenceRule::EpsilonUnit,
        InferenceRuleData::TerminalSpan => InferenceRule::TerminalSpan,
        InferenceRuleData::ReferenceNamed { rule } => InferenceRule::ReferenceNamed {
            rule: RuleId::bind(*revision, *rule),
        },
        InferenceRuleData::SequenceProduct { children } => InferenceRule::SequenceProduct {
            children: NodeIds::new(revision, children),
        },
        InferenceRuleData::ChoiceSum { children } => InferenceRule::ChoiceSum {
            children: NodeIds::new(revision, children),
        },
        InferenceRuleData::Optional { child } => InferenceRule::Optional {
            child: NodeId::bind(*revision, *child),
        },
        InferenceRuleData::RepetitionList { child } => InferenceRule::RepetitionList {
            child: NodeId::bind(*revision, *child),
        },
        InferenceRuleData::RecordProjection { owner, slots } => InferenceRule::RecordProjection {
            owner: RuleId::bind(*revision, *owner),
            slots: RecordWitnessSlots::new(revision, slots),
        },
        InferenceRuleData::TransformPreserves { child, transform } => {
            InferenceRule::TransformPreserves {
                child: NodeId::bind(*revision, *child),
                transform: TransformId::bind(*revision, *transform),
            }
        }
        InferenceRuleData::Projection { left, right, keep } => InferenceRule::Projection {
            left: NodeId::bind(*revision, *left),
            right: NodeId::bind(*revision, *right),
            keep: *keep,
        },
        InferenceRuleData::Difference { include, exclude } => InferenceRule::Difference {
            include: NodeId::bind(*revision, *include),
            exclude: NodeId::bind(*revision, *exclude),
        },
        InferenceRuleData::Trim { child } => InferenceRule::Trim {
            child: NodeId::bind(*revision, *child),
        },
        InferenceRuleData::Pratt { atom } => InferenceRule::Pratt {
            atom: NodeId::bind(*revision, *atom),
        },
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct NodeTypeDerivationData {
    node: NodeIx,
    type_id: TypeIx,
    inference: InferenceRuleData,
}

#[derive(Clone, Copy, Debug)]
pub struct NodeTypeDerivation<'a> {
    revision: &'a Revision,
    data: &'a NodeTypeDerivationData,
}

impl<'a> NodeTypeDerivation<'a> {
    pub const fn revision(self) -> Revision {
        *self.revision
    }

    pub const fn node(self) -> NodeId {
        NodeId::bind(*self.revision, self.data.node)
    }

    pub const fn type_id(self) -> TypeId {
        TypeId::bind(*self.revision, self.data.type_id)
    }

    pub fn inference(self) -> InferenceRule<'a> {
        inference_view(self.revision, &self.data.inference)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct RuleTypeDerivationData {
    rule: RuleIx,
    named_type: TypeIx,
    root_type: TypeIx,
}

#[derive(Clone, Copy, Debug)]
pub struct RuleTypeDerivation<'a> {
    revision: &'a Revision,
    data: &'a RuleTypeDerivationData,
}

impl RuleTypeDerivation<'_> {
    pub const fn revision(self) -> Revision {
        *self.revision
    }

    pub const fn rule(self) -> RuleId {
        RuleId::bind(*self.revision, self.data.rule)
    }

    pub const fn named_type(self) -> TypeId {
        TypeId::bind(*self.revision, self.data.named_type)
    }

    pub const fn root_type(self) -> TypeId {
        TypeId::bind(*self.revision, self.data.root_type)
    }
}

macro_rules! type_rows {
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

type_rows!(
    NodeTypeDerivations,
    NodeTypeDerivation,
    NodeTypeDerivationData
);
type_rows!(
    RuleTypeDerivations,
    RuleTypeDerivation,
    RuleTypeDerivationData
);

#[derive(Clone, Debug)]
pub struct SemanticTypes<'a> {
    revision: &'a Revision,
    rows: &'a [SemanticTypeData],
    remaining: Range<usize>,
}

impl<'a> SemanticTypes<'a> {
    pub(crate) fn new(revision: &'a Revision, rows: &'a [SemanticTypeData]) -> Self {
        Self {
            revision,
            rows,
            remaining: 0..rows.len(),
        }
    }

    pub fn get(&self, index: usize) -> Option<SemanticType<'a>> {
        self.rows
            .get(index)
            .map(|data| semantic_view(self.revision, data))
    }

    pub fn iter(&self) -> Self {
        Self::new(self.revision, self.rows)
    }

    pub fn is_empty(&self) -> bool {
        self.remaining.start >= self.remaining.end
    }
}

impl<'a> Iterator for SemanticTypes<'a> {
    type Item = SemanticType<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let index = self.remaining.next()?;
        self.get(index)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.remaining.size_hint()
    }
}

impl DoubleEndedIterator for SemanticTypes<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let index = self.remaining.next_back()?;
        self.get(index)
    }
}

impl ExactSizeIterator for SemanticTypes<'_> {}

fn semantic_view<'a>(revision: &'a Revision, data: &'a SemanticTypeData) -> SemanticType<'a> {
    match data {
        SemanticTypeData::Unit => SemanticType::Unit,
        SemanticTypeData::Span => SemanticType::Span,
        SemanticTypeData::Named(rule) => SemanticType::Named(RuleId::bind(*revision, *rule)),
        SemanticTypeData::Product(children) => {
            SemanticType::Product(TypeIds::new(revision, children))
        }
        SemanticTypeData::Sum(children) => SemanticType::Sum(TypeIds::new(revision, children)),
        SemanticTypeData::Option(child) => SemanticType::Option(TypeId::bind(*revision, *child)),
        SemanticTypeData::List(child) => SemanticType::List(TypeId::bind(*revision, *child)),
        SemanticTypeData::Record { owner, fields } => SemanticType::Record {
            owner: RuleId::bind(*revision, *owner),
            fields: RecordFieldTypes::new(revision, fields),
        },
    }
}

/// Deterministic interned type arena plus a principal derivation for every
/// node and named rule binder in one source revision.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeSolution {
    revision: Revision,
    arena: Vec<SemanticTypeData>,
    nodes: Vec<NodeTypeDerivationData>,
    rules: Vec<RuleTypeDerivationData>,
}

pub(super) struct TypeSolutionReplay<'a>(&'a TypeSolution);

impl Serialize for TypeSolutionReplay<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        #[derive(Serialize)]
        struct Wire<'a> {
            revision: Revision,
            arena: &'a [SemanticTypeData],
            nodes: NodeDerivationWireRows<'a>,
            rules: RuleDerivationWireRows<'a>,
        }

        Wire {
            revision: self.0.revision,
            arena: &self.0.arena,
            nodes: NodeDerivationWireRows {
                revision: self.0.revision,
                rows: &self.0.nodes,
            },
            rules: RuleDerivationWireRows {
                revision: self.0.revision,
                rows: &self.0.rules,
            },
        }
        .serialize(serializer)
    }
}

struct NodeDerivationWireRows<'a> {
    revision: Revision,
    rows: &'a [NodeTypeDerivationData],
}

impl Serialize for NodeDerivationWireRows<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        #[derive(Serialize)]
        struct Row<'a> {
            revision: Revision,
            node: NodeIx,
            type_id: TypeIx,
            inference: &'a InferenceRuleData,
        }

        let mut sequence = serializer.serialize_seq(Some(self.rows.len()))?;
        for row in self.rows {
            sequence.serialize_element(&Row {
                revision: self.revision,
                node: row.node,
                type_id: row.type_id,
                inference: &row.inference,
            })?;
        }
        sequence.end()
    }
}

struct RuleDerivationWireRows<'a> {
    revision: Revision,
    rows: &'a [RuleTypeDerivationData],
}

impl Serialize for RuleDerivationWireRows<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        #[derive(Serialize)]
        struct Row {
            revision: Revision,
            rule: RuleIx,
            named_type: TypeIx,
            root_type: TypeIx,
        }

        let mut sequence = serializer.serialize_seq(Some(self.rows.len()))?;
        for row in self.rows {
            sequence.serialize_element(&Row {
                revision: self.revision,
                rule: row.rule,
                named_type: row.named_type,
                root_type: row.root_type,
            })?;
        }
        sequence.end()
    }
}

impl TypeSolution {
    pub(super) fn replay_value(&self) -> TypeSolutionReplay<'_> {
        TypeSolutionReplay(self)
    }

    pub(crate) fn solve(program: &Program, dependency: &DependencySolution) -> StageResult<Self> {
        Self::solve_with_order(
            program,
            dependency,
            &(0..program.node_data_rows().len()).collect::<Vec<_>>(),
        )
    }

    fn solve_with_order(
        program: &Program,
        dependency: &DependencySolution,
        order: &[usize],
    ) -> StageResult<Self> {
        if program.revision() != dependency.revision() {
            return StageResult::rejected(revision_mismatch(
                "type solution",
                program.revision(),
                dependency.revision(),
            ));
        }

        let constraints = match collect_constraints(program, order) {
            Ok(constraints) => constraints,
            Err(diagnostic) => return StageResult::rejected(diagnostic),
        };

        let mut arena = Vec::new();
        let mut interned = BTreeMap::new();
        let mut named_by_rule = Vec::with_capacity(program.rule_data_rows().len());
        for rule in program.rule_data_rows() {
            named_by_rule.push(intern_type(
                SemanticTypeData::Named(rule.id()),
                &mut arena,
                &mut interned,
            ));
        }

        let mut node_types = vec![None; program.node_data_rows().len()];
        let mut states = vec![VisitState::Unseen; program.node_data_rows().len()];
        for index in 0..program.node_data_rows().len() {
            if let Err(diagnostic) = derive_node_type(
                index,
                program,
                &constraints,
                &mut states,
                &mut node_types,
                &mut arena,
                &mut interned,
            ) {
                return StageResult::rejected(diagnostic);
            }
        }

        let revision = program.revision();
        let nodes = program
            .node_data_rows()
            .iter()
            .map(|node| NodeTypeDerivationData {
                node: node.id(),
                type_id: node_types[node.id().index()].expect("all node types were derived"),
                inference: constraints[node.id().index()].inference(),
            })
            .collect();
        let rules = program
            .rule_data_rows()
            .iter()
            .map(|rule| RuleTypeDerivationData {
                rule: rule.id(),
                named_type: named_by_rule[rule.id().index()],
                root_type: node_types[rule.root().index()]
                    .expect("all rule root types were derived"),
            })
            .collect();

        StageResult::accepted(Self {
            revision,
            arena,
            nodes,
            rules,
        })
    }

    pub const fn revision(&self) -> Revision {
        self.revision
    }

    pub fn arena(&self) -> SemanticTypes<'_> {
        SemanticTypes::new(&self.revision, &self.arena)
    }

    pub fn semantic_type(&self, id: TypeId) -> Option<SemanticType<'_>> {
        (id.revision() == self.revision)
            .then(|| self.arena.get(id.local().index()))
            .flatten()
            .map(|data| semantic_view(&self.revision, data))
    }

    pub fn node_derivations(&self) -> NodeTypeDerivations<'_> {
        NodeTypeDerivations::new(&self.revision, &self.nodes)
    }

    pub fn rule_derivations(&self) -> RuleTypeDerivations<'_> {
        RuleTypeDerivations::new(&self.revision, &self.rules)
    }

    pub fn node_type(&self, node: NodeId) -> Option<TypeId> {
        (node.revision() == self.revision)
            .then(|| self.nodes.get(node.local().index()))
            .flatten()
            .map(|value| TypeId::bind(self.revision, value.type_id))
    }

    pub fn rule_type(&self, rule: RuleId) -> Option<TypeId> {
        (rule.revision() == self.revision)
            .then(|| self.rules.get(rule.local().index()))
            .flatten()
            .map(|value| TypeId::bind(self.revision, value.named_type))
    }

    pub fn rule_root_type(&self, rule: RuleId) -> Option<TypeId> {
        (rule.revision() == self.revision)
            .then(|| self.rules.get(rule.local().index()))
            .flatten()
            .map(|value| TypeId::bind(self.revision, value.root_type))
    }

    pub(crate) fn arena_data(&self) -> &[SemanticTypeData] {
        &self.arena
    }

    pub(crate) fn semantic_type_local(&self, id: TypeIx) -> &SemanticTypeData {
        &self.arena[id.index()]
    }

    pub(crate) fn node_type_local(&self, node: NodeIx) -> TypeIx {
        self.nodes[node.index()].type_id
    }

    pub(crate) fn rule_type_local(&self, rule: RuleIx) -> TypeIx {
        self.rules[rule.index()].named_type
    }

    /// Independently validate the sealed inference certificates against the
    /// resolved program. This does not rerun the solver.
    pub fn verify(&self, program: &Program) -> StageResult<()> {
        if self.revision != program.revision() {
            return StageResult::rejected(revision_mismatch(
                "type verification",
                program.revision(),
                self.revision,
            ));
        }
        if self.nodes.len() != program.node_data_rows().len()
            || self.rules.len() != program.rule_data_rows().len()
            || self.arena.len() > program.node_data_rows().len() + program.rule_data_rows().len()
        {
            return StageResult::rejected(type_invariant(
                "type carrier counts do not match the resolved program",
            ));
        }

        let unique = self.arena.iter().collect::<BTreeSet<_>>();
        if unique.len() != self.arena.len() {
            return StageResult::rejected(type_invariant(
                "type arena contains duplicate semantic witnesses",
            ));
        }

        for semantic in &self.arena {
            if let Err(message) = self.verify_semantic_type(semantic, program) {
                return StageResult::rejected(type_invariant(message));
            }
        }

        let mut used = BTreeSet::new();
        for (index, derivation) in self.nodes.iter().enumerate() {
            let node = &program.node_data_rows()[index];
            if derivation.node != node.id() || derivation.type_id.index() >= self.arena.len() {
                return StageResult::rejected(type_invariant(
                    "node type derivation identity is not canonical",
                ));
            }
            let expected = match self.expected_node_type(program, node, &derivation.inference) {
                Ok(expected) => expected,
                Err(message) => return StageResult::rejected(type_invariant(message)),
            };
            if self.semantic_type_local(derivation.type_id) != &expected {
                return StageResult::rejected(type_invariant(
                    "node inference conclusion does not follow from its certified premises",
                ));
            }
            used.insert(derivation.type_id);
        }

        for (index, derivation) in self.rules.iter().enumerate() {
            let rule = &program.rule_data_rows()[index];
            if derivation.rule != rule.id()
                || derivation.named_type.index() >= self.arena.len()
                || derivation.root_type.index() >= self.arena.len()
                || self.semantic_type_local(derivation.named_type)
                    != &SemanticTypeData::Named(rule.id())
                || derivation.root_type != self.nodes[rule.root().index()].type_id
            {
                return StageResult::rejected(type_invariant(
                    "rule type derivation is not the canonical named binder and root conclusion",
                ));
            }
            used.insert(derivation.named_type);
            used.insert(derivation.root_type);
        }

        let mut frontier = used.iter().copied().collect::<Vec<_>>();
        while let Some(type_id) = frontier.pop() {
            for child in semantic_children(self.semantic_type_local(type_id)) {
                if used.insert(child) {
                    frontier.push(child);
                }
            }
        }
        if used.len() != self.arena.len() {
            return StageResult::rejected(type_invariant(
                "type arena contains an unreachable semantic witness",
            ));
        }
        StageResult::accepted(())
    }

    fn verify_semantic_type(
        &self,
        semantic: &SemanticTypeData,
        program: &Program,
    ) -> Result<(), &'static str> {
        match semantic {
            SemanticTypeData::Unit | SemanticTypeData::Span => {}
            SemanticTypeData::Named(rule) => {
                if rule.index() >= program.rule_data_rows().len() {
                    return Err("named semantic type references an absent rule");
                }
            }
            SemanticTypeData::Product(children) | SemanticTypeData::Sum(children) => {
                if children
                    .iter()
                    .any(|child| child.index() >= self.arena.len())
                {
                    return Err("aggregate semantic type references an absent type");
                }
            }
            SemanticTypeData::Option(child) | SemanticTypeData::List(child) => {
                if child.index() >= self.arena.len() {
                    return Err("unary semantic type references an absent type");
                }
            }
            SemanticTypeData::Record { owner, fields } => {
                if owner.index() >= program.rule_data_rows().len() {
                    return Err("record semantic type references an absent owner");
                }
                let mut previous = None;
                for field in fields {
                    if field.field.index() >= program.field_data_rows().len()
                        || field.value.index() >= self.arena.len()
                        || program.field_data_rows()[field.field.index()].owner() != *owner
                    {
                        return Err("record semantic type contains a dangling field");
                    }
                    if previous.is_some_and(|id| id >= field.field) {
                        return Err("record semantic fields are not in canonical order");
                    }
                    previous = Some(field.field);
                }
            }
        }
        Ok(())
    }

    fn expected_node_type(
        &self,
        program: &Program,
        node: &super::syntax::NodeData,
        inference: &InferenceRuleData,
    ) -> Result<SemanticTypeData, &'static str> {
        let child_type = |child: NodeIx| {
            self.nodes
                .get(child.index())
                .filter(|derivation| derivation.node == child)
                .map(|derivation| derivation.type_id)
                .ok_or("inference premise references an absent child")
        };
        match (node.kind(), inference) {
            (NodeKindData::Epsilon, InferenceRuleData::EpsilonUnit) => Ok(SemanticTypeData::Unit),
            (
                NodeKindData::Literal(_) | NodeKindData::Regex(_),
                InferenceRuleData::TerminalSpan,
            ) => Ok(SemanticTypeData::Span),
            (NodeKindData::RuleRef(actual), InferenceRuleData::ReferenceNamed { rule })
                if actual == rule =>
            {
                Ok(SemanticTypeData::Named(*rule))
            }
            (NodeKindData::Sequence(actual), InferenceRuleData::SequenceProduct { children })
                if actual == children =>
            {
                Ok(SemanticTypeData::Product(
                    children
                        .iter()
                        .map(|child| child_type(*child))
                        .collect::<Result<_, _>>()?,
                ))
            }
            (NodeKindData::Choice(actual), InferenceRuleData::ChoiceSum { children })
                if actual == children =>
            {
                Ok(SemanticTypeData::Sum(
                    children
                        .iter()
                        .map(|child| child_type(*child))
                        .collect::<Result<_, _>>()?,
                ))
            }
            (
                NodeKindData::Repeat {
                    child: actual,
                    minimum: 0,
                    maximum: Some(1),
                },
                InferenceRuleData::Optional { child },
            ) if actual == child => Ok(SemanticTypeData::Option(child_type(*child)?)),
            (
                NodeKindData::Repeat { child: actual, .. },
                InferenceRuleData::RepetitionList { child },
            ) if actual == child => Ok(SemanticTypeData::List(child_type(*child)?)),
            (
                NodeKindData::Transform {
                    child: actual_child,
                    transform: actual_transform,
                    ..
                },
                InferenceRuleData::TransformPreserves { child, transform },
            ) if actual_child == child && actual_transform == transform => {
                Ok(self.semantic_type_local(child_type(*child)?).clone())
            }
            (
                NodeKindData::Record {
                    owner: actual_owner,
                    slots: actual_slots,
                },
                InferenceRuleData::RecordProjection { owner, slots },
            ) if actual_owner == owner && record_witness_matches(actual_slots, slots) => {
                let fields = slots
                    .iter()
                    .filter_map(|slot| match slot {
                        RecordWitnessSlotData::Field { field, child } => {
                            Some(child_type(*child).map(|value| RecordFieldTypeData {
                                field: *field,
                                value,
                            }))
                        }
                        RecordWitnessSlotData::Discard { .. } => None,
                    })
                    .collect::<Result<_, _>>()?;
                for slot in slots {
                    if let RecordWitnessSlotData::Field { field, child } = slot {
                        let declaration = &program.field_data_rows()[field.index()];
                        if declaration.owner() != *owner || declaration.child() != *child {
                            return Err("record inference field does not match its declaration");
                        }
                    }
                }
                Ok(SemanticTypeData::Record {
                    owner: *owner,
                    fields,
                })
            }
            (
                NodeKindData::Projection {
                    left: actual_left,
                    right: actual_right,
                    keep: actual_keep,
                },
                InferenceRuleData::Projection { left, right, keep },
            ) if actual_left == left && actual_right == right && actual_keep == keep => {
                let left_type = child_type(*left)?;
                let right_type = child_type(*right)?;
                let kept_type = match keep {
                    ProjectionKeep::Left => left_type,
                    ProjectionKeep::Right => right_type,
                };
                Ok(self.semantic_type_local(kept_type).clone())
            }
            (
                NodeKindData::Difference {
                    include: actual_include,
                    exclude: actual_exclude,
                },
                InferenceRuleData::Difference { include, exclude },
            ) if actual_include == include && actual_exclude == exclude => {
                let include_type = child_type(*include)?;
                let _ = child_type(*exclude)?;
                Ok(self.semantic_type_local(include_type).clone())
            }
            (
                NodeKindData::Trim {
                    child: actual_child,
                },
                InferenceRuleData::Trim { child },
            ) if actual_child == child => Ok(self.semantic_type_local(child_type(*child)?).clone()),
            (
                NodeKindData::Pratt {
                    atom: actual_atom, ..
                },
                InferenceRuleData::Pratt { atom },
            ) if actual_atom == atom => Ok(self.semantic_type_local(child_type(*atom)?).clone()),
            _ => Err("inference rule does not match the resolved node form"),
        }
    }
}

#[derive(Clone, Debug)]
enum TypeConstraint {
    Unit,
    Span,
    Named(RuleIx),
    Product(Vec<NodeIx>),
    Sum(Vec<NodeIx>),
    Option(NodeIx),
    List(NodeIx),
    Record {
        owner: RuleIx,
        slots: Vec<RecordSlotData>,
    },
    TransformAlias {
        child: NodeIx,
        transform: TransformIx,
    },
    Projection {
        left: NodeIx,
        right: NodeIx,
        keep: ProjectionKeep,
    },
    Difference {
        include: NodeIx,
        exclude: NodeIx,
    },
    Trim {
        child: NodeIx,
    },
    Pratt {
        atom: NodeIx,
    },
}

impl TypeConstraint {
    fn inference(&self) -> InferenceRuleData {
        match self {
            Self::Unit => InferenceRuleData::EpsilonUnit,
            Self::Span => InferenceRuleData::TerminalSpan,
            Self::Named(rule) => InferenceRuleData::ReferenceNamed { rule: *rule },
            Self::Product(children) => InferenceRuleData::SequenceProduct {
                children: children.clone(),
            },
            Self::Sum(children) => InferenceRuleData::ChoiceSum {
                children: children.clone(),
            },
            Self::Option(child) => InferenceRuleData::Optional { child: *child },
            Self::List(child) => InferenceRuleData::RepetitionList { child: *child },
            Self::Record { owner, slots } => InferenceRuleData::RecordProjection {
                owner: *owner,
                slots: slots
                    .iter()
                    .map(|slot| match slot {
                        RecordSlotData::Field { field, child } => RecordWitnessSlotData::Field {
                            field: *field,
                            child: *child,
                        },
                        RecordSlotData::Discard { child } => {
                            RecordWitnessSlotData::Discard { child: *child }
                        }
                    })
                    .collect(),
            },
            Self::TransformAlias { child, transform } => InferenceRuleData::TransformPreserves {
                child: *child,
                transform: *transform,
            },
            Self::Projection { left, right, keep } => InferenceRuleData::Projection {
                left: *left,
                right: *right,
                keep: *keep,
            },
            Self::Difference { include, exclude } => InferenceRuleData::Difference {
                include: *include,
                exclude: *exclude,
            },
            Self::Trim { child } => InferenceRuleData::Trim { child: *child },
            Self::Pratt { atom } => InferenceRuleData::Pratt { atom: *atom },
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum VisitState {
    Unseen,
    Visiting,
    Sealed,
}

fn collect_constraints(
    program: &Program,
    order: &[usize],
) -> Result<Vec<TypeConstraint>, Diagnostic> {
    if order.len() != program.node_data_rows().len() {
        return Err(invalid_constraint_order(program));
    }

    let mut constraints = vec![None; program.node_data_rows().len()];
    for &index in order {
        let Some(node) = program.node_data_rows().get(index) else {
            return Err(invalid_constraint_order(program));
        };
        if constraints[index].is_some() {
            return Err(invalid_constraint_order(program));
        }
        let constraint = match node.kind() {
            NodeKindData::Epsilon => TypeConstraint::Unit,
            NodeKindData::Literal(_) | NodeKindData::Regex(_) => TypeConstraint::Span,
            NodeKindData::RuleRef(rule) => TypeConstraint::Named(*rule),
            NodeKindData::Sequence(children) => TypeConstraint::Product(children.clone()),
            NodeKindData::Choice(children) => TypeConstraint::Sum(children.clone()),
            NodeKindData::Repeat {
                child,
                minimum: 0,
                maximum: Some(1),
            } => TypeConstraint::Option(*child),
            NodeKindData::Repeat { child, .. } => TypeConstraint::List(*child),
            NodeKindData::Record { owner, slots } => TypeConstraint::Record {
                owner: *owner,
                slots: slots.clone(),
            },
            NodeKindData::Transform {
                child, transform, ..
            } => TypeConstraint::TransformAlias {
                child: *child,
                transform: *transform,
            },
            NodeKindData::Projection { left, right, keep } => TypeConstraint::Projection {
                left: *left,
                right: *right,
                keep: *keep,
            },
            NodeKindData::Difference { include, exclude } => TypeConstraint::Difference {
                include: *include,
                exclude: *exclude,
            },
            NodeKindData::Trim { child } => TypeConstraint::Trim { child: *child },
            NodeKindData::Pratt { atom, .. } => TypeConstraint::Pratt { atom: *atom },
        };
        constraints[index] = Some(constraint);
    }

    constraints
        .into_iter()
        .map(|constraint| constraint.ok_or_else(|| invalid_constraint_order(program)))
        .collect()
}

#[allow(clippy::too_many_arguments)]
fn derive_node_type(
    index: usize,
    program: &Program,
    constraints: &[TypeConstraint],
    states: &mut [VisitState],
    node_types: &mut [Option<TypeIx>],
    arena: &mut Vec<SemanticTypeData>,
    interned: &mut BTreeMap<SemanticTypeData, TypeIx>,
) -> Result<TypeIx, Diagnostic> {
    match states.get(index).copied() {
        Some(VisitState::Sealed) => {
            return Ok(node_types[index].expect("sealed node carries a type"));
        }
        Some(VisitState::Visiting) => {
            let span = program.node_data_rows()[index].span();
            return Err(Diagnostic::new(
                DiagnosticCode::InternalInvariant,
                span.start() as usize,
                span.end() as usize,
                "structural type occurs-check failed: node containment is cyclic; \
                 recursive grammar references must cross a Named(RuleId) binder",
            ));
        }
        Some(VisitState::Unseen) => {}
        None => return Err(invalid_containment(program, index)),
    }

    states[index] = VisitState::Visiting;
    let witness = match &constraints[index] {
        TypeConstraint::Unit => SemanticTypeData::Unit,
        TypeConstraint::Span => SemanticTypeData::Span,
        TypeConstraint::Named(rule) => {
            if rule.index() >= program.rule_data_rows().len() {
                return Err(invalid_containment(program, index));
            }
            SemanticTypeData::Named(*rule)
        }
        TypeConstraint::Product(children) => SemanticTypeData::Product(derive_children(
            children,
            program,
            constraints,
            states,
            node_types,
            arena,
            interned,
        )?),
        TypeConstraint::Sum(children) => SemanticTypeData::Sum(derive_children(
            children,
            program,
            constraints,
            states,
            node_types,
            arena,
            interned,
        )?),
        TypeConstraint::Option(child) => SemanticTypeData::Option(derive_node_type(
            child.index(),
            program,
            constraints,
            states,
            node_types,
            arena,
            interned,
        )?),
        TypeConstraint::List(child) => SemanticTypeData::List(derive_node_type(
            child.index(),
            program,
            constraints,
            states,
            node_types,
            arena,
            interned,
        )?),
        TypeConstraint::Record { owner, slots } => {
            let mut fields = Vec::new();
            for slot in slots {
                match slot {
                    RecordSlotData::Field { field, child } => {
                        fields.push(RecordFieldTypeData {
                            field: *field,
                            value: derive_node_type(
                                child.index(),
                                program,
                                constraints,
                                states,
                                node_types,
                                arena,
                                interned,
                            )?,
                        });
                    }
                    RecordSlotData::Discard { child } => {
                        let _ = derive_node_type(
                            child.index(),
                            program,
                            constraints,
                            states,
                            node_types,
                            arena,
                            interned,
                        )?;
                    }
                }
            }
            SemanticTypeData::Record {
                owner: *owner,
                fields,
            }
        }
        TypeConstraint::TransformAlias { child, .. } => {
            let type_id = derive_node_type(
                child.index(),
                program,
                constraints,
                states,
                node_types,
                arena,
                interned,
            )?;
            node_types[index] = Some(type_id);
            states[index] = VisitState::Sealed;
            return Ok(type_id);
        }
        TypeConstraint::Projection { left, right, keep } => {
            let left_type = derive_node_type(
                left.index(),
                program,
                constraints,
                states,
                node_types,
                arena,
                interned,
            )?;
            let right_type = derive_node_type(
                right.index(),
                program,
                constraints,
                states,
                node_types,
                arena,
                interned,
            )?;
            let type_id = match keep {
                ProjectionKeep::Left => left_type,
                ProjectionKeep::Right => right_type,
            };
            node_types[index] = Some(type_id);
            states[index] = VisitState::Sealed;
            return Ok(type_id);
        }
        TypeConstraint::Difference { include, exclude } => {
            let type_id = derive_node_type(
                include.index(),
                program,
                constraints,
                states,
                node_types,
                arena,
                interned,
            )?;
            let _ = derive_node_type(
                exclude.index(),
                program,
                constraints,
                states,
                node_types,
                arena,
                interned,
            )?;
            node_types[index] = Some(type_id);
            states[index] = VisitState::Sealed;
            return Ok(type_id);
        }
        TypeConstraint::Trim { child } => {
            let type_id = derive_node_type(
                child.index(),
                program,
                constraints,
                states,
                node_types,
                arena,
                interned,
            )?;
            node_types[index] = Some(type_id);
            states[index] = VisitState::Sealed;
            return Ok(type_id);
        }
        TypeConstraint::Pratt { atom } => {
            let type_id = derive_node_type(
                atom.index(),
                program,
                constraints,
                states,
                node_types,
                arena,
                interned,
            )?;
            node_types[index] = Some(type_id);
            states[index] = VisitState::Sealed;
            return Ok(type_id);
        }
    };

    let type_id = intern_type(witness, arena, interned);
    node_types[index] = Some(type_id);
    states[index] = VisitState::Sealed;
    Ok(type_id)
}

fn record_witness_matches(actual: &[RecordSlotData], witness: &[RecordWitnessSlotData]) -> bool {
    actual.len() == witness.len()
        && actual
            .iter()
            .zip(witness)
            .all(|(actual, witness)| match (actual, witness) {
                (
                    RecordSlotData::Field {
                        field: actual_field,
                        child: actual_child,
                    },
                    RecordWitnessSlotData::Field {
                        field: witness_field,
                        child: witness_child,
                    },
                ) => actual_field == witness_field && actual_child == witness_child,
                (
                    RecordSlotData::Discard {
                        child: actual_child,
                    },
                    RecordWitnessSlotData::Discard {
                        child: witness_child,
                    },
                ) => actual_child == witness_child,
                _ => false,
            })
}

fn semantic_children(semantic: &SemanticTypeData) -> Vec<TypeIx> {
    match semantic {
        SemanticTypeData::Unit | SemanticTypeData::Span | SemanticTypeData::Named(_) => Vec::new(),
        SemanticTypeData::Product(children) | SemanticTypeData::Sum(children) => children.clone(),
        SemanticTypeData::Option(child) | SemanticTypeData::List(child) => vec![*child],
        SemanticTypeData::Record { fields, .. } => fields.iter().map(|field| field.value).collect(),
    }
}

#[allow(clippy::too_many_arguments)]
fn derive_children(
    children: &[NodeIx],
    program: &Program,
    constraints: &[TypeConstraint],
    states: &mut [VisitState],
    node_types: &mut [Option<TypeIx>],
    arena: &mut Vec<SemanticTypeData>,
    interned: &mut BTreeMap<SemanticTypeData, TypeIx>,
) -> Result<Vec<TypeIx>, Diagnostic> {
    children
        .iter()
        .map(|child| {
            derive_node_type(
                child.index(),
                program,
                constraints,
                states,
                node_types,
                arena,
                interned,
            )
        })
        .collect()
}

fn intern_type(
    witness: SemanticTypeData,
    arena: &mut Vec<SemanticTypeData>,
    interned: &mut BTreeMap<SemanticTypeData, TypeIx>,
) -> TypeIx {
    if let Some(&existing) = interned.get(&witness) {
        return existing;
    }
    let id = TypeIx::checked(arena.len()).expect("node budget bounds type ordinals");
    interned.insert(witness.clone(), id);
    arena.push(witness);
    id
}

fn invalid_constraint_order(program: &Program) -> Diagnostic {
    Diagnostic::new(
        DiagnosticCode::InternalInvariant,
        0,
        0,
        format!(
            "type constraint schedule is not a permutation of 0..{}",
            program.node_data_rows().len()
        ),
    )
}

fn invalid_containment(program: &Program, index: usize) -> Diagnostic {
    let (start, end) = program
        .node_data_rows()
        .get(index)
        .map(|node| {
            let span = node.span();
            (span.start() as usize, span.end() as usize)
        })
        .unwrap_or((0, 0));
    Diagnostic::new(
        DiagnosticCode::InternalInvariant,
        start,
        end,
        format!("type containment references invalid node identity {index}"),
    )
}

fn type_invariant(message: impl Into<String>) -> Diagnostic {
    Diagnostic::new(DiagnosticCode::InternalInvariant, 0, 0, message)
}

fn revision_mismatch(stage: &str, program: Revision, dependency: Revision) -> Diagnostic {
    Diagnostic::new(
        DiagnosticCode::RevisionMismatch,
        0,
        0,
        format!(
            "{stage} cannot mix program revision {} with dependency revision {}",
            program.hex(),
            dependency.hex()
        ),
    )
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::*;
    use crate::compiler::instance::UNIT_INSTANCE;
    use crate::compiler::{CompileOptions, GrammarInstanceInput, compile_instance};

    fn with_type_fixture(source: &str, inspect: impl FnOnce(&Program, &DependencySolution)) {
        let options = CompileOptions::default();
        let compiled = match compile_instance(
            GrammarInstanceInput::new(UNIT_INSTANCE, source.as_bytes()),
            options,
        ) {
            StageResult::Accepted { value, .. } => value,
            StageResult::Rejected { .. } => panic!("grammar-instance fixture must compile"),
        };
        inspect(compiled.program(), compiled.dependency());
    }

    #[test]
    fn type_storage_uses_four_byte_ordinals_and_pre_branding_row_widths() {
        assert_eq!(size_of::<TypeIx>(), 4);
        assert_eq!(size_of::<RecordFieldTypeData>(), 8);
        assert!(size_of::<SemanticTypeData>() <= 32);
        assert!(size_of::<InferenceRuleData>() <= 40);
        assert!(size_of::<TypeConstraint>() <= 40);
        assert!(size_of::<RuleTypeDerivationData>() <= 12);
        assert!(size_of::<NodeTypeDerivationData>() <= 48);
    }

    #[test]
    fn constraint_discovery_permutations_seal_identically() {
        with_type_fixture(
            r#"
root = value;
value = object | array | "null";
object = "{", (value, ":", value)?, "}";
array = "[", (value, (",", value)*)?, "]";
"#,
            |program, dependency| {
                let canonical = match TypeSolution::solve(program, dependency) {
                    StageResult::Accepted { value, .. } => value,
                    StageResult::Rejected { .. } => panic!("recursive binders must solve"),
                };
                for seed in 1..=16 {
                    let schedule = deterministic_permutation(program.node_data_rows().len(), seed);
                    let candidate =
                        match TypeSolution::solve_with_order(program, dependency, &schedule) {
                            StageResult::Accepted { value, .. } => value,
                            StageResult::Rejected { .. } => {
                                panic!("permuted constraints must solve")
                            }
                        };
                    assert_eq!(candidate, canonical);
                }
            },
        );
    }

    #[test]
    fn structural_occurs_check_rejects_non_nominal_cycle() {
        with_type_fixture(r#"root = "x";"#, |program, _| {
            let root = program.node_data_rows()[0].id();
            let constraints = vec![TypeConstraint::TransformAlias {
                child: root,
                transform: TransformIx::checked(0).expect("zero ordinal"),
            }];
            let mut states = vec![VisitState::Unseen];
            let mut node_types = vec![None];
            let mut arena = Vec::new();
            let mut interned = BTreeMap::new();
            let error = derive_node_type(
                0,
                program,
                &constraints,
                &mut states,
                &mut node_types,
                &mut arena,
                &mut interned,
            )
            .expect_err("a structural self occurrence is not a named binder");
            assert_eq!(error.code(), DiagnosticCode::InternalInvariant);
            assert!(error.message().contains("occurs-check"));
        });
    }

    fn deterministic_permutation(count: usize, mut state: u64) -> Vec<usize> {
        let mut order = (0..count).collect::<Vec<_>>();
        for upper in (1..count).rev() {
            state = state
                .wrapping_mul(6_364_136_223_846_793_005)
                .wrapping_add(1_442_695_040_888_963_407);
            let selected = (state as usize) % (upper + 1);
            order.swap(upper, selected);
        }
        order
    }
}
