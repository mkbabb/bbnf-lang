use std::collections::{BTreeMap, BTreeSet};

use serde::Serialize;

use super::dependency::DependencySolution;
use super::diagnostic::{Diagnostic, DiagnosticCode, StageResult};
use super::source::Revision;
use super::syntax::{FieldId, NodeId, NodeKind, Program, RecordSlot, RuleId, TransformId, TypeId};

/// First-order semantic witnesses admitted by the W2 scalar language.
///
/// `Named` is the only recursive constructor. A rule reference therefore
/// closes over a finite `RuleId` binder instead of unfolding the referenced
/// rule's root type into an infinite arena.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[non_exhaustive]
pub enum SemanticType {
    Unit,
    Span,
    Named(RuleId),
    Product(Vec<TypeId>),
    Sum(Vec<TypeId>),
    Option(TypeId),
    List(TypeId),
    Record {
        owner: RuleId,
        fields: Vec<RecordFieldType>,
    },
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize)]
pub struct RecordFieldType {
    field: FieldId,
    value: TypeId,
}

impl RecordFieldType {
    pub const fn field(&self) -> FieldId {
        self.field
    }

    pub const fn value(&self) -> TypeId {
        self.value
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[non_exhaustive]
pub enum RecordWitnessSlot {
    Field { field: FieldId, child: NodeId },
    Discard { child: NodeId },
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[non_exhaustive]
pub enum InferenceRule {
    EpsilonUnit,
    TerminalSpan,
    ReferenceNamed {
        rule: RuleId,
    },
    SequenceProduct {
        children: Vec<NodeId>,
    },
    ChoiceSum {
        children: Vec<NodeId>,
    },
    Optional {
        child: NodeId,
    },
    RepetitionList {
        child: NodeId,
    },
    RecordProjection {
        owner: RuleId,
        slots: Vec<RecordWitnessSlot>,
    },
    TransformPreserves {
        child: NodeId,
        transform: TransformId,
    },
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct NodeTypeDerivation {
    revision: Revision,
    node: NodeId,
    type_id: TypeId,
    inference: InferenceRule,
}

impl NodeTypeDerivation {
    pub const fn revision(&self) -> Revision {
        self.revision
    }

    pub const fn node(&self) -> NodeId {
        self.node
    }

    pub const fn type_id(&self) -> TypeId {
        self.type_id
    }

    pub const fn inference(&self) -> &InferenceRule {
        &self.inference
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct RuleTypeDerivation {
    revision: Revision,
    rule: RuleId,
    named_type: TypeId,
    root_type: TypeId,
}

impl RuleTypeDerivation {
    pub const fn revision(&self) -> Revision {
        self.revision
    }

    pub const fn rule(&self) -> RuleId {
        self.rule
    }

    pub const fn named_type(&self) -> TypeId {
        self.named_type
    }

    pub const fn root_type(&self) -> TypeId {
        self.root_type
    }
}

/// Deterministic interned type arena plus a principal derivation for every
/// node and named rule binder in one source revision.
#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct TypeSolution {
    revision: Revision,
    arena: Vec<SemanticType>,
    nodes: Vec<NodeTypeDerivation>,
    rules: Vec<RuleTypeDerivation>,
}

impl TypeSolution {
    pub(crate) fn solve(program: &Program, dependency: &DependencySolution) -> StageResult<Self> {
        Self::solve_with_order(
            program,
            dependency,
            &(0..program.nodes().len()).collect::<Vec<_>>(),
        )
    }

    /// Collect constraints in an arbitrary order, then seal them canonically.
    ///
    /// Keeping collection order separate from canonicalization is important:
    /// parallel or incremental discovery must not perturb `TypeId`s or the
    /// serialized solution.
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
        let mut named_by_rule = Vec::with_capacity(program.rules().len());
        for rule in program.rules() {
            let type_id = intern_type(SemanticType::Named(rule.id()), &mut arena, &mut interned);
            named_by_rule.push(type_id);
        }

        let mut node_types = vec![None; program.nodes().len()];
        let mut states = vec![VisitState::Unseen; program.nodes().len()];
        for index in 0..program.nodes().len() {
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

        let nodes = program
            .nodes()
            .iter()
            .map(|node| NodeTypeDerivation {
                revision: program.revision(),
                node: node.id(),
                type_id: node_types[node.id().index()].expect("all node types were derived"),
                inference: constraints[node.id().index()].inference(),
            })
            .collect();
        let rules = program
            .rules()
            .iter()
            .map(|rule| RuleTypeDerivation {
                revision: program.revision(),
                rule: rule.id(),
                named_type: named_by_rule[rule.id().index()],
                root_type: node_types[rule.root().index()]
                    .expect("all rule root types were derived"),
            })
            .collect();

        StageResult::accepted(Self {
            revision: program.revision(),
            arena,
            nodes,
            rules,
        })
    }

    pub const fn revision(&self) -> Revision {
        self.revision
    }

    pub fn arena(&self) -> &[SemanticType] {
        &self.arena
    }

    pub fn semantic_type(&self, id: TypeId) -> Option<&SemanticType> {
        self.arena.get(id.index())
    }

    pub fn node_derivations(&self) -> &[NodeTypeDerivation] {
        &self.nodes
    }

    pub fn rule_derivations(&self) -> &[RuleTypeDerivation] {
        &self.rules
    }

    pub fn node_type(&self, node: NodeId) -> Option<TypeId> {
        self.nodes.get(node.index()).map(|value| value.type_id)
    }

    pub fn rule_type(&self, rule: RuleId) -> Option<TypeId> {
        self.rules.get(rule.index()).map(|value| value.named_type)
    }

    pub fn rule_root_type(&self, rule: RuleId) -> Option<TypeId> {
        self.rules.get(rule.index()).map(|value| value.root_type)
    }

    pub(crate) fn semantic_type_required(&self, id: TypeId) -> &SemanticType {
        self.semantic_type(id)
            .expect("type solution owns every internal type identity")
    }

    pub(crate) fn node_type_required(&self, node: NodeId) -> TypeId {
        self.node_type(node)
            .expect("type solution owns every internal node identity")
    }

    pub(crate) fn rule_type_required(&self, rule: RuleId) -> TypeId {
        self.rule_type(rule)
            .expect("type solution owns every internal rule identity")
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
        if self.nodes.len() != program.nodes().len()
            || self.rules.len() != program.rules().len()
            || self.arena.len() > program.nodes().len() + program.rules().len()
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

        for (index, semantic) in self.arena.iter().enumerate() {
            if let Err(message) = self.verify_semantic_type(index, semantic, program) {
                return StageResult::rejected(type_invariant(message));
            }
        }

        let mut used = BTreeSet::new();
        for (index, derivation) in self.nodes.iter().enumerate() {
            let Some(node) = program.nodes().get(index) else {
                return StageResult::rejected(type_invariant(
                    "type derivation references an absent node",
                ));
            };
            if derivation.revision != self.revision
                || derivation.node != node.id()
                || derivation.type_id.index() >= self.arena.len()
            {
                return StageResult::rejected(type_invariant(
                    "node type derivation identity is not canonical",
                ));
            }
            let expected = match self.expected_node_type(program, node, &derivation.inference) {
                Ok(expected) => expected,
                Err(message) => return StageResult::rejected(type_invariant(message)),
            };
            if self.semantic_type_required(derivation.type_id) != &expected {
                return StageResult::rejected(type_invariant(
                    "node inference conclusion does not follow from its certified premises",
                ));
            }
            used.insert(derivation.type_id);
        }

        for (index, derivation) in self.rules.iter().enumerate() {
            let Some(rule) = program.rules().get(index) else {
                return StageResult::rejected(type_invariant(
                    "rule type derivation references an absent rule",
                ));
            };
            if derivation.revision != self.revision
                || derivation.rule != rule.id()
                || derivation.named_type.index() >= self.arena.len()
                || derivation.root_type.index() >= self.arena.len()
                || self.semantic_type_required(derivation.named_type)
                    != &SemanticType::Named(rule.id())
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
            for child in semantic_children(self.semantic_type_required(type_id)) {
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
        _index: usize,
        semantic: &SemanticType,
        program: &Program,
    ) -> Result<(), &'static str> {
        match semantic {
            SemanticType::Unit | SemanticType::Span => {}
            SemanticType::Named(rule) => {
                if rule.index() >= program.rules().len() {
                    return Err("named semantic type references an absent rule");
                }
            }
            SemanticType::Product(children) | SemanticType::Sum(children) => {
                if children
                    .iter()
                    .any(|child| child.index() >= self.arena.len())
                {
                    return Err("aggregate semantic type references an absent type");
                }
            }
            SemanticType::Option(child) | SemanticType::List(child) => {
                if child.index() >= self.arena.len() {
                    return Err("unary semantic type references an absent type");
                }
            }
            SemanticType::Record { owner, fields } => {
                if owner.index() >= program.rules().len() {
                    return Err("record semantic type references an absent owner");
                }
                let mut previous = None;
                for field in fields {
                    if field.field.index() >= program.fields().len()
                        || field.value.index() >= self.arena.len()
                        || program.field_required(field.field).owner() != *owner
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
        node: &super::syntax::Node,
        inference: &InferenceRule,
    ) -> Result<SemanticType, &'static str> {
        let child_type = |child: NodeId| {
            self.nodes
                .get(child.index())
                .filter(|derivation| derivation.node == child)
                .map(|derivation| derivation.type_id)
                .ok_or("inference premise references an absent child")
        };
        match (node.kind(), inference) {
            (NodeKind::Epsilon, InferenceRule::EpsilonUnit) => Ok(SemanticType::Unit),
            (NodeKind::Literal(_) | NodeKind::Regex(_), InferenceRule::TerminalSpan) => {
                Ok(SemanticType::Span)
            }
            (NodeKind::RuleRef(actual), InferenceRule::ReferenceNamed { rule })
                if actual == rule =>
            {
                Ok(SemanticType::Named(*rule))
            }
            (NodeKind::Sequence(actual), InferenceRule::SequenceProduct { children })
                if actual == children =>
            {
                Ok(SemanticType::Product(
                    children
                        .iter()
                        .map(|child| child_type(*child))
                        .collect::<Result<_, _>>()?,
                ))
            }
            (NodeKind::Choice(actual), InferenceRule::ChoiceSum { children })
                if actual == children =>
            {
                Ok(SemanticType::Sum(
                    children
                        .iter()
                        .map(|child| child_type(*child))
                        .collect::<Result<_, _>>()?,
                ))
            }
            (
                NodeKind::Repeat {
                    child: actual,
                    minimum: 0,
                    maximum: Some(1),
                },
                InferenceRule::Optional { child },
            ) if actual == child => Ok(SemanticType::Option(child_type(*child)?)),
            (NodeKind::Repeat { child: actual, .. }, InferenceRule::RepetitionList { child })
                if actual == child =>
            {
                Ok(SemanticType::List(child_type(*child)?))
            }
            (
                NodeKind::Transform {
                    child: actual_child,
                    transform: actual_transform,
                    ..
                },
                InferenceRule::TransformPreserves { child, transform },
            ) if actual_child == child && actual_transform == transform => {
                Ok(self.semantic_type_required(child_type(*child)?).clone())
            }
            (
                NodeKind::Record {
                    owner: actual_owner,
                    slots: actual_slots,
                },
                InferenceRule::RecordProjection { owner, slots },
            ) if actual_owner == owner && record_witness_matches(actual_slots, slots) => {
                let fields = slots
                    .iter()
                    .filter_map(|slot| match slot {
                        RecordWitnessSlot::Field { field, child } => {
                            Some(child_type(*child).map(|value| RecordFieldType {
                                field: *field,
                                value,
                            }))
                        }
                        RecordWitnessSlot::Discard { .. } => None,
                    })
                    .collect::<Result<_, _>>()?;
                for slot in slots {
                    if let RecordWitnessSlot::Field { field, child } = slot {
                        let declaration = program.field_required(*field);
                        if declaration.owner() != *owner || declaration.child() != *child {
                            return Err("record inference field does not match its declaration");
                        }
                    }
                }
                Ok(SemanticType::Record {
                    owner: *owner,
                    fields,
                })
            }
            _ => Err("inference rule does not match the resolved node form"),
        }
    }
}

#[derive(Clone, Debug)]
enum TypeConstraint {
    Unit,
    Span,
    Named(RuleId),
    Product(Vec<NodeId>),
    Sum(Vec<NodeId>),
    Option(NodeId),
    List(NodeId),
    Record {
        owner: RuleId,
        slots: Vec<RecordSlot>,
    },
    TransformAlias {
        child: NodeId,
        transform: TransformId,
    },
}

impl TypeConstraint {
    fn inference(&self) -> InferenceRule {
        match self {
            Self::Unit => InferenceRule::EpsilonUnit,
            Self::Span => InferenceRule::TerminalSpan,
            Self::Named(rule) => InferenceRule::ReferenceNamed { rule: *rule },
            Self::Product(children) => InferenceRule::SequenceProduct {
                children: children.clone(),
            },
            Self::Sum(children) => InferenceRule::ChoiceSum {
                children: children.clone(),
            },
            Self::Option(child) => InferenceRule::Optional { child: *child },
            Self::List(child) => InferenceRule::RepetitionList { child: *child },
            Self::Record { owner, slots } => InferenceRule::RecordProjection {
                owner: *owner,
                slots: slots
                    .iter()
                    .map(|slot| match slot {
                        RecordSlot::Field { field, child } => RecordWitnessSlot::Field {
                            field: *field,
                            child: *child,
                        },
                        RecordSlot::Discard { child } => {
                            RecordWitnessSlot::Discard { child: *child }
                        }
                    })
                    .collect(),
            },
            Self::TransformAlias { child, transform } => InferenceRule::TransformPreserves {
                child: *child,
                transform: *transform,
            },
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
    if order.len() != program.nodes().len() {
        return Err(invalid_constraint_order(program));
    }

    let mut constraints = vec![None; program.nodes().len()];
    for &index in order {
        let Some(node) = program.nodes().get(index) else {
            return Err(invalid_constraint_order(program));
        };
        if constraints[index].is_some() {
            return Err(invalid_constraint_order(program));
        }
        let constraint = match node.kind() {
            NodeKind::Epsilon => TypeConstraint::Unit,
            NodeKind::Literal(_) | NodeKind::Regex(_) => TypeConstraint::Span,
            NodeKind::RuleRef(rule) => TypeConstraint::Named(*rule),
            NodeKind::Sequence(children) => TypeConstraint::Product(children.clone()),
            NodeKind::Choice(children) => TypeConstraint::Sum(children.clone()),
            NodeKind::Repeat {
                child,
                minimum,
                maximum,
            } if *minimum == 0 && *maximum == Some(1) => TypeConstraint::Option(*child),
            NodeKind::Repeat { child, .. } => TypeConstraint::List(*child),
            NodeKind::Record { owner, slots } => TypeConstraint::Record {
                owner: *owner,
                slots: slots.clone(),
            },
            NodeKind::Transform {
                child, transform, ..
            } => TypeConstraint::TransformAlias {
                child: *child,
                transform: *transform,
            },
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
    node_types: &mut [Option<TypeId>],
    arena: &mut Vec<SemanticType>,
    interned: &mut BTreeMap<SemanticType, TypeId>,
) -> Result<TypeId, Diagnostic> {
    match states.get(index).copied() {
        Some(VisitState::Sealed) => {
            return Ok(node_types[index].expect("sealed node carries a type"));
        }
        Some(VisitState::Visiting) => {
            let span = program.nodes()[index].span();
            return Err(Diagnostic::new(
                DiagnosticCode::InternalInvariant,
                span.start() as usize,
                span.end() as usize,
                "structural type occurs-check failed: node containment is cyclic; \
                 recursive grammar references must cross a Named(RuleId) binder",
            ));
        }
        Some(VisitState::Unseen) => {}
        None => {
            return Err(invalid_containment(program, index));
        }
    }

    states[index] = VisitState::Visiting;
    let witness = match &constraints[index] {
        TypeConstraint::Unit => SemanticType::Unit,
        TypeConstraint::Span => SemanticType::Span,
        TypeConstraint::Named(rule) => {
            if rule.index() >= program.rules().len() {
                return Err(invalid_containment(program, index));
            }
            // Deliberately do not visit the referenced rule root. This is the
            // explicit finite binder boundary that admits recursive grammars.
            SemanticType::Named(*rule)
        }
        TypeConstraint::Product(children) => SemanticType::Product(derive_children(
            children,
            program,
            constraints,
            states,
            node_types,
            arena,
            interned,
        )?),
        TypeConstraint::Sum(children) => SemanticType::Sum(derive_children(
            children,
            program,
            constraints,
            states,
            node_types,
            arena,
            interned,
        )?),
        TypeConstraint::Option(child) => SemanticType::Option(derive_node_type(
            child.index(),
            program,
            constraints,
            states,
            node_types,
            arena,
            interned,
        )?),
        TypeConstraint::List(child) => SemanticType::List(derive_node_type(
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
                    RecordSlot::Field { field, child } => {
                        fields.push(RecordFieldType {
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
                    RecordSlot::Discard { child } => {
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
            SemanticType::Record {
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
    };

    let type_id = intern_type(witness, arena, interned);
    node_types[index] = Some(type_id);
    states[index] = VisitState::Sealed;
    Ok(type_id)
}

fn record_witness_matches(actual: &[RecordSlot], witness: &[RecordWitnessSlot]) -> bool {
    actual.len() == witness.len()
        && actual
            .iter()
            .zip(witness)
            .all(|(actual, witness)| match (actual, witness) {
                (
                    RecordSlot::Field {
                        field: actual_field,
                        child: actual_child,
                    },
                    RecordWitnessSlot::Field {
                        field: witness_field,
                        child: witness_child,
                    },
                ) => actual_field == witness_field && actual_child == witness_child,
                (
                    RecordSlot::Discard {
                        child: actual_child,
                    },
                    RecordWitnessSlot::Discard {
                        child: witness_child,
                    },
                ) => actual_child == witness_child,
                _ => false,
            })
}

fn semantic_children(semantic: &SemanticType) -> Vec<TypeId> {
    match semantic {
        SemanticType::Unit | SemanticType::Span | SemanticType::Named(_) => Vec::new(),
        SemanticType::Product(children) | SemanticType::Sum(children) => children.clone(),
        SemanticType::Option(child) | SemanticType::List(child) => vec![*child],
        SemanticType::Record { fields, .. } => fields.iter().map(RecordFieldType::value).collect(),
    }
}

fn type_invariant(message: impl Into<String>) -> Diagnostic {
    Diagnostic::new(DiagnosticCode::InternalInvariant, 0, 0, message)
}

#[allow(clippy::too_many_arguments)]
fn derive_children(
    children: &[NodeId],
    program: &Program,
    constraints: &[TypeConstraint],
    states: &mut [VisitState],
    node_types: &mut [Option<TypeId>],
    arena: &mut Vec<SemanticType>,
    interned: &mut BTreeMap<SemanticType, TypeId>,
) -> Result<Vec<TypeId>, Diagnostic> {
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
    witness: SemanticType,
    arena: &mut Vec<SemanticType>,
    interned: &mut BTreeMap<SemanticType, TypeId>,
) -> TypeId {
    if let Some(&existing) = interned.get(&witness) {
        return existing;
    }
    let id = TypeId::checked(arena.len()).expect("node budget bounds type identities");
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
            program.nodes().len()
        ),
    )
}

fn invalid_containment(program: &Program, index: usize) -> Diagnostic {
    let (start, end) = program
        .nodes()
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
    use super::*;
    use crate::compiler::budget::CompileOptions;
    use crate::compiler::frontend::parse_source;
    use crate::compiler::source::ModuleInput;
    use crate::compiler::syntax::Program;

    fn program(source: &str) -> (Program, DependencySolution) {
        let module = match parse_source(
            ModuleInput::new("type-order-law.bbnf", source.as_bytes()),
            CompileOptions::default(),
        ) {
            StageResult::Accepted { value, .. } => value,
            StageResult::Rejected { .. } => panic!("source fixture must parse"),
        };
        let program = match Program::resolve(&module) {
            StageResult::Accepted { value, .. } => value,
            StageResult::Rejected { .. } => panic!("source fixture must resolve"),
        };
        let dependency = match DependencySolution::build(&program) {
            StageResult::Accepted { value, .. } => value,
            StageResult::Rejected { .. } => panic!("dependency fixture must resolve"),
        };
        (program, dependency)
    }

    #[test]
    fn constraint_discovery_permutations_seal_to_identical_principal_solution() {
        let (program, dependency) = program(
            r#"
root = value;
value = object | array | "null";
object = "{", (value, ":", value)?, "}";
array = "[", (value, (",", value)*)?, "]";
"#,
        );
        let canonical = match TypeSolution::solve(&program, &dependency) {
            StageResult::Accepted { value, .. } => value,
            StageResult::Rejected { .. } => panic!("recursive binders must solve"),
        };

        for seed in 1..=16 {
            let schedule = deterministic_permutation(program.nodes().len(), seed);
            let candidate = match TypeSolution::solve_with_order(&program, &dependency, &schedule) {
                StageResult::Accepted { value, .. } => value,
                StageResult::Rejected { .. } => panic!("permuted constraints must solve"),
            };
            assert_eq!(candidate, canonical);
        }
    }

    #[test]
    fn structural_occurs_check_rejects_a_cycle_that_does_not_cross_a_named_binder() {
        let (program, _) = program(r#"root = "x";"#);
        let root = program.nodes()[0].id();
        let constraints = vec![TypeConstraint::TransformAlias {
            child: root,
            transform: TransformId::checked(0).expect("zero identity"),
        }];
        let mut states = vec![VisitState::Unseen];
        let mut node_types = vec![None];
        let mut arena = Vec::new();
        let mut interned = BTreeMap::new();

        let error = derive_node_type(
            0,
            &program,
            &constraints,
            &mut states,
            &mut node_types,
            &mut arena,
            &mut interned,
        )
        .expect_err("a structural self occurrence is not a named recursive binder");
        assert_eq!(error.code(), DiagnosticCode::InternalInvariant);
        assert!(error.message().contains("occurs-check"));
    }

    #[test]
    fn recursive_nominal_records_are_finite_and_certificates_execute() {
        let (program, dependency) = program(
            r#"
root = node;
node = list | atom;
list = %{ ~"(", children: node*, ~")" };
atom = %{ text: @raw(/[A-Za-z]+/) };
"#,
        );
        let solution = match TypeSolution::solve(&program, &dependency) {
            StageResult::Accepted { value, .. } => value,
            StageResult::Rejected { .. } => panic!("recursive record fixture must solve"),
        };
        assert!(matches!(
            solution.verify(&program),
            StageResult::Accepted { .. }
        ));

        let list = program
            .rules()
            .iter()
            .find(|rule| rule.name() == "list")
            .expect("list rule");
        let record_type = solution
            .rule_root_type(list.id())
            .and_then(|root| solution.semantic_type(root));
        let Some(SemanticType::Record { owner, fields }) = record_type else {
            panic!("list root must retain its nominal record type");
        };
        assert_eq!(*owner, list.id());
        assert_eq!(fields.len(), 1);
        let Some(SemanticType::List(element)) = solution.semantic_type(fields[0].value()) else {
            panic!("recursive children field must be a list");
        };
        let node = program
            .rules()
            .iter()
            .find(|rule| rule.name() == "node")
            .expect("node rule");
        assert_eq!(
            solution.semantic_type(*element),
            Some(&SemanticType::Named(node.id()))
        );
        assert!(solution.arena().len() <= program.nodes().len() + program.rules().len());
    }

    #[test]
    fn certificate_tampering_is_rejected_without_rerunning_the_solver() {
        let (program, dependency) = program(
            r#"
root = %{ left: "a", right: @raw("b") };
other = %{ value: "c" };
"#,
        );
        let canonical = match TypeSolution::solve(&program, &dependency) {
            StageResult::Accepted { value, .. } => value,
            StageResult::Rejected { .. } => panic!("record fixture must solve"),
        };
        assert!(matches!(
            canonical.verify(&program),
            StageResult::Accepted { .. }
        ));

        let record_index = program
            .nodes()
            .iter()
            .position(
                |node| matches!(node.kind(), NodeKind::Record { owner, .. } if owner.index() == 0),
            )
            .expect("root record");
        let transform_index = program
            .nodes()
            .iter()
            .position(|node| matches!(node.kind(), NodeKind::Transform { .. }))
            .expect("transform node");

        let mut wrong_rule = canonical.clone();
        wrong_rule.nodes[record_index].inference = InferenceRule::EpsilonUnit;
        assert!(matches!(
            wrong_rule.verify(&program),
            StageResult::Rejected { .. }
        ));

        let mut dangling_type = canonical.clone();
        dangling_type.nodes[record_index].type_id =
            TypeId::checked(canonical.arena.len()).expect("adjacent type identity");
        assert!(matches!(
            dangling_type.verify(&program),
            StageResult::Rejected { .. }
        ));

        let mut wrong_owner = canonical.clone();
        if let InferenceRule::RecordProjection { owner, .. } =
            &mut wrong_owner.nodes[record_index].inference
        {
            *owner = program.rules()[1].id();
        }
        assert!(matches!(
            wrong_owner.verify(&program),
            StageResult::Rejected { .. }
        ));

        let mut wrong_field_order = canonical.clone();
        if let InferenceRule::RecordProjection { slots, .. } =
            &mut wrong_field_order.nodes[record_index].inference
        {
            slots.swap(0, 1);
        }
        assert!(matches!(
            wrong_field_order.verify(&program),
            StageResult::Rejected { .. }
        ));

        let mut wrong_transform = canonical.clone();
        if let InferenceRule::TransformPreserves { child, .. } =
            &mut wrong_transform.nodes[transform_index].inference
        {
            *child = program.nodes()[0].id();
        }
        assert!(matches!(
            wrong_transform.verify(&program),
            StageResult::Rejected { .. }
        ));
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
