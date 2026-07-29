use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use super::dependency::DependencySolution;
use super::diagnostic::{Diagnostic, DiagnosticCode, StageResult};
use super::source::Revision;
use super::syntax::{NodeId, NodeKind, Program, RuleId, TypeId};

/// First-order semantic witnesses admitted by the W2 scalar language.
///
/// `Named` is the only recursive constructor. A rule reference therefore
/// closes over a finite `RuleId` binder instead of unfolding the referenced
/// rule's root type into an infinite arena.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum SemanticType {
    Unit,
    Span,
    Named(RuleId),
    Product(Vec<TypeId>),
    Sum(Vec<TypeId>),
    Option(TypeId),
    List(TypeId),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct NodeTypeDerivation {
    revision: Revision,
    node: NodeId,
    type_id: TypeId,
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
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
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
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
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

    pub fn semantic_type(&self, id: TypeId) -> &SemanticType {
        &self.arena[id.index()]
    }

    pub fn node_derivations(&self) -> &[NodeTypeDerivation] {
        &self.nodes
    }

    pub fn rule_derivations(&self) -> &[RuleTypeDerivation] {
        &self.rules
    }

    pub fn node_type(&self, node: NodeId) -> TypeId {
        self.nodes[node.index()].type_id
    }

    pub fn rule_type(&self, rule: RuleId) -> TypeId {
        self.rules[rule.index()].named_type
    }

    pub fn rule_root_type(&self, rule: RuleId) -> TypeId {
        self.rules[rule.index()].root_type
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
    Alias(NodeId),
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
            NodeKind::Transform { child, .. } => TypeConstraint::Alias(*child),
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
        TypeConstraint::Alias(child) => {
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
        let constraints = vec![TypeConstraint::Alias(root)];
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
