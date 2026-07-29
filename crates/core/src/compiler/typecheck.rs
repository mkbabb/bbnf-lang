use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use super::dependency::DependencySolution;
use super::diagnostic::{Diagnostic, DiagnosticCode, StageResult};
use super::source::Revision;
use super::syntax::{NodeId, NodeKind, Program, RuleId, TypeId};

/// First-order semantic witnesses admitted by the W2 scalar language.
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

/// Deterministic interned type arena plus a derivation record for every node
/// and rule in one source revision.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct TypeSolution {
    revision: Revision,
    arena: Vec<SemanticType>,
    nodes: Vec<NodeTypeDerivation>,
    rules: Vec<RuleTypeDerivation>,
}

impl TypeSolution {
    pub(crate) fn solve(program: &Program, dependency: &DependencySolution) -> StageResult<Self> {
        if program.revision() != dependency.revision() {
            return StageResult::rejected(revision_mismatch(
                "type solution",
                program.revision(),
                dependency.revision(),
            ));
        }

        let mut arena = Vec::new();
        let mut interned = BTreeMap::new();
        let mut named_by_rule = Vec::with_capacity(program.rules().len());
        for rule in program.rules() {
            let type_id = intern_type(SemanticType::Named(rule.id()), &mut arena, &mut interned);
            named_by_rule.push(type_id);
        }

        let mut node_types = vec![None; program.nodes().len()];
        let mut remaining = program.nodes().len();
        while remaining != 0 {
            let mut progressed = false;
            for node in program.nodes() {
                if node_types[node.id().index()].is_some() {
                    continue;
                }
                let witness = match node.kind() {
                    NodeKind::Epsilon => Some(SemanticType::Unit),
                    NodeKind::Literal(_) | NodeKind::Regex(_) => Some(SemanticType::Span),
                    NodeKind::RuleRef(rule) => Some(SemanticType::Named(*rule)),
                    NodeKind::Sequence(children) => {
                        collect_children(children, &node_types, SemanticType::Product)
                    }
                    NodeKind::Choice(children) => {
                        collect_children(children, &node_types, SemanticType::Sum)
                    }
                    NodeKind::Repeat {
                        child,
                        minimum,
                        maximum,
                    } => node_types[child.index()].map(|child_type| {
                        if *minimum == 0 && *maximum == Some(1) {
                            SemanticType::Option(child_type)
                        } else {
                            SemanticType::List(child_type)
                        }
                    }),
                };
                let Some(witness) = witness else {
                    continue;
                };
                let type_id = intern_type(witness, &mut arena, &mut interned);
                node_types[node.id().index()] = Some(type_id);
                remaining -= 1;
                progressed = true;
            }

            if !progressed {
                let node = program
                    .nodes()
                    .iter()
                    .find(|node| node_types[node.id().index()].is_none())
                    .expect("remaining node count is nonzero");
                let span = node.span();
                return StageResult::rejected(Diagnostic::new(
                    DiagnosticCode::InternalInvariant,
                    span.start() as usize,
                    span.end() as usize,
                    "node containment is cyclic or references an invalid node identity",
                ));
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

fn collect_children(
    children: &[NodeId],
    node_types: &[Option<TypeId>],
    constructor: impl FnOnce(Vec<TypeId>) -> SemanticType,
) -> Option<SemanticType> {
    let children = children
        .iter()
        .map(|child| node_types[child.index()])
        .collect::<Option<Vec<_>>>()?;
    Some(constructor(children))
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
