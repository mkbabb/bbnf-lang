use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use super::diagnostic::{Diagnostic, DiagnosticCode, DiagnosticSet, StageResult};
use super::source::{Revision, SourceModule, SourceNodeKind, SourceSpan};

macro_rules! id_type {
    ($name:ident) => {
        #[derive(
            Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize, Deserialize,
        )]
        pub struct $name(u32);

        impl $name {
            pub(crate) fn checked(index: usize) -> Option<Self> {
                Some(Self(u32::try_from(index).ok()?))
            }

            pub const fn index(self) -> usize {
                self.0 as usize
            }
        }
    };
}

id_type!(NodeId);
id_type!(RuleId);
id_type!(TypeId);
id_type!(SccId);
id_type!(PlanNodeId);

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum NodeKind {
    Epsilon,
    Literal(Vec<u8>),
    Regex(String),
    RuleRef(RuleId),
    Sequence(Vec<NodeId>),
    Choice(Vec<NodeId>),
    Repeat {
        child: NodeId,
        minimum: u32,
        maximum: Option<u32>,
    },
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Node {
    id: NodeId,
    kind: NodeKind,
    span: SourceSpan,
}

impl Node {
    pub const fn id(&self) -> NodeId {
        self.id
    }

    pub const fn kind(&self) -> &NodeKind {
        &self.kind
    }

    pub const fn span(&self) -> SourceSpan {
        self.span
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Rule {
    id: RuleId,
    name: String,
    root: NodeId,
    span: SourceSpan,
}

impl Rule {
    pub const fn id(&self) -> RuleId {
        self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub const fn root(&self) -> NodeId {
        self.root
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Program {
    revision: Revision,
    nodes: Vec<Node>,
    rules: Vec<Rule>,
    entry: RuleId,
}

impl Program {
    pub(crate) fn resolve(module: &SourceModule) -> StageResult<Self> {
        let mut names = BTreeMap::new();
        let mut diagnostics = Vec::new();
        for rule in &module.rules {
            if names.insert(rule.name.clone(), rule.id).is_some() {
                diagnostics.push(Diagnostic::new(
                    DiagnosticCode::DuplicateRule,
                    rule.span.start() as usize,
                    rule.span.end() as usize,
                    format!("duplicate rule `{}`", rule.name),
                ));
            }
        }
        if !diagnostics.is_empty() {
            return StageResult::rejected_set(DiagnosticSet::from_vec(diagnostics));
        }

        let mut nodes = Vec::with_capacity(module.nodes.len());
        for (index, source) in module.nodes.iter().enumerate() {
            let id = NodeId::checked(index).expect("source budget checked node identity");
            let kind = match &source.kind {
                SourceNodeKind::Epsilon => NodeKind::Epsilon,
                SourceNodeKind::Literal(bytes) => NodeKind::Literal(bytes.clone()),
                SourceNodeKind::Regex(pattern) => NodeKind::Regex(pattern.clone()),
                SourceNodeKind::Reference(name) => {
                    let Some(&rule) = names.get(name) else {
                        diagnostics.push(Diagnostic::new(
                            DiagnosticCode::UndefinedRule,
                            source.span.start() as usize,
                            source.span.end() as usize,
                            format!("undefined rule `{name}`"),
                        ));
                        continue;
                    };
                    NodeKind::RuleRef(rule)
                }
                SourceNodeKind::Sequence(children) => NodeKind::Sequence(children.clone()),
                SourceNodeKind::Choice(children) => NodeKind::Choice(children.clone()),
                SourceNodeKind::Repeat {
                    child,
                    minimum,
                    maximum,
                } => NodeKind::Repeat {
                    child: *child,
                    minimum: *minimum,
                    maximum: *maximum,
                },
            };
            nodes.push(Node {
                id,
                kind,
                span: source.span,
            });
        }
        if !diagnostics.is_empty() {
            return StageResult::rejected_set(DiagnosticSet::from_vec(diagnostics));
        }

        let rules = module
            .rules
            .iter()
            .map(|rule| Rule {
                id: rule.id,
                name: rule.name.clone(),
                root: rule.root,
                span: rule.span,
            })
            .collect::<Vec<_>>();
        let entry = rules
            .iter()
            .find(|rule| rule.name == "root")
            .or_else(|| rules.first())
            .map(|rule| rule.id)
            .expect("frontend requires at least one rule");

        StageResult::accepted(Self {
            revision: module.revision(),
            nodes,
            rules,
            entry,
        })
    }

    pub const fn revision(&self) -> Revision {
        self.revision
    }

    pub fn nodes(&self) -> &[Node] {
        &self.nodes
    }

    pub fn rules(&self) -> &[Rule] {
        &self.rules
    }

    pub const fn entry(&self) -> RuleId {
        self.entry
    }

    pub fn node(&self, id: NodeId) -> &Node {
        &self.nodes[id.index()]
    }

    pub fn rule(&self, id: RuleId) -> &Rule {
        &self.rules[id.index()]
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct NodeRemap {
    from: Revision,
    to: Revision,
    nodes: Vec<Option<NodeId>>,
}

impl NodeRemap {
    pub fn identity(program: &Program) -> Self {
        Self {
            from: program.revision,
            to: program.revision,
            nodes: program.nodes.iter().map(|node| Some(node.id)).collect(),
        }
    }

    pub fn get(&self, old: NodeId) -> Option<NodeId> {
        self.nodes.get(old.index()).copied().flatten()
    }
}
