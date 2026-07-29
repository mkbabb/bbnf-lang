use std::collections::BTreeMap;

use serde::Serialize;

use super::diagnostic::{Diagnostic, DiagnosticCode, DiagnosticSet, StageResult};
use super::source::{Revision, SourceModule, SourceNodeKind, SourceRecordSlot, SourceSpan};

macro_rules! id_type {
    ($name:ident) => {
        #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
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
id_type!(TransformId);
id_type!(FieldId);

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[non_exhaustive]
pub enum RecordSlot {
    Field { field: FieldId, child: NodeId },
    Discard { child: NodeId },
}

impl RecordSlot {
    pub const fn child(&self) -> NodeId {
        match self {
            Self::Field { child, .. } | Self::Discard { child } => *child,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct FieldDecl {
    id: FieldId,
    owner: RuleId,
    slot: u32,
    name: String,
    child: NodeId,
    span: SourceSpan,
}

impl FieldDecl {
    pub const fn id(&self) -> FieldId {
        self.id
    }

    pub const fn owner(&self) -> RuleId {
        self.owner
    }

    pub const fn slot(&self) -> u32 {
        self.slot
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub const fn child(&self) -> NodeId {
        self.child
    }

    pub const fn span(&self) -> SourceSpan {
        self.span
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[non_exhaustive]
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
    Transform {
        child: NodeId,
        transform: TransformId,
        name: String,
    },
    Record {
        owner: RuleId,
        slots: Vec<RecordSlot>,
    },
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
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

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
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

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct Program {
    revision: Revision,
    nodes: Vec<Node>,
    rules: Vec<Rule>,
    fields: Vec<FieldDecl>,
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

        let mut record_owners = BTreeMap::new();
        for rule in &module.rules {
            if !matches!(
                module.nodes.get(rule.root.index()).map(|node| &node.kind),
                Some(SourceNodeKind::Record(_))
            ) {
                continue;
            }
            if let Some(previous) = record_owners.insert(rule.root, rule.id) {
                diagnostics.push(Diagnostic::new(
                    DiagnosticCode::RecordMustBeRuleRoot,
                    rule.span.start() as usize,
                    rule.span.end() as usize,
                    format!(
                        "record root is shared by rules {} and {}; each record must have one nominal owner",
                        previous.index(),
                        rule.id.index()
                    ),
                ));
            }
        }

        let mut nodes = Vec::with_capacity(module.nodes.len());
        let mut fields = Vec::new();
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
                SourceNodeKind::Transform {
                    child,
                    transform,
                    name,
                } => NodeKind::Transform {
                    child: *child,
                    transform: *transform,
                    name: name.clone(),
                },
                SourceNodeKind::Record(source_slots) => {
                    let Some(&owner) = record_owners.get(&id) else {
                        diagnostics.push(Diagnostic::new(
                            DiagnosticCode::RecordMustBeRuleRoot,
                            source.span.start() as usize,
                            source.span.end() as usize,
                            "a `%{ ... }` record must be the entire root expression of exactly one rule",
                        ));
                        continue;
                    };

                    let mut names_in_record = BTreeMap::new();
                    let mut slots = Vec::with_capacity(source_slots.len());
                    for (slot_index, source_slot) in source_slots.iter().enumerate() {
                        match source_slot {
                            SourceRecordSlot::Field {
                                name,
                                child,
                                name_span,
                                span,
                            } => {
                                if names_in_record.insert(name.as_str(), *name_span).is_some() {
                                    diagnostics.push(Diagnostic::new(
                                        DiagnosticCode::DuplicateField,
                                        name_span.start() as usize,
                                        name_span.end() as usize,
                                        format!(
                                            "duplicate field `{name}` in record owned by rule `{}`",
                                            module.rules[owner.index()].name
                                        ),
                                    ));
                                }
                                let field = FieldId::checked(fields.len())
                                    .expect("node budget bounds retained field identities");
                                fields.push(FieldDecl {
                                    id: field,
                                    owner,
                                    slot: u32::try_from(slot_index)
                                        .expect("node budget bounds record slot ordinals"),
                                    name: name.clone(),
                                    child: *child,
                                    span: *span,
                                });
                                slots.push(RecordSlot::Field {
                                    field,
                                    child: *child,
                                });
                            }
                            SourceRecordSlot::Discard { child, .. } => {
                                slots.push(RecordSlot::Discard { child: *child });
                            }
                        }
                    }
                    NodeKind::Record { owner, slots }
                }
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
            fields,
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

    pub fn fields(&self) -> &[FieldDecl] {
        &self.fields
    }

    pub fn field(&self, id: FieldId) -> Option<&FieldDecl> {
        self.fields.get(id.index())
    }

    pub const fn entry(&self) -> RuleId {
        self.entry
    }

    pub fn node(&self, id: NodeId) -> Option<&Node> {
        self.nodes.get(id.index())
    }

    pub fn rule(&self, id: RuleId) -> Option<&Rule> {
        self.rules.get(id.index())
    }

    pub(crate) fn field_required(&self, id: FieldId) -> &FieldDecl {
        self.field(id)
            .expect("resolved program owns every internal field identity")
    }

    pub(crate) fn node_required(&self, id: NodeId) -> &Node {
        self.node(id)
            .expect("resolved program owns every internal node identity")
    }

    pub(crate) fn rule_required(&self, id: RuleId) -> &Rule {
        self.rule(id)
            .expect("resolved program owns every internal rule identity")
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
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

    pub const fn from_revision(&self) -> Revision {
        self.from
    }

    pub const fn to_revision(&self) -> Revision {
        self.to
    }
}
