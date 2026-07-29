use std::collections::BTreeMap;
use std::ops::Range;

use serde::Serialize;

use super::diagnostic::{Diagnostic, DiagnosticCode, DiagnosticSet, StageResult};
use super::source::{Revision, SourceModule, SourceNodeKindData, SourceRecordSlotData, SourceSpan};

// Arena ordinals are compiler-private and always remain four bytes. They are
// meaningful only inside the carrier that owns the corresponding arena.
macro_rules! local_index_type {
    ($name:ident) => {
        #[repr(transparent)]
        #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
        pub(crate) struct $name(u32);

        impl $name {
            pub(crate) fn checked(index: usize) -> Option<Self> {
                Some(Self(u32::try_from(index).ok()?))
            }

            pub(crate) const fn index(self) -> usize {
                self.0 as usize
            }
        }
    };
}

local_index_type!(NodeIx);
local_index_type!(RuleIx);
local_index_type!(TypeIx);
local_index_type!(SccIx);
local_index_type!(PlanNodeIx);
local_index_type!(TransformIx);
local_index_type!(FieldIx);

// Public identities are unforgeable full-revision capabilities. The local
// ordinal is deliberately private; serialization emits that compact ordinal
// because every replay packet already supplies the authoritative revision
// envelope.
macro_rules! capability_type {
    ($name:ident, $local:ident) => {
        #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub struct $name {
            local: $local,
            revision: Revision,
        }

        impl $name {
            pub(crate) const fn bind(revision: Revision, local: $local) -> Self {
                Self { local, revision }
            }

            pub(crate) const fn local(self) -> $local {
                self.local
            }

            pub const fn revision(self) -> Revision {
                self.revision
            }

            pub const fn index(self) -> usize {
                self.local.index()
            }
        }
    };
}

capability_type!(NodeId, NodeIx);
capability_type!(RuleId, RuleIx);
capability_type!(TypeId, TypeIx);
capability_type!(SccId, SccIx);
capability_type!(PlanNodeId, PlanNodeIx);
capability_type!(TransformId, TransformIx);
capability_type!(FieldId, FieldIx);

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) enum RecordSlotData {
    Field { field: FieldIx, child: NodeIx },
    Discard { child: NodeIx },
}

impl RecordSlotData {
    pub(crate) const fn child(&self) -> NodeIx {
        match self {
            Self::Field { child, .. } | Self::Discard { child } => *child,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum RecordSlot {
    Field { field: FieldId, child: NodeId },
    Discard { child: NodeId },
}

impl RecordSlot {
    pub const fn child(self) -> NodeId {
        match self {
            Self::Field { child, .. } | Self::Discard { child } => child,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct FieldData {
    id: FieldIx,
    owner: RuleIx,
    slot: u32,
    name: String,
    child: NodeIx,
    span: SourceSpan,
}

#[derive(Clone, Copy, Debug)]
pub struct FieldDecl<'a> {
    revision: &'a Revision,
    data: &'a FieldData,
}

impl<'a> FieldDecl<'a> {
    pub const fn id(self) -> FieldId {
        FieldId::bind(*self.revision, self.data.id)
    }

    pub const fn owner(self) -> RuleId {
        RuleId::bind(*self.revision, self.data.owner)
    }

    pub const fn slot(self) -> u32 {
        self.data.slot
    }

    pub fn name(self) -> &'a str {
        &self.data.name
    }

    pub const fn child(self) -> NodeId {
        NodeId::bind(*self.revision, self.data.child)
    }

    pub const fn span(self) -> SourceSpan {
        self.data.span
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) enum NodeKindData {
    Epsilon,
    Literal(Vec<u8>),
    Regex(String),
    RuleRef(RuleIx),
    Sequence(Vec<NodeIx>),
    Choice(Vec<NodeIx>),
    Repeat {
        child: NodeIx,
        minimum: u32,
        maximum: Option<u32>,
    },
    Transform {
        child: NodeIx,
        transform: TransformIx,
        name: String,
    },
    Record {
        owner: RuleIx,
        slots: Vec<RecordSlotData>,
    },
}

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum NodeKind<'a> {
    Epsilon,
    Literal(&'a [u8]),
    Regex(&'a str),
    RuleRef(RuleId),
    Sequence(NodeIds<'a>),
    Choice(NodeIds<'a>),
    Repeat {
        child: NodeId,
        minimum: u32,
        maximum: Option<u32>,
    },
    Transform {
        child: NodeId,
        transform: TransformId,
        name: &'a str,
    },
    Record {
        owner: RuleId,
        slots: RecordSlots<'a>,
    },
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct NodeData {
    id: NodeIx,
    kind: NodeKindData,
    span: SourceSpan,
}

#[derive(Clone, Copy, Debug)]
pub struct Node<'a> {
    revision: &'a Revision,
    data: &'a NodeData,
}

impl<'a> Node<'a> {
    pub const fn id(self) -> NodeId {
        NodeId::bind(*self.revision, self.data.id)
    }

    pub fn kind(self) -> NodeKind<'a> {
        match &self.data.kind {
            NodeKindData::Epsilon => NodeKind::Epsilon,
            NodeKindData::Literal(bytes) => NodeKind::Literal(bytes),
            NodeKindData::Regex(pattern) => NodeKind::Regex(pattern),
            NodeKindData::RuleRef(rule) => NodeKind::RuleRef(RuleId::bind(*self.revision, *rule)),
            NodeKindData::Sequence(children) => {
                NodeKind::Sequence(NodeIds::new(self.revision, children))
            }
            NodeKindData::Choice(children) => {
                NodeKind::Choice(NodeIds::new(self.revision, children))
            }
            NodeKindData::Repeat {
                child,
                minimum,
                maximum,
            } => NodeKind::Repeat {
                child: NodeId::bind(*self.revision, *child),
                minimum: *minimum,
                maximum: *maximum,
            },
            NodeKindData::Transform {
                child,
                transform,
                name,
            } => NodeKind::Transform {
                child: NodeId::bind(*self.revision, *child),
                transform: TransformId::bind(*self.revision, *transform),
                name,
            },
            NodeKindData::Record { owner, slots } => NodeKind::Record {
                owner: RuleId::bind(*self.revision, *owner),
                slots: RecordSlots::new(self.revision, slots),
            },
        }
    }

    pub const fn span(self) -> SourceSpan {
        self.data.span
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct RuleData {
    id: RuleIx,
    name: String,
    root: NodeIx,
    span: SourceSpan,
}

#[derive(Clone, Copy, Debug)]
pub struct Rule<'a> {
    revision: &'a Revision,
    data: &'a RuleData,
}

impl<'a> Rule<'a> {
    pub const fn id(self) -> RuleId {
        RuleId::bind(*self.revision, self.data.id)
    }

    pub fn name(self) -> &'a str {
        &self.data.name
    }

    pub const fn root(self) -> NodeId {
        NodeId::bind(*self.revision, self.data.root)
    }

    pub const fn span(self) -> SourceSpan {
        self.data.span
    }
}

macro_rules! branded_rows {
    ($name:ident, $view:ident, $data:ident) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
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

branded_rows!(Nodes, Node, NodeData);
branded_rows!(Rules, Rule, RuleData);
branded_rows!(Fields, FieldDecl, FieldData);

macro_rules! branded_ids {
    ($name:ident, $capability:ident, $local:ident) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $name<'a> {
            revision: &'a Revision,
            locals: &'a [$local],
            remaining: Range<usize>,
        }

        impl<'a> $name<'a> {
            pub(crate) fn new(revision: &'a Revision, locals: &'a [$local]) -> Self {
                Self {
                    revision,
                    locals,
                    remaining: 0..locals.len(),
                }
            }

            pub fn get(&self, index: usize) -> Option<$capability> {
                self.locals
                    .get(index)
                    .copied()
                    .map(|local| $capability::bind(*self.revision, local))
            }

            pub fn iter(&self) -> Self {
                Self::new(self.revision, self.locals)
            }

            pub fn is_empty(&self) -> bool {
                self.remaining.start >= self.remaining.end
            }
        }

        impl Iterator for $name<'_> {
            type Item = $capability;

            fn next(&mut self) -> Option<Self::Item> {
                self.remaining.next().and_then(|index| self.get(index))
            }

            fn size_hint(&self) -> (usize, Option<usize>) {
                self.remaining.size_hint()
            }
        }

        impl DoubleEndedIterator for $name<'_> {
            fn next_back(&mut self) -> Option<Self::Item> {
                self.remaining.next_back().and_then(|index| self.get(index))
            }
        }

        impl ExactSizeIterator for $name<'_> {}
    };
}

branded_ids!(NodeIds, NodeId, NodeIx);
branded_ids!(RuleIds, RuleId, RuleIx);
branded_ids!(TypeIds, TypeId, TypeIx);
branded_ids!(SccIds, SccId, SccIx);
branded_ids!(PlanNodeIds, PlanNodeId, PlanNodeIx);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RecordSlots<'a> {
    revision: &'a Revision,
    slots: &'a [RecordSlotData],
    remaining: Range<usize>,
}

impl<'a> RecordSlots<'a> {
    fn new(revision: &'a Revision, slots: &'a [RecordSlotData]) -> Self {
        Self {
            revision,
            slots,
            remaining: 0..slots.len(),
        }
    }

    pub fn get(&self, index: usize) -> Option<RecordSlot> {
        match self.slots.get(index)? {
            RecordSlotData::Field { field, child } => Some(RecordSlot::Field {
                field: FieldId::bind(*self.revision, *field),
                child: NodeId::bind(*self.revision, *child),
            }),
            RecordSlotData::Discard { child } => Some(RecordSlot::Discard {
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

impl Iterator for RecordSlots<'_> {
    type Item = RecordSlot;

    fn next(&mut self) -> Option<Self::Item> {
        self.remaining.next().and_then(|index| self.get(index))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.remaining.size_hint()
    }
}

impl DoubleEndedIterator for RecordSlots<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.remaining.next_back().and_then(|index| self.get(index))
    }
}

impl ExactSizeIterator for RecordSlots<'_> {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Program {
    revision: Revision,
    nodes: Vec<NodeData>,
    rules: Vec<RuleData>,
    fields: Vec<FieldData>,
    entry: RuleIx,
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
                Some(SourceNodeKindData::Record(_))
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
            let id = NodeIx::checked(index).expect("source budget checked node identity");
            let kind = match &source.kind {
                SourceNodeKindData::Epsilon => NodeKindData::Epsilon,
                SourceNodeKindData::Literal(bytes) => NodeKindData::Literal(bytes.clone()),
                SourceNodeKindData::Regex(pattern) => NodeKindData::Regex(pattern.clone()),
                SourceNodeKindData::Reference(name) => {
                    let Some(&rule) = names.get(name) else {
                        diagnostics.push(Diagnostic::new(
                            DiagnosticCode::UndefinedRule,
                            source.span.start() as usize,
                            source.span.end() as usize,
                            format!("undefined rule `{name}`"),
                        ));
                        continue;
                    };
                    NodeKindData::RuleRef(rule)
                }
                SourceNodeKindData::Sequence(children) => NodeKindData::Sequence(children.clone()),
                SourceNodeKindData::Choice(children) => NodeKindData::Choice(children.clone()),
                SourceNodeKindData::Repeat {
                    child,
                    minimum,
                    maximum,
                } => NodeKindData::Repeat {
                    child: *child,
                    minimum: *minimum,
                    maximum: *maximum,
                },
                SourceNodeKindData::Transform {
                    child,
                    transform,
                    name,
                } => NodeKindData::Transform {
                    child: *child,
                    transform: *transform,
                    name: name.clone(),
                },
                SourceNodeKindData::Record(source_slots) => {
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
                            SourceRecordSlotData::Field {
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
                                let field = FieldIx::checked(fields.len())
                                    .expect("node budget bounds retained field identities");
                                fields.push(FieldData {
                                    id: field,
                                    owner,
                                    slot: u32::try_from(slot_index)
                                        .expect("node budget bounds record slot ordinals"),
                                    name: name.clone(),
                                    child: *child,
                                    span: *span,
                                });
                                slots.push(RecordSlotData::Field {
                                    field,
                                    child: *child,
                                });
                            }
                            SourceRecordSlotData::Discard { child, .. } => {
                                slots.push(RecordSlotData::Discard { child: *child });
                            }
                        }
                    }
                    NodeKindData::Record { owner, slots }
                }
            };
            nodes.push(NodeData {
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
            .map(|rule| RuleData {
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

    pub fn nodes(&self) -> Nodes<'_> {
        Nodes::new(&self.revision, &self.nodes)
    }

    pub fn rules(&self) -> Rules<'_> {
        Rules::new(&self.revision, &self.rules)
    }

    pub fn fields(&self) -> Fields<'_> {
        Fields::new(&self.revision, &self.fields)
    }

    pub fn field(&self, id: FieldId) -> Option<FieldDecl<'_>> {
        (id.revision() == self.revision)
            .then(|| {
                self.fields.get(id.local().index()).map(|data| FieldDecl {
                    revision: &self.revision,
                    data,
                })
            })
            .flatten()
    }

    pub const fn entry(&self) -> RuleId {
        RuleId::bind(self.revision, self.entry)
    }

    pub fn node(&self, id: NodeId) -> Option<Node<'_>> {
        (id.revision() == self.revision)
            .then(|| {
                self.nodes.get(id.local().index()).map(|data| Node {
                    revision: &self.revision,
                    data,
                })
            })
            .flatten()
    }

    pub fn rule(&self, id: RuleId) -> Option<Rule<'_>> {
        (id.revision() == self.revision)
            .then(|| {
                self.rules.get(id.local().index()).map(|data| Rule {
                    revision: &self.revision,
                    data,
                })
            })
            .flatten()
    }

    pub(crate) fn node_data(&self, id: NodeIx) -> &NodeData {
        &self.nodes[id.index()]
    }

    pub(crate) fn node_data_rows(&self) -> &[NodeData] {
        &self.nodes
    }

    pub(crate) fn rule_data_rows(&self) -> &[RuleData] {
        &self.rules
    }

    pub(crate) fn field_data_rows(&self) -> &[FieldData] {
        &self.fields
    }

    pub(crate) const fn entry_local(&self) -> RuleIx {
        self.entry
    }

    pub(super) fn replay_value(&self) -> ProgramReplay<'_> {
        ProgramReplay {
            revision: self.revision,
            nodes: &self.nodes,
            rules: &self.rules,
            fields: &self.fields,
            entry: self.entry,
        }
    }
}

#[derive(Serialize)]
pub(super) struct ProgramReplay<'a> {
    revision: Revision,
    nodes: &'a [NodeData],
    rules: &'a [RuleData],
    fields: &'a [FieldData],
    entry: RuleIx,
}

impl NodeData {
    pub(crate) const fn id(&self) -> NodeIx {
        self.id
    }

    pub(crate) const fn kind(&self) -> &NodeKindData {
        &self.kind
    }

    pub(crate) const fn span(&self) -> SourceSpan {
        self.span
    }
}

impl RuleData {
    pub(crate) const fn id(&self) -> RuleIx {
        self.id
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) const fn root(&self) -> NodeIx {
        self.root
    }
}

impl FieldData {
    pub(crate) const fn id(&self) -> FieldIx {
        self.id
    }

    pub(crate) const fn owner(&self) -> RuleIx {
        self.owner
    }

    pub(crate) const fn slot(&self) -> u32 {
        self.slot
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) const fn child(&self) -> NodeIx {
        self.child
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NodeRemap {
    from: Revision,
    to: Revision,
    nodes: Vec<Option<NodeIx>>,
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
        (old.revision() == self.from)
            .then(|| {
                self.nodes
                    .get(old.local().index())
                    .copied()
                    .flatten()
                    .map(|local| NodeId::bind(self.to, local))
            })
            .flatten()
    }

    pub const fn from_revision(&self) -> Revision {
        self.from
    }

    pub const fn to_revision(&self) -> Revision {
        self.to
    }

    pub(super) fn replay_value(&self) -> NodeRemapReplay<'_> {
        NodeRemapReplay {
            from: self.from,
            to: self.to,
            nodes: &self.nodes,
        }
    }
}

#[derive(Serialize)]
pub(super) struct NodeRemapReplay<'a> {
    from: Revision,
    to: Revision,
    nodes: &'a [Option<NodeIx>],
}

#[cfg(test)]
mod representation_laws {
    use std::mem::size_of;

    use super::*;

    #[test]
    fn local_ordinals_and_syntax_rows_stay_compact() {
        assert_eq!(size_of::<NodeIx>(), 4);
        assert_eq!(size_of::<RuleIx>(), 4);
        assert_eq!(size_of::<FieldIx>(), 4);
        assert_eq!(size_of::<TransformIx>(), 4);
        assert_eq!(size_of::<TypeIx>(), 4);
        assert_eq!(size_of::<SccIx>(), 4);
        assert_eq!(size_of::<PlanNodeIx>(), 4);
        assert!(size_of::<Option<NodeIx>>() <= 8);
        assert!(size_of::<Option<RuleIx>>() <= 8);
        assert!(size_of::<Option<FieldIx>>() <= 8);
        assert!(size_of::<Option<TransformIx>>() <= 8);
        assert!(size_of::<Option<TypeIx>>() <= 8);
        assert!(size_of::<Option<SccIx>>() <= 8);
        assert!(size_of::<Option<PlanNodeIx>>() <= 8);
        assert_eq!(size_of::<NodeId>(), 36);

        // These are the pre-branding row ceilings. Public capabilities may be
        // wider, but the persistent carrier rows must not inherit that width.
        assert!(size_of::<NodeData>() <= 56);
        assert!(size_of::<RuleData>() <= 40);
        assert!(size_of::<FieldData>() <= 48);
        assert!(size_of::<NodeKindData>() <= 40);
        assert!(size_of::<RecordSlotData>() <= 12);
    }

    #[test]
    fn remap_accepts_only_its_source_brand_and_emits_its_target_brand() {
        let from = Revision::for_module("before.bbnf", b"root = \"a\";");
        let to = Revision::for_module("after.bbnf", b"root = \"b\";");
        let local = NodeIx::checked(0).expect("zero ordinal");
        let remap = NodeRemap {
            from,
            to,
            nodes: vec![Some(local)],
        };

        let mapped = remap
            .get(NodeId::bind(from, local))
            .expect("source capability maps");
        assert_eq!(mapped.revision(), to);
        assert_eq!(mapped.index(), 0);
        assert!(remap.get(NodeId::bind(to, local)).is_none());
    }
}
