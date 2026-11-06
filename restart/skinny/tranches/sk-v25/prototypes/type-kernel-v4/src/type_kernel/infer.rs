//! Immutable, grammar-agnostic input snapshot and whole-grammar constraint generation.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

use super::store::{ExportKey, Primitive, ProblemBuilder, RowKind};
use super::{
    ClosedProblem, OperationContext, Origin, TypeError, TypeErrorKind, charge_text, fnv1a64,
};

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct NodeId(pub u32);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct RuleId(pub u32);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct HostTypeId(pub u32);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum HostEffect {
    Pure,
    Fallible,
    Stateful,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum HostTypeNode {
    Primitive(Primitive),
    Named(String),
    Tuple(Vec<HostTypeId>),
    List(HostTypeId),
    Option(HostTypeId),
    Function {
        parameters: Vec<HostTypeId>,
        result: HostTypeId,
    },
    Record(Vec<(String, HostTypeId)>),
    Variant(Vec<(String, HostTypeId)>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FnDescriptor {
    BoxWrap,
    NumberConvert,
    HexConvert,
    SpanCapture,
    EnumWrap {
        variant: String,
    },
    Expr {
        function: u32,
        return_type: Option<HostTypeId>,
        effect: HostEffect,
    },
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct SeqPolicy {
    pub collapse_simple_spans: bool,
    pub preserve_pretty: bool,
    pub has_sp_method: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RefMode {
    Scalar,
    Compound,
    Vector,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum GrammarNode {
    Literal(String),
    Regex(String),
    Epsilon,
    Seq {
        items: Vec<NodeId>,
        policy: SeqPolicy,
    },
    Alt {
        branches: Vec<NodeId>,
        homogeneous: bool,
    },
    Optional {
        child: NodeId,
        collapse_span: bool,
    },
    Repeat {
        child: NodeId,
        one_or_more: bool,
        collapse_span: bool,
    },
    Ref {
        rule: RuleId,
        mode: RefMode,
    },
    Map {
        child: NodeId,
        function: FnDescriptor,
    },
    Skip {
        kept: NodeId,
        discarded: NodeId,
    },
    Next {
        discarded: NodeId,
        kept: NodeId,
    },
    Minus {
        kept: NodeId,
        predicate: NodeId,
    },
    Negate(NodeId),
    OptionalWhitespace(NodeId),
    TokenDispatch {
        arms: Vec<(String, NodeId)>,
        fallback: NodeId,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RuleSpec {
    pub id: RuleId,
    pub name: String,
    pub root: NodeId,
    pub declared_type: Option<HostTypeId>,
    pub transparent: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GrammarSpec {
    pub rules: Vec<RuleSpec>,
    pub nodes: Vec<(NodeId, GrammarNode)>,
    pub host_types: Vec<HostTypeNode>,
    pub source_name: String,
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct InputDigest(String);

impl InputDigest {
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for InputDigest {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}

/// Immutable canonical snapshot. It has no result sidecars to mutate or ingest.
#[derive(Clone, Debug)]
pub struct KernelInput {
    rules: Vec<RuleSpec>,
    nodes: Vec<(NodeId, GrammarNode)>,
    host_types: Vec<HostTypeNode>,
    source_name: String,
    digest: InputDigest,
}

impl KernelInput {
    pub fn snapshot(
        mut grammar: GrammarSpec,
        operations: &mut OperationContext,
    ) -> Result<Self, TypeError> {
        charge_text(
            operations,
            "input.source_name",
            &grammar.source_name,
            &[],
            Origin::boundary("input.snapshot"),
        )?;
        grammar.rules.sort_by_key(|rule| rule.id);
        grammar.nodes.sort_by_key(|(id, _)| *id);
        reject_duplicate_ids(
            grammar.rules.iter().map(|rule| rule.id.0),
            "rule",
            operations,
        )?;
        reject_duplicate_ids(grammar.nodes.iter().map(|(id, _)| id.0), "node", operations)?;
        let rule_ids = grammar
            .rules
            .iter()
            .map(|rule| rule.id)
            .collect::<BTreeSet<_>>();
        let node_ids = grammar
            .nodes
            .iter()
            .map(|(id, _)| *id)
            .collect::<BTreeSet<_>>();
        for (index, host) in grammar.host_types.iter().enumerate() {
            operations.charge(
                1,
                "input.host_type",
                &[],
                [Origin::new("input.host_type", index.to_string())],
            )?;
            validate_host_type(index, host, operations)?;
        }
        for rule in &grammar.rules {
            charge_text(
                operations,
                "input.rule.name",
                &rule.name,
                &[],
                Origin::new("input.rule", rule.id.0.to_string()),
            )?;
            if !node_ids.contains(&rule.root) {
                return Err(operations.fail(
                    TypeErrorKind::MissingNode { node: rule.root.0 },
                    &[],
                    "input.rule.root",
                ));
            }
            validate_host_id(rule.declared_type, grammar.host_types.len(), operations)?;
        }
        for (id, node) in &grammar.nodes {
            validate_node(
                *id,
                node,
                &node_ids,
                &rule_ids,
                grammar.host_types.len(),
                operations,
            )?;
        }
        let bytes = canonical_input_bytes(&grammar, operations)?;
        operations.charge(
            bytes.len().max(1),
            "input.digest.bytes",
            &[],
            [Origin::boundary("input.digest")],
        )?;
        let digest = InputDigest(format!("fnv1a64:{:016x}", fnv1a64(&bytes)));
        Ok(Self {
            rules: grammar.rules,
            nodes: grammar.nodes,
            host_types: grammar.host_types,
            source_name: grammar.source_name,
            digest,
        })
    }

    #[must_use]
    pub fn digest(&self) -> &InputDigest {
        &self.digest
    }

    #[must_use]
    pub fn source_name(&self) -> &str {
        &self.source_name
    }

    #[must_use]
    pub fn rule_count(&self) -> usize {
        self.rules.len()
    }

    #[must_use]
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }
}

/// Generate constraints for every node and rule, then consume the draft builder.
pub fn infer(
    input: &KernelInput,
    operations: &mut OperationContext,
) -> Result<ClosedProblem, TypeError> {
    let mut builder = ProblemBuilder::with_input_digest(input.digest.clone());
    let mut host_types = Vec::with_capacity(input.host_types.len());
    for (index, host) in input.host_types.iter().enumerate() {
        operations.charge(
            1,
            "infer.host_type",
            &[],
            [Origin::new("infer.host_type", index.to_string())],
        )?;
        let ty = match host {
            HostTypeNode::Primitive(primitive) => builder.primitive(*primitive, operations)?,
            HostTypeNode::Named(name) => builder.named(name.clone(), operations)?,
            HostTypeNode::Tuple(items) => {
                builder.tuple(items.iter().map(|id| host_types[id.0 as usize]), operations)?
            }
            HostTypeNode::List(item) => builder.list(host_types[item.0 as usize], operations)?,
            HostTypeNode::Option(item) => {
                builder.option(host_types[item.0 as usize], operations)?
            }
            HostTypeNode::Function { parameters, result } => builder.function(
                parameters.iter().map(|id| host_types[id.0 as usize]),
                host_types[result.0 as usize],
                operations,
            )?,
            HostTypeNode::Record(fields) | HostTypeNode::Variant(fields) => {
                let kind = if matches!(host, HostTypeNode::Record(_)) {
                    RowKind::Record
                } else {
                    RowKind::Variant
                };
                let row = builder.row(
                    kind,
                    fields
                        .iter()
                        .map(|(label, id)| (label.clone(), host_types[id.0 as usize])),
                    None,
                    Origin::new("infer.host_type", index.to_string()),
                    operations,
                )?;
                match kind {
                    RowKind::Record => builder.record(row, operations)?,
                    RowKind::Variant => builder.variant(row, operations)?,
                }
            }
        };
        host_types.push(ty);
    }
    let mut node_types = BTreeMap::new();
    for (id, _) in &input.nodes {
        node_types.insert(*id, builder.meta(0, operations)?);
    }
    let rules = input
        .rules
        .iter()
        .map(|rule| (rule.id, rule))
        .collect::<BTreeMap<_, _>>();
    for (id, node) in &input.nodes {
        let origin = Origin::new("infer.node", id.0.to_string());
        let actual = node_types[id];
        let expected = infer_node(
            node,
            &rules,
            &node_types,
            &host_types,
            &mut builder,
            origin.clone(),
            operations,
        )?;
        builder.equal(actual, expected, origin, operations)?;
    }
    let mut exports = Vec::new();
    for rule in &input.rules {
        let root = node_types[&rule.root];
        if let Some(declared) = rule.declared_type {
            builder.equal(
                root,
                host_types[declared.0 as usize],
                Origin::new("infer.rule.declared", rule.name.clone()),
                operations,
            )?;
        }
        exports.push((ExportKey::new(format!("rule:{}", rule.name)), root));
    }
    for (id, _) in &input.nodes {
        exports.push((ExportKey::new(format!("node:{}", id.0)), node_types[id]));
    }
    builder
        .close(exports, operations)
        .map(|(problem, _)| problem)
}

fn infer_node(
    node: &GrammarNode,
    rules: &BTreeMap<RuleId, &RuleSpec>,
    node_types: &BTreeMap<NodeId, super::DraftTypeId>,
    host_types: &[super::DraftTypeId],
    builder: &mut ProblemBuilder,
    origin: Origin,
    operations: &mut OperationContext,
) -> Result<super::DraftTypeId, TypeError> {
    operations.charge(1, "infer.node.classify", &[], [origin.clone()])?;
    match node {
        GrammarNode::Literal(_) | GrammarNode::Regex(_) | GrammarNode::Epsilon => {
            builder.primitive(Primitive::Span, operations)
        }
        GrammarNode::Seq { items, policy } => {
            if items.is_empty() {
                builder.tuple([], operations)
            } else if items.len() == 1 {
                Ok(node_types[&items[0]])
            } else if policy.collapse_simple_spans
                && !policy.preserve_pretty
                && !policy.has_sp_method
            {
                builder.primitive(Primitive::Span, operations)
            } else {
                builder.tuple(items.iter().map(|id| node_types[id]), operations)
            }
        }
        GrammarNode::Alt {
            branches,
            homogeneous,
        } => {
            if branches.is_empty() {
                builder.tuple([], operations)
            } else if *homogeneous {
                let first = node_types[&branches[0]];
                for branch in &branches[1..] {
                    builder.equal(first, node_types[branch], origin.clone(), operations)?;
                }
                Ok(first)
            } else {
                let row = builder.row(
                    RowKind::Variant,
                    branches
                        .iter()
                        .enumerate()
                        .map(|(index, id)| (format!("branch_{index}"), node_types[id])),
                    None,
                    origin,
                    operations,
                )?;
                builder.variant(row, operations)
            }
        }
        GrammarNode::Optional {
            child,
            collapse_span,
        }
        | GrammarNode::Repeat {
            child,
            collapse_span,
            ..
        } if *collapse_span => {
            let _ = node_types[child];
            builder.primitive(Primitive::Span, operations)
        }
        GrammarNode::Optional { child, .. } => builder.option(node_types[child], operations),
        GrammarNode::Repeat { child, .. } => builder.list(node_types[child], operations),
        GrammarNode::Ref { rule, mode } => {
            let target = rules.get(rule).ok_or_else(|| {
                operations.fail(
                    TypeErrorKind::MissingRule { rule: rule.0 },
                    &[],
                    "infer.ref",
                )
            })?;
            match mode {
                RefMode::Compound => builder.primitive(Primitive::BoxedEnum, operations),
                RefMode::Vector => builder.primitive(Primitive::Enum, operations),
                RefMode::Scalar => target.declared_type.map_or_else(
                    || builder.named(target.name.clone(), operations),
                    |id| Ok(host_types[id.0 as usize]),
                ),
            }
        }
        GrammarNode::Map { child, function } => {
            let _ = node_types[child];
            match function {
                FnDescriptor::BoxWrap => builder.primitive(Primitive::BoxedEnum, operations),
                FnDescriptor::NumberConvert => builder.primitive(Primitive::F64, operations),
                FnDescriptor::HexConvert => builder.primitive(Primitive::U32, operations),
                FnDescriptor::SpanCapture => builder.primitive(Primitive::Span, operations),
                FnDescriptor::EnumWrap { variant } => {
                    charge_text(operations, "infer.enum_variant", variant, &[], origin)?;
                    builder.primitive(Primitive::Enum, operations)
                }
                FnDescriptor::Expr {
                    function,
                    return_type,
                    effect: _,
                } => return_type.map_or_else(
                    || {
                        Err(operations.fail(
                            TypeErrorKind::MissingHostSignature {
                                function: *function,
                            },
                            &[],
                            "infer.host_signature",
                        ))
                    },
                    |id| Ok(host_types[id.0 as usize]),
                ),
            }
        }
        GrammarNode::Skip { kept, discarded }
        | GrammarNode::Minus {
            kept,
            predicate: discarded,
        }
        | GrammarNode::Next { discarded, kept } => {
            let _ = node_types[discarded];
            Ok(node_types[kept])
        }
        GrammarNode::Negate(child) => {
            let _ = node_types[child];
            builder.tuple([], operations)
        }
        GrammarNode::OptionalWhitespace(child) => Ok(node_types[child]),
        GrammarNode::TokenDispatch { arms, fallback } => {
            let row = builder.row(
                RowKind::Variant,
                arms.iter()
                    .map(|(token, id)| (token.clone(), node_types[id]))
                    .chain(std::iter::once((
                        "fallback".to_owned(),
                        node_types[fallback],
                    ))),
                None,
                origin,
                operations,
            )?;
            builder.variant(row, operations)
        }
    }
}

fn reject_duplicate_ids(
    ids: impl IntoIterator<Item = u32>,
    kind: &str,
    operations: &OperationContext,
) -> Result<(), TypeError> {
    let mut seen = BTreeSet::new();
    for id in ids {
        if !seen.insert(id) {
            return Err(operations.fail(
                TypeErrorKind::InvalidInput {
                    detail: format!("duplicate {kind} id {id}"),
                },
                &[],
                "input.snapshot",
            ));
        }
    }
    Ok(())
}

fn validate_host_type(
    index: usize,
    host: &HostTypeNode,
    operations: &mut OperationContext,
) -> Result<(), TypeError> {
    let ids = match host {
        HostTypeNode::Tuple(items) => items.clone(),
        HostTypeNode::List(item) | HostTypeNode::Option(item) => vec![*item],
        HostTypeNode::Function { parameters, result } => {
            let mut ids = parameters.clone();
            ids.push(*result);
            ids
        }
        HostTypeNode::Record(fields) | HostTypeNode::Variant(fields) => {
            let mut labels = BTreeSet::new();
            for (label, _) in fields {
                charge_text(
                    operations,
                    "input.host_type.label",
                    label,
                    &[],
                    Origin::new("input.host_type", index.to_string()),
                )?;
                if !labels.insert(label) {
                    return Err(operations.fail(
                        TypeErrorKind::DuplicateRowLabel {
                            label: label.clone(),
                        },
                        &[],
                        "input.host_type",
                    ));
                }
            }
            fields.iter().map(|(_, id)| *id).collect()
        }
        HostTypeNode::Named(name) => {
            charge_text(
                operations,
                "input.host_type.name",
                name,
                &[],
                Origin::new("input.host_type", index.to_string()),
            )?;
            Vec::new()
        }
        HostTypeNode::Primitive(_) => Vec::new(),
    };
    for id in ids {
        if id.0 as usize >= index {
            return Err(operations.fail(
                TypeErrorKind::InvalidInput {
                    detail: format!("host type {} is not a prior flat-arena node", id.0),
                },
                &[],
                "input.host_type",
            ));
        }
    }
    Ok(())
}

fn validate_host_id(
    id: Option<HostTypeId>,
    len: usize,
    operations: &OperationContext,
) -> Result<(), TypeError> {
    if id.is_some_and(|id| id.0 as usize >= len) {
        return Err(operations.fail(
            TypeErrorKind::InvalidInput {
                detail: "dangling host type".to_owned(),
            },
            &[],
            "input.host_type",
        ));
    }
    Ok(())
}

fn validate_node(
    id: NodeId,
    node: &GrammarNode,
    nodes: &BTreeSet<NodeId>,
    rules: &BTreeSet<RuleId>,
    host_len: usize,
    operations: &mut OperationContext,
) -> Result<(), TypeError> {
    operations.charge(
        1,
        "input.node",
        &[],
        [Origin::new("input.node", id.0.to_string())],
    )?;
    let mut edges = Vec::new();
    match node {
        GrammarNode::Literal(value) | GrammarNode::Regex(value) => {
            charge_text(
                operations,
                "input.node.text",
                value,
                &[],
                Origin::new("input.node", id.0.to_string()),
            )?;
        }
        GrammarNode::Seq { items, .. } => edges.extend(items),
        GrammarNode::Alt { branches, .. } => edges.extend(branches),
        GrammarNode::Optional { child, .. }
        | GrammarNode::Repeat { child, .. }
        | GrammarNode::Negate(child)
        | GrammarNode::OptionalWhitespace(child) => edges.push(child),
        GrammarNode::Ref { rule, .. } => {
            if !rules.contains(rule) {
                return Err(operations.fail(
                    TypeErrorKind::MissingRule { rule: rule.0 },
                    &[],
                    "input.node.ref",
                ));
            }
        }
        GrammarNode::Map { child, function } => {
            edges.push(child);
            match function {
                FnDescriptor::EnumWrap { variant } => {
                    charge_text(
                        operations,
                        "input.function.variant",
                        variant,
                        &[],
                        Origin::new("input.node", id.0.to_string()),
                    )?;
                }
                FnDescriptor::Expr { return_type, .. } => {
                    validate_host_id(*return_type, host_len, operations)?;
                }
                FnDescriptor::BoxWrap
                | FnDescriptor::NumberConvert
                | FnDescriptor::HexConvert
                | FnDescriptor::SpanCapture => {}
            }
        }
        GrammarNode::Skip { kept, discarded }
        | GrammarNode::Next { discarded, kept }
        | GrammarNode::Minus {
            kept,
            predicate: discarded,
        } => edges.extend([kept, discarded]),
        GrammarNode::TokenDispatch { arms, fallback } => {
            edges.push(fallback);
            for (token, arm) in arms {
                charge_text(
                    operations,
                    "input.node.token",
                    token,
                    &[],
                    Origin::new("input.node", id.0.to_string()),
                )?;
                edges.push(arm);
            }
        }
        GrammarNode::Epsilon => {}
    }
    operations.charge(
        edges.len().max(1),
        "input.node.edges",
        &[],
        [Origin::new("input.node", id.0.to_string())],
    )?;
    if let Some(missing) = edges.into_iter().find(|edge| !nodes.contains(edge)) {
        return Err(operations.fail(
            TypeErrorKind::MissingNode { node: missing.0 },
            &[],
            "input.node.edge",
        ));
    }
    Ok(())
}

fn canonical_input_bytes(
    grammar: &GrammarSpec,
    operations: &OperationContext,
) -> Result<Vec<u8>, TypeError> {
    // Debug is used only as a complete local encoding after validation; its bytes are
    // stable because every collection is ordered and no address/scope enters the value.
    let text = format!(
        "TKV4-INPUT\nsource={:?}\nhost={:?}\nrules={:?}\nnodes={:?}\n",
        grammar.source_name, grammar.host_types, grammar.rules, grammar.nodes
    );
    if text.len() > u32::MAX as usize {
        return Err(operations.fail(
            TypeErrorKind::ArithmeticOverflow,
            &[],
            "input.canonical_bytes",
        ));
    }
    Ok(text.into_bytes())
}
