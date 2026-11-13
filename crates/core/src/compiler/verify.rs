//! The stage verifier (sk-v27 LW3.COMP-6, CB-3).
//!
//! Every spine stage hands the next one an immutable carrier: `Program`,
//! `DependencySolution`, `TypeSolution`, `AnalysisFacts`, `LoweringPlan` and
//! `ScalarEventPlan`. The verifier reads each carrier through its own typed
//! views into plain rows (every reference as an arena ordinal) and checks the
//! laws the next stage relies on: identities are canonical, every reference
//! lands inside its arena, rule bodies are trees, the dependency graph and its
//! condensation agree with the program, types and facts cover the program row
//! for row, and the plan and its event sites agree with each other.
//!
//! A carrier that breaks a law is a compiler defect, never a grammar error, so
//! the verifier rejects with `DiagnosticCode::InternalInvariant` naming the
//! stage, the law and the row. Laws are read in stage order, so the first
//! violation reported is the earliest stage that broke one. `TypeSolution`'s
//! own certificate check (`TypeSolution::verify`) stays where it is; this
//! verifier reads the type carrier's shape and its agreement with the program.
//!
//! Each stage has its own check (`Spine::check_*`), so a pass admitted later
//! (LW5.COMP-1's OptIR islands, COMP-1f's ported passes) re-reads the carrier
//! it rewrote. The reads match every view variant exhaustively: a new node
//! kind, plan op, semantic type or event kind does not compile here until the
//! verifier reads it.

use core::fmt;

use super::analysis::AnalysisFacts;
use super::dependency::DependencySolution;
use super::diagnostic::{Diagnostic, DiagnosticCode, StageResult};
use super::event::{EventKind, ScalarEventPlan};
use super::lower::{LoweredRegex, LoweringPlan, PlanOp, PlanRecordSlot};
use super::pratt::{PrattFixity, PrattOperator, PrattTable};
use super::source::Revision;
use super::syntax::{NodeKind, Program, RecordSlot, RuleIx, SccIx};
use super::typecheck::{SemanticType, TypeSolution};

mod laws;
#[cfg(test)]
mod mutants;

/// Verify every stage carrier of one compilation, in stage order.
pub(super) fn spine(
    program: &Program,
    dependency: &DependencySolution,
    types: &TypeSolution,
    facts: &AnalysisFacts,
    lowering: &LoweringPlan,
    events: &ScalarEventPlan,
) -> StageResult<()> {
    match Spine::read(program, dependency, types, facts, lowering, events).check() {
        Ok(()) => StageResult::accepted(()),
        Err(violation) => StageResult::rejected(Diagnostic::new(
            DiagnosticCode::InternalInvariant,
            0,
            0,
            violation.to_string(),
        )),
    }
}

/// The spine stage whose carrier broke a law.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum Stage {
    Program,
    Dependency,
    Types,
    Analysis,
    Lowering,
    Events,
}

/// The law a carrier broke. Each law is stated where it is checked.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum Law {
    OneRevision,
    RowIdentity,
    NodeInRange,
    RuleInRange,
    FieldInRange,
    PrattInRange,
    RegexInRange,
    TypeInRange,
    AggregateHasChildren,
    RepeatBounds,
    TransformOrdinals,
    RuleNamesUnique,
    RuleBodiesAreTrees,
    NodeOwners,
    RecordSlotIsItsField,
    RecordBelongsToItsRule,
    PrattOperatorInItsTable,
    PrattFixityShape,
    EdgeRowPerRule,
    EdgesSortedUnique,
    EdgesAreReferences,
    ComponentsPartitionRules,
    ComponentIndex,
    ComponentsStronglyConnected,
    DependencyFirstOrder,
    RowPerProgramRow,
    NamedBinder,
    RootConclusion,
    ArenaCanonical,
    NullableHasZeroMinimum,
    RuleFactIsRootFact,
    FactTransfer,
    PlanKeepsRules,
    RegexTableWellFormed,
    TrimNeedsTrivia,
    EventSchema,
    SiteShape,
    FieldSiteIsItsField,
    PrattReduceIsItsTable,
    RuleEnteredAndLeftOnce,
    InputEndsOnceLast,
    TransformSites,
}

/// One broken law: the stage, the law and the row of that stage's carrier.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(super) struct Violation {
    pub(super) stage: Stage,
    pub(super) law: Law,
    pub(super) row: usize,
}

impl fmt::Display for Violation {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "stage verifier: the {:?} carrier breaks {:?} at row {}",
            self.stage, self.law, self.row
        )
    }
}

/// One node of a rule body, from the program or the plan, with every
/// reference read as an arena ordinal.
#[derive(Clone, Debug, Eq, PartialEq)]
enum Op {
    Epsilon,
    /// The literal's byte length.
    Literal(usize),
    /// The plan's regex-table ordinal; the program holds the pattern text.
    Regex(Option<usize>),
    RuleRef(usize),
    Sequence(Vec<usize>),
    Choice(Vec<usize>),
    Repeat {
        child: usize,
        minimum: u32,
        maximum: Option<u32>,
    },
    Transform {
        child: usize,
        transform: usize,
        name: String,
    },
    Projection {
        left: usize,
        right: usize,
    },
    Difference {
        include: usize,
        exclude: usize,
    },
    Trim {
        child: usize,
    },
    Pratt {
        atom: usize,
        table: usize,
    },
    Record {
        owner: usize,
        slots: Vec<Slot>,
    },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Slot {
    field: Option<usize>,
    child: usize,
}

impl Op {
    fn children(&self) -> Vec<usize> {
        match self {
            Self::Epsilon | Self::Literal(_) | Self::Regex(_) | Self::RuleRef(_) => Vec::new(),
            Self::Sequence(children) | Self::Choice(children) => children.clone(),
            Self::Repeat { child, .. } | Self::Transform { child, .. } | Self::Trim { child } => {
                vec![*child]
            }
            Self::Projection { left, right } => vec![*left, *right],
            Self::Difference { include, exclude } => vec![*include, *exclude],
            Self::Pratt { atom, .. } => vec![*atom],
            Self::Record { slots, .. } => slots.iter().map(|slot| slot.child).collect(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct RuleRow {
    id: usize,
    name: String,
    root: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct FieldRow {
    id: usize,
    owner: usize,
    slot: u32,
    child: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct TableRow {
    id: usize,
    site: usize,
    atom: usize,
    start: usize,
    len: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct OperatorRow {
    id: usize,
    table: usize,
    matcher: usize,
    fixity: PrattFixity,
    left: Option<u16>,
    right: Option<u16>,
}

/// The rule bodies the program and the plan share in shape.
#[derive(Clone, Debug)]
struct Graph {
    revision: Revision,
    nodes: Vec<(usize, Op)>,
    rules: Vec<RuleRow>,
    fields: Vec<FieldRow>,
    tables: Vec<TableRow>,
    operators: Vec<OperatorRow>,
    entry: usize,
}

#[derive(Clone, Debug)]
struct ProgramRead {
    graph: Graph,
    owners: Vec<usize>,
}

#[derive(Clone, Debug)]
struct DependencyRead {
    revision: Revision,
    graph_revision: Revision,
    edges: Vec<Vec<usize>>,
    components: Vec<Vec<usize>>,
    component_by_rule: Vec<usize>,
    order: Vec<usize>,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum TypeRow {
    Unit,
    Span,
    Named(usize),
    Product(Vec<usize>),
    Sum(Vec<usize>),
    Option(usize),
    List(usize),
    Record {
        owner: usize,
        fields: Vec<(usize, usize)>,
    },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct RuleTypeRow {
    rule: usize,
    named: usize,
    root: usize,
}

#[derive(Clone, Debug)]
struct TypesRead {
    revision: Revision,
    arena: Vec<TypeRow>,
    /// (node, type) per program node.
    nodes: Vec<(usize, usize)>,
    rules: Vec<RuleTypeRow>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct FactRow {
    id: usize,
    nullable: bool,
    minimum: Option<u64>,
}

#[derive(Clone, Debug)]
struct FactsRead {
    revision: Revision,
    nodes: Vec<FactRow>,
    rules: Vec<FactRow>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct RegexRow {
    byte_classes: Vec<u8>,
    transitions: Vec<u32>,
    accepting: Vec<bool>,
    state_count: u32,
    class_count: u16,
    start_state: u32,
}

#[derive(Clone, Debug)]
struct PlanRead {
    graph: Graph,
    node_types: Vec<usize>,
    rule_types: Vec<usize>,
    field_types: Vec<usize>,
    arena: Vec<TypeRow>,
    regexes: Vec<RegexRow>,
    trivia: Option<RegexRow>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct SiteRow {
    kind: EventKind,
    node: Option<usize>,
    rule: Option<usize>,
    field: Option<usize>,
    operator: Option<usize>,
}

#[derive(Clone, Debug)]
struct EventsRead {
    revision: Revision,
    schema: String,
    sites: Vec<SiteRow>,
    /// (transform, plan node, name).
    transforms: Vec<(usize, usize, String)>,
}

/// One compilation's carriers, read for the laws.
#[derive(Clone, Debug)]
struct Spine {
    program: ProgramRead,
    dependency: DependencyRead,
    types: TypesRead,
    facts: FactsRead,
    plan: PlanRead,
    events: EventsRead,
}

impl Spine {
    fn read(
        program: &Program,
        dependency: &DependencySolution,
        types: &TypeSolution,
        facts: &AnalysisFacts,
        lowering: &LoweringPlan,
        events: &ScalarEventPlan,
    ) -> Self {
        Self {
            program: read_program(program),
            dependency: read_dependency(dependency),
            types: TypesRead {
                revision: types.revision(),
                arena: types.arena().map(type_row).collect(),
                nodes: types
                    .node_derivations()
                    .map(|row| (row.node().index(), row.type_id().index()))
                    .collect(),
                rules: types
                    .rule_derivations()
                    .map(|row| RuleTypeRow {
                        rule: row.rule().index(),
                        named: row.named_type().index(),
                        root: row.root_type().index(),
                    })
                    .collect(),
            },
            facts: FactsRead {
                revision: facts.revision(),
                nodes: facts
                    .nodes()
                    .map(|row| FactRow {
                        id: row.node().index(),
                        nullable: row.nullable(),
                        minimum: row.minimum_consumption(),
                    })
                    .collect(),
                rules: facts
                    .rules()
                    .map(|row| FactRow {
                        id: row.rule().index(),
                        nullable: row.nullable(),
                        minimum: row.minimum_consumption(),
                    })
                    .collect(),
            },
            plan: read_plan(lowering),
            events: EventsRead {
                revision: events.revision(),
                schema: events.schema().to_owned(),
                sites: events
                    .sites()
                    .map(|(kind, node, rule, field, operator)| SiteRow {
                        kind,
                        node: node.map(|id| id.index()),
                        rule: rule.map(|id| id.index()),
                        field: field.map(|id| id.index()),
                        operator: operator.map(|id| id.index()),
                    })
                    .collect(),
                transforms: events
                    .transforms()
                    .map(|site| {
                        (
                            site.id().index(),
                            site.node().index(),
                            site.name().to_owned(),
                        )
                    })
                    .collect(),
            },
        }
    }

    fn check(&self) -> Result<(), Violation> {
        self.check_program()?;
        self.check_dependency()?;
        self.check_types()?;
        self.check_facts()?;
        self.check_lowering()?;
        self.check_events()
    }
}

fn read_program(program: &Program) -> ProgramRead {
    ProgramRead {
        graph: Graph {
            revision: program.revision(),
            nodes: program
                .nodes()
                .map(|node| (node.id().index(), node_op(node.kind())))
                .collect(),
            rules: program
                .rules()
                .map(|rule| RuleRow {
                    id: rule.id().index(),
                    name: rule.name().to_owned(),
                    root: rule.root().index(),
                })
                .collect(),
            fields: program
                .fields()
                .map(|field| FieldRow {
                    id: field.id().index(),
                    owner: field.owner().index(),
                    slot: field.slot(),
                    child: field.child().index(),
                })
                .collect(),
            tables: program.pratt_table_rows().iter().map(table_row).collect(),
            operators: program
                .pratt_operator_rows()
                .iter()
                .map(operator_row)
                .collect(),
            entry: program.entry().index(),
        },
        owners: program
            .node_owner_rows()
            .iter()
            .map(|rule| rule.index())
            .collect(),
    }
}

fn node_op(kind: NodeKind<'_>) -> Op {
    match kind {
        NodeKind::Epsilon => Op::Epsilon,
        NodeKind::Literal(bytes) => Op::Literal(bytes.len()),
        NodeKind::Regex(_) => Op::Regex(None),
        NodeKind::RuleRef(rule) => Op::RuleRef(rule.index()),
        NodeKind::Sequence(children) => Op::Sequence(children.map(|id| id.index()).collect()),
        NodeKind::Choice(children) => Op::Choice(children.map(|id| id.index()).collect()),
        NodeKind::Repeat {
            child,
            minimum,
            maximum,
        } => Op::Repeat {
            child: child.index(),
            minimum,
            maximum,
        },
        NodeKind::Transform {
            child,
            transform,
            name,
        } => Op::Transform {
            child: child.index(),
            transform: transform.index(),
            name: name.to_owned(),
        },
        NodeKind::Projection { left, right, .. } => Op::Projection {
            left: left.index(),
            right: right.index(),
        },
        NodeKind::Difference { include, exclude } => Op::Difference {
            include: include.index(),
            exclude: exclude.index(),
        },
        NodeKind::Trim { child } => Op::Trim {
            child: child.index(),
        },
        NodeKind::Pratt { atom, table } => Op::Pratt {
            atom: atom.index(),
            table: table.index(),
        },
        NodeKind::Record { owner, slots } => Op::Record {
            owner: owner.index(),
            slots: slots
                .map(|slot| match slot {
                    RecordSlot::Field { field, child } => Slot {
                        field: Some(field.index()),
                        child: child.index(),
                    },
                    RecordSlot::Discard { child } => Slot {
                        field: None,
                        child: child.index(),
                    },
                })
                .collect(),
        },
    }
}

fn plan_op(op: PlanOp<'_>) -> Op {
    match op {
        PlanOp::Epsilon => Op::Epsilon,
        PlanOp::Literal(bytes) => Op::Literal(bytes.len()),
        PlanOp::Regex(index) => Op::Regex(Some(index)),
        PlanOp::RuleRef(rule) => Op::RuleRef(rule.index()),
        PlanOp::Sequence(children) => Op::Sequence(children.map(|id| id.index()).collect()),
        PlanOp::Choice(children) => Op::Choice(children.map(|id| id.index()).collect()),
        PlanOp::Repeat {
            child,
            minimum,
            maximum,
        } => Op::Repeat {
            child: child.index(),
            minimum,
            maximum,
        },
        PlanOp::Record { owner, slots } => Op::Record {
            owner: owner.index(),
            slots: slots
                .map(|slot| match slot {
                    PlanRecordSlot::Field { field, child } => Slot {
                        field: Some(field.index()),
                        child: child.index(),
                    },
                    PlanRecordSlot::Discard { child } => Slot {
                        field: None,
                        child: child.index(),
                    },
                })
                .collect(),
        },
        PlanOp::Transform {
            child,
            transform,
            name,
        } => Op::Transform {
            child: child.index(),
            transform: transform.index(),
            name: name.to_owned(),
        },
        PlanOp::Projection { left, right, .. } => Op::Projection {
            left: left.index(),
            right: right.index(),
        },
        PlanOp::Difference { include, exclude } => Op::Difference {
            include: include.index(),
            exclude: exclude.index(),
        },
        PlanOp::Trim { child } => Op::Trim {
            child: child.index(),
        },
        PlanOp::Pratt { atom, table } => Op::Pratt {
            atom: atom.index(),
            table: table.index(),
        },
    }
}

fn table_row(table: &PrattTable) -> TableRow {
    TableRow {
        id: table.id().index(),
        site: table.site_rule().index(),
        atom: table.atom_rule().index(),
        start: table.operator_start_local().index(),
        len: table.operator_len() as usize,
    }
}

fn operator_row(operator: &PrattOperator) -> OperatorRow {
    OperatorRow {
        id: operator.id().index(),
        table: operator.table().index(),
        matcher: operator.matcher_rule().index(),
        fixity: operator.fixity(),
        left: operator.left_binding_power(),
        right: operator.right_binding_power(),
    }
}

fn type_row(semantic: SemanticType<'_>) -> TypeRow {
    match semantic {
        SemanticType::Unit => TypeRow::Unit,
        SemanticType::Span => TypeRow::Span,
        SemanticType::Named(rule) => TypeRow::Named(rule.index()),
        SemanticType::Product(children) => {
            TypeRow::Product(children.map(|id| id.index()).collect())
        }
        SemanticType::Sum(children) => TypeRow::Sum(children.map(|id| id.index()).collect()),
        SemanticType::Option(child) => TypeRow::Option(child.index()),
        SemanticType::List(child) => TypeRow::List(child.index()),
        SemanticType::Record { owner, fields } => TypeRow::Record {
            owner: owner.index(),
            fields: fields
                .map(|field| (field.field().index(), field.value().index()))
                .collect(),
        },
    }
}

fn read_dependency(dependency: &DependencySolution) -> DependencyRead {
    let graph = dependency.graph();
    let rules = (0..graph.rule_count()).filter_map(RuleIx::checked);
    DependencyRead {
        revision: dependency.revision(),
        graph_revision: graph.revision(),
        edges: rules
            .clone()
            .map(|rule| {
                graph
                    .dependencies_local(rule)
                    .iter()
                    .map(|target| target.index())
                    .collect()
            })
            .collect(),
        components: (0..dependency.component_count())
            .filter_map(SccIx::checked)
            .map(|component| {
                dependency
                    .component_local(component)
                    .iter()
                    .map(|rule| rule.index())
                    .collect()
            })
            .collect(),
        component_by_rule: rules
            .map(|rule| dependency.component_of_local(rule).index())
            .collect(),
        order: dependency
            .dependency_order_local()
            .iter()
            .map(|component| component.index())
            .collect(),
    }
}

fn read_plan(lowering: &LoweringPlan) -> PlanRead {
    PlanRead {
        graph: Graph {
            revision: lowering.revision(),
            nodes: lowering
                .nodes()
                .map(|node| (node.id().index(), plan_op(node.op())))
                .collect(),
            rules: lowering
                .rules()
                .map(|rule| RuleRow {
                    id: rule.id().index(),
                    name: rule.name().to_owned(),
                    root: rule.root().index(),
                })
                .collect(),
            fields: lowering
                .fields()
                .map(|field| FieldRow {
                    id: field.id().index(),
                    owner: field.owner().index(),
                    slot: field.slot(),
                    child: field.child().index(),
                })
                .collect(),
            tables: lowering.pratt_tables().iter().map(table_row).collect(),
            operators: lowering
                .pratt_operators()
                .iter()
                .map(operator_row)
                .collect(),
            entry: lowering.entry().index(),
        },
        node_types: lowering
            .nodes()
            .map(|node| node.semantic_type().index())
            .collect(),
        rule_types: lowering
            .rules()
            .map(|rule| rule.named_type().index())
            .collect(),
        field_types: lowering
            .fields()
            .map(|field| field.semantic_type().index())
            .collect(),
        arena: lowering.semantic_types().map(type_row).collect(),
        regexes: lowering.regexes().iter().map(regex_row).collect(),
        trivia: lowering.trivia().map(regex_row),
    }
}

fn regex_row(regex: &LoweredRegex) -> RegexRow {
    RegexRow {
        byte_classes: regex.byte_classes().to_vec(),
        transitions: regex.transitions().to_vec(),
        accepting: regex.accepting().to_vec(),
        state_count: regex.state_count(),
        class_count: regex.class_count(),
        start_state: regex.start_state(),
    }
}
