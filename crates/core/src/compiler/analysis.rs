use std::collections::{BTreeSet, VecDeque};
use std::ops::Range;

use bbnf_regex::{DEAD_STATE, RegexTablePlan};
use serde::ser::SerializeSeq;
use serde::{Serialize, Serializer};

use super::budget::CompileOptions;
use super::dependency::DependencySolution;
use super::diagnostic::{Diagnostic, DiagnosticCode, DiagnosticSet, StageResult};
use super::source::Revision;
use super::syntax::{NodeId, NodeIx, NodeKindData, Program, RuleId, RuleIx, SccIx};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct NodeFactsData {
    node: NodeIx,
    nullable: bool,
    minimum_consumption: Option<u64>,
}

#[derive(Clone, Copy, Debug)]
pub struct NodeFacts<'a> {
    revision: &'a Revision,
    data: &'a NodeFactsData,
}

impl NodeFacts<'_> {
    pub const fn revision(self) -> Revision {
        *self.revision
    }

    pub const fn node(self) -> NodeId {
        NodeId::bind(*self.revision, self.data.node)
    }

    pub const fn nullable(self) -> bool {
        self.data.nullable
    }

    /// Least bytes consumed by a successful match. `None` denotes an empty
    /// language rather than an unknown result.
    pub const fn minimum_consumption(self) -> Option<u64> {
        self.data.minimum_consumption
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct RuleFactsData {
    rule: RuleIx,
    nullable: bool,
    minimum_consumption: Option<u64>,
}

#[derive(Clone, Copy, Debug)]
pub struct RuleFacts<'a> {
    revision: &'a Revision,
    data: &'a RuleFactsData,
}

impl RuleFacts<'_> {
    pub const fn revision(self) -> Revision {
        *self.revision
    }

    pub const fn rule(self) -> RuleId {
        RuleId::bind(*self.revision, self.data.rule)
    }

    pub const fn nullable(self) -> bool {
        self.data.nullable
    }

    pub const fn minimum_consumption(self) -> Option<u64> {
        self.data.minimum_consumption
    }
}

macro_rules! fact_rows {
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

fact_rows!(NodeFactsRows, NodeFacts, NodeFactsData);
fact_rows!(RuleFactsRows, RuleFacts, RuleFactsData);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AnalysisFacts {
    revision: Revision,
    iterations: usize,
    work_units: usize,
    nodes: Vec<NodeFactsData>,
    rules: Vec<RuleFactsData>,
}

pub(super) struct AnalysisFactsReplay<'a>(&'a AnalysisFacts);

impl Serialize for AnalysisFactsReplay<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        #[derive(Serialize)]
        struct Wire<'a> {
            revision: Revision,
            iterations: usize,
            work_units: usize,
            nodes: FactWireRows<'a, NodeFactsData>,
            rules: FactWireRows<'a, RuleFactsData>,
        }

        Wire {
            revision: self.0.revision,
            iterations: self.0.iterations,
            work_units: self.0.work_units,
            nodes: FactWireRows {
                revision: self.0.revision,
                rows: &self.0.nodes,
            },
            rules: FactWireRows {
                revision: self.0.revision,
                rows: &self.0.rules,
            },
        }
        .serialize(serializer)
    }
}

struct FactWireRows<'a, T> {
    revision: Revision,
    rows: &'a [T],
}

impl Serialize for FactWireRows<'_, NodeFactsData> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        #[derive(Serialize)]
        struct Row {
            revision: Revision,
            node: NodeIx,
            nullable: bool,
            minimum_consumption: Option<u64>,
        }

        let mut sequence = serializer.serialize_seq(Some(self.rows.len()))?;
        for row in self.rows {
            sequence.serialize_element(&Row {
                revision: self.revision,
                node: row.node,
                nullable: row.nullable,
                minimum_consumption: row.minimum_consumption,
            })?;
        }
        sequence.end()
    }
}

impl Serialize for FactWireRows<'_, RuleFactsData> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        #[derive(Serialize)]
        struct Row {
            revision: Revision,
            rule: RuleIx,
            nullable: bool,
            minimum_consumption: Option<u64>,
        }

        let mut sequence = serializer.serialize_seq(Some(self.rows.len()))?;
        for row in self.rows {
            sequence.serialize_element(&Row {
                revision: self.revision,
                rule: row.rule,
                nullable: row.nullable,
                minimum_consumption: row.minimum_consumption,
            })?;
        }
        sequence.end()
    }
}

impl AnalysisFacts {
    pub(super) fn replay_value(&self) -> AnalysisFactsReplay<'_> {
        AnalysisFactsReplay(self)
    }

    pub(crate) fn derive(
        program: &Program,
        dependency: &DependencySolution,
        options: CompileOptions,
    ) -> StageResult<Self> {
        Self::derive_with_orders(
            program,
            dependency,
            options,
            &(0..program.node_data_rows().len()).collect::<Vec<_>>(),
            &(0..program.rule_data_rows().len()).collect::<Vec<_>>(),
        )
    }

    fn derive_with_orders(
        program: &Program,
        dependency: &DependencySolution,
        options: CompileOptions,
        node_order: &[usize],
        rule_order: &[usize],
    ) -> StageResult<Self> {
        if program.revision() != dependency.revision() {
            return StageResult::rejected(Diagnostic::new(
                DiagnosticCode::RevisionMismatch,
                0,
                0,
                format!(
                    "analysis cannot mix program revision {} with dependency revision {}",
                    program.revision().hex(),
                    dependency.revision().hex()
                ),
            ));
        }
        if !is_permutation(node_order, program.node_data_rows().len())
            || !is_permutation(rule_order, program.rule_data_rows().len())
        {
            return StageResult::rejected(Diagnostic::new(
                DiagnosticCode::InternalInvariant,
                0,
                0,
                "analysis schedule is not a complete identity permutation",
            ));
        }

        let revision = program.revision();
        let rule_rank = permutation_rank(rule_order);
        let mut work = vec![WorkFacts::default(); program.node_data_rows().len()];
        for &index in node_order {
            let node = &program.node_data_rows()[index];
            match node.kind() {
                NodeKindData::Epsilon => {
                    work[node.id().index()] = WorkFacts::some(0);
                }
                NodeKindData::Literal(bytes) => {
                    work[node.id().index()] = WorkFacts::some(bytes.len() as u64);
                }
                NodeKindData::Regex(pattern) => {
                    let plan = match RegexTablePlan::compile_with(
                        pattern,
                        options.regex.table_options(),
                    ) {
                        Ok(plan) => plan,
                        Err(error) => {
                            let span = node.span();
                            return StageResult::rejected(Diagnostic::new(
                                DiagnosticCode::Regex,
                                span.start() as usize,
                                span.end() as usize,
                                format!("regex analysis failed: {error}"),
                            ));
                        }
                    };
                    work[node.id().index()] = regex_facts(&plan);
                }
                NodeKindData::RuleRef(_)
                | NodeKindData::Sequence(_)
                | NodeKindData::Choice(_)
                | NodeKindData::Repeat { .. }
                | NodeKindData::Record { .. }
                | NodeKindData::Transform { .. } => {}
            }
        }
        let mut rule_work = vec![WorkFacts::default(); program.rule_data_rows().len()];

        let mut iterations = 0_usize;
        let mut work_budget = AnalysisWorkBudget::new(options.analysis_work);
        let rule_node_ranges = match rule_node_ranges(program, &mut work_budget) {
            Ok(ranges) => ranges,
            Err(diagnostic) => return StageResult::rejected(diagnostic),
        };
        for &component in dependency.dependency_order_local() {
            let members = dependency.component_local(component);
            let mut scheduled_members = members.to_vec();
            scheduled_members.sort_by_key(|rule| rule_rank[rule.index()]);
            let nodes =
                match component_nodes(&scheduled_members, &rule_node_ranges, &mut work_budget) {
                    Ok(nodes) => nodes,
                    Err(diagnostic) => return StageResult::rejected(diagnostic),
                };
            let pass_units = nodes
                .len()
                .checked_add(members.len())
                .expect("node and rule budgets fit usize");
            let recursive = dependency.is_recursive_local(component) == Some(true);
            let mut component_iterations = 0_usize;

            loop {
                if recursive && component_iterations >= options.analysis_iterations {
                    return StageResult::rejected(Diagnostic::new(
                        DiagnosticCode::ResourceLimit,
                        0,
                        0,
                        format!(
                            "analysis component {} exceeded the deterministic limit of {} iterations",
                            component.index(),
                            options.analysis_iterations
                        ),
                    ));
                }
                if let Err(diagnostic) = work_budget.charge(pass_units) {
                    return StageResult::rejected(diagnostic);
                }
                component_iterations += 1;
                iterations = iterations
                    .checked_add(1)
                    .expect("analysis work budget bounds iteration accounting");

                let changed = evaluate_component_pass(
                    program,
                    dependency,
                    component,
                    members,
                    &scheduled_members,
                    &nodes,
                    &mut work,
                    &mut rule_work,
                );
                if !recursive || !changed {
                    break;
                }
            }
        }

        if let Some(node) = minimum_overflow_origin(program, &work) {
            let span = node.span();
            return StageResult::rejected(Diagnostic::new(
                DiagnosticCode::MinimumConsumptionOverflow,
                span.start() as usize,
                span.end() as usize,
                "the exact minimum successful consumption exceeds u64::MAX bytes",
            ));
        }

        let mut semantic_diagnostics = Vec::new();
        for node in program.node_data_rows() {
            let NodeKindData::Repeat { child, .. } = node.kind() else {
                continue;
            };
            if work[child.index()].nullable {
                let span = node.span();
                semantic_diagnostics.push(Diagnostic::new(
                    DiagnosticCode::NullableRepeat,
                    span.start() as usize,
                    span.end() as usize,
                    "repetition of a nullable expression is rejected because parse-that requires every repeated match to advance input",
                ));
            }
        }

        let zero_call_edges = zero_consumption_calls(program, &work);
        let zero_cycles = cyclic_components(&zero_call_edges);
        let zero_cycle_rules = zero_cycles
            .iter()
            .flatten()
            .copied()
            .collect::<BTreeSet<_>>();

        for component_index in 0..dependency.component_count() {
            let component = SccIx::checked(component_index).expect("component ordinal was bounded");
            if dependency.is_recursive_local(component) != Some(true) {
                continue;
            }
            let members = dependency.component_local(component);
            let empty_members = members
                .iter()
                .copied()
                .filter(|rule| rule_work[rule.index()].minimum_consumption.is_empty())
                .collect::<Vec<_>>();
            if !empty_members.is_empty()
                && !members.iter().any(|rule| zero_cycle_rules.contains(rule))
            {
                let first = empty_members[0];
                let rule = &program.rule_data_rows()[first.index()];
                let span = program.node_data(rule.root()).span();
                let names = empty_members
                    .iter()
                    .map(|rule| program.rule_data_rows()[rule.index()].name())
                    .collect::<Vec<_>>()
                    .join(", ");
                semantic_diagnostics.push(Diagnostic::new(
                    DiagnosticCode::RecursiveRule,
                    span.start() as usize,
                    span.end() as usize,
                    format!(
                        "recursive component {component_index} has no finite successful derivation for: {names}"
                    ),
                ));
            }
        }

        for cycle in zero_cycles {
            let first = cycle[0];
            let rule = &program.rule_data_rows()[first.index()];
            let span = program.node_data(rule.root()).span();
            let names = cycle
                .iter()
                .map(|rule| program.rule_data_rows()[rule.index()].name())
                .collect::<Vec<_>>()
                .join(" -> ");
            semantic_diagnostics.push(Diagnostic::new(
                DiagnosticCode::RecursiveRule,
                span.start() as usize,
                span.end() as usize,
                format!(
                    "recursive call cycle can re-enter without consuming input: {names}; \
                     every recursive cycle must cross a consuming terminal"
                ),
            ));
        }

        if !semantic_diagnostics.is_empty() {
            return StageResult::rejected_set(DiagnosticSet::from_vec(semantic_diagnostics));
        }

        let nodes = program
            .node_data_rows()
            .iter()
            .map(|node| NodeFactsData {
                node: node.id(),
                nullable: work[node.id().index()].nullable,
                minimum_consumption: work[node.id().index()].minimum_consumption.as_finite(),
            })
            .collect();
        let rules = program
            .rule_data_rows()
            .iter()
            .map(|rule| RuleFactsData {
                rule: rule.id(),
                nullable: rule_work[rule.id().index()].nullable,
                minimum_consumption: rule_work[rule.id().index()].minimum_consumption.as_finite(),
            })
            .collect();

        StageResult::accepted(Self {
            revision,
            iterations,
            work_units: work_budget.used(),
            nodes,
            rules,
        })
    }

    pub const fn revision(&self) -> Revision {
        self.revision
    }

    pub const fn iterations(&self) -> usize {
        self.iterations
    }

    /// Deterministic count of postorder validation, component scheduling, and
    /// node/rule fact evaluation work. Compilation rejects before this exceeds
    /// `analysis_work`.
    pub const fn work_units(&self) -> usize {
        self.work_units
    }

    pub fn nodes(&self) -> NodeFactsRows<'_> {
        NodeFactsRows::new(&self.revision, &self.nodes)
    }

    pub fn rules(&self) -> RuleFactsRows<'_> {
        RuleFactsRows::new(&self.revision, &self.rules)
    }

    pub fn node(&self, id: NodeId) -> Option<NodeFacts<'_>> {
        (id.revision() == self.revision)
            .then(|| self.nodes.get(id.local().index()))
            .flatten()
            .map(|data| NodeFacts {
                revision: &self.revision,
                data,
            })
    }

    pub fn rule(&self, id: RuleId) -> Option<RuleFacts<'_>> {
        (id.revision() == self.revision)
            .then(|| self.rules.get(id.local().index()))
            .flatten()
            .map(|data| RuleFacts {
                revision: &self.revision,
                data,
            })
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
enum MinimumConsumption {
    #[default]
    Empty,
    Finite(u64),
    Overflow,
}

impl MinimumConsumption {
    const fn as_finite(self) -> Option<u64> {
        match self {
            Self::Finite(value) => Some(value),
            Self::Empty | Self::Overflow => None,
        }
    }

    const fn is_empty(self) -> bool {
        matches!(self, Self::Empty)
    }

    const fn minimum_success(self, other: Self) -> Self {
        match (self, other) {
            (Self::Empty, value) | (value, Self::Empty) => value,
            (Self::Overflow, Self::Overflow) => Self::Overflow,
            (Self::Finite(value), Self::Overflow) | (Self::Overflow, Self::Finite(value)) => {
                Self::Finite(value)
            }
            (Self::Finite(left), Self::Finite(right)) => {
                Self::Finite(if left < right { left } else { right })
            }
        }
    }

    fn sequence(values: impl IntoIterator<Item = Self>) -> Self {
        let mut total = 0_u64;
        let mut overflowed = false;
        for value in values {
            match value {
                Self::Empty => return Self::Empty,
                Self::Overflow => overflowed = true,
                Self::Finite(value) if !overflowed => {
                    let Some(next) = total.checked_add(value) else {
                        overflowed = true;
                        continue;
                    };
                    total = next;
                }
                Self::Finite(_) => {}
            }
        }
        if overflowed {
            Self::Overflow
        } else {
            Self::Finite(total)
        }
    }

    const fn repeated(self, count: u32) -> Self {
        if count == 0 {
            return Self::Finite(0);
        }
        match self {
            Self::Empty => Self::Empty,
            Self::Overflow => Self::Overflow,
            Self::Finite(value) => match value.checked_mul(count as u64) {
                Some(product) => Self::Finite(product),
                None => Self::Overflow,
            },
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
struct WorkFacts {
    nullable: bool,
    minimum_consumption: MinimumConsumption,
}

impl WorkFacts {
    const fn some(minimum_consumption: u64) -> Self {
        Self {
            nullable: minimum_consumption == 0,
            minimum_consumption: MinimumConsumption::Finite(minimum_consumption),
        }
    }
}

fn regex_facts(plan: &RegexTablePlan) -> WorkFacts {
    let start = plan.start_state() as usize;
    if plan.accepting()[start] {
        return WorkFacts::some(0);
    }

    let classes = plan.class_count() as usize;
    let mut distance = vec![None; plan.state_count() as usize];
    distance[start] = Some(0_u64);
    let mut pending = VecDeque::from([start]);
    while let Some(state) = pending.pop_front() {
        let next_distance = distance[state]
            .expect("queued regex states have a distance")
            .saturating_add(1);
        for &target in &plan.transitions()[state * classes..(state + 1) * classes] {
            if target == DEAD_STATE {
                continue;
            }
            let target = target as usize;
            if distance[target].is_some() {
                continue;
            }
            if plan.accepting()[target] {
                return WorkFacts::some(next_distance);
            }
            distance[target] = Some(next_distance);
            pending.push_back(target);
        }
    }
    WorkFacts::default()
}

fn sequence_facts(children: &[NodeIx], facts: &[WorkFacts]) -> WorkFacts {
    let nullable = children.iter().all(|child| facts[child.index()].nullable);
    let minimum_consumption = MinimumConsumption::sequence(
        children
            .iter()
            .map(|child| facts[child.index()].minimum_consumption),
    );
    WorkFacts {
        nullable,
        minimum_consumption,
    }
}

fn sequence_slot_facts(slots: &[super::syntax::RecordSlotData], facts: &[WorkFacts]) -> WorkFacts {
    let nullable = slots
        .iter()
        .all(|slot| facts[slot.child().index()].nullable);
    let minimum_consumption = MinimumConsumption::sequence(
        slots
            .iter()
            .map(|slot| facts[slot.child().index()].minimum_consumption),
    );
    WorkFacts {
        nullable,
        minimum_consumption,
    }
}

fn choice_facts(children: &[NodeIx], facts: &[WorkFacts]) -> WorkFacts {
    WorkFacts {
        nullable: children.iter().any(|child| facts[child.index()].nullable),
        minimum_consumption: children
            .iter()
            .map(|child| facts[child.index()].minimum_consumption)
            .fold(MinimumConsumption::Empty, |minimum, candidate| {
                minimum.minimum_success(candidate)
            }),
    }
}

fn repeat_facts(child: WorkFacts, minimum: u32) -> WorkFacts {
    if minimum == 0 {
        return WorkFacts::some(0);
    }
    WorkFacts {
        nullable: child.nullable,
        minimum_consumption: child.minimum_consumption.repeated(minimum),
    }
}

struct AnalysisWorkBudget {
    used: usize,
    limit: usize,
}

impl AnalysisWorkBudget {
    const fn new(limit: usize) -> Self {
        Self { used: 0, limit }
    }

    const fn used(&self) -> usize {
        self.used
    }

    fn charge(&mut self, units: usize) -> Result<(), Diagnostic> {
        let Some(next) = self.used.checked_add(units) else {
            return Err(self.exhausted());
        };
        if next > self.limit {
            return Err(self.exhausted());
        }
        self.used = next;
        Ok(())
    }

    fn exhausted(&self) -> Diagnostic {
        Diagnostic::new(
            DiagnosticCode::ResourceLimit,
            0,
            0,
            format!(
                "analysis work exceeded the deterministic limit of {} traversal/evaluation units",
                self.limit
            ),
        )
    }
}

fn permutation_rank(order: &[usize]) -> Vec<usize> {
    let mut rank = vec![0; order.len()];
    for (position, &index) in order.iter().enumerate() {
        rank[index] = position;
    }
    rank
}

fn rule_node_ranges(
    program: &Program,
    work_budget: &mut AnalysisWorkBudget,
) -> Result<Vec<Range<usize>>, Diagnostic> {
    let mut ranges = Vec::with_capacity(program.rule_data_rows().len());
    let mut next_node = 0_usize;
    for rule in program.rule_data_rows() {
        work_budget.charge(1)?;
        let root = rule.root().index();
        if root < next_node || root >= program.node_data_rows().len() {
            return Err(Diagnostic::new(
                DiagnosticCode::InternalInvariant,
                0,
                0,
                "rule roots do not terminate contiguous postorder node ranges",
            ));
        }
        for index in next_node..=root {
            work_budget.charge(1)?;
            let node = &program.node_data_rows()[index];
            validate_postorder_children(node, work_budget)?;
        }
        ranges.push(next_node..root + 1);
        next_node = root + 1;
    }
    if next_node != program.node_data_rows().len() {
        return Err(Diagnostic::new(
            DiagnosticCode::InternalInvariant,
            0,
            0,
            "syntax contains nodes outside every rule's contiguous postorder range",
        ));
    }
    Ok(ranges)
}

fn validate_postorder_children(
    node: &super::syntax::NodeData,
    work_budget: &mut AnalysisWorkBudget,
) -> Result<(), Diagnostic> {
    let parent = node.id().index();
    let valid = match node.kind() {
        NodeKindData::Sequence(children) | NodeKindData::Choice(children) => {
            work_budget.charge(children.len())?;
            children.iter().all(|child| child.index() < parent)
        }
        NodeKindData::Repeat { child, .. } | NodeKindData::Transform { child, .. } => {
            work_budget.charge(1)?;
            child.index() < parent
        }
        NodeKindData::Record { slots, .. } => {
            work_budget.charge(slots.len())?;
            slots.iter().all(|slot| slot.child().index() < parent)
        }
        NodeKindData::Epsilon
        | NodeKindData::Literal(_)
        | NodeKindData::Regex(_)
        | NodeKindData::RuleRef(_) => true,
    };
    if !valid {
        let span = node.span();
        return Err(Diagnostic::new(
            DiagnosticCode::InternalInvariant,
            span.start() as usize,
            span.end() as usize,
            "syntax structural children are not in deterministic postorder",
        ));
    }
    Ok(())
}

fn component_nodes(
    scheduled_members: &[RuleIx],
    rule_node_ranges: &[Range<usize>],
    work_budget: &mut AnalysisWorkBudget,
) -> Result<Vec<NodeIx>, Diagnostic> {
    let capacity = scheduled_members.iter().try_fold(0_usize, |total, member| {
        total.checked_add(rule_node_ranges[member.index()].len())
    });
    let Some(capacity) = capacity else {
        return Err(work_budget.exhausted());
    };
    work_budget.charge(scheduled_members.len())?;
    work_budget.charge(capacity)?;

    let mut nodes = Vec::with_capacity(capacity);
    for &member in scheduled_members {
        nodes.extend(rule_node_ranges[member.index()].clone().map(|index| {
            NodeIx::checked(index).expect("validated rule range contains bounded node ordinals")
        }));
    }
    Ok(nodes)
}

#[allow(clippy::too_many_arguments)]
fn evaluate_component_pass(
    program: &Program,
    dependency: &DependencySolution,
    component: SccIx,
    members: &[RuleIx],
    scheduled_members: &[RuleIx],
    nodes: &[NodeIx],
    work: &mut [WorkFacts],
    rule_work: &mut [WorkFacts],
) -> bool {
    let previous_rules = members
        .iter()
        .map(|rule| rule_work[rule.index()])
        .collect::<Vec<_>>();

    for &node_ix in nodes {
        let node = program.node_data(node_ix);
        let derived = match node.kind() {
            NodeKindData::Epsilon | NodeKindData::Literal(_) | NodeKindData::Regex(_) => {
                work[node_ix.index()]
            }
            NodeKindData::RuleRef(rule) => {
                if dependency.component_of_local(*rule) == component {
                    let position = members
                        .binary_search(rule)
                        .expect("dependency components contain every same-component rule");
                    previous_rules[position]
                } else {
                    rule_work[rule.index()]
                }
            }
            NodeKindData::Sequence(children) => sequence_facts(children, work),
            NodeKindData::Choice(children) => choice_facts(children, work),
            NodeKindData::Repeat { child, minimum, .. } => {
                repeat_facts(work[child.index()], *minimum)
            }
            NodeKindData::Record { slots, .. } => sequence_slot_facts(slots, work),
            NodeKindData::Transform { child, .. } => work[child.index()],
        };
        work[node_ix.index()] = derived;
    }

    let mut changed = false;
    for &member in scheduled_members {
        let root = program.rule_data_rows()[member.index()].root();
        changed |= update_monotone(&mut rule_work[member.index()], work[root.index()]);
    }

    changed
}

fn minimum_overflow_origin<'a>(
    program: &'a Program,
    facts: &[WorkFacts],
) -> Option<&'a super::syntax::NodeData> {
    let overflowed = program.node_data_rows().iter().filter(|node| {
        facts[node.id().index()].minimum_consumption == MinimumConsumption::Overflow
    });
    let mut first_overflow = None;
    for node in overflowed {
        first_overflow.get_or_insert(node);
        let direct = match node.kind() {
            NodeKindData::Sequence(children) => sequence_directly_overflows(
                children
                    .iter()
                    .map(|child| facts[child.index()].minimum_consumption),
            ),
            NodeKindData::Record { slots, .. } => sequence_directly_overflows(
                slots
                    .iter()
                    .map(|slot| facts[slot.child().index()].minimum_consumption),
            ),
            NodeKindData::Repeat { child, minimum, .. } => {
                matches!(
                    facts[child.index()].minimum_consumption,
                    MinimumConsumption::Finite(value)
                        if value.checked_mul(u64::from(*minimum)).is_none()
                )
            }
            NodeKindData::Epsilon
            | NodeKindData::Literal(_)
            | NodeKindData::Regex(_)
            | NodeKindData::RuleRef(_)
            | NodeKindData::Choice(_)
            | NodeKindData::Transform { .. } => false,
        };
        if direct {
            return Some(node);
        }
    }
    first_overflow
}

fn sequence_directly_overflows(values: impl IntoIterator<Item = MinimumConsumption>) -> bool {
    let mut total = 0_u64;
    for value in values {
        let MinimumConsumption::Finite(value) = value else {
            return false;
        };
        let Some(next) = total.checked_add(value) else {
            return true;
        };
        total = next;
    }
    false
}

fn is_permutation(order: &[usize], count: usize) -> bool {
    if order.len() != count {
        return false;
    }
    let mut seen = vec![false; count];
    for &index in order {
        let Some(slot) = seen.get_mut(index) else {
            return false;
        };
        if *slot {
            return false;
        }
        *slot = true;
    }
    true
}

fn zero_consumption_calls(program: &Program, facts: &[WorkFacts]) -> Vec<Vec<RuleIx>> {
    let mut leading = vec![BTreeSet::new(); program.node_data_rows().len()];
    for node in program.node_data_rows() {
        let mut calls = BTreeSet::new();
        match node.kind() {
            NodeKindData::RuleRef(rule) => {
                calls.insert(*rule);
            }
            NodeKindData::Sequence(children) => {
                for child in children {
                    calls.extend(leading[child.index()].iter().copied());
                    if !facts[child.index()].nullable {
                        break;
                    }
                }
            }
            NodeKindData::Choice(children) => {
                for child in children {
                    calls.extend(leading[child.index()].iter().copied());
                }
            }
            NodeKindData::Repeat { child, maximum, .. } => {
                if *maximum != Some(0) {
                    calls.extend(leading[child.index()].iter().copied());
                }
            }
            NodeKindData::Record { slots, .. } => {
                for slot in slots {
                    let child = slot.child();
                    calls.extend(leading[child.index()].iter().copied());
                    if !facts[child.index()].nullable {
                        break;
                    }
                }
            }
            NodeKindData::Transform { child, .. } => {
                calls.extend(leading[child.index()].iter().copied());
            }
            NodeKindData::Epsilon | NodeKindData::Literal(_) | NodeKindData::Regex(_) => {}
        }
        leading[node.id().index()] = calls;
    }

    program
        .rule_data_rows()
        .iter()
        .map(|rule| leading[rule.root().index()].iter().copied().collect())
        .collect()
}

fn cyclic_components(edges: &[Vec<RuleIx>]) -> Vec<Vec<RuleIx>> {
    let count = edges.len();
    let mut visited = vec![false; count];
    let mut finish = Vec::with_capacity(count);

    for index in 0..count {
        if visited[index] {
            continue;
        }
        visited[index] = true;
        let root = RuleIx::checked(index).expect("rule ordinal was bounded");
        let mut stack = vec![(root, 0_usize)];
        while let Some((rule, next)) = stack.last_mut() {
            if *next < edges[rule.index()].len() {
                let target = edges[rule.index()][*next];
                *next += 1;
                if !visited[target.index()] {
                    visited[target.index()] = true;
                    stack.push((target, 0));
                }
            } else {
                finish.push(stack.pop().expect("stack is nonempty").0);
            }
        }
    }

    let mut reverse = vec![Vec::new(); count];
    for (source_index, targets) in edges.iter().enumerate() {
        let source = RuleIx::checked(source_index).expect("rule ordinal was bounded");
        for &target in targets {
            reverse[target.index()].push(source);
        }
    }
    for predecessors in &mut reverse {
        predecessors.sort_unstable();
        predecessors.dedup();
    }

    visited.fill(false);
    let mut components = Vec::new();
    for root in finish.into_iter().rev() {
        if visited[root.index()] {
            continue;
        }
        visited[root.index()] = true;
        let mut pending = vec![root];
        let mut component = Vec::new();
        while let Some(rule) = pending.pop() {
            component.push(rule);
            for &predecessor in reverse[rule.index()].iter().rev() {
                if !visited[predecessor.index()] {
                    visited[predecessor.index()] = true;
                    pending.push(predecessor);
                }
            }
        }
        component.sort_unstable();
        let recursive = component.len() > 1
            || edges[component[0].index()]
                .binary_search(&component[0])
                .is_ok();
        if recursive {
            components.push(component);
        }
    }
    components.sort();
    components
}

fn update_monotone(current: &mut WorkFacts, derived: WorkFacts) -> bool {
    let next = WorkFacts {
        nullable: current.nullable || derived.nullable,
        minimum_consumption: current
            .minimum_consumption
            .minimum_success(derived.minimum_consumption),
    };
    if *current == next {
        false
    } else {
        *current = next;
        true
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Write as _;
    use std::mem::size_of;

    use super::*;
    use crate::compiler::frontend::parse_source;
    use crate::compiler::source::ModuleInput;

    fn program(source: &str) -> (Program, DependencySolution) {
        let module = match parse_source(
            ModuleInput::new("analysis-order-law.bbnf", source.as_bytes()),
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
    fn fact_rows_retain_pre_branding_width() {
        assert_eq!(size_of::<NodeIx>(), 4);
        assert_eq!(size_of::<RuleIx>(), 4);
        assert!(size_of::<NodeFactsData>() <= 24);
        assert!(size_of::<RuleFactsData>() <= 24);
    }

    #[test]
    fn minimum_lattice_refines_overflow_to_a_smaller_finite_choice() {
        assert_eq!(
            MinimumConsumption::Overflow.minimum_success(MinimumConsumption::Finite(1)),
            MinimumConsumption::Finite(1)
        );
        assert_eq!(
            MinimumConsumption::sequence([
                MinimumConsumption::Finite(u64::MAX),
                MinimumConsumption::Finite(1),
            ]),
            MinimumConsumption::Overflow
        );
        assert_eq!(
            MinimumConsumption::Finite(u64::MAX).repeated(2),
            MinimumConsumption::Overflow
        );
        assert_eq!(
            MinimumConsumption::sequence(
                [MinimumConsumption::Overflow, MinimumConsumption::Empty,]
            ),
            MinimumConsumption::Empty
        );
    }

    #[test]
    fn fixed_point_constraint_permutations_are_identical() {
        let (program, dependency) = program(
            r#"
root = value;
value = array | "null";
array = "[", (value, (",", value)*)?, "]";
"#,
        );
        let options = CompileOptions::default();
        let canonical = match AnalysisFacts::derive(&program, &dependency, options) {
            StageResult::Accepted { value, .. } => value,
            StageResult::Rejected { .. } => panic!("recursive grammar must analyze"),
        };

        for seed in 1..=16 {
            let node_order = deterministic_permutation(program.node_data_rows().len(), seed);
            let rule_order = deterministic_permutation(
                program.rule_data_rows().len(),
                seed ^ 0xa076_1d64_78bd_642f,
            );
            let candidate = match AnalysisFacts::derive_with_orders(
                &program,
                &dependency,
                options,
                &node_order,
                &rule_order,
            ) {
                StageResult::Accepted { value, .. } => value,
                StageResult::Rejected { .. } => panic!("permuted analysis must converge"),
            };
            assert_eq!(candidate, canonical);
        }
    }

    #[test]
    fn overflow_diagnostic_is_identical_across_discovery_permutations() {
        let mut source = String::from("r0 = \"x\";\n");
        for index in 1..=65 {
            writeln!(source, "r{index} = r{}, r{};", index - 1, index - 1)
                .expect("writing to String is infallible");
        }
        source.push_str("root = r65;\n");
        let (program, dependency) = program(&source);
        let options = CompileOptions::default();
        let canonical = match AnalysisFacts::derive(&program, &dependency, options) {
            StageResult::Accepted { .. } => panic!("overflowing grammar must be rejected"),
            StageResult::Rejected { diagnostics } => diagnostics,
        };

        for seed in 1..=16 {
            let node_order = deterministic_permutation(program.node_data_rows().len(), seed);
            let rule_order = deterministic_permutation(
                program.rule_data_rows().len(),
                seed ^ 0xa076_1d64_78bd_642f,
            );
            let candidate = match AnalysisFacts::derive_with_orders(
                &program,
                &dependency,
                options,
                &node_order,
                &rule_order,
            ) {
                StageResult::Accepted { .. } => panic!("overflowing grammar must be rejected"),
                StageResult::Rejected { diagnostics } => diagnostics,
            };
            assert_eq!(candidate, canonical);
        }
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
