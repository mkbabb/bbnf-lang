use std::collections::{BTreeSet, VecDeque};
use std::ops::Range;

use super::regex_table::{DEAD_STATE, RegexTablePlan};
use serde::ser::SerializeSeq;
use serde::{Serialize, Serializer};

use super::budget::CompileOptions;
use super::dependency::DependencySolution;
use super::diagnostic::{Diagnostic, DiagnosticCode, DiagnosticSet, StageResult};
use super::pratt::PrattFixity;
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

    /// Proven lower bound on bytes consumed by a successful match. `None`
    /// denotes an empty language; difference does not attempt exact language
    /// subtraction.
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

    /// Proven lower bound on bytes consumed by a successful match. `None`
    /// denotes an empty language; difference does not attempt exact language
    /// subtraction.
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
        #[cfg(not(test))]
        {
            Self::derive_inner(program, dependency, options)
        }
        #[cfg(test)]
        {
            Self::derive_inner(program, dependency, options, None, None)
        }
    }

    #[cfg(test)]
    fn derive_with_orders(
        program: &Program,
        dependency: &DependencySolution,
        options: CompileOptions,
        node_order: &[usize],
        rule_order: &[usize],
    ) -> StageResult<Self> {
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
        Self::derive_inner(
            program,
            dependency,
            options,
            Some(node_order),
            Some(rule_order),
        )
    }

    fn derive_inner(
        program: &Program,
        dependency: &DependencySolution,
        options: CompileOptions,
        #[cfg(test)] node_order: Option<&[usize]>,
        #[cfg(test)] rule_order: Option<&[usize]>,
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
        let mut work_budget = AnalysisWorkBudget::new(options.analysis_work);
        let revision = program.revision();
        #[cfg(test)]
        let rule_rank = rule_order.map(permutation_rank);
        if let Err(diagnostic) =
            work_budget.charge(program.node_data_rows().len().saturating_mul(2))
        {
            return StageResult::rejected(diagnostic);
        }
        let mut work = vec![WorkFacts::default(); program.node_data_rows().len()];
        for position in 0..program.node_data_rows().len() {
            #[cfg(not(test))]
            let index = position;
            #[cfg(test)]
            let index = node_order.map_or(position, |order| order[position]);
            if let Err(diagnostic) = work_budget.charge(2) {
                return StageResult::rejected(diagnostic);
            }
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
                    work[node.id().index()] = match regex_facts(&plan, &mut work_budget) {
                        Ok(facts) => facts,
                        Err(diagnostic) => return StageResult::rejected(diagnostic),
                    };
                }
                NodeKindData::RuleRef(_)
                | NodeKindData::Sequence(_)
                | NodeKindData::Choice(_)
                | NodeKindData::Repeat { .. }
                | NodeKindData::Record { .. }
                | NodeKindData::Transform { .. }
                | NodeKindData::Projection { .. }
                | NodeKindData::Difference { .. }
                | NodeKindData::Trim { .. }
                | NodeKindData::Pratt { .. } => {}
            }
        }
        if let Err(diagnostic) =
            work_budget.charge(program.rule_data_rows().len().saturating_mul(2))
        {
            return StageResult::rejected(diagnostic);
        }
        let mut rule_work = vec![WorkFacts::default(); program.rule_data_rows().len()];
        if let Err(diagnostic) =
            work_budget.charge(program.rule_data_rows().len().saturating_mul(2))
        {
            return StageResult::rejected(diagnostic);
        }
        let mut component_positions = vec![usize::MAX; program.rule_data_rows().len()];

        let mut iterations = 0_usize;
        let nodes_by_owner = match validate_program_postorder(program, &mut work_budget) {
            Ok(nodes_by_owner) => nodes_by_owner,
            Err(diagnostic) => return StageResult::rejected(diagnostic),
        };
        for &component in dependency.dependency_order_local() {
            if let Err(diagnostic) = work_budget.charge(1) {
                return StageResult::rejected(diagnostic);
            }
            let members = dependency.component_local(component);
            #[cfg(test)]
            let scheduled_storage = rule_rank.as_ref().map(|rank| {
                let mut scheduled = members.to_vec();
                scheduled.sort_by_key(|rule| rank[rule.index()]);
                scheduled
            });
            #[cfg(test)]
            let scheduled_members = scheduled_storage.as_deref().unwrap_or(members);
            #[cfg(not(test))]
            let scheduled_members = members;
            if let Err(diagnostic) = work_budget.charge(members.len().saturating_mul(2)) {
                return StageResult::rejected(diagnostic);
            }
            for (position, &rule) in members.iter().enumerate() {
                component_positions[rule.index()] = position;
            }
            let nodes = match component_nodes(&nodes_by_owner, scheduled_members, &mut work_budget)
            {
                Ok(nodes) => nodes,
                Err(diagnostic) => return StageResult::rejected(diagnostic),
            };
            if let Err(diagnostic) = work_budget.charge(nodes.len()) {
                return StageResult::rejected(diagnostic);
            }
            let pass_units = match component_pass_units(program, &nodes, members.len()) {
                Some(units) => units,
                None => return StageResult::rejected(work_budget.exhausted()),
            };
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
                    scheduled_members,
                    &nodes,
                    &component_positions,
                    &mut work,
                    &mut rule_work,
                );
                if !recursive || !changed {
                    break;
                }
            }
        }

        let overflow_origin = match minimum_overflow_origin(program, &work, &mut work_budget) {
            Ok(origin) => origin,
            Err(diagnostic) => return StageResult::rejected(diagnostic),
        };
        if let Some(node) = overflow_origin {
            let span = node.span();
            return StageResult::rejected(Diagnostic::new(
                DiagnosticCode::MinimumConsumptionOverflow,
                span.start() as usize,
                span.end() as usize,
                "the analyzed minimum successful consumption exceeds u64::MAX bytes",
            ));
        }

        let diagnostic_capacity = match program
            .node_data_rows()
            .len()
            .checked_add(program.pratt_table_rows().len())
            .and_then(|count| count.checked_add(program.pratt_operator_rows().len()))
            .and_then(|count| {
                program
                    .rule_data_rows()
                    .len()
                    .checked_mul(2)
                    .and_then(|rules| count.checked_add(rules))
            }) {
            Some(capacity) => capacity,
            None => return StageResult::rejected(work_budget.exhausted()),
        };
        if let Err(diagnostic) = work_budget.charge(diagnostic_capacity) {
            return StageResult::rejected(diagnostic);
        }
        let mut semantic_diagnostics = Vec::with_capacity(diagnostic_capacity);
        for table in program.pratt_table_rows() {
            if let Err(diagnostic) = work_budget.charge(1) {
                return StageResult::rejected(diagnostic);
            }
            let atom = program.rule_data_rows()[table.atom_rule().local().index()].root();
            let atom_facts = rule_work[table.atom_rule().local().index()];
            if atom_facts.nullable
                || !matches!(
                    atom_facts.minimum_consumption,
                    MinimumConsumption::Finite(value) if value > 0
                )
            {
                let span = program.node_data(atom).span();
                if let Err(diagnostic) = work_budget.charge(1) {
                    return StageResult::rejected(diagnostic);
                }
                semantic_diagnostics.push(Diagnostic::new(
                    DiagnosticCode::InvalidPrattTable,
                    span.start() as usize,
                    span.end() as usize,
                    format!(
                        "Pratt atom rule `{}` must have a nonempty, consuming language",
                        program.rule_data_rows()[table.atom_rule().local().index()].name()
                    ),
                ));
            }
            for operator in table.operators() {
                if let Err(diagnostic) = work_budget.charge(1) {
                    return StageResult::rejected(diagnostic);
                }
                let matcher = program.pratt_operator_rows()[operator.index()].matcher_rule();
                let matcher_facts = rule_work[matcher.local().index()];
                if matcher_facts.nullable
                    || !matches!(
                        matcher_facts.minimum_consumption,
                        MinimumConsumption::Finite(value) if value > 0
                    )
                {
                    let matcher_rule = &program.rule_data_rows()[matcher.local().index()];
                    let span = program.node_data(matcher_rule.root()).span();
                    if let Err(diagnostic) = work_budget.charge(1) {
                        return StageResult::rejected(diagnostic);
                    }
                    semantic_diagnostics.push(Diagnostic::new(
                        DiagnosticCode::InvalidPrattTable,
                        span.start() as usize,
                        span.end() as usize,
                        format!(
                            "Pratt matcher rule `{}` must have a nonempty, consuming language",
                            matcher_rule.name()
                        ),
                    ));
                }
            }
        }
        for node in program.node_data_rows() {
            if let Err(diagnostic) = work_budget.charge(1) {
                return StageResult::rejected(diagnostic);
            }
            let NodeKindData::Repeat { child, .. } = node.kind() else {
                continue;
            };
            if work[child.index()].nullable {
                let span = node.span();
                if let Err(diagnostic) = work_budget.charge(1) {
                    return StageResult::rejected(diagnostic);
                }
                semantic_diagnostics.push(Diagnostic::new(
                    DiagnosticCode::NullableRepeat,
                    span.start() as usize,
                    span.end() as usize,
                    "repetition of a nullable expression is rejected because parse-that requires every repeated match to advance input",
                ));
            }
        }

        let zero_call_edges = match zero_consumption_calls(program, &work, &mut work_budget) {
            Ok(edges) => edges,
            Err(diagnostic) => return StageResult::rejected(diagnostic),
        };
        let zero_cycles = match cyclic_components(&zero_call_edges, &mut work_budget) {
            Ok(cycles) => cycles,
            Err(diagnostic) => return StageResult::rejected(diagnostic),
        };
        if let Err(diagnostic) = work_budget.charge(zero_cycles.len()) {
            return StageResult::rejected(diagnostic);
        }
        let cycle_members = match zero_cycles
            .iter()
            .try_fold(0_usize, |count, cycle| count.checked_add(cycle.len()))
        {
            Some(count) => count,
            None => return StageResult::rejected(work_budget.exhausted()),
        };
        if let Err(diagnostic) = work_budget.charge(
            program
                .rule_data_rows()
                .len()
                .checked_mul(2)
                .and_then(|units| units.checked_add(cycle_members))
                .unwrap_or(usize::MAX),
        ) {
            return StageResult::rejected(diagnostic);
        }
        let mut zero_cycle_rules = vec![false; program.rule_data_rows().len()];
        for &rule in zero_cycles.iter().flatten() {
            zero_cycle_rules[rule.index()] = true;
        }

        for component_index in 0..dependency.component_count() {
            if let Err(diagnostic) = work_budget.charge(1) {
                return StageResult::rejected(diagnostic);
            }
            let component = SccIx::checked(component_index).expect("component ordinal was bounded");
            if dependency.is_recursive_local(component) != Some(true) {
                continue;
            }
            let members = dependency.component_local(component);
            if let Err(diagnostic) = work_budget.charge(members.len().saturating_mul(2)) {
                return StageResult::rejected(diagnostic);
            }
            let empty_members = members
                .iter()
                .copied()
                .filter(|rule| rule_work[rule.index()].minimum_consumption.is_empty())
                .collect::<Vec<_>>();
            if let Err(diagnostic) = work_budget.charge(members.len().saturating_mul(2)) {
                return StageResult::rejected(diagnostic);
            }
            if !empty_members.is_empty()
                && !members.iter().any(|rule| zero_cycle_rules[rule.index()])
            {
                let origin = empty_members[0];
                let rule = &program.rule_data_rows()[origin.index()];
                let span = program.node_data(rule.root()).span();
                let names = empty_members
                    .iter()
                    .map(|rule| program.rule_data_rows()[rule.index()].name())
                    .collect::<Vec<_>>()
                    .join(", ");
                if let Err(diagnostic) = work_budget.charge(1) {
                    return StageResult::rejected(diagnostic);
                }
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
            if let Err(diagnostic) = work_budget.charge(cycle.len().saturating_mul(2)) {
                return StageResult::rejected(diagnostic);
            }
            let origin = cycle[0];
            let rule = &program.rule_data_rows()[origin.index()];
            let span = program.node_data(rule.root()).span();
            let names = cycle
                .iter()
                .map(|rule| program.rule_data_rows()[rule.index()].name())
                .collect::<Vec<_>>()
                .join(" -> ");
            if let Err(diagnostic) = work_budget.charge(1) {
                return StageResult::rejected(diagnostic);
            }
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
            if let Err(diagnostic) = work_budget.charge(
                sort_work_bound(semantic_diagnostics.len())
                    .saturating_add(semantic_diagnostics.len()),
            ) {
                return StageResult::rejected(diagnostic);
            }
            return StageResult::rejected_set(DiagnosticSet::from_vec(semantic_diagnostics));
        }

        if let Err(diagnostic) = work_budget.charge(
            program
                .node_data_rows()
                .len()
                .checked_add(program.rule_data_rows().len())
                .and_then(|rows| rows.checked_mul(3))
                .unwrap_or(usize::MAX),
        ) {
            return StageResult::rejected(diagnostic);
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

    /// Deterministic conservative logical-work bound for analysis-owned
    /// schedules, carrier allocations and fills, traversal, fixed-point
    /// evaluation, ordered-set/search work, semantic aggregation, zero-call
    /// graphs, and output materialization. Compilation rejects before this
    /// exceeds `analysis_work`.
    ///
    /// Regex automaton construction is governed independently by
    /// [`RegexBudget`](super::budget::RegexBudget). Diagnostic string storage
    /// is bounded by the source/rule/node envelopes; this count covers
    /// diagnostic-row aggregation and canonical ordering, not allocator or
    /// formatting implementation details.
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

fn regex_facts(
    plan: &RegexTablePlan,
    work_budget: &mut AnalysisWorkBudget,
) -> Result<WorkFacts, Diagnostic> {
    work_budget.charge(1)?;
    let start = plan.start_state() as usize;
    if plan.accepting()[start] {
        return Ok(WorkFacts::some(0));
    }

    let classes = plan.class_count() as usize;
    let states = plan.state_count() as usize;
    work_budget.charge(
        states
            .checked_mul(5)
            .ok_or_else(|| work_budget.exhausted())?,
    )?;
    let mut distance = vec![None; states];
    distance[start] = Some(0_u64);
    let mut pending = VecDeque::with_capacity(states);
    pending.push_back(start);
    while let Some(state) = pending.pop_front() {
        work_budget.charge(
            classes
                .checked_add(1)
                .ok_or_else(|| work_budget.exhausted())?,
        )?;
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
                return Ok(WorkFacts::some(next_distance));
            }
            distance[target] = Some(next_distance);
            pending.push_back(target);
        }
    }
    Ok(WorkFacts::default())
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

fn difference_facts(include: WorkFacts, exclude: WorkFacts) -> WorkFacts {
    if exclude.nullable {
        WorkFacts::default()
    } else {
        WorkFacts {
            nullable: include.nullable,
            minimum_consumption: include.minimum_consumption,
        }
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

fn sort_work_bound(count: usize) -> usize {
    if count < 2 {
        return 0;
    }
    let passes = usize::BITS as usize - (count - 1).leading_zeros() as usize;
    count.saturating_mul(passes)
}

fn search_work_bound(count: usize) -> usize {
    let search_space = count.saturating_add(1);
    let levels = usize::BITS as usize - search_space.leading_zeros() as usize;
    levels.saturating_add(1)
}

fn charge_ordered_set_operation(
    work_budget: &mut AnalysisWorkBudget,
    count: usize,
) -> Result<(), Diagnostic> {
    work_budget.charge(search_work_bound(count))
}

#[cfg(test)]
fn permutation_rank(order: &[usize]) -> Vec<usize> {
    let mut rank = vec![0; order.len()];
    for (position, &index) in order.iter().enumerate() {
        rank[index] = position;
    }
    rank
}

fn validate_program_postorder(
    program: &Program,
    work_budget: &mut AnalysisWorkBudget,
) -> Result<Vec<Vec<NodeIx>>, Diagnostic> {
    if program.node_owner_rows().len() != program.node_data_rows().len() {
        return Err(Diagnostic::new(
            DiagnosticCode::InternalInvariant,
            0,
            0,
            "syntax node ownership does not cover the complete node arena",
        ));
    }
    let rule_count = program.rule_data_rows().len();
    let node_count = program.node_data_rows().len();
    work_budget.charge(rule_count.saturating_mul(2))?;
    let mut owner_counts = vec![0_usize; rule_count];
    for &owner in program.node_owner_rows() {
        work_budget.charge(3)?;
        let Some(count) = owner_counts.get_mut(owner.index()) else {
            return Err(Diagnostic::new(
                DiagnosticCode::InternalInvariant,
                0,
                0,
                "syntax node identities or owners are not stable local ordinals",
            ));
        };
        *count = count
            .checked_add(1)
            .ok_or_else(|| work_budget.exhausted())?;
    }
    work_budget.charge(
        rule_count
            .checked_mul(2)
            .and_then(|rules| rules.checked_add(node_count))
            .ok_or_else(|| work_budget.exhausted())?,
    )?;
    let mut nodes_by_owner = owner_counts
        .into_iter()
        .map(Vec::with_capacity)
        .collect::<Vec<_>>();
    for (index, node) in program.node_data_rows().iter().enumerate() {
        work_budget.charge(3)?;
        let owner = program.node_owner_rows()[index];
        if node.id().index() != index || owner.index() >= nodes_by_owner.len() {
            return Err(Diagnostic::new(
                DiagnosticCode::InternalInvariant,
                0,
                0,
                "syntax node identities or owners are not stable local ordinals",
            ));
        }
        nodes_by_owner[owner.index()].push(node.id());
        validate_postorder_children(node, work_budget)?;
    }
    Ok(nodes_by_owner)
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
        NodeKindData::Projection { left, right, .. }
        | NodeKindData::Difference {
            include: left,
            exclude: right,
        } => {
            work_budget.charge(2)?;
            left.index() < parent && right.index() < parent
        }
        NodeKindData::Repeat { child, .. }
        | NodeKindData::Transform { child, .. }
        | NodeKindData::Trim { child }
        | NodeKindData::Pratt { atom: child, .. } => {
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
    nodes_by_owner: &[Vec<NodeIx>],
    scheduled_members: &[RuleIx],
    work_budget: &mut AnalysisWorkBudget,
) -> Result<Vec<NodeIx>, Diagnostic> {
    let mut node_count = 0_usize;
    for &member in scheduled_members {
        work_budget.charge(1)?;
        node_count = node_count
            .checked_add(nodes_by_owner[member.index()].len())
            .ok_or_else(|| work_budget.exhausted())?;
    }
    work_budget.charge(node_count.saturating_mul(2))?;
    let mut nodes = Vec::with_capacity(node_count);
    for &member in scheduled_members {
        work_budget.charge(1)?;
        let owned = &nodes_by_owner[member.index()];
        work_budget.charge(owned.len())?;
        nodes.extend_from_slice(owned);
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
    component_positions: &[usize],
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
                    previous_rules[component_positions[rule.index()]]
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
            NodeKindData::Projection { left, right, .. } => sequence_facts(&[*left, *right], work),
            NodeKindData::Difference { include, exclude } => {
                difference_facts(work[include.index()], work[exclude.index()])
            }
            NodeKindData::Trim { child } => work[child.index()],
            NodeKindData::Pratt { atom, .. } => work[atom.index()],
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

fn component_pass_units(program: &Program, nodes: &[NodeIx], member_count: usize) -> Option<usize> {
    let mut units = member_count.checked_mul(5)?;
    for &node in nodes {
        units = units.checked_add(1)?;
        let children = match program.node_data(node).kind() {
            NodeKindData::Sequence(children) | NodeKindData::Choice(children) => {
                children.len().checked_mul(2)?
            }
            NodeKindData::Record { slots, .. } => slots.len().checked_mul(2)?,
            NodeKindData::Projection { .. } => 4,
            NodeKindData::Difference { .. } => 2,
            NodeKindData::RuleRef(_)
            | NodeKindData::Repeat { .. }
            | NodeKindData::Transform { .. }
            | NodeKindData::Trim { .. }
            | NodeKindData::Pratt { .. } => 1,
            NodeKindData::Epsilon | NodeKindData::Literal(_) | NodeKindData::Regex(_) => 0,
        };
        units = units.checked_add(children)?;
    }
    Some(units)
}

fn minimum_overflow_origin<'a>(
    program: &'a Program,
    facts: &[WorkFacts],
    work_budget: &mut AnalysisWorkBudget,
) -> Result<Option<&'a super::syntax::NodeData>, Diagnostic> {
    let mut fallback_overflow = None;
    for node in program.node_data_rows() {
        work_budget.charge(1)?;
        if facts[node.id().index()].minimum_consumption != MinimumConsumption::Overflow {
            continue;
        }
        fallback_overflow.get_or_insert(node);
        let direct = match node.kind() {
            NodeKindData::Sequence(children) => {
                work_budget.charge(children.len())?;
                sequence_directly_overflows(
                    children
                        .iter()
                        .map(|child| facts[child.index()].minimum_consumption),
                )
            }
            NodeKindData::Record { slots, .. } => {
                work_budget.charge(slots.len())?;
                sequence_directly_overflows(
                    slots
                        .iter()
                        .map(|slot| facts[slot.child().index()].minimum_consumption),
                )
            }
            NodeKindData::Repeat { child, minimum, .. } => {
                work_budget.charge(1)?;
                matches!(
                    facts[child.index()].minimum_consumption,
                    MinimumConsumption::Finite(value)
                        if value.checked_mul(u64::from(*minimum)).is_none()
                )
            }
            NodeKindData::Projection { left, right, .. } => {
                work_budget.charge(2)?;
                sequence_directly_overflows(
                    [*left, *right]
                        .into_iter()
                        .map(|child| facts[child.index()].minimum_consumption),
                )
            }
            NodeKindData::Epsilon
            | NodeKindData::Literal(_)
            | NodeKindData::Regex(_)
            | NodeKindData::RuleRef(_)
            | NodeKindData::Choice(_)
            | NodeKindData::Transform { .. }
            | NodeKindData::Difference { .. }
            | NodeKindData::Trim { .. }
            | NodeKindData::Pratt { .. } => false,
        };
        if direct {
            return Ok(Some(node));
        }
    }
    Ok(fallback_overflow)
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

#[cfg(test)]
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

fn zero_consumption_calls(
    program: &Program,
    facts: &[WorkFacts],
    work_budget: &mut AnalysisWorkBudget,
) -> Result<Vec<Vec<RuleIx>>, Diagnostic> {
    fn extend_calls(
        calls: &mut BTreeSet<RuleIx>,
        inherited: &BTreeSet<RuleIx>,
        work_budget: &mut AnalysisWorkBudget,
    ) -> Result<(), Diagnostic> {
        for &rule in inherited {
            work_budget.charge(1)?;
            charge_ordered_set_operation(work_budget, calls.len())?;
            calls.insert(rule);
        }
        Ok(())
    }

    work_budget.charge(program.node_data_rows().len().saturating_mul(2))?;
    let mut leading = vec![BTreeSet::new(); program.node_data_rows().len()];
    for node in program.node_data_rows() {
        work_budget.charge(2)?;
        let mut calls = BTreeSet::new();
        match node.kind() {
            NodeKindData::RuleRef(rule) => {
                work_budget.charge(1)?;
                charge_ordered_set_operation(work_budget, calls.len())?;
                calls.insert(*rule);
            }
            NodeKindData::Sequence(children) => {
                for child in children {
                    work_budget.charge(1)?;
                    extend_calls(&mut calls, &leading[child.index()], work_budget)?;
                    if !facts[child.index()].nullable {
                        break;
                    }
                }
            }
            NodeKindData::Choice(children) => {
                for child in children {
                    work_budget.charge(1)?;
                    extend_calls(&mut calls, &leading[child.index()], work_budget)?;
                }
            }
            NodeKindData::Repeat { child, maximum, .. } => {
                work_budget.charge(1)?;
                if *maximum != Some(0) {
                    extend_calls(&mut calls, &leading[child.index()], work_budget)?;
                }
            }
            NodeKindData::Record { slots, .. } => {
                for slot in slots {
                    work_budget.charge(1)?;
                    let child = slot.child();
                    extend_calls(&mut calls, &leading[child.index()], work_budget)?;
                    if !facts[child.index()].nullable {
                        break;
                    }
                }
            }
            NodeKindData::Transform { child, .. } => {
                work_budget.charge(1)?;
                extend_calls(&mut calls, &leading[child.index()], work_budget)?;
            }
            NodeKindData::Projection { left, right, .. } => {
                work_budget.charge(1)?;
                extend_calls(&mut calls, &leading[left.index()], work_budget)?;
                if facts[left.index()].nullable {
                    work_budget.charge(1)?;
                    extend_calls(&mut calls, &leading[right.index()], work_budget)?;
                }
            }
            NodeKindData::Difference { include, exclude } => {
                work_budget.charge(1)?;
                extend_calls(&mut calls, &leading[exclude.index()], work_budget)?;
                if !facts[exclude.index()].nullable {
                    work_budget.charge(1)?;
                    extend_calls(&mut calls, &leading[include.index()], work_budget)?;
                }
            }
            NodeKindData::Trim { child } => {
                work_budget.charge(1)?;
                extend_calls(&mut calls, &leading[child.index()], work_budget)?;
            }
            NodeKindData::Pratt { atom, table } => {
                work_budget.charge(2)?;
                extend_calls(&mut calls, &leading[atom.index()], work_budget)?;
                let table = &program.pratt_table_rows()[table.index()];
                for operator in table.operators() {
                    work_budget.charge(1)?;
                    let operator = &program.pratt_operator_rows()[operator.index()];
                    if operator.fixity() == PrattFixity::Prefix {
                        work_budget.charge(1)?;
                        charge_ordered_set_operation(work_budget, calls.len())?;
                        calls.insert(operator.matcher_rule().local());
                    }
                }
            }
            NodeKindData::Epsilon | NodeKindData::Literal(_) | NodeKindData::Regex(_) => {}
        }
        leading[node.id().index()] = calls;
    }

    work_budget.charge(program.rule_data_rows().len().saturating_mul(2))?;
    let mut edges = Vec::with_capacity(program.rule_data_rows().len());
    for rule in program.rule_data_rows() {
        work_budget.charge(1)?;
        let calls = &leading[rule.root().index()];
        work_budget.charge(calls.len().saturating_mul(2))?;
        edges.push(calls.iter().copied().collect());
    }
    Ok(edges)
}

fn cyclic_components(
    edges: &[Vec<RuleIx>],
    work_budget: &mut AnalysisWorkBudget,
) -> Result<Vec<Vec<RuleIx>>, Diagnostic> {
    let count = edges.len();
    let mut edge_count = 0usize;
    for targets in edges {
        work_budget.charge(1)?;
        edge_count = edge_count
            .checked_add(targets.len())
            .ok_or_else(|| work_budget.exhausted())?;
    }
    work_budget.charge(
        count
            .checked_mul(12)
            .and_then(|nodes| {
                edge_count
                    .checked_mul(2)
                    .and_then(|edge_units| nodes.checked_add(edge_units))
            })
            .ok_or_else(|| work_budget.exhausted())?,
    )?;
    let mut visited = vec![false; count];
    let mut finish = Vec::with_capacity(count);
    let mut stack = Vec::with_capacity(count);

    for index in 0..count {
        work_budget.charge(1)?;
        if visited[index] {
            continue;
        }
        visited[index] = true;
        let root = RuleIx::checked(index).expect("rule ordinal was bounded");
        stack.clear();
        stack.push((root, 0_usize));
        while let Some((rule, next)) = stack.last_mut() {
            work_budget.charge(1)?;
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
        work_budget.charge(1)?;
        let source = RuleIx::checked(source_index).expect("rule ordinal was bounded");
        for &target in targets {
            work_budget.charge(1)?;
            reverse[target.index()].push(source);
        }
    }
    for predecessors in &mut reverse {
        work_budget.charge(
            sort_work_bound(predecessors.len())
                .checked_add(predecessors.len())
                .ok_or_else(|| work_budget.exhausted())?,
        )?;
        predecessors.sort_unstable();
        predecessors.dedup();
    }

    work_budget.charge(count)?;
    visited.fill(false);
    let mut components = Vec::with_capacity(count);
    let mut pending = Vec::with_capacity(count);
    for root in finish.into_iter().rev() {
        work_budget.charge(1)?;
        if visited[root.index()] {
            continue;
        }
        visited[root.index()] = true;
        pending.clear();
        pending.push(root);
        let mut component = Vec::new();
        while let Some(rule) = pending.pop() {
            work_budget.charge(1)?;
            component.push(rule);
            for &predecessor in reverse[rule.index()].iter().rev() {
                work_budget.charge(1)?;
                if !visited[predecessor.index()] {
                    visited[predecessor.index()] = true;
                    pending.push(predecessor);
                }
            }
        }
        work_budget.charge(sort_work_bound(component.len()))?;
        component.sort_unstable();
        work_budget.charge(search_work_bound(edges[component[0].index()].len()))?;
        let recursive = component.len() > 1
            || edges[component[0].index()]
                .binary_search(&component[0])
                .is_ok();
        if recursive {
            components.push(component);
        }
    }
    work_budget.charge(
        sort_work_bound(components.len())
            .checked_mul(count.max(1))
            .ok_or_else(|| work_budget.exhausted())?,
    )?;
    components.sort();
    Ok(components)
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
    use crate::compiler::instance::UNIT_INSTANCE;
    use crate::compiler::{GrammarInstanceInput, compile_instance};

    fn with_analysis_fixture(source: &str, inspect: impl FnOnce(&Program, &DependencySolution)) {
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
        with_analysis_fixture(
            r#"
root = value;
value = array | "null";
array = "[", (value, (",", value)*)?, "]";
"#,
            |program, dependency| {
                let options = CompileOptions::default();
                let canonical = match AnalysisFacts::derive(program, dependency, options) {
                    StageResult::Accepted { value, .. } => value,
                    StageResult::Rejected { .. } => panic!("recursive grammar must analyze"),
                };

                for seed in 1..=16 {
                    let node_order =
                        deterministic_permutation(program.node_data_rows().len(), seed);
                    let rule_order = deterministic_permutation(
                        program.rule_data_rows().len(),
                        seed ^ 0xa076_1d64_78bd_642f,
                    );
                    let candidate = match AnalysisFacts::derive_with_orders(
                        program,
                        dependency,
                        options,
                        &node_order,
                        &rule_order,
                    ) {
                        StageResult::Accepted { value, .. } => value,
                        StageResult::Rejected { .. } => panic!("permuted analysis must converge"),
                    };
                    assert_eq!(candidate, canonical);
                }
            },
        );
    }

    #[test]
    fn one_under_budget_diagnostic_is_identical_across_discovery_permutations() {
        let mut source = String::from("r0 = \"x\";\n");
        for index in 1..=65 {
            writeln!(source, "r{index} = r{};", index - 1)
                .expect("writing to String is infallible");
        }
        source.push_str("root = r65;\n");
        with_analysis_fixture(&source, |program, dependency| {
            let accepted =
                match AnalysisFacts::derive(program, dependency, CompileOptions::default()) {
                    StageResult::Accepted { value, .. } => value,
                    StageResult::Rejected { .. } => panic!("linear grammar must analyze"),
                };
            let options = CompileOptions {
                analysis_work: accepted
                    .work_units()
                    .checked_sub(1)
                    .expect("accepted analysis performs work"),
                ..CompileOptions::default()
            };
            let node_order = (0..program.node_data_rows().len()).collect::<Vec<_>>();
            let rule_order = (0..program.rule_data_rows().len()).collect::<Vec<_>>();
            let canonical = match AnalysisFacts::derive_with_orders(
                program,
                dependency,
                options,
                &node_order,
                &rule_order,
            ) {
                StageResult::Accepted { .. } => panic!("one-under analysis must be rejected"),
                StageResult::Rejected { diagnostics } => diagnostics,
            };

            for seed in 1..=16 {
                let node_order = deterministic_permutation(program.node_data_rows().len(), seed);
                let rule_order = deterministic_permutation(
                    program.rule_data_rows().len(),
                    seed ^ 0xa076_1d64_78bd_642f,
                );
                let candidate = match AnalysisFacts::derive_with_orders(
                    program,
                    dependency,
                    options,
                    &node_order,
                    &rule_order,
                ) {
                    StageResult::Accepted { .. } => panic!("one-under analysis must be rejected"),
                    StageResult::Rejected { diagnostics } => diagnostics,
                };
                assert_eq!(candidate, canonical);
            }
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
