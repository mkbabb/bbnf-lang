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
            nodes: FactWireRows<'a, NodeFactsData>,
            rules: FactWireRows<'a, RuleFactsData>,
        }

        Wire {
            revision: self.0.revision,
            iterations: self.0.iterations,
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

        let mut iterations = 0;
        let converged = loop {
            if iterations >= options.analysis_iterations {
                break false;
            }
            iterations += 1;
            let previous_nodes = work.clone();
            let previous_rules = rule_work.clone();
            let mut next_nodes = previous_nodes.clone();
            let mut next_rules = previous_rules.clone();

            for &index in node_order {
                let node = &program.node_data_rows()[index];
                let derived = match node.kind() {
                    NodeKindData::Epsilon | NodeKindData::Literal(_) | NodeKindData::Regex(_) => {
                        previous_nodes[node.id().index()]
                    }
                    NodeKindData::RuleRef(rule) => previous_rules[rule.index()],
                    NodeKindData::Sequence(children) => sequence_facts(children, &previous_nodes),
                    NodeKindData::Choice(children) => choice_facts(children, &previous_nodes),
                    NodeKindData::Repeat { child, minimum, .. } => {
                        repeat_facts(previous_nodes[child.index()], *minimum)
                    }
                    NodeKindData::Record { slots, .. } => {
                        sequence_slot_facts(slots, &previous_nodes)
                    }
                    NodeKindData::Transform { child, .. } => previous_nodes[child.index()],
                };
                update_monotone(&mut next_nodes[node.id().index()], derived);
            }

            for &index in rule_order {
                let rule = &program.rule_data_rows()[index];
                let derived = previous_nodes[rule.root().index()];
                update_monotone(&mut next_rules[rule.id().index()], derived);
            }

            let changed = next_nodes != previous_nodes || next_rules != previous_rules;
            work = next_nodes;
            rule_work = next_rules;
            if !changed {
                break true;
            }
        };

        if !converged {
            return StageResult::rejected(Diagnostic::new(
                DiagnosticCode::ResourceLimit,
                0,
                0,
                format!(
                    "analysis fixed point exceeded the deterministic limit of {} iterations",
                    options.analysis_iterations
                ),
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
                .filter(|rule| rule_work[rule.index()].minimum_consumption.is_none())
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
                minimum_consumption: work[node.id().index()].minimum_consumption,
            })
            .collect();
        let rules = program
            .rule_data_rows()
            .iter()
            .map(|rule| RuleFactsData {
                rule: rule.id(),
                nullable: rule_work[rule.id().index()].nullable,
                minimum_consumption: rule_work[rule.id().index()].minimum_consumption,
            })
            .collect();

        StageResult::accepted(Self {
            revision,
            iterations,
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
struct WorkFacts {
    nullable: bool,
    minimum_consumption: Option<u64>,
}

impl WorkFacts {
    const fn some(minimum_consumption: u64) -> Self {
        Self {
            nullable: minimum_consumption == 0,
            minimum_consumption: Some(minimum_consumption),
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
    let minimum_consumption = children.iter().try_fold(0_u64, |total, child| {
        total.checked_add(facts[child.index()].minimum_consumption?)
    });
    WorkFacts {
        nullable,
        minimum_consumption,
    }
}

fn sequence_slot_facts(slots: &[super::syntax::RecordSlotData], facts: &[WorkFacts]) -> WorkFacts {
    let nullable = slots
        .iter()
        .all(|slot| facts[slot.child().index()].nullable);
    let minimum_consumption = slots.iter().try_fold(0_u64, |total, slot| {
        total.checked_add(facts[slot.child().index()].minimum_consumption?)
    });
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
            .filter_map(|child| facts[child.index()].minimum_consumption)
            .min(),
    }
}

fn repeat_facts(child: WorkFacts, minimum: u32) -> WorkFacts {
    if minimum == 0 {
        return WorkFacts::some(0);
    }
    WorkFacts {
        nullable: child.nullable,
        minimum_consumption: child
            .minimum_consumption
            .and_then(|minimum_child| minimum_child.checked_mul(u64::from(minimum))),
    }
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
        minimum_consumption: match (current.minimum_consumption, derived.minimum_consumption) {
            (Some(current), Some(derived)) => Some(current.min(derived)),
            (current @ Some(_), None) => current,
            (None, derived) => derived,
        },
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
