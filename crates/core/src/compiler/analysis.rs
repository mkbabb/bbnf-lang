use std::collections::{BTreeSet, VecDeque};

use bbnf_regex::{DEAD_STATE, RegexTablePlan};
use serde::Serialize;

use super::budget::CompileOptions;
use super::dependency::DependencySolution;
use super::diagnostic::{Diagnostic, DiagnosticCode, DiagnosticSet, StageResult};
use super::source::Revision;
use super::syntax::{NodeId, NodeKind, Program, RuleId};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
pub struct NodeFacts {
    revision: Revision,
    node: NodeId,
    nullable: bool,
    minimum_consumption: Option<u64>,
}

impl NodeFacts {
    pub const fn revision(self) -> Revision {
        self.revision
    }

    pub const fn node(self) -> NodeId {
        self.node
    }

    pub const fn nullable(self) -> bool {
        self.nullable
    }

    /// Least number of bytes consumed by a successful match. `None` denotes
    /// an empty language rather than an unknown result.
    pub const fn minimum_consumption(self) -> Option<u64> {
        self.minimum_consumption
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
pub struct RuleFacts {
    revision: Revision,
    rule: RuleId,
    nullable: bool,
    minimum_consumption: Option<u64>,
}

impl RuleFacts {
    pub const fn revision(self) -> Revision {
        self.revision
    }

    pub const fn rule(self) -> RuleId {
        self.rule
    }

    pub const fn nullable(self) -> bool {
        self.nullable
    }

    pub const fn minimum_consumption(self) -> Option<u64> {
        self.minimum_consumption
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct AnalysisFacts {
    revision: Revision,
    iterations: usize,
    nodes: Vec<NodeFacts>,
    rules: Vec<RuleFacts>,
}

impl AnalysisFacts {
    pub(crate) fn derive(
        program: &Program,
        dependency: &DependencySolution,
        options: CompileOptions,
    ) -> StageResult<Self> {
        Self::derive_with_orders(
            program,
            dependency,
            options,
            &(0..program.nodes().len()).collect::<Vec<_>>(),
            &(0..program.rules().len()).collect::<Vec<_>>(),
        )
    }

    /// Evaluate constraints in arbitrary orders but commit every fixed-point
    /// round synchronously. This makes both the result and iteration receipt
    /// independent of traversal, parallel discovery, or hash iteration.
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
        if !is_permutation(node_order, program.nodes().len())
            || !is_permutation(rule_order, program.rules().len())
        {
            return StageResult::rejected(Diagnostic::new(
                DiagnosticCode::InternalInvariant,
                0,
                0,
                "analysis schedule is not a complete identity permutation",
            ));
        }

        let revision = program.revision();
        let mut work = vec![WorkFacts::default(); program.nodes().len()];
        for &index in node_order {
            let node = &program.nodes()[index];
            match node.kind() {
                NodeKind::Epsilon => {
                    work[node.id().index()] = WorkFacts::some(0);
                }
                NodeKind::Literal(bytes) => {
                    work[node.id().index()] = WorkFacts::some(bytes.len() as u64);
                }
                NodeKind::Regex(pattern) => {
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
                NodeKind::RuleRef(_)
                | NodeKind::Sequence(_)
                | NodeKind::Choice(_)
                | NodeKind::Repeat { .. }
                | NodeKind::Record { .. }
                | NodeKind::Transform { .. } => {}
            }
        }
        let mut rule_work = vec![WorkFacts::default(); program.rules().len()];

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
                let node = &program.nodes()[index];
                let derived = match node.kind() {
                    NodeKind::Epsilon | NodeKind::Literal(_) | NodeKind::Regex(_) => {
                        previous_nodes[node.id().index()]
                    }
                    NodeKind::RuleRef(rule) => previous_rules[rule.index()],
                    NodeKind::Sequence(children) => sequence_facts(children, &previous_nodes),
                    NodeKind::Choice(children) => choice_facts(children, &previous_nodes),
                    NodeKind::Repeat { child, minimum, .. } => {
                        repeat_facts(previous_nodes[child.index()], *minimum)
                    }
                    NodeKind::Record { slots, .. } => sequence_facts(
                        &slots.iter().map(|slot| slot.child()).collect::<Vec<_>>(),
                        &previous_nodes,
                    ),
                    NodeKind::Transform { child, .. } => previous_nodes[child.index()],
                };
                update_monotone(&mut next_nodes[node.id().index()], derived);
            }

            // Rules observe the prior node round as well. This Jacobi-style
            // update is slower than in-place propagation but order-invariant.
            for &index in rule_order {
                let rule = &program.rules()[index];
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
        for node in program.nodes() {
            let NodeKind::Repeat { child, .. } = node.kind() else {
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
        let zero_cycles = cyclic_components(program.revision(), &zero_call_edges);
        let zero_cycle_rules = zero_cycles
            .iter()
            .flatten()
            .copied()
            .collect::<BTreeSet<_>>();

        for component_index in 0..dependency.component_count() {
            let component_id = super::syntax::SccId::checked(program.revision(), component_index)
                .expect("component identity was already checked");
            if dependency.is_recursive(component_id) != Some(true) {
                continue;
            }
            let members = dependency.component_required(component_id);
            let empty_members = members
                .iter()
                .copied()
                .filter(|rule| rule_work[rule.index()].minimum_consumption.is_none())
                .collect::<Vec<_>>();
            if !empty_members.is_empty()
                && !members.iter().any(|rule| zero_cycle_rules.contains(rule))
            {
                let first = empty_members[0];
                let span = program
                    .node_required(program.rule_required(first).root())
                    .span();
                let names = empty_members
                    .iter()
                    .map(|&rule| program.rule_required(rule).name())
                    .collect::<Vec<_>>()
                    .join(", ");
                semantic_diagnostics.push(Diagnostic::new(
                    DiagnosticCode::RecursiveRule,
                    span.start() as usize,
                    span.end() as usize,
                    format!(
                        "recursive component {} has no finite successful derivation for: {names}",
                        component_index
                    ),
                ));
            }
        }

        for cycle in zero_cycles {
            let first = cycle[0];
            let span = program
                .node_required(program.rule_required(first).root())
                .span();
            let names = cycle
                .iter()
                .map(|&rule| program.rule_required(rule).name())
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
            .nodes()
            .iter()
            .map(|node| NodeFacts {
                revision,
                node: node.id(),
                nullable: work[node.id().index()].nullable,
                minimum_consumption: work[node.id().index()].minimum_consumption,
            })
            .collect();
        let rules = program
            .rules()
            .iter()
            .map(|rule| RuleFacts {
                revision,
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

    pub fn nodes(&self) -> &[NodeFacts] {
        &self.nodes
    }

    pub fn rules(&self) -> &[RuleFacts] {
        &self.rules
    }

    pub fn node(&self, id: NodeId) -> Option<NodeFacts> {
        (id.revision() == self.revision)
            .then(|| self.nodes.get(id.index()).copied())
            .flatten()
    }

    pub fn rule(&self, id: RuleId) -> Option<RuleFacts> {
        (id.revision() == self.revision)
            .then(|| self.rules.get(id.index()).copied())
            .flatten()
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

fn sequence_facts(children: &[NodeId], facts: &[WorkFacts]) -> WorkFacts {
    let nullable = children.iter().all(|child| facts[child.index()].nullable);
    let minimum_consumption = children.iter().try_fold(0_u64, |total, child| {
        total.checked_add(facts[child.index()].minimum_consumption?)
    });
    WorkFacts {
        nullable,
        minimum_consumption,
    }
}

fn choice_facts(children: &[NodeId], facts: &[WorkFacts]) -> WorkFacts {
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

/// Rule calls that may be entered before the current rule has consumed input.
///
/// The frontend assigns children before their containing node, so this is a
/// linear dynamic program over immutable syntax. A sequence exposes calls from
/// successive children only while every preceding child is nullable.
fn zero_consumption_calls(program: &Program, facts: &[WorkFacts]) -> Vec<Vec<RuleId>> {
    let mut leading = vec![BTreeSet::new(); program.nodes().len()];
    for node in program.nodes() {
        let mut calls = BTreeSet::new();
        match node.kind() {
            NodeKind::RuleRef(rule) => {
                calls.insert(*rule);
            }
            NodeKind::Sequence(children) => {
                for child in children {
                    calls.extend(leading[child.index()].iter().copied());
                    if !facts[child.index()].nullable {
                        break;
                    }
                }
            }
            NodeKind::Choice(children) => {
                for child in children {
                    calls.extend(leading[child.index()].iter().copied());
                }
            }
            NodeKind::Repeat { child, maximum, .. } => {
                if *maximum != Some(0) {
                    calls.extend(leading[child.index()].iter().copied());
                }
            }
            NodeKind::Record { slots, .. } => {
                for slot in slots {
                    let child = slot.child();
                    calls.extend(leading[child.index()].iter().copied());
                    if !facts[child.index()].nullable {
                        break;
                    }
                }
            }
            NodeKind::Transform { child, .. } => {
                calls.extend(leading[child.index()].iter().copied());
            }
            NodeKind::Epsilon | NodeKind::Literal(_) | NodeKind::Regex(_) => {}
        }
        leading[node.id().index()] = calls;
    }

    program
        .rules()
        .iter()
        .map(|rule| leading[rule.root().index()].iter().copied().collect())
        .collect()
}

/// Canonical SCCs of the zero-consumption call graph, filtered to cycles.
fn cyclic_components(revision: Revision, edges: &[Vec<RuleId>]) -> Vec<Vec<RuleId>> {
    let count = edges.len();
    let mut visited = vec![false; count];
    let mut finish = Vec::with_capacity(count);

    for index in 0..count {
        if visited[index] {
            continue;
        }
        visited[index] = true;
        let root = RuleId::checked(revision, index).expect("rule identity is checked");
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
        let source = RuleId::checked(revision, source_index).expect("rule identity is checked");
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
            let node_order = deterministic_permutation(program.nodes().len(), seed);
            let rule_order =
                deterministic_permutation(program.rules().len(), seed ^ 0xa076_1d64_78bd_642f);
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
