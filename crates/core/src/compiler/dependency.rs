use std::collections::BTreeSet;

use serde::Serialize;

use super::diagnostic::StageResult;
use super::source::Revision;
use super::syntax::{NodeKind, Program, RuleId, SccId};

/// Direct, sorted rule dependencies for one immutable source revision.
#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct DependencyGraph {
    revision: Revision,
    edges: Vec<Vec<RuleId>>,
}

impl DependencyGraph {
    fn from_program(program: &Program) -> Self {
        let mut edges = Vec::with_capacity(program.rules().len());
        for rule in program.rules() {
            let mut dependencies = BTreeSet::new();
            let mut visited = BTreeSet::new();
            let mut pending = vec![rule.root()];

            while let Some(node_id) = pending.pop() {
                if !visited.insert(node_id) {
                    continue;
                }
                match program.node_required(node_id).kind() {
                    NodeKind::RuleRef(target) => {
                        dependencies.insert(*target);
                    }
                    NodeKind::Sequence(children) | NodeKind::Choice(children) => {
                        // Reverse insertion keeps the traversal itself in NodeId order.
                        pending.extend(children.iter().rev().copied());
                    }
                    NodeKind::Record { slots, .. } => {
                        pending.extend(slots.iter().rev().map(|slot| slot.child()));
                    }
                    NodeKind::Repeat { child, .. } | NodeKind::Transform { child, .. } => {
                        pending.push(*child);
                    }
                    NodeKind::Epsilon | NodeKind::Literal(_) | NodeKind::Regex(_) => {}
                }
            }
            edges.push(dependencies.into_iter().collect());
        }

        Self {
            revision: program.revision(),
            edges,
        }
    }

    pub const fn revision(&self) -> Revision {
        self.revision
    }

    pub fn dependencies(&self, rule: RuleId) -> Option<&[RuleId]> {
        self.edges.get(rule.index()).map(Vec::as_slice)
    }

    fn dependencies_required(&self, rule: RuleId) -> &[RuleId] {
        self.dependencies(rule)
            .expect("dependency graph owns every internal rule identity")
    }

    pub const fn rule_count(&self) -> usize {
        self.edges.len()
    }
}

/// Canonical SCC decomposition and dependency-first condensation order.
///
/// Recursion is a property of the grammar, not a dependency-stage error.  A
/// component is retained as one finite binder group; later semantic passes
/// decide whether that group has a productive, input-advancing interpretation.
#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct DependencySolution {
    revision: Revision,
    graph: DependencyGraph,
    components: Vec<Vec<RuleId>>,
    component_by_rule: Vec<SccId>,
    dependency_order: Vec<SccId>,
}

impl DependencySolution {
    pub(crate) fn build(program: &Program) -> StageResult<Self> {
        let graph = DependencyGraph::from_program(program);
        let raw_components = strongly_connected_components(&graph);

        // Component identities are independent of DFS discovery order: members
        // are sorted and components are ordered lexicographically by members.
        let mut components = raw_components;
        for component in &mut components {
            component.sort_unstable();
        }
        components.sort();

        let mut component_by_rule = vec![
            SccId::checked(0)
                .expect("zero is a valid component identity");
            graph.rule_count()
        ];
        for (index, members) in components.iter().enumerate() {
            let component = SccId::checked(index).expect("rule budget bounds SCC identities");
            for &member in members {
                component_by_rule[member.index()] = component;
            }
        }

        let dependency_order = dependency_first_order(&graph, &components, &component_by_rule);
        let solution = Self {
            revision: program.revision(),
            graph,
            components,
            component_by_rule,
            dependency_order,
        };

        StageResult::accepted(solution)
    }

    pub const fn revision(&self) -> Revision {
        self.revision
    }

    pub const fn graph(&self) -> &DependencyGraph {
        &self.graph
    }

    pub fn component(&self, id: SccId) -> Option<&[RuleId]> {
        self.components.get(id.index()).map(Vec::as_slice)
    }

    pub const fn component_count(&self) -> usize {
        self.components.len()
    }

    pub fn component_of(&self, rule: RuleId) -> Option<SccId> {
        self.component_by_rule.get(rule.index()).copied()
    }

    pub(crate) fn component_required(&self, id: SccId) -> &[RuleId] {
        self.component(id)
            .expect("dependency solution owns every internal component identity")
    }

    /// Whether a component is genuinely recursive rather than a singleton in
    /// the condensation DAG.
    pub fn is_recursive(&self, id: SccId) -> bool {
        self.component(id).is_some_and(|members| {
            members.len() > 1
                || members.first().is_some_and(|rule| {
                    self.graph
                        .dependencies_required(*rule)
                        .binary_search(rule)
                        .is_ok()
                })
        })
    }

    /// Components in dependency-first order with `SccId` as the stable tie
    /// breaker between simultaneously ready components.
    pub fn dependency_order(&self) -> &[SccId] {
        &self.dependency_order
    }
}

fn strongly_connected_components(graph: &DependencyGraph) -> Vec<Vec<RuleId>> {
    let count = graph.rule_count();
    let mut visited = vec![false; count];
    let mut finish = Vec::with_capacity(count);

    // Iterative DFS with an explicit neighbor cursor avoids both call-stack
    // limits and nondeterminism from hash-backed traversal.
    for index in 0..count {
        if visited[index] {
            continue;
        }
        visited[index] = true;
        let root = RuleId::checked(index).expect("graph identities are checked");
        let mut stack = vec![(root, 0_usize)];
        while let Some((rule, next)) = stack.last_mut() {
            let dependencies = graph.dependencies_required(*rule);
            if *next < dependencies.len() {
                let dependency = dependencies[*next];
                *next += 1;
                if !visited[dependency.index()] {
                    visited[dependency.index()] = true;
                    stack.push((dependency, 0));
                }
            } else {
                let (finished, _) = stack.pop().expect("stack is nonempty");
                finish.push(finished);
            }
        }
    }

    let mut reverse = vec![Vec::new(); count];
    for source_index in 0..count {
        let source = RuleId::checked(source_index).expect("graph identities are checked");
        for &target in graph.dependencies_required(source) {
            reverse[target.index()].push(source);
        }
    }
    for incoming in &mut reverse {
        incoming.sort_unstable();
        incoming.dedup();
    }

    visited.fill(false);
    let mut components = Vec::new();
    for root in finish.into_iter().rev() {
        if visited[root.index()] {
            continue;
        }
        visited[root.index()] = true;
        let mut pending = vec![root];
        let mut members = Vec::new();
        while let Some(rule) = pending.pop() {
            members.push(rule);
            for &predecessor in reverse[rule.index()].iter().rev() {
                if !visited[predecessor.index()] {
                    visited[predecessor.index()] = true;
                    pending.push(predecessor);
                }
            }
        }
        components.push(members);
    }
    components
}

fn dependency_first_order(
    graph: &DependencyGraph,
    components: &[Vec<RuleId>],
    component_by_rule: &[SccId],
) -> Vec<SccId> {
    let mut dependents = vec![BTreeSet::new(); components.len()];
    let mut unresolved_dependencies = vec![0_usize; components.len()];

    for (source_index, _) in graph.edges.iter().enumerate() {
        let source = component_by_rule[source_index];
        for &target_rule in &graph.edges[source_index] {
            let target = component_by_rule[target_rule.index()];
            if source == target {
                continue;
            }
            if dependents[target.index()].insert(source) {
                unresolved_dependencies[source.index()] += 1;
            }
        }
    }

    let mut ready = BTreeSet::new();
    for (index, &count) in unresolved_dependencies.iter().enumerate() {
        if count == 0 {
            ready.insert(SccId::checked(index).expect("component identities are checked"));
        }
    }

    let mut order = Vec::with_capacity(components.len());
    while let Some(component) = ready.pop_first() {
        order.push(component);
        for &dependent in &dependents[component.index()] {
            let remaining = &mut unresolved_dependencies[dependent.index()];
            *remaining -= 1;
            if *remaining == 0 {
                ready.insert(dependent);
            }
        }
    }

    debug_assert_eq!(order.len(), components.len());
    order
}
