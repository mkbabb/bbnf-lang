use std::collections::BTreeSet;

use serde::Serialize;

use super::diagnostic::StageResult;
use super::source::Revision;
use super::syntax::{NodeKindData, Program, RuleId, RuleIds, RuleIx, SccId, SccIds, SccIx};

/// Direct, sorted rule dependencies for one immutable source revision.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DependencyGraph {
    revision: Revision,
    edges: Vec<Vec<RuleIx>>,
}

impl DependencyGraph {
    fn from_program(program: &Program) -> Self {
        let mut edges = Vec::with_capacity(program.rule_data_rows().len());
        for rule in program.rule_data_rows() {
            let mut dependencies = BTreeSet::new();
            let mut visited = BTreeSet::new();
            let mut pending = vec![rule.root()];

            while let Some(node_ix) = pending.pop() {
                if !visited.insert(node_ix) {
                    continue;
                }
                match program.node_data(node_ix).kind() {
                    NodeKindData::RuleRef(target) => {
                        dependencies.insert(*target);
                    }
                    NodeKindData::Sequence(children) | NodeKindData::Choice(children) => {
                        pending.extend(children.iter().rev().copied());
                    }
                    NodeKindData::Record { slots, .. } => {
                        pending.extend(slots.iter().rev().map(|slot| slot.child()));
                    }
                    NodeKindData::Repeat { child, .. } | NodeKindData::Transform { child, .. } => {
                        pending.push(*child);
                    }
                    NodeKindData::Epsilon | NodeKindData::Literal(_) | NodeKindData::Regex(_) => {}
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

    pub fn dependencies(&self, rule: RuleId) -> Option<RuleIds<'_>> {
        (rule.revision() == self.revision)
            .then(|| {
                self.edges
                    .get(rule.local().index())
                    .map(|edges| RuleIds::new(&self.revision, edges))
            })
            .flatten()
    }

    pub(crate) fn dependencies_local(&self, rule: RuleIx) -> &[RuleIx] {
        &self.edges[rule.index()]
    }

    pub const fn rule_count(&self) -> usize {
        self.edges.len()
    }
}

/// Canonical SCC decomposition and dependency-first condensation order.
///
/// Recursion is a property of the grammar, not a dependency-stage error. A
/// component is retained as one finite binder group; later semantic passes
/// decide whether that group has a productive, input-advancing interpretation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DependencySolution {
    revision: Revision,
    graph: DependencyGraph,
    components: Vec<Vec<RuleIx>>,
    component_by_rule: Vec<SccIx>,
    dependency_order: Vec<SccIx>,
}

impl DependencySolution {
    pub(crate) fn build(program: &Program) -> StageResult<Self> {
        let graph = DependencyGraph::from_program(program);
        let raw_components = strongly_connected_components(&graph);

        let mut components = raw_components;
        for component in &mut components {
            component.sort_unstable();
        }
        components.sort();

        let zero = SccIx::checked(0).expect("zero is a valid component ordinal");
        let mut component_by_rule = vec![zero; graph.rule_count()];
        for (index, members) in components.iter().enumerate() {
            let component = SccIx::checked(index).expect("rule budget bounds component ordinals");
            for &member in members {
                component_by_rule[member.index()] = component;
            }
        }

        let dependency_order = dependency_first_order(&graph, &components, &component_by_rule);
        StageResult::accepted(Self {
            revision: program.revision(),
            graph,
            components,
            component_by_rule,
            dependency_order,
        })
    }

    pub const fn revision(&self) -> Revision {
        self.revision
    }

    pub const fn graph(&self) -> &DependencyGraph {
        &self.graph
    }

    pub fn component(&self, id: SccId) -> Option<RuleIds<'_>> {
        (id.revision() == self.revision)
            .then(|| {
                self.components
                    .get(id.local().index())
                    .map(|members| RuleIds::new(&self.revision, members))
            })
            .flatten()
    }

    pub const fn component_count(&self) -> usize {
        self.components.len()
    }

    pub fn component_of(&self, rule: RuleId) -> Option<SccId> {
        (rule.revision() == self.revision)
            .then(|| {
                self.component_by_rule
                    .get(rule.local().index())
                    .copied()
                    .map(|local| SccId::bind(self.revision, local))
            })
            .flatten()
    }

    pub(crate) fn component_local(&self, id: SccIx) -> &[RuleIx] {
        &self.components[id.index()]
    }

    pub(crate) fn component_of_local(&self, rule: RuleIx) -> SccIx {
        self.component_by_rule[rule.index()]
    }

    pub(crate) fn dependency_order_local(&self) -> &[SccIx] {
        &self.dependency_order
    }

    /// Whether a component is genuinely recursive rather than a singleton in
    /// the condensation DAG.
    pub fn is_recursive(&self, id: SccId) -> Option<bool> {
        if id.revision() != self.revision {
            return None;
        }
        self.is_recursive_local(id.local())
    }

    pub(crate) fn is_recursive_local(&self, id: SccIx) -> Option<bool> {
        let members = self.components.get(id.index())?;
        Some(
            members.len() > 1
                || members.first().is_some_and(|rule| {
                    self.graph
                        .dependencies_local(*rule)
                        .binary_search(rule)
                        .is_ok()
                }),
        )
    }

    /// Components in dependency-first order with local component ordinal as
    /// the stable tie breaker between simultaneously ready components.
    pub fn dependency_order(&self) -> SccIds<'_> {
        SccIds::new(&self.revision, &self.dependency_order)
    }

    pub(super) fn replay_value(&self) -> DependencySolutionReplay<'_> {
        DependencySolutionReplay {
            revision: self.revision,
            graph: DependencyGraphReplay {
                revision: self.graph.revision,
                edges: &self.graph.edges,
            },
            components: &self.components,
            component_by_rule: &self.component_by_rule,
            dependency_order: &self.dependency_order,
        }
    }
}

#[derive(Serialize)]
struct DependencyGraphReplay<'a> {
    revision: Revision,
    edges: &'a [Vec<RuleIx>],
}

#[derive(Serialize)]
pub(super) struct DependencySolutionReplay<'a> {
    revision: Revision,
    graph: DependencyGraphReplay<'a>,
    components: &'a [Vec<RuleIx>],
    component_by_rule: &'a [SccIx],
    dependency_order: &'a [SccIx],
}

fn strongly_connected_components(graph: &DependencyGraph) -> Vec<Vec<RuleIx>> {
    let count = graph.rule_count();
    let mut visited = vec![false; count];
    let mut finish = Vec::with_capacity(count);

    for index in 0..count {
        if visited[index] {
            continue;
        }
        visited[index] = true;
        let root = RuleIx::checked(index).expect("rule ordinal was previously bounded");
        let mut stack = vec![(root, 0_usize)];
        while let Some((rule, next)) = stack.last_mut() {
            let dependencies = graph.dependencies_local(*rule);
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
        let source = RuleIx::checked(source_index).expect("rule ordinal was previously bounded");
        for &target in graph.dependencies_local(source) {
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
    components: &[Vec<RuleIx>],
    component_by_rule: &[SccIx],
) -> Vec<SccIx> {
    let mut dependents = vec![BTreeSet::new(); components.len()];
    let mut unresolved_dependencies = vec![0_usize; components.len()];

    for (source_index, edges) in graph.edges.iter().enumerate() {
        let source = component_by_rule[source_index];
        for &target_rule in edges {
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
            ready.insert(SccIx::checked(index).expect("component ordinal was previously bounded"));
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

#[cfg(test)]
mod representation_laws {
    use std::mem::size_of;

    use super::*;

    #[test]
    fn dependency_edges_and_component_maps_use_local_ordinals() {
        assert_eq!(size_of::<RuleIx>(), 4);
        assert_eq!(size_of::<SccIx>(), 4);
        assert_eq!(size_of::<Vec<RuleIx>>(), size_of::<Vec<u32>>());
        assert_eq!(size_of::<Vec<SccIx>>(), size_of::<Vec<u32>>());
    }
}
