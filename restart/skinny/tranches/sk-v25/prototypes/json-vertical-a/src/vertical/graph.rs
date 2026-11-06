use std::{collections::BTreeSet, sync::Arc};

use serde::Serialize;

use super::{
    Digest, VerticalError, budget::Budget, canonical_digest, normalize::NormalizedProgram,
    program::RuleId,
};

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize)]
pub struct DependencyEdge {
    pub from: RuleId,
    pub to: RuleId,
}

#[derive(Clone, Debug)]
pub struct DependencyGraph {
    program_revision: Digest,
    digest: Digest,
    edges: Arc<[DependencyEdge]>,
    components: Arc<[Arc<[RuleId]>]>,
}

impl DependencyGraph {
    pub fn derive(program: &NormalizedProgram, budget: Budget) -> Result<Self, VerticalError> {
        let rules = program.program().rules();
        let mut edges = rules
            .iter()
            .flat_map(|rule| {
                rule.references().iter().map(|target| DependencyEdge {
                    from: rule.id(),
                    to: *target,
                })
            })
            .collect::<Vec<_>>();
        edges.sort_unstable();
        edges.dedup();
        Budget::require(budget.graph_edges, edges.len(), "graph_edges")?;

        let forward = adjacency(rules.len(), &edges, false);
        let reverse = adjacency(rules.len(), &edges, true);
        let mut visited = vec![false; rules.len()];
        let mut order = Vec::with_capacity(rules.len());
        for root in 0..rules.len() {
            if !visited[root] {
                finish_order(root, &forward, &mut visited, &mut order);
            }
        }
        visited.fill(false);
        let mut components: Vec<Arc<[RuleId]>> = Vec::new();
        for &root in order.iter().rev() {
            if visited[root] {
                continue;
            }
            let mut stack = vec![root];
            let mut component = Vec::new();
            visited[root] = true;
            while let Some(node) = stack.pop() {
                component.push(rules[node].id());
                for &next in reverse[node].iter().rev() {
                    if !visited[next] {
                        visited[next] = true;
                        stack.push(next);
                    }
                }
            }
            component.sort_unstable();
            components.push(Arc::from(component));
        }
        components.sort_by_key(|component| component[0]);
        let components: Arc<[Arc<[RuleId]>]> = components.into();
        let digest = canonical_digest(&(program.digest(), &edges, &components))?;
        Ok(Self {
            program_revision: program.program().revision(),
            digest,
            edges: edges.into(),
            components,
        })
    }

    #[must_use]
    pub const fn program_revision(&self) -> Digest {
        self.program_revision
    }

    #[must_use]
    pub const fn digest(&self) -> Digest {
        self.digest
    }

    #[must_use]
    pub fn edges(&self) -> &[DependencyEdge] {
        &self.edges
    }

    #[must_use]
    pub fn components(&self) -> &[Arc<[RuleId]>] {
        &self.components
    }

    #[must_use]
    pub fn condensation_edges(&self) -> BTreeSet<(usize, usize)> {
        let mut owner = vec![0; self.components.iter().map(|c| c.len()).sum()];
        for (component, members) in self.components.iter().enumerate() {
            for member in members.iter() {
                owner[member.index()] = component;
            }
        }
        self.edges
            .iter()
            .filter_map(|edge| {
                let pair = (owner[edge.from.index()], owner[edge.to.index()]);
                (pair.0 != pair.1).then_some(pair)
            })
            .collect()
    }
}

fn adjacency(size: usize, edges: &[DependencyEdge], reverse: bool) -> Vec<Vec<usize>> {
    let mut result = vec![Vec::new(); size];
    for edge in edges {
        let (from, to) = if reverse {
            (edge.to.index(), edge.from.index())
        } else {
            (edge.from.index(), edge.to.index())
        };
        result[from].push(to);
    }
    for targets in &mut result {
        targets.sort_unstable();
    }
    result
}

fn finish_order(root: usize, graph: &[Vec<usize>], visited: &mut [bool], order: &mut Vec<usize>) {
    let mut stack = vec![(root, false)];
    while let Some((node, exiting)) = stack.pop() {
        if exiting {
            order.push(node);
            continue;
        }
        if visited[node] {
            continue;
        }
        visited[node] = true;
        stack.push((node, true));
        for &next in graph[node].iter().rev() {
            if !visited[next] {
                stack.push((next, false));
            }
        }
    }
}
