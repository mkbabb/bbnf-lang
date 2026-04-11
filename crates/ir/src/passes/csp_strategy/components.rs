//! Connected-components decomposition for the strategy CSP.
//!
//! Tranche Y.5 introduced the [`UnionFind`] substrate as a deferred
//! hook — a union-find data structure waiting for its first real
//! production consumer. Tranche AF.3 wakes it up.
//!
//! # The AF.3 upgrade
//!
//! Previously, `solve_strategy_and_materialization` walked the rule
//! list and solved each rule body as its own isolated CSP. Under the
//! pre-AF.3 constraint topology (a single intra-rule
//! `ImplicationConstraint` wiring `AltMode = ByteDispatch` to its
//! child `RegexEngine` sites) every component was exactly one rule
//! body, so the per-rule loop *was* the connected-components
//! decomposition — just written as a direct iteration with no
//! partition step.
//!
//! AF.3 lands the first genuine **cross-rule** constraints
//! (`EnginePropagation`, `ParentCompatibility`,
//! `TierFollowsMaterialization`), and at that point per-rule
//! iteration is no longer sufficient: a cross-rule constraint binds
//! variables in two distinct rule bodies and must be solved inside a
//! single `csp_solver::Csp` instance, not spread across two
//! independent solves. The new `partition_by_call_graph` function
//! below builds the connected-components partition **of the rule
//! call graph** — not of the constraint edges — so every rule that
//! could possibly reach another rule via `IrNode::Ref` shares a
//! component. The strategy CSP then solves one component at a time,
//! with all cross-rule constraints local to that component.
//!
//! # Semantics
//!
//! Two rules are in the same component iff one transitively
//! references the other via `IrNode::Ref`. The partition treats the
//! call graph as **undirected** — the component closure under the
//! reflexive, symmetric, transitive closure of the Ref edge
//! relation. This is the coarsest partition under which every
//! cross-rule constraint `{A, B}` fits inside a single component
//! (and therefore inside a single CSP).
//!
//! Isolated rules (no incoming or outgoing `IrNode::Ref` edges)
//! become trivial singleton components, and fall through the existing
//! per-variable fast path in `decode_min_cost_per_variable`.
//!
//! # Component-scoped CSP solves are first-class
//!
//! Per the AF.3 narrative, the strategy solver's unit of work is no
//! longer the rule — it is the *component*. The rename from
//! `solve_strategy_and_materialization` → `solve_grammar_components`
//! makes the contract explicit at the type level: the public entry
//! point takes a `GrammarIR`, builds a [`GrammarComponents`]
//! partition once, and iterates components (not rules). A component
//! with one rule is still a component; a component with fifty rules
//! is still a component. The solver does not care.
//!
//! # Node budget safety net
//!
//! The Y.-1 `SolveConfig::node_budget` guards every component solve.
//! A pathological component (e.g., one where the cross-rule
//! propagation explodes the search space) falls back to per-variable
//! trivial picks for that component without hanging the compile. The
//! per-component logging is preserved from the existing
//! `BBNF_CSP_REPORT=1` path.

use std::collections::HashMap;

use crate::{GrammarIR, IrNode, RuleId};

/// A tiny, path-compressing union-find over flat `u32` keys.
///
/// Kept deliberately minimal: the strategy solver only needs
/// `union`, `find`, and `components`. The union-find is per-compile
/// scratch, discarded after decomposition — no persistent state.
#[derive(Debug, Clone)]
pub struct UnionFind {
    parent: Vec<u32>,
    rank: Vec<u8>,
}

impl UnionFind {
    /// Create a union-find with `n` singleton components.
    pub fn new(n: usize) -> Self {
        Self {
            parent: (0..n as u32).collect(),
            rank: vec![0; n],
        }
    }

    /// Find the root of `x` with path compression.
    pub fn find(&mut self, x: u32) -> u32 {
        let mut root = x;
        while self.parent[root as usize] != root {
            root = self.parent[root as usize];
        }
        // Path compression: point everything on the walk to the root.
        let mut cur = x;
        while self.parent[cur as usize] != root {
            let next = self.parent[cur as usize];
            self.parent[cur as usize] = root;
            cur = next;
        }
        root
    }

    /// Union the components containing `a` and `b`. Returns `true` if
    /// the two were in different components prior to the call.
    pub fn union(&mut self, a: u32, b: u32) -> bool {
        let ra = self.find(a);
        let rb = self.find(b);
        if ra == rb {
            return false;
        }
        let (smaller, larger) = match self.rank[ra as usize].cmp(&self.rank[rb as usize]) {
            std::cmp::Ordering::Less => (ra, rb),
            std::cmp::Ordering::Greater => (rb, ra),
            std::cmp::Ordering::Equal => {
                self.rank[ra as usize] += 1;
                (rb, ra)
            }
        };
        self.parent[smaller as usize] = larger;
        true
    }

    /// Enumerate components as a map from root → member list.
    ///
    /// Called at most once per decomposition — ordering-stable so the
    /// caller's solve order is deterministic.
    pub fn components(&mut self) -> HashMap<u32, Vec<u32>> {
        let n = self.parent.len();
        let mut out: HashMap<u32, Vec<u32>> = HashMap::new();
        for i in 0..n {
            let root = self.find(i as u32);
            out.entry(root).or_default().push(i as u32);
        }
        out
    }

    /// Number of distinct components.
    pub fn component_count(&mut self) -> usize {
        let n = self.parent.len();
        let mut roots = std::collections::HashSet::new();
        for i in 0..n {
            roots.insert(self.find(i as u32));
        }
        roots.len()
    }
}

// ── Rule call-graph partition (Tranche AF.3) ────────────────────────────────

/// Connected-components partition of the rule call graph.
///
/// Built once per compile by [`partition_by_call_graph`] and consumed
/// by `csp_strategy::solve_grammar_components` to drive the
/// component-scoped CSP solve loop. The strategy CSP treats a
/// component as its unit of work: every rule in the component goes
/// into the same `csp_solver::Csp` instance, so the cross-rule
/// constraints (`EnginePropagation`, `ParentCompatibility`,
/// `TierFollowsMaterialization`) can bind variables across rule
/// bodies without leaking outside the component boundary.
///
/// # Fields
///
/// - `uf`: the populated [`UnionFind`] over `RuleId`s. Held for the
///   lifetime of the partition so callers can continue to call
///   `find(rule_id)` while building per-component CSPs.
/// - `rule_to_component`: for every rule, the representative root
///   (the `uf.find(rule.id)` result after all unions). Using the root
///   as the component key means two rules unified into the same
///   component share the same key regardless of traversal order.
/// - `components`: inverse mapping — component root → member
///   `RuleId`s. Populated deterministically by iterating
///   `ir.rules` in order, so downstream solve order is stable across
///   runs.
#[derive(Debug, Clone)]
pub struct GrammarComponents {
    /// The populated union-find over rule IDs. Kept live so callers
    /// can re-query `find()` without rebuilding.
    pub uf: UnionFind,

    /// Rule → component representative root.
    pub rule_to_component: HashMap<RuleId, u32>,

    /// Component representative root → member rule IDs, in
    /// `ir.rules` insertion order.
    pub components: HashMap<u32, Vec<RuleId>>,
}

impl GrammarComponents {
    /// Number of distinct components in the partition.
    pub fn component_count(&self) -> usize {
        self.components.len()
    }

    /// Iterate components in a deterministic order (sorted by
    /// component root ID). Each yielded value is the component's
    /// member list, which itself preserves `ir.rules` insertion
    /// order.
    ///
    /// Ordering stability is required so the strategy solve order is
    /// reproducible across runs — the CSP's optimization search is
    /// order-sensitive because branch-and-bound prunes via the
    /// running best cost, and stable order means stable stats.
    pub fn iter_components(&self) -> impl Iterator<Item = (u32, &Vec<RuleId>)> {
        let mut roots: Vec<u32> = self.components.keys().copied().collect();
        roots.sort_unstable();
        roots.into_iter().map(move |root| {
            (root, self.components.get(&root).expect("root is a key"))
        })
    }

    /// The representative component ID for a given rule, or `None`
    /// if the rule is not in the partition (shouldn't happen in
    /// practice since `partition_by_call_graph` visits every rule).
    pub fn component_of(&self, rule: RuleId) -> Option<u32> {
        self.rule_to_component.get(&rule).copied()
    }
}

/// Walk the rule call graph and build the connected-components
/// partition.
///
/// For every rule, every `IrNode::Ref(target)` inside the rule body
/// is treated as an undirected edge between the caller's `RuleId`
/// and the callee's `RuleId`. The resulting [`UnionFind`] groups
/// rules into components such that any two rules transitively linked
/// by a Ref chain (in either direction) share a component.
///
/// # Semantics
///
/// - **Isolated rules** (no Ref edges in or out) remain singleton
///   components — each such rule is its own component root.
/// - **Cycles** collapse into a single component via the usual
///   union operations.
/// - **Dangling refs** (target rule not in `ir.rules`, which
///   shouldn't happen after `prune_unreachable` but is defended
///   against anyway) are skipped without unioning anything.
///
/// # Cost
///
/// `O(N + E)` where `N = ir.rules.len()` and `E` is the total count
/// of `IrNode::Ref` nodes across all rule bodies. The union-find
/// operations are near-constant amortized. Called exactly once per
/// compile from `solve_grammar_components`.
///
/// # Returns
///
/// A [`GrammarComponents`] carrying the populated union-find, the
/// rule → component-root map, and the inverse component-root → rule
/// list map. All three are consumed together by the strategy solver
/// — the CSP variable-building loop walks `iter_components()` and
/// builds one `Csp` per component.
pub fn partition_by_call_graph(ir: &GrammarIR) -> GrammarComponents {
    let n = ir.rules.len();
    let mut uf = UnionFind::new(n);

    // Walk every rule body and union caller ↔ callee on every
    // `IrNode::Ref`. The walker is intentionally inline (rather than
    // reusing `passes::sets::deps::compute_rule_deps`) because that
    // helper returns a `Vec<Vec<RuleId>>` adjacency list we'd then
    // immediately re-walk to drive the unions — the inline walk is
    // one pass.
    for rule in &ir.rules {
        let caller = rule.id;
        if (caller as usize) >= n {
            continue;
        }
        collect_ref_unions(&rule.body, caller, n, &mut uf);
    }

    // Resolve each rule to its component root (post-unions so path
    // compression has run) and bucket rules by root. Iteration in
    // `ir.rules` order keeps bucket contents deterministic.
    let mut rule_to_component: HashMap<RuleId, u32> = HashMap::with_capacity(n);
    let mut components: HashMap<u32, Vec<RuleId>> = HashMap::new();
    for rule in &ir.rules {
        let root = uf.find(rule.id);
        rule_to_component.insert(rule.id, root);
        components.entry(root).or_default().push(rule.id);
    }

    GrammarComponents {
        uf,
        rule_to_component,
        components,
    }
}

/// Recursive walker: for every `IrNode::Ref(callee)` under `node`,
/// union `caller` with `callee` in `uf`. Mirrors the traversal
/// surface in `passes::sets::deps::compute_rule_deps`.
fn collect_ref_unions(node: &IrNode, caller: RuleId, n: usize, uf: &mut UnionFind) {
    match node {
        IrNode::Ref(callee) => {
            if (*callee as usize) < n {
                uf.union(caller, *callee);
            }
        }

        IrNode::Seq(children) => {
            for child in children {
                collect_ref_unions(child, caller, n, uf);
            }
        }
        IrNode::Alt(branches, _) => {
            for branch in branches {
                collect_ref_unions(&branch.node, caller, n, uf);
            }
        }

        IrNode::Repeat { inner, .. }
        | IrNode::Map { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Negate(inner) => collect_ref_unions(inner, caller, n, uf),

        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            collect_ref_unions(a, caller, n, uf);
            collect_ref_unions(b, caller, n, uf);
        }

        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            collect_ref_unions(token, caller, n, uf);
            for arm in arms {
                collect_ref_unions(&arm.continuation, caller, n, uf);
            }
            collect_ref_unions(fallback, caller, n, uf);
        }

        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
    }
}

// Unit tests live in `crates/ir/tests/csp_components.rs` per
// the `no-inline-tests` convention.
