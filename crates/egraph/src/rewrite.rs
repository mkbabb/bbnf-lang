//! Rewrite rules: search for patterns and add equivalences.
//!
//! A rewrite rule is a named pair of a *searcher* (finds matches in the
//! e-graph) and an *applier* (installs the rewritten form as a new e-node
//! in the matched class). Unlike destructive rewrites, applying a rewrite
//! never removes the original form — it only adds a new equivalence.

use crate::analysis::Analysis;
use crate::egraph::EGraph;
use crate::id::Id;
use crate::language::Language;

/// A rewrite rule over a language `N` with analysis `A`.
///
/// Rewrites are intentionally imperative rather than pattern-based: each
/// rule implements `search` directly and returns the list of e-class IDs
/// it would like to rewrite (plus any rule-specific match data). `apply`
/// then installs the new form for each match. This mirrors `csp-solver`'s
/// constraint-with-domain approach — more verbose than a pattern DSL but
/// easier to debug and extend.
pub trait Rewrite<N: Language, A: Analysis<N>>: Send + Sync {
    /// Human-readable name (used for debugging and scheduler backoff).
    fn name(&self) -> &str;

    /// Per-call match payload. Carry information from `search` into
    /// `apply` (e.g., the matched sub-tree shape).
    type Match;

    /// Find all matches in the e-graph. Called once per saturation iteration.
    fn search(&self, egraph: &EGraph<N, A>) -> Vec<(Id, Self::Match)>;

    /// Apply one match, installing the rewritten form and unioning with
    /// the matched class. Returns whether any new node was added.
    fn apply(&self, egraph: &mut EGraph<N, A>, class_id: Id, matched: Self::Match) -> bool;
}

/// A type-erased rewrite wrapper: consumers pass `&[&dyn RewriteFn]` to
/// the saturation driver without caring about the `Match` associated type.
pub trait RewriteFn<N: Language, A: Analysis<N>>: Send + Sync {
    /// Human-readable name.
    fn name(&self) -> &str;
    /// Run one full iteration: search + apply all matches. Returns the
    /// number of matches that produced a new node.
    fn run(&self, egraph: &mut EGraph<N, A>) -> usize;
}

/// Blanket erasure: any `Rewrite` is also a `RewriteFn`.
impl<N, A, R> RewriteFn<N, A> for R
where
    N: Language,
    A: Analysis<N>,
    R: Rewrite<N, A>,
{
    fn name(&self) -> &str {
        Rewrite::name(self)
    }

    fn run(&self, egraph: &mut EGraph<N, A>) -> usize {
        let matches = self.search(egraph);
        let mut applied = 0;
        for (class_id, m) in matches {
            if self.apply(egraph, class_id, m) {
                applied += 1;
            }
        }
        applied
    }
}
