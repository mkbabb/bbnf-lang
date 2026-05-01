//! E-graph fast-path equivalence pre-filter.
//!
//! Before invoking the (expensive) [`crate::ruler::oracle`] on a candidate
//! `(lhs, rhs)` pair, the residue filter asks the e-graph whether the
//! pair is *already* known equivalent — either by structural canonical
//! form (the e-graph's hashcons collapses identical sub-trees) or by a
//! prior rewrite that's been seeded into the filter. Pairs the e-graph
//! can discharge are *not* residual; pairs it cannot are.
//!
//! The "residue" framing follows Wang et al., Theia (2023) — the goal is
//! to mine novel rewrites by stripping out everything the e-graph
//! already knows. Pairs the e-graph can already prove are noise; the
//! signal lives in what's left.
//!
//! Reference: Wang et al., "SpEQ: Speeding Up Equality Saturation"
//! (Theia, 2023), §4 "Lemma Mining". See also Nandi et al., Ruler
//! (OOPSLA 2021) §3.3 — Ruler uses an e-graph filter for the same
//! reason but expresses it as "candidate caching".

use crate::analysis::NoAnalysis;
use crate::egraph::EGraph;
use crate::id::Id;
use crate::language::Language;
use crate::ruler::enumerate::{LangNode, Pattern};

/// Pre-filter for candidate rule pairs.
///
/// A `ResidueFilter` carries an internal [`EGraph`] seeded with known
/// rewrites via [`ResidueFilter::add_known_rewrite`]. The
/// [`ResidueFilter::is_residual`] method tests whether a fresh `(lhs,
/// rhs)` pair is *not* discharged by the seeded e-graph — i.e., whether
/// the oracle still needs to weigh in.
///
/// The filter is generic over a [`LangNode`] that also implements
/// [`Language`], so the same e-node type used for the production e-graph
/// can be used for residue checking.
pub struct ResidueFilter<N>
where
    N: LangNode + Language,
{
    egraph: EGraph<N, NoAnalysis>,
}

impl<N> Default for ResidueFilter<N>
where
    N: LangNode + Language,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<N> ResidueFilter<N>
where
    N: LangNode + Language,
{
    /// Construct an empty residue filter. The internal e-graph is
    /// initially empty; seed it via [`Self::add_known_rewrite`] before
    /// querying.
    pub fn new() -> Self {
        Self {
            egraph: EGraph::new(),
        }
    }

    /// Seed a known rewrite into the filter. After this call, the
    /// filter treats `lhs` and `rhs` as equivalent — any future
    /// [`Self::is_residual`] query asking about `(lhs, rhs)` (or any
    /// sub-tree of either) will return `false`.
    ///
    /// Call this once per known rule before querying. Multiple known
    /// rewrites compose transitively through the e-graph's union
    /// operation: seeding `(a, b)` and `(b, c)` makes `(a, c)`
    /// non-residual as well.
    pub fn add_known_rewrite(&mut self, lhs: &Pattern<N>, rhs: &Pattern<N>) {
        let lhs_id = self.intern(lhs);
        let rhs_id = self.intern(rhs);
        self.egraph.union(lhs_id, rhs_id);
        self.egraph.rebuild();
    }

    /// Test whether `(lhs, rhs)` is residual — i.e., whether the
    /// e-graph CANNOT discharge the pair on its own.
    ///
    /// Returns `true` IFF the oracle needs to check this pair (the
    /// e-graph can't already prove the equivalence). Returns `false`
    /// when the e-graph collapses both sides to the same e-class,
    /// either by structural canonicalisation (identical sub-trees) or
    /// by a previously-seeded rewrite.
    ///
    /// Note: this method mutates the internal e-graph — interning the
    /// candidate pair is what surfaces the equivalence. Subsequent
    /// queries see the freshly-interned classes, which is generally
    /// fine (re-asking the same pair is cheap and gives the same
    /// answer).
    pub fn is_residual(&mut self, lhs: &Pattern<N>, rhs: &Pattern<N>) -> bool {
        let lhs_id = self.intern(lhs);
        let rhs_id = self.intern(rhs);
        // Note: we do NOT union here — the question is whether the
        // *existing* e-graph already considers the two equivalent.
        self.egraph.find(lhs_id) != self.egraph.find(rhs_id)
    }

    /// Read-only view of the internal e-graph. Useful for tests and
    /// instrumentation; consumers should not normally need direct
    /// access.
    pub fn egraph(&self) -> &EGraph<N, NoAnalysis> {
        &self.egraph
    }

    /// Recursively intern a pattern: every sub-tree is added bottom-up
    /// so each child's e-class ID is known before constructing the
    /// parent e-node.
    fn intern(&mut self, pattern: &Pattern<N>) -> Id {
        let child_ids: Vec<Id> = pattern
            .children
            .iter()
            .map(|child| self.intern(child))
            .collect();
        let node = N::build_node(pattern.tag, child_ids).expect(
            "ResidueFilter::intern: pattern arity must agree with LangNode::build_node — \
             this is a contract violation between Alphabet::variants and LangNode::build_node",
        );
        self.egraph.add(node)
    }
}
