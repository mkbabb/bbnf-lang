//! CVC-style candidate enumeration.
//!
//! Generates left-hand-side patterns up to a bounded AST depth/size, using
//! a per-grammar [`Alphabet`] (the inventory of leaves and constructors)
//! and a per-grammar [`LangNode`] (the AST node enum). Output is a stream
//! of [`Pattern`] trees — pre-interning, so they live entirely outside the
//! e-graph until the oracle and residue filter accept them.
//!
//! Reference: Nandi et al., Ruler (OOPSLA 2021), §3.1 "Term Generation".
//! Patterns enumerate by depth in BFS-like order, with a deterministic
//! seeded shuffle inside each depth bucket so the consumer can vary the
//! exploration order without changing the search space.
//!
//! The enumerator is grammar-agnostic: it never inspects what a "variable"
//! or "constant" means, only what arities the alphabet exposes.

/// Per-grammar leaf and constructor inventory.
///
/// An alphabet enumerates every *variant tag* the language exposes,
/// alongside its arity (number of child holes). Leaves have arity 0;
/// internal constructors have arity ≥ 1. The enumerator uses this to
/// build candidate trees of bounded depth without baking in any
/// language-specific knowledge.
pub trait Alphabet {
    /// Opaque variant tag — typically the grammar's node-kind enum, but
    /// can be any `Copy + Eq` type. The enumerator passes these tags
    /// back to [`LangNode::build`] when assembling patterns.
    type Tag: Copy + Eq + std::fmt::Debug;

    /// Every variant tag this alphabet exposes, with its arity.
    ///
    /// Order is significant: the enumerator iterates in this order, so
    /// alphabets that want a deterministic exploration order should
    /// return the slice in a stable form (typically a `const` array).
    fn variants(&self) -> &[(Self::Tag, usize)];
}

/// Per-grammar AST node trait — the construction half of [`Language`].
///
/// `Language` itself only requires reading children (because the e-graph
/// stores children as e-class IDs, which are assigned at intern time).
/// The enumerator runs *before* interning, so it needs a way to mint a
/// concrete node from a variant tag and an arity-checked vector of
/// child *patterns*. `LangNode::build` is that constructor.
///
/// Implementations are typically a single `match tag { … }` over the
/// language's variant enum, dispatching to the appropriate constructor.
///
/// The companion [`LangNode::build_node`] method projects a variant tag
/// + child e-class IDs to a concrete e-node, used by the residue filter
/// when interning a [`Pattern`] into the e-graph.
pub trait LangNode: Sized {
    /// The alphabet whose tags this node recognises.
    type Alphabet: Alphabet;

    /// Construct a tree-shaped pattern from a variant tag and an
    /// arity-checked list of child *patterns*. Must return `None` if
    /// the tag is unknown or the arity disagrees with the variant's
    /// expectation — callers will skip mis-shaped candidates rather
    /// than panic.
    fn build(
        tag: <Self::Alphabet as Alphabet>::Tag,
        children: Vec<Pattern<Self>>,
    ) -> Option<Pattern<Self>>;

    /// Construct a concrete e-node (suitable for `EGraph::add`) from a
    /// variant tag and an arity-checked vector of child e-class IDs.
    /// Mirrors [`LangNode::build`] but threads e-class IDs (post-intern)
    /// instead of pattern sub-trees (pre-intern).
    ///
    /// Returns `None` for the same reasons as [`LangNode::build`].
    fn build_node(
        tag: <Self::Alphabet as Alphabet>::Tag,
        children: Vec<crate::id::Id>,
    ) -> Option<Self>;
}

/// A pre-interning candidate pattern: a tree of nodes with explicit
/// child sub-patterns (rather than e-class IDs).
///
/// Patterns are produced by [`enumerate`] and consumed by both the
/// oracle (which interprets them) and the residue filter (which interns
/// them on demand). The variant tag is preserved alongside the children
/// so consumers can pattern-match without re-deriving structure.
pub struct Pattern<N: LangNode> {
    /// The variant tag this pattern represents (i.e., which constructor
    /// of the language enum this node is).
    pub tag: <N::Alphabet as Alphabet>::Tag,
    /// Child sub-patterns, in the order the constructor expects them.
    pub children: Vec<Pattern<N>>,
}

// Hand-written `Clone` and `Debug` impls — `derive` would impose
// `N: Clone` and `N: Debug` bounds, but `Pattern` carries no actual
// `N` field (only `Tag`, which is `Copy`, and recursive `Pattern<N>`
// children). Forwarding the bounds to `<N::Alphabet as Alphabet>::Tag`
// gives the right semantics.
impl<N: LangNode> Clone for Pattern<N> {
    fn clone(&self) -> Self {
        Self {
            tag: self.tag,
            children: self.children.clone(),
        }
    }
}

impl<N: LangNode> std::fmt::Debug for Pattern<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Pattern")
            .field("tag", &self.tag)
            .field("children", &self.children)
            .finish()
    }
}

impl<N: LangNode> Pattern<N> {
    /// AST depth: 1 for a leaf, 1 + max child depth for an internal node.
    pub fn depth(&self) -> usize {
        1 + self
            .children
            .iter()
            .map(|c| c.depth())
            .max()
            .unwrap_or(0)
    }

    /// AST size: total number of nodes in the pattern.
    pub fn size(&self) -> usize {
        1 + self.children.iter().map(|c| c.size()).sum::<usize>()
    }
}

/// Enumerator configuration knobs.
#[derive(Clone, Copy, Debug)]
pub struct EnumerateConfig {
    /// Maximum AST depth. Depth 1 = leaves only; depth 2 = leaves and
    /// one-level-deep internal nodes; etc.
    pub max_depth: usize,
    /// Maximum AST size (total node count). The enumerator skips
    /// patterns whose size exceeds this even when their depth is
    /// allowed, which keeps the search space tractable for high-arity
    /// constructors.
    pub max_size: usize,
    /// Deterministic seed for the per-depth tiebreak shuffle. Two
    /// invocations with the same seed and config produce identical
    /// pattern streams.
    pub seed: u64,
}

impl Default for EnumerateConfig {
    fn default() -> Self {
        Self {
            max_depth: 3,
            max_size: 6,
            seed: 0,
        }
    }
}

/// Enumerate every pattern up to `cfg.max_depth` and `cfg.max_size` over
/// the given alphabet, in deterministic seeded order.
///
/// The implementation is a simple bottom-up BFS: depth-1 patterns are
/// every arity-0 variant; depth-`d` patterns are every constructor of
/// arity `k > 0` whose `k` children are drawn (with replacement) from
/// the union of patterns of depth `< d`. Size is enforced post-hoc per
/// candidate.
///
/// This is deliberately not lazy / not infinite — Ruler-style synthesis
/// runs the enumerator to completion and then applies oracle + residue
/// filters. The output is bounded by `max_depth` × `max_size` × variant
/// count and remains tractable for the small alphabets these substrates
/// target (typically ≤ 16 tags).
pub fn enumerate<A, N>(cfg: &EnumerateConfig, alphabet: &A) -> impl Iterator<Item = Pattern<N>>
where
    A: Alphabet,
    N: LangNode<Alphabet = A>,
{
    let mut output: Vec<Pattern<N>> = Vec::new();
    // patterns_by_depth[d] holds every pattern of depth exactly d+1.
    // (depth is 1-indexed externally; internally we 0-index for vec
    // alignment.)
    let mut by_depth: Vec<Vec<Pattern<N>>> = Vec::with_capacity(cfg.max_depth);

    let variants = alphabet.variants();

    // Depth 1: every arity-0 variant.
    let mut depth_1: Vec<Pattern<N>> = Vec::new();
    for &(tag, arity) in variants {
        if arity != 0 {
            continue;
        }
        if let Some(p) = N::build(tag, Vec::new()) {
            if p.size() <= cfg.max_size {
                depth_1.push(p.clone());
                output.push(p);
            }
        }
    }
    seeded_shuffle(&mut depth_1, cfg.seed);
    by_depth.push(depth_1);

    // Depths 2..=max_depth: every constructor with children drawn from
    // strictly-shallower buckets, requiring at least one child to be
    // from depth d-1 (otherwise this pattern would have depth < d).
    for d in 2..=cfg.max_depth {
        let mut at_d: Vec<Pattern<N>> = Vec::new();
        // The pool of children allowed at depth d: anything with
        // depth < d. We snapshot it before generating to avoid a
        // borrow conflict.
        let allowed_pool: Vec<&Pattern<N>> = (0..d - 1)
            .flat_map(|i| by_depth[i].iter())
            .collect();
        // To force exact-depth d, at least one chosen child must be
        // from depth d-1. We track that bucket separately.
        let must_include_depth = d - 1;
        let must_include_pool: Vec<&Pattern<N>> = by_depth[must_include_depth - 1].iter().collect();

        for &(tag, arity) in variants {
            if arity == 0 {
                continue;
            }
            // Cartesian product of `allowed_pool` to the power `arity`,
            // with the constraint that ≥ 1 child be from
            // `must_include_pool` (i.e., depth d-1).
            cartesian_with_required(
                &allowed_pool,
                &must_include_pool,
                arity,
                &mut |children_refs: &[&Pattern<N>]| {
                    let owned: Vec<Pattern<N>> =
                        children_refs.iter().map(|&p| (*p).clone()).collect();
                    if let Some(p) = N::build(tag, owned) {
                        if p.depth() == d && p.size() <= cfg.max_size {
                            at_d.push(p.clone());
                            output.push(p);
                        }
                    }
                },
            );
        }
        seeded_shuffle(&mut at_d, cfg.seed.wrapping_add(d as u64));
        by_depth.push(at_d);
    }

    output.into_iter()
}

/// Cartesian product over `pool^arity`, calling `emit` for every tuple
/// that contains at least one element from `required`. Both pools
/// share the same backing patterns by reference, so this is a simple
/// pointer-equality check via slice index containment.
///
/// Used by the enumerator to ensure depth-`d` patterns actually reach
/// depth `d` (rather than accidentally stalling at depth < d when the
/// caller passed a multi-bucket pool).
fn cartesian_with_required<'a, N: LangNode>(
    pool: &[&'a Pattern<N>],
    required: &[&'a Pattern<N>],
    arity: usize,
    emit: &mut dyn FnMut(&[&'a Pattern<N>]),
) {
    let mut current: Vec<&'a Pattern<N>> = Vec::with_capacity(arity);
    fn recurse<'a, N: LangNode>(
        pool: &[&'a Pattern<N>],
        required: &[&'a Pattern<N>],
        arity: usize,
        current: &mut Vec<&'a Pattern<N>>,
        emit: &mut dyn FnMut(&[&'a Pattern<N>]),
    ) {
        if current.len() == arity {
            // At least one element of `current` must be from `required`
            // (i.e., be one of the pointers in `required`).
            let any_required = current
                .iter()
                .any(|&c| required.iter().any(|&r| std::ptr::eq(c, r)));
            if any_required {
                emit(current);
            }
            return;
        }
        for &p in pool {
            current.push(p);
            recurse(pool, required, arity, current, emit);
            current.pop();
        }
    }
    recurse(pool, required, arity, &mut current, emit);
}

/// Deterministic in-place shuffle using a SplitMix64 PRNG seeded from
/// `seed`. Equivalent in spirit to Fisher–Yates with `rand::SeedableRng`
/// but avoids the dependency for what is a one-shot tiebreak.
fn seeded_shuffle<T>(slice: &mut [T], seed: u64) {
    if slice.len() < 2 {
        return;
    }
    let mut state = seed.wrapping_add(0x9E3779B97F4A7C15);
    for i in (1..slice.len()).rev() {
        state = state.wrapping_add(0x9E3779B97F4A7C15);
        let mut z = state;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D049BB133111EB);
        z ^= z >> 31;
        let j = (z as usize) % (i + 1);
        slice.swap(i, j);
    }
}

