//! Foundational types for the rewrite-rule substrate.
//!
//! Defines the alphabet- and node-agnostic [`Pattern`] type and
//! supporting witness primitives that the per-grammar rule storage
//! layer (in `mod.rs`, `rank.rs`, `tiering.rs`, `schema.rs`) builds
//! on. Recycled BA's rule-discovery substrate consumes these
//! primitives once it lands.
//!
//! The design follows three rules:
//!
//! - **No circular crate edge.** `crates/ir/` already depends on
//!   `crates/egraph/` for the `Language` / `EGraph` machinery; this
//!   module keeps the storage substrate alphabet- and node-agnostic
//!   so downstream consumers can specialise per-grammar without
//!   rewiring the foundation.
//!
//! - **RON-serialisable from day one.** Every type in this module is
//!   `Serialize + Deserialize`. Pattern bodies that hold opaque
//!   downstream content (e.g. an interned `egraph::Id` referring to a
//!   per-grammar rule storage) carry a [`PatternRef`] payload — a
//!   stable string handle plus an optional numeric tag. Round-trip is
//!   tested at the schema layer.
//!
//! - **`base` re-exports nothing else.** The other rewrite modules
//!   (`mod`, `rank`, `tiering`, `schema`) build on these primitives;
//!   this module never imports from them.

use serde::{Deserialize, Serialize};

/// An alphabet symbol used by an [`Atom::Term`] leaf — the `terminal'
/// alphabet of the surrounding grammar collapsed to the smallest enum
/// the rewrite-rule storage needs.
///
/// `bbnf-ir` works with byte-shaped grammars (`bbnf_regex::CharSet128`
/// covers the substrate); this enum is the lossy projection that's
/// stable across grammars. Class-1/2 elimination rules need only
/// distinguish *anonymous* terminals (a particular byte) from
/// *grammar-named* terminals (a [`StringId`]-backed reference); the
/// bbnf-regex DFA tier carries the exact bytes.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Alphabet {
    /// A literal byte.
    Byte(u8),
    /// A named terminal — references a grammar-internal string id but
    /// the rewrite layer doesn't dereference (storage-format detail).
    Named(String),
    /// `epsilon` — the empty match.
    Epsilon,
}

/// A reference to a grammar-internal entity (a rule, an interned
/// string, an e-graph node id) that survives serialisation. Stored as
/// a stable name + numeric tag pair: the name is grammar-stable, the
/// tag is the in-memory id at *serialisation time* and is recomputed
/// when the rule file is loaded against a fresh grammar.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PatternRef {
    /// Stable name (grammar-author-assigned).
    pub name: String,
    /// Optional numeric tag — present at serialisation, advisory at
    /// load. `None` for refs minted in-memory before serialisation.
    pub tag: Option<u32>,
}

impl PatternRef {
    /// Construct a ref by name only (tag is filled in by the
    /// re-resolution pass at load time).
    pub fn by_name(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            tag: None,
        }
    }
}

/// The leaf shape inside a [`Pattern`]. Mirrors the `IrNode` /
/// `GrammarENode` leaf set without committing to either — the schema
/// layer round-trips this enum verbatim.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Atom {
    /// A literal alphabet element.
    Term(Alphabet),
    /// A named non-terminal reference (resolved against the grammar
    /// rule table at load).
    Rule(PatternRef),
    /// A pattern variable bound by the LHS — substituted into the RHS
    /// during apply. Variables are 0-indexed within a rule.
    Var(u32),
    /// The empty pattern.
    Epsilon,
}

/// An owned rewrite-rule pattern tree. Mirrors `IrNode` shape
/// (Seq / Alt / Repeat / Skip / Next / Minus / Negate / Map) without
/// taking the IR's own definition — this lets the rewrite substrate
/// round-trip RON files authored against a different grammar than the
/// one currently loaded.
///
/// Recycled BA (post-AZ-IV) lowers a [`Pattern`] into the per-grammar
/// e-graph shape via an adapter authored alongside the rule-discovery
/// cohort; storage carries the abstract representation directly so
/// the substrate is independent of any specific node language.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Pattern {
    /// A leaf atom.
    Atom(Atom),
    /// Sequential concatenation.
    Seq(Vec<Pattern>),
    /// Ordered alternation.
    Alt(Vec<Pattern>),
    /// Repetition with `lo..=hi` count bounds.
    Repeat {
        /// The repeating sub-pattern.
        inner: Box<Pattern>,
        /// Lower bound (inclusive).
        lo: u32,
        /// Upper bound (inclusive). `u32::MAX` ≈ unbounded.
        hi: u32,
    },
    /// `a << b` — left-biased sequence.
    Skip(Box<Pattern>, Box<Pattern>),
    /// `a >> b` — right-biased sequence.
    Next(Box<Pattern>, Box<Pattern>),
    /// Set-difference: `lhs` minus `rhs`.
    Minus(Box<Pattern>, Box<Pattern>),
    /// Negative lookahead.
    Negate(Box<Pattern>),
    /// Host-fn map; the fn name is opaque storage (resolved against
    /// the host-fn table at load).
    Map {
        /// Mapped sub-pattern.
        inner: Box<Pattern>,
        /// Host fn handle.
        fn_ref: PatternRef,
    },
}

impl Pattern {
    /// Recursive AST node count. Used by the ranker `size` axis and
    /// by the [`crate::rewrites::tiering::classify`] strict-shrink check.
    pub fn ast_size(&self) -> usize {
        match self {
            Pattern::Atom(_) => 1,
            Pattern::Seq(xs) | Pattern::Alt(xs) => {
                1 + xs.iter().map(Pattern::ast_size).sum::<usize>()
            }
            Pattern::Repeat { inner, .. } | Pattern::Negate(inner) | Pattern::Map { inner, .. } => {
                1 + inner.ast_size()
            }
            Pattern::Skip(a, b) | Pattern::Next(a, b) | Pattern::Minus(a, b) => {
                1 + a.ast_size() + b.ast_size()
            }
        }
    }

    /// Multi-set of [`Atom`] leaves reachable from this pattern. Used
    /// by the ranker `similarity` axis (Jaccard distance is the
    /// symmetric difference / union ratio of the leaf multisets).
    pub fn atoms(&self) -> Vec<Atom> {
        let mut out = Vec::new();
        self.atoms_into(&mut out);
        out
    }

    fn atoms_into(&self, out: &mut Vec<Atom>) {
        match self {
            Pattern::Atom(a) => out.push(a.clone()),
            Pattern::Seq(xs) | Pattern::Alt(xs) => {
                for x in xs {
                    x.atoms_into(out);
                }
            }
            Pattern::Repeat { inner, .. } | Pattern::Negate(inner) | Pattern::Map { inner, .. } => {
                inner.atoms_into(out)
            }
            Pattern::Skip(a, b) | Pattern::Next(a, b) | Pattern::Minus(a, b) => {
                a.atoms_into(out);
                b.atoms_into(out);
            }
        }
    }
}

/// Provenance for a rule: how it was discovered, which oracle accepted
/// it, and a serialisable trace the auditor can replay. BB.close
/// (Wave 4) populates this from the ruler's CVC enumerator + VM
/// oracle; the authoring path leaves [`Witness::Authored`].
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Witness {
    /// Hand-written rule (Tranche H rules, fixture rules).
    Authored {
        /// Free-text rationale.
        note: String,
    },
    /// Discovered by the CVC enumerator + e-graph fast-path equality
    /// check (Class-1 / Class-2 candidates).
    EgraphEquiv {
        /// Number of seed inputs the enumerator agreed on before
        /// promoting the equivalence.
        seed_count: u32,
    },
    /// Discovered by the VM oracle (Class-3 candidates that the
    /// e-graph fast-path can't decide).
    VmOracle {
        /// Number of randomised inputs the oracle agreed on.
        sample_count: u32,
        /// Inputs disagreed on (must be 0 to accept).
        counterexamples: u32,
    },
}

impl Witness {
    /// Whether this witness is sufficient to accept the rule
    /// unconditionally (no oracle counter-examples).
    pub fn is_sound(&self) -> bool {
        match self {
            Witness::Authored { .. } => true,
            Witness::EgraphEquiv { seed_count } => *seed_count > 0,
            Witness::VmOracle {
                sample_count,
                counterexamples,
            } => *sample_count > 0 && *counterexamples == 0,
        }
    }
}
