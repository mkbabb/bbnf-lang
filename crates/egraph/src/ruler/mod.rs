//! Ruler-style rule synthesis substrate.
//!
//! Three orthogonal concerns:
//!
//! - [`enumerate`]: candidate-equivalence-class (CVC) enumeration. Generates
//!   left-hand-side patterns up to a configurable depth/size, parameterised
//!   by an [`Alphabet`] (terminal/non-terminal vocabulary) and a [`LangNode`]
//!   (per-grammar AST node enum).
//! - [`oracle`]: VM-based equivalence checker. Given two terms, evaluates
//!   both against an [`Interpreter`] across a sample input domain and
//!   declares them equivalent IFF outputs agree on all witnesses.
//! - [`residue`]: e-graph fast-path filter. Pre-discharges pairs the e-graph
//!   can already prove equivalent (by seeded rewrites + structural
//!   canonicalisation), leaving only the *residue* — the pairs the oracle
//!   must actually check.
//!
//! References:
//!
//! - Nandi et al., "Rewrite Rule Inference Using Equality Saturation" (Ruler,
//!   OOPSLA 2021). The CVC + oracle + e-graph filter pipeline is theirs.
//! - Wang et al., "SpEQ: Speeding Up Equality Saturation" (Theia, 2023). The
//!   "residue" framing — discharge what the e-graph can, oracle the rest —
//!   tracks Theia's lemma-mining pipeline.
//! - Willsey et al., "egg: Fast and Extensible Equality Saturation" (POPL
//!   2021). The underlying e-graph data structure used by [`residue`].

pub mod enumerate;
pub mod oracle;
pub mod residue;

pub use enumerate::{Alphabet, EnumerateConfig, LangNode, Pattern, enumerate};
pub use oracle::{EquivalenceResult, Interpreter, OracleConfig, check_equivalence};
pub use residue::ResidueFilter;
