//! VM-oracle equivalence check for candidate rule pairs.
//!
//! Validates `(lhs, rhs)` candidates by interpreting both terms against a
//! per-grammar [`Interpreter`] across a sample input domain. Two terms
//! are declared equivalent iff the interpreter produces identical
//! outputs on every witness.
//!
//! Reference: Nandi et al., Ruler (OOPSLA 2021), §3.2 "Validation". The
//! sample-input fingerprint approach is theirs — a small set of
//! witnesses gives high-confidence equivalence at a tiny fraction of
//! the cost of a full proof obligation.
//!
//! This module is *grammar-agnostic*: the [`Interpreter`] trait abstracts
//! away both the term language ([`LangNode`]) and the input/output
//! domain. JSON, CSS, BBNF, and arithmetic interpreters all plug in
//! through the same surface.

use std::time::{Duration, Instant};

use crate::ruler::enumerate::{LangNode, Pattern};

/// Per-grammar interpreter.
///
/// A consumer implements `Interpreter` to give the oracle a way to
/// evaluate candidate terms on concrete inputs. The output type carries
/// `Eq` so the oracle can compare two evaluations directly; `Debug` is
/// required so divergence reports can name the exact value mismatch.
pub trait Interpreter<N: LangNode> {
    /// The interpreter's output value type.
    type Output: Eq + std::fmt::Debug + Clone;

    /// The interpreter's input type. For pure terms (no inputs) this
    /// can be `()`; for grammar-style terms with free variables it is
    /// typically a per-variable assignment map.
    type Input: Clone + std::fmt::Debug;

    /// Evaluate `term` under `input`. Must be deterministic — the
    /// oracle relies on `eval(t, x) == eval(t, x)` for every `(t, x)`.
    fn eval(&self, term: &Pattern<N>, input: &Self::Input) -> Self::Output;

    /// Sample inputs used as witnesses. The oracle declares two terms
    /// equivalent iff their outputs agree on every input in this set;
    /// a richer sample set buys higher confidence at proportional
    /// validation cost. Implementations should return a stable order
    /// for reproducibility.
    fn sample_inputs(&self) -> Vec<Self::Input>;
}

/// Oracle configuration knobs.
#[derive(Clone, Copy, Debug)]
pub struct OracleConfig {
    /// Minimum number of agreeing witnesses before declaring
    /// equivalence. The oracle still evaluates every sample input —
    /// this knob just gates the verdict so a narrow witness set
    /// can't accidentally short-circuit (e.g., when `sample_inputs`
    /// returns one witness and that witness happens to agree).
    pub min_witness_count: usize,

    /// Wall-clock cap on a single equivalence check. Each `eval` call
    /// is unbounded, but the oracle re-checks the elapsed time
    /// between witnesses and bails with [`EquivalenceResult::Timeout`]
    /// when the cap is hit. Set to a very large duration to
    /// effectively disable the timeout.
    pub timeout: Duration,
}

impl Default for OracleConfig {
    fn default() -> Self {
        Self {
            min_witness_count: 1,
            timeout: Duration::from_secs(1),
        }
    }
}

/// Outcome of a single oracle check.
#[derive(Debug)]
pub enum EquivalenceResult<I: std::fmt::Debug, O: std::fmt::Debug> {
    /// All witnesses agreed. The pair is equivalent (modulo the
    /// witness set's strength).
    Equivalent,
    /// At least one witness disagreed. The first divergent witness
    /// and the two outputs are reported for debugging.
    Diverge {
        /// The input on which `lhs` and `rhs` disagreed.
        input: I,
        /// `lhs`'s output on that input.
        lhs_out: O,
        /// `rhs`'s output on that input.
        rhs_out: O,
    },
    /// The configured timeout was exceeded before all witnesses could
    /// be checked. Treat as inconclusive — the pair *may* be
    /// equivalent, but the oracle didn't get to prove it.
    Timeout,
    /// The witness set was smaller than `min_witness_count`. Treat as
    /// inconclusive — the oracle declines to rule on a too-narrow
    /// sample.
    Inconclusive,
}

/// Check whether `lhs` and `rhs` are extensionally equivalent under
/// `interpreter`'s sample inputs.
///
/// Returns `Equivalent` iff every witness agrees and the witness count
/// meets `cfg.min_witness_count`; `Diverge` on the first disagreement;
/// `Timeout` when wall-clock cap is exceeded; `Inconclusive` when the
/// witness set was too narrow.
pub fn check_equivalence<N, I>(
    cfg: &OracleConfig,
    interpreter: &I,
    lhs: &Pattern<N>,
    rhs: &Pattern<N>,
) -> EquivalenceResult<I::Input, I::Output>
where
    N: LangNode,
    I: Interpreter<N>,
{
    let inputs = interpreter.sample_inputs();
    if inputs.len() < cfg.min_witness_count {
        return EquivalenceResult::Inconclusive;
    }

    let start = Instant::now();
    for input in inputs {
        if start.elapsed() > cfg.timeout {
            return EquivalenceResult::Timeout;
        }
        let lhs_out = interpreter.eval(lhs, &input);
        let rhs_out = interpreter.eval(rhs, &input);
        if lhs_out != rhs_out {
            return EquivalenceResult::Diverge {
                input,
                lhs_out,
                rhs_out,
            };
        }
    }
    EquivalenceResult::Equivalent
}
