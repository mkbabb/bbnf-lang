//! `EnginePropagation` — cross-rule pairwise equality on
//! [`RegexEngine`] variables within the same component.
//!
//! # Rationale
//!
//! Regex engine choice carries fixed amortized overhead: the
//! DFA's state-table construction, the aho-corasick corpus
//! builder, the nibble LUT's class-to-bitmask materialization,
//! the `OnePass` prelude's per-grammar setup. Every one of
//! these costs is paid once per engine instance and repaid
//! across every site that uses it.
//!
//! A per-rule CSP solver that picks `RegexEngine::Dfa` for one
//! rule and `RegexEngine::SmallDfa` for the rule's caller is
//! leaving amortization on the table — the component pays the
//! DFA startup once *and* the SmallDfa startup once, for two
//! sites that could have shared a single DFA instance. The
//! constraint closes this gap by unifying the domains of every
//! regex variable within a component: once one site commits to
//! `Dfa`, every other site that carries a regex variable is
//! pinned to `Dfa` too.
//!
//! # `FamilyHelper` exemption
//!
//! `FamilyHelper` is a named function call (e.g.,
//! `quoted_string_scan_full`, `scan_json_number_fused`) —
//! not a compiled automaton. It carries zero startup cost and
//! shares no state-table infrastructure with the DFA/SmallDfa/
//! OnePass/NibbleLut/memchr family. Forcing a `FamilyHelper`-
//! eligible pattern to use `Dfa` because a sibling in the same
//! SCC component can only use `Dfa` replaces a single SIMD
//! helper call with a 100+ line inline HIR per-byte loop — the
//! opposite of amortization.
//!
//! The installer therefore **excludes** any variable whose
//! domain contains `FamilyHelper` from the pairwise equality
//! entirely. Variables that lack `FamilyHelper` (i.e., patterns
//! that can only use compiled engines) still participate in the
//! equality, preserving amortization among DFA/SmallDfa/etc.
//! sites. `FamilyHelper`-eligible variables are left free to
//! pick `FamilyHelper` via the cost model, which always prefers
//! it (lowest tier cost in `build_engine_domain`).
//!
//! # Encoding
//!
//! For every pair of regex variables `(a, b)` within a
//! component whose domains could hold the same engine, install
//! an [`ImplicationConstraint`] for every **compiled** engine
//! value `v` that triggers `a = v ⇒ b ∈ {v}`. Each implication
//! is a hard equality-forcing edge — the consequent's domain
//! collapses to a single value the moment the antecedent binds.
//!
//! The constraint is symmetric: the installer emits one
//! implication per direction per engine value, so whichever
//! variable is assigned first pulls the other to match. The
//! constraint count grows as
//! `O(|component_regex_vars|^2 * |compiled_engines|)` per
//! component; in practice the compiled-engine domain is ≤ 7 and
//! most components have ≤ 2 regex variables, keeping the growth
//! linear on real grammars.
//!
//! # Integration with the per-rule `ImplicationConstraint`s
//!
//! The pre-existing intra-rule constraint
//! (`csp_strategy::mod::add_token_dispatch_constraints`) wires
//! an Alt parent's byte-dispatch decision to its child regex
//! engines, enforcing the one-pass-eligible subset. The two
//! constraint families compose cleanly: a byte-dispatch Alt
//! forces its child regex to be one-pass-eligible, and the
//! cross-rule propagation then forces every other regex in the
//! component to match. If the CSP can't satisfy both (e.g., a
//! cross-rule pair contains a byte-dispatch child that must be
//! one-pass-eligible and an unrelated site that would prefer
//! `Dfa`), branch-and-bound backtracks and picks an assignment
//! consistent with both.
//!
//! The [`engine`] installer itself consults no cost weights —
//! the amortization reward is already baked into the per-site
//! engine domain (tier-priced via `build_engine_domain` in the
//! parent module). This installer's job is purely structural:
//! enforce the "same engine per component" invariant across the
//! compiled-engine family and let the existing cost model decide
//! which engine the component should settle on.

use csp_solver::constraint::{ImplicationConstraint, VarId};
use csp_solver::Csp;

use super::ConstraintCtx;
use crate::passes::csp_strategy::{RegexEngine, StrategyDomain, StrategyValue};
use crate::GrammarIR;

/// Install the `EnginePropagation` pairwise equality
/// constraint across every regex variable in the component.
///
/// Walks `ctx.engine_vars` filtered to the rules listed in
/// `ctx.component`, collects the unique `VarId`s, and for each
/// ordered pair `(a, b)` with `a != b` emits one
/// `ImplicationConstraint` per [`RegexEngine`] value binding
/// the two variables to identical engines. Returns the number
/// of implication constraints added.
pub fn install(ctx: &ConstraintCtx<'_>, csp: &mut Csp<StrategyDomain>, _ir: &GrammarIR) -> usize {
    // Gather every engine variable owned by a rule in this
    // component. `engine_vars` is keyed by `(RuleId, NodeId)`
    // because a single rule may carry multiple regex sites;
    // the pair filter walks them uniformly.
    //
    // Variables whose domain contains `FamilyHelper` are excluded
    // from the pairwise equality entirely. `FamilyHelper` is a
    // named SIMD function call with zero startup cost — including
    // it in the pairwise equality (even in the consequent's
    // allowed set) creates constraint interactions that can drag
    // `FamilyHelper`-eligible patterns to compiled engines during
    // branch-and-bound search. The clean fix: skip any variable
    // that can use `FamilyHelper`, letting the cost model pick it
    // independently.
    let family_helper_val = StrategyValue::Engine(RegexEngine::FamilyHelper);
    let mut vars: Vec<VarId> = ctx
        .engine_vars
        .iter()
        .filter(|((rule, _), _)| ctx.component.contains(rule))
        .filter(|(_, v)| {
            // Exclude variables whose domain includes FamilyHelper.
            let domain = &csp.variables[**v as usize].domain;
            !domain.values.contains(&family_helper_val)
        })
        .map(|(_, &v)| v)
        .collect();

    // Deduplicate so a shared pattern across two rules does not
    // self-constrain (a variable equal to itself is a no-op in
    // the CSP but wastes solver work).
    vars.sort_unstable();
    vars.dedup();

    if vars.len() < 2 {
        // Zero or one regex variable in the component — the
        // propagation constraint has no pair to wire. Return
        // early so callers can tell the installer was trivially
        // satisfied.
        return 0;
    }

    // Only propagate across compiled engines. `FamilyHelper`-
    // eligible variables are already filtered out above, so every
    // surviving variable holds only compiled-engine values.
    let engines: &[RegexEngine] = &[
        RegexEngine::Memchr1,
        RegexEngine::Memchr2,
        RegexEngine::Memchr3,
        RegexEngine::NibbleLut,
        RegexEngine::OnePass,
        RegexEngine::SmallDfa,
        RegexEngine::Dfa,
    ];

    let mut count = 0usize;
    for i in 0..vars.len() {
        for j in 0..vars.len() {
            if i == j {
                continue;
            }
            let a = vars[i];
            let b = vars[j];
            for engine in engines {
                // `a = engine ⇒ b ∈ {engine}` — pairwise equality
                // for compiled-engine amortization, applied only to
                // variables that lack `FamilyHelper`.
                let trigger = StrategyValue::Engine(engine.clone());
                let allowed = vec![StrategyValue::Engine(engine.clone())];
                csp.add_constraint(ImplicationConstraint::new(a, b, trigger, allowed));
                count += 1;
            }
        }
    }

    count
}
