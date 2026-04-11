//! `TierFollowsMaterialization` — unary domain clamp linking
//! AF.1's materialization classifier to AF.4's `EmissionTier`
//! lattice.
//!
//! Every rule in the component carries one `EmissionTier`
//! variable. The installer walks each rule in the component,
//! reads its materialization class from
//! [`ConstraintCtx::mat_classes`], and prunes every value from
//! the tier variable's domain that is not legal for that class:
//!
//! - `MustTape` → `{Tape}` only.
//!   A rule pinned to `MustTape` cannot emit a Tier B direct
//!   shim — the shim bypasses the tape record the pin demands.
//!   Tier C (`Lazy`) also requires an elidable tape shape for
//!   the view-layer projection to dispatch through, which
//!   `MustTape` does not guarantee.
//!
//! - `TapeSpanOnly` → `{Tape, Lazy}`.
//!   Single-record span leaves can emit a lazy view-layer slot
//!   that dispatches between the span walk and an eager
//!   typed value — but Tier B requires `FixedShape`, which
//!   `TapeSpanOnly` explicitly does not guarantee (the span's
//!   byte length is variable). `Direct` is pruned.
//!
//! - `TransparentElide` → `{Tape, Lazy, Direct}`.
//!   A rule that's structurally eligible for elision is also
//!   eligible for Tier B direct-to-struct projection: its body
//!   is pure-conversion, its shape is fixed, and its
//!   descendants are uniformly elidable. Every tier is legal;
//!   the cost model picks the cheapest.
//!
//! # Why a unary clamp, not a pairwise constraint
//!
//! The class is a monotone fact of the rule body — already
//! computed bottom-up by `classify_materialization` before the
//! CSP solve begins. No other CSP variable can weaken or
//! strengthen it, so there is no pairwise propagation to
//! perform. Pruning the tier domain at variable-construction
//! time gives the search the smallest possible domain without
//! waiting for AC-3 to discover the restriction.
//!
//! The constraint is the link from AF.1's `EClassFacts`
//! pre-seed (via the materialization class) to AF.4's
//! `EmissionTier` lattice — a rule that's structurally eligible
//! for elision is eligible for Tier B, and one that's not is
//! pinned to Tape.

use csp_solver::Csp;

use super::ConstraintCtx;
use crate::passes::csp_strategy::{StrategyDomain, StrategyValue};
use crate::passes::materialization::EmissionTier;
use crate::GrammarIR;

/// Install the `TierFollowsMaterialization` unary clamp on
/// every rule in the component.
///
/// The clamp walks every `RuleId` in `ctx.component`, looks up
/// its tier variable id (if any), resolves its materialization
/// class via [`ConstraintCtx::rule_materialization`], then
/// prunes every domain value not in the legal-tier set for that
/// class. Returns the number of distinct value prunes applied
/// across the component — callers use the count for reporting
/// and to confirm the clamp actually fired.
///
/// Variables without an entry in `ctx.tier_vars` are silently
/// skipped (the rule has no tier variable, typically because
/// the parent dispatcher elided it for a trivial rule).
pub fn install(ctx: &ConstraintCtx<'_>, csp: &mut Csp<StrategyDomain>, ir: &GrammarIR) -> usize {
    use csp_solver::domain::Domain;

    let mut pruned = 0usize;

    for &rule in ctx.component {
        let Some(&var) = ctx.tier_vars.get(&rule) else {
            continue;
        };

        let class = ctx.rule_materialization(ir, rule);
        let legal = ConstraintCtx::legal_tiers(class);

        let domain = &mut csp.variables[var as usize].domain;

        // Collect values that fall outside the legal set. We
        // don't mutate the domain while iterating it — snapshot
        // the values first, then prune.
        let victims: Vec<StrategyValue> = domain
            .values()
            .into_iter()
            .filter(|v| match v {
                StrategyValue::Tier(tier) => !legal.contains(tier),
                // Defensive: the domain is tier-only by
                // construction, but we leave non-tier values
                // alone if the dispatcher hands us a mixed
                // domain.
                _ => false,
            })
            .collect();

        for v in &victims {
            if domain.remove(v) {
                pruned += 1;
            }
        }

        // If the clamp emptied the domain, the rule is
        // structurally unsatisfiable — the materialization
        // class is incompatible with every tier we could build.
        // That should never happen (Tape is universal), but we
        // re-install Tape as a safety net rather than returning
        // an unsatisfiable CSP.
        if domain.size() == 0 {
            domain.add(&StrategyValue::Tier(EmissionTier::Tape));
        }
    }

    pruned
}
