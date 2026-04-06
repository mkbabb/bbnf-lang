//! Pass 2.6: Dispatch table generation from IR FIRST sets.
//!
//! Walks the entire IR tree and annotates any `Alt` node with a dispatch table
//! when all branches have pairwise disjoint FIRST sets.
//!
//! When FOLLOW sets are available and an Alt branch is nullable, the pass uses
//! `FOLLOW(containing_rule)` to include the nullable branch in the dispatch
//! table — if `next_char ∈ FOLLOW(rule)` and FOLLOW is disjoint from other
//! branches' FIRST sets, the nullable branch gets O(1) dispatch instead of
//! falling through as a linear fallback.
//!
//! ## CSP Pre-computation
//!
//! Before the main tree walk, `precompute_dispatch_eligibility` collects every
//! Alt node (identified by pointer), resolves its effective FIRST sets, and uses
//! a `csp_solver::Csp<DispatchDomain>` with AC-3 propagation to determine
//! dispatchability in a single pass. The results are stored in a side table
//! (`DispatchEligibility`) that `annotate_node` reads — avoiding redundant
//! pairwise FIRST set recomputation during the tree walk.

use std::collections::HashMap;

use rayon::prelude::*;

use csp_solver::Csp;
use csp_solver::constraint::{Constraint, Revision, VarId};
use csp_solver::domain::{Domain, LatticeDomain};
use csp_solver::variable::Variable;

use crate::{AltBranch, AltDispatch, CharSet128, GrammarIR, IrNode, regex_first};

// ── CSP Domain for Dispatch Eligibility ───────────────────────────────────────

/// Tri-state dispatch eligibility: Unknown → Dispatchable or NonDispatchable.
///
/// Models each Alt node as a CSP variable whose domain monotonically
/// converges from `Unknown` to a concrete decision.
#[derive(Clone, Debug, PartialEq)]
enum DispatchDecision {
    Unknown,
    Dispatchable,
    NonDispatchable,
}

/// Lattice domain wrapping `DispatchDecision`.
///
/// Bottom = Unknown. Once resolved to Dispatchable/NonDispatchable, the
/// domain is a singleton — no search needed, AC-3 propagation suffices.
#[derive(Clone, Debug, PartialEq)]
struct DispatchDomain {
    decision: DispatchDecision,
}

impl DispatchDomain {
    fn unknown() -> Self {
        Self {
            decision: DispatchDecision::Unknown,
        }
    }
}

impl Domain for DispatchDomain {
    type Value = DispatchDecision;

    fn size(&self) -> usize {
        1
    }

    fn is_singleton(&self) -> bool {
        true
    }

    fn singleton_value(&self) -> Option<Self::Value> {
        Some(self.decision.clone())
    }

    fn contains(&self, val: &Self::Value) -> bool {
        self.decision == *val
    }

    fn remove(&mut self, _val: &Self::Value) -> bool {
        false
    }

    fn add(&mut self, _val: &Self::Value) {}

    fn values(&self) -> Vec<Self::Value> {
        vec![self.decision.clone()]
    }
}

impl LatticeDomain for DispatchDomain {
    fn bottom() -> Self {
        Self::unknown()
    }

    fn join(&mut self, other: &Self) -> bool {
        match (&self.decision, &other.decision) {
            (DispatchDecision::Unknown, d) if *d != DispatchDecision::Unknown => {
                self.decision = d.clone();
                true
            }
            _ => false,
        }
    }
}

// ── CSP Constraint: Disjoint FIRST sets ───────────────────────────────────────

/// Constraint that resolves an Alt's dispatch eligibility from its branch FIRST
/// sets. Checks pairwise disjointness and immediately assigns the result.
#[derive(Debug)]
struct DisjointConstraint {
    var: VarId,
    dispatchable: bool,
}

impl DisjointConstraint {
    fn new(var: VarId, dispatchable: bool) -> Self {
        Self { var, dispatchable }
    }
}

impl Constraint<DispatchDomain> for DisjointConstraint {
    fn scope(&self) -> &[VarId] {
        std::slice::from_ref(&self.var)
    }

    fn check(&self, assignment: &[Option<DispatchDecision>]) -> bool {
        match &assignment[self.var as usize] {
            Some(d) => {
                if self.dispatchable {
                    *d == DispatchDecision::Dispatchable
                } else {
                    *d == DispatchDecision::NonDispatchable
                }
            }
            None => true,
        }
    }

    fn revise(&self, vars: &mut [Variable<DispatchDomain>], _depth: usize) -> Revision {
        let target = if self.dispatchable {
            DispatchDecision::Dispatchable
        } else {
            DispatchDecision::NonDispatchable
        };
        let slot = &mut vars[self.var as usize].domain.decision;
        if *slot == DispatchDecision::Unknown {
            *slot = target;
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

// ── Pre-computed eligibility table ────────────────────────────────────────────

/// Pre-computed dispatch eligibility for all Alt nodes, keyed by pointer.
type DispatchEligibility = HashMap<usize, bool>;

/// Collect all Alt nodes from the IR tree and determine dispatch eligibility
/// via CSP propagation.
///
/// For each Alt node with >=2 branches and <=127 branches, checks pairwise
/// FIRST set disjointness (without FOLLOW context). The result maps Alt
/// pointer → bool. The tree walk uses this to skip redundant disjointness
/// checks for non-dispatchable Alts.
fn precompute_dispatch_eligibility(
    ir: &GrammarIR,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
) -> DispatchEligibility {
    let mut alts: Vec<(usize, Vec<Option<CharSet128>>)> = Vec::new();

    // Phase 1: Collect every Alt node in the IR.
    for rule in &ir.rules {
        collect_alts(&rule.body, rule_metas, strings, &mut alts);
        if let Some(ref recover) = rule.meta.directives.recover {
            collect_alts(recover, rule_metas, strings, &mut alts);
        }
    }

    if alts.is_empty() {
        return HashMap::new();
    }

    // Phase 2: Build CSP — one variable per Alt, one constraint per Alt.
    let mut csp: Csp<DispatchDomain> = Csp::new();
    let mut var_ids: Vec<VarId> = Vec::with_capacity(alts.len());
    let mut alt_ptrs: Vec<usize> = Vec::with_capacity(alts.len());

    for (ptr, branch_firsts) in &alts {
        let var = csp.add_variable(DispatchDomain::unknown());
        var_ids.push(var);
        alt_ptrs.push(*ptr);

        let dispatchable = is_pairwise_disjoint(branch_firsts);
        csp.add_constraint(DisjointConstraint::new(var, dispatchable));
    }

    // Phase 3: Propagate.
    let _ = csp.propagate_monotonic();

    // Phase 4: Extract results.
    let mut result = HashMap::with_capacity(alts.len());
    for (i, ptr) in alt_ptrs.iter().enumerate() {
        let decision = &csp.variables[var_ids[i] as usize].domain.decision;
        let eligible = matches!(decision, DispatchDecision::Dispatchable);
        result.insert(*ptr, eligible);
    }

    result
}

/// Check if a set of branch FIRST sets are pairwise disjoint.
fn is_pairwise_disjoint(branch_firsts: &[Option<CharSet128>]) -> bool {
    let sets: Vec<&CharSet128> = match branch_firsts
        .iter()
        .map(|f| f.as_ref())
        .collect::<Option<Vec<_>>>()
    {
        Some(v) => v,
        None => return false,
    };

    for i in 0..sets.len() {
        for j in (i + 1)..sets.len() {
            if !sets[i].is_disjoint(sets[j]) {
                return false;
            }
        }
    }
    true
}

/// Recursively collect Alt nodes and their branch FIRST sets.
fn collect_alts(
    node: &IrNode,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
    out: &mut Vec<(usize, Vec<Option<CharSet128>>)>,
) {
    match node {
        IrNode::Alt(branches, dispatch) => {
            for branch in branches {
                collect_alts(&branch.node, rule_metas, strings, out);
            }

            if dispatch.is_none() && branches.len() >= 2 && branches.len() <= 127 {
                let ptr = node as *const IrNode as usize;
                let firsts: Vec<Option<CharSet128>> = branches
                    .iter()
                    .map(|b| {
                        b.first_set
                            .clone()
                            .or_else(|| node_first_set(&b.node, rule_metas, strings))
                    })
                    .collect();
                out.push((ptr, firsts));
            }
        }
        IrNode::Seq(children) => {
            for child in children {
                collect_alts(child, rule_metas, strings, out);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => {
            collect_alts(inner, rule_metas, strings, out);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            collect_alts(a, rule_metas, strings, out);
            collect_alts(b, rule_metas, strings, out);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            collect_alts(token, rule_metas, strings, out);
            for arm in arms {
                collect_alts(&arm.continuation, rule_metas, strings, out);
            }
            collect_alts(fallback, rule_metas, strings, out);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}

// ── Main Pass ────────────────────────────────────────────────────────────────

/// Generate dispatch tables for all eligible Alt nodes in the IR.
///
/// First runs CSP pre-computation to classify all Alts as dispatchable or not,
/// then walks the tree to annotate eligible Alts with `AltDispatch` tables.
///
/// Uses contextual FOLLOW sets for nullable branch dispatch: within a Seq,
/// a nested Alt uses `FIRST(suffix)` (the first set of the remaining sequence
/// elements), not the rule-level FOLLOW. This prevents incorrect dispatch
/// table entries for nullable branches in non-tail positions.
pub fn generate_dispatch_tables(ir: &mut GrammarIR) {
    // Clone follow sets, strings, and rules metadata to avoid borrow conflicts.
    let follow_sets = ir.follow_sets.clone();
    let strings = ir.strings.clone();
    let rule_metas: Vec<(CharSet128, bool)> = ir
        .rules
        .iter()
        .map(|r| (r.meta.first_set.clone(), r.meta.nullable))
        .collect();

    // Pre-compute dispatch eligibility for all Alt nodes via CSP.
    let eligibility = precompute_dispatch_eligibility(ir, &rule_metas, &strings);

    if ir.rules.len() >= 16 {
        ir.rules.par_iter_mut().for_each(|rule| {
            let follow = follow_sets.get(&rule.id);
            annotate_node(
                &mut rule.body,
                follow,
                &rule_metas,
                &strings,
                &eligibility,
            );
            if let Some(ref mut recover) = rule.meta.directives.recover {
                annotate_node(recover, follow, &rule_metas, &strings, &eligibility);
            }
        });
    } else {
        for rule in &mut ir.rules {
            let follow = follow_sets.get(&rule.id);
            annotate_node(
                &mut rule.body,
                follow,
                &rule_metas,
                &strings,
                &eligibility,
            );
            if let Some(ref mut recover) = rule.meta.directives.recover {
                annotate_node(recover, follow, &rule_metas, &strings, &eligibility);
            }
        }
    }
}

/// Compute the FIRST set of an IrNode.
///
/// Returns `Some(charset)` for non-nullable nodes, `None` for nullable ones.
/// `rule_metas` provides pre-computed (first_set, nullable) for rule references.
fn node_first_set(
    node: &IrNode,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
) -> Option<CharSet128> {
    match node {
        IrNode::Literal(sid) => {
            let s = &strings[*sid as usize];
            if let Some(b) = s.bytes().next() {
                let mut cs = CharSet128::new();
                cs.add(b);
                Some(cs)
            } else {
                None // empty literal = nullable
            }
        }
        IrNode::Regex(sid) => {
            let pattern = &strings[*sid as usize];
            regex_first::regex_first_chars(pattern).filter(|cs| !cs.is_empty())
        }
        IrNode::Ref(rule_id) => {
            let (ref first_set, nullable) = rule_metas[*rule_id as usize];
            if nullable {
                None
            } else {
                Some(first_set.clone())
            }
        }
        IrNode::Seq(children) => {
            let mut combined = CharSet128::new();
            for child in children {
                if let Some(fs) = node_first_set(child, rule_metas, strings) {
                    combined.union(&fs);
                    return Some(combined); // non-nullable child stops propagation
                }
                // Child is nullable — continue to include next child's FIRST
                if let Some(fs) = node_first_set_nullable_part(child, rule_metas) {
                    combined.union(&fs);
                }
            }
            None // all children nullable
        }
        IrNode::Alt(branches, _) => {
            let mut combined = CharSet128::new();
            for b in branches {
                if let Some(ref fs) = b.first_set {
                    combined.union(fs);
                } else {
                    return None; // nullable branch
                }
            }
            Some(combined)
        }
        IrNode::Repeat { lo: 0, .. } | IrNode::Epsilon => None,
        IrNode::Repeat { inner, .. } => node_first_set(inner, rule_metas, strings),
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            node_first_set(inner, rule_metas, strings)
        }
        IrNode::Skip(a, _) | IrNode::Next(a, _) => node_first_set(a, rule_metas, strings),
        IrNode::Minus(a, _) => node_first_set(a, rule_metas, strings),
        IrNode::Negate(_) => None, // zero-width, nullable
        IrNode::TokenDispatch { token, .. } => node_first_set(token, rule_metas, strings),
    }
}

/// Get the FIRST set contribution from a nullable node (the non-None part).
fn node_first_set_nullable_part(
    node: &IrNode,
    rule_metas: &[(CharSet128, bool)],
) -> Option<CharSet128> {
    match node {
        IrNode::Ref(rule_id) => {
            let (ref first_set, _) = rule_metas[*rule_id as usize];
            if first_set.is_empty() {
                None
            } else {
                Some(first_set.clone())
            }
        }
        IrNode::Repeat { inner, .. } => node_first_set_nullable_part(inner, rule_metas),
        _ => None,
    }
}

/// Compute the contextual FOLLOW for a node at position `from` within a Seq.
///
/// Returns `FIRST(children[from..])` unioned with `rule_follow` if the suffix
/// is entirely nullable.
fn suffix_follow(
    children: &[IrNode],
    from: usize,
    rule_follow: Option<&CharSet128>,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
) -> Option<CharSet128> {
    let mut combined = CharSet128::new();
    for child in &children[from..] {
        if let Some(fs) = node_first_set(child, rule_metas, strings) {
            combined.union(&fs);
            // Non-nullable child — suffix can't produce anything beyond this.
            return Some(combined);
        }
        // Nullable child — include its FIRST and continue.
        if let Some(fs) = node_first_set_nullable_part(child, rule_metas) {
            combined.union(&fs);
        }
    }
    // All suffix children are nullable — include rule FOLLOW.
    if let Some(follow) = rule_follow {
        combined.union(follow);
    }
    if combined.is_empty() {
        None
    } else {
        Some(combined)
    }
}

/// Recursively walk an IrNode tree and annotate eligible Alt nodes.
///
/// `containing_follow` is the contextual FOLLOW set — for top-level nodes this
/// is `FOLLOW(rule)`, but for nodes inside a Seq it is `FIRST(suffix)` (the
/// first set of the remaining sequence elements).
///
/// `eligibility` is the CSP pre-computed dispatch eligibility table: when an
/// Alt is marked non-dispatchable (without FOLLOW context), the walk can skip
/// the full `try_build_dispatch` call. FOLLOW-aware dispatch still proceeds
/// for nullable branches, since that context is only available during the walk.
fn annotate_node(
    node: &mut IrNode,
    containing_follow: Option<&CharSet128>,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
    eligibility: &DispatchEligibility,
) {
    // Capture the node pointer before the mutable destructuring borrow.
    let node_ptr = node as *const IrNode as usize;

    match node {
        IrNode::Alt(branches, dispatch) => {
            // Recurse into children first.
            for branch in branches.iter_mut() {
                annotate_node(
                    &mut branch.node,
                    containing_follow,
                    rule_metas,
                    strings,
                    eligibility,
                );
            }

            // Skip if already annotated.
            if dispatch.is_some() {
                return;
            }

            // Recompute FIRST sets for branches that lack them (after inlining/fusing,
            // newly created AltBranch nodes may have first_set=None).
            for branch in branches.iter_mut() {
                if branch.first_set.is_none() {
                    branch.first_set = node_first_set(&branch.node, rule_metas, strings);
                }
            }

            // Consult the CSP pre-computed eligibility.
            // When the pre-computation determined non-dispatchable AND there are
            // no nullable branches (where FOLLOW context could rescue dispatch),
            // we skip the full try_build_dispatch and go straight to fallback.
            let pre_eligible = eligibility.get(&node_ptr).copied();
            let has_nullable = branches.iter().any(|b| b.first_set.is_none());

            if pre_eligible == Some(false) && !has_nullable {
                // CSP says non-dispatchable, no nullable branch could be rescued
                // by FOLLOW — try fallback dispatch only.
                if let Some(table) = try_build_fallback_dispatch(branches) {
                    *dispatch = Some(table);
                }
                return;
            }

            // Try to build a full dispatch table (all branches disjoint).
            // Falls back to fallback-aware dispatch when the last branch
            // has a superset FIRST set (e.g. genericDecl catch-all).
            if let Some(table) = try_build_dispatch(branches, containing_follow) {
                *dispatch = Some(table);
            } else if let Some(table) = try_build_fallback_dispatch(branches) {
                *dispatch = Some(table);
            }
        }
        IrNode::Seq(children) => {
            // For each child in the Seq, compute the contextual follow from
            // FIRST(suffix) rather than blindly passing the rule-level FOLLOW.
            for i in 0..children.len() {
                let child_follow = if i + 1 < children.len() {
                    suffix_follow(children, i + 1, containing_follow, rule_metas, strings)
                } else {
                    containing_follow.cloned()
                };
                annotate_node(
                    &mut children[i],
                    child_follow.as_ref().or(containing_follow),
                    rule_metas,
                    strings,
                    eligibility,
                );
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => {
            annotate_node(inner, containing_follow, rule_metas, strings, eligibility);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            annotate_node(a, containing_follow, rule_metas, strings, eligibility);
            annotate_node(b, containing_follow, rule_metas, strings, eligibility);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            annotate_node(token, containing_follow, rule_metas, strings, eligibility);
            for arm in arms {
                annotate_node(
                    &mut arm.continuation,
                    containing_follow,
                    rule_metas,
                    strings,
                    eligibility,
                );
            }
            annotate_node(
                fallback,
                containing_follow,
                rule_metas,
                strings,
                eligibility,
            );
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}

/// Try to build a dispatch table for an alternation's branches.
///
/// Returns `Some(AltDispatch)` if all branches have pairwise disjoint effective
/// dispatch sets. A branch's effective set is its FIRST set, or — when the branch
/// is nullable and `containing_follow` is available — the FOLLOW set of the
/// containing rule.
fn try_build_dispatch(
    branches: &[AltBranch],
    containing_follow: Option<&CharSet128>,
) -> Option<AltDispatch> {
    // Limit to 127 branches (u8 range with 255 as sentinel).
    if branches.len() > 127 {
        return None;
    }

    // At most one nullable branch can be handled via FOLLOW dispatch.
    let mut nullable_idx: Option<usize> = None;

    // Build effective dispatch sets per branch.
    let mut effective_sets: Vec<CharSet128> = Vec::with_capacity(branches.len());

    for (i, branch) in branches.iter().enumerate() {
        if let Some(ref first) = branch.first_set {
            effective_sets.push(first.clone());
        } else if nullable_idx.is_none() {
            // Nullable branch — use FOLLOW(rule) if available.
            if let Some(follow) = containing_follow {
                if !follow.is_empty() {
                    nullable_idx = Some(i);
                    effective_sets.push(follow.clone());
                } else {
                    // Empty FOLLOW — can't dispatch.
                    return None;
                }
            } else {
                // No FOLLOW sets available — can't dispatch nullable branch.
                return None;
            }
        } else {
            // Multiple nullable branches — can't dispatch.
            return None;
        }
    }

    if effective_sets.len() != branches.len() {
        return None;
    }

    // Check pairwise disjointness.
    for i in 0..effective_sets.len() {
        for j in (i + 1)..effective_sets.len() {
            if !effective_sets[i].is_disjoint(&effective_sets[j]) {
                return None;
            }
        }
    }

    // Build dispatch table.
    let mut table = vec![255u8; 128];
    for (idx, cs) in effective_sets.iter().enumerate() {
        for code in cs.iter() {
            table[code as usize] = idx as u8;
        }
    }

    Some(AltDispatch {
        table,
        // A nullable branch dispatched via FOLLOW is semantically a fallback:
        // it matches when no other branch does (unmatched bytes + EOF).
        fallback_idx: nullable_idx.map(|i| i as u8),
    })
}

/// Build a dispatch table with a fallback branch.
///
/// When strict pairwise disjointness fails, checks whether excluding the LAST
/// branch makes all remaining branches pairwise disjoint.  If so, the last
/// branch becomes a fallback: dispatched branches that fail at runtime fall
/// through to it, and bytes not claimed by any dispatched branch go directly
/// to it.
///
/// This handles the common pattern of typed branches + generic catch-all
/// (e.g. `colorDecl | sizeDecl | ... | genericDecl`).
fn try_build_fallback_dispatch(branches: &[AltBranch]) -> Option<AltDispatch> {
    // Need at least 3 branches: 2+ typed + 1 fallback.  Below 3, the
    // sequential path is already fast enough.
    if branches.len() < 3 || branches.len() > 127 {
        return None;
    }

    let fallback_idx = branches.len() - 1;
    let typed_branches = &branches[..fallback_idx];

    // The fallback branch must have a FIRST set (non-nullable).
    let fallback_first = branches[fallback_idx].first_set.as_ref()?;

    // Collect FIRST sets for typed branches.
    let mut effective_sets: Vec<CharSet128> = Vec::with_capacity(typed_branches.len());
    for branch in typed_branches {
        effective_sets.push(branch.first_set.as_ref()?.clone());
    }

    // The fallback's FIRST set must be a superset of the union of all typed
    // FIRST sets (otherwise it can't catch everything the typed branches miss).
    let typed_union: CharSet128 = {
        let mut u = CharSet128::new();
        for s in &effective_sets {
            u.union(s);
        }
        u
    };
    if !is_superset(fallback_first, &typed_union) {
        return None;
    }

    // Check pairwise disjointness among typed branches ONLY.
    for i in 0..effective_sets.len() {
        for j in (i + 1)..effective_sets.len() {
            if !effective_sets[i].is_disjoint(&effective_sets[j]) {
                return None;
            }
        }
    }

    // Build table: typed branches get their entries, everything else → 255 (fallback).
    let mut table = vec![255u8; 128];
    for (idx, cs) in effective_sets.iter().enumerate() {
        for code in cs.iter() {
            table[code as usize] = idx as u8;
        }
    }

    Some(AltDispatch {
        table,
        fallback_idx: Some(fallback_idx as u8),
    })
}

/// Check if charset A is a superset of charset B (B ⊆ A).
fn is_superset(a: &CharSet128, b: &CharSet128) -> bool {
    (b.bits[0] & !a.bits[0]) == 0 && (b.bits[1] & !a.bits[1]) == 0
}
