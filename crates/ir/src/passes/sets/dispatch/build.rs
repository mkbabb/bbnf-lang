//! `try_build_dispatch` / `try_build_fallback_dispatch` — build a 128-entry
//! `AltDispatch` table from a branch list.
//!
//! Two strategies:
//!
//! 1. **Strict** ([`try_build_dispatch`]): all branches must have pairwise
//!    disjoint effective dispatch sets. A nullable branch can participate via
//!    a FOLLOW set (passed in by the annotator).
//! 2. **Fallback** ([`try_build_fallback_dispatch`]): the LAST branch becomes
//!    a catch-all whose FIRST set is a superset of the typed branches' union.
//!    Bytes claimed by typed branches dispatch to them; everything else (and
//!    runtime failures) fall through to the fallback. This handles the
//!    common pattern of typed declarations + a generic catch-all.

use crate::{AltBranch, AltDispatch, CharSet128};

/// Try to build a dispatch table for an alternation's branches.
///
/// Returns `Some(AltDispatch)` if all branches have pairwise disjoint effective
/// dispatch sets. A branch's effective set is its FIRST set, or — when the branch
/// is nullable and `containing_follow` is available — the FOLLOW set of the
/// containing rule.
pub(super) fn try_build_dispatch(
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
pub(super) fn try_build_fallback_dispatch(branches: &[AltBranch]) -> Option<AltDispatch> {
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
