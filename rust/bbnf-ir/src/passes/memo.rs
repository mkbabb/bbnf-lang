//! Pass 2.5: SCC-aware memoization strategy refinement.
//!
//! Assigns memoization strategy per rule based on SCC membership, reference counts,
//! and FOLLOW set cardinality:
//! - Cyclic SCC entry points → Full (memoization required for termination)
//! - Other cyclic rules in the same SCC → None (entry point handles the cache)
//! - Non-cyclic, highly-referenced (>threshold) → Selective (avoids redundant parsing)
//! - FOLLOW set cardinality adjusts the effective threshold: rules with large FOLLOW
//!   sets (many calling contexts) benefit more from memoization, so their threshold
//!   is lowered. Rules with small FOLLOW sets are cheaper to re-parse.
//! - Otherwise → None

use std::collections::{HashMap, HashSet};

use crate::{GrammarIR, IrNode, MemoStrategy, RuleId};

/// Default reference-count threshold for selective memoization.
const SELECTIVE_THRESHOLD: u32 = 3;

/// FOLLOW set cardinality above which memoization benefit is boosted.
/// Rules appearing in many syntactic contexts (large FOLLOW set) are more likely
/// to be re-parsed at different offsets, so memoization pays off sooner.
const FOLLOW_BOOST_THRESHOLD: usize = 8;

/// Refine memoization strategies based on SCC entry points, reference counts,
/// and FOLLOW set cardinality.
///
/// Within each SCC, only the *entry points* (rules referenced from outside the SCC)
/// get Full memoization. Other cyclic rules in the same SCC skip memoization since
/// the entry point's cache subsumes them.
///
/// For non-cyclic rules, FOLLOW set cardinality modulates the selective threshold:
/// - Large FOLLOW set (>= 8 chars): threshold reduced by 1 (memoize sooner).
/// - Small FOLLOW set (< 4 chars): threshold increased by 1 (skip memoization more).
pub fn refine_memo_strategies(ir: &mut GrammarIR) {
    let ref_counts = compute_ref_counts(ir);
    let scc_entries = find_scc_entry_points(ir);

    // Clone follow_sets to avoid borrow conflict.
    let follow_sets = ir.follow_sets.clone();

    for rule in &mut ir.rules {
        rule.meta.memo = if rule.meta.is_cyclic {
            // Only memoize SCC entry points — other cyclic rules are reached
            // through the entry point which already has the cache.
            if scc_entries.contains(&rule.id) {
                MemoStrategy::Full
            } else {
                MemoStrategy::None
            }
        } else {
            let refs = ref_counts.get(&rule.id).copied().unwrap_or(0);

            // Adjust threshold based on FOLLOW set cardinality when available.
            // If FOLLOW sets have not been computed (empty map), use the default.
            let threshold = if follow_sets.is_empty() {
                SELECTIVE_THRESHOLD
            } else {
                let follow_card = follow_sets
                    .get(&rule.id)
                    .map(|cs| cs.len())
                    .unwrap_or(0);

                if follow_card >= FOLLOW_BOOST_THRESHOLD {
                    // Large FOLLOW → many calling contexts → memoize sooner.
                    SELECTIVE_THRESHOLD.saturating_sub(1)
                } else if follow_card < 4 {
                    // Small FOLLOW → few contexts → re-parsing is cheap.
                    SELECTIVE_THRESHOLD + 1
                } else {
                    SELECTIVE_THRESHOLD
                }
            };

            if refs > threshold {
                MemoStrategy::Selective
            } else {
                MemoStrategy::None
            }
        };
    }
}

/// Find SCC entry points: cyclic rules that are referenced from outside their SCC.
///
/// A rule is an SCC entry point if:
/// 1. It is cyclic (has an scc_id)
/// 2. At least one reference to it comes from a rule in a different SCC (or no SCC)
///
/// If an SCC has no external references, the first rule in the SCC is the entry point
/// (it must be reachable from the grammar entry).
fn find_scc_entry_points(ir: &GrammarIR) -> HashSet<RuleId> {
    // Build scc_id lookup.
    let rule_scc: HashMap<RuleId, u32> = ir
        .rules
        .iter()
        .filter_map(|r| r.meta.scc_id.map(|scc| (r.id, scc)))
        .collect();

    // Collect all Ref(target) with their source rule's SCC.
    let mut external_refs: HashSet<RuleId> = HashSet::new();
    for rule in &ir.rules {
        let src_scc = rule_scc.get(&rule.id);
        collect_cross_scc_refs(&rule.body, src_scc, &rule_scc, &mut external_refs);
    }

    // For each SCC, ensure at least one entry point exists.
    let mut scc_has_entry: HashMap<u32, bool> = HashMap::new();
    for rule in &ir.rules {
        if let Some(scc) = rule.meta.scc_id {
            if rule.meta.is_cyclic && external_refs.contains(&rule.id) {
                scc_has_entry.insert(scc, true);
            }
        }
    }

    // If an SCC has no external entry, pick the first cyclic rule as entry.
    for rule in &ir.rules {
        if let Some(scc) = rule.meta.scc_id {
            if rule.meta.is_cyclic && !scc_has_entry.contains_key(&scc) {
                external_refs.insert(rule.id);
                scc_has_entry.insert(scc, true);
            }
        }
    }

    external_refs
}

/// Collect rule IDs that are referenced from a different SCC than `src_scc`.
fn collect_cross_scc_refs(
    node: &IrNode,
    src_scc: Option<&u32>,
    rule_scc: &HashMap<RuleId, u32>,
    entries: &mut HashSet<RuleId>,
) {
    match node {
        IrNode::Ref(id) => {
            let target_scc = rule_scc.get(id);
            // Cross-SCC reference: src and target are in different SCCs (or one has no SCC).
            if src_scc != target_scc {
                entries.insert(*id);
            }
        }
        IrNode::Seq(children) => {
            for child in children {
                collect_cross_scc_refs(child, src_scc, rule_scc, entries);
            }
        }
        IrNode::Alt(branches, _) => {
            for branch in branches {
                collect_cross_scc_refs(&branch.node, src_scc, rule_scc, entries);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner) => {
            collect_cross_scc_refs(inner, src_scc, rule_scc, entries);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            collect_cross_scc_refs(a, src_scc, rule_scc, entries);
            collect_cross_scc_refs(b, src_scc, rule_scc, entries);
        }
        IrNode::Map { inner, .. } => {
            collect_cross_scc_refs(inner, src_scc, rule_scc, entries);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
    }
}

/// Count how many times each rule is referenced via `Ref(id)` across all rule bodies.
fn compute_ref_counts(ir: &GrammarIR) -> HashMap<RuleId, u32> {
    let mut counts: HashMap<RuleId, u32> = HashMap::new();

    for rule in &ir.rules {
        count_refs(&rule.body, &mut counts);
        if let Some(ref recover) = rule.meta.recover {
            count_refs(recover, &mut counts);
        }
    }

    counts
}

fn count_refs(node: &IrNode, counts: &mut HashMap<RuleId, u32>) {
    match node {
        IrNode::Ref(id) => {
            *counts.entry(*id).or_insert(0) += 1;
        }
        IrNode::Seq(children) => {
            for child in children {
                count_refs(child, counts);
            }
        }
        IrNode::Alt(branches, _) => {
            for branch in branches {
                count_refs(&branch.node, counts);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner) => {
            count_refs(inner, counts);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            count_refs(a, counts);
            count_refs(b, counts);
        }
        IrNode::Map { inner, .. } => {
            count_refs(inner, counts);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{AltBranch, IrRule, RuleMeta};

    #[test]
    fn cyclic_entry_point_gets_full_memo() {
        // Single cyclic rule — it is its own entry point.
        let mut ir = GrammarIR {
            rules: vec![IrRule {
                id: 0,
                name: 0,
                body: IrNode::Alt(
                    vec![
                        AltBranch {
                            node: IrNode::Seq(vec![IrNode::Ref(0), IrNode::Literal(2)]),
                            first_set: None,
                        },
                        AltBranch {
                            node: IrNode::Literal(3),
                            first_set: None,
                        },
                    ],
                    None,
                ),
                meta: RuleMeta {
                    is_cyclic: true,
                    scc_id: Some(0),
                    ..Default::default()
                },
            }],
            entry: 0,
            strings: vec!["expr".into(), "term".into(), "+".into(), "x".into()],
            fns: vec![],
            types: vec![],
            follow_sets: HashMap::new(),
        };

        refine_memo_strategies(&mut ir);
        assert_eq!(ir.rules[0].meta.memo, MemoStrategy::Full);
    }

    #[test]
    fn scc_non_entry_gets_none() {
        // Two mutually-recursive rules in the same SCC.
        // Rule 0 is referenced from outside (entry), rule 1 is only referenced from rule 0.
        let mut ir = GrammarIR {
            rules: vec![
                IrRule {
                    id: 0,
                    name: 0,
                    body: IrNode::Ref(1),
                    meta: RuleMeta {
                        is_cyclic: true,
                        scc_id: Some(0),
                        ..Default::default()
                    },
                },
                IrRule {
                    id: 1,
                    name: 1,
                    body: IrNode::Alt(
                        vec![
                            AltBranch {
                                node: IrNode::Ref(0),
                                first_set: None,
                            },
                            AltBranch {
                                node: IrNode::Literal(2),
                                first_set: None,
                            },
                        ],
                        None,
                    ),
                    meta: RuleMeta {
                        is_cyclic: true,
                        scc_id: Some(0),
                        ..Default::default()
                    },
                },
                // External rule that references rule 0.
                IrRule {
                    id: 2,
                    name: 2,
                    body: IrNode::Ref(0),
                    meta: RuleMeta::default(),
                },
            ],
            entry: 2,
            strings: vec!["a".into(), "b".into(), "x".into()],
            fns: vec![],
            types: vec![],
            follow_sets: HashMap::new(),
        };

        refine_memo_strategies(&mut ir);
        // Rule 0 is the SCC entry point (referenced from rule 2, outside the SCC).
        assert_eq!(ir.rules[0].meta.memo, MemoStrategy::Full);
        // Rule 1 is NOT an entry point — only referenced from within the SCC.
        assert_eq!(ir.rules[1].meta.memo, MemoStrategy::None);
    }

    #[test]
    fn highly_referenced_gets_selective() {
        let mut ir = GrammarIR {
            rules: vec![
                IrRule {
                    id: 0,
                    name: 0,
                    body: IrNode::Seq(vec![
                        IrNode::Ref(1),
                        IrNode::Ref(1),
                        IrNode::Ref(1),
                        IrNode::Ref(1),
                    ]),
                    meta: RuleMeta::default(),
                },
                IrRule {
                    id: 1,
                    name: 1,
                    body: IrNode::Literal(2),
                    meta: RuleMeta::default(),
                },
            ],
            entry: 0,
            strings: vec!["start".into(), "common".into(), "x".into()],
            fns: vec![],
            types: vec![],
            follow_sets: HashMap::new(),
        };

        refine_memo_strategies(&mut ir);
        assert_eq!(ir.rules[1].meta.memo, MemoStrategy::Selective);
    }

    #[test]
    fn low_ref_count_gets_none() {
        let mut ir = GrammarIR {
            rules: vec![
                IrRule {
                    id: 0,
                    name: 0,
                    body: IrNode::Ref(1),
                    meta: RuleMeta::default(),
                },
                IrRule {
                    id: 1,
                    name: 1,
                    body: IrNode::Literal(2),
                    meta: RuleMeta::default(),
                },
            ],
            entry: 0,
            strings: vec!["start".into(), "a".into(), "x".into()],
            fns: vec![],
            types: vec![],
            follow_sets: HashMap::new(),
        };

        refine_memo_strategies(&mut ir);
        assert_eq!(ir.rules[1].meta.memo, MemoStrategy::None);
    }

    #[test]
    fn large_follow_set_lowers_memo_threshold() {
        // Rule 1 referenced 3 times (= default threshold).
        // Without FOLLOW boost, 3 refs does not exceed threshold → None.
        // With large FOLLOW set (>= 8 chars), threshold drops to 2 → Selective.
        use crate::CharSet128;

        let mut follow = CharSet128::new();
        // Add 10 chars to FOLLOW set (exceeds FOLLOW_BOOST_THRESHOLD of 8).
        for c in b'a'..=b'j' {
            follow.add(c);
        }
        let mut follow_sets = HashMap::new();
        follow_sets.insert(1u32, follow);

        let mut ir = GrammarIR {
            rules: vec![
                IrRule {
                    id: 0,
                    name: 0,
                    body: IrNode::Seq(vec![
                        IrNode::Ref(1),
                        IrNode::Ref(1),
                        IrNode::Ref(1),
                    ]),
                    meta: RuleMeta::default(),
                },
                IrRule {
                    id: 1,
                    name: 1,
                    body: IrNode::Literal(2),
                    meta: RuleMeta::default(),
                },
            ],
            entry: 0,
            strings: vec!["start".into(), "common".into(), "x".into()],
            fns: vec![],
            types: vec![],
            follow_sets,
        };

        refine_memo_strategies(&mut ir);
        // 3 refs > lowered threshold of 2 → Selective.
        assert_eq!(ir.rules[1].meta.memo, MemoStrategy::Selective);
    }

    #[test]
    fn small_follow_set_raises_memo_threshold() {
        // Rule 1 referenced 4 times (> default threshold of 3).
        // Without FOLLOW, 4 > 3 → Selective.
        // With small FOLLOW set (< 4 chars), threshold rises to 4 → None (4 not > 4).
        use crate::CharSet128;

        let mut follow = CharSet128::new();
        follow.add(b';'); // Only 1 char in FOLLOW.
        let mut follow_sets = HashMap::new();
        follow_sets.insert(1u32, follow);

        let mut ir = GrammarIR {
            rules: vec![
                IrRule {
                    id: 0,
                    name: 0,
                    body: IrNode::Seq(vec![
                        IrNode::Ref(1),
                        IrNode::Ref(1),
                        IrNode::Ref(1),
                        IrNode::Ref(1),
                    ]),
                    meta: RuleMeta::default(),
                },
                IrRule {
                    id: 1,
                    name: 1,
                    body: IrNode::Literal(2),
                    meta: RuleMeta::default(),
                },
            ],
            entry: 0,
            strings: vec!["start".into(), "common".into(), "x".into()],
            fns: vec![],
            types: vec![],
            follow_sets,
        };

        refine_memo_strategies(&mut ir);
        // 4 refs not > raised threshold of 4 → None.
        assert_eq!(ir.rules[1].meta.memo, MemoStrategy::None);
    }
}
