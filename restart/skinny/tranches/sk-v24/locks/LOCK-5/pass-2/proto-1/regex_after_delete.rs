//! Regex algebra rewrite rules: redundant alternation, superset
//! absorption, char-class union merging, and pipe-fusion of
//! all-regex/literal alternations.
//!
//! These are the permanent grammar-tier regex optimization layer.
//! The destructive `simplify_regex_algebra` and `merge_regex_alts`
//! passes (together 802 LOC) were deleted in Tranche H-7 once
//! these e-graph rules proved parity across the full test suite.
//! Each rule delegates the actual regex-level work (superset
//! checking, class union, literal escaping, top-level pipe
//! detection) to `bbnf_regex::algebra` helpers, keeping the rule
//! files thin and the underlying algebra functions reusable by
//! both the grammar tier and the HIR-tier rules in
//! `bbnf-regex::egraph::rules`.
//!
//! - [`DeduplicateAltBranches`]  — `Alt([.., R, .., R, ..])` → `Alt([.., R, ..])`
//! - [`SupersetAbsorbAlt`]       — `Alt([.., [a-z], .., [a-c], ..])` → `Alt([.., [a-z], ..])`
//! - [`UnionMergeAlt`]           — `Alt([.., [a-c], [d-f], ..])` → `Alt([.., [a-f], ..])`
//! - [`FuseAltRegexBranches`]    — `Alt([Regex(a), Lit(b), Regex(c)])` → `Regex("a|b|c")`

use rustc_hash::FxHashSet;

use egraph::{Analysis, EGraph, Id, Rewrite};

use crate::egraph::interner::SharedStrings;
use crate::egraph::node::GrammarENode;
use crate::{AltDispatch, StringId};

// ── Rule: Alt([.., R, .., R, ..]) ≡ Alt([.., R, ..]) ────────────────────────

/// Deduplicate Alt branches by e-class canonical identity. Two
/// branches that share the same canonical e-class are structurally
/// identical (the e-graph already merged them), and keeping both in
/// the branch list is redundant.
pub struct DeduplicateAltBranches;

#[derive(Clone, Debug)]
pub struct DedupMatch {
    pub deduped: Box<[Id]>,
    pub dispatch: Option<AltDispatch>,
}

impl<A: Analysis<GrammarENode>> Rewrite<GrammarENode, A> for DeduplicateAltBranches {
    type Match = DedupMatch;

    fn name(&self) -> &str {
        "deduplicate-alt-branches"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, A>) -> Vec<(Id, Self::Match)> {
        let mut matches = Vec::new();
        for class in egraph.classes() {
            for node in class.iter() {
                let GrammarENode::Alt(children, dispatch) = node else {
                    continue;
                };
                let mut seen: FxHashSet<Id> = FxHashSet::default();
                let mut deduped: Vec<Id> = Vec::with_capacity(children.len());
                for &id in children.iter() {
                    let canon = egraph.find_ref(id);
                    if seen.insert(canon) {
                        deduped.push(id);
                    }
                }
                if deduped.len() < children.len() {
                    matches.push((
                        class.id,
                        DedupMatch {
                            deduped: deduped.into_boxed_slice(),
                            dispatch: dispatch.clone(),
                        },
                    ));
                    break;
                }
            }
        }
        matches
    }

    fn apply(&self, egraph: &mut EGraph<GrammarENode, A>, class_id: Id, m: Self::Match) {
        let new_id = if m.deduped.len() == 1 {
            m.deduped[0]
        } else {
            egraph.add(GrammarENode::Alt(m.deduped, m.dispatch))
        };
        egraph.union(class_id, new_id);
    }
}
// ── Rule: Alt([Regex(a), Lit(b), Regex(c), ..]) ≡ Regex("a|b|c|..") ─────────

/// Fuse an entire all-regex/literal alternation into a single combined
/// regex pattern. The e-graph cost model picks the fused form for
/// alternations without dispatch tables (where per-branch engine
/// invocation would dominate) and the original form for branches
/// better served by dispatch or prefix factoring.
///
/// Handles grammar-level heterogeneous alternation fusion — the
/// permanent owner of this rewrite since the destructive
/// `merge_regex_alts` pass was deleted in Tranche H-7.
pub struct FuseAltRegexBranches {
    pool: SharedStrings,
}

impl FuseAltRegexBranches {
    pub fn new(pool: SharedStrings) -> Self {
        Self { pool }
    }
}

#[derive(Clone, Debug)]
pub struct FuseMatch {
    /// The combined pattern's freshly-interned `StringId`.
    pub fused_sid: StringId,
}

impl<A: Analysis<GrammarENode>> Rewrite<GrammarENode, A> for FuseAltRegexBranches {
    type Match = FuseMatch;

    fn name(&self) -> &str {
        "fuse-alt-regex-branches"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, A>) -> Vec<(Id, Self::Match)> {
        let mut matches = Vec::new();
        for class in egraph.classes() {
            for node in class.iter() {
                let GrammarENode::Alt(children, dispatch) = node else {
                    continue;
                };
                // Skip alts that already have a dispatch table — the
                // per-byte dispatch is faster than a fused regex.
                if dispatch.is_some() || children.len() < 2 {
                    continue;
                }

                // Every branch must resolve to a Regex or Literal form,
                // and at least one branch must be a Regex (otherwise
                // `MergeLiterals` already handles the case).
                let mut parts: Vec<String> = Vec::with_capacity(children.len());
                let mut has_regex = false;
                let mut all_fusable = true;
                for &child_id in children.iter() {
                    let kind = egraph.class(child_id).iter().find_map(|n| match n {
                        GrammarENode::Regex(sid) => Some((true, *sid)),
                        GrammarENode::Literal(sid) => Some((false, *sid)),
                        _ => None,
                    });
                    let Some((is_regex, sid)) = kind else {
                        all_fusable = false;
                        break;
                    };
                    if is_regex {
                        has_regex = true;
                        let pat = self.pool.get(sid);
                        // Wrap patterns with top-level `|` so they stay
                        // atomic under the outer alternation.
                        if pattern_has_top_level_pipe(&pat) {
                            parts.push(format!("(?:{})", pat));
                        } else {
                            parts.push(pat);
                        }
                    } else {
                        let lit = self.pool.get(sid);
                        parts.push(regex_escape_literal(&lit));
                    }
                }
                if !all_fusable || !has_regex {
                    continue;
                }

                let combined = parts.join("|");
                let fused_sid = self.pool.intern(&combined);
                matches.push((class.id, FuseMatch { fused_sid }));
                break;
            }
        }
        matches
    }

    fn apply(&self, egraph: &mut EGraph<GrammarENode, A>, class_id: Id, m: Self::Match) {
        let new_regex_id = egraph.add(GrammarENode::Regex(m.fused_sid));
        egraph.union(class_id, new_regex_id);
    }
}
