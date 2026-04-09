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

use std::collections::HashSet;

use egraph::{EGraph, Id, NoAnalysis, Rewrite};

use crate::egraph::interner::SharedStrings;
use crate::egraph::node::GrammarENode;
use crate::{AltDispatch, StringId};

/// Find the `StringId` of a class's Regex form, if any.
fn class_regex(
    egraph: &EGraph<GrammarENode, NoAnalysis>,
    id: Id,
) -> Option<StringId> {
    egraph.class(id).iter().find_map(|n| {
        if let GrammarENode::Regex(sid) = n {
            Some(*sid)
        } else {
            None
        }
    })
}

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

impl Rewrite<GrammarENode, NoAnalysis> for DeduplicateAltBranches {
    type Match = DedupMatch;

    fn name(&self) -> &str {
        "deduplicate-alt-branches"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, NoAnalysis>) -> Vec<(Id, Self::Match)> {
        let mut matches = Vec::new();
        for class in egraph.classes() {
            for node in class.iter() {
                let GrammarENode::Alt(children, dispatch) = node else {
                    continue;
                };
                let mut seen: HashSet<Id> = HashSet::new();
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

    fn apply(
        &self,
        egraph: &mut EGraph<GrammarENode, NoAnalysis>,
        class_id: Id,
        m: Self::Match,
    ) {
        let new_id = if m.deduped.len() == 1 {
            m.deduped[0]
        } else {
            egraph.add(GrammarENode::Alt(m.deduped, m.dispatch))
        };
        egraph.union(class_id, new_id);
    }
}

// ── Rule: Alt([.., R₁, .., R₂, ..]) ≡ Alt([.., R₁, ..]) when R₁ ⊇ R₂ ────────

/// Absorb a subsumed branch: when one regex char-class branch is a
/// strict superset of another, the subsumed branch can be dropped.
/// Delegates to `bbnf_regex::algebra::pattern_is_superset`.
pub struct SupersetAbsorbAlt {
    pool: SharedStrings,
}

impl SupersetAbsorbAlt {
    pub fn new(pool: SharedStrings) -> Self {
        Self { pool }
    }
}

#[derive(Clone, Debug)]
pub struct AbsorbMatch {
    pub survivors: Box<[Id]>,
    pub dispatch: Option<AltDispatch>,
}

impl Rewrite<GrammarENode, NoAnalysis> for SupersetAbsorbAlt {
    type Match = AbsorbMatch;

    fn name(&self) -> &str {
        "superset-absorb-alt"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, NoAnalysis>) -> Vec<(Id, Self::Match)> {
        let mut matches = Vec::new();
        for class in egraph.classes() {
            for node in class.iter() {
                let GrammarENode::Alt(children, dispatch) = node else {
                    continue;
                };
                let regex_sids: Vec<Option<StringId>> =
                    children.iter().map(|&id| class_regex(egraph, id)).collect();

                // Drop any branch whose regex is a strict subset of another.
                let mut keep = vec![true; children.len()];
                for i in 0..children.len() {
                    let Some(sid_i) = regex_sids[i] else { continue };
                    for j in 0..children.len() {
                        if i == j || !keep[j] {
                            continue;
                        }
                        let Some(sid_j) = regex_sids[j] else { continue };
                        let pat_i = self.pool.get(sid_i);
                        let pat_j = self.pool.get(sid_j);
                        if pat_i != pat_j
                            && ::bbnf_regex::algebra::pattern_is_superset(&pat_i, &pat_j)
                        {
                            keep[j] = false;
                        }
                    }
                }
                let survivors: Vec<Id> = children
                    .iter()
                    .zip(&keep)
                    .filter_map(|(id, k)| if *k { Some(*id) } else { None })
                    .collect();
                if survivors.len() < children.len() {
                    matches.push((
                        class.id,
                        AbsorbMatch {
                            survivors: survivors.into_boxed_slice(),
                            dispatch: dispatch.clone(),
                        },
                    ));
                    break;
                }
            }
        }
        matches
    }

    fn apply(
        &self,
        egraph: &mut EGraph<GrammarENode, NoAnalysis>,
        class_id: Id,
        m: Self::Match,
    ) {
        let new_id = if m.survivors.len() == 1 {
            m.survivors[0]
        } else {
            egraph.add(GrammarENode::Alt(m.survivors, m.dispatch))
        };
        egraph.union(class_id, new_id);
    }
}

// ── Rule: Alt([.., [a-c], [d-f], ..]) ≡ Alt([.., [a-f], ..]) ────────────────

/// Merge two char-class regex branches into a single union
/// class. Delegates to `bbnf_regex::algebra::try_union_patterns`.
pub struct UnionMergeAlt {
    pool: SharedStrings,
}

impl UnionMergeAlt {
    pub fn new(pool: SharedStrings) -> Self {
        Self { pool }
    }
}

#[derive(Clone, Debug)]
pub struct UnionMatch {
    /// The merged `StringId`, already interned into the shared pool.
    pub merged_sid: StringId,
    /// The first branch's index in the original Alt — replaced with
    /// the merged regex.
    pub left_idx: usize,
    /// The second branch's index — dropped.
    pub right_idx: usize,
    /// The original Alt's children, used to rebuild the rewritten Alt.
    pub original_children: Box<[Id]>,
    pub dispatch: Option<AltDispatch>,
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

impl Rewrite<GrammarENode, NoAnalysis> for FuseAltRegexBranches {
    type Match = FuseMatch;

    fn name(&self) -> &str {
        "fuse-alt-regex-branches"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, NoAnalysis>) -> Vec<(Id, Self::Match)> {
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

    fn apply(
        &self,
        egraph: &mut EGraph<GrammarENode, NoAnalysis>,
        class_id: Id,
        m: Self::Match,
    ) {
        let new_regex_id = egraph.add(GrammarENode::Regex(m.fused_sid));
        egraph.union(class_id, new_regex_id);
    }
}

// ── Helpers (ported from legacy passes/regex/merge.rs) ──────────────────────

/// Escape a literal for safe inclusion inside a regex alternation.
fn regex_escape_literal(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 4);
    for ch in s.chars() {
        if "\\^$.|?*+()[]{}".contains(ch) {
            out.push('\\');
        }
        out.push(ch);
    }
    out
}

/// Whether a regex pattern contains a top-level `|` (outside groups and
/// character classes). Used to decide if the pattern needs wrapping
/// before being fused into an outer alternation.
fn pattern_has_top_level_pipe(pattern: &str) -> bool {
    let mut depth = 0u32;
    let mut in_bracket = false;
    let mut escape = false;
    for c in pattern.chars() {
        if escape {
            escape = false;
            continue;
        }
        if c == '\\' {
            escape = true;
            continue;
        }
        if in_bracket {
            if c == ']' {
                in_bracket = false;
            }
            continue;
        }
        match c {
            '[' => in_bracket = true,
            '(' => depth += 1,
            ')' => depth = depth.saturating_sub(1),
            '|' if depth == 0 => return true,
            _ => {}
        }
    }
    false
}

impl Rewrite<GrammarENode, NoAnalysis> for UnionMergeAlt {
    type Match = UnionMatch;

    fn name(&self) -> &str {
        "union-merge-alt"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, NoAnalysis>) -> Vec<(Id, Self::Match)> {
        let mut matches = Vec::new();
        'outer: for class in egraph.classes() {
            for node in class.iter() {
                let GrammarENode::Alt(children, dispatch) = node else {
                    continue;
                };
                for i in 0..children.len() {
                    for j in (i + 1)..children.len() {
                        let (Some(sid_i), Some(sid_j)) = (
                            class_regex(egraph, children[i]),
                            class_regex(egraph, children[j]),
                        ) else {
                            continue;
                        };
                        let pat_i = self.pool.get(sid_i);
                        let pat_j = self.pool.get(sid_j);
                        if let Some(merged) =
                            ::bbnf_regex::algebra::try_union_patterns(&pat_i, &pat_j)
                        {
                            // Only fire when the merged form is a true
                            // simplification, not just
                            // `"[a]" ∪ "[b]" = "[ab]"` when the
                            // original already had equal length.
                            if merged.len() < pat_i.len() + pat_j.len() {
                                let merged_sid = self.pool.intern(&merged);
                                matches.push((
                                    class.id,
                                    UnionMatch {
                                        merged_sid,
                                        left_idx: i,
                                        right_idx: j,
                                        original_children: children.clone(),
                                        dispatch: dispatch.clone(),
                                    },
                                ));
                                continue 'outer;
                            }
                        }
                    }
                }
            }
        }
        matches
    }

    fn apply(
        &self,
        egraph: &mut EGraph<GrammarENode, NoAnalysis>,
        class_id: Id,
        m: Self::Match,
    ) {
        let new_regex_id = egraph.add(GrammarENode::Regex(m.merged_sid));
        let mut new_children: Vec<Id> = Vec::with_capacity(m.original_children.len() - 1);
        for (k, &id) in m.original_children.iter().enumerate() {
            if k == m.right_idx {
                continue;
            }
            new_children.push(if k == m.left_idx { new_regex_id } else { id });
        }
        let new_id = if new_children.len() == 1 {
            new_children[0]
        } else {
            egraph.add(GrammarENode::Alt(new_children.into_boxed_slice(), m.dispatch))
        };
        egraph.union(class_id, new_id);
    }
}
