//! Normalization rewrite rules: always-valid equivalences the extractor
//! can choose between.
//!
//! These rules add simpler forms alongside the original — the cost model
//! will prefer the simpler form, but the original stays available.

use egraph::{EGraph, Id, Rewrite};

use crate::StringId;
use crate::egraph::analysis::GrammarAnalysis;
use crate::egraph::interner::SharedStrings;
use crate::egraph::node::GrammarENode;

// ── Rule: Seq([.., Epsilon, ..]) ≡ Seq([..]) ─────────────────────────────────

/// Eliminate epsilon children from Seq nodes. The filtered Seq joins the
/// same e-class as the original.
pub struct EliminateEpsilon;

impl Rewrite<GrammarENode, GrammarAnalysis> for EliminateEpsilon {
    type Match = (Id, Box<[Id]>);

    fn name(&self) -> &str {
        "eliminate-epsilon-in-seq"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, GrammarAnalysis>) -> Vec<(Id, Self::Match)> {
        let mut matches = Vec::new();
        for class in egraph.classes() {
            for node in class.iter() {
                let GrammarENode::Seq(children) = node else {
                    continue;
                };
                // Check if any child is Epsilon.
                let has_epsilon = children.iter().any(|&id| {
                    egraph
                        .class(id)
                        .iter()
                        .any(|n| matches!(n, GrammarENode::Epsilon))
                });
                if !has_epsilon {
                    continue;
                }
                let filtered: Vec<Id> = children
                    .iter()
                    .copied()
                    .filter(|&id| {
                        !egraph
                            .class(id)
                            .iter()
                            .any(|n| matches!(n, GrammarENode::Epsilon))
                    })
                    .collect();
                if filtered.len() < children.len() {
                    matches.push((class.id, (class.id, filtered.into_boxed_slice())));
                }
            }
        }
        matches
    }

    fn apply(
        &self,
        egraph: &mut EGraph<GrammarENode, GrammarAnalysis>,
        class_id: Id,
        (_, filtered): Self::Match,
    ) -> bool {
        if filtered.is_empty() {
            let eps_id = egraph.add(GrammarENode::Epsilon);
            let before = egraph.find(class_id);
            egraph.union(class_id, eps_id);
            egraph.find(class_id) != before || egraph.find(class_id) == eps_id
        } else if filtered.len() == 1 {
            let target = filtered[0];
            let before = egraph.find(class_id);
            egraph.union(class_id, target);
            egraph.find(class_id) != before
        } else {
            let new_seq = egraph.add(GrammarENode::Seq(filtered));
            let before = egraph.find(class_id);
            egraph.union(class_id, new_seq);
            egraph.find(class_id) != before
        }
    }
}

// ── Rule: Seq([x]) ≡ x ───────────────────────────────────────────────────────

/// A singleton Seq is equivalent to its sole child.
pub struct UnwrapSingletonSeq;

impl Rewrite<GrammarENode, GrammarAnalysis> for UnwrapSingletonSeq {
    type Match = Id;

    fn name(&self) -> &str {
        "unwrap-singleton-seq"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, GrammarAnalysis>) -> Vec<(Id, Self::Match)> {
        let mut matches = Vec::new();
        for class in egraph.classes() {
            for node in class.iter() {
                if let GrammarENode::Seq(children) = node {
                    if children.len() == 1 {
                        matches.push((class.id, children[0]));
                    }
                }
            }
        }
        matches
    }

    fn apply(
        &self,
        egraph: &mut EGraph<GrammarENode, GrammarAnalysis>,
        class_id: Id,
        child_id: Self::Match,
    ) -> bool {
        let before = egraph.find(class_id);
        egraph.union(class_id, child_id);
        egraph.find(class_id) != before
    }
}

// ── Rule: Seq([.., Lit(a), Lit(b), ..]) ≡ Seq([.., Lit(a+b), ..]) ────────────

/// Collapse consecutive literal children in `Seq` into a single
/// concatenated literal. Carries a `SharedStrings` handle so it can
/// intern new literals during saturation.
///
/// Replaces the destructive `merge_literals` pass.
pub struct MergeLiterals {
    pool: SharedStrings,
}

impl MergeLiterals {
    pub fn new(pool: SharedStrings) -> Self {
        Self { pool }
    }
}

/// One component of a merge plan — either a freshly-interned literal or
/// a pass-through child that was not consumed by the merge.
///
/// `pub` only to satisfy the `Rewrite::Match` public-in-public
/// requirement; consumers never construct this directly.
#[derive(Clone, Debug)]
pub enum MergeGroup {
    /// A new literal with this interned `StringId` (may be an existing
    /// pool entry if the concatenation happened to match something).
    Literal(StringId),
    /// Keep this child as-is.
    Existing(Id),
}

impl Rewrite<GrammarENode, GrammarAnalysis> for MergeLiterals {
    type Match = Box<[MergeGroup]>;

    fn name(&self) -> &str {
        "merge-adjacent-literals"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, GrammarAnalysis>) -> Vec<(Id, Self::Match)> {
        let mut matches = Vec::new();

        for class in egraph.classes() {
            for node in class.iter() {
                let GrammarENode::Seq(children) = node else {
                    continue;
                };

                let mut groups: Vec<MergeGroup> = Vec::with_capacity(children.len());
                let mut pending_text: Option<String> = None;
                let mut merged_any = false;

                for &child_id in children.iter() {
                    // Pick a Literal form from the child's class, if any.
                    let literal_sid = egraph.class(child_id).iter().find_map(|n| {
                        if let GrammarENode::Literal(sid) = n {
                            Some(*sid)
                        } else {
                            None
                        }
                    });

                    match literal_sid {
                        Some(sid) => {
                            let s = self.pool.get(sid);
                            if let Some(ref mut acc) = pending_text {
                                acc.push_str(&s);
                                merged_any = true;
                            } else {
                                pending_text = Some(s);
                            }
                        }
                        None => {
                            // Flush pending literal before the non-literal child.
                            if let Some(acc) = pending_text.take() {
                                let sid = self.pool.intern(&acc);
                                groups.push(MergeGroup::Literal(sid));
                            }
                            groups.push(MergeGroup::Existing(child_id));
                        }
                    }
                }
                // Flush trailing pending.
                if let Some(acc) = pending_text.take() {
                    let sid = self.pool.intern(&acc);
                    groups.push(MergeGroup::Literal(sid));
                }

                if merged_any {
                    matches.push((class.id, groups.into_boxed_slice()));
                }
            }
        }

        matches
    }

    fn apply(
        &self,
        egraph: &mut EGraph<GrammarENode, GrammarAnalysis>,
        class_id: Id,
        groups: Self::Match,
    ) -> bool {
        let new_children: Box<[Id]> = groups
            .iter()
            .map(|g| match g {
                MergeGroup::Literal(sid) => egraph.add(GrammarENode::Literal(*sid)),
                MergeGroup::Existing(id) => *id,
            })
            .collect();

        // Single-element result: union with the sole child directly;
        // the singleton-Seq unwrap rule would do the same but firing
        // here avoids an intermediate class.
        let new_id = if new_children.len() == 1 {
            new_children[0]
        } else {
            egraph.add(GrammarENode::Seq(new_children))
        };

        let before = egraph.find(class_id);
        egraph.union(class_id, new_id);
        egraph.find(class_id) != before
    }
}

// ── Rule: Alt([x]) ≡ x ───────────────────────────────────────────────────────

/// A singleton Alt is equivalent to its sole branch.
pub struct UnwrapSingletonAlt;

impl Rewrite<GrammarENode, GrammarAnalysis> for UnwrapSingletonAlt {
    type Match = Id;

    fn name(&self) -> &str {
        "unwrap-singleton-alt"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, GrammarAnalysis>) -> Vec<(Id, Self::Match)> {
        let mut matches = Vec::new();
        for class in egraph.classes() {
            for node in class.iter() {
                if let GrammarENode::Alt(children, _) = node {
                    if children.len() == 1 {
                        matches.push((class.id, children[0]));
                    }
                }
            }
        }
        matches
    }

    fn apply(
        &self,
        egraph: &mut EGraph<GrammarENode, GrammarAnalysis>,
        class_id: Id,
        child_id: Self::Match,
    ) -> bool {
        let before = egraph.find(class_id);
        egraph.union(class_id, child_id);
        egraph.find(class_id) != before
    }
}
