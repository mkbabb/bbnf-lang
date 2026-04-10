//! Common-suffix factoring rule (Tranche Y.11).
//!
//! Dual of the normalizer's prefix-factoring pass at
//! `crates/ir/src/passes/prefix.rs`. Detects alternations whose
//! branches share a common trailing sub-expression and lifts the
//! shared tail out of the branches:
//!
//! ```text
//! Alt([Seq([A, x]), Seq([B, x])])  ≡  Seq([Alt([A, B]), x])
//! ```
//!
//! where `x` is structurally identical across branches (same e-class
//! canonical id). The rewrite is a non-destructive e-graph union —
//! the cost model picks between the forms via `GrammarCostModel`
//! during extraction. Post-extraction the chosen form feeds directly
//! into the single post-normalizer saturation; no separate pass is
//! needed.
//!
//! # Shape constraints
//!
//! The rule matches only `Alt`s whose branches are **all** `Seq`s
//! with at least two children whose **last** child shares a canonical
//! e-class id. A mixed Alt (`Alt([Seq([A, x]), B])`) is ignored
//! because lifting `x` out would change the structural shape of the
//! non-Seq branch.
//!
//! The narrow shape keeps the rule low-risk — it only fires when the
//! dual of prefix factoring is unambiguously beneficial — and
//! matches the Y.11 plan commitment that new e-graph rules land only
//! when they are clearly within scope of existing canonicalization.

use rustc_hash::FxHashMap;

use egraph::{Analysis, EGraph, Id, Rewrite};

use crate::egraph::node::GrammarENode;

/// Common-suffix factoring for `Alt` nodes with uniform-Seq branches.
pub struct CommonSuffixFactor;

#[derive(Clone, Debug)]
pub struct SuffixMatch {
    /// The new branches' heads — each branch's original body minus
    /// its last element, preserved as an owned Seq-children-array.
    pub head_branches: Vec<Vec<Id>>,
    /// The shared suffix e-class id (canonical).
    pub suffix_id: Id,
}

impl<A: Analysis<GrammarENode>> Rewrite<GrammarENode, A> for CommonSuffixFactor {
    type Match = SuffixMatch;

    fn name(&self) -> &str {
        "common-suffix-factor"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, A>) -> Vec<(Id, Self::Match)> {
        let mut matches = Vec::new();
        for class in egraph.classes() {
            for node in class.iter() {
                let GrammarENode::Alt(children, _dispatch) = node else {
                    continue;
                };
                if children.len() < 2 {
                    continue;
                }

                // Collect the last child of each branch's representative
                // Seq, if every branch is a Seq with ≥2 children. Abort
                // on any non-Seq branch or any Seq of length <2.
                let mut tail_ids: Vec<Id> = Vec::with_capacity(children.len());
                let mut head_lists: Vec<Vec<Id>> = Vec::with_capacity(children.len());
                let mut aborted = false;
                for &branch_id in children.iter() {
                    let seq_children = egraph
                        .class(branch_id)
                        .iter()
                        .find_map(|n| match n {
                            GrammarENode::Seq(c) if c.len() >= 2 => Some(c.clone()),
                            _ => None,
                        });
                    let Some(seq) = seq_children else {
                        aborted = true;
                        break;
                    };
                    let last = egraph.find_ref(seq[seq.len() - 1]);
                    tail_ids.push(last);
                    head_lists.push(
                        seq[..seq.len() - 1]
                            .iter()
                            .map(|&id| egraph.find_ref(id))
                            .collect(),
                    );
                }
                if aborted {
                    continue;
                }

                // All branches' tails must share one canonical id.
                let first_tail = tail_ids[0];
                if tail_ids.iter().any(|&t| t != first_tail) {
                    continue;
                }

                matches.push((
                    class.id,
                    SuffixMatch {
                        head_branches: head_lists,
                        suffix_id: first_tail,
                    },
                ));
                break; // Only one match per class per iteration.
            }
        }
        matches
    }

    fn apply(
        &self,
        egraph: &mut EGraph<GrammarENode, A>,
        class_id: Id,
        m: Self::Match,
    ) {
        // Build each head branch as a Seq node (or a single child if
        // only one element remains).
        let mut new_alt_branches: Vec<Id> = Vec::with_capacity(m.head_branches.len());
        for head in &m.head_branches {
            let head_id = if head.len() == 1 {
                head[0]
            } else {
                egraph.add(GrammarENode::Seq(head.clone().into_boxed_slice()))
            };
            new_alt_branches.push(head_id);
        }

        // Fresh Alt over the head-only branches (no dispatch — lifting
        // the suffix may invalidate the precomputed AltDispatch if one
        // existed). The cost model decides post-saturation whether
        // this form is preferred.
        let new_alt_id = egraph.add(GrammarENode::Alt(
            new_alt_branches.into_boxed_slice(),
            None,
        ));

        // Wrap the new Alt + the shared suffix in a Seq.
        let new_seq_id = egraph.add(GrammarENode::Seq(
            vec![new_alt_id, m.suffix_id].into_boxed_slice(),
        ));

        // Union the original Alt class with the new Seq.
        egraph.union(class_id, new_seq_id);

        // Keep the compiler happy about the unused map type import so
        // the rule file stays parallel with its siblings.
        let _ = std::mem::size_of::<FxHashMap<Id, Id>>();
    }
}
