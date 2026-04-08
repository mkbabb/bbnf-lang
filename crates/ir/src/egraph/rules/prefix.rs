//! Common-prefix factoring rewrite rules.
//!
//! Ports the legacy `factor_common_prefixes` pass (416 LOC) into two
//! focused e-graph rewrite rules:
//!
//! - [`FactorSharedSeqPrefix`] — `Alt([Seq(A, B), Seq(A, C)])` →
//!   `Seq(A, Alt([B, C]))`. Hoists a shared leading element out of
//!   two or more consecutive Alt branches.
//! - [`FactorLiteralByteTrie`] — `Alt([Literal("rem"), Literal("rlh")])` →
//!   `Seq(Literal("r"), Alt([Literal("em"), Literal("lh")]))`.
//!   Byte-level trie splitting that enables dispatch-table codegen.
//!
//! Both rules match on consecutive branch runs to preserve PEG-style
//! ordered-choice semantics — non-matching branches act as barriers.
//! Non-destructive: the unfactored form stays in the e-class and the
//! cost model picks between the two representations at extraction.

use egraph::{EGraph, Id, Rewrite};

use crate::egraph::analysis::GrammarAnalysis;
use crate::egraph::interner::SharedStrings;
use crate::egraph::node::GrammarENode;
use crate::{AltDispatch, FnId, StringId};

// ── Rule: Alt([Seq([A, B, …]), Seq([A, C, …])]) ≡ Alt([Seq([A, Alt([Seq([B…]), Seq([C…])])])]) ─

/// Hoist a shared leading child out of consecutive `Seq`-headed Alt
/// branches. Two or more branches whose first element is the same
/// e-class get rewritten into a single `Seq(prefix, Alt(remainders))`
/// branch; other branches are preserved in place.
pub struct FactorSharedSeqPrefix;

#[derive(Clone, Debug)]
pub struct SeqPrefixMatch {
    /// The rewritten branch list with the factored form spliced in.
    pub new_children: Box<[Id]>,
    pub dispatch: Option<AltDispatch>,
    /// Plan details captured by `search` — apply materializes the
    /// factored Seq/Alt nodes from these.
    pub plan: FactorPlan,
}

#[derive(Clone, Debug)]
pub struct FactorPlan {
    /// Index in `new_children` where the factored `Seq(prefix, inner_alt)`
    /// placeholder is; apply materializes the actual node here.
    pub placeholder_idx: usize,
    /// The shared leading child e-class.
    pub prefix_id: Id,
    /// Per-consumed-branch, the remainder Seq children (after stripping
    /// the leading prefix). Each entry becomes one branch of the
    /// inner Alt.
    pub remainders: Box<[Box<[Id]>]>,
}

impl Rewrite<GrammarENode, GrammarAnalysis> for FactorSharedSeqPrefix {
    type Match = SeqPrefixMatch;

    fn name(&self) -> &str {
        "factor-shared-seq-prefix"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, GrammarAnalysis>) -> Vec<(Id, Self::Match)> {
        let mut matches = Vec::new();
        for class in egraph.classes() {
            for node in class.iter() {
                let GrammarENode::Alt(children, dispatch) = node else {
                    continue;
                };
                if children.len() < 2 {
                    continue;
                }

                // Pre-extract each branch's leading e-class (canonical).
                // A branch contributes a leader iff its class contains a
                // Seq node — we take the first such Seq's first child.
                // Branches without a Seq form have `None` and act as
                // barriers.
                let leaders: Vec<Option<(Id, Box<[Id]>)>> = children
                    .iter()
                    .map(|&cid| {
                        egraph.class(cid).iter().find_map(|n| {
                            if let GrammarENode::Seq(seq_children) = n {
                                if !seq_children.is_empty() {
                                    return Some((
                                        egraph.find_ref(seq_children[0]),
                                        seq_children.clone(),
                                    ));
                                }
                            }
                            None
                        })
                    })
                    .collect();

                // Scan for consecutive runs sharing the same leader.
                let mut i = 0;
                while i < children.len() {
                    let Some((leader_i, _)) = &leaders[i] else {
                        i += 1;
                        continue;
                    };
                    let leader_i = *leader_i;
                    let mut j = i + 1;
                    while j < children.len() {
                        let Some((leader_j, _)) = &leaders[j] else {
                            break;
                        };
                        if *leader_j != leader_i {
                            break;
                        }
                        j += 1;
                    }
                    if j - i < 2 {
                        i += 1;
                        continue;
                    }

                    // Build the factored plan: strip the leading child
                    // from each of the i..j Seq forms, collecting the
                    // tails as remainders.
                    let mut remainders: Vec<Box<[Id]>> = Vec::with_capacity(j - i);
                    let mut all_empty = true;
                    for k in i..j {
                        let Some((_, seq_children)) = &leaders[k] else {
                            unreachable!()
                        };
                        let tail: Vec<Id> = seq_children[1..].to_vec();
                        if !tail.is_empty() {
                            all_empty = false;
                        }
                        remainders.push(tail.into_boxed_slice());
                    }
                    // If every tail is empty, factoring just produces
                    // `Seq(leader, Alt([Eps, Eps, …]))` which is
                    // non-productive. Skip.
                    if all_empty {
                        i = j;
                        continue;
                    }

                    // Splice the factored branch into a rebuilt children list:
                    //   children[..i] ++ [placeholder] ++ children[j..]
                    let mut new_children: Vec<Id> = Vec::with_capacity(children.len() - (j - i) + 1);
                    new_children.extend_from_slice(&children[..i]);
                    // Placeholder — apply replaces this with the actual
                    // Seq(leader, Alt(tails)) Id.
                    new_children.push(Id(u32::MAX));
                    let placeholder_idx = new_children.len() - 1;
                    new_children.extend_from_slice(&children[j..]);

                    matches.push((
                        class.id,
                        SeqPrefixMatch {
                            new_children: new_children.into_boxed_slice(),
                            dispatch: dispatch.clone(),
                            plan: FactorPlan {
                                placeholder_idx,
                                prefix_id: leader_i,
                                remainders: remainders.into_boxed_slice(),
                            },
                        },
                    ));
                    break; // one match per class per iteration
                }
            }
        }
        matches
    }

    fn apply(
        &self,
        egraph: &mut EGraph<GrammarENode, GrammarAnalysis>,
        class_id: Id,
        m: Self::Match,
    ) -> bool {
        // Materialize each remainder as either a Seq (multi-element),
        // a single child Id (singleton), or Epsilon (empty).
        let mut inner_branches: Vec<Id> = Vec::with_capacity(m.plan.remainders.len());
        for tail in m.plan.remainders.iter() {
            let id = match tail.len() {
                0 => egraph.add(GrammarENode::Epsilon),
                1 => tail[0],
                _ => egraph.add(GrammarENode::Seq(tail.clone())),
            };
            inner_branches.push(id);
        }

        let inner_alt_id = if inner_branches.len() == 1 {
            inner_branches[0]
        } else {
            egraph.add(GrammarENode::Alt(inner_branches.into_boxed_slice(), None))
        };

        let factored_id = egraph.add(GrammarENode::Seq(
            Box::new([m.plan.prefix_id, inner_alt_id]),
        ));

        let mut final_children: Vec<Id> = m.new_children.into_vec();
        final_children[m.plan.placeholder_idx] = factored_id;

        let new_alt = if final_children.len() == 1 {
            final_children[0]
        } else {
            egraph.add(GrammarENode::Alt(
                final_children.into_boxed_slice(),
                m.dispatch,
            ))
        };

        let before = egraph.find(class_id);
        egraph.union(class_id, new_alt);
        egraph.find(class_id) != before
    }
}

// ── Rule: Alt([Literal("rem"), Literal("rlh")]) ≡ Seq(Literal("r"), Alt([Literal("em"), Literal("lh")])) ─

/// Trie-style byte-level prefix factoring for consecutive Literal
/// branches. Two or more literals sharing a first byte get rewritten
/// into `Seq(Literal(byte), Alt(remainders))`. `Map(Literal, fn)`
/// wrappers are preserved on the continuation, so the enum variant
/// survives across the rewrite.
///
/// The shared interner is used to create the 1-byte prefix literal
/// and the remainder literals.
pub struct FactorLiteralByteTrie {
    pool: SharedStrings,
}

impl FactorLiteralByteTrie {
    pub fn new(pool: SharedStrings) -> Self {
        Self { pool }
    }

    /// Inspect an e-class for a `Literal` or `Map { inner: Literal, fn_id }`
    /// form with a ≥2-byte string. Returns the literal's `StringId` and
    /// an optional Map `FnId` wrapper.
    fn literal_form(
        egraph: &EGraph<GrammarENode, GrammarAnalysis>,
        id: Id,
    ) -> Option<(StringId, Option<FnId>)> {
        for node in egraph.class(id).iter() {
            match node {
                GrammarENode::Literal(sid) => return Some((*sid, None)),
                GrammarENode::Map { inner, fn_id } => {
                    for inner_node in egraph.class(*inner).iter() {
                        if let GrammarENode::Literal(sid) = inner_node {
                            return Some((*sid, Some(*fn_id)));
                        }
                    }
                }
                _ => {}
            }
        }
        None
    }
}

#[derive(Clone, Debug)]
pub struct TrieMatch {
    /// Rewritten Alt children: the consumed run at indices `run_start..run_end`
    /// is replaced by a single placeholder, later materialized in apply.
    pub new_children: Box<[Id]>,
    pub placeholder_idx: usize,
    pub dispatch: Option<AltDispatch>,
    /// The shared first-byte's pre-interned `StringId`.
    pub prefix_sid: StringId,
    /// Per-consumed-branch remainder plans: (remainder_sid_or_epsilon, map_fn).
    pub remainders: Box<[TrieRemainder]>,
}

#[derive(Clone, Debug)]
pub struct TrieRemainder {
    /// `Some(sid)` → `Literal(sid)`; `None` → `Epsilon` (the branch was
    /// a single-byte literal exactly equal to the prefix).
    pub sid: Option<StringId>,
    pub map_fn: Option<FnId>,
}

impl Rewrite<GrammarENode, GrammarAnalysis> for FactorLiteralByteTrie {
    type Match = TrieMatch;

    fn name(&self) -> &str {
        "factor-literal-byte-trie"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, GrammarAnalysis>) -> Vec<(Id, Self::Match)> {
        let mut matches = Vec::new();
        for class in egraph.classes() {
            for node in class.iter() {
                let GrammarENode::Alt(children, dispatch) = node else {
                    continue;
                };
                if children.len() < 2 {
                    continue;
                }

                // Classify each child: Some(first_byte, sid, map_fn) if it
                // has a Literal form with len ≥ 2, else None (barrier).
                let classified: Vec<Option<(u8, StringId, Option<FnId>)>> = children
                    .iter()
                    .map(|&cid| {
                        Self::literal_form(egraph, cid).and_then(|(sid, map_fn)| {
                            let s = self.pool.get(sid);
                            let bytes = s.as_bytes();
                            if bytes.len() >= 2 {
                                Some((bytes[0], sid, map_fn))
                            } else {
                                None
                            }
                        })
                    })
                    .collect();

                // Scan for a consecutive run of ≥2 literals sharing a first byte.
                let mut i = 0;
                while i < children.len() {
                    let Some((first_byte, _, _)) = classified[i] else {
                        i += 1;
                        continue;
                    };
                    let mut j = i + 1;
                    while j < children.len() {
                        match classified[j] {
                            Some((b, _, _)) if b == first_byte => j += 1,
                            _ => break,
                        }
                    }
                    if j - i < 2 {
                        i += 1;
                        continue;
                    }

                    // Intern the prefix literal and each remainder.
                    let prefix_str = (first_byte as char).to_string();
                    let prefix_sid = self.pool.intern(&prefix_str);

                    let mut remainders: Vec<TrieRemainder> = Vec::with_capacity(j - i);
                    for k in i..j {
                        let (_, sid, map_fn) = classified[k].unwrap();
                        let full = self.pool.get(sid);
                        let tail = &full[1..];
                        let rem_sid = if tail.is_empty() {
                            None
                        } else {
                            Some(self.pool.intern(tail))
                        };
                        remainders.push(TrieRemainder {
                            sid: rem_sid,
                            map_fn,
                        });
                    }

                    // Rebuild the outer children with a placeholder in slot i.
                    let mut new_children: Vec<Id> = Vec::with_capacity(children.len() - (j - i) + 1);
                    new_children.extend_from_slice(&children[..i]);
                    new_children.push(Id(u32::MAX));
                    let placeholder_idx = new_children.len() - 1;
                    new_children.extend_from_slice(&children[j..]);

                    matches.push((
                        class.id,
                        TrieMatch {
                            new_children: new_children.into_boxed_slice(),
                            placeholder_idx,
                            dispatch: dispatch.clone(),
                            prefix_sid,
                            remainders: remainders.into_boxed_slice(),
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
        egraph: &mut EGraph<GrammarENode, GrammarAnalysis>,
        class_id: Id,
        m: Self::Match,
    ) -> bool {
        // Materialize the prefix literal.
        let prefix_id = egraph.add(GrammarENode::Literal(m.prefix_sid));

        // Materialize each remainder, wrapping in Map(fn) when needed.
        let mut inner_branches: Vec<Id> = Vec::with_capacity(m.remainders.len());
        for rem in m.remainders.iter() {
            let base_id = match rem.sid {
                Some(sid) => egraph.add(GrammarENode::Literal(sid)),
                None => egraph.add(GrammarENode::Epsilon),
            };
            let wrapped_id = match rem.map_fn {
                Some(fn_id) => egraph.add(GrammarENode::Map {
                    inner: base_id,
                    fn_id,
                }),
                None => base_id,
            };
            inner_branches.push(wrapped_id);
        }

        let inner_alt_id = if inner_branches.len() == 1 {
            inner_branches[0]
        } else {
            egraph.add(GrammarENode::Alt(inner_branches.into_boxed_slice(), None))
        };

        let factored_id = egraph.add(GrammarENode::Seq(Box::new([prefix_id, inner_alt_id])));

        // Splice the placeholder with the factored Seq id.
        let mut final_children: Vec<Id> = m.new_children.into_vec();
        final_children[m.placeholder_idx] = factored_id;

        let new_id = if final_children.len() == 1 {
            final_children[0]
        } else {
            egraph.add(GrammarENode::Alt(
                final_children.into_boxed_slice(),
                m.dispatch,
            ))
        };

        let before = egraph.find(class_id);
        egraph.union(class_id, new_id);
        egraph.find(class_id) != before
    }
}
