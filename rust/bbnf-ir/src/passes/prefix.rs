//! Pass: Common prefix factoring for alternations.
//!
//! Rewrites `Alt([Seq(A, B), Seq(A, C)])` → `Seq(A, Alt([B, C]))` when branches
//! share a common prefix. Reduces backtracking by hoisting shared work.
//!
//! Also performs trie-style byte-level splitting of Literal alternation branches:
//! `Alt([Literal("rem"), Literal("rlh")])` → `Seq(Literal("r"), Alt([Literal("em"), Literal("lh")]))`
//! This enables dispatch tables for alternations where branches share a first byte.

use std::collections::HashMap;

use crate::{AltBranch, FnId, GrammarIR, IrNode, StringId};

/// Factor common prefixes out of alternation branches.
///
/// Walks the entire IR tree and rewrites any `Alt` node whose branches
/// share a common leading node. The factored prefix becomes a `Seq`
/// wrapping the remaining alternation.
///
/// After node-level factoring, performs byte-level literal splitting:
/// groups Literal branches by their first byte and splits groups of 2+
/// into `Seq(prefix_byte, Alt(remainders))`.
pub fn factor_common_prefixes(ir: &mut GrammarIR) {
    // Cannot use par_iter_mut because literal splitting needs &mut ir.strings.
    for rule in &mut ir.rules {
        rule.body = factor(
            std::mem::replace(&mut rule.body, IrNode::Epsilon),
            &mut ir.strings,
        );
    }
}

fn factor(node: IrNode, strings: &mut Vec<String>) -> IrNode {
    match node {
        IrNode::Alt(branches, dispatch) => {
            // Recurse into children first.
            let branches: Vec<AltBranch> = branches
                .into_iter()
                .map(|mut b| {
                    b.node = factor(b.node, strings);
                    b
                })
                .collect();

            // Group branches by their leading node, then re-factor remainder
            // alternations to catch depth-2+ prefixes. E.g.:
            //   Alt([Seq(A,B,C), Seq(A,B,D)]) → Seq(A, Alt([Seq(B,C), Seq(B,D)]))
            // First pass factors out A, second pass (via recursive factor call on
            // the remainder) factors out B.
            let factored = factor_branches(branches);

            // Byte-level literal prefix splitting (trie-style).
            let factored = factor_literal_prefixes(factored, strings);

            // Re-factor each produced branch to catch nested prefixes.
            let factored: Vec<AltBranch> = factored
                .into_iter()
                .map(|mut b| {
                    b.node = factor(b.node, strings);
                    b
                })
                .collect();

            if factored.len() == 1 {
                factored.into_iter().next().unwrap().node
            } else {
                IrNode::Alt(factored, dispatch)
            }
        }
        IrNode::Seq(children) => {
            IrNode::Seq(children.into_iter().map(|c| factor(c, strings)).collect())
        }
        IrNode::Repeat { inner, lo, hi } => IrNode::Repeat {
            inner: Box::new(factor(*inner, strings)),
            lo,
            hi,
        },
        IrNode::Skip(a, b) => {
            IrNode::Skip(Box::new(factor(*a, strings)), Box::new(factor(*b, strings)))
        }
        IrNode::Next(a, b) => {
            IrNode::Next(Box::new(factor(*a, strings)), Box::new(factor(*b, strings)))
        }
        IrNode::Minus(a, b) => {
            IrNode::Minus(Box::new(factor(*a, strings)), Box::new(factor(*b, strings)))
        }
        IrNode::Negate(inner) => IrNode::Negate(Box::new(factor(*inner, strings))),
        IrNode::OptionalWhitespace(inner) => {
            IrNode::OptionalWhitespace(Box::new(factor(*inner, strings)))
        }
        IrNode::Map { inner, fn_id } => IrNode::Map {
            inner: Box::new(factor(*inner, strings)),
            fn_id,
        },
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => IrNode::TokenDispatch {
            token: Box::new(factor(*token, strings)),
            arms: arms
                .into_iter()
                .map(|mut a| {
                    a.continuation = factor(a.continuation, strings);
                    a
                })
                .collect(),
            fallback: Box::new(factor(*fallback, strings)),
        },
        other => other,
    }
}

/// Extract the leading node from a branch (first element of Seq, or the node itself).
fn leading_node(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Seq(children) if !children.is_empty() => &children[0],
        other => other,
    }
}

/// Strip the leading node from a branch, returning the remainder.
fn strip_leading(node: IrNode) -> IrNode {
    match node {
        IrNode::Seq(mut children) if children.len() > 1 => {
            children.remove(0);
            if children.len() == 1 {
                children.into_iter().next().unwrap()
            } else {
                IrNode::Seq(children)
            }
        }
        // Single-node or non-Seq: nothing left after stripping.
        _ => IrNode::Epsilon,
    }
}

/// Group branches by common leading node and merge groups of size > 1.
fn factor_branches(branches: Vec<AltBranch>) -> Vec<AltBranch> {
    if branches.len() < 2 {
        return branches;
    }

    // Collect runs of branches with the same leading node.
    // We use sequential grouping (not arbitrary grouping) to preserve
    // alternation order semantics.
    let mut result: Vec<AltBranch> = Vec::new();
    let mut i = 0;

    while i < branches.len() {
        let leader = leading_node(&branches[i].node).clone();

        // Find how many consecutive branches share this leader.
        let mut j = i + 1;
        while j < branches.len() && leading_node(&branches[j].node) == &leader {
            j += 1;
        }

        if j - i == 1 {
            // No common prefix — keep as-is.
            result.push(branches[i].clone());
        } else {
            // Factor out the common prefix.
            let remainder_branches: Vec<AltBranch> = branches[i..j]
                .iter()
                .map(|b| AltBranch {
                    node: strip_leading(b.node.clone()),
                    first_set: None,
                })
                .collect();

            // If all remainders are Epsilon, the branches were identical single
            // nodes — factoring just wraps in Seq(leader, Alt([Eps,...])) which
            // is non-productive. Keep the original branches as-is.
            if remainder_branches
                .iter()
                .all(|b| b.node == IrNode::Epsilon)
            {
                for b in &branches[i..j] {
                    result.push(b.clone());
                }
                i = j;
                continue;
            }

            let remainder_alt = if remainder_branches.len() == 1 {
                remainder_branches.into_iter().next().unwrap().node
            } else {
                IrNode::Alt(remainder_branches, None)
            };

            let factored_node = IrNode::Seq(vec![leader.clone(), remainder_alt]);

            result.push(AltBranch {
                node: factored_node,
                first_set: None,
            });
        }

        i = j;
    }

    result
}

// ─── Byte-level literal prefix splitting ─────────────────────────────────────

/// Information about a literal branch that can participate in byte-level splitting.
/// Handles both bare `Literal(sid)` and `Map { inner: Literal(sid), fn_id }`.
struct LiteralBranchInfo {
    /// The string ID of the literal.
    literal_sid: StringId,
    /// If the literal was wrapped in Map, the FnId to preserve on the continuation.
    map_fn: Option<FnId>,
}

/// Intern a string, deduplicating against existing entries.
fn intern_or_reuse(s: &str, strings: &mut Vec<String>, dedup: &mut HashMap<String, u32>) -> u32 {
    if let Some(&existing) = dedup.get(s) {
        return existing;
    }
    let sid = strings.len() as u32;
    dedup.insert(s.to_owned(), sid);
    strings.push(s.to_owned());
    sid
}

/// Trie-style byte-level prefix factoring for Literal alternation branches.
///
/// Groups Literal (and Map(Literal, fn)) branches by their first byte and,
/// for each group of 2+ members, splits them:
///
/// ```text
/// Alt([Literal("rem"), Literal("rlh"), Literal("em"), Literal("ex")])
/// →
/// Alt([
///     Seq(Literal("r"), Alt([Literal("em"), Literal("lh")])),
///     Seq(Literal("e"), Alt([Literal("m"), Literal("x")])),
/// ])
/// ```
///
/// Map wrappers are preserved on the continuation, not the prefix:
/// `Map(Literal("rem"), fn)` → prefix `Literal("r")` + continuation `Map(Literal("em"), fn)`.
fn factor_literal_prefixes(
    branches: Vec<AltBranch>,
    strings: &mut Vec<String>,
) -> Vec<AltBranch> {
    if branches.len() < 2 {
        return branches;
    }

    // Classify each branch: is it a Literal or Map(Literal, _) with len >= 2?
    let mut literal_infos: Vec<Option<LiteralBranchInfo>> = Vec::with_capacity(branches.len());
    for branch in branches.iter() {
        literal_infos.push(classify_literal_branch(&branch.node, strings));
    }

    // Group by first byte. We need to preserve order, so we do sequential grouping
    // (consecutive branches with the same first byte).
    // But unlike the node-level grouping, here we group ALL branches with the same
    // first byte (not just consecutive), since literal ordering within a group doesn't
    // affect correctness — they're all disjoint on continuation.
    //
    // Actually, to preserve alternation order semantics (PEG-style ordered choice),
    // we must only group CONSECUTIVE literal branches with the same first byte.
    // Non-literal branches act as barriers.

    // Collect runs of literal branches that share a first byte.
    let mut result: Vec<AltBranch> = Vec::new();
    let mut dedup: HashMap<String, u32> = HashMap::new();

    // Pre-populate dedup with existing strings for efficient reuse.
    for (i, s) in strings.iter().enumerate() {
        dedup.entry(s.clone()).or_insert(i as u32);
    }

    // Pre-extract first bytes to avoid borrowing strings during mutation.
    let first_bytes: Vec<Option<u8>> = literal_infos
        .iter()
        .map(|info| {
            info.as_ref()
                .map(|li| strings[li.literal_sid as usize].as_bytes()[0])
        })
        .collect();

    let mut i = 0;
    while i < branches.len() {
        if let Some(first_byte) = first_bytes[i] {
            // Find how many consecutive literal branches share this first byte.
            let mut j = i + 1;
            while j < branches.len() {
                if let Some(fb) = first_bytes[j] {
                    if fb == first_byte {
                        j += 1;
                        continue;
                    }
                }
                break;
            }

            if j - i >= 2 {
                // Split this group: factor out the shared first byte.
                let prefix_s = String::from(first_byte as char);
                let prefix_sid = intern_or_reuse(&prefix_s, strings, &mut dedup);

                // Collect remainders (clone strings to avoid borrow conflict).
                let remainders: Vec<(String, Option<FnId>)> = (i..j)
                    .map(|k| {
                        let li = literal_infos[k].as_ref().unwrap();
                        let full_str = strings[li.literal_sid as usize].clone();
                        (full_str[1..].to_owned(), li.map_fn)
                    })
                    .collect();

                let mut continuation_branches: Vec<AltBranch> = Vec::new();
                for (remainder, map_fn) in &remainders {
                    let continuation_node = if remainder.is_empty() {
                        IrNode::Epsilon
                    } else {
                        let rem_sid = intern_or_reuse(remainder, strings, &mut dedup);
                        IrNode::Literal(rem_sid)
                    };

                    // Preserve Map wrapper on the continuation.
                    let wrapped = if let Some(fn_id) = map_fn {
                        IrNode::Map {
                            inner: Box::new(continuation_node),
                            fn_id: *fn_id,
                        }
                    } else {
                        continuation_node
                    };

                    continuation_branches.push(AltBranch {
                        node: wrapped,
                        first_set: None,
                    });
                }

                let continuation = if continuation_branches.len() == 1 {
                    continuation_branches.into_iter().next().unwrap().node
                } else {
                    IrNode::Alt(continuation_branches, None)
                };

                result.push(AltBranch {
                    node: IrNode::Seq(vec![IrNode::Literal(prefix_sid), continuation]),
                    first_set: None,
                });

                i = j;
            } else {
                // Single literal branch with this first byte — keep as-is.
                result.push(branches[i].clone());
                i += 1;
            }
        } else {
            // Non-literal branch — keep as-is.
            result.push(branches[i].clone());
            i += 1;
        }
    }

    result
}

/// Classify a branch node as a splittable literal.
/// Returns `Some(info)` if the node is `Literal(sid)` or `Map { inner: Literal(sid), fn_id }`
/// and the literal has length >= 2 (single-byte literals can't be split further).
fn classify_literal_branch(
    node: &IrNode,
    strings: &[String],
) -> Option<LiteralBranchInfo> {
    match node {
        IrNode::Literal(sid) => {
            let s = &strings[*sid as usize];
            if s.len() >= 2 {
                Some(LiteralBranchInfo {
                    literal_sid: *sid,
                    map_fn: None,
                })
            } else {
                None
            }
        }
        IrNode::Map { inner, fn_id } => {
            if let IrNode::Literal(sid) = inner.as_ref() {
                let s = &strings[*sid as usize];
                if s.len() >= 2 {
                    Some(LiteralBranchInfo {
                        literal_sid: *sid,
                        map_fn: Some(*fn_id),
                    })
                } else {
                    None
                }
            } else {
                None
            }
        }
        _ => None,
    }
}
