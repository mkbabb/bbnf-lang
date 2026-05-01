//! Pass: Common prefix factoring for alternations.
//!
//! Rewrites `Alt([Seq(A, B), Seq(A, C)])` → `Seq(A, Alt([B, C]))` when branches
//! share a common prefix. Reduces backtracking by hoisting shared work.
//!
//! Also performs trie-style byte-level splitting of Literal alternation branches:
//! `Alt([Literal("rem"), Literal("rlh")])` → `Seq(Literal("r"), Alt([Literal("em"), Literal("lh")]))`
//! This enables dispatch tables for alternations where branches share a first byte.
//!
//! ## Performance
//!
//! The dedup map used for string interning is built **once per pass invocation**
//! in [`FactorCtx::new`] and updated incrementally as new prefix / suffix
//! substrings get interned via [`FactorCtx::intern`]. Branch ownership is
//! threaded via `std::mem::replace`, so no `IrNode` subtree is ever cloned in
//! the recursive trie split.

use rustc_hash::FxHashMap;

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
    let mut ctx = FactorCtx::new(&mut ir.strings);
    for rule in &mut ir.rules {
        let body = std::mem::replace(&mut rule.body, IrNode::Epsilon);
        rule.body = ctx.factor(body);
    }
}

// ─── Per-pass mutable context ───────────────────────────────────────────────

/// Per-pass mutable context for prefix factoring.
///
/// Owns the cross-call interning dedup map: built **once** per pass invocation
/// from the existing `ir.strings`, then updated incrementally as new prefix /
/// suffix substrings get interned. The previous implementation rebuilt this
/// map on every recursive `factor_literal_prefixes` call, which on CSS L4
/// dominated compile time (~92% inclusive on the post-V baseline).
struct FactorCtx<'a> {
    strings: &'a mut Vec<String>,
    /// Maps interned strings to their `StringId`. Built once at construction
    /// from the initial `strings` vector; updated by `intern`.
    dedup: FxHashMap<String, u32>,
}

impl<'a> FactorCtx<'a> {
    fn new(strings: &'a mut Vec<String>) -> Self {
        let mut dedup = FxHashMap::with_capacity_and_hasher(strings.len(), Default::default());
        for (i, s) in strings.iter().enumerate() {
            dedup.entry(s.clone()).or_insert(i as u32);
        }
        Self { strings, dedup }
    }

    /// Intern a string slice, returning its `StringId`. Reuses an existing
    /// entry if one is present.
    fn intern(&mut self, s: &str) -> u32 {
        if let Some(&existing) = self.dedup.get(s) {
            return existing;
        }
        let sid = self.strings.len() as u32;
        self.strings.push(s.to_owned());
        self.dedup.insert(s.to_owned(), sid);
        sid
    }

    /// Recursively factor common prefixes throughout the IR tree.
    fn factor(&mut self, node: IrNode) -> IrNode {
        match node {
            IrNode::Alt(branches, dispatch) => {
                // Recurse into children first.
                let branches: Vec<AltBranch> = branches
                    .into_iter()
                    .map(|mut b| {
                        b.node = self.factor(b.node);
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
                let factored = self.factor_literal_prefixes(factored);

                // Re-factor each produced branch to catch nested prefixes.
                let factored: Vec<AltBranch> = factored
                    .into_iter()
                    .map(|mut b| {
                        b.node = self.factor(b.node);
                        b
                    })
                    .collect();

                if factored.len() == 1 {
                    factored
                        .into_iter()
                        .next()
                        .expect("factored branch set verified non-empty")
                        .node
                } else {
                    IrNode::Alt(factored, dispatch)
                }
            }
            IrNode::Seq(children) => {
                IrNode::Seq(children.into_iter().map(|c| self.factor(c)).collect())
            }
            IrNode::Repeat { inner, lo, hi } => IrNode::Repeat {
                inner: Box::new(self.factor(*inner)),
                lo,
                hi,
            },
            IrNode::Skip(a, b) => {
                IrNode::Skip(Box::new(self.factor(*a)), Box::new(self.factor(*b)))
            }
            IrNode::Next(a, b) => {
                IrNode::Next(Box::new(self.factor(*a)), Box::new(self.factor(*b)))
            }
            IrNode::Minus(a, b) => {
                IrNode::Minus(Box::new(self.factor(*a)), Box::new(self.factor(*b)))
            }
            IrNode::Negate(inner) => IrNode::Negate(Box::new(self.factor(*inner))),
            IrNode::OptionalWhitespace(inner) => {
                IrNode::OptionalWhitespace(Box::new(self.factor(*inner)))
            }
            IrNode::Map { inner, fn_id } => IrNode::Map {
                inner: Box::new(self.factor(*inner)),
                fn_id,
            },
            IrNode::TokenDispatch {
                token,
                arms,
                fallback,
            } => IrNode::TokenDispatch {
                token: Box::new(self.factor(*token)),
                arms: arms
                    .into_iter()
                    .map(|mut a| {
                        a.continuation = self.factor(a.continuation);
                        a
                    })
                    .collect(),
                fallback: Box::new(self.factor(*fallback)),
            },
            other => other,
        }
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
    fn factor_literal_prefixes(&mut self, mut branches: Vec<AltBranch>) -> Vec<AltBranch> {
        if branches.len() < 2 {
            return branches;
        }

        // Classify each branch and pre-extract first bytes. We hold an
        // immutable borrow on `self.strings` for both walks; both release
        // before any mutation below.
        let mut literal_infos: Vec<Option<LiteralBranchInfo>> = Vec::with_capacity(branches.len());
        for branch in branches.iter() {
            literal_infos.push(classify_literal_branch(&branch.node, self.strings));
        }

        let first_bytes: Vec<Option<u8>> = literal_infos
            .iter()
            .map(|info| {
                info.as_ref()
                    .map(|li| self.strings[li.literal_sid as usize].as_bytes()[0])
            })
            .collect();

        // To preserve PEG-style ordered choice semantics, we only group
        // CONSECUTIVE literal branches with the same first byte. Non-literal
        // branches act as barriers.
        let mut result: Vec<AltBranch> = Vec::with_capacity(branches.len());
        let mut i = 0;
        while i < branches.len() {
            if let Some(first_byte) = first_bytes[i] {
                // Find consecutive run of literal branches sharing this byte.
                let mut j = i + 1;
                while j < branches.len() {
                    match first_bytes[j] {
                        Some(fb) if fb == first_byte => j += 1,
                        _ => break,
                    }
                }

                if j - i >= 2 {
                    // Pre-extract all remainder strings in the group while
                    // `self.strings` is still immutably borrowed; this releases
                    // the borrow before we mutate via `intern_byte` / `intern`.
                    let group: Vec<(Box<str>, Option<FnId>)> = (i..j)
                        .map(|k| {
                            let li = literal_infos[k]
                                .as_ref()
                                .expect("literal info present for group member");
                            // Safe because the first byte is ASCII (single
                            // codepoint) — see classify_literal_branch.
                            let full = &self.strings[li.literal_sid as usize];
                            (Box::<str>::from(&full[1..]), li.map_fn)
                        })
                        .collect();

                    // Now `self.strings` is no longer borrowed; we can intern.
                    let prefix_sid = self.intern_byte(first_byte);

                    let mut continuation_branches: Vec<AltBranch> = Vec::with_capacity(group.len());
                    for (rem, map_fn) in group {
                        let continuation_node = if rem.is_empty() {
                            IrNode::Epsilon
                        } else {
                            let rem_sid = self.intern(&rem);
                            IrNode::Literal(rem_sid)
                        };

                        let wrapped = if let Some(fn_id) = map_fn {
                            IrNode::Map {
                                inner: Box::new(continuation_node),
                                fn_id,
                            }
                        } else {
                            continuation_node
                        };

                        continuation_branches.push(AltBranch {
                            node: wrapped,
                            first_set: None,
                        });
                    }

                    // Recursively factor continuation branches (deep trie).
                    // After splitting on first byte, the remainders may still
                    // share a common second byte → recurse to build a full trie.
                    let continuation_branches = self.factor_literal_prefixes(continuation_branches);

                    let continuation = if continuation_branches.len() == 1 {
                        continuation_branches
                            .into_iter()
                            .next()
                            .expect("continuation branch set verified non-empty")
                            .node
                    } else {
                        IrNode::Alt(continuation_branches, None)
                    };

                    result.push(AltBranch {
                        node: IrNode::Seq(vec![IrNode::Literal(prefix_sid), continuation]),
                        first_set: None,
                    });

                    i = j;
                    continue;
                }
            }

            // Single literal or non-literal branch — take ownership without
            // cloning. The placeholder `Epsilon` is overwritten on the next
            // iteration's `result.push`.
            result.push(take_branch(&mut branches[i]));
            i += 1;
        }

        result
    }

    /// Intern a single ASCII byte as a one-character string.
    ///
    /// Single-byte literal prefixes are emitted only when the first byte of
    /// a literal is a complete UTF-8 codepoint (i.e. ASCII), which is enforced
    /// upstream by [`classify_literal_branch`].
    fn intern_byte(&mut self, b: u8) -> u32 {
        debug_assert!(b.is_ascii(), "single-byte prefix must be ASCII");
        let buf = [b];
        let s = std::str::from_utf8(&buf).expect("ASCII byte is valid UTF-8");
        self.intern(s)
    }
}

// ─── Free helpers ────────────────────────────────────────────────────────────

/// Take an `AltBranch` from a vector slot via `mem::replace`, leaving an
/// `Epsilon` placeholder. Avoids cloning the branch's `IrNode` subtree.
#[inline]
fn take_branch(slot: &mut AltBranch) -> AltBranch {
    std::mem::replace(
        slot,
        AltBranch {
            node: IrNode::Epsilon,
            first_set: None,
        },
    )
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
                children
                    .into_iter()
                    .next()
                    .expect("Seq child list verified non-empty")
            } else {
                IrNode::Seq(children)
            }
        }
        // Single-node or non-Seq: nothing left after stripping.
        _ => IrNode::Epsilon,
    }
}

/// True if `node` is a `Seq` with more than one child (productive after `strip_leading`).
#[inline]
fn is_multi_seq(node: &IrNode) -> bool {
    matches!(node, IrNode::Seq(c) if c.len() > 1)
}

/// Group branches by common leading node and merge groups of size > 1.
///
/// Branch ownership is threaded via `std::mem::replace`, so no `IrNode`
/// subtree is cloned for branches in the no-prefix or non-productive cases.
/// Only the leader is cloned (one shallow clone per productive group), since
/// it must appear in the factored Seq output.
fn factor_branches(mut branches: Vec<AltBranch>) -> Vec<AltBranch> {
    if branches.len() < 2 {
        return branches;
    }

    let mut result: Vec<AltBranch> = Vec::with_capacity(branches.len());
    let mut i = 0;

    while i < branches.len() {
        // Find run of branches sharing leader[i]. Both `branches[i].node` and
        // `branches[j].node` are immutably borrowed within the inner loop;
        // both borrows release before the `result.push` below.
        let mut j = i + 1;
        while j < branches.len()
            && leading_node(&branches[j].node) == leading_node(&branches[i].node)
        {
            j += 1;
        }

        if j - i == 1 {
            // No common prefix — take ownership without cloning.
            result.push(take_branch(&mut branches[i]));
        } else {
            // Pre-detect the all-single (non-productive) case: all branches
            // are single nodes (not multi-element Seqs), so stripping the
            // leader would yield Epsilon for every remainder.
            let all_single = (i..j).all(|k| !is_multi_seq(&branches[k].node));

            if all_single {
                for k in i..j {
                    result.push(take_branch(&mut branches[k]));
                }
            } else {
                // Productive factoring. Clone the leader once for the result;
                // strip leaders from each branch by taking ownership.
                let leader = leading_node(&branches[i].node).clone();

                let mut remainder_branches: Vec<AltBranch> = Vec::with_capacity(j - i);
                for k in i..j {
                    let original_node = std::mem::replace(&mut branches[k].node, IrNode::Epsilon);
                    let stripped = strip_leading(original_node);
                    remainder_branches.push(AltBranch {
                        node: stripped,
                        first_set: None,
                    });
                }

                let remainder_alt = if remainder_branches.len() == 1 {
                    remainder_branches
                        .into_iter()
                        .next()
                        .expect("remainder branch set verified non-empty")
                        .node
                } else {
                    IrNode::Alt(remainder_branches, None)
                };

                result.push(AltBranch {
                    node: IrNode::Seq(vec![leader, remainder_alt]),
                    first_set: None,
                });
            }
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

/// Classify a branch node as a splittable literal.
///
/// Returns `Some(info)` if the node is `Literal(sid)` or
/// `Map { inner: Literal(sid), fn_id }` AND the literal has length >= 2 AND
/// the first byte is ASCII (so a single-byte split is a complete UTF-8
/// codepoint).
fn classify_literal_branch(node: &IrNode, strings: &[String]) -> Option<LiteralBranchInfo> {
    match node {
        IrNode::Literal(sid) => {
            let s = &strings[*sid as usize];
            if s.len() >= 2 && s.as_bytes()[0].is_ascii() {
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
                if s.len() >= 2 && s.as_bytes()[0].is_ascii() {
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
