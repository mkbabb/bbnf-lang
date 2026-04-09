//! `IrNode` — the expression tree node — plus its source-span tag, alternation
//! sub-types, token-dispatch arm, and the structural walking helpers
//! (`for_each_child`, `for_each_child_mut`, `count_nodes`).

use serde::{Deserialize, Serialize};

use bbnf_regex::sets::charset::CharSet128;

use super::{FnId, RuleId, StringId};

// ── Source mapping ──────────────────────────────────────────────────────────

/// Byte range in the original grammar source file.
///
/// Used for DWARF-like source mapping from IR/bytecode back to grammar source.
/// Owned (no lifetime) so it survives serialization across the WASM boundary.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Default)]
pub struct GrammarSpan {
    pub start: u32,
    pub end: u32,
}

// ── IR Nodes ────────────────────────────────────────────────────────────────

/// A single node in the grammar IR tree.
///
/// Each variant maps 1:1 from an `Expression` AST variant, with syntactic sugar
/// (Group, OptionalWhitespace) eliminated and all string references interned.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum IrNode {
    // ── Leaves ──────────────────────────────────────────────────────────
    /// A literal string match. The `StringId` indexes `GrammarIR::strings`.
    Literal(StringId),

    /// A regex match. The `StringId` indexes `GrammarIR::strings`.
    Regex(StringId),

    /// Matches the empty string.
    Epsilon,

    // ── Combinators (N-ary flat) ────────────────────────────────────────
    /// Sequential concatenation: all children must match in order.
    Seq(Vec<IrNode>),

    /// Ordered alternation: try each branch until one matches.
    /// The optional `AltDispatch` is set by the dispatch pass when all branches
    /// have pairwise disjoint FIRST sets.
    Alt(Vec<AltBranch>, Option<AltDispatch>),

    /// Repetition: `lo..=hi` occurrences of `inner`.
    /// - `(0, 1)` → optional
    /// - `(0, u32::MAX)` → many (zero or more)
    /// - `(1, u32::MAX)` → many1 (one or more)
    Repeat {
        inner: Box<IrNode>,
        lo: u32,
        hi: u32,
    },

    /// Reference to another rule by its `RuleId`.
    Ref(RuleId),

    // ── Binary ──────────────────────────────────────────────────────────
    /// `a << b` — parse both, keep left result.
    Skip(Box<IrNode>, Box<IrNode>),

    /// `a >> b` — parse both, keep right result.
    Next(Box<IrNode>, Box<IrNode>),

    /// Set-difference: match `lhs` only if `rhs` does NOT match at the same position.
    Minus(Box<IrNode>, Box<IrNode>),

    // ── Lookahead ───────────────────────────────────────────────────────
    /// Zero-width negative assertion: fails if inner matches, consumes nothing.
    Negate(Box<IrNode>),

    // ── Host integration ────────────────────────────────────────────────
    /// Apply a host function to the parse result.
    Map { inner: Box<IrNode>, fn_id: FnId },

    // ── Whitespace ──────────────────────────────────────────────────────
    /// Marks the inner expression as having optional surrounding whitespace.
    /// This is a flag-level concept: backends translate it to `trim_whitespace(inner)`.
    OptionalWhitespace(Box<IrNode>),

    // ── Lexer-Parser Fusion ──────────────────────────────────────────
    /// Parse a token once, then dispatch on its string value.
    /// Eliminates sequential checkpoint/restore for alternations where multiple
    /// branches start with the same token class (e.g., identifier-led CSS values).
    TokenDispatch {
        /// The token expression to parse (e.g., Regex for identifier class).
        token: Box<IrNode>,
        /// Dispatch arms: each maps string patterns to a continuation body.
        arms: Vec<TokenDispatchArm>,
        /// Fallback arm: tried when no pattern matches (e.g., bare ident).
        fallback: Box<IrNode>,
    },
}

/// A single arm of a `TokenDispatch` node.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct TokenDispatchArm {
    /// String values that trigger this arm (e.g., ["var"], ["calc", "min"]).
    /// Each `StringId` indexes `GrammarIR::strings`.
    pub patterns: Vec<StringId>,
    /// Optional guard: a single-byte literal that must follow the token
    /// (e.g., `(` for function calls). `None` means no guard.
    pub guard_byte: Option<u8>,
    /// Continuation body after the token (+ guard) is consumed.
    pub continuation: IrNode,
    /// The original branch's Map wrapping (enum variant).
    pub map_fn: Option<FnId>,
}

/// A single branch of an `Alt` node, with optional pre-computed FIRST set.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct AltBranch {
    /// The branch expression.
    pub node: IrNode,
    /// Pre-computed FIRST set for this branch (from analysis pass).
    /// `None` if not yet computed or if the branch is nullable.
    pub first_set: Option<CharSet128>,
}

/// Pre-computed dispatch table for an `Alt` node with disjoint FIRST sets.
/// Set by the dispatch pass; consumed directly by the bytecode compiler.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub struct AltDispatch {
    /// 128-entry table: `table[byte]` = branch index, or 255 for no match.
    pub table: Vec<u8>,
    /// When set, this branch is a catch-all fallback tried when a dispatched
    /// branch fails, or when no dispatch entry matches.  Enables dispatch
    /// tables even when the last Alt branch has a superset FIRST set.
    pub fallback_idx: Option<u8>,
}

// ── Walking helpers ─────────────────────────────────────────────────────────

impl IrNode {
    /// Visit each direct child node, calling `f` on each.
    ///
    /// Leaves (Literal, Regex, Epsilon, Ref) have no children.
    /// For Alt, visits the `node` field of each `AltBranch`.
    /// For TokenDispatch, visits `token`, each arm's `continuation`, and `fallback`.
    pub fn for_each_child(&self, f: &mut impl FnMut(&IrNode)) {
        match self {
            IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
            IrNode::Seq(children) => children.iter().for_each(f),
            IrNode::Alt(branches, _) => branches.iter().for_each(|b| f(&b.node)),
            IrNode::Repeat { inner, .. }
            | IrNode::Negate(inner)
            | IrNode::OptionalWhitespace(inner)
            | IrNode::Map { inner, .. } => f(inner),
            IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
                f(a);
                f(b);
            }
            IrNode::TokenDispatch {
                token,
                arms,
                fallback,
            } => {
                f(token);
                for arm in arms {
                    f(&arm.continuation);
                }
                f(fallback);
            }
        }
    }

    /// Mutable version of `for_each_child`.
    pub fn for_each_child_mut(&mut self, f: &mut impl FnMut(&mut IrNode)) {
        match self {
            IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
            IrNode::Seq(children) => children.iter_mut().for_each(f),
            IrNode::Alt(branches, _) => branches.iter_mut().for_each(|b| f(&mut b.node)),
            IrNode::Repeat { inner, .. }
            | IrNode::Negate(inner)
            | IrNode::OptionalWhitespace(inner)
            | IrNode::Map { inner, .. } => f(inner),
            IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
                f(a);
                f(b);
            }
            IrNode::TokenDispatch {
                token,
                arms,
                fallback,
            } => {
                f(token);
                for arm in arms {
                    f(&mut arm.continuation);
                }
                f(fallback);
            }
        }
    }
}

/// Count the total number of nodes in an `IrNode` tree.
pub fn count_nodes(node: &IrNode) -> usize {
    let mut count = 0;
    node.for_each_child(&mut |child| count += count_nodes(child));
    1 + count
}
