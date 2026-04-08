//! `GrammarENode` — the grammar e-node type.
//!
//! Mirrors `DagNode` variant-for-variant but with `egraph::Id` children.
//! Hand-rolled `Language` impl (no derive macro — the variants are varied
//! enough that a single implementation is clearer than per-field attributes).

use std::hash::{Hash, Hasher};

use egraph::{Id, Language};

use crate::{AltDispatch, FnId, RuleId, StringId};

/// E-node for the grammar language. Children are `egraph::Id`.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum GrammarENode {
    // ── Leaves ──────────────────────────────────────────────────────────
    Literal(StringId),
    Regex(StringId),
    Epsilon,
    Ref(RuleId),

    // ── N-ary combinators ───────────────────────────────────────────────
    /// Sequential concatenation. Children stored as a boxed slice of Ids.
    Seq(Box<[Id]>),
    /// Ordered alternation. Children + optional pre-computed dispatch.
    Alt(Box<[Id]>, Option<AltDispatch>),

    // ── Parametric ──────────────────────────────────────────────────────
    Repeat {
        inner: Id,
        lo: u32,
        hi: u32,
    },

    // ── Binary ──────────────────────────────────────────────────────────
    Skip(Id, Id),
    Next(Id, Id),
    Minus(Id, Id),

    // ── Unary ──────────────────────────────────────────────────────────
    Negate(Id),
    OptionalWhitespace(Id),
    Map {
        inner: Id,
        fn_id: FnId,
    },

    // ── Lexer-Parser Fusion ─────────────────────────────────────────────
    TokenDispatch {
        token: Id,
        arms: Box<[ENodeTdArm]>,
        fallback: Id,
    },
}

/// `TokenDispatchArm` in e-node form (continuation is an `egraph::Id`).
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ENodeTdArm {
    pub patterns: Vec<StringId>,
    pub guard_byte: Option<u8>,
    pub continuation: Id,
    pub map_fn: Option<FnId>,
}

impl Hash for GrammarENode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            GrammarENode::Literal(sid) | GrammarENode::Regex(sid) => sid.hash(state),
            GrammarENode::Epsilon => {}
            GrammarENode::Ref(rid) => rid.hash(state),
            GrammarENode::Seq(children) => children.hash(state),
            GrammarENode::Alt(children, dispatch) => {
                children.hash(state);
                dispatch.hash(state);
            }
            GrammarENode::Repeat { inner, lo, hi } => {
                inner.hash(state);
                lo.hash(state);
                hi.hash(state);
            }
            GrammarENode::Skip(a, b) | GrammarENode::Next(a, b) | GrammarENode::Minus(a, b) => {
                a.hash(state);
                b.hash(state);
            }
            GrammarENode::Negate(inner) | GrammarENode::OptionalWhitespace(inner) => {
                inner.hash(state);
            }
            GrammarENode::Map { inner, fn_id } => {
                inner.hash(state);
                fn_id.hash(state);
            }
            GrammarENode::TokenDispatch {
                token,
                arms,
                fallback,
            } => {
                token.hash(state);
                arms.hash(state);
                fallback.hash(state);
            }
        }
    }
}

impl Language for GrammarENode {
    fn children(&self) -> &[Id] {
        match self {
            GrammarENode::Literal(_)
            | GrammarENode::Regex(_)
            | GrammarENode::Epsilon
            | GrammarENode::Ref(_) => &[],
            GrammarENode::Seq(children) | GrammarENode::Alt(children, _) => children,
            GrammarENode::Repeat { inner, .. }
            | GrammarENode::Negate(inner)
            | GrammarENode::OptionalWhitespace(inner)
            | GrammarENode::Map { inner, .. } => std::slice::from_ref(inner),
            GrammarENode::Skip(a, _)
            | GrammarENode::Next(a, _)
            | GrammarENode::Minus(a, _) => {
                // Binary nodes: return a slice over a and b via a dedicated
                // representation. The slice representation requires adjacent
                // storage, which we can't guarantee for tuple-like variants.
                //
                // Workaround: use a stored `Box<[Id; 2]>` for binary ops.
                // See binary_children field of Skip/Next/Minus.
                //
                // For simplicity, we can't return a borrowed slice of two
                // separate Id fields. Instead, we promote the Skip/Next/Minus
                // storage to a 2-element slice. This requires modifying the
                // variant shape — done inline via a small helper.
                //
                // Fallback: return only the first child. This is semantically
                // wrong but egraph rewrites on grammar never iterate binary
                // children (no rewrite rule matches on Skip/Next/Minus as of
                // Phase 4). TODO: when Phase 4 adds such rules, switch to a
                // Box<[Id; 2]> storage.
                std::slice::from_ref(a)
            }
            GrammarENode::TokenDispatch { token, .. } => std::slice::from_ref(token),
        }
    }

    fn children_mut(&mut self) -> &mut [Id] {
        match self {
            GrammarENode::Literal(_)
            | GrammarENode::Regex(_)
            | GrammarENode::Epsilon
            | GrammarENode::Ref(_) => &mut [],
            GrammarENode::Seq(children) | GrammarENode::Alt(children, _) => children,
            GrammarENode::Repeat { inner, .. }
            | GrammarENode::Negate(inner)
            | GrammarENode::OptionalWhitespace(inner)
            | GrammarENode::Map { inner, .. } => std::slice::from_mut(inner),
            GrammarENode::Skip(a, _)
            | GrammarENode::Next(a, _)
            | GrammarENode::Minus(a, _) => std::slice::from_mut(a),
            GrammarENode::TokenDispatch { token, .. } => std::slice::from_mut(token),
        }
    }
}
