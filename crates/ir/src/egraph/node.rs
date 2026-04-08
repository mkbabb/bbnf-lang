//! `GrammarENode` — the grammar e-node type.
//!
//! Mirrors `DagNode` variant-for-variant but with `egraph::Id` children.
//! `Language` is auto-derived via the augmented `egraph-derive`, which
//! classifies fields by type (`Id`, `Box<[Id]>`, `[Id; N]`) without any
//! per-field attributes.
//!
//! Multi-child tuple variants (`Skip`, `Next`, `Minus`) store both ids
//! in a fixed `[Id; 2]` array so `children() -> &[Id]` can return a
//! contiguous slice — the former "return only the first child" stub
//! (which was semantically wrong) is now correct.
//!
//! `TokenDispatch` currently flattens only its `token` field as a
//! child; the per-arm continuations stay in the metadata struct. When
//! Tranche B adds rewrite rules that need to match on arm continuations,
//! this representation will be extended accordingly.

use egraph_derive::Language;

use crate::{AltDispatch, FnId, RuleId, StringId};

/// E-node for the grammar language. Children are `egraph::Id`.
#[derive(Clone, PartialEq, Eq, Hash, Debug, Language)]
pub enum GrammarENode {
    // ── Leaves ──────────────────────────────────────────────────────────
    Literal(StringId),
    Regex(StringId),
    Epsilon,
    Ref(RuleId),

    // ── N-ary combinators ───────────────────────────────────────────────
    /// Sequential concatenation. Children stored as a boxed slice of Ids.
    Seq(Box<[::egraph::Id]>),
    /// Ordered alternation. Children + optional pre-computed dispatch.
    Alt(Box<[::egraph::Id]>, Option<AltDispatch>),

    // ── Parametric ──────────────────────────────────────────────────────
    Repeat {
        inner: ::egraph::Id,
        lo: u32,
        hi: u32,
    },

    // ── Binary (two Ids stored as fixed-size array for slice projection) ─
    Skip([::egraph::Id; 2]),
    Next([::egraph::Id; 2]),
    Minus([::egraph::Id; 2]),

    // ── Unary ──────────────────────────────────────────────────────────
    Negate(::egraph::Id),
    OptionalWhitespace(::egraph::Id),
    Map {
        inner: ::egraph::Id,
        fn_id: FnId,
    },

    // ── Lexer-Parser Fusion ─────────────────────────────────────────────
    TokenDispatch {
        token: ::egraph::Id,
        // arms carry their own per-arm `continuation: Id` but are treated
        // as opaque metadata by the Language impl — rewrite rules that
        // need to reach into arms will extend this representation.
        #[language(skip)]
        arms: Box<[ENodeTdArm]>,
        #[language(skip)]
        fallback: ::egraph::Id,
    },
}

/// `TokenDispatchArm` in e-node form (continuation is an `egraph::Id`).
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ENodeTdArm {
    pub patterns: Vec<StringId>,
    pub guard_byte: Option<u8>,
    pub continuation: ::egraph::Id,
    pub map_fn: Option<FnId>,
}
