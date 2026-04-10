//! Tranche AA.2 — grammar-tier `Analysis<GrammarENode>` substrate.
//!
//! Replaces `NoAnalysis` with a concrete `GrammarAnalysis` struct whose
//! per-class data (`EClassFacts`) records the facts that downstream
//! phases of the tranche consume:
//!
//! - `first_set: CharSet128` — ASCII first-byte set. Union over all
//!   e-nodes in the class; monotonically grows under class unions.
//! - `nullable: bool` — true iff the class matches the empty string.
//!   Lattice join is `||`.
//! - `width: WidthBound` — `(min_bytes, Option<max_bytes>)`. Join is
//!   `(min(a, b), max(a, b) merged)` — meet on `min`, join on `max`,
//!   both widened lattice operations.
//! - `regex_sid` / `literal_sid` — fast-path accessors. Set iff the
//!   class contains at least one `Regex(sid)` / `Literal(sid)` leaf.
//!   Set to `None` when siblings disagree on the sid.
//!
//! The analysis is computed bottom-up during `make`; `merge` joins two
//! lattice values when classes union. `CspScheduler` re-runs analyses
//! incrementally as classes grow.
//!
//! Downstream consumers (Tranche AA.5 extraction→CSP bridge, AA.7
//! TaggedUnion narrowing, AA.11 structural bitmap miner) read from
//! `egraph.class(id).data` rather than walking subtrees.

pub mod facts;

pub use facts::{EClassFacts, WidthBound};

use bbnf_regex::sets::charset::CharSet128;
use egraph::{Analysis, EGraph};

use crate::egraph::node::GrammarENode;

/// Zero-sized substrate type. All per-class state lives in `EClassFacts`.
#[derive(Clone, Copy, Debug, Default)]
pub struct GrammarAnalysis;

impl Analysis<GrammarENode> for GrammarAnalysis {
    type Data = EClassFacts;

    fn make(egraph: &EGraph<GrammarENode, Self>, node: &GrammarENode) -> Self::Data {
        match node {
            GrammarENode::Epsilon => EClassFacts::epsilon(),
            GrammarENode::Literal(sid) => EClassFacts::literal(*sid),
            GrammarENode::Regex(sid) => EClassFacts::regex(*sid),
            GrammarENode::Ref(_) => EClassFacts::unknown_ref(),

            GrammarENode::Seq(children) => {
                // Seq: first_set accumulates from leading children while
                // they remain nullable; nullable iff every child nullable;
                // width = sum of child widths.
                let mut first = CharSet128::new();
                let mut nullable = true;
                let mut min_width: u32 = 0;
                let mut max_width: Option<u32> = Some(0);
                for id in children.iter() {
                    let child = &egraph.class(*id).data;
                    if nullable {
                        first.union(&child.first_set);
                    }
                    if !child.nullable {
                        nullable = false;
                    }
                    min_width = min_width.saturating_add(child.width.min);
                    max_width = match (max_width, child.width.max) {
                        (Some(a), Some(b)) => Some(a.saturating_add(b)),
                        _ => None,
                    };
                }
                EClassFacts {
                    first_set: first,
                    nullable,
                    width: WidthBound { min: min_width, max: max_width },
                    literal_sid: None,
                    regex_sid: None,
                }
            }

            GrammarENode::Alt(children, _) => {
                // Alt: first_set = union. Nullable iff any nullable.
                // Width = (min of min, max of max).
                let mut first = CharSet128::new();
                let mut nullable = false;
                let mut min_width: Option<u32> = None;
                let mut max_width: Option<u32> = Some(0);
                for id in children.iter() {
                    let child = &egraph.class(*id).data;
                    first.union(&child.first_set);
                    nullable |= child.nullable;
                    min_width = Some(match min_width {
                        None => child.width.min,
                        Some(cur) => cur.min(child.width.min),
                    });
                    max_width = match (max_width, child.width.max) {
                        (Some(a), Some(b)) => Some(a.max(b)),
                        _ => None,
                    };
                }
                EClassFacts {
                    first_set: first,
                    nullable,
                    width: WidthBound {
                        min: min_width.unwrap_or(0),
                        max: max_width,
                    },
                    literal_sid: None,
                    regex_sid: None,
                }
            }

            GrammarENode::Repeat { inner, lo, hi } => {
                let child = &egraph.class(*inner).data;
                let nullable = *lo == 0 || child.nullable;
                let min_width = child.width.min.saturating_mul(*lo);
                let max_width = if *hi == u32::MAX {
                    None
                } else {
                    child.width.max.map(|m| m.saturating_mul(*hi))
                };
                EClassFacts {
                    first_set: child.first_set.clone(),
                    nullable,
                    width: WidthBound { min: min_width, max: max_width },
                    literal_sid: None,
                    regex_sid: None,
                }
            }

            GrammarENode::Skip([l, r]) | GrammarENode::Next([l, r]) => {
                // `Skip(l, r)` parses both; result type comes from `l`. `Next(l, r)`
                // also parses both; result from `r`. Width is sum; for purposes
                // of first_set we use the left's first_set (since left runs first),
                // widened with the right's when the left is nullable.
                let left = &egraph.class(*l).data;
                let right = &egraph.class(*r).data;
                let min_width = left.width.min.saturating_add(right.width.min);
                let max_width = match (left.width.max, right.width.max) {
                    (Some(a), Some(b)) => Some(a.saturating_add(b)),
                    _ => None,
                };
                let nullable = left.nullable && right.nullable;
                let mut first_set = left.first_set.clone();
                if left.nullable {
                    first_set.union(&right.first_set);
                }
                EClassFacts {
                    first_set,
                    nullable,
                    width: WidthBound { min: min_width, max: max_width },
                    literal_sid: None,
                    regex_sid: None,
                }
            }

            GrammarENode::Minus([l, _r]) => {
                // Minus: l without matches of r. Approximate with l's facts.
                egraph.class(*l).data.clone()
            }

            GrammarENode::Negate(_inner) => {
                // Negative lookahead: consumes no input. Empty first_set,
                // nullable. Not a consumer — siblings that actually
                // consume will widen the lattice at merge time.
                EClassFacts {
                    first_set: CharSet128::new(),
                    nullable: true,
                    width: WidthBound { min: 0, max: Some(0) },
                    literal_sid: None,
                    regex_sid: None,
                }
            }

            GrammarENode::OptionalWhitespace(inner) => {
                let inner_facts = egraph.class(*inner).data.clone();
                EClassFacts {
                    first_set: inner_facts.first_set,
                    nullable: true,
                    width: WidthBound { min: 0, max: inner_facts.width.max },
                    literal_sid: None,
                    regex_sid: None,
                }
            }

            GrammarENode::Map { inner, .. } => {
                // Map preserves structure, transforms output. Facts match inner.
                egraph.class(*inner).data.clone()
            }

            GrammarENode::TokenDispatch { token, .. } => {
                // TokenDispatch: outermost facts match the token e-class.
                egraph.class(*token).data.clone()
            }
        }
    }

    fn merge(a: &mut Self::Data, b: Self::Data) -> bool {
        a.merge(&b)
    }
}
