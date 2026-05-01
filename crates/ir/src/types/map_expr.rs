//! Structured value expressions used by the `->` map syntax.
//!
//! `MapExpr` is the user-facing payload of `FnDescriptor::Expr`. Every `->`
//! mapping in the grammar lowers to a `MapExpr` tree that IR passes can
//! introspect for constant folding, type projection, and pattern-based
//! specialization.

use serde::{Deserialize, Serialize};

use super::StringId;

/// A structured value expression used in `->` map syntax.
///
/// Transparent to IR passes — every node is inspectable for constant folding,
/// type projection, and pattern-based specialization.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum MapExpr {
    /// Integer literal: `0`, `42`, `-1`.
    IntLit(i64),
    /// Float literal: `3.14`, `-1.0`.
    FloatLit(f64),
    /// Boolean literal: `true`, `false`.
    BoolLit(bool),
    /// String literal (interned).
    StringLit(StringId),
    /// The parse result (implicit `input`).
    Input,
    /// Property access on input: `input.len`, `input.as_str`.
    InputProp { prop: StringId },
    /// Function call: `parse_hex(input)`, `len(input)`.
    FnCall { name: StringId, args: Vec<MapExpr> },
    /// Binary operation: `a + b`, `a == b`.
    BinOp {
        op: MapBinOp,
        lhs: Box<MapExpr>,
        rhs: Box<MapExpr>,
    },
    /// Unary operation: `-a`, `!a`.
    UnaryOp { op: MapUnaryOp, inner: Box<MapExpr> },
}

/// Binary operators for value expressions.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Copy)]
pub enum MapBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
    BitAnd,
    BitOr,
    Shl,
    Shr,
}

/// Unary operators for value expressions.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Copy)]
pub enum MapUnaryOp {
    Neg,
    Not,
}

impl MapExpr {
    /// Visit each direct child expression.
    pub fn for_each_child(&self, f: &mut impl FnMut(&MapExpr)) {
        match self {
            MapExpr::IntLit(_)
            | MapExpr::FloatLit(_)
            | MapExpr::BoolLit(_)
            | MapExpr::StringLit(_)
            | MapExpr::Input
            | MapExpr::InputProp { .. } => {}
            MapExpr::FnCall { args, .. } => args.iter().for_each(f),
            MapExpr::BinOp { lhs, rhs, .. } => {
                f(lhs);
                f(rhs);
            }
            MapExpr::UnaryOp { inner, .. } => f(inner),
        }
    }

    /// Mutable version of `for_each_child`.
    pub fn for_each_child_mut(&mut self, f: &mut impl FnMut(&mut MapExpr)) {
        match self {
            MapExpr::IntLit(_)
            | MapExpr::FloatLit(_)
            | MapExpr::BoolLit(_)
            | MapExpr::StringLit(_)
            | MapExpr::Input
            | MapExpr::InputProp { .. } => {}
            MapExpr::FnCall { args, .. } => args.iter_mut().for_each(f),
            MapExpr::BinOp { lhs, rhs, .. } => {
                f(lhs);
                f(rhs);
            }
            MapExpr::UnaryOp { inner, .. } => f(inner),
        }
    }

    /// Constant-fold this expression tree where possible.
    pub fn constant_fold(&mut self) {
        // Recurse first.
        self.for_each_child_mut(&mut |child| child.constant_fold());

        // Then fold this node.
        let folded = match self {
            MapExpr::BinOp { op, lhs, rhs } => match (op, lhs.as_ref(), rhs.as_ref()) {
                (MapBinOp::Add, MapExpr::IntLit(a), MapExpr::IntLit(b)) => {
                    Some(MapExpr::IntLit(a.wrapping_add(*b)))
                }
                (MapBinOp::Sub, MapExpr::IntLit(a), MapExpr::IntLit(b)) => {
                    Some(MapExpr::IntLit(a.wrapping_sub(*b)))
                }
                (MapBinOp::Mul, MapExpr::IntLit(a), MapExpr::IntLit(b)) => {
                    Some(MapExpr::IntLit(a.wrapping_mul(*b)))
                }
                (MapBinOp::Div, MapExpr::IntLit(a), MapExpr::IntLit(b)) if *b != 0 => {
                    Some(MapExpr::IntLit(a / b))
                }
                (MapBinOp::Mod, MapExpr::IntLit(a), MapExpr::IntLit(b)) if *b != 0 => {
                    Some(MapExpr::IntLit(a % b))
                }
                (MapBinOp::Add, MapExpr::FloatLit(a), MapExpr::FloatLit(b)) => {
                    Some(MapExpr::FloatLit(a + b))
                }
                (MapBinOp::Sub, MapExpr::FloatLit(a), MapExpr::FloatLit(b)) => {
                    Some(MapExpr::FloatLit(a - b))
                }
                (MapBinOp::Mul, MapExpr::FloatLit(a), MapExpr::FloatLit(b)) => {
                    Some(MapExpr::FloatLit(a * b))
                }
                (MapBinOp::Div, MapExpr::FloatLit(a), MapExpr::FloatLit(b)) if *b != 0.0 => {
                    Some(MapExpr::FloatLit(a / b))
                }
                (MapBinOp::And, MapExpr::BoolLit(a), MapExpr::BoolLit(b)) => {
                    Some(MapExpr::BoolLit(*a && *b))
                }
                (MapBinOp::Or, MapExpr::BoolLit(a), MapExpr::BoolLit(b)) => {
                    Some(MapExpr::BoolLit(*a || *b))
                }
                _ => None,
            },
            MapExpr::UnaryOp { op, inner } => match (op, inner.as_ref()) {
                (MapUnaryOp::Neg, MapExpr::IntLit(a)) => Some(MapExpr::IntLit(-a)),
                (MapUnaryOp::Neg, MapExpr::FloatLit(a)) => Some(MapExpr::FloatLit(-a)),
                (MapUnaryOp::Not, MapExpr::BoolLit(a)) => Some(MapExpr::BoolLit(!a)),
                _ => None,
            },
            _ => None,
        };
        if let Some(result) = folded {
            *self = result;
        }
    }

    /// Returns true if this is a simple constant (no input dependency).
    pub fn is_constant(&self) -> bool {
        match self {
            MapExpr::IntLit(_)
            | MapExpr::FloatLit(_)
            | MapExpr::BoolLit(_)
            | MapExpr::StringLit(_) => true,
            MapExpr::Input | MapExpr::InputProp { .. } => false,
            MapExpr::FnCall { args, .. } => args.iter().all(|a| a.is_constant()),
            MapExpr::BinOp { lhs, rhs, .. } => lhs.is_constant() && rhs.is_constant(),
            MapExpr::UnaryOp { inner, .. } => inner.is_constant(),
        }
    }
}
