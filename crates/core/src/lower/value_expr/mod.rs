//! Value expression lowering: `->` map syntax.
//!
//! Lowers the value expression sub-language (arithmetic, boolean, function
//! calls, closures, literals) from `BbnfView` nodes into `MapExpr`.
//!
//! Tranche AZ-II.cutover.D2 — struct-direct projection. Under the
//! struct-direct BBNF runtime (`crate::runtime::bbnf::BbnfDocument`),
//! every value-expression compound rule emits a typed
//! [`crate::runtime::bbnf::BbnfCompoundKind`] arm and every leaf rule
//! projects directly to its [`crate::runtime::bbnf::BbnfValue`] variant
//! (`Int` / `Float` / `Bool` / `Span`). Compound rules that surface:
//!
//!   - `value_expr` (alt of closure / value_or chain)
//!   - `value_and`, `value_cmp`, `value_add`, `value_mul`
//!   - `value_unary`, `value_atom`
//!   - `value_path`, `value_input`, `value_fn_call`, `value_closure`
//!
//! Leaf rules (`int_lit` / `float_lit` / `bool_lit` / `string_lit` /
//! `value_ident` / `identifier`) project to their typed payload
//! directly — `BbnfValue::Int(i)` / `BbnfValue::Float(f)` /
//! `BbnfValue::Bool(b)` / `BbnfValue::Span(s)` — never carrying their
//! own compound. The dispatch matches on these payload arms first,
//! then on compound-kind arms for structural rules.
//!
//! Operator tokens (`||`, `&&`, `==` ... `*`, `/`, `%`, unary `-`/`!`,
//! closure `|...|` markers, function-call parentheses, etc.) consume
//! source bytes but project no payload. The lowering recovers them
//! by inspecting the source slice between adjacent operand spans (for
//! binary chains) or the leading bytes of a compound's span text (for
//! atoms / closures / unary prefixes).
//!
//! Every kind reachable from the value-expression grammar gets an
//! explicit handler in `dispatch_value_expr`. Unknown kinds panic
//! with a descriptive message; silent `MapExpr::Input` fallbacks are
//! forbidden — they would corrupt every downstream `->` mapping
//! invisibly.

use bbnf_ir::{MapBinOp, MapExpr, MapUnaryOp, NumericDomain, TypeDesc};

use crate::runtime::bbnf::{BbnfCompoundKind, BbnfValue, BbnfView};

use super::LowerCtx;

mod atom;
mod literals;
mod precedence;
mod simple_kinds;
mod unwrap;
mod view_walk;

pub(crate) use unwrap::{
    deep_unwrap_value, extract_value_func_name, is_type_name, unwrap_value_ident_str,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum ValueDomain {
    Unknown,
    Bool,
    Numeric(NumericDomain),
}

/// A map expression paired with the domain proven while walking the source
/// expression. This is intentionally the return type of the real recursive
/// lowering path: parentheses and precedence layers cannot erase a suffix and
/// no second source-inference pass exists.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct LoweredValueExpr {
    pub(crate) expr: MapExpr,
    pub(super) domain: ValueDomain,
    /// At least one numeric leaf carried an explicit suffix. Unsuffixed
    /// arithmetic keeps the historical `return_type: None` surface.
    pub(super) explicit_numeric_domain: bool,
}

impl LoweredValueExpr {
    pub(super) fn unknown(expr: MapExpr) -> Self {
        Self {
            expr,
            domain: ValueDomain::Unknown,
            explicit_numeric_domain: false,
        }
    }

    pub(super) fn bool(expr: MapExpr) -> Self {
        Self {
            expr,
            domain: ValueDomain::Bool,
            explicit_numeric_domain: false,
        }
    }

    pub(super) fn numeric_literal(
        expr: MapExpr,
        lexical_domain: NumericDomain,
        has_explicit_domain: bool,
        expected_domain: Option<NumericDomain>,
        source: &str,
    ) -> Self {
        if has_explicit_domain {
            if let Some(expected) = expected_domain {
                assert_eq!(
                    lexical_domain, expected,
                    "numeric literal {source:?} has domain {lexical_domain:?}, \
                     incompatible with expected {expected:?}"
                );
            }
        }
        let domain = expected_domain.unwrap_or(lexical_domain);
        let mut expr = expr;
        expr.checked_fold(Some(domain)).unwrap_or_else(|error| {
            panic!(
                "numeric literal {source:?} is invalid in {domain:?}: {}",
                error.message()
            )
        });
        Self {
            expr,
            domain: ValueDomain::Numeric(domain),
            explicit_numeric_domain: has_explicit_domain,
        }
    }

    pub(super) fn inferred_return_type(&self) -> Option<TypeDesc> {
        match self.domain {
            ValueDomain::Numeric(domain) if self.explicit_numeric_domain => {
                Some(domain.type_desc())
            }
            ValueDomain::Bool => Some(TypeDesc::Bool),
            _ => None,
        }
    }

    pub(super) fn binary(
        op: MapBinOp,
        lhs: Self,
        rhs: Self,
        expected_numeric_domain: Option<NumericDomain>,
        source: &str,
    ) -> Self {
        let unfolded = |operand_domain| MapExpr::BinOp {
            op,
            operand_domain,
            lhs: Box::new(lhs.expr.clone()),
            rhs: Box::new(rhs.expr.clone()),
        };
        match op {
            MapBinOp::Add
            | MapBinOp::Sub
            | MapBinOp::Mul
            | MapBinOp::Div
            | MapBinOp::Mod
            | MapBinOp::BitAnd
            | MapBinOp::BitOr
            | MapBinOp::Shl
            | MapBinOp::Shr => match (lhs.domain, rhs.domain) {
                (ValueDomain::Numeric(left), ValueDomain::Numeric(right)) => {
                    assert_eq!(
                        left, right,
                        "mixed numeric domains in {source:?}: {left:?} and {right:?}"
                    );
                    let mut expr = unfolded(Some(left));
                    expr.checked_fold(Some(left)).unwrap_or_else(|error| {
                        panic!(
                            "invalid {left:?} numeric expression {source:?}: {}",
                            error.message()
                        )
                    });
                    Self {
                        expr,
                        domain: ValueDomain::Numeric(left),
                        explicit_numeric_domain: lhs.explicit_numeric_domain
                            || rhs.explicit_numeric_domain,
                    }
                }
                (ValueDomain::Unknown, ValueDomain::Unknown) => {
                    if let Some(domain) = expected_numeric_domain {
                        let mut expr = unfolded(Some(domain));
                        expr.checked_fold(Some(domain)).unwrap_or_else(|error| {
                            panic!(
                                "invalid {domain:?} numeric expression {source:?}: {}",
                                error.message()
                            )
                        });
                        Self {
                            expr,
                            domain: ValueDomain::Numeric(domain),
                            explicit_numeric_domain: false,
                        }
                    } else {
                        Self::unknown(unfolded(None))
                    }
                }
                (ValueDomain::Unknown, ValueDomain::Numeric(domain))
                | (ValueDomain::Numeric(domain), ValueDomain::Unknown) => {
                    if expected_numeric_domain.is_some()
                        || lhs.explicit_numeric_domain
                        || rhs.explicit_numeric_domain
                    {
                        let mut expr = unfolded(Some(domain));
                        expr.checked_fold(Some(domain)).unwrap_or_else(|error| {
                            panic!(
                                "invalid {domain:?} numeric expression {source:?}: {}",
                                error.message()
                            )
                        });
                        Self {
                            expr,
                            domain: ValueDomain::Numeric(domain),
                            explicit_numeric_domain: lhs.explicit_numeric_domain
                                || rhs.explicit_numeric_domain,
                        }
                    } else {
                        Self::unknown(unfolded(None))
                    }
                }
                (left, right) => panic!(
                    "numeric operator {op:?} in {source:?} has incompatible \
                     operand domains {left:?} and {right:?}"
                ),
            },
            MapBinOp::Eq
            | MapBinOp::Ne
            | MapBinOp::Lt
            | MapBinOp::Gt
            | MapBinOp::Le
            | MapBinOp::Ge => {
                let expr = match (lhs.domain, rhs.domain) {
                    (ValueDomain::Numeric(left), ValueDomain::Numeric(right)) => {
                        assert_eq!(
                            left, right,
                            "mixed numeric comparison domains in {source:?}: \
                             {left:?} and {right:?}"
                        );
                        let mut expr = unfolded(Some(left));
                        expr.checked_fold(Some(left)).unwrap_or_else(|error| {
                            panic!(
                                "invalid {left:?} comparison {source:?}: {}",
                                error.message()
                            )
                        });
                        expr
                    }
                    (ValueDomain::Bool, ValueDomain::Bool)
                        if matches!(op, MapBinOp::Eq | MapBinOp::Ne) =>
                    {
                        match (&lhs.expr, &rhs.expr) {
                            (MapExpr::BoolLit(left), MapExpr::BoolLit(right)) => {
                                MapExpr::BoolLit(if matches!(op, MapBinOp::Eq) {
                                    left == right
                                } else {
                                    left != right
                                })
                            }
                            _ => unfolded(None),
                        }
                    }
                    (ValueDomain::Numeric(domain), ValueDomain::Unknown)
                    | (ValueDomain::Unknown, ValueDomain::Numeric(domain)) => {
                        if matches!(op, MapBinOp::Eq | MapBinOp::Ne)
                            || expected_numeric_domain.is_some()
                            || lhs.explicit_numeric_domain
                            || rhs.explicit_numeric_domain
                        {
                            let mut expr = unfolded(Some(domain));
                            expr.checked_fold(Some(domain)).unwrap_or_else(|error| {
                                panic!(
                                    "invalid {domain:?} comparison {source:?}: {}",
                                    error.message()
                                )
                            });
                            expr
                        } else {
                            unfolded(None)
                        }
                    }
                    (ValueDomain::Unknown, ValueDomain::Unknown) => {
                        if let Some(domain) = expected_numeric_domain {
                            let mut expr = unfolded(Some(domain));
                            expr.checked_fold(Some(domain)).unwrap_or_else(|error| {
                                panic!(
                                    "invalid {domain:?} comparison {source:?}: {}",
                                    error.message()
                                )
                            });
                            expr
                        } else {
                            unfolded(None)
                        }
                    }
                    (ValueDomain::Bool, ValueDomain::Unknown)
                    | (ValueDomain::Unknown, ValueDomain::Bool)
                        if matches!(op, MapBinOp::Eq | MapBinOp::Ne) =>
                    {
                        unfolded(None)
                    }
                    (left, right) => panic!(
                        "comparison {op:?} in {source:?} has incompatible \
                         operand domains {left:?} and {right:?}"
                    ),
                };
                Self::bool(expr)
            }
            MapBinOp::And | MapBinOp::Or => {
                assert!(
                    matches!(
                        (lhs.domain, rhs.domain),
                        (ValueDomain::Bool, ValueDomain::Bool)
                            | (ValueDomain::Unknown, ValueDomain::Unknown)
                    ),
                    "boolean operator {op:?} in {source:?} has non-boolean operands \
                     {:?} and {:?}",
                    lhs.domain,
                    rhs.domain,
                );
                let expr = match (&lhs.expr, &rhs.expr) {
                    (MapExpr::BoolLit(left), MapExpr::BoolLit(right)) => {
                        MapExpr::BoolLit(if matches!(op, MapBinOp::And) {
                            *left && *right
                        } else {
                            *left || *right
                        })
                    }
                    _ => unfolded(None),
                };
                Self::bool(expr)
            }
        }
    }

    pub(super) fn unary(
        op: MapUnaryOp,
        inner: Self,
        expected_numeric_domain: Option<NumericDomain>,
        source: &str,
    ) -> Self {
        match op {
            MapUnaryOp::Neg => match inner.domain {
                ValueDomain::Numeric(domain) => {
                    let mut expr = MapExpr::UnaryOp {
                        op,
                        operand_domain: Some(domain),
                        inner: Box::new(inner.expr),
                    };
                    expr.checked_fold(Some(domain)).unwrap_or_else(|error| {
                        panic!(
                            "invalid {domain:?} unary expression {source:?}: {}",
                            error.message()
                        )
                    });
                    Self {
                        expr,
                        domain: ValueDomain::Numeric(domain),
                        explicit_numeric_domain: inner.explicit_numeric_domain,
                    }
                }
                ValueDomain::Unknown => match expected_numeric_domain {
                    Some(domain) => Self {
                        expr: MapExpr::UnaryOp {
                            op,
                            operand_domain: Some(domain),
                            inner: Box::new(inner.expr),
                        },
                        domain: ValueDomain::Numeric(domain),
                        explicit_numeric_domain: false,
                    },
                    None => Self::unknown(MapExpr::UnaryOp {
                        op,
                        operand_domain: None,
                        inner: Box::new(inner.expr),
                    }),
                },
                other => panic!("unary negation in {source:?} has non-numeric domain {other:?}"),
            },
            MapUnaryOp::Not => match inner.domain {
                ValueDomain::Bool => match inner.expr {
                    MapExpr::BoolLit(value) => Self::bool(MapExpr::BoolLit(!value)),
                    expr => Self::bool(MapExpr::UnaryOp {
                        op,
                        operand_domain: None,
                        inner: Box::new(expr),
                    }),
                },
                ValueDomain::Unknown => Self::bool(MapExpr::UnaryOp {
                    op,
                    operand_domain: None,
                    inner: Box::new(inner.expr),
                }),
                other => {
                    panic!("logical not in {source:?} has non-boolean domain {other:?}")
                }
            },
        }
    }
}

// ─── Public entry: lower a value expression ─────────────────────────────────

/// Lower a value expression view to a `MapExpr`. Single source of
/// truth for value-expression dispatch — every recursive descent
/// (operator-layer fold, atom unwrap, function-call args, closure
/// body) routes back through here.
pub(crate) fn lower_value_expr<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    source: &'p str,
    expected_numeric_domain: Option<NumericDomain>,
    ctx: &mut LowerCtx<'p>,
) -> LoweredValueExpr {
    dispatch_value_expr(node, Some(source), expected_numeric_domain, ctx)
}

/// Central kind dispatcher. The bbnf value-expression grammar is a
/// closed schema; every reachable kind has an explicit handler.
/// Unknown kinds panic with a descriptive message pointing at the
/// offending node — silent fallbacks are forbidden.
pub(super) fn dispatch_value_expr<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    source_hint: Option<&'p str>,
    expected_numeric_domain: Option<NumericDomain>,
    ctx: &mut LowerCtx<'p>,
) -> LoweredValueExpr {
    let source = node
        .span_text_opt()
        .filter(|source| !source.trim().is_empty())
        .or(source_hint);
    // Leaf-payload dispatch comes first — typed projection delivers
    // the parsed payload directly without a per-leaf compound.
    match node.focus() {
        BbnfValue::Int(v) => return atom::lower_projected_int(v, source, expected_numeric_domain),
        BbnfValue::Float(v) => {
            return atom::lower_projected_float(v, source, expected_numeric_domain);
        }
        BbnfValue::Bool(v) => return LoweredValueExpr::bool(MapExpr::BoolLit(v)),
        BbnfValue::Span(s) => {
            return atom::lower_bare_ident_or_string(s, expected_numeric_domain, ctx);
        }
        BbnfValue::Tag(_) | BbnfValue::Unit => {
            panic!(
                "lower/value_expr/mod.rs: dispatch_value_expr called on \
                 Tag / Unit leaf — value-expression rules never project \
                 to these payload arms"
            );
        }
        BbnfValue::Compound(_) => {}
    }

    // Compound-kind dispatch.
    match node.compound_kind() {
        // Top-level value_expr wrapper (= value_closure | value_or).
        // Under struct-direct projection the alt may surface as a
        // preserved `ValueExpr` compound wrapping the chosen branch,
        // or the optimizer may inline the wrapper down to the chosen
        // branch directly. Inspect the leading byte to discriminate
        // closure from or-chain even when the wrapper is preserved.
        Some(BbnfCompoundKind::ValueExpr) => {
            simple_kinds::lower_value_expr_or_closure(node, source, expected_numeric_domain, ctx)
        }

        // value_or chain — body is `value_and , ( "||" ?w , value_and ) *`.
        // When `value_or` surfaces directly (the optimizer collapsed
        // the outer `value_expr` wrapper), the closure case never
        // applies — closures resolve to a `ValueClosure` compound.
        Some(BbnfCompoundKind::ValueOr) => precedence::fold_value_chain(
            node,
            source,
            &precedence::LAYER_OR,
            expected_numeric_domain,
            ctx,
        ),

        // Precedence-chain layers. Each is `(operand, (op operand)*)`
        // under structural mode the chain layer's compound has shape
        // `[first_operand, (rest_operands)...]` with operator bytes
        // consumed-but-not-pushed between adjacent operand spans.
        Some(BbnfCompoundKind::ValueAnd) => precedence::fold_value_chain(
            node,
            source,
            &precedence::LAYER_AND,
            expected_numeric_domain,
            ctx,
        ),
        Some(BbnfCompoundKind::ValueCmp) => precedence::fold_value_chain(
            node,
            source,
            &precedence::LAYER_CMP,
            expected_numeric_domain,
            ctx,
        ),
        Some(BbnfCompoundKind::ValueAdd) => precedence::fold_value_chain(
            node,
            source,
            &precedence::LAYER_ADD,
            expected_numeric_domain,
            ctx,
        ),
        Some(BbnfCompoundKind::ValueMul) => precedence::fold_value_chain(
            node,
            source,
            &precedence::LAYER_MUL,
            expected_numeric_domain,
            ctx,
        ),

        // Unary prefix layer. Body is `( "!" | "-" )? value_atom`;
        // the prefix consumes bytes without pushing, so we
        // disambiguate via leading-byte inspection of the unary
        // span text.
        Some(BbnfCompoundKind::ValueUnary) => {
            atom::lower_value_unary(node, source, expected_numeric_domain, ctx)
        }

        // Atom layer. Inlined alt over int/float/bool/string lits,
        // input chains, paths, function calls, and parenthesised
        // sub-expressions. Discriminated by leading-byte inspection
        // of the atom span text.
        Some(BbnfCompoundKind::ValueAtom) => {
            atom::lower_value_atom(node, source, expected_numeric_domain, ctx)
        }

        // Standalone leaf-shaped compound rules — these surface when
        // the optimizer preserves the wrapper rather than inlining.
        Some(BbnfCompoundKind::ValueInput) => simple_kinds::lower_value_input(node, source, ctx),
        Some(BbnfCompoundKind::ValuePath) => simple_kinds::lower_value_path(node, source, ctx),
        Some(BbnfCompoundKind::ValueFnCall) => {
            simple_kinds::lower_value_fn_call(node, source, expected_numeric_domain, ctx)
        }
        Some(BbnfCompoundKind::ValueClosure) => {
            simple_kinds::lower_value_closure(node, source, expected_numeric_domain, ctx)
        }

        // Other compound kinds reachable from the value-expression
        // grammar would only appear if an outer wrapper rule (e.g.
        // a `binary_factor` that wraps a `value_expr`) leaks into
        // the value-expression dispatch. The defensive route is to
        // descend into the first child and re-dispatch.
        Some(BbnfCompoundKind::Other) => {
            // The optimizer fully inlined the value layer wrapper
            // and what reached us is an anonymous compound carrying
            // the atom's span. Route through the atom path.
            atom::lower_value_atom(node, source, expected_numeric_domain, ctx)
        }
        Some(other) => panic!(
            "lower/value_expr/mod.rs: dispatch_value_expr called on \
             unhandled compound kind {:?} (focus = {:?}, \
             span_text = {:?}). Add an explicit handler for this kind.",
            other,
            node.focus(),
            node.span_text(),
        ),
        None => {
            // Cannot reach: leaf payload handled above. If we ever
            // do, the panic surfaces a typed-AST invariant break.
            panic!(
                "lower/value_expr/mod.rs: dispatch_value_expr saw a leaf \
                 payload {:?} after the leaf-dispatch fall-through — \
                 the typed-AST invariant is broken",
                node.focus()
            );
        }
    }
}
