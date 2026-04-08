//! Value expression lowering: `->` map syntax.
//!
//! Lowers the value expression sub-language (arithmetic, boolean, function
//! calls, closures, literals) from `BbnfBootstrapEnum` nodes into `MapExpr`.
//!
//! Operator chains (`value_or`/`and`/`cmp`/`add`/`mul`) all share the same
//! left-associative fold shape; the precedence + symbol → `MapBinOp` mapping
//! lives in the `PRECEDENCE` table and is consumed by the single
//! `fold_precedence_layer` helper. There is no per-layer hand-written fold.

use std::collections::HashMap;

use bbnf_ir::{MapBinOp, MapExpr, MapUnaryOp};
use parse_that::Span;

use crate::grammar::generated::BbnfBootstrapEnum;

use super::LowerCtx;

// ─── Precedence table (declarative) ───────────────────────────────────────────

/// One precedence layer: a set of operator symbols mapping to `MapBinOp`.
struct PrecedenceLayer {
    ops: &'static [(&'static str, MapBinOp)],
}

/// All binary operator layers in order from lowest to highest precedence.
/// Indices match the rule names: `value_or`/`and`/`cmp`/`add`/`mul`.
const PRECEDENCE: &[PrecedenceLayer] = &[
    // value_or
    PrecedenceLayer {
        ops: &[("||", MapBinOp::Or)],
    },
    // value_and
    PrecedenceLayer {
        ops: &[("&&", MapBinOp::And)],
    },
    // value_cmp
    PrecedenceLayer {
        ops: &[
            ("==", MapBinOp::Eq),
            ("!=", MapBinOp::Ne),
            ("<", MapBinOp::Lt),
            (">", MapBinOp::Gt),
            ("<=", MapBinOp::Le),
            (">=", MapBinOp::Ge),
        ],
    },
    // value_add
    PrecedenceLayer {
        ops: &[("+", MapBinOp::Add), ("-", MapBinOp::Sub)],
    },
    // value_mul
    PrecedenceLayer {
        ops: &[
            ("*", MapBinOp::Mul),
            ("/", MapBinOp::Div),
            ("%", MapBinOp::Mod),
        ],
    },
];

const LAYER_OR: &PrecedenceLayer = &PRECEDENCE[0];
const LAYER_AND: &PrecedenceLayer = &PRECEDENCE[1];
const LAYER_CMP: &PrecedenceLayer = &PRECEDENCE[2];
const LAYER_ADD: &PrecedenceLayer = &PRECEDENCE[3];
const LAYER_MUL: &PrecedenceLayer = &PRECEDENCE[4];

/// Single generic left-associative fold over a binary operator chain.
///
/// `op_text` extracts the operator's textual form from each `(op_repr, operand)`
/// pair (it varies between `Span` and `&Enum` shapes across rules). The text
/// is looked up in `layer.ops` to recover the canonical `MapBinOp`.
fn fold_precedence_layer<'a, OpT, I, F>(
    first: &'a BbnfBootstrapEnum<'a>,
    rest: I,
    op_text: F,
    layer: &PrecedenceLayer,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr
where
    I: IntoIterator<Item = (OpT, &'a BbnfBootstrapEnum<'a>)>,
    F: Fn(&OpT) -> &str,
{
    let mut result = lower_value_expr(first, ctx);
    for (op_repr, operand) in rest {
        let text = op_text(&op_repr);
        let op = layer
            .ops
            .iter()
            .find(|(t, _)| *t == text)
            .map(|(_, o)| *o)
            .unwrap_or_else(|| layer.ops[0].1);
        result = MapExpr::BinOp {
            op,
            lhs: Box::new(result),
            rhs: Box::new(lower_value_expr(operand, ctx)),
        };
    }
    result
}

// ─── ValueExpr lowering ────────────────────────────────────────────────────────

/// Lower a value expression bootstrap node to a `MapExpr`.
pub(crate) fn lower_value_expr<'a>(
    node: &'a BbnfBootstrapEnum<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    match node {
        // Precedence chain: or → and → cmp → add → mul → unary → atom
        BbnfBootstrapEnum::value_or((first, rest)) => fold_precedence_layer(
            first,
            rest.iter().map(|(s, e)| (*s, *e)),
            |s: &Span<'a>| s.as_str(),
            LAYER_OR,
            ctx,
        ),
        BbnfBootstrapEnum::value_and((first, rest)) => fold_precedence_layer(
            first,
            rest.iter().map(|(s, e)| (*s, *e)),
            |s: &Span<'a>| s.as_str(),
            LAYER_AND,
            ctx,
        ),
        BbnfBootstrapEnum::value_cmp((first, rest)) => fold_precedence_layer(
            first,
            rest.iter().map(|(op, e)| (*op, *e)),
            |op: &&BbnfBootstrapEnum<'a>| op_node_text(op),
            LAYER_CMP,
            ctx,
        ),
        BbnfBootstrapEnum::value_add((first, rest)) => fold_precedence_layer(
            first,
            rest.iter().map(|(op, e)| (*op, *e)),
            |op: &&BbnfBootstrapEnum<'a>| op_node_text(op),
            LAYER_ADD,
            ctx,
        ),
        BbnfBootstrapEnum::value_mul((first, rest)) => fold_precedence_layer(
            first,
            rest.iter().map(|(op, e)| (*op, *e)),
            |op: &&BbnfBootstrapEnum<'a>| op_node_text(op),
            LAYER_MUL,
            ctx,
        ),

        // Unary
        BbnfBootstrapEnum::value_unary(inner) => lower_value_expr(inner, ctx),
        BbnfBootstrapEnum::value_unary_0((op_span, inner)) => {
            let op = match op_span.as_str() {
                "-" => MapUnaryOp::Neg,
                "!" => MapUnaryOp::Not,
                _ => MapUnaryOp::Neg,
            };
            MapExpr::UnaryOp {
                op,
                inner: Box::new(lower_value_expr(inner, ctx)),
            }
        }

        // Atom
        BbnfBootstrapEnum::value_atom(inner) => lower_value_expr(inner, ctx),
        BbnfBootstrapEnum::value_atom_0((_open, inner, _close)) => lower_value_expr(inner, ctx),

        // Literals
        BbnfBootstrapEnum::int_lit(s) => parse_int_literal(s.as_str()),
        BbnfBootstrapEnum::float_lit(s) => parse_float_literal(s.as_str()),
        BbnfBootstrapEnum::bool_lit(s) => MapExpr::BoolLit(s.as_str() == "true"),
        BbnfBootstrapEnum::string_lit(s) => {
            let raw = s.as_str();
            let inner = &raw[1..raw.len() - 1]; // Strip quotes.
            let sid = ctx.strings.intern(inner);
            MapExpr::StringLit(sid)
        }

        // Input
        BbnfBootstrapEnum::value_input((_, props)) => {
            if props.is_empty() {
                MapExpr::Input
            } else {
                // Take the last property access.
                let (_dot, prop_node) = &props[props.len() - 1];
                let prop_name = match prop_node {
                    BbnfBootstrapEnum::value_ident(s) => s.as_str(),
                    BbnfBootstrapEnum::identifier(s) => s.as_str(),
                    _ => "unknown",
                };
                let sid = ctx.strings.intern(prop_name);
                MapExpr::InputProp { prop: sid }
            }
        }

        // Function call
        BbnfBootstrapEnum::value_fn_call((name_enum, _open, args_opt, _close)) => {
            let name_str = crate::grammar::host::extract_path_text(name_enum);
            let sid = ctx.strings.intern(&name_str);
            let ir_args: Vec<MapExpr> = match args_opt {
                Some((first_arg, rest_args)) => {
                    let mut args = vec![lower_value_expr(first_arg, ctx)];
                    for (_comma, arg) in *rest_args {
                        args.push(lower_value_expr(arg, ctx));
                    }
                    args
                }
                None => vec![],
            };
            MapExpr::FnCall {
                name: sid,
                args: ir_args,
            }
        }

        // Path (e.g., crate::module::func — treat as bare function reference)
        BbnfBootstrapEnum::value_path((first, rest)) => {
            if rest.is_empty() {
                lower_value_expr(first, ctx)
            } else {
                let path = crate::grammar::host::extract_path_text(node);
                let sid = ctx.strings.intern(&path);
                MapExpr::FnCall {
                    name: sid,
                    args: vec![MapExpr::Input],
                }
            }
        }

        // Identifier (bare name — treat as function call on input)
        BbnfBootstrapEnum::value_ident(s) => {
            let sid = ctx.strings.intern(s.as_str());
            MapExpr::FnCall {
                name: sid,
                args: vec![MapExpr::Input],
            }
        }

        // Value closure: |params| body
        BbnfBootstrapEnum::value_closure((_pipe, first_param, rest_params, _pipe2, body)) => {
            lower_value_expr_with_bindings(body, first_param, rest_params, ctx)
        }

        // Fallback
        _ => MapExpr::Input,
    }
}

// ─── Op-node text extraction ─────────────────────────────────────────────────

/// Extract the textual operator from a `cmp_op`/`add_op`/`mul_op` enum node.
/// Used by `fold_precedence_layer` to look up the canonical `MapBinOp`.
fn op_node_text<'a>(node: &BbnfBootstrapEnum<'a>) -> &'a str {
    match node {
        BbnfBootstrapEnum::cmp_op(s)
        | BbnfBootstrapEnum::add_op(s)
        | BbnfBootstrapEnum::mul_op(s) => s.as_str(),
        _ => "",
    }
}

// ─── Value closure bindings ───────────────────────────────────────────────────

/// Lower a value closure body with param→Input bindings.
///
/// `first_param` is the first closure parameter (mapped to `MapExpr::Input`).
/// `rest_params` are the remaining `(comma, param)` pairs (mapped to `InputProp`).
pub(crate) fn lower_value_expr_with_bindings<'a>(
    body: &'a BbnfBootstrapEnum<'a>,
    first_param: &'a BbnfBootstrapEnum<'a>,
    rest_params: &'a [(Span<'a>, &'a BbnfBootstrapEnum<'a>)],
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    let mut bindings: HashMap<&str, MapExpr> = HashMap::new();

    let first_name = match first_param {
        BbnfBootstrapEnum::value_ident(s) | BbnfBootstrapEnum::identifier(s) => s.as_str(),
        _ => "",
    };
    bindings.insert(first_name, MapExpr::Input);

    for (_comma, param_node) in rest_params {
        let name = match param_node {
            BbnfBootstrapEnum::value_ident(s) | BbnfBootstrapEnum::identifier(s) => s.as_str(),
            _ => "",
        };
        let sid = ctx.strings.intern(name);
        bindings.insert(name, MapExpr::InputProp { prop: sid });
    }

    lower_value_expr_substituted(body, &bindings, ctx)
}

pub(crate) fn lower_value_expr_substituted<'a>(
    node: &'a BbnfBootstrapEnum<'a>,
    bindings: &HashMap<&str, MapExpr>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    match node {
        BbnfBootstrapEnum::value_ident(s) => {
            let name = s.as_str();
            if let Some(bound) = bindings.get(name) {
                return bound.clone();
            }
            let sid = ctx.strings.intern(name);
            MapExpr::FnCall {
                name: sid,
                args: vec![MapExpr::Input],
            }
        }
        BbnfBootstrapEnum::value_path((first, rest)) => {
            if rest.is_empty() {
                lower_value_expr_substituted(first, bindings, ctx)
            } else {
                let path = crate::grammar::host::extract_path_text(node);
                let sid = ctx.strings.intern(&path);
                MapExpr::FnCall {
                    name: sid,
                    args: vec![MapExpr::Input],
                }
            }
        }
        BbnfBootstrapEnum::value_fn_call((name_enum, _open, args_opt, _close)) => {
            let name_str = crate::grammar::host::extract_path_text(name_enum);
            let sid = ctx.strings.intern(&name_str);
            let ir_args: Vec<MapExpr> = match args_opt {
                Some((first_arg, rest_args)) => {
                    let mut args = vec![lower_value_expr_substituted(first_arg, bindings, ctx)];
                    for (_comma, arg) in *rest_args {
                        args.push(lower_value_expr_substituted(arg, bindings, ctx));
                    }
                    args
                }
                None => vec![],
            };
            MapExpr::FnCall {
                name: sid,
                args: ir_args,
            }
        }
        BbnfBootstrapEnum::value_atom(inner) | BbnfBootstrapEnum::value_unary(inner) => {
            lower_value_expr_substituted(inner, bindings, ctx)
        }
        BbnfBootstrapEnum::value_atom_0((_open, inner, _close)) => {
            lower_value_expr_substituted(inner, bindings, ctx)
        }
        BbnfBootstrapEnum::value_closure((_pipe, first_param, rest_params, _pipe2, body)) => {
            lower_value_expr_with_bindings(body, first_param, rest_params, ctx)
        }
        // Literals and Input don't reference params — delegate to standard lowering.
        _ => lower_value_expr(node, ctx),
    }
}

// ─── Value expression helpers ─────────────────────────────────────────────────

/// Extract a `&str` from a `value_ident` or a single-segment `value_path`,
/// recursively unwrapping value expression precedence wrappers.
pub(crate) fn unwrap_value_ident_str<'a>(node: &'a BbnfBootstrapEnum<'a>) -> Option<&'a str> {
    match node {
        BbnfBootstrapEnum::value_ident(s) => Some(s.as_str()),
        BbnfBootstrapEnum::value_path((first, rest)) if rest.is_empty() => {
            unwrap_value_ident_str(first)
        }
        // Unwrap value expression precedence chain.
        BbnfBootstrapEnum::value_or((first, rest)) if rest.is_empty() => {
            unwrap_value_ident_str(first)
        }
        BbnfBootstrapEnum::value_and((first, rest)) if rest.is_empty() => {
            unwrap_value_ident_str(first)
        }
        BbnfBootstrapEnum::value_cmp((first, rest)) if rest.is_empty() => {
            unwrap_value_ident_str(first)
        }
        BbnfBootstrapEnum::value_add((first, rest)) if rest.is_empty() => {
            unwrap_value_ident_str(first)
        }
        BbnfBootstrapEnum::value_mul((first, rest)) if rest.is_empty() => {
            unwrap_value_ident_str(first)
        }
        BbnfBootstrapEnum::value_unary(inner)
        | BbnfBootstrapEnum::value_atom(inner) => unwrap_value_ident_str(inner),
        _ => None,
    }
}

/// Unwrap transparent value wrappers to reach the inner value node.
/// Peels one layer of value expression wrapper.
fn unwrap_value_atom<'a>(node: &'a BbnfBootstrapEnum<'a>) -> Option<&'a BbnfBootstrapEnum<'a>> {
    match node {
        BbnfBootstrapEnum::value_atom(inner)
        | BbnfBootstrapEnum::value_unary(inner) => Some(inner),
        BbnfBootstrapEnum::value_or((first, rest)) if rest.is_empty() => Some(first),
        BbnfBootstrapEnum::value_and((first, rest)) if rest.is_empty() => Some(first),
        BbnfBootstrapEnum::value_cmp((first, rest)) if rest.is_empty() => Some(first),
        BbnfBootstrapEnum::value_add((first, rest)) if rest.is_empty() => Some(first),
        BbnfBootstrapEnum::value_mul((first, rest)) if rest.is_empty() => Some(first),
        BbnfBootstrapEnum::value_atom_0((_open, inner, _close)) => Some(inner),
        BbnfBootstrapEnum::value_path((first, rest)) if rest.is_empty() => Some(first),
        _ => None,
    }
}

/// Recursively unwrap the full value expression chain to reach a leaf node.
pub(crate) fn deep_unwrap_value<'a>(node: &'a BbnfBootstrapEnum<'a>) -> &'a BbnfBootstrapEnum<'a> {
    match unwrap_value_atom(node) {
        Some(inner) => deep_unwrap_value(inner),
        None => node,
    }
}

pub(crate) fn extract_value_func_name(node: &BbnfBootstrapEnum<'_>) -> Option<String> {
    match node {
        BbnfBootstrapEnum::value_ident(s) => Some(s.as_str().to_string()),
        BbnfBootstrapEnum::value_path(_) => Some(crate::grammar::host::extract_path_text(node)),
        BbnfBootstrapEnum::value_fn_call((name, _, _, _)) => {
            Some(crate::grammar::host::extract_path_text(name))
        }
        _ => None,
    }
}

pub(crate) fn is_type_name(name: &str) -> bool {
    matches!(
        name,
        "f64" | "f32" | "u32" | "u64" | "i32" | "i64" | "usize" | "u8" | "u16" | "i8" | "i16"
    )
}

// ─── Numeric parsing helpers ───────────────────────────────────────────────────

pub(crate) fn parse_int_literal(text: &str) -> MapExpr {
    let (digits, _suffix) = split_numeric_suffix(text);
    let value = if digits.starts_with("0x") || digits.starts_with("0X") {
        i64::from_str_radix(&digits[2..], 16).unwrap_or(0)
    } else {
        digits.parse::<i64>().unwrap_or(0)
    };
    MapExpr::IntLit(value)
}

pub(crate) fn parse_float_literal(text: &str) -> MapExpr {
    let (digits, _suffix) = split_numeric_suffix(text);
    let value = digits.parse::<f64>().unwrap_or(0.0);
    MapExpr::FloatLit(value)
}

pub(crate) fn split_numeric_suffix(text: &str) -> (&str, &str) {
    let bytes = text.as_bytes();
    let mut i = 0;
    if bytes.len() > 2 && bytes[0] == b'0' && (bytes[1] == b'x' || bytes[1] == b'X') {
        i = 2;
        while i < bytes.len() && bytes[i].is_ascii_hexdigit() {
            i += 1;
        }
    } else {
        while i < bytes.len()
            && (bytes[i].is_ascii_digit()
                || bytes[i] == b'.'
                || bytes[i] == b'e'
                || bytes[i] == b'E'
                || bytes[i] == b'+'
                || bytes[i] == b'-')
        {
            i += 1;
        }
    }
    (&text[..i], &text[i..])
}
