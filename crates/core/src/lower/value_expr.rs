//! Value expression lowering: `->` map syntax.
//!
//! Lowers the value expression sub-language (arithmetic, boolean, function
//! calls, closures, literals) from `BbnfBootstrapNodeView` nodes into
//! `MapExpr`.
//!
//! Operator chains (`value_or`/`and`/`cmp`/`add`/`mul`) all share the same
//! left-associative fold shape; the precedence + symbol → `MapBinOp` mapping
//! lives in the `PRECEDENCE` table and is consumed by the single
//! `fold_precedence_layer` helper. There is no per-layer hand-written fold.
//!
//! Tranche AC.2: every `BbnfBootstrapEnum` pattern-match is now a
//! `rule_kind()` dispatch + typed child accessors on the view.

use std::collections::HashMap;

use bbnf_ir::{MapBinOp, MapExpr, MapUnaryOp};

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};

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

/// Generic left-associative fold over a precedence layer of a
/// `value_or`/`and`/`cmp`/`add`/`mul` view.
///
/// Each layer rule has the shape `(first, (op, operand)*)` where
/// `op` is either a bare punctuation leaf (|| / &&) or a typed
/// `cmp_op`/`add_op`/`mul_op` wrapper whose `.span_text()` gives the
/// textual operator. The wrapper distinction is irrelevant to the
/// fold: both cases resolve via `operator_node.span_text()`.
fn fold_precedence_layer<'a>(
    node: BbnfBootstrapNodeView<'a>,
    layer: &PrecedenceLayer,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    let first = node
        .child(0)
        .expect("value precedence layer: missing first operand");
    let rest_list = node.child(1);
    let mut result = lower_value_expr(first, ctx);
    if let Some(rest) = rest_list {
        for pair in rest.children() {
            // pair = (op_leaf, operand)
            let op_node = match pair.child(0) {
                Some(o) => o,
                None => continue,
            };
            let operand = match pair.child(1) {
                Some(o) => o,
                None => continue,
            };
            let text = op_node.span_text();
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
    }
    result
}

// ─── ValueExpr lowering ────────────────────────────────────────────────────────

/// Lower a value expression view to a `MapExpr`.
pub(crate) fn lower_value_expr<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    match node.rule_kind() {
        // Precedence chain: or → and → cmp → add → mul → unary → atom
        BbnfBootstrapRuleKind::value_or => fold_precedence_layer(node, LAYER_OR, ctx),
        BbnfBootstrapRuleKind::value_and => fold_precedence_layer(node, LAYER_AND, ctx),
        BbnfBootstrapRuleKind::value_cmp => fold_precedence_layer(node, LAYER_CMP, ctx),
        BbnfBootstrapRuleKind::value_add => fold_precedence_layer(node, LAYER_ADD, ctx),
        BbnfBootstrapRuleKind::value_mul => fold_precedence_layer(node, LAYER_MUL, ctx),

        // Unary
        BbnfBootstrapRuleKind::value_unary => {
            let inner = node.child(0).expect("value_unary: missing inner");
            lower_value_expr(inner, ctx)
        }
        BbnfBootstrapRuleKind::value_unary_0 => {
            // value_unary_0 = (op_span, inner)
            let op_span = node.child(0).expect("value_unary_0: missing op");
            let inner = node.child(1).expect("value_unary_0: missing inner");
            let op = match op_span.span_text() {
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
        BbnfBootstrapRuleKind::value_atom => {
            let inner = node.child(0).expect("value_atom: missing inner");
            lower_value_expr(inner, ctx)
        }
        BbnfBootstrapRuleKind::value_atom_0 => {
            // value_atom_0 = ("(", inner, ")")
            let inner = node.child(1).expect("value_atom_0: missing inner");
            lower_value_expr(inner, ctx)
        }

        // Literals
        BbnfBootstrapRuleKind::int_lit => parse_int_literal(node.span_text()),
        BbnfBootstrapRuleKind::float_lit => parse_float_literal(node.span_text()),
        BbnfBootstrapRuleKind::bool_lit => MapExpr::BoolLit(node.span_text() == "true"),
        BbnfBootstrapRuleKind::string_lit => {
            let raw = node.span_text();
            let inner = &raw[1..raw.len() - 1]; // Strip quotes.
            let sid = ctx.strings.intern(inner);
            MapExpr::StringLit(sid)
        }

        // Input
        BbnfBootstrapRuleKind::value_input => {
            // value_input = (input_kw, (dot, prop)*) — the first
            // child is the `input` keyword leaf, the second is the
            // property chain (possibly absent / empty).
            let props = node.child(1);
            let last_prop = props.and_then(|p| p.children().last());
            match last_prop {
                Some(pair) => {
                    let prop_node = pair.child(1).unwrap_or(pair);
                    let prop_name = match prop_node.rule_kind() {
                        BbnfBootstrapRuleKind::value_ident
                        | BbnfBootstrapRuleKind::identifier => prop_node.span_text(),
                        _ => "unknown",
                    };
                    let sid = ctx.strings.intern(prop_name);
                    MapExpr::InputProp { prop: sid }
                }
                None => MapExpr::Input,
            }
        }

        // Function call
        BbnfBootstrapRuleKind::value_fn_call => {
            // value_fn_call = (name, "(", args_opt, ")")
            let name_node = node.child(0).expect("value_fn_call: missing name");
            let args_opt = node.child(2);
            let name_str = join_value_path(name_node);
            let sid = ctx.strings.intern(&name_str);
            let ir_args: Vec<MapExpr> = match args_opt {
                Some(args_group) if args_group.span().1 > args_group.span().0 => {
                    // args_group = (first_arg, (",", arg)*)
                    let first_arg = args_group
                        .child(0)
                        .expect("value_fn_call args: missing first arg");
                    let rest = args_group.child(1);
                    let mut args = vec![lower_value_expr(first_arg, ctx)];
                    if let Some(rest_list) = rest {
                        for pair in rest_list.children() {
                            if let Some(arg) = pair.child(1) {
                                args.push(lower_value_expr(arg, ctx));
                            }
                        }
                    }
                    args
                }
                _ => vec![],
            };
            MapExpr::FnCall {
                name: sid,
                args: ir_args,
            }
        }

        // Path (e.g., crate::module::func — treat as bare function reference)
        BbnfBootstrapRuleKind::value_path => {
            // value_path = (first, ("::", segment)*) — unwrap
            // single-segment paths transparently.
            let first = node.child(0).expect("value_path: missing first segment");
            let rest = node.child(1);
            let rest_empty = rest.map(|r| r.children().next().is_none()).unwrap_or(true);
            if rest_empty {
                lower_value_expr(first, ctx)
            } else {
                let path = join_value_path(node);
                // Single-segment paths that match a value-closure
                // binding resolve to the bound MapExpr instead of a
                // function call.
                if let Some(bound) = lookup_value_env(&path, &ctx.value_env) {
                    return bound;
                }
                let sid = ctx.strings.intern(&path);
                MapExpr::FnCall {
                    name: sid,
                    args: vec![MapExpr::Input],
                }
            }
        }

        // Identifier (bare name — treat as function call on input, unless
        // shadowed by a value-closure parameter binding).
        BbnfBootstrapRuleKind::value_ident => {
            let name = node.span_text();
            if let Some(bound) = lookup_value_env(name, &ctx.value_env) {
                return bound;
            }
            let sid = ctx.strings.intern(name);
            MapExpr::FnCall {
                name: sid,
                args: vec![MapExpr::Input],
            }
        }

        // Value closure: |params| body
        BbnfBootstrapRuleKind::value_closure => {
            // value_closure = "|", first_param, rest_params, "|", body
            let first_param = node
                .child(1)
                .expect("value_closure: missing first param");
            let rest_params = node.child(2);
            let body = node.child(4).expect("value_closure: missing body");
            lower_value_expr_with_bindings(body, first_param, rest_params, ctx)
        }

        // Fallback
        _ => MapExpr::Input,
    }
}

// ─── Path extraction ─────────────────────────────────────────────────────────

/// Join a `value_path` view's segments with `::`. Returns the leaf
/// text for non-path views. Used by value-expression lowering to
/// recover fully-qualified function paths (e.g. `crate::module::func`).
fn join_value_path<'a>(node: BbnfBootstrapNodeView<'a>) -> String {
    if node.rule_kind() == BbnfBootstrapRuleKind::value_path {
        let first = match node.child(0) {
            Some(f) => f,
            None => return String::new(),
        };
        let first_str = first.span_text();
        let rest = node.child(1);
        let rest_empty = rest.map(|r| r.children().next().is_none()).unwrap_or(true);
        if rest_empty {
            first_str.to_string()
        } else {
            let rest = rest.unwrap();
            let rest_len = rest.children().count();
            let mut path = String::with_capacity(first_str.len() + rest_len * 10);
            path.push_str(first_str);
            for pair in rest.children() {
                // pair = ("::", segment)
                if let Some(segment) = pair.child(1) {
                    path.push_str("::");
                    path.push_str(segment.span_text());
                }
            }
            path
        }
    } else {
        node.span_text().to_string()
    }
}

// ─── Value closure bindings ───────────────────────────────────────────────────

/// Lower a value closure body with param→Input bindings via env push/pop.
///
/// `first_param` is the first closure parameter (mapped to `MapExpr::Input`).
/// `rest_params` is the optional rest list: `(",", param)*`.
///
/// Pushes a frame onto `ctx.value_env` before lowering the body, pops
/// afterwards. `lower_value_expr` consults the stack at `value_ident` /
/// single-segment `value_path` sites — no parallel substitution walker.
pub(crate) fn lower_value_expr_with_bindings<'a>(
    body: BbnfBootstrapNodeView<'a>,
    first_param: BbnfBootstrapNodeView<'a>,
    rest_params: Option<BbnfBootstrapNodeView<'a>>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    let mut frame: HashMap<&'a str, MapExpr> = HashMap::new();

    let first_name = match first_param.rule_kind() {
        BbnfBootstrapRuleKind::value_ident | BbnfBootstrapRuleKind::identifier => {
            first_param.span_text()
        }
        _ => "",
    };
    frame.insert(first_name, MapExpr::Input);

    if let Some(rest) = rest_params {
        for pair in rest.children() {
            // pair = (",", param)
            let param_node = match pair.child(1) {
                Some(p) => p,
                None => continue,
            };
            let name = match param_node.rule_kind() {
                BbnfBootstrapRuleKind::value_ident | BbnfBootstrapRuleKind::identifier => {
                    param_node.span_text()
                }
                _ => "",
            };
            let sid = ctx.strings.intern(name);
            frame.insert(name, MapExpr::InputProp { prop: sid });
        }
    }

    ctx.value_env.push(frame);
    let result = lower_value_expr(body, ctx);
    ctx.value_env.pop();
    result
}

/// Look up a name in the value-environment stack (top frame first, mirroring
/// lexical scope). Returns a clone of the bound `MapExpr` if found.
fn lookup_value_env(name: &str, env: &[HashMap<&str, MapExpr>]) -> Option<MapExpr> {
    for frame in env.iter().rev() {
        if let Some(bound) = frame.get(name) {
            return Some(bound.clone());
        }
    }
    None
}

// ─── Value expression helpers ─────────────────────────────────────────────────

/// Extract a `&str` from a `value_ident` or a single-segment `value_path`,
/// recursively unwrapping value expression precedence wrappers.
pub(crate) fn unwrap_value_ident_str<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<&'a str> {
    match node.rule_kind() {
        BbnfBootstrapRuleKind::value_ident => Some(node.span_text()),
        BbnfBootstrapRuleKind::value_path => {
            let first = node.child(0)?;
            let rest = node.child(1);
            let rest_empty = rest.map(|r| r.children().next().is_none()).unwrap_or(true);
            if rest_empty {
                unwrap_value_ident_str(first)
            } else {
                None
            }
        }
        // Unwrap precedence chain layers.
        BbnfBootstrapRuleKind::value_or
        | BbnfBootstrapRuleKind::value_and
        | BbnfBootstrapRuleKind::value_cmp
        | BbnfBootstrapRuleKind::value_add
        | BbnfBootstrapRuleKind::value_mul => {
            let first = node.child(0)?;
            let rest = node.child(1);
            let rest_empty = rest.map(|r| r.children().next().is_none()).unwrap_or(true);
            if rest_empty {
                unwrap_value_ident_str(first)
            } else {
                None
            }
        }
        BbnfBootstrapRuleKind::value_unary | BbnfBootstrapRuleKind::value_atom => {
            let inner = node.child(0)?;
            unwrap_value_ident_str(inner)
        }
        _ => None,
    }
}

/// Unwrap transparent value wrappers to reach the inner value node.
/// Peels one layer of value expression wrapper.
fn unwrap_value_atom<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<BbnfBootstrapNodeView<'a>> {
    match node.rule_kind() {
        BbnfBootstrapRuleKind::value_atom | BbnfBootstrapRuleKind::value_unary => node.child(0),
        BbnfBootstrapRuleKind::value_or
        | BbnfBootstrapRuleKind::value_and
        | BbnfBootstrapRuleKind::value_cmp
        | BbnfBootstrapRuleKind::value_add
        | BbnfBootstrapRuleKind::value_mul => {
            let first = node.child(0)?;
            let rest = node.child(1);
            let rest_empty = rest.map(|r| r.children().next().is_none()).unwrap_or(true);
            if rest_empty { Some(first) } else { None }
        }
        BbnfBootstrapRuleKind::value_atom_0 => node.child(1),
        BbnfBootstrapRuleKind::value_path => {
            let first = node.child(0)?;
            let rest = node.child(1);
            let rest_empty = rest.map(|r| r.children().next().is_none()).unwrap_or(true);
            if rest_empty { Some(first) } else { None }
        }
        _ => None,
    }
}

/// Recursively unwrap the full value expression chain to reach a leaf node.
pub(crate) fn deep_unwrap_value<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> BbnfBootstrapNodeView<'a> {
    match unwrap_value_atom(node) {
        Some(inner) => deep_unwrap_value(inner),
        None => node,
    }
}

pub(crate) fn extract_value_func_name<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<String> {
    match node.rule_kind() {
        BbnfBootstrapRuleKind::value_ident => Some(node.span_text().to_string()),
        BbnfBootstrapRuleKind::value_path => Some(join_value_path(node)),
        BbnfBootstrapRuleKind::value_fn_call => {
            let name_node = node.child(0)?;
            Some(join_value_path(name_node))
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
