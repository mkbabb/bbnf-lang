//! Value expression lowering: `->` map syntax.
//!
//! Lowers the value expression sub-language (arithmetic, boolean, function
//! calls, closures, literals) from `BbnfBootstrapNodeView` nodes into
//! `MapExpr`.
//!
//! Tranche AE.1 — shape-agnostic walking. Under structural-mode tape
//! emission, the value-expression sub-grammar collapses far more
//! aggressively than the outer expression layers: most leaf rules
//! (`int_lit` / `float_lit` / `bool_lit` / `string_lit` / `value_ident` /
//! `value_path` / `value_input` / `value_fn_call` / `value_closure` /
//! `mul_op` / `add_op` / `cmp_op`) are inlined into their callers and
//! never produce a tape compound of their own. The rule_kinds that
//! actually surface on the tape are:
//!
//!   - `value_expr` (alt of closure / value_or chain)
//!   - `value_and`, `value_cmp`, `value_add`, `value_mul`
//!   - `value_unary`, `value_atom`
//!
//! Operator tokens (`||`, `&&`, `==` ... `*`, `/`, `%`, unary `-`/`!`,
//! closure `|...|` markers, function-call parentheses, etc.) consume
//! source bytes but push **nothing**. The lowering recovers them by
//! inspecting the source slice between adjacent operand spans (for
//! binary chains) or the leading bytes of a compound's span text (for
//! atoms / closures / unary prefixes).
//!
//! Every rule_kind reachable from the value-expression grammar gets
//! an explicit handler in `dispatch_value_expr`. Unknown rule_kinds
//! panic with a descriptive message; silent `MapExpr::Input`
//! fallbacks are forbidden — they would corrupt every downstream
//! `->` mapping invisibly.

use std::collections::HashMap;

use bbnf_ir::{MapBinOp, MapExpr, MapUnaryOp};

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};
use crate::lower::tape_walk::find_descendant_by_kind;

use super::LowerCtx;

// ─── Precedence layer descriptors ─────────────────────────────────────────────

/// One precedence layer: the set of operator symbols at this level
/// mapped to `MapBinOp` constructors. Symbols are listed
/// **longest-first** so prefix matching against a recovered byte
/// gap doesn't return `<` when the actual operator was `<=`.
struct PrecedenceLayer {
    ops: &'static [(&'static str, MapBinOp)],
}

const LAYER_OR: PrecedenceLayer = PrecedenceLayer {
    ops: &[("||", MapBinOp::Or)],
};
const LAYER_AND: PrecedenceLayer = PrecedenceLayer {
    ops: &[("&&", MapBinOp::And)],
};
const LAYER_CMP: PrecedenceLayer = PrecedenceLayer {
    ops: &[
        ("==", MapBinOp::Eq),
        ("!=", MapBinOp::Ne),
        ("<=", MapBinOp::Le),
        (">=", MapBinOp::Ge),
        ("<", MapBinOp::Lt),
        (">", MapBinOp::Gt),
    ],
};
const LAYER_ADD: PrecedenceLayer = PrecedenceLayer {
    ops: &[("+", MapBinOp::Add), ("-", MapBinOp::Sub)],
};
const LAYER_MUL: PrecedenceLayer = PrecedenceLayer {
    ops: &[
        ("*", MapBinOp::Mul),
        ("/", MapBinOp::Div),
        ("%", MapBinOp::Mod),
    ],
};

// ─── Public entry: lower a value expression ─────────────────────────────────

/// Lower a value expression view to a `MapExpr`. Single source of
/// truth for value-expression dispatch — every recursive descent
/// (operator-layer fold, atom unwrap, function-call args, closure
/// body) routes back through here.
pub(crate) fn lower_value_expr<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    dispatch_value_expr(node, ctx)
}

/// Central rule_kind dispatcher. The bbnf value-expression grammar
/// is a closed schema; every reachable rule_kind has an explicit
/// handler. Unknown rule_kinds panic with a descriptive message
/// pointing at the offending node — silent fallbacks are forbidden.
fn dispatch_value_expr<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    match node.rule_kind() {
        // Top-level value_expr wrapper (= value_closure | value_or).
        // Peel by re-dispatching on the inner head.
        //
        // Under DTA, the value_expr rule body is wrapped in a Seq
        // compound — `node.child(0)` picks the anonymous wrapper, not
        // the semantic head. Descend through anonymous wrappers to the
        // first value-layer compound (`value_closure` or `value_or`);
        // if the optimizer inlined even those, descend to any
        // downstream value-precedence-chain head.
        BbnfBootstrapRuleKind::value_expr => {
            let inner = value_expr_head(node).unwrap_or(node);
            if inner.cursor().offset() == node.cursor().offset() {
                // Defensive — avoid recursion if descent returned the
                // same node. Fall through to the atom path.
                return lower_value_atom(node, ctx);
            }
            dispatch_value_expr(inner, ctx)
        }

        // Top of the value sub-grammar — alt of closure / value_or chain.
        BbnfBootstrapRuleKind::value_or => lower_value_expr_or_closure(node, ctx),

        // Precedence-chain layers. Each is `(operand, (op operand)*)`
        // under structural mode the chain layer's compound has shape
        // `[first_operand, Repeat([rest_operands])]` with operator
        // bytes consumed-but-not-pushed between adjacent operand
        // spans.
        BbnfBootstrapRuleKind::value_and => fold_value_chain(node, &LAYER_AND, ctx),
        BbnfBootstrapRuleKind::value_cmp => fold_value_chain(node, &LAYER_CMP, ctx),
        BbnfBootstrapRuleKind::value_add => fold_value_chain(node, &LAYER_ADD, ctx),
        BbnfBootstrapRuleKind::value_mul => fold_value_chain(node, &LAYER_MUL, ctx),

        // Unary prefix layer. Body is `( "!" | "-" )? value_atom`;
        // the prefix consumes bytes without pushing, so we
        // disambiguate via leading-byte inspection of the unary
        // span text.
        BbnfBootstrapRuleKind::value_unary => lower_value_unary(node, ctx),

        // Atom layer. Inlined alt over int/float/bool/string lits,
        // input chains, paths, function calls, and parenthesised
        // sub-expressions. Discriminated by leading-byte inspection
        // of the atom span text.
        BbnfBootstrapRuleKind::value_atom => lower_value_atom(node, ctx),

        // Leaf rule_kinds — these only surface when the optimizer
        // happens to preserve the wrapper compound rather than
        // inlining.
        //
        // Under DTA `int_lit` is ALSO the walker's sentinel rule_kind
        // for compounds emitted without a `DtaState::Ref` (the
        // optimizer fully inlined the value_unary + value_atom
        // layers, so the value-expression body surfaces as a
        // sentinel-tagged compound carrying the atom's span).
        // Distinguish real int literals from the sentinel by the
        // leading byte: real numeric starts with a digit or `.`,
        // anything else is the inlined atom and should route through
        // `lower_value_atom` for proper classification by span text.
        BbnfBootstrapRuleKind::int_lit => {
            let text = node.span_text();
            let first = text.trim_start().as_bytes().first().copied();
            match first {
                Some(b'0'..=b'9') | Some(b'.') => parse_int_literal(text),
                _ => lower_value_atom(node, ctx),
            }
        }
        BbnfBootstrapRuleKind::float_lit => parse_float_literal(node.span_text()),
        BbnfBootstrapRuleKind::bool_lit => MapExpr::BoolLit(node.span_text() == "true"),
        BbnfBootstrapRuleKind::string_lit => lower_string_lit(node, ctx),
        BbnfBootstrapRuleKind::value_ident => lower_bare_ident(node.span_text(), ctx),
        BbnfBootstrapRuleKind::identifier => lower_bare_ident(node.span_text(), ctx),
        BbnfBootstrapRuleKind::value_input => lower_value_input(node, ctx),
        BbnfBootstrapRuleKind::value_path => lower_value_path(node, ctx),
        BbnfBootstrapRuleKind::value_fn_call => lower_value_fn_call(node, ctx),
        BbnfBootstrapRuleKind::value_closure => lower_value_closure(node, ctx),

        other => panic!(
            "lower/value_expr.rs: dispatch_value_expr called on \
             unhandled rule_kind {:?} (span = {:?}, text = {:?}). \
             Add an explicit handler for this rule_kind.",
            other,
            node.span(),
            node.span_text(),
        ),
    }
}

// ─── DTA descent helpers ─────────────────────────────────────────────────────

/// Outermost-first ordering of value-layer rule kinds used to find
/// the inner head of a `value_expr` compound under DTA.
///
/// The DTA walker wraps rule bodies in Seq compounds; `node.child(0)`
/// picks the anonymous wrapper, not the semantic head. A descendant
/// search against this ordered list returns the outermost semantic
/// rule in document order. `value_closure` and `value_or` alternate
/// at the `value_expr` body layer; the remaining precedence-chain
/// kinds + atom handle optimizer-collapsed shapes where the outer
/// wrapper was inlined away.
const VALUE_HEAD_KINDS: &[BbnfBootstrapRuleKind] = &[
    BbnfBootstrapRuleKind::value_closure,
    BbnfBootstrapRuleKind::value_or,
    BbnfBootstrapRuleKind::value_and,
    BbnfBootstrapRuleKind::value_cmp,
    BbnfBootstrapRuleKind::value_add,
    BbnfBootstrapRuleKind::value_mul,
    BbnfBootstrapRuleKind::value_unary,
    BbnfBootstrapRuleKind::value_atom,
];

/// Find the semantic head of a `value_expr` compound under DTA.
///
/// `find_descendant_by_kind` is called against each value-layer rule
/// kind in outermost-first priority order. First hit wins; returns
/// `None` only if none of the value-layer rule kinds surface as
/// descendants (pathological input — every `value_expr` body must
/// resolve to at least a `value_atom`).
fn value_expr_head<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<BbnfBootstrapNodeView<'a>> {
    for &kind in VALUE_HEAD_KINDS {
        if let Some(v) = find_descendant_by_kind(node, kind) {
            // Skip `node` itself — if `node.rule_kind() == value_expr`
            // and `find_descendant_by_kind` matches the root (it won't
            // here because value_expr is not in VALUE_HEAD_KINDS), we
            // still want to descend. Defensive: only return distinct
            // views.
            if v.cursor().offset() != node.cursor().offset() {
                return Some(v);
            }
        }
    }
    None
}

// ─── value_expr: closure / or-chain ──────────────────────────────────────────

/// Distinguish closure (`|p1, ...| body`) from a value_or chain by
/// inspecting the leading byte of the compound's span text. Under
/// structural mode the closure markers consume bytes without
/// pushing, so the closure case is identified by the leading `|`.
fn lower_value_expr_or_closure<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    let text = node.span_text();
    if text.as_bytes().first() == Some(&b'|') {
        lower_value_closure(node, ctx)
    } else {
        // value_or chain — body is `value_and , ( "||" ?w , value_and ) *`.
        fold_value_chain(node, &LAYER_OR, ctx)
    }
}

// ─── Operator-chain folding ──────────────────────────────────────────────────

/// Generic left-associative fold over a precedence layer.
///
/// Under structural-mode emission, every `value_X` chain rule pushes
/// a compound whose direct children are:
///   - the lower-precedence operand (one rule compound)
///   - a single `Repeat` compound containing each remaining operand
///     as its own rule compound (the operator tokens between them
///     consume bytes but push nothing)
///
/// The operator at position `i` is recovered from the source bytes
/// between `operands[i].span().1` and `operands[i+1].span().0`. We
/// match the longest valid operator prefix from the layer table —
/// the table lists multi-char operators first to avoid `<` shadowing
/// `<=`.
fn fold_value_chain<'a>(
    node: BbnfBootstrapNodeView<'a>,
    layer: &PrecedenceLayer,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    let operands: Vec<BbnfBootstrapNodeView<'a>> = collect_chain_operands(node);
    debug_assert!(
        !operands.is_empty(),
        "fold_value_chain: chain compound {:?} produced zero operands \
         (text = {:?})",
        node.rule_kind(),
        node.span_text(),
    );

    let mut iter = operands.into_iter();
    let first = iter.next().expect("fold_value_chain: missing first operand");
    let mut prev_end = first.span().1;
    let mut result = dispatch_value_expr(first, ctx);

    let input = node.input();
    for operand in iter {
        let op_text = recover_op_between(input, prev_end, operand.span().0, layer)
            .unwrap_or_else(|| {
                panic!(
                    "lower/value_expr.rs: failed to recover operator for layer \
                     in source gap {:?} (chain rule_kind = {:?})",
                    &input[prev_end as usize..operand.span().0 as usize],
                    node.rule_kind(),
                )
            });
        let op = layer
            .ops
            .iter()
            .find(|(t, _)| *t == op_text)
            .map(|(_, o)| *o)
            .expect("recover_op_between returned a token outside the layer table");
        prev_end = operand.span().1;
        result = MapExpr::BinOp {
            op,
            lhs: Box::new(result),
            rhs: Box::new(dispatch_value_expr(operand, ctx)),
        };
    }
    result
}

/// Collect all operand views from a chain compound. The shape under
/// structural mode is `[first, Repeat([rest...])]`; under flattened
/// (non-structural) mode the optimizer may have inlined the Repeat
/// wrapper, in which case the chain's children are already flat.
/// Under DTA the entire body may be wrapped in an anonymous Seq
/// compound — descend through anonymous wrappers first to reach the
/// true operand layout.
///
/// # Pratt cousin-leak guard (B3.W0.η)
///
/// Pratt-shape `value_X` rules emit a pre-order outer Rule compound
/// whose body sits at depth `parent + 1` while subsequent post-order
/// Seq/Repeat wrappers around the rule's outer iteration body push
/// records that, after their own `end_compound_post_order` bumps
/// cascade, settle at the same `parent + 1` depth. The finaliser's
/// depth-only sib_skip computation then erroneously chains
/// `value_X`'s direct child to a cousin record sitting AFTER
/// `value_X`'s span_hi (the type-annotation iteration's Seq wrapper,
/// for example). Bound the children walk by the parent compound's
/// span: any child whose span_lo lies at or beyond `node.span().1`
/// is NOT an operand of `node` — discard it. The check is a strict
/// span containment, so contiguous operand spans inside the chain
/// remain admitted.
///
/// Note: a single-operand chain compound (no operators) collapses
/// to one element; the loop in `fold_value_chain` simply returns
/// that operand's lowering unchanged.
fn collect_chain_operands<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Vec<BbnfBootstrapNodeView<'a>> {
    use ::bbnf::runtime::tape::TapeKind;

    // Under DTA the chain body may sit inside one or more anonymous
    // Seq wrappers. Descend through those wrappers to reach the
    // compound whose direct children are the semantic operand
    // layout `[first, iter_wrapper]`.
    let body = descend_anonymous_wrappers(node);
    let body_hi = body.span().1;
    let in_scope = |c: &BbnfBootstrapNodeView<'a>| {
        let (lo, hi) = c.span();
        hi > lo && lo < body_hi
    };

    let mut children = body.children().filter(in_scope);
    let Some(first) = children.next() else {
        return Vec::new();
    };
    // The remaining children are either zero (single-operand
    // chain), one `Repeat` compound (fn-per-rule structural mode),
    // or — under DTA — one `Rule` compound that's actually the
    // Repeat frame (the walker emits `frame_to_tape_kind(Repeat)
    // == Rule` per the AW-I substrate). Peel a single trailing
    // compound child whose children are the iteration operands.
    let mut operands = vec![first];
    let rest: Vec<BbnfBootstrapNodeView<'a>> = children.collect();
    let is_iteration_wrapper = |c: &BbnfBootstrapNodeView<'a>| {
        matches!(c.kind(), TapeKind::Repeat | TapeKind::Rule)
    };
    if rest.len() == 1 && is_iteration_wrapper(&rest[0]) {
        for child in rest[0].children().filter(in_scope) {
            operands.push(child);
        }
    } else {
        for child in rest {
            operands.push(child);
        }
    }
    operands
}

/// Descend through anonymous Seq/Alt/Repeat wrappers (those with
/// `rule_kind ∈ {Unknown, int_lit}`, the walker's sentinel for
/// compounds never stamped by a `DtaState::Ref`) until reaching a
/// compound whose direct children are the caller's semantic body.
///
/// Returns the innermost anonymous-wrapper view whose direct-child
/// count either exceeds one, or equals one but the sole child is
/// itself a semantic-rule compound. Single-anonymous-child chains
/// get collapsed; semantic content is preserved.
///
/// Intended for use by chain-operand / call-arg / body-content
/// collectors that walk direct children but whose caller's compound
/// is a DTA-wrapped rule body.
fn descend_anonymous_wrappers<'a>(
    mut view: BbnfBootstrapNodeView<'a>,
) -> BbnfBootstrapNodeView<'a> {
    loop {
        let children: Vec<BbnfBootstrapNodeView<'a>> = view.children().collect();
        if children.len() != 1 {
            return view;
        }
        let only_child = children[0];
        // Only descend if the child is itself an anonymous wrapper
        // (no semantic rule identity); otherwise stop — the child is
        // the semantic content the caller is after.
        if !is_anonymous_wrapper(only_child) {
            return view;
        }
        view = only_child;
    }
}

/// Whether `view` is an anonymous structural wrapper under DTA. Kept
/// in-file to avoid cross-module coupling; mirrors the helper in
/// `lower/tape_walk.rs` that gates sibling descents.
fn is_anonymous_wrapper(view: BbnfBootstrapNodeView<'_>) -> bool {
    use ::bbnf::runtime::tape::TapeKind;
    if !matches!(
        view.kind(),
        TapeKind::Rule | TapeKind::Seq | TapeKind::Alt | TapeKind::Repeat,
    ) {
        return false;
    }
    matches!(
        view.rule_kind(),
        BbnfBootstrapRuleKind::Unknown | BbnfBootstrapRuleKind::int_lit,
    )
}

/// Recover the operator token from the byte gap between two
/// adjacent operand spans. Skips leading whitespace and matches the
/// longest valid operator prefix from the layer table.
fn recover_op_between<'a>(
    input: &'a str,
    lhs_end: u32,
    rhs_start: u32,
    layer: &PrecedenceLayer,
) -> Option<&'a str> {
    let gap = &input[lhs_end as usize..rhs_start as usize];
    let trimmed = gap.trim_start();
    for &(op, _) in layer.ops {
        if trimmed.starts_with(op) {
            return Some(op);
        }
    }
    None
}

// ─── Unary ───────────────────────────────────────────────────────────────────

/// Lower a `value_unary` compound. Body is `( "!" | "-" )? value_atom`.
/// The prefix consumes bytes without pushing, so we identify it by
/// inspecting the unary compound's span text. If the leading byte
/// is `!` or `-`, wrap the atom in a unary op; otherwise the body
/// is the bare atom.
fn lower_value_unary<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    let text = node.span_text();
    let first_byte = text.as_bytes().first().copied();
    match first_byte {
        Some(b'!') | Some(b'-') => {
            let atom = first_atom_child(node).unwrap_or(node);
            // Avoid an infinite recursion if `first_atom_child`
            // returned the unary compound itself (defensive — the
            // grammar guarantees a value_atom child).
            if atom.cursor().offset() == node.cursor().offset() {
                panic!(
                    "lower/value_expr.rs: lower_value_unary saw a unary \
                     compound with no atom child (text = {:?})",
                    text,
                );
            }
            let inner = dispatch_value_expr(atom, ctx);
            let op = if first_byte == Some(b'!') {
                MapUnaryOp::Not
            } else {
                MapUnaryOp::Neg
            };
            MapExpr::UnaryOp {
                op,
                inner: Box::new(inner),
            }
        }
        _ => {
            // Bare atom — descend through the single child.
            let atom = first_atom_child(node).unwrap_or(node);
            if atom.cursor().offset() == node.cursor().offset() {
                // No child compound at all — fall back to atom
                // dispatch on `node` itself (which classifies the
                // span text).
                lower_value_atom(node, ctx)
            } else {
                dispatch_value_expr(atom, ctx)
            }
        }
    }
}

/// Find the first child compound of a unary view that looks like an
/// atom (or a transparent wrapper around one). The grammar
/// guarantees `value_atom` is the sole child compound.
///
/// Under DTA the atom sits inside an anonymous Seq wrapper emitted
/// by the walker for the unary's body; `node.children().next()` may
/// return that wrapper rather than the `value_atom` compound itself.
/// Descend to the first `value_atom` descendant to resolve under
/// both DTA and fn-per-rule shapes uniformly.
fn first_atom_child<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<BbnfBootstrapNodeView<'a>> {
    find_descendant_by_kind(node, BbnfBootstrapRuleKind::value_atom)
        .filter(|v| v.cursor().offset() != node.cursor().offset())
        .or_else(|| node.children().next())
}

// ─── Atom classification ─────────────────────────────────────────────────────

/// Lower a `value_atom` compound. Under structural mode the atom
/// rule has its leaf alts (int/float/bool/string/ident/path/input/
/// fn_call/parenthesised) inlined into a single function with no
/// per-alt sub-rule pushes — only the inner Repeat compounds (path
/// segments, input prop chain, function-call arg list) and any
/// recursive `value_expr` compound (for `(expr)` and fn-call args)
/// reach the tape.
///
/// Disambiguation walks the atom's span text from the first
/// non-whitespace byte:
///
///   - `0`-`9` or `.` followed by digit → numeric literal
///   - `'` / `"` → string literal
///   - `t` / `f` (followed by `rue`/`alse`) → bool literal
///   - `(` → parenthesised sub-expression — descend into the
///     value_expr child
///   - leading `input` keyword (`input` followed by `.` or end) →
///     input chain
///   - identifier-leading + `(` → function call
///   - identifier-leading + `::` → path
///   - bare identifier → ident
fn lower_value_atom<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    let text = node.span_text();
    let trimmed = text.trim_start();
    let first = trimmed.as_bytes().first().copied();

    match first {
        Some(b'0'..=b'9') => parse_numeric_literal_text(trimmed),
        Some(b'.') => {
            // `.5e3` — leading dot float.
            parse_float_literal(trimmed)
        }
        Some(b'"') => MapExpr::StringLit(intern_string_lit_inner(trimmed, ctx)),
        Some(b'\'') => MapExpr::StringLit(intern_string_lit_inner(trimmed, ctx)),
        Some(b'(') => lower_paren_atom(node, ctx),
        Some(b'!') | Some(b'-') => {
            // Unary prefix leaked into atom dispatch — the optimizer
            // collapsed the `value_unary` wrapper and what reached us
            // is a unary-shaped compound tagged `value_atom`. Route
            // through the unary path so the `!` / `-` prefix is honoured.
            lower_value_unary(node, ctx)
        }
        Some(b) if b == b'_' || (b as char).is_ascii_alphabetic() => {
            lower_atom_named(node, trimmed, ctx)
        }
        Some(_) | None => panic!(
            "lower/value_expr.rs: lower_value_atom saw an unexpected \
             leading byte in atom span {:?} (rule_kind = {:?})",
            text,
            node.rule_kind(),
        ),
    }
}

/// Lower a parenthesised atom: `( value_expr )`. The structural
/// shape pushes only the inner `value_expr` compound (the parens
/// consume bytes without pushing). The grammar guarantees exactly
/// one semantic child compound here.
///
/// Under DTA the atom's body is wrapped in an anonymous Seq
/// compound; `children().next()` may return that wrapper rather
/// than the `value_expr` record directly. Descend to the first
/// `value_expr` descendant; the outer `node.rule_kind()` is
/// `value_atom`, not `value_expr`, so the descent correctly returns
/// a distinct inner view.
fn lower_paren_atom<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    let inner = find_descendant_by_kind(node, BbnfBootstrapRuleKind::value_expr)
        .or_else(|| node.children().next())
        .expect("lower_paren_atom: parenthesised atom is missing its value_expr child");
    dispatch_value_expr(inner, ctx)
}

/// Lower an atom whose leading text is identifier-shaped: bool
/// literal, `input` chain, function call, path, or bare ident.
/// Disambiguates from the source slice without depending on
/// per-leaf rule_kind pushes.
fn lower_atom_named<'a>(
    node: BbnfBootstrapNodeView<'a>,
    trimmed: &'a str,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    // Bool literals.
    if trimmed.starts_with("true") && !next_ident_byte(trimmed, 4) {
        return MapExpr::BoolLit(true);
    }
    if trimmed.starts_with("false") && !next_ident_byte(trimmed, 5) {
        return MapExpr::BoolLit(false);
    }

    // Input chain: `input` keyword followed by zero or more `.ident`
    // accessors. The structural-mode tape pushes a single Repeat
    // compound for the prop chain (always pushed even when empty).
    if trimmed.starts_with("input") && !next_ident_byte(trimmed, 5) {
        return lower_input_chain(node, trimmed, ctx);
    }

    // Identifier head: scan the contiguous identifier run.
    let head_len = scan_ident_len(trimmed);
    debug_assert!(
        head_len > 0,
        "lower_atom_named: failed to extract identifier head from {:?}",
        trimmed,
    );
    let after_head = trimmed[head_len..].trim_start();
    if after_head.starts_with('(') {
        return lower_fn_call_atom(node, trimmed, ctx);
    }
    if after_head.starts_with("::") {
        return lower_path_atom(node, trimmed, ctx);
    }
    // Bare ident — possibly bound by a value-closure parameter.
    let name = &trimmed[..head_len];
    lower_bare_ident(name, ctx)
}

/// True iff `text[idx]` looks like a continuation byte of an
/// identifier. Used to ensure `true` / `input` keywords aren't
/// misidentified as the prefix of a longer identifier.
fn next_ident_byte(text: &str, idx: usize) -> bool {
    text.as_bytes().get(idx).is_some_and(|b| {
        b.is_ascii_alphanumeric() || *b == b'_'
    })
}

/// Scan the leading identifier run length: `[_a-zA-Z][_a-zA-Z0-9]*`.
fn scan_ident_len(text: &str) -> usize {
    let bytes = text.as_bytes();
    let mut i = 0;
    if let Some(b) = bytes.first() {
        if b.is_ascii_alphabetic() || *b == b'_' {
            i += 1;
            while let Some(b) = bytes.get(i) {
                if b.is_ascii_alphanumeric() || *b == b'_' {
                    i += 1;
                } else {
                    break;
                }
            }
        }
    }
    i
}

/// Lower an `input` chain. The atom compound's children are exactly
/// one `Repeat` compound (the prop chain — always pushed, may be
/// empty). Each prop-chain iteration consumed `.` then an
/// identifier; the identifier text comes from the source slice
/// after the dot.
fn lower_input_chain<'a>(
    node: BbnfBootstrapNodeView<'a>,
    trimmed: &'a str,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    // The grammar produces `Repeat(.ident)*`. Recover the prop name
    // by scanning the source slice past the `input` keyword: each
    // `.ident` segment yields one prop name; we use the LAST prop
    // (matching the legacy semantics — only the leaf prop is
    // surfaced as a `MapExpr::InputProp`).
    //
    // Source-slice walk is cheaper than enumerating the Repeat's
    // children (which carry no useful payload — the inner ident
    // scan doesn't push) and equivalent in semantics.
    let after = &trimmed[5..]; // skip "input"
    let mut last_prop: Option<&str> = None;
    let mut rest = after;
    loop {
        let stripped = rest.trim_start();
        if let Some(after_dot) = stripped.strip_prefix('.') {
            let after_dot = after_dot.trim_start();
            let name_len = scan_ident_len(after_dot);
            if name_len == 0 {
                break;
            }
            last_prop = Some(&after_dot[..name_len]);
            rest = &after_dot[name_len..];
        } else {
            break;
        }
    }
    // Suppress unused warning on `node` — the source-slice walk above
    // is the canonical recovery path; we keep the parameter to mirror
    // the other atom lowering helpers.
    let _ = node;
    match last_prop {
        Some(name) => {
            let sid = ctx.strings.intern(name);
            MapExpr::InputProp { prop: sid }
        }
        None => MapExpr::Input,
    }
}

/// Lower a function-call atom: `path(args?)`. The atom compound's
/// children are: `Repeat` (path segments — always pushed),
/// `Repeat` (the optional arg list — always pushed; contains
/// `[value_expr, Repeat([rest_value_exprs])]` when non-empty,
/// otherwise empty).
fn lower_fn_call_atom<'a>(
    node: BbnfBootstrapNodeView<'a>,
    trimmed: &'a str,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    // Recover the full path from the source slice. The path is
    // `ident (:: ident)*`; everything up to the opening `(` belongs
    // to the path.
    let path_text = recover_call_path(trimmed);
    let name_sid = ctx.strings.intern(&path_text);

    // Walk the atom's children to find the arg list. The path's
    // `(::ident)*` Repeat contains no Rule pushes, so the only Rule
    // compounds we encounter are the arg `value_expr` rules.
    let args: Vec<MapExpr> = collect_fn_call_args(node, ctx);
    MapExpr::FnCall {
        name: name_sid,
        args,
    }
}

/// Reconstruct a `::`-separated path text from the atom's source
/// slice. Walks the leading bytes, accumulating identifier segments
/// joined by `::`, until the run terminates (whitespace, `(`, end
/// of slice, or any non-identifier non-`::` byte). Used for both
/// path-only atoms and the path prefix of function-call atoms.
fn recover_call_path(trimmed: &str) -> String {
    let mut out = String::new();
    let mut i = 0;
    let bytes = trimmed.as_bytes();
    loop {
        // Identifier segment.
        let start = i;
        while let Some(b) = bytes.get(i) {
            if b.is_ascii_alphanumeric() || *b == b'_' {
                i += 1;
            } else {
                break;
            }
        }
        if i == start {
            break;
        }
        if !out.is_empty() {
            out.push_str("::");
        }
        out.push_str(&trimmed[start..i]);
        // `::` separator?
        if bytes.get(i).copied() == Some(b':') && bytes.get(i + 1).copied() == Some(b':') {
            i += 2;
            continue;
        }
        break;
    }
    out
}

/// Collect a function call's argument expressions. The atom
/// compound's children are a sequence of Repeat compounds; the arg
/// list is the trailing Repeat whose own children include
/// `value_expr` compounds. We scan all Repeat children and gather
/// any nested compound whose kind is `Rule` (i.e., a `value_expr`
/// rule push) — that gives us each arg in source order.
///
/// Under DTA the atom compound's body sits inside an anonymous Seq
/// wrapper; `node.children()` returns `[Seq]` rather than the
/// expected Repeat siblings. Descend through anonymous wrappers
/// first, then apply the Repeat-scan logic on the true body.
/// Additionally — under DTA a `value_expr` push surfaces as a
/// compound whose `rule_kind == value_expr`, not just any
/// `TapeKind::Rule`. Gate on rule_kind to avoid mistaking a nested
/// anonymous Seq-Rule for an argument.
fn collect_fn_call_args<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> Vec<MapExpr> {
    use ::bbnf::runtime::tape::TapeKind;
    let mut args: Vec<MapExpr> = Vec::new();

    // Descend through the DTA Seq wrapper to reach the atom body
    // whose direct children are the path + arg-list Repeats.
    let body = descend_anonymous_wrappers(node);

    // Arg is any descendant `value_expr` compound — but we must
    // avoid collecting args from any nested parenthesised
    // sub-expression or closure body. The arg list lives inside
    // the trailing Repeat of the fn-call atom; iterate that
    // Repeat's direct children (not descendants) to avoid
    // cross-boundary collection.
    //
    // The atom may have multiple Repeat children — one for the
    // path's `(::ident)*` segment list and one for the optional
    // arg list. Walk every Repeat (direct children of the atom
    // body) and pull out any nested compound. The path Repeat
    // contains only inlined ident scans which push nothing; the
    // arg-list Repeat contains `value_expr` rule compounds.
    let is_value_expr = |v: &BbnfBootstrapNodeView<'a>| {
        v.rule_kind() == BbnfBootstrapRuleKind::value_expr
    };
    for child in body.children() {
        if child.kind() == TapeKind::Repeat {
            for inner in child.children() {
                match inner.kind() {
                    TapeKind::Rule => {
                        // Under DTA a Rule compound may be an
                        // anonymous Seq-wrapper around the real
                        // value_expr rather than the value_expr
                        // itself. Prefer the value_expr descendant
                        // when the direct child's rule_kind isn't
                        // already value_expr.
                        if is_value_expr(&inner) {
                            args.push(dispatch_value_expr(inner, ctx));
                        } else if let Some(ve) = find_descendant_by_kind(
                            inner,
                            BbnfBootstrapRuleKind::value_expr,
                        ) {
                            args.push(dispatch_value_expr(ve, ctx));
                        } else {
                            // Optimizer fully inlined — dispatch on
                            // the compound directly; the handler
                            // classifies by rule_kind.
                            args.push(dispatch_value_expr(inner, ctx));
                        }
                    }
                    TapeKind::Repeat => {
                        // The optional arg list's tail-rest Repeat
                        // (`(, value_expr)*`) — recurse one level.
                        for grand in inner.children() {
                            if grand.kind() == TapeKind::Rule {
                                if is_value_expr(&grand) {
                                    args.push(dispatch_value_expr(grand, ctx));
                                } else if let Some(ve) = find_descendant_by_kind(
                                    grand,
                                    BbnfBootstrapRuleKind::value_expr,
                                ) {
                                    args.push(dispatch_value_expr(ve, ctx));
                                } else {
                                    args.push(dispatch_value_expr(grand, ctx));
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    args
}

/// Lower a path atom: `ident::ident::...` with no trailing `(`.
/// Mirrors the legacy semantics — single-segment paths fall back to
/// bare-ident resolution; multi-segment paths become a function
/// call on `MapExpr::Input`.
fn lower_path_atom<'a>(
    node: BbnfBootstrapNodeView<'a>,
    trimmed: &'a str,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    // The path is fully recoverable from the source slice; the
    // structural-mode tape doesn't push per-segment records.
    let _ = node;
    let path = recover_call_path(trimmed);
    if !path.contains("::") {
        return lower_bare_ident(&path, ctx);
    }
    if let Some(bound) = lookup_value_env(&path, &ctx.value_env) {
        return bound;
    }
    let sid = ctx.strings.intern(&path);
    MapExpr::FnCall {
        name: sid,
        args: vec![MapExpr::Input],
    }
}

// ─── Bare-ident resolution ───────────────────────────────────────────────────

/// Resolve a bare identifier in a value-expression context: closure
/// param bindings shadow ambient names; otherwise the identifier
/// becomes a one-arg function call on `MapExpr::Input` (the legacy
/// semantics — bare names are treated as transformer functions).
fn lower_bare_ident<'a>(name: &str, ctx: &mut LowerCtx<'a>) -> MapExpr {
    if let Some(bound) = lookup_value_env(name, &ctx.value_env) {
        return bound;
    }
    let sid = ctx.strings.intern(name);
    MapExpr::FnCall {
        name: sid,
        args: vec![MapExpr::Input],
    }
}

// ─── String literal ──────────────────────────────────────────────────────────

fn lower_string_lit<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    MapExpr::StringLit(intern_string_lit_inner(node.span_text(), ctx))
}

/// Strip the surrounding quote characters from a string-literal
/// span and intern the inner text. The grammar permits `"..."` and
/// `'...'`; the same delimiter byte appears at both ends.
fn intern_string_lit_inner<'a>(text: &str, ctx: &mut LowerCtx<'a>) -> bbnf_ir::StringId {
    let inner = if text.len() >= 2 {
        &text[1..text.len() - 1]
    } else {
        text
    };
    ctx.strings.intern(inner)
}

// ─── Standalone leaf accessors (when the tape DOES preserve the rule) ───────

/// Lower a `value_input` rule compound (when preserved). Defers to
/// the same source-slice walk used for inlined input chains —
/// behavioural parity with `lower_input_chain`.
fn lower_value_input<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    lower_input_chain(node, node.span_text().trim_start(), ctx)
}

/// Lower a `value_path` rule compound (when preserved). Same
/// source-slice walk as the inlined path-atom case.
fn lower_value_path<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    lower_path_atom(node, node.span_text().trim_start(), ctx)
}

/// Lower a `value_fn_call` rule compound (when preserved).
fn lower_value_fn_call<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    lower_fn_call_atom(node, node.span_text().trim_start(), ctx)
}

// ─── Closures ────────────────────────────────────────────────────────────────

/// Lower a value closure compound. The grammar is
/// `value_closure = "|", value_ident, ( "," ?w , value_ident ) * , "|", value_expr`.
///
/// Under structural-mode emission the `|` markers and parameter
/// idents consume bytes without pushing, but the inner `value_expr`
/// body DOES push a rule compound. Param recovery walks the source
/// slice between the leading `|` and the matching closing `|`,
/// stripping `,` separators and whitespace.
fn lower_value_closure<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    let text: &'a str = node.span_text();
    debug_assert!(
        text.as_bytes().first() == Some(&b'|'),
        "lower_value_closure: closure span doesn't start with `|`: {:?}",
        text,
    );

    // Find the closing `|` matching the opening one. Closure
    // params are bare identifiers separated by `,`; no nested `|`
    // appears within the param list.
    let after_open: &'a str = &text[1..];
    let close_rel = after_open
        .find('|')
        .expect("lower_value_closure: missing closing `|` for closure params");
    let params_text: &'a str = &after_open[..close_rel];

    // `text` is `&'a str` (the parser input lifetime), so every
    // sub-slice carries the same lifetime. No unsafe needed.
    let params: Vec<&'a str> = params_text
        .split(',')
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .collect();

    // The body is the trailing `value_expr` rule compound — it's
    // the only Rule child this compound contains (the param
    // identifiers don't push).
    //
    // Under DTA the body `value_expr` may sit inside an anonymous
    // Seq wrapper emitted by the walker; a direct `TapeKind::Rule`
    // first-match could pick that wrapper (also `TapeKind::Rule`
    // under the walker's `frame_to_tape_kind(Seq) == Rule` policy)
    // rather than the real value_expr. Descend to the value_expr
    // descendant; fall back to the first TapeKind::Rule child for
    // non-DTA shapes.
    use ::bbnf::runtime::tape::TapeKind;
    let body = find_descendant_by_kind(node, BbnfBootstrapRuleKind::value_expr)
        .or_else(|| node.children().find(|c| c.kind() == TapeKind::Rule))
        .expect("lower_value_closure: missing body value_expr child");

    // Bind each param into the value-environment frame. The first
    // param maps to `MapExpr::Input` (the convention — the closure
    // is applied at parse time to the matched span); the remaining
    // params map to `InputProp { prop: <param_name> }` so users can
    // pull additional payloads off composite inputs.
    let mut frame: HashMap<&'a str, MapExpr> = HashMap::new();
    for (i, name) in params.iter().copied().enumerate() {
        let value = if i == 0 {
            MapExpr::Input
        } else {
            let sid = ctx.strings.intern(name);
            MapExpr::InputProp { prop: sid }
        };
        frame.insert(name, value);
    }

    ctx.value_env.push(frame);
    let result = dispatch_value_expr(body, ctx);
    ctx.value_env.pop();
    result
}

// ─── Value env lookup ────────────────────────────────────────────────────────

/// Look up a name in the value-environment stack (top frame first,
/// mirroring lexical scope). Returns a clone of the bound `MapExpr`
/// if found.
fn lookup_value_env(name: &str, env: &[HashMap<&str, MapExpr>]) -> Option<MapExpr> {
    for frame in env.iter().rev() {
        if let Some(bound) = frame.get(name) {
            return Some(bound.clone());
        }
    }
    None
}

// ─── Value-expression helpers exported to expression.rs ──────────────────────

/// Extract a `&str` from a value expression view that's a bare
/// identifier or a single-segment path. Used by
/// `lower/expression.rs::lower_map_arrow` to detect the type-
/// shorthand `-> f64` form (where the value expression is just a
/// type name).
///
/// Recursively unwraps precedence-chain wrappers (`value_or` /
/// `value_and` / `value_cmp` / `value_add` / `value_mul` /
/// `value_unary` / `value_atom`) when each wrapper has only one
/// child compound (i.e. no operators in the chain). Returns the
/// raw identifier byte slice without copying.
pub(crate) fn unwrap_value_ident_str<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<&'a str> {
    use ::bbnf::runtime::tape::TapeKind;
    let mut cur = node;
    loop {
        match cur.rule_kind() {
            BbnfBootstrapRuleKind::value_ident
            | BbnfBootstrapRuleKind::identifier => {
                return Some(cur.span_text());
            }
            BbnfBootstrapRuleKind::value_path => {
                let text = cur.span_text().trim();
                return if text.contains("::") { None } else { Some(text) };
            }
            // Top-level value_expr wrapper — peel into the inner
            // head. Under DTA, `children().next()` picks the
            // anonymous Seq wrapper rather than the semantic head;
            // descend to the first value-layer rule compound.
            BbnfBootstrapRuleKind::value_expr => {
                cur = value_expr_head(cur)?;
            }
            // Precedence-chain wrappers: descend through the
            // first-and-only operand if there are no operators.
            BbnfBootstrapRuleKind::value_or
            | BbnfBootstrapRuleKind::value_and
            | BbnfBootstrapRuleKind::value_cmp
            | BbnfBootstrapRuleKind::value_add
            | BbnfBootstrapRuleKind::value_mul => {
                // Single-operand chain → text == operand text. Use
                // `collect_chain_operands` to detect. The operand
                // collection is already DTA-aware (W4.2 migration)
                // so this site inherits the descent through any
                // anonymous wrappers transparently.
                let operands = collect_chain_operands(cur);
                if operands.len() != 1 {
                    return None;
                }
                cur = operands.into_iter().next().unwrap();
            }
            BbnfBootstrapRuleKind::value_unary => {
                // Bare unary (no `!`/`-`) — descend into the atom.
                // Under DTA the atom sits one Seq deeper; descend to
                // the value_atom descendant rather than picking the
                // anonymous wrapper via `children().next()`.
                let text = cur.span_text();
                let first = text.as_bytes().first().copied();
                if first == Some(b'!') || first == Some(b'-') {
                    return None;
                }
                cur = find_descendant_by_kind(cur, BbnfBootstrapRuleKind::value_atom)
                    .filter(|v| v.cursor().offset() != cur.cursor().offset())
                    .or_else(|| cur.children().next())?;
            }
            BbnfBootstrapRuleKind::value_atom => {
                // Atom is identifier-shaped iff its leading non-ws
                // byte is `_`/alpha and the contiguous identifier
                // run equals the trimmed text length.
                let text = cur.span_text();
                let trimmed = text.trim();
                let first = trimmed.as_bytes().first().copied()?;
                if first != b'_' && !(first as char).is_ascii_alphabetic() {
                    return None;
                }
                let head = scan_ident_len(trimmed);
                if head == trimmed.len() {
                    return Some(&trimmed[..head]);
                } else {
                    return None;
                }
            }
            // Under DTA the walker surfaces `int_lit` / `Unknown` as
            // the sentinel rule_kind for compounds emitted without a
            // `DtaState::Ref`. In the value-expression chain, these
            // appear when the optimizer inlined the `value_unary` +
            // `value_atom` layers entirely — the sentinel compound's
            // span text IS the atom's text (for a type-name / bare
            // ident: `"i64"`, `"Span"`). Classify it as an atom when
            // the span is identifier-shaped.
            BbnfBootstrapRuleKind::int_lit | BbnfBootstrapRuleKind::Unknown => {
                let text = cur.span_text();
                let trimmed = text.trim();
                let first = trimmed.as_bytes().first().copied()?;
                // If the span starts with a digit or `.`, this is a
                // real int_lit / numeric — not an identifier.
                if first.is_ascii_digit() || first == b'.' {
                    return None;
                }
                // Treat as value_atom body: identifier-shaped span.
                if first != b'_' && !(first as char).is_ascii_alphabetic() {
                    return None;
                }
                let head = scan_ident_len(trimmed);
                if head == trimmed.len() {
                    return Some(&trimmed[..head]);
                } else {
                    return None;
                }
            }
            _ => return None,
        }
        // Termination is guaranteed because every iteration either
        // returns or descends through `children().next()` /
        // `collect_chain_operands`, both of which strictly shrink
        // the visited subtree. Defensive guard: stop on any
        // non-compound tape kind (every value-expression rule_kind
        // we care about emits a Rule compound).
        if !matches!(cur.kind(), TapeKind::Rule | TapeKind::Repeat | TapeKind::Seq | TapeKind::Alt) {
            return None;
        }
    }
}

/// Recursively unwrap value-expression wrappers down to the
/// innermost atom view. Used by `lower_map_arrow` to extract the
/// leaf node for type-suffix and bool-literal detection.
pub(crate) fn deep_unwrap_value<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> BbnfBootstrapNodeView<'a> {
    let mut cur = node;
    loop {
        match cur.rule_kind() {
            // Top-level value_expr wrapper — peel into the inner
            // head. Under DTA, `children().next()` picks the
            // anonymous Seq wrapper rather than the semantic head;
            // use `value_expr_head` to descend to the first
            // value-layer rule.
            BbnfBootstrapRuleKind::value_expr => {
                if let Some(head) = value_expr_head(cur) {
                    cur = head;
                } else {
                    return cur;
                }
            }
            BbnfBootstrapRuleKind::value_or
            | BbnfBootstrapRuleKind::value_and
            | BbnfBootstrapRuleKind::value_cmp
            | BbnfBootstrapRuleKind::value_add
            | BbnfBootstrapRuleKind::value_mul => {
                let operands = collect_chain_operands(cur);
                if operands.len() != 1 {
                    return cur;
                }
                cur = operands.into_iter().next().unwrap();
            }
            BbnfBootstrapRuleKind::value_unary => {
                let text = cur.span_text();
                let first = text.as_bytes().first().copied();
                if first == Some(b'!') || first == Some(b'-') {
                    return cur;
                }
                // Under DTA the atom sits one Seq deeper; descend to
                // the value_atom descendant rather than picking the
                // anonymous wrapper via `children().next()`.
                if let Some(atom) =
                    find_descendant_by_kind(cur, BbnfBootstrapRuleKind::value_atom)
                        .filter(|v| v.cursor().offset() != cur.cursor().offset())
                        .or_else(|| cur.children().next())
                {
                    cur = atom;
                } else {
                    return cur;
                }
            }
            BbnfBootstrapRuleKind::value_atom => {
                // Atoms are leaf-shaped — return as-is. Callers
                // (`lower_map_arrow`'s suffix / bool detection)
                // inspect the span text directly.
                return cur;
            }
            _ => return cur,
        }
    }
}

/// Extract a function name from a value expression that's a bare
/// function call (e.g. `myfunc(input)`) or a callable identifier.
/// Used by `lower_map_arrow` for `@host` return-type propagation —
/// the recovered name is looked up in the host-fn table.
///
/// Identifier-shaped atoms reach this helper because the structural-
/// mode tape inlines `value_ident` / `value_path` / `value_fn_call`
/// into `value_atom`. We classify by inspecting the atom span text:
/// any leading identifier byte yields a candidate name (path
/// segments included). The downstream lookup tolerates misses, so
/// returning a name eagerly is correct — non-host names simply
/// fail the host-table check.
pub(crate) fn extract_value_func_name<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<String> {
    match node.rule_kind() {
        BbnfBootstrapRuleKind::value_ident | BbnfBootstrapRuleKind::identifier => {
            Some(node.span_text().to_string())
        }
        BbnfBootstrapRuleKind::value_path => {
            Some(recover_call_path(node.span_text().trim_start()))
        }
        BbnfBootstrapRuleKind::value_fn_call => {
            Some(recover_call_path(node.span_text().trim_start()))
        }
        BbnfBootstrapRuleKind::value_atom => {
            let trimmed = node.span_text().trim_start();
            let first = *trimmed.as_bytes().first()?;
            if !(first.is_ascii_alphabetic() || first == b'_') {
                return None;
            }
            Some(recover_call_path(trimmed))
        }
        _ => None,
    }
}

/// Whitelist of builtin Rust type names recognised by the type-
/// shorthand `-> T` form.
///
/// `"Span"` admits the grammar-level `-> Span` shorthand so token rules
/// (`identifier`, `string_lit`, `regex`, `big_comment`, `comment`) route
/// through the backend-agnostic span-payload path rather than degrading
/// to `TypeDesc::Named("Span")`.
pub(crate) fn is_type_name(name: &str) -> bool {
    matches!(
        name,
        "Span"
            | "f64"
            | "f32"
            | "u32"
            | "u64"
            | "i32"
            | "i64"
            | "usize"
            | "u8"
            | "u16"
            | "i8"
            | "i16"
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

/// Discriminate int vs float by inspecting whether the digit run
/// contains a `.`, `e`, or `E`. Used by `lower_value_atom` when
/// the source slice's leading byte is a digit.
fn parse_numeric_literal_text(text: &str) -> MapExpr {
    let (digits, _) = split_numeric_suffix(text);
    if digits.starts_with("0x") || digits.starts_with("0X") {
        return parse_int_literal(text);
    }
    if digits.contains('.') || digits.contains('e') || digits.contains('E') {
        parse_float_literal(text)
    } else {
        parse_int_literal(text)
    }
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
