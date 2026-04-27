//! Binary-factor Pratt operator-precedence lowering.
//!
//! `binary_factor = mapped_factor , ( binary_operators ?w , mapped_factor ) *`
//!
//! Two tape shapes reach this entry, dispatched on structure:
//!
//! 1. **Pratt reducer chain** — `parse_pratt_<rule>` emits an outer
//!    compound whose `child_off` points at the LAST reducer in a
//!    right-leaning chain. Each reducer is a Rule compound whose
//!    `variant_idx` carries the op_discriminant and whose three
//!    children are `[LHS, Span_op_leaf, RHS]`. The walk below
//!    flattens the chain into source order.
//! 2. **Walker-era iteration-pair layout** — `[first_operand,
//!    iteration_wrapper]` where the wrapper holds per-iteration `Seq`
//!    compounds each wrapping `(operator, optional_ws, operand)`.
//!
//! Pratt detection fires first; the walker-era branch is the
//! fallback for grammars whose `binary_factor` is not routed through
//! the shape dispatcher.

use bbnf_ir::IrNode;

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};

use super::super::LowerCtx;
use super::alt::{is_iteration_pair_wrapper, iter_pair_children};
use super::dispatch_expression;

/// Lower a `binary_factor = mapped_factor , ( binary_operators ?w , mapped_factor ) *` view.
///
/// Tape shape under DTA:
///
/// ```text
/// Seq (binary_factor)
///   [0]: Seq (mapped_factor)                  -- first operand
///   [1]: Rule (iteration-pair wrapper)
///     [0]: Seq (int_lit)                      -- iteration pair 1
///       [0]: Seq (int_lit)                    -- operator wrapper
///         [0]: Alt (binary_operators)         -- operator itself
///       [1]: Seq (mapped_factor)              -- pair operand
///     [1]: Seq (int_lit)                      -- iteration pair 2
///       ... (same shape)
/// ```
///
/// [`collect_binary_operands`] flattens the iteration-pair wrapping so
/// the partition loop sees a linear `[operand, operator, operand,
/// operator, operand, ...]` sequence with operator compounds surfaced
/// as `rule_kind=binary_operators` Alts (emitted by the walker's
/// variant_idx stamping) — or, as a belt-and-braces fallback, any
/// compound whose trimmed span matches the fixed `<<` / `>>` / `-`
/// set.
///
/// Single-operand chains (no operators) collapse to the bare
/// operand without wrapping.
pub(crate) fn lower_binary_factor<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let all_children: Vec<BbnfBootstrapNodeView<'a>> = collect_binary_operands(node);
    debug_assert!(
        !all_children.is_empty(),
        "binary_factor: chain compound produced zero children (text = {:?})",
        node.span_text(),
    );

    // Partition children into (operators, operands). An operator child
    // is recognized by any of:
    //
    //  1. `rule_kind() == binary_operators` — the walker stamped the
    //     Alt compound with the binary_operators rule id. This is the
    //     structurally-rich signal under DTA when the walker's
    //     variant_idx stamping reaches the Alt.
    //  2. Trimmed span text matches one of `<<` / `>>` / `-` —
    //     lightweight belt-and-braces recognition that catches the
    //     case where the operator compound was lifted/inlined to a
    //     bare Seq whose only meaningful content is the literal
    //     punctuation. The fixed alphabet is safe: the surrounding
    //     grammar rules out operand compounds whose full trimmed span
    //     is exactly one of these three tokens.
    //
    // Every other child is an operand.
    let mut operands: Vec<BbnfBootstrapNodeView<'a>> = Vec::new();
    let mut inline_ops: Vec<&'a str> = Vec::new();
    for child in &all_children {
        if let Some(op) = recognize_binary_operator(*child) {
            inline_ops.push(op);
        } else {
            operands.push(*child);
        }
    }

    if operands.len() <= 1 {
        let only = operands
            .into_iter()
            .next()
            .expect("binary_factor: no operands after partition");
        return dispatch_expression(only, ctx);
    }

    let input = node.input();
    let mut iter = operands.into_iter();
    let first = iter.next().unwrap();
    let mut prev_end = first.span().1;
    let mut result = dispatch_expression(first, ctx);
    let mut op_iter = inline_ops.into_iter();

    for operand in iter {
        // Prefer structurally-identified operator children; fall back
        // to source-gap recovery when the operator compound was fully
        // span-elided (no record to recognize).
        let op_text = op_iter.next().or_else(|| {
            recover_binary_op(input, prev_end, operand.span().0)
        }).unwrap_or_else(|| {
            panic!(
                "lower/expression.rs: binary_factor could not resolve \
                 operator — no binary_operators child and source gap \
                 {:?} contains no recognized token (chain = {:?})",
                &input[prev_end as usize..operand.span().0 as usize],
                node.span_text(),
            )
        });
        prev_end = operand.span().1;
        result = apply_binary_op(result, op_text, operand, ctx);
    }
    result
}

/// Recognize a `binary_factor` operator child. Returns the operator
/// token (`"<<"` / `">>"` / `"-"`) if the child represents an operator
/// compound, `None` if it is an operand.
///
/// Matches either:
///  * `child.rule_kind() == binary_operators` — the walker's
///    variant_idx stamping reached the Alt compound; its trimmed
///    span carries the operator literal directly.
///  * A compound whose trimmed span is exactly one of the fixed
///    operator tokens — the DTA lifter may wrap the Alt in an
///    anonymous `Seq` whose own `rule_kind` is not `binary_operators`
///    but whose full text is still just the operator punctuation.
///
/// The fixed-alphabet span check is safe because operand compounds
/// are never literal `<<` / `>>` / `-` — the grammar's
/// `mapped_factor` layer produces quoted literals, identifiers,
/// regex atoms, or bracket-grouped sub-expressions, none of which
/// trim down to one of these three tokens.
fn recognize_binary_operator<'a>(
    child: BbnfBootstrapNodeView<'a>,
) -> Option<&'a str> {
    if child.rule_kind() == BbnfBootstrapRuleKind::binary_operators {
        return Some(child.span_text().trim());
    }
    let trimmed = child.span_text().trim();
    if matches!(trimmed, "<<" | ">>" | "-") {
        return Some(trimmed);
    }
    None
}

/// Collect the flattened child sequence of a `binary_factor`
/// compound as `[first_operand, op, operand, op, operand, ...]`.
///
/// Two tape shapes reach this entry, dispatched on structure:
///
/// 1. **Pratt reducer chain** (post-W0a.2.l under shape-dispatched
///    routing). `parse_pratt_BbnfBootstrap_binary_factor` emits an
///    outer `TapeKind::Rule` compound whose `child_off` points at the
///    LAST reducer in a right-leaning chain. Each reducer is itself
///    a `TapeKind::Rule` compound stamped with the op_discriminant as
///    its `variant_idx` (0/1/2 → `<<`/`>>`/`-`), holding three
///    children `[LHS_compound, Span_op_leaf, RHS_compound]`. The LHS
///    is either the previous reducer or the initial operand;
///    the op-leaf is a `TapeKind::Span` leaf with `variant_idx == 0`
///    carrying the operator's punctuation via its source span. This
///    shape surfaces as a SINGLE direct child on the outer compound
///    even when multiple operators fire — the `sib_skip` finaliser
///    only admits the last reducer at the outer's frame depth. Walk
///    the chain backward to produce `[initial, op, rhs₁, op, rhs₂,
///    …]` in source order.
///
/// 2. **Walker-era iteration-pair layout** (pre-Pratt routing; still
///    reaches this entry for grammars whose binary_factor is not
///    routed through the shape dispatcher, plus any residual walker-
///    tape callers until W0b). The walker emits `[first_operand,
///    iteration_wrapper]` where the wrapper holds per-iteration
///    `Seq` compounds each wrapping `(operator, optional_ws,
///    operand)`. `is_iteration_pair_wrapper` + `iter_pair_children`
///    flatten that wrapping.
///
/// Pratt detection fires first so the Pratt shape is always caught
/// before walker-era heuristics get to inspect the outer. When the
/// detector rejects (no reducer-chain structure), control falls
/// through to the walker-era branch unchanged — preserving the
/// tape-shape compatibility required until W0b deletes the walker
/// outright.
fn collect_binary_operands<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Vec<BbnfBootstrapNodeView<'a>> {
    use crate::runtime::tape::TapeKind;

    // Pratt-shape branch: detect a reducer-chain outer and walk it
    // into the flat `[operand, op_leaf, operand, op_leaf, …]`
    // sequence the partition loop consumes. The op-leaves are
    // `TapeKind::Span` records whose `span_text().trim()` carries one
    // of `<<` / `>>` / `-`, so the existing `recognize_binary_operator`
    // partition fires on the span-text arm.
    if let Some(chain) = collect_pratt_reducer_chain(node) {
        return chain;
    }

    // The cousin-leak boundary that previously lived here lives in
    // [`crate::runtime::tape::ChildIter`] post-B5.W4 — the iterator
    // terminates when a candidate's `span_lo` reaches the parent's
    // `span_hi`, so the walker-era iteration-pair branch sees only
    // operands strictly inside the parent's source extent.

    // Walker-era iteration-pair branch.
    let mut children = node.children();
    let Some(first) = children.next() else {
        return Vec::new();
    };
    let mut operands = vec![first];
    let rest: Vec<BbnfBootstrapNodeView<'a>> = children.collect();

    // Collect iteration-pair compounds. Under DTA there is always a
    // single Rule/Repeat wrapper; under the legacy fn-per-rule tape
    // the wrapper may be absent and operands sit directly after the
    // first mapped_factor.
    let pairs: Vec<BbnfBootstrapNodeView<'a>> =
        if rest.len() == 1 && matches!(rest[0].kind(), TapeKind::Repeat | TapeKind::Rule) {
            rest[0].children().collect()
        } else {
            rest
        };

    for pair in pairs {
        // Each pair is either a direct operand (legacy shape) or a
        // Seq compound wrapping `(operator, optional_ws, operand)`.
        // Descend one level and flatten when the pair is a wrapper;
        // keep it whole otherwise.
        if is_iteration_pair_wrapper(pair) {
            for grandchild in iter_pair_children(pair) {
                operands.push(grandchild);
            }
        } else {
            operands.push(pair);
        }
    }

    operands
}

/// Detect a Pratt reducer-chain outer and walk it into the flat
/// `[initial_operand, op_leaf, operand, op_leaf, operand, …]`
/// sequence expected by [`lower_binary_factor`]'s partition loop.
///
/// Returns `None` when `outer` does not look like a Pratt reducer-
/// chain outer — single-operand binary_factor (no reducer fires) or
/// walker-era iteration-pair layout both land here.
///
/// # Shape invariants (see `emit_parse_pratt` in
/// `backend/rust/emitter/shapes/pratt.rs`)
///
/// - The outer compound has exactly ONE direct child visible via
///   the frame-depth child iteration: `sib_skip` admits only the
///   last reducer because the outer's `child_off` points at it and
///   every intermediate operand / op-leaf sits at a deeper frame.
/// - That child is `TapeKind::Rule` with `variant_idx ∈ {0,1,2}`
///   (the op_discriminant). When surfaced through `rule_kind()`'s
///   variant_idx → rule table the reducer aliases to `int_lit`,
///   `float_lit`, or `bool_lit` — a pure-coincidence collision
///   with the literal rules' variant ids; the structural check on
///   `(kind, variant_idx, children.len(), children[1].kind())` is
///   the disambiguator.
/// - The reducer compound has EXACTLY three direct children
///   `[LHS, op_leaf, RHS]`. The middle child is `TapeKind::Span`
///   with `variant_idx == 0` whose span text is one of the fixed
///   operator tokens.
///
/// Every reducer whose LHS is ALSO a reducer chains the same shape
/// recursively. Termination: the deepest reducer's LHS is the
/// initial operand — a different structure (literal / identifier /
/// regex / grouped expression) that fails the reducer check.
fn collect_pratt_reducer_chain<'a>(
    outer: BbnfBootstrapNodeView<'a>,
) -> Option<Vec<BbnfBootstrapNodeView<'a>>> {
    // Three entry shapes reach this detector:
    //
    //  (a) `outer` is the proper `binary_factor` Rule compound with
    //      `variant_idx = 34` pushed by `parse_pratt_<rule>`'s close
    //      step. Its `children()` yield EXACTLY ONE direct child, the
    //      tail reducer. Chain walk proceeds from there.
    //
    //  (b) `outer` is the tail reducer itself. Under the post-W0a.2.l
    //      tape a concatenation-iteration `Seq vi=0` wrapper surfaces
    //      this reducer through its `children()` sib-skip walk even
    //      though the true direct child (per frame-depth) is the
    //      binary_factor outer compound. The wrapper's first-child
    //      backward walk leaps past the binary_factor outer into the
    //      reducer's own children. When a caller dispatches on those
    //      children, the tail reducer arrives as `outer` here.
    //
    //  (c) `outer` is a concatenation-iteration wrapper whose own
    //      `children()` surfaces `[lhs, op_leaf, rhs]` directly — the
    //      tail reducer's three children rather than the tail reducer
    //      compound itself. AX.W0a.2.o: treat `outer` AS IF it were
    //      the tail reducer — process these 3 children as the
    //      tail-level `[lhs, op_leaf, rhs]` triple and recurse on `lhs`
    //      for any deeper reducers.
    //
    // Case (a) vs (b) vs (c) is discriminated by inspecting (outer,
    // outer.children()): outer-is-reducer → (b), outer has exactly one
    // reducer child → (a), outer has a 3-element `[X, op_leaf, Y]`
    // sequence → (c). Cases (a) and (b) converge on walking a single
    // tail reducer; case (c) starts the walk at `outer` as the virtual
    // tail reducer.
    let tail_reducer = if is_pratt_reducer(outer) {
        // Case (b): outer IS the tail reducer.
        outer
    } else {
        let mut iter = outer.children();
        let first = iter.next()?;
        let second = iter.next();
        let third = iter.next();
        let fourth = iter.next();

        if second.is_none() && fourth.is_none() && is_pratt_reducer(first) {
            // Case (a): outer has exactly ONE direct child (the tail
            // reducer). Walk starts from `first`.
            first
        } else if fourth.is_none() {
            // Case (c): outer has three children `[lhs, op_leaf, rhs]`
            // surfaced by the wrapper's sib-skip leak past the outer
            // binary_factor compound. Confirm shape: middle is an
            // op-leaf (Span, variant_idx 0, fixed-alphabet span), and
            // `first` / `third` are present. Treat `outer` as the
            // virtual tail reducer and walk from here — `is_pratt_
            // reducer(first)` decides whether the chain continues up
            // or terminates at `first` as the initial operand.
            let op_leaf = second?;
            let _rhs = third?;
            if !op_leaf_has_pratt_shape(op_leaf) {
                return None;
            }
            outer
        } else {
            return None;
        }
    };

    // Walk the chain: each reducer (or virtual tail in Case c)
    // contributes `[op_leaf, RHS]`; after the chain, the deepest
    // reducer's LHS is the initial operand. Push in reverse order
    // (tail reducer's RHS first) then reverse at the end.
    let mut reversed: Vec<BbnfBootstrapNodeView<'a>> = Vec::new();
    let mut current = tail_reducer;
    loop {
        let mut kids_iter = current.children();
        let lhs = kids_iter.next()?;
        let op_leaf = kids_iter.next()?;
        let rhs = kids_iter.next()?;
        // Sanity: exactly 3 children.
        if kids_iter.next().is_some() {
            return None;
        }
        reversed.push(rhs);
        reversed.push(op_leaf);
        if is_pratt_reducer(lhs) {
            current = lhs;
        } else {
            reversed.push(lhs);
            break;
        }
    }
    reversed.reverse();
    Some(reversed)
}

/// Shape check: `view` is a Pratt op-leaf — a `TapeKind::Span` record
/// with `variant_idx == 0` whose trimmed span text is one of the fixed
/// binary_operators tokens (`<<` / `>>` / `-`).
///
/// Used by [`collect_pratt_reducer_chain`] to confirm that Case (c)'s
/// middle-child position carries an op-leaf before treating the
/// surrounding wrapper as a virtual tail reducer.
fn op_leaf_has_pratt_shape<'a>(view: BbnfBootstrapNodeView<'a>) -> bool {
    use crate::runtime::tape::TapeKind;
    if view.kind() != TapeKind::Span {
        return false;
    }
    if view.variant_idx() != 0u8 {
        return false;
    }
    matches!(view.span_text().trim(), "<<" | ">>" | "-")
}

/// Structural check: `view` matches the shape of a Pratt reducer
/// compound — `TapeKind::Rule` with op_discriminant `variant_idx`
/// and three children whose middle is the `TapeKind::Span` op-leaf.
///
/// Accepts `variant_idx ∈ {0,1,2}` to cover every binary operator's
/// op_discriminant within `PRECEDENCE_ENTRIES_binary_factor`; if a
/// future grammar introduces a fourth binary operator the range
/// widens naturally (the check on `op_leaf.kind() == Span` + its
/// variant_idx == 0 + span text in the fixed alphabet remains the
/// authoritative discriminator).
pub(super) fn is_pratt_reducer<'a>(view: BbnfBootstrapNodeView<'a>) -> bool {
    use crate::runtime::tape::TapeKind;
    if view.kind() != TapeKind::Rule {
        return false;
    }
    // op_discriminant lives in [0, 2] inclusive for binary_factor's
    // three operators (`<<` / `>>` / `-`).
    if !matches!(view.variant_idx(), 0 | 1 | 2) {
        return false;
    }
    let mut kids = view.children();
    let _lhs = match kids.next() {
        Some(v) => v,
        None => return false,
    };
    let op_leaf = match kids.next() {
        Some(v) => v,
        None => return false,
    };
    let _rhs = match kids.next() {
        Some(v) => v,
        None => return false,
    };
    if kids.next().is_some() {
        return false;
    }
    // Middle child must be the op-leaf: Span kind, variant_idx = 0,
    // trimmed span text is one of the fixed operator tokens.
    if op_leaf.kind() != TapeKind::Span {
        return false;
    }
    if op_leaf.variant_idx() != 0u8 {
        return false;
    }
    let trimmed = op_leaf.span_text().trim();
    matches!(trimmed, "<<" | ">>" | "-")
}

/// Structural check: `view`'s direct children surface as a flat
/// `[operand, op_leaf, operand, op_leaf, …]` Pratt sequence — the
/// tail reducer's three children `[LHS_reducer, op_leaf, RHS]`
/// leaked through an enclosing wrapper's sib-skip backward walk
/// instead of the true `binary_factor` outer compound.
///
/// Discriminator: at least one direct child is a `TapeKind::Span`
/// record with `variant_idx == 0` whose trimmed span text is one of
/// the fixed operator tokens `<<` / `>>` / `-`. No walker-era
/// wrapper carries a span-text op-leaf at its first structural
/// level (the walker produces explicit `binary_operators` Alt
/// compounds, not bare Span leaves), so the presence of this shape
/// is authoritative evidence that the current tape is Pratt-flat.
///
/// Returns `false` when no such op-leaf child exists — the caller
/// falls through to the standard wrapper-substantive branch.
pub(super) fn looks_like_pratt_flat<'a>(view: BbnfBootstrapNodeView<'a>) -> bool {
    use crate::runtime::tape::TapeKind;
    for child in view.children() {
        if child.kind() == TapeKind::Span
            && child.variant_idx() == 0
            && matches!(child.span_text().trim(), "<<" | ">>" | "-")
        {
            return true;
        }
    }
    false
}

/// Recover a binary-factor operator (`<<` / `>>` / `-`) from the
/// source slice between two adjacent operand spans. Skips leading
/// whitespace and matches the longest valid operator prefix; the
/// two-character operators are listed first so `<` doesn't shadow
/// `<<`.
fn recover_binary_op<'a>(input: &'a str, lhs_end: u32, rhs_start: u32) -> Option<&'a str> {
    let gap = &input[lhs_end as usize..rhs_start as usize];
    let trimmed = gap.trim_start();
    for &op in &["<<", ">>", "-"] {
        if trimmed.starts_with(op) {
            return Some(op);
        }
    }
    None
}

fn apply_binary_op<'a>(
    lhs: IrNode,
    op_text: &str,
    operand: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let rhs = dispatch_expression(operand, ctx);
    match op_text {
        "<<" => IrNode::Skip(Box::new(lhs), Box::new(rhs)),
        ">>" => IrNode::Next(Box::new(lhs), Box::new(rhs)),
        "-" => IrNode::Minus(Box::new(lhs), Box::new(rhs)),
        other => panic!(
            "lower/expression.rs: apply_binary_op saw an unknown \
             binary_operator token {:?} (recovered from operand gap)",
            other,
        ),
    }
}
