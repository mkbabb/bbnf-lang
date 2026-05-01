//! IR-body decomposition for `@pretty sep(...)` rules.
//!
//! When a rule declares `@pretty name sep(X)`, the `sep(X)` directive is the
//! *canonical* place to express the inter-iteration separator. Rule bodies
//! in the grammar typically also parse the same separator token natively
//! (either via `main << sep_token` or via a leading literal inside a Seq),
//! because parse combinators must consume the source bytes. Without this
//! split, prettify codegen would emit both — the body's natural token *and*
//! the `sep(...)` directive's flat bytes — yielding doubled separators
//! (`A1,, A2,, A3`) plus a trailing-leak when the outermost Repeat loops
//! one extra time through a nullable inner.
//!
//! The decomposition runs per-Repeat, splits the inner into
//! `(body_minus_sep, silent_sep)`, and returns the pair so the emitter can:
//!
//! - compile `body_minus_sep` with normal parse + emit, AND
//! - compile `silent_sep` with parse-only (no builder ops; wrapped in
//!   `light_checkpoint` + `light_restore` so parse state advances but no
//!   bytes are appended to the FmtOp stream).
//!
//! This keeps parse semantics identical (the separator tokens are still
//! consumed) while `sep(X)` becomes the sole emitter of inter-iteration
//! separator bytes, eliminating the double-emission.

use bbnf_ir::{GrammarIR, IrNode};

/// Where the silent-parsed separator node is positioned relative to the
/// main node within the original body.
///
/// Preserves source-order when emitting `main_out + silent_out` or
/// `silent_out + main_out` — parse semantics require the separator
/// consumes source bytes at its original position.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SilentPosition {
    /// Separator follows the main content (e.g., `main << sep`).
    After,
    /// Separator precedes the main content (e.g., `sep , main`).
    Before,
}

/// Decompose a Repeat's inner expression for `Custom(sep)` prettify
/// handling. Returns:
///
/// - `Some((main, silent, pos))` when the body parses + emits the
///   separator natively; the caller compiles `silent` under a
///   light-checkpoint wrapper so its emission is suppressed, but its
///   parse advances state at position `pos` within the body.
/// - `None` when no separator pattern is detected; no rewrite needed and
///   the inner compiles as-is.
///
/// Handles two body patterns encountered in grammars with `@pretty sep(X)`:
///
/// 1. `Skip(main, rhs)` — `main << sep_token` or `main << comma ?`. Yields
///    `Some((main, rhs, After))`; emit `main` first, then silent-parse rhs.
///
/// 2. `Seq([sep_token, rest...])` — `("," ?w , expression) *` form where
///    the separator is the leading child. Yields
///    `Some((Seq([rest...]), sep_token, Before))`; silent-parse the sep
///    first, then emit the rest.
pub fn split_inner_for_sep(
    inner: &IrNode,
    sep: &str,
    ir: &GrammarIR,
) -> Option<(IrNode, IrNode, SilentPosition)> {
    match inner {
        IrNode::Skip(main, rhs) => Some(((**main).clone(), (**rhs).clone(), SilentPosition::After)),

        IrNode::Seq(children) if !children.is_empty() => {
            if is_sep_token(&children[0], sep, ir) {
                let sep_len =
                    if children.len() > 1 && matches!(children[1], IrNode::OptionalWhitespace(_)) {
                        2
                    } else {
                        1
                    };
                let silent: Vec<IrNode> = children.iter().take(sep_len).cloned().collect();
                let rest: Vec<IrNode> = children.iter().skip(sep_len).cloned().collect();
                let main_node = match rest.len() {
                    0 => IrNode::Epsilon,
                    1 => rest.into_iter().next().unwrap(),
                    _ => IrNode::Seq(rest),
                };
                let silent_node = match silent.len() {
                    0 => IrNode::Epsilon,
                    1 => silent.into_iter().next().unwrap(),
                    _ => IrNode::Seq(silent),
                };
                Some((main_node, silent_node, SilentPosition::Before))
            } else {
                None
            }
        }

        _ => None,
    }
}

/// True if the node is a literal / regex / ref whose match text equals or
/// is a prefix-match of the `sep` directive's flat bytes.
///
/// Accepts:
/// - `Literal(s)` where `sep.starts_with(s)` (e.g., `","` matches sep `", "`).
/// - `Regex(pat)` where the pattern is a verbatim sep string.
/// - `Ref(rule_id)` whose rule body is itself a sep-matching token (recurses
///   one level to handle `comma = "," ?w`).
/// - `OptionalWhitespace(inner)` where `inner` is itself a sep token (the
///   lowering folds `"," ?w` into an OptionalWhitespace-wrapped literal).
fn is_sep_token(node: &IrNode, sep: &str, ir: &GrammarIR) -> bool {
    match node {
        IrNode::Literal(sid) => {
            let s = ir.get_string(*sid);
            !s.is_empty() && sep.starts_with(s)
        }
        IrNode::Regex(sid) => {
            let pat = ir.get_string(*sid);
            !pat.is_empty() && sep.starts_with(pat)
        }
        IrNode::Ref(rule_id) => {
            let body = &ir.rules[*rule_id as usize].body;
            ref_starts_with_sep(body, sep, ir)
        }
        IrNode::OptionalWhitespace(inner) => is_sep_token(inner, sep, ir),
        _ => false,
    }
}

/// Recursive helper: does a rule body start with (or consist of) the
/// separator token? Accepts a Seq with a sep-matching leading literal,
/// an OptionalWhitespace wrapping same, or a bare Literal/Regex. Bounded
/// recursion — only descends into the first child / inner wrapper.
fn ref_starts_with_sep(body: &IrNode, sep: &str, ir: &GrammarIR) -> bool {
    match body {
        IrNode::Literal(sid) => {
            let s = ir.get_string(*sid);
            !s.is_empty() && sep.starts_with(s)
        }
        IrNode::Regex(sid) => {
            let pat = ir.get_string(*sid);
            !pat.is_empty() && sep.starts_with(pat)
        }
        IrNode::Seq(children) if !children.is_empty() => is_sep_token(&children[0], sep, ir),
        IrNode::OptionalWhitespace(inner) => ref_starts_with_sep(inner, sep, ir),
        _ => false,
    }
}
