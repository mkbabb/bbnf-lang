//! ArgList-shape detector — `name(arg, arg, ...)` positional call.
//!
//! # Predicate
//!
//! A rule is ArgList-shaped when its body unwraps to a Seq whose
//! structural positions are:
//!
//! 1. A **name head** — either a `Literal` (`"calc"`, `"rgb"`,
//!    `"url"`), a `Regex` (Sheets `LET(` / `LAMBDA(` case-insensitive
//!    regex-with-embedded-`(`), OR a `Ref` to an identifier-like rule
//!    (Sheets `func_open = identifier , "("`).
//! 2. A **`(` open literal** — either as a sibling Seq position OR
//!    folded into the regex/ref head.
//! 3. A **positional arg repeat** — `Repeat(arg)` (possibly with
//!    `Skip(arg, sep?)` separator structure).
//! 4. A **`)` close literal** — the last non-trivia Seq position.
//!
//! # Canonical sources
//!
//! - CSS `calcFunction = "calc" , "(" >> mathExpr << ")"` per
//!   `grammar/css/l4/values.bbnf`.
//! - CSS `rgbFunction` / `hsl` / `colorFunction` family per
//!   `grammar/css/l4/color.bbnf`.
//! - CSS `transforms.bbnf` / `filters.bbnf` per-transform /
//!   per-filter functions (`translate(x, y)`, `blur(r)`, etc.).
//! - Sheets `func_call = func_open , (func_args ?) ?w , ")"` where
//!   `func_open = identifier , "("` per
//!   `grammar/google-sheets/google-sheets.bbnf:139-143`.
//! - Sheets `let_call = /[lL][eE][tT]\(/ , let_args ?w , ")"`
//!   per `grammar/google-sheets/google-sheets.bbnf:148`.
//! - Sheets `lambda_call` per
//!   `grammar/google-sheets/google-sheets.bbnf:152`.
//!
//! # Projection
//!
//! Pure structural inspection of the rule body. No new mining.

use crate::passes::inspect::{single_byte_literal, unwrap_map_ow};
use crate::types::{GrammarIR, IrNode, RuleId};

/// Detect ArgList-shape: a Seq of `name ( args )` structure.
pub fn detect_arglist(rule_id: RuleId, ir: &GrammarIR) -> bool {
    let rule = &ir.rules[rule_id as usize];
    let body = unwrap_map_ow(&rule.body);
    classify_arglist(body, ir)
}

/// Return true when `node` matches the ArgList structural shape.
///
/// The classifier is tolerant: it admits both the literal-head shape
/// (CSS `"calc" , "(" >> body << ")"`) and the ref-head shape (Sheets
/// `func_open , (args) , ")"` where `func_open = identifier , "("`).
fn classify_arglist(node: &IrNode, ir: &GrammarIR) -> bool {
    // The Seq children list — after stripping Next / Skip wrappers —
    // must be a minimum of two positions: a head and a closing `)`.
    // We flatten through Next / Skip because the BBNF lowering wraps
    // sequential composition into those nodes.
    let mut positions: Vec<&IrNode> = Vec::new();
    flatten_seq(node, &mut positions);
    if positions.len() < 2 {
        return false;
    }

    // First position: name head — Literal, Regex, or Ref to identifier /
    // identifier-with-`(` rule.
    let head = unwrap_map_ow(positions[0]);
    let head_admitted = match head {
        IrNode::Literal(_) => true,
        // Sheets `LET(` / `LAMBDA(` regex head — includes the open
        // paren inline via `\(` at pattern tail.
        IrNode::Regex(_) => true,
        // Sheets `func_open = identifier , "("` — ref head that
        // resolves through `func_open` to an identifier + `"("` seq.
        IrNode::Ref(rid) => is_identifier_or_func_open_ref(*rid, ir),
        _ => false,
    };
    if !head_admitted {
        return false;
    }

    // Last position: closing `)` literal.
    let close = unwrap_map_ow(positions[positions.len() - 1]);
    if single_byte_literal(close, ir) != Some(b')') {
        return false;
    }

    // We need at least one "body" position between head and close —
    // either an args ref, a Repeat, or a Seq of arg specifiers.
    let body_positions = &positions[1..positions.len() - 1];
    if body_positions.is_empty() {
        // `name()` with no body — only admitted when the head is a
        // literal/regex that already includes the open paren
        // (`LET(` / `LAMBDA(` regex case). Reject the plain
        // Literal-head case — it would never consume the `(`.
        return matches!(head, IrNode::Regex(_));
    }

    // Otherwise the head must be the name; either the body carries
    // the `(` as an early Literal, OR the head's own rule body
    // already carries it (func_open ref).
    body_contains_open_paren_or_args(head, body_positions, ir)
}

/// Flatten Next / Skip chains into a positional list. Strips
/// OptionalWhitespace / Map trivia — Epsilon nodes are dropped.
fn flatten_seq<'a>(node: &'a IrNode, out: &mut Vec<&'a IrNode>) {
    match unwrap_map_ow(node) {
        IrNode::Seq(children) => {
            for child in children {
                flatten_seq(child, out);
            }
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            flatten_seq(lhs, out);
            flatten_seq(rhs, out);
        }
        IrNode::Epsilon => {}
        other => out.push(other),
    }
}

/// Check the body carries either an early `(` literal (literal-head
/// case) OR a Repeat / Ref / Seq representing the args (ref-head case).
fn body_contains_open_paren_or_args(
    head: &IrNode,
    body_positions: &[&IrNode],
    ir: &GrammarIR,
) -> bool {
    // If the head is a Literal, the body must include a `(` literal
    // as its first structural position.
    if matches!(head, IrNode::Literal(_)) {
        if body_positions.is_empty() {
            return false;
        }
        let first_body = unwrap_map_ow(body_positions[0]);
        if single_byte_literal(first_body, ir) != Some(b'(') {
            return false;
        }
        // Body must also carry at least one more position (the args).
        return body_positions.len() >= 2;
    }
    // If the head is a Regex with embedded `(` (Sheets `LET(` case),
    // body positions need to carry the args + we already matched
    // the trailing `)`.
    if matches!(head, IrNode::Regex(_)) {
        return !body_positions.is_empty();
    }
    // Ref head — the ref must resolve to a rule with `( ` embedded
    // (func_open shape). Body positions carry the args.
    if let IrNode::Ref(_) = head {
        return !body_positions.is_empty();
    }
    false
}

/// Return true when `rid` refers to an identifier-like rule OR a
/// `name , "("` Seq rule (Sheets `func_open`).
fn is_identifier_or_func_open_ref(rid: RuleId, ir: &GrammarIR) -> bool {
    let Some(rule) = ir.rules.iter().find(|r| r.id == rid) else {
        return false;
    };
    let body = unwrap_map_ow(&rule.body);

    // Plain identifier-regex-like rule. We admit any regex body here —
    // specific regex classification is the HRegex detector's job; from
    // the ArgList perspective any lead-in that resolves to a regex-
    // shaped token is an admissible name head.
    if matches!(body, IrNode::Regex(_)) {
        return true;
    }

    // Sheets `func_open = identifier , "("` — Seq with exactly two
    // positions: a Ref (to identifier) + a "(" literal.
    let mut positions: Vec<&IrNode> = Vec::new();
    flatten_seq(body, &mut positions);
    if positions.len() >= 2 {
        let last = unwrap_map_ow(positions[positions.len() - 1]);
        if single_byte_literal(last, ir) == Some(b'(') {
            // The first position must resolve (through a Ref) to a
            // regex-like identifier.
            let first = unwrap_map_ow(positions[0]);
            return matches!(first, IrNode::Regex(_))
                || matches!(first, IrNode::Ref(_));
        }
    }
    false
}
