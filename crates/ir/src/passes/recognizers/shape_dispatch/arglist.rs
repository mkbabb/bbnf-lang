//! ArgList-shape detector — `name(arg, arg, ...)` positional call.
//!
//! # Predicate
//!
//! A rule is ArgList-shaped when its body unwraps to a Seq whose
//! structural positions are:
//!
//! 1. A **name head** — either a `Literal` (`"calc"`, `"rgb"`,
//!    `"url"`), a `Regex` (Sheets `LET(` / `LAMBDA(` case-insensitive
//!    regex-with-embedded-`(`), OR a `Ref` to any rule (Sheets
//!    `func_open = identifier , "("`, BBNF `value_path = value_ident ,
//!    ("::" , value_ident)*`).
//! 2. A **`(` open literal** — either as a sibling Seq position OR
//!    folded into the regex/ref head.
//! 3. A **positional arg repeat** — `Repeat(arg)` (possibly with
//!    `Skip(arg, sep?)` separator structure).
//! 4. A **`)` close literal** — the last non-trivia Seq position.
//!
//! # Canonical sources
//!
//! - CSS `calcFunction = "calc" , "(" >> mathExpr << ")"` per
//!   `grammar/css/l4/values.bbnf` — literal head + explicit `(` / `)`.
//! - CSS `rgbFunction` / `hsl` / `colorFunction` family per
//!   `grammar/css/l4/color.bbnf`.
//! - CSS `transforms.bbnf` / `filters.bbnf` per-transform /
//!   per-filter functions (`translate(x, y)`, `blur(r)`, etc.).
//! - Sheets `func_call = func_open , (func_args ?) ?w , ")"` where
//!   `func_open = identifier , "("` per
//!   `grammar/google-sheets/google-sheets.bbnf:139-143`.
//! - Sheets `let_call = /[lL][eE][tT]\(/ , let_args ?w , ")"`
//!   per `grammar/google-sheets/google-sheets.bbnf:148` — regex head
//!   with embedded `(`.
//! - Sheets `lambda_call` per
//!   `grammar/google-sheets/google-sheets.bbnf:152` — same shape.
//! - BBNF `value_fn_call = value_path , "(" , ( value_expr , ( "," ?w
//!   , value_expr ) * ) ? , ")"` — Ref head (to identifier path),
//!   explicit `(` / `)`.
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
/// Admits two head variants:
///
/// - **Open-paren-embedded head** — Regex head with `\(` pattern tail,
///   OR Ref head whose target rule already includes `"("` (Sheets
///   `func_open = identifier , "("`). The `"("` is consumed by the
///   head; body positions carry only the args + closing `")"`.
/// - **Separate-paren head** — Literal / Regex / Ref head followed by
///   an explicit `"("` body position. This matches both the classical
///   `"calc" , "(" >> body << ")"` CSS shape and the BBNF
///   `value_fn_call = value_path , "(" , args , ")"` shape.
fn classify_arglist(node: &IrNode, ir: &GrammarIR) -> bool {
    // The Seq children list — after stripping Next / Skip wrappers —
    // must be a minimum of two positions: a head and a closing `)`.
    let mut positions: Vec<&IrNode> = Vec::new();
    flatten_seq(node, &mut positions);
    if positions.len() < 2 {
        return false;
    }

    // First position: name head — Literal, Regex, or Ref.
    let head = unwrap_map_ow(positions[0]);
    let head_admitted = matches!(head, IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Ref(_),);
    if !head_admitted {
        return false;
    }

    // Last position: closing `)` literal.
    let close = unwrap_map_ow(positions[positions.len() - 1]);
    if single_byte_literal(close, ir) != Some(b')') {
        return false;
    }

    // Positions between head and close — the args.
    let body_positions = &positions[1..positions.len() - 1];

    // Case A: open-paren-embedded head. The head consumes the `(`;
    // body positions are the raw args. Valid when the head is either
    // a Regex with `\(` tail OR a Ref whose target ends with `"("`.
    if head_consumes_open_paren(head, ir) {
        // Must have at least one arg body position; else the `()` pair
        // would have no args (admit anyway for nullary calls).
        return true;
    }

    // Case B: separate-paren head. Body[0] must be `"("`; args fill
    // the remaining positions between `(` and `)`.
    if body_positions.is_empty() {
        return false;
    }
    let first_body = unwrap_map_ow(body_positions[0]);
    if single_byte_literal(first_body, ir) != Some(b'(') {
        return false;
    }
    // Need at least one more position after `(` before `)` — i.e.
    // `name , "(" , args , ")"`. Empty-arg calls `name()` are admitted
    // when body_positions is exactly one `(`.
    true
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

/// Return true when `head` already consumes the `(` — either a Regex
/// with `\(` tail OR a Ref whose target's last structural position is
/// `"("` (Sheets `func_open = identifier , "("`).
fn head_consumes_open_paren(head: &IrNode, ir: &GrammarIR) -> bool {
    match head {
        IrNode::Regex(sid) => {
            // Check if the regex pattern ends with the literal `\(`
            // escape. Conservative detection: look for `\(` at or near
            // the pattern tail.
            let pattern = ir.get_string(*sid);
            pattern.ends_with(r"\(") || pattern.contains(r"\(")
        }
        IrNode::Ref(rid) => head_ref_ends_with_open_paren(*rid, ir),
        _ => false,
    }
}

/// Return true when `rid`'s target rule body ends with a `"("`
/// literal as its last structural position. Walks Seq / Next / Skip
/// chains. Recursion-bounded: unwraps one Ref hop.
fn head_ref_ends_with_open_paren(rid: RuleId, ir: &GrammarIR) -> bool {
    let Some(rule) = ir.rules.iter().find(|r| r.id == rid) else {
        return false;
    };
    let body = unwrap_map_ow(&rule.body);
    let mut positions: Vec<&IrNode> = Vec::new();
    flatten_seq(body, &mut positions);
    let Some(last) = positions.last() else {
        return false;
    };
    single_byte_literal(unwrap_map_ow(last), ir) == Some(b'(')
}
