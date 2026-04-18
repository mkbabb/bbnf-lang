//! Flat-shape detector — typed `Seq` fallthrough for rules without a
//! more-specific shape.
//!
//! # Predicate
//!
//! A rule is Flat-shaped when its body resolves to a sequential
//! composition (`Seq` or `Next / Skip` chain) of two or more typed
//! positions, no structural-opening delimiter (`(`, `[`, `{`, `"`,
//! `'`) at the head, and no operator-chain / disjoint-FIRST-Alt /
//! `name(args)` shape claim. Flat is the specificity fallback for
//! typed `Seq` bodies that earlier detectors reject.
//!
//! Admitted heads:
//!
//! - `Literal` — classical literal-led Seq (`"display"`, `"@import"`).
//! - `Alt` of `Literal` branches — short keyword Alt head (CSS
//!   `overflowDecl`'s `"overflow-x" | "overflow-y" | "overflow"`).
//! - `Ref` to any rule — includes:
//!   - `Ref` to a Keyword-shaped keyword-set rule (CSS `colorDecl`'s
//!     `Ref(colorProps)`, where `colorProps` is itself a typed literal
//!     Alt).
//!   - `Ref` to a Number / HRegex / String regex rule (typed
//!     dimension rules — CSS `length = number , lengthUnit`).
//!   - `Ref` to a Flat / Wrap / Pratt / Unordered / ArgList rule
//!     (structural scaffolding — CSS `qualifiedRule =
//!     selectorList , ruleBlock`).
//! - `Regex` — regex-head rules (CSS `customPropertyDecl =
//!   /--[\w-]+/, ":", …`, `genericDecl`).
//!
//! # Canonical sources
//!
//! - CSS 28 `*Decl` rules per `grammar/css/l4/properties.bbnf:161-197`
//!   — Ref-headed (`colorDecl = colorProps, ":"` …) and literal-headed
//!   (`displayDecl = "display" , ":"` …) alike.
//! - CSS typed dimensions — `length = number , lengthUnit`,
//!   `angle = number , angleUnit`, etc. per
//!   `grammar/css/l4/value-unit.bbnf`.
//! - CSS rule scaffolding — `qualifiedRule = selectorList, ruleBlock`,
//!   `mediaRule = "@media", mediaQueryList, ruleBlock`,
//!   `keyframesRule = "@keyframes", identifier, "{", …`.
//! - CSS selector scaffolding — `selectorList = complexSelector,
//!   (","?w, complexSelector)*`, `wqName`, `nsPrefix`.
//! - CSS attribute / functional selectors — `attrSelector`.
//! - CSS custom property / generic declarations —
//!   `customPropertyDecl`, `genericDecl`.
//! - BBNF `rule = lhs, "=" ?w, rhs ?w, ( ";" | "." )` per
//!   `grammar/bbnf/bbnf.bbnf:56`.
//! - BBNF 7 `*_directive` rules — literal head + refs + terminator.
//!
//! # Structural variants
//!
//! Two alternate structures also classify as Flat when they match the
//! H1 audit §A.2 catalogue:
//!
//! - `Repeat { lo = 0, hi = 1, inner = Seq(…) }` — an optional
//!   typed Seq. The CSS `importantSuffix = ("!" ?w, "important") ?`
//!   lowering produces this shape. Treated as Flat since the inner
//!   Seq carries the typed structure.
//!
//! # Exclusion guards
//!
//! Flat rejects:
//!
//! - Structural-wrap bodies (`Skip(Next(open, middle), close)`) —
//!   those are Object / Array / Wrap territory.
//! - Single-position bodies (covered by Scalar / Keyword / String /
//!   HRegex / Number).
//! - Alt-rooted bodies — those are Keyword / Wrap.
//! - Repeat-rooted bodies EXCEPT the optional-Seq (`lo=0, hi=1`)
//!   special case — multi-iter Repeats are Unordered / Array.
//! - Bodies whose first structural byte is `(`, `[`, `{`, `"`, `'`
//!   — those are ArgList / Wrap / Object / String.
//!
//! # Projection
//!
//! Pure structural inspection of the rule body. No new mining. The
//! dispatch precedence in [`super`] guarantees Object / Array /
//! String / Number / Keyword / HRegex / Pratt / Unordered / ArgList
//! have already rejected the rule before Flat fires; the detector
//! therefore admits any *remaining* typed Seq body.

use crate::passes::inspect::{single_byte_literal, unwrap_map_ow, unwrap_wrap};
use crate::types::{GrammarIR, IrNode, RuleId};

/// Detect Flat-shape: typed Seq (or optional typed Seq) with admissible
/// head.
pub fn detect_flat(rule_id: RuleId, ir: &GrammarIR) -> bool {
    let rule = &ir.rules[rule_id as usize];
    let body = unwrap_map_ow(&rule.body);
    classify_flat(body, ir)
}

/// Return true when `node` matches the Flat-shape predicate.
fn classify_flat(node: &IrNode, ir: &GrammarIR) -> bool {
    // Reject leaf shapes — single Literal / Regex / Ref bodies are
    // handled by Scalar / HRegex / Keyword / the wrapper chain.
    match node {
        IrNode::Alt(_, _) => return false,
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Ref(_) => return false,
        IrNode::Epsilon => return false,
        _ => {}
    }

    // Reject Wrap-shape first: a `Wrap(open, body, close)` at the
    // outer level is Object / Array / Wrap territory.
    if unwrap_wrap(node).is_some() {
        return false;
    }

    // Special-case: `Repeat { lo = 0, hi = 1, inner = Seq(...) }` —
    // an optional typed Seq. Lowered from BBNF's `(…) ?` postfix
    // over a Seq body (CSS `importantSuffix = ("!" ?w, "important") ?`).
    // Delegate to the inner Seq's flat predicate.
    if let IrNode::Repeat { inner, lo, hi } = node {
        if *lo == 0 && *hi == 1 {
            let inner_body = unwrap_map_ow(inner);
            // Only admit when the inner is a multi-position Seq —
            // `Repeat(0, 1, Literal(...))` is a degenerate case better
            // classified as Scalar.
            if matches!(inner_body, IrNode::Seq(_)) {
                return classify_flat(inner_body, ir);
            }
        }
        return false;
    }

    // Flatten the Seq / Next / Skip chain.
    let mut positions: Vec<&IrNode> = Vec::new();
    flatten_seq(node, &mut positions);
    if positions.len() < 2 {
        return false;
    }

    // Head must not be a structural-opening delimiter — those would
    // have matched Object / Array / ArgList / Wrap / String earlier.
    let head = unwrap_map_ow(positions[0]);
    if let Some(byte) = single_byte_literal(head, ir) {
        if matches!(byte, b'(' | b'[' | b'{' | b'"' | b'\'') {
            return false;
        }
    }

    // Head must be a shape-admissible leaf or Ref — no bare Repeat /
    // Alt heads (those would suggest Unordered / Wrap).
    if !head_is_admissible(head, ir) {
        return false;
    }

    // At least one typed body position must follow the head.
    positions[1..].iter().any(|pos| {
        let inner = unwrap_map_ow(pos);
        !matches!(inner, IrNode::Epsilon)
    })
}

/// Flatten Next / Skip chains into a positional list. Strips
/// OptionalWhitespace / Map trivia and Epsilon nodes.
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

/// Return true when `node` is an admissible Flat head — any node that
/// starts a typed structural sequence.
///
/// Literal / Regex / Ref / Alt-of-Literal / Repeat-of-optional heads
/// pass. Bare `Alt` / `Repeat { lo > 0 }` heads do not — those would
/// have been claimed by Wrap / Unordered / Keyword detectors earlier.
fn head_is_admissible(node: &IrNode, ir: &GrammarIR) -> bool {
    let _ = ir;
    match unwrap_map_ow(node) {
        IrNode::Literal(_) => true,
        IrNode::Regex(_) => true,
        IrNode::Ref(_) => true,
        IrNode::Alt(branches, _) => {
            // A head that is itself an Alt only passes when every branch
            // is literal-led (classical keyword-set head). Mixed or
            // Ref-containing Alts suggest Wrap / decision-point shapes.
            branches
                .iter()
                .all(|b| matches!(unwrap_map_ow(&b.node), IrNode::Literal(_)))
        }
        // `Repeat { lo = 0, hi = 1, ... }` as head position — CSS
        // `mediaQuery = (mediaQualifier)?, (mediaType)?, …`. The
        // optional head is admissible when its inner is a Ref /
        // Literal leaf.
        IrNode::Repeat { inner, lo, hi } => {
            if *lo == 0 && *hi == 1 {
                matches!(
                    unwrap_map_ow(inner),
                    IrNode::Ref(_) | IrNode::Literal(_) | IrNode::Regex(_)
                )
            } else {
                false
            }
        }
        _ => false,
    }
}
