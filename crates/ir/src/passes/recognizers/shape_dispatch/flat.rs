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
//! - Structural-wrap bodies — left to Object / Array detectors which
//!   check for pair-shaped or homogeneous-repeat middles. Flat admits
//!   every other `Wrap(open, body, close)` shape by treating the
//!   open / middle / close bytes as three sequential positions.
//! - Single-position bodies that reduce to a plain leaf — covered by
//!   Scalar / Keyword / String / HRegex / Number.
//! - Alt-rooted bodies — those are Keyword / Wrap / AltDispatch.
//!
//! Post AX.W0a.2.b, Flat admits broader Seq shapes:
//!
//! - **Repeat-rooted bodies** where the body is a single `Repeat` —
//!   covers BBNF / EBNF / BNF `alternation = (concat "|"?) +` and
//!   other Kleene-plus bodies. The Flat emitter's generic Repeat
//!   branch iterates over the inner.
//! - **Delimiter-opening heads** (`(`, `[`, `{`, `"`, `'`). Earlier
//!   detectors (Object / Array / ArgList / Wrap / String) run first
//!   and claim their narrow shapes; anything that reaches Flat with
//!   such a head is a structural Seq the earlier detectors rejected
//!   (CSS `attrSelector = "[" ?w, …, "]"`, `mediaFeature = "(" ?w,
//!   …, ")"`, `ruleBlock = "{" >> … << "}"`, BBNF `import_items`,
//!   `import_path`, BNF `nonterminal = "<" … ">"`).
//!
//! # Projection
//!
//! Pure structural inspection of the rule body. No new mining. The
//! dispatch precedence in [`super`] guarantees Object / Array /
//! String / Number / Keyword / HRegex / Pratt / Unordered / ArgList
//! have already rejected the rule before Flat fires; the detector
//! therefore admits any *remaining* typed Seq body.

use crate::passes::inspect::unwrap_map_ow;
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

    // AX.W0a.2.b: admit Repeat-rooted bodies (any lo / hi). The Flat
    // emitter's generic Repeat branch iterates the inner greedily.
    //
    // Canonical sources:
    //   - BBNF `alternation = ( concat ?w , "|" ? ) +` —
    //     `Repeat(1, MAX, Seq(..))`.
    //   - EBNF `alternation = ( S , concat , S , "|" ? ) +`.
    //   - BNF `alternation = expr , ( /\s*/ , "|" , /\s*/ , expr )*` —
    //     still Seq-rooted; this is handled by the Seq path below.
    //   - CSS `importantSuffix = ("!" ?w, "important") ?` —
    //     `Repeat(0, 1, Seq(..))`.
    //
    // The admission constraint: the inner must be a non-trivial
    // structural body (Seq / Next / Skip / Ref / Literal / Regex /
    // Alt / nested Repeat / Map-wrap thereof). A plain `Epsilon`
    // inner would be a degenerate `()` body; reject.
    if let IrNode::Repeat { inner, .. } = node {
        let inner_body = unwrap_map_ow(inner);
        return !matches!(inner_body, IrNode::Epsilon);
    }

    // Flatten the Seq / Next / Skip chain.
    let mut positions: Vec<&IrNode> = Vec::new();
    flatten_seq(node, &mut positions);
    if positions.len() < 2 {
        return false;
    }

    // Head must be a shape-admissible leaf or Ref — no bare Alt heads
    // (those would suggest Wrap / AltDispatch). Repeat heads are
    // admitted per AX.W0a.2.b for rules like
    // `unary_expr = unary_prefix * , postfix_expr`.
    let head = unwrap_map_ow(positions[0]);
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
/// Literal / Regex / Ref / Alt-of-Literal / Repeat heads pass. Bare
/// `Alt` with non-literal branches does not — those would have been
/// claimed by Wrap / AltDispatch / Keyword detectors earlier.
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
        // AX.W0a.2.b: admit any `Repeat` head whose inner is a
        // Literal / Ref / Regex leaf — covers Sheets
        // `unary_expr = unary_prefix * , postfix_expr`
        // (Repeat(0, MAX) head) as well as the older optional-head
        // case (CSS `mediaQuery = (mediaQualifier)?, (mediaType)?,
        // …`).
        IrNode::Repeat { inner, .. } => {
            matches!(
                unwrap_map_ow(inner),
                IrNode::Ref(_) | IrNode::Literal(_) | IrNode::Regex(_)
            )
        }
        _ => false,
    }
}
