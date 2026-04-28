//! AST helper functions over the struct-direct [`BbnfView`] surface.
//!
//! AZ-II.cutover.D4 — every analysis-side ast walker migrates from
//! the tape-cursor `BbnfBootstrapNodeView` to the struct-direct
//! [`bbnf::runtime::bbnf::BbnfView`] (a [`bbnf::runtime::RuntimeView`]
//! impl). Pure functions consumed by `state/diagnostics.rs` and
//! `features/*.rs`.
//!
//! # Discriminator translation
//!
//! Pre-cutover.D the helpers dispatched on `BbnfBootstrapRuleKind`,
//! whose alphabet included sub-variant tokens (`term_0`, `term_1`,
//! `term_2`) emitted by the tape-first generator. The struct-direct
//! `BbnfCompoundKind` alphabet collapses every Alt sub-variant onto
//! the parent `term` arm and exposes the branch index via
//! [`BbnfView::branch_tag`]. The translation:
//!
//! - `term_0` (epsilon)        → `BbnfCompoundKind::Term` +
//!   `branch_tag == Some(0)`.
//! - `term_1` (ident + call)   → `BbnfCompoundKind::Term` +
//!   `branch_tag == Some(1)`.
//! - `term_2` (grouped expr)   → `BbnfCompoundKind::Term` +
//!   `branch_tag in 4..=7`.
//! - `value_atom_0` (grouped value-expr) — preserved when the
//!   analysis-layer call sites carry it; today every consumer that
//!   matches `term_2 | value_atom_0` also accepts the grouped Term
//!   branches, so the unified predicate is `is_grouped_term`.
//!
//! Anonymous-wrapper compounds (the tape-first generator's
//! `int_lit`-collision wrappers around quantified groups) do not
//! exist in the struct-direct world: every compound entry resolves
//! through [`BbnfArena::compound`] to an explicit
//! [`BbnfCompoundKind`] arm, including [`BbnfCompoundKind::Other`]
//! for rules outside the alphabet. The analysis helpers therefore
//! drop the tape-only `Unknown / int_lit` peeling branches.

use bbnf::runtime::RuntimeView;
use bbnf::runtime::bbnf::{BbnfCompoundKind, BbnfView};

pub mod cycles;
pub mod format;
pub mod references;
pub mod spans;
pub mod tokens;

pub use cycles::{build_cycle_path, compute_reachable_rules};
pub use format::{format_expression_short, format_value_expr_short};
pub use references::collect_references;
pub use spans::{compute_expression_end, compute_expression_end_pub};
pub use tokens::collect_semantic_tokens;

/// Format a byte as a display-friendly character for inlay hints.
pub fn format_char(b: u8) -> String {
    match b {
        b'\t' => "\\t".into(),
        b'\n' => "\\n".into(),
        b'\r' => "\\r".into(),
        b' ' => "SP".into(),
        0x0b => "\\v".into(),
        0x0c => "\\f".into(),
        c if c.is_ascii_graphic() => format!("'{}'", c as char),
        c => format!("0x{:02x}", c),
    }
}

/// Check if a rule RHS is effectively empty (epsilon only).
///
/// Walks the [`BbnfView`] structure, peeling transparent wrappers
/// (`Term`, `Rhs`, `GrammarItem`, `Directive`, `Lhs` — the
/// single-child compounds whose body is a pure indirection) to their
/// leaf, and returns `true` when the leaf is the `Term` epsilon
/// branch (`branch_tag == Some(0)`).
pub fn is_empty_rhs(node: BbnfView<'_, '_>) -> bool {
    match node.compound_kind() {
        // Epsilon term branch.
        Some(BbnfCompoundKind::Term) if node.branch_tag() == Some(0) => true,

        // Transparent wrappers — descend into the single inner child.
        Some(BbnfCompoundKind::Term)
        | Some(BbnfCompoundKind::Rhs)
        | Some(BbnfCompoundKind::GrammarItem)
        | Some(BbnfCompoundKind::Directive)
        | Some(BbnfCompoundKind::Lhs) => node.child(0).map(is_empty_rhs).unwrap_or(false),

        // Factor: epsilon iff the inner term is epsilon AND there's
        // no modifier child.
        Some(BbnfCompoundKind::Factor) => {
            let term = node
                .children()
                .find(|c| c.compound_kind() == Some(BbnfCompoundKind::Term));
            // The `modifier` rule lowers to a Span leaf in the BBNF
            // struct-direct runtime (regex-only single-token rule).
            // Modifier presence is signalled by a non-empty Span leaf
            // in the factor's children outside the Term position.
            let modifier_present = node.children().any(|c| {
                if c.compound_kind().is_some() {
                    return false;
                }
                // Span / leaf with non-empty source slice → modifier
                // (the only span-emitting non-compound child of
                // factor is the modifier).
                c.span_text().is_some_and(|t| !t.trim().is_empty())
            });
            match (term, modifier_present) {
                (Some(t), false) => is_empty_rhs(t),
                _ => false,
            }
        }

        // Mapped factor: epsilon iff inner is epsilon AND there's no
        // `->` mapping. Mapping presence is signalled by the inner
        // factor being followed by a value_expr child (the
        // `mapped_factor = factor , ( "->" ?w , value_expr ,
        // type_annotation ? ) ?` shape carries the optional mapping
        // as additional children when present).
        Some(BbnfCompoundKind::MappedFactor) => {
            let inner = match node.child(0) {
                Some(c) => c,
                None => return false,
            };
            // Heuristic: any non-Factor compound child indicates a
            // mapping is attached.
            let mapping_present = node.children().skip(1).any(|c| {
                c.compound_kind().is_some_and(|k| k != BbnfCompoundKind::Factor)
            });
            if mapping_present {
                false
            } else {
                is_empty_rhs(inner)
            }
        }

        // Binary factor: epsilon iff single operand and that operand
        // is epsilon.
        Some(BbnfCompoundKind::BinaryFactor) => {
            let operands: Vec<_> = collect_binary_operand_views(node).collect();
            operands.len() == 1 && is_empty_rhs(operands[0])
        }

        // Concatenation / alternation: epsilon iff exactly one branch
        // and that branch is epsilon.
        Some(BbnfCompoundKind::Concatenation) | Some(BbnfCompoundKind::Alternation) => {
            let parts: Vec<_> = iter_iteration_views(node).collect();
            parts.len() == 1 && is_empty_rhs(parts[0])
        }

        _ => false,
    }
}

/// True iff the focused view participates in the `term` family —
/// either the polymorphic `Term` compound or its leaf payloads
/// (`identifier`, `literal`, `regex`). Mirrors the pre-cutover.D
/// `is_term_kind` predicate over `BbnfBootstrapRuleKind`.
pub(crate) fn is_term_kind(node: BbnfView<'_, '_>) -> bool {
    match node.compound_kind() {
        Some(BbnfCompoundKind::Term) => true,
        // value_atom (grouped value-expr) lowers to its own compound
        // alphabet entry; analysis treats it as a structural peer of
        // Term for grouped-expression handling.
        _ => {
            // Bare leaf identifiers / literals / regex carry no
            // compound kind but the analysis layer treats them as
            // term-family for descent purposes.
            !node.is_compound() && node.is_span()
        }
    }
}

/// True iff the focused compound is a grouped-expression term
/// (`(rhs)` / `[rhs]` / `{rhs}` / `@{rhs}`) — encodes the analysis
/// layer's `term_2 | value_atom_0` discrimination over the
/// struct-direct alphabet.
#[inline]
pub(crate) fn is_grouped_term(node: BbnfView<'_, '_>) -> bool {
    match node.compound_kind() {
        Some(BbnfCompoundKind::Term) => matches!(node.branch_tag(), Some(4..=7)),
        _ => false,
    }
}

/// Collect binary-factor operand views.
///
/// `binary_factor = mapped_factor , ( binary_operators ?w ,
/// mapped_factor ) *` lowers to a Compound whose first child is the
/// leading mapped_factor and whose remaining children are operator /
/// operand pairs (with the optional `*` repeat collapsed inline).
/// The analysis layer needs every operand `mapped_factor` view; this
/// helper extracts them in source order.
pub(crate) fn collect_binary_operand_views<'a, 'p>(
    node: BbnfView<'a, 'p>,
) -> impl Iterator<Item = BbnfView<'a, 'p>> {
    let kept: Vec<BbnfView<'a, 'p>> = node
        .children()
        .filter(|c| {
            // Operands are mapped_factor compounds; skip the
            // binary_operators leaf compounds and any anonymous
            // wrapper shapes the lowering may have introduced.
            matches!(
                c.compound_kind(),
                Some(BbnfCompoundKind::MappedFactor)
                    | Some(BbnfCompoundKind::BinaryFactor)
                    | Some(BbnfCompoundKind::Factor)
            )
        })
        .collect();
    kept.into_iter()
}

/// Iterate the operand views of an iteration-pair compound.
///
/// `concatenation = ( binary_factor ?w , "," ? ) +` and
/// `alternation = ( concatenation ?w , "|" ? ) +` lower to a
/// Compound whose direct children are the iteration bodies. Each
/// body wraps its content; this helper unwraps to the substantive
/// inner expression and drops separator literals (the `,` and `|`
/// tokens that ride alongside the body in the source order).
pub(crate) fn iter_iteration_views<'a, 'p>(
    node: BbnfView<'a, 'p>,
) -> impl Iterator<Item = BbnfView<'a, 'p>> {
    let kept: Vec<BbnfView<'a, 'p>> = node
        .children()
        .filter_map(|child| {
            // Drop separator-only / whitespace-only Span children —
            // the analysis layer is interested in the substantive
            // operand compounds, not the delimiters.
            if !child.is_compound() {
                let text = child.span_text().unwrap_or("").trim();
                if text.is_empty() || text == "|" || text == "," {
                    return None;
                }
                return Some(child);
            }
            Some(child)
        })
        .collect();
    kept.into_iter()
}
