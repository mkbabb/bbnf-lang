//! Modular heuristic rules for auto-inferring `@pretty` hints.
//!
//! When a rule has no explicit `@pretty` directive, the heuristic pipeline
//! examines its shape (expression structure + inferred type) and produces
//! formatting hints automatically.
//!
//! ## Design
//!
//! - Each heuristic is a standalone function returning `Option<Vec<String>>`.
//! - The pipeline chains heuristics with first-match-wins semantics.
//! - Explicit `@pretty` directives ALWAYS win — heuristics only fire for
//!   un-annotated rules.
//! - A grammar-level `@pretty * <mode>` meta-directive controls the mode.

use crate::types::Expression;

use super::super::type_inference::type_is_span;
use super::super::types::GeneratedGrammarAttributes;
use super::prettify_utils::{is_vec_type, is_box_enum_type};

/// Heuristic mode for auto-inferring `@pretty` hints.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HeuristicMode {
    /// No heuristics at all — raw codegen (backward compat).
    Off,
    /// Only apply wrapped-pattern fix (Step 1.3).
    Minimal,
    /// Full structural inference (default).
    Auto,
}

impl HeuristicMode {
    /// Parse a mode string from a `@pretty * <mode>` directive.
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "off" => Some(Self::Off),
            "minimal" => Some(Self::Minimal),
            "auto" => Some(Self::Auto),
            _ => None,
        }
    }
}

/// Context passed to each heuristic rule.
pub struct HeuristicContext<'a, 'b> {
    pub rule_name: &'a str,
    pub expr: &'b Expression<'b>,
    pub ty: &'a syn::Type,
    pub grammar_attrs: &'a GeneratedGrammarAttributes<'b>,
}

/// A single heuristic rule: a named predicate + hint generator.
pub struct HeuristicRule {
    pub name: &'static str,
    pub apply: fn(&HeuristicContext) -> Option<Vec<String>>,
}

/// The ordered pipeline of heuristic rules. First match wins.
pub fn default_rules() -> Vec<HeuristicRule> {
    vec![
        HeuristicRule {
            name: "detect_toplevel",
            apply: detect_toplevel,
        },
        HeuristicRule {
            name: "detect_block_delimited",
            apply: detect_block_delimited,
        },
        HeuristicRule {
            name: "detect_large_compound",
            apply: detect_large_compound,
        },
    ]
}

/// Top-level entry point: resolve hints for a rule with no explicit `@pretty`.
pub fn infer_hints(ctx: &HeuristicContext, mode: HeuristicMode) -> Vec<String> {
    match mode {
        HeuristicMode::Off => Vec::new(),
        HeuristicMode::Minimal => Vec::new(),
        HeuristicMode::Auto => {
            for rule in &default_rules() {
                if let Some(hints) = (rule.apply)(ctx) {
                    return hints;
                }
            }
            Vec::new()
        }
    }
}

/// Resolve the heuristic mode from the `@pretty` directives map.
///
/// Looks for a special `@pretty * <mode>` entry (key `"*"`).
/// Returns `Auto` when no `@pretty *` is present.
pub fn resolve_mode(grammar_attrs: &GeneratedGrammarAttributes) -> HeuristicMode {
    grammar_attrs
        .pretties
        .and_then(|p| p.get("*"))
        .and_then(|hints| hints.first())
        .and_then(|mode_str| HeuristicMode::from_str(mode_str))
        .unwrap_or(HeuristicMode::Auto)
}

// ---------------------------------------------------------------------------
// Individual heuristic functions
// ---------------------------------------------------------------------------

/// Detect top-level / entry-point rules.
///
/// Matches by name (`grammar`, `program`, `stylesheet`, `module`, `document`,
/// `file`) or by shape (Vec of nonterminals at the root level).
fn detect_toplevel(ctx: &HeuristicContext) -> Option<Vec<String>> {
    const TOPLEVEL_NAMES: &[&str] = &[
        "grammar",
        "program",
        "stylesheet",
        "module",
        "document",
        "file",
        "root",
    ];

    if TOPLEVEL_NAMES.contains(&ctx.rule_name) {
        return Some(vec!["block".to_string()]);
    }

    // Shape-based: a Vec of nonterminals (repetition of rules) at the root.
    if is_vec_type(ctx.ty) {
        // Check if the inner expression is a repetition of nonterminals.
        if is_nonterminal_repetition(ctx.expr) {
            return Some(vec!["block".to_string()]);
        }
    }

    None
}

/// Detect block-delimited rules: rules containing `"{" >> body << "}"` with
/// a nonterminal body.
fn detect_block_delimited(ctx: &HeuristicContext) -> Option<Vec<String>> {
    if contains_brace_wrapped(ctx.expr) {
        return Some(vec!["group".to_string(), "indent".to_string()]);
    }
    None
}

/// Detect large compound types: tuple types with >3 elements.
fn detect_large_compound(ctx: &HeuristicContext) -> Option<Vec<String>> {
    if let syn::Type::Tuple(tuple) = ctx.ty {
        if tuple.elems.len() > 3 {
            return Some(vec!["group".to_string()]);
        }
    }
    // Box<Enum> types (alternation roots) also benefit from grouping.
    if is_box_enum_type(ctx.ty) && !type_is_span(ctx.ty) {
        // Only if the expression is an alternation with many branches.
        if let Expression::Alternation(token) = ctx.expr {
            if token.value.len() > 2 {
                return Some(vec!["group".to_string()]);
            }
        }
    }
    None
}

// ---------------------------------------------------------------------------
// Helper predicates
// ---------------------------------------------------------------------------

/// Check if an expression is a repetition (Many/Many1) of nonterminals.
fn is_nonterminal_repetition(expr: &Expression) -> bool {
    match expr {
        Expression::Many(inner) | Expression::Many1(inner) => {
            is_or_contains_nonterminal(&inner.value)
        }
        Expression::Rule(inner, _) => is_nonterminal_repetition(inner),
        Expression::OptionalWhitespace(inner) | Expression::Group(inner) => {
            is_nonterminal_repetition(&inner.value)
        }
        _ => false,
    }
}

/// Check if an expression is or directly contains a Nonterminal.
fn is_or_contains_nonterminal(expr: &Expression) -> bool {
    match expr {
        Expression::Nonterminal(_) => true,
        Expression::OptionalWhitespace(inner) | Expression::Group(inner) => {
            is_or_contains_nonterminal(&inner.value)
        }
        Expression::Concatenation(token) => {
            token.value.iter().any(is_or_contains_nonterminal)
        }
        _ => false,
    }
}

/// Check if an expression contains a brace-wrapped pattern `"{" >> ... << "}"`.
fn contains_brace_wrapped(expr: &Expression) -> bool {
    match expr {
        Expression::Skip(left_token, right_token) => {
            let left = unwrap_ws(&left_token.value);
            let right = unwrap_ws(&right_token.value);
            if let Expression::Next(next_left, _) = left {
                let next_left_inner = unwrap_ws(&next_left.value);
                if let Expression::Literal(l) = next_left_inner {
                    if let Expression::Literal(r) = right {
                        return l.value.as_ref() == "{" && r.value.as_ref() == "}";
                    }
                }
            }
            false
        }
        Expression::Concatenation(token) => {
            token.value.iter().any(contains_brace_wrapped)
        }
        Expression::Rule(inner, _) => contains_brace_wrapped(inner),
        Expression::OptionalWhitespace(inner) | Expression::Group(inner) => {
            contains_brace_wrapped(&inner.value)
        }
        _ => false,
    }
}

/// Unwrap whitespace/group wrappers (local version to avoid circular dep).
fn unwrap_ws<'a>(expr: &'a Expression<'a>) -> &'a Expression<'a> {
    match expr {
        Expression::OptionalWhitespace(inner) => unwrap_ws(&inner.value),
        Expression::Group(inner) => unwrap_ws(&inner.value),
        other => other,
    }
}
