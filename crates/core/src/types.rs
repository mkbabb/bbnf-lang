use std::borrow::Cow;

use parse_that::Span;

use indexmap::IndexMap;

use crate::grammar::generated::BbnfBootstrapNodeView;

// ─── Grammar AST ─────────────────────────────────────────────────────────────

/// A single rule entry: name span + reference to the RHS in the bootstrap AST.
///
/// Tranche AC.2 — under the tape-first generated parser, the RHS is no
/// longer a borrowed `&'a BbnfBootstrapEnum<'a>`; it's a
/// [`BbnfBootstrapNodeView`], a cursor-backed view over the finished
/// tape that carries both a `TapeCursor<'a>` and a `&'a str` slice of
/// the owning source buffer. Walking the RHS is identical in shape to
/// the old enum (iterate children, dispatch on rule kind), but every
/// access goes through view methods instead of pattern matching.
#[derive(Debug, Clone, Copy)]
pub struct RuleEntry<'a> {
    /// Span of the rule's LHS identifier.
    pub name_span: Span<'a>,
    /// The RHS expression — a view over the bootstrap parse tape.
    pub rhs: BbnfBootstrapNodeView<'a>,
}

/// Grammar rules: rule name → RHS entry. Insertion-ordered to preserve source order.
pub type AST<'a> = IndexMap<&'a str, RuleEntry<'a>>;

// ─── Directives ──────────────────────────────────────────────────────────────

/// A single imported name in a selective `@import { a, b } from "path"` directive.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportedName<'a> {
    pub name: Cow<'a, str>,
    pub span: Span<'a>,
}

/// An `@import` directive at the top of a grammar file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportDirective<'a> {
    /// The path string from the import (relative to importing file).
    pub path: Cow<'a, str>,
    /// The byte-offset span of the entire import directive.
    pub span: Span<'a>,
    /// If `Some`, selective import: only these rule names are imported.
    /// If `None`, glob import: all rules are imported.
    pub items: Option<Vec<ImportedName<'a>>>,
}

/// An `@recover` directive that annotates a rule with error recovery.
#[derive(Debug, Clone, Copy)]
pub struct RecoverDirective<'a> {
    /// The name of the rule to wrap with recovery.
    pub rule_name: &'a str,
    /// The sync expression — a view into the bootstrap parse tape.
    pub sync_expr: BbnfBootstrapNodeView<'a>,
    /// The byte-offset span of the entire recover directive.
    pub span: Span<'a>,
}

/// An `@pretty` directive that provides formatting hints for a rule.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrettyDirective<'a> {
    /// The name of the rule to apply formatting hints to.
    pub rule_name: Cow<'a, str>,
    /// Formatting hints (e.g. "group", "indent", "block", "blankline", "nobreak", "fast").
    pub hints: Vec<Cow<'a, str>>,
    /// The byte-offset span of the entire pretty directive.
    pub span: Span<'a>,
}

/// A host function declaration: `@host funcName : ReturnType ;`
#[derive(Debug, Clone, PartialEq)]
pub struct HostFnDecl<'a> {
    pub name: Cow<'a, str>,
    /// Abstract return type name (backend-agnostic). `None` for untyped declarations.
    pub return_type: Option<Cow<'a, str>>,
}

// ─── Parsed Grammar ──────────────────────────────────────────────────────────

/// The result of parsing a complete grammar file: imports + rules + directives.
#[derive(Debug, Clone)]
pub struct ParsedGrammar<'a> {
    pub imports: Vec<ImportDirective<'a>>,
    pub recovers: Vec<RecoverDirective<'a>>,
    pub pretties: Vec<PrettyDirective<'a>>,
    pub rules: AST<'a>,
    /// Custom whitespace pattern from `@ws /regex/ ;` directive.
    pub ws_pattern: Option<Cow<'a, str>>,
    /// Rules to instrument for debugging from `@debug ruleName ;` directives.
    /// `"*"` means all rules.
    pub debug_rules: Vec<Cow<'a, str>>,
    /// Rules marked as lexical tokens from `@token ruleName ;` directives.
    pub token_rules: Vec<Cow<'a, str>>,
    /// Host function declarations from `@host funcName : ReturnType ;` directives.
    pub host_fns: Vec<HostFnDecl<'a>>,
}

impl<'a> ParsedGrammar<'a> {
    /// Create an empty `ParsedGrammar` (no rules, no directives).
    pub fn empty() -> Self {
        Self {
            imports: Vec::new(),
            recovers: Vec::new(),
            pretties: Vec::new(),
            rules: IndexMap::new(),
            ws_pattern: None,
            debug_rules: Vec::new(),
            token_rules: Vec::new(),
            host_fns: Vec::new(),
        }
    }
}
