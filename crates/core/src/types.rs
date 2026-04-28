use std::borrow::Cow;

use parse_that::Span;

use indexmap::IndexMap;

use crate::runtime::bbnf::BbnfView;

// ─── Grammar AST ─────────────────────────────────────────────────────────────

/// A single rule entry: name span + reference to the RHS in the bootstrap AST.
///
/// AZ-II.cutover.D — under the struct-direct BBNF parser, the RHS is a
/// [`BbnfView`] focused on the rule's RHS [`crate::runtime::bbnf::BbnfValue`]
/// inside the owning [`crate::runtime::bbnf::BbnfDocument`]. The view
/// shares its `'a` lifetime with the parse arena and the source slice
/// (a single shared lifetime since the document and input borrow from
/// the same parse call site). Walking the RHS dispatches on the
/// focused compound's [`crate::runtime::bbnf::BbnfCompoundKind`]
/// (compound shape) or on [`crate::runtime::bbnf::BbnfKind`] (when
/// scalar / span / unit semantics matter).
#[derive(Debug, Clone, Copy)]
pub struct RuleEntry<'a> {
    /// Span of the rule's LHS identifier.
    pub name_span: Span<'a>,
    /// The RHS expression — a view focused on the rule body inside the
    /// owning [`crate::runtime::bbnf::BbnfDocument`].
    pub rhs: BbnfView<'a, 'a>,
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
    /// The sync expression — a view focused on the recover-directive's
    /// body expression inside the owning
    /// [`crate::runtime::bbnf::BbnfDocument`].
    pub sync_expr: BbnfView<'a, 'a>,
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

// ─── Grammar Extract ─────────────────────────────────────────────────────────
//
// Tranche AU.4.1: the historical `ParsedGrammar` aggregate is gone. The
// lowering pipeline now walks the bootstrap tape directly into
// `DirectiveMaps` + `AST` (see `pipeline::directives::extract_from_tape`) —
// no intermediate grab-bag struct on the hot compile path.
//
// `GrammarExtract` survives purely as the query facade for observational
// consumers (LSP analysis, gorgeous JIT, debug binaries) that want a
// single handle grouping every directive vector plus the rule map.
// Pipeline code does NOT construct one.

/// Observational view over a parsed BBNF file.
///
/// Aggregates every directive vector + the rule AST so LSP / gorgeous
/// / debug callers can consume a single value. The compile pipeline
/// bypasses this type entirely; use
/// [`crate::pipeline::directives::extract_from_tape`] instead.
#[derive(Debug, Clone)]
pub struct GrammarExtract<'a> {
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

impl<'a> GrammarExtract<'a> {
    /// Create an empty `GrammarExtract` (no rules, no directives).
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
