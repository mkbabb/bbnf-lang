use std::collections::{HashMap, HashSet};

use ls_types::*;

use super::pretty::PrettyInfo;

/// Information about a single production rule.
#[derive(Debug, Clone)]
pub struct RuleInfo {
    /// The rule name (LHS nonterminal).
    pub name: String,
    /// Byte offset range of the LHS name.
    pub name_span: (usize, usize),
    /// Byte offset range of the entire rule (LHS = RHS ;).
    pub full_span: (usize, usize),
    /// Pretty-printed RHS for hover display.
    pub rhs_text: String,
    /// All nonterminal references in the RHS.
    pub references: Vec<ReferenceInfo>,
}

/// A reference to a nonterminal in the RHS of a rule.
#[derive(Debug, Clone)]
pub struct ReferenceInfo {
    /// The referenced nonterminal name.
    pub name: String,
    /// Byte offset range of this reference.
    pub span: (usize, usize),
}

/// Semantic token data for a single token.
#[derive(Debug, Clone)]
pub struct SemanticTokenInfo {
    pub span: (usize, usize),
    pub token_type: u32,
}

/// A single imported name with its byte span.
#[derive(Debug, Clone)]
pub struct ImportedItem {
    pub name: String,
    pub span: (usize, usize),
}

/// Owned representation of an import directive.
#[derive(Debug, Clone)]
pub struct ImportInfo {
    /// The path string from the import.
    pub path: String,
    /// Byte offset range of the entire import directive.
    pub span: (usize, usize),
    /// If `Some`, selective import; if `None`, glob import.
    pub items: Option<Vec<ImportedItem>>,
}

/// Owned representation of a @recover directive.
#[derive(Debug, Clone)]
pub struct RecoverInfo {
    /// The name of the rule to wrap with recovery.
    pub rule_name: String,
    /// Byte offset range of the entire recover directive.
    pub span: (usize, usize),
    /// Byte offset range of the rule name within the directive.
    pub rule_name_span: (usize, usize),
    /// The sync expression source text (e.g. `";" | "}"` or `/[;\n]/`).
    pub sync_expr_text: String,
}

/// Owned representation of a `@debug` directive.
#[derive(Debug, Clone)]
pub struct DebugInfo {
    /// Rule name, or `"*"` for all rules.
    pub rule_name: String,
    pub span: (usize, usize),
    pub rule_name_span: (usize, usize),
}

/// Owned representation of a `@token` directive.
#[derive(Debug, Clone)]
pub struct TokenInfo {
    pub rule_name: String,
    pub span: (usize, usize),
    pub rule_name_span: (usize, usize),
}

/// Owned representation of a `@ws` directive.
#[derive(Debug, Clone)]
pub struct WsPatternInfo {
    pub pattern: String,
    pub span: (usize, usize),
}

/// IR-derived metadata for a single rule (optional enrichment from the full pipeline).
#[derive(Debug, Clone, Default)]
pub struct IrRuleMeta {
    /// FOLLOW set characters (formatted for display).
    pub follow_set_label: Option<String>,
    /// Whether a dispatch table exists for this rule's top-level alternation.
    pub has_dispatch: bool,
    /// Memoization strategy name.
    pub memo_strategy: String,
    /// Whether this rule is span-eligible.
    pub span_eligible: bool,
    /// Whether this rule gets an `_sp()` SpanParser method.
    pub has_sp_method: bool,
    /// Inferred output type (formatted for display).
    pub projected_type: Option<String>,
    /// Whether this rule is a transparent alternation.
    pub is_transparent: bool,
}

/// Pre-analyzed document state -- all data is owned (no lifetimes).
#[derive(Debug, Clone)]
pub struct DocumentInfo {
    pub rules: Vec<RuleInfo>,
    pub diagnostics: Vec<Diagnostic>,
    /// name -> index into `rules`
    pub rule_index: HashMap<String, usize>,
    /// Semantic tokens in document order.
    pub semantic_tokens: Vec<SemanticTokenInfo>,
    /// FIRST set labels per rule name (formatted for display).
    pub first_set_labels: HashMap<String, String>,
    /// Rules that can derive the empty string.
    pub nullable_rules: HashSet<String>,
    /// Rules that participate in a cycle, with their cycle path.
    pub cyclic_rule_paths: HashMap<String, String>,
    /// Import directives parsed from the document.
    pub imports: Vec<ImportInfo>,
    /// Recover directives parsed from the document.
    pub recovers: Vec<RecoverInfo>,
    /// Pretty directives parsed from the document.
    pub pretties: Vec<PrettyInfo>,
    /// Debug directives parsed from the document.
    pub debugs: Vec<DebugInfo>,
    /// Token directives parsed from the document.
    pub tokens: Vec<TokenInfo>,
    /// Custom whitespace pattern from `@ws` directive.
    pub ws_pattern: Option<WsPatternInfo>,
    /// IR-derived metadata per rule name (empty if IR pipeline failed or was skipped).
    pub ir_meta: HashMap<String, IrRuleMeta>,
}

/// Diagnostic info extracted from the parser state (owned, no lifetimes).
#[derive(Debug, Clone)]
pub struct ParseDiagnostics {
    /// Parser offset after parsing (how far it consumed).
    pub offset: usize,
    /// Furthest offset reached during parsing (for error reporting).
    pub furthest_offset: usize,
    /// If parsing panicked, the panic message.
    pub panic_message: Option<String>,
}

/// Semantic token type indices matching our legend.
pub mod token_types {
    pub const RULE_DEFINITION: u32 = 0;
    pub const RULE_REFERENCE: u32 = 1;
    pub const STRING: u32 = 2;
    pub const REGEXP: u32 = 3;
    pub const KEYWORD: u32 = 5;
}
