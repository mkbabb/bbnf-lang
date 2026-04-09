//! Per-rule data: `IrRule`, `RuleMeta` (analysis facts), `RuleDirectives`
//! (`@` decorators), `MemoStrategy`, `DispatchHint`, `PrettyHints`,
//! `SubVariant`, plus the `parse_sep_hint` / `parse_split_hint` parsers.

use serde::{Deserialize, Serialize};

use bbnf_regex::sets::charset::CharSet128;

use super::{GrammarSpan, IrNode, RuleId, StringId, TypeDesc};

/// A single rule in the grammar IR.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct IrRule {
    /// Unique identifier for this rule.
    pub id: RuleId,

    /// The rule name (interned).
    pub name: StringId,

    /// The rule body expression.
    pub body: IrNode,

    /// Metadata from analysis passes.
    pub meta: RuleMeta,

    /// Source span of the rule definition in the grammar file (byte offsets).
    /// Populated during lowering; survives all IR passes unchanged.
    #[serde(default)]
    pub source_span: Option<GrammarSpan>,
}

/// Memoization strategy for a rule.
#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq, Eq)]
pub enum MemoStrategy {
    /// Never memoize (acyclic rules).
    #[default]
    None,
    /// Always memoize (cyclic rules).
    Full,
    /// Memoize only when reference count exceeds threshold.
    Selective,
}

/// Hint for dispatch table generation.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum DispatchHint {
    /// Single-byte dispatch: 128-entry table mapping first byte → branch index.
    ByteTable {
        /// 128-entry table: `table[byte] = branch_index` or `-1` for no match.
        table: Vec<i8>,
    },
}

/// Pretty-printing hints for a rule, derived from `@pretty` directives.
#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq)]
pub struct PrettyHints {
    /// Wrap output in a Group (break to multi-line if too wide).
    pub group: bool,
    /// Indent content by one level.
    pub indent: bool,
    /// Dedent content by one level.
    pub dedent: bool,
    /// Hardline separator between list elements.
    pub block: bool,
    /// Double hardline between elements.
    pub blankline: bool,
    /// Space separator that never breaks.
    pub nobreak: bool,
    /// Softline separator.
    pub softbreak: bool,
    /// Hardline separator between tuple elements.
    pub hardbreak: bool,
    /// Suppress all breaks.
    pub compact: bool,
    /// Hardline separator, skip SmartJoin.
    pub fast: bool,
    /// Disable auto-heuristics.
    pub off: bool,
    /// Custom separator string (from `sep("...")`).
    pub sep: Option<String>,
    /// Format-time balanced splitting delimiter (from `split("...")`).
    /// Currently preserved for prettify codegen to reject explicitly.
    pub split: Option<String>,
}

/// Parse a `sep("...")` hint string, returning the separator content.
pub fn parse_sep_hint(h: &str) -> Option<&str> {
    h.strip_prefix("sep(\"")?.strip_suffix("\")")
}

/// Parse a `split("...")` hint string, returning the delimiter content.
pub fn parse_split_hint(h: &str) -> Option<&str> {
    h.strip_prefix("split(\"")?.strip_suffix("\")")
}

/// A sub-variant for a heterogeneous alternation branch.
///
/// When an `Alt` node produces `BoxedEnum` overall, branches that produce
/// non-`BoxedEnum` types get anonymous enum variants (e.g. `rule_0(Ty)`).
/// These are used by codegen to wrap individual branches.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct SubVariant {
    /// The generated variant name (e.g. `"jsonValue_0"`).
    pub variant_name: StringId,
    /// The type produced by this branch.
    pub ty: TypeDesc,
    /// Which branch in the Alt this corresponds to.
    pub branch_index: u32,
}

/// Per-rule directives from `@` decorators in the grammar.
/// Extensible: new decorators add fields here.
#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq)]
pub struct RuleDirectives {
    /// Pretty-printing hints from `@pretty` directive.
    pub pretty: Option<PrettyHints>,
    /// Error recovery sync expression from `@recover` directive.
    pub recover: Option<IrNode>,
    /// Whether `@token` is set (lexical token, fusion-inlined).
    #[serde(default)]
    pub token: bool,
    /// Whether `@debug` is set (trace instrumentation).
    #[serde(default)]
    pub debug: bool,
}

/// Analysis metadata for a single rule.
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct RuleMeta {
    // ── Analysis ────────────────────────────────────────────────────────
    /// FIRST set for this rule.
    pub first_set: CharSet128,

    /// Whether this rule can match the empty string.
    pub nullable: bool,

    /// SCC index (if part of a strongly-connected component).
    pub scc_id: Option<u32>,

    /// Whether this rule participates in a cycle.
    pub is_cyclic: bool,

    // ── Optimization hints ──────────────────────────────────────────────
    /// Memoization strategy.
    pub memo: MemoStrategy,

    /// Dispatch table hint for alternation rules.
    pub dispatch: Option<DispatchHint>,

    /// Whether the entire rule body can be expressed as a SpanParser.
    pub span_eligible: bool,

    /// Whether this rule actually gets an `_sp()` method.
    /// A subset of `span_eligible`: requires all referenced rules to also have `_sp()` methods.
    /// Computed by `compute_sp_method_rules` pass.
    pub has_sp_method: bool,

    /// If this rule is an alias (simple reference to another rule).
    pub is_alias: Option<RuleId>,

    /// Whether this rule is a transparent alternation of nonterminals.
    pub is_transparent: bool,

    /// Whether this rule's identity must be preserved through optimization.
    /// When true, the rule is never pruned (treated as a DFS root), never aliased,
    /// and never marked transparent — but its body IS fully optimized.
    pub preserve_identity: bool,

    // ── Directives ──────────────────────────────────────────────────────
    /// Per-rule `@` directives (pretty, recover, token, debug).
    #[serde(default)]
    pub directives: RuleDirectives,

    // ── Sub-variants ────────────────────────────────────────────────────
    /// Sub-variants for heterogeneous alternation branches.
    /// Empty if the rule is not an alternation or if all branches have the same type.
    #[serde(default)]
    pub sub_variants: Vec<SubVariant>,
}
