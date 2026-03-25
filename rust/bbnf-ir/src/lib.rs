//! Canonical Grammar IR for the BBNF compiler pipeline.
//!
//! This crate defines the post-analysis intermediate representation consumed by all
//! backends: Rust codegen (AOT), WASM executor (bytecode VM), TS interpreter, and
//! the pretty-printing backend.
//!
//! The IR is fully owned (no lifetimes), serializable via MessagePack for WASM
//! boundary transfer, and captures all analysis results (FIRST sets, SCC info,
//! dispatch hints, span eligibility, pretty hints) so backends need not recompute them.

pub mod charset;
pub mod passes;
pub mod bytecode;
pub mod compiler;
pub mod interpreter;
pub mod regex_first;

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

pub use charset::CharSet128;

// ─── Identifiers ─────────────────────────────────────────────────────────────

/// Index into `GrammarIR::rules`.
pub type RuleId = u32;

/// Index into `GrammarIR::strings` interning table.
pub type StringId = u32;

/// Index into `GrammarIR::fns` host function table.
pub type FnId = u32;

// ─── Source Mapping ──────────────────────────────────────────────────────────

/// Byte range in the original grammar source file.
///
/// Used for DWARF-like source mapping from IR/bytecode back to grammar source.
/// Owned (no lifetime) so it survives serialization across WASM boundary.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Default)]
pub struct GrammarSpan {
    pub start: u32,
    pub end: u32,
}

// ─── IR Nodes ────────────────────────────────────────────────────────────────

/// A single node in the grammar IR tree.
///
/// Each variant maps 1:1 from an `Expression` AST variant, with syntactic sugar
/// (Group, OptionalWhitespace) eliminated and all string references interned.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum IrNode {
    // ── Leaves ──────────────────────────────────────────────────────────

    /// A literal string match. The `StringId` indexes `GrammarIR::strings`.
    Literal(StringId),

    /// A regex match. The `StringId` indexes `GrammarIR::strings`.
    Regex(StringId),

    /// Matches the empty string.
    Epsilon,

    // ── Combinators (N-ary flat) ────────────────────────────────────────

    /// Sequential concatenation: all children must match in order.
    Seq(Vec<IrNode>),

    /// Ordered alternation: try each branch until one matches.
    /// The optional `AltDispatch` is set by the dispatch pass when all branches
    /// have pairwise disjoint FIRST sets.
    Alt(Vec<AltBranch>, Option<AltDispatch>),

    /// Repetition: `lo..=hi` occurrences of `inner`.
    /// - `(0, 1)` → optional
    /// - `(0, u32::MAX)` → many (zero or more)
    /// - `(1, u32::MAX)` → many1 (one or more)
    Repeat {
        inner: Box<IrNode>,
        lo: u32,
        hi: u32,
    },

    /// Reference to another rule by its `RuleId`.
    Ref(RuleId),

    // ── Binary ──────────────────────────────────────────────────────────

    /// `a << b` — parse both, keep left result.
    Skip(Box<IrNode>, Box<IrNode>),

    /// `a >> b` — parse both, keep right result.
    Next(Box<IrNode>, Box<IrNode>),

    /// Set-difference: match `lhs` only if `rhs` does NOT match at the same position.
    Minus(Box<IrNode>, Box<IrNode>),

    // ── Lookahead ───────────────────────────────────────────────────────

    /// Zero-width negative assertion: fails if inner matches, consumes nothing.
    Negate(Box<IrNode>),

    // ── Host integration ────────────────────────────────────────────────

    /// Apply a host function to the parse result.
    Map {
        inner: Box<IrNode>,
        fn_id: FnId,
    },

    // ── Whitespace ──────────────────────────────────────────────────────

    /// Marks the inner expression as having optional surrounding whitespace.
    /// This is a flag-level concept: backends translate it to `trim_whitespace(inner)`.
    OptionalWhitespace(Box<IrNode>),
}

/// A single branch of an `Alt` node, with optional pre-computed FIRST set.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct AltBranch {
    /// The branch expression.
    pub node: IrNode,
    /// Pre-computed FIRST set for this branch (from analysis pass).
    /// `None` if not yet computed or if the branch is nullable.
    pub first_set: Option<CharSet128>,
}

/// Pre-computed dispatch table for an `Alt` node with disjoint FIRST sets.
/// Set by the dispatch pass; consumed directly by the bytecode compiler.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct AltDispatch {
    /// 128-entry table: `table[byte]` = branch index, or 255 for no match.
    pub table: Vec<u8>,
}

// ─── Host Function Descriptors ───────────────────────────────────────────────

/// Describes a host-side function referenced by `IrNode::Map`.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum FnDescriptor {
    /// Wrap the parse result in an enum variant: `EnumName::VariantName(result)`.
    EnumWrap {
        /// The variant name (e.g., `"Value"` for `JsonEnum::Value`).
        variant: StringId,
    },

    /// Box the parse result: `Box::new(result)`.
    BoxWrap,

    /// A custom closure provided as source text (Rust-only).
    /// The `StringId` indexes `GrammarIR::strings` and contains the closure text.
    /// The optional `return_type` is parsed from the closure's `-> ReturnType` annotation.
    Custom {
        source: StringId,
        return_type: Option<TypeDesc>,
    },
}

// ─── Rule-Level Metadata ─────────────────────────────────────────────────────

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
    pub split: Option<String>,
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

    // ── Pretty ──────────────────────────────────────────────────────────

    /// Pretty-printing hints from `@pretty` directives.
    pub pretty: Option<PrettyHints>,

    /// Error recovery sync expression.
    pub recover: Option<IrNode>,

    /// Whether `@no_collapse` is set for this rule.
    pub no_collapse: bool,

    /// Whether `@inline` is set for this rule. Force-inlined rules have their
    /// body substituted at every call site — no enum variant, no function.
    #[serde(default)]
    pub force_inline: bool,

    /// Whether `@debug` is set for this rule. Instrumented rules emit
    /// trace output (compiled paths) or `DebugBreak` opcodes (VM path).
    #[serde(default)]
    pub debug: bool,

    // ── Sub-variants ────────────────────────────────────────────────────

    /// Sub-variants for heterogeneous alternation branches.
    /// Empty if the rule is not an alternation or if all branches have the same type.
    #[serde(default)]
    pub sub_variants: Vec<SubVariant>,
}

// ─── Type Descriptors ────────────────────────────────────────────────────────

/// Serialized type information for a rule's output.
///
/// This captures the essential structure of the Rust/TS type that the rule produces,
/// without depending on `syn` or any specific type system representation.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum TypeDesc {
    /// A borrowed string span: `Span<'a>` in Rust, `string` in TS.
    Span,
    /// An optional value.
    Option(Box<TypeDesc>),
    /// A vector of values.
    Vec(Box<TypeDesc>),
    /// A fixed-size tuple.
    Tuple(Vec<TypeDesc>),
    /// A boxed enum variant: `Box<EnumName<'a>>`.
    BoxedEnum,
    /// An enum variant: `EnumName<'a>`.
    Enum,
    /// A named type (for custom mapping results).
    Named(StringId),
}

// ─── Top-Level Grammar IR ────────────────────────────────────────────────────

/// The canonical Grammar IR — the single intermediary between the BBNF frontend
/// and all backends.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct GrammarIR {
    /// All rules in topological order.
    pub rules: Vec<IrRule>,

    /// The entry rule (first rule parsed).
    pub entry: RuleId,

    /// String interning table. All `StringId` values index into this vector.
    pub strings: Vec<String>,

    /// Host function table. All `FnId` values index into this vector.
    pub fns: Vec<FnDescriptor>,

    /// Type information for rules that have been inferred.
    pub types: Vec<(RuleId, TypeDesc)>,

    /// FOLLOW sets for all rules, keyed by `RuleId`.
    /// Populated by the `compute_follow_sets` pass; empty until that pass runs.
    #[serde(default)]
    pub follow_sets: HashMap<RuleId, CharSet128>,

    /// Custom whitespace pattern from `@ws /regex/ ;` directive.
    /// When set, `?w` (OptionalWhitespace) compiles to this regex instead of the
    /// default ASCII `\s*` trim. The StringId indexes `self.strings`.
    #[serde(default)]
    pub ws_pattern: Option<StringId>,

    /// When true, the B.1 all-Span guard keeps overrides for simple Seqs,
    /// collapsing them to Span and eliminating arena allocation.
    /// Set to true when prettify is disabled (no @pretty formatting constraints).
    #[serde(default)]
    pub b1_span_collapse: bool,

    /// When true, all rules are instrumented for debugging.
    /// Set by `@debug * ;` directive or `#[parser(debug)]` attribute.
    #[serde(default)]
    pub debug_all: bool,

    /// Debug labels from `DebugExpression` AST nodes.
    /// Preserved through lowering for display in debug adapters.
    #[serde(default)]
    pub debug_labels: Vec<(RuleId, StringId)>,
}

impl GrammarIR {
    /// Look up an interned string by its `StringId`.
    pub fn get_string(&self, id: StringId) -> &str {
        &self.strings[id as usize]
    }

    /// Look up a rule by its `RuleId`.
    pub fn get_rule(&self, id: RuleId) -> &IrRule {
        &self.rules[id as usize]
    }

    /// Find a rule by name.
    pub fn find_rule(&self, name: &str) -> Option<&IrRule> {
        self.rules.iter().find(|r| self.get_string(r.name) == name)
    }
}

// ─── Serialization ──────────────────────────────────────────────────────────

impl GrammarIR {
    /// Serialize to MessagePack bytes (compact binary, suitable for WASM boundary).
    pub fn to_msgpack(&self) -> Result<Vec<u8>, rmp_serde::encode::Error> {
        rmp_serde::to_vec(self)
    }

    /// Deserialize from MessagePack bytes.
    pub fn from_msgpack(bytes: &[u8]) -> Result<Self, rmp_serde::decode::Error> {
        rmp_serde::from_slice(bytes)
    }

    /// Serialize to JSON string (for debugging).
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }

    /// Deserialize from JSON string.
    pub fn from_json(json: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(json)
    }
}
