//! Canonical Grammar IR for the BBNF compiler pipeline.
//!
//! This crate defines the post-analysis intermediate representation consumed by all
//! backends: Rust codegen (AOT), WASM executor (bytecode VM), TS interpreter, and
//! the pretty-printing backend.
//!
//! The IR is fully owned (no lifetimes), serializable via MessagePack for WASM
//! boundary transfer, and captures all analysis results (FIRST sets, SCC info,
//! dispatch hints, span eligibility, pretty hints) so backends need not recompute them.

pub mod passes;
pub mod vm;

// Re-export from bbnf-regex (canonical source of CharSet128, regex_first, classify)
pub use bbnf_regex::sets::charset::CharSet128;
pub mod regex_first {
    //! FIRST set extraction — delegates to bbnf_regex.
    pub use bbnf_regex::first::regex_first_chars;
}

pub use vm::bytecode;
pub use vm::compiler;
pub use vm::debug;
pub use vm::interpreter;

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

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
    Map { inner: Box<IrNode>, fn_id: FnId },

    // ── Whitespace ──────────────────────────────────────────────────────
    /// Marks the inner expression as having optional surrounding whitespace.
    /// This is a flag-level concept: backends translate it to `trim_whitespace(inner)`.
    OptionalWhitespace(Box<IrNode>),

    // ── Lexer-Parser Fusion ──────────────────────────────────────────
    /// Parse a token once, then dispatch on its string value.
    /// Eliminates sequential checkpoint/restore for alternations where multiple
    /// branches start with the same token class (e.g., identifier-led CSS values).
    TokenDispatch {
        /// The token expression to parse (e.g., Regex for identifier class).
        token: Box<IrNode>,
        /// Dispatch arms: each maps string patterns to a continuation body.
        arms: Vec<TokenDispatchArm>,
        /// Fallback arm: tried when no pattern matches (e.g., bare ident).
        fallback: Box<IrNode>,
    },
}

/// A single arm of a `TokenDispatch` node.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct TokenDispatchArm {
    /// String values that trigger this arm (e.g., ["var"], ["calc", "min"]).
    /// Each `StringId` indexes `GrammarIR::strings`.
    pub patterns: Vec<StringId>,
    /// Optional guard: a single-byte literal that must follow the token
    /// (e.g., `(` for function calls). `None` means no guard.
    pub guard_byte: Option<u8>,
    /// Continuation body after the token (+ guard) is consumed.
    pub continuation: IrNode,
    /// The original branch's Map wrapping (enum variant).
    pub map_fn: Option<FnId>,
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
    /// When set, this branch is a catch-all fallback tried when a dispatched
    /// branch fails, or when no dispatch entry matches.  Enables dispatch
    /// tables even when the last Alt branch has a superset FIRST set.
    pub fallback_idx: Option<u8>,
}

// ─── Value Expressions ──────────────────────────────────────────────────────

/// A structured value expression used in `->` map syntax.
///
/// Transparent to IR passes — every node is inspectable for constant folding,
/// type projection, and pattern-based specialization.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum MapExpr {
    /// Integer literal: `0`, `42`, `-1`.
    IntLit(i64),
    /// Float literal: `3.14`, `-1.0`.
    FloatLit(f64),
    /// Boolean literal: `true`, `false`.
    BoolLit(bool),
    /// String literal (interned).
    StringLit(StringId),
    /// The parse result (implicit `input`).
    Input,
    /// Property access on input: `input.len`, `input.as_str`.
    InputProp { prop: StringId },
    /// Function call: `parse_hex(input)`, `len(input)`.
    FnCall { name: StringId, args: Vec<MapExpr> },
    /// Binary operation: `a + b`, `a == b`.
    BinOp { op: MapBinOp, lhs: Box<MapExpr>, rhs: Box<MapExpr> },
    /// Unary operation: `-a`, `!a`.
    UnaryOp { op: MapUnaryOp, inner: Box<MapExpr> },
}

/// Binary operators for value expressions.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Copy)]
pub enum MapBinOp {
    Add, Sub, Mul, Div, Mod,
    Eq, Ne, Lt, Gt, Le, Ge,
    And, Or,
    BitAnd, BitOr, Shl, Shr,
}

/// Unary operators for value expressions.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Copy)]
pub enum MapUnaryOp {
    Neg, Not,
}

impl MapExpr {
    /// Visit each direct child expression.
    pub fn for_each_child(&self, f: &mut impl FnMut(&MapExpr)) {
        match self {
            MapExpr::IntLit(_)
            | MapExpr::FloatLit(_)
            | MapExpr::BoolLit(_)
            | MapExpr::StringLit(_)
            | MapExpr::Input
            | MapExpr::InputProp { .. } => {}
            MapExpr::FnCall { args, .. } => args.iter().for_each(f),
            MapExpr::BinOp { lhs, rhs, .. } => {
                f(lhs);
                f(rhs);
            }
            MapExpr::UnaryOp { inner, .. } => f(inner),
        }
    }

    /// Mutable version of `for_each_child`.
    pub fn for_each_child_mut(&mut self, f: &mut impl FnMut(&mut MapExpr)) {
        match self {
            MapExpr::IntLit(_)
            | MapExpr::FloatLit(_)
            | MapExpr::BoolLit(_)
            | MapExpr::StringLit(_)
            | MapExpr::Input
            | MapExpr::InputProp { .. } => {}
            MapExpr::FnCall { args, .. } => args.iter_mut().for_each(f),
            MapExpr::BinOp { lhs, rhs, .. } => {
                f(lhs);
                f(rhs);
            }
            MapExpr::UnaryOp { inner, .. } => f(inner),
        }
    }

    /// Constant-fold this expression tree where possible.
    pub fn constant_fold(&mut self) {
        // Recurse first.
        self.for_each_child_mut(&mut |child| child.constant_fold());

        // Then fold this node.
        let folded = match self {
            MapExpr::BinOp { op, lhs, rhs } => {
                match (op, lhs.as_ref(), rhs.as_ref()) {
                    (MapBinOp::Add, MapExpr::IntLit(a), MapExpr::IntLit(b)) => Some(MapExpr::IntLit(a.wrapping_add(*b))),
                    (MapBinOp::Sub, MapExpr::IntLit(a), MapExpr::IntLit(b)) => Some(MapExpr::IntLit(a.wrapping_sub(*b))),
                    (MapBinOp::Mul, MapExpr::IntLit(a), MapExpr::IntLit(b)) => Some(MapExpr::IntLit(a.wrapping_mul(*b))),
                    (MapBinOp::Div, MapExpr::IntLit(a), MapExpr::IntLit(b)) if *b != 0 => Some(MapExpr::IntLit(a / b)),
                    (MapBinOp::Mod, MapExpr::IntLit(a), MapExpr::IntLit(b)) if *b != 0 => Some(MapExpr::IntLit(a % b)),
                    (MapBinOp::Add, MapExpr::FloatLit(a), MapExpr::FloatLit(b)) => Some(MapExpr::FloatLit(a + b)),
                    (MapBinOp::Sub, MapExpr::FloatLit(a), MapExpr::FloatLit(b)) => Some(MapExpr::FloatLit(a - b)),
                    (MapBinOp::Mul, MapExpr::FloatLit(a), MapExpr::FloatLit(b)) => Some(MapExpr::FloatLit(a * b)),
                    (MapBinOp::Div, MapExpr::FloatLit(a), MapExpr::FloatLit(b)) if *b != 0.0 => Some(MapExpr::FloatLit(a / b)),
                    (MapBinOp::And, MapExpr::BoolLit(a), MapExpr::BoolLit(b)) => Some(MapExpr::BoolLit(*a && *b)),
                    (MapBinOp::Or, MapExpr::BoolLit(a), MapExpr::BoolLit(b)) => Some(MapExpr::BoolLit(*a || *b)),
                    _ => None,
                }
            }
            MapExpr::UnaryOp { op, inner } => {
                match (op, inner.as_ref()) {
                    (MapUnaryOp::Neg, MapExpr::IntLit(a)) => Some(MapExpr::IntLit(-a)),
                    (MapUnaryOp::Neg, MapExpr::FloatLit(a)) => Some(MapExpr::FloatLit(-a)),
                    (MapUnaryOp::Not, MapExpr::BoolLit(a)) => Some(MapExpr::BoolLit(!a)),
                    _ => None,
                }
            }
            _ => None,
        };
        if let Some(result) = folded {
            *self = result;
        }
    }

    /// Returns true if this is a simple constant (no input dependency).
    pub fn is_constant(&self) -> bool {
        match self {
            MapExpr::IntLit(_) | MapExpr::FloatLit(_) | MapExpr::BoolLit(_) | MapExpr::StringLit(_) => true,
            MapExpr::Input | MapExpr::InputProp { .. } => false,
            MapExpr::FnCall { args, .. } => args.iter().all(|a| a.is_constant()),
            MapExpr::BinOp { lhs, rhs, .. } => lhs.is_constant() && rhs.is_constant(),
            MapExpr::UnaryOp { inner, .. } => inner.is_constant(),
        }
    }
}

// ─── Host Function Descriptors ───────────────────────────────────────────────

/// Describes a host-side function referenced by `IrNode::Map`.
///
/// Internal variants (EnumWrap, BoxWrap, SpanCapture, NumberConvert, HexConvert)
/// are produced by the compiler during lowering and specialization.
/// `Expr` is the only user-facing variant — all `->` map syntax produces it.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum FnDescriptor {
    /// Wrap the parse result in an enum variant: `EnumName::VariantName(result)`.
    EnumWrap {
        /// The variant name (e.g., `"Value"` for `JsonEnum::Value`).
        variant: StringId,
    },

    /// Box the parse result: `Box::new(result)`.
    BoxWrap,

    /// Fused numeric regex scan + f64 conversion (CSS-compatible).
    /// Internal specialization of `Expr { Input, return_type: F64 }` when inner is
    /// a numeric regex. Codegen emits `css_number_scan_f64(state)` → `Option<f64>`.
    NumberConvert,

    /// Fused hex regex scan + user conversion function.
    /// Internal specialization of `Expr { FnCall(name, [Input]), return_type: U32 }` when
    /// inner is a hex digit regex. Codegen emits inline char-class loop + conversion call.
    HexConvert { fn_path: StringId },

    /// Span capture: parse inner expression for validation, discard result, return Span.
    /// Used by `@{expr}` syntax to capture raw text while validating structure.
    SpanCapture,

    /// User-facing value expression. All `->` map syntax lowers to this variant.
    /// The `MapExpr` tree is fully transparent to IR passes for constant folding,
    /// type projection, and pattern-based optimization.
    Expr {
        expr: MapExpr,
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
    /// Currently preserved for prettify codegen to reject explicitly.
    pub split: Option<String>,
}

// ─── Hint Parsing Helpers ──────────────────────────────────────────────────

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

// ─── Type Descriptors ────────────────────────────────────────────────────────

/// Serialized type information for a rule's output.
///
/// This captures the essential structure of the Rust/TS type that the rule produces,
/// without depending on `syn` or any specific type system representation.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeDesc {
    /// A borrowed string span: `Span<'a>` in Rust, `string` in TS.
    Span,
    /// A 64-bit float (produced by fused number scan+convert).
    F64,
    /// An unsigned 32-bit integer (produced by fused hex scan+convert).
    U32,
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

    /// When true, Seq nodes where all children are simple Span leaves collapse
    /// to a single Span, eliminating slab allocation. Enabled when prettify is
    /// disabled (no @pretty formatting constraints require individual Span identity).
    #[serde(default)]
    pub collapse_simple_spans: bool,

    /// When true, all rules are instrumented for debugging.
    /// Set by `@debug * ;` directive or `#[parser(debug)]` attribute.
    #[serde(default)]
    pub debug_all: bool,

    /// Debug labels from `DebugExpression` AST nodes.
    /// Preserved through lowering for display in debug adapters.
    #[serde(default)]
    pub debug_labels: Vec<(RuleId, StringId)>,

    /// Precomputed sub-expression types for codegen. Built by `project_types` pass.
    /// Keyed by `IrNode` raw pointer — valid only within the process that ran
    /// `project_types`. Not serializable (skipped for WASM boundary transfer).
    #[serde(skip)]
    pub type_map: Option<passes::TypeMap>,

    /// Structural pattern annotations per rule. Built by `recognize_patterns` pass.
    #[serde(default)]
    pub pattern_annotations: std::collections::HashMap<RuleId, passes::patterns::PatternAnnotations>,

    /// Cached regex analysis per interned regex pattern. Built by `compute_regex_info` pass.
    /// Pointer-stable within a compile session. Not serializable.
    #[serde(skip)]
    pub regex_info: std::collections::HashMap<StringId, bbnf_regex::RegexInfo>,

    /// Per-node structural facts. Built by `recognize_patterns` pass (tree walk).
    /// Keyed by `IrNode` raw pointer — valid only within the process that ran
    /// the pass. Not serializable.
    #[serde(skip)]
    pub node_facts: std::collections::HashMap<usize, passes::patterns::NodeFacts>,
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

    /// Compute a structural fingerprint of the IR for fixed-point convergence detection.
    ///
    /// Uses rule count + total node count + string pool size as a coarse-grained
    /// change detector. Used as a `debug_assert!` backup for the Changed-flag loop.
    pub fn structural_fingerprint(&self) -> (usize, usize, usize) {
        let node_count: usize = self.rules.iter().map(|r| count_nodes(&r.body)).sum();
        (self.rules.len(), node_count, self.strings.len())
    }
}

impl IrNode {
    /// Visit each direct child node, calling `f` on each.
    ///
    /// Leaves (Literal, Regex, Epsilon, Ref) have no children.
    /// For Alt, visits the `node` field of each `AltBranch`.
    /// For TokenDispatch, visits `token`, each arm's `continuation`, and `fallback`.
    pub fn for_each_child(&self, f: &mut impl FnMut(&IrNode)) {
        match self {
            IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
            IrNode::Seq(children) => children.iter().for_each(f),
            IrNode::Alt(branches, _) => branches.iter().for_each(|b| f(&b.node)),
            IrNode::Repeat { inner, .. }
            | IrNode::Negate(inner)
            | IrNode::OptionalWhitespace(inner)
            | IrNode::Map { inner, .. } => f(inner),
            IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
                f(a);
                f(b);
            }
            IrNode::TokenDispatch {
                token,
                arms,
                fallback,
            } => {
                f(token);
                for arm in arms {
                    f(&arm.continuation);
                }
                f(fallback);
            }
        }
    }

    /// Mutable version of `for_each_child`.
    pub fn for_each_child_mut(&mut self, f: &mut impl FnMut(&mut IrNode)) {
        match self {
            IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
            IrNode::Seq(children) => children.iter_mut().for_each(f),
            IrNode::Alt(branches, _) => branches.iter_mut().for_each(|b| f(&mut b.node)),
            IrNode::Repeat { inner, .. }
            | IrNode::Negate(inner)
            | IrNode::OptionalWhitespace(inner)
            | IrNode::Map { inner, .. } => f(inner),
            IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
                f(a);
                f(b);
            }
            IrNode::TokenDispatch {
                token,
                arms,
                fallback,
            } => {
                f(token);
                for arm in arms {
                    f(&mut arm.continuation);
                }
                f(fallback);
            }
        }
    }
}

/// Count the total number of nodes in an IrNode tree.
fn count_nodes(node: &IrNode) -> usize {
    let mut count = 0;
    node.for_each_child(&mut |child| count += count_nodes(child));
    1 + count
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
