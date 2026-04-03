//! Backend infrastructure: shared analysis, compilation driver, and emission trait.
//!
//! The backend module separates **target-agnostic decisions** (what to emit) from
//! **target-specific emission** (how to emit). The shared `driver` walks `GrammarIR`,
//! makes optimization decisions (dispatch strategy, span compression, inlining, etc.),
//! and calls `Emitter` trait methods with pre-resolved data.
//!
//! Each backend (Rust, TypeScript, WASM) implements `Emitter` to translate decisions
//! into target syntax. See `rust/`, `ts/`, `wasm/` sub-modules for implementations.

pub mod analysis;
pub mod driver;
pub mod rust;
pub mod ts;
pub mod wasm;

pub use analysis::{
    BackendAnalysis, BackendPreparation, EffectiveBackendConfig, PreparedGrammar, TypeAnalysis,
    prepare_grammar,
};

use bbnf_ir::{AltDispatch, GrammarIR, IrRule, RuleId, TypeDesc};

// ─── Compilation Driver Types ───────────────────────────────────────────────

/// A group of children within a `Seq` node, as classified by the driver.
///
/// Consecutive Span-typed children are compressed into a single `SpanGroup`,
/// while non-Span children remain as individual `Single` entries.
pub enum SeqChildGroup<O> {
    /// A single non-Span child with its emitted output and projected type.
    Single { output: O, ty: TypeDesc },
    /// Consecutive Span-typed children compressed into one Span.
    /// The outputs were emitted for their side effects (advancing the parser offset).
    SpanCompressed { outputs: Vec<O> },
}

/// Configuration for a `sep_by` loop, as detected by the driver.
///
/// All three sep_by variants (bare, ws-aware, delimited-with-terminator) are
/// unified into this configuration. The driver detects the variant and fills
/// in the appropriate fields.
pub struct SepByConfig {
    /// Whether to trim whitespace around elements and separators.
    pub ws: bool,
    /// Minimum element count (from `Repeat { lo, .. }`).
    pub lo: u32,
    /// If the sep_by is delimited (e.g., `"(" >> items << ")"`), the closing
    /// delimiter's byte(s) for early-exit checking.
    pub terminator_bytes: Option<Vec<u8>>,
}

/// How a rule reference should be compiled, as decided by the inline analysis.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CallStrategy {
    /// Emit a direct function call to the rule.
    DirectCall,
    /// Inline the rule body at the call site.
    InlineBody,
    /// Fusion-inline: inline the body but preserve the enum variant
    /// (used for `@token`-marked rules).
    InlineFusion,
}

/// Result type classification for a `Seq` node, as resolved by the driver.
pub enum SeqResultStrategy {
    /// All children are Span-typed → compress to single Span.
    AllSpan,
    /// Mixed types → assemble as tuple or single value.
    Mixed {
        /// The projected result type.
        result_type: TypeDesc,
        /// If the result is `(T, Vec<T>)` or `(Vec<T>, T)`, this indicates
        /// the flattening strategy.
        flatten: Option<FlattenStrategy>,
    },
}

/// How to flatten a `(T, Vec<T>)` pair into `Vec<T>`.
pub enum FlattenStrategy {
    /// First element is scalar, second is Vec: `prepend`.
    HeadThenVec,
    /// First element is Vec, second is scalar: `append`.
    VecThenTail,
}

/// Which dispatch strategy the driver selected for an `Alt` node.
pub enum AltStrategy<'a> {
    /// All branches are literals (or Map(Literal, Constant)): emit sequential byte match.
    AllLiteral,
    /// O(1) byte-dispatch table is available from the IR analysis pass.
    Dispatch { table: &'a AltDispatch },
    /// Key-based dispatch on multi-byte lookahead.
    KeyDispatch,
    /// Fallback: checkpoint/restore per branch.
    Checkpoint,
}

/// Information about a single Alt branch, as resolved by the driver.
pub struct AltBranchInfo {
    /// The branch's projected type.
    pub ty: TypeDesc,
    /// If the branch needs coercion to a different type (heterogeneous alt),
    /// the sub-variant name for wrapping.
    pub coercion_variant: Option<String>,
}

/// How allocation should be handled for a value.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AllocStrategy {
    /// Return the value inline (by-value). No heap allocation.
    Elide,
    /// Allocate the value (Rust: slab alloc, TS: no-op, WASM: linear memory).
    Alloc,
}

// ─── Emitter Trait ──────────────────────────────────────────────────────────

/// Backend-specific code emission.
///
/// The compilation driver walks `GrammarIR`, makes target-agnostic decisions
/// (dispatch strategy, span compression, inlining, etc.), and calls these methods
/// with pre-resolved data. Each backend implements this trait to produce target code.
///
/// ## Naming convention
/// - **`compile_*`** = shared driver methods (make decisions, call emitter)
/// - **`emit_*`** = emitter trait methods (produce target syntax)
///
/// ## Output type
/// `Self::Output` is an opaque representation of emitted code:
/// - Rust backend: `proc_macro2::TokenStream`
/// - TypeScript backend: `String`
/// - WASM backend: instruction sequence
pub trait Emitter {
    /// Opaque code fragment produced by emission methods.
    type Output;

    /// Backend-specific mutable context (variable naming, scope tracking, etc.).
    type Ctx;

    // ── Leaves ──────────────────────────────────────────────────────────

    /// Emit a literal string match.
    ///
    /// `guaranteed_byte`: if `Some(b)`, the dispatch table has already proven
    /// that the input byte at current offset == `b`, so the emitter may skip
    /// the first byte check.
    fn emit_literal_match(
        &mut self,
        value: &str,
        guaranteed_byte: Option<u8>,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit a regex pattern match.
    fn emit_regex_match(
        &mut self,
        pattern: &str,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit an epsilon (zero-width) match.
    fn emit_epsilon(&mut self, ctx: &mut Self::Ctx) -> Self::Output;

    // ── Sequences ───────────────────────────────────────────────────────

    /// Emit a sequence where all children are Span-typed.
    ///
    /// The `child_outputs` were emitted for their side effects (advancing offset).
    /// The emitter should return a single combined Span from start to current offset.
    fn emit_seq_all_span(
        &mut self,
        child_outputs: Vec<Self::Output>,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit a sequence with grouped children.
    ///
    /// `groups` contains the classified children (individual values or span-compressed groups).
    /// `result_type` is the projected output type.
    fn emit_seq_grouped(
        &mut self,
        groups: Vec<SeqChildGroup<Self::Output>>,
        result_type: &TypeDesc,
        flatten: Option<FlattenStrategy>,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    // ── Alternations ────────────────────────────────────────────────────

    /// Emit an alternation using a pre-computed O(1) dispatch table.
    fn emit_alt_dispatch(
        &mut self,
        table: &AltDispatch,
        branches: Vec<(AltBranchInfo, Self::Output)>,
        fallback: Option<(AltBranchInfo, Self::Output)>,
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit an alternation as sequential checkpoint/restore attempts.
    fn emit_alt_checkpoint(
        &mut self,
        branches: Vec<(AltBranchInfo, Self::Output)>,
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit an alternation where all branches are literals.
    ///
    /// `literals` maps each literal string to its emitted branch body.
    fn emit_alt_all_literal(
        &mut self,
        literals: Vec<(String, Self::Output)>,
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    // ── Repetition ──────────────────────────────────────────────────────

    /// Emit a many-loop (0+ or 1+ repetitions).
    ///
    /// `elem_type` is the per-element type for collection.
    fn emit_repeat_many(
        &mut self,
        body: Self::Output,
        lo: u32,
        hi: u32,
        elem_type: &TypeDesc,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit an optional match (0..1 repetition).
    ///
    /// `result_type` is `Option(inner_type)`.
    fn emit_repeat_optional(
        &mut self,
        body: Self::Output,
        inner_type: &TypeDesc,
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit a sep_by loop (element separated by separator, with optional ws/delimiters).
    fn emit_sep_by(
        &mut self,
        element: Self::Output,
        separator: Self::Output,
        config: &SepByConfig,
        elem_type: &TypeDesc,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    // ── References ──────────────────────────────────────────────────────

    /// Emit a rule reference as a direct function call.
    fn emit_call(
        &mut self,
        rule_id: RuleId,
        rule_name: &str,
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit an inlined rule body with enum variant wrapping.
    ///
    /// `body` is the already-emitted rule body. `variant_name` is the enum variant
    /// to wrap with (if the rule is non-transparent).
    fn emit_inline_wrap(
        &mut self,
        body: Self::Output,
        variant_name: Option<&str>,
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    // ── Binary operators ────────────────────────────────────────────────

    /// Emit Skip: parse both sides, return the left result.
    fn emit_skip(
        &mut self,
        kept: Self::Output,
        discarded: Self::Output,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit Next: parse both sides, return the right result.
    fn emit_next(
        &mut self,
        discarded: Self::Output,
        kept: Self::Output,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit Minus: set-difference (match `lhs` only if `rhs` does NOT match).
    fn emit_minus(
        &mut self,
        lhs: Self::Output,
        rhs: Self::Output,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit Negate: zero-width negative lookahead.
    fn emit_negate(
        &mut self,
        inner: Self::Output,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    // ── Value manipulation ──────────────────────────────────────────────

    /// Emit an enum variant wrapper around a value.
    fn emit_enum_wrap(
        &mut self,
        inner: Self::Output,
        variant_name: &str,
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit a fused number scan+convert (regex → f64).
    fn emit_number_convert(
        &mut self,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit a constant value, discarding the parse result.
    fn emit_constant(
        &mut self,
        discard_inner: Self::Output,
        value: &str,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit whitespace trimming (from `?w` / OptionalWhitespace) as a side effect.
    fn emit_ws_trim(
        &mut self,
        ws_pattern: Option<&str>,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit an expression wrapped with optional whitespace trimming.
    ///
    /// Trims whitespace before and after `inner`, returning `inner`'s value.
    /// This is the correct compilation of `?w` (OptionalWhitespace):
    /// ws_trim is a side-effect (advances offset), inner's value propagates.
    fn emit_with_ws_trim(
        &mut self,
        inner: Self::Output,
        ws_pattern: Option<&str>,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    // ── Rule-level emission ─────────────────────────────────────────────

    /// Emit a complete rule function definition.
    ///
    /// `body` is the already-emitted function body.
    fn emit_rule_function(
        &mut self,
        rule: &IrRule,
        body: Self::Output,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit type definitions for the grammar (enum, discriminated union, etc.).
    fn emit_type_definitions(
        &mut self,
        ir: &GrammarIR,
        analysis: &BackendAnalysis,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Combine all emitted rule functions and type definitions into a final output.
    fn emit_grammar(
        &mut self,
        type_defs: Self::Output,
        rule_functions: Vec<Self::Output>,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;
}
