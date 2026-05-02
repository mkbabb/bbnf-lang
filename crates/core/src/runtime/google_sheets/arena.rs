//! AZ-I.W2-act.B2 — Sheets parse arena.
//!
//! The arena owns every compound's child slice plus the per-compound
//! [`SheetsCompoundKind`] discriminator that records the originating
//! grammar rule's structural shape.
//!
//! # Allocation strategy
//!
//! A slab-of-Vec model mirroring `JsonArena`: one
//! [`SheetsCompound`] entry per non-empty compound, holding the child
//! [`SheetsValue`] slice and the compound's structural-kind tag.
//! Empty compounds resolve to a sentinel `EMPTY` handle without
//! consulting the slab.
//!
//! Per `feedback_no-orthogonal-codepaths`, the arena is a singular
//! collection strategy — no conditional branching between Vec and
//! scratch buffers. The Vec-backed slab keeps the surface lean; a
//! switch to a bump arena later is a private refactor on this module.

use bbnf_ir::RuleId;

use crate::runtime::google_sheets::value::SheetsValue;

/// Structural-kind discriminator for [`SheetsCompound`] entries.
///
/// Captures the grammar rule the compound originated from so consumers
/// walking the tree can disambiguate role-overlapping shapes (an
/// operator-tagged Seq inside `AddExpr` vs `MulExpr`; a Tag inside a
/// comparison expression's RHS vs an LHS).
///
/// The variant set covers every compound rule the grammar declares.
/// Wrap-shape rules (`primary`, `range_end`, `cell_or_range`,
/// `expression` post-flattening) collapse onto [`SheetsCompoundKind::Wrap`]
/// — the structural-kind alphabet stays finite even as the grammar's
/// per-rule alphabet grows.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SheetsCompoundKind {
    /// Default / unknown / structural fallback. The kind is filled in
    /// at `begin_compound` time from the layout's `rule_id`; rules
    /// the variant set doesn't enumerate fall through to this arm.
    Unknown,
    /// `formula = /=?/ , expression` — top-level entry-point rule.
    Formula,
    /// `expression = comparison_expr` — transparent forwarder.
    Expression,
    /// `comparison_expr = concat_expr ?w , (compare_op ?w , concat_expr ?w) *`.
    ComparisonExpr,
    /// `concat_expr = add_expr ?w , ("&" ?w , add_expr ?w) *`.
    ConcatExpr,
    /// `add_expr = mul_expr ?w , (add_op ?w , mul_expr ?w) *`.
    AddExpr,
    /// `mul_expr = exp_expr ?w , (mul_op ?w , exp_expr ?w) *`.
    MulExpr,
    /// `exp_expr = unary_expr ?w , ("^" ?w , unary_expr ?w) *`.
    ExpExpr,
    /// `unary_expr = unary_prefix * , postfix_expr`.
    UnaryExpr,
    /// `postfix_expr = primary , "%" *`.
    PostfixExpr,
    /// `primary = let_call | lambda_call | func_call | …`.
    Primary,
    /// `paren_expr = "(" , expression ?w , ")"`.
    ParenExpr,
    /// `func_open = identifier , "("` — admin compound (function
    /// header).
    FuncOpen,
    /// `arg = expression ?` — single-arg holder.
    Arg,
    /// `func_args = (arg << comma ?) +` — Vec of args.
    FuncArgs,
    /// `func_call = func_open , (func_args ?) ?w , ")"`.
    FuncCall,
    /// `let_binding = expression << comma , expression` — paired
    /// (name, value) entry inside `let_args`.
    LetBinding,
    /// `let_args = (let_binding << comma) * , expression`.
    LetArgs,
    /// `let_call = /[lL][eE][tT]\(/ , let_args ?w , ")"`.
    LetCall,
    /// `lambda_params = (expression << comma ?) +`.
    LambdaParams,
    /// `lambda_call = /[lL][aA][mM][bB][dD][aA]\(/ , lambda_params ?w , ")"`.
    LambdaCall,
    /// `array_row = expression , ("," ?w , expression) *`.
    ArrayRow,
    /// `array_rows = array_row , (";" ?w , array_row) *`.
    ArrayRows,
    /// `array_literal = "{" , (array_rows) ?w , "}"`.
    ArrayLiteral,
    /// `cell = sheet_prefix ? , cell_ref`.
    Cell,
    /// `range_ref = sheet_prefix ? , range_end , ":" , range_end`.
    RangeRef,
    /// `range_end = cell_ref | /\$?[A-Za-z]{1,3}/ | /\$?\d+/` —
    /// transparent Wrap forwarder.
    RangeEnd,
    /// `cell_or_range = range_ref | cell` — Wrap forwarder.
    CellOrRange,
    /// `error_literal = "#N/A" -> 0u8 | … | "#SPILL!" -> 8u8` — the
    /// Flat-shape compound carries the per-branch typed `Tag(b)` /
    /// `Error(b)` discriminant. Serializer routes the parent kind to
    /// `error_lexeme(n)` so the compact form re-emits the canonical
    /// `#NAME?` lexeme rather than the operator-tag fallback.
    ErrorLiteral,
    /// `sheet_prefix = /'…'!/ -> 0u8 | /[A-Za-z…]!/ -> 1u8` — the
    /// borrowed span lands as `SheetPrefix { tag, text }` directly so
    /// no compound kind is needed for serialization, but the
    /// structural-kind tag distinguishes `Tag(0)` inside `SheetPrefix`
    /// from operator usage.
    SheetPrefix,
    /// `compare_op = "<>" -> 0u8 | … (6 branches)` — Keyword-shape
    /// rule whose typed `Tag(b)` discriminator routes through
    /// `tag_lexeme(ComparisonExpr, b)` when nested inside a
    /// comparison expression. The own-rule kind exists so the kind
    /// discriminator survives Wrap-collapse for direct `compare_op`
    /// emissions.
    CompareOp,
    /// `add_op = "+" -> 0u8 | "-" -> 1u8` — Keyword-shape `+`/`-`
    /// discriminator. Emits via `tag_lexeme(AddExpr, b)`.
    AddOp,
    /// `mul_op = "*" -> 0u8 | "/" -> 1u8` — Keyword-shape `*`/`/`
    /// discriminator. Emits via `tag_lexeme(MulExpr, b)`.
    MulOp,
    /// `unary_prefix = "+" -> 0u8 | "-" -> 1u8` — Keyword-shape
    /// unary-prefix discriminator.
    UnaryPrefix,
    /// Any other transparent / wrap-shape rule that admits a single
    /// child forwarded through `OpenFrame::Wrap`.
    Wrap,
}

impl SheetsCompoundKind {
    /// Returns `true` when the compound is a transparent Wrap-shape
    /// rule (Alt-of-Refs / single-child forwarder). Such compounds
    /// with exactly one child collapse to the child's value rather
    /// than allocating a slab entry — preserving the grammar's
    /// transparent-wrap semantics.
    pub fn is_transparent_wrap(self) -> bool {
        matches!(
            self,
            Self::Wrap | Self::Primary | Self::Expression | Self::RangeEnd | Self::CellOrRange
        )
    }

    /// Resolve a kind from the rule id the layout carries.
    ///
    /// Integer literals match the rule-id allocation in
    /// `crates/core/src/grammar/generated/google_sheets.rs`. Used by
    /// the builder when admitting a `begin_compound` against an
    /// arbitrary rule; rules unmatched by the rule-id alphabet
    /// collapse into [`SheetsCompoundKind::Wrap`] (the structural
    /// transparent-wrap fallback).
    pub fn from_rule_id(rule_id: RuleId) -> Self {
        match rule_id {
            3 => Self::ErrorLiteral,
            6 => Self::Cell,
            7 => Self::FuncOpen,
            8 => Self::RangeRef,
            9 => Self::CellOrRange,
            10 => Self::ComparisonExpr,
            11 => Self::MulExpr,
            12 => Self::UnaryExpr,
            13 => Self::ParenExpr,
            14 => Self::Arg,
            15 => Self::FuncArgs,
            16 => Self::LetBinding,
            17 => Self::LambdaParams,
            18 => Self::ArrayRow,
            19 => Self::ArrayRows,
            20 => Self::ArrayLiteral,
            21 => Self::ConcatExpr,
            22 => Self::AddExpr,
            23 => Self::ExpExpr,
            24 => Self::LambdaCall,
            26 => Self::FuncCall,
            27 => Self::LetArgs,
            28 => Self::LetCall,
            30 => Self::PostfixExpr,
            31 => Self::Formula,
            // Expression / Primary / RangeEnd / SheetPrefix /
            // CompareOp / AddOp / MulOp / UnaryPrefix / Unknown are
            // retained for AST exhaustiveness; their grammar rules
            // emit no struct layout (the values flow through Wrap or
            // operator-tagged Seq leaves). Transparent / wrap-shape
            // rules and the structural fallback collapse to Wrap,
            // matching JSON's `OpenFrame::Wrap` discipline.
            _ => Self::Wrap,
        }
    }
}

/// One compound entry in the arena. Owns its child [`SheetsValue`]
/// slice and carries the originating rule's structural-kind tag.
///
/// The kind is co-located with the slice so consumers walking through
/// a [`SheetsValue::Compound`] handle observe the rule's structural
/// shape in one indirection.
#[derive(Debug, Clone)]
pub struct SheetsCompound<'p> {
    /// The compound's structural kind (the originating rule's
    /// shape).
    pub kind: SheetsCompoundKind,
    /// The child slice — every direct payload-bearing pushes from
    /// inside this compound's frame.
    pub children: Vec<SheetsValue<'p>>,
}

/// Opaque handle for a compound entry's slot in the arena.
///
/// The `0` value is reserved for [`Self::EMPTY`]; non-empty handles
/// take values `1..=u32::MAX`. Resolved via [`SheetsArena::compound`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SheetsCompoundId(u32);

impl SheetsCompoundId {
    /// Sentinel handle for an empty compound. Resolves to a synthetic
    /// `Wrap`-kind entry with no children, regardless of the owning
    /// arena's slab state.
    pub const EMPTY: Self = Self(0);

    /// `true` iff this is the empty-compound sentinel.
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    /// Index into the owning arena's slab, less one (since `0` is
    /// reserved for [`Self::EMPTY`]).
    #[inline]
    fn slab_index(self) -> Option<usize> {
        if self.0 == 0 {
            None
        } else {
            Some((self.0 - 1) as usize)
        }
    }
}

/// Owning slab for compound child entries.
///
/// One Vec slot per non-empty compound; empty compounds bypass the
/// slab via the [`SheetsCompoundId::EMPTY`] sentinel. Capacity hints
/// come from
/// [`bbnf_ir::registry::StructRegistry`]'s per-grammar capacity
/// estimates threaded through the emitter at codegen time.
#[derive(Debug, Default)]
pub struct SheetsArena<'p> {
    /// Per-compound slab. Index = `SheetsCompoundId.0 - 1`.
    compounds: Vec<SheetsCompound<'p>>,
}

impl<'p> SheetsArena<'p> {
    /// Construct an empty arena.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Construct an arena with a pre-sized compound slab. Hints come
    /// from
    /// [`bbnf_ir::registry::StructRegistry`] capacity estimates the
    /// emitter folds into the parse function.
    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            compounds: Vec::with_capacity(compounds),
        }
    }

    /// Push a populated compound into the slab and return the
    /// resolving handle. Empty compounds (no children) still return a
    /// fresh handle so the structural-kind tag survives — the
    /// `EMPTY` sentinel is reserved for compounds the emitter
    /// affirmatively elides.
    #[inline]
    pub fn push_compound(
        &mut self,
        kind: SheetsCompoundKind,
        children: Vec<SheetsValue<'p>>,
    ) -> SheetsCompoundId {
        self.compounds.push(SheetsCompound { kind, children });
        // Indices start at 1 so 0 maps to EMPTY.
        let idx = self.compounds.len() as u32;
        SheetsCompoundId(idx)
    }

    /// Resolve a compound handle to its entry. Returns a synthetic
    /// `Wrap`-kind empty entry for [`SheetsCompoundId::EMPTY`].
    #[inline]
    pub fn compound(&self, id: SheetsCompoundId) -> SheetsCompoundView<'_, 'p> {
        match id.slab_index() {
            None => SheetsCompoundView {
                kind: SheetsCompoundKind::Wrap,
                children: &[],
            },
            Some(i) => {
                let entry = &self.compounds[i];
                SheetsCompoundView {
                    kind: entry.kind,
                    children: entry.children.as_slice(),
                }
            }
        }
    }

    /// Number of registered (non-empty) compounds.
    #[inline]
    pub fn compound_count(&self) -> usize {
        self.compounds.len()
    }

    /// Roll back the arena to a prior compound-count snapshot.
    #[inline]
    pub fn truncate(&mut self, compounds: usize) {
        self.compounds.truncate(compounds);
    }
}

/// Borrowed view of one compound entry — the arena lends its `kind`
/// and its `children` slice without exposing the owning Vec.
///
/// The lifetime `'a` ties the slice to the arena's borrow; the inner
/// `'p` is the parse-input lifetime carried by every interior
/// [`SheetsValue::String`] / [`SheetsValue::CellRef`] / etc.
#[derive(Debug, Clone, Copy)]
pub struct SheetsCompoundView<'a, 'p: 'a> {
    /// The compound's structural-kind tag.
    pub kind: SheetsCompoundKind,
    /// The compound's child slice.
    pub children: &'a [SheetsValue<'p>],
}
