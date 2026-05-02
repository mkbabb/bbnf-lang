//! AZ-II.cutover.A — BBNF parse arena.
//!
//! The arena owns every compound child slice ([`BbnfValue`] elements
//! across every BBNF compound rule). Compounds reference their
//! children via opaque [`BbnfCompoundId`] handles that resolve to
//! slices through [`BbnfArena::compound`].
//!
//! # Allocation strategy
//!
//! A simple slab-of-Vec model mirroring
//! [`crate::runtime::google_sheets::SheetsArena`]: one inner `Vec`
//! per non-empty compound, indexed by handle. Empty compounds resolve
//! to `&[]` without allocating a slab entry — the empty-handle constant
//! ([`BbnfCompoundId::EMPTY`]) carries the discriminator for a
//! zero-cost empty-resolution branch.
//!
//! Per `feedback_no-orthogonal-codepaths`, this is one collection
//! strategy with no conditional branching; a future switch to a bump
//! arena is a private refactor on this module that doesn't ripple
//! beyond [`crate::runtime::bbnf::BbnfStructBuilder`].

use bbnf_ir::RuleId;

use crate::runtime::bbnf::value::BbnfValue;

/// Discriminator — the structural shape of a [`BbnfValue::Compound`].
///
/// One arm per compound rule in `grammar/bbnf/bbnf.bbnf` and its
/// imported `expressions.bbnf` / `types.bbnf` companions. Consumers
/// walking the AST match on `kind()` to distinguish (e.g.) an
/// `alternation` from a `concatenation` without re-parsing the source.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BbnfCompoundKind {
    /// `rule = lhs , "=" ?w , rhs ?w , ( ";" | "." )` — top-level rule.
    Rule,
    /// `term = "ε" | … | "{" , rhs ?w , "}"` — the eight-branch term
    /// alternation.
    Term,
    /// `factor = big_comment ? , term ?w , modifier ? , big_comment ?`.
    Factor,
    /// `mapped_factor = factor , ( "->" ?w , … ) ?` — typed-leaf site.
    MappedFactor,
    /// `binary_factor = mapped_factor , ( binary_operators ?w …) *`.
    BinaryFactor,
    /// `concatenation = ( binary_factor ?w , "," ? ) +`.
    Concatenation,
    /// `alternation = ( concatenation ?w , "|" ? ) +`.
    Alternation,
    /// `closure = "|" , identifier , ( "," ?w , identifier ) * , "|" ?w , rhs`.
    Closure,
    /// `rhs = closure | alternation`.
    Rhs,
    /// `lhs = identifier`.
    Lhs,
    /// `call_arg = ( binary_factor ?w , "|" ? ) +`.
    CallArg,
    /// `import_path = "\"" , /…/ , "\""`.
    ImportPath,
    /// `import_items = "{" ?w , ( identifier , … ) ?w , "}"`.
    ImportItems,
    /// `import_directive = "@import" ?w , …`.
    ImportDirective,
    /// `recover_directive = "@recover" ?w , identifier ?w , rhs ?w , …`.
    RecoverDirective,
    /// `pretty_hint = identifier , ( "(" , /[^)]*/ , ")" ) ?`.
    PrettyHint,
    /// `pretty_directive = "@pretty" ?w , …`.
    PrettyDirective,
    /// `ws_directive = "@ws" ?w , regex ?w , …`.
    WsDirective,
    /// `token_directive = "@token" ?w , identifier ?w , …`.
    TokenDirective,
    /// `debug_directive = "@debug" ?w , …`.
    DebugDirective,
    /// `host_directive = "@host" ?w , identifier ?w , …`.
    HostDirective,
    /// `directive = import_directive | recover_directive | …`.
    Directive,
    /// `grammar_item = comment | big_comment | directive | rule`.
    GrammarItem,
    /// `grammar = ( grammar_item ?w ) *` — the document root.
    Grammar,
    // ─── value expression sub-grammar (grammar/bbnf/expressions.bbnf) ───────
    /// `value_expr = value_closure | value_or` — top-level value
    /// expression on the right side of a `->` map arrow.
    ValueExpr,
    /// `value_closure = "|" , value_ident , ( "," ?w , value_ident ) * ,
    /// "|" , value_expr` — first-class value-level closure.
    ValueClosure,
    /// `value_or = value_and , ( "||" ?w , value_and ) *` — logical
    /// disjunction precedence layer.
    ValueOr,
    /// `value_and = value_cmp , ( "&&" ?w , value_cmp ) *` — logical
    /// conjunction precedence layer.
    ValueAnd,
    /// `value_cmp = value_add , ( cmp_op ?w , value_add ) *` —
    /// comparison precedence layer.
    ValueCmp,
    /// `value_add = value_mul , ( add_op ?w , value_mul ) *` —
    /// additive precedence layer.
    ValueAdd,
    /// `value_mul = value_unary , ( mul_op ?w , value_unary ) *` —
    /// multiplicative precedence layer.
    ValueMul,
    /// `value_unary = ( "!" | "-" ) , value_atom | value_atom` — unary
    /// prefix layer.
    ValueUnary,
    /// `value_atom = int_lit | float_lit | bool_lit | string_lit |
    /// value_fn_call | value_input | value_path | "(" , value_expr , ")"`
    /// — the atom alt; under struct-direct projection most atom branches
    /// collapse to their typed leaf payload, so the compound surfaces
    /// only when the atom resolved to a parenthesised sub-expression
    /// or a multi-segment path / fn-call structural body.
    ValueAtom,
    /// `value_path = value_ident , ( "::" , value_ident ) *` —
    /// `::`-separated identifier chain.
    ValuePath,
    /// `value_input = "input" , ( "." , value_ident ) *` — input chain.
    ValueInput,
    /// `value_fn_call = value_path , "(" , ( value_expr , ( "," ,
    /// value_expr ) * ) ? , ")"` — function-call syntax.
    ValueFnCall,
    /// `type_annotation = ":" ?w , type_name` — the typed `: <Type>`
    /// suffix on a `mapped_factor`'s value-expression. AZ-IV.W1.9:
    /// admitted into the alphabet so [`super::super::lower::expression::wrap::lower_mapped_factor`]
    /// can identify the compound by structural kind even when its
    /// span collapses to empty (the `:` literal is consumed without
    /// a Span push under struct-direct projection, and `type_name`'s
    /// alt branches push only `push_branch_tag` + `push_leaf_with_unit`,
    /// leaving the wrapping `type_annotation` compound with no
    /// recoverable byte-span).
    TypeAnnotation,
    /// Catch-all for compound rules not recognised by the
    /// [`BbnfCompoundKind`] alphabet — the layout-resolver consults
    /// the rule name when [`BbnfStructBuilder::begin_compound`]
    /// admits an entry; unrecognised rule names land on `Other` so
    /// the runtime stays exhaustive without panicking.
    Other,
}

impl BbnfCompoundKind {
    /// Resolve a rule id (from
    /// [`bbnf_ir::registry::StructLayout::rule_id`]) to a kind.
    ///
    /// Integer literals match the rule-id allocation in
    /// `crates/core/src/grammar/generated/bbnf.rs`. Ids not in the
    /// alphabet collapse to [`Self::Other`].
    pub fn from_rule_id(rule_id: RuleId) -> Self {
        match rule_id {
            // Top-level grammar surface.
            16 => Self::ImportPath,
            21 => Self::ImportItems,
            22 => Self::PrettyHint,
            23 => Self::TokenDirective,
            24 => Self::DebugDirective,
            25 => Self::HostDirective,
            26 => Self::WsDirective,
            37 => Self::ImportDirective,
            38 => Self::PrettyDirective,
            39 => Self::Alternation,
            40 => Self::CallArg,
            41 => Self::Concatenation,
            42 => Self::Closure,
            43 => Self::Term,
            44 => Self::BinaryFactor,
            46 => Self::Factor,
            47 => Self::MappedFactor,
            48 => Self::Rule,
            49 => Self::RecoverDirective,
            51 => Self::GrammarItem,
            52 => Self::Grammar,
            // Value-expression sub-grammar.
            17 => Self::ValuePath,
            18 => Self::ValueInput,
            19 => Self::TypeAnnotation,
            27 => Self::ValueMul,
            28 => Self::ValueOr,
            29 => Self::ValueAdd,
            30 => Self::ValueCmp,
            31 => Self::ValueAnd,
            32 => Self::ValueClosure,
            33 => Self::ValueFnCall,
            35 => Self::ValueAtom,
            36 => Self::ValueUnary,
            // ValueExpr / Rhs / Lhs / Directive are retained for AST
            // exhaustiveness; their grammar rules collapse to Wrap or
            // compose through typed-leaf surfaces, so no struct
            // layout is emitted.
            _ => Self::Other,
        }
    }
}

/// A compound entry in the arena — child slice plus structural
/// kind discriminator and optional Alt branch tag.
#[derive(Debug, Clone)]
pub struct BbnfCompound<'p> {
    /// Structural shape of this compound (which rule emitted it).
    pub kind: BbnfCompoundKind,
    /// Alt sub-variant index, when the rule is Alt-typed; `None`
    /// otherwise. Recorded via
    /// [`crate::runtime::builder::StructBuilder::push_branch_tag`].
    pub branch_tag: Option<u32>,
    /// AZ-IV.W1.9 — the source byte-offset bounds the compound
    /// actually consumed during parsing, recorded at
    /// `begin_compound` / `end_compound` time via the
    /// [`super::super::builder::StructBuilder`]'s
    /// `record_compound_bounds_start` / `record_compound_bounds_end`
    /// trait extensions.
    ///
    /// Distinct from the leftmost/rightmost Span-leaf union the
    /// `compute_byte_span` walk derives: alt-branch literals
    /// (`@import`, `:`, `(`, `)`) are consumed by byte advance
    /// without a Span push, so the descendant union under-reports
    /// the compound's actual extent. The recorded bounds capture
    /// the parser's view directly.
    ///
    /// `None` when the codegen did not record bounds (legacy
    /// emission paths or non-BBNF builders that do not surface this
    /// extension); `byte_span()` falls back to the descendant
    /// union in that case.
    pub bounds: Option<(u32, u32)>,
    /// Child values, in source order.
    pub children: Vec<BbnfValue<'p>>,
}

impl<'p> Default for BbnfCompound<'p> {
    fn default() -> Self {
        Self {
            kind: BbnfCompoundKind::Other,
            branch_tag: None,
            bounds: None,
            children: Vec::new(),
        }
    }
}

/// Opaque handle for a compound entry.
///
/// The `0` value is reserved for [`Self::EMPTY`]; non-empty handles
/// take values `1..=u32::MAX`. Resolved via [`BbnfArena::compound`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BbnfCompoundId(u32);

impl BbnfCompoundId {
    /// The empty-compound handle. Resolves to a default
    /// [`BbnfCompound`] with no children.
    pub const EMPTY: Self = Self(0);

    /// True iff this is the empty-compound handle.
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

/// Owning slab for compound entries.
///
/// Owns one [`BbnfCompound`] per non-empty handle; resolves handles
/// to compound entries via [`Self::compound`]. The empty-handle
/// constant ([`BbnfCompoundId::EMPTY`]) routes to a stable default
/// without consulting the slab.
#[derive(Debug, Default)]
pub struct BbnfArena<'p> {
    /// Per-handle compound entries. Index = `BbnfCompoundId.0 - 1`.
    compounds: Vec<BbnfCompound<'p>>,
    /// Stable default for the empty-compound resolution branch.
    /// Constructed lazily on first `compound(EMPTY)` query.
    empty: BbnfCompound<'p>,
}

impl<'p> BbnfArena<'p> {
    /// Construct an empty arena. The typical caller is the generated
    /// parse function which then mutably borrows the arena to populate
    /// every compound during parsing.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Construct an arena with pre-sized slab capacity.
    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            compounds: Vec::with_capacity(compounds),
            empty: BbnfCompound {
                kind: BbnfCompoundKind::Other,
                branch_tag: None,
                bounds: None,
                children: Vec::new(),
            },
        }
    }

    /// Push a populated compound entry into the slab and return the
    /// resolving handle. Empty children-vec compounds still allocate
    /// a slab entry (the kind discriminator is load-bearing); the
    /// caller may opt to return [`BbnfCompoundId::EMPTY`] instead
    /// when the compound has zero structural significance.
    #[inline]
    pub fn push_compound(&mut self, compound: BbnfCompound<'p>) -> BbnfCompoundId {
        self.compounds.push(compound);
        let idx = self.compounds.len() as u32;
        BbnfCompoundId(idx)
    }

    /// Resolve a compound handle to its entry. Returns a stable
    /// default (`Other` kind, no children) for [`BbnfCompoundId::EMPTY`].
    #[inline]
    pub fn compound(&self, id: BbnfCompoundId) -> &BbnfCompound<'p> {
        match id.slab_index() {
            None => &self.empty,
            Some(i) => &self.compounds[i],
        }
    }

    /// Number of registered compounds.
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
