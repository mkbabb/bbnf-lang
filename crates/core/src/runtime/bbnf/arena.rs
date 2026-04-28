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
    /// Catch-all for compound rules not recognised by the
    /// [`BbnfCompoundKind`] alphabet — the layout-resolver consults
    /// the rule name when [`BbnfStructBuilder::begin_compound`]
    /// admits an entry; unrecognised rule names land on `Other` so
    /// the runtime stays exhaustive without panicking.
    Other,
}

impl BbnfCompoundKind {
    /// Resolve a rule name (from
    /// [`bbnf_ir::registry::StructLayout::rule_name`]) to a kind.
    /// Names not in the alphabet collapse to [`Self::Other`].
    pub fn from_rule_name(name: &str) -> Self {
        match name {
            "rule" => Self::Rule,
            "term" => Self::Term,
            "factor" => Self::Factor,
            "mapped_factor" => Self::MappedFactor,
            "binary_factor" => Self::BinaryFactor,
            "concatenation" => Self::Concatenation,
            "alternation" => Self::Alternation,
            "closure" => Self::Closure,
            "rhs" => Self::Rhs,
            "lhs" => Self::Lhs,
            "call_arg" => Self::CallArg,
            "import_path" => Self::ImportPath,
            "import_items" => Self::ImportItems,
            "import_directive" => Self::ImportDirective,
            "recover_directive" => Self::RecoverDirective,
            "pretty_hint" => Self::PrettyHint,
            "pretty_directive" => Self::PrettyDirective,
            "ws_directive" => Self::WsDirective,
            "token_directive" => Self::TokenDirective,
            "debug_directive" => Self::DebugDirective,
            "host_directive" => Self::HostDirective,
            "directive" => Self::Directive,
            "grammar_item" => Self::GrammarItem,
            "grammar" => Self::Grammar,
            // value-expression sub-grammar.
            "value_expr" => Self::ValueExpr,
            "value_closure" => Self::ValueClosure,
            "value_or" => Self::ValueOr,
            "value_and" => Self::ValueAnd,
            "value_cmp" => Self::ValueCmp,
            "value_add" => Self::ValueAdd,
            "value_mul" => Self::ValueMul,
            "value_unary" => Self::ValueUnary,
            "value_atom" => Self::ValueAtom,
            "value_path" => Self::ValuePath,
            "value_input" => Self::ValueInput,
            "value_fn_call" => Self::ValueFnCall,
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
    /// Child values, in source order.
    pub children: Vec<BbnfValue<'p>>,
}

impl<'p> Default for BbnfCompound<'p> {
    fn default() -> Self {
        Self {
            kind: BbnfCompoundKind::Other,
            branch_tag: None,
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
}
