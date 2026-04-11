//! `TapeKind` — classification tag for [`TapeRec`].
//!
//! The `kind` byte is the view layer's primary dispatch axis. Codegen
//! assigns a [`TapeKind`] per grammar-rule projection shape; view
//! accessors match on `kind()` to decide how to decode.
//!
//! [`TapeRec`]: crate::TapeRec

/// Classification tag for a tape record.
///
/// The variants partition into five groups:
///
/// 1. **Leaf shapes** — `Span`, `Epsilon`, `Literal`, `Regex` — carry
///    their result directly in `span_lo`/`span_hi`, no children.
/// 2. **Compound shapes** — `Seq`, `Alt`, `Repeat`, `Rule` — point to
///    a contiguous child run via `child_off`.
/// 3. **Annotation shapes** — `VariantTag`, `SubVariant`, `MapValue` —
///    decorate the sibling record with codegen-assigned metadata.
/// 4. **Recovery marker** — `Recovered` — sentinel leaf pushed by
///    `@recover` arms when a recovery path fires. Views expose this
///    as `.is_recovered()`.
/// 5. **Sentinel** — `None` — represents parse failure / absence.
///
/// Codegen may assign additional domain-specific tags by extending
/// this enum (e.g., `TapeKind::JsonObject`, `TapeKind::CssDeclaration`).
/// The generic shape above is the starting set that every grammar
/// produces; specialised tags are opt-in via the tape emitter's
/// classification pass.
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TapeKind {
    /// Sentinel absence — used by accessors for failed parses and
    /// out-of-range child lookups. The tape builder itself never
    /// emits `None` records; the sentinel is a view-layer-only value.
    None = 0,

    /// Leaf Span — the entire record is `(span_lo, span_hi)` with no
    /// children. Emitted for every `Span`-projected rule and every
    /// literal / regex match in the parser's hot loop.
    Span = 1,

    /// Leaf Epsilon — matches the empty string. `span_lo == span_hi`.
    Epsilon = 2,

    /// Leaf Literal — a specific literal matched. The `flags` byte's
    /// variant index identifies WHICH literal when the rule is an
    /// all-literal Alt (useful for dispatch without re-parsing the
    /// span bytes).
    Literal = 3,

    /// Leaf Regex — a regex matched. `flags` carries the variant
    /// index for heterogeneous Alt branches.
    Regex = 4,

    /// Compound Seq — children are a contiguous run in pre-order.
    Seq = 5,

    /// Compound Alt — exactly one child (the chosen branch). The
    /// branch index lives in `flags`.
    Alt = 6,

    /// Compound Repeat — children are the matched repetitions in
    /// order. `span_hi - span_lo` covers the entire run.
    Repeat = 7,

    /// Compound Rule entry — marks the top of a rule's output. The
    /// parser pushes one `Rule` record per call to a rule function;
    /// the view layer keys on this to identify rule boundaries.
    Rule = 8,

    /// Annotation — the immediately-following record's variant
    /// index has been overridden by codegen (sub-variant coercion
    /// for heterogeneous Alts). Rarely emitted; present so codegen
    /// has a way to decorate a leaf with a different variant than
    /// its shape would imply.
    VariantTag = 9,

    /// Annotation — the immediately-following record is a mapped
    /// value (e.g., `-> f64`). The tape builder emits this tag so
    /// the view layer knows to call the mapper fn path stored in
    /// per-rule metadata rather than returning the raw span.
    MapValue = 10,

    /// Compound TokenDispatch — carries a token lookup result + the
    /// matched arm's continuation. Used for `@token` fused rules.
    TokenDispatch = 11,

    /// Recovery marker — pushed by generated parsers when an
    /// `@recover` arm fires. Replaces the eager-AST `Recovered`
    /// enum variant. The record's span covers the recovered input
    /// region; `flags` carries the recovering rule's variant index.
    /// Views expose this as `.is_recovered()`.
    Recovered = 12,

    /// Reserved for future grammar-specific shapes (structural bitmap
    /// dispatch, keyword PHF lookup, etc.). Codegen may assign tags
    /// in the range 64..=255 without colliding with the shared
    /// vocabulary above.
    Reserved = 13,
}

impl TapeKind {
    /// True iff this kind represents a leaf (no children to walk).
    #[inline]
    pub fn is_leaf(self) -> bool {
        matches!(
            self,
            TapeKind::Span | TapeKind::Epsilon | TapeKind::Literal | TapeKind::Regex,
        )
    }

    /// True iff this kind represents a compound (carries `child_off`).
    #[inline]
    pub fn is_compound(self) -> bool {
        matches!(
            self,
            TapeKind::Seq
                | TapeKind::Alt
                | TapeKind::Repeat
                | TapeKind::Rule
                | TapeKind::TokenDispatch,
        )
    }

    /// Human-readable name for diagnostics / debugging.
    pub fn name(self) -> &'static str {
        match self {
            TapeKind::None => "none",
            TapeKind::Span => "span",
            TapeKind::Epsilon => "epsilon",
            TapeKind::Literal => "literal",
            TapeKind::Regex => "regex",
            TapeKind::Seq => "seq",
            TapeKind::Alt => "alt",
            TapeKind::Repeat => "repeat",
            TapeKind::Rule => "rule",
            TapeKind::VariantTag => "variant_tag",
            TapeKind::MapValue => "map_value",
            TapeKind::TokenDispatch => "token_dispatch",
            TapeKind::Recovered => "recovered",
            TapeKind::Reserved => "reserved",
        }
    }

    /// True iff this kind marks a recovery region pushed by an
    /// `@recover` arm. Views expose this as `.is_recovered()`.
    #[inline]
    pub fn is_recovered(self) -> bool {
        matches!(self, TapeKind::Recovered)
    }
}
