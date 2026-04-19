//! Visitor-recognition pass (Tranche AV Phase 2 — AV.2.5).
//!
//! Identifies the visitor-like reductions a grammar admits over its
//! typed payload columns and materialises them as
//! [`VisitorDescriptor`] records. The emitter consumes this list to
//! produce 4-lane reordered-unrolling kernels for each descriptor —
//! `cols.pay_f64.iter().sum::<f64>()` compiles to a scalar left-fold
//! (strict-IEEE `f64` addition is non-associative, LLVM cannot
//! reorder), whereas a reordered accumulator of the shape
//! `lane0 + lane4 + lane8 + …` vectorises cleanly on NEON / AVX2.
//!
//! # Grammar-side syntax
//!
//! AV.md reserves a `@visitor name : column T reduce OP ;` directive
//! for grammar authors to declare the visitors they need. That syntax
//! is not yet wired through the lexer/parser — per AV.md, the codegen
//! lands first and a downstream wave wires the grammar syntax when a
//! consumer needs it. V2.5 therefore ships the pass returning an
//! empty slice for every grammar shipped today; the emitter path is
//! exercised by tests that populate [`GrammarProfile::reorder_unroll_visitors`]
//! directly via the synthetic harness (see `crates/core/tests/visitor_reorder.rs`).
//!
//! [`GrammarProfile::reorder_unroll_visitors`]: tape::GrammarProfile::reorder_unroll_visitors

use crate::types::{GrammarIR, TypeDesc};

/// The reduction operator a visitor performs over its column.
///
/// Each variant maps one-to-one to an emitted kernel shape:
/// [`Sum`](Self::Sum) / [`Min`](Self::Min) / [`Max`](Self::Max) use a
/// 4-lane reordered accumulator; [`Count`](Self::Count) collapses to a
/// column-length probe.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum VisitorReduce {
    /// Scalar sum. Lane-wise `+=` then horizontal add of the lanes.
    Sum,
    /// Scalar minimum. Lane-wise `min` then horizontal min of the lanes.
    Min,
    /// Scalar maximum. Lane-wise `max` then horizontal max of the lanes.
    Max,
    /// Column length. Emits `col.len()` directly — no accumulator needed,
    /// but carried in the same descriptor for codegen uniformity.
    Count,
}

impl VisitorReduce {
    /// Snake-case suffix used in the emitted function name
    /// (`sum_of_f64`, `min_of_u32`, etc.).
    pub const fn name_prefix(self) -> &'static str {
        match self {
            Self::Sum => "sum",
            Self::Min => "min",
            Self::Max => "max",
            Self::Count => "count",
        }
    }
}

/// The typed payload column a visitor reads from.
///
/// Each column corresponds to one of the typed payload vectors in the
/// columnar substrate AV.2.1–AV.2.4 materialises (`pay_f64`, `pay_u32`,
/// `pay_u64`, `pay_u8`). AV.2.5 is substrate-agnostic: the emitter
/// produces kernels taking `&[T]` directly so the V2 substrate wave
/// can wire `tape.columns.pay_f64` into the call site without
/// rewriting the kernel.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum VisitorColumn {
    /// `Vec<f64>` — numeric leaves (JSON numbers, CSS dimensions).
    F64,
    /// `Vec<u32>` — hex colors, packed refs, widened unit enums.
    U32,
    /// `Vec<u64>` — integer literals, packed timestamps.
    U64,
    /// `Vec<u8>` — booleans, small enums, unit discriminants.
    U8,
}

impl VisitorColumn {
    /// Rust type identifier the emitted kernel reads from (`f64`,
    /// `u32`, `u64`, `u8`).
    pub const fn rust_ty(self) -> &'static str {
        match self {
            Self::F64 => "f64",
            Self::U32 => "u32",
            Self::U64 => "u64",
            Self::U8 => "u8",
        }
    }

    /// Try to resolve from a [`TypeDesc`]; `None` for types with no
    /// corresponding typed payload column (aggregates, `Span`, heap
    /// enums, etc.).
    pub fn from_type_desc(td: &TypeDesc) -> Option<Self> {
        match td {
            TypeDesc::F64 => Some(Self::F64),
            TypeDesc::U32 => Some(Self::U32),
            TypeDesc::U64 => Some(Self::U64),
            TypeDesc::U8 | TypeDesc::Bool => Some(Self::U8),
            _ => None,
        }
    }
}

/// One reorder-unroll visitor declared for a grammar.
///
/// The emitter produces one Rust function per descriptor with the
/// shape `pub fn <name>(col: &[<column::rust_ty()>]) -> <out_ty>`
/// where `<out_ty>` is derived from the reduce operator
/// ([`Sum`][VisitorReduce::Sum] / [`Min`][VisitorReduce::Min] /
/// [`Max`][VisitorReduce::Max] return the column type;
/// [`Count`][VisitorReduce::Count] returns `usize`).
///
/// The `name` is the emitted function identifier — conventionally
/// `<reduce>_of_<column>` (`sum_of_f64`, `max_of_u32`). Callers that
/// want a different naming can supply an explicit name; the emitter
/// does not re-mangle.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct VisitorDescriptor {
    /// The emitted function name. Must be a valid Rust identifier.
    pub name: String,
    /// Reduction operator (determines accumulator shape + return type).
    pub reduce: VisitorReduce,
    /// Typed payload column the visitor reads from.
    pub column: VisitorColumn,
}

impl VisitorDescriptor {
    /// Canonical naming: `<reduce>_of_<column_ty>` (e.g. `sum_of_f64`,
    /// `count_of_u32`). Used by the test harness + any grammar-side
    /// default when `@visitor name` is omitted.
    pub fn canonical_name(reduce: VisitorReduce, column: VisitorColumn) -> String {
        format!("{}_of_{}", reduce.name_prefix(), column.rust_ty())
    }

    /// Construct a descriptor with the canonical name.
    pub fn canonical(reduce: VisitorReduce, column: VisitorColumn) -> Self {
        Self {
            name: Self::canonical_name(reduce, column),
            reduce,
            column,
        }
    }
}

/// Mine visitor descriptors from a grammar.
///
/// Returns the visitor list that [`GrammarProfile::reorder_unroll_visitors`]
/// is populated from. The current implementation returns an empty
/// list — the grammar-side `@visitor` directive is not yet wired.
/// Tests populate the list directly via
/// [`VisitorDescriptor::canonical`] + manual profile construction.
///
/// When the directive lands, this function walks
/// `ir.visitor_directives` (or an equivalent IR-side carrier) and
/// produces one descriptor per declaration. The signature is stable
/// across that transition; callers need not change.
///
/// [`GrammarProfile::reorder_unroll_visitors`]: tape::GrammarProfile::reorder_unroll_visitors
pub fn mine_visitors(_ir: &GrammarIR) -> Vec<VisitorDescriptor> {
    // Placeholder: the grammar-side `@visitor` directive is not yet
    // wired through the lexer/parser. Codegen is already exercised via
    // the synthetic harness in `crates/core/tests/visitor_reorder.rs`;
    // when the directive lands, extend this function to walk the
    // parsed descriptors rather than returning the empty slice.
    Vec::new()
}
