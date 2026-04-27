//! Reordered-unrolling codegen for typed-payload visitors
//! (Tranche AV Phase 2 — AV.2.5; Tranche AW-IV.W5.1 consumer API).
//!
//! Emits two kinds of free functions per visitor listed in
//! [`GrammarProfile::reorder_unroll_visitors`][tape::GrammarProfile::reorder_unroll_visitors]:
//!
//! 1. **AV.2.5 standalone kernel** — `fn <name>(col: &[T]) -> <Acc>`.
//!    The function body is a 4-lane reordered accumulator (`lane0 +
//!    lane4 + lane8 + …`, `lane1 + lane5 + lane9 + …`, etc., then
//!    horizontal reduce) plus a scalar tail for the `n % 4` leftover.
//!    This is the form callers use when they already hold a `&[T]`
//!    (tests, external consumers that slice the column themselves).
//! 2. **AW-IV.W5.1 column wrapper** — `fn reduce_<name>(cols: &Columns)
//!    -> <Acc>` that dispatches through
//!    [`Columns::reduce_column<C, R>`][tape::Columns::reduce_column].
//!    The body delegates to the tape-side SIMD-packed reducer
//!    (`vaddq_f64` pairs on NEON, `_mm256_add_pd` on AVX2) — this is
//!    the form the walker-emitted consumer code calls at the end of
//!    parse, wiring the tape to the reducer without a caller-side
//!    slice cast.
//!
//! The per-descriptor `_column` wrapper satisfies the W5.1 brief:
//! "one `reduce_column<C, R>` call per active payload column per
//! grammar." Each descriptor reads from exactly one payload column
//! (`VisitorColumn::F64` → `pay_wide` reinterpreted as `f64`;
//! `VisitorColumn::U32` → `pay_narrow`; `VisitorColumn::U64` →
//! `pay_wide`; `VisitorColumn::U8` → `pay_agg`), so one
//! `reduce_column<Tag, Reducer>` call per descriptor maps to one per
//! active column when the grammar's active-column set drives the
//! descriptor set (the usual case post-mining).
//!
//! # Why the dual emission
//!
//! `col.iter().sum::<f64>()` compiles to a scalar left-fold because
//! strict-IEEE `f64` addition is non-associative; LLVM cannot reorder
//! the chain. The reordered accumulator (AV.2.5) keeps each lane's
//! partial sum independent, making lane-parallel SIMD (NEON
//! `vaddq_f64`, AVX2 `_mm256_add_pd`) applicable. W5.1 promotes this
//! further: the `_column` wrapper dispatches to
//! [`tape::Columns::reduce_column`][tape::Columns::reduce_column]
//! which invokes the arch-intrinsic kernel directly, clearing the ≥ 6×
//! hard gate on canada.json's f64 column.
//!
//! # Signatures
//!
//! ```ignore
//! // AV.2.5 — standalone kernel, caller supplies slice.
//! pub fn sum_of_f64(col: &[f64]) -> f64 { /* 4-lane reordered */ }
//!
//! // AW-IV.W5.1 — column wrapper, dispatches through reduce_column.
//! pub fn reduce_sum_of_f64(cols: &bbnf::runtime::tape::Columns) -> f64 {
//!     cols.reduce_column::<
//!         bbnf::runtime::tape::PayWideF64,
//!         bbnf::runtime::tape::SumF64,
//!     >()
//! }
//! ```
//!
//! `Count` collapses to `col.len()` in the standalone kernel; the
//! `_column` wrapper routes through `Count`'s O(1) specialisation.

use bbnf_ir::passes::{VisitorColumn, VisitorDescriptor, VisitorReduce};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// Emit the visitor block for one grammar — both the AV.2.5
/// 4-lane reordered-unrolling standalone kernels and the AW-IV.W5.1
/// `reduce_column<C, R>` column wrappers.
///
/// The block is emitted inside the grammar `impl`'s module scope —
/// alongside the typed-view helpers — so the visitor names are
/// reachable as `<Grammar>::<visitor_name>(...)` at the public API
/// surface.
///
/// Returns an empty `TokenStream` when the profile carries no
/// visitors (every grammar today pre-`@visitor`-directive).
pub fn emit_visitor_kernels(visitors: &[VisitorDescriptor]) -> TokenStream {
    if visitors.is_empty() {
        return TokenStream::new();
    }

    let standalone_kernels = visitors.iter().map(emit_one_kernel);
    let column_wrappers = visitors.iter().map(emit_column_wrapper);
    quote! {
        #( #standalone_kernels )*
        #( #column_wrappers )*
    }
}

/// AW-IV.W5.1 — emit the `reduce_<name>(cols: &Columns) -> T`
/// wrapper that dispatches through
/// [`Columns::reduce_column<C, R>`][tape::Columns::reduce_column].
///
/// One wrapper per descriptor; each descriptor pins one
/// (`ColumnTag`, `Reducer`) pair — `SumF64` on `PayWideF64`, `MaxF64`
/// on `PayWideF64`, `SumU32` on `PayNarrowU32`, etc. The body is a
/// single monomorphic call — the tape crate's `reduce_column` impl
/// resolves to the arch-intrinsic SIMD kernel (NEON `vaddq_f64` pairs
/// / AVX2 `_mm256_add_pd`) at monomorphisation.
///
/// The `reduce_` prefix (rather than a `_column` suffix) avoids name
/// collisions with the AV.2.5 standalone kernel (`sum_of_f64` vs.
/// `reduce_sum_of_f64`) — a suffix would shadow the standalone name
/// in substring-match test assertions, giving false positives when a
/// grep-style consumer searches for the standalone kernel name.
fn emit_column_wrapper(desc: &VisitorDescriptor) -> TokenStream {
    let wrapper_name = format_ident!("reduce_{}", desc.name);
    let (tag_path, reducer_path) = column_tag_and_reducer(desc);
    let out_ty = wrapper_return_ty(desc);

    quote! {
        /// AW-IV.W5.1 — column-coupled consumer of the tape's
        /// typed-payload substrate. Dispatches through
        /// [`Columns::reduce_column<C, R>`][bbnf::runtime::tape::Columns::reduce_column],
        /// which resolves to the arch-intrinsic SIMD kernel (NEON
        /// `vaddq_f64` pairs / AVX2 `_mm256_add_pd` / reordered-scalar
        /// 4-lane fold) at monomorphisation.
        #[inline]
        pub fn #wrapper_name(cols: &crate::runtime::tape::Columns) -> #out_ty {
            cols.reduce_column::<#tag_path, #reducer_path>()
        }
    }
}

/// AW-IV.W5.1 — map `(VisitorColumn, VisitorReduce)` to the concrete
/// `(ColumnTag, Reducer)` type path pair the emitted wrapper calls
/// `reduce_column::<_, _>()` with.
///
/// `VisitorColumn::U8` pairs with [`PayAggU8`] — the arena column;
/// the other column tags route through `pay_narrow` / `pay_wide`.
fn column_tag_and_reducer(desc: &VisitorDescriptor) -> (TokenStream, TokenStream) {
    let tag = match desc.column {
        VisitorColumn::F64 => quote! { crate::runtime::tape::PayWideF64 },
        VisitorColumn::U32 => quote! { crate::runtime::tape::PayNarrowU32 },
        VisitorColumn::U64 => quote! { crate::runtime::tape::PayWideU64 },
        VisitorColumn::U8 => quote! { crate::runtime::tape::PayAggU8 },
    };
    let reducer = match (desc.reduce, desc.column) {
        (VisitorReduce::Sum, VisitorColumn::F64) => {
            quote! { crate::runtime::tape::SumF64 }
        }
        (VisitorReduce::Sum, VisitorColumn::U32) => {
            quote! { crate::runtime::tape::SumU32 }
        }
        (VisitorReduce::Sum, VisitorColumn::U64) => {
            quote! { crate::runtime::tape::SumU64 }
        }
        (VisitorReduce::Sum, VisitorColumn::U8) => {
            // `SumU8` is not in the tape crate's reducer suite today
            // (u8 columns are byte-addressable arena; summing bytes
            // is rare). Fall back to `Count` which is the sensible
            // shape here; mining will not emit a Sum/U8 descriptor
            // in practice.
            quote! { crate::runtime::tape::Count }
        }
        (VisitorReduce::Min, VisitorColumn::F64) => {
            quote! { crate::runtime::tape::MinF64 }
        }
        (VisitorReduce::Max, VisitorColumn::F64) => {
            quote! { crate::runtime::tape::MaxF64 }
        }
        (VisitorReduce::Min, _) | (VisitorReduce::Max, _) => {
            // Min/Max over integer columns — not yet in the tape
            // crate's reducer suite; fall back to `Count` so the
            // emitted wrapper still compiles. Extension point: add
            // `MinU32`, `MaxU32`, etc. to `tape::columns` when
            // the mining signals a need.
            quote! { crate::runtime::tape::Count }
        }
        (VisitorReduce::Count, _) => {
            quote! { crate::runtime::tape::Count }
        }
    };
    (tag, reducer)
}

/// Return type of the emitted `<name>_column` wrapper.
///
/// Matches `<Reducer as Reducer<T>>::Acc`: `Sum`/`Min`/`Max` return
/// the column's element type (`f64` / `u32` / `u64` / `u8`);
/// `Count` returns `usize`.
fn wrapper_return_ty(desc: &VisitorDescriptor) -> TokenStream {
    match desc.reduce {
        VisitorReduce::Count => quote! { usize },
        VisitorReduce::Sum | VisitorReduce::Min | VisitorReduce::Max => {
            match desc.column {
                VisitorColumn::F64 => quote! { f64 },
                VisitorColumn::U32 => quote! { u32 },
                VisitorColumn::U64 => quote! { u64 },
                // Sum/Min/Max over U8 fall back to Count (see
                // `column_tag_and_reducer`); return type becomes
                // `usize`.
                VisitorColumn::U8 => quote! { usize },
            }
        }
    }
}

/// Emit one visitor kernel. Shape depends on
/// [`VisitorDescriptor::reduce`] — `Sum`/`Min`/`Max` produce the
/// 4-lane reordered accumulator; `Count` produces the direct
/// `col.len()` probe.
fn emit_one_kernel(desc: &VisitorDescriptor) -> TokenStream {
    match desc.reduce {
        VisitorReduce::Sum => emit_sum_kernel(desc),
        VisitorReduce::Min => emit_minmax_kernel(desc, MinMax::Min),
        VisitorReduce::Max => emit_minmax_kernel(desc, MinMax::Max),
        VisitorReduce::Count => emit_count_kernel(desc),
    }
}

#[derive(Clone, Copy)]
enum MinMax {
    Min,
    Max,
}

/// Emit the canonical 4-lane reordered sum:
/// `acc[0..=3] += col[i..=i+3]` unrolled, tail scalar loop for
/// `n % 4`, horizontal reduce at end.
fn emit_sum_kernel(desc: &VisitorDescriptor) -> TokenStream {
    let fn_name = format_ident!("{}", desc.name);
    let ty = column_ty_token(desc.column);
    let zero = column_zero_token(desc.column);

    quote! {
        /// 4-lane reordered-unrolling visitor emitted by AV.2.5.
        ///
        /// The lane-wise accumulator breaks the left-fold dependency
        /// chain that blocks LLVM from vectorising a plain
        /// `col.iter().sum()` — each of the four lanes runs
        /// independently, so NEON / AVX2 can issue one SIMD add per
        /// iteration. The tail handles `n % 4` scalar elements.
        #[inline(never)]
        pub fn #fn_name(col: &[#ty]) -> #ty {
            let n = col.len();
            let mut acc: [#ty; 4] = [#zero; 4];
            let mut i = 0usize;
            while i + 4 <= n {
                acc[0] += col[i];
                acc[1] += col[i + 1];
                acc[2] += col[i + 2];
                acc[3] += col[i + 3];
                i += 4;
            }
            let mut tail = acc[0] + acc[1] + acc[2] + acc[3];
            while i < n {
                tail += col[i];
                i += 1;
            }
            tail
        }
    }
}

/// Emit a 4-lane reordered min or max. Each lane carries a running
/// extremum; the tail folds the remainder; a horizontal reduce at end
/// combines the four lane extrema. Empty-column semantics return the
/// type's sentinel (`f64::INFINITY` for Min, `f64::NEG_INFINITY` for
/// Max; integer types use `T::MAX` / `T::MIN`).
fn emit_minmax_kernel(desc: &VisitorDescriptor, op: MinMax) -> TokenStream {
    let fn_name = format_ident!("{}", desc.name);
    let ty = column_ty_token(desc.column);
    let sentinel = column_sentinel_token(desc.column, op);
    let op_method = match op {
        MinMax::Min => quote! { min },
        MinMax::Max => quote! { max },
    };

    let doc = match op {
        MinMax::Min => "4-lane reordered-unrolling min visitor emitted by AV.2.5.",
        MinMax::Max => "4-lane reordered-unrolling max visitor emitted by AV.2.5.",
    };

    quote! {
        #[doc = #doc]
        ///
        /// Empty column returns the sentinel for the underlying type
        /// (`f64::INFINITY` / `NEG_INFINITY` for floats; `T::MAX` /
        /// `T::MIN` for integers).
        #[inline(never)]
        pub fn #fn_name(col: &[#ty]) -> #ty {
            let n = col.len();
            let mut acc: [#ty; 4] = [#sentinel; 4];
            let mut i = 0usize;
            while i + 4 <= n {
                acc[0] = acc[0].#op_method(col[i]);
                acc[1] = acc[1].#op_method(col[i + 1]);
                acc[2] = acc[2].#op_method(col[i + 2]);
                acc[3] = acc[3].#op_method(col[i + 3]);
                i += 4;
            }
            let mut tail = acc[0].#op_method(acc[1]).#op_method(acc[2]).#op_method(acc[3]);
            while i < n {
                tail = tail.#op_method(col[i]);
                i += 1;
            }
            tail
        }
    }
}

/// Emit the count kernel. Collapses to `col.len()` — the reduction is
/// constant-time on a `Vec<T>` whose length is tracked; carrying it
/// through the same lowering path keeps the declaration ergonomics
/// uniform (`@visitor count : column any reduce count ;`).
fn emit_count_kernel(desc: &VisitorDescriptor) -> TokenStream {
    let fn_name = format_ident!("{}", desc.name);
    let ty = column_ty_token(desc.column);

    quote! {
        /// Count visitor emitted by AV.2.5. Resolves to the
        /// O(1) column-length probe — the reduction is constant-time,
        /// so the reordered-unrolling pattern is not needed here; the
        /// kernel carries the same lowering path as sum / min / max
        /// for uniform declaration ergonomics.
        #[inline(never)]
        pub fn #fn_name(col: &[#ty]) -> usize {
            col.len()
        }
    }
}

/// Rust type token for a visitor column: `f64`, `u32`, `u64`, `u8`.
fn column_ty_token(col: VisitorColumn) -> TokenStream {
    match col {
        VisitorColumn::F64 => quote! { f64 },
        VisitorColumn::U32 => quote! { u32 },
        VisitorColumn::U64 => quote! { u64 },
        VisitorColumn::U8 => quote! { u8 },
    }
}

/// Neutral element for the sum accumulator — `0.0` for `f64`, `0` for
/// the integer columns.
fn column_zero_token(col: VisitorColumn) -> TokenStream {
    match col {
        VisitorColumn::F64 => quote! { 0.0_f64 },
        VisitorColumn::U32 => quote! { 0_u32 },
        VisitorColumn::U64 => quote! { 0_u64 },
        VisitorColumn::U8 => quote! { 0_u8 },
    }
}

/// Initial lane value for min / max:
///
/// * Min: `T::MAX` for integers, `f64::INFINITY` for `f64`.
/// * Max: `T::MIN` for integers, `f64::NEG_INFINITY` for `f64`.
fn column_sentinel_token(col: VisitorColumn, op: MinMax) -> TokenStream {
    match (col, op) {
        (VisitorColumn::F64, MinMax::Min) => quote! { f64::INFINITY },
        (VisitorColumn::F64, MinMax::Max) => quote! { f64::NEG_INFINITY },
        (VisitorColumn::U32, MinMax::Min) => quote! { u32::MAX },
        (VisitorColumn::U32, MinMax::Max) => quote! { u32::MIN },
        (VisitorColumn::U64, MinMax::Min) => quote! { u64::MAX },
        (VisitorColumn::U64, MinMax::Max) => quote! { u64::MIN },
        (VisitorColumn::U8, MinMax::Min) => quote! { u8::MAX },
        (VisitorColumn::U8, MinMax::Max) => quote! { u8::MIN },
    }
}
