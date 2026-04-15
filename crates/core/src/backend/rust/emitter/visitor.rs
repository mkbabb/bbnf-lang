//! Reordered-unrolling codegen for typed-payload visitors
//! (Tranche AV Phase 2 — AV.2.5).
//!
//! Emits one Rust free-function per visitor listed in
//! [`GrammarProfile::reorder_unroll_visitors`][bbnf_tape::GrammarProfile::reorder_unroll_visitors].
//! The function body is a 4-lane reordered accumulator — `lane0 +
//! lane4 + lane8 + …`, `lane1 + lane5 + lane9 + …`, etc., then
//! horizontal reduce — plus a scalar tail for the `n % 4` leftover.
//!
//! # Why
//!
//! `col.iter().sum::<f64>()` compiles to a scalar left-fold because
//! strict-IEEE `f64` addition is non-associative; LLVM cannot reorder
//! the chain. The reordered accumulator keeps each lane's partial sum
//! independent of the others through the whole loop, so lane-parallel
//! SIMD (NEON `fadd.4s`, AVX2 `vaddpd`) is trivially applicable. The
//! same shape serves integer `sum`/`min`/`max` — LLVM will vectorise
//! them too but the four independent chains still help latency.
//!
//! # Signature
//!
//! The emitted kernel takes the typed column as `&[T]` directly. Wave
//! V2's columnar substrate (`tape.columns.pay_f64`) slots in at the
//! call site without changing the kernel — the caller writes
//! `sum_of_f64(&tape.columns.pay_f64)`. For V2.5's landing state we
//! exercise the kernel on a synthetic `Vec<f64>` (tests in
//! `crates/core/tests/visitor_reorder.rs`) to prove the emitted
//! pattern is scalar-left-fold-free.
//!
//! # Canonical form
//!
//! For `sum_of_f64`:
//!
//! ```ignore
//! pub fn sum_of_f64(col: &[f64]) -> f64 {
//!     let n = col.len();
//!     let mut acc = [0.0_f64; 4];
//!     let mut i = 0;
//!     while i + 4 <= n {
//!         acc[0] += col[i];
//!         acc[1] += col[i + 1];
//!         acc[2] += col[i + 2];
//!         acc[3] += col[i + 3];
//!         i += 4;
//!     }
//!     let mut tail = acc[0] + acc[1] + acc[2] + acc[3];
//!     while i < n {
//!         tail += col[i];
//!         i += 1;
//!     }
//!     tail
//! }
//! ```
//!
//! `Count` collapses to `col.len()` and skips the accumulator — the
//! reduction is constant-time on a `Vec<T>` whose length is already
//! tracked.

use bbnf_ir::passes::{VisitorColumn, VisitorDescriptor, VisitorReduce};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// Emit the 4-lane reordered-unrolling visitor block for one grammar.
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

    let kernels = visitors.iter().map(emit_one_kernel);
    quote! {
        #( #kernels )*
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
