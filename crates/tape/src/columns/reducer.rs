//! [`Reducer`] trait + 6 reducers (SumF64 / MinF64 / MaxF64 / SumU32 /
//! SumU64 / Count).
//!
//! Captures identity + fold + combine + slice driver. The default
//! [`Reducer::reduce_slice`] is the 4-lane reordered scalar fold;
//! [`SumF64`] overrides to invoke the packed-SIMD kernel in
//! [`crate::simd::sum_f64`] (NEON / AVX2 / scalar fallback).


/// A reduction operator over `&[T]`.
///
/// Captures the four facts the driver needs to fold a column into a
/// single accumulator:
///
/// 1. [`Acc`](Reducer::Acc) — the accumulator type (usually `T`;
///    sometimes wider, e.g. `usize` for `Count`).
/// 2. [`IDENT`](Reducer::IDENT) — the identity value (`0.0` for Sum,
///    `T::MAX` for Min, `T::MIN` for Max, `0` for Count).
/// 3. [`fold`](Reducer::fold) — the binary fold step.
/// 4. [`combine`](Reducer::combine) — the horizontal reduce.
/// 5. [`reduce_slice`](Reducer::reduce_slice) — the slice driver;
///    default implementation is the 4-lane reordered scalar fold,
///    overridable per `(Reducer, T)` pair.
pub trait Reducer<T: Copy> {
    /// The accumulator type.
    type Acc: Copy;
    /// The identity value — the accumulator's starting state.
    const IDENT: Self::Acc;
    /// Fold one element into a lane accumulator.
    fn fold(acc: Self::Acc, x: T) -> Self::Acc;
    /// Combine two lane accumulators horizontally.
    fn combine(a: Self::Acc, b: Self::Acc) -> Self::Acc;

    /// Drive the reducer over a slice. Default implementation is the
    /// 4-lane reordered scalar fold.
    #[inline]
    fn reduce_slice(col: &[T]) -> Self::Acc {
        let n = col.len();
        let mut acc: [Self::Acc; 4] = [Self::IDENT; 4];
        let mut i = 0usize;
        while i + 4 <= n {
            acc[0] = Self::fold(acc[0], col[i]);
            acc[1] = Self::fold(acc[1], col[i + 1]);
            acc[2] = Self::fold(acc[2], col[i + 2]);
            acc[3] = Self::fold(acc[3], col[i + 3]);
            i += 4;
        }
        let mut tail = Self::combine(
            Self::combine(acc[0], acc[1]),
            Self::combine(acc[2], acc[3]),
        );
        while i < n {
            tail = Self::fold(tail, col[i]);
            i += 1;
        }
        tail
    }
}

/// Sum reducer over `f64`. Identity `0.0`; fold `+`; combine `+`.
///
/// Overrides [`Reducer::reduce_slice`] to invoke the packed-SIMD
/// kernel in [`crate::simd::sum_f64`] — `vaddq_f64` pairs on NEON,
/// `_mm256_add_pd` on AVX2, 4-lane reordered scalar fold otherwise.
pub struct SumF64;

impl Reducer<f64> for SumF64 {
    type Acc = f64;
    const IDENT: f64 = 0.0;

    #[inline]
    fn fold(acc: f64, x: f64) -> f64 {
        acc + x
    }

    #[inline]
    fn combine(a: f64, b: f64) -> f64 {
        a + b
    }

    #[inline]
    fn reduce_slice(col: &[f64]) -> f64 {
        crate::simd::sum_f64(col)
    }
}

/// Min reducer over `f64`. Identity `f64::INFINITY`; fold `f64::min`;
/// combine `f64::min`.
pub struct MinF64;

impl Reducer<f64> for MinF64 {
    type Acc = f64;
    const IDENT: f64 = f64::INFINITY;

    #[inline]
    fn fold(acc: f64, x: f64) -> f64 {
        acc.min(x)
    }

    #[inline]
    fn combine(a: f64, b: f64) -> f64 {
        a.min(b)
    }
}

/// Max reducer over `f64`. Identity `f64::NEG_INFINITY`; fold
/// `f64::max`; combine `f64::max`.
pub struct MaxF64;

impl Reducer<f64> for MaxF64 {
    type Acc = f64;
    const IDENT: f64 = f64::NEG_INFINITY;

    #[inline]
    fn fold(acc: f64, x: f64) -> f64 {
        acc.max(x)
    }

    #[inline]
    fn combine(a: f64, b: f64) -> f64 {
        a.max(b)
    }
}

/// Sum reducer over `u32`. Wrapping arithmetic so overflow is
/// saturating-defined rather than a panic site.
pub struct SumU32;

impl Reducer<u32> for SumU32 {
    type Acc = u32;
    const IDENT: u32 = 0;

    #[inline]
    fn fold(acc: u32, x: u32) -> u32 {
        acc.wrapping_add(x)
    }

    #[inline]
    fn combine(a: u32, b: u32) -> u32 {
        a.wrapping_add(b)
    }
}

/// Sum reducer over `u64`. Wrapping arithmetic.
pub struct SumU64;

impl Reducer<u64> for SumU64 {
    type Acc = u64;
    const IDENT: u64 = 0;

    #[inline]
    fn fold(acc: u64, x: u64) -> u64 {
        acc.wrapping_add(x)
    }

    #[inline]
    fn combine(a: u64, b: u64) -> u64 {
        a.wrapping_add(b)
    }
}

/// Count reducer. Collapses to `col.len()` — O(1) on every column
/// since the `Vec<T>` carries its length.
pub struct Count;

impl<T: Copy> Reducer<T> for Count {
    type Acc = usize;
    const IDENT: usize = 0;

    #[inline]
    fn fold(acc: usize, _x: T) -> usize {
        acc + 1
    }

    #[inline]
    fn combine(a: usize, b: usize) -> usize {
        a + b
    }

    /// O(1) specialisation — `col.len()`.
    #[inline]
    fn reduce_slice(col: &[T]) -> usize {
        col.len()
    }
}
