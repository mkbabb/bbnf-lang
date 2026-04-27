//! Packed-SIMD kernels for the [`crate::columns::Reducer`] hot paths.
//!
//! The entry points dispatch to per-arch SIMD (NEON `vaddq_f64` pairs
//! on aarch64, AVX2 `_mm256_add_pd` on x86_64) or the portable 4-lane
//! reordered scalar fold on other targets. The public API is
//! arch-agnostic; callers use [`crate::columns::Reducer::reduce_slice`]
//! which resolves to the correct kernel at monomorphisation time.

/// Packed-SIMD f64 sum.
#[inline]
pub fn sum_f64(col: &[f64]) -> f64 {
    #[cfg(target_arch = "aarch64")]
    {
        sum_f64_neon(col)
    }
    #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
    {
        // SAFETY: the cfg above gates the call on compile-time
        // `target_feature = "avx2"`.
        unsafe { sum_f64_avx2(col) }
    }
    #[cfg(not(any(
        target_arch = "aarch64",
        all(target_arch = "x86_64", target_feature = "avx2"),
    )))]
    {
        sum_f64_scalar_4lane(col)
    }
}

/// NEON kernel.
#[cfg(target_arch = "aarch64")]
#[inline]
fn sum_f64_neon(col: &[f64]) -> f64 {
    use core::arch::aarch64::*;
    let n = col.len();
    // SAFETY: NEON intrinsics require aarch64 + neon, both of which
    // are gated by `#[cfg(target_arch = "aarch64")]` + NEON being
    // a baseline feature of aarch64-*-* targets.
    unsafe {
        let ptr = col.as_ptr();
        let mut acc0 = vdupq_n_f64(0.0);
        let mut acc1 = vdupq_n_f64(0.0);
        let mut acc2 = vdupq_n_f64(0.0);
        let mut acc3 = vdupq_n_f64(0.0);
        let mut i = 0usize;
        while i + 8 <= n {
            let v0 = vld1q_f64(ptr.add(i));
            let v1 = vld1q_f64(ptr.add(i + 2));
            let v2 = vld1q_f64(ptr.add(i + 4));
            let v3 = vld1q_f64(ptr.add(i + 6));
            acc0 = vaddq_f64(acc0, v0);
            acc1 = vaddq_f64(acc1, v1);
            acc2 = vaddq_f64(acc2, v2);
            acc3 = vaddq_f64(acc3, v3);
            i += 8;
        }
        let merged_lo = vaddq_f64(acc0, acc1);
        let merged_hi = vaddq_f64(acc2, acc3);
        let merged = vaddq_f64(merged_lo, merged_hi);
        let mut tail = vgetq_lane_f64(merged, 0) + vgetq_lane_f64(merged, 1);
        while i < n {
            tail += *ptr.add(i);
            i += 1;
        }
        tail
    }
}

/// AVX2 kernel.
#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
#[target_feature(enable = "avx2")]
#[inline]
unsafe fn sum_f64_avx2(col: &[f64]) -> f64 {
    use core::arch::x86_64::*;
    let n = col.len();
    let ptr = col.as_ptr();
    let mut acc0 = _mm256_setzero_pd();
    let mut acc1 = _mm256_setzero_pd();
    let mut i = 0usize;
    while i + 8 <= n {
        let v0 = _mm256_loadu_pd(ptr.add(i));
        let v1 = _mm256_loadu_pd(ptr.add(i + 4));
        acc0 = _mm256_add_pd(acc0, v0);
        acc1 = _mm256_add_pd(acc1, v1);
        i += 8;
    }
    let merged = _mm256_add_pd(acc0, acc1);
    let hi = _mm256_extractf128_pd::<1>(merged);
    let lo = _mm256_castpd256_pd128(merged);
    let sum2 = _mm_add_pd(lo, hi);
    let shuf = _mm_unpackhi_pd(sum2, sum2);
    let reduced = _mm_add_sd(sum2, shuf);
    let mut tail = _mm_cvtsd_f64(reduced);
    while i < n {
        tail += *ptr.add(i);
        i += 1;
    }
    tail
}

/// Reordered-scalar 4-lane fold for targets without NEON / AVX2.
#[cfg(not(any(
    target_arch = "aarch64",
    all(target_arch = "x86_64", target_feature = "avx2"),
)))]
#[inline]
fn sum_f64_scalar_4lane(col: &[f64]) -> f64 {
    let n = col.len();
    let mut acc: [f64; 4] = [0.0; 4];
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
