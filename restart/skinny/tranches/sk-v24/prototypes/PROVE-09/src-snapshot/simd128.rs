//! Stable 128-bit byte-lane SIMD — the de-nightly replacement for
//! `std::simd::prelude` (`#![feature(portable_simd)]`).
//!
//! The scanners use one narrow, uniform vocabulary over `u8x16`:
//! `splat` / `from_slice` / `from_array` / `to_array`, `& | >>`,
//! `simd_eq` / `simd_ne`, `to_bitmask` / `any` / `all`, and the dynamic
//! table shuffle `swizzle_dyn`. This module exposes exactly that surface
//! so each scanner converts by swapping its `use` line — no call-site churn.
//!
//! * **aarch64** → `core::arch::aarch64` NEON intrinsics. The lowering is
//!   the same instruction stream `std::simd` emitted (`ld1`/`cmeq`/`tbl`/
//!   `and`/`ushr`/`ushl`), so the port is codegen-neutral, not a slowdown.
//! * **other arches** → a `[u8; 16]` scalar parity oracle, so the crate
//!   still builds everywhere (no x86 intrinsics anywhere).
//!
//! `to_bitmask()` matches `std::simd::Mask::to_bitmask`: lane `i` → bit `i`.

#![allow(non_camel_case_types)]

#[cfg(target_arch = "aarch64")]
mod imp {
    use core::arch::aarch64::*;

    /// 16 packed `u8` lanes over a NEON `uint8x16_t`.
    #[derive(Clone, Copy)]
    #[repr(transparent)]
    pub struct u8x16(pub uint8x16_t);

    /// Per-lane boolean result of a comparison: `0xFF` set, `0x00` clear.
    #[derive(Clone, Copy)]
    #[repr(transparent)]
    pub struct mask8x16(uint8x16_t);

    impl u8x16 {
        #[inline(always)]
        pub fn splat(v: u8) -> Self {
            unsafe { u8x16(vdupq_n_u8(v)) }
        }

        /// Load the first 16 bytes of `s`. Caller guarantees `s.len() >= 16`
        /// (every call site slices exactly 16 bytes or reads a padded view).
        #[inline(always)]
        pub fn from_slice(s: &[u8]) -> Self {
            debug_assert!(s.len() >= 16);
            unsafe { u8x16(vld1q_u8(s.as_ptr())) }
        }

        #[inline(always)]
        pub fn from_array(a: [u8; 16]) -> Self {
            unsafe { u8x16(vld1q_u8(a.as_ptr())) }
        }

        #[inline(always)]
        pub fn to_array(self) -> [u8; 16] {
            let mut a = [0u8; 16];
            unsafe { vst1q_u8(a.as_mut_ptr(), self.0) };
            a
        }

        #[inline(always)]
        pub fn simd_eq(self, o: Self) -> mask8x16 {
            unsafe { mask8x16(vceqq_u8(self.0, o.0)) }
        }

        #[inline(always)]
        pub fn simd_ne(self, o: Self) -> mask8x16 {
            unsafe { mask8x16(vmvnq_u8(vceqq_u8(self.0, o.0))) }
        }

        /// Dynamic byte shuffle (`tbl`): lane `i` ← `self[idx[i]]`, and
        /// `0` when `idx[i] >= 16` — matching `Simd::swizzle_dyn`.
        #[inline(always)]
        pub fn swizzle_dyn(self, idx: Self) -> Self {
            unsafe { u8x16(vqtbl1q_u8(self.0, idx.0)) }
        }
    }

    impl core::ops::BitAnd for u8x16 {
        type Output = Self;
        #[inline(always)]
        fn bitand(self, o: Self) -> Self {
            unsafe { u8x16(vandq_u8(self.0, o.0)) }
        }
    }

    impl core::ops::Shr<i32> for u8x16 {
        type Output = Self;
        /// Logical right shift by a runtime lane count (`ushl` by `-n`).
        #[inline(always)]
        fn shr(self, n: i32) -> Self {
            unsafe { u8x16(vshlq_u8(self.0, vdupq_n_s8((-n) as i8))) }
        }
    }

    impl mask8x16 {
        /// Pack lane `i`'s boolean into bit `i` (`std::simd` `to_bitmask`).
        #[inline(always)]
        pub fn to_bitmask(self) -> u64 {
            const BITS: [u8; 16] = [1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128];
            unsafe {
                let masked = vandq_u8(self.0, vld1q_u8(BITS.as_ptr()));
                let lo = vaddv_u8(vget_low_u8(masked)) as u64;
                let hi = vaddv_u8(vget_high_u8(masked)) as u64;
                lo | (hi << 8)
            }
        }

        #[inline(always)]
        pub fn any(self) -> bool {
            unsafe { vmaxvq_u8(self.0) != 0 }
        }

        #[inline(always)]
        pub fn all(self) -> bool {
            unsafe { vminvq_u8(self.0) == 0xFF }
        }
    }

    impl core::ops::BitOr for mask8x16 {
        type Output = Self;
        #[inline(always)]
        fn bitor(self, o: Self) -> Self {
            unsafe { mask8x16(vorrq_u8(self.0, o.0)) }
        }
    }

    impl core::ops::BitAnd for mask8x16 {
        type Output = Self;
        #[inline(always)]
        fn bitand(self, o: Self) -> Self {
            unsafe { mask8x16(vandq_u8(self.0, o.0)) }
        }
    }

    impl core::ops::Not for mask8x16 {
        type Output = Self;
        #[inline(always)]
        fn not(self) -> Self {
            unsafe { mask8x16(vmvnq_u8(self.0)) }
        }
    }
}

#[cfg(not(target_arch = "aarch64"))]
mod imp {
    // Scalar parity oracle — identical semantics, no target intrinsics.
    #[derive(Clone, Copy)]
    pub struct u8x16([u8; 16]);
    #[derive(Clone, Copy)]
    pub struct mask8x16([bool; 16]);

    impl u8x16 {
        #[inline(always)]
        pub fn splat(v: u8) -> Self {
            u8x16([v; 16])
        }
        #[inline(always)]
        pub fn from_slice(s: &[u8]) -> Self {
            let mut a = [0u8; 16];
            a.copy_from_slice(&s[..16]);
            u8x16(a)
        }
        #[inline(always)]
        pub fn from_array(a: [u8; 16]) -> Self {
            u8x16(a)
        }
        #[inline(always)]
        pub fn to_array(self) -> [u8; 16] {
            self.0
        }
        #[inline(always)]
        pub fn simd_eq(self, o: Self) -> mask8x16 {
            let mut m = [false; 16];
            for i in 0..16 {
                m[i] = self.0[i] == o.0[i];
            }
            mask8x16(m)
        }
        #[inline(always)]
        pub fn simd_ne(self, o: Self) -> mask8x16 {
            let mut m = [false; 16];
            for i in 0..16 {
                m[i] = self.0[i] != o.0[i];
            }
            mask8x16(m)
        }
        #[inline(always)]
        pub fn swizzle_dyn(self, idx: Self) -> Self {
            let mut a = [0u8; 16];
            for i in 0..16 {
                let j = idx.0[i];
                a[i] = if j < 16 { self.0[j as usize] } else { 0 };
            }
            u8x16(a)
        }
    }

    impl core::ops::BitAnd for u8x16 {
        type Output = Self;
        #[inline(always)]
        fn bitand(self, o: Self) -> Self {
            let mut a = [0u8; 16];
            for i in 0..16 {
                a[i] = self.0[i] & o.0[i];
            }
            u8x16(a)
        }
    }
    impl core::ops::Shr<i32> for u8x16 {
        type Output = Self;
        #[inline(always)]
        fn shr(self, n: i32) -> Self {
            let mut a = [0u8; 16];
            for i in 0..16 {
                a[i] = self.0[i] >> n;
            }
            u8x16(a)
        }
    }

    impl mask8x16 {
        #[inline(always)]
        pub fn to_bitmask(self) -> u64 {
            let mut b = 0u64;
            for i in 0..16 {
                if self.0[i] {
                    b |= 1 << i;
                }
            }
            b
        }
        #[inline(always)]
        pub fn any(self) -> bool {
            self.0.iter().any(|&x| x)
        }
        #[inline(always)]
        pub fn all(self) -> bool {
            self.0.iter().all(|&x| x)
        }
    }
    impl core::ops::BitOr for mask8x16 {
        type Output = Self;
        #[inline(always)]
        fn bitor(self, o: Self) -> Self {
            let mut m = [false; 16];
            for i in 0..16 {
                m[i] = self.0[i] | o.0[i];
            }
            mask8x16(m)
        }
    }
    impl core::ops::BitAnd for mask8x16 {
        type Output = Self;
        #[inline(always)]
        fn bitand(self, o: Self) -> Self {
            let mut m = [false; 16];
            for i in 0..16 {
                m[i] = self.0[i] & o.0[i];
            }
            mask8x16(m)
        }
    }
    impl core::ops::Not for mask8x16 {
        type Output = Self;
        #[inline(always)]
        fn not(self) -> Self {
            let mut m = [false; 16];
            for i in 0..16 {
                m[i] = !self.0[i];
            }
            mask8x16(m)
        }
    }
}

pub use imp::{mask8x16, u8x16};
