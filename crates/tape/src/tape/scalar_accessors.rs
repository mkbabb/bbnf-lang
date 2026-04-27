//! Read-side payload accessors for `Tape<R>`.
//!
//! Every payload kind the substrate stores has a typed read entry
//! point: inline scalars (`pay_narrow` / `pay_agg`), wide scalars
//! (`pay_wide` / `pay_agg`), variable-length byte and string frames
//! (`pay_agg`), and source-borrowed strings (no arena read at all).

use super::{Tape, TapeRec};

impl<R> Tape<R> {
    // ── Payload accessors ─────────────────────────────────────────

    /// Read an inline-packed scalar payload (≤ 4 bytes) from
    /// `pay_narrow` / `pay_agg`.
    #[inline]
    fn payload_inline<T: Copy>(&self, rec: TapeRec) -> Option<T> {
        debug_assert!(std::mem::size_of::<T>() <= 4);
        if rec.child_off.is_none() {
            return None;
        }
        let n = std::mem::size_of::<T>();
        let bytes = if rec.payload_in_arena() {
            let off = rec.child_off.0 as usize;
            let arena = &self.columns.pay_agg;
            if off + n > arena.len() {
                return None;
            }
            let mut buf = [0u8; 4];
            buf[..n].copy_from_slice(&arena[off..off + n]);
            buf
        } else {
            let rank = rec.child_off.0 as usize;
            if rank >= self.columns.pay_narrow.len() {
                return None;
            }
            self.columns.pay_narrow[rank].to_le_bytes()
        };
        let mut v: std::mem::MaybeUninit<T> = std::mem::MaybeUninit::uninit();
        // SAFETY: `T` is `Copy` and size_of::<T>() <= 4, matching the
        // width of `bytes`. The copy writes size_of::<T>() bytes from
        // a fully-initialised 4-byte buffer.
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                v.as_mut_ptr() as *mut u8,
                n,
            );
            Some(v.assume_init())
        }
    }

    /// Read a wide (8-byte) scalar payload from `pay_wide` (column-
    /// rank path; the [`TapeRec::PAYLOAD_F64_DIRECT_BIT`] flag selects
    /// f64-vs-u64 interpretation at the typed cast below) or `pay_agg`
    /// (arena byte-offset path).
    #[inline]
    fn payload_wide<T: Copy>(&self, rec: TapeRec) -> Option<T> {
        debug_assert!(std::mem::size_of::<T>() == 8);
        if rec.child_off.is_none() {
            return None;
        }
        let raw = if rec.payload_in_arena() {
            let off = rec.child_off.0 as usize;
            let arena = &self.columns.pay_agg;
            if off + 8 > arena.len() {
                return None;
            }
            let arr: [u8; 8] = arena[off..off + 8].try_into().ok()?;
            arr
        } else {
            // B5.W2.4 — the `PAYLOAD_F64_DIRECT_BIT` and the generic
            // `WideScalar` path both project from `pay_wide`; the bit
            // is now an f64-interpretation marker on the unified
            // column rather than a column selector.
            let rank = rec.child_off.0 as usize;
            if rank >= self.columns.pay_wide.len() {
                return None;
            }
            self.columns.pay_wide[rank].to_le_bytes()
        };
        let mut v: std::mem::MaybeUninit<T> = std::mem::MaybeUninit::uninit();
        // SAFETY: `T` is `Copy` of size 8; the source is the full
        // 8-byte LE representation of the stored `u64`.
        unsafe {
            std::ptr::copy_nonoverlapping(
                raw.as_ptr(),
                v.as_mut_ptr() as *mut u8,
                8,
            );
            Some(v.assume_init())
        }
    }

    /// Read an arbitrary scalar payload from the record.
    #[inline]
    pub fn payload_scalar<T: Copy>(&self, rec: TapeRec) -> Option<T> {
        debug_assert!(std::mem::size_of::<T>() <= 8);
        if std::mem::size_of::<T>() <= 4 {
            self.payload_inline::<T>(rec)
        } else {
            self.payload_wide::<T>(rec)
        }
    }

    /// Read an `f64` payload from `pay_wide`.
    #[inline]
    pub fn payload_f64(&self, rec: TapeRec) -> Option<f64> {
        self.payload_wide::<f64>(rec)
    }

    /// Read a `bool` payload from `pay_narrow`.
    #[inline]
    pub fn payload_bool(&self, rec: TapeRec) -> Option<bool> {
        self.payload_inline::<u8>(rec).map(|b| b != 0)
    }

    /// Read an `i8` payload from `pay_narrow`.
    #[inline]
    pub fn payload_i8(&self, rec: TapeRec) -> Option<i8> {
        self.payload_inline::<i8>(rec)
    }

    /// Read a `u8` payload from `pay_narrow`.
    #[inline]
    pub fn payload_u8(&self, rec: TapeRec) -> Option<u8> {
        self.payload_inline::<u8>(rec)
    }

    /// Read an `i16` payload from `pay_narrow`.
    #[inline]
    pub fn payload_i16(&self, rec: TapeRec) -> Option<i16> {
        self.payload_inline::<i16>(rec)
    }

    /// Read a `u16` payload from `pay_narrow`.
    #[inline]
    pub fn payload_u16(&self, rec: TapeRec) -> Option<u16> {
        self.payload_inline::<u16>(rec)
    }

    /// Read an `i32` payload from `pay_narrow`.
    #[inline]
    pub fn payload_i32(&self, rec: TapeRec) -> Option<i32> {
        self.payload_inline::<i32>(rec)
    }

    /// Read a `u32` payload from `pay_narrow`.
    #[inline]
    pub fn payload_u32(&self, rec: TapeRec) -> Option<u32> {
        self.payload_inline::<u32>(rec)
    }

    /// Read an `i64` payload from `pay_wide`.
    #[inline]
    pub fn payload_i64(&self, rec: TapeRec) -> Option<i64> {
        self.payload_wide::<i64>(rec)
    }

    /// Read a `u64` payload from `pay_wide`.
    #[inline]
    pub fn payload_u64(&self, rec: TapeRec) -> Option<u64> {
        self.payload_wide::<u64>(rec)
    }

    /// Read a `Span` payload (lo: u32, hi: u32) from `pay_wide`.
    #[inline]
    #[allow(non_snake_case)]
    pub fn payload_Span(&self, rec: TapeRec) -> Option<(u32, u32)> {
        let raw = self.payload_u64(rec)?;
        let lo = raw as u32;
        let hi = (raw >> 32) as u32;
        Some((lo, hi))
    }

    /// Read a variable-length decoded payload as `&str`.
    #[inline]
    pub fn payload_string(&self, rec: TapeRec) -> Option<&str> {
        let bytes = self.payload_string_bytes(rec)?;
        debug_assert!(
            std::str::from_utf8(bytes).is_ok(),
            "Tape::payload_string: malformed UTF-8 in arena slot at offset {}",
            rec.child_off.0,
        );
        // SAFETY: byte-string callers route UTF-8 through the decoder
        // kernels that enforce well-formed output; the debug_assert
        // round-trips `std::str::from_utf8` in debug builds.
        Some(unsafe { std::str::from_utf8_unchecked(bytes) })
    }

    /// Read a variable-length decoded payload as raw bytes.
    #[inline]
    pub fn payload_string_bytes(&self, rec: TapeRec) -> Option<&[u8]> {
        if rec.child_off.is_none() {
            return None;
        }
        let start = rec.child_off.0 as usize;
        let arena = self.arena();
        if start + 4 > arena.len() {
            return None;
        }
        let len_bytes: [u8; 4] = arena[start..start + 4].try_into().ok()?;
        let len = u32::from_le_bytes(len_bytes) as usize;
        let body_start = start + 4;
        let body_end = body_start + len;
        if body_end > arena.len() {
            return None;
        }
        Some(&arena[body_start..body_end])
    }

    /// Source-aware string accessor — returns the decoded UTF-8 of
    /// a string leaf without touching the arena when the record is
    /// borrow-safe.
    #[inline]
    pub fn payload_string_with_source<'s, 'a: 's, 't: 's>(
        &'t self,
        rec: TapeRec,
        source: &'a [u8],
    ) -> Option<&'s str> {
        if rec.is_string_borrowed() {
            let lo = rec.span_lo as usize + 1;
            let hi = (rec.span_hi as usize).checked_sub(1)?;
            if hi > source.len() || lo > hi {
                return None;
            }
            let bytes = unsafe { source.get_unchecked(lo..hi) };
            debug_assert!(
                std::str::from_utf8(bytes).is_ok(),
                "borrowed string at span [{}, {}) is not UTF-8",
                lo,
                hi,
            );
            // SAFETY: callers route bytes through the JSON decoder
            // kernel which only emits Borrowed for ASCII-clean
            // sources; the debug_assert round-trips std::str::from_utf8
            // in debug builds.
            return Some(unsafe { std::str::from_utf8_unchecked(bytes) });
        }
        self.payload_string(rec)
    }

    /// Borrow the tape's unified payload arena (read-only).
    #[inline]
    pub fn arena(&self) -> &[u8] {
        &self.columns.pay_agg
    }

    /// Read a slice of raw aggregate payload bytes for a record
    /// whose payload was written via [`super::PayloadData::Aggregate`].
    ///
    /// # B5.W0 bonus — `PAYLOAD_IN_ARENA_BIT` precondition assert
    ///
    /// The `debug_assert!` below validates that the leaf's record
    /// kind belongs to the set the arena conventions admit — a record
    /// that reaches this reader without falling into either
    /// convention trips the assert in debug runs (release elides),
    /// enforcing the audit-flagged invariant at zero release cost.
    #[inline]
    pub fn payload_bytes(&self, rec: TapeRec, byte_count: usize) -> Option<&[u8]> {
        if rec.child_off.is_none() {
            return None;
        }
        debug_assert!(
            rec.payload_in_arena()
                || matches!(
                    rec.kind(),
                    crate::TapeKind::Span
                        | crate::TapeKind::KvPair
                        | crate::TapeKind::ShapeRef
                ),
            "payload_bytes precondition: record kind {:?} did not fall into \
             either arena convention (PAYLOAD_IN_ARENA_BIT clear AND kind not \
             in {{Span, KvPair, ShapeRef}}); `child_off` likely names a column \
             rank, not an arena byte offset",
            rec.kind(),
        );
        let start = rec.child_off.0 as usize;
        let arena = self.arena();
        if start + byte_count > arena.len() {
            return None;
        }
        Some(&arena[start..start + byte_count])
    }
}
