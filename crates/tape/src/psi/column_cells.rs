//! Column-write substrate for Stage-B decoding.
//!
//! Stage-B workers use [`ColumnCells`] to write decoded payload bytes
//! into [`Columns::pay_agg`] without holding `&mut Vec<_>` references
//! across thread boundaries. The disjointness invariant — every job's
//! `(arena_offset, width)` pair is unique — is established by Stage A
//! and made explicit through [`ColumnCapacities`] reservation.

use crate::columns::Columns;

use super::job::{PayloadJob, PayloadKind};

/// Arena capacity hint derived from the PSI stream.
///
/// Stage B reserves enough space in [`Columns::pay_agg`] to land
/// every job's decoded payload at its allocated byte offset before
/// any decode runs. The reservation is `resize`-with-zero so
/// subsequent slot writes land in pre-existing memory — no growth
/// during the Stage-B walk.
///
/// AW-III.W1: every payload kind serialises to `pay_agg`; the prior
/// `narrow` / `wide` capacity slots are gone alongside the column
/// emission paths.
#[derive(Debug, Default)]
pub(super) struct ColumnCapacities {
    pub(super) arena: usize,
}

impl ColumnCapacities {
    pub(super) fn reserve(&self, columns: &mut Columns) {
        if self.arena > columns.pay_agg.len() {
            columns.pay_agg.resize(self.arena, 0);
        }
    }
}

/// Raw cell pointers + lengths into a [`Columns::pay_agg`] arena.
///
/// Captured by [`super::PayloadStream::fill_columns`]'s sequential and
/// parallel walks before the walk so workers can write into disjoint
/// byte ranges without holding a `&mut Vec<_>` across thread
/// boundaries. The disjointness invariant — every job's `(arena_offset,
/// width)` pair is unique — comes from Stage A's monotonic per-job
/// allocation, so two writes never target the same byte.
///
/// The struct is `Send + Sync` because raw pointers carry no `Send`
/// inheritance constraint; the safety contract lives at the
/// `unsafe` write site.
///
/// AW-III.W1: the unified arena emission path collapsed the prior
/// per-kind cell trio (narrow / wide / agg) to a single agg pointer —
/// every payload kind serialises into `pay_agg`.
#[derive(Clone, Copy)]
pub(super) struct ColumnCells {
    pay_agg: *mut u8,
    pay_agg_len: usize,
}

// SAFETY: `ColumnCells` carries raw pointers into a `&mut Columns`
// that outlives the `par_chunks` walk — the `fill_parallel` /
// `fill_sequential` callers hold exclusive `&mut Columns` access for
// the entire duration of the closure, and the disjointness invariant
// above guarantees no two concurrent writes touch the same address.
unsafe impl Send for ColumnCells {}
unsafe impl Sync for ColumnCells {}

impl From<&mut Columns> for ColumnCells {
    fn from(columns: &mut Columns) -> Self {
        Self {
            pay_agg: columns.pay_agg.as_mut_ptr(),
            pay_agg_len: columns.pay_agg.len(),
        }
    }
}

/// Decode `job`'s source slice and write the result into the arena
/// (`pay_agg`) slot at `job.column_idx`.
///
/// AW-III.W1 unified arena emission: every payload kind serialises
/// to little-endian bytes in [`Columns::pay_agg`]. Downstream
/// consumers read via `payload_bytes(rec, width)` /
/// `payload_scalar::<T>` keyed by the record's `child_off` (set to
/// `job.column_idx` at Stage A by the walker).
///
/// # Safety
///
/// - `cells` must point at a valid `Columns` substrate that outlives
///   this call — established by the sequential and parallel
///   [`super::PayloadStream`] walks holding a `&mut Columns`
///   throughout.
/// - Every job's `(kind, column_idx)` must be unique across all
///   concurrent invocations — established by Stage A's per-kind
///   monotonic `column_idx` allocation.
/// - `column_idx` must address a pre-allocated slot — established by
///   [`super::PayloadStream`]'s capacity reservation +
///   [`ColumnCapacities::reserve`] sizing the arena to admit every
///   job before the walk begins.
#[inline]
pub(super) unsafe fn write_decoded(job: &PayloadJob, input: &[u8], cells: &ColumnCells) {
    let lo = job.input_lo as usize;
    let hi = job.input_hi as usize;
    let slice = &input[lo..hi];
    let dst_off = job.arena_offset as usize;
    match job.kind {
        PayloadKind::F64 => {
            let s = std::str::from_utf8(slice).unwrap_or("0");
            let bits = s.parse::<f64>().unwrap_or(0.0).to_bits();
            let bytes = bits.to_le_bytes();
            debug_assert!(dst_off + 8 <= cells.pay_agg_len);
            unsafe {
                std::ptr::copy_nonoverlapping(bytes.as_ptr(), cells.pay_agg.add(dst_off), 8);
            }
        }
        PayloadKind::I64 => {
            let s = std::str::from_utf8(slice).unwrap_or("0");
            let bits = s.parse::<i64>().unwrap_or(0) as u64;
            let bytes = bits.to_le_bytes();
            debug_assert!(dst_off + 8 <= cells.pay_agg_len);
            unsafe {
                std::ptr::copy_nonoverlapping(bytes.as_ptr(), cells.pay_agg.add(dst_off), 8);
            }
        }
        PayloadKind::U8 => {
            let value = slice.first().copied().unwrap_or(0);
            debug_assert!(dst_off + 1 <= cells.pay_agg_len);
            unsafe {
                *cells.pay_agg.add(dst_off) = value;
            }
        }
        PayloadKind::Bool => {
            let value: u8 = if slice.eq_ignore_ascii_case(b"true") { 1 } else { 0 };
            debug_assert!(dst_off + 1 <= cells.pay_agg_len);
            unsafe {
                *cells.pay_agg.add(dst_off) = value;
            }
        }
        PayloadKind::HexU32 => {
            let value = parse_hex_u32(slice);
            let bytes = value.to_le_bytes();
            debug_assert!(dst_off + 4 <= cells.pay_agg_len);
            unsafe {
                std::ptr::copy_nonoverlapping(bytes.as_ptr(), cells.pay_agg.add(dst_off), 4);
            }
        }
        PayloadKind::String => {
            // AW-III.W1.A: route through the JSON string-escape
            // decoder kernel — `\n`, `\t`, `\"`, `\\`, `\/`, `\b`,
            // `\f`, `\r`, `\uXXXX`, and `\uD8XX\uDCXX` surrogate
            // pairs all decode into the arena frame. The kernel is
            // general per `decoders/json_string`; the dispatch sits
            // here because `PayloadKind::String` is the lifter's
            // canonical "string with escapes" classification.
            unsafe {
                crate::decoders::json_string::decode_into(
                    slice,
                    cells.pay_agg,
                    dst_off,
                    cells.pay_agg_len,
                );
            }
        }
        PayloadKind::AggregateLarge => {
            debug_assert!(dst_off + slice.len() <= cells.pay_agg_len);
            unsafe {
                std::ptr::copy_nonoverlapping(
                    slice.as_ptr(),
                    cells.pay_agg.add(dst_off),
                    slice.len(),
                );
            }
        }
    }
}

/// Parse a hex colour byte slice (`#rgb` / `#rgba` / `#rrggbb` /
/// `#rrggbbaa`) into a `u32` per CSS Color Level 4. The 3-digit and
/// 4-digit forms expand each nibble: `#abc` → `0xAABBCCFF`,
/// `#abcd` → `0xAABBCCDD`. Missing alpha defaults to `0xFF`.
/// Used by the `PayloadKind::HexU32` decode path.
#[inline]
fn parse_hex_u32(slice: &[u8]) -> u32 {
    let bytes = if slice.first() == Some(&b'#') {
        &slice[1..]
    } else {
        slice
    };
    fn nibble(b: u8) -> Option<u32> {
        Some(match b {
            b'0'..=b'9' => (b - b'0') as u32,
            b'a'..=b'f' => (b - b'a' + 10) as u32,
            b'A'..=b'F' => (b - b'A' + 10) as u32,
            _ => return None,
        })
    }
    let mut nibbles = [0u32; 8];
    let n = bytes.len().min(8);
    for (i, &b) in bytes.iter().take(n).enumerate() {
        match nibble(b) {
            Some(v) => nibbles[i] = v,
            None => return 0,
        }
    }
    match n {
        // #rgb → #rrggbbff
        3 => {
            ((nibbles[0] * 0x11) << 24)
                | ((nibbles[1] * 0x11) << 16)
                | ((nibbles[2] * 0x11) << 8)
                | 0xFF
        }
        // #rgba → #rrggbbaa
        4 => {
            ((nibbles[0] * 0x11) << 24)
                | ((nibbles[1] * 0x11) << 16)
                | ((nibbles[2] * 0x11) << 8)
                | (nibbles[3] * 0x11)
        }
        // #rrggbb → #rrggbbff
        6 => {
            (nibbles[0] << 28)
                | (nibbles[1] << 24)
                | (nibbles[2] << 20)
                | (nibbles[3] << 16)
                | (nibbles[4] << 12)
                | (nibbles[5] << 8)
                | 0xFF
        }
        // #rrggbbaa → verbatim
        8 => {
            (nibbles[0] << 28)
                | (nibbles[1] << 24)
                | (nibbles[2] << 20)
                | (nibbles[3] << 16)
                | (nibbles[4] << 12)
                | (nibbles[5] << 8)
                | (nibbles[6] << 4)
                | nibbles[7]
        }
        _ => 0,
    }
}
