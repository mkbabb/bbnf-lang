//! 6C resident-bytes probe — raw-C-ABI wasm surface over the SHIPPING
//! `simd-scan` v128 structural kernel (`simd_scan::scan_structural`,
//! which lowers to `simd_scan::wasm::scan` on `wasm32` + `simd128`).
//!
//! The whole point of the probe: expose the kernel over bytes that are
//! ALREADY RESIDENT in wasm linear memory, so a Node/browser harness can
//! time `scan` with NO per-call `TextEncoder`/marshal inside the timed
//! region. The harness realises the three arms of the brief:
//!
//!   (a) resident   — write bytes into `input_ptr()` ONCE (outside timing),
//!                    then time `scan_resident(off, len)` alone.
//!   (b) marshalled — time `TextEncoder.encode(str)` + copy-to-arena +
//!                    `scan_resident` (the PROTO-6C browser lane, anchor).
//!   (c) scan.ts    — TS-native scalar LUT loop (in the harness).
//!
//! Arms (a) and (b) differ ONLY in whether the copy is inside the timed
//! region, so a single wasm export (`scan_resident`) serves both — the
//! marshal is pure JS. Parsimony: the smallest surface that answers the
//! question.

#![cfg(target_arch = "wasm32")]

use simd_scan::{StructuralAlphabet, scan_structural};

/// Linear-memory arena the harness fills once and scans in place.
/// 4 MiB covers twitter.json (631 KB) and every KB-scale batch.
const CAP: usize = 4 * 1024 * 1024;
static mut ARENA: [u8; CAP] = [0u8; CAP];

/// JSON structural alphabet: the six singletons `{ } [ ] : ,` plus the
/// `"` string-quote class (no digraphs). Six singletons ⇒ `NibbleLut`
/// lowering — the single-swizzle v128 fast path.
const JSON_ALPHABET: StructuralAlphabet =
    StructuralAlphabet::from_parts(&[b'{', b'}', b'[', b']', b':', b','], &[], &[b'"']);

/// Base pointer of the resident arena (stable across calls).
#[no_mangle]
pub extern "C" fn input_ptr() -> *mut u8 {
    &raw mut ARENA as *mut u8
}

/// Arena capacity in bytes.
#[no_mangle]
pub extern "C" fn input_cap() -> usize {
    CAP
}

/// Scan `arena[off .. off+len]` with the shipping v128 kernel and return
/// the structural-offset count. The kernel fully MATERIALISES every
/// `(position, kind)` pair into its `StructuralIndex` (the emit work the
/// TS lane matches by writing each offset into an `Int32Array`); we return
/// the count so V8 cannot DCE the scan.
#[no_mangle]
pub extern "C" fn scan_resident(off: usize, len: usize) -> u32 {
    let base = &raw const ARENA as *const u8;
    // SAFETY: harness guarantees off+len <= CAP and the region initialised.
    let slice = unsafe { core::slice::from_raw_parts(base.add(off), len) };
    let idx = scan_structural(slice, &JSON_ALPHABET);
    idx.positions.len() as u32
}
