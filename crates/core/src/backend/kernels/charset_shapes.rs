//! Shared CharSet128 shape classification for the kernel layer.
//!
//! Routes the three canonical shapes (digits, alnum, hex) that the
//! hoisted `parse_that::scan_*_mut` helpers accept. Consumed by both
//! `kernels::charclass` (quantified char-class scanning) and
//! `kernels::prefix_class` (literal prefix + class tail). The
//! classification is an O(62) pattern match against constant byte
//! arrays; it's *not* a fact derived from IR and has no upstream
//! cache to read from. Living in one place prevents the two kernel
//! consumers from drifting on the canonical shape definitions.
//!
//! Tranche AF.1 Wave 3E: consolidated out of parallel copies in
//! `charclass.rs` and `prefix_class.rs`.

use bbnf_ir::CharSet128;

/// Canonical `[0-9]` byte set.
pub(super) const DIGITS: [u8; 10] = *b"0123456789";

/// Canonical `[a-zA-Z0-9]` byte set.
pub(super) const ALNUM: [u8; 62] =
    *b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

/// Canonical `[0-9a-fA-F]` byte set.
pub(super) const HEX: [u8; 22] = *b"0123456789abcdefABCDEF";

/// `true` iff `chars` is exactly the byte set `expected`.
pub(super) fn matches_set(chars: &CharSet128, expected: &[u8]) -> bool {
    if chars.len() != expected.len() {
        return false;
    }
    for &b in expected {
        if !chars.has(b) {
            return false;
        }
    }
    true
}
