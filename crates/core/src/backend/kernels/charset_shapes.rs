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
pub(super) const DIGITS: [u8; 10] = [b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9'];

/// Canonical `[a-zA-Z0-9]` byte set.
pub(super) const ALNUM: [u8; 62] = [
    b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'A', b'B', b'C', b'D', b'E', b'F',
    b'G', b'H', b'I', b'J', b'K', b'L', b'M', b'N', b'O', b'P', b'Q', b'R', b'S', b'T', b'U', b'V',
    b'W', b'X', b'Y', b'Z', b'a', b'b', b'c', b'd', b'e', b'f', b'g', b'h', b'i', b'j', b'k', b'l',
    b'm', b'n', b'o', b'p', b'q', b'r', b's', b't', b'u', b'v', b'w', b'x', b'y', b'z',
];

/// Canonical `[0-9a-fA-F]` byte set.
pub(super) const HEX: [u8; 22] = [
    b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'a', b'b', b'c', b'd', b'e', b'f',
    b'A', b'B', b'C', b'D', b'E', b'F',
];

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
