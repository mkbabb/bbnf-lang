//! Scalar reference for `COMMENT_BODY_MASK_64` — the executable specification.
//!
//! Contract:
//!   Input  — 64 contiguous source bytes + the inter-block carry
//!            (`CommentCarry`): `in_comment` (the block opens already inside a
//!            comment) and `pending_half` (the previous block's LAST byte was
//!            the first half of a digraph — an `open[0]` outside a comment, or
//!            a `close[0]` inside one — awaiting its second half in this
//!            block).
//!   Output — a 64-bit mask where bit `i` is set iff `src[i]` is INTERIOR to a
//!            block comment: the byte after the `open` digraph through the
//!            `close` digraph inclusive. The `open` digraph bytes are not
//!            marked; the `close` digraph bytes are. Plus the carry for the
//!            NEXT block.
//!
//! Interior-marking is chosen so the mask is exactly the set of positions a
//! structural-delimiter scan must AND-NOT away: a CSS structural delimiter
//! (`;{}:,`) is never a comment-digraph byte, so suppressing the interior
//! suppresses every commented delimiter.
//!
//! Digraph parameterisation (open = `/*`, close = `*/` for CSS) is data; the
//! kernel names no grammar (Lock 14: delimiter policy is caller data).
//!
//! Citations:
//!   * Lock 16 (SOTA-BEAT-DESIGN, dav1d primitive-lift row): this scalar
//!     reference is the parity anchor for the checkasm admission gate.
//!   * Lemire/Langdale comment-region suppression: the comment-body mask is
//!     AND-NOT'ed against a structural-delimiter mask.
//!
//! Body status: source-of-truth implementation.

/// Carry threaded across `COMMENT_BODY_MASK_64` blocks WITHIN a single parse.
/// Initialised to `default()` per parse; never retained across parses (Lock 1
/// transient-producer clause).
#[derive(Copy, Clone, Debug, PartialEq, Eq, Default)]
pub struct CommentCarry {
    /// The block opens already inside a comment region.
    pub in_comment: bool,
    /// The previous block's last byte was the first half of a digraph awaiting
    /// completion: `open[0]` when `!in_comment`, `close[0]` when `in_comment`.
    pub pending_half: bool,
}

/// Scalar reference for `COMMENT_BODY_MASK_64`.
///
/// Returns `(mask, next_carry)`. Bit `i` of `mask` is set iff `src[i]` is
/// interior to a block comment delimited by `open` … `close` (CSS: `/*` …
/// `*/`).
#[inline]
pub fn comment_body_mask_64_scalar(
    src: &[u8; 64],
    open: [u8; 2],
    close: [u8; 2],
    carry: CommentCarry,
) -> (u64, CommentCarry) {
    let mut mask: u64 = 0;
    let mut in_comment = carry.in_comment;
    let mut pending_half = carry.pending_half;

    let mut i = 0usize;
    while i < 64 {
        if in_comment {
            mask |= 1u64 << i;
            // A `close` digraph (`close[0] close[1]`) terminates the comment;
            // both bytes are interior. `pending_half` here means the prior
            // block ended on `close[0]`.
            if pending_half {
                pending_half = false;
                if src[i] == close[1] {
                    in_comment = false;
                    i += 1;
                    continue;
                }
                // not a close; fall through to re-test this byte below.
            }
            if src[i] == close[0] {
                if i + 1 < 64 {
                    if src[i + 1] == close[1] {
                        mask |= 1u64 << (i + 1);
                        in_comment = false;
                        i += 2;
                        continue;
                    }
                } else {
                    // `close[0]` at block boundary; second half pends.
                    pending_half = true;
                }
            }
            i += 1;
            continue;
        }

        // Outside a comment. `open` digraph (`open[0] open[1]`) opens one; the
        // open digraph bytes are NOT interior. `pending_half` means the prior
        // block ended on `open[0]`.
        if pending_half {
            pending_half = false;
            if src[i] == open[1] {
                in_comment = true;
                i += 1;
                continue;
            }
            // not an open; fall through to re-test this byte below.
        }
        if src[i] == open[0] {
            if i + 1 < 64 {
                if src[i + 1] == open[1] {
                    in_comment = true;
                    i += 2;
                    continue;
                }
            } else {
                pending_half = true;
            }
        }
        i += 1;
    }

    (mask, CommentCarry { in_comment, pending_half })
}
