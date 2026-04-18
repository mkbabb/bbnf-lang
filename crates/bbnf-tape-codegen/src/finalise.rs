//! Body fragment for `bbnf_tape::finaliser::finalise`.
//!
//! This helper is the Stage-C forward scan that patches `sib_skip`,
//! `span_hi` (for compounds), and `child_off` (for compounds) after
//! the walker has emitted every tape record. P1 §3 put it at
//! 11–14 % self-time on JSON. Per B2 §3, the per-shape emitter
//! knows `span_hi` / `child_off` / `sib_skip` at emit time for the
//! shapes it owns; inlining the body allows the emitter to collapse
//! the pass entirely for those shapes, while the runtime helper
//! remains callable for the cold-path `dta_run_cold` + legacy
//! fn-per-rule reconstruction.

use proc_macro2::TokenStream;
use quote::ToTokens;

/// Verbatim source for the body of `bbnf_tape::finaliser::finalise`
/// — the text between the outer `fn ... { ... }` braces.
///
/// The runtime helper at `crates/bbnf-tape/src/finaliser.rs:154`
/// survives unchanged; `TapeBuilder::finish` still calls it on the
/// cold-path `has_inline_frame_depth == false` reconstruction route
/// per B2 §3.a. This constant is the splice source for the
/// per-shape emitter's Stage-C inline path; divergence between the
/// two is detected by `tests/parse_fragments.rs`.
pub const SOURCE: &str = r#"{
    let n = columns.len();
    debug_assert_eq!(
        frame_depth.len(),
        n,
        "Stage-C finalise: frame_depth length {} != columns length {}",
        frame_depth.len(),
        n,
    );
    if n == 0 {
        return;
    }

    let max_depth = frame_depth.iter().copied().max().unwrap_or(0) as usize;
    // Scratch sized one slot per depth, plus a sentinel for the
    // `d + 1` lookup at the deepest record. Initialised to NONE
    // (no record-yet-seen) at every depth.
    let scratch_len = max_depth + 2;
    let mut prev_at_depth: Vec<Option<u32>> = vec![None; scratch_len];
    let mut first_at_depth: Vec<Option<u32>> = vec![None; scratch_len];
    let mut last_at_depth: Vec<Option<u32>> = vec![None; scratch_len];

    // High-water mark for invalidation. Tracks the largest depth
    // currently populated in the scratch arrays so the invalidation
    // loop only touches live slots; amortises the per-visit
    // invalidation cost to `O(1)` across the whole pass.
    let mut tracked_depth: usize = 0;

    for i in 0..n {
        let d = frame_depth[i] as usize;
        debug_assert!(
            d < scratch_len,
            "Stage-C: frame_depth[{}] = {} exceeds scratch_len {}",
            i,
            d,
            scratch_len,
        );
        let i_u32 = i as u32;

        // ── Step 1: close compound `i` against its child frame ────
        // The children at depth `d + 1` were tracked in the
        // immediately-preceding visits and have not yet been
        // invalidated — we read them first.
        //
        // AW-I.W4δ: skip the `child_off` / `span_hi` re-derivation
        // when the parser's inline writes are already authoritative
        // (every `child_off != NONE` compound has the walker's own
        // `close_compound` write). Pre-order tape layout (W1 adoption)
        // places children AFTER the parent — the per-depth scratch
        // slots read here would reflect an EARLIER sibling's frame,
        // not this compound's own children, so re-writing would
        // corrupt `span_hi` / `child_off` with stale data from a
        // prior iteration of the same outer Repeat. Post-order tapes
        // (legacy fn-per-rule) carry a `child_off == NONE` placeholder
        // at close time; those still need the re-derivation.
        if columns.has_children_at(i_u32)
            && columns.child_off[i] == TapeOffset::NONE
        {
            let child_d = d + 1;
            if let (Some(first), Some(last)) =
                (first_at_depth[child_d], last_at_depth[child_d])
            {
                columns.child_off[i] = TapeOffset(first);
                columns.span_hi[i] = columns.span_hi[last as usize];
            }
        }

        // ── Step 2: invalidate every depth strictly greater than d ─
        // Visiting `i` at depth `d` pops every deeper frame.
        if d < tracked_depth {
            for slot in &mut prev_at_depth[d + 1..=tracked_depth] {
                *slot = None;
            }
            for slot in &mut first_at_depth[d + 1..=tracked_depth] {
                *slot = None;
            }
            for slot in &mut last_at_depth[d + 1..=tracked_depth] {
                *slot = None;
            }
            tracked_depth = d;
        }

        // ── Step 3: stamp sib_skip on the previous same-depth record
        //    in the current frame ────────────────────────────────────
        if let Some(prev) = prev_at_depth[d] {
            columns.sib_skip[prev as usize] = i_u32 - prev;
        }

        // ── Step 4: update tracking for THIS record at depth d ────
        if first_at_depth[d].is_none() {
            first_at_depth[d] = Some(i_u32);
        }
        last_at_depth[d] = Some(i_u32);
        prev_at_depth[d] = Some(i_u32);
        if d > tracked_depth {
            tracked_depth = d;
        }
    }
}"#;

/// Parse [`SOURCE`] as a [`syn::Block`] and return the
/// [`TokenStream`] the per-shape emitter splices inline.
///
/// # Panics
///
/// Panics if [`SOURCE`] fails to parse — caught by
/// `tests/parse_fragments.rs`.
pub fn fragment() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE)
        .expect(
            "bbnf-tape-codegen: finalise body fragment must parse as \
             syn::Block — the runtime helper at \
             crates/bbnf-tape/src/finaliser.rs has likely diverged \
             from this crate's SOURCE constant",
        )
        .to_token_stream()
}
