# Tranche AJ — Tape Hot-Path Surgery + Parse Correctness

Post-AI performance audit. The tape-first architecture introduced in
Tranche AC was slower than the pre-tape BumpSlab era due to five
structural defects in the hot path. AJ fixes them.

## AJ.0 — Zero-alloc TapeCursor child access

`TapeCursor::child(i)` previously delegated to `children().nth(i)`,
which allocated a full `Vec` of all K children just to access one.
Every view accessor call on every compound node caused a heap
allocation.

**Fix:** Two-pass backward walk. Pass 1 counts K direct children;
Pass 2 walks backward to the target index. O(K) per call with
zero heap allocation. Added `child_count()` for O(K) zero-alloc
counting. `children()` retains forward-order Vec collection for
callers that need source-order iteration (analysis crate, repeat
iter), but now pre-reserves exact capacity.

Deleted the unused `ChildIter` scaffold and `subtree_size` helper.
Updated view repeat `len()` codegen to use `child_count()` instead
of `children().count()`.

## AJ.1 — Materialization classifier: leaf-like Alt promotion

The classifier unconditionally assigned `MustTape` to Alt nodes,
even when all branches were leaves (Literal, Regex). Both
`push_leaf` and `push_compound` store `variant_idx` in the flags
byte, so the view layer's discriminated access works identically.

**Fix:** Alt is classified as `TapeSpanOnly` when every branch is
`TapeSpanOnly` or `TransparentElide`. Removed the forced `MustTape`
override for entry rules in `materialization_for_rule` — the body
classification now determines the record shape.

## AJ.2 — Zero-copy parse entry + trailing whitespace

Two bugs in the generated `parse()` function:

1. **Input copy:** `let owned = input.to_owned()` copied the entire
   input on every parse call. For canada.json (2.2 MB) this added
   ~0.5 ms of pure memcpy overhead.

2. **Trailing whitespace rejection:** `state.offset < owned.len()`
   rejected inputs with a trailing newline. canada.json ends with
   `\n` at offset 2,251,050 of 2,251,051 bytes.

**Fix:** `Parsed<R>` rewritten as `Parsed<'p, R>`, borrowing
`&'p str` instead of owning a `String`. Generated `parse()` now
operates directly on the caller's input buffer. After the root rule
completes, trailing ASCII whitespace is consumed before the EOF
check.

The bootstrap parser (`grammar/mod.rs`) leaks the input String to
`'static` before parsing, maintaining the `'static` Parsed lifetime
the pipeline assumes.

## AJ.3 — TapeBuilder adaptive capacity

The heuristic `len / 8` underestimated for typical JSON/CSS density.
Changed to `len / 4` (one record per 4 input bytes).

## Results

| Benchmark   | Post-Z.7   | Post-AJ     | Delta   |
|-------------|------------|-------------|---------|
| JSON canada | FAIL       | 1,332 MB/s  | Fixed   |
| JSON citm   | 1,811 MB/s | 1,880 MB/s  | +3.8%   |
| JSON data   | —          | 1,454 MB/s  | —       |
| JSON twitter| —          | 1,543 MB/s  | —       |

canada.json parsing restored. citm throughput exceeds the post-Z.7
baseline. The zero-copy `Parsed<'p, R>` eliminates the per-parse
input allocation for all grammars.
