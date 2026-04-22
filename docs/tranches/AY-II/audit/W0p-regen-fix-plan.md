# AY-II.W0' — Regen fix plan (post-research)

## Intended outcome

Replace the O(N²) `subtree_size` recount inside `FusedBuilder::value_end_compound` with an O(1) lookup of `direct_child_count` maintained on the `ValueCheckpoint` stack at push time, restoring regen wall-clock to pre-W0' baseline (3–6 min) and preserving every tape-crate test's runtime artefact (`frame.child_count`, `frame.span_hi`, `HAS_CHILDREN_BIT`, `child_off`) byte-for-byte.

## File-level change set (ordered)

### Change 1 — `crates/tape/src/builder/value.rs` — `ValueCheckpoint` + invariant doc

**Why this first**: single-pass compile dependency — `mod.rs` consumes the new field; `value.rs` defines it. No builder-side callers yet on this commit.

**Before** (`crates/tape/src/builder/value.rs:130-148`):
```rust
/// Opaque checkpoint produced by the value substrate at compound-open
/// time and consumed by rollback.
///
/// Encodes the arena + payload-column sizes at the open point so
/// rollback truncates every family atomically. The fused builder
/// surfaces a single `u32` tape-offset to the emitter; the value
/// substrate maintains this richer checkpoint internally alongside
/// each open frame.
#[derive(Clone, Copy, Debug)]
pub(super) struct ValueCheckpoint {
    /// Arena frame offset at open time. `frames.len()` snaps back to
    /// this value on rollback; the frame itself is pushed at this
    /// index.
    pub(super) frame_offset: u32,
    /// Narrow payload column rank at open time.
    pub(super) narrow_rank: u32,
    /// Wide payload column rank at open time.
    pub(super) wide_rank: u32,
}
```

**After** (same location — append `direct_child_count` + extend the doc):
```rust
/// Opaque checkpoint produced by the value substrate at compound-open
/// time and consumed by rollback.
///
/// Encodes the arena + payload-column sizes at the open point so
/// rollback truncates every family atomically. The fused builder
/// surfaces a single `u32` tape-offset to the emitter; the value
/// substrate maintains this richer checkpoint internally alongside
/// each open frame.
///
/// # Direct-child counter (AY-II.W0'.d3)
///
/// `direct_child_count` is the in-stack tally the builder increments
/// on every frame push that lands as a direct child of this open
/// compound — every `push_value_leaf` call while this checkpoint is
/// the top-of-stack, and every nested `value_begin_compound` call
/// whose parent checkpoint is this one (incremented on the parent's
/// counter, second-from-top after the new checkpoint pushes). At
/// `value_end_compound` time the counter is read directly into
/// `ValueFrame::child_count`, replacing the O(subtree_size) walk
/// landed in W0'.a. See
/// `docs/tranches/AY-II/audit/W0p-regen-root-cause.md` for the
/// attribution.
#[derive(Clone, Copy, Debug)]
pub(super) struct ValueCheckpoint {
    /// Arena frame offset at open time. `frames.len()` snaps back to
    /// this value on rollback; the frame itself is pushed at this
    /// index.
    pub(super) frame_offset: u32,
    /// Narrow payload column rank at open time.
    pub(super) narrow_rank: u32,
    /// Wide payload column rank at open time.
    pub(super) wide_rank: u32,
    /// Count of direct children pushed under this checkpoint since
    /// `value_begin_compound`. Incremented by every `push_value_leaf`
    /// whose parent is this checkpoint (top-of-stack) and by every
    /// nested `value_begin_compound` (increment on the parent, i.e.
    /// second-from-top after the nested push). Consumed by
    /// `value_end_compound` into `ValueFrame::child_count` — O(1)
    /// replacement for the pre-W0'.d3 `subtree_size` walk.
    pub(super) direct_child_count: u32,
}
```

**Note**: `subtree_size` at `crates/tape/src/builder/value.rs:356-374` is **retained unchanged** — it is still used by the `ValueChildren::next` iterator at `crates/tape/src/builder/value.rs:349` (projection path, Θ(N) amortised, runs once per projected tree — not per builder close).

### Change 2 — `crates/tape/src/builder/mod.rs` — `value_begin_compound`

Initialise the new counter to 0 on every open.

**Before** (`crates/tape/src/builder/mod.rs:1127-1147`):
```rust
    /// Open a value-arena frame in lockstep with the tape's
    /// `begin_compound`. Pushes a compound frame + checkpoint onto
    /// the open-stack.
    #[inline(always)]
    fn value_begin_compound(&mut self, kind: TapeKind, span_lo: u32, variant_idx: u8) {
        let frame_offset = self.value_frames.len() as u32;
        self.value_frames.push(ValueFrame {
            span_lo,
            span_hi: span_lo,
            first_child: frame_offset + 1,
            child_count: 0,
            variant_idx,
            kind,
            payload_tag: PayloadTag::NONE,
        });
        self.value_open_stack.push(ValueCheckpoint {
            frame_offset,
            narrow_rank: self.value_payloads_narrow.len() as u32,
            wide_rank: self.value_payloads_wide.len() as u32,
        });
    }
```

**After** (same site):
```rust
    /// Open a value-arena frame in lockstep with the tape's
    /// `begin_compound`. Pushes a compound frame + checkpoint onto
    /// the open-stack and bumps the parent checkpoint's
    /// `direct_child_count` (this nested compound is a direct child
    /// of whatever was on top at entry).
    #[inline(always)]
    fn value_begin_compound(&mut self, kind: TapeKind, span_lo: u32, variant_idx: u8) {
        // Nested-compound push: bump the PARENT checkpoint's direct-
        // child counter BEFORE pushing this compound's own checkpoint.
        // After the push, this compound becomes top-of-stack; its
        // counter starts at 0 and is incremented by its own children.
        if let Some(parent) = self.value_open_stack.last_mut() {
            parent.direct_child_count += 1;
        }
        let frame_offset = self.value_frames.len() as u32;
        self.value_frames.push(ValueFrame {
            span_lo,
            span_hi: span_lo,
            first_child: frame_offset + 1,
            child_count: 0,
            variant_idx,
            kind,
            payload_tag: PayloadTag::NONE,
        });
        self.value_open_stack.push(ValueCheckpoint {
            frame_offset,
            narrow_rank: self.value_payloads_narrow.len() as u32,
            wide_rank: self.value_payloads_wide.len() as u32,
            direct_child_count: 0,
        });
    }
```

**Interaction with Change 1**: `ValueCheckpoint` literal construction now carries the new field. Compile-fail catches any unmigrated call site (there are none outside this file; the struct is `pub(super)`).

### Change 3 — `crates/tape/src/builder/mod.rs` — `value_end_compound` (core fix)

**Before** (`crates/tape/src/builder/mod.rs:1149-1173`):
```rust
    /// Close the most recently opened value frame — patches `span_hi`
    /// + reconstructs `child_count` by walking the pre-order range
    /// between the open frame and the current arena tip.
    #[inline(always)]
    fn value_end_compound(&mut self, span_hi: u32) {
        let checkpoint = self
            .value_open_stack
            .pop()
            .expect("FusedBuilder::value_end_compound called with empty open_stack");
        let frame_offset = checkpoint.frame_offset as usize;
        // Direct children occupy the range [frame_offset+1,
        // frames.len()); reconstruct the direct-child count by
        // stepping past each subtree in turn.
        let mut cursor = frame_offset + 1;
        let total = self.value_frames.len();
        let mut direct_count: u32 = 0;
        while cursor < total {
            let size = subtree_size(&self.value_frames, cursor);
            cursor += size;
            direct_count += 1;
        }
        let frame = &mut self.value_frames[frame_offset];
        frame.span_hi = span_hi;
        frame.child_count = direct_count;
    }
```

**After** (O(1), reads the in-stack counter):
```rust
    /// Close the most recently opened value frame — patches `span_hi`
    /// and reads `direct_child_count` straight off the popped
    /// checkpoint. O(1) per call. See
    /// `docs/tranches/AY-II/audit/W0p-regen-root-cause.md` for the
    /// O(N²) walk this replaces.
    #[inline(always)]
    fn value_end_compound(&mut self, span_hi: u32) {
        let checkpoint = self
            .value_open_stack
            .pop()
            .expect("FusedBuilder::value_end_compound called with empty open_stack");
        let frame = &mut self.value_frames[checkpoint.frame_offset as usize];
        frame.span_hi = span_hi;
        frame.child_count = checkpoint.direct_child_count;
    }
```

**What dies**: the `while cursor < total { subtree_size(...) }` reconstruction loop at `mod.rs:1162-1169` and, with it, the per-close invocation of `subtree_size` that drove the O(N²) regen pathology.

**What keeps**: `subtree_size` at `value.rs:362` stays in-place — still used by `ValueChildren::next` at `value.rs:349`. The `use value::{subtree_size, ValueCheckpoint};` import at `mod.rs:66` can be narrowed to `use value::ValueCheckpoint;` since `mod.rs` no longer references `subtree_size` after this change.

### Change 4 — every direct-child push path

The new invariant: **every push that lands a direct child under a currently-open compound bumps that compound's `direct_child_count`**. Enumerate every site that pushes a `ValueFrame` and audit.

#### 4.1 `push_value_leaf` — the sole leaf-frame push helper

Every public `push_leaf_*` funnels through `push_value_leaf` at `mod.rs:1178-1196`. Increment the top-of-stack counter there so one edit covers every leaf path (`push_leaf`, `push_leaf_with`, `push_leaf_borrowed_string`, `push_leaf_with_f64_direct`, `push_leaf_with_arena_frame`, `push_leaf_with_arena_payload`).

**Before** (`mod.rs:1175-1196`):
```rust
    /// Append a leaf value frame carrying a source span + payload
    /// tag. The tape-side leaf push is the caller's responsibility;
    /// this only appends the paired value frame.
    #[inline(always)]
    fn push_value_leaf(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        payload_tag: PayloadTag,
    ) {
        self.value_frames.push(ValueFrame {
            span_lo,
            span_hi,
            first_child: 0,
            child_count: 0,
            variant_idx,
            kind,
            payload_tag,
        });
    }
```

**After**:
```rust
    /// Append a leaf value frame carrying a source span + payload
    /// tag. The tape-side leaf push is the caller's responsibility;
    /// this only appends the paired value frame. If an open compound
    /// is on the stack, bumps its `direct_child_count` — a leaf push
    /// is always a direct child of the enclosing compound.
    #[inline(always)]
    fn push_value_leaf(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        payload_tag: PayloadTag,
    ) {
        if let Some(parent) = self.value_open_stack.last_mut() {
            parent.direct_child_count += 1;
        }
        self.value_frames.push(ValueFrame {
            span_lo,
            span_hi,
            first_child: 0,
            child_count: 0,
            variant_idx,
            kind,
            payload_tag,
        });
    }
```

Leaf entry points calling `push_value_leaf` (all carried unchanged; the bump is central):

- `push_leaf` — `mod.rs:389-410` (calls `push_value_leaf` at 408)
- `push_leaf_with` — `mod.rs:541-615` (calls `push_value_leaf` at 613)
- `push_leaf_with_arena_frame` — `mod.rs:721-753` (calls `push_value_leaf` at 751)
- `push_leaf_with_arena_payload` — `mod.rs:773-814` (calls `push_value_leaf` at 812)
- `push_leaf_borrowed_string` — `mod.rs:829-860` (calls `push_value_leaf` at 858)
- `push_leaf_with_f64_direct` — `mod.rs:880-918` (calls `push_value_leaf` at 910)

No per-site edits — centralising the bump in `push_value_leaf` preserves the discipline.

#### 4.2 `value_begin_compound` — nested-compound push

Already covered by **Change 2** above: bumps the parent checkpoint's counter before pushing its own checkpoint.

#### 4.3 `begin_compound` (public) — `end_compound` pre-order close

No additional edit. `begin_compound` (`mod.rs:441-469`) calls `value_begin_compound` at line 467, which handles the parent-counter bump. `end_compound` (`mod.rs:483-493`) calls `value_end_compound` at 492, which reads the counter.

#### 4.4 `end_compound_post_order` — shares the same close path

No separate edit. `end_compound_post_order` (`mod.rs:513-526`) invokes `self.value_end_compound(span_hi)` at line 525. The close semantics are identical — both entry points share the counter read. See **Change 5** for the explicit N/A statement.

#### 4.5 `rollback_to` — failed-branch cleanup

**Before** (`mod.rs:316-377`, focus on the truncation block at `345-376`):
```rust
        while let Some(&checkpoint) = self.value_open_stack.last() {
            // ...(omitted comments)...
            self.value_frames.truncate(checkpoint.frame_offset as usize);
            self.value_payloads_narrow
                .truncate(checkpoint.narrow_rank as usize);
            self.value_payloads_wide
                .truncate(checkpoint.wide_rank as usize);
            self.value_open_stack.pop();
            // Only pop one — the matched retry compound. If the
            // caller left additional opens deeper than `open_offset`
            // on the stack ...
            break;
        }
```

**After** (same site; add counter-decrement for the parent whose failed nested compound is being discarded):
```rust
        while let Some(&checkpoint) = self.value_open_stack.last() {
            // ...(omitted comments, unchanged)...
            self.value_frames.truncate(checkpoint.frame_offset as usize);
            self.value_payloads_narrow
                .truncate(checkpoint.narrow_rank as usize);
            self.value_payloads_wide
                .truncate(checkpoint.wide_rank as usize);
            self.value_open_stack.pop();
            // After popping the failed compound, undo the parent-
            // counter bump that `value_begin_compound` applied when
            // the failed compound opened. The retry-IIFE will
            // re-open a fresh compound (which bumps the parent
            // again), so symmetric decrement here keeps
            // `direct_child_count` equal to the number of
            // SURVIVED direct children at close time.
            if let Some(parent) = self.value_open_stack.last_mut() {
                parent.direct_child_count =
                    parent.direct_child_count.saturating_sub(1);
            }
            // Only pop one — the matched retry compound. ... (unchanged)
            break;
        }
```

**Correctness argument for the single decrement**: pre-W0'.d3, `rollback_to` pops ONE open-stack entry (the retry-IIFE's compound). That compound's own counter dies with the pop (the `ValueCheckpoint` is discarded). But the PARENT of that failed compound had its counter bumped at `value_begin_compound` time for the about-to-be-popped child. That bump must be reversed — otherwise the subsequent `end_compound` on the parent will over-count by 1 per failed retry. `saturating_sub` is defensive against double-rollback (rare; documented above).

**Edge case**: `rollback_to` at top-level (i.e. `value_open_stack.last() == None`) is a no-op; the early loop never runs, so no decrement is attempted. Covered by the `if let Some(parent)` guard after the pop.

**Verification**: the existing `rollback_to_unwinds_begin_compound_cleanly` test at `crates/tape/tests/close_compound.rs:261-308` exercises a retry that discards one compound and re-opens another. Post-fix, the outer compound's `child_count` must still equal 1 (one surviving child — the retry compound). The test asserts `cursor.child_count() == 1` at line 304; under the fix, the outer's `direct_child_count` is incremented on retry-compound open (+1 at value_begin_compound for attempt), decremented on rollback (-1 at rollback_to), incremented again on the retry (+1 at value_begin_compound), closed at 1. Matches the pre-fix semantics exactly.

### Change 5 — `value_end_compound_post_order` path

**N/A — shares `value_end_compound`.** `end_compound_post_order` at `mod.rs:513-526` calls `self.value_end_compound(span_hi)` at line 525. There is no separate value-side post-order fn. Change 3 covers both entry points in one edit.

### Change 6 — test-time counter semantics (green-bar gate)

Existing tape + core tests that assert on the runtime artefacts the fix touches (`frame.child_count` via `cursor.child_count()`, `HAS_CHILDREN_BIT` via `cols.has_children_at`, `child_off`, `span_hi`). All must pass post-fix with byte-identical artefact values — the fix is a pure perf transposition, not a semantic change.

Tests to re-run as the redress-time gate:

- `crates/tape/tests/close_compound.rs`:
  - `nested_begin_end_produces_pre_order_tape` (asserts `cursor.child_count() == 2` on nested compounds)
  - `end_compound_post_order_stamps_backward_child_off_and_has_children` (asserts `cursor.child_count() == 2` on post-order compound)
  - `end_compound_post_order_empty_frame` (asserts zero-child compound semantics)
  - `end_compound_without_children` (asserts zero-child pre-order close)
  - `rollback_to_unwinds_begin_compound_cleanly` (asserts `cursor.child_count() == 1` after retry; exercises Change 4.5 directly)
  - `rollback_to_idempotent` (asserts idempotent rollback; exercises Change 4.5's `saturating_sub` guard)
  - `legacy_push_compound_path_still_closes_via_finaliser` (asserts `cursor.child_count() == 2` on legacy post-order path)
  - `sibling_begin_end_subtrees_under_outer_begin` (asserts `cursor.child_count() == 2` on outer containing two sibling compounds each with one leaf)

- `crates/tape/tests/tape_basic.rs`:
  - `push_compound_with_children` (direct `has_children()` + `child_off` assertion)
  - `cursor_walks_children` (asserts `children().count() == 3`)
  - `sibling_skip_walks_direct_children_forward` (asserts `children().count() == 3` on `(a (b c) d)`)
  - `sibling_skip_nested_compound` (asserts nested compound child iteration)
  - `empty_compound_sibling_skip_is_zero` (asserts empty-compound semantics)
  - `flags_encode_variant_and_has_children`
  - `meta_idx_round_trip_compound` (asserts compound `child(0)` access)
  - `meta_idx_and_has_children_coexist`

- `crates/tape/tests/fused_writes.rs`:
  - All tests compile against `Columns` directly, not via `FusedBuilder`'s `value_end_compound` — they exercise the tape columns, not the value arena. Confirm compile-clean (Change 1 modifies a `pub(super)` type; no crate-external exposure).

- `crates/tape/tests/packed_cache.rs`:
  - Same as `fused_writes.rs` — no `value_end_compound` contact. Compile-clean gate.

- `crates/core/tests/tape_walker_allocs.rs`:
  - `child_iter_yields_three_leaves_in_forward_order` (asserts 3-child iteration on compound with 3 leaves via `FusedBuilder`)
  - `child_iter_does_not_descend_into_grandchildren` (asserts 2 direct children; exercises nested-compound counter)
  - `child_iter_empty_on_compound_with_no_children`
  - `child_iter_matches_children_set` (asserts `children()` and `children_zero_alloc()` yield equal sets)
  - `child_iter_supports_iterator_combinators`
  - `child_iter_exit_condition_does_not_underflow`

**Expected green-bar command**: `cargo test -p tape --tests && cargo test -p bbnf --test tape_walker_allocs --release`. Expected result: all tests green, identical byte-output for every `frame.child_count` / `cursor.child_count()` / `has_children_at` probe.

### Change 7 — W0' post-fix validation

After the code changes land:

1. **Regen sequence** (expected < 3 min wall-clock; was 12–15 min pre-fix):
   ```
   rm -rf target/.bbnf-cache
   time bash scripts/bootstrap-bbnf.sh
   ```
   Gate: wall time strictly < 5 min. If > 5 min, halt and escalate — the counter thread may have a missed-increment site.

2. **Double-regen idempotency**:
   ```
   cp crates/core/src/grammar/generated.rs /tmp/gen1.rs
   rm -rf target/.bbnf-cache
   bash scripts/bootstrap-bbnf.sh
   md5 /tmp/gen1.rs crates/core/src/grammar/generated.rs
   ```
   Gate: MD5 hashes equal.

3. **Workspace check + tape test**:
   ```
   cargo check -p tape --tests
   cargo test -p tape --tests
   cargo test -p bbnf --test tape_walker_allocs --release
   ```

4. **Wrap-up**: commit per the template in §Commit message template.

## Invariant preservation ledger

| Invariant | How the fix preserves it |
|---|---|
| W0p.md §14 FusedBuilder sole builder | `FusedBuilder` type unchanged; one `u32` field appended to the private `ValueCheckpoint` struct. No `pub` surface change. |
| W0p.md §15 `push_compound` / `mark_children` absent | Not re-introduced; the fix lives inside `value_begin_compound` / `value_end_compound` / `push_value_leaf` / `rollback_to` — all already extant. |
| W0p.md §16 materializer call-count truth | Projection path (`view/value.rs`, `project_value_<Grammar>`, `materialize_projection_*`) is untouched. The `frame.child_count` the projection reads is identical to pre-fix values on every fixture (Change 6 tests lock this). |
| W0p.md §17 `STRUCTURAL_SCAN_POLICY` splice | `view/value.rs` untouched. |
| W0p.md §18 zero W0-era `#[allow(dead_code)]` | No new `#[allow(...)]` attributes introduced. The new field is used on every write site (Change 2, Change 4.1, Change 4.5) and every read site (Change 3). |
| W0p.md §19 `Parsed::to_value()` non-panic | Contract preserved: for every closed compound, `frame.child_count == direct-child-count` post-fix just as pre-fix. The only change is O(1) vs O(subtree_size) computation — the answer is identical. |
| AY-II.md §5 fused pipeline single-pass lockstep | Retained — Change 2/4 run inline on the existing push paths; no second pass introduced. |
| AY-II.md §7 projection totality | `ValueChildren` iterator at `value.rs:337-354` still walks the contiguous frame range via `subtree_size`; projection reads are byte-for-byte identical. |
| Close-path O(1) | Change 3 reduces `value_end_compound` to four field accesses + one write; the sole remaining non-constant factor is the `Vec::pop` amortised O(1). `end_compound_post_order` shares this path → both entry points are O(1) per call. |
| `frame.child_count == direct-child-count` across all entry paths | Pre-order (`end_compound` → `value_end_compound`): counter is incremented on every nested compound push (Change 2) and every leaf push (Change 4.1), decremented on retry-compound rollback (Change 4.5). Post-order (`end_compound_post_order` → `value_end_compound`): same counter, same code path. |

## Redress-agent checklist

1. **Worktree**: operate at master `5cb76753` (or a fresh worktree branched from it).
2. **Edit Change 1** in `crates/tape/src/builder/value.rs`: extend `ValueCheckpoint` struct + docblock.
3. **Edit Change 2** in `crates/tape/src/builder/mod.rs` at line 1131-1147: add parent-counter bump in `value_begin_compound`.
4. **Edit Change 3** in `crates/tape/src/builder/mod.rs` at line 1149-1173: replace `value_end_compound` body.
5. **Edit Change 4.1** in `crates/tape/src/builder/mod.rs` at line 1178-1196: add parent-counter bump in `push_value_leaf`.
6. **Edit Change 4.5** in `crates/tape/src/builder/mod.rs` inside `rollback_to` at line 345-376: add parent-counter decrement after the pop.
7. **Narrow the `subtree_size` import** in `crates/tape/src/builder/mod.rs:66`: change `use value::{subtree_size, ValueCheckpoint};` to `use value::ValueCheckpoint;` (the `subtree_size` import is now dead in this file).
8. **Compile gate**: `cargo check -p tape --tests` — must exit 0.
9. **Test gate**: `cargo test -p tape --tests` — all tests green.
10. **Cross-crate gate**: `cargo test -p bbnf --test tape_walker_allocs --release`.
11. **Commit** with the template below.
12. **Regen gate**: `rm -rf target/.bbnf-cache && time bash scripts/bootstrap-bbnf.sh` — fail unless wall-clock < 5 min.
13. **Idempotency gate**: double-regen MD5 must match.

## Commit message template

```
refactor(tape): O(1) direct_child_count in value_end_compound (AY-II.W0'.d3)

ValueCheckpoint now carries `direct_child_count`, incremented inline
at every direct-child push (push_value_leaf bumps the top-of-stack
checkpoint; value_begin_compound bumps the parent checkpoint before
pushing its own). value_end_compound reads the counter in O(1),
replacing the recursive `subtree_size` walk landed at W0'.a that
produced Θ(N^2) regen wall-clock over bbnf-bootstrap's 2-3 k value
frames. Rollback decrements the parent counter to undo a failed-
compound open before the retry-IIFE re-opens.

Fixes AY-II.W0' regen pathology documented at
docs/tranches/AY-II/audit/W0p-regen-root-cause.md.

Artefact parity: frame.child_count, HAS_CHILDREN_BIT, child_off,
and span_hi are byte-identical to pre-fix on every tape-crate
fixture (close_compound, tape_basic, fused_writes, packed_cache,
tape_walker_allocs). subtree_size retained at
crates/tape/src/builder/value.rs:362 — still used by the
ValueChildren projection iterator.
```

## Risk / rollback

**Risk**: a missed push site in the counter-increment discipline causes `direct_child_count` to be too low, which cascades into `ValueChildren::next` iterator stopping early (one of the N children silently dropped on `to_value()` reads).

**Mitigation**:

1. All leaf pushes funnel through `push_value_leaf` (single site — Change 4.1). Any leaf entry point that bypasses it is a pre-existing bug, not a bug introduced by this fix.
2. All compound opens funnel through `value_begin_compound` (single site — Change 2). Any compound open that bypasses it likewise pre-existed.
3. The test suite in Change 6 asserts `child_count` on every compound-structure fixture (`cursor.child_count() == K` for K ∈ {0, 1, 2, 3}). Under-count would trip at least one of those.
4. `projection_totality.rs` and `value_api_apples_to_apples.rs` both assert runtime equivalence over full grammar fixtures — a systemic counter miss would surface there.

**Rollback**: revert the single commit. No architectural or data-layout lock-in; the counter field is private and deletable.

## Out of scope

- **Emitter changes** (`crates/core/src/backend/rust/emitter/**`). The emitter's `quote!`-generated call sites reference `FusedBuilder` or `TapeBuilder` — both resolve to the same type. The earlier stash (`/tmp/w0p-regen-draft-fix.diff`) is orthogonal and stays deferred.
- **`generated.rs`** — regen is the VALIDATION of this fix, not a target of it.
- **IR, admission, projection, materializer emission** — untouched. The contract on `frame.child_count` is preserved exactly.
- **Docs outside `docs/tranches/AY-II/audit/`** — no changes. PROGRESS.md and W0'.d close-ceremony docs update in the W0' close commit, not in this `d3` perf commit.
- **Tests outside `crates/tape` + `crates/core/tests/tape_walker_allocs.rs`** — not modified; they're green-bar gates, not change sites.
- **`TapeBuilder → FusedBuilder` emitter rename** (stash at `/tmp/w0p-regen-draft-fix.diff`) — cosmetic under the `pub type TapeBuilder = FusedBuilder;` alias; unrelated to the regen pathology; defer to a later W0'.e if deemed worth chasing.
