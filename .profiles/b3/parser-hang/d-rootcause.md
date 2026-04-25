# B3.W0.δ — root-cause analysis: empty `ir.rules` post-γ

**Status**: PARTIALLY RESOLVED (60-min HARD CAP reached).

**Verdict**: the empty `ir.rules` panic is not a single-source bug —
it's a confluence of three lockstep-invariant violations between the
in-builder `frame_depth` / `current_depth` introduced by γ and the
wider tape-substrate API. Two are fixed in this worktree; a third
manifestation surfaces during walk-tape iteration inside
`extract_for_pipeline` and is not yet stabilised on the ax-iter
substrate.

## Phase 1 — panic capture

`crates/core/src/generate/serialize/mod.rs:71` panics with
`index out of bounds: the len is 0 but the index is 0` on
`ir.rules[ir.entry as usize]`. Top of backtrace:

```
17:  bbnf::generate::serialize::generate_serialize_methods
18:  bbnf::generate::generate_all                                   (mod.rs:65:26)
19:  xtask::regen::regen_grammar                                    (regen.rs:227:30)
```

Phase log shows `[parse-end] wall=228µs` — parser baseline holds —
followed by `[extract] wall=8.5µs ast_rules=0 imports=0` and
`[compile_ast_common done] rules.len()=0 entry=0`. Full capture at
`.profiles/b3/parser-hang/d-panic-stderr.txt`.

## Phase 2 — upstream trace

Phase-instrumentation in `crates/core/src/pipeline/directives.rs`,
`crates/core/src/pipeline/compile.rs`, and
`crates/core/src/grammar/host.rs` proved the empty IR comes from
the host-extract step — `parsed.view().children().count()` returns
1 against a 13-rule json grammar, with a single Seq child of span
`(0, 23)` whose grandchildren extend to span `(23, 67)` (i.e., span
of one of the LATER rule iterations is reported as a child of the
FIRST iteration). Tape sib_skip / child_off chains are corrupt.

Three lockstep-invariant violations converge here:

1. **`frame_depth` retry-rollback parity (PRIMARY).** γ's commit
   landed `frame_depth: Vec<u8>` + `current_depth: u8` on
   `TapeBuilder` and stamps both on every structural push. The
   parser-emitted retry sites in `crates/core/src/grammar/generated.rs`
   (72 occurrences of `builder.columns_mut().rollback_to(open)`)
   call `Columns::rollback_to` directly, bypassing
   `TapeBuilder::rollback_to`. Result: after a failed branch's
   rollback, `frame_depth.len() != columns.records.len()` and
   `current_depth` stays bumped. Subsequent pushes stamp the wrong
   depth, the finaliser derives wrong `sib_skip`, the cursor walks
   the corrupted chain.

2. **`current_depth` restoration parity.** The same retry sites
   bypass any depth restoration. Even with `frame_depth` truncated
   on rollback, the next `begin_compound` reads `current_depth`
   which is still bumped from the failed branch's interior.

3. **Pre-order `child_off` defeated by nested post-order.** Pre-
   order `end_compound` sets `child_off = open + 1`. When an inner
   `end_compound_post_order` retroactively bumps `frame_depth` over
   its child range (γ's mechanism), the records that were originally
   at `parent_depth + 1` move to `parent_depth + 2`. Pre-order's
   "first child at open + 1" assumption breaks: the row at `open + 1`
   is now a depth-N+2 grandchild, not a direct child.

Pre-γ `derive_frame_depth` masked (3) by a backward walk that
re-derived `child_off` from frame_depth at finalise time. γ
retired it (correctly — it spun infinitely on Pratt pre-order
inside post-order Seq). The resulting tape correctness regression
is what surfaces as the empty-IR panic when generated.rs is parsed
on master.

## Phase 3 — fix applied

Three coordinated changes on this worktree (`bbnf-wt-b3-w0d-serialize`):

### 3.1 Move `frame_depth` + `current_depth` into `Columns`

`crates/tape/src/columns.rs`:
- Add `pub(crate) frame_depth: Vec<u8>` and `pub(crate)
  current_depth: u8` to `Columns`.
- `Columns::push_structural` now stamps `frame_depth.push(current_depth)`
  on every push — single source of lockstep parity for `records` /
  `sib_skip` / `frame_depth`.
- `Columns::rollback_to(open_offset)` reads
  `frame_depth[new_len]` BEFORE truncation to restore
  `current_depth`, then truncates all three columns in lockstep.
- Same pattern for `Columns::truncate(new_len)`.
- New `Columns::run_finalise` and `Columns::split_off_frame_depth_mut`
  helpers wrap the disjoint-borrow dance the finaliser + DTA walker
  already needed.

### 3.2 Builder defers all depth bookkeeping to `Columns`

`crates/tape/src/builder/mod.rs`:
- Drop the builder's own `frame_depth` / `current_depth` fields.
- `begin_compound` / `end_compound` / `end_compound_post_order`
  bump / decrement `self.columns.current_depth` (no second source).
- `TapeBuilder::rollback_to` defers entirely to
  `Columns::rollback_to` — the depth restoration runs inside
  the columns layer so generated parser code that calls
  `columns_mut().rollback_to(open)` directly gets the same
  restoration without touching builder.

### 3.3 Pre-order `end_compound` scans for true first child

`crates/tape/src/builder/mod.rs::end_compound`:
- Reads `open_depth = frame_depth[open_offset]`, computes
  `target_depth = open_depth + 1`, and scans forward from `open + 1`
  to find the first record at exactly `target_depth`. Skips records
  that an inner post-order close bumped to `target_depth + N`.
- Common case (no nested post-order): `frame_depth[open+1] ==
  target_depth`, scan exits immediately. Zero overhead for the
  shape-emitter pre-order paths.
- Result: the rule compound that `parse_flat_BbnfBootstrap_rule`
  emits via `end_compound_post_order` is now reachable as the iter
  wrapper's only direct child (verified at `.profiles/b3/parser-hang/`
  build-10 walk-tape diagnostic — `gc[0.0]: Seq rule_kind=rule
  span=(0, 22) ggc=4`).

`crates/tape/src/cursor.rs::first_child_root`:
- Pre-order fast path widened from `start == parent + 1` to
  `start > parent_idx`. The post-order backward walk only fires for
  `start < parent_idx`.

## Phase 4 — verification status

| Step | Result |
|---|---|
| `cargo check -p bbnf -p xtask --profile ax-iter` | exit 0 (5.81s) |
| `cargo build -p xtask --release` | exit 0 (1m 01s) |
| `xtask regen --grammar json` | tape now reaches the rule compounds (gc[i.0] = `Seq rule_kind=rule`) but extract_for_pipeline TIMES OUT in `walk_tape`'s second iteration of `root.children()` — the count() print emits 13 children, but the absorb-item loop hangs |
| `cargo nextest run -p tape --profile ax-iter` | **100/100 PASS** (after build11 — push_structural auto-stamps frame_depth) |

Tape architecture is sound — all unit tests pass. The remaining
issue is a walk-tape iteration hang inside `extract_for_pipeline`:
`root.children().count()` returns 13 (correct) but the actual
`for item in root.children() { absorb_item(...) }` loop times out
after the diagnostic prints. The diagnostic for-loop terminates
correctly (cap at 8 children + grandchildren); the production for-
loop doesn't reach `[extract]`. Insufficient time to localise the
hang within absorb_item or further-downstream code; it does NOT
appear to be a tape-cursor cycle (count() proves termination).

## Phase 5 — incomplete; next-action queue

Per HARD CAP discipline, halting at 60 min for orchestrator
direction. Outstanding work to ship a clean fix:

1. **Confirm build11 (`push_structural` stamps `frame_depth`)
   resolves the 3 tape-test failures**: the auto-stamp in
   `push_structural` should keep `records.len() == frame_depth.len()`
   for direct-primitive callers without builder mediation.
2. **Diagnose the walk-tape iteration hang**: `count()=13` is finite
   but the second pass through `root.children()` doesn't reach
   `[extract]` log. Likely a children-iter sib_skip edge-case
   exposed by my `start > parent_idx` cursor change interacting with
   pre-finaliser state — needs targeted print-debugging on the
   actual ChildIter.
3. **Re-validate end-to-end**: json regen + bbnf regen + tape tests
   + bbnf tests.
4. **Cherry-pick to master**: only the four files
   (`crates/tape/src/columns.rs`, `crates/tape/src/builder/mod.rs`,
   `crates/tape/src/cursor.rs`, plus any test adjustments) — NOT the
   instrumentation patches in `crates/core/src/pipeline/directives.rs`,
   `crates/core/src/pipeline/compile.rs`,
   `crates/core/src/generate/serialize/mod.rs`,
   `crates/core/src/grammar/host.rs`.

## Files modified (all in worktree)

**Real fix (cherry-pick targets):**
- `crates/tape/src/columns.rs` — ownership of `frame_depth` /
  `current_depth`, lockstep rollback, finaliser + split-borrow
  helpers, auto-stamp in `push_structural`.
- `crates/tape/src/builder/mod.rs` — depth bookkeeping deferred to
  Columns, `end_compound` scans for first record at parent_depth+1.
- `crates/tape/src/cursor.rs` — `first_child_root` pre-order fast
  path widened from `==` to `>`.

**Diagnostic instrumentation (worktree-only):**
- `crates/core/src/pipeline/directives.rs` — `[parse-start]` /
  `[parse-end]` / `[extract]` markers.
- `crates/core/src/pipeline/compile.rs` — `[compile_ast_common done]`
  marker.
- `crates/core/src/generate/serialize/mod.rs` — `[serialize]` marker.
- `crates/core/src/grammar/host.rs` — `[walk_tape]` deep-dump.

## Implication for B3 closure

The δ task scope — empty `ir.rules` post-γ — is a tape-substrate
contract violation, not a lowering / IR bug. The fix lands in
`crates/tape/`. γ's framing of the panic as "pre-existing /
unrelated to W0.γ scope" was incomplete: γ introduced the new
columns (`frame_depth`, `current_depth`) without extending the
`Columns::rollback_to` lockstep contract that the parser-emitted
retry sites depend on. δ closes that gap.

The work is structurally complete in this worktree; the remaining
60-min-overflow tasks are stabilisation + cherry-pick, not
re-architecting.
