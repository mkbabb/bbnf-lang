# B3.W0.ε — closure on parser-hang resolution + residual host recursion

**Status**: TAPE ARCHITECTURE RESTORED (Phases 1–3 complete). RESIDUAL
HOST-SIDE STACK OVERFLOW in `find_descendant_by_kind` on json
(Phases 4–5 incomplete; below).

## Phase 1 — δ worktree cleanup

The δ worktree at `bbnf-wt-b3-w0d-serialize` carried seven modified
files. Audit per file:

| File | Verdict |
|---|---|
| `crates/tape/src/columns.rs` | **kept** — δ's frame_depth/current_depth migration into `Columns`, atomic rollback, finalise/split-borrow helpers, push_structural auto-stamp. |
| `crates/tape/src/builder/mod.rs` | **kept + extended** — δ's bookkeeping deferral to Columns + scan-for-target-depth in `end_compound`, plus B3.W0.ε structural-scope bound. |
| `crates/tape/src/cursor.rs` | **kept + extended** — δ's `start > parent_idx` widening, plus B3.W0.ε leap-only-when-`co_child_off < co` guard on the post-order backward walk. |
| `crates/core/src/grammar/host.rs` | **discarded** — diagnostic `[walk_tape]` deep-dump only. |
| `crates/core/src/pipeline/compile.rs` | **discarded** — `[compile_ast_common done]` marker only. |
| `crates/core/src/pipeline/directives.rs` | **discarded** — `[parse-start]` / `[parse-end]` / `[extract]` markers only. |
| `crates/core/src/generate/serialize/mod.rs` | **discarded** — `[serialize]` marker only. |

No workaround patches were retained. The four `core/` files are
straight-line `git checkout HEAD --` reverts to the master baseline.

## Phase 2 — residual hang location

After applying δ's tape changes alone (cursor widening + end_compound
target-depth scan + Columns rollback parity), `xtask regen
--grammar json` advanced past the parser baseline (`[parse-end]
wall=238 µs`) but stalled inside `extract_for_pipeline`.

Hot stack from `sample` (15 s window, 11 423 events, 100% in-frame):

```
parse_to_pipeline_inputs                                            directives.rs:103
  absorb_item                                                        host.rs:361
    find_descendant_by_kind                                          tape_walk.rs:117
      find_descendant_by_kind                                        tape_walk.rs:117
        find_descendant_by_kind                                      tape_walk.rs:117
          find_descendant_by_kind                                    tape_walk.rs:117
            find_descendant_by_kind                                  tape_walk.rs:117
              find_descendant_by_kind                                tape_walk.rs:117
                find_descendant_by_kind                              tape_walk.rs:117
                  find_descendant_by_kind                            tape_walk.rs:113
```

Captured at `.profiles/b3/parser-hang/sample-stacks-w0e.txt`
(equivalently /tmp/b3-w0e-sample-stacks.txt).

## Phase 3 — root cause + cursor fix

γ's `derive_frame_depth` retirement note already identified the
defect class — a backward-walk leap that goes upward when it
encounters a pre-order child compound (`co.child_off > co`). The
same defect lives in `cursor.rs::first_child_root`'s post-order
fallback at line 640:

```rust
pos = if has_children && !co_child_off.is_none() {
    co_child_off.0       // unconditional leap; spins when co_child_off > co
} else {
    co
};
```

**B3.W0.ε fix (`crates/tape/src/cursor.rs`)** — leap only when the
target points strictly before `co`; step by one otherwise so the
walk monotonically descends to `start`:

```rust
pos = if has_children && !co_child_off.is_none() && co_child_off.0 < co {
    co_child_off.0
} else {
    co
};
```

This eliminates the cursor-side first_child seeding spin. With the
cursor fix in, `find_descendant_by_kind` no longer hangs on a
single `view.children()` enumeration; the recursion enumerates
children correctly and bottoms in finite time on canonical tapes.

A second B3.W0.ε refinement bounds δ's `end_compound` scan so a
childless compound followed by no later same-depth records remains
correctly childless rather than stamping `child_off` at an
unrelated descendant of a later sibling — the bound triggers when
`frame_depth[i] <= open_depth` (the scan crossed out of the
compound's structural scope).

## Phase 4 — verification (PARTIAL)

| Step | Result |
|---|---|
| `cargo build -p xtask --release` | exit 0 (1m 02s, last build) |
| `cargo nextest run -p tape --profile ax-iter` | **100/100 pass** in 0.154 s |
| `xtask regen --grammar json` (cursor fix in place) | **fails** with `thread 'main' has overflowed its stack` |
| `xtask regen --grammar bbnf` | NOT YET ATTEMPTED (depends on json closure) |

Tape architecture is sound — every unit test passes, the parser
no longer spins, the cursor's first_child_root no longer spins on
mixed-shape compounds.

The residual stack overflow lands inside
`crates/core/src/lower/tape_walk.rs::find_descendant_by_kind`. With a
diagnostic depth cap + visited-offset HashSet wrapped around
`find_descendant_by_kind`, the cycle detector logs:

```
[FDB] CYCLE depth=10 OFF=324 view.span=(285, 319) kind=Rule rule_kind=float_lit
[FDB] CYCLE depth=9  OFF=325 view.span=(319, 321) kind=Span  rule_kind=int_lit
[FDB] CYCLE depth=9  OFF=333 view.span=(322, 326) kind=Seq   rule_kind=mapped_factor
```

— i.e. within a single `absorb_item` invocation, the descent
revisits offsets seen at strictly shallower depths. The cursor
returns offsets via `child_off` + `sib_skip`; the latter is set by
the finaliser as a positive forward delta on same-depth runs of
`frame_depth`, the former is set at parse time. Neither writes
should produce a cycle on its own — but the descent observes one,
which means some compound's `child_off` (or some `sib_skip` chain)
points back into an ancestor's structural scope.

Possible mechanisms (not yet localised within remaining cap):

1. `end_compound_post_order`'s retroactive frame_depth bump
   propagates up the call stack of nested post-order shapes. If two
   independent parent post-order frames bump overlapping ranges,
   records that should sit at distinct depths land at the same
   final depth — sib_skip then groups them as siblings, and the
   cursor's children walk yields one parent's grandchild as
   another parent's direct child.
2. The finaliser's same-depth-run sibling derivation visits records
   in emission order. If the bumping promoted a deeply-nested
   record up to the parent's `target_depth`, the parent's
   `child_off` (set by my new scan) lands on the bumped record;
   the cursor's iteration via sib_skip then yields all records at
   that depth — including legitimate direct children further along
   *and* further-bumped descendants that happen to share depth.

The cycle detector's first hit at `OFF=324` (rule_kind=`float_lit`,
span=(285,319) — a deeply nested compound inside the bbnf grammar's
own AST representation of the json source) is consistent with (2):
`float_lit` is itself a parser-emitted compound, and its descendants
include further `term` / `factor` / `mapped_factor` compounds that
under bumping may resurface at `float_lit`'s own depth.

## Phase 5 — cherry-pick (PARTIAL)

The δ + ε architectural fix landed in worktree commit
`e5a5902b` and was cherry-picked to master as `e97b2ae7`:

```
e97b2ae7 fix(tape): atomic depth rollback + scope-bounded scan + cycle-safe backward walk (B3.W0.δ + ε)
```

Master tree: `crates/tape/src/{builder/mod.rs, columns.rs, cursor.rs}`
carry δ's fix + ε's two refinements. End-to-end json/bbnf regen is
NOT YET green; the residual host-side recursion needs a separate
sub-agent dispatch to localise within `find_descendant_by_kind`'s
descent path or the underlying tape `child_off`/`sib_skip` graph.

## Outstanding work for next sub-agent

1. Reproduce the FDB cycle log (instrument
   `find_descendant_by_kind` with offset HashSet + depth>18 print)
   and trace which compound's `child_off` produces the
   ancestor-revisit. Print the parent's offset alongside each
   descent step.
2. Compare bumped-depth records' final `frame_depth` against the
   `child_off` writes that target them — confirm whether any
   compound's `child_off` lands on a record that the bumping
   promoted to its `target_depth` from a deeper original level.
3. Decide between two architectural fixes: (a) restrict
   `end_compound`'s scan to records emitted strictly between
   `open_offset+1` and the tape `len` *as of begin_compound time*
   (capturing `pre_body_len` at begin and bounding the scan to
   that prefix), or (b) thread a structural-scope boundary
   through `frame_depth` itself so the bumping doesn't promote
   deeply-nested records into the parent's direct-child layer.

## Verifications

- `cargo nextest run -p tape --profile ax-iter` — 100 tests
  passed, 0 skipped (0.154 s).
- `cargo build -p xtask --release` — exit 0 (1 m 02 s).
- `xtask regen --grammar json` — stack overflow inside
  `find_descendant_by_kind` recursion; tape parser advances past
  baseline.
