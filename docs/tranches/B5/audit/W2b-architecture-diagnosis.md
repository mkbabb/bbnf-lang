# B5.W2b — Architectural diagnosis: Parts 1+2 viability under contact

## Executive verdict

**Verdict B.** Parts 1+2 ARE achievable, but only via a substrate
transposition that inverts the depth-stamp invariant — a change
larger than W2's "single agent, 90 min, 250 LOC" budget. The α
audit's "bookkeeping correcting bookkeeping" framing was technically
correct (the cascade IS a second writer to `frame_depth`), but the
W2.md prescription pointed at the wrong refactor target. The right
fix moves the depth-write from compound-close-time to
compound-children-enter-time; W2.md instead added a derived field to
`ValueCheckpoint`, which composes badly with rollback's
`direct_child_count` accounting (the failure mode the prior agent
hit on bbnf wrap shapes).

## Why ζ's cascade exists

Fused parse-without-pre-pass produces records in **emission order**;
shape emitters publish two layouts:

- **Pre-order**: `begin_compound` writes the compound row FIRST, then
  body emits children at offsets `> open_offset`. Closing
  (`end_compound`) backpatches `span_hi` and `child_off`. The
  compound row stamps at the parser's `current_depth` BEFORE the bump
  (`tape.rs:849-862`); children stamp at `current_depth + 1` (post-bump
  state). Depth is correct at push time — no fix-up needed.
- **Post-order** (flat Seq, Wrap rule, repeat iter, arglist, Pratt
  outer): body emits children FIRST; the compound row pushes LAST via
  `begin_compound`. At children's push time, `current_depth` reflects
  the OUTER frame, not the to-be-opened compound — so children stamp
  one level too SHALLOW. The `+1` cascade in
  `end_compound_post_order` (`tape.rs:914-938`) walks
  `[leftmost_descendant_offset(first_child) .. open_offset)` and
  bumps every byte by 1, retroactively lifting every transitive
  descendant into the correct depth slot.

The cascade is NOT redundant accounting; it is the **structural
consequence of stamping depth at push-time when the depth bit is
inferable only after the compound's opening becomes structurally
visible** (B3 FINAL §2 — fix 3, "End-compound bump scope widened",
landed in `B3.W0.ζ`). Without the cascade, post-order children sit
at the outer depth; the finaliser (`finaliser.rs:268-302`) reads
those bytes as authoritative and chains cousins as siblings of their
parent's siblings — the exact pathology B3.W0.η's lowering
cousin-leak guard caught.

The cascade reaches transitively because of nested post-order:
`first_child` may itself be a post-order compound. `child_off` for
that compound points strictly backward (its body lives at offsets
below `first_child.0`); so the bump must extend down to
`leftmost_descendant_offset` (`tape.rs:1520-1530`). For a balanced
tree the per-close walk is O(log N); pathological deep-nest inputs
push it toward O(N) per close.

## Mechanism diagram — `Seq[Pratt[a, b]] -> Wrap` worked example

Outer Wrap is post-order (rule wraps a single Seq branch); inner Seq
is post-order (flat shape); Pratt's outer is post-order. Push trace:

```
push leaf a       idx=0  current_depth=0  frame_depth[0]=0
push leaf op      idx=1  current_depth=0  frame_depth[1]=0
push leaf b       idx=2  current_depth=0  frame_depth[2]=0
begin Pratt outer idx=3  current_depth=0  frame_depth[3]=0   (bumps to 1)
end Pratt p.o.    cascade [0..3] += 1 → frame_depth = [1,1,1,0]
                  current_depth → 0
begin Seq outer   idx=4  current_depth=0  frame_depth[4]=0   (bumps to 1)
end Seq p.o.      cascade [0..4] += 1 → frame_depth = [2,2,2,1,0]
                  current_depth → 0
begin Wrap outer  idx=5  current_depth=0  frame_depth[5]=0   (bumps to 1)
end Wrap p.o.     cascade [0..5] += 1 → frame_depth = [3,3,3,2,1,0]
                  current_depth → 0
```

Final depth column: `[3, 3, 3, 2, 1, 0]` — leaves at depth 3, Pratt
at 2, Seq at 1, Wrap at 0. Each `+1` cascade is the ancestor-
acquisition under post-order reveal. The finaliser (Stage-C) reads
these bytes and derives `sib_skip`/`child_off`/`span_hi` correctly.

## Why naive Part 1+2 does not work

W2.md Part 1 prescribes: extend `ValueCheckpoint` with
`first_child_off: Option<u32>`; set it in `Columns::push_*` on the
first push since the latest open checkpoint; `end_compound` reads it
and skips the forward scan.

Two independent failure modes compose:

**Failure 1 — post-order has no checkpoint for the closing
compound.** In the Wrap shape (`shapes/wrap.rs:506-553`), the body
emits branches; `begin_compound` runs LAST. At each child push,
`value_open_stack.last()` is the OUTER-OUTER's checkpoint (the
parent of Wrap), not Wrap itself — Wrap has no checkpoint yet. So
the push-time hook writes Wrap's first child as the OUTER-OUTER's
`first_child_off`. If outer-outer already has children, the Option
guard suppresses the write (correct); but Wrap's own first_child is
never recorded by the hook because no checkpoint represents Wrap at
push time.

**Failure 2 — rollback semantics for derived fields.** `Columns::rollback_to`
(`columns.rs:259-316`) decrements survivor's `direct_child_count`
by 1 (saturating) — the failed branch represented one direct child
of the survivor. `first_child_off` cannot be saturating-decremented:
it is an offset, not a count. If the failed branch contained the
SURVIVOR's first child, post-rollback `first_child_off` points at a
truncated row; the next attempt re-pushes at the same offset, but
the Option guard suppresses the re-write because the field is
already `Some(...)`. The cached value happens to be correct ONLY by
coincidence (re-push at same offset). Multi-branch alternation
where the first branch fails and subsequent branches push at a
different offset breaks this — `first_child_off` retains the stale
offset of the rolled-back row.

The bbnf wrap-shape grammar exercises both: the recursive `value`
rule alternates over `wrap`/`pratt`/`literal` branches with
post-order Seq inners. The prior agent's tape produced empty IRs
because outer-Wrap's `child_off` resolved to a stale offset that the
finaliser then chained as a zero-length sibling run.

W2.md Part 2 (delete cascade, single depth stamp at close) compounds
this: without the cascade, post-order children's depth stays at the
outer level; the finaliser reads them as siblings of the
to-be-closed compound's parent. There is no "stamp once at close"
solution that doesn't traverse the children — and traversing the
children IS the cascade in different clothing.

## The architectural fix (Verdict B path forward)

Invert the depth-stamp invariant: instead of stamping push-time then
fixing up at close, stamp push-time at the CORRECT depth by bumping
`current_depth` BEFORE the body emits.

**New substrate API**:

1. `Tape::enter_post_order_children() -> u8` — saves and returns
   `current_depth`, then increments. Shape emitters call this before
   emitting a post-order shape's body.
2. Split `begin_compound` into `begin_compound_pre` (current
   semantics: stamp at `current_depth`, then bump) and
   `begin_compound_post` (stamp at `current_depth - 1` — the saved
   outer depth — without bumping, since `enter_post_order_children`
   already bumped).
3. `end_compound_post_order` reduces to `set_span_hi_at` +
   `set_child_off_at` + `or_extra_at(HAS_CHILDREN_BIT)` +
   `current_depth -= 1`. The leftmost-descendant cascade and the
   helper `leftmost_descendant_offset` (`tape.rs:1504-1530`) delete.
4. `end_compound` (pre-order) loses its forward scan
   (`tape.rs:884-897`): `child_off = open_offset + 1` directly,
   because no inner post-order close mutates frame_depth anymore.
5. Each post-order shape emitter holds the depth returned by
   `enter_post_order_children` as a local, mirroring `attempt_len`'s
   lifecycle. On retry-loop branch failure where no compound row
   gets emitted (e.g. `flat.rs:584-587`'s repeat-iter rollback),
   the emitter calls a complementary `Tape::exit_post_order_children(saved)`
   to restore `current_depth`. The successful path's
   `end_compound_post_order` decrements `current_depth` once,
   absorbing the bump the matching `enter_post_order_children`
   added.

**Worked example, same fixture under new substrate**:

```
enter_post_order_children (Wrap)  current_depth: 0 → 1; saved=0
enter_post_order_children (Seq)   current_depth: 1 → 2; saved=1
enter_post_order_children (Pratt) current_depth: 2 → 3; saved=2
push leaf a       idx=0  frame_depth[0]=3 ✓
push leaf op      idx=1  frame_depth[1]=3 ✓
push leaf b       idx=2  frame_depth[2]=3 ✓
begin_compound_post(Pratt)   idx=3  frame_depth[3]=2 ✓  (no bump)
end_compound_post_order      current_depth → 2
begin_compound_post(Seq)     idx=4  frame_depth[4]=1 ✓  (no bump)
end_compound_post_order      current_depth → 1
begin_compound_post(Wrap)    idx=5  frame_depth[5]=0 ✓  (no bump)
end_compound_post_order      current_depth → 0
```

Final: `[3, 3, 3, 2, 1, 0]` — identical to current substrate, but
each byte is **written exactly once at push time**.

**Single-writer invariant**: `frame_depth` is written by
`push_structural` (and only by `push_structural`); never by
`end_compound_post_order`; never by `end_compound`. Cascade gone.
Forward scan gone. `leftmost_descendant_offset` gone.

## Consumers + ripple analysis

Files that change under Verdict B:

- `crates/tape/src/columns.rs` — `rollback_to` unchanged (existing
  `frame_depth[new_len]` recovery already gives the right answer
  under the inverted invariant: rolled-back row's stamped depth is
  the depth-to-resume-at).
- `crates/tape/src/tape.rs` — `begin_compound` splits into
  `begin_compound_pre`/`begin_compound_post` (or one method with a
  `post_order: bool` arg); `enter_post_order_children` /
  `exit_post_order_children` land; `end_compound_post_order`
  shrinks ~30 LOC; `leftmost_descendant_offset` deletes; `end_compound`
  forward scan deletes ~15 LOC.
- `crates/core/src/backend/rust/emitter/shapes/flat.rs` — every
  `let outer_child = builder.position();` paired with a new
  `builder.enter_post_order_children();` call; `begin_compound` →
  `begin_compound_post`. ~6 sites.
- `crates/core/src/backend/rust/emitter/shapes/wrap.rs` — same. 1 site.
- `crates/core/src/backend/rust/emitter/shapes/arglist.rs` — same. ~4 sites.
- `crates/core/src/backend/rust/emitter/shapes/pratt.rs` — same. ~2 sites.
- `crates/core/src/backend/rust/emitter/shapes/dispatcher.rs`,
  `inline.rs` — audit for post-order calls.
- `crates/core/src/grammar/generated/{bbnf,json,css_l4,css_pretty,csv,ebnf,bnf,google_sheets,math}.rs`
  — full regen.

LOC delta estimate: **net negative ~50 LOC in `crates/tape/`**
(cascade + leftmost helper + forward scan all delete; two new
trivial primitives land), **net positive ~30 LOC in shape emitters**
(one enter_/exit_ pair per post-order site), **regenerated grammar
files flat-to-slightly-larger**. Confidence: **medium-high** that
the substrate works — the worked example composes, rollback paths
need no substrate ceremony beyond the existing
`frame_depth[new_len]` read, and the per-shape `enter_/exit_`
discipline mirrors the `attempt_len`/`save_p` pairs already
threaded through retry-loop emitters. **Medium** that every retry
edge case (repeat zero-width breakout `flat.rs:589-593`, wrap
multi-branch alternation `wrap.rs:509-522`, optional retry) finds a
clean exit-without-compound-row site for the matching
`exit_post_order_children` call.

Performance: `compile_bbnf` median expected within 5% of B4 baseline
(2.831 ms). The replaced cascade is O(spine-depth) per close;
amortised total work is O(N) for both substrates. Hot-path effect
should be neutral or marginally positive.

## Decision recommendation

**Open B5.W6** (or replace W2 in-place if PROGRESS.md permits)
"Depth-stamp invariant inversion" with the substrate transposition
above. W2's gate items 4 (`leftmost_descendant_offset` deletion) and
the forward-scan deletion in `end_compound` carry over; Parts 3 and 4
(packed_cache delete + pay_wide/pay_f64 merge — landed cleanly at
`0daf6f01`/`1b462092`) stay closed; Part 1's `ValueCheckpoint::first_child_off`
mechanism is **retired from the gate as plan-time-incorrect**. The
correct mechanism captures `first_child` via shape-level
`builder.position()` (already done in current emitters at e.g.
`flat.rs:136`, `flat.rs:351`, `wrap.rs:506`); `first_child` flows
through `end_compound_post_order`'s `first_child: TapeOffset`
parameter, not through the value checkpoint.

Action item for the orchestrator: dispatch a B5.W6 plan-author
sub-agent against this diagnosis. The wave's substrate budget is
~90 min implementation + 30 min regen sweep + 60 min grammar parity
verification. Single-agent feasible.

## Top-3 file:line citations to read first

1. `crates/tape/src/tape.rs:914-938` — `end_compound_post_order` body
   carrying the cascade; the surface that shrinks.
2. `crates/tape/src/tape.rs:871-906` — `end_compound` body carrying
   the forward scan; the surface that simplifies.
3. `crates/core/src/backend/rust/emitter/shapes/wrap.rs:504-555` — the
   highest-risk shape under the proposed transposition; the
   rollback-loop pattern that must compose with `enter_depth` save
   and restore.
