# B3.W0.ζ — tape-graph cycle resolved end-to-end

**Status**: TAPE-GRAPH CYCLE RESOLVED (Phases 1–5 complete, Phase 6
audit landed). Residual non-tape lowering panic in
`lower_binary_factor` is a pre-existing Pratt-emission bug, distinct
from the W0.γ–ζ tape architecture work.

## Phase 1 — reproduction + offending compound

Diagnostic instrumentation in `end_compound_post_order` and a
post-finalise frame_depth dump captured the offending state. With
the W0.ε `find_descendant_by_kind` HashSet+depth probe carried
forward, the cycle log surfaced:

```
[FDB-ζ] depth=10 off=324 revisit=true kind=Rule rule_kind=float_lit span=(285,319)
[FDB-ζ] depth=9  off=325 revisit=true kind=Span rule_kind=int_lit span=(319,321)
[FDB-ζ] depth=9  off=333 revisit=true kind=Seq  rule_kind=mapped_factor span=(322,326)
```

The post-finalise depth dump for OFF≈324 read:

```
off=323 d=11  kind=Seq variant=32 child_off=264
off=324 d=11  kind=Rule variant=1 child_off=255  ← float_lit
off=325 d=11  kind=Span variant=0
off=333 d=11  kind=Seq  variant=32 child_off=326
```

Records 262, 263, 323, 324, 325, 333 ALL sat at `frame_depth=11`,
forming an unbroken finaliser sib_skip chain at depth 11.

## Phase 2 — case identification

Case (b) (per ε's outstanding-work classification): **the
`end_compound_post_order` bump scope was too narrow**, not too
broad. The bump applied to the offset range `[first_child,
open_offset)`. For OFF=334 (Rule, post-order, `first_child=324`),
the range `[324, 334)` SKIPPED records `[255, 323]` — but those are
descendants of 324 (themselves wrapped under 334), so they
under-bumped by 1 relative to 324.

Concretely 324 ended up at depth 11, and its child 323 also at
depth 11 (instead of 12). The finaliser's same-depth chain
grouped 324 with the records ahead of it (262, 263, 323) AND with
325, 333 — and the cursor's `first_child_root(324)`'s post-order
backward walk then leaped through the chain such that
`ChildIter(324)` yielded 324 itself as one of its own children
through 323's child_off=264 leap-and-walk.

## Phase 3 — fix (commit `62a62a29` worktree, `0f1c3fea` master)

`crates/tape/src/builder/mod.rs` — extend the bump range to cover
the entire subtree by walking the leftmost-descendant chain from
`first_child`:

```rust
fn leftmost_descendant_offset(columns: &Columns, start: u32) -> u32 {
    let mut off = start;
    while columns.has_children_at(off) {
        let co = columns.child_off_at(off);
        if co.is_none() || co.0 >= off { break; }
        off = co.0;
    }
    off
}
```

`end_compound_post_order` calls
`leftmost_descendant_offset(&self.columns, first_child.0)` and
bumps `[leftmost, open_offset)`. The walk follows `child_off` only
when it points strictly backward (canonical post-order subtree
root); pre-order children and leaves stop the walk because their
subtrees occupy offsets ABOVE the parent. Bounded by post-order
chain depth, runs at-most once per close.

## Phase 4 — verification

| Step | Result |
|---|---|
| `cargo build -p xtask --release` | exit 0 |
| `cargo nextest run -p tape --profile ax-iter` | **100/100 pass** in 0.154 s |
| `cargo check -p bbnf -p xtask -p bbnf_derive --profile ax-iter` | **exit 0** |
| `xtask regen --grammar json` | parser advances past prior cycle; lowering panic at `lower/expression.rs:465` (residual, see Phase 5) |
| `xtask regen --grammar bbnf` | same residual lowering panic |

The tape architecture is sound. Tape tests 100/100. Workspace
type-checks exit 0. The cycle is gone — `find_descendant_by_kind`
runs to completion without revisits.

## Phase 5 — residual lowering panic (PRE-EXISTING, OUT OF SCOPE)

End-to-end json/bbnf regen still does not complete. The new
failure is at:

```
crates/core/src/lower/expression.rs:465
panicked: binary_factor could not resolve operator —
no binary_operators child and source gap "" contains no recognized
token (chain = "\"null\" -> 0u8 ")
```

This is a Pratt-emission ↔ lowering bridge bug, distinct from the
tape-graph cycle. `parse_pratt_BbnfBootstrap_binary_factor`
(crates/core/src/grammar/generated.rs:8617) emits an outer Rule
compound and then unconditionally writes `set_child_off_at(outer_off,
this_operand_root)` AFTER `end_compound`. When the operator-
detection loop never fires (the binary_factor chain has only one
mapped_factor), `this_operand_root` retains its initial value
`outer_off + 1` — i.e. the FIRST RECORD INSIDE the operand
compound's body emission, NOT the operand compound itself.

The cursor's children iteration on `outer_off` therefore enters
the operand's body interior rather than landing on the operand
compound's row. `lower_binary_factor`'s walker-era branch then
sees multiple body interior records as separate "operands" and
panics when no operator separates them.

This bug is independent of the W0.γ–ζ tape work and was masked by
the tape-graph cycle — the parser never reached this lowering
path. End-to-end completion needs a separate fix in the Pratt
emitter (or its lowering bridge) to either (a) skip the
`set_child_off_at` override when no reducers fired, leaving
`end_compound`'s scan-found `child_off` (which correctly points at
the operand compound), or (b) record operand offset structurally so
`this_operand_root` reflects the operand compound's row.

## Phase 6 — cherry-pick + audit

| Artefact | SHA / path |
|---|---|
| Worktree fix-commit | `62a62a29` (`bbnf-wt-b3-w0z-cycle` branch) |
| Master fix-commit | `0f1c3fea` |
| Audit (this doc) | `.profiles/b3/parser-hang/z-rootcause.md` |

Worktree: `/Users/mkbabb/Programming/bbnf-wt-b3-w0z-cycle` (clean).
Main repo: `/Users/mkbabb/Programming/bbnf-lang` (cherry-pick
landed; this audit + audit-commit follow).
