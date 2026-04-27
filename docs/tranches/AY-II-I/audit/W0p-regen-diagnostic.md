# AY-II.W0' — Bootstrap regen diagnostic relinquish

**Date**: 2026-04-21
**Master HEAD at halt**: `60f92743`
**Halting orchestrator**: self (after two 12-15 min wall-clock regen attempts with no forward motion)
**Relinquish trigger**: SPEC §Diagnostic-loop relinquish — 30+ min wall time, zero forward motion, per user direction ("Triumvirate assay and redress").

## Symptom

`bash scripts/bootstrap-bbnf.sh` runs the `cargo expand -p bbnf-bootstrap --lib`
step and does not return. Two observed runs:

| Run | Elapsed at kill | rustc CPU | Phase reached | `generated.rs` changed? |
|---|---|---|---|---|
| 1 | 12:00 | 99-100% | still in `cargo expand` | no (md5 unchanged) |
| 2 | 15:22 | 99-100% | still in `cargo expand` | no (md5 unchanged) |

Historical baseline (pre-W0'):
- Pause snapshot entry noted "7+ min rustc expand without output" in-worktree
  under contention
- Clean-master regen historically 3-6 min
- 15 min under no target contention is 2-5× the prior ceiling

`generated.rs` MD5 before & during regen: `5dc64f644d5719edef3cabc8f3dfef68`
(pre-W0'-regen content — the script never reaches its `> $OUTPUT` redirect).

## What changed at `60f92743`

W0' cherry-pick ledger onto master (no conflicts except view/value.rs in W0'.b
which resolved by splicing W0'.c's `emit_path_query_impls` into W0'.b's file):

1. `bd563c1d` — `refactor(tape): collapse TapeBuilder + ValueBuilder into FusedBuilder`
2. `4edfac88` — `refactor(tape): rename finish -> finish_fused; keep finish tape-only`
3. `9c9906c8` — `refactor(runtime): retire standalone ValueBuilder; Parsed holds FusedOutput`
4. `0beda457` — `refactor(emitter): single FusedBuilder parse-entry; retire parse_with_visitor`
5. `91355d25` — `fix(runtime): preserve 4-arg new_fused pre-regen`
6. `3a7df479` — `refactor(tape): ungate FusedBuilder new-call counter`
7. `1bfcf359` — `docs(tape, runtime): scrub hard-gate pattern matches from doc comments`
8. `30aa83aa` — `feat(view): splice STRUCTURAL_SCAN_POLICY into emit_path_walk at codegen`
9. `0993cc89` — `chore(emitter): retire W0-era #[allow(dead_code)] at 8 sites`
10. `bc8fa8b2` — `fix(view): route scan-policy match arms through raw rule names`
11. `550dac11` — `feat(view,emitter,tests): route project_value_<G> through materialize_projection_*_<G>`
12. `b1bb4579` — `fix(view,emitter): raw_name for materializer lookup + runtime tape path`
13. `60f92743` — `refactor(tests): migrate push_compound/mark_children -> FusedBuilder API`

Pre-W0' master HEAD was `5741b835` (pause snapshot). The last known-good regen
occurred around `f372e7ef` (W0 compose bridge). No regen has been attempted on
master between `5741b835` and `60f92743`.

## Orthogonal: draft-fix diff

The orchestrator applied a 26-site sed substitution across 14 files under
`crates/core/src/backend/rust/emitter/shapes/` to rename
`::bbnf::runtime::tape::TapeBuilder` → `::bbnf::runtime::tape::FusedBuilder`
in the emitter's `quote!` output. The diff was stashed uncommitted as
`W0p-regen-draft-fix emitter TapeBuilder->FusedBuilder` (also saved at
`/tmp/w0p-regen-draft-fix.diff`, 290 lines). Both regen attempts above
included this diff in the working tree.

Whether the stash is load-bearing for the slowdown is an open question the
research agent must answer: the rename is cosmetic pre-alias-retirement
(the tape crate carries `pub type TapeBuilder = FusedBuilder;` at
`crates/tape/src/builder/mod.rs:1203` so either name resolves to the same
type), so reverting the stash should not functionally change expansion —
but the proc-macro's code path feeding through it may differ.

## Baseline facts

- `cargo check --profile ax-iter -p bbnf --lib` completes in **0.36s** at the
  composed state — the library is structurally sound; the proc-macro trip
  alone is the slow path.
- `cargo check --profile ax-iter -p tape --tests` completes in **0.16s**.
- `cargo check --profile ax-iter --workspace` entered gorgeous compilation
  with multiple background rustc processes each consuming 99% CPU for
  10-50 minutes across earlier attempts. gorgeous uses
  `#[derive(Parser)]` on five different grammar files
  (ebnf.rs, bbnf.rs, json.rs, google_sheets.rs, jit.rs), so each cargo check
  invocation re-runs the emitter five times. The fact that gorgeous
  compile-times match the bbnf-bootstrap regen cost (10-15 min per
  proc-macro invocation) points at a common cause.

## Candidate root causes (for research agent to confirm/reject)

1. **`view/value.rs` merge introduced a pathological emit path.** W0'.b's
   rewrite of `emit_value_surface` + `collect_variant_classes` +
   `classify_shape` against `ProjectionAdmission` could have a loop,
   redundant IR walk, or exponential clone chain. View the merged file at
   `crates/core/src/backend/rust/view/value.rs` (1114 lines) vs pre-W0'-b
   (`git show 550dac11^:crates/core/src/backend/rust/view/value.rs`, 733
   lines).
2. **`collect_projection_admissions` called twice on the same IR** —
   `view/value.rs:102` and `view/value.rs:1054` both call it; if the fn is
   expensive and a ProjectionAdmission builder clones children deeply, two
   calls compound.
3. **W0'.c scan-policy splice emits N-way match arms over rule_kind()** —
   the emitter walks every non-transparent rule in `emit_path_query_impls`
   and composes `field_fast_key_seek` / `field_fast_bounded` /
   `index_fast_scan` arm groups. For a 600-rule grammar that's >1800 match
   arms; if any helper inside that walk is O(N²) the total is O(N³).
3. **W0'.a value-substrate parallel columns ballooned the per-record cost
   of `begin_compound` / `end_compound_post_order` at codegen time** —
   each `quote!` block in the emitter that references the builder now
   expands to more tokens (two column families stamped per call) than
   pre-W0'.
4. **The `cargo expand` post-processor (python regex) is fine** — the
   stall is pre-post-processor; cargo expand itself does not return.
5. **Something on macOS Darwin 25.4 rustc nightly** (low-priority given
   pre-W0' regen worked at the same toolchain).

## Questions for the research agent

1. Can you reproduce the stall at master `60f92743` (with stash popped,
   with stash unpopped, or both)?
2. At the cargo-expand layer, can you capture what rustc is doing —
   `RUST_LOG=rustc_expand=trace` or equivalent? Which macro invocation is
   the bottleneck?
3. Compare the pre-W0'-b `view/value.rs` emit output against the post-W0'-b
   version for a small grammar (e.g. JSON) — do the LOC of the emitted
   `impl ValueRoot` grow by 2×, 10×, 100×?
4. Is there a deep-clone in `collect_variant_classes` /
   `collect_projection_admissions` that compounds across `emit_project_arm`
   iterations?
5. Does reverting the `TapeBuilder → FusedBuilder` stash measurably change
   regen time?

## Invariants that must hold after redress

- Every W0' cherry-picked commit's change stays landed (AY-II.md §Plan-audit
  findings, §Invariants 1-13, W0p.md §Invariants 14-19). Reverting any
  W0' commit is out of scope for this redress.
- `FusedBuilder` is the sole builder type (W0p.md §14).
- `push_compound` / `mark_children` do not return (W0p.md §15).
- Projection totality + scan-policy splice remain (W0p.md §16, §17).
- `Parsed::to_value()` does not panic (W0p.md §19).
- Regen idempotency holds (W0p.md §Hard gate 7): cycle-1 == cycle-2
  byte-identical.

## Deliverable

Root-cause attribution document at
`docs/tranches/AY-II/audit/W0p-regen-root-cause.md` naming the code path +
file:line + mechanism. The plan agent reads it next to author the fix.

## Relinquish artefacts

- `/tmp/regen.log` — script stdout capture; contains only "Expanding
  bbnf-bootstrap..." (Post-processing never reached).
- `/tmp/w0p-regen-draft-fix.diff` — 290-line TapeBuilder→FusedBuilder diff
  stash (currently popped off working tree; recover with
  `git stash pop` or `git apply /tmp/w0p-regen-draft-fix.diff`).
- Master HEAD: `60f92743` — clean working tree as of this halt.
