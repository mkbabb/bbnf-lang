# AZ-II.cutover.C - Tape Deletion Scope Reveal
**Opens after**: AZ-II.cutover.B close
**Agents**: up to 10 parallel
**Hard gate**: tape deletion is blocked by measured BBNF consumer surface and rerouted before any partial deletion lands.
**Status**: complete_with_misses

## Scope

1. Attempt the planned `crates/tape/` deletion gate after byte-equal
   BBNF bootstrap.
2. Measure live BBNF node-view and tape-shaped consumer references.
3. Identify the cross-crate consumer surface too large for the original
   120-minute deletion cap.
4. Preserve benchmark/archive placeholders without claiming terminal
   performance truth.
5. Route consumer migration to cutover.D and substrate repair to
   cutover.E/F/G/H.
6. Commit the scope-reveal report before continuing.

## File bounds

| File | Access |
|---|---|
| `docs/tranches/AZ-II/audit/cutover.C-SCOPE-REVEAL.md` | create |
| `docs/benchmarks/post-AZ-II.json` | create |
| `docs/benchmarks/post-AY-AZ-II-close-*.txt` | create |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/README.md` | modify |

**Do NOT touch**: source-code tape deletion, generated parser
substantial rewrites, BBNF runtime substrate, resolver arms.
Deployment invariant: diagnostic agents work in sibling fully-contained
worktrees; no source deletion proceeds until the scope-reveal document
lands and names follow-on waves.

## Phase sub-items

### AZ-II.cutover.C.1 Live Tape Reference Census

Mechanism: scan `crates/` and generated BBNF for `TapeCursor`,
`BbnfBootstrapNodeView`, `ValueRoot`, and tape runtime references.

Files touched: `docs/tranches/AZ-II/audit/cutover.C-SCOPE-REVEAL.md`.

Sub-gate: the report names reference counts and owning directories.

### AZ-II.cutover.C.2 Consumer Surface Classification

Mechanism: partition references into host, lower, graph, pipeline,
analysis, tests, generated views, and true tape crate consumers.

Files touched: `docs/tranches/AZ-II/audit/cutover.C-SCOPE-REVEAL.md`.

Sub-gate: each consumer class has a destination wave.

### AZ-II.cutover.C.3 Benchmark Placeholder Archive

Mechanism: record current close-matrix placeholder state without
claiming terminal values.

Files touched: `docs/benchmarks/post-AZ-II.json`,
`docs/benchmarks/post-AY-AZ-II-close-*.txt`.

Sub-gate: benchmark docs are marked partial/historical.

### AZ-II.cutover.C.4 Replan Boundary

Mechanism: update AZ-II progress and cutover docs to route consumer
migration to cutover.D and substrate expansion to later waves.

Files touched: `docs/tranches/AZ-II/PROGRESS.md`,
`docs/tranches/AZ-II/waves/cutover/README.md`.

Sub-gate: no document says tape deletion is complete.

## Hard gate

1. Scope-reveal report exists and names the measured consumer surface.
2. Tape deletion does not land partially.
3. Follow-on destinations are named for every consumer class.
4. Benchmark artifacts are marked partial/historical, not terminal.

## Verification artefacts

- Commit `e91df301`.
- Commit `99024342`.
- `docs/tranches/AZ-II/audit/cutover.C-SCOPE-REVEAL.md`.
- `docs/benchmarks/post-AZ-II.json`.

## Dependencies

- **Depends on**: AZ-II.cutover.B
- **Blocks**: AZ-II.cutover.D, AZ-II.cutover.E

## Archaeology

cutover.C is the first explicit mid-tranche scope reveal. It did not
ship a workaround; it halted deletion and created follow-on wave
ownership for the measured consumer surface.
