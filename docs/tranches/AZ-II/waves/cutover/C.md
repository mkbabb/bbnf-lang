# AZ-II.cutover.C - Tape Deletion Scope Reveal
**Opens after**: AZ-II.cutover.B close
**Agents**: up to 10 parallel
**Hard gate**: tape deletion is blocked by measured BBNF consumer surface and rerouted before any partial deletion lands.
**Status**: complete_with_misses

Canonical location: `docs/tranches/AZ-II/waves/cutover/C.md`.
The pre-reorg root file name `waves/cutover.C.md` is historical; active
dispatch and archaeology read this subdirectory file.

## 2026-04-29 Hardening Addendum

cutover.C is a diagnostic boundary, not an implementation wave. Its
job is to prove that deletion is unsafe, preserve the measured surface,
and force written successor ownership before any source redress
continues. Replaying or auditing cutover.C must not re-open tape
deletion directly.

Current successor ownership:

| Discovered surface | Historical finding | Current owner |
|---|---|---|
| BBNF consumer migration | `BbnfBootstrapNodeView` consumers across host/lower/graph/pipeline/analysis/tests exceeded the original cap | cutover.D/H/I/K/L/M landed the migration path; remaining generated-view residue is O3/P1 |
| Generated tape views | StructDirect output still emitted node-view, `TapeCursor`, `ValueRoot`, and Root/view residue | O3 and O3a-P1 |
| `Parsed<R>` return surface | `Parsed` still carried tape and remained a public/runtime test surface | O4 |
| `TapeDirect` fallback | unknown grammar selection could still downgrade to tape | O4 |
| `crates/tape` crate | workspace crate still had runtime, generated, simd-scan, and prototype consumers | O5 |
| Benchmark placeholders | `post-AZ-II.json` was historical placeholder truth, not close evidence | O6 |
| FINAL wording | cutover.H-era close artifact was an interim manifest, not terminal close | O7 |

Non-negotiables inherited by all successor waves:

1. Do not preserve `BbnfBootstrapNodeView`, `Parsed<R>`, or tape
   runtime names as compatibility shims.
2. Do not delete `crates/tape` until O3 and O4 have removed generated
   view residue plus `Parsed<R>` / `TapeDirect`.
3. Do not promote cutover.C benchmark artifacts as performance truth.
   O6 owns the first post-O5 semantic/performance baseline.
4. Do not open BA/BB on cutover.C evidence. BA/BB open only after O7
   cites O0-O6 close artifacts.

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
7. Record the successor wave owner for every discovered surface, so
   no successor agent treats the report as permission for an unplanned
   deletion attempt.

## File bounds

| File | Access |
|---|---|
| `docs/tranches/AZ-II/audit/cutover.C-SCOPE-REVEAL.md` | create |
| `docs/benchmarks/post-AZ-II.json` | create |
| `docs/benchmarks/archive/post-AY-AZ-II-close-*.txt` | create |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/README.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/D.md` | create or modify if replaying C before D exists |
| `docs/tranches/AZ-II/waves/cutover/O3.md` | read-only current owner |
| `docs/tranches/AZ-II/waves/cutover/O4.md` | read-only current owner |
| `docs/tranches/AZ-II/waves/cutover/O5.md` | read-only current owner |
| `docs/tranches/AZ-II/waves/cutover/O6.md` | read-only current owner |
| `docs/tranches/AZ-II/waves/cutover/O7.md` | read-only current owner |

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

### AZ-II.cutover.C.2a Successor Owner Crosswalk

Mechanism: after classifying the consumer surface, write an explicit
crosswalk from each class to the next wave that owns it. In the current
history this crosswalk must point to D/H/I/K/L/M for already-landed
consumer migration and to O3/O4/O5/O6/O7 for terminal blockers.

Files touched: `docs/tranches/AZ-II/audit/cutover.C-SCOPE-REVEAL.md`,
`docs/tranches/AZ-II/waves/cutover/README.md`, successor wave specs if
the replay happens before those specs exist.

Sub-gate: every row in the discovered-surface table above has a
successor owner; no row uses unnamed or open-ended ownership.

### AZ-II.cutover.C.3 Benchmark Placeholder Archive

Mechanism: record current close-matrix placeholder state without
claiming terminal values.

Files touched: `docs/benchmarks/post-AZ-II.json`,
`docs/benchmarks/archive/post-AY-AZ-II-close-*.txt`.

Sub-gate: benchmark docs are marked partial/historical.

### AZ-II.cutover.C.4 Replan Boundary

Mechanism: update AZ-II progress and cutover docs to route consumer
migration to cutover.D and substrate expansion to successor waves.

Files touched: `docs/tranches/AZ-II/PROGRESS.md`,
`docs/tranches/AZ-II/waves/cutover/README.md`.

Sub-gate: no document says tape deletion is complete.

## Hard gate

1. Scope-reveal report exists and names the measured consumer surface.
2. Tape deletion does not land partially.
3. Follow-on destinations are named for every consumer class.
4. Benchmark artifacts are marked partial/historical, not terminal.
5. Successor owners are named for generated views, `Parsed<R>`,
   `TapeDirect`, `crates/tape`, benchmark truth, and final conversion.
6. `rg 'T[B]D|UNASSIGNE[D]|owner ga[p]' docs/tranches/AZ-II/waves/cutover/C.md docs/tranches/AZ-II/audit/cutover.C-SCOPE-REVEAL.md`
   returns zero active routing holes.

## Verification artefacts

- Commit `e91df301`.
- Commit `99024342`.
- `docs/tranches/AZ-II/audit/cutover.C-SCOPE-REVEAL.md`.
- `docs/benchmarks/post-AZ-II.json`.
- Successor owner crosswalk in this file's hardening addendum.

## Dependencies

- **Depends on**: AZ-II.cutover.B
- **Blocks**: AZ-II.cutover.D, AZ-II.cutover.E

## Archaeology

cutover.C is the first explicit mid-tranche scope reveal. It did not
ship a workaround; it halted deletion and created follow-on wave
ownership for the measured consumer surface.

The important lesson is not that tape deletion was too large; it is
that source deletion without a written successor map would have created
an orthogonal substrate. cutover.C is therefore preserved as the
template for halting, measuring, writing the re-plan, and only then
dispatching implementation.
