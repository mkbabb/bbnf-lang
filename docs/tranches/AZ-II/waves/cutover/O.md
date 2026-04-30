# AZ-II.cutover.O - Terminal Hardening Index
**Opens after**: AZ-II.cutover.N halt snapshot
**Agents**: up to 10 parallel
**Hard gate**: O0-O4 landed inside AZ-II; O5-O7 are carried by AZ-III continuation waves with refreshed evidence.
**Status**: superseded by AZ-III

## Continuation Close

AZ-II.cutover.O is closed as a historical index. O0 through O4 landed.
O5 did not close green, O6 did not run, and O7 did not run. AZ-III
continues this index as W1 (O5 reclose), W2 (semantic parity and BBNF
bootstrap proof), W4 (benchmark/profile truth), and W5 (terminal close).

## Scope

1. Repair proof-command surfaces before collecting close evidence.
2. Make StructDirect speculative builder mutation transactional.
3. Flip EBNF through StructDirect with structural branch projection.
4. Capture the post-O2 failure baseline and dispatch triumvirate
   redress cohorts before implementation continues.
5. Purge generated tape-view / `ValueRoot` residue.
6. Delete production `Parsed<R>` and `TapeDirect`.
7. Delete `crates/tape` after relocating only genuinely non-tape
   primitives.
8. Refresh semantic parity and the 17-entry close matrix.
9. Convert AZ-II FINAL from partial to terminal close.

## File bounds

| File | Access |
|---|---|
| `docs/tranches/AZ-II/waves/cutover/O0.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/O1.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/O2.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/O3a.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/O3.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/O4.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/O5.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/O6.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/O7.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/README.md` | modify |
| `docs/tranches/AZ-II/AZ-II.md` | modify |
| `docs/tranches/AZ-II/FINAL.md` | modify |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |

**Do NOT touch**: implementation files from this index. Child specs own
source file bounds. Deployment invariant: each O child wave may use up
to 10 parallel fully-contained sibling worktrees; the orchestrator owns
master, regen windows, and cross-child status reconciliation.

## Phase sub-items

### AZ-II.cutover.O.1 O0 Tooling Preflight

Mechanism: dispatch per `O0.md`.

Files touched: `docs/tranches/AZ-II/waves/cutover/O0.md`.

Sub-gate: proof-command surfaces are repaired or de-canonicalized.

### AZ-II.cutover.O.2 O1 Builder Transactions

Mechanism: dispatch per `O1.md`.

Files touched: `docs/tranches/AZ-II/waves/cutover/O1.md`.

Sub-gate: speculative StructDirect mutation is transactional.

### AZ-II.cutover.O.3 O2 EBNF Direct Projection

Mechanism: dispatch per `O2.md`.

Files touched: `docs/tranches/AZ-II/waves/cutover/O2.md`.

Sub-gate: `EbnfParser::parse` returns `EbnfDocument`.

### AZ-II.cutover.O.4 O3a Failure Baseline and Triumvirate Redress

Mechanism: dispatch per `O3a.md`.

Files touched: `docs/tranches/AZ-II/waves/cutover/O3a.md`.

Sub-gate: all current failures are assigned to research/plan/redress
cohorts with wave owners.

### AZ-II.cutover.O.5 O3 Generated View Purge

Mechanism: dispatch per `O3.md`.

Files touched: `docs/tranches/AZ-II/waves/cutover/O3.md`.

Sub-gate: StructDirect generated output carries no tape-backed view
surface.

### AZ-II.cutover.O.6 O4 Parsed/TapeDirect Deletion

Mechanism: dispatch per `O4.md`.

Files touched: `docs/tranches/AZ-II/waves/cutover/O4.md`.

Sub-gate: production `Parsed<R>` and `TapeDirect` are absent.

### AZ-II.cutover.O.7 O5 Tape Crate Deletion

Mechanism: dispatch per `O5.md`.

Files touched: `docs/tranches/AZ-II/waves/cutover/O5.md`.

Sub-gate: `crates/tape` is absent and no production tape symbols remain.

### AZ-II.cutover.O.8 O6 Semantic and Performance Close

Mechanism: dispatch per `O6.md`.

Files touched: `docs/tranches/AZ-II/waves/cutover/O6.md`.

Sub-gate: semantic parity and close-matrix artifacts are refreshed.

### AZ-II.cutover.O.9 O7 Final Conversion

Mechanism: dispatch per `O7.md`.

Files touched: `docs/tranches/AZ-II/waves/cutover/O7.md`.

Sub-gate: AZ-II FINAL is terminal and cites O0-O6 evidence.

## Hard gate

1. O0, O1, O2, O3, and O4 are complete.
2. O3a is routed `complete_with_misses`; child dispositions are carried
   by O5/O6/O7 and must not be bypassed.
3. O5 is next active and blocks O6 until its tape deletion hard gate is
   green.
4. O6 and O7 remain planned; O7 must not convert FINAL or unblock BA/BB
   until O6 closes.
5. AZ-III is not opened for tape deletion, stale
   benches, or parity gaps.
6. Every child wave has a full wave spec before dispatch.

## Verification artefacts

- `docs/tranches/AZ-II/waves/cutover/O0.md`.
- `docs/tranches/AZ-II/waves/cutover/O1.md`.
- `docs/tranches/AZ-II/waves/cutover/O2.md`.
- `docs/tranches/AZ-II/waves/cutover/O3a.md`.
- `docs/tranches/AZ-II/waves/cutover/O3.md`.
- `docs/tranches/AZ-II/waves/cutover/O4.md`.
- `docs/tranches/AZ-II/waves/cutover/O5.md`.
- `docs/tranches/AZ-II/waves/cutover/O6.md`.
- `docs/tranches/AZ-II/waves/cutover/O7.md`.

## Dependencies

- **Depends on**: AZ-II.cutover.N
- **Blocks**: AZ-II terminal close, BA open gate, BB close gate

## Archaeology

cutover.O exists because cutover.N revealed that terminal deletion and
benchmark work could not safely continue as an underspecified tail. O
turns that reveal into a written sequence before implementation resumes.
