# AZ-II.cutover.O7 — Terminal Close Conversion
**Opens after**: AZ-II.cutover.O6 close
**Agents**: up to 10 parallel
**Hard gate**: AZ-II FINAL converts from PARTIAL CLOSE to terminal close with every O0-O6 gate cited by artifact and commit.
**Status**: planned

## Scope

1. Convert AZ-II `FINAL.md` from partial close to terminal close.
2. Update every appurtenant planning document so BA/BB open only on the verified post-O6 state.
3. Archive close scans, bench matrix, parity status, and residual named gaps.
4. Retire or mark historical documents that still describe EBNF or tape as live production substrate.
5. Cite O3a baseline artifacts and all triumvirate outcomes in the
   terminal close so the post-O2 failure surface is not hidden by the
   later code waves.
6. Run close-document consistency scans before declaring the tranche closed.

## File bounds

| File | Access |
|---|---|
| `docs/tranches/AZ-II/FINAL.md` | modify |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |
| `docs/tranches/AZ-II/AZ-II.md` | modify |
| `docs/tranches/AZ-II/PROGRESS-SNAPSHOT-2026-04-29.md` | modify |
| `docs/tranches/AZ-II/waves/cutover.md` | modify |
| `docs/tranches/AZ-II/waves/cutover.O*.md` | modify |
| `docs/tranches/AZ-II/audit/AZ-II-HARDENING-AUDIT-2026-04-29.md` | modify |
| `docs/GESTALT.md` | modify |
| `docs/codegen-paths.md` | modify |
| `docs/tranches/REMAINING-TRAJECTORY.md` | modify |
| `docs/benchmarks/post-AZ-II.json` | modify |
| `docs/benchmarks/AZ-II/cutover/O7-close-doc-scan.txt` | create |
| `docs/tranches/meta-audit/ARCHIVE.md` | modify |
| `docs/tranches/next-tranche-research/ARCHIVE.md` | modify |

**Do NOT touch**: source code, generated parser files, benchmark harnesses, parity tests, sibling repos. O7 is documentation/close conversion after code and measurement gates are already green.
Deployment invariant: every sub-agent runs in a sibling
fully-contained worktree seeded with `scripts/seed-worktree.sh`, with
explicit allow/forbidden lists; the orchestrator owns final status
reconciliation across `FINAL.md`, `PROGRESS.md`, parent plan, and wave
specs.

## Phase sub-items

### AZ-II.cutover.O7.1 FINAL Conversion

Mechanism: rewrite `FINAL.md` as terminal close: phase recap, commit hashes, hard-gate table, verification artifacts, bench matrix, and residual risks.

Files touched: `docs/tranches/AZ-II/FINAL.md`.

Sub-gate: every O0-O6 gate has an artifact path and commit hash.

### AZ-II.cutover.O7.2 Progress Closure

Mechanism: update `PROGRESS.md` with O7 close entry and final wave statuses.

Files touched: `docs/tranches/AZ-II/PROGRESS.md`.

Sub-gate: the top status says terminal close, not partial close.

### AZ-II.cutover.O7.3 Parent Plan Closure

Mechanism: update `AZ-II.md` so the parent plan describes the achieved terminal state and handoff contract.

Files touched: `docs/tranches/AZ-II/AZ-II.md`.

Sub-gate: handoff to BA/BB references the post-O6 artifact set.

### AZ-II.cutover.O7.4 Snapshot and Cutover Index

Mechanism: convert the snapshot and cutover index into historical read-only records that point to the final close docs.

Files touched: `docs/tranches/AZ-II/PROGRESS-SNAPSHOT-2026-04-29.md`, `docs/tranches/AZ-II/waves/cutover.md`, `docs/tranches/AZ-II/waves/cutover.O*.md`.

Sub-gate: old "active gate" wording is absent.

### AZ-II.cutover.O7.5 Hardening Audit Closure

Mechanism: mark the hardening audit resolved by O0-O6, or carry only named residuals into BA/BB with exact destination.

Files touched: `docs/tranches/AZ-II/audit/AZ-II-HARDENING-AUDIT-2026-04-29.md`.

Sub-gate: no unresolved P0 item lacks a destination.

### AZ-II.cutover.O7.6 Gestalt and Codegen Canon

Mechanism: update `GESTALT.md` and `codegen-paths.md` to state the terminal direct-to-struct, tape-free production state.

Files touched: `docs/GESTALT.md`, `docs/codegen-paths.md`.

Sub-gate: neither file says EBNF, `Parsed<R>`, `TapeDirect`, or `crates/tape` are live production blockers.

### AZ-II.cutover.O7.7 Remaining Trajectory Handoff

Mechanism: update remaining trajectory so BA and BB are unblocked only by the actual close commit and O6 artifact paths.

Files touched: `docs/tranches/REMAINING-TRAJECTORY.md`.

Sub-gate: BA/BB opening contract cites the final AZ-II close commit and benchmark JSON.

### AZ-II.cutover.O7.8 Archive Disposition

Mechanism: mark meta-audit and next-tranche-research directories as historical provenance, with inbound references either rewritten or preserved as archaeology links.

Files touched: `docs/tranches/meta-audit/ARCHIVE.md`, `docs/tranches/next-tranche-research/ARCHIVE.md`.

Sub-gate: archive docs explain whether directories can be deleted, moved, or kept as provenance.

### AZ-II.cutover.O7.9 Close Document Scan

Mechanism: run doc scans for stale blocker language and archive results.

Files touched: `docs/benchmarks/AZ-II/cutover/O7-close-doc-scan.txt`.

Sub-gate: stale wording is either absent or explicitly historical.

### AZ-II.cutover.O7.10 Final Consistency Pass

Mechanism: run markdown link/path sanity checks and ensure all wave spec status lines agree with `PROGRESS.md`.

Files touched: `docs/tranches/AZ-II/FINAL.md`, `docs/tranches/AZ-II/PROGRESS.md`, `docs/tranches/AZ-II/waves/cutover.O*.md`.

Sub-gate: one O7 commit closes the documentation boundary.

### AZ-II.cutover.O7.11 O3a Baseline Conversion

Mechanism: convert O3a from active failure-baseline routing into
historical close evidence. FINAL must cite the test failure artifact,
the failed JSON bench artifact, every J1/C1/S1/P1/A1 triad output, and
the wave that closed each cohort.

Files touched: `docs/tranches/AZ-II/FINAL.md`,
`docs/tranches/AZ-II/PROGRESS.md`,
`docs/tranches/AZ-II/waves/cutover.O3a.md`,
`docs/tranches/AZ-II/audit/O3a-*.md`.

Sub-gate: no O3a cohort remains `in_progress` when AZ-II is declared
terminally closed.

## Hard gate

1. `docs/tranches/AZ-II/FINAL.md` is terminal close and cites O0-O6 commits plus artifacts.
2. `docs/benchmarks/post-AZ-II.json` exists and has no placeholder entries.
3. `docs/benchmarks/AZ-II/cutover/O7-close-doc-scan.txt` records zero stale active-blocker claims outside explicitly historical sections.
4. `PROGRESS.md`, `AZ-II.md`, `cutover.md`, and `cutover.O*.md` statuses agree.
5. BA/BB handoff in `docs/tranches/REMAINING-TRAJECTORY.md` points at the final close commit and post-AZ-II benchmark artifact.
6. O3a artifacts and cohort triad outputs are cited in FINAL and no
   cohort remains active.

## Verification artefacts

- `docs/benchmarks/AZ-II/cutover/O7-close-doc-scan.txt`
- `docs/benchmarks/post-AZ-II.json`
- `/tmp/az-ii-o7-doc-stale-scan.txt`
- `/tmp/az-ii-o7-link-path-scan.txt`
- `docs/tranches/AZ-II/audit/O3a-*.md`
- O7 close commit hash recorded in `docs/tranches/AZ-II/PROGRESS.md`.

## Dependencies

- **Depends on**: AZ-II.cutover.O6, O3a cohort close
- **Blocks**: BA open gate, BB close gate, AZ-II archive handoff

## Archaeology

AZ-II has carried a PARTIAL CLOSE `FINAL.md` since cutover.H. O7 is the conversion step after the implementation, deletion, and measurement gates close; it may not hide any O0-O6 miss as "future work" without a named successor and rationale.
