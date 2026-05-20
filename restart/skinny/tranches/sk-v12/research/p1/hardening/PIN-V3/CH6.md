# SK-V12 S-P1 Hardening PIN-V3 CH6

Lens: anti-paper-close integrity after the PIN-V2 fold.
Date: 2026-05-20.
Reviewer scope: current S-P1 packet at repo head `9559a2c4`, capture root
`/tmp/skv12-pin-p1`, PIN-V2 consolidation, USER PIN, HANDOFF, and assigned
output file only.

## Verdict

ACCEPT.

Score: 95%.

The final S-P1 fold is clean enough to advance to S-P2. The live profile
claims are backed by TSV/status/table/trace artifacts; replay ledger shape is
mechanically clean; `rc=54` acceptance is backed by stdout evidence; stale
partial-capture wording no longer changes the final state; CSS L4 is explicitly
absent and not admitted; and the handoff keeps CSS bring-up in S-P2/S-P3 rather
than paper-closing it with prose.

## Blocking Findings

None.

## Evidence Checked

Authority and provenance:

- The manifest pins the capture source `cf7848b2`, initial S-P1 fold
  `b1043383`, PIN-V2 review base `d4ef80b2`, capture root
  `/tmp/skv12-pin-p1`, build root `/tmp/skv12-pin-profile-target-cf7848b2`,
  binaries, and completion stamps (`skv12-p1-capture-manifest.md:10-29`).
- It marks `/tmp/skv12-p1`, `/tmp/skv12-profile-target-50bd1648`, and the
  pre-pin replay TSV as historical only (`skv12-p1-capture-manifest.md:51-53`).
- It keeps result authority separate from profile evidence and moves no rows
  (`skv12-p1-capture-manifest.md:28-29`).

Replay and artifact checks:

- `skv12-p1-pin-replay.tsv`: 458 data rows, exactly 10 fields per row, zero
  noncanonical modes, zero missing core artifact/status fields, and every
  listed artifact plus status artifact exists.
- Status files match the manifest: PMU 82/82 `PASS rc=0`, samply 82/82
  `PASS`, xctrace 212/212 `PASS`, and XML export status 82/82 `SKIP rc=0`
  with every exported XML file present and nonzero.
- Hot-leaf tables are backed and clean: summary 82 rows split parse 34 /
  direct 34 / typed 14, details 410 rows split parse 170 / direct 170 / typed
  70, with zero bad source anchors in the checked source fields.

`rc=54` acceptance:

- The manifest now states that `rc=54` is accepted when the captured xctrace log
  stream records an accepted stop condition and `Output file saved as`, and
  that the current pin root has those strings in the stdout path recorded by
  `capture_status.tsv` (`skv12-p1-capture-manifest.md:109-113`).
- Direct check: 185 `rc=54` rows; 185 stdout logs contain both saved-output and
  stop-condition evidence; stderr contains 0 saved-output / 0 stop-condition
  hits. All 212 trace artifacts and stdout/stderr log paths exist.

Claim backing:

- P1-A parse coverage is backed by `/tmp/skv12-pin-p1/pmu/parse_pmu_rows.tsv`,
  `/tmp/skv12-pin-p1/{samply,xctrace}/capture_status.tsv`, and the derived
  hot-leaf tables (`p1a-samply-mode-1.md:16-21`, `:27-59`, `:87-143`).
- P1-B product coverage is backed by `product_pmu_rows.tsv`, samply/xctrace
  status, product-v2 traces/exports, and derived tables while preserving Track
  1 / Track 2 separation (`p1b-samply-mode-2.md:24-50`, `:137-180`,
  `:187-213`).
- P1-D derives PMU aggregates from the PMU TSVs only, refuses branch/L1/LLC
  inference, and records companion xctrace/samply as non-PMU evidence
  (`p1d-pmu-cycles.md:17-36`, `:54-80`, `:87-116`, `:213-231`).
- P1-E hot-leaf claims are backed by the summary/detail TSVs and readable
  tables; it records the pin capture complete and source anchors clean
  (`p1e-hot-leaf-attribution.md:19-50`, `:80-116`, `:159-178`).
- P1-F keeps row movement tied to `skinny/RESULTS.md`/REDRESS rather than the
  profile fold and records 0 admitted CSS L4 rows (`p1f-results-delta.md:15-36`,
  `:73-93`, `:145-210`).

CSS L4 and downstream bring-up:

- CSS L4 is explicitly absent from profile/admission: no generated CSS runtime,
  no generated `css_l4`/`css_l4_declaration_values` module, no lightningcss
  comparator row, and no strict equality oracle (`p1a-samply-mode-1.md:145-158`,
  `p1c-samply-mode-3.md:96-110`, `p1f-results-delta.md:177-182`).
- JSON parse/direct/typed rows are kept diagnostic or guard-only and do not
  populate the `lightningcss_mbps + 1` bar (`p1b-samply-mode-2.md:49-50`,
  `:164-168`; `p1f-results-delta.md:89-93`, `:205-210`).
- HANDOFF requires S-P2/S-P3 re-derivation under the user pin, exact CSS row
  selection, generated Track 1 path, oracle/comparator/equality/benchmark/gate
  commands, `GrammarConfig`, generated-size/O(N) checks, and gate-consumed
  telemetry; missing lightningcss evidence and stale run ids fail closed
  (`HANDOFF.md:103-128`, `:142-155`, `:157-177`).

## Nonblocking Notes

1. `skv12-p1-pin-replay.tsv` has a clean 10-column schema and canonical modes,
   but two PMU parse ledger rows use `update-center` in the corpus column while
   the status/PMU TSVs use canonical `update_center`. The commands, logs, TSV
   rows, and artifacts are present, so this does not undermine a current claim.
   A future ledger normalization pass should canonicalize those two corpus
   cells.
2. `p1f-results-delta.md:68-71` still records that a parent-owned PMU replay was
   running during that lane's method section. Its header and the manifest record
   final PMU/samply/xctrace completion, so this is historical method context,
   not a live partial-capture contradiction.
3. `p1e-hot-leaf-attribution.md:143` says "A JSON-only fold is not
   pin-converged." In context, the file records CSS L4 as a first-class absence
   and does not admit it. Clearer future wording would be: "A fold that omits
   the CSS L4 absence row is not pin-converged."

## Exact Fold Edits If REVISE

N/A. Verdict is ACCEPT.
