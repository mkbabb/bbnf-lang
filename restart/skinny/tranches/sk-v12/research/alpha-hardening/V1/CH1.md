# SK-V12 Alpha Hardening V1 - CH1 Correctness

Date: 2026-05-20.

Scope: CH1 correctness review for Pass Alpha SK-V11 -> SK-V12, V1. Reviewed
`PASS-ALPHA.md` Section 3, SK-V12 `SYNTHESIS.md`, `HANDOFF.md`, all
`research/alpha/*.md`, `skinny/RESULTS.md`, and `skinny/REDRESS.md` through
REDRESS 120.

## Overall Disposition

REVISE.

The alpha packet is directionally correct: the current result surface matches
`skinny/RESULTS.md:5-45` and `skinny/REDRESS.md:3531-3553`; the direct residual
floor table matches REDRESS 119 (`skinny/REDRESS.md:3508-3522`); and alpha-B
keeps competitor comparisons on the correct output plane. The packet should not
be rejected because I found no fabricated row movement, no parse-only SOTA
admission, and no direct-vs-typed plane swap.

Revision is required because CH1 asks whether every claim cites a result row,
REDRESS entry, commit SHA, or measurement file
(`restart/prompts/pass-contracts/PASS-ALPHA.md:35-39`). Several load-bearing
tables and newly introduced thresholds are correct or plausible but not tightly
sourced enough for the CH1 standard.

## Critical Findings

1. REVISE: Alpha-E introduces baseline thresholds without authority.

   `alpha-E-candidate-shortlist.md:67-75`, `alpha-E-candidate-shortlist.md:118-126`,
   and `alpha-E-candidate-shortlist.md:164-171` require generated Track 1 and
   oracle throughput `>= 1 Mbps, finite` plus sample count `>= 100`. The
   measured close and Alpha-F/SYNTHESIS baseline contract require finite
   same-run throughput, strict equality, provenance, telemetry, and gate
   consumption (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:35-48`), while
   REDRESS 111 only records finite placeholder Mbps for the non-admitting lane
   (`skinny/REDRESS.md:3289-3293`). I found no cited source for the numeric
   `1 Mbps` floor or `100` sample floor as a baseline admission threshold.
   Either cite the gate policy/measurement row that owns those numbers or change
   the baseline gates to finite positive throughput plus recorded sample count.

2. REVISE: Guard-floor tables need source and formula citations.

   The guard floors in `alpha-F-contract-draft.md:130-144` and
   `SYNTHESIS.md:111-132` match SK-V11 SPEC Section 0.5, which defines direct
   guard floors as `max(ceil(sonic direct / 1.10), floor(SK-V11-open track Mbps
   * 0.98))` and typed guard floors similarly
   (`restart/skinny/tranches/sk-v11/SPEC.md:149-172`). The alpha packet does
   not cite that formula near the copied floors, so the thresholds read as
   asserted constants. Add the SPEC citation or restate the formula next to the
   table.

3. REVISE: Alpha-A and the top-level SK-V12 summary docs carry many numeric
   claims with only file-level authority.

   The values in alpha-A's parse, direct, and typed tables are consistent with
   `skinny/RESULTS.md`, but `alpha-A-results-extraction.md:65-83`,
   `alpha-A-results-extraction.md:92-110`, and
   `alpha-A-results-extraction.md:118-126` do not cite result row lines or
   REDRESS lines per row/table. `SYNTHESIS.md:82-109` and
   `HANDOFF.md:31-42` similarly summarize the result surface and residual
   fixpoint without local row citations. Add compact citations to
   `skinny/RESULTS.md:5-45`, `skinny/RESULTS.md:143-146`, and
   `skinny/REDRESS.md:3508-3553` beside those summaries.

## Dispositions By Artifact

| Artifact | Disposition | CH1 rationale |
|---|---|---|
| `alpha-A-results-extraction.md` | REVISE | Numeric extraction is consistent with `RESULTS.md`, but row/table claims need local citations rather than only a source list. |
| `alpha-B-competitor-deltas.md` | ACCEPT | Direct rows use same-run sonic/serde digest-plane comparators; typed rows use typed direct comparators; parse-only sidecars are diagnostic only. Minor one-decimal drift from recomputing rounded Mbps does not change any disposition. |
| `alpha-C-redress-digest.md` | ACCEPT | REDRESS 111-120 classifications, fixpoint rows, and hard pre-blocks match REDRESS 119/120 and include commits or measurement-file references. |
| `alpha-D-validated-invalidated.md` | ACCEPT | Validated, invalidated, demoted, and open ledgers preserve the measured fixpoint and commit anchors; no row movement is invented. |
| `alpha-E-candidate-shortlist.md` | REVISE | Candidate order and same-wave consumer rules are correct, but E1-E3 numeric baseline thresholds need cited authority or must be reduced to finite measured throughput. |
| `alpha-F-contract-draft.md` | REVISE | Contract decisions are correct, measurable, and plane-safe, but guard thresholds and copied result surfaces need direct citations/formula provenance. |
| `SYNTHESIS.md` | REVISE | Goalset is correct and measurable; add citations for result-surface tables, direct residual table, and guard-floor formula. |
| `HANDOFF.md` | REVISE | Handoff is factually aligned with REDRESS 119/120, but its current-state and goalset summaries are uncited. |

## Strict-Plane Check

ACCEPT. Alpha-B correctly keeps:

- `parse_only` as diagnostic borrowed-view-vs-DOM evidence, not SOTA admission
  (`alpha-B-competitor-deltas.md:30-46`);
- `direct_to_struct` on the digest plane against same-run sonic-rs strict direct
  and serde_json direct only (`alpha-B-competitor-deltas.md:35-38`,
  `alpha-B-competitor-deltas.md:73-87`);
- `real_typed_struct` on the typed direct plane against same-run sonic-rs strict
  typed and serde_json typed only (`alpha-B-competitor-deltas.md:39-42`,
  `alpha-B-competitor-deltas.md:116-124`);
- absent C++ direct/typed sidecars as absence evidence, not negative or positive
  SOTA evidence (`alpha-B-competitor-deltas.md:48-58`).

## Gate Measurability Check

REVISE only for threshold provenance. The major gates are otherwise measurable:

- non-JSON baseline requires generated Track 1, independent oracle/Track 2,
  strict equality, finite same-run throughput, provenance, telemetry, and gate
  consumption (`alpha-F-contract-draft.md:71-82`);
- intervention threshold is explicit:
  `ceil(baseline_mbps * 1.01)` unless S-P3 sets a stricter threshold
  (`alpha-F-contract-draft.md:78-82`);
- JSON residual reopen requires fresh evidence beyond REDRESS 114-119 and
  same-run direct-plane gate consumption (`alpha-F-contract-draft.md:83-87`);
- telemetry fail-closed fields are named and validator-consumable
  (`alpha-F-contract-draft.md:154-173`).

## Required Revisions Before CH1 Accept

1. Add citations beside alpha-A's extracted result tables and no-row-movement
   claims.
2. Cite SK-V11 SPEC Section 0.5 or restate its formula beside every SK-V12
   guard-floor table.
3. Source or remove Alpha-E's `>= 1 Mbps` and `sample count >= 100` baseline
   admission thresholds.
4. Add compact REDRESS/RESULTS citations to `SYNTHESIS.md` and `HANDOFF.md`
   current-state summaries.
