# SK-V12 W1b-2b Plan Audit

Date: 2026-05-20.
Scope: audit of Section 7.2 plan surface and W1b-2b research artifacts A1-A6.

## Required Corrections Before Implementation

1. REDRESS slot drift: A1 still requires `redress_entry == REDRESS-124`, but A5
   correctly identifies W1b-2b as REDRESS item 125 because W1b-2a consumed 124.
   Update the plan/schema/tests to require REDRESS 125 only.

2. Gate label drift: A2 recommends
   `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA PASS <path>`, while A6 mentions
   `G-W1b-2-CSS-L4-LIGHTNINGCSS ...`. Use the Section 7.2 gate name exactly:
   `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA`.

3. Artifact path drift: A5 names
   `restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json`.
   Confirm whether W1b-2b should really write outside `research/w1b-2b/`. If
   that legacy path is intentional, make it explicit in the implementation plan;
   otherwise all report references should move to the W1b-2b artifact area.

## Hidden Coupling Risks

- The new CSS SOTA schema must not reuse or loosen `sk-v12-nonjson-generated-v1`.
  Add a separate validator and keep W1b-1/W1a report behavior unchanged.
- Lightningcss may transitively use cssparser; that is not coupling by itself.
  The failure condition is direct cssparser API use in the lightningcss
  comparator path or any reuse of generated Track 1 code by the oracle.
- Do not let report-provided Mbps, threshold, margin, sample count, or guard
  state become authority. The gate must recompute/verify them from Criterion
  and retained artifact paths where Section 7.2 requires consumed telemetry.

## No-Write And RESULTS Movement

- Companion report mode must reject `--update-results`, `--write-results`,
  `--include-volatile-probes`, mixed companion reports, and probe invocations.
- `PASS-MEASURED-BASELINE` must not move `skinny/RESULTS.md`.
- `skinny/RESULTS.md` moves only for a real CSS `PASS-ADMIT-CANDIDATE` row or
  for an accepted measured JSON guard demotion. Stale-results guidance must not
  tell operators to update RESULTS for CSS measured-baseline evidence.

## Criterion Artifact Freshness

- Consume only `new/benchmark.json`, `new/estimates.json`, and
  `new/sample.json` from the three W1b-2a lanes. Never fall back to `base/`,
  `change/`, report JSON values, or hand-entered Mbps.
- Fail closed unless Track 1, cssparser oracle, lightningcss, equality facts,
  and benchmark artifact paths bind to the same W1b-2b `run_id` and the CSS L4
  declaration-values row.
- Require each consumed lane to expose `throughput.Bytes == 187`,
  `mean.point_estimate > 0`, finite derived Mbps, and at least 30 samples.

## Threshold Math

- Compute `threshold_mbps = lightningcss_mbps + 1`.
- Compute `admission_margin_mbps = track1_mbps - threshold_mbps`.
- Admit only when `track1_mbps > lightningcss_mbps + 1` and
  `admission_margin_mbps > 0`. Equality at the threshold is
  `PASS-MEASURED-BASELINE`, not admit.
- Reject serialized reports whose threshold or margin disagrees with derived
  values beyond a documented float tolerance.

## JSON Guard Root

- A CSS-only Criterion root is invalid for JSON guards. The no-write guard path
  must use an accepted JSON Criterion root or a fresh populated JSON guard
  capture containing the JSON fixture and SIMD rows consumed by `gate-json`.
- Accept `json_guard_state=not_refreshed:no_behavior_drift` only when the plan
  proves no JSON-producing behavior moved and RESULTS is unchanged. Otherwise
  require a refreshed guards-pass state from a populated JSON root.
- Validate the CSS report first, then let `--check-results` or
  `--with-cost-facts` continue into the existing JSON guard path so guard
  failure can demote/fail the overall command.

## Lock14 And Lock16

- Lock14 is process-level gate context, not just report text. The companion
  report may claim Lock14 pass only after `lock14_baseline::validate` has run in
  the same gate process.
- Lock16 must be explicit. For this scalar CSS row, `n/a:no_simd_or_asm_claim`
  is acceptable only if no SIMD/ASM admission is being claimed and scalar or
  parity coverage is recorded for the measured equality path.

## Actionable Test Focus

- Add targeted report tests for REDRESS 125, derived threshold/margin,
  threshold equality as measured baseline, stale Criterion identity, CSS-only
  JSON guard roots, Lock14/Lock16 statuses, and unknown producer fields.
- Add CLI tests for the full no-write matrix and mixed companion report
  rejection.
- Add an integration no-write check that proves `skinny/RESULTS.md` is
  byte-identical before/after a valid measured-baseline companion invocation.
