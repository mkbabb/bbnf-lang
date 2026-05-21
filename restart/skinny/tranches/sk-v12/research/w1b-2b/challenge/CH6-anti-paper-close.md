# SK-V12 W1b-2b CH6 Challenge - Anti-Paper-Close

Date: 2026-05-20.
Wave: W1b-2b - CSS L4 Lightningcss SOTA Report + Admission Gate.
Lens: CH6 anti-paper-close.
Owned artifact: `restart/skinny/tranches/sk-v12/research/w1b-2b/challenge/CH6-anti-paper-close.md`.

## Authorities Read

- `restart/skinny/tranches/sk-v12/SPEC.md` Section 7.2.
- `restart/skinny/tranches/sk-v12/SPEC.md` Section 0.1 close rules.
- `restart/skinny/tranches/sk-v12/SPEC.md` Section 10 W5 close rules.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/PLAN.md`.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/A3-criterion-consumption.md`.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/A5-outcome-routing.md`.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/A6-test-plan.md`.

## CH6 Question

Can the W1b-2b plan close SK-V12, move `skinny/RESULTS.md`, or claim CSS ADMIT
on prose, report presence, stale Criterion values, future W3/W4/W5 work, or a
baseline that is merely measurable?

Answer: no, if executed as written. The plan is gate/report work for
`G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA`, not final campaign close. It may record a
CSS `PASS-ADMIT-CANDIDATE` only when the gate consumes strict equality,
independent oracle evidence, same-plane lightningcss telemetry, threshold math,
provenance, generated-size telemetry, JSON guard state, and REDRESS identity.
Otherwise it records `PASS-MEASURED-BASELINE` or `BLOCKED/FAIL` evidence.

## Findings

1. Paper ADMIT is blocked. The plan requires the gate to recompute Track 1,
   cssparser oracle, and lightningcss Mbps from `new/` Criterion
   `benchmark.json`, `estimates.json`, and `sample.json` files. Hand-entered
   throughput, stale `base/` data, `change/` summaries, malformed estimates,
   non-finite means, low sample counts, and stale run-id artifact paths all fail
   closed.

2. Baseline-as-ADMIT is blocked. The only ADMIT-candidate predicate is
   `track1_mbps > lightningcss_mbps + 1` with a positive derived margin.
   Equality at the threshold or any lower result routes to
   `PASS-MEASURED-BASELINE`, which is useful REDRESS/FIXPOINT evidence but not
   CSS SOTA admission.

3. Report-only close is blocked. The report schema is not self-authenticating:
   validation must derive `threshold_mbps = lightningcss_mbps + 1` and
   `admission_margin_mbps = track1_mbps - threshold_mbps`, require exact W1b-2b
   row identity, require strict three-way equality, require independent
   cssparser oracle status, and consume generated source/runtime provenance,
   grammar/input checksums, host/build context, sample count, generated LOC, and
   module size telemetry.

4. `RESULTS.md` paper movement is blocked. A CSS row may move
   `skinny/RESULTS.md` only for `PASS-ADMIT-CANDIDATE` with the consumed
   telemetry and equality/oracle evidence above. `PASS-MEASURED-BASELINE` and
   `BLOCKED/FAIL` do not move the CSS row. Any unrelated `RESULTS.md` movement
   is limited to a measured JSON guard demotion accepted by the existing JSON
   gate, not by W1b-2b prose.

5. Future-work close is blocked. W1b-2b does not close SK-V12. The plan states
   W3/W4 may proceed after a measured CSS row and W5 owns final ADMIT/FIXPOINT
   reconciliation. This matches SPEC Section 10: W5 must determine ADMIT or
   FIXPOINT after the required wave dispositions and cannot be bypassed by a
   W1b-2b report.

6. Empty-guard and write/probe bypasses are blocked. The plan requires JSON
   guards to run against an accepted populated JSON Criterion root or fresh
   populated JSON guard capture, not an empty CSS-only directory. The companion
   flag must reject write/probe combinations and the test plan requires a
   no-write `RESULTS.md` byte-identity check plus a negative empty-root check.

## Must-Fix Before Redress

None.

## Redress Reject Conditions

Reject W1b-2b if any of these occur:

- `PASS-ADMIT-CANDIDATE` is recorded without gate-consumed Track 1, cssparser
  oracle, and lightningcss Criterion telemetry from valid `new/` lanes.
- ADMIT is claimed for equality at or below `lightningcss_mbps + 1`.
- The report trusts serialized threshold or margin fields instead of deriving
  and validating them.
- Strict three-way equality, independent oracle status, generated-size telemetry,
  provenance, JSON guard state, or REDRESS identity is missing or producer-only.
- `skinny/RESULTS.md` moves for a measured CSS miss, blocked/fail result, stale
  report, or future W3/W4/W5 promise.
- The companion gate accepts `--update-results`, `--write-results`,
  `--include-volatile-probes`, mixed companion reports, or an empty CSS-only
  Criterion root as JSON guard proof.
- W1b-2b wording claims final SK-V12 ADMIT/FIXPOINT, bypasses W3/W4
  dispositions where required, or bypasses W5 close reconciliation.

## Disposition

DISPOSITION: ACCEPT

The W1b-2b plan is redressable under CH6. It proves either a CSS
`PASS-ADMIT-CANDIDATE` or a measured baseline only through consumed telemetry,
strict equality, independent oracle evidence, derived threshold math, JSON guard
state, no-write gate behavior, and explicit REDRESS/RESULTS routing. It does
not permit RESULTS paper movement, future-promise admission, or bypass of
W3/W4/W5 campaign closure.
