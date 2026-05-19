# SK-V11 S-P1 V1 CH6: Anti-Paper-Close

Disposition: ACCEPT.
Date: 2026-05-19.
Scope: `PASS-1-PROFILE.md` Section 3 CH6, `ORCHESTRATOR.md` Sections 3W and
3Z, P1-A through P1-F, W0 baseline, `/tmp/skv11-p1` artifact roots, and
`/tmp/skv11-p1/pmu/capture_status.tsv`.
Output: this file.

## Verdict

The V1 S-P1 packet passes CH6. The packet does not close rows from self-report,
PMU/cycles facts, structural-scan facts, parse-only facts, or W0-clamped
throughput. The profile claims that are made have live artifacts on disk, and
the symbol-resolution limitation is stated honestly: saved samply profiles are
not treated as the self-time authority when their metadata reports
`symbolicated=false`; the actionable percentages come from xctrace Time
Profiler summary exports and per-trace symbol JSON files.

No required fold.

## Evidence Checks

Artifact existence:

- `capture_status.tsv` names 34 `samply-parse`, 34 `samply-direct`, and
  14 `samply-typed` raw profile paths; every named path exists and all have
  `rc=0`.
- Samply raw and sidecar counts match the claims: 34/34 parse `.json.gz` plus
  `.json.syms.json`, 34/34 direct `.json.gz` plus `.json.syms.json`, and 14/14
  typed `.json.gz` plus `.json.syms.json`.
- xctrace Time Profiler artifacts exist at the claimed coverage: 34 parse trace
  bundles and 48 product trace bundles. The export roots contain 34 parse
  `*.symbols.json`, 48 product `*.symbols.json`, and both `summary.json` files.
- xctrace CPU Counter trace bundles exist at the claimed coverage: 34 parse and
  48 product bundles.
- PMU row files are present and nonempty:
  `/tmp/skv11-p1/pmu/parse_pmu_rows.tsv`,
  `/tmp/skv11-p1/pmu/product_pmu_rows.tsv`, and
  `/tmp/skv11-p1/pmu/capture_status.tsv`.
- W0 roots cited by the packet exist:
  `/tmp/skv11-open-criterion-3ce75df`,
  `/tmp/skv11-open-target-3ce75df`, and
  `/tmp/skv11-profile-target-9c8da194`.

Return-code honesty:

- PMU captures are clean: 34/34 parse, 48/48 product, all `rc=0`.
- Samply captures are clean: 34/34 parse, 34/34 direct, 14/14 typed, all
  `rc=0`.
- xctrace `rc=54` is not hidden. The packet states it is the time-limit exit
  path and accepts it only when trace bundles and exported symbol JSON exist.
  `capture_status.tsv` matches that story: Time Profiler has 33/34 parse
  `rc=54` plus one `rc=0`, and 48/48 product `rc=54`; CPU Counters has 34/34
  parse `rc=54`, and 47/48 product `rc=54` plus one `rc=0`.

Symbol-resolution caveat:

- P1-A, P1-B, and P1-E explicitly fence saved samply JSON as
  `symbolicated=false` and use xctrace exports as the self-time authority.
- The sidecar symbol files exist, but the packet does not pretend they turn the
  saved samply JSON into the percent self-time authority. That satisfies the
  `samply-symbol-resolution` caveat rather than papering it over.
- P1-C does not claim a new samply call-stack capture. It uses W0 Criterion
  diagnostic artifacts for mode-III masking and structural-scan facts. That is
  a scope caveat, not an unbacked flame-profile claim.

Placeholder scan:

- No P1 artifact or W0 baseline contains an `unprofiled`, `n/a`, `N/A`,
  `not profiled`, or `no profile` table hole.

Row-admission fences:

- P1-A keeps parse-only evidence diagnostic and states parse rows cannot count
  toward SK-V11 close.
- P1-C keeps masking probes, structural scan, and lazy-tape materialization as
  diagnostic nonproducer evidence only.
- P1-D states that PMU/cycles rows do not change any `skinny/RESULTS.md`
  outcome and remain diagnostic nonproducers.
- P1-E uses PMU only for c/B shape and says Criterion/gate state remains the
  admission authority.
- P1-F records `Diagnostic nonproducer` as
  `structural_scan+masking_probes+pmu+cycles:nonproducer` and says those
  signals must not admit direct or typed rows.

W0-clamped rows:

- W0 names `instruments`, `numbers`, and `unicode_mixed` as W0-clamped
  `N-direct / NO-GO` non-admissions.
- P1-B repeats that all three remain non-admissions.
- P1-C repeats that W0 captures are planning evidence, not behavior-wave
  admissions.
- P1-E isolates them in a W0-clamped table and warns that treating them as
  closed would be a paper close.
- P1-F keeps them `N-direct / NO-GO` in the current floor-gap table.

Future-phase language:

- References to S-P2, S-P3, or later waves are routed as negative constraints
  or guard-surface context. They do not stand in for evidence and do not close
  a current row.

## Required Fold

None. Carry the CH6 constraints forward unchanged: downstream packets must keep
using the live profile artifacts and W0 row classifier as the evidence floor;
they must not convert PMU/cycles, structural scan, parse-only wins, lazy-tape
facts, or W0-clamped throughput into row admission.
