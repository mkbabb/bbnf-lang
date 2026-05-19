ACCEPT

# SK-V11 S-P1 Hardening V2 CH6: Anti-Paper-Close

Date: 2026-05-19.
Scope: PASS-1 Section 3 CH6, ORCHESTRATOR Section 3Z, the folded S-P1 V2
packet, W0 baseline, `skinny/RESULTS.md`, live `/tmp/skv11-p1` roots, and the
V1 consolidation.

## Findings

- V2 satisfies CH6. `PASS-1-PROFILE.md` rejects any "profiled" claim without an
  on-disk flame profile or resolvable symbol evidence, and treats unstated
  `unprofiled` or `n/a` cells as paper-close
  (`restart/prompts/skinny/PASS-1-PROFILE.md:155`). ORCHESTRATOR requires each
  CH6 close to cite live evidence and forbids foldless advancement
  (`restart/prompts/ORCHESTRATOR.md:88`,
  `restart/prompts/ORCHESTRATOR.md:112`,
  `restart/prompts/ORCHESTRATOR.md:116`).
- The V1 required folds were applied into the packet now marked `Cycle: V2
  fold`: shared capture provenance, xctrace authority where samply is
  artifact-only, P1-C retitled to W0 Criterion masking-probe extraction, and
  P1-E source/vocabulary/pre-block tightening
  (`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md:23`,
  `restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md:49`).
- No placeholder or unprofiled S-P1 table cell remains in P1-A through P1-F or
  W0. `skinny/RESULTS.md` still has intentional comparator `n/a` fields, but
  those are source-qualified absences such as `source=absence:w0:...` and every
  live row carries `structural_scan+masking_probes+pmu+cycles:nonproducer`
  (`skinny/RESULTS.md:49`, `skinny/RESULTS.md:51`).
- Live artifacts are present. `/tmp/skv11-p1` contains 34 parse, 34 direct, and
  14 typed samply `.json.gz` profiles with matching `.json.syms.json` files;
  xctrace has 34 parse and 48 product Time Profiler traces, CPU Counter traces,
  symbol JSON exports, and 34/48-row summary JSON files; PMU row TSVs hold 34
  parse rows and 48 product rows. The headerless `capture_status.tsv` reports
  PMU `rc=0` for all 82 PMU rows and samply `rc=0` for all 82 samply rows.
- V2 does not close rows from PMU/cycles, parse-only, structural-scan, masking,
  or W0-clamped evidence. W0 freezes parse-only as diagnostic and names
  `instruments`, `numbers`, and `unicode_mixed` as W0-clamped non-admissions
  (`restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:54`,
  `restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:84`). P1-C
  says masking and structural-scan rows are not behavior admissions
  (`restart/skinny/tranches/sk-v11/research/p1/p1c-samply-mode-3.md:79`,
  `restart/skinny/tranches/sk-v11/research/p1/p1c-samply-mode-3.md:109`). P1-D
  says PMU/cycles do not change any `RESULTS.md` outcome
  (`restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:211`). P1-E
  keeps Criterion and W0 gate state as the row authority and treats PMU as c/B
  shape only (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:91`,
  `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:302`).
  P1-F repeats the diagnostic-nonproducer fence
  (`restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md:223`).
- The xctrace and samply caveats are honest. P1-A records that the exact per-row
  samply transcript is not embedded and keeps saved samply as artifact-only
  while using xctrace for self-time
  (`restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:60`,
  `restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:96`). P1-B
  states the raw samply JSON reports `symbolicated=false` and uses xctrace
  summary plus per-trace symbol JSON as authority
  (`restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:96`). P1-C
  explicitly says it is not a new samply call-stack capture
  (`restart/skinny/tranches/sk-v11/research/p1/p1c-samply-mode-3.md:13`).
  P1-D and P1-E disclose the xctrace `rc=54` time-limit path and do not claim
  those rows were clean exits
  (`restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:244`,
  `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:296`).

## Required Fold

None. Carry the existing CH6 guard forward unchanged: future S-P1 consumers must
continue to treat PMU/cycles, parse-only wins, structural-scan/masking probes,
and W0-clamped throughput as evidence context, not row-closing authority.
