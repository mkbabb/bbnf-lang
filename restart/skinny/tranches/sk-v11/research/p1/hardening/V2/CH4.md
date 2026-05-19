ACCEPT

# SK-V11 S-P1 Hardening V2 CH4 Cost/Reproducibility

CH4 asks whether the S-P1 profile packet is rerunnable and honestly framed:
PASS-1 requires verbatim method commands plus run id, host triple, and build
flags; ORCHESTRATOR Section 3Z requires every V1 challenge disposition to fold
before the next cycle advances.

## Findings

- The V1 CH4 blockers are folded. The V1 consolidation records shared capture
  provenance added to P1-A through P1-F, P1-A samply evidence reframed as
  artifact-only where the exact transcript was not embedded, P1-C retitled as a
  W0 Criterion masking-probe extraction, P1-E run id added, and artifact caveats
  preserved.
- W0 is reproducible: it names commit `3ce75df4`, Criterion root
  `/tmp/skv11-open-criterion-3ce75df`, target root
  `/tmp/skv11-open-target-3ce75df`, run id
  `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`, the exact advisory capture
  command, and the exact `gate-json --with-cost-facts --check-results`
  verification command.
- The folded P1 packet now carries a common run id, host/toolchain, build
  profile, `RUSTFLAGS="-C target-cpu=native"`, source SHA `3ce75df4`,
  documentation freeze SHA `9c8da194`, target directory, binary paths, and exact
  build command for `xctrace_probe` and `profile_direct`.
- Binary/source provenance is explicit enough for CH4. The packet distinguishes
  the behavior/probe source SHA (`3ce75df4`) from the later documentation/results
  freeze (`9c8da194`) and states that the V2 fold edits documents only.
- Artifact caveats are not hidden. P1-A says saved samply logs do not embed the
  exact per-row transcript and uses samply as artifact-only flame-profile
  evidence, with xctrace as self-time authority. P1-B and P1-E preserve the
  `symbolicated=false` samply caveat. P1-D records that xctrace CPU Counter and
  Time Profiler traces mostly exit with `rc=54` on the time-limit path and that
  the PMU numeric authority is the rusage TSVs, not synthesized branch/L1/LLC
  columns.
- `/tmp/skv11-p1/pmu/capture_status.tsv` supports the coverage story: all 34
  parse samply rows, 34 direct samply rows, 14 typed samply rows, 34 parse PMU
  rows, and 48 product PMU rows are `rc=0`; the retained xctrace trace bundles
  carry the expected time-limit return-code caveat.
- P1-A and P1-C are now honestly framed. P1-A no longer claims unavailable
  samply-only self-time percentages. P1-C no longer presents itself as a new
  samply Mode III call-stack capture; it is a W0 Criterion diagnostic extraction
  for masking probes, structural scan, and lazy-tape evidence.
- The method framing remains read-only profile evidence. PMU/cycles,
  structural-scan-only, masking probes, parse-only rows, and W0-clamped rows are
  consistently treated as diagnostic/nonproducer evidence, not behavior-wave
  admissions.

## Required Fold

None. CH4 V2 can advance with the existing caveats carried forward: samply
artifact-only evidence where transcripts are absent, xctrace time-limit `rc=54`
as retained-trace behavior rather than clean exit, and PMU/cycles as planning
cost facts rather than row-admission evidence.
