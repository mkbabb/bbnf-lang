ACCEPT

# SK-V11 S-P1 Hardening V3 CH6: Anti-Paper-Close

Date: 2026-05-19.
Scope: PASS-1-PROFILE.md Section 3 CH6, ORCHESTRATOR.md Section 3Z,
folded S-P1 packet P1-A through P1-F after commit `2e988a6a`, W0 baseline,
`skinny/RESULTS.md`, live `/tmp/skv11-p1` artifact roots, and the V1/V2
hardening consolidations.

## Findings

1. CH6 remains satisfied after the V2 -> V3 fold. PASS-1 CH6 rejects any
   "profiled" claim without an on-disk flame artifact and resolvable symbol
   evidence, and treats unexplained `unprofiled` or `n/a` table cells as
   paper-close (`restart/prompts/skinny/PASS-1-PROFILE.md:155`). ORCHESTRATOR
   Section 3Z requires challenge findings to fold before advancement
   (`restart/prompts/ORCHESTRATOR.md:104`). The V2 consolidation records CH6
   as ACCEPT with no required fold, and the only V2 fold changed Lock 14 wording
   in P1-B/P1-E, not capture evidence, row outcomes, gate floors, or artifacts
   (`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md:17`,
   `:22`, `:33`).

2. No placeholder or unprofiled profiling cell remains in the V3 packet. A text
   scan of P1-A through P1-F, W0, and `skinny/RESULTS.md` found no
   `unprofiled`, `placeholder`, `TODO`, `TBD`, `not profiled`, or `no profile`
   cell. `skinny/RESULTS.md` still has comparator `n/a` values, but those are
   source-qualified absences or historical-baseline absences, not S-P1 profile
   holes; the S-P1 row-classification surface is the W0 16 `S / NO-GO` plus
   1 `L / NO-GO` parse plane, 4 `A / GO` plus 13 `N-direct / NO-GO` direct
   plane, and 7 `A / GO` typed guard plane
   (`restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:24`).

3. Live artifacts back the profile claims. `/tmp/skv11-p1` is present. It
   contains 34 parse, 34 direct, and 14 typed samply `.json.gz` profiles, each
   with matching `.json.syms.json` sidecars. It also contains 34 parse and
   48 product xctrace Time Profiler trace bundles, 34 parse and 48 product CPU
   Counter trace bundles, 34 parse and 48 product `*.symbols.json` exports, and
   summary JSONs with 34/48 trace rows. The PMU row files contain 34 parse rows
   and 48 product rows. `capture_status.tsv` reports PMU `rc=0` for all
   82 PMU rows and samply `rc=0` for all 82 samply rows.

4. The xctrace and samply caveats are honest. P1-A states that exact per-row
   samply shell transcripts are not embedded, treats saved samply profiles as
   artifact-only evidence, and uses xctrace as the self-time percentage
   authority (`restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:60`).
   P1-A and P1-B both disclose that saved samply JSON reports
   `symbolicated=false` and relies on sidecar symbols plus xctrace exports for
   resolved self-time (`restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:93`,
   `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:96`).
   P1-D and P1-E also disclose the xctrace `rc=54` time-limit path: retained
   trace/export artifacts are used, but the packet does not claim those
   xctrace rows were clean exits
   (`restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:242`,
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:296`).

5. No row is closed from PMU, cycles, parse-only, structural-scan, or masking
   evidence. P1-D says no PMU row changes a `skinny/RESULTS.md` outcome and
   keeps PMU/cycles diagnostic (`restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:211`).
   P1-E says Criterion Mbps and W0 gate state remain row authority, while PMU
   throughput is only diagnostic c/B shape
   (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:91`).
   P1-C states masking probes and structural-scan rows are not behavior
   admissions or SOTA close targets, and structural scan is not a direct-row
   close (`restart/skinny/tranches/sk-v11/research/p1/p1c-samply-mode-3.md:79`,
   `:109`). P1-F keeps
   `structural_scan+masking_probes+pmu+cycles:nonproducer` fenced from direct
   or typed admission (`restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md:223`).

6. W0-clamped rows remain non-admissions. W0 names `instruments`, `numbers`,
   and `unicode_mixed` as W0-clamped `N-direct / NO-GO` rows and states W0
   captures do not admit behavior rows
   (`restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:47`,
   `:54`). P1-B, P1-C, P1-E, and P1-F preserve that classification; P1-E
   explicitly says treating them as closed would be a paper close
   (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:180`).

## Required Fold

None. Carry the existing CH6 guard forward unchanged: future consumers must
continue to require live profile artifacts and W0/RESULTS row authority, and
must not convert PMU/cycles, parse-only wins, structural-scan or masking probes,
or W0-clamped throughput into row admission.
