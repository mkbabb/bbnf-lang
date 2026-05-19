# SK-V11 S-P1 Challenge V1 - CH1 Correctness

Disposition: REVISE.

CH1 scope is the S-P1 correctness contract: every hot-leaf claim needs symbol
path, self-time, and source file:line or an explicit xctrace authority; c/B must
come from real PMU counters; coverage must be 17/17 parse/direct and 7/7 typed;
and no `unprofiled` cell may remain unresolved
(`restart/prompts/skinny/PASS-1-PROFILE.md:123`,
`restart/prompts/skinny/PASS-1-PROFILE.md:127`). The universal CH1 lens also
requires resolving claim citations and measurable gates
(`restart/prompts/ORCHESTRATOR.md:83`), and any REVISE must fold before the next
cycle (`restart/prompts/ORCHESTRATOR.md:110`,
`restart/prompts/ORCHESTRATOR.md:116`).

## Findings

1. BLOCKING - P1-E does not make every hot-leaf claim source-resolvable.

   P1-E correctly declares xctrace self-time and PMU c/B authority
   (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:65`,
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:66`)
   and gives a vocabulary map for several clusters
   (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:74`,
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:90`).
   But the row tables still contain shorthand hot-leaf claims that lack an
   exact demangled symbol plus source file:line in P1-E: `memcpy`, `container`,
   `key colon`, `trailing-zeros`, `array next`, `wrapping-add`, `split-at`,
   `option copied`, `object direct`, `NonNull eq`, and `UTF-8 validation`
   (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:107`,
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:121`,
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:133`,
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:140`,
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:163`,
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:181`,
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:192`,
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:198`).
   Raw xctrace exports do contain the demangled leaves, for example
   `/tmp/skv11-p1/direct-xctrace/exports/update_center__real_typed_track2.symbols.json:9`
   for `core::str::validations::run_utf8_validation` and
   `/tmp/skv11-p1/parse-xctrace/exports/random__track2.symbols.json:25` for
   `<bbnf_bench::track2::json::Parser>::parse_key_colon`; the defect is the P1-E
   artifact's incomplete citation fold, not missing raw evidence. P1-B already
   shows the required pattern with a source-abbreviation map
   (`restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:84`,
   `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:121`).

2. PASS - xctrace self-time authority is justified for the P1-A/P1-B tables.

   P1-A records that samply parse artifacts exist but are
   `symbolicated=false`, so it uses xctrace summary and `*.symbols.json` exports
   for self-time rather than inventing samply percentages
   (`restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:66`,
   `restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:73`).
   P1-B makes the same product-plane authority choice
   (`restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:71`,
   `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:76`).
   This satisfies the "justified xctrace authority" allowance for self-time.

3. PASS - c/B figures are derived from real PMU counters.

   P1-D states that `xctrace_probe` and `profile_direct` emit `PROBE_RESULT`
   rows from `ri_cycles` and `ri_instructions`, and names the PMU TSVs as the
   numeric authority (`restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:51`,
   `restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:58`). Its
   coverage table has 34 parse, 34 direct, and 14 typed rows, all PMU `rc=0`,
   with aggregate c/B values
   (`restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:64`,
   `restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:68`). The raw
   TSV rows expose cycles, instructions, and `cycles_per_byte`, for example
   `/tmp/skv11-p1/pmu/parse_pmu_rows.tsv:1` and
   `/tmp/skv11-p1/pmu/product_pmu_rows.tsv:1`. Recomputing aggregates from the
   raw counters produced the same c/B figures: parse 2.777033, direct 4.428342,
   typed 3.190644.

4. PASS - corpus coverage is complete.

   W0 requires S-P1 to profile all 17 corpora, isolate 13 direct residual rows,
   four direct guard rows, and seven typed guard rows
   (`restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:82`,
   `restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:88`).
   P1-A reports parse-only Track 1/Track 2 coverage at 17/17
   (`restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:16`).
   P1-B reports direct 17/17 and typed 7/7 coverage
   (`restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:16`,
   `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:18`).
   P1-E's raw xctrace summary count matches that surface: 34 parse rows and 48
   direct-summary rows, meaning 17 parse corpora x 2 tracks and 17 direct corpora
   x 2 tracks plus seven typed corpora x 2 tracks
   (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:50`,
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:55`).

5. PASS - no `unprofiled` cell remains.

   A text scan found no `unprofiled` cells in the six P1 artifacts, W0, or
   `skinny/RESULTS.md`. `skinny/RESULTS.md` does contain comparator `n/a` and
   `absent:not-collected-*` sidecar fields, but P1-F explicitly classifies those
   as absent/stale comparator sidecar freshness rather than unresolved profile
   cells (`restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md:186`,
   `restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md:191`), and
   `RESULTS.md` fences PMU/cycles/masking/structural-scan as nonproducers
   (`skinny/RESULTS.md:146`).

## Required Fold

No new capture is required. Fold P1-E before any S-P1 V2 advance: add a
P1-B-style hot-leaf source map, or expand every P1-E row cell, so each shorthand
leaf resolves to the exact demangled symbol path and source file:line while
retaining the existing xctrace summary as self-time authority. The fold must
cover at least the currently unmapped or ambiguously mapped core/support leaves:
`memcpy`, `container`, `key colon`, `trailing-zeros`, `array next`,
`wrapping-add`, `split-at`, `option copied`, `object direct`, `NonNull eq`, and
`UTF-8 validation`.
