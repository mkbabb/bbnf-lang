ACCEPT

# SK-V11 S-P1 V2 CH1 Correctness

Scope: CH1 correctness only, after commit `b8d8fb94`. The acceptance criteria
checked here are hot-leaf source resolution, PMU-backed c/B, complete profiling
coverage, and closure of any `unprofiled` cells.

## Findings

1. PASS - Hot-leaf claims now resolve to symbol and source locus, with explicit
   xctrace authority for self-time.

   The V1 blocker was P1-E shorthand. The V2 fold adds a hot-leaf vocabulary
   and source map covering the formerly unresolved support leaves, including
   `memcpy`, container/key-colon/array dispatch, `trailing_zeros`,
   `wrapping_add`, `split_at_checked`, `Option::copied`, `NonNull::eq`, and
   UTF-8 validation
   (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:104`).
   Local source loci for the generated parser, Track 2 parser,
   `parse-that-regex`, number scanner, direct digest, typed parser, and SIMD
   movemask functions resolve at the cited files and line numbers. P1-A and
   P1-B also state that saved samply profiles are `symbolicated=false` and that
   xctrace summaries plus `*.symbols.json` exports are the self-time symbol
   authority
   (`restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:93`,
   `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:96`,
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:91`).

2. PASS - PMU c/B is PMU-derived, not estimated.

   P1-D names `xctrace_probe` and `profile_direct` `PROBE_RESULT` rows from
   `ri_cycles` and `ri_instructions` as the numeric authority
   (`restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:76`).
   The raw PMU TSVs under `/tmp/skv11-p1/pmu/` expose `cycles`,
   `instructions`, and `cycles_per_byte`. Recomputing aggregate c/B from those
   rows matched P1-D exactly: parse 34 rows at 2.777033 c/B, direct 34 rows at
   4.428342 c/B, and typed 14 rows at 3.190644 c/B
   (`restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:89`).

3. PASS - Coverage is complete for CH1.

   W0 requires all 17 corpora, 13 direct residual rows, four direct guard rows,
   and seven typed guard rows
   (`restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:82`).
   P1-A reports 17/17 parse coverage; P1-B reports 17/17 direct and 7/7 typed
   coverage; P1-E's raw xctrace summaries report 34 parse rows and 48 product
   rows; P1-D reports 34 parse PMU rows, 34 direct PMU rows, and 14 typed PMU
   rows, all PMU `rc=0`
   (`restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:16`,
   `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:16`,
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:76`,
   `restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:89`).

4. PASS - No unprofiled cells remain.

   A text scan of P1-A through P1-F, W0, and `skinny/RESULTS.md` found no
   `unprofiled` cells. The remaining `n/a` and absent sidecar facts in
   `skinny/RESULTS.md` are comparator or diagnostic nonproducer fields, and
   P1-F explicitly routes PMU/cycles to P1-D rather than treating them as live
   result-row telemetry
   (`restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md:204`,
   `skinny/RESULTS.md:146`).

## Required Fold

None for CH1. The V2 packet is acceptable for correctness: hot-leaf claims are
source-resolvable or tied to explicit xctrace authority, c/B is backed by PMU
rows, coverage is complete, and no `unprofiled` cells remain.
