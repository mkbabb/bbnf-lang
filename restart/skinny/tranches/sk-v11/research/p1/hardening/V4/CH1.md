ACCEPT

# SK-V11 S-P1 V4 CH1 Confirmation

Scope: CH1 correctness only, confirming the folded S-P1 packet at HEAD
`cc8656b8`. The governing checks are PASS-1-PROFILE.md Section 3 CH1
(`restart/prompts/skinny/PASS-1-PROFILE.md:123` through `:127`) and
ORCHESTRATOR.md Section 3Z (`restart/prompts/ORCHESTRATOR.md:104` through
`:121`).

## Findings

1. PASS - Hot-leaf claims resolve to exact symbols/source loci or explicit
   xctrace authority.

   P1-A and P1-B state that saved samply profiles are artifact evidence while
   xctrace summaries and `*.symbols.json` exports are the self-time authority
   (`restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:93`,
   `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:96`,
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:91`).
   P1-E reports 34 parse xctrace rows and 48 product xctrace rows
   (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:76`).
   P1-B's hot-leaf source map covers direct/typed shorthand leaves
   (`restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:127`),
   and P1-E's vocabulary resolves the formerly blocked support leaves, including
   generated runtime, Track 2, parse-that-regex, number, direct digest, typed,
   serde/core, and SIMD leaves
   (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:104`).
   Spot checks resolve the cited local loci, including
   `skinny/crates/runtime/src/grammars/json/generated.rs:171`,
   `skinny/crates/bbnf-bench/src/track2/json.rs:314`,
   `skinny/crates/parse-that-regex/src/lib.rs:113`,
   `skinny/crates/parse-that-regex/src/number/mod.rs:106`,
   `skinny/crates/bbnf-bench/src/direct_struct.rs:565`,
   `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1811`, and
   `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4`.

2. PASS - c/B comes from PMU, not estimates.

   P1-D names `xctrace_probe` and `profile_direct` `PROBE_RESULT` rows,
   reading `ri_cycles` and `ri_instructions`, as the numeric authority
   (`restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:76`).
   The source loci for those counters resolve at
   `skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs:149` and
   `skinny/crates/bbnf-bench/src/bin/profile_direct.rs:115`. Recomputing from
   `/tmp/skv11-p1/pmu/parse_pmu_rows.tsv` and
   `/tmp/skv11-p1/pmu/product_pmu_rows.tsv` matches P1-D exactly: parse 34 rows
   at 2.777033 c/B, direct 34 rows at 4.428342 c/B, and typed guards 14 rows
   at 3.190644 c/B
   (`restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:89`).

3. PASS - Coverage is complete for CH1.

   PASS-1 requires all 17 corpora (`restart/prompts/skinny/PASS-1-PROFILE.md:69`
   through `:77`), and W0 requires S-P1 to cover 17 corpora, 13 direct
   residuals, four direct guards, and seven typed guards
   (`restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:82`).
   The folded packet reports P1-A parse coverage 17/17
   (`restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:16`),
   P1-B direct 17/17 and typed 7/7
   (`restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:16`),
   P1-C diagnostic coverage 17/17
   (`restart/skinny/tranches/sk-v11/research/p1/p1c-samply-mode-3.md:16`),
   P1-D parse/direct/typed PMU coverage 17/17, 17/17, and 7/7
   (`restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:15`),
   P1-E parse/direct/typed coverage 17/17, 17/17, and 7/7
   (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:14`),
   and P1-F 17/17 corpora plus 41/41 result rows
   (`restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md:14`).

4. PASS - No unresolved `unprofiled` cells remain.

   A current text scan found no `unprofiled` matches in P1-A through P1-F, W0,
   or `skinny/RESULTS.md`. The remaining `n/a` and `absent:*` fields are
   comparator or diagnostic nonproducer fields. P1-F explicitly routes
   PMU/cycles absence from the RESULTS row surface to P1-D
   (`restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md:204`),
   and the manifest fences `structural_scan+masking_probes+pmu+cycles` as
   `nonproducer`
   (`restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md:223`,
   `skinny/RESULTS.md:47`).

## Required Fold

None for CH1. V1's hot-leaf source-map blocker was folded, V2 and V3 accepted
CH1 with no required fold, and V4 confirmation finds no new CH1 defect.
