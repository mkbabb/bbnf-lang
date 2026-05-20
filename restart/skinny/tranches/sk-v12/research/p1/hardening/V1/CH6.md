# SK-V12 S-P1 Hardening V1 CH6: Anti-Paper-Close

Disposition: REVISE.
Date: 2026-05-20.
Lens: CH6 anti-paper-close / next-tranche impact.
Scope: `restart/prompts/skinny/PASS-1-PROFILE.md`, SK-V12 P1-A through P1-F,
`/tmp/skv12-p1`, `restart/skinny/tranches/sk-v12/{SYNTHESIS,HANDOFF}.md`,
`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md`,
`skinny/RESULTS.md`, and `skinny/REDRESS.md` through REDRESS 120.
Output: this file.

## Verdict

The V1 packet is anti-paper-close on the next-tranche direction, but it is not
complete enough to pass CH6. It correctly prevents S-P2/S-P3 from treating JSON
profile evidence as the generated non-JSON baseline, and it does not invent a
close from PMU, parse-only, structural-scan, masking-probe, or JSON guard facts.
However, the packet also admits that fresh SK-V12 inline self-time percentages
were not exported. The raw profiles and symbol sidecars exist, but V1 does not
yet provide the per-row `% self-time` authority required by S-P1 for hot-leaf
closure.

Required fold: produce V2 with either symbolicated/exported self-time summaries
for the claimed parse/direct/typed hot leaves, or explicitly downgrade the V1
hot-leaf tables to source-map attribution only and block S-P2/S-P3 from using
them as exact hot-leaf antecedents. In all cases, carry the generated non-JSON
baseline-first gate forward unchanged.

## Findings

### CH6-1 - The cohort does not let S-P2/S-P3 skip the generated non-JSON baseline

Disposition: ACCEPT.

The opening SK-V12 contract says close requires S-P1, S-P2, and S-P3 convergence
and that the first material behavior target is one generated non-JSON direct or
typed parser baseline before JSON-only micro-waves
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:28`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:38`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:80`). `HANDOFF.md` repeats the same
priority and entry pre-gate: generated emission/runtime path, fixture corpus,
same-plane independent oracle, compile/equality smoke, and REDRESS 111 gate
consumption (`restart/skinny/tranches/sk-v12/HANDOFF.md:51`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:53`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:55`).

The P1 packet preserves that boundary. P1-B states that no non-JSON product row
exists in its capture set and that JSON product profiling does not substitute for
the required generated non-JSON baseline
(`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:236`,
`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:240`). P1-E
names the generated non-JSON blocker directly: `json_provider` remains JSON-only,
the runtime grammar inventory has no generated CSS L4 / Sheets / BBNF-self
baseline module, and the next plan should stand up exactly one generated
non-JSON baseline first
(`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:275`,
`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:281`,
`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:294`).
P1-F makes the same next-tranche distinction: the REDRESS 111 non-JSON report
lane is not a generated baseline, and current source reads still preserve the
REDRESS 112 blocker
(`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:183`,
`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:188`).

Required fold: S-P2/S-P3 must carry an explicit entry-gate sentence that no P1
profile row, PMU row, JSON guard, JSON residual, or REDRESS 111 report lane can
satisfy the generated non-JSON baseline requirement. The first behavior packet
must either create the generated non-JSON baseline evidence or record a measured
BLOCKED route inside the accepted owner surface.

### CH6-2 - Missing inline self-time percentages are honestly routed, but not closed

Disposition: REVISE.

`PASS-1-PROFILE.md` requires P1-E to resolve each hot-leaf cell to a named symbol
plus `% self-time` plus source file:line
(`restart/prompts/skinny/PASS-1-PROFILE.md:56`), and CH6 rejects self-reported
profiling without a citable flame artifact and resolvable symbol evidence
(`restart/prompts/skinny/PASS-1-PROFILE.md:155`,
`restart/prompts/skinny/PASS-1-PROFILE.md:160`).

The V1 packet does not fabricate those percentages, which is good anti-paper
behavior. P1-A says the retained samply JSON reports `symbolicated=false`, no
fresh xctrace summary export exists, and it therefore does not claim fresh
top-leaf percentages
(`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:95`,
`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:97`,
`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:99`).
P1-B repeats that no fresh direct-xctrace self-time summary exists and that
inline leaf percentages are not extracted
(`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:114`,
`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:116`,
`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:202`).
P1-E states the critical caveat directly: profiles and symbol sidecars exist, but
there are no exact per-inlined-frame self-time percentages
(`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:73`,
`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:76`,
`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:336`).
P1-F also flags that main-table hot leaves are Criterion slope artifact bindings,
not resolved samply symbols with `% self-time` and file:line
(`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:211`).

That routing prevents a paper-close, but it is still an unresolved CH6 fold. V1
cannot both claim S-P1 hot-leaf completeness and leave exact inline self-time
percentages absent.

Required fold: V2 must either add exported/symbolicated self-time tables for the
P1-A/P1-B/P1-E claimed rows, or reword the P1 conclusion and consolidation so
S-P1 advances only as a generated-baseline blocker inventory plus PMU/source-map
attribution. If the latter path is chosen, S-P2 may use V1 hot families only as
research cues, not as exact `% self-time` antecedents for primitive selection.

### CH6-3 - Raw artifacts are present and citable

Disposition: ACCEPT.

The raw capture inventory is present under `/tmp/skv12-p1`. P1-F records 328
PASS capture-status rows: 82 PMU captures, 82 samply captures, and 164 xctrace
captures (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:24`,
`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:25`). P1-E
records the same artifact shape as 82 samply `.json.gz` files, 82 companion
`.json.syms.json` files, and 164 retained xctrace trace bundles
(`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:26`).
P1-D names the PMU row files, target binaries, toolchain, and done markers
(`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:19`,
`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:31`).

Live inventory checks matched those claims at review time: 82 samply raw
profiles, 82 symbol sidecars, 164 xctrace `.trace` bundles, zero xctrace export
files, 752 log files, and PMU TSVs with 34 parse rows plus 48 product rows after
headers. The sample sidecar `/tmp/skv12-p1/samply/parse/twitter__track1.json.syms.json`
contains resolved Rust function names and source paths, while the matching raw
profile `/tmp/skv12-p1/samply/parse/twitter__track1.json.gz` reports
`symbolicated=false`. That combination supports artifact citation and symbol-map
availability, but not exact inline percentage closure without an export step.

Required fold: the S-P1 consolidation should include the `/tmp/skv12-p1`
inventory counts and explicitly distinguish raw profile/symbol-map presence from
self-time percentage authority.

### CH6-4 - S-P1 conclusion is actionable without inventing a close

Disposition: ACCEPT with carry-forward fold.

The actionable conclusion is narrow and valid: SK-V12-open is a freshness
rebinding of the SK-V11 measured close, not new JSON row movement; the material
work is the generated non-JSON baseline. P1-F states that bottom line directly
(`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:241`). P1-C
also says S-P1 proposes no behavior route and S-P2/S-P3 may use the evidence only
after respecting the priority order
(`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:218`).
`skinny/RESULTS.md` still records overall `N-direct / NoGo` and identifies Track
1 as generated JSON while Track 2 is the independent hand-coded parser
(`skinny/RESULTS.md:143`, `skinny/RESULTS.md:144`). REDRESS 119/120 likewise
records no behavior source, generated runtime, gate semantic, or RESULTS row
movement, and routes SK-V12 to solve the generated non-JSON baseline first
(`skinny/REDRESS.md:3497`, `skinny/REDRESS.md:3531`,
`skinny/REDRESS.md:3545`, `skinny/REDRESS.md:3549`).

This is enough to steer next-tranche planning away from another JSON-only direct
retry. It is not enough to close S-P1 as a complete hot-leaf percentage profile
until CH6-2 is folded.

Required fold: the V1 consolidation must not say `ready-for-S-P2` unless it also
records the self-time limitation and the chosen fold path. If the user pins S-P1
forward despite missing percentages, the handoff must state that exact hot-leaf
percentages are unavailable and cannot be used as proof of a primitive close.

## Required Folds

1. Add either symbolicated/exported self-time summaries for the claimed P1
   hot-leaf rows, or downgrade the V1 hot-leaf tables to source-map attribution
   only.
2. Carry an explicit S-P2/S-P3 entry gate: no JSON profile, PMU row, guard row,
   residual row, parse-only row, structural/masking diagnostic, or REDRESS 111
   report lane satisfies the generated non-JSON baseline requirement.
3. Include the `/tmp/skv12-p1` raw artifact inventory in the S-P1 consolidation:
   82 samply profiles, 82 samply symbol sidecars, 164 xctrace trace bundles, 82
   PMU rows, zero xctrace summary exports, and the final clean PMU rerun after
   the isolated cwd failure.
4. Keep PMU/cycles, Criterion slope, structural scan, masking probes, sidecar
   freshness, and parser inventory as diagnostics/nonproducers unless a later
   same-wave gate consumes them as behavior evidence.
