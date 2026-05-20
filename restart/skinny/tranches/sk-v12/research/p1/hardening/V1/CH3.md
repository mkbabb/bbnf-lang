ACCEPT

# SK-V12 S-P1 Hardening V1 CH3: Regression / REDRESS

Date: 2026-05-20.
Scope: CH3 regression / REDRESS only. Reviewed the SK-V12 S-P1 profile packet
`p1a` through `p1f`, `PASS-1-PROFILE.md`, SK-V12 `SYNTHESIS.md`,
`HANDOFF.md`, G-Alpha, `skinny/REDRESS.md` through REDRESS 120, and the SK-V11
S-P1 hardening precedent.
Output: this file.

## Standard

PASS-1 CH3 asks whether any S-P1 anomaly silently re-proposes a route already in
`skinny/REDRESS.md`; a suggestive "hot leaf suggests X" note must cite the
entry and mark the route pre-blocked, not reopen it
(`restart/prompts/skinny/PASS-1-PROFILE.md:137` through `:141`). S-P1 remains a
measurement pass, not an intervention pass
(`restart/prompts/skinny/PASS-1-PROFILE.md:270` through `:279`).

The SK-V12 opening contract tightens that standard for this cycle: generated
non-JSON baseline first; `parse_only` diagnostic only; JSON direct residuals
reopen only with fresh material evidence beyond REDRESS 114-119; W3
union/event/class-column/streaming-cursor/class-lane/sidecar substrate routes
closed; and W0-clamped direct admission blocked by docs-only accounting
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:38` through `:49`,
`:62` through `:72`, `:228` through `:236`). G-Alpha repeats the same fences:
JSON direct residuals stay pre-blocked by REDRESS 119/120, `parse_only` remains
diagnostic, and W3 substrate routes remain pre-blocked by REDRESS 96/97 plus
SK-V11 fixpoint evidence
(`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:33`
through `:45`).

## Findings

1. No retired W3, substrate, or parse-only route is reopened. REDRESS 96 and
   97 measured two faithful W3 substrate implementations and both failed every W3
   must-improve row; REDRESS 98 then retired `G-W3-UNION-SUBSTRATE` rather than
   leaving it merely blocked (`skinny/REDRESS.md:2797` through `:2848`,
   `:2852` through `:2906`, `:2910` through `:2950`). The SK-V12 P1 packet
   carries that negative authority forward: P1-A keeps parse-only diagnostic and
   blocks W3/substrate retries (`p1a-samply-mode-1.md:105` through `:107`,
   `:185` through `:188`); P1-C says structural-scan observations cannot reopen
   sidecar, retained-vector, class-column, streaming-cursor, `UnionTape`, or
   class-lane routes (`p1c-samply-mode-3.md:208` through `:221`,
   `:225` through `:240`); and P1-E carries the load-bearing pre-block matrix
   for structural rediscovery, parse-only, sidecar, parser-owned cursor, and
   `UnionTape` temptations (`p1e-hot-leaf-attribution.md:302` through `:312`).

2. The JSON direct residual fixpoint is honored. REDRESS 119 closes W8 as a
   measured direct fixpoint with no behavior source intervention, no W8a split,
   no gate semantic change, and no `skinny/RESULTS.md` row movement
   (`skinny/REDRESS.md:3497` through `:3505`). Its table keeps all 13 residual
   direct rows pre-blocked, including W3/W4/W5/W6/W7 blocked or measured-rejected
   routes (`skinny/REDRESS.md:3506` through `:3524`). REDRESS 120 closes SK-V11
   as a measured fixpoint, not overall direct `GO`, and says future direct work
   needs material evidence beyond REDRESS 114-119
   (`skinny/REDRESS.md:3531` through `:3553`). P1-F repeats the same authority
   and preserves the 4 `A / GO`, 13 `N-direct / NO-GO` surface
   (`p1f-results-delta.md:138` through `:165`, `:232` through `:244`). P1-B and
   P1-E treat fresh product PMU and hot-family data as diagnostic cost shape, not
   as a source delta, same-wave gate consumer, or residual reopen
   (`p1b-samply-mode-2.md:161` through `:175`,
   `p1e-hot-leaf-attribution.md:327` through `:332`).

3. W0-clamped rows are not admitted by profile data. The live result surface
   still classifies `instruments/direct_to_struct`, `numbers/direct_to_struct`,
   and `unicode_mixed/direct_to_struct` as `N-direct / NO-GO`
   (`skinny/RESULTS.md:33`, `:35`, `:37`). REDRESS 119 names those same rows as
   W0-clamped or W0-clamped-plus-route-blocked, and says W8 admits no direct row
   and no W0-clamped row (`skinny/REDRESS.md:3517` through `:3524`). P1-B
   records fresh PMU rows for all three but keeps them as W0-clamped
   non-admissions (`p1b-samply-mode-2.md:153` through `:175`). P1-E and P1-F
   preserve the same treatment in their residual/fixpoint tables
   (`p1e-hot-leaf-attribution.md:245` through `:247`,
   `p1f-results-delta.md:154` through `:156`).

4. REDRESS 112/113 non-JSON baseline blocking is routed accurately. REDRESS 111
   admitted only the companion non-JSON report lane; it did not relax JSON W0
   validation, update `skinny/RESULTS.md`, create generated non-JSON baseline
   authority, or move a parser row (`skinny/REDRESS.md:3284` through `:3309`).
   REDRESS 112 rejected W1b because codegen/runtime remained JSON-profiled and
   no generated CSS L4 runtime existed (`skinny/REDRESS.md:3313` through
   `:3338`). REDRESS 113 then blocked W2 because W2 could not create the first
   measurable non-JSON row and consume a missing `W1b_css_baseline_mbps`
   (`skinny/REDRESS.md:3342` through `:3355`). P1-E and P1-F route this exactly:
   generated non-JSON baseline remains the first SK-V12 planning target, the W1a
   report lane is not a generated baseline, and current source inventory still
   shows `json_provider::ensure_runtime_profile` gating direct and typed emission
   to JSON with no generated `css_l4`, `css_l4_declaration_values`, `sheets`, or
   `bbnf_self` runtime module (`p1e-hot-leaf-attribution.md:275` through `:298`,
   `p1f-results-delta.md:183` through `:193`). Source read confirms the blocker:
   `json_provider.rs` accepts only `backend.grammar_name == "json"`, and both
   direct and typed emit paths call that guard
   (`skinny/crates/codegen/src/json_provider.rs:4` through `:13`,
   `skinny/crates/codegen/src/lib.rs:102` through `:109`,
   `:139` through `:147`).

5. Diagnostic nonproducer telemetry stays nonproducer. P1-C explicitly marks
   product admission from probes or structural scan as 0/17 and pre-blocked by
   the `structural_scan+masking_probes+pmu+cycles:nonproducer` metadata
   (`p1c-samply-mode-3.md:139` through `:155`). P1-D states the PMU values do
   not move `skinny/RESULTS.md`, admit a direct or typed row, or change the
   opening `N-direct / NoGo` surface, and keeps PMU/cycles/structural-scan data
   inside nonproducer telemetry (`p1d-pmu-cycles.md:81` through `:89`,
   `:262` through `:265`). P1-F also records the same nonproducer marker and
   requires same-wave gate consumption before any diagnostic metadata can become
   behavior evidence (`p1f-results-delta.md:225` through `:228`).

6. The remaining profile-method caveats are not CH3 regressions. P1-E says exact
   SK-V12 per-inlined-frame self-time percentages are absent and hardening may
   require regenerated summaries, but it does not convert the absent percentages
   into behavior prescriptions or row movement
   (`p1e-hot-leaf-attribution.md:336` through `:342`). That is CH1/CH6 evidence
   quality territory, not a REDRESS reopen.

## Required Fixes

None for CH3. The V1 packet keeps S-P1 as profile evidence, preserves the W3 and
parse-only retirements, honors the REDRESS 119/120 direct residual fixpoint,
does not admit W0-clamped rows from profile data, and routes REDRESS 112/113 as
the first SK-V12 blocker.
