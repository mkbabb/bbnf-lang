ACCEPT

# SK-V12 S-P1 Hardening V2 CH3: Regression / REDRESS

Date: 2026-05-20.
Scope: CH3 regression / REDRESS lens for the current SK-V12 S-P1 packet after
commit `d1e6938a`.
Output: this file.

## Findings

1. The V2 packet does not reopen a REDRESS-blocked route. PASS-1 CH3 requires
   any anomaly that points at a pre-blocked route to cite the REDRESS entry and
   mark it pre-blocked, not silently reopen it
   (`restart/prompts/skinny/PASS-1-PROFILE.md:137` through `:141`). The folded
   P1-E matrix does that explicitly: structural/cursor/`UnionTape` rediscovery,
   parse-only throughput, string/decode paths, parser-control carry, numeric
   reuse, PMULL/CTZ body fill, direct-digest-as-typed proof, JSON residual row
   movement, and the generated non-JSON report-lane temptation are all bound to
   their prior REDRESS entries and given diagnostic/pre-blocked treatment
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:324`
   through `:336`). The underlying ledger supports those blocks: cursor/sidecar
   routes are rejected or narrowed by REDRESS 51 and 53
   (`skinny/REDRESS.md:742` through `:767`, `:784` through `:813`), decoded
   direct string stats/hash routes are rejected by REDRESS 54/55
   (`skinny/REDRESS.md:815` through `:882`), retained string/control routes are
   blocked or constrained across REDRESS 60-65 (`skinny/REDRESS.md:1346` through
   `:1681`), direct decoded/string materialization routes are rejected across
   REDRESS 66-69 (`skinny/REDRESS.md:1688` through `:1882`), numeric reuse is
   rejected by REDRESS 80 (`skinny/REDRESS.md:2217` through `:2248`), the
   single-quartet escape classifier is rejected by REDRESS 82
   (`skinny/REDRESS.md:2287` through `:2314`), StringBlock16 and object-pair
   value-byte compaction are rejected by REDRESS 83/84
   (`skinny/REDRESS.md:2320` through `:2356`, `:2360` through `:2395`), and
   aarch64 bitmap body fills are rejected by REDRESS 88/89
   (`skinny/REDRESS.md:2510` through `:2585`).

2. Row movement claims are consistent: no current P1 artifact claims
   `skinny/RESULTS.md` row movement. The capture manifest states that result
   authority remains `skinny/RESULTS.md`, the manifest records profile evidence
   only, and it moves no rows
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:15`
   through `:16`). P1-D says the PMU values do not move any result row, admit a
   direct or typed row, or change the opening `N-direct / NoGo` surface
   (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:87` through
   `:89`). P1-F records a clean SK-V11-close comparison with no `skinny/RESULTS.md`
   or `skinny/REDRESS.md` diff (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:66`
   through `:68`), then classifies all row deltas as unchanged
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:91`
   through `:136`). P1-E separately states no `skinny/RESULTS.md` row moved
   between SK-V11 close and SK-V12-open
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:340`
   through `:352`).

3. Parse/direct/typed result authority is preserved. The live result surface is
   unchanged: `parse_only` is 16 `S / NO-GO` plus one `L / NO-GO`,
   `direct_to_struct` is 4 `A / GO` plus 13 `N-direct / NO-GO`,
   `real_typed_struct` is 7 `A / GO`, and overall remains `N-direct / NoGo`
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:70`
   through `:87`; `skinny/RESULTS.md:5` through `:45`, `:143` through `:146`).
   P1-B keeps direct rows as JSON digest-plane rows, with four admitted guards
   and thirteen residual/W0-clamped rows, and keeps `real_typed_struct` rows as
   guarded JSON typed rows, not non-JSON baselines
   (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:140`
   through `:143`). It also states typed rows are output-plane-specific and
   direct digest evidence cannot admit a typed row
   (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:197`
   through `:204`). The SK-V12 synthesis agrees: parse-only is diagnostic, JSON
   direct residuals reopen only beyond REDRESS 114-119, and direct digest as
   typed proof is pre-blocked (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:62`
   through `:72`, `:228` through `:243`). That matches the typed-plane REDRESS
   history: REDRESS 70 rejected the first benchmark-private typed close, while
   REDRESS 71 admitted host/API-schema typed rows as a distinct output plane and
   explicitly kept the old `direct_to_struct` digest stressor visible as
   `N-direct`, not relabeled as typed product proof (`skinny/REDRESS.md:1890`
   through `:1940`, `:1944` through `:1992`).

4. W3 and parse-only remain nonproducer/diagnostic. P1-A says parse-only rows
   cannot count toward SK-V12 SOTA admission or close
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:105`
   through `:107`), and its anomaly section keeps parse-only movement, W3
   substrate routes, sidecar/cursor variants, decoded-byte/materialization routes,
   and JSON direct residual retries pre-blocked
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:175`
   through `:190`). P1-C records 0/17 fresh Mode III samply call stacks and no
   fresh structural-scan capture, then points to the fold manifest boundary:
   W0 Mode III throughput and structural-scan values are diagnostic nonproducer
   evidence only, not fresh SK-V12 hot-leaf authority
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:54`
   through `:63`; `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:160`
   through `:168`). P1-C's pre-block section states parse-only cannot admit
   product rows or close SK-V12, W3 substrate routes remain pre-blocked, and
   PMU/cycles/structural-scan/masking-probe/sidecar freshness/parser-inventory
   rows are nonproducers
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:229`
   through `:245`). REDRESS 102 is the matching firewall: it admits W3 only as
   proof-only, with no behavior source, generated output, benchmark body, or row
   movement, and validates that parse-only SOTA movement is rejected
   (`skinny/REDRESS.md:3042` through `:3057`).

5. The direct residual fixpoint is not loosened. REDRESS 119 closes W8 as a
   measured direct fixpoint, not direct `GO`, with no behavior source
   intervention, no W8a split, no gate semantic change, and no `skinny/RESULTS.md`
   row movement (`skinny/REDRESS.md:3497` through `:3505`). Its table preserves
   all 13 residual direct rows as uncloseable, W0-clamped, or blocked by W3-W7
   attempts (`skinny/REDRESS.md:3506` through `:3524`). REDRESS 120 closes
   SK-V11 as a measured fixpoint, not overall direct `GO` or grammar-generalized
   admission, preserves the same result surface, makes REDRESS 119 the direct
   row authority, and routes SK-V12 to solve the generated non-JSON baseline
   first (`skinny/REDRESS.md:3531` through `:3553`). P1-B, P1-C, P1-E, and P1-F
   all carry the same rule: fresh product PMU/xctrace data is diagnostic unless a
   later pass supplies material evidence beyond REDRESS 114-119 and the non-JSON
   priority resolves (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:169`
   through `:181`, `:276` through `:280`;
   `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:178`
   through `:199`; `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:247`
   through `:279`; `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:138`
   through `:165`).

6. The generated non-JSON blocker is routed as the first material target, not as
   a reopened JSON route. REDRESS 111 admitted only the companion non-JSON
   report/gate lane and did not update `skinny/RESULTS.md`, create generated
   non-JSON baseline authority, or move a parser row (`skinny/REDRESS.md:3284`
   through `:3309`). REDRESS 112 rejected the generated CSS L4 baseline because
   codegen still routed through `json_provider::ensure_runtime_profile` and no
   generated CSS L4 runtime existed (`skinny/REDRESS.md:3313` through `:3338`).
   REDRESS 113 blocked W2 because the missing baseline made the first measurable
   non-JSON intervention undefined (`skinny/REDRESS.md:3342` through `:3355`).
   P1-E and P1-F keep that distinction: the W1a report lane is not a generated
   Track 1 baseline, and the generated non-JSON codegen/runtime gap remains the
   first SK-V12 planning target
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:299`
   through `:322`; `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:183`
   through `:193`).

## Required Fixes

None for CH3. The V2 fold preserves the V1 CH3 ACCEPT result while strengthening
self-time provenance and the Mode III boundary
(`restart/skinny/tranches/sk-v12/research/p1/hardening/V1/FOLD-REVISIONS.md:10`
through `:20`, `:40` through `:44`). The packet keeps S-P1 evidentiary, marks
REDRESS-adjacent observations as pre-blocked or diagnostic, preserves the
parse/direct/typed result authority, keeps W3/parse-only nonproducer status
closed, and does not reopen a rejected route.
