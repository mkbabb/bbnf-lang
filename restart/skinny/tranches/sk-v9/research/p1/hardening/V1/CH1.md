# SK-V9 S-P1 Hardening V1 CH1: Correctness

Pass: S-P1 Profile hardening.
Cycle: V1.
Date: 2026-05-18.
Scope: correctness review of P1-A through P1-F for file:line citations, corpus coverage, hot-leaf claims, PMU/c/B derivation, and honesty of missing fresh samply/PMU data.
Disposition: REVISE.
Confidence: 94%.

## Verdict

G-Alpha is treated closed by user instruction for this review; this file does not reopen Alpha. The P1 bundle is CH1-honest but not CH1-complete. It correctly marks missing fresh SK-V9-open samply and PMU data instead of inventing profiles, hot leaves, self-time percentages, or cycles-per-byte. That avoids REJECT.

It still cannot be ACCEPTed as a completed S-P1 profile. The S-P1 contract requires a checked SK-V{N}-open W0 baseline with no placeholder hot leaves before profiling (`restart/prompts/skinny/PASS-1-PROFILE.md:29-31`), all seventeen corpora for profiling agents (`restart/prompts/skinny/PASS-1-PROFILE.md:67-86`), samply symbol path + self-time + source file:line for hot leaves (`restart/prompts/skinny/PASS-1-PROFILE.md:123-127`), and real PMU-derived c/B (`restart/prompts/skinny/PASS-1-PROFILE.md:55`, `restart/prompts/skinny/PASS-1-PROFILE.md:123-127`). The current authority remains W0-rendered SK-V8-open telemetry, not SK-V9-open telemetry (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:21-27`; `skinny/RESULTS.md:44-85`).

## Defects

### CH1-D1 - Critical - No fresh SK-V9-open profile authority

The artifacts consistently say fresh SK-V9-open samply/PMU data is absent. P1-A reports W0 parse row enumeration as 17/17, but fresh SK-V9-open samply artifact coverage as 0/17 (`restart/skinny/tranches/sk-v9/research/p1/p1a-samply-mode-1.md:7-11`). P1-B reports the same missing fresh samply capture for mode II (`restart/skinny/tranches/sk-v9/research/p1/p1b-samply-mode-2.md:7-11`, `restart/skinny/tranches/sk-v9/research/p1/p1b-samply-mode-2.md:40-42`). P1-C reports all profile measurement cells as absent until W0 telemetry-lock (`restart/skinny/tranches/sk-v9/research/p1/p1c-samply-mode-3.md:7-11`, `restart/skinny/tranches/sk-v9/research/p1/p1c-samply-mode-3.md:34-40`). P1-D reports PMU/cycles coverage as 0/17 (`restart/skinny/tranches/sk-v9/research/p1/p1d-pmu-cycles.md:7-11`).

This is an honest gap, but it is a pass-blocking gap. W0 telemetry-lock is the next required fold because the planned SK-V9-open telemetry refresh is explicitly gate-only, behavior-frozen, and must produce/consume the SK-V9-open manifest before these rows can be treated as SK-V9-open evidence (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:47-52`, `restart/skinny/tranches/sk-v9/SYNTHESIS.md:111-117`).

### CH1-D2 - Critical - Hot-leaf attribution is unresolved

Current `skinny/RESULTS.md` hot-leaf cells are Criterion slope artifact bindings, not samply symbols with self-time percentages (`skinny/RESULTS.md:5-42`). P1-E states this explicitly: the gate formats Criterion bindings, accepts them as W0 hot-leaf cells, and those bindings are not resolved samply symbols with percentages (`restart/skinny/tranches/sk-v9/research/p1/p1e-hot-leaf-attribution.md:72-81`). P1-E therefore marks every main-row `% self-time` as a gap and every main-row hot-leaf class as `GAP:not-classified` (`restart/skinny/tranches/sk-v9/research/p1/p1e-hot-leaf-attribution.md:156-179`, `restart/skinny/tranches/sk-v9/research/p1/p1e-hot-leaf-attribution.md:189-207`).

That is correct as gap handling, but it does not satisfy CH1. Source-surface eligibility in P1-E Section 3 is useful scaffolding, not attribution; it must not feed S-P2 as measured hot-leaf evidence.

### CH1-D3 - Critical - PMU/c/B baseline is absent and correctly not derived

P1-D refuses to convert Criterion `ns/B` into c/B and states the governing rule: no c/B without same-run cycles and input bytes (`restart/skinny/tranches/sk-v9/research/p1/p1d-pmu-cycles.md:71-73`). It also states that `track1_ns` and `ns/B` are Criterion metadata, not PMU counters (`restart/skinny/tranches/sk-v9/research/p1/p1d-pmu-cycles.md:127-129`). The required fold is already named in P1-D: emit same-run cycles, instructions, branch misses, L1 misses, LLC misses, and derive `cycles_per_byte = cycles / input_bytes`; do not infer from wall time, throughput, CPU model, or frequency (`restart/skinny/tranches/sk-v9/research/p1/p1d-pmu-cycles.md:271-284`).

This is a hard REVISE because gate-json cannot consume a CH1 c/B baseline until the PMU fields exist.

### CH1-D4 - Major - Corpus coverage is enumeration coverage, not profiling coverage

The 17-corpus list is present and no float-only overfit is visible. P1-A enumerates all seventeen parse rows (`restart/skinny/tranches/sk-v9/research/p1/p1a-samply-mode-1.md:51-69`), P1-B enumerates all seventeen direct rows (`restart/skinny/tranches/sk-v9/research/p1/p1b-samply-mode-2.md:63-81`), and P1-C enumerates all seventeen mode-III rows (`restart/skinny/tranches/sk-v9/research/p1/p1c-samply-mode-3.md:46-64`).

But CH1 coverage is not just row enumeration. Fresh samply coverage is 0/17 for P1-A/P1-C and absent for P1-B. P1-B's `real_typed_struct` table is status coverage, not measured profile coverage: only four current measured typed rows exist, Apache/CITM are source/product-only, Canada is rejected, and the remaining rows are unsupported (`restart/skinny/tranches/sk-v9/research/p1/p1b-samply-mode-2.md:91-109`). The fold must split coverage columns into `enumerated`, `current W0 measured row`, `fresh SK-V9 samply artifact`, `fresh PMU row`, and `unsupported/rejected by contract`.

### CH1-D5 - Major - Masking probes are not current rendered telemetry

P1-C and P1-D correctly avoid pretending that masking probes are current `RESULTS.md` rows. P1-C says the report only renders masking rows when probe rows exist and that current `skinny/RESULTS.md` has no `## Masking Probes` section (`restart/skinny/tranches/sk-v9/research/p1/p1c-samply-mode-3.md:42-45`). P1-D likewise says masking probes are not currently rendered and cannot be attributed from the current report (`restart/skinny/tranches/sk-v9/research/p1/p1d-pmu-cycles.md:243-254`). REDRESS records masking probes as a required report artifact surface (`skinny/REDRESS.md:163-170`), so W0 telemetry-lock must either render or separately manifest these probe rows before P1-C/P1-D can close.

### CH1-D6 - Major - Typed row-table boundaries are handled honestly but remain open

P1-B and P1-F correctly do not promote Apache/CITM typed source/product parity into measured row-table progress. REDRESS 91 says Apache/CITM `real_typed_struct` rows are not present as measured rows in current W0 (`skinny/REDRESS.md:2622-2625`) and that `skinny/RESULTS.md` stayed unchanged with no claim of six measured `real_typed_struct A / GO` rows (`skinny/REDRESS.md:2648-2652`). Canada typed remains rejected on checksum mismatch (`skinny/REDRESS.md:2637-2640`). This is a correctness credit, but it also means P1-B's typed coverage cannot be counted as fresh product-plane profiling coverage.

## Correctness Credits

1. Missing fresh samply data is marked, not invented. P1-A names `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` for expected mode-I profiles and symbols (`restart/skinny/tranches/sk-v9/research/p1/p1a-samply-mode-1.md:151-158`). P1-B, P1-C, and P1-E use the same fail-closed posture (`restart/skinny/tranches/sk-v9/research/p1/p1b-samply-mode-2.md:150-152`; `restart/skinny/tranches/sk-v9/research/p1/p1c-samply-mode-3.md:66`; `restart/skinny/tranches/sk-v9/research/p1/p1e-hot-leaf-attribution.md:248-253`).

2. Missing PMU data is marked, not estimated. P1-D explicitly rejects c/B derivation without cycles (`restart/skinny/tranches/sk-v9/research/p1/p1d-pmu-cycles.md:71-73`, `restart/skinny/tranches/sk-v9/research/p1/p1d-pmu-cycles.md:127-129`).

3. P1-F's zero delta versus SK-V8 close is defensible because it compares `skinny/RESULTS.md` and `skinny/REDRESS.md` between SK-V8 close and current HEAD and reports no diff (`restart/skinny/tranches/sk-v9/research/p1/p1f-results-delta.md:19-27`, `restart/skinny/tranches/sk-v9/research/p1/p1f-results-delta.md:87-95`). It also flags the current manifest as RUN8 rather than SK-V9-open (`restart/skinny/tranches/sk-v9/research/p1/p1f-results-delta.md:39-40`, `restart/skinny/tranches/sk-v9/research/p1/p1f-results-delta.md:108-110`).

## Fold Requirements

1. Run W0 telemetry-lock before any S-P1 convergence claim. Produce a SK-V9-open run id and manifest consumed by `gate-json`, with no parser/scanner/SIMD/codegen behavior movement and no row additions unless separately gated (`restart/skinny/tranches/sk-v9/HANDOFF.md:47-51`; `restart/skinny/tranches/sk-v9/SYNTHESIS.md:111-117`).

2. Re-run P1-A/P1-B/P1-C on the SK-V9-open baseline with interactive, symbol-resolving samply and `debug=true`; do not use `--save-only` as a close substitute (`restart/prompts/skinny/PASS-1-PROFILE.md:251-254`). Each corpus/workload row must cite the flame artifact path, top symbol path, `% self-time`, source file:line, run id, host triple, and build flags.

3. Re-run P1-D with same-run PMU counters for every admitted corpus/workload row. Derive c/B only as `cycles / input_bytes` from same-run non-zero fields. Record cycles, instructions, branch misses, L1 misses, LLC misses, input bytes, sample count, run id, host/build metadata, and profile artifact.

4. Rebuild P1-E from the fresh P1-A/P1-B/P1-C artifacts. Every current Criterion hot-leaf proxy must either become a named samply symbol with `% self-time` and source file:line, or remain explicitly marked absent with a reason. Source-surface eligibility must stay separate from measured attribution.

5. Render or manifest masking-probe telemetry for mode III. `host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`, structural-scan-only, and any disabled/host-ineligible probes need explicit row status, measured signal, and PMU/samply linkage where runnable.

6. Keep REDRESS boundaries intact during the fold. Apache/CITM typed rows remain source/product-only until a measured row-table wave admits them (`skinny/REDRESS.md:2622-2652`); structural-heavy parse remains blocked until retained class/event grammar and `ValueRef` cursor proof exist (`skinny/REDRESS.md:2661-2690`); scalar-parent direct folding remains rejected without a V9-aware gate, full-table maintain measurement, and independent Track 2 backstop (`skinny/REDRESS.md:2692-2729`).

## Close

REVISE is the only correct CH1 disposition. The P1 bundle is valuable as a W0 gap ledger and it is honest about missing data, but it is not yet the measured SK-V9 S-P1 profile that CH1 can accept.
