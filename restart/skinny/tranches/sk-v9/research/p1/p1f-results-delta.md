# SK-V9 P1-F: RESULTS Extraction And Delta

Pass: S-P1 Profile. Cycle: V9.
Date: 2026-05-18.
Scope: P1-F RESULTS extraction, schema-v3 classification, delta versus SK-V8 close, and stale/absent telemetry flags.
Output: this file.
Baseline: current HEAD `b258a406` (`docs(sk-v9-alpha): close alpha challenge for G-Alpha`), using SK-V8 close commit `32870fea` as the prior close comparison point.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max` as rendered in the current telemetry manifest.
Build flags: `profile=bench;rustflags=-C target-cpu=native;target_cpu=native` as rendered in the current telemetry manifest.
Profile tool: current `RESULTS.md` exposes Criterion slope-profile artifact bindings only; no P1-A/P1-B/P1-C samply artifacts were committed before this P1-F artifact.
Corpus coverage: 17/17 corpora, 38/38 current measured rows.

## Section 1 - Method

P1-F's assigned scope is to extract every `skinny/RESULTS.md` row, compute per-row delta versus the prior SK close, classify with the schema-v3 outcome enum, and flag absent or stale telemetry against SK-V8 SPEC Section 0.4; the S-P1 prompt binds that role at `restart/prompts/skinny/PASS-1-PROFILE.md:50-57`. The same prompt requires reading `skinny/RESULTS.md`, `skinny/REDRESS.md`, the current tranche handoff, the prior iteration's RESULTS, and the S-P1 prompt before producing the artifact; it permits P1-F to consume P1-A/P1-B/P1-C only if those artifacts committed first at `restart/prompts/skinny/PASS-1-PROFILE.md:59-65`.

Current HEAD is treated as the Alpha-closed opening authority because SK-V9 Alpha hardening V4 reports two consecutive ACCEPT cycles and says Pass Alpha is converged for G-Alpha presentation at `restart/skinny/tranches/sk-v9/research/alpha-hardening/V4/CONSOLIDATED.md:35-42`. The G-Alpha presentation recommends `G-Alpha closed` and lists the current SK-V8 close state as the benchmark authority at `restart/skinny/tranches/sk-v9/research/g-alpha/G-ALPHA-PRESENTATION.md:7-34`.

Prior close comparison used `32870fea` (`docs(sk-v8-wave6-close): close SK-V8 after V2 convergence`). The comparison commands were:

```bash
git diff --quiet 32870fea..HEAD -- skinny/RESULTS.md
git diff --quiet 32870fea..HEAD -- skinny/REDRESS.md
```

Both commands exited 0 with no output. This matches the written close authority: SK-V8 W6 closes with no source, generated-output, benchmark-row, `skinny/RESULTS.md`, or `skinny/REDRESS.md` change at `restart/skinny/tranches/sk-v8/research/wave-6-hardening/V2/HARDENING-W6-V2-CONSOLIDATED.md:44-50`, and SK-V9 synthesis says the benchmark authority remains the W0-rendered report with 38 `SK-V8-open` manifest rows and overall `N-direct / NoGo` at `restart/skinny/tranches/sk-v9/SYNTHESIS.md:21-27`.

## Section 2 - Extraction

The current rendered table has 26 schema-v3 columns at `skinny/RESULTS.md:3-4`; the 38 current measured rows span `skinny/RESULTS.md:5-42`. The current telemetry manifest has 21 adjacent fields at `skinny/RESULTS.md:44-85`. The overall report outcome remains `N-direct / NoGo` at `skinny/RESULTS.md:138-141`.

Telemetry flag legend:

| Flag | Meaning |
|---|---|
| `D` | `Strictness=deferred`; not strict admission evidence. |
| `VB` | `parse_utf8=view-boundary` / measured validation is not `measured-row`. |
| `HL` | Hot leaf is a Criterion slope-profile binding, not a symbol plus self-time percentage. |
| `CF0` | `RESULTS.md` manifest CostFacts field is still `none:pre-W1` rather than the later W1 CostFacts gate manifest. |
| `RUN8` | Manifest run id and wave remain `SK-V8-open`; no `SK-V9-open` telemetry refresh exists in current `RESULTS.md`. |
| `CPP-H` | At least one populated C++ sidecar comparator cell is historical planning signal. |
| `CPP-A` | At least one C++ sidecar comparator cell is explicitly absent / `n/a`. |

| Source | Row | schema-v3 class | Output plane | T1 | T2 | sonic strict | serde_json | Delta sonic | Delta simdjson | Delta yyjson | Delta vs SK-V8 close | Telemetry flags |
|---|---|---|---|---:|---:|---:|---:|---:|---:|---:|---|---|
| `skinny/RESULTS.md:5` | `twitter/parse_only` | S/NO-GO | borrowed view over offset tape vs DOM | 9581 | 9741 | 18176 | 3829 | -47.3% | -60.9% | -69.0% | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-H, CPP-A |
| `skinny/RESULTS.md:6` | `twitter/direct_to_struct` | N-direct/NO-GO | digest | 11859 | 9881 | 12890 | 6673 | -8.0% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:7` | `twitter/real_typed_struct` | A/GO | typed direct | 15333 | 14516 | 13646 | 15046 | +12.4% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:8` | `citm_catalog/parse_only` | S/NO-GO | borrowed view over offset tape vs DOM | 28644 | 19214 | 21717 | 7401 | +31.9% | -20.0% | +36.7% | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-H, CPP-A |
| `skinny/RESULTS.md:9` | `citm_catalog/direct_to_struct` | A/GO | digest | 21151 | 19434 | 18241 | 12992 | +16.0% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:10` | `canada/parse_only` | L/NO-GO | borrowed view over offset tape vs DOM | 15497 | 12171 | 8729 | 4050 | +77.5% | +34.8% | +19.2% | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-H, CPP-A |
| `skinny/RESULTS.md:11` | `canada/direct_to_struct` | N-direct/NO-GO | digest | 6586 | 9769 | 12430 | 7080 | -47.0% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:12` | `apache_builds/parse_only` | S/NO-GO | borrowed view over offset tape vs DOM | 12694 | 11715 | 16904 | 4278 | -24.9% | -64.8% | -22.0% | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-H, CPP-A |
| `skinny/RESULTS.md:13` | `apache_builds/direct_to_struct` | N-direct/NO-GO | digest | 8306 | 7796 | 8852 | 6750 | -6.2% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:14` | `github_events/parse_only` | S/NO-GO | borrowed view over offset tape vs DOM | 10689 | 10073 | 16408 | 4675 | -34.9% | -73.0% | -50.1% | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-H, CPP-A |
| `skinny/RESULTS.md:15` | `github_events/direct_to_struct` | N-direct/NO-GO | digest | 9088 | 7337 | 9818 | 8152 | -7.4% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:16` | `update_center/parse_only` | S/NO-GO | borrowed view over offset tape vs DOM | 11926 | 9312 | 18769 | 4131 | -36.5% | -61.0% | -35.7% | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-H, CPP-A |
| `skinny/RESULTS.md:17` | `update_center/direct_to_struct` | N-direct/NO-GO | digest | 7863 | 7514 | 10525 | 8218 | -25.3% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:18` | `update_center/real_typed_struct` | A/GO | typed direct | 11958 | 10367 | 11952 | 10296 | +0.0% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:19` | `mesh/parse_only` | S/NO-GO | borrowed view over offset tape vs DOM | 9367 | 10000 | 8143 | 4123 | +15.0% | -0.5% | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-H, CPP-A |
| `skinny/RESULTS.md:20` | `mesh/direct_to_struct` | N-direct/NO-GO | digest | 8640 | 9049 | 9967 | 7176 | -13.3% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:21` | `mesh/real_typed_struct` | A/GO | typed direct | 9623 | 7674 | 9305 | 8212 | +3.4% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:22` | `random/parse_only` | S/NO-GO | borrowed view over offset tape vs DOM | 10011 | 8018 | 15639 | 3486 | -36.0% | -51.5% | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-H, CPP-A |
| `skinny/RESULTS.md:23` | `random/direct_to_struct` | N-direct/NO-GO | digest | 7751 | 6952 | 8141 | 5922 | -4.8% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:24` | `gsoc-2018/parse_only` | S/NO-GO | borrowed view over offset tape vs DOM | 23209 | 21857 | 49101 | 10741 | -52.7% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:25` | `gsoc-2018/direct_to_struct` | N-direct/NO-GO | digest | 15042 | 14380 | 23356 | 19398 | -35.6% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:26` | `marine_ik/parse_only` | S/NO-GO | borrowed view over offset tape vs DOM | 13100 | 12164 | 9921 | 4091 | +32.1% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:27` | `marine_ik/direct_to_struct` | A/GO | digest | 9357 | 9488 | 8559 | 7018 | +9.3% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:28` | `marine_ik/real_typed_struct` | A/GO | typed direct | 11783 | 8321 | 6951 | 7450 | +69.5% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:29` | `instruments/parse_only` | S/NO-GO | borrowed view over offset tape vs DOM | 13320 | 11351 | 17976 | 3028 | -25.9% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-H, CPP-A |
| `skinny/RESULTS.md:30` | `instruments/direct_to_struct` | N-direct/NO-GO | digest | 8494 | 8766 | 9872 | 7576 | -14.0% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:31` | `numbers/parse_only` | S/NO-GO | borrowed view over offset tape vs DOM | 12818 | 13537 | 9854 | 4422 | +30.1% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:32` | `numbers/direct_to_struct` | N-direct/NO-GO | digest | 9773 | 6966 | 7953 | 5753 | +22.9% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:33` | `unicode_mixed/parse_only` | S/NO-GO | borrowed view over offset tape vs DOM | 6390 | 4970 | 9943 | 2654 | -35.7% | -51.4% | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-H, CPP-A |
| `skinny/RESULTS.md:34` | `unicode_mixed/direct_to_struct` | N-direct/NO-GO | digest | 3596 | 3694 | 10077 | 4911 | -64.3% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:35` | `unicode_escapes/parse_only` | S/NO-GO | borrowed view over offset tape vs DOM | 12731 | 8521 | 13851 | 4040 | -8.1% | +125.9% | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-H, CPP-A |
| `skinny/RESULTS.md:36` | `unicode_escapes/direct_to_struct` | N-direct/NO-GO | digest | 4020 | 4016 | 13999 | 3720 | -71.3% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:37` | `unicode_basic/parse_only` | S/NO-GO | borrowed view over offset tape vs DOM | 11189 | 10040 | 15797 | 3611 | -29.2% | -31.3% | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-H, CPP-A |
| `skinny/RESULTS.md:38` | `unicode_basic/direct_to_struct` | A/GO | digest | 9363 | 8420 | 8971 | 6002 | +4.4% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:39` | `distinct_values/parse_only` | S/NO-GO | borrowed view over offset tape vs DOM | 10279 | 6457 | 18282 | 3158 | -43.8% | -55.0% | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-H, CPP-A |
| `skinny/RESULTS.md:40` | `distinct_values/direct_to_struct` | N-direct/NO-GO | digest | 4438 | 4151 | 8950 | 5598 | -50.4% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |
| `skinny/RESULTS.md:41` | `y_string_unicode/parse_only` | S/NO-GO | borrowed view over offset tape vs DOM | 5577 | 5480 | 12009 | 5657 | -53.6% | -59.1% | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-H, CPP-A |
| `skinny/RESULTS.md:42` | `y_string_unicode/direct_to_struct` | N-direct/NO-GO | digest | 4828 | 3563 | 9065 | 7599 | -46.7% | n/a | n/a | 0 / 0 / same | D, VB, HL, CF0, RUN8, CPP-A |

## Section 3 - Delta vs SK-V8 Close

The delta comparison is fully derivable for current rows because `skinny/RESULTS.md` is byte-identical between SK-V8 close commit `32870fea` and current HEAD `b258a406`. Therefore:

| Delta axis | Result | Evidence |
|---|---|---|
| Row set | 38/38 unchanged; no added or removed current measured rows | Current rows are `skinny/RESULTS.md:5-42`; SK-V8 W6 says no `RESULTS.md` edit was needed because no measured row status changed at `restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md:53-55`. |
| Track 1 Mbps | 0 Mbps delta on every row | Git comparison command in Section 1; current row cells are extracted from `skinny/RESULTS.md:5-42`. |
| Track 2 Mbps | 0 Mbps delta on every row | Git comparison command in Section 1; current row cells are extracted from `skinny/RESULTS.md:5-42`. |
| Outcome/verdict | unchanged on every row | Current outcome counts are repeated by the SK-V9 G-Alpha presentation at `restart/skinny/tranches/sk-v9/research/g-alpha/G-ALPHA-PRESENTATION.md:20-34`. |
| REDRESS route ledger | unchanged from SK-V8 close | W6 records no W6 REDRESS entry and no `REDRESS.md` edit at `restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md:46-55`. |

The current table's own `Delta vs SK-V6` field is not this P1-F delta. It remains non-derivable from machine-readable SK-V6 baseline data in the current report rows, and SK-V8 handoff calls that caveat out at `restart/skinny/tranches/sk-v8/HANDOFF.md:48-56`.

## Section 4 - Stale Or Absent Telemetry

SK-V8 SPEC Section 0.4 requires row id, grammar, domain, comparator metadata, measured validation path, profile artifact, sample cost, sample count, build and host metadata, CostFacts, redress entry, wave id, run id, sidecar freshness, SK-V8-open delta, substrate fields, consumer class, and Track 2 independence at `restart/skinny/tranches/sk-v8/SPEC.md:103-146`. P3-D further defines allowed values and refusal rules at `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:59-101` and `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:119-129`.

| Field / family | Current RESULTS status | P1-F flag | Citation |
|---|---|---|---|
| Strictness | All current main rows render `deferred`; no row can be used as strict admission evidence. | `D` | `skinny/RESULTS.md:5-42`; strict-admission refusal rules at `restart/skinny/tranches/sk-v8/SPEC.md:73-81`. |
| Measured validation path / UTF-8 | All current main rows render `parse_utf8=view-boundary`; current rows are not measured-row validation proof. | `VB` | `skinny/RESULTS.md:5-42`; P3-D `measured_validation_path` allowed values at `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:95`. |
| Hot leaf and profile artifact | Each current main row has a Criterion slope-profile binding, not a symbol plus self-time percentage from P1-A/P1-B/P1-C/P1-E. | `HL` | `skinny/RESULTS.md:5-42`; P1-E's expected symbol + percentage role at `restart/prompts/skinny/PASS-1-PROFILE.md:56-57`; P3-D hot-leaf shape at `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:81-83`. |
| CostFacts in RESULTS | Manifest CostFacts cells remain `none:pre-W1:none:pre-W1:none:pre-W1`. SK-V8 W1 later bound CostFacts externally but left `skinny/RESULTS.md` unchanged, so current RESULTS is stale for post-W1 CostFacts detail. | `CF0` | Manifest rows `skinny/RESULTS.md:48-85`; W1 unchanged-RESULTS record at `restart/skinny/tranches/sk-v8/HANDOFF.md:170-179`. |
| Run identity | Manifest wave/run id remains `SK-V8-open` / `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`; no SK-V9-open run exists in current RESULTS. | `RUN8` | Manifest rows `skinny/RESULTS.md:48-85`; SK-V9-open refresh is a gate-only prerequisite at `restart/skinny/tranches/sk-v9/SYNTHESIS.md:47-52`. |
| Sidecar freshness | Native Rust comparators are same-run, but C++ sidecars are historical where populated and absent elsewhere; no structured same-run sidecar manifest exists. | `CPP-H`, `CPP-A` | W0 telemetry note at `skinny/RESULTS.md:141`; SK-V8 handoff caveat at `restart/skinny/tranches/sk-v8/HANDOFF.md:48-56`; sidecar missing-validation list at `restart/skinny/tranches/sk-v8/research/wave-0-sidecar-freshness-research.md:78-106`. |
| SK-V9 row movement | No SK-V9 measured row addition, no Apache/CITM measured typed rows, and no direct/parse row movement appears in current RESULTS. | absent by design | Apache/CITM are source/product parity only under REDRESS 91 at `skinny/REDRESS.md:2620-2659`; SK-V9 G-Alpha names typed row admission as a future candidate only at `restart/skinny/tranches/sk-v9/research/g-alpha/G-ALPHA-PRESENTATION.md:36-45`. |

## Section 5 - Sources

Primary current authority:

- `skinny/RESULTS.md:3-42` - current 26-column main measured table and 38 rows.
- `skinny/RESULTS.md:44-85` - current telemetry manifest.
- `skinny/RESULTS.md:138-141` - overall `N-direct / NoGo` and W0 telemetry note.
- `skinny/REDRESS.md:2620-2729` - REDRESS 91, 92, and 93 route boundaries.

Process and pass authority:

- `restart/prompts/skinny/PASS-1-PROFILE.md:50-65` - P1-F role and source requirements.
- `restart/skinny/tranches/sk-v9/research/alpha-hardening/V4/CONSOLIDATED.md:35-42` - Alpha convergence for G-Alpha presentation.
- `restart/skinny/tranches/sk-v9/research/g-alpha/G-ALPHA-PRESENTATION.md:7-34` - G-Alpha recommendation and current close-state summary.
- `restart/skinny/tranches/sk-v9/SYNTHESIS.md:21-57` - SK-V9 opening state, W6 residual candidates, and gate-only prerequisites.
- `restart/skinny/tranches/sk-v8/research/wave-6-hardening/V2/HARDENING-W6-V2-CONSOLIDATED.md:44-50` - SK-V8 W6 no-RESULTS/no-REDRESS close.
- `restart/skinny/tranches/sk-v8/SPEC.md:103-146` - required telemetry and rejection conditions.
- `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:59-101` - field shapes and allowed values.
