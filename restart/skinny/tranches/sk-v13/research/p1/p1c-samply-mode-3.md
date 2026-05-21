# SK-V13 P1-C: Samply Mode III Masking + Structural Probe Profile

Pass: S-P1 Profile. Cycle: V13.
Date: 2026-05-21.
Scope: samply profiling for masking-probe workloads (`host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`) and the structural-scan-only path.
Output: this file.
Baseline: SK-V13-open capture identity `/tmp/skv13-p1/artifacts/identity.txt` commit `f8be692068e9e464b6ed24027ab26edfd05303fd`; current checkout while writing was `1f06f3847d0c1409837a7e9f5944698323ec296f`.
Host triple: `aarch64-apple-darwin`; rustc host `aarch64-apple-darwin`; CPU per checked results metadata: Apple M5 Max.
Build flags: release profile + debug=true profile expectation from S-P1; capture binary root `/tmp/skv13-profile-target-0a7b41c5/release`; bench metadata uses `RUSTFLAGS="-C target-cpu=native"`.
Profile tool: samply `0.13.1`; adjacent PMU source `/tmp/skv13-p1/pmu/pmu_rows.tsv`; no dedicated mode III samply capture found.
Corpus coverage: 0/17 for P1-C mode III samply; adjacent parse/direct samply and PMU cover 17/17, typed covers 7/17. Reason: `/tmp/skv13-p1/samply/capture_status.tsv` has only `parse`, `direct`, and `typed` lanes, and no host/eager/alternate/cold/structural/scan profile files exist under `/tmp/skv13-p1`.

## §1 — Method (commands run; verbatim, reproducible)

Read authority and scope:

```sh
sed -n '1,420p' restart/prompts/skinny/PASS-1-PROFILE.md
sed -n '1,240p' skinny/RESULTS.md
sed -n '1,240p' skinny/REDRESS.md
sed -n '1,240p' restart/skinny/tranches/sk-v13/HANDOFF.md
sed -n '1,260p' restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md
sed -n '1,220p' restart/skinny/tranches/sk-v13/SYNTHESIS.md
```

Inventory the fresh capture:

```sh
cat /tmp/skv13-p1/artifacts/identity.txt
sed -n '1,220p' /tmp/skv13-p1/samply/capture_status.tsv
sed -n '1,220p' /tmp/skv13-p1/pmu/pmu_rows.tsv
awk -F '\t' 'NR==1{next}{c[$1]++; m[$1"/"$3]++} END{for(k in c) print k,c[k]; print "-- modes --"; for(k in m) print k,m[k]}' /tmp/skv13-p1/samply/capture_status.tsv | sort
awk -F '\t' 'NR==1{next}{c[$1]++; m[$1"/"$3]++} END{for(k in c) print k,c[k]; print "-- modes --"; for(k in m) print k,m[k]}' /tmp/skv13-p1/pmu/pmu_rows.tsv | sort
find /tmp/skv13-p1 -maxdepth 3 -type f \( -name '*host*' -o -name '*eager*' -o -name '*alternate*' -o -name '*cold*' -o -name '*structural*' -o -name '*scan*' \) | sort
rustc -vV
samply --version
```

Inspect mode III source surfaces:

```sh
rg -n "host_call|eager_decode|alternate_scalar|cold_first|structural|MASKING|masking|scan_json_structurals|scan_json_parse_index" skinny/crates skinny/RESULTS.md skinny/REDRESS.md restart/skinny/tranches/sk-v13 -g '!target'
nl -ba skinny/crates/bbnf-bench/src/bin/gate.rs | sed -n '2140,2220p'
nl -ba skinny/crates/bbnf-bench/src/probes.rs | sed -n '1,90p'
nl -ba skinny/crates/runtime/src/grammars/json/scan.rs | sed -n '1,75p'
rg -n "structural scan|host_call_eager_decode|alternate_scalar_plan|cold_first_parse|host_call_dispatch_overhead|alternate_pext_mask_plan|MASKING" skinny/RESULTS.md
```

Result of the negative inventory: the samply summary contains `parse 34`, `direct 34`, and `typed 14`; PMU contains `parse 34`, `direct 68`, and `typed 28`; the mode III file search returned no paths.

## §2 — Findings (per-corpus table; file:line on every hot-leaf claim)

No P1-C hot-leaf claim is made in this artifact. The required mode III profiles are absent, so any symbol self-time or file:line attribution for `host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`, or structural-scan-only would be invented. The only source file:line facts below identify where the probe rows and structural path are defined:

| Surface | Source inspection finding |
|---|---|
| Masking probe row emission | `skinny/crates/bbnf-bench/src/bin/gate.rs:2143` builds `json_probes_{corpus}` groups; `gate.rs:2144`-`2150` enumerates `host_call_dispatch_overhead`, `host_call_eager_decode`, `alternate_scalar_plan`, `alternate_dispatch_table_plan`, `alternate_pext_mask_plan`, and `cold_first_parse`. |
| `host_call_eager_decode` masking threshold | `skinny/crates/bbnf-bench/src/bin/gate.rs:2197`-`2210` marks `MASKING` when eager decode exceeds the per-corpus Track 1 ratio. |
| Probe registry | `skinny/crates/bbnf-bench/src/probes.rs:30`-`45` configures the six masking probes as pending; `probes.rs:48`-`56` gives default thresholds. |
| Structural-scan-only source | `skinny/crates/runtime/src/grammars/json/scan.rs:22`-`30` exposes `scan_structurals`; `scan.rs:32`-`35` exposes `scan_structurals_scalar`; `scan.rs:47`-`53` routes capacity plans including `OneShotSimd`. |
| Current checked result | `skinny/RESULTS.md:106` reports only `canada structural scan: 41758 Mbps; floor is 40000 Mbps`; it is a result note, not a samply mode III capture. |

Adjacent evidence exists for parse/direct/typed capture, but it is not mode III:

| Corpus | Adjacent samply parse Track 1 | Adjacent samply direct Track 1 | Adjacent samply typed Track 1 | PMU parse T1 c/B | PMU direct T1 c/B | PMU typed T1 c/B | P1-C mode III status |
|---|---|---|---|---:|---:|---:|---|
| twitter | `/tmp/skv13-p1/samply/profiles/parse__twitter__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/direct__twitter__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/typed__twitter__real_typed_track1.json.gz` | 2.256353 | 2.979732 | 1.860272 | missing |
| citm_catalog | `/tmp/skv13-p1/samply/profiles/parse__citm_catalog__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/direct__citm_catalog__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/typed__citm_catalog__real_typed_track1.json.gz` | 1.135500 | 1.615142 | 0.971973 | missing |
| canada | `/tmp/skv13-p1/samply/profiles/parse__canada__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/direct__canada__track1.json.gz` | missing | 1.941305 | 3.252040 | missing | missing; only checked-result structural note exists |
| apache_builds | `/tmp/skv13-p1/samply/profiles/parse__apache_builds__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/direct__apache_builds__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/typed__apache_builds__real_typed_track1.json.gz` | 2.821539 | 3.085860 | 4.024964 | missing |
| github_events | `/tmp/skv13-p1/samply/profiles/parse__github_events__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/direct__github_events__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/typed__github_events__real_typed_track1.json.gz` | 2.406529 | 2.839007 | 2.735252 | missing |
| update_center | `/tmp/skv13-p1/samply/profiles/parse__update_center__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/direct__update_center__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/typed__update_center__real_typed_track1.json.gz` | 3.058022 | 4.142268 | 2.834568 | missing |
| mesh | `/tmp/skv13-p1/samply/profiles/parse__mesh__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/direct__mesh__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/typed__mesh__real_typed_track1.json.gz` | 2.632188 | 3.889578 | 3.794074 | missing |
| random | `/tmp/skv13-p1/samply/profiles/parse__random__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/direct__random__track1.json.gz` | missing | 3.481770 | 4.436292 | missing | missing |
| gsoc-2018 | `/tmp/skv13-p1/samply/profiles/parse__gsoc-2018__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/direct__gsoc-2018__track1.json.gz` | missing | 1.599432 | 2.919522 | missing | missing |
| marine_ik | `/tmp/skv13-p1/samply/profiles/parse__marine_ik__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/direct__marine_ik__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/typed__marine_ik__real_typed_track1.json.gz` | 2.634606 | 3.683827 | 2.854909 | missing |
| instruments | `/tmp/skv13-p1/samply/profiles/parse__instruments__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/direct__instruments__track1.json.gz` | missing | 2.014174 | 2.870026 | missing | missing |
| numbers | `/tmp/skv13-p1/samply/profiles/parse__numbers__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/direct__numbers__track1.json.gz` | missing | 1.867778 | 2.775024 | missing | missing |
| unicode_mixed | `/tmp/skv13-p1/samply/profiles/parse__unicode_mixed__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/direct__unicode_mixed__track1.json.gz` | missing | 4.711000 | 7.536545 | missing | missing |
| unicode_escapes | `/tmp/skv13-p1/samply/profiles/parse__unicode_escapes__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/direct__unicode_escapes__track1.json.gz` | missing | 3.264229 | 6.833959 | missing | missing |
| unicode_basic | `/tmp/skv13-p1/samply/profiles/parse__unicode_basic__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/direct__unicode_basic__track1.json.gz` | missing | 2.919726 | 3.789464 | missing | missing |
| distinct_values | `/tmp/skv13-p1/samply/profiles/parse__distinct_values__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/direct__distinct_values__track1.json.gz` | missing | 3.664440 | 5.512909 | missing | missing |
| y_string_unicode | `/tmp/skv13-p1/samply/profiles/parse__y_string_unicode__track1.json.gz` | `/tmp/skv13-p1/samply/profiles/direct__y_string_unicode__track1.json.gz` | missing | 5.674081 | 10.621480 | missing | missing |

## §3 — Delta vs SK-V12 (per row; Mbps + c/B + classification)

Mode III delta cannot be computed. There are no fresh mode III samply or PMU rows for `host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`, or structural-scan-only in `/tmp/skv13-p1`, and SK-V12 did not provide a comparable mode III samply corpus matrix. Classify the P1-C row family as `S-P1 V1 gap / CH1-CH6 risk`, not as a measured SK-V13 delta.

The adjacent PMU data in `/tmp/skv13-p1/pmu/pmu_rows.tsv` is useful for P1-A/P1-B/P1-D cross-checking, but it must not be promoted into P1-C coverage because the lane names are only `parse`, `direct`, and `typed`.

## §4 — Anomalies + masking signals (flagged for S-P2)

| Required P1-C probe | Current evidence | Missing fold-cycle capture | CH risk |
|---|---|---|---|
| `host_call_eager_decode` | Source-defined and report-classified as a masking probe; no `/tmp/skv13-p1` samply/PMU profile path found. | Capture 17/17 corpus samply profiles and PMU rows for `json_probes_{corpus}/host_call_eager_decode`, with self-time symbols and file:line attribution. | CH1 rejects missing corpus coverage; CH4 rejects non-reproducible profile rows; CH6 rejects paper-close. |
| `alternate_scalar_plan` | Source-defined; no fresh profile path found. | Capture 17/17 corpus samply profiles and PMU rows for `json_probes_{corpus}/alternate_scalar_plan`, and compare against canonical Track 1. | CH1/CH6 gap; CH3 must check any later scalar-plan interpretation against REDRESS rejected routes. |
| `cold_first_parse` | Source-defined; no fresh profile path found. | Capture 17/17 corpus samply profiles and PMU rows for `json_probes_{corpus}/cold_first_parse`, with first-parse allocation/cache effects separated from warmed loops. | CH4/CH6 gap; S-P1 cannot certify cold sensitivity. |
| Structural-scan-only path | `skinny/RESULTS.md` has only the Canada note `41758 Mbps`; no samply profile path under `/tmp/skv13-p1`. | Capture 17/17 corpus samply profiles and PMU rows for the structural-scan-only executable/path, including Canada `simd_structural_scan/canada_simd` and equivalent rows for the other 16 corpora. | CH1 corpus gap; CH5 Lock 1 risk if structural scan is treated as a separable sidecar instead of substrate evidence. |
| `host_call_dispatch_overhead` | Not in P1-C's named three masking probes, but source registry includes it. | Fold-cycle P1-C/P1-D should either capture it with the masking matrix or explicitly route ownership elsewhere. | CH6 risk if RESULTS claims probe coverage without artifact paths. |
| `alternate_pext_mask_plan` | Source registry includes it; no capture path found. | Fold-cycle should capture or mark unsupported on aarch64 with explicit reason and no stale row promotion. | CH4/CH6 risk. |
| `alternate_dispatch_table_plan` | Source marks it invalid duplicate-probe disabled after a real function-pointer table regressed. | Do not capture as valid coverage unless a distinct implementation is restored; preserve invalid status. | CH3 regression risk if reopened without REDRESS context. |

P1-C V1 finding: the capture is strong for adjacent parse/direct/typed profiling, but the mode III work requested by S-P1 §2 is absent. This must fold as a V1 gap before S-P1 convergence; otherwise P1-C is a CH1/CH4/CH5/CH6 paper-close risk.

## §5 — Sources (every artefact path + run id)

| Source | Evidence used |
|---|---|
| `/tmp/skv13-p1/artifacts/identity.txt` | Capture root `/tmp/skv13-p1`, binary root `/tmp/skv13-profile-target-0a7b41c5/release`, commit `f8be692068e9e464b6ed24027ab26edfd05303fd`, date `2026-05-21T06:01:45Z`. |
| `/tmp/skv13-p1/samply/capture_status.tsv` | Adjacent samply inventory: parse 17x2, direct 17x2, typed 7x2; no mode III lanes. |
| `/tmp/skv13-p1/pmu/pmu_rows.tsv` | Adjacent PMU inventory and c/B table; no mode III lanes. |
| `/tmp/skv13-p1/samply/profiles/*.json.gz` | Adjacent parse/direct/typed profile files cited per corpus above. |
| `/tmp/skv13-p1/pmu/logs/*.log` | Adjacent parse/direct/typed PMU logs; not P1-C coverage. |
| `skinny/RESULTS.md` | Current checked result authority; Canada structural scan note only. |
| `skinny/REDRESS.md` | Rejected-route and masking-probe context, including prior statement that masking probes are report artifacts. |
| `restart/prompts/skinny/PASS-1-PROFILE.md` | S-P1 schema and P1-C scope. |
| `restart/skinny/tranches/sk-v13/HANDOFF.md` | SK-V13 sequencing and no-source-edit constraint before G-Omega. |
| `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md` | Fresh S-P1 capture requirement and profile staleness framing. |
| `restart/skinny/tranches/sk-v13/SYNTHESIS.md` | G5 full JSON row obligation and telemetry binding. |
