# SK-V12 P1-D: PMU And Cycles-Per-Byte

Pass: S-P1 Profile. Cycle: V12 pin reprofile.
Date: 2026-05-20.
Scope: fresh PMU counters, Mbps, CPI, and cycles-per-byte for capture source
commit `cf7848b2` JSON parse, direct, and typed guard lanes; CSS L4 target
coverage is explicitly missing.
Output: this file.
Baseline: SK-V12 pin-aware G-Alpha capture source commit `cf7848b2`.
Host triple: `aarch64-apple-darwin; arch=arm64; cpu=Apple M5 Max`.
Build flags: `RUSTFLAGS=-C target-cpu=native`, release profile, debug symbols inherited from parent replay target.
Profile tool: PMU TSVs emitted from parent-owned `xctrace_probe` and `profile_direct` replays using `proc_pid_rusage(RUSAGE_INFO_V5)` cycles/instructions.
Corpus coverage: JSON parse Track 1/Track 2 17/17; JSON direct Track 1/Track 2 17/17; JSON typed guards Track 1/Track 2 7/7; CSS L4 target 0/1.

Shared capture provenance:

- Capture root: `/tmp/skv12-pin-p1`.
- PMU row authority: `/tmp/skv12-pin-p1/pmu/parse_pmu_rows.tsv` and `/tmp/skv12-pin-p1/pmu/product_pmu_rows.tsv`.
- Capture status: `/tmp/skv12-pin-p1/pmu/capture_status.tsv`.
- Command ledger: `/tmp/skv12-pin-p1/pmu/pmu-commands.sh`.
- Done marker: `/tmp/skv12-pin-p1/pmu/done.txt` = `2026-05-20T18:05:34Z`.
- Target directory: `/tmp/skv12-pin-profile-target-cf7848b2`.
- Binary paths: `/tmp/skv12-pin-profile-target-cf7848b2/release/xctrace_probe` and `/tmp/skv12-pin-profile-target-cf7848b2/release/profile_direct`.
- Toolchain observed by P1-D: `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`, LLVM `22.1.2`.
- Host OS observed by P1-D: macOS `26.4.1` build `25E253`, Darwin `25.4.0 arm64`.

## Section 1 - Method

P1-D did not run cargo, xctrace, or samply. The parent replay owned capture and
left the TSVs above. This artifact is a deterministic reduction of those TSVs:

- bytes = `iters * corpus_bytes`;
- aggregate Mbps = `sum(bytes) * 8 / sum(elapsed_s) / 1e6`;
- aggregate cycles/B = `sum(cycles) / sum(bytes)`;
- aggregate CPI = `sum(cycles) / sum(instructions)`;
- aggregate IPC = `sum(instructions) / sum(cycles)`.

Readable replay command shapes from `pmu-commands.sh`:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
/tmp/skv12-pin-profile-target-cf7848b2/release/xctrace_probe \
  /Users/mkbabb/Programming/bbnf-lang/skinny/test_data/<corpus>.json \
  <track1-or-track2> <iters>

/tmp/skv12-pin-profile-target-cf7848b2/release/profile_direct \
  <iters> <corpus-or-update-center-alias> <track1-or-track2>

/tmp/skv12-pin-profile-target-cf7848b2/release/profile_direct \
  <iters> <corpus-or-update-center-alias> \
  <real_typed_track1-or-real_typed_track2>
```

Capture status:

| Lane | Rows | Status |
| --- | ---: | --- |
| `pmu-direct` | 34 | `PASS rc=0` |
| `pmu-parse` | 34 | `PASS rc=0` |
| `pmu-typed` | 14 | `PASS rc=0` |

Final companion capture status outside the PMU authority: samply has 82/82
PASS rows and xctrace has 212/212 PASS rows under `/tmp/skv12-pin-p1`. P1-D
does not derive cycles/B from those companion artifacts; it cites them only to
reconcile that the full S-P1 profile capture finished.

Absent coverage is binding and not synthesized:

| Coverage item | Status | Disposition |
| --- | --- | --- |
| branch misses | missing | No `branch_misses` or equivalent column in `parse_pmu_rows.tsv` or `product_pmu_rows.tsv`; not inferred. |
| L1 misses | missing | No L1 miss/load/store column in either PMU TSV; not inferred. |
| LLC misses | missing | No LLC/cache-last-level column in either PMU TSV; not inferred. |
| xctrace CPU Counters / Time Profiler | complete companion capture | The PMU row authority for this artifact is `parse_pmu_rows.tsv` + `product_pmu_rows.tsv`; xctrace bundles are retained for P1-A/P1-B/P1-E, not cycles/B arithmetic. |
| samply | complete companion capture | Samply is not part of the PMU TSVs and is owned by P1-A/P1-B/P1-C if separately captured; P1-D does not use samply rows for cycles/B. |
| CSS L4 target PMU row | missing | No generated CSS L4 runtime/fixture row exists in the PMU TSVs. Missing row authority remains `css_l4/declaration_values/{direct_to_struct|real_typed_struct}/main`, pending S-P3/W1 selection and W1b implementation. |

These values are profile evidence only. They do not move any row in
`skinny/RESULTS.md`, do not admit a direct or typed row, and do not create the
missing CSS L4 row.

## Section 2 - Findings

Aggregate values are weighted by captured bytes, cycles, instructions, and
elapsed time across rows in each workload family.

| Workload family | Rows | Corpora | PMU rc=0 | Aggregate Mbps | Aggregate cycles/B | Aggregate CPI | Aggregate IPC |
| --- | ---: | ---: | ---: | --- | --- | --- | --- |
| parse | 34 | 17 | 34 | 8669.019 | 2.971206 | 0.208405 | 4.798 |
| direct | 34 | 17 | 34 | 5773.975 | 4.411311 | 0.188854 | 5.295 |
| typed guards | 14 | 7 | 14 | 8959.011 | 3.137378 | 0.185866 | 5.380 |

Min, median, and max row values by workload family:

| Family | Metric | Min row | Min | Median | Max row | Max |
| --- | --- | --- | ---: | ---: | --- | ---: |
| parse | cycles/B | `citm_catalog/track1` | 1.117 | 2.792 | `y_string_unicode/track2` | 5.935 |
| parse | CPI | `canada/track1` | 0.121 | 0.214 | `unicode_mixed/track1` | 0.384 |
| parse | MBPS | `y_string_unicode/track2` | 4391.643 | 9268.390 | `citm_catalog/track1` | 22483.527 |
| direct | cycles/B | `citm_catalog/track1` | 1.627 | 3.727 | `y_string_unicode/track2` | 11.370 |
| direct | CPI | `canada/track1` | 0.115 | 0.192 | `unicode_mixed/track1` | 0.411 |
| direct | MBPS | `y_string_unicode/track2` | 2064.319 | 6871.764 | `citm_catalog/track2` | 15335.018 |
| typed guards | cycles/B | `citm_catalog/real_typed_track1` | 0.984 | 2.945 | `apache_builds/real_typed_track2` | 6.071 |
| typed guards | CPI | `citm_catalog/real_typed_track1` | 0.139 | 0.183 | `apache_builds/real_typed_track1` | 0.238 |
| typed guards | MBPS | `apache_builds/real_typed_track2` | 4424.251 | 8834.016 | `citm_catalog/real_typed_track1` | 23275.358 |

Track split summary:

| Family | Mode | Rows | Aggregate Mbps | Aggregate cycles/B | Aggregate CPI | Median cycles/B | Median CPI | Median Mbps |
| --- | ---: | ---: | --- | --- | --- | --- | --- | --- |
| parse | `track1` | 17 | 9308.370 | 2.776096 | 0.193508 | 2.686 | 0.197 | 9389.461 |
| parse | `track2` | 17 | 8111.851 | 3.166317 | 0.223490 | 2.845 | 0.240 | 8934.040 |
| direct | `track1` | 17 | 5969.729 | 4.285116 | 0.188035 | 3.808 | 0.191 | 6944.617 |
| direct | `track2` | 17 | 5590.652 | 4.537506 | 0.189633 | 3.603 | 0.193 | 6341.388 |
| typed guards | `real_typed_track1` | 7 | 9974.863 | 2.750416 | 0.175446 | 2.867 | 0.175 | 10325.712 |
| typed guards | `real_typed_track2` | 7 | 8130.946 | 3.524341 | 0.194899 | 3.399 | 0.216 | 8651.976 |

### Parse PMU rows

| Corpus | Mode | Mbps | cycles/B | CPI | cycles | instructions |
| --- | ---: | ---: | --- | --- | ---: | ---: |
| `twitter` | `track1` | 11331.782 | 2.245 | 0.200 | 8979783262 | 44987673497 |
| `twitter` | `track2` | 9368.936 | 2.862 | 0.264 | 11449662737 | 43331432149 |
| `citm_catalog` | `track1` | 22483.527 | 1.117 | 0.146 | 4470105333 | 30631086083 |
| `citm_catalog` | `track2` | 14880.233 | 1.666 | 0.210 | 6664292219 | 31804857850 |
| `canada` | `track1` | 12242.138 | 2.009 | 0.121 | 8037246188 | 66385044111 |
| `canada` | `track2` | 11492.206 | 2.090 | 0.127 | 8359390957 | 65784532315 |
| `apache_builds` | `track1` | 9270.515 | 2.755 | 0.216 | 11018633477 | 50968289662 |
| `apache_builds` | `track2` | 9079.080 | 2.845 | 0.241 | 11379494468 | 47226396165 |
| `github_events` | `track1` | 11820.946 | 2.315 | 0.212 | 9259697120 | 43725320552 |
| `github_events` | `track2` | 8832.828 | 2.680 | 0.256 | 10721059816 | 41907349924 |
| `update_center` | `track1` | 9258.339 | 2.907 | 0.202 | 11630595195 | 57637543345 |
| `update_center` | `track2` | 6784.238 | 3.761 | 0.272 | 15046868359 | 55406518877 |
| `mesh` | `track1` | 9266.265 | 2.649 | 0.133 | 10595953735 | 79388738727 |
| `mesh` | `track2` | 8522.801 | 2.794 | 0.140 | 11174183605 | 80027120308 |
| `random` | `track1` | 6184.217 | 3.644 | 0.193 | 14575366434 | 75634663083 |
| `random` | `track2` | 6247.977 | 4.426 | 0.240 | 17705830167 | 73745344553 |
| `gsoc-2018` | `track1` | 16642.690 | 1.525 | 0.225 | 6098376433 | 27089086199 |
| `gsoc-2018` | `track2` | 14949.855 | 1.591 | 0.243 | 6364422747 | 26162758752 |
| `marine_ik` | `track1` | 9573.793 | 2.686 | 0.147 | 10746572106 | 73309199721 |
| `marine_ik` | `track2` | 8934.040 | 2.781 | 0.152 | 11125201713 | 73382854293 |
| `instruments` | `track1` | 12415.798 | 2.057 | 0.162 | 8230028806 | 50839679225 |
| `instruments` | `track2` | 9337.860 | 2.941 | 0.232 | 11763396917 | 50750685221 |
| `numbers` | `track1` | 13514.851 | 1.816 | 0.144 | 7266089667 | 50539967971 |
| `numbers` | `track2` | 10005.747 | 1.959 | 0.153 | 7837803134 | 51097047560 |
| `unicode_mixed` | `track1` | 5851.791 | 4.571 | 0.384 | 18287029658 | 47561811685 |
| `unicode_mixed` | `track2` | 6788.599 | 4.110 | 0.356 | 16441899147 | 46165903656 |
| `unicode_escapes` | `track1` | 8671.757 | 3.039 | 0.234 | 12158379945 | 51960361217 |
| `unicode_escapes` | `track2` | 10187.882 | 2.790 | 0.215 | 11161938901 | 51833729773 |
| `unicode_basic` | `track1` | 9389.461 | 2.916 | 0.197 | 11665952325 | 59095124888 |
| `unicode_basic` | `track2` | 8231.577 | 3.207 | 0.217 | 12829909212 | 59014537728 |
| `distinct_values` | `track1` | 7255.305 | 3.630 | 0.194 | 14521964754 | 74828935697 |
| `distinct_values` | `track2` | 4595.925 | 5.693 | 0.301 | 22773284016 | 75720249663 |
| `y_string_unicode` | `track1` | 4934.259 | 5.624 | 0.236 | 20021600233 | 84695563618 |
| `y_string_unicode` | `track2` | 4391.643 | 5.935 | 0.252 | 21129509331 | 83856214168 |

### Direct PMU rows

| Corpus | Mode | Mbps | cycles/B | CPI | cycles | instructions |
| --- | ---: | ---: | --- | --- | ---: | ---: |
| `twitter` | `track1` | 8269.176 | 2.978 | 0.214 | 11910879264 | 55585237428 |
| `twitter` | `track2` | 8361.634 | 3.218 | 0.207 | 12871649486 | 62259246976 |
| `citm_catalog` | `track1` | 15060.968 | 1.627 | 0.158 | 6507735490 | 41182659488 |
| `citm_catalog` | `track2` | 15335.018 | 1.718 | 0.160 | 6874182142 | 42842783546 |
| `canada` | `track1` | 8146.067 | 3.256 | 0.115 | 13025829660 | 113408704446 |
| `canada` | `track2` | 7084.015 | 3.387 | 0.121 | 13549529191 | 111807729023 |
| `apache_builds` | `track1` | 9395.709 | 3.076 | 0.208 | 12302960637 | 59121939057 |
| `apache_builds` | `track2` | 8179.309 | 3.350 | 0.205 | 13399650875 | 65511763120 |
| `github_events` | `track1` | 9983.765 | 2.834 | 0.215 | 11334585036 | 52662259458 |
| `github_events` | `track2` | 9428.485 | 3.075 | 0.212 | 12301133023 | 57917914427 |
| `update_center` | `track1` | 5721.474 | 4.167 | 0.230 | 16668605993 | 72560144182 |
| `update_center` | `track2` | 5391.019 | 4.734 | 0.231 | 18936801925 | 81842788659 |
| `mesh` | `track1` | 6944.617 | 3.995 | 0.121 | 15978717829 | 132373996875 |
| `mesh` | `track2` | 6341.388 | 3.937 | 0.123 | 15747931730 | 128449022906 |
| `random` | `track1` | 5457.127 | 4.501 | 0.188 | 18006197032 | 95809788070 |
| `random` | `track2` | 4680.853 | 4.929 | 0.188 | 19716394627 | 105088454665 |
| `gsoc-2018` | `track1` | 12256.151 | 2.352 | 0.242 | 9409487675 | 38856393533 |
| `gsoc-2018` | `track2` | 11505.602 | 2.447 | 0.241 | 9787175940 | 40666750443 |
| `marine_ik` | `track1` | 6966.353 | 3.808 | 0.130 | 15235236051 | 117269966546 |
| `marine_ik` | `track2` | 7467.298 | 3.603 | 0.125 | 14414234769 | 115553528885 |
| `instruments` | `track1` | 9602.372 | 2.880 | 0.179 | 11521765036 | 64496955865 |
| `instruments` | `track2` | 9145.521 | 3.112 | 0.178 | 12449923303 | 69906504795 |
| `numbers` | `track1` | 4983.514 | 3.647 | 0.165 | 14586522890 | 88486220397 |
| `numbers` | `track2` | 6026.171 | 3.184 | 0.139 | 12735681462 | 91338712919 |
| `unicode_mixed` | `track1` | 3349.465 | 7.904 | 0.411 | 31623009460 | 77010270839 |
| `unicode_mixed` | `track2` | 3225.563 | 8.095 | 0.406 | 32386814293 | 79812165834 |
| `unicode_escapes` | `track1` | 3683.009 | 7.133 | 0.248 | 28535323191 | 115275140020 |
| `unicode_escapes` | `track2` | 3429.353 | 7.286 | 0.252 | 29148291792 | 115824366756 |
| `unicode_basic` | `track1` | 6798.911 | 3.832 | 0.191 | 15327713589 | 80312691786 |
| `unicode_basic` | `track2` | 6262.027 | 4.230 | 0.193 | 16921740429 | 87766336406 |
| `distinct_values` | `track1` | 4699.561 | 5.477 | 0.239 | 21910242157 | 91531624917 |
| `distinct_values` | `track2` | 4516.395 | 6.213 | 0.236 | 24851260249 | 105424846013 |
| `y_string_unicode` | `track1` | 2538.011 | 10.009 | 0.146 | 35633582204 | 243757573912 |
| `y_string_unicode` | `track2` | 2064.319 | 11.370 | 0.159 | 40478381138 | 254637744771 |

### Typed guard PMU rows

| Corpus | Mode | Mbps | cycles/B | CPI | cycles | instructions |
| --- | ---: | ---: | --- | --- | ---: | ---: |
| `twitter` | `real_typed_track1` | 16721.813 | 1.855 | 0.175 | 7418716843 | 42428619970 |
| `twitter` | `real_typed_track2` | 14958.657 | 2.132 | 0.223 | 8526094272 | 38237000042 |
| `citm_catalog` | `real_typed_track1` | 23275.358 | 0.984 | 0.139 | 3936469666 | 28386197937 |
| `citm_catalog` | `real_typed_track2` | 15162.193 | 1.820 | 0.155 | 7281006182 | 47073835240 |
| `apache_builds` | `real_typed_track1` | 6652.316 | 4.066 | 0.238 | 16265753412 | 68322408519 |
| `apache_builds` | `real_typed_track2` | 4424.251 | 6.071 | 0.216 | 24286162414 | 112364979375 |
| `github_events` | `real_typed_track1` | 10325.712 | 2.810 | 0.216 | 11239637171 | 51969577430 |
| `github_events` | `real_typed_track2` | 9016.055 | 3.002 | 0.226 | 12009650295 | 53212567258 |
| `update_center` | `real_typed_track1` | 10936.245 | 2.867 | 0.191 | 11468245466 | 60161539098 |
| `update_center` | `real_typed_track2` | 8651.976 | 3.538 | 0.226 | 14153325237 | 62655972284 |
| `mesh` | `real_typed_track1` | 7645.301 | 3.783 | 0.141 | 15132146908 | 107520946635 |
| `mesh` | `real_typed_track2` | 6540.410 | 4.708 | 0.164 | 18830987320 | 114563253689 |
| `marine_ik` | `real_typed_track1` | 7716.854 | 2.888 | 0.144 | 11555263274 | 80184784831 |
| `marine_ik` | `real_typed_track2` | 8150.748 | 3.399 | 0.174 | 13600201280 | 78242738747 |

## Section 3 - Delta vs Prior PMU Surface

The prior pre-pin SK-V12 P1-D artifact was captured at commit `50bd1648`
under `/tmp/skv12-p1`. Capture source commit `cf7848b2` was reprofiled under
the user pin. The fresh aggregate c/B is slightly higher on all three JSON
families; this is profile drift, not row movement.

| Family | Fresh c/B | Prior pre-pin c/B | c/B delta | Fresh CPI | Prior pre-pin CPI | CPI delta |
| --- | --- | --- | --- | --- | --- | --- |
| parse | 2.971206 | 2.920217 | +1.7% | 0.208405 | 0.204887 | +1.7% |
| direct | 4.411311 | 4.290305 | +2.8% | 0.188854 | 0.183717 | +2.8% |
| typed guards | 3.137378 | 3.123172 | +0.5% | 0.185866 | 0.185056 | +0.4% |

SK-V11 row-disposition state remains governed by `skinny/RESULTS.md` and
`skinny/REDRESS.md` through REDRESS 120. This P1-D pass records
capture-source cycles/instructions only; it does not reopen parse-only SOTA and
does not claim CSS L4 coverage.

## Section 4 - Anomalies And Masking Signals

Low and high cycles/B rows:

| Family | Low c/B rows | High c/B rows |
| --- | --- | --- |
| parse | `citm_catalog` track1 1.117; `gsoc-2018` track1 1.525; `gsoc-2018` track2 1.591; `citm_catalog` track2 1.666; `numbers` track1 1.816 | `y_string_unicode` track2 5.935; `distinct_values` track2 5.693; `y_string_unicode` track1 5.624; `unicode_mixed` track1 4.571; `random` track2 4.426 |
| direct | `citm_catalog` track1 1.627; `citm_catalog` track2 1.718; `gsoc-2018` track1 2.352; `gsoc-2018` track2 2.447; `github_events` track1 2.834 | `y_string_unicode` track2 11.370; `y_string_unicode` track1 10.009; `unicode_mixed` track2 8.095; `unicode_mixed` track1 7.904; `unicode_escapes` track2 7.286 |
| typed guards | `citm_catalog` real_typed_track1 0.984; `citm_catalog` real_typed_track2 1.820; `twitter` real_typed_track1 1.855; `twitter` real_typed_track2 2.132; `github_events` real_typed_track1 2.810 | `apache_builds` real_typed_track2 6.071; `mesh` real_typed_track2 4.708; `apache_builds` real_typed_track1 4.066; `mesh` real_typed_track1 3.783; `update_center` real_typed_track2 3.538 |

Notable outliers and interpretation:

- `y_string_unicode/direct` remains the highest instruction-volume row pair:
  10.009 c/B on Track 1 and 11.370 c/B on Track 2, with CPI 0.146 and 0.159.
  The pressure is bytes of work per input byte, not a simple stall signature.
- `unicode_mixed` is the highest-CPI family in this capture: parse Track 1
  0.384 CPI, direct Track 1 0.411 CPI, and direct Track 2 0.391 CPI. It remains
  the strongest PMU hint for Unicode decode/classification cost, but this
  artifact does not propose a route.
- `unicode_escapes/direct` is high c/B on both tracks, 7.133 and 7.286, while
  parse stays near 2.8-3.0 c/B. That split keeps escape-heavy direct output work in
  the S-P2 search space, subject to Lock 14/16 and the CSS L4 pin.
- Numeric-heavy `canada`, `mesh`, `marine_ik`, and `numbers` retain low
  CPI in direct mode. Low CPI means the wide core is issuing efficiently; a
  route still needs micro-proof rather than assuming PMU stalls.
- The CSS L4 target has no PMU row because no generated CSS L4 runtime/fixture
  row exists in this capture. Under the user pin, S-P2/S-P3 may use this JSON
  PMU data only as nomination evidence; CSS L4 still requires its own measured
  row before admission.

Masking-probe status: this pin-era PMU replay did not include fresh
`host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`, or
structural-scan-only rows. Any Mode III claim must come from P1-C or a later
fresh capture; P1-D does not infer it here.

## Section 5 - Sources

- `restart/prompts/skinny/PASS-1-PROFILE.md`.
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`.
- `restart/skinny/tranches/sk-v12/HANDOFF.md`.
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md` through REDRESS 120.
- `/tmp/skv12-pin-p1/pmu/capture_status.tsv`: 82 data rows, all `PASS rc=0`.
- `/tmp/skv12-pin-p1/pmu/parse_pmu_rows.tsv`: 34 data rows.
- `/tmp/skv12-pin-p1/pmu/product_pmu_rows.tsv`: 48 data rows.
- `/tmp/skv12-pin-p1/pmu/pmu-commands.sh`: parent replay command ledger.
- `/tmp/skv12-pin-p1/pmu/done.txt`: `2026-05-20T18:05:34Z`.
