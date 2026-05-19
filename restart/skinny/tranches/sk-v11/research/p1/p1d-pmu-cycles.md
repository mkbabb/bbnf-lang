# SK-V11 P1-D: PMU And Cycles-Per-Byte

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-19.
Scope: fresh PMU counters and cycles-per-byte for parse, direct, and typed
guard lanes.
Output: this file.
Baseline: SK-V11-open commit `3ce75df4`, run id
`sk-v9-open:criterion-fnv64-c8d7e0468358f98c`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `RUSTFLAGS=-C target-cpu=native`, release profile, debug symbols.
Profile tool: `proc_pid_rusage(RUSAGE_INFO_V5)` counters emitted by
`xctrace_probe` and `profile_direct`, plus retained `xcrun xctrace record
--template "CPU Counters"` trace bundles.
Corpus coverage: parse Track 1/Track 2 17/17; direct Track 1/Track 2 17/17;
typed guard Track 1/Track 2 7/7.

## Section 1 - Method

Commands, parameterized by corpus and track:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv11-profile-target-9c8da194 \
  RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin xctrace_probe --bin profile_direct

/tmp/skv11-profile-target-9c8da194/release/xctrace_probe \
  /Users/mkbabb/Programming/bbnf-lang/skinny/test_data/<corpus>.json \
  <track1-or-track2> 400 \
  > /tmp/skv11-p1/parse-xctrace/logs/<corpus>__<track>.pmu.log 2>&1

/tmp/skv11-profile-target-9c8da194/release/profile_direct \
  400 <corpus> <track1-or-track2> \
  > /tmp/skv11-p1/direct-xctrace/logs/<corpus>__<track>.pmu.log 2>&1

/tmp/skv11-profile-target-9c8da194/release/profile_direct \
  400 <corpus> <real_typed_track1-or-real_typed_track2> \
  > /tmp/skv11-p1/direct-xctrace/logs/<corpus>__<mode>.pmu.log 2>&1

xcrun xctrace record \
  --template "CPU Counters" \
  --time-limit 1s \
  --no-prompt \
  --output /tmp/skv11-p1/<parse-or-direct>-xctrace/cpu-counters/<corpus>__<mode>.trace \
  --launch -- \
  /tmp/skv11-profile-target-9c8da194/release/<xctrace_probe-or-profile_direct> \
  <arguments>
```

`xctrace_probe` and `profile_direct` read `ri_cycles` and `ri_instructions`
before and after the timed loop and emit one `PROBE_RESULT` line per row.
The PMU row files are the numeric authority for cycles/B and CPI:
`/tmp/skv11-p1/pmu/parse_pmu_rows.tsv` and
`/tmp/skv11-p1/pmu/product_pmu_rows.tsv`. `capture_status.tsv` is the
coverage and return-code ledger. The Mbps values below come from the PMU timed
loops and are profiling evidence only; row admission still belongs to
`skinny/RESULTS.md` and `gate-json`.

## Section 2 - Findings

Coverage and integrity:

| Plane | Rows | PMU rc=0 | CPU Counter trace dirs | Aggregate cycles/B | Aggregate CPI | Aggregate IPC |
|---|---:|---:|---:|---:|---:|---:|
| parse | 34 | 34 | 34 | 2.777033 | 0.211017 | 4.739 |
| direct | 34 | 34 | 34 | 4.428342 | 0.211681 | 4.724 |
| typed guards | 14 | 14 | 14 | 3.190644 | 0.190381 | 5.253 |

### Parse PMU rows

| Corpus | Track | Mbps | cycles/B | CPI | cycles | instructions | Trace |
|---|---|---:|---:|---:|---:|---:|---|
| `twitter` | Track 1 | 3842.307 | 2.743204 | 0.243418 | 692949873 | 2846745319 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/twitter__track1.trace` |
| `twitter` | Track 2 | 3187.530 | 3.211296 | 0.295223 | 811192691 | 2747732331 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/twitter__track2.trace` |
| `citm_catalog` | Track 1 | 7738.921 | 1.331740 | 0.173676 | 920074742 | 5297659925 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/citm_catalog__track1.trace` |
| `citm_catalog` | Track 2 | 5623.865 | 1.840762 | 0.231148 | 1271748669 | 5501872518 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/citm_catalog__track2.trace` |
| `canada` | Track 1 | 9582.962 | 1.967545 | 0.118543 | 1771617516 | 14944978599 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/canada__track1.trace` |
| `canada` | Track 2 | 9975.771 | 2.061187 | 0.125324 | 1855934690 | 14809147603 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/canada__track2.trace` |
| `apache_builds` | Track 1 | 3117.064 | 3.265414 | 0.254984 | 166242209 | 651970062 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/apache_builds__track1.trace` |
| `apache_builds` | Track 2 | 3118.576 | 3.320362 | 0.280014 | 169039613 | 603683008 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/apache_builds__track2.trace` |
| `github_events` | Track 1 | 6397.373 | 2.431678 | 0.221840 | 63352011 | 285575637 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/github_events__track1.trace` |
| `github_events` | Track 2 | 5720.638 | 2.734909 | 0.260708 | 71252027 | 273301563 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/github_events__track2.trace` |
| `update_center` | Track 1 | 5291.103 | 2.996409 | 0.207846 | 639047676 | 3074618768 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/update_center__track1.trace` |
| `update_center` | Track 2 | 4269.796 | 3.878651 | 0.279635 | 827204511 | 2958154024 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/update_center__track2.trace` |
| `mesh` | Track 1 | 3185.280 | 3.242468 | 0.163138 | 938496103 | 5752757389 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/mesh__track1.trace` |
| `mesh` | Track 2 | 3140.854 | 3.303866 | 0.164963 | 956266897 | 5796873531 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/mesh__track2.trace` |
| `random` | Track 1 | 2470.373 | 4.086989 | 0.215714 | 834523840 | 3868652828 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/random__track1.trace` |
| `random` | Track 2 | 2113.691 | 4.827105 | 0.261003 | 985648491 | 3776387324 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/random__track2.trace` |
| `gsoc-2018` | Track 1 | 6201.100 | 1.918167 | 0.282743 | 2553333841 | 9030570028 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/gsoc-2018__track1.trace` |
| `gsoc-2018` | Track 2 | 6533.291 | 1.959541 | 0.299088 | 2608408583 | 8721201206 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/gsoc-2018__track2.trace` |
| `marine_ik` | Track 1 | 3310.265 | 3.139744 | 0.171147 | 3746927292 | 21893040831 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/marine_ik__track1.trace` |
| `marine_ik` | Track 2 | 3155.965 | 3.248424 | 0.176844 | 3876625005 | 21921204975 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/marine_ik__track2.trace` |
| `instruments` | Track 1 | 4279.725 | 2.482847 | 0.195132 | 218834198 | 1121465547 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/instruments__track1.trace` |
| `instruments` | Track 2 | 3385.878 | 3.130124 | 0.246388 | 275884101 | 1119713543 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/instruments__track2.trace` |
| `numbers` | Track 1 | 5439.881 | 2.267104 | 0.179272 | 136138678 | 759397082 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/numbers__track1.trace` |
| `numbers` | Track 2 | 4625.345 | 2.318250 | 0.181376 | 139209964 | 767522641 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/numbers__track2.trace` |
| `unicode_mixed` | Track 1 | 2449.258 | 5.283244 | 0.444075 | 2225484120 | 5011504532 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/unicode_mixed__track1.trace` |
| `unicode_mixed` | Track 2 | 2806.778 | 5.226170 | 0.452372 | 2201442386 | 4866444714 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/unicode_mixed__track2.trace` |
| `unicode_escapes` | Track 1 | 10453.399 | 3.194394 | 0.245931 | 1342663667 | 5459509015 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/unicode_escapes__track1.trace` |
| `unicode_escapes` | Track 2 | 11743.438 | 2.880196 | 0.222286 | 1210600583 | 5446132093 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/unicode_escapes__track2.trace` |
| `unicode_basic` | Track 1 | 11567.257 | 2.910839 | 0.197010 | 1220906001 | 6197168833 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/unicode_basic__track1.trace` |
| `unicode_basic` | Track 2 | 9565.681 | 3.313254 | 0.224156 | 1389692709 | 6199664381 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/unicode_basic__track2.trace` |
| `distinct_values` | Track 1 | 9314.445 | 3.597468 | 0.192254 | 221071583 | 1149890485 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/distinct_values__track1.trace` |
| `distinct_values` | Track 2 | 5910.217 | 5.707204 | 0.301440 | 350719123 | 1163478007 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/distinct_values__track2.trace` |
| `y_string_unicode` | Track 1 | 5639.646 | 6.021943 | 0.252882 | 85754883 | 339109602 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/y_string_unicode__track1.trace` |
| `y_string_unicode` | Track 2 | 5621.418 | 5.964069 | 0.253032 | 84930723 | 335651523 | `/tmp/skv11-p1/parse-xctrace/cpu-counters/y_string_unicode__track2.trace` |

### Direct PMU rows

| Corpus | Track | Mbps | cycles/B | CPI | cycles | instructions | Trace |
|---|---|---:|---:|---:|---:|---:|---|
| `twitter` | Track 1 | 2551.904 | 3.788777 | 0.271845 | 957067677 | 3520643334 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/twitter__track1.trace` |
| `twitter` | Track 2 | 2566.737 | 4.021420 | 0.257899 | 1015834884 | 3938886256 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/twitter__track2.trace` |
| `citm_catalog` | Track 1 | 5408.185 | 2.054284 | 0.199319 | 1419266801 | 7120569941 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/citm_catalog__track1.trace` |
| `citm_catalog` | Track 2 | 6052.968 | 2.154968 | 0.200962 | 1488827449 | 7408506149 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/citm_catalog__track2.trace` |
| `canada` | Track 1 | 6399.905 | 3.312871 | 0.116842 | 2982976318 | 25529940147 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/canada__track1.trace` |
| `canada` | Track 2 | 6526.373 | 3.495142 | 0.125028 | 3147097590 | 25171144071 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/canada__track2.trace` |
| `apache_builds` | Track 1 | 2567.935 | 3.851386 | 0.259419 | 196074073 | 755820493 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/apache_builds__track1.trace` |
| `apache_builds` | Track 2 | 1933.553 | 4.223625 | 0.256877 | 215024737 | 837073009 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/apache_builds__track2.trace` |
| `github_events` | Track 1 | 5350.687 | 2.902896 | 0.220212 | 75628562 | 343435991 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/github_events__track1.trace` |
| `github_events` | Track 2 | 5060.674 | 3.111679 | 0.214830 | 81067949 | 377358951 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/github_events__track2.trace` |
| `update_center` | Track 1 | 3881.039 | 4.808980 | 0.264696 | 1025617032 | 3874700194 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/update_center__track1.trace` |
| `update_center` | Track 2 | 3155.694 | 5.741338 | 0.279372 | 1224462000 | 4382907102 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/update_center__track2.trace` |
| `mesh` | Track 1 | 1836.550 | 5.408942 | 0.163151 | 1565557694 | 9595735625 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/mesh__track1.trace` |
| `mesh` | Track 2 | 2365.692 | 5.353517 | 0.166416 | 1549515569 | 9311124694 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/mesh__track2.trace` |
| `random` | Track 1 | 1976.991 | 5.563805 | 0.232050 | 1136075491 | 4895815105 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/random__track1.trace` |
| `random` | Track 2 | 1756.800 | 5.980091 | 0.227322 | 1221077267 | 5371579323 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/random__track2.trace` |
| `gsoc-2018` | Track 1 | 3709.974 | 3.112215 | 0.319361 | 4142770382 | 12972063956 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/gsoc-2018__track1.trace` |
| `gsoc-2018` | Track 2 | 3129.891 | 3.226418 | 0.315741 | 4294790025 | 13602244616 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/gsoc-2018__track2.trace` |
| `marine_ik` | Track 1 | 2272.092 | 4.910266 | 0.167252 | 5859844428 | 35035953326 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/marine_ik__track1.trace` |
| `marine_ik` | Track 2 | 2059.564 | 4.907457 | 0.169594 | 5856492833 | 34532525543 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/marine_ik__track2.trace` |
| `instruments` | Track 1 | 2846.935 | 3.632491 | 0.224958 | 320161936 | 1423205544 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/instruments__track1.trace` |
| `instruments` | Track 2 | 2698.432 | 3.839065 | 0.219416 | 338369058 | 1542136419 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/instruments__track2.trace` |
| `numbers` | Track 1 | 3099.592 | 3.793946 | 0.171545 | 227824914 | 1328072969 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/numbers__track1.trace` |
| `numbers` | Track 2 | 2772.817 | 3.951598 | 0.173005 | 237291894 | 1371593168 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/numbers__track2.trace` |
| `unicode_mixed` | Track 1 | 1253.541 | 9.039499 | 0.468891 | 3807747796 | 8120755401 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/unicode_mixed__track1.trace` |
| `unicode_mixed` | Track 2 | 1177.005 | 9.222427 | 0.461533 | 3884803544 | 8417167489 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/unicode_mixed__track2.trace` |
| `unicode_escapes` | Track 1 | 4670.595 | 7.202233 | 0.250073 | 3027233935 | 12105396740 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/unicode_escapes__track1.trace` |
| `unicode_escapes` | Track 2 | 4646.969 | 7.244011 | 0.250210 | 3044793943 | 12168930572 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/unicode_escapes__track2.trace` |
| `unicode_basic` | Track 1 | 8735.096 | 3.844277 | 0.191470 | 1612421927 | 8421295782 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/unicode_basic__track1.trace` |
| `unicode_basic` | Track 2 | 7846.798 | 4.301602 | 0.196079 | 1804239674 | 9201585320 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/unicode_basic__track2.trace` |
| `distinct_values` | Track 1 | 6119.936 | 5.525195 | 0.241463 | 339534292 | 1406156646 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/distinct_values__track1.trace` |
| `distinct_values` | Track 2 | 5499.565 | 6.165911 | 0.233954 | 378907536 | 1619579087 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/distinct_values__track2.trace` |
| `y_string_unicode` | Track 1 | 3417.666 | 9.912037 | 0.144789 | 141151370 | 974873820 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/y_string_unicode__track1.trace` |
| `y_string_unicode` | Track 2 | 2926.911 | 11.492228 | 0.160671 | 163653926 | 1018564322 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/y_string_unicode__track2.trace` |

### Typed guard PMU rows

| Corpus | Track | Mbps | cycles/B | CPI | cycles | instructions | Trace |
|---|---|---:|---:|---:|---:|---:|---|
| `twitter` | Track 1 | 12845.696 | 2.068877 | 0.194991 | 522610747 | 2680181640 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/twitter__real_typed_track1.trace` |
| `twitter` | Track 2 | 11528.620 | 2.270121 | 0.237350 | 573446252 | 2416035959 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/twitter__real_typed_track2.trace` |
| `citm_catalog` | Track 1 | 33111.284 | 0.988137 | 0.139248 | 682685701 | 4902667029 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/citm_catalog__real_typed_track1.trace` |
| `citm_catalog` | Track 2 | 16901.611 | 1.903636 | 0.161665 | 1315186970 | 8135256342 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/citm_catalog__real_typed_track2.trace` |
| `apache_builds` | Track 1 | 7601.480 | 4.194671 | 0.245418 | 213550723 | 870150208 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/apache_builds__real_typed_track1.trace` |
| `apache_builds` | Track 2 | 5048.289 | 6.255773 | 0.222654 | 318481395 | 1430388696 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/apache_builds__real_typed_track2.trace` |
| `github_events` | Track 1 | 7722.363 | 2.673360 | 0.205824 | 69648502 | 338388648 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/github_events__real_typed_track1.trace` |
| `github_events` | Track 2 | 7479.519 | 3.346743 | 0.251556 | 87192017 | 346611239 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/github_events__real_typed_track2.trace` |
| `update_center` | Track 1 | 6829.678 | 3.046802 | 0.202273 | 649795030 | 3212461257 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/update_center__real_typed_track1.trace` |
| `update_center` | Track 2 | 5469.855 | 3.858657 | 0.246244 | 822940376 | 3341975946 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/update_center__real_typed_track2.trace` |
| `mesh` | Track 1 | 4136.278 | 4.840097 | 0.179876 | 1400911835 | 7788209495 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/mesh__real_typed_track1.trace` |
| `mesh` | Track 2 | 3216.785 | 5.829572 | 0.203277 | 1687304397 | 8300510414 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/mesh__real_typed_track2.trace` |
| `marine_ik` | Track 1 | 5459.069 | 3.598585 | 0.179474 | 4294501959 | 23928235998 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/marine_ik__real_typed_track1.trace` |
| `marine_ik` | Track 2 | 4818.537 | 3.935701 | 0.201032 | 4696811526 | 23363548100 | `/tmp/skv11-p1/direct-xctrace/cpu-counters/marine_ik__real_typed_track2.trace` |

## Section 3 - Delta vs SK-V10

SK-V10 P1-D had parse-only PMU coverage and explicitly recorded direct/typed
PMU as absent because no product-plane probe emitted `PROBE_RESULT` rows.
SK-V11 changes that state: `profile_direct` now emits the same
cycles/instructions tuple as `xctrace_probe`, so direct is present for all 17
corpora and typed guards are present for all seven guard corpora.

The parse shape is consistent with SK-V10 even though this V1 capture should
not be used as row admission evidence. Low parse c/B remains concentrated in
`citm_catalog`, `gsoc-2018`, and `canada`. High parse c/B remains concentrated
in `y_string_unicode`, `distinct_values` Track 2, and `unicode_mixed`.

The new product-plane PMU data makes the direct pressure visible:
`citm_catalog` is the low direct row at 2.054/2.155 c/B, while
`y_string_unicode` and `unicode_mixed` dominate the high end. The typed guard
plane is not uniformly cheap: `citm_catalog` Track 1 is 0.988 c/B, but
`apache_builds` Track 2 is 6.256 c/B and `mesh` Track 2 is 5.830 c/B.

No PMU row changes an outcome in `skinny/RESULTS.md`. PMU/cycles remain
diagnostic non-producers for S-P2/S-P3 planning.

## Section 4 - Anomalies And Masking Signals

Low/high c/B rows:

| Plane | Low c/B rows | High c/B rows |
|---|---|---|
| parse | `citm_catalog` T1 1.332, `citm_catalog` T2 1.841, `gsoc-2018` T1 1.918, `gsoc-2018` T2 1.960, `canada` T1 1.968 | `y_string_unicode` T1 6.022, `y_string_unicode` T2 5.964, `distinct_values` T2 5.707, `unicode_mixed` T1 5.283, `unicode_mixed` T2 5.226 |
| direct | `citm_catalog` T1 2.054, `citm_catalog` T2 2.155, `github_events` T1 2.903, `github_events` T2 3.112, `gsoc-2018` T1 3.112 | `y_string_unicode` T2 11.492, `y_string_unicode` T1 9.912, `unicode_mixed` T2 9.222, `unicode_mixed` T1 9.039, `unicode_escapes` T2 7.244 |
| typed guards | `citm_catalog` T1 0.988, `citm_catalog` T2 1.904, `twitter` T1 2.069, `twitter` T2 2.270, `github_events` T1 2.673 | `apache_builds` T2 6.256, `mesh` T2 5.830, `mesh` T1 4.840, `apache_builds` T1 4.195, `marine_ik` T2 3.936 |

Wide-issue evidence:

- Aggregate CPI is 0.211 on parse, 0.212 on direct, and 0.190 on typed guards.
  That is 4.7 to 5.3 aggregate instructions retired per cycle.
- Every row has CPI below 0.469. The highest CPI rows are
  `unicode_mixed` parse Track 2 at 0.452 and direct Track 1 at 0.469, still
  above 2 retired instructions per cycle.
- Some high-c/B rows are not high-CPI rows. Direct `y_string_unicode` Track 2
  is 11.492 c/B at 0.161 CPI, about 71.5 instructions/B. That is instruction
  volume per byte, not a simple branch-stall signature.
- `canada` has low CPI on both parse and direct (parse Track 1 0.119, direct
  Track 1 0.117), but direct still costs 3.313/3.495 c/B because it retires
  roughly 28 instructions/B. The host is issuing widely; the target pressure
  is work per byte and data movement, not a retained sidecar cursor.

Capture anomalies:

- All 82 PMU log captures returned `rc=0`: 34 parse rows, 34 direct rows, and
  14 typed guard rows.
- All 82 CPU Counter `.trace` bundles exist, but `capture_status.tsv` records
  `xctrace-cpu-counters` as `rc=54` for 81 of 82 captures. The lone `rc=0`
  CPU Counter capture is `direct/twitter/track1`. Time Profiler has the same
  shape: 81 of 82 `rc=54`, with only `parse/apache_builds/track1` at `rc=0`.
  The retained trace bundles are source artifacts, but the numeric table above
  uses the `PROBE_RESULT` rusage counters.
- The CPU Counter exports consulted for this artifact did not provide stable
  branch-miss, L1, or LLC event columns in the PMU TSVs. Those columns are not
  synthesized here.
- `update_center` remains normalized as `update_center` in tables and paths.
  Some direct `PROBE_RESULT` lines print `corpus=update-center`; row identity
  follows `skinny/RESULTS.md` and the trace/log filenames.

Masking signals:

- `skinny/RESULTS.md` marks structural scan, masking probes, PMU, and cycles
  as non-producers. This artifact follows that contract: it reports cost facts
  and anomalies only.
- No supplied P1-D TSV contains a masking-probe row. The masking signal for
  S-P2 is therefore the PMU/cycles shape itself: direct and typed work is now
  measured rather than absent, but it remains fenced from row admission.

## Section 5 - Sources

- Baseline: SK-V11-open commit `3ce75df4`, run id
  `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`.
- PMU window: `2026-05-19T22:00:30Z..2026-05-19T22:53:19Z`.
- `/tmp/skv11-p1/pmu/parse_pmu_rows.tsv`
- `/tmp/skv11-p1/pmu/product_pmu_rows.tsv`
- `/tmp/skv11-p1/pmu/capture_status.tsv`
- `/tmp/skv11-p1/parse-xctrace/logs/*.pmu.log`
- `/tmp/skv11-p1/direct-xctrace/logs/*.pmu.log`
- `/tmp/skv11-p1/parse-xctrace/cpu-counters/*.trace`
- `/tmp/skv11-p1/direct-xctrace/cpu-counters/*.trace`
- `skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs:149`
- `skinny/crates/bbnf-bench/src/bin/profile_direct.rs:115`
- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/tranches/sk-v11/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md`
- `skinny/RESULTS.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1d-pmu-cycles.md`
