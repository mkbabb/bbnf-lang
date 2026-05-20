# SK-V12 P1-D: PMU And Cycles-Per-Byte

Pass: S-P1 Profile. Cycle: V12.
Date: 2026-05-20.
Scope: fresh PMU counters, Mbps, CPI, and cycles-per-byte for parse, direct,
and typed guard lanes.
Output: this file.
Baseline: SK-V12-open commit `50bd1648`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `RUSTFLAGS=-C target-cpu=native`, release profile, debug symbols.
Profile tool: `proc_pid_rusage(RUSAGE_INFO_V5)` cycles/instructions emitted by
`xctrace_probe` and `profile_direct`; xctrace trace bundles retained under the
capture root.
Corpus coverage: parse Track 1/Track 2 17/17; direct Track 1/Track 2 17/17;
typed guard Track 1/Track 2 7/7.

Shared capture provenance:

- Capture root: `/tmp/skv12-p1`.
- PMU row authority:
  `/tmp/skv12-p1/pmu/parse_pmu_rows.tsv`,
  `/tmp/skv12-p1/pmu/product_pmu_rows.tsv`, and
  `/tmp/skv12-p1/pmu/capture_status.tsv`.
- Target directory: `/tmp/skv12-profile-target-50bd1648`.
- Binary paths:
  `/tmp/skv12-profile-target-50bd1648/release/xctrace_probe` and
  `/tmp/skv12-profile-target-50bd1648/release/profile_direct`.
- Toolchain: `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`, LLVM 22.1.2;
  `cargo 1.96.0-nightly (eb94155a9 2026-04-09)`.
- Host OS: Darwin 25.4.0, arm64, Apple M5 Max.
- PMU done markers:
  `/tmp/skv12-p1/pmu/done.txt` at `2026-05-20T06:34:59Z`,
  `/tmp/skv12-p1/pmu/product_done.txt` at `2026-05-20T06:38:35Z`,
  and `/tmp/skv12-p1/xctrace_done.txt` at `2026-05-20T06:54:56Z`.

Exact build command:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv12-profile-target-50bd1648 \
RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin xctrace_probe --bin profile_direct
```

## Section 1 - Method

Exact PMU replay rows are enumerated in
`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`; that ledger
also records the companion samply, xctrace Time Profiler, CPU Counter, and
export commands. The block below is the readable PMU command shape:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny

/tmp/skv12-profile-target-50bd1648/release/xctrace_probe \
  /Users/mkbabb/Programming/bbnf-lang/skinny/test_data/<corpus>.json \
  <track1-or-track2> <iters> \
  > /tmp/skv12-p1/logs/pmu-parse-<corpus>-<track>.log.out 2> \
    /tmp/skv12-p1/logs/pmu-parse-<corpus>-<track>.log.err

/tmp/skv12-profile-target-50bd1648/release/profile_direct \
  <iters> <corpus> <track1-or-track2> \
  > /tmp/skv12-p1/logs/pmu-direct-<corpus>-<track>.rerun.log.out 2> \
    /tmp/skv12-p1/logs/pmu-direct-<corpus>-<track>.rerun.log.err

/tmp/skv12-profile-target-50bd1648/release/profile_direct \
  <iters> <corpus> <real_typed_track1-or-real_typed_track2> \
  > /tmp/skv12-p1/logs/pmu-typed-<corpus>-<mode>.rerun.log.out 2> \
    /tmp/skv12-p1/logs/pmu-typed-<corpus>-<mode>.rerun.log.err
```

The parse binary loads `skinny/test_data/<corpus>.json`; the product binary
locates `skinny/crates/test-fixtures/corpus/json/<corpus>.json` when run from
`skinny/`. The product wrapper was first run from the repository root, which
made `profile_direct` panic with fixture lookup failures such as
`could not locate fixture twitter.json under crates/test-fixtures/corpus/json`.
That failed product-only run is isolated in
`/tmp/skv12-p1/pmu/capture_status.initial-product-cwd-fail.tsv`: parse 34/34
passed, direct 34/34 failed with `rc=134`, and typed 14/14 failed with
`rc=134`. The final `/tmp/skv12-p1/pmu/capture_status.tsv` records the clean
rerun from `skinny/`: parse 34/34 `PASS rc=0`, direct 34/34 `PASS rc=0`, and
typed guard 14/14 `PASS rc=0`.

The numeric PMU tables below use only the `PROBE_RESULT` rows from the primary
TSVs. Branch misses, L1 misses, and LLC misses are not present as extracted
columns in these TSVs and are not synthesized. xctrace CPU Counter and Time
Profiler bundles are retained as artifacts, but the PMU number authority here
is cycles/instructions from `proc_pid_rusage(RUSAGE_INFO_V5)`.

These values are profile evidence only. They do not move any row in
`skinny/RESULTS.md`, do not admit a direct or typed row, and do not change the
SK-V12 opening `N-direct / NoGo` surface.

## Section 2 - Findings

Aggregate values are weighted by captured bytes, cycles, instructions, and
elapsed time across rows in each workload family.

| Workload family | Rows | Corpora | PMU rc=0 | Aggregate Mbps | Aggregate cycles/B | Aggregate CPI | Aggregate IPC |
|---|---:|---:|---:|---:|---:|---:|---:|
| parse | 34 | 17 | 34 | 12274.872 | 2.920217 | 0.204887 | 4.881 |
| direct | 34 | 17 | 34 | 8278.039 | 4.290305 | 0.183717 | 5.443 |
| typed guards | 14 | 7 | 14 | 11338.859 | 3.123172 | 0.185056 | 5.404 |

Min, median, and max row values by workload family:

| Family | Metric | Min row | Min | Median | Max row | Max |
|---|---|---|---:|---:|---|---:|
| parse | cycles/B | `citm_catalog/track1` | 1.123 | 2.801 | `y_string_unicode/track2` | 5.901 |
| parse | CPI | `canada/track1` | 0.117 | 0.210 | `unicode_mixed/track1` | 0.362 |
| parse | Mbps | `y_string_unicode/track2` | 6072.193 | 12865.588 | `citm_catalog/track1` | 31987.293 |
| direct | cycles/B | `citm_catalog/track1` | 1.612 | 3.622 | `y_string_unicode/track2` | 11.302 |
| direct | CPI | `canada/track1` | 0.115 | 0.189 | `unicode_mixed/track1` | 0.387 |
| direct | Mbps | `y_string_unicode/track2` | 3092.310 | 9637.542 | `citm_catalog/track1` | 22113.028 |
| typed guards | cycles/B | `citm_catalog/real_typed_track1` | 0.964 | 2.966 | `apache_builds/real_typed_track2` | 6.081 |
| typed guards | CPI | `citm_catalog/real_typed_track1` | 0.136 | 0.182 | `apache_builds/real_typed_track1` | 0.239 |
| typed guards | Mbps | `apache_builds/real_typed_track2` | 5808.402 | 11893.859 | `citm_catalog/real_typed_track1` | 36477.111 |

Track split summary:

| Family | Mode | Rows | Aggregate Mbps | Aggregate cycles/B | Aggregate CPI | Median cycles/B | Median CPI | Median Mbps |
|---|---|---:|---:|---:|---:|---:|---:|---:|
| parse | `track1` | 17 | 13196.008 | 2.707355 | 0.188764 | 2.653 | 0.194 | 13365.857 |
| parse | `track2` | 17 | 11473.944 | 3.133080 | 0.221215 | 2.841 | 0.239 | 12572.809 |
| direct | `track1` | 17 | 8577.264 | 4.146942 | 0.182019 | 3.650 | 0.188 | 9571.232 |
| direct | `track2` | 17 | 7998.988 | 4.433668 | 0.185335 | 3.593 | 0.190 | 9703.853 |
| typed guards | `real_typed_track1` | 7 | 13085.906 | 2.723230 | 0.173738 | 2.798 | 0.177 | 12864.135 |
| typed guards | `real_typed_track2` | 7 | 10003.351 | 3.523115 | 0.194869 | 3.396 | 0.217 | 10355.461 |

### Parse PMU rows

| Corpus | Mode | Mbps | cycles/B | CPI | cycles | instructions |
|---|---|---:|---:|---:|---:|---:|
| `twitter` | `track1` | 16334.457 | 2.214 | 0.197 | 8856987651 | 44979819738 |
| `twitter` | `track2` | 12863.922 | 2.845 | 0.263 | 11380153924 | 43319963122 |
| `citm_catalog` | `track1` | 31987.293 | 1.123 | 0.147 | 4490239577 | 30625183554 |
| `citm_catalog` | `track2` | 22138.318 | 1.653 | 0.208 | 6611645138 | 31795508066 |
| `canada` | `track1` | 18308.584 | 1.933 | 0.117 | 7733258642 | 66371652697 |
| `canada` | `track2` | 16934.283 | 2.076 | 0.126 | 8305939224 | 65772601032 |
| `apache_builds` | `track1` | 13365.857 | 2.737 | 0.215 | 10947342361 | 50952531900 |
| `apache_builds` | `track2` | 12867.254 | 2.841 | 0.241 | 11365379766 | 47211344260 |
| `github_events` | `track1` | 16028.827 | 2.281 | 0.209 | 9123315299 | 43712495642 |
| `github_events` | `track2` | 13762.399 | 2.657 | 0.254 | 10628494039 | 41881443720 |
| `update_center` | `track1` | 12516.376 | 2.893 | 0.201 | 11574927959 | 57626888611 |
| `update_center` | `track2` | 9784.092 | 3.735 | 0.270 | 14939873601 | 55386523695 |
| `mesh` | `track1` | 13334.199 | 2.653 | 0.134 | 10610641162 | 79375765137 |
| `mesh` | `track2` | 12552.333 | 2.803 | 0.140 | 11213953902 | 80011640796 |
| `random` | `track1` | 10280.709 | 3.519 | 0.186 | 14074612329 | 75614940212 |
| `random` | `track2` | 8245.315 | 4.407 | 0.239 | 17628295434 | 73730176648 |
| `gsoc-2018` | `track1` | 24008.597 | 1.481 | 0.219 | 5924237899 | 27079055114 |
| `gsoc-2018` | `track2` | 22634.359 | 1.572 | 0.241 | 6289636210 | 26150153121 |
| `marine_ik` | `track1` | 13674.284 | 2.556 | 0.139 | 10224152029 | 73296309371 |
| `marine_ik` | `track2` | 12572.809 | 2.798 | 0.153 | 11194613527 | 73367755252 |
| `instruments` | `track1` | 17458.141 | 2.028 | 0.160 | 8110397951 | 50827270189 |
| `instruments` | `track2` | 12318.076 | 2.933 | 0.231 | 11733212813 | 50740822355 |
| `numbers` | `track1` | 19951.329 | 1.742 | 0.138 | 6968126095 | 50528380745 |
| `numbers` | `track2` | 19266.962 | 1.812 | 0.142 | 7249225416 | 51062234310 |
| `unicode_mixed` | `track1` | 8412.339 | 4.297 | 0.362 | 17189613205 | 47525830039 |
| `unicode_mixed` | `track2` | 9259.055 | 3.893 | 0.338 | 15575187168 | 46123666927 |
| `unicode_escapes` | `track1` | 12660.466 | 2.819 | 0.217 | 11277912634 | 51936329801 |
| `unicode_escapes` | `track2` | 13128.572 | 2.726 | 0.210 | 10903801784 | 51823104337 |
| `unicode_basic` | `track1` | 12296.831 | 2.865 | 0.194 | 11459083078 | 59084995756 |
| `unicode_basic` | `track2` | 10913.723 | 3.229 | 0.219 | 12917597338 | 59000785648 |
| `distinct_values` | `track1` | 9957.208 | 3.585 | 0.192 | 14339768482 | 74811677847 |
| `distinct_values` | `track2` | 6354.598 | 5.684 | 0.300 | 22737819641 | 75698183263 |
| `y_string_unicode` | `track1` | 6282.424 | 5.622 | 0.236 | 20014379296 | 84685813394 |
| `y_string_unicode` | `track2` | 6072.193 | 5.901 | 0.251 | 21007687623 | 83832850738 |

### Direct PMU rows

| Corpus | Mode | Mbps | cycles/B | CPI | cycles | instructions |
|---|---|---:|---:|---:|---:|---:|
| `twitter` | `track1` | 12228.288 | 2.950 | 0.212 | 11798563633 | 55570806116 |
| `twitter` | `track2` | 11367.466 | 3.200 | 0.206 | 12798367635 | 62244037064 |
| `citm_catalog` | `track1` | 22113.028 | 1.612 | 0.157 | 6446734783 | 41179587350 |
| `citm_catalog` | `track2` | 20846.893 | 1.717 | 0.160 | 6866990994 | 42839951922 |
| `canada` | `track1` | 10720.653 | 3.254 | 0.115 | 13014836081 | 113405066947 |
| `canada` | `track2` | 10412.202 | 3.366 | 0.120 | 13463514193 | 111802821212 |
| `apache_builds` | `track1` | 11746.122 | 3.058 | 0.207 | 12232530435 | 59119178049 |
| `apache_builds` | `track2` | 10578.866 | 3.374 | 0.206 | 13494487954 | 65508811642 |
| `github_events` | `track1` | 12742.067 | 2.830 | 0.215 | 11319295297 | 52660410559 |
| `github_events` | `track2` | 11690.509 | 3.092 | 0.214 | 12367006488 | 57916852637 |
| `update_center` | `track1` | 8770.736 | 4.120 | 0.227 | 16482383717 | 72531343372 |
| `update_center` | `track2` | 7784.773 | 4.597 | 0.225 | 18391270623 | 81822763504 |
| `mesh` | `track1` | 8860.097 | 3.956 | 0.120 | 15825000522 | 132371402701 |
| `mesh` | `track2` | 9095.560 | 3.832 | 0.119 | 15327995136 | 128428706569 |
| `random` | `track1` | 8028.981 | 4.403 | 0.184 | 17612623742 | 95774442267 |
| `random` | `track2` | 7218.882 | 4.890 | 0.186 | 19559152628 | 105051657983 |
| `gsoc-2018` | `track1` | 15517.289 | 2.336 | 0.240 | 9342906716 | 38856312944 |
| `gsoc-2018` | `track2` | 14732.760 | 2.427 | 0.239 | 9707287326 | 40664417628 |
| `marine_ik` | `track1` | 9571.232 | 3.650 | 0.125 | 14604955287 | 117265222276 |
| `marine_ik` | `track2` | 9703.853 | 3.593 | 0.124 | 14376649030 | 115550293101 |
| `instruments` | `track1` | 12332.348 | 2.863 | 0.178 | 11452442259 | 64494856217 |
| `instruments` | `track2` | 11406.965 | 3.099 | 0.177 | 12394706320 | 69904116116 |
| `numbers` | `track1` | 12911.856 | 2.703 | 0.122 | 10812580095 | 88357337108 |
| `numbers` | `track2` | 12612.575 | 2.761 | 0.121 | 11044175220 | 91273610575 |
| `unicode_mixed` | `track1` | 4854.792 | 7.454 | 0.387 | 29822810664 | 76970347052 |
| `unicode_mixed` | `track2` | 4686.790 | 7.663 | 0.384 | 30658246219 | 79773565703 |
| `unicode_escapes` | `track1` | 5328.114 | 6.722 | 0.233 | 26889904349 | 115242307573 |
| `unicode_escapes` | `track2` | 5231.279 | 6.846 | 0.237 | 27385821536 | 115776897948 |
| `unicode_basic` | `track1` | 9357.366 | 3.768 | 0.188 | 15072083843 | 80288488724 |
| `unicode_basic` | `track2` | 8423.205 | 4.161 | 0.190 | 16647276406 | 87749882594 |
| `distinct_values` | `track1` | 6590.713 | 5.469 | 0.239 | 21875816494 | 91520958822 |
| `distinct_values` | `track2` | 5760.466 | 6.209 | 0.236 | 24835308310 | 105419605679 |
| `y_string_unicode` | `track1` | 3502.554 | 9.993 | 0.146 | 35577380225 | 243699039704 |
| `y_string_unicode` | `track2` | 3092.310 | 11.302 | 0.158 | 40236852301 | 254565214174 |

### Typed guard PMU rows

| Corpus | Mode | Mbps | cycles/B | CPI | cycles | instructions |
|---|---|---:|---:|---:|---:|---:|
| `twitter` | `real_typed_track1` | 19050.828 | 1.881 | 0.177 | 7526020037 | 42424142714 |
| `twitter` | `real_typed_track2` | 16707.754 | 2.124 | 0.222 | 8495066299 | 38233664810 |
| `citm_catalog` | `real_typed_track1` | 36477.111 | 0.964 | 0.136 | 3855054852 | 28380106838 |
| `citm_catalog` | `real_typed_track2` | 19449.573 | 1.815 | 0.154 | 7261919629 | 47068009074 |
| `apache_builds` | `real_typed_track1` | 8822.084 | 4.088 | 0.239 | 16352869108 | 68308899865 |
| `apache_builds` | `real_typed_track2` | 5808.402 | 6.081 | 0.217 | 24323739476 | 112312519289 |
| `github_events` | `real_typed_track1` | 13330.578 | 2.706 | 0.208 | 10825283940 | 51960291020 |
| `github_events` | `real_typed_track2` | 11839.401 | 3.000 | 0.226 | 11998479203 | 53208023909 |
| `update_center` | `real_typed_track1` | 12864.135 | 2.798 | 0.186 | 11192889124 | 60154378284 |
| `update_center` | `real_typed_track2` | 10003.854 | 3.515 | 0.224 | 14061692852 | 62648744348 |
| `mesh` | `real_typed_track1` | 9503.550 | 3.694 | 0.137 | 14774331230 | 107511475239 |
| `mesh` | `real_typed_track2` | 7388.700 | 4.732 | 0.165 | 18927160781 | 114549736054 |
| `marine_ik` | `real_typed_track1` | 11948.316 | 2.932 | 0.146 | 11728526376 | 80168876226 |
| `marine_ik` | `real_typed_track2` | 10355.461 | 3.396 | 0.174 | 13585039524 | 78232463425 |

## Section 3 - Delta vs SK-V11

SK-V11 P1-D V2 had aggregate c/B of parse 2.777033, direct 4.428342, and
typed guards 3.190644. The fresh SK-V12-open run is close but not identical:
parse is 2.920217 c/B, direct is 4.290305 c/B, and typed guards are
3.123172 c/B. Aggregate CPI is lower in the fresh run for all three families:
parse 0.204887, direct 0.183717, typed guards 0.185056.

The row shape is stable enough for S-P2/S-P3 cost triage: `citm_catalog`
remains the cheapest parse/direct/product row family, while `y_string_unicode`,
`unicode_mixed`, `unicode_escapes`, `distinct_values`, and typed
`apache_builds` carry the high cycles/B pressure. This is not evidence of row
admission or row movement; it is a fresh profile of the SK-V12-open baseline.

## Section 4 - Anomalies And Masking Signals

Low and high cycles/B rows:

| Family | Low c/B rows | High c/B rows |
|---|---|---|
| parse | `citm_catalog` T1 1.123; `gsoc-2018` T1 1.481; `gsoc-2018` T2 1.572; `citm_catalog` T2 1.653; `numbers` T1 1.742 | `y_string_unicode` T2 5.901; `distinct_values` T2 5.684; `y_string_unicode` T1 5.622; `random` T2 4.407; `unicode_mixed` T1 4.297 |
| direct | `citm_catalog` T1 1.612; `citm_catalog` T2 1.717; `gsoc-2018` T1 2.336; `gsoc-2018` T2 2.427; `numbers` T1 2.703 | `y_string_unicode` T2 11.302; `y_string_unicode` T1 9.993; `unicode_mixed` T2 7.663; `unicode_mixed` T1 7.454; `unicode_escapes` T2 6.846 |
| typed guards | `citm_catalog` T1 0.964; `citm_catalog` T2 1.815; `twitter` T1 1.881; `twitter` T2 2.124; `github_events` T1 2.706 | `apache_builds` T2 6.081; `mesh` T2 4.732; `apache_builds` T1 4.088; `mesh` T1 3.694; `update_center` T2 3.515 |

Notable outliers and interpretation:

- `unicode_mixed` is the highest-CPI family in the fresh PMU rows: direct T1
  0.387 CPI, direct T2 0.384 CPI, parse T1 0.362 CPI, and parse T2 0.338 CPI.
  It is both high c/B and high CPI, so it remains a masking signal for Unicode
  decode/classification cost rather than a direct row admission path.
- `y_string_unicode` direct is the highest cycles/B row pair, at 9.993 and
  11.302 c/B, but CPI is only 0.146 and 0.158. The pressure is instruction
  volume per byte, not a simple low-IPC stall signature.
- `canada`, `mesh`, `marine_ik`, and `numbers` have very low direct CPI
  despite material cycles/B. That points at high retired work per byte and data
  movement, not a retained sidecar or W3-style substrate reopening.
- Typed guard `apache_builds` T2 is the high typed guard at 6.081 c/B and the
  low typed Mbps row at 5808.402 Mbps. It remains a guard-profile signal only.
- No supplied P1-D primary TSV contains masking-probe rows, branch misses, L1
  misses, or LLC misses. `skinny/RESULTS.md` already fences
  `structural_scan+masking_probes+pmu+cycles` as nonproducer telemetry; this
  artifact follows that contract.

Capture-status anomalies:

- Final PMU row capture is clean: 82 PMU rows are `PASS rc=0`.
- The initial product wrapper CWD failure is confined to
  `capture_status.initial-product-cwd-fail.tsv`: 48 product rows failed
  (`pmu-direct` 34 and `pmu-typed` 14), while parse rows passed. The product
  rerun from `skinny/` produced the `.rerun` logs and all final product PMU rows
  passed.
- xctrace status rows are artifact-retention evidence, not numeric PMU
  authority. `capture_status.tsv` records all Time Profiler bundles as
  `PASS rc=54`; CPU Counter bundles are `PASS rc=54` except direct
  `instruments/track1` and direct `unicode_escapes/track2`, which are
  `PASS rc=0`.

## Section 5 - Sources

- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` through REDRESS 120
- `restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md`
- `/tmp/skv12-p1/pmu/parse_pmu_rows.tsv`
- `/tmp/skv12-p1/pmu/product_pmu_rows.tsv`
- `/tmp/skv12-p1/pmu/capture_status.tsv`
- `/tmp/skv12-p1/pmu/capture_status.initial-product-cwd-fail.tsv`
- `/tmp/skv12-p1/logs/*.log.out`
- `/tmp/skv12-p1/logs/*.log.err`
- `/tmp/skv12-p1/parse-xctrace/time-profiler/*.trace`
- `/tmp/skv12-p1/parse-xctrace/cpu-counters/*.trace`
- `/tmp/skv12-p1/direct-xctrace/time-profiler/*.trace`
- `/tmp/skv12-p1/direct-xctrace/cpu-counters/*.trace`
- `skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs`
- `skinny/crates/bbnf-bench/src/bin/profile_direct.rs`
