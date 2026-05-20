# SK-V12 P1-B: Direct And Real-Typed Product-Plane Profile

Pass: S-P1 Profile. Cycle: V12.
Date: 2026-05-20.
Scope: product-plane profiling for `direct_to_struct` Track 1/Track 2 and
`real_typed_struct` Track 1/Track 2 on the SK-V12-open JSON baseline.
Output: this file.
Baseline: SK-V12-open at commit `50bd1648`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: release profile with debug symbols, `RUSTFLAGS=-C target-cpu=native`,
profile target `/tmp/skv12-profile-target-50bd1648`.
Profile tool: `samply`, `xcrun xctrace record --template "Time Profiler"`,
`xcrun xctrace record --template "CPU Counters"`, and `proc_pid_rusage` PMU
rows from `profile_direct`.
Corpus coverage: `direct_to_struct` 17/17 corpora x Track 1/Track 2;
`real_typed_struct` 7/7 guard corpora x Track 1/Track 2. `parse_only` is not a
product-plane target in this artifact.

Shared capture provenance:

- Capture root: `/tmp/skv12-p1`.
- Profile target: `/tmp/skv12-profile-target-50bd1648`.
- Baseline source: `50bd1648`, `docs(sk-v12-g-alpha): present converged alpha
  contract`.
- Toolchain: `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`, LLVM 22.1.2,
  host `aarch64-apple-darwin`.
- Product binary: `/tmp/skv12-profile-target-50bd1648/release/profile_direct`.
- xctrace probe binary:
  `/tmp/skv12-profile-target-50bd1648/release/xctrace_probe`.
- Fresh capture completion stamps:
  `/tmp/skv12-p1/pmu/product_done.txt` at `2026-05-20T06:38:35Z`,
  `/tmp/skv12-p1/samply/done.txt` at `2026-05-20T06:41:16Z`, and
  `/tmp/skv12-p1/xctrace_done.txt` at `2026-05-20T06:54:56Z`.

Exact build command for the profile target:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv12-profile-target-50bd1648 \
RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin xctrace_probe --bin profile_direct
```

## Section 1 - Method

Fresh captures were already present under `/tmp/skv12-p1`; this artifact
consumes those captures rather than re-recording them. Reproducible command
shape:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny

samply record --save-only --unstable-presymbolicate \
  -o /tmp/skv12-p1/samply/direct/<corpus>__<track>.json.gz \
  -- \
  /tmp/skv12-profile-target-50bd1648/release/profile_direct \
  <iters> <corpus-or-update-center-alias> <track1-or-track2>

samply record --save-only --unstable-presymbolicate \
  -o /tmp/skv12-p1/samply/typed/<corpus>__real_typed_<track>.json.gz \
  -- \
  /tmp/skv12-profile-target-50bd1648/release/profile_direct \
  <iters> <corpus-or-update-center-alias> real_typed_<track1-or-track2>

xcrun xctrace record \
  --template "Time Profiler" \
  --output /tmp/skv12-p1/direct-xctrace/time-profiler/<corpus>__<mode>.trace \
  --no-prompt \
  --time-limit 1000ms \
  --launch -- \
  /tmp/skv12-profile-target-50bd1648/release/profile_direct \
  <iters> <corpus-or-update-center-alias> <mode>

xcrun xctrace record \
  --template "CPU Counters" \
  --output /tmp/skv12-p1/direct-xctrace/cpu-counters/<corpus>__<mode>.trace \
  --no-prompt \
  --time-limit 1000ms \
  --launch -- \
  /tmp/skv12-profile-target-50bd1648/release/profile_direct \
  <iters> <corpus-or-update-center-alias> <mode>
```

Artifact verification commands:

```bash
awk -F '\t' 'NR>1{c[$1]++; s[$1":"$4]++} END{for(k in c) print k,c[k]; for(k in s) print k,s[k]}' \
  /tmp/skv12-p1/pmu/capture_status.tsv

find /tmp/skv12-p1/samply/direct -maxdepth 1 -name '*.json.gz' | wc -l
find /tmp/skv12-p1/samply/typed -maxdepth 1 -name '*.json.gz' | wc -l
find /tmp/skv12-p1/direct-xctrace/time-profiler -maxdepth 1 -name '*.trace' | wc -l
find /tmp/skv12-p1/direct-xctrace/cpu-counters -maxdepth 1 -name '*.trace' | wc -l
```

Accepted product-plane captures:

| Family | Expected | Fresh status |
|---|---:|---|
| `pmu-direct` | 34 | 34/34 PASS, `rc=0` |
| `pmu-typed` | 14 | 14/14 PASS, `rc=0` |
| `samply-direct` | 34 | 34/34 PASS, `rc=0` |
| `samply-typed` | 14 | 14/14 PASS, `rc=0` |
| `xctrace-time-profiler-direct` | 34 | 34/34 PASS, `rc=54` time-limit exit accepted with trace bundle |
| `xctrace-time-profiler-typed` | 14 | 14/14 PASS, `rc=54` time-limit exit accepted with trace bundle |
| `xctrace-cpu-counters-direct` | 34 | 34/34 PASS, trace bundle present; most `rc=54`, two `rc=0` |
| `xctrace-cpu-counters-typed` | 14 | 14/14 PASS, `rc=54` time-limit exit accepted with trace bundle |

Method caveats:

- `profile_direct` performs 16 sanity parses before the timed loop at
  `skinny/crates/bbnf-bench/src/bin/profile_direct.rs:95`; the rows below are
  product hot-loop evidence, not cold Criterion admissions.
- The samply profile JSONs record `symbolicated=false`; same-prefix
  `.json.syms.json` sidecars exist for every direct and typed row. The original
  product Time Profiler exports were shallow for several rows because the target
  app exited before collecting a useful hot-loop table. The V1 hardening fold
  recaptured all 48 product Time Profiler rows under
  `/tmp/skv12-p1/direct-xctrace/time-profiler-v2` with 20,000 iterations and a
  2s xctrace limit, exported them to
  `/tmp/skv12-p1/direct-xctrace/exports-v2`, and parsed top leaves into
  `/tmp/skv12-p1/time_profile_hot_leaf_{summary,details}.tsv`.
- Product v2 export coverage is 48/48 PASS, 23,383,417 XML bytes, minimum row
  export 284,543 bytes. Target-sample coverage is 64,541/64,593 selected
  direct samples and 25,692/25,713 selected typed samples. The two direct
  `update_center` rows were replayed with the required `update-center` launch
  alias; the correction is recorded in
  `/tmp/skv12-p1/product_time_profile_v2_alias_fixes.tsv`.
- PMU numbers come from `/tmp/skv12-p1/pmu/product_pmu_rows.tsv`. P1-D owns the
  full cycles-per-byte ledger; P1-B uses the product rows only to bind this
  product-plane profile.

## Section 2 - Findings

Notation:

- `T1` is generated Track 1. `T2` is the independent Track 2 or oracle.
- PMU Mbps and cycles-per-byte are from
  `/tmp/skv12-p1/pmu/product_pmu_rows.tsv`.
- `direct_to_struct` rows are JSON digest-plane rows. Four are admitted guards
  from SK-V11 close; thirteen are pre-blocked residual or W0-clamped rows.
- `real_typed_struct` rows are guarded JSON typed direct rows. They are not
  non-JSON baseline rows.

### Product PMU Summary - Direct

| Corpus | SK-V12 role | PMU Mbps T1/T2 | PMU c/B T1/T2 |
|---|---|---:|---:|
| `twitter` | pre-blocked residual | 12228 / 11367 | 2.95 / 3.20 |
| `citm_catalog` | direct guard | 22113 / 20847 | 1.61 / 1.72 |
| `canada` | pre-blocked residual | 10721 / 10412 | 3.25 / 3.37 |
| `apache_builds` | direct guard | 11746 / 10579 | 3.06 / 3.37 |
| `github_events` | pre-blocked residual | 12742 / 11691 | 2.83 / 3.09 |
| `update_center` | pre-blocked residual | 8771 / 7785 | 4.12 / 4.60 |
| `mesh` | pre-blocked residual | 8860 / 9096 | 3.96 / 3.83 |
| `random` | pre-blocked residual | 8029 / 7219 | 4.40 / 4.89 |
| `gsoc-2018` | pre-blocked residual | 15517 / 14733 | 2.34 / 2.43 |
| `marine_ik` | direct guard | 9571 / 9704 | 3.65 / 3.59 |
| `instruments` | W0-clamped non-admission | 12332 / 11407 | 2.86 / 3.10 |
| `numbers` | W0-clamped, T2 short | 12912 / 12613 | 2.70 / 2.76 |
| `unicode_mixed` | W0-clamped, T2 short | 4855 / 4687 | 7.45 / 7.66 |
| `unicode_escapes` | pre-blocked residual | 5328 / 5231 | 6.72 / 6.85 |
| `unicode_basic` | direct guard | 9357 / 8423 | 3.77 / 4.16 |
| `distinct_values` | pre-blocked residual | 6591 / 5760 | 5.47 / 6.21 |
| `y_string_unicode` | pre-blocked residual | 3503 / 3092 | 9.99 / 11.30 |

Direct synthesis:

- The four direct guards remain `citm_catalog`, `apache_builds`, `marine_ik`,
  and `unicode_basic`. This file records fresh product profile evidence for
  them but admits no new row.
- The thirteen non-guard direct rows remain diagnostic/pre-blocked by REDRESS
  119 and REDRESS 120. Fresh product PMU rows do not by themselves satisfy the
  SK-V12 reopen rule because they do not provide a new behavior source delta,
  same-wave gate consumption, or non-JSON priority resolution.
- Highest direct PMU c/B rows are still the unicode/string escape surface:
  `y_string_unicode` at 9.99/11.30 c/B, `unicode_mixed` at 7.45/7.66 c/B, and
  `unicode_escapes` at 6.72/6.85 c/B.
- Numeric-heavy rows (`canada`, `mesh`, `numbers`, `marine_ik`) remain a
  separate family from escaped-string rows. The fresh PMU row for `numbers`
  does not override its W0-clamped, T2-short status in `skinny/RESULTS.md`.

### Product PMU Summary - Real Typed

| Corpus | SK-V12 role | PMU Mbps T1/T2 | PMU c/B T1/T2 |
|---|---|---:|---:|
| `twitter` | typed guard | 19051 / 16708 | 1.88 / 2.12 |
| `citm_catalog` | typed guard | 36477 / 19450 | 0.96 / 1.82 |
| `apache_builds` | typed guard | 8822 / 5808 | 4.09 / 6.08 |
| `github_events` | typed guard | 13331 / 11839 | 2.71 / 3.00 |
| `update_center` | typed guard | 12864 / 10004 | 2.80 / 3.52 |
| `mesh` | typed guard | 9504 / 7389 | 3.69 / 4.73 |
| `marine_ik` | typed guard | 11948 / 10355 | 2.93 / 3.40 |

Typed synthesis:

- All seven `real_typed_struct` rows are guarded JSON product rows:
  `twitter`, `citm_catalog`, `apache_builds`, `github_events`,
  `update_center`, `mesh`, and `marine_ik`.
- Typed rows are output-plane specific. They cannot admit a direct digest row,
  and direct digest evidence cannot admit a typed row.
- Typed Track 2 is an oracle/independence surface. Its PMU rows are useful for
  guard maintenance and profiling, not for replacing the SK-V12 non-JSON
  baseline requirement.

### Accepted Product Hot Families

This section names accepted product hot families from the fresh product-v2
xctrace self-time export. Exact top leaf symbol, percent, and file:line for
every direct and typed row are in
`/tmp/skv12-p1/time_profile_hot_leaf_details.tsv`.

| Canonical product family | Evidence members and source anchors |
|---|---|
| `bounded_plain_string_scan` | generated tiny string at `skinny/crates/runtime/src/grammars/json/generated.rs:171`; hand tiny/plain string at `skinny/crates/bbnf-bench/src/direct_struct.rs:541` and `skinny/crates/bbnf-bench/src/direct_struct.rs:565`; typed tiny/plain skip at `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1811` and `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1825` |
| `string_escape_decode` | trusted string scan and escape validation at `skinny/crates/parse-that-regex/src/lib.rs:162`, `skinny/crates/parse-that-regex/src/lib.rs:284`, `skinny/crates/parse-that-regex/src/lib.rs:347`, `skinny/crates/parse-that-regex/src/lib.rs:547`, and `skinny/crates/parse-that-regex/src/lib.rs:718` |
| `unicode_escape_hex_decode` | scalar hex unit and nibble decode at `skinny/crates/parse-that-regex/src/lib.rs:945` and `skinny/crates/parse-that-regex/src/lib.rs:959` |
| `number_digit_span` | number span and digit run at `skinny/crates/parse-that-regex/src/number/mod.rs:38` and `skinny/crates/parse-that-regex/src/number/mod.rs:106`; numeric materialization at `skinny/crates/parse-that-regex/src/number/mod.rs:247` |
| `ascii_whitespace_skip` | whitespace and spaces at `skinny/crates/parse-that-regex/src/lib.rs:113` and `skinny/crates/parse-that-regex/src/lib.rs:128` |
| `simd_movemask` | NEON movemask helper at `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4` |
| `container_dispatch` | generated object/array direct dispatch at `skinny/crates/runtime/src/grammars/json/generated.rs:468` and `skinny/crates/runtime/src/grammars/json/generated.rs:508`; hand Track 2 value dispatch at `skinny/crates/bbnf-bench/src/direct_struct.rs:460` |
| `output_digest_hash` | digest string folding at `skinny/crates/bbnf-bench/src/direct_struct.rs:123`; direct digest entry points at `skinny/crates/bbnf-bench/src/direct_struct.rs:401` and `skinny/crates/bbnf-bench/src/direct_struct.rs:408` |
| `typed_direct_projection` | typed parser skip/value surface at `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1593`, `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1599`, and `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1739`; typed checksum at `skinny/crates/bbnf-bench/src/real_typed_struct.rs:465` |
| `serde_json_oracle_read_parse` | serde oracle whitespace/number/read routines at `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/serde_json-1.0.149/src/de.rs:255`, `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/serde_json-1.0.149/src/de.rs:462`, `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/serde_json-1.0.149/src/de.rs:530`, `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/serde_json-1.0.149/src/read.rs:286`, `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/serde_json-1.0.149/src/read.rs:302`, and `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/serde_json-1.0.149/src/read.rs:432` |

The product hot families accepted for S-P2 consideration are therefore:

- bounded/plain string scanning and typed string skipping;
- unicode escape validation and scalar hex decode;
- number span/digit scanning and numeric materialization;
- whitespace skip;
- container dispatch and sequence/value dispatch;
- digest folding and typed projection plumbing;
- serde_json oracle read/parse costs as an independence comparator only.

Leading product self-time family distribution by row:

| Plane | Leading families |
|---|---|
| direct | `output_digest_hash` 18 rows; `container_dispatch` 10; `string_escape_decode` 4; `bounded_plain_string_scan` 1; `number_digit_span` 1 |
| typed guards | `serde_json_oracle_read_parse` 7 rows; `typed_direct_projection` 5; `number_digit_span` 2 |

## Section 3 - Delta vs SK-V11

P1-F owns the full per-row delta ledger. P1-B records only the product-plane
state relevant to hot-family interpretation:

| Surface | SK-V11 close | SK-V12-open product profile |
|---|---|---|
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO`; direct residuals closed by REDRESS 119 | 17/17 direct corpora profiled x T1/T2 with fresh samply, xctrace, and PMU. No new admission. |
| `real_typed_struct` | 7 `A / GO` typed guard rows | 7/7 typed guard corpora profiled x T1/T2 with fresh samply, xctrace, and PMU. No demotion evidence in this artifact. |
| non-JSON generated parser | no admitted generated baseline; first SK-V12 material target | no non-JSON product row exists in this P1-B capture set. This file does not substitute JSON product profiling for the required generated non-JSON baseline. |

The profile confirms the SK-V12 opening distinction:

- Guarded JSON direct/typed rows are preservation surfaces.
- JSON direct residual rows are diagnostic/pre-blocked reopen ledger rows.
- The SK-V12 implementation target remains a generated non-JSON direct or
  typed baseline first, followed by the same row's measured
  grammar-generalized intervention.

## Section 4 - Anomalies And Masking Signals

- Fresh direct/typed xctrace bundles and product-v2 exports exist. The
  product-v2 exports are the self-time authority; the original product exports
  are retained only as the shallow-capture caveat.
- Fresh samply direct/typed profiles are present with `.json.syms.json`
  sidecars, but the raw profile metadata records `symbolicated=false`.
  Symbol material is present; leaf percentages in this artifact come from the
  exported xctrace Time Profiler XML, not from samply JSON.
- `profile_direct` is a hot-loop product profiler with 16 pre-loop sanity
  parses. It is not a cold Criterion gate and does not move result rows.
- The fresh PMU direct rows can differ materially from the Criterion Mbps in
  `skinny/RESULTS.md` because the profiler loop and gate loop are different
  authorities. `skinny/RESULTS.md` remains the row-admission authority.
- No observation here reopens W3 union/event/class-column/streaming-cursor,
  parser-owned sidecar, retained structural vector, direct digest as typed
  proof, parse-only SOTA, or JSON direct residual work before the non-JSON
  priority. REDRESS 119 and 120 keep the direct residual surface pre-blocked
  unless a later pass names fresh material evidence beyond REDRESS 114-119.

## Section 5 - Sources

- `/tmp/skv12-p1/pmu/product_pmu_rows.tsv`
- `/tmp/skv12-p1/pmu/capture_status.tsv`
- `/tmp/skv12-p1/pmu/product_done.txt`
- `/tmp/skv12-p1/samply/direct/*.json.gz`
- `/tmp/skv12-p1/samply/direct/*.json.syms.json`
- `/tmp/skv12-p1/samply/typed/*.json.gz`
- `/tmp/skv12-p1/samply/typed/*.json.syms.json`
- `/tmp/skv12-p1/samply/done.txt`
- `/tmp/skv12-p1/direct-xctrace/time-profiler/*.trace`
- `/tmp/skv12-p1/direct-xctrace/time-profiler-v2/*.trace`
- `/tmp/skv12-p1/direct-xctrace/exports-v2/*.time-profile.xml`
- `/tmp/skv12-p1/direct-xctrace/cpu-counters/*.trace`
- `/tmp/skv12-p1/product_time_profile_v2_status.tsv`
- `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv`
- `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv`
- `/tmp/skv12-p1/xctrace_done.txt`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`
- `/tmp/skv12-profile-target-50bd1648/.rustc_info.json`
- `/tmp/skv12-profile-target-50bd1648/release/profile_direct`
- `/tmp/skv12-profile-target-50bd1648/release/xctrace_probe`
- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md`
- `skinny/crates/bbnf-bench/src/bin/profile_direct.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/parse-that-regex/src/number/mod.rs`
- `skinny/crates/bbnf-simd/src/aarch64/movemask.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
