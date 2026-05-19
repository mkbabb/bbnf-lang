# SK-V11 P1-B: Direct And Real-Typed Product-Plane Profile

Pass: S-P1 Profile. Cycle: V2 fold.
Date: 2026-05-19.
Scope: product-plane profiling for `direct_to_struct` Track 1/Track 2 and
`real_typed_struct` Track 1/Track 2 on the SK-V11-open JSON baseline.
Output: this file.
Baseline: SK-V11-open at commit `3ce75df4`, run
`sk-v9-open:criterion-fnv64-c8d7e0468358f98c`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: release profile, debug symbols, `RUSTFLAGS=-C target-cpu=native`,
profile target `/tmp/skv11-profile-target-9c8da194`.
Profile tool: `samply 0.13.1`, `xcrun xctrace record --template "Time
Profiler"`, `xcrun xctrace record --template "CPU Counters"`, and
`proc_pid_rusage` PMU rows from `profile_direct`.
Corpus coverage: `direct_to_struct` 17/17 corpora x Track 1/Track 2;
`real_typed_struct` 7/7 guard corpora x Track 1/Track 2. `parse_only` is not a
product-plane target in this artifact.

Shared capture provenance:

- Run id: `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`.
- Capture root: `/tmp/skv11-p1`; W0 Criterion root:
  `/tmp/skv11-open-criterion-3ce75df`.
- Host/toolchain: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`;
  `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`, LLVM 22.1.2.
- Source SHA for `xctrace_probe` and `profile_direct`: `3ce75df4`, the last
  behavior/probe source commit before profiling. Documentation/results freeze:
  `9c8da194`. This V2 fold edits docs only.
- Build profile: release with debug symbols, `RUSTFLAGS="-C target-cpu=native"`,
  target directory `/tmp/skv11-profile-target-9c8da194`.
- Binary paths:
  `/tmp/skv11-profile-target-9c8da194/release/xctrace_probe` and
  `/tmp/skv11-profile-target-9c8da194/release/profile_direct`.

Exact build command:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv11-profile-target-9c8da194 \
RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin xctrace_probe --bin profile_direct
```

## Section 1 - Method

Commands:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv11-profile-target-9c8da194 \
RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin profile_direct

samply record --save-only --unstable-presymbolicate \
  -o /tmp/skv11-p1/samply/direct/<corpus>__<track>.json.gz \
  -- \
  /tmp/skv11-profile-target-9c8da194/release/profile_direct \
  400 <corpus-or-update-center-alias> <track1-or-track2>

samply record --save-only --unstable-presymbolicate \
  -o /tmp/skv11-p1/samply/typed/<corpus>__real_typed_<track>.json.gz \
  -- \
  /tmp/skv11-profile-target-9c8da194/release/profile_direct \
  400 <corpus-or-update-center-alias> real_typed_<track1-or-track2>

xcrun xctrace record \
  --template "Time Profiler" \
  --output /tmp/skv11-p1/direct-xctrace/time-profiler/<corpus>__<mode>.trace \
  --no-prompt \
  --time-limit 1000ms \
  --launch -- \
  /tmp/skv11-profile-target-9c8da194/release/profile_direct \
  100000 <corpus-or-update-center-alias> <mode>

xcrun xctrace export \
  --input /tmp/skv11-p1/direct-xctrace/time-profiler/<corpus>__<mode>.trace \
  --xpath '/trace-toc/run[@number="1"]/data/table[@schema="time-profile"]'
```

Accepted product-plane captures:

- `samply-direct`: 34/34 rows, all `rc=0`.
- `samply-typed`: 14/14 rows, all `rc=0`.
- `pmu-direct`: 48/48 product rows, all `rc=0`.
- `xctrace-time-profiler`: product rows present under
  `/tmp/skv11-p1/direct-xctrace/time-profiler`; `rc=54` is the normal
  time-limit exit and is accepted when the trace bundle and exported
  `*.symbols.json` exist.

Method caveats:

- `profile_direct` still performs 16 sanity parses before the timed loop
  (`skinny/crates/bbnf-bench/src/bin/profile_direct.rs:95`). Treat this as
  product hot-loop attribution, not a replacement for the cold Criterion gate.
- The samply profile JSONs report `symbolicated=false`; each raw profile has a
  same-prefix `.json.syms.json` sidecar. The resolved self-time tables below
  use `/tmp/skv11-p1/direct-xctrace/exports/summary.json` and the corresponding
  `/tmp/skv11-p1/direct-xctrace/exports/*.symbols.json` as the symbol authority.
- PMU numbers in this file come from
  `/tmp/skv11-p1/pmu/product_pmu_rows.tsv`. P1-D owns the full cycles ledger.

## Section 2 - Findings

Notation:

- `T1` is generated Track 1. `T2` is the independent hand-coded Track 2.
- Percentages are `pct_of_process_time` from xctrace Time Profiler exports.
- Source abbreviations resolve to the symbol plus file:line below.
- Canonical primitive names are grammar-neutral; implementation-specific JSON,
  hand, generated, typed, or serde symbols are evidence members under those
  primitives, not separate generic claims.

Canonical primitive bridge:

| Canonical primitive | Evidence members in this artifact |
|---|---|
| `bounded_plain_string_scan` | `tiny_string`, `hand_tiny`, `typed_tiny`, `typed_skip_plain`, `plain_string`, `hand_string` |
| `string_escape_decode` | `full_string`, `unescape`, `validate_escape`, `validate_unicode` |
| `unicode_escape_hex_decode` | `hex_unit`, `hex_nibble` |
| `number_digit_span` | `digits`, `number_span`, `serde_decimal`, `serde_integer` |
| `ascii_whitespace_skip` | `ws`, `skip_spaces`, `serde_ws` |
| `simd_movemask` | `movemask` |
| `container_dispatch` | `container_dispatch_object`, `sequence_element_dispatch`, `hand_value`, generated direct container functions |
| `output_digest_hash` | `fold_string`, `u64_add`, typed hash/state compare support |

Hot-leaf source map:

- `tiny_string`: `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<8>` at `skinny/crates/runtime/src/grammars/json/generated.rs:171`.
- `container_dispatch_object`: `runtime::generated_json::generated::parse_object_value_at_direct::<JsonDigestSink>` at `skinny/crates/runtime/src/grammars/json/generated.rs:468`.
- `sequence_element_dispatch`: `runtime::generated_json::generated::parse_array_element_at_direct::<JsonDigestSink>` at `skinny/crates/runtime/src/grammars/json/generated.rs:508`.
- `hand_tiny`: `<bbnf_bench::direct_struct::hand::HandParser>::tiny_plain_string` at `skinny/crates/bbnf-bench/src/direct_struct.rs:565`.
- `hand_string`: `<bbnf_bench::direct_struct::hand::HandParser>::string` at `skinny/crates/bbnf-bench/src/direct_struct.rs:541`.
- `hand_value`: `<bbnf_bench::direct_struct::hand::HandParser>::value` at `skinny/crates/bbnf-bench/src/direct_struct.rs:460`.
- `ws`: `parse_that_regex::skip_ascii_whitespace` at `skinny/crates/parse-that-regex/src/lib.rs:113`.
- `skip_spaces`: `parse_that_regex::skip_ascii_spaces` at `skinny/crates/parse-that-regex/src/lib.rs:128`.
- `full_string`: `parse_that_regex::match_string_at_quote_trusted_utf8` at `skinny/crates/parse-that-regex/src/lib.rs:162`.
- `plain_string`: `parse_that_regex::skip_string_plain_trusted` at `skinny/crates/parse-that-regex/src/lib.rs:547`.
- `unescape`: `parse_that_regex::unescape_string` at `skinny/crates/parse-that-regex/src/lib.rs:718`.
- `validate_escape`: `parse_that_regex::validate_string_escape` at `skinny/crates/parse-that-regex/src/lib.rs:284`.
- `validate_unicode`: `parse_that_regex::validate_unicode_escape_run` at `skinny/crates/parse-that-regex/src/lib.rs:347`.
- `hex_unit`: `parse_that_regex::read_hex_unit_scalar` at `skinny/crates/parse-that-regex/src/lib.rs:945`.
- `hex_nibble`: `parse_that_regex::hex_nibble` at `skinny/crates/parse-that-regex/src/lib.rs:959`.
- `digits`: `parse_that_regex::number::scan_digit_run` at `skinny/crates/parse-that-regex/src/number/mod.rs:106`.
- `number_span`: `parse_that_regex::number::match_number_span_from_first` at `skinny/crates/parse-that-regex/src/number/mod.rs:38`.
- `copy`: `core::ptr::copy_nonoverlapping::<u8>` at `/Users/mkbabb/.rustup/toolchains/nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr/mod.rs:531`.
- `split_checked`: `<[u8]>::split_at_checked` at `/Users/mkbabb/.rustup/toolchains/nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice/mod.rs:2153`.
- `u64_add`: `<u64>::wrapping_add` at `/Users/mkbabb/.rustup/toolchains/nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num/uint_macros.rs:2510`.
- `u16_tz`: `<u16>::trailing_zeros` at `/Users/mkbabb/.rustup/toolchains/nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num/uint_macros.rs:177`.
- `option_copied`: `<core::option::Option<&u8>>::copied` at `/Users/mkbabb/.rustup/toolchains/nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/option.rs:2134`, usually inlined from `skinny/crates/runtime/src/grammars/json/generated.rs:474` or `skinny/crates/bbnf-bench/src/direct_struct.rs:466`.
- `movemask`: `bbnf_simd::aarch64::movemask::movemask_u8x16` at `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4`.
- `fold_string`: `<bbnf_bench::direct_struct::JsonDirectDigest>::fold_string_scalar` at `skinny/crates/bbnf-bench/src/direct_struct.rs:123`.
- `typed_skip_value`: `<bbnf_bench::generated_real_typed::DirectParser>::skip_value` at `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1739`.
- `typed_skip_plain`: `<bbnf_bench::generated_real_typed::DirectParser>::skip_plain_string_end` at `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1825`.
- `typed_tiny`: `<bbnf_bench::generated_real_typed::DirectParser>::tiny_plain_string_end` at `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1811`.
- `ptr_eq`: `<core::ptr::non_null::NonNull<u8> as core::cmp::PartialEq>::eq` at `/Users/mkbabb/.rustup/toolchains/nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr/non_null.rs:1716`.
- `serde_ws`: `<serde_json::de::Deserializer<SliceRead>>::parse_whitespace` at `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/serde_json-1.0.149/src/de.rs:255`.
- `serde_skip_escape`: `<serde_json::read::SliceRead>::skip_to_escape` at `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/serde_json-1.0.149/src/read.rs:432`.
- `serde_peek`: `<serde_json::read::SliceRead as serde_json::read::Read>::peek` at `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/serde_json-1.0.149/src/read.rs:286`.
- `serde_discard`: `<serde_json::read::SliceRead as serde_json::read::Read>::discard` at `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/serde_json-1.0.149/src/read.rs:302`.
- `serde_decimal`: `<serde_json::de::Deserializer<SliceRead>>::parse_decimal` at `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/serde_json-1.0.149/src/de.rs:530`.
- `serde_integer`: `<serde_json::de::Deserializer<SliceRead>>::parse_integer` at `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/serde_json-1.0.149/src/de.rs:462`.

### Direct residual and W0-clamped rows

These 13 rows are the SK-V11 primary closure surface. `instruments`,
`numbers`, and `unicode_mixed` are W0-clamped `N-direct / NO-GO` rows even
where one fresh number clears the seed floor; they remain planning evidence,
not admissions.

| Corpus | W0 state | Criterion Mbps T1/T2/sonic | PMU c/B T1/T2 | T1 hot leaves | T2 hot leaves |
|---|---|---:|---:|---|---|
| `twitter` | residual | 11613 / 10816 / 15113 | 3.79 / 4.02 | 20.0% `tiny_string`; 17.6% `ws`; 14.5% `movemask` | 18.7% `hand_tiny`; 14.7% `ws`; 10.5% `movemask` |
| `canada` | residual | 10316 / 9819 / 11700 | 3.31 / 3.50 | 23.7% `digits`; 14.2% `sequence_element_dispatch`; 11.8% `copy` | 21.6% `digits`; 10.8% `ws`; 10.4% `copy` |
| `github_events` | residual | 11918 / 10596 / 14743 | 2.90 / 3.11 | 24.4% `tiny_string`; 15.2% `movemask`; 13.6% `ws` | 19.9% `hand_tiny`; 14.3% `movemask`; 9.7% `ws` |
| `update_center` | residual | 8187 / 7474 / 11064 | 4.81 / 5.74 | 26.3% `tiny_string`; 10.0% `movemask`; 7.9% `u64_add` | 22.3% `hand_tiny`; 12.3% `plain_string`; 10.9% `movemask` |
| `mesh` | near-floor residual | 8561 / 8652 / 9542 | 5.41 / 5.35 | 21.9% `sequence_element_dispatch`; 14.4% `digits`; 12.7% `ws` | 20.8% `ws`; 18.3% `digits`; 8.3% `copy` |
| `random` | near-floor residual | 7693 / 6949 / 8665 | 5.56 / 5.98 | 23.8% `tiny_string`; 17.9% `ws`; 6.6% `option_copied` | 20.2% `hand_tiny`; 16.9% `ws`; 8.5% `u64_add` |
| `gsoc-2018` | residual | 2665 / 2578 / 4110 | 3.11 / 3.23 | 22.9% `movemask`; 12.9% `split_checked`; 9.1% `tiny_string` | 21.6% `movemask`; 15.9% `plain_string`; 12.7% `split_checked` |
| `instruments` | W0-clamped non-admission | 11569 / 10736 / 9865 | 3.63 / 3.84 | 15.7% `tiny_string`; 15.7% `ws`; 12.4% `container_dispatch_object` | 19.7% `ws`; 13.1% `hand_tiny`; 7.7% `option_copied` |
| `numbers` | W0-clamped, T2 still short | 4479 / 2366 / 2667 | 3.79 / 3.95 | 26.8% `digits`; 13.4% `sequence_element_dispatch`; 10.1% `copy` | 27.5% `digits`; 11.5% `ws`; 9.8% `copy` |
| `unicode_mixed` | W0-clamped, T2 still short | 3753 / 2427 / 2846 | 9.04 / 9.22 | 28.0% `full_string`; 20.3% `unescape`; 12.9% `validate_escape` | 26.4% `full_string`; 18.4% `unescape`; 13.8% `validate_escape` |
| `unicode_escapes` | residual | 1345 / 1341 / 3785 | 7.20 / 7.24 | 25.1% `unescape`; 22.1% `full_string`; 10.3% `hex_unit` | 23.4% `unescape`; 22.0% `full_string`; 9.0% `hex_unit` |
| `distinct_values` | residual | 1750 / 1625 / 2923 | 5.53 / 6.17 | 22.1% `tiny_string`; 15.8% `ws`; 11.6% `fold_string` | 19.4% `hand_tiny`; 16.5% `ws`; 9.0% `option_copied` |
| `y_string_unicode` | residual | 1983 / 1029 / 4344 | 9.91 / 11.49 | 10.2% `hex_nibble`; 10.0% `hex_unit`; 7.5% `unescape` | 16.1% `hex_unit`; 10.3% `hex_nibble`; 8.4% `validate_unicode` |

Direct residual synthesis:

- String-heavy direct rows (`twitter`, `github_events`, `update_center`,
  `random`, `distinct_values`) are still tiny-string/plain-string, whitespace,
  and movemask dominated on both tracks. Track 2 swaps generated `tiny_string`
  for `hand_tiny`, but the primitive class stays the same.
- Numeric direct rows (`canada`, `mesh`, `numbers`, `marine_ik` as a guard)
  are digit-scan plus array-walk rows. `numbers` is W0-clamped: Track 1 clears
  the seed floor in the W0 table, but Track 2 remains short and the row is
  still `N-direct / NO-GO`.
- Unicode rows are a separate closure surface. `unicode_mixed`,
  `unicode_escapes`, and `y_string_unicode` burn time in `full_string`,
  `unescape`, `validate_escape`, and hex decode rather than in object/tape
  traversal. Their PMU costs are the highest direct rows here:
  `unicode_mixed` is 9.04/9.22 c/B and `y_string_unicode` is 9.91/11.49 c/B.
- `instruments` is W0-clamped even though both W0 throughput numbers clear the
  seed floor. The hot leaves are ordinary string/whitespace/object leaves; the
  row needs behavior-wave provenance, not a retrospective W0 admission.

### Direct guard rows

These four `direct_to_struct A / GO` rows are not closure targets, but they
bound any S-P2/S-P3 primitive that touches product-plane direct code.

| Corpus | W0 state | Criterion Mbps T1/T2/sonic | PMU c/B T1/T2 | T1 hot leaves | T2 hot leaves |
|---|---|---:|---:|---|---|
| `citm_catalog` | direct guard | 18563 / 17787 / 15530 | 2.05 / 2.15 | 25.9% `ws`; 14.3% `tiny_string`; 10.5% `copy` | 25.6% `ws`; 13.6% `hand_tiny`; 8.1% `skip_spaces` |
| `apache_builds` | direct guard | 11254 / 10189 / 10995 | 3.85 / 4.22 | 18.4% `u64_add`; 14.6% `tiny_string`; 11.6% `ws` | 15.2% `u64_add`; 14.1% `hand_tiny`; 12.7% `ws` |
| `marine_ik` | direct guard | 8938 / 9437 / 8473 | 4.91 / 4.91 | 17.3% `digits`; 16.3% `sequence_element_dispatch`; 11.2% `ws` | 17.8% `digits`; 15.1% `ws`; 7.9% `number_span` |
| `unicode_basic` | direct guard | 2299 / 2227 / 2353 | 3.84 / 4.30 | 15.7% `tiny_string`; 11.0% `ws`; 10.8% `u16_tz` | 15.7% `hand_tiny`; 12.7% `plain_string`; 11.4% `hand_string` |

Guard synthesis:

- `citm_catalog` is whitespace dominated while already admitted. It is a
  maintain row, not a direct closure row.
- `apache_builds` exposes hash/arithmetic and tiny-string cost but is already
  admitted. Treat it as a regression sentinel for digest-side changes.
- `marine_ik` is the numeric/array guard. It shares the numeric leaf family
  with `canada`, `mesh`, and `numbers`.
- `unicode_basic` is the unicode guard that does not look like
  `unicode_mixed` or `unicode_escapes`: it is mostly tiny/plain string and
  bit-position work, not escape materialization.

### Real-typed guard rows

`real_typed_struct` is a guard surface for SK-V11, not the primary closure
surface. All seven rows are `A / GO` in W0. The typed Track 2 profiles are
serde_json oracle profiles and are useful mainly as an independence check.

| Corpus | W0 typed Mbps T1/T2/sonic | PMU c/B T1/T2 | T1 hot leaves | T2 oracle hot leaves |
|---|---:|---:|---|---|
| `twitter` | 17740 / 15912 / 15010 | 2.07 / 2.27 | 39.2% `typed_skip_plain`; 14.9% `ws`; 10.5% `ptr_eq` | 15.1% `serde_ws`; 12.9% `serde_skip_escape`; 11.9% `serde_peek` |
| `citm_catalog` | 30539 / 17675 / 20726 | 0.99 / 1.90 | 35.7% `ws`; 18.0% `typed_skip_plain`; 11.1% `typed_skip_value` | 35.6% `serde_peek`; 34.5% `serde_ws`; 12.7% `serde_discard` |
| `apache_builds` | 8478 / 6892 / 8106 | 4.19 / 6.26 | 26.0% `ptr_eq`; 24.6% `typed_tiny`; 8.5% `ws` | 23.3% `ptr_eq`; 13.5% `core::str::validations::run_utf8_validation` at `/rustc/02c7f9bec0fd583160f8bcccb830216023b07bee/library/core/src/str/validations.rs:145`; 9.3% `u64_add` |
| `github_events` | 11871 / 12275 / 12224 | 2.67 / 3.35 | 28.5% `typed_skip_plain`; 20.5% `ptr_eq`; 10.9% `typed_tiny` | 19.7% `ptr_eq`; 10.6% `serde_skip_escape`; 10.2% `core::str::validations::run_utf8_validation` at `/rustc/02c7f9bec0fd583160f8bcccb830216023b07bee/library/core/src/str/validations.rs:145` |
| `update_center` | 11851 / 10358 / 12467 | 3.05 / 3.86 | 31.8% `typed_skip_plain`; 15.7% `typed_tiny`; 12.8% `ptr_eq` | 17.4% `core::str::validations::run_utf8_validation` at `/rustc/02c7f9bec0fd583160f8bcccb830216023b07bee/library/core/src/str/validations.rs:145`; 13.4% `ptr_eq`; 10.8% `serde_skip_escape` |
| `mesh` | 9403 / 7897 / 8923 | 4.84 / 5.83 | 24.0% `digits`; 14.2% `ws`; 11.5% `number_span` | 24.6% `serde_peek`; 15.6% `serde_discard`; 13.0% `serde_decimal` |
| `marine_ik` | 11788 / 10096 / 9010 | 3.60 / 3.94 | 17.3% `ws`; 17.1% `digits`; 13.5% `number_span` | 23.5% `serde_peek`; 16.2% `serde_discard`; 13.8% `serde_ws` |

Typed synthesis:

- The string-heavy typed Track 1 rows are `typed_skip_plain`/`typed_tiny`
  rows, with `ptr_eq` visible in product struct plumbing. This is a guard
  surface: moving direct string primitives must not erode these admissions.
- The numeric typed guards (`mesh`, `marine_ik`) share the `digits` and
  `number_span` primitive family with direct numeric residuals, but the typed
  rows are already admitted and should be maintained.
- Typed Track 2 is serde_json dominated by read/whitespace/peek/discard and
  numeric parse functions. It remains an independence/oracle surface, not a
  strict direct closure anchor.

## Section 3 - Delta vs SK-V10

P1-F owns the full per-row delta ledger. P1-B records only the product-plane
shape change that matters for hot-leaf interpretation:

| Surface | SK-V10 P1-B shape | SK-V11 W0 shape | P1-B consequence |
|---|---|---|---|
| `direct_to_struct` | 17 profiled rows, 14 `N-direct / NO-GO`, 3 direct guards | 17 profiled rows, 13 `N-direct / NO-GO`, 4 direct guards | Primary closure narrows by one row, but still spans string, numeric, unicode, and container leaves. |
| `real_typed_struct` | 6 typed guard rows profiled | 7 typed guard rows profiled | `github_events/real_typed_struct` is now a guard row and must be protected. |
| `parse_only` | diagnostic profile only | diagnostic concession only | Still not a SK-V11 target; no parse-only win can admit a product row. |

The direct residual floor gaps from W0 are unchanged by this profile artifact.
The profile adds attribution:

- Direct string residuals map to `tiny_string`, `hand_tiny`, `plain_string`,
  `ws`, and `movemask`.
- Direct numeric residuals map to `digits`, `number_span`,
  `sequence_element_dispatch`/`container_dispatch`, and `copy`.
- Direct unicode residuals map to `full_string`, `unescape`,
  `validate_escape`, `hex_unit`, and `hex_nibble`.
- Typed guards map to typed direct string skip, numeric scan, and serde_json
  oracle read/number routines.

## Section 4 - Anomalies And Masking Signals

- The samply JSON profiles are raw `symbolicated=false` captures. Their
  `.json.syms.json` files exist and resolve frames, but inlined direct leaves
  often collapse to container functions such as `container_dispatch_object` or
  `sequence_element_dispatch`.
  The xctrace `*.symbols.json` exports resolve the actionable scanner leaves
  more clearly, so this file uses xctrace for the tables and cites samply as
  profile coverage/source material.
- `profile_direct` includes 16 sanity parses before the timed loop. This is a
  product hot-loop profile and must not be treated as a cold Criterion
  admission run.
- `y_string_unicode` has low process share in xctrace Time Profiler
  (`track1` process samples 440 ms of 996 ms, `track2` 465 ms of 972 ms) and
  very small samply sample counts (41/44 samples). Its PMU c/B and xctrace
  unicode leaf shape are still useful, but the exact rank order of lower
  leaves is noisier than the larger corpora.
- `unicode_mixed` is W0-clamped and still Track 2 short. It is also the most
  expensive direct row by PMU after `y_string_unicode`, at 9.04/9.22 c/B, with
  hot leaves in full string scan and escape validation. Treat it separately
  from `unicode_basic`.
- `numbers` is W0-clamped and Track 2 still short. Its hot leaf is not unicode
  or string policy; it is digit scan plus sequence/container dispatch and
  copy, matching the numeric direct family.
- `instruments` is W0-clamped even though W0 throughput clears both seed
  floors. Its hot leaves are ordinary string/whitespace/object leaves. It
  cannot be admitted from W0 capture alone.
- No observation here reopens the SK-V9 W3 union/event/class-column/sidecar
  substrate family. REDRESS 50, 51, 53, 96, 97, 98, and 102 keep sidecar,
  cursor, class-column, streaming-cursor, retired-W3, and parse-only-firewall
  routes closed. Product-plane leaves are in the existing generated direct
  parser, hand Track 2 parser, parse-that-regex primitives, digest folding, and
  typed direct parser.

## Section 5 - Sources

- `/tmp/skv11-p1/direct-xctrace/exports/summary.json`
- `/tmp/skv11-p1/direct-xctrace/exports/*.symbols.json`
- `/tmp/skv11-p1/direct-xctrace/time-profiler/*.trace`
- `/tmp/skv11-p1/direct-xctrace/cpu-counters/*.trace`
- `/tmp/skv11-p1/direct-xctrace/logs/*.time-profiler.log`
- `/tmp/skv11-p1/direct-xctrace/logs/*.cpu-counters.log`
- `/tmp/skv11-p1/direct-xctrace/logs/*.pmu.log`
- `/tmp/skv11-p1/pmu/product_pmu_rows.tsv`
- `/tmp/skv11-p1/pmu/capture_status.tsv`
- `/tmp/skv11-p1/samply/direct/*.json.gz`
- `/tmp/skv11-p1/samply/direct/*.json.syms.json`
- `/tmp/skv11-p1/samply/typed/*.json.gz`
- `/tmp/skv11-p1/samply/typed/*.json.syms.json`
- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/tranches/sk-v11/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md`
- `skinny/RESULTS.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1b-samply-mode-2.md`
- `skinny/crates/bbnf-bench/src/bin/profile_direct.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/parse-that-regex/src/number/mod.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
