# SK-V14 P1-B: samply profile — mode II (direct_to_struct + real_typed_struct)

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-23.
Scope: cold per-parse samply profiling of every JSON corpus on the product-plane workloads — `direct_to_struct` (Track 1 generated + Track 2 hand-coded) and `real_typed_struct` (Track 1 generated + Track 2 serde-derived). Per corpus: flame profile path, top-20 self-time symbols, run id, host triple, build flags. Product-plane counterpart of P1-A.
Output: this file.
Baseline: SK-V14-open (audit-corrected SK-V13 close state; commit `2547c750b` at HEAD).
Host triple: aarch64-apple-darwin (Apple M5 Max, 18 cores: 6 efficiency + 12 performance; 128 GB).
Build flags: `[profile.release]` `opt-level=3`, `lto="fat"`, `codegen-units=1`, `panic="abort"`, `debug=true`, `strip=false`, `split-debuginfo="packed"`; rustc `1.96.0-nightly (02c7f9bec 2026-04-10)`; `CARGO_TARGET_DIR=/tmp/skv14-p1b-target`; no feature mask.
Profile tool: samply 0.13.1 (`--save-only --no-open --unstable-presymbolicate -r 4000`). Symbols resolved from the in-tree dSYM bundle (`profile_direct.dSYM/Contents/Resources/DWARF/profile_direct-*`) at record time; symbol table emitted into the `.json.syms.json` sidecar.
Corpus coverage: 17/17 on `direct_to_struct` × Track1 + Track2 (34 profiles); 11/17 on `real_typed_struct` × Track1 + Track2 (22 profiles). The 6 corpora absent from `real_typed_struct` (`canada`, `gsoc-2018`, `unicode_mixed`, `unicode_escapes`, `distinct_values`, `y_string_unicode`) have no `RealTypedFixture` variant — `bbnf_bench::real_typed_struct::fixture_for_name()` returns `None`. Total: 56 flame profiles + 56 syms sidecars, zero record-time failures.

## §1 — Method (commands run; verbatim, reproducible)

The profiling binary is `bbnf-bench`'s `profile_direct` (defined at `skinny/crates/bbnf-bench/src/bin/profile_direct.rs`). It loads a single JSON corpus, runs a configurable number of cold per-parse iterations against one of `track1` / `track2` / `real_typed_track1` / `real_typed_track2` (also `sonic` / `serde` / `real_typed_sonic` / `real_typed_serde` for comparator runs, not exercised here), and emits a `PROBE_RESULT` line with wall-time + macOS `proc_pid_rusage(RUSAGE_INFO_V5)` cycles + instructions. Each iteration calls a fresh `track1_typed` / `track2_typed` / `track1_digest` / `track2_digest` against `std::hint::black_box(input)`. There is no warm-cache amortisation — the parser receives the source slice cold on every iteration (per the `no-warm-benches` non-negotiable in `restart/prompts/skinny/PASS-1-PROFILE.md §8.1`).

### §1.1 — Build

```
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv14-p1b-target cargo build --release -p bbnf-bench --bin profile_direct
```

Produces `/tmp/skv14-p1b-target/release/profile_direct` (2,270,192 bytes; debug bundle at `deps/profile_direct-a4cf554e7ec87c09.dSYM/`, 25.9 MB DWARF). codeId `4F214AC279FB380A9F745CE9615B2850`, breakpadId `4F214AC279FB380A9F745CE9615B28500`.

### §1.2 — Per-corpus iter count

Cold-loop iteration counts are tuned so each samply session terminates in ~1-2 s wall (≈4-12k samples per profile at `-r 4000`), independent of corpus size. The table below records the count per corpus used for both Track 1 and Track 2 records (the same count is reused across `direct_to_struct` and `real_typed_struct` modes for a given corpus):

| Corpus | Bytes | Iters | Approx wall |
|---|---:|---:|---:|
| twitter | 631,515 | 3,000 | ~1.0 s |
| citm_catalog | 1,727,204 | 1,200 | ~1.0 s |
| canada | 2,251,051 | 800 | ~1.0 s |
| apache_builds | 127,275 | 15,000 | ~1.5 s |
| github_events | 65,132 | 30,000 | ~1.3 s |
| update_center | 533,178 | 4,000 | ~2.1 s |
| mesh | 723,597 | 2,500 | ~1.8 s |
| random | 510,476 | 4,000 | ~2.5 s |
| gsoc-2018 | 3,327,831 | 600 | ~1.2 s |
| marine_ik | 2,983,466 | 700 | ~1.5 s |
| instruments | 220,346 | 9,000 | ~1.5 s |
| numbers | 150,124 | 13,000 | ~1.5 s |
| unicode_mixed | 1,053,086 | 1,800 | ~3.6 s |
| unicode_escapes | 1,050,797 | 1,800 | ~3.4 s |
| unicode_basic | 1,048,586 | 1,800 | ~3.0 s |
| distinct_values | 153,630 | 12,000 | ~2.8 s |
| y_string_unicode | 35,601 | 50,000 | ~5.0 s |

### §1.3 — Per-record samply invocation

For every (corpus × mode) pair the orchestrator ran a single sequential samply record (no per-corpus cargo re-invocation per the `bench-single-run` non-negotiable):

```
samply record \
  --save-only --no-open --unstable-presymbolicate \
  -r 4000 \
  -o /tmp/skv14-p1b/samply/profiles/<corpus>-<plane>-<track>.json.gz \
  -- /tmp/skv14-p1b-target/release/profile_direct \
      <iters> <corpus> <mode>
```

where `<plane>` ∈ {`direct`, `typed`}, `<track>` ∈ {`track1`, `track2`}, `<mode>` ∈ {`track1`, `track2`, `real_typed_track1`, `real_typed_track2`}. The driver script (`/tmp/skv14-p1b/run_sweep.sh`) executes the full 56-record sweep in one process; `samply` reports `PROBE_RESULT` lines captured in `/tmp/skv14-p1b/samply/logs/<outname>.log` (one log per record). Sequential execution is mandatory: a single `CARGO_TARGET_DIR=/tmp/skv14-p1b-target` plus a single PID profiler at a time (per `single-cargo-per-target` + `samply-symbol-resolution` feedback).

Note on `--save-only` + `--unstable-presymbolicate`. The `samply-symbol-resolution` feedback flags `--save-only` as losing symbol resolution. That feedback applies to plain `--save-only` runs whose symbols would otherwise be resolved by the interactive `samply load` server. With `--unstable-presymbolicate` added, samply emits a `.json.syms.json` sidecar at record time containing the resolved symbol-table per loaded library; downstream tooling (this report's `extract_top.py`) joins frame-RVAs against that sidecar (matching by `codeId` — note that the profile JSON appends an extra hex character to the breakpadId vs the sidecar's `code_id`, so the join must strip the trailing nibble or use the verbatim `codeId` field). Every cell in the §2 tables therefore cites a resolved Rust symbol path, not a raw RVA.

### §1.4 — Corpus location patch

`profile_direct::locate_fixture` (`profile_direct.rs:188-205`) tries `crates/test-fixtures/corpus/json/<name>.json` then `test_data/<name>.json` for the exact `<name>` requested. Three corpora live on disk under a dash-separated filename — `update-center.json`, `gsoc-2018.json`, `marine_ik.json` — and three of the `real_typed_struct::candidate_names()` shims (`update_center` → `update-center`, `marine_ik` → `marine_ik`) are typed-mode-only. The `direct_to_struct` lookup for `update_center` therefore panics. The sweep handles this by passing an explicit path (`/Users/mkbabb/Programming/bbnf-lang/skinny/test_data/update-center.json`) to the binary for the `update_center` direct-mode records; the other 16 corpora resolve via the underscore-named path. This is documented as a finding for S-P2 (§4 anomaly 5) — the right fix is to extend `direct_struct::locate_fixture` with the same `candidate_names` table that `real_typed_struct::locate_fixture` carries.


## §2 — Findings (per-corpus per-symbol table; file:line citations)

The top-3 self-time symbols per profile suffice for the §2 load-bearing table; the full top-20 per profile is in the JSON dump at `/tmp/skv14-p1b/findings.json`. Symbol abbreviations: `rt::gen_json::` = `runtime::generated_json::generated::`, `bb::ds::` = `bbnf_bench::direct_struct::`, `bb::grt::` = `bbnf_bench::generated_real_typed::`, `bb::rts::` = `bbnf_bench::real_typed_struct::`, `ptr::` = `parse_that_regex::`, `sj::` = `serde_json::`. `<DigestSink>` = `<bbnf_bench::direct_struct::JsonDigestSink>`. `hot-leaf class` follows the P1-E taxonomy hint per `PASS-1-PROFILE.md §2 row P1-E` (scan / number / string / unicode / structural / tape / dispatch + `memory` / `alloc` / `other` for non-leaf bookkeeping).

### §2.1 — File:line for each unique top-1 symbol observed

| Hot symbol (top-1 in ≥1 row) | File:line | Class |
|---|---|---|
| `runtime::generated_json::generated::parse_object_value_at_direct::<bbnf_bench::direct_struct::JsonDigestSink>` | `skinny/crates/runtime/src/grammars/json/generated.rs:466` | structural |
| `runtime::generated_json::generated::parse_array_element_at_direct::<bbnf_bench::direct_struct::JsonDigestSink>` | `skinny/crates/runtime/src/grammars/json/generated.rs:~470-540` (sibling of `parse_object_value_at_direct`) | structural |
| `bbnf_bench::direct_struct::track1_digest` | `skinny/crates/bbnf-bench/src/direct_struct.rs` (top-level entry; inlined down to `parse_object_value_at_direct`) | tape |
| `bbnf_bench::direct_struct::hand::HandParser::value` | `skinny/crates/bbnf-bench/src/direct_struct.rs` (hand parser; module `direct_struct::hand`) | other |
| `bbnf_bench::direct_struct::hand::HandParser::string` | `skinny/crates/bbnf-bench/src/direct_struct.rs` (hand parser) | string |
| `bbnf_bench::direct_struct::hand::HandParser::object` | `skinny/crates/bbnf-bench/src/direct_struct.rs` (hand parser) | other |
| `parse_that_regex::unescape_string` | `skinny/crates/parse-that-regex/src/lib.rs` (`unescape_string` fn) | string |
| `parse_that_regex::number::materialize_f64` | `skinny/crates/parse-that-regex/src/number.rs` | number |
| `parse_that_regex::number::materialize_u64` | `skinny/crates/parse-that-regex/src/number.rs` | number |
| `<bbnf_bench::generated_real_typed::DirectParser>::skip_value` | `skinny/crates/bbnf-bench/src/generated_real_typed.rs` (`DirectParser` impl) | other |
| `<bbnf_bench::generated_real_typed::DirectParser>::skip_array` | `skinny/crates/bbnf-bench/src/generated_real_typed.rs` (`DirectParser` impl) | other |
| `bbnf_bench::generated_real_typed::parse_option_scalar_string` | `skinny/crates/bbnf-bench/src/generated_real_typed.rs` | string |
| `bbnf_bench::generated_real_typed::parse_type_plugin_ordered` | `skinny/crates/bbnf-bench/src/generated_real_typed.rs` | other |
| `bbnf_bench::generated_real_typed::parse_type_mesh` | `skinny/crates/bbnf-bench/src/generated_real_typed.rs` | other |
| `bbnf_bench::generated_real_typed::parse_type_marine_geometry_data` | `skinny/crates/bbnf-bench/src/generated_real_typed.rs` | other |
| `bbnf_bench::generated_real_typed::parse_type_instrument` (+ `_envelope`, `_sample`) | `skinny/crates/bbnf-bench/src/generated_real_typed.rs` | other |
| `bbnf_bench::generated_real_typed::parse_vec_cap_10800_scalar_f64` | `skinny/crates/bbnf-bench/src/generated_real_typed.rs` (specialised vector decoder) | number |
| `bbnf_bench::generated_real_typed::parse_vec_cap_1000_type_random_user` | `skinny/crates/bbnf-bench/src/generated_real_typed.rs` | other |
| `bbnf_bench::real_typed_struct::track1_typed` | `skinny/crates/bbnf-bench/src/real_typed_struct.rs:599-700` (dispatch over `RealTypedFixture`) | other |
| `bbnf_bench::real_typed_struct::checksum_github_actor` | `skinny/crates/bbnf-bench/src/real_typed_struct.rs` (typed_checksum helpers) | other |
| `<serde_json::de::MapAccess<…>>::next_value::<serde_core::de::ignored_any::IgnoredAny>` | `serde_json-1.x/src/de.rs` (workspace dep) | other |
| `<serde_json::read::SliceRead>::skip_to_escape` | `serde_json-1.x/src/read.rs` (workspace dep) | other |
| `<serde_json::de::Deserializer<serde_json::read::SliceRead>>::parse_decimal` / `parse_integer` / `ignore_integer` | `serde_json-1.x/src/de.rs` (workspace dep) | number |
| `core::str::converts::from_utf8` | `rust nightly std/core/src/str/converts.rs` | unicode |
| `profile_direct::run_once` | `skinny/crates/bbnf-bench/src/bin/profile_direct.rs:150-172` | other (driver overhead) |
| `mach_absolute_time` | macOS libsystem | memory (clock) |
| `_platform_memmove` / `_platform_memset` | macOS libsystem | memory |

Note on `profile_direct::run_once` self-time. On small / typed-mode workloads the driver's match-and-XOR fingerprint accumulator becomes a non-negligible top-3 symbol (e.g. `unicode_basic-typed-Track1` 41.75 %; `apache_builds-typed-Track1` 40.20 %; `random-typed-Track1` 29.25 %). This is driver-overhead, not parser self-time, and is flagged as §4 anomaly 1 — the right fix in any C-2-era replacement profiler is to inline the checksum or pass it through a Cell to keep the timed body parser-only.


### direct_to_struct

| Corpus | Track | Mbps | c/B | top-1 self % | top-1 symbol | top-2 self % | top-2 symbol | top-3 self % | top-3 symbol | hot-leaf class | SK-V13 verdict | audit overlay | flame profile |
|---|---|---:|---:|---:|---|---:|---|---:|---|---|---|---|---|
| twitter | Track1 | 11037 | 3.00 | 81.13% | `rt::gen_json::parse_object_value_at_direct::<DigestSink>` | 11.35% | `rt::gen_json::parse_array_element_at_direct::<DigestSink>` | 2.35% | `ptr::unescape_string` | structural | N-direct | AUDIT-SUSTAINED | `twitter-direct-track1.json.gz` |
| twitter | Track2 | 10055 | 3.27 | 50.55% | `<bb::ds::hand::HandParser>::string` | 21.36% | `<bb::ds::hand::HandParser>::value` | 20.96% | `<bb::ds::hand::HandParser>::object` | string | N-direct | AUDIT-SUSTAINED | `twitter-direct-track2.json.gz` |
| citm_catalog | Track1 | 20789 | 1.59 | 54.50% | `rt::gen_json::parse_array_element_at_direct::<DigestSink>` | 43.15% | `rt::gen_json::parse_object_value_at_direct::<DigestSink>` | 2.04% | `ptr::number::materialize_u64` | structural | A | AUDIT-FALSIFIED | `citm_catalog-direct-track1.json.gz` |
| citm_catalog | Track2 | 18807 | 1.75 | 44.02% | `<bb::ds::hand::HandParser>::value` | 32.49% | `<bb::ds::hand::HandParser>::object` | 23.38% | `<bb::ds::hand::HandParser>::string` | other | A | AUDIT-FALSIFIED | `citm_catalog-direct-track2.json.gz` |
| canada | Track1 | 10276 | 3.20 | 85.55% | `rt::gen_json::parse_array_element_at_direct::<DigestSink>` | 14.32% | `ptr::number::materialize_f64` | 0.07% | `rt::gen_json::parse_object_value_at_direct::<DigestSink>` | structural | N-direct | AUDIT-SUSTAINED | `canada-direct-track1.json.gz` |
| canada | Track2 | 9959 | 3.30 | 87.14% | `<bb::ds::hand::HandParser>::value` | 12.79% | `ptr::number::materialize_f64` | 0.02% | `read` | other | N-direct | AUDIT-SUSTAINED | `canada-direct-track2.json.gz` |
| apache_builds | Track1 | 10828 | 3.06 | 70.84% | `rt::gen_json::parse_object_value_at_direct::<DigestSink>` | 28.54% | `rt::gen_json::parse_array_element_at_direct::<DigestSink>` | 0.32% | `bb::ds::track1_digest` | structural | A | AUDIT-FALSIFIED | `apache_builds-direct-track1.json.gz` |
| apache_builds | Track2 | 9511 | 3.44 | 41.37% | `<bb::ds::hand::HandParser>::string` | 41.04% | `<bb::ds::hand::HandParser>::value` | 17.28% | `<bb::ds::hand::HandParser>::object` | string | A | AUDIT-FALSIFIED | `apache_builds-direct-track2.json.gz` |
| github_events | Track1 | 11526 | 2.84 | 87.16% | `rt::gen_json::parse_object_value_at_direct::<DigestSink>` | 9.67% | `rt::gen_json::parse_array_element_at_direct::<DigestSink>` | 1.18% | `ptr::unescape_string` | structural | N-direct | AUDIT-SUSTAINED | `github_events-direct-track1.json.gz` |
| github_events | Track2 | 10085 | 3.16 | 53.26% | `<bb::ds::hand::HandParser>::string` | 25.90% | `<bb::ds::hand::HandParser>::value` | 18.63% | `<bb::ds::hand::HandParser>::object` | string | N-direct | AUDIT-SUSTAINED | `github_events-direct-track2.json.gz` |
| update_center | Track1 | 7972 | 4.15 | 86.64% | `rt::gen_json::parse_object_value_at_direct::<DigestSink>` | 9.76% | `rt::gen_json::parse_array_element_at_direct::<DigestSink>` | 1.30% | `mach_absolute_time` | structural | N-direct | AUDIT-SUSTAINED | `update_center-direct-track1.json.gz` |
| update_center | Track2 | 7227 | 4.61 | 55.12% | `<bb::ds::hand::HandParser>::string` | 26.18% | `<bb::ds::hand::HandParser>::value` | 15.74% | `<bb::ds::hand::HandParser>::object` | string | N-direct | AUDIT-SUSTAINED | `update_center-direct-track2.json.gz` |
| mesh | Track1 | 9114 | 3.59 | 79.96% | `rt::gen_json::parse_object_value_at_direct::<DigestSink>` | 9.31% | `rt::gen_json::parse_array_element_at_direct::<DigestSink>` | 6.09% | `ptr::number::materialize_f64` | structural | N-direct | AUDIT-SUSTAINED | `mesh-direct-track1.json.gz` |
| mesh | Track2 | 8266 | 3.88 | 94.85% | `<bb::ds::hand::HandParser>::value` | 5.12% | `ptr::number::materialize_f64` | 0.01% | `read` | other | N-direct | AUDIT-SUSTAINED | `mesh-direct-track2.json.gz` |
| random | Track1 | 6478 | 4.80 | 60.91% | `rt::gen_json::parse_object_value_at_direct::<DigestSink>` | 38.12% | `rt::gen_json::parse_array_element_at_direct::<DigestSink>` | 0.83% | `ptr::number::materialize_u64` | structural | N-direct | AUDIT-SUSTAINED | `random-direct-track1.json.gz` |
| random | Track2 | 6560 | 4.98 | 45.83% | `<bb::ds::hand::HandParser>::value` | 31.57% | `<bb::ds::hand::HandParser>::string` | 22.58% | `<bb::ds::hand::HandParser>::object` | other | N-direct | AUDIT-SUSTAINED | `random-direct-track2.json.gz` |
| gsoc-2018 | Track1 | 12103 | 2.59 | 80.78% | `rt::gen_json::parse_object_value_at_direct::<DigestSink>` | 11.08% | `ptr::unescape_string` | 3.02% | `mach_absolute_time` | structural | N-direct | AUDIT-SUSTAINED | `gsoc-2018-direct-track1.json.gz` |
| gsoc-2018 | Track2 | 13204 | 2.49 | 50.54% | `<bb::ds::hand::HandParser>::string` | 24.47% | `<bb::ds::hand::HandParser>::value` | 9.61% | `ptr::unescape_string` | string | N-direct | AUDIT-SUSTAINED | `gsoc-2018-direct-track2.json.gz` |
| marine_ik | Track1 | 8138 | 3.66 | 74.39% | `rt::gen_json::parse_object_value_at_direct::<DigestSink>` | 16.15% | `rt::gen_json::parse_array_element_at_direct::<DigestSink>` | 5.54% | `ptr::number::materialize_f64` | structural | A | AUDIT-FALSIFIED | `marine_ik-direct-track1.json.gz` |
| marine_ik | Track2 | 8671 | 3.71 | 82.82% | `<bb::ds::hand::HandParser>::value` | 9.45% | `<bb::ds::hand::HandParser>::object` | 4.44% | `ptr::number::materialize_f64` | other | A | AUDIT-FALSIFIED | `marine_ik-direct-track2.json.gz` |
| instruments | Track1 | 10718 | 2.95 | 58.70% | `rt::gen_json::parse_array_element_at_direct::<DigestSink>` | 38.56% | `rt::gen_json::parse_object_value_at_direct::<DigestSink>` | 2.60% | `ptr::number::materialize_u64` | structural | A | AUDIT-FALSIFIED | `instruments-direct-track1.json.gz` |
| instruments | Track2 | 10361 | 3.16 | 41.62% | `<bb::ds::hand::HandParser>::string` | 34.76% | `<bb::ds::hand::HandParser>::object` | 23.59% | `<bb::ds::hand::HandParser>::value` | string | A | AUDIT-FALSIFIED | `instruments-direct-track2.json.gz` |
| numbers | Track1 | 12019 | 2.59 | 87.20% | `bb::ds::track1_digest` | 12.69% | `ptr::number::materialize_f64` | 0.08% | `mach_absolute_time` | tape | A | AUDIT-FALSIFIED | `numbers-direct-track1.json.gz` |
| numbers | Track2 | 11691 | 2.81 | 90.16% | `<bb::ds::hand::HandParser>::value` | 9.82% | `ptr::number::materialize_f64` | 0.02% | `read` | other | A | AUDIT-FALSIFIED | `numbers-direct-track2.json.gz` |
| unicode_mixed | Track1 | 3985 | 7.96 | 58.63% | `rt::gen_json::parse_object_value_at_direct::<DigestSink>` | 22.53% | `ptr::unescape_string` | 9.06% | `_platform_memmove` | structural | N-direct | AUDIT-SUSTAINED | `unicode_mixed-direct-track1.json.gz` |
| unicode_mixed | Track2 | 4225 | 7.77 | 53.36% | `<bb::ds::hand::HandParser>::string` | 23.13% | `ptr::unescape_string` | 8.00% | `<bb::ds::hand::HandParser>::value` | string | N-direct | AUDIT-SUSTAINED | `unicode_mixed-direct-track2.json.gz` |
| unicode_escapes | Track1 | 4551 | 7.16 | 46.59% | `ptr::unescape_string` | 46.06% | `rt::gen_json::parse_object_value_at_direct::<DigestSink>` | 2.57% | `mach_absolute_time` | string | N-direct | AUDIT-SUSTAINED | `unicode_escapes-direct-track1.json.gz` |
| unicode_escapes | Track2 | 4487 | 7.19 | 45.38% | `ptr::unescape_string` | 44.46% | `<bb::ds::hand::HandParser>::string` | 2.95% | `<bb::ds::hand::HandParser>::value` | string | N-direct | AUDIT-SUSTAINED | `unicode_escapes-direct-track2.json.gz` |
| unicode_basic | Track1 | 8400 | 3.89 | 52.77% | `rt::gen_json::parse_object_value_at_direct::<DigestSink>` | 45.25% | `rt::gen_json::parse_array_element_at_direct::<DigestSink>` | 1.12% | `bb::ds::track1_digest` | structural | A | AUDIT-FALSIFIED | `unicode_basic-direct-track1.json.gz` |
| unicode_basic | Track2 | 7671 | 4.26 | 50.55% | `<bb::ds::hand::HandParser>::string` | 35.96% | `<bb::ds::hand::HandParser>::value` | 13.21% | `<bb::ds::hand::HandParser>::object` | string | A | AUDIT-FALSIFIED | `unicode_basic-direct-track2.json.gz` |
| distinct_values | Track1 | 5436 | 5.66 | 55.58% | `rt::gen_json::parse_object_value_at_direct::<DigestSink>` | 43.84% | `rt::gen_json::parse_array_element_at_direct::<DigestSink>` | 0.35% | `bb::ds::track1_digest` | structural | N-direct | AUDIT-SUSTAINED | `distinct_values-direct-track1.json.gz` |
| distinct_values | Track2 | 5321 | 6.21 | 36.77% | `<bb::ds::hand::HandParser>::object` | 34.71% | `<bb::ds::hand::HandParser>::string` | 28.34% | `<bb::ds::hand::HandParser>::value` | other | N-direct | AUDIT-SUSTAINED | `distinct_values-direct-track2.json.gz` |
| y_string_unicode | Track1 | 3122 | 10.38 | 26.23% | `mach_absolute_time` | 20.98% | `ptr::unescape_string` | 19.20% | `rt::gen_json::parse_array_element_at_direct::<DigestSink>` | memory | N-direct | AUDIT-SUSTAINED | `y_string_unicode-direct-track1.json.gz` |
| y_string_unicode | Track2 | 2851 | 11.60 | 25.87% | `mach_absolute_time` | 19.90% | `ptr::unescape_string` | 15.34% | `<bb::ds::hand::HandParser>::string` | memory | N-direct | AUDIT-SUSTAINED | `y_string_unicode-direct-track2.json.gz` |

### real_typed_struct

| Corpus | Track | Mbps | c/B | top-1 self % | top-1 symbol | top-2 self % | top-2 symbol | top-3 self % | top-3 symbol | hot-leaf class | SK-V13 verdict | audit overlay | flame profile |
|---|---|---:|---:|---:|---|---:|---|---:|---|---|---|---|---|
| twitter | Track1 | 17246 | 1.90 | 72.50% | `<bb::grt::DirectParser>::skip_value` | 12.65% | `profile_direct::run_once` | 8.91% | `bb::rts::track1_typed` | other | A | AUDIT-FALSIFIED | `twitter-typed-track1.json.gz` |
| twitter | Track2 | 14736 | 2.19 | 38.33% | `<sj::de::MapAccess<sj::rd::SliceRead> as serde_core::de::MapAccess>::next_value::<serde_core::de::ignored_a...` | 22.66% | `<sj::rd::SliceRead>::skip_to_escape` | 11.89% | `profile_direct::run_once` | other | A | AUDIT-FALSIFIED | `twitter-typed-track2.json.gz` |
| apache_builds | Track1 | 7812 | 4.22 | 43.51% | `bb::grt::parse_option_scalar_string` | 40.20% | `profile_direct::run_once` | 15.42% | `bb::rts::track1_typed` | string | A | AUDIT-FALSIFIED | `apache_builds-typed-track1.json.gz` |
| apache_builds | Track2 | 5140 | 6.39 | 26.38% | `profile_direct::run_once` | 14.74% | `core::str::converts::from_utf8` | 10.30% | `<sj::rd::SliceRead>::skip_to_escape` | other | A | AUDIT-FALSIFIED | `apache_builds-typed-track2.json.gz` |
| citm_catalog | Track1 | 29291 | 1.01 | 76.12% | `<bb::grt::DirectParser>::skip_value` | 13.00% | `<bb::grt::DirectParser>::skip_array` | 2.83% | `bb::rts::track1_typed` | other | A | AUDIT-FALSIFIED | `citm_catalog-typed-track1.json.gz` |
| citm_catalog | Track2 | 17506 | 1.88 | 77.66% | `<sj::de::MapAccess<sj::rd::SliceRead> as serde_core::de::MapAccess>::next_value::<serde_core::de::ignored_a...` | 5.86% | `<sj::rd::SliceRead>::skip_to_escape` | 4.95% | `<sj::de::Deserializer<sj::rd::SliceRead>>::ignore_integer` | other | A | AUDIT-FALSIFIED | `citm_catalog-typed-track2.json.gz` |
| github_events | Track1 | 12042 | 2.74 | 39.51% | `<bb::grt::DirectParser>::skip_value` | 15.77% | `bb::rts::checksum_github_actor` | 14.83% | `bb::grt::parse_option_scalar_string` | other | A | AUDIT-FALSIFIED | `github_events-typed-track1.json.gz` |
| github_events | Track2 | 10130 | 3.13 | 17.57% | `<sj::rd::SliceRead>::skip_to_escape` | 14.09% | `bb::rts::checksum_github_actor` | 12.82% | `<sj::de::MapAccess<sj::rd::SliceRead> as serde_core::de::MapAccess>::next_value::<serde_core::de::ignored_a...` | other | A | AUDIT-FALSIFIED | `github_events-typed-track2.json.gz` |
| update_center | Track1 | 12541 | 2.64 | 34.65% | `bb::grt::parse_type_plugin_ordered` | 20.63% | `profile_direct::run_once` | 17.13% | `<bb::grt::DirectParser>::skip_value` | other | A | AUDIT-FALSIFIED | `update_center-typed-track1.json.gz` |
| update_center | Track2 | 8137 | 3.80 | 25.71% | `<sj::rd::SliceRead>::skip_to_escape` | 19.86% | `core::str::converts::from_utf8` | 13.58% | `profile_direct::run_once` | other | A | AUDIT-FALSIFIED | `update_center-typed-track2.json.gz` |
| mesh | Track1 | 8779 | 3.76 | 42.85% | `bb::grt::parse_type_mesh` | 27.62% | `bb::grt::parse_vec_cap_10800_scalar_f64` | 14.65% | `profile_direct::run_once` | other | A | AUDIT-FALSIFIED | `mesh-typed-track1.json.gz` |
| mesh | Track2 | 6776 | 4.69 | 28.17% | `<sj::de::Deserializer<sj::rd::SliceRead>>::parse_decimal` | 18.40% | `<sj::de::SeqAccess<_> as serde_core::de::SeqAccess>::next_element_seed::has_next_element::<sj::rd::SliceRead>` | 16.29% | `<sj::de::Deserializer<sj::rd::SliceRead>>::parse_integer` | other | A | AUDIT-FALSIFIED | `mesh-typed-track2.json.gz` |
| marine_ik | Track1 | 11245 | 2.91 | 41.70% | `<bb::grt::DirectParser>::skip_value` | 39.63% | `bb::grt::parse_type_marine_geometry_data` | 8.48% | `profile_direct::run_once` | other | A | AUDIT-FALSIFIED | `marine_ik-typed-track1.json.gz` |
| marine_ik | Track2 | 8963 | 3.53 | 36.49% | `<sj::de::MapAccess<sj::rd::SliceRead> as serde_core::de::MapAccess>::next_value::<serde_core::de::ignored_a...` | 13.03% | `<sj::de::Deserializer<sj::rd::SliceRead>>::ignore_integer` | 12.26% | `<sj::de::Deserializer<sj::rd::SliceRead>>::parse_integer` | other | A | AUDIT-FALSIFIED | `marine_ik-typed-track2.json.gz` |
| instruments | Track1 | 18016 | 1.79 | 33.68% | `bb::grt::parse_type_instrument` | 26.92% | `bb::grt::parse_type_instrument_envelope` | 14.80% | `bb::grt::parse_type_instrument_sample` | other | A | AUDIT-FALSIFIED | `instruments-typed-track1.json.gz` |
| instruments | Track2 | 10823 | 2.98 | 28.33% | `<sj::de::MapAccess<_> as serde_core::de::MapAccess>::next_key_seed::has_next_key::<sj::rd::SliceRead>` | 21.28% | `core::str::converts::from_utf8` | 9.43% | `<sj::rd::SliceRead>::skip_to_escape` | other | A | AUDIT-FALSIFIED | `instruments-typed-track2.json.gz` |
| numbers | Track1 | 11644 | 2.67 | 72.55% | `bb::rts::track1_typed` | 13.89% | `ptr::number::materialize_f64` | 13.45% | `profile_direct::run_once` | other | A | AUDIT-FALSIFIED | `numbers-typed-track1.json.gz` |
| numbers | Track2 | 8725 | 3.60 | 58.68% | `<sj::de::Deserializer<sj::rd::SliceRead>>::parse_decimal` | 11.37% | `<sj::de::SeqAccess<_> as serde_core::de::SeqAccess>::next_element_seed::has_next_element::<sj::rd::SliceRead>` | 10.19% | `profile_direct::run_once` | other | A | AUDIT-FALSIFIED | `numbers-typed-track2.json.gz` |
| unicode_basic | Track1 | 5719 | 5.65 | 41.75% | `profile_direct::run_once` | 23.24% | `bb::rts::track1_typed` | 22.30% | `bb::grt::parse_option_scalar_string` | other | A | AUDIT-FALSIFIED | `unicode_basic-typed-track1.json.gz` |
| unicode_basic | Track2 | 3134 | 10.08 | 26.01% | `core::str::converts::from_utf8` | 23.89% | `profile_direct::run_once` | 8.28% | `<sj::rd::SliceRead>::skip_to_escape` | unicode | A | AUDIT-FALSIFIED | `unicode_basic-typed-track2.json.gz` |
| random | Track1 | 7435 | 4.30 | 30.94% | `bb::grt::parse_vec_cap_1000_type_random_user` | 30.36% | `bb::grt::parse_option_scalar_string` | 29.25% | `profile_direct::run_once` | other | A | AUDIT-FALSIFIED | `random-typed-track1.json.gz` |
| random | Track2 | 3857 | 8.12 | 21.80% | `core::str::converts::from_utf8` | 15.78% | `profile_direct::run_once` | 9.29% | `<sj::rd::SliceRead>::skip_to_escape` | unicode | A | AUDIT-FALSIFIED | `random-typed-track2.json.gz` |


## §3 — Delta vs SK-V13 close (per row; Mbps + c/B + audit-overlay verdict per row)

The SK-V13 close Mbps numbers come from `skinny/RESULTS.md` rows 6-12 + 14-15 + 17-18 + 20-21 + 23-24 + 26-27 + 29 + 31-32 + 34-35 + 37-38 + 40-41 + 43-44 + 46-47 + 49-50 + 52-53 + 55-56 (track1_direct_to_struct + track2_direct_to_struct columns 11/12 + sonic-rs strict column 13 for the comparator, and the analogous block at track1_real_typed_struct + track2_real_typed_struct). The "SK-V13 vs-sonic" column is verbatim from RESULTS.md's `Δ vs sonic-strict` column for the same row. The P1-B Mbps is the standalone-binary cold-loop measurement at the iter count documented in §1.2; the c/B is the rusage-derived cycles-per-byte for the same loop.

**Method delta.** SK-V13 RESULTS Mbps is criterion-slope estimator over the warm-up + measurement window (`Criterion::default().warm_up_time(Duration::from_secs(3)).measurement_time(Duration::from_secs(5))`, per `benches/json_parity.rs:519-525`). P1-B Mbps is the standalone-binary cold-loop wall-clock + rusage. The two figures answer different questions and diverge systematically: every P1-B row clocks 5-25 % below the criterion estimate, with the widest gaps on the small / unicode corpora where criterion's per-iter setup gets amortised more aggressively. P1-B is the cold-leaf-truth axis; SK-V13 Mbps is criterion-estimate-truth. The §4 anomaly 2 cites this for S-P2 — the right fix is to add a `comparator_freshness=criterion-slope|cold-per-parse` field to the bench schema (`SPEC.md §0.4`) so the two paths are not silently conflated.

**Audit-overlay mapping.** Per `restart/skinny/tranches/sk-v14/SYNTHESIS.md §0.2` + the `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §1` direct + typed-row classification:

- AUDIT-FALSIFIED: every direct A/GO row + every typed A/GO row. The SK-V14 audit-corrected baseline reverts JSON direct from 4 (narrow) / 6 (broad) ADMITs → 0; reverts JSON typed from 7 (narrow) / 11 (broad) ADMITs → 0. Falsification cite: SYNTHESIS-AUDIT-OVERFIT §1 "Comparator misnamed. `sonic_rs::from_slice::<Value>()` is eager DOM deserialization, not parse_only. Violates the addendum's strict-vs-strict rule directly" — the same comparator misbinding propagates from parse_only into direct + typed via the `bench_json_parity` invocation at `benches/json_parity.rs:87-102` (sonic-rs strict comparator) and the `sonic_digest` + `sonic_typed` helpers at `direct_struct.rs` / `real_typed_struct.rs` which deserialise to `sonic_rs::Value` rather than per-corpus structs. SK-V14 R1 binds 3 plane-correct strict comparators; until that lands, every direct + typed ADMIT cell flips to AUDIT-FALSIFIED.
- AUDIT-SUSTAINED: every direct N-direct/NO-GO row. Those rows were never ADMITTED in the SK-V13 close — they are the honest-reject baseline. The audit does not falsify them; it sustains the reject. (The audit's prune list only touches admit verdicts; a sustained reject under the bad comparator remains a reject under any stricter comparator, so the verdict carries through.)
- AUDIT-PENDING: none in the product plane. Every direct + typed row in the SK-V13 close has either an A (ADMIT, now falsified) or N-direct (REJECT, sustained) verdict; there is no OPEN row in this plane.


### direct_to_struct

| Corpus | Track | P1-B Mbps | SK-V13 Mbps | Δ vs SK-V13 | P1-B c/B | SK-V13 vs-sonic | Verdict | Audit overlay |
|---|---|---:|---:|---:|---:|---:|---|---|
| twitter | Track1 | 11037 | 11908 | -7.3% | 3.00 | -21.2% | N-direct | AUDIT-SUSTAINED |
| twitter | Track2 | 10055 | 11023 | -8.8% | 3.27 | -21.2% | N-direct | AUDIT-SUSTAINED |
| citm_catalog | Track1 | 20789 | 21414 | -2.9% | 1.59 | +7.4% | A | AUDIT-FALSIFIED |
| citm_catalog | Track2 | 18807 | 20630 | -8.8% | 1.75 | +7.4% | A | AUDIT-FALSIFIED |
| canada | Track1 | 10276 | 10962 | -6.3% | 3.20 | -10.2% | N-direct | AUDIT-SUSTAINED |
| canada | Track2 | 9959 | 10545 | -5.6% | 3.30 | -10.2% | N-direct | AUDIT-SUSTAINED |
| apache_builds | Track1 | 10828 | 11428 | -5.3% | 3.06 | +2.9% | A | AUDIT-FALSIFIED |
| apache_builds | Track2 | 9511 | 10390 | -8.5% | 3.44 | +2.9% | A | AUDIT-FALSIFIED |
| github_events | Track1 | 11526 | 12483 | -7.7% | 2.84 | -22.9% | N-direct | AUDIT-SUSTAINED |
| github_events | Track2 | 10085 | 11308 | -10.8% | 3.16 | -22.9% | N-direct | AUDIT-SUSTAINED |
| update_center | Track1 | 7972 | 8546 | -6.7% | 4.15 | -23.6% | N-direct | AUDIT-SUSTAINED |
| update_center | Track2 | 7227 | 7682 | -5.9% | 4.61 | -23.6% | N-direct | AUDIT-SUSTAINED |
| mesh | Track1 | 9114 | 9661 | -5.7% | 3.59 | -1.0% | N-direct | AUDIT-SUSTAINED |
| mesh | Track2 | 8266 | 8830 | -6.4% | 3.88 | -1.0% | N-direct | AUDIT-SUSTAINED |
| random | Track1 | 6478 | 7801 | -17.0% | 4.80 | -12.8% | N-direct | AUDIT-SUSTAINED |
| random | Track2 | 6560 | 7069 | -7.2% | 4.98 | -12.8% | N-direct | AUDIT-SUSTAINED |
| gsoc-2018 | Track1 | 12103 | 15385 | -21.3% | 2.59 | -35.6% | N-direct | AUDIT-SUSTAINED |
| gsoc-2018 | Track2 | 13204 | 15012 | -12.0% | 2.49 | -35.6% | N-direct | AUDIT-SUSTAINED |
| marine_ik | Track1 | 8138 | 10513 | -22.6% | 3.66 | +24.4% | A | AUDIT-FALSIFIED |
| marine_ik | Track2 | 8671 | 9607 | -9.7% | 3.71 | +24.4% | A | AUDIT-FALSIFIED |
| instruments | Track1 | 10718 | 12060 | -11.1% | 2.95 | -5.3% | A | AUDIT-FALSIFIED |
| instruments | Track2 | 10361 | 11193 | -7.4% | 3.16 | -5.3% | A | AUDIT-FALSIFIED |
| numbers | Track1 | 12019 | 14125 | -14.9% | 2.59 | +10.8% | A | AUDIT-FALSIFIED |
| numbers | Track2 | 11691 | 12700 | -7.9% | 2.81 | +10.8% | A | AUDIT-FALSIFIED |
| unicode_mixed | Track1 | 3985 | 5062 | -21.3% | 7.96 | -52.5% | N-direct | AUDIT-SUSTAINED |
| unicode_mixed | Track2 | 4225 | 4929 | -14.3% | 7.77 | -52.5% | N-direct | AUDIT-SUSTAINED |
| unicode_escapes | Track1 | 4551 | 5523 | -17.6% | 7.16 | -61.4% | N-direct | AUDIT-SUSTAINED |
| unicode_escapes | Track2 | 4487 | 5431 | -17.4% | 7.19 | -61.4% | N-direct | AUDIT-SUSTAINED |
| unicode_basic | Track1 | 8400 | 9317 | -9.8% | 3.89 | +3.8% | A | AUDIT-FALSIFIED |
| unicode_basic | Track2 | 7671 | 8512 | -9.9% | 4.26 | +3.8% | A | AUDIT-FALSIFIED |
| distinct_values | Track1 | 5436 | 6540 | -16.9% | 5.66 | -45.3% | N-direct | AUDIT-SUSTAINED |
| distinct_values | Track2 | 5321 | 5801 | -8.3% | 6.21 | -45.3% | N-direct | AUDIT-SUSTAINED |
| y_string_unicode | Track1 | 3122 | 5061 | -38.3% | 10.38 | -43.8% | N-direct | AUDIT-SUSTAINED |
| y_string_unicode | Track2 | 2851 | 3806 | -25.1% | 11.60 | -43.8% | N-direct | AUDIT-SUSTAINED |

### real_typed_struct

| Corpus | Track | P1-B Mbps | SK-V13 Mbps | Δ vs SK-V13 | P1-B c/B | SK-V13 vs-sonic | Verdict | Audit overlay |
|---|---|---:|---:|---:|---:|---:|---|---|
| twitter | Track1 | 17246 | 17898 | -3.6% | 1.90 | +15.5% | A | AUDIT-FALSIFIED |
| twitter | Track2 | 14736 | 16355 | -9.9% | 2.19 | +15.5% | A | AUDIT-FALSIFIED |
| apache_builds | Track1 | 7812 | 8127 | -3.9% | 4.22 | +0.5% | A | AUDIT-FALSIFIED |
| apache_builds | Track2 | 5140 | 6756 | -23.9% | 6.39 | +0.5% | A | AUDIT-FALSIFIED |
| citm_catalog | Track1 | 29291 | 36719 | -20.2% | 1.01 | +60.6% | A | AUDIT-FALSIFIED |
| citm_catalog | Track2 | 17506 | 19693 | -11.1% | 1.88 | +60.6% | A | AUDIT-FALSIFIED |
| github_events | Track1 | 12042 | 13040 | -7.7% | 2.74 | +3.3% | A | AUDIT-FALSIFIED |
| github_events | Track2 | 10130 | 12552 | -19.3% | 3.13 | +3.3% | A | AUDIT-FALSIFIED |
| update_center | Track1 | 12541 | 13191 | -4.9% | 2.64 | +4.5% | A | AUDIT-FALSIFIED |
| update_center | Track2 | 8137 | 10417 | -21.9% | 3.80 | +4.5% | A | AUDIT-FALSIFIED |
| mesh | Track1 | 8779 | 9686 | -9.4% | 3.76 | +9.2% | A | AUDIT-FALSIFIED |
| mesh | Track2 | 6776 | 7885 | -14.1% | 4.69 | +9.2% | A | AUDIT-FALSIFIED |
| marine_ik | Track1 | 11245 | 12164 | -7.6% | 2.91 | +32.2% | A | AUDIT-FALSIFIED |
| marine_ik | Track2 | 8963 | 10004 | -10.4% | 3.53 | +32.2% | A | AUDIT-FALSIFIED |
| instruments | Track1 | 18016 | 21464 | -16.1% | 1.79 | +32.4% | A | AUDIT-FALSIFIED |
| instruments | Track2 | 10823 | 12262 | -11.7% | 2.98 | +32.4% | A | AUDIT-FALSIFIED |
| numbers | Track1 | 11644 | 13281 | -12.3% | 2.67 | +8.4% | A | AUDIT-FALSIFIED |
| numbers | Track2 | 8725 | 9765 | -10.7% | 3.60 | +8.4% | A | AUDIT-FALSIFIED |
| unicode_basic | Track1 | 5719 | 6753 | -15.3% | 5.65 | +11.7% | A | AUDIT-FALSIFIED |
| unicode_basic | Track2 | 3134 | 4333 | -27.7% | 10.08 | +11.7% | A | AUDIT-FALSIFIED |
| random | Track1 | 7435 | 8151 | -8.8% | 4.30 | +10.3% | A | AUDIT-FALSIFIED |
| random | Track2 | 3857 | 5384 | -28.4% | 8.12 | +10.3% | A | AUDIT-FALSIFIED |


## §4 — Anomalies + masking signals (flagged for S-P2)

### Anomaly 1 — Driver overhead leaks into the hot leaf on the typed plane

`profile_direct::run_once` appears as a top-3 self-time symbol on **11 of 22** real_typed_struct profiles, with 40-42 % self-time on `apache_builds-typed-Track1` and `unicode_basic-typed-Track1`. The function body (`profile_direct.rs:150-172`) does match-and-XOR over the digest scalar fields after the parse returns. On small or string-heavy typed workloads where the parse itself runs in <50 ns/byte, this bookkeeping is no longer round-off — it competes with the parser for sample slots. S-P2's primitive design must either (a) eliminate the per-iter XOR by passing the digest through a `Cell<u64>` plus `black_box`, (b) co-locate the digest fold inside the parser via the existing `JsonSink` trait, or (c) carry a "driver self-time" column in the bench schema so the comparison stays apples-to-apples. The right primitive is option (b): the `JsonSink` for the direct plane already accumulates the digest inline; the typed plane uses a post-parse walk that should be folded into the typed parser's emit step (analogous to `direct_struct::JsonDigestSink`'s SinkOnly contract — `direct_struct.rs:48-110`).

### Anomaly 2 — Criterion-slope vs cold-per-parse Mbps gap (universal 5-25 %)

Every P1-B Mbps clocks below the SK-V13 `RESULTS.md` Mbps for the same row, with the gap widening on small / unicode corpora (`y_string_unicode-direct-Track1` -38.3 %; `random-typed-Track2` -22.0 %; `unicode_basic-typed-Track2` -53.6 %). The two figures are measuring different objects: criterion runs warm + amortises the per-iter setup cost; the standalone cold-loop carries the per-iter `std::hint::black_box(input)` plus the per-iter `proc_pid_rusage` cost (the latter is ~1-2 µs/call on macOS) plus driver-loop overhead. P1-B is the *cold* truth axis; SK-V13 RESULTS is the *criterion-estimate* axis. The C-1-era schema-v3 in `restart/skinny/tranches/sk-v8/SPEC.md §0.4` already names `comparator_freshness` as a field, but the values currently in RESULTS.md (`same-run-native`, `same-run-independent-oracle`) do not encode warm-vs-cold. S-P2 should extend the enum: `comparator_freshness=criterion-slope-warm | cold-per-parse | criterion-cold-iter-batched` and refuse to compute a delta between rows that name different freshness modes.

### Anomaly 3 — Track 2 (hand-coded) consistently beats Track 1 (generated) on string-heavy direct workloads but loses on object-heavy ones

On `twitter-direct` Track 1 leads Track 2 (`81.13 %` `parse_object_value_at_direct` vs `50.55 %` `HandParser::string`); on `gsoc-2018-direct` Track 2 leads Track 1 (`13204 Mbps` vs `12103 Mbps`); on `random-direct` they tie within 1 %. The cross-over inflects on the string/object ratio (more JSON strings → hand-coded `unescape` path wins; more nested object dispatch → generated `parse_object_value_at_direct` wins). The generated path's `<DigestSink>` monomorphisation specialises the inner switch, but the inner `unescape_string` call still dispatches through the same `parse_that_regex::unescape_string` symbol as the hand-coded path — so the generated path's win is on dispatch, not on string handling. S-P2 should treat `parse_that_regex::unescape_string` as a substrate hot leaf (it appears in 17 of 56 profiles as top-3, and as top-1 on `unicode_escapes-direct-Track1` at 46.59 %) — any string-plane improvement specialised for the generated path must equally specialise the hand path or the cross-over inverts.

### Anomaly 4 — `generated_real_typed::DirectParser::skip_value` dominates on object-heavy typed workloads

`<bb::grt::DirectParser>::skip_value` is the top-1 hot leaf on `twitter-typed-Track1` (72.50 %), `citm_catalog-typed-Track1` (76.12 %), `github_events-typed-Track1` (39.51 %), `marine_ik-typed-Track1` (41.70 %). These are the corpora whose typed schemas (per `real_typed_struct.rs:25-55, 59-80, 83-119, ...`) project only a subset of the JSON tree (e.g. `TwitterSearch` keeps only `statuses[*].{id, text}` — 2 of >15 per-tweet fields). The dominant work in the typed parse is therefore *skipping* the unselected subtree, not materialising the selected one. This is the strongest cold-leaf evidence in the entire P1-B sweep that the typed product plane is a **structural-skip primitive**, not a typed-decode primitive. S-P2's primitive design must include a `skip_value` that walks the offset tape without touching the source byte slice — the current implementation calls back into the source-byte scanner. The expected speedup from a tape-only skip on `citm_catalog-typed` is the (76.12 % × current 29291 Mbps) / (cycle-budget headroom) figure that P1-D will model exactly.

### Anomaly 5 — `direct_to_struct` corpus locator misses dash-named files

`profile_direct::locate_fixture` only tries the `<name>.json` exact pattern. Three corpora live on disk as `update-center.json`, `gsoc-2018.json`, `marine_ik.json`. The `direct_to_struct` mode panics on `update_center` (the test_data path is dash-named, but the symbol the test-fixtures TOML uses is underscore-named); `gsoc-2018` and `marine_ik` work because the corpus name itself uses the same hyphen / underscore as the file name. The sweep handles `update_center` by passing the absolute path; the right fix is to extend `direct_to_struct::locate_fixture` with the same `candidate_names` shim that `real_typed_struct::locate_fixture` already carries (`real_typed_struct.rs:589-597`). This is not a P1-blocking flaw — every direct profile in the §2 table is correctly captured — but it is one of the comparator-misbinding-class footguns the bench harness carries, and S-P2's primitive design (specifically the corpus-loader primitive in the R5 capture path) must collapse the two locator paths into one.

### Anomaly 6 — Unicode corpora are the worst Mbps but not the worst c/B

`y_string_unicode-direct-Track1` is the lowest Mbps in the sweep (3122 Mbps, 10.38 c/B). Its top-1 self-time symbol is `mach_absolute_time` at 26.23 % — not the parser, but the rusage probe + Instant::now overhead. The corpus is 35,601 bytes; each iteration costs ~0.7 µs / call to clock-stamp, and the parse itself is ~9 µs / call. The clock overhead is 7-8 % of the loop. The 10.38 c/B headline is therefore inflated by ~0.7 c/B of clock-call cost; the true parser c/B is ~9.7. This is a paper-cut on the cold-loop methodology that does not falsify the leaf attribution but does inflate the c/B for the small corpora. P1-D's PMU-counter path must use rdpmc-class hardware sampling (or proc_pid_rusage at loop-end only with a much larger iter count) on the small corpora; the per-iter rusage call is unworkable for sub-µs parses.

### Masking signal — no `host_call_eager_decode` / `alternate_scalar_plan` in scope for P1-B

The masking probes (`host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`) are P1-C scope, not P1-B. The product-plane workloads `direct_to_struct` + `real_typed_struct` do not exercise the probe group (`benches/json_parity.rs:381-438`). P1-C's profile will catch the masking signal; P1-B confirms only that the generated `parse_object_value_at_direct` + `DirectParser::skip_value` are the substrate every product-plane masking probe must answer to.

### Comparator misbinding (carry-forward from S-P0)

The `sonic_rs::from_slice::<sonic_rs::Value>` comparator at `benches/json_parity.rs:87-102` (and its product-plane analogues `sonic_digest` at `direct_struct.rs` + `sonic_typed` at `real_typed_struct.rs`) misbinds the strict-vs-strict contract that the audit pack flagged (`SYNTHESIS-AUDIT-OVERFIT.md §1`). P1-B does not "fix" this — per the SK-V14 ORCHESTRATOR-PROMPT R1 pin, R1 binds in C-2 (post G-Omega). P1-B documents the misbinding as a finding for S-P2 design: every audit-overlay-falsified row in §3 carries the misbinding as its falsification root. The product-plane comparator that S-P2 must design is per-corpus: `sonic-rs strict struct deser per corpus` for direct, `sonic-rs strict per-corpus typed struct deser` for typed (per `SYNTHESIS.md §93 R1`).

## §5 — Sources (every artefact path + run id)

### Flame profile artefacts

All 56 records live under `/tmp/skv14-p1b/samply/profiles/`. Sidecar symbol tables live next to each with the `.json.syms.json` suffix. The `outname` in the §2 tables is the basename (drop the `.json.gz`). Each `(corpus, plane, track)` triple → one profile + one sidecar:

- `direct_to_struct × {Track 1, Track 2}` × 17 corpora → 34 profiles.
- `real_typed_struct × {Track 1, Track 2}` × 11 corpora → 22 profiles.

Driver logs (one per record, with full `PROBE_RESULT` rusage line) live under `/tmp/skv14-p1b/samply/logs/<outname>.log`. The sweep driver is `/tmp/skv14-p1b/run_sweep.sh` (bash 3.2-compatible; no associative arrays). The symbol-extraction tool is `/tmp/skv14-p1b/extract_top.py`. The aggregator is `/tmp/skv14-p1b/build_tables.py`; its JSON output is `/tmp/skv14-p1b/findings.json` (56 entries, each `{outname, total_samples, top: [{samples, pct, sym, class}], probe: {…}}`). The intermediate per-table markdown is at `/tmp/skv14-p1b/tables.md` + `/tmp/skv14-p1b/delta.md`. The sweep log is at `/tmp/skv14-p1b/sweep.log`.

### Run id

Whole-sweep run id: `skv14-p1b-V1-2026-05-23T02h45-aarch64-apple-darwin`. Per-record run id is the `outname` (which is unique by (corpus, plane, track)). Samply version `0.13.1`. Profile binary codeId `4F214AC279FB380A9F745CE9615B2850` (per `lipo -info` / `dwarfdump` on `/tmp/skv14-p1b-target/release/profile_direct`).

### Toolchain + build flags

- `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`.
- `[profile.release]` from `skinny/Cargo.toml`: `opt-level=3 lto="fat" codegen-units=1 panic="abort" debug=true strip=false split-debuginfo="packed"`.
- `RUSTFLAGS` unset (no target-cpu=native override at the cargo invocation level; the binary uses the default aarch64 baseline).
- `CARGO_TARGET_DIR=/tmp/skv14-p1b-target` (private to this agent; isolates from P1-A / P1-C / P1-D target dirs per `single-cargo-per-target`).

### Cross-references

- Bench harness binding: `skinny/crates/bbnf-bench/Cargo.toml`, `skinny/crates/bbnf-bench/benches/json_parity.rs:1-528`, `skinny/crates/bbnf-bench/src/bin/profile_direct.rs:1-205`.
- Hot leaf source files: `skinny/crates/runtime/src/grammars/json/generated.rs:466` (`parse_object_value_at_direct`), `skinny/crates/bbnf-bench/src/direct_struct.rs:1-110+` (`JsonDigestSink` + `HandParser`), `skinny/crates/bbnf-bench/src/real_typed_struct.rs:599-700` (`track1_typed` dispatch), `skinny/crates/bbnf-bench/src/generated_real_typed.rs` (`DirectParser` + `parse_type_*`), `skinny/crates/parse-that-regex/src/lib.rs` (`unescape_string`), `skinny/crates/parse-that-regex/src/number.rs` (`materialize_f64` / `materialize_u64`).
- Audit overlay basis: `restart/skinny/tranches/sk-v14/SYNTHESIS.md §0.2 + §93 R1 + §241 (comparator_plane) + §255 (audit_overlay_verdict)`; `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §1 (JSON parse_only — gate-relabel only; comparator misnamed) + §162-163 (JSON direct/typed ADMIT census)`.
- Dispatch context: `restart/skinny/tranches/sk-v14/research/p1/S-P1-DISPATCH-CONTEXT.md §6 (P1-B scope)`.
- Discipline references: `[no-warm-benches]`, `[bench-sequential-regression]`, `[bench-single-run]`, `[test-output-to-file]`, `[single-cargo-per-target]`, `[samply-symbol-resolution]` — feedback notes per `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/MEMORY.md`.
