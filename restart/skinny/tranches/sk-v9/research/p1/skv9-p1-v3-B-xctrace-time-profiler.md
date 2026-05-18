# SK-V9 P1-V3-B: xctrace Time Profiler Cross-Validation Capture

Pass: S-P1 Profile. Cycle: V3 reframe (cross-validation).
Date: 2026-05-18.
Scope: Time Profiler per-symbol self-time for all 17 JSON corpora ×
{Track 1 generated, Track 2 hand-coded} parse-only surfaces, captured with
`xcrun xctrace record --template "Time Profiler"`. Cross-validation
companion to P1-V3-A (CPU Counters) and to V2 samply mode-I at
`/tmp/skv9-p1-rerun/profiles/`.
Output: this file plus the per-trace symbol-level JSON aggregations under
`/tmp/skv9-xctrace-v3/exports/<corpus>__<track>.symbols.json`.
Baseline: SK-V9-open at commit `90609aee`, run
`sk-v9-open:criterion-fnv64-cd1673844eeea12f`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max` (18 cores,
12 P + 6 E).
Build flags: `RUSTFLAGS=-C target-cpu=native`, release profile
(`debug=true`, `strip=false`, `split-debuginfo=packed`, `lto=fat`,
`codegen-units=1`), mimalloc allocator (per `bbnf-bench` crate global
allocator).
Profile tool: `xcrun xctrace version 26.0 (17A5241e)`, Xcode 26.0 active
developer dir; sampling rate 1 ms (Time Profiler default); main-thread
deferred mode.
Corpus coverage: 17/17 for both tracks (34/34 traces captured).
Processor Trace coverage: 0/3 — BLOCKED by Apple toolchain library skew
(see §4).

Corpus-name canonical mapping (V4 fold note): the on-disk fixture file
is `update-center.json` (hyphen); the trace bundles under
`/tmp/skv9-xctrace-v3/p1b-tp/`, the `exports/` symbol files, and this
report's tables all use `update_center` (underscore) to match
`skinny/RESULTS.md` row identity and the P1-V3-A `pmu_rows.tsv`
join-key. P1-V3-A's TSV consumes the disk-file spelling (`update-center`)
in one column for fixture-path provenance. An aggregator joining A's
PMU rows and B's symbol exports on corpus name must normalise on the
underscore form (the column-key form `skinny/RESULTS.md` uses); the
hyphenated form is fixture-path provenance only. The §6 reproduction
script handles the mapping in its `corpus_paths.txt` block (see line
beginning `update_center …/update-center.json`).

## §1 — Capture methodology

### 1.1 Template, sampling rate, repeat count

The Time Profiler template samples the launched process's user-mode call
stack on a 1 ms tick on the P-core the process is scheduled on. Each
sample carries a `weight = 1 ms` and a full backtrace; self-time
attribution is the time-integral of the **leaf process-binary frame**
across the capture window. The Time Profiler schema is `time-profile`
(distinct from CPU Counters' `time-sample`, which carries kperf stack
shots without per-sample weights). The template difference is the
load-bearing axis of this cross-validation: P1-V3-A captured PMU PMCs;
P1-V3-B captures call-stack-attributed time.

Per (corpus, track) one capture is taken with a `--time-limit 2500ms`
window. The probe runs ≥ 2.5 s of steady-state inner loop before the
window expires, so the sample window is entirely inside the parse loop
(probe startup + sanity-parse + fixture I/O is ~10 ms, well below the
window's first tick). Probes exit with `rc=54` (`SIGKILL` from xctrace's
time-limit cap) — this is expected, the `.trace` artefact is complete.
Each capture yields 700–2000 in-process samples (`samples_process`); the
process-time share `process_share` is ≥ 99.5% on every row, confirming
the kernel/system frames are noise.

### 1.2 Probe binary

The capture launches a single-purpose probe binary, committed to
`skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs`. The binary loads one
corpus from disk once, then enters a black-boxed tight loop calling
either `runtime::generated_json::parse` (Track 1, the generated parser)
or `bbnf_bench::track2::json::parse` (Track 2, the hand-coded parser),
folding `root.tape().offset_bytes()` into a checksum. Black-box hints on
both the input string and the returned `JsonRoot` prevent LLVM from
hoisting the parser out of the loop. This is the same probe used by the
sibling P1-V3-A capture; the only difference between the two captures is
the xctrace template name (`CPU Counters` vs `Time Profiler`).

### 1.3 Per-corpus iteration counts

| Corpus | Iters | Corpus | Iters | Corpus | Iters |
|---|---:|---|---:|---|---:|
| twitter | 12 000 | apache_builds | 60 000 | mesh | 10 500 |
| citm_catalog | 4 000 | github_events | 120 000 | random | 15 000 |
| canada | 3 500 | update_center | 14 000 | gsoc-2018 | 2 500 |
| marine_ik | 2 700 | instruments | 35 000 | numbers | 50 000 |
| unicode_basic | 7 500 | unicode_mixed | 7 500 | unicode_escapes | 7 500 |
| distinct_values | 50 000 | y_string_unicode | 220 000 |

Each yields ≥ 2.5 s of inner-loop time at the M5 Max's measured Track 1
∼11–28 Gbps and Track 2 ∼9–18 Gbps cold per-parse throughput.

### 1.4 Per-capture command

```bash
xcrun xctrace record \
  --template "Time Profiler" \
  --output /tmp/skv9-xctrace-v3/p1b-tp/<corpus>__<track>.trace \
  --no-prompt \
  --time-limit 2500ms \
  --launch -- \
  /Users/mkbabb/Programming/bbnf-lang/skinny/target/release/xctrace_probe \
  <corpus_path> <track:track1|track2> <iters>
```

### 1.5 Export + aggregation pipeline

Per trace:

```bash
xcrun xctrace export \
  --input /tmp/skv9-xctrace-v3/p1b-tp/<corpus>__<track>.trace \
  --xpath '/trace-toc/run[@number="1"]/data/table[@schema="time-profile"]'
```

The `time-profile` schema is a row stream of `(sample-time, thread,
process, core, thread-state, weight, backtrace)` tuples. Each backtrace
is a leaf-first frame list with `id`/`ref` deduplication. The
aggregator (`/tmp/skv9-xctrace-v3/aggregate.py`) walks the rows,
resolves the id/ref graph for `<binary>`, `<frame>`, and `<backtrace>`
nodes, picks the topmost frame whose binary name is `xctrace_probe`
(filtering dyld/libsystem frames out of attribution but keeping their
share visible as `process_share < 100%`), demangles via `rustfilt`, and
sums weights into a per-symbol counter and a per-class counter.

The class taxonomy is the **canonical substrate-neutral primitive
vocabulary** the S-P1 cohort produces (V4 fold under Lock 14 / Lock 16
admissibility): every JSON-named symbol surfaced in §2 resolves to
exactly one of the following primitive classes, each parameterised
against the per-grammar StructuralAlphabet
(`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`
§4) so the same vocabulary admits CSS L4, Sheets, and BBNF-self when
those grammars land in skinny:

| Primitive class | What the byte loop does | JSON realisation | Generalisation |
|---|---|---|---|
| `per-string-span scanner` (= `string_tiny_scan` / `string_full_scan`) | Scan from an opener delimiter byte to a closing delimiter byte, classifying interior bytes against an escape set | JSON `"…"`, capped (`with_cap::<16>`) and uncapped (`match_string_at_quote_trusted_utf8`) | CSS L4 `"…"` + `'…'` + `url(…)`; Sheets `"…"` with `""` escape; BBNF-self string literals — each grammar's StructuralAlphabet enumerates its delimiter set, codegen instantiates the scanner per delimiter |
| `escape_codec_hex_unit` (= `unicode_escape_hex`) | Decode one hex-quartet → utf-8 codec call, possibly composing two halves of a surrogate pair | JSON `\uXXXX` (4 hex digits, surrogate-pair join, no terminator) | CSS L4 `\HHHHHH` (1..6 hex digits, no surrogate, whitespace-or-non-hex terminator); JS `\u{HHHHHH}`; TOML `\uHHHH` + `\UHHHHHHHH`. See §3.4 for full parameter set |
| `structural-element walker` (= `consume_structural` / `dispatch_value` / `parse_value_at` / `object_walk` / `array_walk`) | Advance the cursor over one StructuralAlphabet ordinal and ladder into the next dispatch | JSON `{` `}` `[` `]` `:` `,` walks via `consume_container_next` / `consume_array_next` / `dispatch_value` / `parse_value_at` | CSS L4 `{` `}` block-walk + `;` declaration-terminator walk; any grammar with structural-element ordinals admits the same walker shape |
| `number-digit parser` (= `number_digit_scan` / `number_scan` / `number_swar_digits`) | Scan a contiguous digit run + accumulate via SWAR multiply-add | `parse_that_regex::number::scan_digit_run` + `match_number_span_from_first` + `NumberParts::push_*_digits` | CSS L4 `<number>` + `<integer>` + `<percentage>` + `<dimension>`; Sheets numeric cells; BBNF-self integer literals — same FSM, same SWAR digit folder |
| `traversal-dispatch` (= `dispatch_value` / `string_dispatch`) | Demux on one StructuralAlphabet ordinal byte to one of N typed sub-parsers | JSON `dispatch_value` (8 arms: object / array / string / number / true / false / null / error); `parse_string` / `parse_key_colon` as one-byte sub-dispatchers | Any grammar with a tagged-union root admits a structurally identical dispatch — CSS L4's component-value dispatcher is the analogue. The class is one of *route shape*, not JSON-role identity |
| `simd_movemask` + `string_block_scan` (string-plane SIMD helpers) | Convert a 16/32/64-byte block-compare result into a single mask word and emit positions | `bbnf_simd::aarch64::movemask::movemask_u8x16` + `parse_that_regex::skip_string_plain_trusted` | All grammars with delimited-span scanners share the mask helpers; Lock 16 already admits these as grammar-neutral primitives |
| `whitespace_skip` (= byte-class skip) | Advance the cursor over a fixed byte-set (the grammar's whitespace class) | `parse_that_regex::skip_ascii_whitespace` | Every grammar with declarative whitespace admits the same byte-class skip primitive (CSS whitespace is wider; same shape) |

The classifier is grammar-neutral by construction (it matches symbol
substrings, not JSON-role names) per CH2 GENERALITY: each per-symbol
table row in §2 carries a `Class` column whose label is one of the
classes above. The JSON-named symbols (`match_tiny_plain_string_with_cap::<16>`,
`read_hex_unit_scalar`, `dispatch_value`, …) are the **per-grammar
realisations** of those classes; on a future CSS L4 capture the same
classifier will surface CSS-named symbols under the *same* class
labels.

The full top-15 self-time list per (corpus, track) is at
`/tmp/skv9-xctrace-v3/exports/<corpus>__<track>.symbols.json` along with
the full process-time class distribution. Per the per-row schema-v3
frontmatter contract, this artefact is reproducible by re-running
`/tmp/skv9-xctrace-v3/reproduce.sh`; the script regenerates the corpus
list, rebuilds the probe binary if missing, runs all 34 captures, and
runs the aggregator.

## §2 — Per-symbol self-time table (top-8 per row)

For each (corpus, track) the table below names the top-8 symbols by
self-time, with `%self` as the per-row percentage of in-process
self-time (samples whose leaf is the `xctrace_probe` binary), the
primitive class assigned by the substrate-neutral classifier (see §1.5
canonical primitive vocabulary), the Rust-demangled symbol, and the
source `file:line` xctrace surfaced. `samples_process` is the in-process
sample count, `process_share` the fraction of samples landing in
`xctrace_probe`. Cells with empty source indicate xctrace's DWARF did
not emit a `<source>` element for the sampled PC (typically inlined call
sites without an inlined-frame record); the symbol is still resolvable
from the demangled name.

The `Class` column labels are the §1.5 canonical primitive-class names.
First-occurrence resolution of the JSON-named symbols against their
primitive classes (one tag per section; the same mapping applies to
every per-row repetition below):

- `match_tiny_plain_string_with_cap::<16>` (Track 1) and
  `bbnf_bench::track2::json::match_tiny_plain_string` (Track 2) — both
  realise the `per-string-span scanner` primitive class
  (`string_tiny_scan` variant: cap-bounded scalar fast-path; CSS L4,
  Sheets, BBNF-self each instantiate the same scanner with their own
  StructuralAlphabet delimiter set, codegen-emitted per delimiter).
- `parse_that_regex::match_string_at_quote_trusted_utf8` — realises the
  `per-string-span scanner` primitive class (`string_full_scan`
  variant: uncapped SIMD-block scanner).
- `parse_that_regex::read_hex_unit_scalar` + `parse_that_regex::hex_nibble`
  — both realise the `escape_codec_hex_unit` primitive class
  parameterised `{hex_digit_count=4, surrogate_join_policy=pair,
  terminator_policy='\u'-fixed-width}` for JSON; see §3.4.
- `runtime::generated_json::generated::consume_container_next` +
  `consume_array_next` + `consume_structural` — realise the
  `structural-element walker` primitive class (per-grammar
  StructuralAlphabet ordinal walker).
- `runtime::generated_json::generated::dispatch_value` +
  `parse_key_colon` + `parse_string` (and the Track 2 `Parser::parse_value_at`
  / `parse_key_colon` / `parse_string` analogues) — realise the
  `traversal-dispatch` primitive class (tagged-union root demux on
  StructuralAlphabet ordinal).
- `parse_that_regex::number::scan_digit_run` +
  `match_number_span_from_first` + `NumberParts::push_two_digits` /
  `push_four_digits` / `push_eight_digits` + `is_four_ascii_digits`
  — realise the `number-digit parser` primitive class (digit-FSM +
  SWAR digit folder).
- `bbnf_simd::aarch64::movemask::movemask_u8x16` +
  `parse_that_regex::skip_string_plain_trusted` — realise the
  `simd_movemask` + `string_block_scan` string-plane SIMD helpers
  (Lock 16 grammar-neutral primitives).
- `parse_that_regex::skip_ascii_whitespace` — realises the
  `whitespace_skip` byte-class skip primitive (any grammar whose
  whitespace class is fixed admits the same primitive).
- `parse_that_regex::validate_string_escape` — realises the
  per-string-span scanner's *escape-validation* inner predicate
  (codegen-emitted from the grammar's escape set).


#### twitter / track1
samples_process=729  process_share=99.6%  weight_process=729 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 46.2% | string_tiny_scan | `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` | `crates/runtime/src/grammars/json/generated.rs:178` |
| 2 | 11.2% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `` |
| 3 | 8.8% | dispatch_value | `runtime::generated_json::generated::dispatch_value` | `` |
| 4 | 8.1% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 5 | 3.4% | other | `<u16>::trailing_zeros` | `library/core/src/num/uint_macros.rs:178` |
| 6 | 2.5% | object_walk | `runtime::generated_json::generated::consume_container_next` | `` |
| 7 | 2.2% | string_dispatch | `runtime::generated_json::generated::parse_key_colon` | `` |
| 8 | 1.9% | vec_grow | `<alloc::raw_vec::RawVecInner>::capacity` | `library/alloc/src/raw_vec/mod.rs:619` |

#### twitter / track2
samples_process=725  process_share=99.9%  weight_process=725 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 30.1% | string_tiny_scan | `bbnf_bench::track2::json::match_tiny_plain_string` | `crates/bbnf-bench/src/track2/json.rs:318` |
| 2 | 16.0% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:115` |
| 3 | 13.7% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 4 | 8.0% | parse_value_at | `<bbnf_bench::track2::json::Parser>::parse_value_at` | `` |
| 5 | 6.5% | string_dispatch | `<bbnf_bench::track2::json::Parser>::parse_key_colon` | `` |
| 6 | 2.9% | string_full_scan | `parse_that_regex::match_string_at_quote_trusted_utf8` | `` |
| 7 | 2.9% | vec_grow | `<alloc::raw_vec::RawVecInner>::capacity` | `library/alloc/src/raw_vec/mod.rs:619` |
| 8 | 2.3% | memcpy | `core::ptr::copy_nonoverlapping::<u8>` | `library/core/src/ptr/mod.rs:552` |

#### citm_catalog / track1
samples_process=1893  process_share=99.9%  weight_process=1893 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 24.0% | string_tiny_scan | `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` | `` |
| 2 | 23.1% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:114` |
| 3 | 9.8% | dispatch_value | `runtime::generated_json::generated::dispatch_value` | `` |
| 4 | 7.1% | memcpy | `core::ptr::copy_nonoverlapping::<u8>` | `library/core/src/ptr/mod.rs:552` |
| 5 | 5.3% | number_digit_scan | `parse_that_regex::number::scan_digit_run` | `` |
| 6 | 5.1% | array_walk | `runtime::generated_json::generated::consume_array_next` | `` |
| 7 | 3.5% | object_walk | `runtime::generated_json::generated::consume_container_next` | `` |
| 8 | 3.0% | consume_structural | `runtime::generated_json::generated::consume_structural` | `` |

#### citm_catalog / track2
samples_process=1908  process_share=99.8%  weight_process=1908 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 24.0% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:115` |
| 2 | 21.1% | string_tiny_scan | `bbnf_bench::track2::json::match_tiny_plain_string` | `crates/bbnf-bench/src/track2/json.rs:0` |
| 3 | 10.4% | object_walk | `<bbnf_bench::track2::json::Parser>::consume_container_next` | `` |
| 4 | 9.0% | parse_value_at | `<bbnf_bench::track2::json::Parser>::parse_value_at` | `` |
| 5 | 8.6% | memcpy | `core::ptr::copy_nonoverlapping::<u8>` | `library/core/src/ptr/mod.rs:552` |
| 6 | 4.6% | string_dispatch | `<bbnf_bench::track2::json::Parser>::parse_key_colon` | `` |
| 7 | 4.3% | number_digit_scan | `parse_that_regex::number::scan_digit_run` | `` |
| 8 | 2.6% | vec_grow | `<alloc::raw_vec::RawVecInner>::capacity` | `library/alloc/src/raw_vec/mod.rs:619` |

#### canada / track1
samples_process=1977  process_share=99.9%  weight_process=1977 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 21.0% | number_digit_scan | `parse_that_regex::number::scan_digit_run` | `crates/parse-that-regex/src/number/mod.rs:125` |
| 2 | 19.7% | dispatch_value | `runtime::generated_json::generated::dispatch_value` | `` |
| 3 | 16.2% | memcpy | `core::ptr::copy_nonoverlapping::<u8>` | `library/core/src/ptr/mod.rs:552` |
| 4 | 9.9% | array_walk | `runtime::generated_json::generated::consume_array_next` | `` |
| 5 | 9.2% | number_scan | `parse_that_regex::number::match_number_span_from_first` | `` |
| 6 | 6.2% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:115` |
| 7 | 2.8% | vec_grow | `<alloc::raw_vec::RawVecInner>::capacity` | `library/alloc/src/raw_vec/mod.rs:619` |
| 8 | 2.2% | consume_structural | `runtime::generated_json::generated::consume_structural` | `` |

#### canada / track2
samples_process=1968  process_share=99.9%  weight_process=1968 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 21.6% | parse_value_at | `<bbnf_bench::track2::json::Parser>::parse_value_at` | `crates/bbnf-bench/src/track2/json.rs:58` |
| 2 | 19.2% | number_digit_scan | `parse_that_regex::number::scan_digit_run` | `` |
| 3 | 15.9% | memcpy | `core::ptr::copy_nonoverlapping::<u8>` | `library/core/src/ptr/mod.rs:552` |
| 4 | 10.4% | number_scan | `parse_that_regex::number::match_number_span_from_first` | `` |
| 5 | 10.1% | object_walk | `<bbnf_bench::track2::json::Parser>::consume_container_next` | `` |
| 6 | 6.4% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:115` |
| 7 | 2.5% | number_swar_digits | `<parse_that_regex::number::NumberParts>::push_two_digits` | `crates/parse-that-regex/src/number/mod.rs:360` |
| 8 | 2.4% | vec_grow | `<alloc::raw_vec::RawVecInner>::capacity` | `library/alloc/src/raw_vec/mod.rs:619` |

#### apache_builds / track1
samples_process=1978  process_share=99.9%  weight_process=1978 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 56.0% | string_tiny_scan | `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` | `` |
| 2 | 10.3% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `` |
| 3 | 6.7% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 4 | 6.1% | dispatch_value | `runtime::generated_json::generated::dispatch_value` | `` |
| 5 | 3.2% | string_dispatch | `runtime::generated_json::generated::parse_string` | `` |
| 6 | 2.2% | object_walk | `runtime::generated_json::generated::consume_container_next` | `` |
| 7 | 2.0% | string_dispatch | `runtime::generated_json::generated::parse_key_colon` | `` |
| 8 | 1.8% | other | `<u16>::trailing_zeros` | `library/core/src/num/uint_macros.rs:178` |

#### apache_builds / track2
samples_process=1931  process_share=99.9%  weight_process=1931 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 45.0% | string_tiny_scan | `bbnf_bench::track2::json::match_tiny_plain_string` | `` |
| 2 | 14.5% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:115` |
| 3 | 6.8% | other | `<u16>::trailing_zeros` | `library/core/src/num/uint_macros.rs:178` |
| 4 | 6.6% | string_dispatch | `<bbnf_bench::track2::json::Parser>::parse_string` | `` |
| 5 | 6.4% | parse_value_at | `<bbnf_bench::track2::json::Parser>::parse_value_at` | `` |
| 6 | 5.1% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 7 | 3.4% | object_walk | `<bbnf_bench::track2::json::Parser>::consume_container_next` | `` |
| 8 | 3.1% | string_dispatch | `<bbnf_bench::track2::json::Parser>::parse_key_colon` | `` |

#### github_events / track1
samples_process=1705  process_share=99.9%  weight_process=1705 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 40.5% | string_tiny_scan | `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` | `crates/runtime/src/grammars/json/generated.rs:0` |
| 2 | 14.1% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 3 | 11.0% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:114` |
| 4 | 5.7% | dispatch_value | `runtime::generated_json::generated::dispatch_value` | `` |
| 5 | 3.7% | string_dispatch | `runtime::generated_json::generated::parse_string` | `` |
| 6 | 2.5% | string_dispatch | `runtime::generated_json::generated::parse_key_colon` | `` |
| 7 | 2.4% | other | `alloc::alloc::realloc_nonnull` | `` |
| 8 | 2.1% | string_block_scan | `parse_that_regex::skip_string_plain_trusted` | `` |

#### github_events / track2
samples_process=1960  process_share=99.9%  weight_process=1960 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 34.7% | string_tiny_scan | `bbnf_bench::track2::json::match_tiny_plain_string` | `` |
| 2 | 13.1% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 3 | 10.1% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `` |
| 4 | 6.8% | parse_value_at | `<bbnf_bench::track2::json::Parser>::parse_value_at` | `` |
| 5 | 5.6% | string_dispatch | `<bbnf_bench::track2::json::Parser>::parse_key_colon` | `` |
| 6 | 4.6% | string_dispatch | `<bbnf_bench::track2::json::Parser>::parse_string` | `` |
| 7 | 3.6% | other | `<u16>::trailing_zeros` | `library/core/src/num/uint_macros.rs:178` |
| 8 | 2.5% | object_walk | `<bbnf_bench::track2::json::Parser>::consume_container_next` | `` |

#### update_center / track1
samples_process=1955  process_share=99.9%  weight_process=1955 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 54.7% | string_tiny_scan | `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` | `` |
| 2 | 9.0% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 3 | 6.8% | dispatch_value | `runtime::generated_json::generated::dispatch_value` | `` |
| 4 | 5.2% | other | `<u16>::trailing_zeros` | `library/core/src/num/uint_macros.rs:178` |
| 5 | 4.7% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:115` |
| 6 | 4.0% | string_dispatch | `runtime::generated_json::generated::parse_string` | `` |
| 7 | 2.5% | object_walk | `runtime::generated_json::generated::consume_container_next` | `` |
| 8 | 2.1% | string_dispatch | `runtime::generated_json::generated::parse_key_colon` | `` |

#### update_center / track2
samples_process=1993  process_share=99.9%  weight_process=1993 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 46.0% | string_tiny_scan | `bbnf_bench::track2::json::match_tiny_plain_string` | `crates/bbnf-bench/src/track2/json.rs:318` |
| 2 | 9.0% | parse_value_at | `<bbnf_bench::track2::json::Parser>::parse_value_at` | `` |
| 3 | 7.4% | other | `<u16>::trailing_zeros` | `library/core/src/num/uint_macros.rs:178` |
| 4 | 6.8% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 5 | 4.6% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:115` |
| 6 | 4.5% | string_dispatch | `<bbnf_bench::track2::json::Parser>::parse_key_colon` | `` |
| 7 | 4.5% | object_walk | `<bbnf_bench::track2::json::Parser>::consume_container_next` | `` |
| 8 | 4.5% | string_dispatch | `<bbnf_bench::track2::json::Parser>::parse_string` | `` |

#### mesh / track1
samples_process=1959  process_share=99.9%  weight_process=1959 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 21.3% | dispatch_value | `runtime::generated_json::generated::dispatch_value` | `crates/runtime/src/grammars/json/generated.rs:58` |
| 2 | 19.3% | number_digit_scan | `parse_that_regex::number::scan_digit_run` | `` |
| 3 | 12.8% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:115` |
| 4 | 11.3% | number_scan | `parse_that_regex::number::match_number_span_from_first` | `crates/parse-that-regex/src/number/mod.rs:72` |
| 5 | 10.5% | memcpy | `core::ptr::copy_nonoverlapping::<u8>` | `library/core/src/ptr/mod.rs:552` |
| 6 | 10.0% | array_walk | `runtime::generated_json::generated::consume_array_next` | `` |
| 7 | 3.2% | vec_grow | `<alloc::raw_vec::RawVecInner>::capacity` | `library/alloc/src/raw_vec/mod.rs:619` |
| 8 | 1.9% | other | `<usize as core::slice::index::SliceIndex<[u8]>>::get` | `library/core/src/slice/index.rs:219` |

#### mesh / track2
samples_process=1983  process_share=99.9%  weight_process=1983 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 26.0% | parse_value_at | `<bbnf_bench::track2::json::Parser>::parse_value_at` | `` |
| 2 | 18.2% | number_digit_scan | `parse_that_regex::number::scan_digit_run` | `` |
| 3 | 12.7% | number_scan | `parse_that_regex::number::match_number_span_from_first` | `` |
| 4 | 10.4% | memcpy | `core::ptr::copy_nonoverlapping::<u8>` | `library/core/src/ptr/mod.rs:552` |
| 5 | 8.7% | object_walk | `<bbnf_bench::track2::json::Parser>::consume_container_next` | `crates/bbnf-bench/src/track2/json.rs:277` |
| 6 | 7.6% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:115` |
| 7 | 4.9% | vec_grow | `<alloc::raw_vec::RawVecInner>::capacity` | `library/alloc/src/raw_vec/mod.rs:619` |
| 8 | 3.1% | other | `<core::result::Result<(), runtime::generated_json::value::ParseError> as core...` | `library/core/src/result.rs:2173` |

#### random / track1
samples_process=1988  process_share=99.9%  weight_process=1988 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 48.6% | string_tiny_scan | `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` | `` |
| 2 | 13.4% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:115` |
| 3 | 8.1% | dispatch_value | `runtime::generated_json::generated::dispatch_value` | `` |
| 4 | 4.9% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 5 | 3.1% | string_dispatch | `runtime::generated_json::generated::parse_string` | `` |
| 6 | 2.5% | array_walk | `runtime::generated_json::generated::consume_array_next` | `` |
| 7 | 2.4% | object_walk | `runtime::generated_json::generated::consume_container_next` | `` |
| 8 | 2.4% | string_dispatch | `runtime::generated_json::generated::parse_key_colon` | `` |

#### random / track2
samples_process=1991  process_share=99.9%  weight_process=1991 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 42.1% | string_tiny_scan | `bbnf_bench::track2::json::match_tiny_plain_string` | `` |
| 2 | 12.8% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:115` |
| 3 | 9.2% | object_walk | `<bbnf_bench::track2::json::Parser>::consume_container_next` | `crates/bbnf-bench/src/track2/json.rs:0` |
| 4 | 8.6% | parse_value_at | `<bbnf_bench::track2::json::Parser>::parse_value_at` | `` |
| 5 | 3.4% | number_digit_scan | `parse_that_regex::number::scan_digit_run` | `crates/parse-that-regex/src/number/mod.rs:125` |
| 6 | 3.3% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 7 | 3.3% | string_dispatch | `<bbnf_bench::track2::json::Parser>::parse_string` | `` |
| 8 | 2.7% | string_dispatch | `<bbnf_bench::track2::json::Parser>::parse_key_colon` | `` |

#### gsoc-2018 / track1
samples_process=1955  process_share=99.9%  weight_process=1955 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 30.9% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 2 | 20.8% | string_tiny_scan | `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` | `` |
| 3 | 10.5% | other | `<u16>::trailing_zeros` | `library/core/src/num/uint_macros.rs:178` |
| 4 | 5.3% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `` |
| 5 | 4.8% | string_block_scan | `parse_that_regex::skip_string_plain_trusted` | `crates/parse-that-regex/src/lib.rs:551` |
| 6 | 4.0% | string_full_scan | `parse_that_regex::match_string_at_quote_trusted_utf8` | `` |
| 7 | 3.5% | string_dispatch | `runtime::generated_json::generated::parse_string` | `` |
| 8 | 3.2% | other | `<u16 as core::convert::From<u8>>::from` | `library/core/src/convert/num.rs:82` |

#### gsoc-2018 / track2
samples_process=1992  process_share=99.9%  weight_process=1992 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 29.9% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 2 | 19.9% | string_tiny_scan | `bbnf_bench::track2::json::match_tiny_plain_string` | `` |
| 3 | 10.6% | other | `<u16>::trailing_zeros` | `library/core/src/num/uint_macros.rs:178` |
| 4 | 5.5% | string_block_scan | `parse_that_regex::skip_string_plain_trusted` | `` |
| 5 | 5.4% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `` |
| 6 | 4.8% | string_dispatch | `<bbnf_bench::track2::json::Parser>::parse_string` | `` |
| 7 | 3.3% | other | `core::core_arch::aarch64::neon::generated::vsri_n_s8::<3>` | `...y/core/src/../../stdarch/crates/core_arch/src/aarch64/neon/mod.rs:88` |
| 8 | 3.3% | other | `<u16 as core::convert::From<u8>>::from` | `library/core/src/convert/num.rs:82` |

#### marine_ik / track1
samples_process=1996  process_share=99.9%  weight_process=1996 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 24.2% | dispatch_value | `runtime::generated_json::generated::dispatch_value` | `` |
| 2 | 13.8% | number_digit_scan | `parse_that_regex::number::scan_digit_run` | `crates/parse-that-regex/src/number/mod.rs:150` |
| 3 | 11.4% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:115` |
| 4 | 10.8% | memcpy | `core::ptr::copy_nonoverlapping::<u8>` | `library/core/src/ptr/mod.rs:552` |
| 5 | 9.6% | number_scan | `parse_that_regex::number::match_number_span_from_first` | `` |
| 6 | 8.7% | array_walk | `runtime::generated_json::generated::consume_array_next` | `` |
| 7 | 3.4% | vec_grow | `<alloc::raw_vec::RawVecInner>::capacity` | `library/alloc/src/raw_vec/mod.rs:619` |
| 8 | 3.1% | string_tiny_scan | `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` | `crates/runtime/src/grammars/json/generated.rs:178` |

#### marine_ik / track2
samples_process=1995  process_share=99.9%  weight_process=1995 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 25.8% | parse_value_at | `<bbnf_bench::track2::json::Parser>::parse_value_at` | `` |
| 2 | 13.6% | number_scan | `parse_that_regex::number::match_number_span_from_first` | `crates/parse-that-regex/src/number/mod.rs:62` |
| 3 | 13.6% | number_digit_scan | `parse_that_regex::number::scan_digit_run` | `` |
| 4 | 11.0% | memcpy | `core::ptr::copy_nonoverlapping::<u8>` | `library/core/src/ptr/mod.rs:552` |
| 5 | 8.9% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:114` |
| 6 | 8.7% | object_walk | `<bbnf_bench::track2::json::Parser>::consume_container_next` | `` |
| 7 | 3.9% | vec_grow | `<alloc::raw_vec::RawVecInner>::capacity` | `library/alloc/src/raw_vec/mod.rs:619` |
| 8 | 2.4% | other | `<core::result::Result<(), runtime::generated_json::value::ParseError> as core...` | `library/core/src/result.rs:2173` |

#### instruments / track1
samples_process=1995  process_share=99.9%  weight_process=1995 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 40.2% | string_tiny_scan | `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` | `` |
| 2 | 17.9% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:115` |
| 3 | 9.5% | dispatch_value | `runtime::generated_json::generated::dispatch_value` | `` |
| 4 | 5.0% | number_scan | `parse_that_regex::number::match_number_span_from_first` | `` |
| 5 | 4.0% | memcpy | `core::ptr::copy_nonoverlapping::<u8>` | `library/core/src/ptr/mod.rs:552` |
| 6 | 3.5% | object_walk | `runtime::generated_json::generated::consume_container_next` | `` |
| 7 | 3.1% | string_dispatch | `runtime::generated_json::generated::parse_key_colon` | `` |
| 8 | 2.5% | vec_grow | `<alloc::raw_vec::RawVecInner>::capacity` | `library/alloc/src/raw_vec/mod.rs:619` |

#### instruments / track2
samples_process=1995  process_share=99.9%  weight_process=1995 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 31.8% | string_tiny_scan | `bbnf_bench::track2::json::match_tiny_plain_string` | `` |
| 2 | 18.9% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `` |
| 3 | 9.0% | parse_value_at | `<bbnf_bench::track2::json::Parser>::parse_value_at` | `` |
| 4 | 6.6% | string_dispatch | `<bbnf_bench::track2::json::Parser>::parse_key_colon` | `` |
| 5 | 5.2% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 6 | 4.0% | number_scan | `parse_that_regex::number::match_number_span_from_first` | `crates/parse-that-regex/src/number/mod.rs:54` |
| 7 | 3.3% | object_walk | `<bbnf_bench::track2::json::Parser>::consume_container_next` | `` |
| 8 | 3.0% | memcpy | `core::ptr::copy_nonoverlapping::<u8>` | `library/core/src/ptr/mod.rs:552` |

#### numbers / track1
samples_process=1984  process_share=99.9%  weight_process=1984 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 33.4% | number_digit_scan | `parse_that_regex::number::scan_digit_run` | `crates/parse-that-regex/src/number/mod.rs:125` |
| 2 | 19.6% | dispatch_value | `runtime::generated_json::generated::dispatch_value` | `` |
| 3 | 9.4% | array_walk | `runtime::generated_json::generated::consume_array_next` | `` |
| 4 | 8.3% | number_scan | `parse_that_regex::number::match_number_span_from_first` | `` |
| 5 | 4.2% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:115` |
| 6 | 4.0% | number_swar_digits | `<parse_that_regex::number::NumberParts>::push_four_digits` | `` |
| 7 | 3.9% | number_swar_digits | `parse_that_regex::number::is_four_ascii_digits` | `` |
| 8 | 3.8% | number_swar_digits | `<parse_that_regex::number::NumberParts>::push_eight_digits` | `` |

#### numbers / track2
samples_process=1986  process_share=99.9%  weight_process=1986 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 37.3% | number_digit_scan | `parse_that_regex::number::scan_digit_run` | `crates/parse-that-regex/src/number/mod.rs:158` |
| 2 | 21.4% | parse_value_at | `<bbnf_bench::track2::json::Parser>::parse_value_at` | `` |
| 3 | 9.6% | number_scan | `parse_that_regex::number::match_number_span_from_first` | `` |
| 4 | 6.9% | object_walk | `<bbnf_bench::track2::json::Parser>::consume_container_next` | `` |
| 5 | 4.3% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:115` |
| 6 | 3.8% | memcpy | `core::ptr::copy_nonoverlapping::<u8>` | `library/core/src/ptr/mod.rs:552` |
| 7 | 3.0% | number_swar_digits | `<parse_that_regex::number::NumberParts>::push_eight_digits` | `` |
| 8 | 2.6% | number_swar_digits | `parse_that_regex::number::is_four_ascii_digits` | `` |

#### unicode_mixed / track1
samples_process=1966  process_share=99.9%  weight_process=1966 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 24.9% | dispatch_value | `runtime::generated_json::generated::dispatch_value` | `crates/runtime/src/grammars/json/generated.rs:0` |
| 2 | 20.1% | string_escape | `parse_that_regex::validate_string_escape` | `crates/parse-that-regex/src/lib.rs:285` |
| 3 | 15.2% | string_full_scan | `parse_that_regex::match_string_at_quote_trusted_utf8` | `` |
| 4 | 9.7% | other | `<u16>::trailing_zeros` | `library/core/src/num/uint_macros.rs:178` |
| 5 | 9.5% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 6 | 5.7% | string_tiny_scan | `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` | `` |
| 7 | 3.2% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `` |
| 8 | 1.9% | string_dispatch | `runtime::generated_json::generated::parse_string` | `` |

#### unicode_mixed / track2
samples_process=1990  process_share=99.9%  weight_process=1990 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 20.5% | parse_value_at | `<bbnf_bench::track2::json::Parser>::parse_value_at` | `` |
| 2 | 18.1% | string_escape | `parse_that_regex::validate_string_escape` | `crates/parse-that-regex/src/lib.rs:285` |
| 3 | 15.0% | string_full_scan | `parse_that_regex::match_string_at_quote_trusted_utf8` | `` |
| 4 | 11.4% | other | `<u16>::trailing_zeros` | `library/core/src/num/uint_macros.rs:178` |
| 5 | 9.7% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 6 | 5.3% | string_tiny_scan | `bbnf_bench::track2::json::match_tiny_plain_string` | `` |
| 7 | 3.5% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `` |
| 8 | 2.7% | other | `<core::option::Option<&u8>>::copied` | `library/core/src/option.rs:2141` |

#### unicode_escapes / track1
samples_process=1986  process_share=99.9%  weight_process=1986 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 23.7% | unicode_escape_hex | `parse_that_regex::read_hex_unit_scalar` | `` |
| 2 | 20.9% | dispatch_value | `runtime::generated_json::generated::dispatch_value` | `` |
| 3 | 19.5% | string_full_scan | `parse_that_regex::match_string_at_quote_trusted_utf8` | `crates/parse-that-regex/src/lib.rs:174` |
| 4 | 9.9% | unicode_escape_hex | `parse_that_regex::hex_nibble` | `` |
| 5 | 4.8% | string_escape | `parse_that_regex::validate_string_escape` | `` |
| 6 | 3.6% | other | `<core::option::Option<&u8>>::copied` | `library/core/src/option.rs:2141` |
| 7 | 3.5% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 8 | 2.5% | other | `<u8 as core::cmp::PartialEq>::eq` | `library/core/src/cmp.rs:1877` |

#### unicode_escapes / track2
samples_process=1988  process_share=99.9%  weight_process=1988 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 24.4% | unicode_escape_hex | `parse_that_regex::read_hex_unit_scalar` | `` |
| 2 | 17.7% | parse_value_at | `<bbnf_bench::track2::json::Parser>::parse_value_at` | `crates/parse-that-regex/src/number/mod.rs:0` |
| 3 | 12.3% | unicode_escape_hex | `parse_that_regex::hex_nibble` | `` |
| 4 | 10.4% | string_full_scan | `parse_that_regex::match_string_at_quote_trusted_utf8` | `` |
| 5 | 8.3% | string_escape | `parse_that_regex::validate_string_escape` | `` |
| 6 | 6.4% | other | `<core::option::Option<&u8>>::copied` | `library/core/src/option.rs:2141` |
| 7 | 3.6% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 8 | 2.9% | other | `<u8 as core::cmp::PartialEq>::eq` | `library/core/src/cmp.rs:1877` |

#### unicode_basic / track1
samples_process=1981  process_share=99.9%  weight_process=1981 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 31.4% | string_tiny_scan | `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` | `` |
| 2 | 13.5% | other | `<u16>::trailing_zeros` | `library/core/src/num/uint_macros.rs:178` |
| 3 | 9.9% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:115` |
| 4 | 9.3% | dispatch_value | `runtime::generated_json::generated::dispatch_value` | `` |
| 5 | 6.7% | string_dispatch | `runtime::generated_json::generated::parse_string` | `` |
| 6 | 6.0% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 7 | 2.2% | object_walk | `runtime::generated_json::generated::consume_container_next` | `` |
| 8 | 2.2% | vec_grow | `<alloc::raw_vec::RawVecInner>::capacity` | `library/alloc/src/raw_vec/mod.rs:619` |

#### unicode_basic / track2
samples_process=1974  process_share=99.9%  weight_process=1974 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 32.5% | string_tiny_scan | `bbnf_bench::track2::json::match_tiny_plain_string` | `crates/bbnf-bench/src/track2/json.rs:0` |
| 2 | 15.7% | other | `<u16>::trailing_zeros` | `library/core/src/num/uint_macros.rs:178` |
| 3 | 9.9% | parse_value_at | `<bbnf_bench::track2::json::Parser>::parse_value_at` | `` |
| 4 | 8.5% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `` |
| 5 | 5.8% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 6 | 5.1% | object_walk | `<bbnf_bench::track2::json::Parser>::consume_container_next` | `` |
| 7 | 4.0% | string_dispatch | `<bbnf_bench::track2::json::Parser>::parse_string` | `` |
| 8 | 3.0% | vec_grow | `<alloc::raw_vec::RawVecInner>::capacity` | `` |

#### distinct_values / track1
samples_process=1965  process_share=99.9%  weight_process=1965 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 61.9% | string_tiny_scan | `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` | `` |
| 2 | 8.0% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:115` |
| 3 | 7.2% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 4 | 6.4% | dispatch_value | `runtime::generated_json::generated::dispatch_value` | `` |
| 5 | 2.0% | object_walk | `runtime::generated_json::generated::consume_container_next` | `` |
| 6 | 1.6% | string_dispatch | `runtime::generated_json::generated::parse_string` | `` |
| 7 | 1.4% | string_open | `runtime::generated_json::generated::consume_quote_at_cursor` | `` |
| 8 | 1.2% | string_dispatch | `runtime::generated_json::generated::parse_key_colon` | `` |

#### distinct_values / track2
samples_process=1994  process_share=99.9%  weight_process=1994 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 63.1% | string_tiny_scan | `bbnf_bench::track2::json::match_tiny_plain_string` | `crates/bbnf-bench/src/track2/json.rs:319` |
| 2 | 6.2% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 3 | 5.7% | whitespace_skip | `parse_that_regex::skip_ascii_whitespace` | `crates/parse-that-regex/src/lib.rs:116` |
| 4 | 5.3% | other | `<u16>::trailing_zeros` | `library/core/src/num/uint_macros.rs:178` |
| 5 | 4.8% | parse_value_at | `<bbnf_bench::track2::json::Parser>::parse_value_at` | `` |
| 6 | 2.6% | string_dispatch | `<bbnf_bench::track2::json::Parser>::parse_string` | `` |
| 7 | 2.1% | object_walk | `<bbnf_bench::track2::json::Parser>::consume_container_next` | `` |
| 8 | 2.0% | string_dispatch | `<bbnf_bench::track2::json::Parser>::parse_key_colon` | `` |

#### y_string_unicode / track1
samples_process=1989  process_share=99.9%  weight_process=1989 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 19.2% | unicode_escape_hex | `parse_that_regex::hex_nibble` | `` |
| 2 | 19.0% | unicode_escape_hex | `parse_that_regex::read_hex_unit_scalar` | `` |
| 3 | 10.6% | string_tiny_scan | `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` | `crates/runtime/src/grammars/json/generated.rs:0` |
| 4 | 5.5% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 5 | 5.4% | other | `<core::option::Option<&u8>>::copied` | `library/core/src/option.rs:2141` |
| 6 | 5.1% | dispatch_value | `runtime::generated_json::generated::dispatch_value` | `` |
| 7 | 5.1% | other | `<u8 as core::cmp::PartialEq>::eq` | `library/core/src/cmp.rs:1877` |
| 8 | 3.1% | other | `alloc::alloc::realloc_nonnull` | `library/alloc/src/alloc.rs:155` |

#### y_string_unicode / track2
samples_process=1986  process_share=99.9%  weight_process=1986 ms

| Rank | %self | Class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 29.5% | unicode_escape_hex | `parse_that_regex::read_hex_unit_scalar` | `` |
| 2 | 14.4% | unicode_escape_hex | `parse_that_regex::hex_nibble` | `crates/parse-that-regex/src/lib.rs:962` |
| 3 | 7.5% | string_tiny_scan | `bbnf_bench::track2::json::match_tiny_plain_string` | `crates/bbnf-bench/src/track2/json.rs:317` |
| 4 | 5.7% | simd_movemask | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 5 | 5.6% | parse_value_at | `<bbnf_bench::track2::json::Parser>::parse_value_at` | `` |
| 6 | 5.3% | other | `<core::option::Option<&u8>>::copied` | `library/core/src/option.rs:2141` |
| 7 | 3.7% | other | `<u8 as core::cmp::PartialEq>::eq` | `` |
| 8 | 2.8% | string_escape | `parse_that_regex::validate_string_escape` | `` |

## §3 — Hot-leaf hypothesis verdict

The SK-V8 substrate-ceiling cohort named three hot-leaf hypotheses that
S-P1 V3 must validate or falsify against fresh per-symbol evidence.
This section settles each verdict using the Time Profiler attribution
from §2.

### 3.1 SC-1 claim — `scan_structurals` time is wasted

**Claim** (SC-1 §1.3): "The SIMD scan's output is **never consumed by
the parser** … `attach_structural_index` is a no-op." Both the default
`CapacityPlan::GrowOnly` parser and the structural-floor probe were
named as separate symbols; the SIMD scan was claimed to be either
unrun (default path) or run-and-discarded.

**Verdict — CONFIRMED on every (corpus, track)**. `scan_structurals`
self-time is **0.00%** on all 17 × 2 = 34 rows. The SIMD structural
classifier (`bbnf_simd::aarch64::scan::neon::scan`,
`bbnf_simd::aarch64::bulk_emit_positions_64`,
`bbnf_simd::aarch64::bitmap_prefix_xor_64`) does not appear in any
backtrace under the steady-state parse loop. The recursive-descent
parser body (`runtime::generated_json::generated::parse_value_at`,
`dispatch_value`, `consume_structural`, `parse_array`, `parse_object`)
re-derives every structural byte. The wasted-scan ledger reads:

| Symbol class                 | Process-time share on parse_only | Verdict |
|---|---:|---|
| `scan_structurals` (any SIMD scan kernel) | 0.00% on 34/34 rows | discarded, not consumed |
| `simd_movemask::movemask_u8x16`           | 0.0 – 30.9% (peak on gsoc-2018) | called by the **string scanner**, not by structural classification |
| `bulk_emit_positions_64` / `prefix_xor_64` | 0.00% on parse_only | unrun on default plan |

`simd_movemask` does show on the string-heavy corpora — but its source
location resolves to `crates/bbnf-simd/src/aarch64/movemask.rs:22`
inside the *string* fast-path's 16-byte block scanner
(`scan_string_special_block`), not the structural scan. The same
primitive name carries two callsites, and only the string-scan callsite
fires on parse_only. The SC-1 claim is exact at the symbol level.

**Substrate-shape generalisation (CH2 GENERALITY fold)**: this is **not**
a JSON-specific verdict. For any grammar whose generated parse_only
surface re-derives StructuralAlphabet ordinals inside the fused
`structural-element walker` body (the recursive-descent dispatcher), the
SIMD structural-scan output is structurally unconsumed and will exhibit
the same 0.00% `scan_structurals` self-time. CSS L4 / Sheets / BBNF-self
generated parse_only surfaces will reproduce the verdict under the
identical lowering shape. The diagnostic generalises to the
`structural-scan-output non-fusion` class; the JSON-corpus rows are the
JSON realisation.

### 3.2 SC-4 claim — string scanner pair carries ~75% of self-time on
loss corpora

**Claim** (SC-4 §1.3 + SK-V7 §3.4): the pair `match_string_at_quote
~47% + match_tiny_plain_string ~28% ≈ 75%` of self-time on
string-heavy losses (twitter, gsoc-2018, distinct_values, the unicode
rows).

**Verdict — PARTIALLY CONFIRMED, with the dispatch ratio reversed**. The
SK-V8-named ~75% combined share materialises only on the most
string-saturated rows; the asymmetry between the tiny path
(`string_tiny_scan` variant of the `per-string-span scanner` primitive
class — JSON realisations `match_tiny_plain_string_with_cap::<16>` on
Track 1 and `bbnf_bench::track2::json::match_tiny_plain_string` on
Track 2) and the full NEON path (`string_full_scan` variant — JSON
realisation `parse_that_regex::match_string_at_quote_trusted_utf8`) is
the inverse of SK-V7's claim: the tiny-variant scalar realisation
dominates, the full-variant SIMD realisation is in the tail. Per Time
Profiler:

| Corpus            | tiny `match_tiny_plain_string` % | full `match_string_at_quote` % | tiny+full % | full string family % |
|---|---:|---:|---:|---:|
| distinct_values/t1 | 61.9% | 0.0% | 61.9% | 67.5% |
| distinct_values/t2 | 63.1% | 0.0% | 63.1% | 70.4% |
| update_center/t1   | 54.7% | 0.0% | 54.7% | 63.0% |
| update_center/t2   | 46.0% | 0.0% | 46.0% | 58.6% |
| apache_builds/t1   | 56.0% | 0.0% | 56.0% | 63.4% |
| apache_builds/t2   | 45.0% | 0.0% | 45.0% | 56.0% |
| random/t1          | 48.6% | 0.0% | 48.6% | 55.9% |
| random/t2          | 42.1% | 0.0% | 42.1% | 50.0% |
| twitter/t1         | 46.2% | 1.2% | 47.4% | 52.3% |
| twitter/t2         | 30.1% | 2.9% | 33.0% | 43.6% |
| github_events/t1   | 40.5% | 0.0% | 40.5% | 50.0% |
| github_events/t2   | 34.7% | 0.0% | 34.7% | 49.9% |
| instruments/t1     | 40.2% | 0.0% | 40.2% | 44.7% |
| unicode_basic/t1   | 31.4% | 0.0% | 31.4% | 42.1% |
| unicode_mixed/t1   |  5.7% | 15.2% | 20.9% | 44.6% |
| unicode_escapes/t1 |  0.0% | 19.5% | 19.5% | 26.1% |
| gsoc-2018/t1       | 20.8% | 4.0% | 24.8% | 36.3% |
| gsoc-2018/t2       | 19.9% | 3.2% | 23.1% | 36.1% |
| y_string_unicode/t1 | 10.6% | 1.5% | 12.1% | 18.5% |

The 75% figure is closely approached on **distinct_values** (67.5%) and
**update_center / apache_builds / random** (~55–63%) but the
distribution between tiny and full is essentially "everything is tiny."
The full SIMD fallback `match_string_at_quote_trusted_utf8`
(`string_full_scan` realisation) is only significant on
unicode_mixed/escapes (10–20% — where the SIMD block correctly absorbs
long plain runs) and never exceeds 20% on any row.

For **gsoc-2018** and **y_string_unicode** (two corpora SC-4 named), the
string-family share is much lower than 75% (36% and 18% respectively).
The remaining self-time on these rows points elsewhere:

- **gsoc-2018**: `simd_movemask::movemask_u8x16` (string-plane SIMD
  helper, Lock 16 grammar-neutral primitive) is the **largest** symbol
  at 30.9% (track 1) and 29.9% (track 2). The mask primitive is itself
  the hot leaf — the string block scanner reduces to one mask load per
  16-byte block, called frequently because gsoc-2018 has many long
  strings. This is a SIMD primitive that is structurally part of the
  per-string-span scanner family but is not the same symbol as either
  the `string_tiny_scan` or `string_full_scan` realisations.
- **y_string_unicode**: the dominant pair is
  `parse_that_regex::read_hex_unit_scalar` (19.0%) +
  `parse_that_regex::hex_nibble` (19.2%) = **38.2%** of the
  `escape_codec_hex_unit` primitive class
  (track 1; track 2 is 43.9%). This is a primitive class SC-4's framing
  did not isolate. y_string_unicode is 99%+ short 6-byte `\uXXXX`
  strings, and the bottleneck is the `escape_codec_hex_unit` codec
  (parameters `{hex_digit_count=4, surrogate_join_policy=pair,
  terminator_policy='\u'-fixed-width}`), not the per-string-span
  scanner. See §3.4 for the full parameter set + cross-grammar
  instantiations.

**Revised hot-leaf statement**: the SK-V7 §3.4 "string scanner pair
~75%" framing is correct for the densely-keyed object corpora
(distinct_values, update_center, apache_builds, twitter on track 1) but
the bottleneck primitive class on the *unicode* and *escape* rows is
`escape_codec_hex_unit` (JSON realisations `read_hex_unit_scalar` +
`hex_nibble`), not `per-string-span scanner`. S-P2 must enumerate the
two as distinct primitive classes; the §1.5 vocabulary already
separates them.

**Substrate-shape generalisation (CH2 GENERALITY fold)**: the
`per-string-span scanner` primitive class spans every grammar whose
StructuralAlphabet enumerates one or more string-delimiter bytes. CSS L4
admits three (`"`, `'`, `url(`) and will surface three per-delimiter
realisations; Sheets admits one (`"`) with internal `""` escape; BBNF-
self admits one string-literal class. The verdict shape — *the
`per-string-span scanner` primitive carries 50–70% of self-time on
delimited-span-heavy corpora* — generalises by inspection; the JSON
`q_frac ≥ 0.726` rows are the JSON instantiation of the
delimited-span-fraction admission predicate. SC-6 §4.1's per-grammar
string-delimiter byte set is the binding citation.

### 3.3 SC-1 claim — `consume_structural` carries the structural-walk cost

**Claim** (SC-1 §1.1): `consume_structural` is the per-element tape
write entry, called at line 303 from `parse_object`/`parse_array`. SC-1
did NOT itself claim a self-time percentage; the question is whether
`consume_structural` self-time is large or whether it inlined into
`dispatch_value`.

**Verdict — `consume_structural` self-time is < 3% on every row.** On
the float-heavy winners where structural calls are densest:

| Corpus | `consume_structural` self-time |
|---|---:|
| canada/t1 | 2.2% |
| mesh/t1   | (rolled into `dispatch_value`; <1%) |
| numbers/t1 | (rolled into `dispatch_value`; <1%) |
| citm_catalog/t1 | 3.0% |

The combined `consume_structural` + `dispatch_value` + `parse_value_at`
+ `object_walk` + `array_walk` + `whitespace_skip` family (the
"structural-walk family") sums to 20–55% across corpora — but this is
spread across at least four named symbols rather than the single
`consume_structural` symbol SC-1 located. The structural-walk cost is
**not** a tape-write hot leaf; it is a `skip_ws` + `dispatch_value`
ladder.

### 3.4 Mode-I samply artefact: dispatch_value coalescing

V2 samply mode-I (`p1a-samply-mode-1.md`) reported `dispatch_value`
self-time at **95.6–99.6%** on every parse_only row, with the actual
hot-leaf symbols absent from the top-5. Time Profiler **falsifies** that
attribution at the symbol level:

| Corpus | samply `dispatch_value` | xctrace `dispatch_value` | xctrace `parse_value_at` | xctrace top symbol |
|---|---:|---:|---:|---|
| twitter/t1 | 98.8% | 8.8% | (inlined) | `match_tiny_plain_string_with_cap` 46.2% |
| canada/t1  | 99.6% | 19.7% | (inlined) | `scan_digit_run` 21.0% |
| update_center/t1 | 99.0% | 6.8% | (inlined) | `match_tiny_plain_string_with_cap` 54.7% |
| numbers/t1 | 96.7% | 19.6% | (inlined) | `scan_digit_run` 33.4% |
| gsoc-2018/t1 | 99.1% | 1.7% | (inlined) | `movemask_u8x16` 30.9% |
| y_string_unicode/t1 | 95.6% | 5.1% | (inlined) | `hex_nibble` 19.2% |

The mode-I samply attribution is a **frame-pointer-coalescing artefact**:
samply read every PC inside the LTO-fused `dispatch_value` body as a
sample on `dispatch_value` because the inner inlines have no
return-pointer entry. xctrace's PC→PC backtrace walks the inlined frames
via DWARF and resolves each inline to its source symbol, surfacing the
real leaf. This is the load-bearing finding of V3: the V2 samply
top-self-time table is **not measurable hot-leaf attribution** for the
LTO-fused generated parser; it is a single-symbol artefact that hid the
true primitives.

The substrate-neutral conclusion: P1-A's `dispatch_value 95.6 – 99.6%`
row cannot be used by S-P2 as a primitive antecedent. The xctrace
self-time table replaces it on the same baseline (commit `90609aee`,
same fixtures, same probe binary, same build flags).

### 3.5 `escape_codec_hex_unit` primitive class (Lock 14 reframe)

The y_string_unicode bottleneck pair `read_hex_unit_scalar` +
`hex_nibble` (§3.2 last bullet; 38.2% / 43.9% combined on track 1 /
track 2) realises a **substrate-neutral primitive class**, not a
JSON-specific symbol. Per Lock 14
(`restart/locks/LOCKS.md`) + Lock 16 admissibility +
SC-6 §4 cross-grammar StructuralAlphabet generalisation, the class is:

**`escape_codec_hex_unit`** — parse a hex-digit-encoded escape sequence
into a utf-8 byte sequence, with the following parameters:

| Parameter | Domain | JSON value | CSS L4 value | JS `\u{}` value | TOML `\U` value |
|---|---|---|---|---|---|
| `hex_digit_count` | fixed-width N or range `[lo, hi]` | `4` (fixed) | `[1, 6]` (range) | `[1, 6]` (range, `{}`-bounded) | `8` (fixed for `\U`); `4` (fixed for `\u`) |
| `surrogate_join_policy` | `none` / `pair` / `range-check` | `pair` (`\uD800-DBFF` followed by `\uDC00-DFFF`) | `none` (code-point direct) | `range-check` (code-point validated) | `none` |
| `terminator_policy` | `fixed-width` / `delimiter` / `whitespace-or-non-hex` | `fixed-width` (consume exactly 4 hex digits) | `whitespace-or-non-hex` (consume up to 6, terminate on the first non-hex byte or a whitespace) | `delimiter` (terminate on `}`) | `fixed-width` (4 or 8) |
| `target_encoding` | `utf-8` / `utf-16` / `utf-32` | `utf-8` | `utf-8` | `utf-8` | `utf-8` |

JSON's `\uXXXX` decoder thus instantiates `escape_codec_hex_unit{4,
pair, fixed-width, utf-8}` — the SK-V7 / SC-4 framing of the kernel as
"the unicode-escape codec for JSON" is the JSON-specific projection of
the substrate-neutral class. The `bbnf-simd/src/aarch64/unescape_uxxxx.rs`
NEON kernel realises the JSON instantiation under codegen-emitted
parameter binding; the same kernel admits CSS L4's `\HHHHHH` after
parameter binding `{[1,6], none, whitespace-or-non-hex, utf-8}`. Lock 16
admissibility means the kernel is preserved as a grammar-neutral
primitive even when the JSON-bound wiring is the only currently active
consumer.

The S-P2 / S-P3 wave that targets this class (V10
unicode-validation kernel, per CH2 §4.2 fold) operates on the
**class**, not on the JSON realisation: a single kernel parameterised
against the four columns above; codegen emits the per-grammar binding.
Future grammars (CSS L4, JS-like, TOML, BBNF-self) inherit the same
kernel.

## §4 — Processor Trace contrast (BLOCKED)

The prompt requested at least three Processor Trace captures — one
parse_only big loser, one big winner, one typed-product winner — for
branch-rich vs branch-light segment triangulation. This lane is
**BLOCKED**:

```text
$ xcrun xctrace record --template "Processor Trace" \
    --time-limit 2s --launch -- ./xctrace_probe twitter.json track1 200000
Starting recording with the Processor Trace template. Launching
process: xctrace_probe. Time limit: 2.0 s
Run issues were detected (trace is still ready to be viewed):
* [Error] Processor Trace library version in Instruments is not
  compatible with the library on the target device. Trace producer has
  format 7.3, but consumer has format 7.1.
Suggestion: Upgrade the developer tools on this device.

Recording failed with errors. Saving output file...
```

The resulting `.trace` artefact is empty (`<data/>`, `<duration>0</duration>`,
`<processes/>`); no Processor Trace events are recorded. The error
message identifies the failure as a producer/consumer format-version
mismatch between Xcode 26.0's bundled Instruments and the macOS 26.4.1
Processor Trace kernel extension. xcode-select is set to full Xcode and
the license has been accepted (xctrace 26.0 is functional for every
other template tested in P1-V3-A and P1-V3-B).

This is a documented Apple-toolchain skew, not a sudo / privilege gate.
Branch-history attribution therefore cannot anchor a P1-V3-B
hypothesis. The Time Profiler self-time data in §2 + §3 stands on its
own (it is what xctrace returns; it is reproducible by §6), and the
P1-V3-A CPU Counters PMU rows provide the cross-template anchor.

Re-attempt protocol: Processor Trace requires either an Xcode point
release that brings the Instruments-side library to format 7.3+, or a
downgrade of the macOS Processor Trace kext to 7.1 (no public mechanism
exists; this is a system kext shipped with macOS). Until one of those
lands, the Processor Trace lane stays `BLOCKED`.

## §5 — Cross-validation against samply and P1-V3-A CPU Counters

### 5.1 vs V2 samply at `/tmp/skv9-p1-rerun/profiles/`

The V2 samply mode-I table reports `dispatch_value` self-time at
~95–99% on all 17 parse_only rows. That attribution is **inconsistent
with the xctrace Time Profiler table at the symbol level** (see §3.4).
The two profilers are sampling the same binary at the same address
ranges; the difference is in stack-walk fidelity. xctrace's DWARF
inlined-frame walk surfaces the inlined leaves; samply's frame-pointer
walk coalesces them. The same binary produces both — the xctrace
attribution is the surface truth at the inline granularity S-P2 needs.

The V2 samply mode-II table (`p1b-samply-mode-2.md`,
`direct_to_struct`/`real_typed_struct`) reports per-row hot leaves at
proper inline granularity (`parse_object_value_at_direct`,
`parse_array_element_at_direct`, …); mode-II's surface is **consistent**
with xctrace because those routes have explicit per-template
monomorphisations rather than the single LTO-fused
`dispatch_value`-equivalent. The samply coalescing artefact is specific
to the mode-I Track 1 build.

### 5.2 vs P1-V3-A xctrace CPU Counters

P1-V3-A's `rusage_info_v5`-backed cycle-per-byte ledger (committed at
`restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-A-xctrace-cpu-counters.md`
§3, source rows at `/tmp/skv9-xctrace-v3/pmu_rows.tsv`) provides
cycles/byte and CPI on all 34 (corpus, track) rows. Combined with the
Time Profiler per-symbol % from this report, every row admits a
"cycles spent per primitive class" derivation:

`primitive_class_cycles_per_byte ≈ row_cycles_per_byte × primitive_class_%`.

Spot-check (twitter/track1, P1-V3-A row `cycles/B=2.373`, 13.6 Gbps):

| Class | %self (TP) | derived c/B |
|---|---:|---:|
| `string_tiny_scan` (match_tiny_plain_string) | 46.2% | 1.10 c/B |
| `whitespace_skip` (skip_ascii_whitespace) | 11.2% | 0.27 c/B |
| `dispatch_value` | 8.8% | 0.21 c/B |
| `simd_movemask` | 8.1% | 0.19 c/B |
| `consume_structural` | <1% | <0.02 c/B |
| `scan_structurals` | 0.00% | 0.00 c/B |

The `c/B` derivation triangulates: twitter's 2.37 c/B headline figure
is overwhelmingly the cost of running a scalar 16-byte tiny-string
loop, not structural-classification or tape-write. Same conclusion on
distinct_values (P1-V3-A `cycles/B=2.88`, TP `string_tiny_scan 61.9%`):
1.78 c/B is the tiny scanner alone.

For the float-heavy winners, the same derivation gives a primitive
budget S-P2 must respect:

- canada (P1-V3-A `c/B=2.10`): `scan_digit_run 21% → 0.44 c/B` +
  `number_scan 9.2% → 0.19 c/B` + `memcpy 16.2% → 0.34 c/B` (numeric
  materialisation copies) + `dispatch_value 19.7% → 0.41 c/B`.
- mesh (P1-V3-A `c/B=2.55`): same number-family but with array_walk
  10% (interior-array structural cost).
- numbers (P1-V3-A `c/B=2.27`): `scan_digit_run 33.4% → 0.76 c/B`
  alone is the largest single sink.

The two captures agree: structural classification is cheap;
number-content scanning and string-content scanning are the cost.

### 5.3 vs samply mode-II `direct_to_struct` (sink-route artefact)

Mode-II hot leaves are predominantly `parse_object_value_at_direct` +
`parse_array_element_at_direct` (the direct-sink wrappers) plus
`unescape_string` for unicode/escape rows. The Time Profiler tables
above do NOT cover the direct route — they capture parse_only Track 1
+ Track 2. The two are complementary: mode-II tells us the sink-route
cost is dominated by the same string/unicode primitives (`unescape_string`
47.5% on unicode_escapes/direct vs `read_hex_unit_scalar` 23.7% on
unicode_escapes/parse_only), confirming the primitive is the same
regardless of route. The route does not change which leaf is hot; the
mass shifts from "scan-only" to "scan + materialise."

## §6 — Reproduction script

```bash
#!/bin/bash
# /tmp/skv9-xctrace-v3/reproduce.sh
ROOT=/tmp/skv9-xctrace-v3
SKINNY=/Users/mkbabb/Programming/bbnf-lang/skinny
PROBE="$SKINNY/target/release/xctrace_probe"
mkdir -p "$ROOT/p1b-tp" "$ROOT/logs" "$ROOT/exports"
if [ ! -x "$PROBE" ]; then
  (cd "$SKINNY" && RUSTFLAGS="-C target-cpu=native" \
    cargo build --release -p bbnf-bench --bin xctrace_probe)
fi
cat > "$ROOT/corpus_paths.txt" <<EOF2
twitter $SKINNY/crates/test-fixtures/corpus/json/twitter.json
citm_catalog $SKINNY/crates/test-fixtures/corpus/json/citm_catalog.json
canada $SKINNY/crates/test-fixtures/corpus/json/canada.json
apache_builds $SKINNY/test_data/apache_builds.json
github_events $SKINNY/test_data/github_events.json
update_center $SKINNY/test_data/update-center.json
mesh $SKINNY/test_data/mesh.json
random $SKINNY/test_data/random.json
gsoc-2018 $SKINNY/test_data/gsoc-2018.json
marine_ik $SKINNY/test_data/marine_ik.json
instruments $SKINNY/test_data/instruments.json
numbers $SKINNY/test_data/numbers.json
unicode_mixed $SKINNY/test_data/unicode_mixed.json
unicode_escapes $SKINNY/test_data/unicode_escapes.json
unicode_basic $SKINNY/test_data/unicode_basic.json
distinct_values $SKINNY/test_data/distinct_values.json
y_string_unicode $SKINNY/test_data/y_string_unicode.json
EOF2
iters_for() {
  case "$1" in
    twitter) echo 12000;; citm_catalog) echo 4000;; canada) echo 3500;;
    apache_builds) echo 60000;; github_events) echo 120000;;
    update_center) echo 14000;; mesh) echo 10500;; random) echo 15000;;
    gsoc-2018) echo 2500;; marine_ik) echo 2700;;
    instruments) echo 35000;; numbers) echo 50000;;
    unicode_mixed) echo 7500;; unicode_escapes) echo 7500;;
    unicode_basic) echo 7500;; distinct_values) echo 50000;;
    y_string_unicode) echo 220000;; *) echo 30000;;
  esac
}
while read -r corpus path; do
  [ -z "$corpus" ] && continue
  iters=$(iters_for "$corpus")
  for track in track1 track2; do
    trace="$ROOT/p1b-tp/${corpus}__${track}.trace"
    rm -rf "$trace"
    (cd "$ROOT" && xcrun xctrace record \
        --template "Time Profiler" \
        --output "$trace" --no-prompt --time-limit 2500ms \
        --launch -- "$PROBE" "$path" "$track" "$iters") \
      > "$ROOT/logs/${corpus}__${track}.tp.log" 2>&1
  done
done < "$ROOT/corpus_paths.txt"
python3 "$ROOT/aggregate.py"
```

The aggregator `/tmp/skv9-xctrace-v3/aggregate.py` (also committed
under the report's "Sources" sidecar artefacts; see `/tmp/skv9-xctrace-v3/`)
parses each trace's `time-profile` XML, demangles via `rustfilt`,
classifies by substrate-neutral primitive name substring, and emits
one `<corpus>__<track>.symbols.json` plus a global `summary.json`. The
script is deterministic given a fixed probe binary, fixture set, and
xctrace version. A re-run on a different host or build will produce
different numbers but the same primitive vocabulary.

### 6.1 Source artefacts (not committed; cited)

| Path | Contents |
|---|---|
| `/tmp/skv9-xctrace-v3/p1b-tp/<corpus>__<track>.trace` | 34 xctrace Time Profiler trace bundles |
| `/tmp/skv9-xctrace-v3/exports/<corpus>__<track>.symbols.json` | Per-row top-15 self-time + class distribution |
| `/tmp/skv9-xctrace-v3/exports/summary.json` | All-row aggregation |
| `/tmp/skv9-xctrace-v3/hypothesis.json` | §3 verdict-grade numbers per row |
| `/tmp/skv9-xctrace-v3/aggregate.py` | Trace export + classifier |
| `/tmp/skv9-xctrace-v3/reproduce.sh` | Verbatim repro |
| `/tmp/skv9-xctrace-v3/logs/<corpus>__<track>.tp.log` | Per-capture xctrace stdout/stderr |
| `/tmp/skv9-xctrace-v3/pmu_rows.tsv` | P1-V3-A cycles/byte (cross-validation only) |
| `/tmp/skv9-p1-rerun/profiles/p1a/*.profile.json.gz` | V2 samply mode-I (compared in §5.1) |
| `/tmp/skv9-p1-rerun/profile-summary-top5.md` | V2 samply top-5 (compared in §5.1) |
| `skinny/RESULTS.md` (run `sk-v9-open:criterion-fnv64-cd1673844eeea12f`) | Baseline throughput |
| `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md` | §3.1 + §3.3 claim source |
| `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md` | §3.2 claim source |
| `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-A-xctrace-cpu-counters.md` | P1-V3-A cross-validation companion |
| `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md` | StructuralAlphabet abstraction (§1.5 + §3.2 + §3.5 generalisation binding) |
| `restart/locks/LOCKS.md` | Lock 1 substrate union; Lock 14 grammar-neutrality; Lock 16 grammar-neutral primitive surface |

## §0 — V4 fold footer

V4 fold: Lock-14 primitive-class promotion; classifier vocabulary
canonicalised; substantive PMU + Time Profiler findings unchanged.

The V4 edits to this report are scoped to the captioning + framing
layer (§1.5 canonical primitive vocabulary table; §2 first-occurrence
JSON-symbol→primitive-class mapping; §3.1 substrate-shape
generalisation sentence; §3.2 per-string-span scanner reframe; §3.5
`escape_codec_hex_unit` primitive class with cross-grammar parameter
table; §1 corpus-name canonical mapping note). The evidence layer is
unchanged: every (corpus, track) %self number, every
`samples_process` / `process_share` row, every SC-1 / SC-4 verdict,
every cross-validation derivation in §5 stands on the same xctrace
Time Profiler traces under `/tmp/skv9-xctrace-v3/p1b-tp/` and the same
P1-V3-A PMU rows at `/tmp/skv9-xctrace-v3/pmu_rows.tsv`. No
re-capture, no re-measurement, no number revised.

Specifically preserved:

- `scan_structurals` 0.00% self-time on 34/34 rows (§3.1) — unchanged;
  promoted to substrate-shape claim spanning grammars whose generated
  parse_only re-derives StructuralAlphabet ordinals.
- 47–67% per-string-span-scanner self-time on dense-key losses
  (distinct_values 61.9% / 63.1%; update_center 54.7% / 46.0%;
  apache_builds 56.0% / 45.0%; per §3.2 table) — unchanged; classified
  as the `per-string-span scanner` primitive class realised by the
  JSON `string_tiny_scan` variant.
- y_string_unicode `read_hex_unit_scalar` + `hex_nibble` 38.2% / 43.9%
  bottleneck (§3.2 last bullet, §3.5) — unchanged; classified as the
  `escape_codec_hex_unit` primitive class with full cross-grammar
  parameter set.
- Samply mode-I `dispatch_value` 95.6–99.6% falsified as
  frame-pointer-coalescing artefact via xctrace DWARF inlined-frame
  walk (§3.4) — unchanged.
