# Lazy-Mode Parse Profile Report — Twitter Track 1

## Setup

- Corpus: `crates/test-fixtures/corpus/json/twitter.json` (631515 bytes)
- Binary: `target/release/profile-lazy` (release profile, `debug = true`, `lto = "thin"`)
- Driver: 80000 iterations of `runtime::generated_json::parse(input)`
- Profiler: `samply 0.13.1` at 4000 Hz, `--save-only --unstable-presymbolicate`
- Wall time observed: 40.06 s @ 10090 Mbps (matches recent outcome-G regime; reproducible refutation evidence on this host)
- Sample count: 159525 raw samples / 159531 weighted (interval 0.25 ms)
- Symbol resolution: addresses extracted from gecko profile, resolved via `xcrun atos -arch arm64 -l 0x100000000` against `target/release/profile-lazy`, demangled via `rustfilt`. 573/573 profile-lazy frame addresses resolved.

## Artefacts

- `lazy-mode-profile.json.gz` — raw samply gecko profile (1.2 MB)
- `lazy-mode-profile.json` — decompressed (3.5 MB)
- `lazy-mode-profile.json.syms.json` — samply's presymbolicated sidecar (sparse)
- `lazy-mode-profile.folded` — inferno collapsed-stack format (95 unique stacks)
- `lazy-mode-profile.svg` — flamegraph rendered by `inferno-flamegraph`
- `lazy-mode-profile.txt` — raw attribution dump (top-25, hypothesis buckets)
- `lazy-mode-profile.txt.json` — machine-readable per-function self / inclusive times

## Top Functions by Self Time

| Rank | Self % | Self samples | Function |
|---|---|---|---|
| 1 | 36.36% | 58005 | `simd_scan::scan_json_parse_index` |
| 2 | 35.58% | 56767 | `runtime::generated_json::generated::parse_value` |
| 3 | 10.28% | 16397 | `runtime::generated_json::generated::consume_structural` |
| 4 |  9.13% | 14570 | `runtime::generated_json::generated::parse_string` |
| 5 |  5.27% |  8410 | `parse_that_regex::match_json_string` |
| 6 |  1.59% |  2539 | `runtime::generated_json::generated::parse_literal` |
| 7 |  1.15% |  1833 | `[libsystem_platform.dylib]` (memcpy / memset internals) |
| 8 |  0.27% |   427 | `DYLD-STUB$$memcmp` |
| 9 |  0.23% |   362 | `[libsystem_malloc.dylib]` |
| 10 | 0.12% |   196 | `[libsystem_kernel.dylib]` |
| 11 | 0.01% |    11 | `<runtime::tape::assembler::TapeAssembler>::finish` |
| 12 | 0.01% |    10 | `runtime::generated_json::parser::parse` |
| 13 | 0.00% |     2 | `core::str::converts::from_utf8` |
| 14 | 0.00% |     2 | `DYLD-STUB$$realloc` |

(Only 14 distinct symbols dominate self time after inlining at `-O3` + `lto = "thin"`.
The aggressive inlining is why `parse_object`, `parse_array`, `parse_pair`, `parse_number`,
`skip_ws`, `peek`, `consume`, `emit_offset`, and most `TapeAssembler` ops disappear into
`parse_value`'s self bucket.)

## Top Functions by Inclusive Time

| Rank | Incl % | Incl samples | Function |
|---|---|---|---|
| 1 | 100.00% | 159531 | `main` / `lang_start` / `profile_lazy::main` |
| 2 |  99.96% | 159468 | `runtime::generated_json::parser::parse` (= entry point of `generated_json::parse`) |
| 3 |  63.27% | 100943 | `runtime::generated_json::generated::parse_value` |
| 4 |  36.41% |  58081 | `simd_scan::scan_json_parse_index` |
| 5 |  22.18% |  35379 | `runtime::generated_json::generated::parse_string` |
| 6 |  10.28% |  16397 | `runtime::generated_json::generated::consume_structural` |
| 7 |   5.27% |   8410 | `parse_that_regex::match_json_string` |
| 8 |   3.01% |   4799 | `runtime::generated_json::generated::parse_literal` |
| 9 |   1.15% |   1833 | `[libsystem_platform.dylib]` |
| 10 |  0.35% |    558 | `[libsystem_malloc.dylib]` |
| 11 |  0.27% |    427 | `DYLD-STUB$$memcmp` |
| 12 |  0.16% |    254 | `<runtime::tape::assembler::TapeAssembler>::finish` |
| 13 |  0.12% |    196 | `[libsystem_kernel.dylib]` |
| 14 |  0.00% |      2 | `core::str::converts::from_utf8` |
| 15 |  0.00% |      2 | `DYLD-STUB$$realloc` |

`scan_json_parse_index` and `match_json_string` are leaves (self == inclusive).
`parse_value` and `parse_string` differ because they include their callees that survived inlining.

## Hypothesis Attribution (self time)

| Bucket | Self % | Self samples |
|---|---|---|
| **H1 — parser control flow + tape assembly** | **56.60%** | 90294 |
| **H3 — structural scan** | **36.36%** | 58005 |
| **H2 — string decode (raw)** | **5.27%** | 8410 |
| memory_alloc | 0.23% | 362 |
| utf8_validate | 0.00% | 2 |
| other (libplatform memcpy, dyld stubs, kernel) | 1.54% | 2458 |

`H4 — bounds checks` and `H5 — view materialization` contribute **0** measurable
samples in this run: no `panic_bounds_check` / `slice::index` symbol appears, and no
`view::` / `kind_at_cursor` / `JsonValue` / `token_stream` / `to_canonical_string`
symbol appears. The lazy-mode parse is parse-only — no view is realised during the
hot loop — so H5 is structurally absent, not just small. H4's absence confirms LLVM
elided every bounds check that survived the structural-scan / structural-cursor
contract; bounds checks are not on the path.

## Conclusion

The cycles divide cleanly between three buckets, and only three:

1. **H1 — parser control flow + tape assembly (~57% self)** is the dominant sink.
   It is concentrated in **`parse_value` (35.58% self)** — the dispatch + skip_ws +
   peek hub — plus **`consume_structural` (10.28%)**, **`parse_string` (9.13%)**, and
   **`parse_literal` (1.59%)**. Everything that survived inlining at `parse_value`
   (the `match peek(state)` dispatch, the recursive descent into `parse_object`/
   `parse_array`/`parse_pair`, the inlined `skip_ws`/`emit_offset`/`TapeAssembler`
   pushes) folded into that one symbol's self bucket. `TapeAssembler::finish`
   itself measures at 0.01% self, so tape *finalization* is free — the offset
   pushes inlined into `parse_value`/`parse_string` are where the tape cost lives.

2. **H3 — structural scan (~36% self)** is a single leaf:
   `simd_scan::scan_json_parse_index`. It is the prepass that produces the offset
   index `parse_value` consults via `state.structural_cursor`. The canada
   scan-floor headline of 48362 Mbps is *throughput in isolation*; here the scan
   is paying its proportional cost on twitter, and the scan stage is roughly
   one-third of T1. So the H3 hypothesis "negligible" prediction is **wrong** —
   scan is large in absolute terms, just not unbounded.

3. **H2 — string decode (~5% self, all in `match_json_string`)** is the smallest
   meaningful bucket and is *not* masked: it appears explicitly in `parse_string`
   only when `needs_unescape` is true (the `string_escape_offsets` contains an
   offset inside the value's content range). On twitter most strings are
   escape-free, so the regex match is only triggered for a minority — hence 5.27%.
   No `decode_json_string` or `host_call` symbol exists in the binary at all;
   "eager decode" in the lazy-tape mode is not occurring on the hot path.

H4 (bounds) and H5 (view materialization) are **floor**, not contributors.

**Where the cycles actually go (one paragraph):** Roughly 57% of cycles burn in
the recursive-descent driver `parse_value` and its inlined neighborhood
(`skip_ws`, `peek`, `consume_structural`, `parse_string`'s structural-cursor
fetch, `parse_literal`'s 4-byte `memcmp`, the offset-tape pushes), 36% burn in
the leaf SIMD prepass `scan_json_parse_index`, and 5% burn in `match_json_string`
for the minority of strings that contain escapes. Bounds checks, UTF-8
validation, allocator traffic, and any view-materialization path are
non-contributors (≤1.4% combined). The refutation threshold gap (~13 Gbps target
vs. ~10–11.8 Gbps observed) sits squarely on **two** levers: the parser
dispatch + offset-emit body inside `parse_value`, and the SIMD scan stage —
*not* on string decode and not on view materialization. Any optimization that
does not move one of those two needles will not close the gap.
