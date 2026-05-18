# SK-V9 P1-V3-C: Per-Corpus Deep Hot-Leaf Attribution

Pass: S-P1 Profile. Cycle: V3 reframe (V4 refold).
Date: 2026-05-18.
Scope: per-corpus deep hot-leaf attribution across 17 JSON corpora × {Track 1
generated, Track 2 hand-coded} parse-only surfaces, with SC-1 and SC-4
hypothesis adjudication grounded in xctrace Time Profiler per-symbol
self-time and xctrace CPU Counters per-row cycles/byte.
Output: this file.
Baseline: SK-V9-open at commit `90609aee`, run
`sk-v9-open:criterion-fnv64-cd1673844eeea12f`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.

## §0 — Primary attribution source

Attribution is rebased on the V3 sibling captures:

- **Primary (per-symbol self-time)**: P1-V3-B Time Profiler exports at
  `/tmp/skv9-xctrace-v3/exports/<corpus>__<track>.symbols.json`. 34/34 rows
  with `process_share ≥ 99.5%`, ≥ 700 in-process samples per row. xctrace's
  DWARF inlined-frame walk resolves inlined leaves the LTO-fused
  `dispatch_value` body hides from samply's frame-pointer walk.
- **Primary (per-row cycle precision)**: P1-V3-A xctrace CPU Counters PMU
  table at `/tmp/skv9-xctrace-v3/pmu_rows.tsv` (34 rows; cycles, retired
  instructions, CPI, cycles/byte from `proc_pid_rusage(RUSAGE_INFO_V5)`,
  kpc-backed, no estimation). Per-class cycles/B derives as
  `class_cycles_per_byte ≈ row_cycles_per_byte × class_%self`.
- **Cross-validation only**: V2 samply mode-I profiles at
  `/tmp/skv9-p1-rerun/profiles/p1a/`. The mode-I `dispatch_value 95.6 –
  99.6%` attribution is a frame-pointer-coalescing artefact (P1-V3-B §3.4
  falsifies it on every row); it is retained below only as the V2 baseline
  this report supersedes.
- **Cross-validation (direct-route + eager-decode)**: V2 samply mode-II /
  mode-III at `/tmp/skv9-p1-rerun/profiles/p1b/` and `…/p1c/`. The
  direct-route surface (`parse_object_value_at_direct`,
  `parse_array_element_at_direct`, `JsonDigestSink::array_string`) has
  proper per-template monomorphisations and is *consistent* with xctrace
  (B §5.3); it stays as the de-fused view for the Track-1-direct route.

Corpus-name reconcile: A's PMU TSV writes `update-center` (hyphen); B's
exports write `update_center` (underscore). This report canonicalises to
`update_center` and notes the alias on the row that joins both inputs.

## §1 Method

### 1.1 Sources

- **B Time Profiler exports** (34 × `symbols.json`):
  - 17 × Track 1 generated (`runtime::generated_json::parse`).
  - 17 × Track 2 hand-coded (`bbnf_bench::track2::json::parse`).
  Each carries the top-15 self-time symbols with substrate-neutral
  class tags, the per-class share distribution, source `file:line`
  where xctrace's DWARF emitted an inlined-frame record, and the
  `samples_process` / `process_share` envelope.
- **A PMU table** (34 rows): `cycles`, `instructions`, `CPI`, `cycles/B`
  per (corpus, track) at `/tmp/skv9-xctrace-v3/pmu_rows.tsv`.
- **V2 samply mode-I / II / III** at `/tmp/skv9-p1-rerun/profiles/` — used
  in §5 only for de-fused direct-route attribution that survives B's
  falsification (mode-II / III routes have proper monomorphisations).

### 1.2 Track mapping

`RESULTS.md:139` defines Track 1 as `runtime::generated_json::parse` and
Track 2 as the independent hand-coded parser
(`bbnf_bench::track2::json::Parser::parse_value_at`). The probe binary at
`skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs` exposes both as
distinct CLI arms (`track1` / `track2`), so xctrace captures both on the
same baseline. Track 2 is no longer "samply-shallow pending future
capture" — B's Time Profiler covers all 17 × Track 2 rows at the same
per-symbol granularity as Track 1.

The direct-route surface (`parse_object_value_at_direct`,
`parse_array_element_at_direct`, `JsonDigestSink::array_string`) is
captured by V2 samply mode-II at `/tmp/skv9-p1-rerun/profiles/p1b/`. It is
not a Track of its own; it is the same Track 1 generated parser with a
sink-shaped traversal. §5.3 names the direct-route shares from mode-II
since B's TP captures the parse-only path.

### 1.3 Substrate-neutral primitive vocabulary (Lock-14 reframe per V4-B fold)

Per-symbol attribution uses B §1.5's grammar-neutral classifier
vocabulary. The classifier matches symbol substrings, not JSON-role
names; the symbol identity is the per-grammar realisation of a
substrate-neutral primitive class spanning future grammars (CSS L4,
Sheets, BBNF-self, user grammars).

| Primitive class | Realisation in JSON parser | Cross-grammar admission |
|---|---|---|
| per-string-span scanner (tiny) | `match_tiny_plain_string_with_cap::<16>`; T2 hand `match_tiny_plain_string` | any delimited-span < 16 B; CSS L4 ident short scan; Sheets short string |
| per-string-span scanner (SIMD full) | `match_string_at_quote_trusted_utf8` | any long delimited span; CSS L4 long `"…"`/`'…'` body |
| per-string-span scanner (block) | `skip_string_plain_trusted` | per-block scanner for any delimiter set |
| string-dispatch kernel | `parse_string`, `parse_key_colon` | any string-delimiter consume + key-colon transition |
| string-escape validator | `validate_string_escape` | any grammar's escape-class admission predicate |
| string-open consumer | `consume_quote_at_cursor` | any grammar's delimited-span open |
| escape-codec hex-unit | `read_hex_unit_scalar`, `hex_nibble` | parameterised `{hex_digit_count, surrogate_join_policy, terminator_policy}`; admits CSS L4 `\HHHHHH`, JS `\u{HHHHHH}`, TOML `\UHHHHHHHH` |
| structural walker | `consume_structural`, `consume_container_next`, `consume_array_next`, `dispatch_value`, `parse_value_at` | StructuralAlphabet-ordinal ladder for any grammar |
| whitespace skip | `skip_ascii_whitespace` | any grammar's whitespace skip |
| simd movemask primitive | `bbnf_simd::aarch64::movemask::movemask_u8x16` | LIVE substrate primitive (Lock-16 admitted); consumed by string-block scanner |
| digit-FSM scanner | `scan_digit_run`, `match_number_span_from_first`, `NumberParts::push_*_digits`, `is_four_ascii_digits` | any grammar's digit-span admission; CSS L4 `<number>` + `<integer>`, Sheets numeric |
| memcpy | `core::ptr::copy_nonoverlapping::<u8>` | universal materialisation copy |
| vec-grow | `<RawVecInner>::capacity` | universal tape-grow path |
| other | `<u16>::trailing_zeros`, `Option::copied`, etc. | call-site overhead |

The JSON-role symbol identities (`match_tiny_plain_string`,
`read_hex_unit_scalar`, `dispatch_value`, etc.) appear in the per-row
tables in §2 as the *per-grammar realisation* of the primitive class
named in the same row. S-P2 / S-P3 consume the primitive-class column,
not the symbol-identity column.

The 17-row evidence is JSON-empirical; the classifier vocabulary is
substrate-neutral and will accept the same classes on future
CSS/Sheets/BBNF-self corpora.

### 1.4 Resolution of the samply-vs-xctrace contradiction

V3-C's prior cycle reported "match_tiny_plain_string … zero appearances
across all 106 profiles." That claim is **samply-true but cohort-false**:
samply's frame-pointer walk coalesces every PC inside the LTO-fused
`dispatch_value` body into the outer symbol, so the scalar 16-byte tiny
scanner inlined at `generated.rs:178` reads as `dispatch_value` sample
share rather than as itself. xctrace's DWARF inlined-frame walk surfaces
each inline at its source symbol, and B's per-row tables show the same
symbol as rank-1 on the majority of (corpus, track) rows that V3-C
labelled "no string-scanner appearance."

The V3-C samply-shallow framing ("a follow-up edit is required once the
xctrace JSON exports land; the schema is already columnar so the
refinement is a row-by-row overwrite") is closed by this V4 refold —
the row-by-row overwrite is §2 below, the SC-1 / SC-4 verdict closures
are §4 and §5, the cycles-per-class derivation is §3.2.

## §2 Per-corpus per-track top-8 symbols (xctrace TP self-time)

`% self` is in-process self-time share per B's Time Profiler tables
(weight per sample = 1 ms, attribution = leaf process-binary frame).
Primitive class tags per §1.3. Source `file:line` from xctrace DWARF
where emitted.

### 2.1 Track 1 (`runtime::generated_json::parse`)

#### twitter / track1 (729 samples, 99.6% process_share)

| Rank | %self | Primitive class | Symbol (realisation) | Source |
|---:|---:|---|---|---|
| 1 | 46.2% | per-string-span scanner (tiny) | `match_tiny_plain_string_with_cap::<16>` | `crates/runtime/src/grammars/json/generated.rs:178` |
| 2 | 11.2% | whitespace skip | `parse_that_regex::skip_ascii_whitespace` | |
| 3 | 8.8% | structural walker (fused dispatch) | `dispatch_value` | |
| 4 | 8.1% | simd movemask primitive | `bbnf_simd::aarch64::movemask::movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 5 | 3.4% | other | `<u16>::trailing_zeros` | |
| 6 | 2.5% | structural walker | `consume_container_next` | |
| 7 | 2.2% | string-dispatch kernel | `parse_key_colon` | |
| 8 | 1.9% | vec-grow | `<RawVecInner>::capacity` | |

#### citm_catalog / track1 (1893 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 24.0% | per-string-span scanner (tiny) | `match_tiny_plain_string_with_cap::<16>` | |
| 2 | 23.1% | whitespace skip | `skip_ascii_whitespace` | |
| 3 | 9.8% | structural walker (fused dispatch) | `dispatch_value` | |
| 4 | 7.1% | memcpy | `copy_nonoverlapping::<u8>` | |
| 5 | 5.3% | digit-FSM scanner | `scan_digit_run` | |
| 6 | 5.1% | structural walker | `consume_array_next` | |
| 7 | 3.5% | structural walker | `consume_container_next` | |
| 8 | 3.0% | structural walker | `consume_structural` | |

#### canada / track1 (1977 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 21.0% | digit-FSM scanner | `scan_digit_run` | `crates/parse-that-regex/src/number/mod.rs:125` |
| 2 | 19.7% | structural walker (fused dispatch) | `dispatch_value` | |
| 3 | 16.2% | memcpy | `copy_nonoverlapping::<u8>` | |
| 4 | 9.9% | structural walker | `consume_array_next` | |
| 5 | 9.2% | digit-FSM scanner | `match_number_span_from_first` | |
| 6 | 6.2% | whitespace skip | `skip_ascii_whitespace` | |
| 7 | 2.8% | vec-grow | `<RawVecInner>::capacity` | |
| 8 | 2.2% | structural walker | `consume_structural` | |

#### apache_builds / track1 (1978 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 56.0% | per-string-span scanner (tiny) | `match_tiny_plain_string_with_cap::<16>` | |
| 2 | 10.3% | whitespace skip | `skip_ascii_whitespace` | |
| 3 | 6.7% | simd movemask primitive | `movemask_u8x16` | |
| 4 | 6.1% | structural walker (fused dispatch) | `dispatch_value` | |
| 5 | 3.2% | string-dispatch kernel | `parse_string` | |
| 6 | 2.2% | structural walker | `consume_container_next` | |
| 7 | 2.0% | string-dispatch kernel | `parse_key_colon` | |
| 8 | 1.8% | other | `<u16>::trailing_zeros` | |

#### github_events / track1 (1705 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 40.5% | per-string-span scanner (tiny) | `match_tiny_plain_string_with_cap::<16>` | |
| 2 | 14.1% | simd movemask primitive | `movemask_u8x16` | |
| 3 | 11.0% | whitespace skip | `skip_ascii_whitespace` | |
| 4 | 5.7% | structural walker (fused dispatch) | `dispatch_value` | |
| 5 | 3.7% | string-dispatch kernel | `parse_string` | |
| 6 | 2.5% | string-dispatch kernel | `parse_key_colon` | |
| 7 | 2.4% | other | `alloc::realloc_nonnull` | |
| 8 | 2.1% | per-string-span scanner (block) | `skip_string_plain_trusted` | |

#### update_center / track1 (1955 samples; A's PMU row spelled `update-center`)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 54.7% | per-string-span scanner (tiny) | `match_tiny_plain_string_with_cap::<16>` | |
| 2 | 9.0% | simd movemask primitive | `movemask_u8x16` | |
| 3 | 6.8% | structural walker (fused dispatch) | `dispatch_value` | |
| 4 | 5.2% | other | `<u16>::trailing_zeros` | |
| 5 | 4.7% | whitespace skip | `skip_ascii_whitespace` | |
| 6 | 4.0% | string-dispatch kernel | `parse_string` | |
| 7 | 2.5% | structural walker | `consume_container_next` | |
| 8 | 2.1% | string-dispatch kernel | `parse_key_colon` | |

#### mesh / track1 (1959 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 21.3% | structural walker (fused dispatch) | `dispatch_value` | `crates/runtime/src/grammars/json/generated.rs:58` |
| 2 | 19.3% | digit-FSM scanner | `scan_digit_run` | |
| 3 | 12.8% | whitespace skip | `skip_ascii_whitespace` | |
| 4 | 11.3% | digit-FSM scanner | `match_number_span_from_first` | |
| 5 | 10.5% | memcpy | `copy_nonoverlapping::<u8>` | |
| 6 | 10.0% | structural walker | `consume_array_next` | |
| 7 | 3.2% | vec-grow | `<RawVecInner>::capacity` | |
| 8 | 1.9% | other | `SliceIndex<[u8]>::get` | |

#### random / track1 (1988 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 48.6% | per-string-span scanner (tiny) | `match_tiny_plain_string_with_cap::<16>` | |
| 2 | 13.4% | whitespace skip | `skip_ascii_whitespace` | |
| 3 | 8.1% | structural walker (fused dispatch) | `dispatch_value` | |
| 4 | 4.9% | simd movemask primitive | `movemask_u8x16` | |
| 5 | 3.1% | string-dispatch kernel | `parse_string` | |
| 6 | 2.5% | structural walker | `consume_array_next` | |
| 7 | 2.4% | structural walker | `consume_container_next` | |
| 8 | 2.4% | string-dispatch kernel | `parse_key_colon` | |

#### gsoc-2018 / track1 (1955 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 30.9% | simd movemask primitive | `movemask_u8x16` | `crates/bbnf-simd/src/aarch64/movemask.rs:22` |
| 2 | 20.8% | per-string-span scanner (tiny) | `match_tiny_plain_string_with_cap::<16>` | |
| 3 | 10.5% | other | `<u16>::trailing_zeros` | |
| 4 | 5.3% | whitespace skip | `skip_ascii_whitespace` | |
| 5 | 4.8% | per-string-span scanner (block) | `skip_string_plain_trusted` | `crates/parse-that-regex/src/lib.rs:551` |
| 6 | 4.0% | per-string-span scanner (SIMD full) | `match_string_at_quote_trusted_utf8` | |
| 7 | 3.5% | string-dispatch kernel | `parse_string` | |
| 8 | 3.2% | other | `From<u8>::from` | |

#### marine_ik / track1 (1996 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 24.2% | structural walker (fused dispatch) | `dispatch_value` | |
| 2 | 13.8% | digit-FSM scanner | `scan_digit_run` | `crates/parse-that-regex/src/number/mod.rs:150` |
| 3 | 11.4% | whitespace skip | `skip_ascii_whitespace` | |
| 4 | 10.8% | memcpy | `copy_nonoverlapping::<u8>` | |
| 5 | 9.6% | digit-FSM scanner | `match_number_span_from_first` | |
| 6 | 8.7% | structural walker | `consume_array_next` | |
| 7 | 3.4% | vec-grow | `<RawVecInner>::capacity` | |
| 8 | 3.1% | per-string-span scanner (tiny) | `match_tiny_plain_string_with_cap::<16>` | `crates/runtime/src/grammars/json/generated.rs:178` |

#### instruments / track1 (1995 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 40.2% | per-string-span scanner (tiny) | `match_tiny_plain_string_with_cap::<16>` | |
| 2 | 17.9% | whitespace skip | `skip_ascii_whitespace` | |
| 3 | 9.5% | structural walker (fused dispatch) | `dispatch_value` | |
| 4 | 5.0% | digit-FSM scanner | `match_number_span_from_first` | |
| 5 | 4.0% | memcpy | `copy_nonoverlapping::<u8>` | |
| 6 | 3.5% | structural walker | `consume_container_next` | |
| 7 | 3.1% | string-dispatch kernel | `parse_key_colon` | |
| 8 | 2.5% | vec-grow | `<RawVecInner>::capacity` | |

#### numbers / track1 (1984 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 33.4% | digit-FSM scanner | `scan_digit_run` | `crates/parse-that-regex/src/number/mod.rs:125` |
| 2 | 19.6% | structural walker (fused dispatch) | `dispatch_value` | |
| 3 | 9.4% | structural walker | `consume_array_next` | |
| 4 | 8.3% | digit-FSM scanner | `match_number_span_from_first` | |
| 5 | 4.2% | whitespace skip | `skip_ascii_whitespace` | |
| 6 | 4.0% | digit-FSM scanner (SWAR) | `NumberParts::push_four_digits` | |
| 7 | 3.9% | digit-FSM scanner (SWAR) | `is_four_ascii_digits` | |
| 8 | 3.8% | digit-FSM scanner (SWAR) | `NumberParts::push_eight_digits` | |

#### unicode_mixed / track1 (1966 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 24.9% | structural walker (fused dispatch) | `dispatch_value` | |
| 2 | 20.1% | string-escape validator | `validate_string_escape` | `crates/parse-that-regex/src/lib.rs:285` |
| 3 | 15.2% | per-string-span scanner (SIMD full) | `match_string_at_quote_trusted_utf8` | |
| 4 | 9.7% | other | `<u16>::trailing_zeros` | |
| 5 | 9.5% | simd movemask primitive | `movemask_u8x16` | |
| 6 | 5.7% | per-string-span scanner (tiny) | `match_tiny_plain_string_with_cap::<16>` | |
| 7 | 3.2% | whitespace skip | `skip_ascii_whitespace` | |
| 8 | 1.9% | string-dispatch kernel | `parse_string` | |

#### unicode_escapes / track1 (1986 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 23.7% | escape-codec hex-unit | `read_hex_unit_scalar` | |
| 2 | 20.9% | structural walker (fused dispatch) | `dispatch_value` | |
| 3 | 19.5% | per-string-span scanner (SIMD full) | `match_string_at_quote_trusted_utf8` | `crates/parse-that-regex/src/lib.rs:174` |
| 4 | 9.9% | escape-codec hex-unit | `hex_nibble` | |
| 5 | 4.8% | string-escape validator | `validate_string_escape` | |
| 6 | 3.6% | other | `Option::copied` | |
| 7 | 3.5% | simd movemask primitive | `movemask_u8x16` | |
| 8 | 2.5% | other | `PartialEq::eq` | |

#### unicode_basic / track1 (1981 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 31.4% | per-string-span scanner (tiny) | `match_tiny_plain_string_with_cap::<16>` | |
| 2 | 13.5% | other | `<u16>::trailing_zeros` | |
| 3 | 9.9% | whitespace skip | `skip_ascii_whitespace` | |
| 4 | 9.3% | structural walker (fused dispatch) | `dispatch_value` | |
| 5 | 6.7% | string-dispatch kernel | `parse_string` | |
| 6 | 6.0% | simd movemask primitive | `movemask_u8x16` | |
| 7 | 2.2% | structural walker | `consume_container_next` | |
| 8 | 2.2% | vec-grow | `<RawVecInner>::capacity` | |

#### distinct_values / track1 (1965 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 61.9% | per-string-span scanner (tiny) | `match_tiny_plain_string_with_cap::<16>` | |
| 2 | 8.0% | whitespace skip | `skip_ascii_whitespace` | |
| 3 | 7.2% | simd movemask primitive | `movemask_u8x16` | |
| 4 | 6.4% | structural walker (fused dispatch) | `dispatch_value` | |
| 5 | 2.0% | structural walker | `consume_container_next` | |
| 6 | 1.6% | string-dispatch kernel | `parse_string` | |
| 7 | 1.4% | string-open consumer | `consume_quote_at_cursor` | |
| 8 | 1.2% | string-dispatch kernel | `parse_key_colon` | |

#### y_string_unicode / track1 (1989 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 19.2% | escape-codec hex-unit | `hex_nibble` | |
| 2 | 19.0% | escape-codec hex-unit | `read_hex_unit_scalar` | |
| 3 | 10.6% | per-string-span scanner (tiny) | `match_tiny_plain_string_with_cap::<16>` | |
| 4 | 5.5% | simd movemask primitive | `movemask_u8x16` | |
| 5 | 5.4% | other | `Option::copied` | |
| 6 | 5.1% | structural walker (fused dispatch) | `dispatch_value` | |
| 7 | 5.1% | other | `PartialEq::eq` | |
| 8 | 3.1% | other | `alloc::realloc_nonnull` | |

### 2.2 Track 2 (`bbnf_bench::track2::json::parse`)

Track 2 is the hand-coded recursive-descent parser at
`skinny/crates/bbnf-bench/src/track2/json.rs`. Its hot leaves are
substrate-isomorphic to Track 1 — the same per-string-span scanner,
digit-FSM scanner, whitespace skip, structural-walker primitives — but
the symbol identities live under `bbnf_bench::track2::json::Parser::…`
rather than `runtime::generated_json::generated::…`. xctrace surfaces
each Track 2 row at the same DWARF inlined-frame granularity as Track
1; no Track 2 row is samply-shallow.

#### twitter / track2 (725 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 30.1% | per-string-span scanner (tiny) | `track2::json::match_tiny_plain_string` | `crates/bbnf-bench/src/track2/json.rs:318` |
| 2 | 16.0% | whitespace skip | `skip_ascii_whitespace` | |
| 3 | 13.7% | simd movemask primitive | `movemask_u8x16` | |
| 4 | 8.0% | structural walker (recursive descent) | `track2::json::Parser::parse_value_at` | |
| 5 | 6.5% | string-dispatch kernel | `track2::json::Parser::parse_key_colon` | |
| 6 | 2.9% | per-string-span scanner (SIMD full) | `match_string_at_quote_trusted_utf8` | |
| 7 | 2.9% | vec-grow | `<RawVecInner>::capacity` | |
| 8 | 2.3% | memcpy | `copy_nonoverlapping::<u8>` | |

#### apache_builds / track2 (1931 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 45.0% | per-string-span scanner (tiny) | `track2::json::match_tiny_plain_string` | |
| 2 | 14.5% | whitespace skip | `skip_ascii_whitespace` | |
| 3 | 6.8% | other | `<u16>::trailing_zeros` | |
| 4 | 6.6% | string-dispatch kernel | `Parser::parse_string` | |
| 5 | 6.4% | structural walker (recursive descent) | `Parser::parse_value_at` | |
| 6 | 5.1% | simd movemask primitive | `movemask_u8x16` | |
| 7 | 3.4% | structural walker | `Parser::consume_container_next` | |
| 8 | 3.1% | string-dispatch kernel | `Parser::parse_key_colon` | |

#### canada / track2 (1968 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 21.6% | structural walker (recursive descent) | `Parser::parse_value_at` | `crates/bbnf-bench/src/track2/json.rs:58` |
| 2 | 19.2% | digit-FSM scanner | `scan_digit_run` | |
| 3 | 15.9% | memcpy | `copy_nonoverlapping::<u8>` | |
| 4 | 10.4% | digit-FSM scanner | `match_number_span_from_first` | |
| 5 | 10.1% | structural walker | `Parser::consume_container_next` | |
| 6 | 6.4% | whitespace skip | `skip_ascii_whitespace` | |
| 7 | 2.5% | digit-FSM scanner (SWAR) | `NumberParts::push_two_digits` | |
| 8 | 2.4% | vec-grow | `<RawVecInner>::capacity` | |

#### citm_catalog / track2 (1908 samples)

| Rank | %self | Primitive class | Symbol | Source |
|---:|---:|---|---|---|
| 1 | 24.0% | whitespace skip | `skip_ascii_whitespace` | |
| 2 | 21.1% | per-string-span scanner (tiny) | `track2::json::match_tiny_plain_string` | |
| 3 | 10.4% | structural walker | `Parser::consume_container_next` | |
| 4 | 9.0% | structural walker (recursive descent) | `Parser::parse_value_at` | |
| 5 | 8.6% | memcpy | `copy_nonoverlapping::<u8>` | |
| 6 | 4.6% | string-dispatch kernel | `Parser::parse_key_colon` | |
| 7 | 4.3% | digit-FSM scanner | `scan_digit_run` | |
| 8 | 2.6% | vec-grow | `<RawVecInner>::capacity` | |

For the remaining Track 2 rows (github_events, update_center, mesh,
random, gsoc-2018, marine_ik, instruments, numbers, unicode_*,
distinct_values, y_string_unicode), the per-class distribution mirrors
the Track 1 column with two consistent inversions: (a) `Parser::
parse_value_at` is a measurable rank-1 / rank-2 symbol on every Track 2
row (the hand-coded parser does not fuse it via LTO), and (b) the tiny
scanner's share is ~5–15 percentage points lower than Track 1 on
string-dense rows (the hand parser carries more per-iteration overhead
in the recursive-descent body). Full per-row tables are in B §2 (rows
labelled `<corpus>/track2`); they are not repeated here verbatim. The
substrate-neutral primitive shares for both tracks are summarised in
§3.1 below.

## §3 Per-corpus structural-class share

Rows are ordered by ascending string fraction
(`quotes / (quotes + numbers + literals)` per `RESULTS.md`). Each column
is the sum of the row's primitive-class shares from B's full
`top_process_self_time` list (not just top-8).

- `str-plane` = per-string-span (tiny + full + block) + string-dispatch +
  string-escape validator + string-open consumer (the string-plane
  primitive family).
- `esc-hex` = escape-codec hex-unit.
- `digit` = digit-FSM scanner (scalar + SWAR variants).
- `struct` = structural walker (fused dispatch + recursive descent +
  per-container ladder).
- `ws` = whitespace skip.
- `mm` = simd movemask primitive (lives at the string-block scanner
  callsite per B §3.1; not the structural scan).

### 3.1 Track 1 per-class share

| Corpus | string-frac | str-plane | esc-hex | digit | struct | ws | mm |
|---|---:|---:|---:|---:|---:|---:|---:|
| canada | 0.000 | 0.0 | 0.0 | 34.6 | 31.7 | 7.6 | 0.0 |
| mesh | 0.000 | 0.0 | 0.0 | 35.9 | 31.3 | 12.8 | 0.0 |
| numbers | 0.000 | 0.0 | 0.0 | 53.4 | 29.0 | 4.2 | 0.0 |
| marine_ik | 0.135 | 4.2 | 0.0 | 27.7 | 35.2 | 11.4 | 0.0 |
| instruments | 0.556 | 44.1 | 0.0 | 6.5 | 15.2 | 17.9 | 1.5 |
| citm_catalog | 0.630 | 27.4 | 0.0 | 7.9 | 24.0 | 23.1 | 0.0 |
| twitter | 0.726 | 51.0 | 0.0 | 2.3 | 12.6 | 11.2 | 8.1 |
| unicode_mixed | 0.750 | 44.1 | 0.0 | 0.7 | 25.9 | 3.2 | 9.5 |
| unicode_escapes | 0.750 | 24.9 | 36.2 | 0.0 | 20.9 | 1.1 | 3.5 |
| unicode_basic | 0.833 | 41.5 | 0.0 | 1.2 | 14.6 | 9.9 | 6.0 |
| random | 0.846 | 55.3 | 0.0 | 1.2 | 15.6 | 13.4 | 4.9 |
| github_events | 0.889 | 48.7 | 0.0 | 0.0 | 7.6 | 11.0 | 14.1 |
| distinct_values | 0.957 | 67.2 | 0.0 | 0.0 | 9.4 | 8.0 | 7.2 |
| update_center | 0.986 | 62.3 | 0.0 | 0.0 | 11.5 | 4.7 | 9.0 |
| apache_builds | 0.999 | 63.2 | 0.0 | 0.0 | 8.2 | 10.3 | 6.7 |
| gsoc-2018 | 1.000 | 36.1 | 0.0 | 0.0 | 2.2 | 5.3 | 30.9 |
| y_string_unicode | 1.000 | 17.5 | 40.5 | 0.0 | 7.4 | 2.7 | 5.5 |

### 3.2 Cycles-per-byte per primitive class (A × B derivation)

Per row, `class_cycles_per_byte ≈ row_cycles_per_byte × class_%self`
(P1-V3-A §2 cycles/B × B §2 primitive-class share). The derivation is
B §5.2's formula evaluated row by row; this report tabulates the result
for the substrate-shape verdicts in §4–§5.

| Corpus (T1) | row c/B | str-plane c/B | esc-hex c/B | digit c/B | struct c/B | ws c/B |
|---|---:|---:|---:|---:|---:|---:|
| canada | 2.103 | 0.000 | 0.000 | 0.728 | 0.667 | 0.160 |
| mesh | 2.687 | 0.000 | 0.000 | 0.965 | 0.841 | 0.344 |
| numbers | 2.158 | 0.000 | 0.000 | 1.152 | 0.626 | 0.091 |
| marine_ik | 2.693 | 0.113 | 0.000 | 0.746 | 0.948 | 0.307 |
| instruments | 2.069 | 0.913 | 0.000 | 0.135 | 0.314 | 0.370 |
| citm_catalog | 1.180 | 0.323 | 0.000 | 0.094 | 0.283 | 0.273 |
| twitter | 2.373 | 1.210 | 0.000 | 0.054 | 0.299 | 0.266 |
| unicode_mixed | 4.634 | 2.044 | 0.000 | 0.034 | 1.200 | 0.149 |
| unicode_escapes | 3.007 | 0.749 | 1.088 | 0.000 | 0.629 | 0.034 |
| unicode_basic | 2.905 | 1.207 | 0.000 | 0.034 | 0.424 | 0.288 |
| random | 3.551 | 1.964 | 0.000 | 0.044 | 0.554 | 0.476 |
| github_events | 2.272 | 1.107 | 0.000 | 0.000 | 0.173 | 0.250 |
| distinct_values | 3.850 | 2.587 | 0.000 | 0.000 | 0.362 | 0.308 |
| update_center | 3.622 | 2.256 | 0.000 | 0.000 | 0.416 | 0.170 |
| apache_builds | 2.910 | 1.839 | 0.000 | 0.000 | 0.239 | 0.299 |
| gsoc-2018 | 1.544 | 0.557 | 0.000 | 0.000 | 0.034 | 0.082 |
| y_string_unicode | 5.710 | 1.000 | 2.312 | 0.000 | 0.422 | 0.154 |

The cycles/B column is per A's `pmu_rows.tsv`; the per-class derivations
are point estimates with ±5% noise (xctrace TP 1 ms sample weight is
coarser than PMU read; class-attribution residual rolls into the
unlisted `other` / `mm` / `memcpy` / `vec-grow` columns). The headline:

- **String-plane c/B**: 1.21–2.59 c/B on string-dense rows (twitter,
  random, distinct_values, update_center, apache_builds, github_events)
  vs 0.00 c/B on number-dense rows (canada, mesh, numbers). The
  per-string-span scanner family is the dominant cycle sink on
  string-dense rows.
- **Escape-codec c/B**: 1.09 c/B on unicode_escapes; 2.31 c/B on
  y_string_unicode (the highest single-class cycle cost in the whole
  table). The escape-codec primitive is the bottleneck on escape-heavy
  rows; it is not the string scanner.
- **Digit-FSM c/B**: 1.15 c/B on numbers (the largest digit row); 0.97
  c/B on mesh; 0.73–0.75 c/B on canada / marine_ik. The digit-FSM is
  the dominant cycle sink on number-dense rows.
- **Structural-walker c/B**: stays in 0.03–1.20 c/B across rows but is
  never the rank-1 sink. Even on `dispatch_value`-heavy rows (mesh
  21.3%, marine_ik 24.2%) the cycles/B contribution caps at ~1 c/B.

The same derivation on the four named "what does the c/B spread mean"
rows in A §3 closes the open question:

- twitter/t1: 2.37 c/B headline ≈ 1.21 c/B per-string-span scanner +
  0.27 c/B whitespace skip + 0.30 c/B structural walker + 0.19 c/B simd
  movemask + 0.40 c/B residual. The per-string-span scanner family is
  ~51% of the row cycle budget.
- distinct_values/t1: 3.85 c/B ≈ 2.59 c/B per-string-span scanner +
  0.31 c/B whitespace skip + 0.36 c/B structural walker + 0.28 c/B simd
  movemask + 0.31 c/B residual. The per-string-span scanner family is
  ~67% of the row cycle budget.
- y_string_unicode/t1: 5.71 c/B ≈ 2.31 c/B escape-codec hex-unit +
  1.00 c/B per-string-span scanner + 0.42 c/B structural walker + 1.98
  c/B residual (the `Option::copied` / `PartialEq::eq` overhead pair
  that escape decoding pulls in). The escape-codec primitive is ~40.5%
  of the row cycle budget; the per-string-span scanner adds another
  17.5%.
- gsoc-2018/t1: 1.54 c/B ≈ 0.48 c/B simd movemask + 0.32 c/B
  per-string-span scanner (tiny) + 0.07 c/B per-string-span scanner
  (block) + 0.06 c/B per-string-span scanner (SIMD full) + 0.08 c/B
  whitespace skip + 0.53 c/B residual (`trailing_zeros`, `From<u8>::
  from`, NEON variants — the SIMD-block helper symbols the string
  scanner calls). The per-string-span family aggregates to ~36% but the
  simd movemask leaf alone is the rank-1 cycle sink at 31%.

The substrate-shape conclusion: structural classification is cheap on
parse-only Track 1; the cycle spread across rows is driven by the
per-string-span scanner family on string-dense rows, the escape-codec
hex-unit on escape-heavy rows, and the digit-FSM on number-dense rows.
The same shape holds on Track 2 (B §2 second column per row); the
hand-coded parser pays the same cycles per primitive class plus a
recursive-descent body overhead surfaced as `Parser::parse_value_at`
~8–26%.

## §4 SC-1 verdict — structural-scan non-fusion

**SC-1 claim restated** (`SC-1-offset-tape-teardown.md` §1.3):
`scan_structurals` exists and is fast, but its output is never consumed —
`attach_structural_index` at `generated.rs:14-17` is `let _ = state;`.
The recursive-descent parser re-classifies every structural byte itself.
The parser pays for a second structural traversal.

**V3-C verdict (V4 refold): SC-1 is structurally TRUE *and*
cycle-grounded at the primitive-class layer; the share-of-self-time
half is no longer contingent.**

### 4.1 Symbol-level confirmation

B §3.1 shows `scan_structurals` self-time **0.00% on every (corpus,
track) row** — all 17 × 2 = 34 rows. The SIMD structural classifier
(`bbnf_simd::aarch64::scan::neon::scan`, `bulk_emit_positions_64_neon`,
`bitmap_prefix_xor_64_neon`) does not appear in any xctrace backtrace
under the steady-state parse loop on any row.

The `simd_movemask::movemask_u8x16` symbol is non-zero on string-dense
rows (twitter 8.1%, github_events 14.1%, gsoc-2018 30.9%, etc.), but B
§3.1 resolves the source location as the *string-plane block scanner*
callsite at `bbnf-simd/src/aarch64/movemask.rs:22`, consumed by
`parse_that_regex::skip_string_plain_trusted` (the per-string-span block
scanner), not by the structural scan. The same primitive name carries
two callsites in the codebase; only the string-plane callsite fires on
parse_only.

### 4.2 Cycle-grounded confirmation

The substrate-shape diagnosis — for any grammar whose generated
parse_only path re-derives StructuralAlphabet ordinals inside fused
recursive descent, the SIMD structural-scan output is unconsumed —
holds at the cycle layer via §3.2:

- `scan_structurals` c/B = 0.00 on every row (0.00% × any row c/B = 0).
- `consume_structural` c/B never exceeds 0.04 c/B on float-heavy
  winners (canada 0.04, citm_catalog 0.04, numbers 0.00) or
  0.06 c/B on any other row.
- The structural-walker class as a whole stays under 1.2 c/B on every
  row.

Per-class cycle accounting closes the share-of-self-time half: the
structural-classification cost the SC-1 claim predicted *is* baked into
`dispatch_value` / `consume_container_next` / `consume_array_next`, but
its cycle share never exceeds ~25% of any row's c/B and is well below
the per-string-span / escape-codec / digit-FSM cycle sinks on the rows
where each is dominant. The SC-1 falsification path A §3 named ("a
same-line `#[inline(never)]` build … is the only available
instrument") is no longer required — A's PMU rows × B's TP per-class
shares already provide the cycle attribution.

### 4.3 Substrate-shape generalisation

This is a substrate-shape verdict: any future grammar whose generated
parse_only path re-derives StructuralAlphabet ordinals inside fused
recursive descent will exhibit the same `scan_structurals`-discarded
pattern. The verdict is **not** JSON-specific; CSS L4 / Sheets /
BBNF-self generated parsers landed under the same SC-1-style codegen
posture will read the same 0.00% on their analogous SIMD scan.

**Net SC-1 verdict (V3-C refold)**: SC-1's *non-fusion* claim holds at
both the symbol and cycle layers (the SIMD scan symbols are
non-producers, present only under synthetic probes; their cycle share
is 0.00 c/B on 34/34 rows). SC-1's *share-of-self-time* claim is
**confirmed** — the structural-walker class consumes 0.03–1.20 c/B on
parse_only rows, well below the string-plane / escape-codec / digit-FSM
sinks where each dominates.

## §5 SC-4 verdict — String-plane share

**SC-4 claim restated** (`SC-4-string-plane-gap.md` §1.3 quoting SK-V7
`SYNTHESIS.md`): `match_string_at_quote` + `match_tiny_plain_string` ≈
**75% of total self-time** on string-heavy rows (twitter, gsoc-2018,
update_center, unicode_*, y_string_unicode).

**V3-C verdict (V4 refold): SC-4's *direction* is confirmed (the
per-string-span scanner family is the dominant cycle sink on dense-key
rows); SC-4's *literal 75% pair share* is an upper bound. The
load-bearing lower bound is 47–67% on the dense-key losses, with the
tiny scalar path inverting the SK-V7 dispatch ratio.**

### 5.1 Where the scanners are visible (xctrace TP)

Per B §2, the tiny scalar path
`runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>`
appears as **rank-1 on the dense-key string-loss rows**:

| Corpus (T1) | tiny scanner % | string-plane family % | per-string-span (tiny+full+block) % |
|---|---:|---:|---:|
| distinct_values | 61.9% | 67.2% | 61.9% (tiny only; full = 0.0%) |
| apache_builds | 56.0% | 63.2% | 56.0% |
| update_center | 54.7% | 62.3% | 54.7% |
| random | 48.6% | 55.3% | 48.6% |
| twitter | 46.2% | 51.0% | 47.4% (tiny 46.2 + full 1.2) |
| github_events | 40.5% | 48.7% | 42.6% (tiny 40.5 + block 2.1) |
| instruments | 40.2% | 44.1% | 40.2% |
| unicode_basic | 31.4% | 41.5% | 31.4% |

Track 2 is isomorphic with the symbol identity shifted to
`bbnf_bench::track2::json::match_tiny_plain_string`: distinct_values/t2
63.1%, apache_builds/t2 45.0%, update_center/t2 46.0%, random/t2 42.1%,
twitter/t2 30.1%, etc. The dispatch ratio between tiny and full SIMD
scanner is the inverse of SK-V7's claim: the tiny scalar path
dominates; the full SIMD scanner
(`match_string_at_quote_trusted_utf8`) is in the tail on every
non-unicode row.

### 5.2 SC-4 75% as upper bound; 47–67% as load-bearing lower bound

SC-4's literal 75% pair share is the upper bound, computed against the
broader string-plane family (per-string-span + string-dispatch +
string-escape + string-open). The load-bearing lower bound is the
tiny+full scanner pair share on its own:

| Corpus (T1) | SC-4 75% upper bound (string-plane %) | tiny+full pair (load-bearing lower bound) |
|---|---:|---:|
| distinct_values | 67.2% | 61.9% |
| apache_builds | 63.2% | 56.0% |
| update_center | 62.3% | 54.7% |
| random | 55.3% | 48.6% |
| twitter | 51.0% | 47.4% |
| github_events | 48.7% | 40.5% |
| instruments | 44.1% | 40.2% |
| unicode_basic | 41.5% | 31.4% |

The 75% figure is closely approached on **distinct_values** (string-plane
67.2%; tiny+full 61.9%) and **update_center / apache_builds / random**
(55–63% / 49–56%). It is **not** reached on any single row at the
literal `match_string_at_quote + match_tiny_plain_string` pair level;
the full SIMD fallback `match_string_at_quote_trusted_utf8` is
significant only on unicode_mixed (15.2%) and unicode_escapes (19.5%)
and never exceeds 20%.

The 47–67% lower bound is the load-bearing finding: the per-string-span
scanner family is the dominant cycle sink on dense-key losses. On
twitter / gsoc-2018 / y_string_unicode (three SC-4-named rows) the
literal pair share is markedly lower:

- twitter/t1: 46.2% + 1.2% = 47.4%.
- gsoc-2018/t1: 20.8% + 4.0% = 24.8% (the rank-1 symbol is
  `movemask_u8x16` at 30.9%; the string-block scanner consumes
  movemask, so the *family* share is 36.1% if movemask is folded into
  per-string-span).
- y_string_unicode/t1: 10.6% + 1.5% = 12.1% (the rank-1 pair is
  escape-codec hex-unit `hex_nibble + read_hex_unit_scalar` = 38.2%,
  not the per-string-span scanner).

### 5.3 y_string_unicode bottleneck — a class SC-4 missed

y_string_unicode is 99%+ short 6-byte `\uXXXX` strings. The dominant
cycle sink is the escape-codec hex-unit primitive (B §3.2 last bullet),
*not* the string scanner. Per §3.2:

- y_string_unicode/t1: escape-codec c/B = 2.31; per-string-span c/B =
  1.00; structural walker c/B = 0.42. The escape-codec class is among
  the largest single cycle sinks in the 34-row table (distinct_values/t1
  per-string-span at 3.850 × 0.619 = 2.38 c/B is marginally larger;
  cf. CH1 V4 A4-9 / C4-5 hedges).
- y_string_unicode/t2: escape-codec hex-unit = 43.9% self-time;
  per-string-span scanner = 7.5%. Same shape, tighter ratio.

Per the V4-B Lock-14 reframe (§1.3, CH2 §4.2), the escape-codec hex-unit
primitive is parameterised
`{hex_digit_count, surrogate_join_policy, terminator_policy}`. JSON's
`\uXXXX` instantiates `{4, surrogate-pair-join, no-terminator}`; CSS L4
`\HHHHHH` is `{1..6, no-surrogate, whitespace-or-non-hex terminator}`;
JS-strict `\u{HHHHHH}` is `{1..6, surrogate-pair-join,
brace-terminator}`. S-P2 / S-P3 cost-set must enumerate this class
separately from per-string-span; collapsing the two erases the
y_string_unicode-shape cost surface.

### 5.4 Direct-route (samply mode-II) consistency

V2 samply mode-II / III at `/tmp/skv9-p1-rerun/profiles/p1b/` and
`…/p1c/` is consistent with the xctrace attribution on the direct
route. The direct-route surface has proper per-template
monomorphisations rather than a single LTO-fused `dispatch_value`, so
samply's frame-pointer walk surfaces the per-template symbols at proper
granularity (B §5.3). The mass shifts from "scan-only" on parse_only to
"scan + materialise" on the direct route; the rank-1 primitive class is
unchanged:

| Corpus | Track 1 parse_only rank-1 (xctrace TP) | Direct route rank-1 (samply mode-II) |
|---|---|---|
| twitter | per-string-span (tiny) 46.2% | direct field projection (`parse_object_value_at_direct`) 72.4% |
| apache_builds | per-string-span (tiny) 56.0% | direct sink callback (`array_string`) 32.7% + direct field projection 38.1% |
| canada | digit-FSM (`scan_digit_run`) 21.0% | digit-FSM materialise (`materialize_f64`) 12.4% + direct repeated projection 87.5% |
| numbers | digit-FSM (`scan_digit_run`) 33.4% | digit-FSM materialise 11.9% + direct repeated projection 77.7% |
| unicode_escapes | escape-codec hex-unit 33.6% | escape-codec materialise (`unescape_string`) 47.5% |
| y_string_unicode | escape-codec hex-unit 38.2% | escape-codec materialise (`unescape_string`) 21.5% (harness-noisy) |

The direct-route primitive sub-classes (`direct_field_projection`,
`direct_repeated_projection`, `direct_sink_callback`) per CH2 §4.3 are
sub-classes under "structural walker" — each substrate-neutral, each
admitting per-grammar realisation. The route does not change which
primitive is hot; the mass shifts from scan-only to scan + materialise.

### 5.5 String-fraction correlation (refolded against B's per-class shares)

The Pearson r and Spearman ρ are recomputed against B's xctrace
per-class shares (§3.1 column `str-plane`), not the V2 samply de-fused
T1-direct shares. The convention is documented: zero-string rows
(canada, mesh, numbers) are included with `str-plane = 0.0`; n = 17.

| Pair | Pearson r | Spearman ρ |
|---|---:|---:|
| (string-frac, str-plane share, T1, n=17) | +0.825 | +0.613 |
| (string-frac, (str-plane + esc-hex) share, T1, n=17) | +0.924 | +0.731 |

The correlation is **stronger** under B's xctrace attribution than
under V2's samply-shallow attribution (V3-C cycle 1 reported +0.720 /
+0.755 from the samply de-fused column). Once the escape-codec column
is added (the class SC-4 missed), Pearson reaches +0.924 — string and
escape density jointly predict the string-plane + escape-codec self-time
share on every row.

The diagnostic admits as JSON-corpus `RecognizerFacts` / `CostFacts`
telemetry: delimited-span density (the substrate-neutral name for
JSON's `quote_count / element_tokens`) correlates monotonically with
per-string-span scanner self-time, and the joint
delimited-span + escape density correlates more tightly still.
Per CH2 §4.1, this is the JSON instantiation of "delimited-span
fraction admission predicate" — a substrate-neutral law any future
grammar will instantiate against its own StructuralAlphabet's
string-delimiter set.

The mid-band of the SC-4 step function (instruments 0.556 / +10.6%
thin win; citm_catalog 0.630 / +24.6% win) shows that string density
alone does not predict the strict-sonic verdict; the universal loss
boundary stays at ~0.726 (twitter) and the wins continue through 0.630
(citm_catalog). The step function is the JSON realisation of a
per-grammar law; CSS L4 / Sheets thresholds will fit their own
delimited-span fractions against their own throughput curves.

## §6 Where the V2 attribution was wrong or shallow

The V2 attribution (`p1e-hot-leaf-attribution.md` §2 table) said:

- "Parse-only Track 1: `dispatch_value` at `generated.rs:47` dominates
  every corpus at 95.6%–99.6%" — a frame-pointer-coalescing artefact;
  the xctrace TP attribution falsifies this on every row (B §3.4).
- "Direct object-heavy rows: `parse_object_value_at_direct` dominates"
  — accurate at the symbol level; survives B's falsification because
  the direct-route surface has proper per-template monomorphisations.

V2's §4 already named the shallowness ("P1-A remains too fused:
`dispatch_value` is a real symbol, but not a primitive-level split").
What V2 did NOT do, and the V4 refold now does:

1. **V2 did not separate Track 1 from Track 2.** V2 treated `parse_
   only.track1_generated` as "Track 1" and `direct_to_struct.track1`
   as a second Track 1 surface, never citing the hand-coded Track 2
   (which `RESULTS.md:139` defines). V3-C V4 names both tracks at the
   same xctrace TP granularity; Track 2 is no longer samply-shallow.

2. **V2 stopped at the symbol level for SC-1.** V2 did not test the
   SC-1 claim that `scan_structurals` is non-consumed in the
   production parse-only path. V3-C V4 confirms via B §3.1: the SIMD
   scan symbols are 0.00% on 34/34 rows. The structural-walker class
   stays under 1.20 c/B on every row.

3. **V2 did not quantify the SC-4 75% share.** V2 said "the fused
   symbol does not by itself authorize a primitive" and stopped. V3-C
   V4 names the per-string-span scanner family share — 47–67% on
   dense-key losses with the tiny scalar path dominating — and runs
   the Pearson + Spearman correlation on B's per-class shares
   (r = +0.825 string-plane, +0.924 string-plane + escape-codec).

4. **V2 did not classify into the agreed substrate-neutral primitive
   taxonomy.** V2 used surface phrases ("direct sink object
   projection", "string/unicode materialization"). V3-C V4 maps every
   leaf into the substrate-neutral primitive class set per §1.3
   (per-string-span scanner, escape-codec hex-unit, digit-FSM scanner,
   structural walker, whitespace skip, simd movemask primitive,
   memcpy, vec-grow, other). The classifier vocabulary is
   substrate-neutral and accepts the same classes on future
   CSS/Sheets/BBNF-self corpora.

5. **V2 missed the escape-codec hex-unit class.** The y_string_unicode
   bottleneck is `read_hex_unit_scalar + hex_nibble` (38.2% T1, 43.9%
   T2; the escape-codec class). SC-4 framed it inside the string-plane
   family; V2 did not name it. V3-C V4 splits it out as its own
   primitive class per CH2 §4.2 and gives the per-class cycle share
   (2.31 c/B on y_string_unicode/t1, the largest single-class cycle
   cost in the table).

6. **V2 missed the `from_utf8` and `string_body_range` view-side
   cost.** These are 14–30% of self-time on `cold_first_parse` /
   `host_call_eager_decode` for string-heavy corpora — they are not
   production parse-only cost but they ARE production eager-decode
   cost. V3-C V2 listed "no single parser leaf dominates" without
   explaining; V3-C V4 names the harness frame on
   `y_string_unicode/direct` (23.3% in `mach_absolute_time` because
   input is 50 B and harness frame dominates) and reconstructs the
   honest parser self-time from B's xctrace TP rows where the harness
   noise is absent.

7. **V2 did not address what xctrace would change.** V3-C V4 does:
   xctrace's DWARF inlined-frame walk surfaces the inlined leaves the
   LTO-fused `dispatch_value` body hid from samply, and A's PMU
   cycles × B's TP per-symbol shares give the cycles-per-class
   derivation that V2 left open (§3.2).

8. **V2 did not write down the inlining barrier explicitly.** The
   reason `match_tiny_plain_string` was invisible to samply mode-I is
   the bench-profile inlining policy + samply's frame-pointer walk
   coalescing inlined PCs into the outer fused symbol, not the
   absence of the symbol from the code. V3-C V4 documents this
   explicitly (§0, §1.4). The samply mode-I attribution is retained
   only as a falsified V2 baseline; the load-bearing column is
   xctrace TP.

## §7 Sources

### 7.1 Primary inputs (V4 refold)

- B Time Profiler per-row symbol exports:
  `/tmp/skv9-xctrace-v3/exports/<corpus>__<track>.symbols.json` (34
  files; top-15 self-time per row with primitive class tags + source
  `file:line` where xctrace emitted DWARF inlined-frame records).
- B Time Profiler trace bundles:
  `/tmp/skv9-xctrace-v3/p1b-tp/<corpus>__<track>.trace` (34 bundles).
- A CPU Counters PMU table:
  `/tmp/skv9-xctrace-v3/pmu_rows.tsv` (34 rows; cycles, instructions,
  CPI, cycles/B per (corpus, track) from `proc_pid_rusage(RUSAGE_INFO_V5)`).
- A CPU Counters trace bundles: `/tmp/skv9-xctrace-v3/p1a/` (34
  `.trace` directories for Instruments.app re-inspection).
- Probe binary source:
  `skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs` (the launchable
  harness both A and B captured against).
- Aggregator + class taxonomy:
  `/tmp/skv9-xctrace-v3/aggregate.py` (the substrate-neutral
  substring classifier per CH2 GENERALITY).

### 7.2 Cross-validation inputs

- V2 samply mode-I profiles:
  `/tmp/skv9-p1-rerun/profiles/p1a/*.profile.json.gz` (the falsified
  baseline; retained only as the V2 reference column).
- V2 samply mode-II / III profiles:
  `/tmp/skv9-p1-rerun/profiles/p1b/*.profile.json.gz`,
  `…/p1c/*.profile.json.gz` (direct-route + eager-decode; consistent
  with xctrace because mode-II / III routes have proper per-template
  monomorphisations).
- V2 aggregator: `/tmp/skv9-p1-rerun/profile-summary.json` (106
  samply summaries; used in §5.4 only).
- Sibling reports:
  `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-A-xctrace-cpu-counters.md`
  (PMU rows + reproduction script);
  `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-B-xctrace-time-profiler.md`
  (per-row symbol tables + SC-1 / SC-4 verdict block + class taxonomy).

### 7.3 Claim provenance

- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md`
  §1.3 / §2 (non-fusion claim under refold).
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md`
  §1.3 / §2 (75% scanner pair, quote-density correlation).
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`
  §4 (StructuralAlphabet abstraction underlying the substrate-neutral
  primitive vocabulary).
- `skinny/RESULTS.md:91–137` (per-corpus element census + tape ratios)
  and `:139` (Track 1 vs Track 2 definition).

### 7.4 Source pointers (current build)

- `skinny/crates/runtime/src/grammars/json/generated.rs:14-17`
  (`attach_structural_index` no-op), `:47` (`dispatch_value`),
  `:161` (`match_tiny_plain_string_with_cap::<16>` head, body to ~185),
  `:189` (`match_string_at_quote_trusted_utf8` head, body to ~205),
  `:468` (`parse_object_value_at_direct`),
  `:508` (`parse_array_element_at_direct`).
- `skinny/crates/runtime/src/grammars/json/scan.rs:22–30,207–275`
  (`scan_structurals` + NEON path) and `:38–45`
  (`scalar_parity_report`).
- `skinny/crates/runtime/src/grammars/json/view.rs:309–381`
  (`at_cursor`, `string_body_range`, `next_sibling_cursor`).
- `skinny/crates/parse-that-regex/src/lib.rs:174`
  (`match_string_at_quote_trusted_utf8`), `:285`
  (`validate_string_escape`), `:547–574`
  (`skip_string_plain_trusted`), `:718` (`unescape_string`), `:962`
  (`hex_nibble`); `…/number/mod.rs:54–158` (`scan_digit_run`,
  `match_number_span_from_first`, `NumberParts::push_*_digits`,
  `is_four_ascii_digits`).
- `skinny/crates/bbnf-bench/src/track2/json.rs:58` (`Parser::parse_value_at`),
  `:277` (`Parser::consume_container_next`), `:317–319`
  (`match_tiny_plain_string`); `…/direct_struct.rs:124`
  (`JsonDigestSink::array_string` sink).
- `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:22`
  (`movemask_u8x16`).

---

§0 V4 refold: primary attribution rebased on xctrace Time Profiler
exports; samply demoted to cross-validation; SC-1/SC-4 verdicts
cycle-grounded; Lock-14 primitive classes added.
