# SK-V5 / A3 — parse-that-regex gap analysis for SOTA-beat primitives

Author: A3 (parse-that-regex audit cohort)
Scope: `skinny/crates/parse-that-regex/` exclusively. Cross-references to
`bbnf-simd/`, `bbnf-bench/`, and `runtime/grammars/json/` are read-only.
Reference: SK-V4 grand-synthesis (commit `1519cf16`,
`restart/skinny/tranches/ASMJSON-DAV1D-GRAND-SYNTHESIS-SK-V4.md` §5/§7) — exact
materialisers belong in `parse-that/number`, `parse-that/string`,
`parse-that/unicode`, not in JSON bench code. Current measured posture:
numbers ~33% of sonic-rs on direct, `serde_json::parse_number` ~23.4% self-time;
twitter / random / unicode_mixed / unicode_basic at 47–78% sonic on parse rows;
direct rows for the same corpora are likewise string-bound.

## 1. Crate inventory

The crate is one file plus a one-trait integration shim:

| File | LOC | Role |
|---|---|---|
| `Cargo.toml` | 11 | manifest; deps = `bbnf-simd`, `thiserror` |
| `src/lib.rs` | 1019 (incl. ~240 LOC tests) | every primitive lives here |
| `src/integration/mod.rs` | 1 | module gate |
| `src/integration/simd_scan_hook.rs` | 19 | `SimdScannerHook` trait shim over `bbnf_simd::SimdClassifier` |

So executable surface is **~780 LOC** of non-test code in a single
`lib.rs`. There is no `number/` submodule. There is no `string/` submodule.
There is no `unicode/` submodule. The directive in SK-V4 §5 ("exact
materialisers belong in parse-that/number…") is a structural mandate that the
crate does not currently satisfy at all — the materialiser layer is
non-existent.

### 1.1 Public API surface (file:line)

| Symbol | Kind | LOC | Classification |
|---|---|---|---|
| `RegexError` (`lib.rs:6`) | struct | 5 | SOTA-grade (offset+kind shape correct) |
| `RegexErrorKind` (`lib.rs:12`) | enum | 9 | partial (no `InvalidNumber*` variants; no `Overflow`) |
| `JsonStringMatch` (`lib.rs:31`) | struct | 8 | partial (JSON-named; should be the generic `StringMatch`) |
| `StringMode` (`lib.rs:40`) | enum | 5 | SOTA-grade tri-state (`StrictJson`/`GrammarString`/`ByteString`) |
| `StringFlags` (`lib.rs:47`) | bit-flags struct | 32 | SOTA-grade (`HAS_ESC`/`HAS_CONTROL`/`HAS_NON_ASCII`/`NEEDS_DECODE`/`UTF8_VALIDATED`) |
| `StringMatch` (`lib.rs:80`) | struct | 23 | SOTA-grade match record |
| `JsonNumberMatch` (`lib.rs:104`) | struct | 6 | partial — only carries `is_integer`, not the parse-derived fast-path facts (mantissa pre-digits, exponent sign, digit count, decimal point index, overflow flag) Eisel-Lemire needs |
| `skip_json_whitespace` (`lib.rs:111`) | fn | 13 | partial (SWAR u64 only; no NEON tail; no AVX-512 tail) |
| `match_json_number` (`lib.rs:148`) | fn | 8 | partial — **scans** but does not **materialise**. No `parse_number_f64` / `parse_number_i64` / `parse_number_u64` / `parse_number_eisel_lemire` peers exist. |
| `match_json_number_from_first` (`lib.rs:158`) | fn | 62 | partial — pure scalar SWAR digit skipper; correct grammar; no SIMD digit-block consumer for the long mantissa fast path |
| `validate_json_number` (`lib.rs:244`) | fn | 7 | SOTA-grade for shape validation |
| `match_json_string` (`lib.rs:252`) | fn | 10 | SOTA-grade scan boundary |
| `match_json_string_at_quote` (`lib.rs:264`) | fn | 13 | partial — pure scan; no `materialize_string_borrowed` / `materialize_string_unescaped` / `materialize_into_bumpalo_arena` peers |
| `match_string` (`lib.rs:278`) | fn | 15 | partial — generic mode-aware scan; no peer materialiser per mode |
| `match_string_at_quote` (`lib.rs:294`) | fn | 54 | SOTA-grade scan with NEON inner loop dispatch (`lib.rs:421`) |
| `decode_json_unicode_escape` (`lib.rs:362`) | fn | 43 | SOTA-grade `\uXXXX` decoder (surrogate-pair correct; rejects lone surrogates) |
| `classify_json_string_content` (`lib.rs:468`) | fn | 13 | SOTA-grade (NEON `vcltq_u8` + `vceqq_u8` dispatch on aarch64) |
| `validate_json_string` (`lib.rs:548`) | fn | 6 | SOTA-grade |
| `unescape_json_string` (`lib.rs:556`) | fn | 79 | partial — owns the slow path but returns `Cow<str>` not a writer; no `unescape_into_writer` / `unescape_into_buf` peer; cannot materialise direct-to-struct without an interim `String` allocation |
| `integration::SimdScannerHook` (`integration/simd_scan_hook.rs:3`) | trait | 17 | placeholder — abstracts only over `SimdClassifier`; the rest of the SOTA contract (block-scan over class set; mask-driven loop; UTF-8 validate-block; digit-block consume) is not on the trait |

### 1.2 Private SWAR / NEON helpers (file:line)

| Symbol | LOC | Notes |
|---|---|---|
| `skip_ascii_spaces` (`lib.rs:127`) | 20 | SWAR-zero-byte test for `0x20` runs after a newline |
| `skip_ascii_digits` (`lib.rs:222`) | 21 | SWAR digit-run (8-byte block) |
| `validate_json_string_escape` (`lib.rs:349`) | 11 | scalar `\\ \" \/ \b \f \n \r \t \uXXXX` table |
| `validate_json_unicode_escape_run` (`lib.rs:406`) | 11 | loops `decode_json_unicode_escape` for dense `\uXXXX...` runs |
| `skip_json_string_plain` (`lib.rs:419`) | 27 | NEON `scan_string_special_block` on aarch64; SWAR `json_string_interesting_mask` elsewhere |
| `json_string_interesting_mask` (`lib.rs:448`) | 13 | quote ∪ slash ∪ control ∪ non-ASCII SWAR |
| `zero_byte_mask` (`lib.rs:463`) | 3 | classic Mycroft byte-zero test |
| `scalar_classify_json_string_content` (`lib.rs:483`) | 18 | scalar reference body for the SIMD classifier |
| `neon_classify_json_string_content` (`lib.rs:502`) | 30 | NEON content-class scanner |
| `movemask_u8x16` (`lib.rs:534`) | 13 | NEON movemask emulation (this is a known-painful operation that bbnf-simd already has a dedicated `aarch64/movemask.rs` for — duplication) |
| `validate_utf8_codepoint` (`lib.rs:637`) | 69 | scalar RFC-3629 codepoint validator; one byte at a time — no DFA, no block-validator |
| `is_utf8_continuation` (`lib.rs:708`) | 3 | byte predicate |
| `read_hex_unit_with_error_offset` (`lib.rs:713`) | 29 | dispatches to `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_neon` on aarch64; scalar `read_hex_unit_scalar` elsewhere |
| `read_hex_unit_scalar` (`lib.rs:745`) | 13 | scalar nibble reader |
| `hex_nibble` (`lib.rs:760`) | 7 | nibble table |
| `is_high_surrogate` / `is_low_surrogate` (`lib.rs:770`, `776`) | 8 total | range predicates |

### 1.3 Classification verdict per primitive

| Domain | State |
|---|---|
| Whitespace skipping | partial (SWAR + scalar tail; no NEON; no AVX) |
| Number **scanning** | partial (SWAR digit run; no NEON digit-block; no SVE) |
| Number **materialisation** | **MISSING ENTIRELY** — no f64 / i64 / u64 / Eisel-Lemire / fallback big-int |
| String scanning (raw bytes → span+flags) | SOTA-grade (NEON dispatch, mask-driven) |
| String materialisation (span → borrowed/owned str) | partial — only `unescape_json_string` returning `Cow<str>`; no writer-based path |
| `\uXXXX` decoding | SOTA-grade |
| UTF-8 validation | partial — scalar codepoint-at-a-time only; no Lemire UTF-8 SIMD validator and no DFA |
| Categorisation (digit / hex / whitespace tables) | placeholder — only inline byte-range tests |
| SIMD ergonomics trait | placeholder — `SimdScannerHook` exposes only `classify_chunk` + `alphabet` |
| Grammar-neutrality | **broken** — every symbol is JSON-prefixed; no `parse-that-regex::number::scan_number` API |

## 2. Number primitive gap analysis

### 2.1 What exists

`match_json_number_from_first` (`lib.rs:158`) reads the JSON-number grammar
exactly: optional `-`, then `0` or `1..9 digits`, optional `. digits`, optional
`(e|E) (+|-)? digits`. It returns the byte span and a single fact —
`is_integer = true` iff there was no `.` and no `e/E`. The SWAR digit-run skipper
(`skip_ascii_digits`, `lib.rs:222`) processes 8 bytes per loop using the
classic `(block - 0x30…) | (0x39… - block)` high-bit test. This is faster than
`is_ascii_digit()` per-byte but it does **not** also compute a mantissa or a
digit count, so the materialiser must re-walk the bytes.

The number is materialised today not in `parse-that-regex` but in
`bbnf-bench/src/direct_struct.rs:501` (`parse_integer_digest`) and
`bbnf-bench/src/direct_struct.rs:530` (`serde_number_digest`). Both routes do
their own ASCII decode; the f64 route calls `serde_json::from_str::<Number>` →
`Number::as_f64`, which is the 23.4% self-time row in samply. Per SK-V4 §5,
this must move into `parse-that-regex` as a generic materialiser.

### 2.2 What is missing

| Lever | Present? | Notes |
|---|---|---|
| Eisel-Lemire fast path (LUT-driven 64×128 → mantissa) | **no** | LUT table not present, mantissa accumulator not present, exponent table not present |
| AVX-IFMA 4-lane Eisel-Lemire mantissa multiply | **stub** | `bbnf-simd/src/x86_64/avx_ifma/mantissa.rs:36` is `unimplemented!`; the scalar reference `mul52_low_scalar` exists at `:22` but parse-that does not call it |
| Slow-path fallback (decimal-to-binary big-int when LUT misses) | **no** | the contract for fallback (currently `serde_json::Number`) is external |
| Integer fast path (i64 / u64 without going through f64) | **partial — wrong location** | exists in `bbnf-bench/direct_struct.rs:501` as `parse_integer_digest`; not exposed via parse-that-regex |
| Edge case: `-0` preserves f64 sign bit | **partial** | bench-side `parse_integer_digest` returns `number_f64(-0.0)`; no centralised primitive |
| Edge case: subnormals (`5e-324`) | **no native path** | falls through to serde |
| Edge case: overflow → `+inf`/`-inf` per IEEE | **no** | parse_integer_digest returns `None` on `checked_mul`, falls through to serde |
| Edge case: exponent boundary (`±308`, `±324`) | **no native path** | falls through |
| Edge case: 2⁵³ mantissa boundary (integer-exact f64 range) | **no native path** | falls through |
| Edge case: denormal (mantissa < 2⁵²) | **no native path** | falls through |
| Long-mantissa overflow signal in the scanner | **no** | `match_json_number_from_first` does not track digit count, so the materialiser must re-count |
| NEON / AVX-512 digit-block accumulator | **no** | the SWAR digit-skipper at `lib.rs:222` only locates the digit run end; it does not compute Σ d·10^k |

### 2.3 What "Eisel-Lemire shipped" looks like

Per the canonical reference (Nigel Tao's exposition of Eisel + Lemire,
"Number Parsing at a Gigabyte per Second", Software: Practice & Experience
2021), the structure is:

1. **Scan** the number span; produce facts: `(sign, mantissa_u64, decimal_exp,
   digit_count, overflow_flag)`. This costs the same as today's scanner +
   one `mul10/add` per ASCII digit (and SWAR can do 8 digits per cycle on
   modern CPUs via `pmaddubsw` / equivalent NEON multiply-add).
2. **LUT lookup** of `10^decimal_exp` into a precomputed table of (high u64,
   low u64) 128-bit floats (the table is ~620 entries covering the IEEE-754
   exponent range, ~10 KB).
3. **128-bit multiply** of mantissa × power-of-10-high (the high bits) +
   carry from mantissa × power-of-10-low ≫ 64. On AVX-IFMA this is
   `vpmadd52luq` + `vpmadd52huq` per Lock 16 (see
   `bbnf-simd/src/x86_64/avx_ifma/mantissa.rs:1`). On scalar/NEON this is a
   `u128` multiply.
4. **Round** to the nearest 53-bit mantissa; detect halfway cases.
5. **Reject** halfway cases or ones where mantissa truncation lost
   information → fall through to the slow path. ~4% of normal doubles miss.

The **slow path** is a decimal-to-binary big-int. Both ryū-style and Andrysco-
style implementations exist; the canonical fallback is `lexical`'s
`slow_radix_to_float` (≈400 LOC). For SOTA-beat we either vendor a slow path or
delegate to a feature-flagged dep — but the slow path must call into
`parse-that-regex` not the other way around so that the digest layer never
sees a half-parsed number.

### 2.4 LOC budget for landing Eisel-Lemire

| Module | LOC est. | Notes |
|---|---|---|
| `src/number/mod.rs` | 40 | re-export surface, `NumberError`, `NumberMatch` extension |
| `src/number/scan.rs` | 60 | move + extend `match_json_number_from_first` to record `digit_count`, `dot_index`, `exp_value`, `mantissa_overflow_flag` |
| `src/number/lut.rs` | 150 | precomputed 10^k table (high u64 + low u64), exponent bounds, plus a `build.rs` generator (≈40 LOC of `build.rs`) |
| `src/number/integer.rs` | 60 | i64 + u64 fast path, lifted from `bbnf-bench/direct_struct.rs:501` and generalised |
| `src/number/eisel_lemire.rs` | 220 | 128-bit multiply, rounding, halfway detection, edge cases (-0, subnormal, overflow → ±inf, exact 2⁵³ boundary) |
| `src/number/slow.rs` | 380 | decimal-to-binary big-int fallback; the 3–5% miss path |
| `src/number/materialize.rs` | 80 | `materialize_f64` / `materialize_i64` / `materialize_u64` / `materialize_dispatch` entrypoints |
| Tests in `tests/number_parity.rs` | 240 | parity against `serde_json::Number::as_f64` + sonic-rs `Number::as_f64` over a corpus including every fixture's number atoms |
| **Subtotal** | **~1230** | |

Of this, the **240** scan-extension + **60** integer + **80** materialize is
the dispatch core (≈380 LOC) that unlocks the integer fast path immediately.
The Eisel-Lemire body (220) + LUT (150) is the 96% f64 fast path
(another 370 LOC). The slow path (380) closes the remaining 4%.

## 3. String primitive gap analysis

### 3.1 What exists

The scanner is already SOTA-grade. `match_string_at_quote` (`lib.rs:294`)
inner-loop dispatches to `bbnf_simd::aarch64::string_block::scan_string_special_block` on aarch64 — a 16-byte block scanner that emits the
first-interesting position (quote ∪ slash ∪ control ∪ non-ASCII). The
classification produces `StringMatch.flags` carrying `HAS_ESC`,
`HAS_CONTROL` (only set if mode-rejected), `HAS_NON_ASCII`,
`UTF8_VALIDATED`, `NEEDS_DECODE`. These flags are exactly what an exact
materialiser needs.

The unescape path is `unescape_json_string` (`lib.rs:556`). It does the
right thing: ASCII-fast borrowed `Cow::Borrowed` when there is no backslash
(and the content prefilter classifies first); otherwise a `String` build,
including the `\uXXXX` surrogate-pair path.

### 3.2 What is missing

| Lever | Present? | Notes |
|---|---|---|
| ASCII-only / no-escape borrowed slice path | **partial** | `unescape_json_string` does this *only* when raw bytes contain no `\\`; but the caller must invoke it. The needs-decode flag is computed in the scanner — the borrowed-slice fast path should be in `parse-that-regex::string::materialize_borrowed` directly, not pushed onto every caller |
| Escape path with scalar spec | SOTA-grade | `\b \f \n \r \t \" \\ \/` table at `lib.rs:565` |
| `\uXXXX` → UTF-8 encoder | partial | `decode_json_unicode_escape` returns `char`, and `out.push(ch)` uses the std encoder. A direct `encode_utf8_into(&mut [u8])` peer that writes into a caller-supplied bumpalo arena or `Vec<u8>` reservation is missing |
| Surrogate pair → 4-byte UTF-8 | SOTA-grade | `decode_json_unicode_escape` (`lib.rs:362`) joins surrogates correctly |
| Noncharacter scalar values (`U+FFFE`, `U+10FFFE`, etc.) | SOTA-grade | `unescape_accepts_unicode_noncharacters` test at `lib.rs:1013` asserts `􏿾` → `"\u{10fffe}"` |
| Control-character strict-mode rejection | SOTA-grade | `lib.rs:323` raises `ControlCharacter` on `0x00..=0x1f`; permissive (`ByteString`) skips the test |
| Invalid UTF-8 rejection at SCAN boundary | SOTA-grade | `validate_utf8_codepoint` (`lib.rs:637`) at scan time, rejecting overlong, surrogate-range, > U+10FFFF, and orphaned-continuation |
| Writer-based unescape (no interim `String`) | **MISSING** | `unescape_json_string` builds a `String`. Direct-to-struct + arena tape want a `unescape_into(&mut dyn Write)` or `unescape_into_arena(&Bump) -> &str` |
| Long-string SIMD memcpy after `interesting` mask | **partial** | the NEON scanner already returns the first-interesting offset, but the bytes `[content_start, first_interesting)` are not block-copied — the caller does a slice borrow only; under unescape the bytes are pushed one char at a time at `lib.rs:625` |
| Per-block "all-ASCII no-special" fast lane (SWAR/NEON 16/32/64 bytes plain copy) | **partial in scanner, missing in unescape** | scanner has it; unescape does char-by-char |

### 3.3 LOC budget

| Module | LOC | Notes |
|---|---|---|
| `src/string/mod.rs` | 40 | re-exports; move `StringMatch`, `StringMode`, `StringFlags` here |
| `src/string/scan.rs` | 220 | move existing scan code (`match_string`, `match_string_at_quote`, `skip_json_string_plain`, `json_string_interesting_mask`, `validate_utf8_codepoint`, `classify_json_string_content`) here verbatim |
| `src/string/materialize.rs` | 180 | `materialize_borrowed(input, span) -> Result<&str>`, `materialize_unescaped(input, span) -> Result<Cow<str>>`, `materialize_into_writer(input, span, &mut dyn UnescapeWriter)`, `materialize_into_arena(input, span, &Bump) -> &str` |
| `src/string/writer.rs` | 60 | `UnescapeWriter` trait (write_ascii_run / write_codepoint / write_escape) |
| `src/string/escape.rs` | 80 | scalar escape table; `decode_one_escape(bytes, slash) -> (Replacement, next_cursor)`; replaces inline switch in `unescape_json_string` |
| Tests in `tests/string_parity.rs` | 200 | parity against `serde_json::from_str::<String>` and `sonic_rs::from_str::<String>` |
| **Subtotal** | **~780** | of which ≈220 is *moving* existing code with no behaviour change |

## 4. Unicode primitive gap analysis

### 4.1 What exists

`validate_utf8_codepoint` (`lib.rs:637`) is a 69-LOC scalar codepoint-at-a-time
RFC-3629 validator with explicit overlong + surrogate + > U+10FFFF rejection.
`decode_json_unicode_escape` (`lib.rs:362`) is a UTF-16-to-codepoint surrogate
joiner. Both correct, both scalar-only.

### 4.2 What is missing

| Lever | Present? | Notes |
|---|---|---|
| Block-level UTF-8 validator (Lemire / `simdjson::stage1::utf8_validation`) | **no** | parse-that today validates one codepoint per branch; sonic-rs uses Lemire's 64-byte UTF-8 validator |
| DFA UTF-8 validator (Hoehrmann) as scalar peer | **no** | the scalar reference for the SIMD validator should be a Hoehrmann 9-state DFA, ≈30 LOC |
| Surrogate-pair join API exposed | **no** | `decode_json_unicode_escape` is in the JSON namespace; should be `unicode::decode_uxxxx_pair(hex_a, hex_b)` |
| `encode_utf8_into(&mut [u8], scalar) -> usize` | **no** | currently delegated to std `char::encode_utf8` |
| Whitespace categorisation table | **partial** | inline byte tests at `skip_json_whitespace`; no published `is_json_whitespace` const-table |
| Hex categorisation table (`is_ascii_hex`) | **partial** | inline in `hex_nibble` (`lib.rs:760`); no exposed `unicode::hex::decode_nibble` |
| Digit categorisation table | **partial** | inline branch ranges; no `unicode::digit::is_ascii_digit_block_swar` peer |

### 4.3 LOC budget

| Module | LOC | Notes |
|---|---|---|
| `src/unicode/mod.rs` | 30 | re-exports |
| `src/unicode/utf8_codepoint.rs` | 90 | move existing `validate_utf8_codepoint` + `is_utf8_continuation`; expose `next_utf8_codepoint(bytes, cursor) -> Result<(char, usize)>` |
| `src/unicode/utf8_block.rs` | 220 | Hoehrmann DFA scalar reference + NEON/AVX-512 dispatch hook (the SIMD bodies live in bbnf-simd, this module exposes the trait+scalar reference) |
| `src/unicode/utf8_encode.rs` | 50 | `encode_utf8_into(buf, scalar) -> usize` (one-codepoint and burst) |
| `src/unicode/uxxxx.rs` | 80 | grammar-neutral `decode_uxxxx_unit` + `decode_uxxxx_pair`; lift `decode_json_unicode_escape` into this and remove the JSON prefix |
| `src/unicode/categories.rs` | 100 | `is_ascii_digit_swar`, `is_ascii_hex_swar`, `is_json_whitespace_swar`, `decode_hex_nibble_block_swar` |
| Tests | 180 | UTF-8 validator vs `core::str::from_utf8`, surrogate-pair decode parity |
| **Subtotal** | **~750** | of which ~80 is *moving* existing code |

## 5. SIMD ergonomics gap analysis

The `SimdScannerHook` trait in `integration/simd_scan_hook.rs:3` exposes only:

```rust
fn classify_chunk(&self, bytes: &[u8; 64]) -> ClassifyResult;
fn alphabet(&self) -> &'static [u8; 64];
```

This is one rung of a ladder that should have at least six. What's missing:

| Helper | Present in parse-that? | Notes |
|---|---|---|
| Block-scan helper: "find next byte from a class" | **partial inline** | exists *inside* `skip_json_string_plain` via `scan_string_special_block`; not exposed as a primitive |
| Mask-driven iteration: "process N bytes per loop, dispatch on first 1-bit" | **partial inline** | inlined in `match_string_at_quote`; not exposed; no helper to iterate bits of a mask |
| Vectorised whitespace skip | **partial** | `skip_ascii_spaces` is SWAR u64 only; no NEON, no AVX-512 |
| Vectorised digit-block classifier | **partial** | `skip_ascii_digits` is SWAR u64 only; no NEON `vcgeq_u8`+`vcleq_u8` block, no AVX-512 `vpcmpb` k-mask |
| Vectorised hex-block classifier | **no** | `read_hex_unit_with_error_offset` does a single 4-byte unit via NEON (`bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_neon`); long `\uXXXX...` runs do not batch |
| Mask popcount / next-bit iterator (`tzcnt` over a `u64` mask, advancing) | **no** | every consumer rolls its own `.trailing_zeros() / 8` |

### 5.1 LOC budget

| Module | LOC | Notes |
|---|---|---|
| `src/simd/block_scan.rs` | 120 | trait `BlockScanner` with `scan_first_in_class(bytes, class_lut) -> Option<(usize, u8)>`, plus scalar reference body |
| `src/simd/mask_iter.rs` | 60 | `MaskIter` over a u64 / u128 / k-mask; emits `(byte_offset, byte_value)` pairs |
| `src/simd/whitespace.rs` | 80 | `skip_ascii_whitespace_block(bytes, cursor) -> usize` — SWAR + NEON + AVX-512 dispatch (NEON/AVX bodies in bbnf-simd) |
| `src/simd/digit_block.rs` | 110 | `scan_digit_run(bytes, cursor) -> (usize, mantissa_partial, digit_count, overflow)` — the SIMD digit accumulator that the number materialiser needs |
| `src/simd/hex_block.rs` | 90 | `decode_hex_block(bytes) -> (u16, ok)` for batched `\uXXXX` runs |
| Tests | 160 | scalar reference parity at each ISA tier |
| **Subtotal** | **~620** | scalar reference + dispatcher; the SIMD bodies remain in `bbnf-simd` |

## 6. parse-that-regex / bbnf-simd contract

### 6.1 The proposed boundary

**parse-that-regex owns**:

- pure scalar reference bodies that are the executable specification;
- ergonomic Rust API: `materialize_*`, `scan_*`, `validate_*`, `decode_*`;
- `&[u8] -> Result<_, RegexError>` shaped boundary;
- error offset reporting;
- mode parameters (`StrictJson` / `GrammarString` / `ByteString`);
- the materialiser layer (number / string / unicode) — this is the SK-V4 §5
  directive;
- dispatch sites that conditionally route to `bbnf-simd` ISA bodies, with the
  scalar reference always reachable.

**bbnf-simd owns**:

- per-ISA SIMD/ASM bodies that conform to the parse-that-regex scalar
  contract;
- the Layer 0 primitive vocabulary (vendored from dav1d/asmjson pattern;
  see commit `9eef728c`);
- the Layer 1 `bbnf.asm` macros;
- checkasm parity gate — every SIMD body has a scalar reference and the
  checkasm differential drives admission.

### 6.2 The current state vs the proposed boundary

| Boundary item | Today | Action |
|---|---|---|
| NEON intrinsics inside `parse-that-regex/src/lib.rs:421-545` | parse-that-regex has *direct* `core::arch::aarch64` intrinsics (`vdupq_n_u8`, `vld1q_u8`, `vceqq_u8`, `vcltq_u8`, `vaddv_u8`, `vandq_u8`) for `neon_classify_json_string_content` and `movemask_u8x16` | **MOVE** to `bbnf-simd/src/aarch64/string_block.rs` (where `scan_string_special_block` already lives) and `bbnf-simd/src/aarch64/movemask.rs` (already exists). parse-that calls into the named primitive. |
| `read_hex_unit_with_error_offset` (`lib.rs:713`) | already correctly dispatches to `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_neon` | leave as-is; this is the model |
| `skip_json_string_plain` (`lib.rs:421`) | already correctly dispatches to `bbnf_simd::aarch64::string_block::scan_string_special_block` | leave as-is; this is the model |
| `SimdScannerHook` (`integration/simd_scan_hook.rs:3`) | placeholder; abstracts only over `SimdClassifier` | **EXPAND** to the six-rung ladder above (block-scan / mask-iter / whitespace / digit / hex / utf8-validate) or **DELETE** in favour of direct `bbnf_simd::aarch64::*` paths matching the existing `unescape_uxxxx` pattern |

**Verdict on the parse-that-regex NEON intrinsics**: they should move to
`bbnf-simd`. They violate the contract (parse-that-regex owns scalar
reference + dispatch; bbnf-simd owns per-ISA bodies). The `movemask_u8x16`
shim in `parse-that-regex/src/lib.rs:534` is an exact duplicate of code that
already lives in `bbnf-simd/src/aarch64/movemask.rs`. The
`neon_classify_json_string_content` function (`lib.rs:502`) is one of the
nine SIMD-body sites that should be a named bbnf-simd primitive
(`scan_class_unescape_run` or similar) so checkasm can gate it.

## 7. Grammar-neutrality audit

Every public symbol that should be grammar-neutral is JSON-prefixed:

| Symbol (file:line) | Grammar-neutral name |
|---|---|
| `JsonStringMatch` (`lib.rs:31`) | `StringMatch` (already exists at `lib.rs:80`! the JSON variant should just be a thin alias) |
| `JsonNumberMatch` (`lib.rs:104`) | `NumberMatch` |
| `skip_json_whitespace` (`lib.rs:111`) | `skip_ws` with a `Whitespace` enum mode (JSON-ws is one preset) |
| `match_json_number` (`lib.rs:148`) | `scan_number` parameterised over a `NumberGrammar` (JSON is one preset: signed, decimal, exp, no underscores) |
| `match_json_number_from_first` | `scan_number_from_first` |
| `validate_json_number` | `validate_number` |
| `match_json_string` (`lib.rs:252`) | `match_string` (already exists at `lib.rs:278`! the JSON variant should reduce to `match_string(_, _, StrictJson)`) |
| `match_json_string_at_quote` | `match_string_at_quote(_, _, StrictJson)` |
| `JsonNumberMatch` | `NumberMatch` |
| `decode_json_unicode_escape` (`lib.rs:362`) | `unicode::decode_uxxxx_pair_or_unit` |
| `validate_json_unicode_escape_run` | `unicode::validate_uxxxx_run` |
| `skip_json_string_plain` (`lib.rs:419`) | `string::skip_plain_run` |
| `json_string_interesting_mask` (`lib.rs:448`) | `string::interesting_mask` parameterised over a `SpecialByteSet` (JSON preset: quote ∪ slash ∪ control ∪ non-ASCII) |
| `classify_json_string_content` (`lib.rs:468`) | `string::classify_content` parameterised over mode |
| `validate_json_string` (`lib.rs:548`) | `validate_string` |
| `unescape_json_string` (`lib.rs:556`) | `string::unescape` |

No JSON-structural-character (`{`, `}`, `[`, `]`, `:`, `,`) appears as a hard-
coded byte in any primitive. The only JSON-specific bytes that *do* appear
are the string-special set — `"`, `\`, `0x00..=0x1f`, `0x80..=0xff` — which
are part of every JSON-shaped string grammar but are not universal. Lifting
these to a `SpecialByteSet` parameter (LUT-driven, exactly the bbnf-simd
Layer 1 `BYTE_CLASS_FROM_EQ_SET_64` primitive landed in commit `9eef728c`)
closes the grammar-neutrality gap without a behavioural change.

## 8. Concrete remediation plan

### 8.1 Wave ordering (foundation → dependents)

```
Wave A (foundation):
  A1. utf8_block.rs  — Hoehrmann DFA + Lemire SIMD validator hook
        (no dependents inside this plan but unlocks block-validate everywhere)
  A2. simd/digit_block.rs  — SIMD digit accumulator
        (foundation for number materialiser fast path)
  A3. simd/block_scan.rs + simd/mask_iter.rs  — block-scan + mask-iter ergonomics
        (foundation for the move of inline NEON in lib.rs:421-545)

Wave B (number, parallel within wave):
  B1. number/scan.rs   — extended scanner producing (mantissa, exp, digits, overflow)
  B2. number/integer.rs — i64/u64 fast path lifted from bbnf-bench
  B3. number/lut.rs     — precomputed 10^k table + build.rs
  B4. number/eisel_lemire.rs — 128-bit multiply + rounding + edge cases
  B5. number/slow.rs    — decimal-to-binary big-int fallback

Wave C (string, parallel with B):
  C1. string/scan.rs    — verbatim move
  C2. string/escape.rs  — escape table extracted
  C3. string/materialize.rs — borrowed/unescaped/writer/arena entrypoints
  C4. string/writer.rs  — UnescapeWriter trait

Wave D (unicode, parallel with B+C):
  D1. unicode/utf8_codepoint.rs — move
  D2. unicode/uxxxx.rs  — move + rename + grammar-neutral
  D3. unicode/utf8_encode.rs — encode-into-buffer

Wave E (cleanup, depends on A+B+C+D):
  E1. Move NEON intrinsics in lib.rs:421-545 into bbnf-simd
  E2. Delete movemask_u8x16 duplicate; use bbnf-simd's
  E3. Rename JSON-prefixed APIs to grammar-neutral; keep JSON aliases at lib.rs:1 for one tranche
  E4. Expand SimdScannerHook to the six-rung ladder or delete in favour of named bbnf-simd entries

Wave F (parity):
  F1. tests/number_parity.rs against serde_json + sonic-rs
  F2. tests/string_parity.rs against serde_json + sonic-rs
  F3. tests/unicode_parity.rs against core::str + char::encode_utf8
  F4. checkasm gate for every SIMD body referenced from parse-that-regex
```

### 8.2 Per-primitive specs (sketches; concrete signatures only)

```rust
// src/number/integer.rs
pub fn materialize_integer(input: &[u8], span: NumberMatch) -> Option<IntegerValue>;
pub enum IntegerValue { I64(i64), U64(u64), NegativeZero, OutOfRange }

// src/number/eisel_lemire.rs
pub fn materialize_f64_fast(
    sign: bool, mantissa: u64, decimal_exp: i32, digit_count: u32,
) -> EiselLemireResult;
pub enum EiselLemireResult { Exact(f64), Halfway, Overflow, Underflow, NeedsSlow }

// src/number/slow.rs
pub fn materialize_f64_slow(input: &[u8], span: NumberMatch) -> f64;

// src/number/materialize.rs
pub fn materialize_f64(input: &[u8], span: NumberMatch) -> f64;            // top-level dispatch
pub fn materialize_dispatch(input: &[u8], span: NumberMatch) -> NumberValue; // i64 | u64 | f64

// src/string/materialize.rs
pub fn materialize_borrowed<'a>(input: &'a [u8], span: StringMatch) -> Result<&'a str, RegexError>;
pub fn materialize_unescaped<'a>(input: &'a [u8], span: StringMatch) -> Result<Cow<'a, str>, RegexError>;
pub fn materialize_into_writer(input: &[u8], span: StringMatch, w: &mut impl UnescapeWriter) -> Result<(), RegexError>;
pub fn materialize_into_arena<'b>(input: &[u8], span: StringMatch, arena: &'b Bump) -> Result<&'b str, RegexError>;

// src/unicode/utf8_block.rs
pub fn validate_block(bytes: &[u8]) -> Result<(), Utf8Error>;
pub fn next_codepoint(bytes: &[u8], cursor: usize) -> Result<(char, usize), Utf8Error>;

// src/simd/digit_block.rs
pub fn scan_digit_run(bytes: &[u8], cursor: usize)
    -> (usize /*end*/, u64 /*mantissa partial, capped at u64::MAX*/, u32 /*digits*/, bool /*overflow*/);
```

### 8.3 Total estimated LOC

| Domain | New LOC | Moved LOC | Test LOC |
|---|---|---|---|
| Number | 990 | 0 | 240 |
| String | 360 | 220 | 200 |
| Unicode | 350 | 80 | 180 |
| SIMD ergonomics | 460 | 0 | 160 |
| Integration / Cleanup | 80 | (delete ~100 NEON inline) | 60 |
| **Total** | **~2240 new** | **~300 moved** | **~840 tests** |

Net crate growth: ~780 LOC today → ~3100 LOC implementation + ~840 LOC tests
across `src/number/`, `src/string/`, `src/unicode/`, `src/simd/`. The current
monolithic `src/lib.rs` drops to a thin re-export hub (≈80 LOC).

## 9. Top three missing primitives by impact

1. **Eisel-Lemire `materialize_f64` + i64 / u64 fast path** in
   `src/number/`. Hits the 23.4% self-time `serde_json::parse_number` row
   directly. **~990 new LOC + 240 test LOC** (`number/scan.rs` extension +
   `number/integer.rs` lifted from `bbnf-bench/direct_struct.rs:501` +
   `number/lut.rs` build-time table + `number/eisel_lemire.rs` + `number/slow.rs`
   fallback + `number/materialize.rs` dispatcher).

2. **Block-level UTF-8 validator** (`src/unicode/utf8_block.rs`). Today
   `validate_utf8_codepoint` (`lib.rs:637`) is one codepoint per branch.
   Lemire's 64-byte SIMD UTF-8 validator + a Hoehrmann DFA scalar reference
   replaces it and removes the per-codepoint branch from every non-ASCII
   string scan (twitter / unicode_mixed / unicode_basic — the 47–78% sonic
   rows). **~220 new LOC + 80 moved LOC + parity tests**.

3. **Writer-based / arena-targeted string unescape**
   (`src/string/materialize.rs`). Today `unescape_json_string` returns
   `Cow<str>` — every direct-to-struct caller therefore allocates an interim
   `String` before pushing into its arena. A `materialize_into_arena(&Bump)`
   peer and an `UnescapeWriter` trait removes that allocation on the escape
   path. **~180 new LOC + 60 LOC trait + 200 LOC parity tests**.

Total LOC budget for "all missing primitives shipped to SOTA-grade":
**~2240 new + ~300 moved + ~840 tests = ~3380 LOC over the current ~780 LOC
crate**.
