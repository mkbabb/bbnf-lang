# SK-V15 P2-E: Parse-That Primitive Gaps

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-28.
Scope: parse-that primitive vocabulary gaps demanded by SK-V15 S-P1 hot leaves.
Output: this file.
P1 hot-leaf antecedents: `skip_ascii_whitespace`, `DirectParser::skip_value`, `DirectParser::ws`, `DirectParser::tiny_plain_string_end`, `match_string_at_quote_trusted_utf8`, `match_number_span_from_first`, `core::str::validations::run_utf8_validation`, `runtime::generated_json::scan::neon::scan`, `bbnf_simd::aarch64::movemask::movemask_u8x16`, plus allocation/tape pressure rows treated as same-substrate consumer pressure rather than standalone primitives.
Lock surface: Lock 1 + Lock 14. Lock 16 is a checkasm admission constraint for any SIMD/ASM body.

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

The live skinny workspace exposes `crates/parse-that-regex` and not a base `parse-that` crate (`skinny/Cargo.toml:3-13`). P2-E therefore treats `parse-that-regex` as the Layer-1 parse-that surface and `bbnf-simd` as the Layer-0 primitive home. The current Layer-1 public surface includes JSON-shaped string and number helpers (`skinny/crates/parse-that-regex/src/lib.rs:8`, `skinny/crates/parse-that-regex/src/lib.rs:119`, `skinny/crates/parse-that-regex/src/lib.rs:168`, `skinny/crates/parse-that-regex/src/lib.rs:179`, `skinny/crates/parse-that-regex/src/lib.rs:190`, `skinny/crates/parse-that-regex/src/lib.rs:243`, `skinny/crates/parse-that-regex/src/lib.rs:299`, `skinny/crates/parse-that-regex/src/lib.rs:339`, `skinny/crates/parse-that-regex/src/lib.rs:441`, `skinny/crates/parse-that-regex/src/lib.rs:867`; `skinny/crates/parse-that-regex/src/number/mod.rs:31-39`, `skinny/crates/parse-that-regex/src/number/mod.rs:225-271`). It does not yet expose grammar-neutral byte-set skip, bounded literal-span, digit-run, local structural-dispatch, UTF-8 run-validation, or escaped-segment primitives as reusable vocabulary.

S-P1 is locked and makes `evidence/p1e-normalized-attribution.tsv` the binding empirical floor (`restart/skinny/tranches/sk-v15/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md:1-17`). The hot leaves split into six useful primitive classes:

| Class | P1 evidence | P2-E implication |
|---|---|---|
| Whitespace / byte-set skip | `skip_ascii_whitespace` is the first resolved parse-only hot leaf for `apache_builds` and direct/typed `unicode_escapes`; `DirectParser::ws` dominates `random` and `gsoc-2018` direct/typed rows (`restart/skinny/tranches/sk-v15/research/p1/p1a-samply-mode-1.md:22`; `restart/skinny/tranches/sk-v15/research/p1/p1b-samply-mode-2.md:26-27`; `restart/skinny/tranches/sk-v15/research/p1/p1b-samply-mode-2.md:32`). | Missing Layer-1 parametric byte-set run skip; current helper is JSON whitespace specific. |
| Structural / dispatch | `DirectParser::skip_value` dominates `twitter`, `citm_catalog`, `github_events`, and `marine_ik`; P1-C sees generated scan wrappers and movemask in structural-scan probes (`restart/skinny/tranches/sk-v15/research/p1/p1b-samply-mode-2.md:19-23`; `restart/skinny/tranches/sk-v15/research/p1/p1b-samply-mode-2.md:28`; `restart/skinny/tranches/sk-v15/research/p1/p1c-samply-mode-3.md:17-37`). | Missing local, transient structural-dispatch block primitive; retained structural indexes remain blocked. |
| Bounded plain literal spans | `DirectParser::tiny_plain_string_end` is the first typed `canada` hot leaf after generated strict product code (`restart/skinny/tranches/sk-v15/research/p1/p1b-samply-mode-2.md:21`). | Existing `scan_tiny_string_prefix_trusted_utf8` is fixed to JSON quote/escape/control policy; missing parameterized grammar-neutral span primitive. |
| UTF-8 / string validation | `core::str::validations::run_utf8_validation` is top JSON-probe self-time across most corpora and appears in product rows (`restart/skinny/tranches/sk-v15/research/p1/p1c-samply-mode-3.md:17-37`; `restart/skinny/tranches/sk-v15/research/p1/p1b-samply-mode-2.md:21`; `restart/skinny/tranches/sk-v15/research/p1/p1b-samply-mode-2.md:32-34`). | Missing Layer-1 validate-run primitive over the existing 16-byte Layer-0 block validator. |
| Numeric digit spans | `mesh` direct misses by c/B and comparator hot leaves resolve to decimal parse; `numbers` rows show allocation/tape pressure rather than a parser primitive, so numeric work needs direct/typed consumer proof (`restart/skinny/tranches/sk-v15/research/p1/p1d-pmu-cycles.md:49-53`; `restart/skinny/tranches/sk-v15/research/p1/p1d-pmu-cycles.md:58-70`). | Missing grammar-neutral digit-run span/accumulate primitive; full JSON number policy is too narrow. |
| Escaped string / unicode decode | Unicode-heavy rows surface validation and materialization pressure; `unescape_string` currently owns allocation and escape decoding (`skinny/crates/parse-that-regex/src/lib.rs:867-958`). | Missing non-allocating escaped-segment stream with SIMD hex support as an optional Layer-0 body. |

The current Layer-0 surface already contains useful scalar references and dispatch wrappers: `byte_class_from_table_64`, `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`, `eob_pad_clamp`, and `byte_class_from_eq_set_64` (`skinny/crates/bbnf-simd/src/lib.rs:251-292`; scalar references under `skinny/crates/bbnf-simd/src/scalar/`). It also contains AArch64 string, UTF-8, class-table, and unicode-escape bodies (`skinny/crates/bbnf-simd/src/aarch64/string_block.rs:31-72`; `skinny/crates/bbnf-simd/src/aarch64/utf8/validate_block.rs:76-158`; `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:21-66`; `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:33-183`). The missing part is not raw intrinsics alone; it is the grammar-neutral Layer-1 vocabulary and same-wave product consumers.

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

P2-E does not select or sequence implementation. The rows below are candidate gaps only. S-P3 must drop any row that cannot name a same-wave consumer, scalar oracle, REDRESS pre-block, and strict checkasm/parity gate.

### PTG-WS-BYTESET-RUN - `skip_byte_set_run`

Missing primitive shape:

```rust
pub fn skip_byte_set_run(input: &[u8], offset: usize, set: ByteSetRef<'_>) -> usize
```

`ByteSetRef` is caller/generated grammar data. JSON whitespace is one instantiation (`b' '`, `b'\r'`, `b'\t'`, `b'\n'`), not the primitive policy. The function returns only the first offset not in the set.

Scalar reference sketch: loop from `offset` while `set.contains(input[cursor])`, then return `cursor`. The scalar oracle may use the existing `skip_ascii_whitespace` behavior as a JSON fixture, but must be implemented and tested as a generic byte-set run (`skinny/crates/parse-that-regex/src/lib.rs:119-153`).

Layer placement: Layer 1 in `parse-that-regex`; optional Layer-0 acceleration via `bbnf_simd::prim::byte_class_from_eq_set_64` for sets <= 8 or `byte_class_from_table_64` for dense sets (`skinny/crates/bbnf-simd/src/lib.rs:254-292`).

Arch: primary AArch64 NEON through existing byte-class bodies; x86 paths are diagnostic for SK-V15. Scalar is mandatory fallback.

Checkasm expectation: scalar parity over empty sets, JSON whitespace, Sheets/CSS/BBNF whitespace sets, all offsets, alignments, and tails; if SIMD is routed, run strict `checkasm_byte_class_from_eq_set_64`, `checkasm_byte_class_from_table_64`, and caller-level `checkasm_ascii_set_member_find_64`.

Same-wave consumer: generated JSON `ws` and colon/comma whitespace sites (`skinny/crates/runtime/src/grammars/json/generated.rs:108-113`, `skinny/crates/runtime/src/grammars/json/generated.rs:239-328`, `skinny/crates/runtime/src/grammars/json/generated.rs:768-788`), plus a non-JSON generated layout consumer such as Sheets `?w` (`grammar/google-sheets/google-sheets.bbnf:103-161`) or BBNF `?w` directives (`grammar/bbnf/bbnf.bbnf:38-70`).

P1 antecedent: `skip_ascii_whitespace` for `apache_builds`, `unicode_escapes`; `DirectParser::ws` for `random` and `gsoc-2018`; allocation/tape pressure rows where whitespace skip feeds existing tape/direct sinks.

Grammar-neutrality: PASS if set policy is supplied by generated grammar metadata or caller data. FAIL if comments, JSON whitespace constants, or CSS trivia policy enter the generic primitive.

REDRESS blocks: REDRESS 51 and 53 block parser-owned whitespace/structural cursors; REDRESS 96/97/98 block retained class columns, streaming structural cursors, and union-substrate variants.

### PTG-STRUCT-DISPATCH-LOCAL - `classify_local_block_64`

Missing primitive shape:

```rust
pub struct LocalClassMasks {
    pub class_mask: u64,
    pub quote_mask: u64,
    pub escape_mask: u64,
    pub control_mask: u64,
}

pub fn classify_local_block_64(input: &[u8], offset: usize, alphabet: ByteSetRef<'_>) -> LocalClassMasks
```

The result is local to one parser step and must not be retained across call boundaries. It is a transient dispatch aid, not a structural index.

Scalar reference sketch: inspect up to 64 live bytes, set bits for bytes in `alphabet`, quote byte, escape byte, and control bytes below the caller-supplied cutoff. Dead tail bits are zero. Existing scalar `swar_8byte::classify_chunk` is the closest reference for full 64-byte blocks (`skinny/crates/bbnf-simd/src/scalar/swar_8byte.rs:3-18`).

Layer placement: Layer 0 in `bbnf-simd` for the block mask body; Layer 1 in `parse-that-regex` only as a cursor-local wrapper returning masks to a same-loop parser consumer.

Arch: AArch64 TBL4 classifier (`skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:21-66`) with scalar fallback. Existing x86 classifier references remain non-close diagnostic.

Checkasm expectation: strict parity with scalar for random, alignment, tail-clamped, all-control, all-quote, and all-escape blocks; caller checkasm must assert no retained mask state. Existing `checkasm_parity`, `checkasm_structural_terminator_64`, and `aarch64_primitives` are precedents, but this candidate needs its own local-wrapper parity if exposed through parse-that.

Same-wave consumer: generated direct `skip_value` / FIRST-set dispatch in JSON (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:4174` per P1 evidence) or a CSS/Sheets/BBNF generated FIRST-set scanner. It cannot ship as a standalone bbnf-simd body.

P1 antecedent: `DirectParser::skip_value` structural/dispatch hot leaves; P1-C `runtime::generated_json::scan::neon::scan` and `bbnf_simd::aarch64::movemask::movemask_u8x16` structural-scan probes.

Grammar-neutrality: PASS only when `alphabet` comes from generated grammar facts. REJECT if it encodes JSON structural bytes inside the generic crate.

REDRESS blocks: REDRESS 36/37 identify JSON hardcoding in SIMD scalar/classifier surfaces; REDRESS 51/53/96/97/98 block parser-local second scanners, retained structural indexes, class columns, and cross-call classifier state. Lock 1 v+1 rejects retained quote/escape/structural masks across call boundaries.

### PTG-PLAIN-LITERAL-SPAN - `bounded_plain_literal_span`

Missing primitive shape:

```rust
pub fn bounded_plain_literal_span(
    input: &[u8],
    open_offset: usize,
    delimiter: u8,
    escape: u8,
    control_limit: u8,
    cap: usize,
) -> Option<usize>
```

Returns the raw end offset if the literal closes within `cap` and no escape/control byte appears first. Returns `None` on miss and lets the caller fall back to the full string/literal matcher.

Scalar reference sketch: start after `open_offset`; scan up to `min(input.len(), open_offset + 1 + cap)`; return `Some(close + 1)` on delimiter, `None` on escape/control/tail. Existing `scan_tiny_string_prefix_trusted_utf8` proves the JSON-specific shape but fixes delimiter and escape policy (`skinny/crates/parse-that-regex/src/lib.rs:187-234`).

Layer placement: Layer 1 in `parse-that-regex`, scalar-first. Optional Layer-0 support may reuse `aarch64::string_block::scan_string_special_block`, but only after scalar/product proof (`skinny/crates/bbnf-simd/src/aarch64/string_block.rs:31-72`).

Arch: scalar first; optional AArch64 NEON only for long-enough caps where per-call overhead is measured below scalar. x86 is diagnostic.

Checkasm expectation: if SIMD is used, add strict parity over delimiter choices (`"`, `'`, `` ` ``), escape bytes, caps 0/1/8/16/64, control bytes, non-ASCII bytes, tails, and every alignment. Existing `aarch64_primitives::string_special_block_matches_scalar_reference` is only a smoke precedent.

Same-wave consumer: generated JSON parse-only string end sites (`skinny/crates/runtime/src/grammars/json/generated.rs:556-563`) and one non-JSON literal site: CSS strings (`grammar/css/l4/tokens.bbnf:7-9`), Sheets doubled-quote strings (`grammar/google-sheets/google-sheets.bbnf:8-12`), or BBNF literals/regex spans (`grammar/bbnf/bbnf.bbnf:11-15`).

P1 antecedent: `DirectParser::tiny_plain_string_end` in typed `canada`; `DirectParser::skip_value` string-heavy direct rows; P1-C UTF-8/string masking rows.

Grammar-neutrality: PASS if delimiter, escape, control cutoff, and cap are parameters. REJECT if this becomes `json_tiny_string_end` or wires only retained JSON parse rows.

REDRESS blocks: REDRESS 28/33 block prior Class A tiny-string NEON wiring; REDRESS 60 blocks deleting the scalar tiny-string early-out; REDRESS 72 admits only scalar cap widening; REDRESS 83 rejects generated-retained `StringBlock16` tiny-probe wiring.

### PTG-UTF8-RUN-VALIDATE - `validate_utf8_run`

Missing primitive shape:

```rust
pub struct Utf8RunStatus {
    pub valid_up_to: usize,
    pub error_offset: Option<usize>,
}

pub fn validate_utf8_run(input: &[u8], start: usize, end: usize) -> Utf8RunStatus
```

The primitive validates a contiguous run and returns only local status. No decoded string, no sink stats, no cross-call carry.

Scalar reference sketch: a byte-walk using the Hoehrmann-style width checks already present in `parse-that-regex/src/unicode/utf8_hoehrmann.rs:3-87`, extended from a single `[u8; 16]` block to a run loop. The existing wrapper currently validates only a 16-byte block (`skinny/crates/parse-that-regex/src/unicode/utf8_block.rs:21-36`).

Layer placement: Layer 1 in `parse-that-regex`; Layer 0 is `bbnf-simd::aarch64::utf8::validate_block` plus scalar reference (`skinny/crates/bbnf-simd/src/aarch64/utf8/validate_block.rs:76-158`).

Arch: AArch64 NEON for ASCII-fast 16-byte blocks; scalar fallback everywhere. Any state required for a split multibyte sequence must remain inside the single `validate_utf8_run` call.

Checkasm expectation: strict `checkasm_utf8_block` remains necessary but insufficient. Add run-level parity for ASCII, complete multibyte, boundary split, overlong, surrogate, invalid continuation, every offset/alignment, and short tails.

Same-wave consumer: string matcher validation in `match_string_at_quote` / `skip_string_plain` (`skinny/crates/parse-that-regex/src/lib.rs:354-408`, `skinny/crates/parse-that-regex/src/lib.rs:600-748`) or generated non-JSON literal validation where input is bytes rather than trusted Rust `&str`.

P1 antecedent: `core::str::validations::run_utf8_validation` top JSON-probe row across most corpora; product rows for `canada`, `random`, `unicode_basic`, `distinct_values`.

Grammar-neutrality: PASS because UTF-8 validity is an input encoding fact, not JSON policy. REJECT if the primitive fuses validation with JSON escape decoding or string materialization.

REDRESS blocks: REDRESS 50-55 block fused parse-time decoded stats, quote-source materializers, and parser-owned sidecar routes; Lock 1 v+1 blocks cross-call continuation state. The admissible material differential is validate-only within one call.

### PTG-DIGIT-RUN-ACCUMULATE - `digit_run_span_accumulate` (REJECTED for this S-P2 cycle)

Missing primitive shape:

```rust
pub struct DigitRun {
    pub end: usize,
    pub digit_count: u32,
    pub mantissa_prefix: u64,
    pub truncated_or_overflow: bool,
}

pub fn digit_run_span_accumulate(input: &[u8], offset: usize, max_accum_digits: u8) -> DigitRun
```

It consumes only ASCII digits. Sign, leading-dot, decimal point, exponent, suffix/unit, and JSON leading-zero policy stay in generated grammar or the existing full number layer. V1 CH1 rejected this row as an implementation candidate because the current SK-V15 P1 ledger does not name a surviving BBNF-side numeric hot leaf: `mesh` Track 1 is a schema-shaped generated wrapper and comparator decimal parsing is diagnostic comparator work (`restart/skinny/tranches/sk-v15/research/p1/evidence/p1e-normalized-attribution.tsv:45`, `restart/skinny/tranches/sk-v15/research/p1/evidence/p1e-normalized-attribution.tsv:46`, `restart/skinny/tranches/sk-v15/research/p1/evidence/p1e-normalized-attribution.tsv:48`).

Scalar reference sketch: factor the private `scan_digit_run` plus `parse_two_digits`, `parse_four_digits`, and `parse_eight_digits` into a public grammar-neutral oracle (`skinny/crates/parse-that-regex/src/number/mod.rs:105-223`). It must preserve `NumberSpan` semantics when used by `match_number_span_from_first` (`skinny/crates/parse-that-regex/src/number/mod.rs:31-103`) and materializers (`skinny/crates/parse-that-regex/src/number/mod.rs:225-271`).

Layer placement: diagnostic only in this cycle. If a later P1 reopens numeric work with a BBNF-side hot leaf, the Layer 1 home would be `parse-that-regex`; optional Layer-0 helpers would be `byte_class_from_range_64` and AArch64 digit MAC / UDOT. The existing 4-digit AArch64 smoke helper is not a production primitive by itself (`skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:4-49`).

Arch: scalar first. AArch64 DotProd / UDOT is admissible only for fixed-width chunks after a scalar oracle and checkasm; x86 VNNI/AVX variants remain diagnostic for SK-V15.

Checkasm expectation: scalar parity for lengths 0..128, offsets, non-digit terminators, tails, mantissa truncation, overflow, fractional/exponent caller compositions, and CSS/Sheets leading-dot cases. Any UDOT body needs a dedicated strict checkasm beyond `aarch64_primitives::digit_mac_parses_four_digit_blocks`.

Same-wave consumer: none for SK-V15 S-P2. JSON direct numeric rows are not valid antecedents in the current ledger, and non-JSON numeric consumers cannot manufacture a missing P1 hot leaf.

P1 antecedent: none accepted for implementation in this cycle. `mesh` direct c/B miss and comparator decimal parse pressure are diagnostic; `numbers` and `canada` are routed to allocation/tape or unicode/string pressure, not digit-run admission.

Grammar-neutrality: REJECT for S-P2 candidate status despite the grammar-neutral shape. A future retry must first arrive through fresh P1 evidence, then keep JSON number grammar, f64 fallback policy, CSS unit policy, and Sheets leading-dot policy outside the generic primitive.

REDRESS blocks: REDRESS 80 blocks mantissa-widen/f64-fallback routes without a same-wave consumer; REDRESS 81 admits capacity hints for typed Vec consumers but does not authorize number-parser policy changes. This row is diagnostic inventory only until reopened by P1.

### PTG-ESCAPED-SEGMENTS - `escaped_literal_segments`

Missing primitive shape:

```rust
pub enum EscapedSegment<'a> {
    Raw(&'a str),
    Simple(char),
    Unicode(char),
}

pub fn escaped_literal_segments<'a>(
    raw_content: &'a str,
    escape_table: EscapeTable,
    visitor: impl FnMut(EscapedSegment<'a>),
) -> Result<(), RegexError>
```

The primitive streams raw spans and decoded scalar values to the caller. It does not allocate decoded scratch, compute output hashes, own semantic string facts, or pick a grammar's escape policy.

Scalar reference sketch: reuse `decode_unicode_escape` and `validate_unicode_escape_run` semantics (`skinny/crates/parse-that-regex/src/lib.rs:441-520`) and the current `unescape_string` loop as a semantic reference (`skinny/crates/parse-that-regex/src/lib.rs:867-958`). The no-escape fast path must preserve `Cow::Borrowed` behavior for current callers (`skinny/crates/parse-that-regex/src/lib.rs:867-870`).

Layer placement: Layer 1 in `parse-that-regex`; Layer 0 optional support is `bbnf-simd::aarch64::unescape_uxxxx` x1/x4 (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:33-183`) and a future stricter x8/x16 body only after checkasm.

Arch: scalar first. AArch64 TBL hex decode is optional support; it cannot be product-routed on valid-only smoke tests.

Checkasm expectation: scalar segment parity for simple escapes, invalid escapes, valid/invalid Unicode escapes, high/low surrogate policy, mixed x4 groups, alignment, tail, and dense escape runs. Current `checkasm_utf8_block::unescape_uxxxx_x4_matches_scalar` is only a valid-case smoke and must be extended before admission.

Same-wave consumer: direct/typed decoded-string field delivery or a generated non-JSON literal consumer such as CSS escaped strings (`grammar/css/l4/tokens.bbnf:9`), CSS hex colors (`grammar/css/l4/color.bbnf:187-190`), Sheets doubled-quote strings (`grammar/google-sheets/google-sheets.bbnf:8-12`), or BBNF literals (`grammar/bbnf/bbnf.bbnf:11-15`).

P1 antecedent: unicode/string masking rows; `unicode_escapes`, `unicode_mixed`, `y_string_unicode` product-plane pressure; current `unescape_string` users in generated real typed and direct structs (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:4485`, `skinny/crates/bbnf-bench/src/generated_real_typed.rs:4863`; `skinny/crates/bbnf-bench/src/direct_struct.rs:625-655`).

Grammar-neutrality: PASS if `EscapeTable` comes from grammar metadata or caller data and surrogate joining remains caller/grammar policy. REJECT if JSON `\uXXXX` semantics become the generic API.

REDRESS blocks: REDRESS 54 and 55 reject decoded stats and fused quote-source materializers; REDRESS 60-72 block retained parse/string materialization variants; REDRESS 82 rejects single-quartet production promotion; REDRESS 106-108/126-style primitive-only promotion remains blocked without a consumer.

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

All live candidates must be expressed as byte-set, range, literal-span, digit-run, UTF-8 validation, local mask, or segment-stream operations. None may expose JSON, CSS, Sheets, or BBNF names in a generic API. This follows Lock 14's rule that generic crates, including `parse-that-regex`, `parse-that`, and `bbnf-simd`, carry zero grammar-specific public APIs or grammar-named policy (`restart/locks/LOCKS.md:349-419`).

| Candidate | Grammar-neutral verdict | Non-JSON witness requirement |
|---|---|---|
| `skip_byte_set_run` | PASS if set supplied by caller/generated facts. | Sheets `?w` or BBNF `?w`; CSS whitespace only if comments remain generated policy. |
| `classify_local_block_64` | PASS if alphabet is generated data and masks are transient-single-call. | CSS selector/value FIRST sets, Sheets formula delimiters, or BBNF punctuation. |
| `bounded_plain_literal_span` | PASS if delimiter/escape/control/cap are parameters. | CSS strings, Sheets strings, or BBNF literal/regex spans. |
| `validate_utf8_run` | PASS; UTF-8 is encoding validation, not grammar policy. | Any non-JSON byte-backed literal path, or negative-control proof that generated Rust `&str` already owns validation. |
| `digit_run_span_accumulate` | REJECT as a current S-P2 candidate because the P1 bridge is missing. | CSS numbers/dimensions, Sheets numbers, and BBNF int/float literals remain future witnesses only after fresh P1 evidence. |
| `escaped_literal_segments` | PASS if escape table and surrogate policy are caller-owned. | CSS escaped strings/hex colors, Sheets doubled quotes, BBNF literals. |

JSON-only wording is rejected. A JSON row may be the first measured consumer, but S-P3 must pair it with a generated non-JSON consumer, a negative-control witness, or a scoped claim that avoids fleet-wide language.

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

Blocked as non-candidates:

| Surface | Reason |
|---|---|
| Harness hashes / checksums | P1 marks many top leaves as harness masking cost. Digest/hash belongs to output verification, not parse-that or bbnf-simd parser semantics. |
| Schema-specific generated builders | `parse_type_*` hot leaves are product builders. They are same-wave consumers, not generic primitives. |
| Retained structural indexes / cursors | REDRESS 51, 53, 96, 97, and 98 falsify parser-owned sidecars, second scanners, retained class columns, streaming cursors, and union-substrate variants. |
| Standalone SIMD body fills | Checkasm-only or smoke-only bodies do not count. Every Layer-0 body needs scalar reference, strict checkasm, and same-wave consumer before product routing. |
| Cross-call classifier state | Lock 1 v+1 rejects retained quote, escape, structural, class, prev-state, prefix-XOR, or carry state across call boundaries. |
| Numeric fallback changes | REDRESS 80 blocks mantissa-widen/f64-fallback routes without fresh measured fallback evidence and same-wave consumer. |
| Tiny-string NEON rewires | REDRESS 28/33/83 block the prior tiny-string NEON/StringBlock16 routes; scalar bounded-literal factoring must not claim those wins. |
| Eager decoded-string materializers | REDRESS 54/55 and 60-72 block decoded stats, fused hash/materializer, retained parse, and source-side decoded scratch routes. |

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

Local sources used:

- S-P2 prompt and output schema: `restart/prompts/skinny/PASS-2-RESEARCH.md`.
- SK-V15 authority: `restart/skinny/tranches/sk-v15/HANDOFF.md`, `restart/skinny/tranches/sk-v15/SYNTHESIS.md`.
- S-P1 evidence and convergence: `restart/skinny/tranches/sk-v15/research/p1/p1a-samply-mode-1.md`, `p1b-samply-mode-2.md`, `p1c-samply-mode-3.md`, `p1d-pmu-cycles.md`, `p1e-hot-leaf-attribution.md`, `p1f-results-delta.md`, `research/p1/evidence/*.tsv`, `research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md`.
- Admission and redress ledgers: `skinny/RESULTS.md`, `skinny/REDRESS.md`.
- Locks: `restart/locks/LOCKS.md`.
- parse-that-regex code: `skinny/crates/parse-that-regex/src/lib.rs`, `src/number/mod.rs`, `src/number/integer.rs`, `src/number/eisel_lemire/algorithm.rs`, `src/unicode/utf8_block.rs`, `src/unicode/utf8_hoehrmann.rs`, `src/integration/simd_scan_hook.rs`.
- bbnf-simd code and tests: `skinny/crates/bbnf-simd/src/lib.rs`, `src/dispatch.rs`, `src/classifier.rs`, `src/scalar/*.rs`, `src/aarch64/*.rs`, `tests/checkasm_*.rs`, `tests/aarch64_primitives.rs`.
- Consumers and grammar witnesses: `skinny/crates/runtime/src/grammars/json/generated.rs`, `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, `skinny/crates/bbnf-bench/src/direct_struct.rs`, `skinny/crates/bbnf-bench/src/track2/json.rs`, `grammar/google-sheets/google-sheets.bbnf`, `grammar/css/l4/tokens.bbnf`, `grammar/css/l4/value-unit.bbnf`, `grammar/css/l4/color.bbnf`, `grammar/bbnf/bbnf.bbnf`, `grammar/bbnf/expressions.bbnf`.
