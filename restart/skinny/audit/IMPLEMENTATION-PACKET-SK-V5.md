# Implementation Packet SK-V5

Date: 2026-05-13.

Workspace: `/Users/mkbabb/Programming/bbnf-lang/skinny`.

Authority:

- `restart/skinny/audit/GRAND-SYNTHESIS-SK-V5.md` (companion synthesis)
- `restart/skinny/audit/SK-V5-COHORT/` (15 audit reports, 5,559 LOC)
- `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT.md` (prior packet)
- `restart/MASTER-PLAN.md` §13 H tranche
- `restart/ARCHITECTURE.md` §7.3 (5-shape BackendShape, derive_backend_shape spec)
- `skinny/RESULTS.md` (current gate authority)

## 0. Close Condition

SK-V5 is complete when `skinny/RESULTS.md` shows no parse-G rows, no
N-direct rows, with strictness disclosed per row, against sonic-rs
1.10× slack on M5 Max. x86 CollapsedStage successor remains out of
scope for SK-V5 close.

Required local M5 Max pass:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo run -p xtask --release -- check-conformance
cargo run -p xtask --release -- bench-json
cargo run -p xtask --release -- gate-json
```

Required final report state:

- expanded 17-row parse matrix: no outcome-G rows;
- expanded 17-row direct matrix: no N-direct rows;
- Strictness | parse_utf8 | escape_complete | flaw_probe columns
  populated for every row;
- direct Track 1 calls generated runtime SinkOnly, not bench-private
  parser;
- Track 2 calls a structurally-different hand-coded path (the
  current Track 1 ≡ Track 2 ≡ bench-private dishonesty is corrected);
- parse_value_at no longer collapses to one symbol without
  PC-level explanation;
- sidecar rows for sonic-rs / simdjson C++ / yyjson / asmjson SWAR
  recorded with API and output plane named;
- `cargo run -p xtask --release -- primitive-checkasm` passes;
- Lock 14 audit clean (zero grammar leaks in generic crates).

## 1. Non-Negotiables

| Rule | Enforcement |
|---|---|
| No new BBNF directives | `rg -n "@(simd\|runtime\|backend\|shape\|asm\|sink\|direct)" grammars restart/skinny` has no new directive surface beyond what's in the grammar today. |
| No hidden metadata backend selector | `LayoutFacts.backend_shape` is cost-model-derived by `derive_backend_shape`; no grammar-side `backend_shape = ` key. |
| No new BIR variant | Use existing `Alt { Dispatch }`, `TapeEmit`, `DirectBuild`, `CallHost`. `DirectBuild` extends in shape (field roster, slot map) not in variant count. |
| No parallel substrate | Mask streams transient; retained APIs seal `OffsetTape`/`EventTape`; direct-only APIs use `SinkOnly`. |
| No JSON code in generic crates | `bbnf-simd`, `parse-that-regex`, `codegen/lower`, `runtime/tape` are grammar-neutral. JSON specifics live in `runtime/grammars/json/`, codegen-emitted `.data` tables, and the grammar definition file. |
| Scalar reference per primitive | Every SIMD/ASM primitive ships with a scalar Rust executable specification + checkasm parity. |
| Same-wave consumer | A primitive lands only with the generated/runtime consumer that exercises it on the hot path. |
| Profiles first | Every SOTA claim cites profile path, c/B or Mbps, and affected corpus rows. |
| Strictness disclosed | Every bench row names strictness plane + output plane. Sidecar rows match the same planes. |

## 2. Wave 0 — Strictness + Diagnostic Infrastructure + Nuke Audit

### 2.1 Owner paths

- `skinny/RESULTS.md` (add columns)
- `skinny/crates/runtime/Cargo.toml` (add `parse-attribution` feature)
- `skinny/crates/runtime/src/grammars/json/generated.rs` (gate
  `#[inline(always)]` → `#[inline(never)]` on named boundaries under
  the feature)
- `skinny/crates/bbnf-bench/src/lib.rs` + relevant harness files (emit
  strictness column from the `Sidecar` trait)
- `restart/skinny/audit/NUKE-PLAN-SK-V5.md` (decision authority)

### 2.2 Strictness columns

Add four columns to `skinny/RESULTS.md`'s per-row tables:

| Column | Meaning |
|---|---|
| `Strictness` | `strict` / `permissive` / `deferred` per RFC 8259 conformance |
| `parse_utf8` | `scan-boundary` / `view-boundary` / `none` |
| `escape_complete` | `yes` / `no` (does the parser fully scan strings for unescaped controls?) |
| `flaw_probe` | one-line summary of where the parser diverges from strict RFC 8259 on JSONTestSuite input |

For each row, fill in:

- bbnf Track 1: `strict` / `view-boundary` / `yes` (today, this is
  `deferred` for parse-only because UTF-8 fall-through is scalar and
  shape-dependent; mark honestly).
- sonic-rs: `strict` / `scan-boundary` / `yes`.
- simdjson C++: `strict` / `scan-boundary` / `yes`.
- yyjson default: `strict` / `scan-boundary` / `yes`.
- asmjson SWAR: `permissive` / `none` / `no` / "accepts 0x00..0x1F as
  whitespace; passes unescaped controls inside strings".
- RapidJSON default: `permissive` / `none` / `no`.
- serde_json: `strict` / `scan-boundary` / `yes`.

The N-direct verdict re-reads as a contract+throughput delta, not pure
throughput.

### 2.3 parse-attribution feature flag

Add `parse-attribution` feature to `skinny/crates/runtime/Cargo.toml`.
In `generated.rs`, gate seven kernel-boundary helpers behind
`#[cfg_attr(feature = "parse-attribution", inline(never))]`
`#[cfg_attr(not(feature = "parse-attribution"), inline(always))]`:

1. `dispatch_value` (the source-byte → handler match)
2. `skip_whitespace_boundary`
3. `open_object` / `close_object` / `open_array` / `close_array`
4. `match_string_at_quote` entry
5. `match_number_at_digit` entry
6. `verify_literal_true` / `verify_literal_false` / `verify_literal_null`
7. `tape_emit_token` / `tape_advance_cursor`

Document the seven boundaries in `restart/skinny/COMPILER.md` and
`skinny/REDRESS.md` so future cohort B agents can attribute at the
symbol level directly.

### 2.4 Nuke audit (decisions only; execution Wave 4)

Catalogue every nuke candidate from `restart/skinny/audit/NUKE-PLAN-SK-V5.md`.
The packet records the decision; the deletions land in Wave 4 alongside
the bbnf-simd Lock-14 remediation so all related changes commit
together.

### 2.5 Exit gates

- `RESULTS.md` shows four new columns populated.
- Strictness honestly disclosed; flaw_probe column references JSONTestSuite
  result IDs where applicable.
- `cargo build --release -p xtask --bin profile-lazy --features
  runtime/parse-attribution` succeeds.
- Initial cohort-B-style attribution run with the feature on yields
  named hot leaves (not one fused symbol).
- `NUKE-PLAN-SK-V5.md` lists each candidate with file, action, dependent
  references, post-nuke verification command.

## 3. Wave 1 — Substrate Authoring (BackendShape + LayoutFacts + derive_backend_shape)

### 3.1 Owner paths

- `skinny/crates/ir/src/lib.rs` (BackendShape enum)
- `skinny/crates/passes/src/lib.rs` (LayoutFacts.backend_shape field + derive_backend_shape function)
- `skinny/crates/codegen/src/lib.rs` (consume `&BackendIr` properly; stop discarding it)
- `skinny/crates/codegen/src/lower/` (NEW directory; per-shape lowerer)
- `skinny/crates/codegen/src/lower/rust.rs` (the actual lowerer)

### 3.2 BackendShape enum

In `ir/src/lib.rs`, add (no new BIR variant; this is a new enum
adjacent to `BackendIr`):

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BackendShape {
    EagerTape,
    OffsetTape,
    EventTape,
    SinkOnly,
    CollapsedStage,
}
```

The five variants match `restart/ARCHITECTURE.md:1048-1072` verbatim.

### 3.3 LayoutFacts.backend_shape field

In `passes/src/lib.rs:46-51`, extend `LayoutFacts`:

```rust
pub struct LayoutFacts {
    /// existing fields ...
    pub backend_shape: HashMap<RuleId, BackendShape>,
}
```

### 3.4 derive_backend_shape function

Implement the 8-step algorithm from `restart/ARCHITECTURE.md:1075-1083`:

```rust
pub fn derive_backend_shape(
    grammar: &Grammar,
    ir: &BackendIr,
    layout: &LayoutFacts,
    target: TargetFeatures,
) -> HashMap<RuleId, BackendShape> {
    // 1. @error(recover) anywhere transitively → EagerTape
    // 2. @host fn parse-time-decoded → EagerTape
    // 3. @layout scope wider than rule → EagerTape
    // 4. First-set overlap (speculative dispatch needed) → EagerTape
    //    or OffsetTape with EventCursor inside
    // 5. DirectBuild present + no retained API consumer → SinkOnly
    // 6. CollapsedStage admissibility: JSON-class one-byte-decidable
    //    + target supports AVX-512BW + NASM author declared → CollapsedStage
    // 7. Hub arity / dispatch entropy → OffsetTape vs EventTape choice
    // 8. Default → OffsetTape (the proven shape from triad wins)
}
```

Per-rule output; not per-grammar. The result is consumed by the lowerer
which dispatches on `BackendShape::*`.

### 3.5 BBNF-BACKEND-SHAPE-INCONSISTENT diagnostic

When `derive_backend_shape` produces a result that requires
`CollapsedStage` but the NASM kernel does not exist for the target ISA,
emit `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` and fall back to `OffsetTape`.
When the cost model produces internally-inconsistent constraints (e.g.
a rule that needs both `@error(recover)` retained recovery AND
`SinkOnly` direct emission), emit `BBNF-BACKEND-SHAPE-INCONSISTENT` and
fall back to `EagerTape`. Diagnostic surface lives in
`passes/src/diagnostics.rs` (new file) and is consumed by the codegen
pipeline.

### 3.6 codegen/src/lower/ hierarchy

Create:

```
skinny/crates/codegen/src/lower/
├── mod.rs        # pub mod rust; pub use rust::lower_to_rust;
├── rust.rs       # the actual lowerer; dispatches on BackendShape
├── eager_tape.rs # per-shape lowering body
├── offset_tape.rs
├── event_tape.rs
├── sink_only.rs
└── collapsed_stage.rs (stub initially; Wave 7 only)
```

Each per-shape module exposes `pub fn lower_<shape>(rule: &Rule, ir:
&RuleBackendIr, ctx: &LowerCtx) -> String` emitting the Rust body for
that rule under that shape.

### 3.7 Codegen pipeline corrected

In `codegen/src/lib.rs:111-117`, replace:

```rust
let _ = backend;
let parser_rs = include_str!("templates/json/parser.rs");
let generated_rs = include_str!("templates/json/generated.rs");
```

with:

```rust
let shapes = derive_backend_shape(&grammar, backend, layout, target);
let parser_rs = lower::lower_to_rust(&grammar, backend, &shapes, &ctx).parser;
let generated_rs = lower::lower_to_rust(&grammar, backend, &shapes, &ctx).generated;
```

The existing static templates at `codegen/src/json_templates/` become
fallback / reference; the lowerer consumes the BIR honestly.

### 3.8 Exit gates

- `BackendShape` enum used in code (not just bbnf.asm comments).
- `LayoutFacts.backend_shape` field populated by `derive_backend_shape`.
- `codegen/src/lib.rs:111-117` no longer discards `&BackendIr`.
- `codegen/src/lower/rust.rs` exists; produces byte-identical output to
  the current `include_str!` templates for the JSON grammar when the
  cost model selects the current shape per rule (regression-free
  transition).
- `cargo test --workspace` passes.
- Parse benchmarks within ±2% of the prior gate (no regression from
  substrate plumbing).

## 4. Wave 2 — Number Lever + Generated SinkOnly

### 4.1 Owner paths

- `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/eisel_lemire/` (source)
- `skinny/crates/parse-that-regex/src/number/` (NEW directory)
- `skinny/crates/parse-that-regex/src/number/mod.rs`
- `skinny/crates/parse-that-regex/src/number/integer.rs`
- `skinny/crates/parse-that-regex/src/number/eisel_lemire/` (vendored)
- `skinny/crates/parse-that-regex/src/lib.rs` (move misplaced bits)
- `skinny/crates/codegen/src/lower/sink_only.rs` (the field-write lowerer)
- `skinny/crates/runtime/src/grammars/json/generated.rs` (consumer)
- `skinny/crates/bbnf-bench/src/direct_struct.rs` (delete bench-private SinkParser; rewire)

### 4.2 Vendor Eisel-Lemire

Copy verbatim from
`/Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/eisel_lemire/`:

- `algorithm.rs` (Clinger fast path + EL slow path)
- `table.rs` (the 619-entry power-of-10 table)
- `mod.rs` (`compute_f64(mantissa: i64, exp10: u64, neg: bool) ->
  Option<f64>`)
- Bit-parity tests at `tests/number_fastpath_test.rs`

Place under `parse-that-regex/src/number/eisel_lemire/`. License headers
preserved (MIT/Apache-2.0 dual per the source). Add to
`parse-that-regex/Cargo.toml` if no new dependencies are needed; the
source has none beyond `std`.

### 4.3 Move integer materializer

`bbnf-bench/src/direct_struct.rs:501-528` carries `parse_integer_digest`
with correct `i64::MIN` handling. Move to
`parse-that-regex/src/number/integer.rs` as `pub fn parse_integer(input:
&[u8]) -> Result<i64, NumberError>`.

### 4.4 Number primitive public API

In `parse-that-regex/src/number/mod.rs`:

```rust
pub fn materialize_f64(span: &NumberSpan) -> Result<f64, NumberError> {
    // Scan span → (mantissa, exp10, neg, digit_count) via scan augmentation;
    // Clinger fast path via eisel_lemire::clinger;
    // EL fast/slow path via eisel_lemire::compute_f64;
    // Ambiguous-rounding sentinel → slow fallback.
}

pub fn materialize_i64(span: &NumberSpan) -> Result<i64, NumberError> {
    // Integer fast path; i64::MIN handling.
}

pub fn materialize_u64(span: &NumberSpan) -> Result<u64, NumberError> { ... }
```

`NumberSpan` extends `JsonNumberMatch` (in `parse-that-regex/src/lib.rs:104`)
to carry `(digit_count, decimal_exp, mantissa_overflow)` so EL skips a
second walk. The scanner augmentation is the cheapest single win.

### 4.5 SinkOnly lowering

In `codegen/src/lower/sink_only.rs`, lower BIR `DirectBuild { shape:
"JsonObject" }` to a generated `parse_object_sink(input, &mut cursor,
sink)` body that walks the input bytes and emits typed sink calls:

```rust
sink.begin_object();
loop {
    skip_whitespace(input, &mut cursor);
    // key
    let key = parse_string_into(input, &mut cursor, sink)?;
    skip_to_colon(input, &mut cursor);
    // value
    parse_value_sink(input, &mut cursor, sink)?;
    // comma or }
    if !consume_comma_or_close(input, &mut cursor) { break }
}
sink.end_object();
```

Similar for `JsonArray`, `JsonString`, `JsonNumber`, `JsonBool`, `JsonNull`.

The `Sink` trait lives in `runtime/src/grammars/json/sink.rs` (new file):

```rust
pub trait JsonSink {
    fn begin_object(&mut self);
    fn end_object(&mut self);
    fn begin_array(&mut self);
    fn end_array(&mut self);
    fn key(&mut self, s: &str);
    fn string(&mut self, s: &str);
    fn key_source(&mut self, raw: &str, needs_unescape: bool) -> Result<(), RegexError>;
    fn string_source(&mut self, raw: &str, needs_unescape: bool) -> Result<(), RegexError>;
    fn array_string_source(&mut self, raw: &str, needs_unescape: bool) -> Result<(), RegexError>;
    fn object_string_source(&mut self, raw: &str, needs_unescape: bool) -> Result<(), RegexError>;
    fn i64(&mut self, n: i64);
    fn f64(&mut self, n: f64);
    fn bool(&mut self, b: bool);
    fn null(&mut self);
}
```

Generated `parse_direct_digest` consumes a `JsonSinkDigest: JsonSink`
that produces the digest hash used by the bench.
Post-redress update: generated direct source now passes raw string spans plus
`needs_unescape` into the `*_source` hooks. The default hooks preserve the
previous allocation behavior, so this is a neutral substrate seam rather than a
throughput claim. A no-allocation decoded-string visitor consumer was measured
and rejected in `skinny/REDRESS.md` item 49; the next admissible close is a
fused decode+sink primitive, not a generic visitor layered on
`unescape_json_string`.

Post-redress update 2: retained projection aux side tables were measured and
rejected in `skinny/REDRESS.md` item 50. Do not implement H.W1 as dense or
sparse parse-time metadata over `Tape::offsets`; both variants improved view
probes but regressed retained parse. H.W1 must consume typed events over the
existing tape projection without adding a retained parse-time side column.

Post-redress update 3: a transient byte-class whitespace `EventCursor` wrapper
was measured and rejected in `skinny/REDRESS.md` item 51. Do not implement
H.W1 by moving `skip_json_whitespace` behind a cursor facade or by using
`BYTE_CLASS_FROM_EQ_SET_64` as a generic "next non-whitespace byte" wrapper.
The admissible implementation is the structural-mask cursor: factor the JSON
scanner's per-64-byte emit-mask calculation, carry only O(1) pending mask /
quote / escape state, yield structural punctuation and quote events directly,
and keep scalar validation in the grammar-neutral string/number/literal
primitives.

### 4.6 Bench rewire + bench-private nuke

`bbnf-bench/src/direct_struct.rs`: delete `SinkParser`, `track1_digest`,
`track2_digest`, `sink_only_digest` private parser bodies. Replace with:

```rust
pub fn track1_digest(input: &[u8]) -> u64 {
    let mut digest = JsonSinkDigest::default();
    runtime::generated_json::parse_direct(input, &mut digest).unwrap();
    digest.into_hash()
}

pub fn track2_digest(input: &[u8]) -> u64 {
    // Hand-coded reference path that does NOT share code with Track 1.
    // Different SinkParser shape (e.g. iterator-based vs callback-based).
    hand::sink_digest(input)
}
```

Track 2 becomes a structurally-different hand-coded path so the
parity is meaningful.

### 4.7 Exit gates

- `parse-that-regex/src/number/` exists with Eisel-Lemire vendored.
- `materialize_f64` parity with sonic-rs / yyjson on canada / numbers /
  mesh / marine_ik corpora (within ±1 ULP per Rust stdlib `dec2flt`
  precedent; document as `lemire_within_1ulp = true` in the gate).
- `codegen/src/lower/sink_only.rs` emits the 7-rule JSON SinkOnly lowering.
  Current implementation status: Track 1 calls generated runtime and the
  direct source is rendered from a BIR-derived `SinkOnlyProgram`; the former
  static `json_templates/sink_direct.rs` splice is removed.
- `bbnf-bench` calls generated runtime for Track 1.
- Track 1 and Track 2 produce different symbol paths under
  `samply` (Track 1: generated `parse_direct`; Track 2: hand-coded
  module).
- numbers / canada / mesh / marine_ik direct rows cross the sonic-rs
  1.10× slack or the report names the exact residual blocker. Latest gate:
  `numbers` passes; `canada`, `mesh`, and `marine_ik` remain near-miss NO-GO.
- All four rows + bench-private removal recorded in REDRESS.md.

## 5. Wave 3 — UTF-8 Fusion + Class B Batched

### 5.1 Owner paths

- `skinny/crates/parse-that-regex/src/lib.rs:295-347` (the
  `match_string_at_quote` dispatcher with 0x80 early-exit)
- `skinny/crates/parse-that-regex/src/string/` (NEW directory)
- `skinny/crates/parse-that-regex/src/unicode/` (NEW directory)
- `skinny/crates/parse-that-regex/src/unicode/utf8_block.rs` (NEW; Lemire 64-byte validator)
- `skinny/crates/parse-that-regex/src/unicode/utf8_hoehrmann.rs` (NEW; scalar reference)
- `skinny/crates/bbnf-simd/src/aarch64/utf8/` (NEW directory; NEON intrinsics)
- `skinny/crates/bbnf-simd/src/aarch64/utf8/validate_block.rs` (NEW)
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs` (extend with `_x4` batched + surrogate pair join)
- `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs` (NEW)

### 5.2 NEON UTF-8 codepoint pipeline

In `parse-that-regex/src/lib.rs:331-339`, the current 0x80 early-exit
in the 16-byte NEON loop falls through to `validate_utf8_codepoint`
scalar. Replace with:

```rust
// Inside the 16-byte NEON block loop:
let chunk = vld1q_u8(input.as_ptr().add(cursor));
let special_mask = match_special_chars(chunk); // existing
let utf8_status = bbnf_simd::aarch64::utf8::validate_block(chunk);
if special_mask.any() {
    // Found ", \, or control — exit on the next-set boundary.
    return position_of_first_set(special_mask);
}
if utf8_status.is_valid_and_complete() {
    cursor += 16;
    continue;
}
if utf8_status.is_valid_but_continues() {
    // Codepoint spans the block boundary; copy the trailing
    // bytes into a scratch and re-enter the validator at the
    // next block start.
    cursor += utf8_status.complete_bytes();
    continue;
}
// Invalid UTF-8 → raise BBNF-INVALID-UTF8 at scan boundary.
return Err(ScanError::InvalidUtf8 { at: cursor + utf8_status.bad_byte_offset() });
```

`validate_block` returns a small struct with `is_valid`,
`complete_bytes` (0..=16), `bad_byte_offset` (Option), and a
`continues` flag for cross-block codepoints.

### 5.3 Lemire 64-byte validator (NEON port)

`bbnf-simd/src/aarch64/utf8/validate_block.rs` implements a NEON port of
Lemire's "Validating UTF-8 In Less Than One Instruction Per Byte" 2020
paper. Three `vqtbl4q_u8` lookup tables (high-nibble class, second-byte
class, third-byte class) reduce per-codepoint validation to vectorized
table lookups. Output: per-byte error mask folded into the four
status flags above.

Scalar reference at `parse-that-regex/src/unicode/utf8_hoehrmann.rs` is
Hoehrmann's 56-state DFA (the canonical executable specification for
UTF-8 validation; see Bjoern Hoehrmann 2010).

### 5.4 unescape_uxxxx batched + surrogate pair join

`bbnf-simd/src/aarch64/unescape_uxxxx.rs` currently exposes
`unescape_uxxxx_neon(quartet: &[u8; 4]) -> Result<u32, EscapeError>` per
the D6 audit (wired at `parse-that-regex/src/lib.rs:728`). Add:

```rust
pub fn unescape_uxxxx_x4_neon(
    quartets: &[u8; 16],  // 4 ASCII hex quartets packed
) -> [u32; 4]
```

Plus a NEON surrogate-pair joiner for `😀` etc.:

```rust
pub fn join_surrogate_pair_neon(high: u32, low: u32) -> Option<u32>
```

### 5.5 utf8_block.rs module + Hoehrmann DFA

`parse-that-regex/src/unicode/utf8_block.rs` exposes:

```rust
pub fn validate_block(input: &[u8; 16]) -> ValidateStatus { ... }
```

with a #[cfg]-gated NEON dispatch to `bbnf_simd::aarch64::utf8::validate_block`
on aarch64 and scalar Hoehrmann fallback elsewhere.

### 5.6 Exit gates

- Per-row checkasm parity on adversarial UTF-8 corpus (multi-byte
  codepoints, surrogate pairs, invalid byte sequences, all four
  CESU-8 anti-patterns).
- JSONTestSuite UTF-8 pack passes.
- twitter / random / unicode_mixed / unicode_basic parse rows
  all cross outcome-G boundary (or report names exact residual).
- Direct rows that were string-bound (gsoc, marine_ik, y_string_unicode,
  unicode_escapes) lift correspondingly.
- `samply` shows the UTF-8 kernel boundary no longer dominates;
  next hot leaf named for any remaining gap.
- Record c/B improvement on each row in REDRESS.md.

## 6. Wave 4 — Lock 14 Remediation + Working-Tree Nukes

### 6.1 Owner paths

- `skinny/crates/bbnf-simd/src/lib.rs` (716 LOC god-module — split)
- `skinny/crates/bbnf-simd/src/aarch64/*.rs` (move JSON specifics out)
- `skinny/crates/bbnf-simd/src/x86_64/avx2/classify.rs` + AVX-512 variants
  (remove hardcoded JSON punctuation)
- `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs` (move TBL4 LUT
  to codegen-emitted .data)
- `skinny/crates/simd-scan/` (entire directory — DELETE)
- `skinny/crates/runtime/src/grammars/json/generated_eventcursor.rs` (DELETE)
- `skinny/crates/runtime/src/grammars/json/mod.rs` (remove eventcursor cfg)
- `skinny/crates/runtime/src/grammars/json/parser.rs` (remove eventcursor cfg)
- `skinny/crates/runtime/Cargo.toml` (remove `eventcursor` feature)
- `skinny/crates/codegen/src/json_templates/*` (drop eventcursor templates)
- `skinny/crates/bbnf-simd/src/parse_index/` (remove `ParseIndexCursor` if unused after Wave 1)

### 6.2 Lock 14 split

`bbnf-simd/src/lib.rs` keeps:
- `StructuralAlphabet` (already grammar-neutral)
- `ScanBackend` enum (already grammar-neutral)
- `StructuralIndex` (already grammar-neutral)
- `select_classifier` (already generic)

Moves OUT to `runtime/grammars/json/`:
- `JSON_STRUCTURAL` constant
- `is_json_structural_alphabet` helper
- `is_json_punctuation`
- `scan_json_tail`
- `JsonParseIndex` (alias for `StructuralIndex` with the JSON alphabet)
- `resolve_json_string_masks_64` (or move to codegen-emitted body)
- The `mod neon` 230 LOC at lines 463-693 with hardcoded JSON
  punctuation `vceqq_u8` fan-in → break into a generic
  `classify_chunk_from_alphabet(chunk: uint8x16_t, alphabet:
  uint8x16_t) -> u16` primitive that takes the alphabet as a runtime
  parameter, then have the JSON-specific caller in
  `runtime/grammars/json/` pass the 7-char alphabet.

`bbnf-simd/src/aarch64/classify_tbl4.rs:65-71`: the `json_ascii_table`
const TABLE is grammar-specific .data. Move to
`codegen/src/grammars/json/tables.rs` as an emitted `.data` table that
the runtime loads at module init. The classifier itself stays generic.

The 4 `classify_block_scalar` functions in
`avx2/classify.rs:31`, `avx512_vbmi2/classify.rs:28`,
`avx512_gfni/classify_affine.rs:31`, `avx512_bitalg/multiclass.rs:30`
all hardcode `b'{' | b'}' | ...`. Replace with a generic
`classify_block_scalar(input: &[u8; 64], alphabet: &[u8; 8],
alphabet_len: usize) -> u64` parameterised on the alphabet.

### 6.3 Working-tree nukes

Execute the nuke plan from `restart/skinny/audit/NUKE-PLAN-SK-V5.md`:

1. Delete `skinny/crates/simd-scan/` entirely (fossil; not in workspace
   members; zero callers).
2. Delete `skinny/crates/runtime/src/grammars/json/generated_eventcursor.rs`.
3. Remove `#[cfg(feature = "eventcursor")]` branches from
   `runtime/src/grammars/json/mod.rs` and `parser.rs`.
4. Remove `eventcursor` from `runtime/Cargo.toml` features.
5. Remove `ParseIndexCursor` + `scan_parse_index` from `bbnf-simd` if
   verified unused (D5 confirmed it's currently only feature-gated
   under eventcursor).
6. Delete `wave2_bench.rs` example if present.
7. Audit `git rm` log for any remaining JSON-isms in generic crates per
   Wave 4 §6.2.

### 6.4 Exit gates

- `cargo build --workspace` succeeds.
- `cargo test --workspace` passes.
- `rg "JSON|json" skinny/crates/bbnf-simd/src/` returns hits ONLY in
  the (post-split) generic dispatch helpers, not in hot kernel bodies.
- `rg "JSON|json" skinny/crates/parse-that-regex/src/` returns hits
  ONLY in the renamed JSON-specific wrappers (which can stay or move
  to `runtime/grammars/json/`).
- `rg "json|JSON" skinny/crates/codegen/src/lower/` returns no hits
  (codegen lowerer is grammar-neutral).
- `rg "OpenFrame" skinny/crates/` returns 0 (verified in A4).
- Generic crates pass Lock 14 audit (manual grep + cohort verification).

## 7. Wave 5 — Consumed bbnf.asm Primitive Admission

### 7.1 Owner paths

- `skinny/crates/bbnf-simd/src/x86_64/<primitive>.asm` + `.rs` shim per
  primitive
- `skinny/crates/bbnf-simd/src/aarch64/<primitive>.rs` (NEON intrinsics)
- `skinny/crates/bbnf-simd/src/scalar/<primitive>.rs` (scalar reference)
- `skinny/crates/bbnf-simd/tests/checkasm_<primitive>.rs`

### 7.2 Admitted primitives

- `BYTE_CLASS_FROM_EQ_SET_64`: pre-existing consumer and checkasm
  coverage.
- `BYTE_CLASS_FROM_TABLE_64`: consumed by generic `scan_dispatch`.
- `BITMAP_PREFIX_XOR_64`: consumed by JSON string-region scan.
- `BITMAP_NEXT_SET_BIT`: consumed by `compact_mask`.
- `EOB_PAD_CLAMP`: consumed by JSON tail scan.

### 7.3 Blocked no-orphan primitives

`BULK_EMIT_COMPRESSED`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`, and
`FSM_DISPATCH_THREADED` are not Wave 5 close requirements. They remain
blocked until their structural-tape compressed sink, bracket-stack
CollapsedStage, or per-grammar `.asm` CollapsedStage consumers exist and
land in the same wave as the primitive body.

### 7.7 dav1d-style checkasm hardening

Per A2 §3 / §8.1, before any of the new primitives land:

- Rust candidate calls use verified stack canaries. Raw callee-saved
  register sentinels are reserved for future FFI/ASM `call_new` shims;
  applying them around Rust closures is a false-positive harness shape.
- Stack canary: XOR-fold 1 KiB pre/post; assert equality.
- Bench cycle counter: `__rdtsc` / `mach_absolute_time` instead of
  `Instant`.
- Lift `Xorshift64::fill` to `tests/checkasm_common.rs` (shared module)
  before per-primitive duplication compounds.

### 7.8 Runtime dispatch table

Per A2 §7.1, add `OnceLock<CpuFeatures>` populated via
`is_x86_feature_detected!` / `std::arch::is_aarch64_feature_detected!`
at first use. Kernel-table at `bbnf-simd/src/dispatch.rs` holds
fn-pointers per admitted primitive × ISA tier. Extend only to primitives
with hot consumers; do not add dispatch entries for no-orphan-blocked
bodies.

### 7.9 Exit gates

- Every admitted primitive has scalar reference + checkasm parity.
- Every admitted primitive has a generated/runtime hot-path consumer.
- `primitive-checkasm` passes for the admitted set.
- `REDRESS.md` records blocked orphan primitives and current `gate-json`
  status.
- No SOTA credit is claimed for blocked primitive bodies or for Wave 5
  itself while `gate-json` remains `N-direct / NoGo`.

## 8. Wave 6 — Strict Workload Matrix

Wave 6 entry gate: Wave 5 consumed-primitive admission is closed and
recorded in `skinny/REDRESS.md`; Wave 6 starts from the current
`N-direct / NoGo` baseline and does not require the four no-orphan-blocked
primitive bodies.

### 8.1 Owner paths

- `skinny/crates/bbnf-bench/`
- `skinny/RESULTS.md`
- `skinny/profile/`
- `restart/skinny/BENCH.md`

### 8.2 Workload matrix per row

For each of 17 corpora × 7 workloads:

- `parse_only`
- `parse_full_traversal`
- `path_lookup`
- `direct_to_struct`
- `unicode_string_float`
- `memory`
- `cycles_per_byte`

Every row records: Mbps, ns/iter, c/B (when accessible via PMU),
memory, arena counters, strictness plane, output plane, hot leaf
top-3.

### 8.3 Sidecar matrix

Sidecars: sonic-rs `Value` / typed direct; simdjson C++ DOM / On Demand;
yyjson inlined DOM; asmjson SWAR strict/permissive; RapidJSON default;
serde_json.

For each sidecar × corpus × workload, document API and output plane in
a separate column. The flaw-probe column flags JSONTestSuite divergences.

### 8.4 Exit gates

- No parse-G rows.
- No N-direct rows.
- All strictness columns populated.
- bbnf beats sonic-rs / simdjson / yyjson on M5 Max for all four
  workloads where applicable.
- `skinny/RESULTS.md` becomes the SK-V5 close-authority.

## 9. Wave 7 — x86 CollapsedStage Successor (Optional)

Out of scope for SK-V5 close. Per the V9.5 PSI excavation: only
hand-written NASM per (grammar × ISA) is admissible. Requires Zen 4
silicon access + declared NASM author + checkasm parity. If preconditions
fire:

- Admit `FSM_DISPATCH_THREADED` only after codegen emits its first
  per-grammar `.asm` CollapsedStage consumer; the body, scalar reference,
  checkasm parity, and consumer land together.
- Codegen emits `runtime/grammars/json/json_collapsed.asm` with
  generated `.data` tables (classifier LUT + state-transition LUT).
- Hand-author the `.asm` wrapper (~150 LOC per grammar × ISA pair).
- Compare strict bbnf rows vs strict asmjson rows on equivalent
  hardware.

Target: ≥1.20× asmjson 10.93 GiB/s on Zen 4 strict-vs-strict.

## 10. Wave Sequencing Rationale

The wave order prioritises measurable corpus moves:

- Wave 0 corrects honest reporting and unblocks profile attribution.
- Wave 1 builds the substrate that everything else consumes; zero
  user-visible delta but absolutely required.
- Wave 2 lands the number lever and removes the bench-private dishonesty;
  current measurements close `numbers` but leave Canada/mesh/marine direct
  residuals.
- Wave 3 removes duplicate UTF-8 validation and lifts string-bound rows;
  generated source hooks are admitted, while a no-allocation decoded-string
  visitor consumer is rejected by measurement. It does not close the parse-G
  or direct Unicode/string gates.
- Wave 4 nukes the Lock 14 / Lock 1 residue so generic crates pass
  audit (no JSON code in `bbnf-simd` / `parse-that-regex` /
  `codegen/lower`).
- Wave 5 admits only consumed grammar-neutral primitives and records
  no-orphan blocks for the remaining macro bodies.
- Wave 6 finalises the strict workload matrix.
- Wave 7 is the x86 successor and is optional.

The SK-V5 close condition has not fired while `skinny/RESULTS.md` remains
`N-direct / NoGo`. Wave 6 may proceed as the strict workload/reporting
wave from that baseline; Wave 7 remains optional x86 CollapsedStage work
gated by real `.asm` consumers.

## 11. Final Handoff

Final SK-V5 report lands at:

```text
restart/skinny/audit/HANDOFF-SK-V5.md
```

Required sections per SK-V4 §9 convention, plus:
- Strictness disclosure table (the Wave 0 addition).
- Nuke log (the Wave 4 deletions, with verification commands).
- Per-wave c/B and Mbps delta on each row.

No wave closes on "future phase will fix it." Every miss becomes a
named blocker, a rejected route with evidence in REDRESS.md, or the
next concrete wave input.
