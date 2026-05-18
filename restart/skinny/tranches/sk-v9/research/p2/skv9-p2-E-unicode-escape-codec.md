# SK-V9 P2-E: Unicode-escape codec primitive (`escape_codec_hex_unit`)

Pass: S-P2 Research. Cycle: V2.
Date: 2026-05-18.
Scope: Design the `escape_codec_hex_unit` cross-grammar primitive — scalar
reference + NEON SIMD body + same-wave consumer wiring + falsifiability
gate — for the four uncloseable parse-only LOSS rows that a delimiter-only
intervention cannot reach.
Output: this file.
P1 hot-leaf antecedents:
- `read_hex_unit_scalar` (P1-V3-B §3.2 / V3-C §2.1 y_string_unicode track 1
  rank 2 = 19.0%; unicode_escapes track 1 rank 1 = 23.7%).
- `hex_nibble` (V3-C y_string_unicode track 1 rank 1 = 19.2%; unicode_escapes
  rank 4 = 9.9%).
- Pair combined: 38.2% (y_string_unicode/t1), 43.9% (y_string_unicode/t2),
  33.6% (unicode_escapes/t1).
- Substrate-neutral class name: `escape_codec_hex_unit` per P1-V3-B §1.5 +
  §3.5; the Lock-14 reframe per `restart/locks/LOCKS.md`.
Lock surface: Lock 14 (substrate-neutral primitive vocabulary), Lock 16
(grammar-neutral primitive admissibility). No Lock 1 surface (the kernel
does not retain a sidecar or split the substrate union).

## §1 — Scalar implementation diagnosis (file:line)

### 1.1 The hot pair, end to end

The y_string_unicode bottleneck is two functions in `parse-that-regex/src/lib.rs`,
both inlined into the `unescape_string` materialiser:

- `read_hex_unit_scalar(hex: &[u8]) -> Option<u16>` at
  `crates/parse-that-regex/src/lib.rs:945-956`. Reads exactly four bytes,
  decodes each via `hex_nibble`, folds via shifted OR; rejects on the
  high-nibble-set test `(n0 | n1 | n2 | n3) & 0xf0 != 0`.
- `hex_nibble(byte: u8) -> u8` at
  `crates/parse-that-regex/src/lib.rs:958-966`. Match-ladder over
  `b'0'..=b'9'` → byte − b'0'; `b'a'..=b'f'` → byte − b'a' + 10;
  `b'A'..=b'F'` → byte − b'A' + 10; else 0xFF (poison).

Per-quartet cost is **four `hex_nibble` calls + one fold + one
high-nibble-set guard**. The compiler does inline the match ladder, but
the ladder is three sequential range-tests per byte (digit, lower,
upper). At four bytes that is up to 12 conditional jumps per `\uXXXX`,
each with two-way branch entropy that the predictor sees as essentially
random across a corpus of mixed hex.

### 1.2 The 4-quartet NEON wrapper (existing, partial)

The codepath at `crates/parse-that-regex/src/lib.rs:384-459`
(`unescape_four_unicode_escapes`) already opportunistically batches four
consecutive `\uXXXX` escapes through
`bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_x4_neon`
(`crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:125-166`). The kernel
exists; the existing NEON kernel decodes 16 bytes (= 4 quartets) in one
TBL pass. But the consumer trigger condition is unforgiving: the
function only fires when the next 24 bytes are *exactly four*
back-to-back `\uXXXX` quartets with no other bytes between. The Time
Profiler self-time table at P1-V3-B §2 `unicode_escapes/track1`
(read_hex_unit_scalar 23.7%, hex_nibble 9.9%) reads as "the scalar path
is what the corpus actually runs on" — the 4-quartet packer prefilter
does not engage on the mixed-escape patterns y_string_unicode and
unicode_mixed contain (mixed escapes, single quartets, surrogate
splits).

### 1.3 The surrogate-pair join — scalar and NEON

Scalar: `decode_unicode_escape` at `crates/parse-that-regex/src/lib.rs:302-344`
calls `read_hex_unit_scalar` for the first quartet, branches on
`is_high_surrogate` (`crates/parse-that-regex/src/lib.rs:968-971`), then
reads a second quartet and folds via
`0x10000 + (((first as u32 - 0xd800) << 10) | (second as u32 - 0xdc00))`.
The surrogate join is *intra-quartet ALU work*; it is not the hot leaf.

NEON: `join_surrogate_pair_neon` at
`crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:169-175` performs the
identical algebra on `u32` inputs. The name is misleading — the function
body is scalar shift-and-OR; no NEON intrinsics are touched. This is
correct (the per-pair algebra is a single ALU dependency chain that
does not vectorise), but the name should not be load-bearing for "NEON
the surrogate path."

### 1.4 The 16-byte block scanner (separate primitive)

The `unicode_mixed` and `unicode_escapes` rows ALSO load on the SIMD
full-scan path `match_string_at_quote_trusted_utf8` (P1-V3-C
unicode_mixed/t1 rank 3 = 15.2%, unicode_escapes/t1 rank 3 = 19.5%).
That path is the `per-string-span scanner` primitive class, NOT the
`escape_codec_hex_unit` class. Per CH1 / Lock-16: the two classes are
*orthogonal*; the per-string-span scanner finds the next special byte
(`b'"'`, `b'\\'`, control), and only at a `b'\\'` `b'u'` does the
escape codec engage. This artefact splits the unicode-row residual
budget between two primitives: the scanner already has a NEON body
(`scan_string_special_block` at
`crates/bbnf-simd/src/aarch64/string_block.rs`), the codec body is
what P2-E designs.

### 1.5 Per-quartet cost ceiling

Cost model (scalar reference, single quartet, branchless lower-bound):
- 4× load (1 byte each, can fuse to one `ldr w0, [x1]` = 1 µop on M-class).
- 4× `hex_nibble` evaluation: ~3 compares + 2 selects per byte = ~5 ops
  per byte = 20 ops per quartet.
- 1× high-nibble-set fold + branch = 5 ops.
- 1× shift-OR fold to u16 = 4 ops.
- Total: ~30 µops/quartet on the scalar reference, dominated by the
  per-byte range branches.

The xctrace 38.2% self-time on y_string_unicode/t1 (where the corpus
is 99%+ short `"\uXXXX"` strings of 6 bytes each) reads as the
match-ladder probability mass. A SIMD body that decodes a quartet in
one TBL + one shift-OR + one guard collapses the 30-µop scalar to
~6-8 µops per 4 bytes — a >4× front-end pressure reduction on the
hot pair.

## §2 — Cross-grammar parameterisation table

The primitive is a **codegen template**, not a runtime switch. Each
grammar instance binds the three free parameters at the codegen layer;
the emitted Rust call site instantiates one of N specialised kernels;
LTO inlines the specialisation into the consuming materialiser. There is
no runtime dispatch table.

| Param | Domain | JSON `\uHHHH` | CSS L4 `\HHHHHH` | JS `\u{HHHHHH}` | TOML `\uHHHH` | TOML `\UHHHHHHHH` |
|---|---|---|---|---|---|---|
| `hex_digit_count` | `Exact(N)` or `Range(lo, hi)` | `Exact(4)` | `Range(1,6)` | `Range(1,6)` | `Exact(4)` | `Exact(8)` |
| `surrogate_join_policy` | `None` / `Pair` / `RangeCheck` | `Pair` | `None` | `RangeCheck` | `None` | `None` |
| `terminator_policy` | `FixedWidth` / `Delimiter(byte)` / `WhitespaceOrNonHex` | `FixedWidth` | `WhitespaceOrNonHex` | `Delimiter(b'}')` | `FixedWidth` | `FixedWidth` |
| `target_encoding` (constant for all) | `Utf8` / `Utf16` / `Utf32` | `Utf8` | `Utf8` | `Utf8` | `Utf8` | `Utf8` |

### Codegen-emitted parameter binding (not a runtime switch)

Per `feedback_pluggable_components` + Lock 16, the codegen binds at
compile time. The bbnf grammar declares the escape rule:

```bbnf
unicode_escape  = "\\u" hex_quartet            -> escape_codec_hex_unit{4, Pair, FixedWidth, Utf8}
css_unicode     = "\\" hex_1_to_6 ws?          -> escape_codec_hex_unit{Range(1,6), None, WhitespaceOrNonHex, Utf8}
js_unicode      = "\\u{" hex_1_to_6 "}"        -> escape_codec_hex_unit{Range(1,6), RangeCheck, Delimiter(b'}'), Utf8}
toml_u4         = "\\u" hex_4                  -> escape_codec_hex_unit{4, None, FixedWidth, Utf8}
toml_u8         = "\\U" hex_8                  -> escape_codec_hex_unit{8, None, FixedWidth, Utf8}
```

The codegen emits one specialised Rust kernel per `escape_codec_hex_unit{…}`
binding tuple. Specialisation parameters are `const` generics on the
NEON intrinsic body. The dispatcher in
`runtime/src/grammars/<grammar>/generated.rs` calls the specific
binding by name; the inliner sees the constant parameters and prunes
every branch the binding has fixed at compile time. Concretely:

```rust
// JSON binding — codegen-emitted, all parameters const
#[inline(always)]
fn decode_json_u4(bytes: &[u8], slash: usize) -> Result<(char, usize), …> {
    escape_codec_hex_unit::<4, SurrogatePolicy::Pair, Terminator::FixedWidth>(bytes, slash)
}

// CSS L4 binding — codegen-emitted, range params via const struct
#[inline(always)]
fn decode_css_unicode(bytes: &[u8], slash: usize) -> Result<(char, usize), …> {
    escape_codec_hex_unit::<Range::new(1,6), SurrogatePolicy::None,
                            Terminator::WhitespaceOrNonHex>(bytes, slash)
}
```

A grammar that introduces a *new* parameter binding emits a fresh
specialisation; the kernel body is shared across grammars.

### 2.1 Why the JSON path is special: surrogate-pair join

JSON is the **only** target whose `surrogate_join_policy = Pair` branch
fires. The Pair branch reads a *second* quartet (and possibly a
`b'\\' b'u'` pair between them) and folds them via the
`0x10000 + ((high - 0xD800) << 10) | (low - 0xDC00)` algebra. CSS L4,
JS `\u{}`, TOML `\u`, TOML `\U` all produce a code-point directly
because their grammar-imposed digit width (≥21 bits for TOML `\U`, ≥17
bits for CSS L4 / JS `\u{}`) absorbs the full Unicode range without
splitting. The `Pair` parameter therefore guards a *control-flow gate*
in the kernel: under `SurrogatePolicy::None` or `RangeCheck`, the
gate is dead-code and constant-folds out.

### 2.2 Why the CSS L4 path is special: variable digit width

CSS L4's `\HHHHHH` consumes between 1 and 6 hex digits, terminating on
the first non-hex byte (and optionally consuming one trailing
whitespace per the CSS L4 unicode-escape rule). The kernel under
`Range(1,6)` cannot decode in a single fixed TBL pass; it loads 8 bytes
(a 7-byte hex slot + 1 byte terminator lookahead), runs the hex
classifier over 8 lanes, finds the first non-hex lane via CSSC CTZ on
the inverted mask, and then folds the variable-length nibble vector.
This case lands in the §3.3 8-byte parallel design.

### 2.3 Why the JS `\u{}` path is special: `}` terminator

JS's `\u{...}` does not have a fixed digit count; the digits run until
`b'}'`. The kernel under `Terminator::Delimiter(b'}')` scans for the
terminator first (8-byte CSSC CTZ on `eq_byte(b'}')`), computes the
digit count, and dispatches to the matching-width fold. The same
8-byte primitive body that powers the CSS L4 `Range(1,6)` case
handles this — the only delta is which byte stops the scan.

## §3 — SIMD design (NEON TBL + 4-byte parallel decode)

### 3.1 Layer-1 primitive shape

`bbnf-simd::primitives::escape_codec_hex_unit` is a Layer-1 bbnf
primitive (per `feedback_general_infra_crates` + `wasm-subcrate-pattern`).
It composes Layer-0 vendored substrate (TBL nibble decode, comparator
masks) into a grammar-neutral kernel. The primitive lives at
`crates/bbnf-simd/src/aarch64/escape_codec/` (directory module per
`feedback_directory_modules`) with siblings:

```
crates/bbnf-simd/src/aarch64/escape_codec/
├── mod.rs                    — kernel surface + const-generic entry
├── scalar.rs                 — scalar reference (parity oracle)
├── hex_x4_neon.rs            — fixed-4-digit NEON body (JSON, TOML \u)
├── hex_x8_neon.rs            — fixed-8-digit NEON body (TOML \U)
├── hex_variable_neon.rs      — variable-width NEON body (CSS L4, JS \u{})
└── surrogate_join.rs         — scalar pair-join (no NEON; algebra)
```

### 3.2 Fixed-4 quartet NEON body (JSON realisation)

The existing kernel at `crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`
is the antecedent body. P2-E refines it as the new
`hex_x4_neon::decode_quartet` with three changes:

1. **Single TBL with affine adjust**, not the existing two-table (low
   + range) shape. The Layer-0 lift uses a single `vqtbl1q_u8` over a
   16-entry LUT keyed by `byte & 0x0F` (the low nibble), and corrects
   for the ASCII collision (digits `1..6` collide with `A..F` /
   `a..f` low nibbles) via three branchless comparators
   `is_digit / is_upper / is_lower` followed by an `is_alpha *
   const(9)` correction. This is the published Lemire 2022
   three-ops-per-nibble shape (cited in the existing kernel
   docstring at `unescape_uxxxx.rs:13-16`). The refinement: replace
   the comparator mask + AND with a single TBL-and-mask via
   `vtstq_u8` over a precomputed character-class table, halving the
   front-end uop count on the alpha-detect lane.

2. **Branchless poison detection** via `vminvq_u8(is_hex)` followed by
   a single conditional branch on whether the min mask is zero. The
   existing kernel at lines 108-114 reads each lane independently
   (four `vgetq_lane_u8` operations) — that pattern stalls on the
   NEON-to-scalar transfer and is the load-bearing speed loss in the
   per-quartet path. Switch to the existing `unescape_uxxxx_x4_neon`
   approach (line 151 uses `vminvq_u8`) for the single-quartet path
   too — one branchless reduce + one branch.

3. **Bit-pack via `vpaddlq_u8` + `vpaddlq_u16`**, not via four
   `vgetq_lane_u8` extracts. The existing kernel reads each lane and
   shifts in scalar; replace with NEON pair-add reductions so the
   final 16-bit value materialises in one NEON-to-scalar move.

Per-quartet target cost (kernel body, excluding load):
- 1× `vqtbl1q_u8` (1 µop on M-class TBL port).
- 1× `vtstq_u8` against alpha class table (1 µop).
- 1× `vaddq_u8` for alpha adjust (1 µop).
- 1× `vminvq_u8` for the hex-validity reduction (1 µop, 4-cycle
  latency on M-class).
- 2× `vpaddlq` for the bit-pack (2 µops).
- 1× branch on min == 0 (predicted not-taken in steady state).

Total: ~6-7 NEON µops per quartet → ~5× reduction vs the ~30-op
scalar reference.

### 3.3 8-byte parallel body (CSS L4 / TOML `\U` / JS `\u{}`)

For grammars admitting ≥8 digit widths (CSS L4 up to 6 digits — fits in
8 lanes; TOML `\U` exactly 8 digits; JS `\u{}` up to 6 digits), the
kernel widens the load to 8 bytes (one `ldr d0, [x1]` = 8-byte NEON
half-vector load) and runs the TBL nibble decode over an
8-lane vector. Three sub-cases:

**Case A: `Terminator::FixedWidth` with N == 8 (TOML `\U`)**:
identical shape to §3.2 but on a vd8 vector; final fold via
`vpaddlq_u8` + `vpaddlq_u16` + `vpaddlq_u32` produces a 32-bit
codepoint in one `vmov.32` to scalar.

**Case B: `Range(1,6)` with `Terminator::WhitespaceOrNonHex` (CSS L4)**:
load 8 bytes (6 hex slots + 1 terminator lookahead + 1 slack), run
the hex-class TBL, compute the per-lane `is_hex` mask, find the
first-non-hex lane via `vclz` on the byte-reversed mask (or CSSC
CTZ where available — per P2-C arch-esoterica, the M5 Max has
CSSC; cost = 1 µop). Mask the nibble vector to the discovered
length, then pair-add fold. Codepoint and digit count both
materialise in one pass.

**Case C: `Range(1,6)` with `Terminator::Delimiter(b'}')` (JS
`\u{}`)**: identical to Case B but the terminator mask is computed
from `vceqq_u8(bytes, vdupq_n_u8(b'}'))` instead of from the
inverted hex mask. The non-hex check still runs as a parity
guard (a non-hex non-`}` byte is an invalid escape and must
fall back to the scalar error path).

### 3.4 Surrogate-pair join (NEON-free, kept algebraic)

JSON's `SurrogatePolicy::Pair` branch is the only path the §3.2 kernel
must produce a u16 codepoint for (not a u32). The scalar surrogate
join algebra
`0x10000 + ((high - 0xD800) << 10) | (low - 0xDC00)`
stays scalar (one shift, one subtract, one OR — three ALU ops; this
does not vectorise because it depends on a *pair* of consecutive
results, not a parallel batch). The branchless surrogate detection
uses range comparators against `0xD800..=0xDBFF` and
`0xDC00..=0xDFFF`, masked against the codepoint vector.

For the JSON 4-quartet batched path (which already exists at
`unescape_uxxxx_x4_neon` and consumes four `\uXXXX` quartets at
once), the surrogate-join classifier runs branchlessly over the
4-codepoint vector via two range comparators; under
`SurrogatePolicy::Pair`, lanes flagged as high-surrogates trigger a
scalar pair-join with the next lane. The scalar pair-join is a
short tail per high-surrogate pair (rare on average corpora, common
on y_string_unicode); since y_string_unicode is the *test* that
must pass the gate, the kernel admits a fast 4-quartet path
specifically for `[high, low, high, low]` patterns: a single
`vshlq_n_u32::<10>` + `vsubq_u32` + `vorrq_u32` pair-join over
2-lane pairs of the 4-codepoint vector.

### 3.5 Same-arch fallback discipline

Per `feedback_no_workarounds` + `inspect-generated-output`: the kernel
is `#[cfg(target_arch = "aarch64")]` gated, with the scalar reference
at `escape_codec/scalar.rs` standing as the cross-arch fallback. No
runtime feature detection; the codegen layer emits the NEON call site
only under `cfg(target_arch = "aarch64")` and the scalar path
otherwise. The compile-time const-generic parameters survive both
arms (the scalar path is also const-generic). checkasm parity (§7)
gates the NEON body against the scalar reference on every commit.

## §4 — Same-wave consumer plan

Per Lock-16 admissibility + the `same-wave-consumer rule` from P2-B and
CH4 of the S-P2 CHALLENGE wave, the primitive lands with a live
consumer in the same wave commit. The wave carries two distinct
consumer kinds, and per CH2 F5 the two must not be conflated:

- **Production consumer** — a code path on the live parse hot loop
  that the bench harness reaches. P2-E's production consumer is the
  JSON materialiser `unescape_string` at
  `parse-that-regex/src/lib.rs:718-810`, specifically the existing
  already-wired `unescape_four_unicode_escapes` x4 path at
  `parse-that-regex/src/lib.rs:402` (CH6-E-2: this call site is
  verified in-tree; it IS the production consumer, not a scaffold).
  The wave commit re-bodies this existing production path onto the
  `escape_codec_hex_unit` kernel; it does not introduce a new
  consumer.
- **Scaffold** — a compile-only `#[cfg(test)]` test in a `/tests/`
  directory that exercises a const-generic binding for parity /
  compile-validation, with no live parse-loop reach. P2-E's scaffold
  is the CSS L4 binding (§4.2) and the TOML `\u` / `\U` bindings
  (§4.4): both ship as compile-validated codegen output, neither
  enters a production parse loop in this wave.

P2-E therefore names **one production consumer** (the already-wired x4
JSON path) and **two scaffolds** (CSS L4, TOML). The Lock-14
generality demonstration (§4.2) is carried by the scaffolds; the
falsifiability gate (§6) is carried by the production consumer alone.

### 4.1 JSON consumer (the wave's primary production target)

`unescape_string` at `parse-that-regex/src/lib.rs:718-810` is the
production consumer. The wave commit replaces the existing `Some(b'u')`
arm (lines 775-786) with a call to the codegen-emitted
`escape_codec_hex_unit::<4, Pair, FixedWidth>` kernel. The arm
becomes:

```rust
Some(b'u') => {
    let slash = cursor - 1;
    let (ch, next) = bbnf_simd::escape_codec::decode_uxxxx_json(
        bytes, slash,
    )?;
    out.push(ch);
    cursor = next;
}
```

The existing 4-quartet batch path
(`unescape_four_unicode_escapes` at line 386) becomes the
4-quartet binding of the same primitive
(`escape_codec_hex_unit_batch::<4, Pair, FixedWidth, 4>`), with
the same surrogate-pair classifier above. Both call sites collapse
to the same const-generic kernel; the dispatcher picks the
4-quartet vs single-quartet binding based on the lookahead
test (the existing condition: four `\u` openers in 24 bytes).

`decode_unicode_escape` at line 302 also calls the
`escape_codec_hex_unit::<4, Pair, FixedWidth>` kernel; that call
site is on the validate path (not the materialise hot path) but
shares the same body for parity.

Per-row impact (rederived from the PMU TSV + V3-C §3 per-class c/B
table — see §6.2 for the full arithmetic):
- **y_string_unicode/t1**: escape-codec class = 2.312 c/B of the
  5.710 row total (40.5%); a 75% codec reduction lifts parse_only
  Mbps from the TSV baseline 5,457 to a projected ~7,837 — a
  NEAR-FAIL at 94.8% of the 0.70 W4-precedent gate.
- **unicode_escapes/t1**: escape-codec class = 1.088 c/B of the
  3.007 row total (36.2%); a 75% codec reduction lifts parse_only
  Mbps from the TSV baseline 11,239 to a projected ~15,423 — a
  NEAR-FAIL at 94.5% of the 0.90 standard gate (the V1 PASS does
  not survive the PMU rederivation).
- **unicode_mixed/t1**: the codec does not surface as a separate
  V3-C `esc-hex` leaf; the `\u`-decode work folds into
  `validate_string_escape` (V3-C §2.1 rank 2 = 20.1%). The
  codec-attributable share is ~10% of the 4.634 row c/B; the kernel
  lifts the row from 7,276 to ~7,864 Mbps — a FAIL at 63.7% of the
  0.85 gate. Not a parity intervention on its own; must compose
  with a same-row scanner knob to close (see §6).
- **gsoc-2018/t1**: the codec is not a top-8 leaf; the row loads on
  `movemask_u8x16` (30.9%) — the per-string-span scanner. The codec
  primitive does not move this row (codec share ≈0%); admission is
  on the no-regression basis only.

### 4.2 CSS L4 consumer sketch (Lock-14 same-wave generality demonstration)

The CSS L4 parser is at `skinny/crates/bbnf-css/` (currently mid-wave
per project memory `css-typed-codegen`). The CSS L4 unicode escape
rule per the CSS Syntax Level 4 spec:

```
unicode_escape = "\" h{1,6} ws?
                 where h is [0-9 a-f A-F]
                 ws is one space/tab/newline that is consumed but not
                    part of the code point
```

The consumer call site is the CSS tokeniser's `consume_escaped`
function (in `bbnf-css/src/tokenizer/escape.rs` per the existing
module layout). The wave commit binds:

```rust
// Codegen-emitted from the CSS L4 grammar's unicode_escape rule
fn decode_css_unicode(bytes: &[u8], slash: usize)
    -> Result<(char, usize), CssError>
{
    bbnf_simd::escape_codec::decode_variable::<
        Range::new(1, 6),
        SurrogatePolicy::None,
        Terminator::WhitespaceOrNonHex,
    >(bytes, slash)
}
```

The sketch establishes Lock-14 generality without requiring a
second performance gate — the CSS L4 binding ships the codegen
template + scalar reference + a unit test in the same wave; the
CSS L4 SIMD body lands when CSS-side benches demand it.

Per CH2 F5 (E.4), the CSS L4 consumer ships explicitly as a
**scaffold**, not a production consumer. The SK-V9 wave lands a
`#[cfg(test)]`-gated test in `bbnf-css/tests/` demonstrating the
const-generic binding compiles and passes a unit test against the
scalar reference; the CSS L4 production tokeniser
(`bbnf-css/src/tokenizer/escape.rs`) wires the kernel into the live
`consume_escaped` path in a later CSS-side wave, once the CSS L4
`.bbnf` source authors the `unicode_escape` rule with
`→ escape_codec_hex_unit{Range(1,6), None, WhitespaceOrNonHex, Utf8}`.
This is the minimum same-wave demonstration needed to refute the
"JSON-overfit" CH2 GENERALITY charge: a second grammar's codegen
output explicitly emits a call to the same kernel under a different
parameter binding, parity-checked at compile time. The scaffold does
not carry a falsifiability gate (§6) — only the JSON production
consumer does.

### 4.3 What the consumer plan does NOT add

- No retained sidecar over `\u` positions (would violate Lock 1 per
  CH5 of S-P2 CHALLENGE).
- No second escape-classifier scan over the string content (the
  per-string-span scanner already locates the `b'\\'` byte; the
  codec engages off the existing scanner's hit).
- No JSON-specific allocator strategy in the codec path; the
  `Cow<str>` heuristic in `unescape_string` line 719 is unchanged
  (and remains JSON policy in the JSON consumer, not in the
  primitive crate).

### 4.4 TOML `\u` / `\U` binding disposition (no production consumer this wave)

The §2 parameter table enumerates two TOML bindings —
`toml_u4 → escape_codec_hex_unit{4, None, FixedWidth, Utf8}` and
`toml_u8 → escape_codec_hex_unit{8, None, FixedWidth, Utf8}` — to
exercise the const-generic surface (fixed-4 and fixed-8 digit widths,
`SurrogatePolicy::None`). Per CH4 F4 (E.4): **TOML `\u` and `\U`
variants have no production consumer in this wave.** There is no TOML
grammar admitted as an SK-V9 target; `bbnf` ships no TOML parser hot
path the bindings could wire into.

Disposition: the TOML bindings ship as **compile-time validation
only** — the codegen template emits the `hex_x4_neon` (TOML `\u`,
shared with JSON) and `hex_x8_neon` (TOML `\U`) specialisations, and
the checkasm parity test (§7) covers both against the scalar
reference, but neither binding enters a production parse loop. The
const-generic emission is dead unless a TOML grammar source is
loaded, so this is not an orphan kernel cost: `hex_x4_neon` is
already live via the JSON consumer, and `hex_x8_neon` adds ~140 LOC
of compile-validated-but-unwired body whose only same-wave consumer
is the checkasm gate. The TOML production consumer wires after Pass
Omega admits TOML as a target grammar; until then the binding is a
parity-green, grammar-generic primitive available to that future
grammar.

## §5 — REDRESS 82 differential

REDRESS 82 (`skinny/REDRESS.md:2285-2316`) rejected the SK-V7 W4
single-quartet unicode-escape classifier. Material differential to
P2-E (per CH3 REGRESSION):

| Axis | REDRESS 82 (W4 single-quartet classifier) | P2-E `escape_codec_hex_unit` |
|---|---|---|
| **Primitive shape** | A *parser-owned, per-quartet classifier* that consumed one quartet at a time, wrapping `unescape_uxxxx_neon` in `parse-that-regex/src/unicode/escape_decode.rs`. The kernel decoded one quartet; the parser owned the per-quartet validation loop. | A *full hex-decoder primitive class* with three free parameters (digit count, surrogate policy, terminator policy). The decoder owns the full decode (TBL → fold → guard → surrogate join); the parser calls it once per escape. Single-quartet and 4-quartet and 8-digit bindings collapse to the same const-generic body. Primitive class (full hex-decoder), NOT classifier (single-quartet). |
| **Hot path entered** | The *dispatch hot path*: each `\u` triggered a primitive call from the materialiser, with no batching. | The *unescape materialiser hot path*: 4-quartet bindings remain available for the y_string_unicode-dense case; single-quartet binding only fires when the 4-quartet pre-filter rejects. |
| **Same-wave consumer** | Only the JSON materialiser. No CSS / TOML / JS consumer. | One production consumer (the already-wired x4 JSON path at `lib.rs:402`) + two scaffolds (CSS L4 + TOML, compile-validated `#[cfg(test)]` codegen output). Lock-14 grammar-neutrality demonstrated by scaffold inspection. |
| **Surrogate policy** | Implicit Pair (JSON-only). | Explicit `SurrogatePolicy` const-generic parameter; Pair / None / RangeCheck. CSS / TOML / JS bindings dead-code-eliminate the join branch. |
| **Terminator policy** | Implicit FixedWidth (JSON-only). | Explicit `Terminator` const-generic; FixedWidth / Delimiter(byte) / WhitespaceOrNonHex. CSS / JS bindings emit a different inner loop. |
| **Per-quartet cost** | Kernel call overhead (per-quartet function boundary + validation wrapper) competed with the body savings — net 0 on the dense rows and a regression on the sparse rows. | Const-generic specialisation: 4-quartet binding amortises kernel-boundary cost over 4 quartets; single-quartet binding inlines to the same ~7-µop body the wrapper added overhead onto. |
| **Falsifiability gate** | Crossed only `unicode_escapes/parse_only`; failed `y_string_unicode/parse_only` at 49.9% of sonic and direct rows below threshold. | §6: gates are set against the four uncloseable rows P1-V3-D §5.3 named; gate is `parse_only` only (the direct plane lives behind REDRESS 66-69 + 93 per V3-D §6.4). |
| **Material new evidence** | None — operated on the SK-V6 profile. | P1-V3-B + P1-V3-C xctrace Time Profiler self-time table at 38.2% / 43.9% on y_string_unicode is post-V3 evidence; the V3-D §5.3 OLS analysis (R²=0.371) names this row as *uncloseable* by a delimiter-only intervention, which W4's wrapper was. |

The differential is on five orthogonal axes:
1. **Shape**: primitive class (with parameter freedom), not classifier
   wrapper. REDRESS 82 was a *parser-owned, per-quartet classifier* —
   it consumed one `\uXXXX` quartet at a time and the parser owned the
   per-quartet validation loop. P2-E's `escape_codec_hex_unit` is a
   **primitive class — a full hex-decoder**, not a single-quartet
   classifier: one kernel that loads the digit run, decodes via TBL,
   folds, range-guards, and (under `SurrogatePolicy::Pair`) joins a
   second quartet, all inside the primitive boundary. The parser owns
   no per-quartet loop; it calls the decoder once per escape and
   receives `(char, next_cursor)`. The distinction is "primitive class
   (full hex-decoder), NOT classifier (single-quartet)".
2. **Surface**: full hex-decoder (TBL + class + fold + guard + optional
   surrogate join), not a per-quartet classifier wrapper that the
   parser drives.
3. **Genericity**: const-generic codegen template, not JSON
   instantiation.
4. **Consumer cardinality**: one production consumer (the already-wired
   x4 JSON path, §4.1) plus two scaffolds (CSS L4 + TOML, §4.2 / §4.4)
   same-wave — not REDRESS 82's single parser-owned classifier.
5. **Evidence**: P1-V3 xctrace Time Profiler (not the SK-V6 samply
   coalesced view that the W4 attempt was sized against).

Per CH3 D-3 (V3-D §6.2): "REDRESS 82 rejected the four-`\uXXXX` AArch64
classifier on exactly these rows. Any successor intervention must
articulate the differential against each cited entry on a same-row
falsification gate." This section satisfies that requirement.

## §6 — Falsifiability gate (per-row Mbps thresholds)

### 6.1 Baseline — rederived from `/tmp/skv9-xctrace-v3/pmu_rows.tsv` (F2)

The V1 §6.1 c/B baseline column (`0.354 / 0.628 / 0.787 / 0.193`) was
rejected under CH6-E-3: those numbers do not reconcile to any column
of the PMU TSV. The V2 baseline below is rederived **directly from
`/tmp/skv9-xctrace-v3/pmu_rows.tsv`**, citing the `cycles_per_byte`
and `ns_per_byte` columns verbatim. Mbps follows the TSV's own
megabit convention `Mbps = 8000 / ns_per_byte` (verified against the
TSV `mbps` column: unicode_escapes/t1 `8000 / 0.711821 = 11,238.8`,
matching the TSV row to one place).

The four uncloseable rows, TSV columns verbatim (track 1 unless noted):

| Corpus / track | TSV `cycles_per_byte` | TSV `ns_per_byte` | TSV `mbps` | bytes | implied host clock (c/B ÷ ns/B) |
|---|---:|---:|---:|---:|---:|
| unicode_escapes / t1 | 3.006864 | 0.711821 | 11,238.780 | 1,050,797 | 4.224 GHz |
| unicode_mixed / t1 | 4.633713 | 1.099530 | 7,275.839 | 1,053,086 | 4.214 GHz |
| y_string_unicode / t1 | 5.709799 | 1.465919 | 5,457.328 | 35,601 | 3.895 GHz |
| gsoc-2018 / t1 | 1.543720 | 0.369581 | 21,646.136 | 3,327,831 | 4.177 GHz |
| gsoc-2018 / t2 | 1.605891 | 0.390459 | 20,488.699 | 3,327,831 | 4.114 GHz |

The implied host clock is the lossless `cycles_per_byte ÷ ns_per_byte`
ratio per row (~4.0–4.2 GHz Apple M5 Max P-core; the spread is
xctrace measurement noise across runs). Each row's projection below
uses that row's exact ratio so the c/B → ns/B → Mbps inversion is
lossless and self-consistent with the TSV.

sonic-rs strict comparators (from `skinny/RESULTS.md`, the
falsifiability targets): unicode_escapes 18,132 Mbps; unicode_mixed
14,515 Mbps; y_string_unicode 11,814 Mbps; gsoc-2018 45,318 Mbps.

### 6.2 Codec c/B share + projected Mbps under the §3 kernel (F2 rederivation)

The escape codec's share of each row is taken from the **P1-V3-C §3
per-class cycle-accounting table** (the `esc-hex` column, which splits
each row's TSV `cycles_per_byte` across primitive classes), cross-read
against the P1-V3-B §3.4 / §3.5 per-symbol self-time data. Cited
verbatim:

- **y_string_unicode / t1**: V3-C §3 escape-codec class = **2.312 c/B**
  of the 5.710 row total = **40.5%** of the row. V3-B §3.2 / §3.5
  corroborates: `hex_nibble` 19.2% + `read_hex_unit_scalar` 19.0% =
  **38.2%** self-time on the codec pair (the c/B share and the
  self-time share agree inside xctrace noise).
- **unicode_escapes / t1**: V3-C §3 escape-codec class = **1.088 c/B**
  of the 3.007 row total = **36.2%** of the row. V3-B §3.5 / V3-C §2.1
  corroborates: `read_hex_unit_scalar` 23.7% + `hex_nibble` 9.9% =
  **33.6%** self-time.
- **unicode_mixed / t1**: V3-C §3 escape-codec `esc-hex` column = 0.000
  c/B — the codec work on this row does **not** surface as a separate
  `esc-hex` leaf; it folds into `validate_string_escape` (V3-C §2.1
  rank 2 = 20.1% self-time, `parse-that-regex/src/lib.rs:285`). The
  `\u`-decode reachable fraction of that validator is conservatively
  ~half (the validator also handles `\n \t \"` etc. single-byte
  escapes), giving a codec-attributable share of **~10%** of the
  4.634 row c/B = **0.463 c/B**.
- **gsoc-2018 / t1 + t2**: V3-C §3 escape-codec `esc-hex` column =
  0.000 c/B; the codec is not a top-8 leaf. The row's c/B is dominated
  by `simd_movemask` (V3-B §3.2: 30.9% on `movemask_u8x16`) — the
  per-string-span scanner, a different primitive class. Codec share
  ≈ **0%**.

The SIMD codec collapses the scalar ~28 µops/quartet (§1.4 / §3) to
~6-7 µops/quartet — a **~75% reduction at the codec class**. The
projection therefore sets `SIMD codec c/B = scalar codec c/B × 0.25`
and `codec savings = scalar codec c/B × 0.75`; the kernel-external
work (string scanner, dispatch, validator's non-`\u` arms) is
unchanged.

| Corpus | scalar codec c/B (V3-C §3) | SIMD codec c/B (×0.25) | codec savings (×0.75) | row c/B new | host clock | ns/B new | Mbps new |
|---|---:|---:|---:|---:|---:|---:|---:|
| unicode_escapes / t1 | 1.088 | 0.272 | 0.816 | 3.007 − 0.816 = **2.191** | 4.224 GHz | 2.191 ÷ 4.224 = **0.5187** | 8000 ÷ 0.5187 = **15,423** |
| y_string_unicode / t1 | 2.312 | 0.578 | 1.734 | 5.710 − 1.734 = **3.976** | 3.895 GHz | 3.976 ÷ 3.895 = **1.0208** | 8000 ÷ 1.0208 = **7,837** |
| unicode_mixed / t1 | 0.463 | 0.116 | 0.347 | 4.634 − 0.347 = **4.287** | 4.214 GHz | 4.287 ÷ 4.214 = **1.0173** | 8000 ÷ 1.0173 = **7,864** |
| gsoc-2018 / t1 | ~0.000 | ~0.000 | ~0.000 | 1.544 (unchanged) | 4.177 GHz | 0.3696 (unchanged) | **21,646** (unchanged) |

Per-row falsifiability verdict (the §6.4 admission rule — 70% slack
for structurally-hard rows per W4 precedent — applied below):

| Corpus | Mbps new | sonic-strict | threshold | gate verdict |
|---|---:|---:|---:|---|
| unicode_escapes | 15,423 | 18,132 | × 0.90 = 16,319 | **NEAR-FAIL** at 94.5% of threshold (rederivation drops it below the V1 100.5% PASS) |
| y_string_unicode | 7,837 | 11,814 | × 0.70 = 8,270 | **NEAR-FAIL** at 94.8% of the 70%-slack threshold |
| unicode_mixed | 7,864 | 14,515 | × 0.85 = 12,338 | **FAIL** at 63.7% of threshold |
| gsoc-2018 | 21,646 | 45,318 | × 0.50 = 22,659 | **FAIL (no-regression basis)** at 95.5% of the 50% slack threshold; admitted on the no-regression rule (§6.3) — Mbps unchanged vs 21,646 baseline, codec neutral |

### 6.3 Per-row admission thresholds + the 70% slack rule

**The slack rule (stated before the projection, per CH6-E-4).** A row
admits at one of two slack levels against the sonic-rs strict
comparator:

- **Standard slack — 0.90 of sonic-strict.** The default falsifiability
  gate for any row whose dominant cycle sink is the primitive under
  design.
- **W4-precedent slack — 0.70 of sonic-strict.** Applied **only** to a
  row that is *structurally hard* by a primary-source criterion: the
  row's corpus shape forces the maximum ratio of codec work to
  kernel-external work, so even an ideal codec leaves the row
  codec-bound. The precedent is the SK-V7 W4 REDRESS 82 gate, which
  used 0.70 on `y_string_unicode/parse_only` for exactly this reason.
  Only `y_string_unicode` qualifies — V3-C §3 shows it at 40.5% codec
  c/B, the highest single-class share in the 17-corpus table, on a
  corpus that is 99%+ short 6-byte `\uXXXX` strings.
- **No-regression basis.** A row whose codec share is ≈0% (the codec
  is not its bottleneck) is not gated against a sonic-relative
  threshold at all; it admits iff `Mbps_new ≥ Mbps_baseline − 1%`.

| Corpus | sonic-strict | slack basis | Mbps target | rationale |
|---|---:|---|---:|---|
| **unicode_escapes** | 18,132 | × 0.90 standard | **≥ 16,319 Mbps** | Codec is the dominant single class (36.2% c/B); standard parity gate applies. |
| **y_string_unicode** | 11,814 | × 0.70 W4-precedent | **≥ 8,270 Mbps** | Structurally hard by the V3-C §3 primary-source criterion (40.5% codec c/B, highest in the table; 99% short-string corpus). The 0.70 slack is fixed by the W4 precedent *before* §6.2 — it is not retrofitted to admit a near-miss. |
| **unicode_mixed** | 14,515 | × 0.85 standard | **≥ 12,338 Mbps** | Codec share is only ~10% c/B (folded into `validate_string_escape`); the row's cycle budget is split across the scanner + validator + dispatch. The codec intervention alone does NOT close this row (§6.2: 63.7%). The row admission is conditional on a same-wave per-string-span scanner intervention. |
| **gsoc-2018** | 45,318 | no-regression basis | **≥ 21,430 Mbps** (= 21,646 − 1%) | Codec share ≈0%; the row's load is `movemask_u8x16` (V3-B §3.2: 30.9%), a different primitive class. The codec is admitted as *not regressing* this row; closing it is out of scope and routes to a scanner-side primitive. |

### 6.4 Honest verdict (per `feedback_accurate_perf_narrative`)

The F2 rederivation from the actual PMU TSV materially **downgrades
the V1 verdicts.** The V1 §6.2 table — built on the fabricated
`0.354 / 0.628 / 0.787 / 0.193` c/B column CH6-E-3 rejected — claimed
unicode_escapes PASS at 100.5%, y_string_unicode NEAR-FAIL at 94.5%,
unicode_mixed FAIL at 68.7%, gsoc-2018 NEAR-FAIL at 98%. The rederived
table is harsher:

- **unicode_escapes — NEAR-FAIL at 94.5%** of the 0.90 threshold
  (15,423 vs the 16,319 gate). The V1 PASS does not survive
  rederivation: the codec is 36.2% of the row, and a 75% codec
  reduction lifts the row to 15,423 Mbps — short of the standard
  parity gate by ~900 Mbps. The codec is a strong contributor but
  does **not** close this row on its own.
- **y_string_unicode — NEAR-FAIL at 94.8%** of the 0.70 W4-precedent
  threshold (7,837 vs the 8,270 gate). The codec class is 40.5% of
  the row's c/B; collapsing it 75% yields 7,837 Mbps — still ~430
  Mbps below the structurally-hard gate. The ~75% reduction is the
  expected best case under NEON µop counting; a measured 70% or 80%
  puts the row at ~90%-100% of threshold.
- **unicode_mixed — FAIL at 63.7%** of the 0.85 threshold (7,864 vs
  the 12,338 gate). The codec touches only ~10% of this row's c/B;
  the intervention alone cannot close it. The row needs the codec
  paired with a same-wave per-string-span scanner intervention.
- **gsoc-2018 — admitted on the no-regression basis.** Codec share
  ≈0%; Mbps unchanged at 21,646, clearing the `baseline − 1%` gate.
  The row's bottleneck is the movemask scanner, a different primitive
  class; closing it is out of scope for the codec.

Rederived posture: **zero of the four rows admit on the codec alone**
at the standard / W4-precedent slack. unicode_escapes and
y_string_unicode both NEAR-FAIL (94.5% / 94.8%) — the codec is a
strong contributor that approaches but does not reliably cross either
gate; admission is a **same-wave conditional rule**: the codec admits
iff the measured post-wave Mbps clears the gate, with the projection
flagged as the expected-best-case bound (CH6-E-4 conditional-admission
rule). unicode_mixed FAILs and needs a paired scanner knob. gsoc-2018
admits only as not-regressing. This is the honest falsification
posture P2-E carries into S-P3 — materially more conservative than the
V1 fabricated-PMU table.

## §7 — LOC + risk + checkasm parity

### 7.1 Per-slice LOC + minute cap + revert + same-wave consumer (F4)

Per CH4 F4, every slice carries an explicit per-slice LOC, minute cap,
one-sentence revert protocol, and named same-wave consumer. The wave
decomposes into **six implementation slices** — five const-generic
kernel bodies (one of which is the scalar reference, parity oracle for
all the rest) plus the checkasm differential gate — followed by the
consumer-wiring + codegen + cleanup slices.

| # | Slice | LOC | Minute cap | Revert protocol | Same-wave consumer |
|---|---|---:|---:|---|---|
| S1 | `escape_codec/scalar.rs` — scalar reference (parity oracle for S2-S5) | ~120 | 30 min | Self-contained new file; revert the file on failure. | S6 checkasm gate (the oracle every NEON body is diffed against). |
| S2 | `escape_codec/hex_x4_neon.rs` — fixed-4 NEON body (JSON `\u`, TOML `\u`) | ~150 | 35 min | New file; if checkasm S6 fails parity, revert S2 and the JSON consumer falls back to S1 scalar. | S7 JSON production consumer (`unescape_string` x4 path). |
| S3 | `escape_codec/hex_x8_neon.rs` — fixed-8 NEON body (TOML `\U`) | ~140 | 30 min | New file; revert on parity failure — no production consumer depends on it (TOML is scaffold-only per §4.4). | S6 checkasm gate only (TOML is compile-validation-only this wave, §4.4). |
| S4 | `escape_codec/hex_variable_neon.rs` — variable-width NEON body (CSS L4, JS `\u{}`) | ~180 | 40 min | New file; revert on parity failure — CSS L4 / JS are scaffold-only. | S6 checkasm gate + S9 CSS L4 scaffold. |
| S5 | `escape_codec/surrogate_join.rs` — scalar pair-join algebra | ~50 | 15 min | New file; revert on failure — S2 falls back to the §3.4 scalar join inline. | S7 JSON production consumer (Pair binding). |
| S6 | `bbnf-simd/tests/checkasm_escape_codec.rs` — per-binding parity gate | ~250 | 40 min | New test file; lands BEFORE any consumer wiring (CH6-E-1 prerequisite). Revert blocks the wave — no consumer slice proceeds until S6 is green. | S1-S5 (the test IS the consumer for the kernel bodies). |
| S7 | `parse-that-regex/src/lib.rs` — re-body the existing x4 + `Some(b'u')` arm onto the kernel | ~30 | 25 min | Edit to existing file; revert the diff and the existing scalar + `unescape_uxxxx` path is restored intact. | Production parse loop (the bench harness reaches it). |
| S8 | `runtime/src/grammars/json/sink.rs` — call-site swap | ~10 | 10 min | Trivial diff; revert restores the prior call site. | Production JSON sink. |
| S9 | `bbnf-css/tests/` — CSS L4 scaffold (`#[cfg(test)]` binding + parity unit test) | ~40 | 20 min | New `#[cfg(test)]` file; revert removes the scaffold, no production path affected. | Scaffold (compile-validation only, §4.2). |
| S10 | `codegen/src/escape_codec/` template module — const-generic emission for the five bindings | ~120 | 30 min | New sub-module (directory module per `feedback_directory_modules`); revert removes the emission, hand-written S2-S5 bodies remain callable. | S7 + S8 (JSON) + S9 (CSS L4). |
| S11 | Existing kernel removal at `unescape_uxxxx.rs` (superseded by S2) | −215 | 15 min | Deletion slice; revert restores the file. Lands LAST, only after S7 is green. | Self (the removal is the consumer migration). |
| `escape_codec/mod.rs` | const-generic kernel surface + dispatcher (lands with S1) | ~80 | folded into S1 cap | — | — |

- **Net new LOC (excluding tests)**: ~890 hand-written + ~120 regen = **~1,010**.
- **Net new LOC (including tests + checkasm)**: **~1,260**.
- **Net deletion**: **−215** (the superseded `unescape_uxxxx.rs` kernel + W4-attempt residue).
- **Net of deletion**: **~1,045**.
- **Total minute budget**: ~6.0 h across the eleven slices; the wave hard cap rolls these per-slice caps into the S-P3-authored wave manifest. P2-E does not author the wave sequence — it supplies the per-slice cost set.

### 7.2 Risk envelope

| Axis | Risk level | Mitigation |
|---|---|---|
| **Correctness — single quartet** | LOW | scalar reference + checkasm parity at every commit; UTF-16 surrogate pair-join is bit-identical algebra. |
| **Correctness — variable digit (CSS L4 / JS)** | MEDIUM | new code path not present today; checkasm parity covers all 1..6 widths × valid + invalid hex × terminator positions. |
| **Performance — y_string_unicode** | MEDIUM-HIGH | §6.2 (PMU-rederived) projects 7,837 Mbps — NEAR-FAIL at 94.8% of the 0.70 W4-precedent threshold. Real µop count could fall short. Mitigation: P2-E names the row at 0.70 (not 0.90) per the W4 precedent + the V3-C §3 structural-hardness criterion (40.5% codec c/B, fixed before the projection). The ~75% codec reduction is the kernel-body best case; the row's kernel-external time (dispatch, scanner) is unchanged, so a smaller measured reduction still moves the row materially. Admission is the §6.4 same-wave conditional rule. |
| **Performance — unicode_escapes** | MEDIUM-HIGH | §6.2 (PMU-rederived) projects 15,423 Mbps — NEAR-FAIL at 94.5% of the 0.90 standard threshold. The V1 PASS at 100.5% was an artefact of the rejected fabricated c/B column; the row does not close on the codec alone. Mitigation: same-wave conditional admission — admits iff measured Mbps clears 16,319. |
| **Performance — unicode_mixed** | HIGH | row does not close on the codec alone (§6.2 rederived: 63.7%). Mitigation: explicit conditional admission tied to a same-wave per-string-span scanner intervention. If no scanner intervention lands the same wave, the row stays NO-GO and the wave admits codec-contribution-only on this row. |
| **Performance — gsoc-2018** | LOW (out of scope) | row admission is no-regression, not parity-cross. |
| **Maintenance — const-generic explosion** | MEDIUM | five bindings (JSON `\u`, TOML `\u`, TOML `\U`, CSS L4 `\HHHHHH`, JS `\u{}`) emit five specialisations. Code-size growth is bounded by the binding count (5 × ~250 LOC body ≈ 1.3KB on hot path); LTO inliner should fold each call site once. Mitigation: codegen template is a single file; per-binding bodies share Layer-0 substrate. |
| **Lock 1 (substrate union)** | NONE | the codec is a transient producer over a borrowed view; no retained sidecar, no second source scan, no parallel substrate. |
| **Lock 14 (substrate-neutral primitive)** | NONE | the primitive class is named, parameterised, and grammar-neutral; the JSON realisation is one binding among five demonstrated. |
| **Lock 16 (admissibility)** | NONE | the primitive admits across JSON, CSS L4, JS, TOML; per the §2 parameter table. |
| **REDRESS 82 (W4 single-quartet)** | LOW | §5 enumerates five-axis material differential. |

### 7.3 checkasm parity discipline

Per `feedback_inspect_generated` + the P2-B dav1d process: every NEON
body has a checkasm-differential test against the scalar reference,
running on every commit, before any consumer wiring lands. The
discipline matches the existing `checkasm_parity.rs` shape (line
643-663 today tests the 4-quartet path):

```rust
#[test]
fn escape_codec_parity_aarch64() {
    // For each binding (5 const-generic specialisations):
    //   For each digit count (1..=8, where the binding allows):
    //     For each alignment (0..16):
    //       For each hex pattern (valid: BMP, surrogate-high, surrogate-low,
    //                             max-codepoint; invalid: non-hex, partial):
    //         For each terminator config:
    //           assert_eq!(scalar(input), neon(input));
}
```

Coverage targets:
- ~6,000 cases per binding (the existing checkasm `unescape_uxxxx`
  test covers ~800 cases; the expansion is proportional to the new
  parameter freedom).
- All five bindings exhaustively tested for digit-count × alignment ×
  terminator pattern × validity.
- The surrogate-pair join (JSON-only) tested over the full
  high+low cross product on the 4-quartet batched path.

The parity gate runs as part of `cargo test -p bbnf-simd --release
--test checkasm_escape_codec`; failure blocks the wave commit. Per
P2-B, this is the same-wave admission gate the SOTA process
requires.

### 7.4 Total wave envelope

| Axis | Value |
|---|---|
| LOC (new + tests) | ~1,260 |
| LOC (net of deletion) | ~1,045 |
| Slices | 11 (S1-S11, §7.1) — per-slice minute caps total ~6.0 h |
| Bindings | 5 const-generic specialisations |
| Consumers same-wave | 1 production (already-wired x4 JSON path) + 2 scaffolds (CSS L4, TOML) |
| Falsifiability gate rows | 4 — PMU-rederived: 0 admit on the codec alone; unicode_escapes + y_string_unicode NEAR-FAIL (94.5% / 94.8%, same-wave conditional admission); unicode_mixed FAIL (63.7%, conditional on a paired scanner knob); gsoc-2018 admitted no-regression-only |
| Risk envelope | LOW on JSON-4 correctness, MEDIUM on variable-width CSS / JS, MEDIUM-HIGH on unicode_escapes + y_string_unicode performance, LOW on locks |
| Wave dispatch shape | One owner (the codec primitive), one production consumer + two scaffolds, one checkasm gate (lands first), one bench-row conditional-admission gate. |

## §8 — Sources

### 8.1 P1 antecedents (bbnf primary inputs)

- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
  §"Load-bearing diagnoses" item 3 (unicode-escape codec dominates
  y_string_unicode at 38-44%).
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-B-xctrace-time-profiler.md`
  §3.2 (string scanner pair partial confirmation + y_string_unicode
  bottleneck reframe), §3.5 (the `escape_codec_hex_unit` primitive
  class parameter table — the load-bearing P1 antecedent for the
  P2-E parameter table at §2 above).
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-C-hot-leaf-attribution.md`
  §2.1 (per-corpus self-time tables — y_string_unicode/t1,
  unicode_escapes/t1, unicode_mixed/t1, gsoc-2018/t1 specific
  attributions).
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-D-structural-breakdown.md`
  §5.3 (the four uncloseable rows + the 460% / 290% / 187% / 132%
  delimiter-only-intervention reductions that are physically
  impossible — naming the per-quartet primitive as the real lever),
  §6.2 (REDRESS 82 material differential requirement).

### 8.2 Current scalar implementation (bbnf source)

- `skinny/crates/parse-that-regex/src/lib.rs:945-956` — `read_hex_unit_scalar`.
- `skinny/crates/parse-that-regex/src/lib.rs:958-966` — `hex_nibble`.
- `skinny/crates/parse-that-regex/src/lib.rs:302-344` — `decode_unicode_escape`
  (surrogate-pair caller).
- `skinny/crates/parse-that-regex/src/lib.rs:347-382` — `validate_unicode_escape_run`
  (the run-validator hot path).
- `skinny/crates/parse-that-regex/src/lib.rs:384-459` — `unescape_four_unicode_escapes`
  (the existing 4-quartet NEON wrapper).
- `skinny/crates/parse-that-regex/src/lib.rs:718-810` — `unescape_string`
  (the materialiser hot path — wave-1 consumer).
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs` — existing
  NEON kernel (the antecedent body P2-E refines into the
  `escape_codec` directory module).
- `skinny/crates/runtime/src/grammars/json/sink.rs:1, 19, 30, 46, 87` —
  the JSON sink consumer (the second consumer call site).
- `skinny/crates/runtime/src/grammars/json/generated.rs:402-641` — the
  generated JSON `parse_string_direct` family + the `needs_unescape`
  flag plumbing.

### 8.3 REDRESS antecedents

- `skinny/REDRESS.md:2285-2316` — REDRESS 82, SK-V7 Wave 4 single-quartet
  unicode-escape classifier rejection (the material differential
  anchor at §5).

### 8.4 Lock surface

- `restart/locks/LOCKS.md` — Lock 14 (substrate-neutral primitive
  vocabulary, the framing P1-V3-B §1.5 + §3.5 binds), Lock 16
  (grammar-neutral primitive admissibility, the §3.5 reframe).

### 8.5 External / SOTA citations

- Validark, "Adventures with simdjson" (2021) — hex-nibble decode via
  a TBL whose `[0..='9']` slots map to `0..=9` and ASCII alpha slots
  map to `10..=15`, with poison value `0xFF` for invalid bytes; the
  Layer-0 substrate citation that the existing `unescape_uxxxx.rs`
  kernel reproduces.
- Lemire, "Parsing short hex strings with SIMD" (2022) — the
  three-ops-per-nibble shape (TBL → shift → OR) is the published
  floor on aarch64; cited in `unescape_uxxxx.rs:13-16`.
- Sneller, "Unicode escapes in JSON without branches" — the
  branchless surrogate-pair join pattern.
- ARM Architecture Reference Manual, A-profile (`ARM DDI 0487`):
  `vqtbl1q_u8` (TBL byte permute, §C7.2 — 1 µop on A78/M-class
  cores); `vminvq_u8` (horizontal min reduction, §C7.2 — 1 µop, 4-cycle
  latency); CSSC CTZ (§A2.7 — count-trailing-zeros instruction,
  available on Apple M3 and later; cited for the §3.3 8-byte
  variable-width body).
- simdjson "JSON String Unescaping" technique reference — the
  4-quartet batched path (loads 16 bytes = 4 quartets, decodes in
  parallel) is the published technique that the existing
  `unescape_uxxxx_x4_neon` already implements.

### 8.6 Cross-grammar grammar references

- ECMA-404 (JSON spec) §9 (string production: `\uXXXX` is fixed 4
  hex digits; `\uD800..\uDBFF` followed by `\uDC00..\uDFFF` forms a
  surrogate pair).
- CSS Syntax Module Level 4 §4.3.7 (consume escaped code point: 1..6
  hex digits, optional single whitespace consumed, no surrogate
  pairing — code points up to 0x10FFFF directly).
- ECMA-262 §11.8.4 (JavaScript string literals: `\u{...}` with
  1..6 hex digits delimited by `{` and `}`).
- TOML v1.0.0 §"String" (basic strings: `\uXXXX` 4 hex digits;
  `\UXXXXXXXX` 8 hex digits; no surrogate pairing).

### 8.7 Process discipline

- `restart/prompts/skinny/PASS-2-RESEARCH.md` — the P2 dispatch
  prompt + the six-lens CHALLENGE wave the candidate primitive must
  survive.
- `feedback_inspect_generated`, `feedback_no_workarounds`,
  `feedback_pluggable_components`, `feedback_general_infra_crates`,
  `feedback_directory_modules`, `feedback_accurate_perf_narrative`
  — the project-memory voice items the design respects.

## §0 — V2 fold footer: PMU rederivation

This V2 cycle folds the S-P2 V1 CHALLENGE dispositions against P2-E:

- **F2 — PMU rederivation (load-bearing, CH6-E-3 REJECT).** §6.1 and
  §6.2 are rebuilt from `/tmp/skv9-xctrace-v3/pmu_rows.tsv` directly.
  The V1 baseline c/B column (`0.354 / 0.628 / 0.787 / 0.193`) was
  fabricated or mis-sourced — it reconciles to no TSV column. The V2
  §6.1 cites the TSV `cycles_per_byte` and `ns_per_byte` columns
  verbatim for the four uncloseable rows; §6.2 derives the codec c/B
  from the P1-V3-C §3 per-class cycle-accounting table (`esc-hex`
  column: unicode_escapes 1.088 c/B / 36.2%, y_string_unicode 2.312
  c/B / 40.5%, unicode_mixed ~0.463 c/B / ~10% folded into the
  validator, gsoc-2018 ≈0%), projects the 75%-codec-reduction Mbps via
  the lossless per-row `c/B → ns/B → Mbps` inversion, and recomputes
  every PASS/NEAR-FAIL/FAIL verdict.
- **§6.4 admission rule (CH6-E-4).** The 0.70 W4-precedent slack is now
  stated *before* the projection and bound to a primary-source
  structural-hardness criterion (V3-C §3: y_string_unicode is the
  only qualifying row at 40.5% codec c/B).
- **F4 — per-slice cost discipline (CH4 12/12 REVISE).** §7.1 rebuilt
  as an eleven-slice table with per-slice LOC + minute cap + revert
  protocol + named same-wave consumer. §4.4 dispositions the TOML
  `\u` / `\U` bindings as compile-time validation only — no production
  consumer this wave, consumer wires after Pass Omega admits TOML.
- **F5 — scaffold-vs-production-consumer (CH2 5/6 ACCEPT, 1 REVISE).**
  §4 distinguishes the production consumer (the already-wired x4 JSON
  path at `parse-that-regex/src/lib.rs:402`) from the scaffolds (CSS
  L4 + TOML, compile-validated `#[cfg(test)]` codegen output).
- **CH3 consumer differential (4/5 ACCEPT, REDRESS 82 weak).** §5
  tightened: REDRESS 82 was a parser-owned per-quartet *classifier*;
  P2-E is a *primitive class — a full hex-decoder*, NOT a
  single-quartet classifier.

**New per-row admission verdicts after rederivation** (V1 → V2):
unicode_escapes PASS 100.5% → **NEAR-FAIL 94.5%**; y_string_unicode
NEAR-FAIL 94.5% → **NEAR-FAIL 94.8%**; unicode_mixed FAIL 68.7% →
**FAIL 63.7%**; gsoc-2018 NEAR-FAIL 98% → **admitted no-regression
basis only**. The honest posture: zero of four rows admit on the codec
alone — the codec is a strong contributor that approaches but does not
reliably cross the gate; admission is the §6.4 same-wave conditional
rule.
