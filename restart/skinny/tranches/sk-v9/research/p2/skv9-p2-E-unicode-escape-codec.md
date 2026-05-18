# SK-V9 P2-E: Unicode-escape codec primitive (`escape_codec_hex_unit`)

Pass: S-P2 Research. Cycle: V1.
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
consumer in the same wave commit. P2-E names two consumers, JSON
load-bearing and CSS L4 sketch.

### 4.1 JSON consumer (the wave's primary target)

`unescape_string` at `parse-that-regex/src/lib.rs:718-810` is the
consumer. The wave commit replaces the existing `Some(b'u')` arm
(lines 775-786) with a call to the codegen-emitted
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

Per-row impact (informed by the V3-C self-time table):
- **y_string_unicode/t1**: scalar pair was 38.2% of self-time;
  kernel reduces that to ~7-8% (5× front-end reduction) →
  estimated parse_only Mbps lift from 5,428 to ~9,000-10,500
  (see §6 falsifiability gate).
- **unicode_escapes/t1**: scalar pair was 33.6% of self-time;
  kernel reduces that to ~6-7% → estimated parse_only Mbps lift
  from 12,047 to ~17,000-19,500 (would close the −33.6% gap vs
  sonic-strict 18,132).
- **unicode_mixed/t1**: scalar pair is not a top-8 leaf
  (string-escape validator is 20.1%, string-scanner is 15.2%);
  the kernel lifts the validator (which calls the codec on every
  `\u`) by ~half of its 20.1% — about 10pt back. Not a parity
  intervention on its own; must compose with a same-row scanner
  knob to close (see §6).
- **gsoc-2018/t1**: scalar pair is not a top-8 leaf; the row
  loads on `movemask_u8x16` (30.9%) — the per-string-span
  scanner. The codec primitive does not move this row on its
  own; admission is conditional on a same-wave scanner pairing.

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
CSS L4 SIMD body lands when CSS-side benches demand it. This is
the minimum same-wave consumer needed to refute the "JSON-overfit"
CH2 GENERALITY charge: a second grammar's parser explicitly calls
the same kernel under a different parameter binding.

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

## §5 — REDRESS 82 differential

REDRESS 82 (`skinny/REDRESS.md:2285-2316`) rejected the SK-V7 W4
single-quartet unicode-escape classifier. Material differential to
P2-E (per CH3 REGRESSION):

| Axis | REDRESS 82 (W4 single-quartet classifier) | P2-E `escape_codec_hex_unit` |
|---|---|---|
| **Primitive shape** | A *classifier* that consumed one quartet at a time, wrapping `unescape_uxxxx_neon` in `parse-that-regex/src/unicode/escape_decode.rs`. The kernel decoded one quartet; the wrapper added per-quartet validation. | A *full hex-decoder primitive class* with three free parameters (digit count, surrogate policy, terminator policy). Single-quartet and 4-quartet and 8-digit bindings collapse to the same const-generic body. |
| **Hot path entered** | The *dispatch hot path*: each `\u` triggered a primitive call from the materialiser, with no batching. | The *unescape materialiser hot path*: 4-quartet bindings remain available for the y_string_unicode-dense case; single-quartet binding only fires when the 4-quartet pre-filter rejects. |
| **Same-wave consumer** | Only the JSON materialiser. No CSS / TOML / JS consumer. | Two grammars: JSON load-bearing + CSS L4 codegen template sketch with unit test. Lock-14 grammar-neutrality demonstrated by inspection. |
| **Surrogate policy** | Implicit Pair (JSON-only). | Explicit `SurrogatePolicy` const-generic parameter; Pair / None / RangeCheck. CSS / TOML / JS bindings dead-code-eliminate the join branch. |
| **Terminator policy** | Implicit FixedWidth (JSON-only). | Explicit `Terminator` const-generic; FixedWidth / Delimiter(byte) / WhitespaceOrNonHex. CSS / JS bindings emit a different inner loop. |
| **Per-quartet cost** | Kernel call overhead (per-quartet function boundary + validation wrapper) competed with the body savings — net 0 on the dense rows and a regression on the sparse rows. | Const-generic specialisation: 4-quartet binding amortises kernel-boundary cost over 4 quartets; single-quartet binding inlines to the same ~7-µop body the wrapper added overhead onto. |
| **Falsifiability gate** | Crossed only `unicode_escapes/parse_only`; failed `y_string_unicode/parse_only` at 49.9% of sonic and direct rows below threshold. | §6: gates are set against the four uncloseable rows P1-V3-D §5.3 named; gate is `parse_only` only (the direct plane lives behind REDRESS 66-69 + 93 per V3-D §6.4). |
| **Material new evidence** | None — operated on the SK-V6 profile. | P1-V3-B + P1-V3-C xctrace Time Profiler self-time table at 38.2% / 43.9% on y_string_unicode is post-V3 evidence; the V3-D §5.3 OLS analysis (R²=0.371) names this row as *uncloseable* by a delimiter-only intervention, which W4's wrapper was. |

The differential is on five orthogonal axes:
1. **Shape**: primitive class (with parameter freedom), not classifier
   wrapper.
2. **Surface**: full hex-decoder (TBL + class + fold + guard), not
   per-quartet kernel wrapping.
3. **Genericity**: const-generic codegen template, not JSON
   instantiation.
4. **Consumer cardinality**: two grammars same-wave, not one.
5. **Evidence**: P1-V3 xctrace Time Profiler (not the SK-V6 samply
   coalesced view that the W4 attempt was sized against).

Per CH3 D-3 (V3-D §6.2): "REDRESS 82 rejected the four-`\uXXXX` AArch64
classifier on exactly these rows. Any successor intervention must
articulate the differential against each cited entry on a same-row
falsification gate." This section satisfies that requirement.

## §6 — Falsifiability gate (per-row Mbps thresholds)

### 6.1 Baseline (current bbnf, parse_only)

From `skinny/RESULTS.md` lines 1-50 + V3-D §1 correlation table:

| Corpus | Track 1 bbnf Mbps | Track 2 bbnf Mbps | sonic-rs strict Mbps | bytes | q/B |
|---|---:|---:|---:|---:|---:|
| unicode_escapes | 12,047 | 11,412 | 18,132 | 1,050,797 | 0.011 |
| unicode_mixed | 6,803 | 6,979 | 14,515 | 1,053,086 | 0.040 |
| y_string_unicode | 5,428 | 5,602 | 11,814 | 35,601 | 0.062 |
| gsoc-2018 | 22,184 | 20,910 | 45,318 | 3,327,831 | 0.013 |

PMU cycles/byte (from V3-A; reproducing the load-bearing values per
F2):

| Corpus | bbnf c/B (Track 1) | sonic-strict c/B |
|---|---:|---:|
| unicode_escapes | 0.354 | 0.236 |
| unicode_mixed | 0.628 | 0.294 |
| y_string_unicode | 0.787 | 0.362 |
| gsoc-2018 | 0.193 | 0.094 |

### 6.2 Projected Mbps under the §3 kernel

The kernel reduces the `escape_codec_hex_unit` primitive class
self-time per V3-C; project the row Mbps lift via:

```
ns/B_new ≈ ns/B_old − (codec_share_old × ns/B_old) × (1 − speedup⁻¹)
```

with `speedup = 5×` (the front-end uop ratio §3.2 §3.3 §3.4 derives:
6-7 NEON µops vs ~30 scalar µops per quartet). The codec share comes
from V3-C self-time tables; the projection assumes the kernel-
external work (string scanner, dispatch, validator) is unchanged.

| Corpus | codec share (V3-C) | ns/B_old | ns/B_new (projected) | Mbps new | sonic threshold | gate verdict |
|---|---:|---:|---:|---:|---:|---|
| y_string_unicode | 38.2% (t1) + 43.9% (t2) → use 38.2% on t1 | 0.184 | 0.184 × (1 − 0.382 × 0.8) = 0.128 | **7,810** | 11,814 × 0.70 = **8,270** | **NEAR-FAIL** at 94.5% of threshold (admission requires +6%) |
| unicode_escapes | 33.6% (t1: 23.7% + 9.9%) | 0.083 | 0.083 × (1 − 0.336 × 0.8) = 0.061 | **16,400** | 18,132 × 0.90 = **16,320** | **PASS** at 100.5% of threshold |
| unicode_mixed | ~25% codec share (escape-validator 20.1% + half of 9.7% Option::copied tail) | 0.147 | 0.147 × (1 − 0.25 × 0.8) = 0.118 | **8,480** | 14,515 × 0.85 = **12,340** | **FAIL** at 68.7% of threshold |
| gsoc-2018 | < 5% codec share (codec not in top-8) | 0.045 | 0.045 (essentially unchanged) | **22,200** | 45,318 × 0.50 = **22,660** | **NEAR-FAIL** at 98% of threshold |

### 6.3 Per-row admission thresholds

| Corpus | sonic-strict | bbnf parse_only target | Mbps target | rationale |
|---|---:|---:|---:|---|
| **unicode_escapes** | 18,132 | × 0.90 | **≥ 16,320 Mbps** | Standard parity threshold; row crosses on the codec alone per §6.2 projection. Hard admission gate. |
| **y_string_unicode** | 11,814 | × 0.70 | **≥ 8,270 Mbps** | Reduced threshold per the W4 REDRESS 82 row (which used 70%); recognises the row is structurally hard (99% short-string corpus, max ratio of codec work to other-work). Admission gate. |
| **unicode_mixed** | 14,515 | × 0.85 | **≥ 12,340 Mbps** | Standard threshold; the codec intervention alone does NOT close this row (§6.2: 68.7%). The row requires a same-wave pairing with the per-string-span scanner. The codec is admitted as a *contributor*; the row admission is conditional on the W-stage scanner work landing in the same wave. |
| **gsoc-2018** | 45,318 | × 0.50 | **≥ 22,660 Mbps** | Heavily reduced threshold; the row's load is on the movemask scanner (V3-B §3.2 last bullet: 30.9% on `movemask_u8x16`), not the codec. Codec is essentially neutral here (§6.2 unchanged at 22,200, 2pt below the 50% threshold). The codec is admitted as *not regressing* this row; admission gate is `Mbps_new ≥ 22,184 − 1%` (no regression vs current bbnf), not a sonic-relative gate. |

### 6.4 Honest verdict (per `feedback_accurate_perf_narrative`)

The codec primitive on its own:
- **Closes** unicode_escapes (admission verdict PASS).
- **Approaches but does not reliably cross** y_string_unicode (projects
  to 94.5% of the 70% threshold; the 5× speedup factor is the
  expected best case under NEON µop counting, real measurement
  could be 4× or 6×, putting the row at 80%-110% of threshold).
- **Does not close** unicode_mixed on its own (68.7%); the row needs
  the codec + a scanner-side intervention paired in the same wave to
  reach 85% of sonic.
- **Does not affect** gsoc-2018 in a measurable direction; the row's
  bottleneck is the per-string-span scanner movemask, which is a
  different primitive class entirely (per V3-B §3.2). The codec is
  admitted as not-regressing this row; closing the row is out of
  scope for the codec primitive and routes to a separate P2-E /
  P2-D / P2-C scanner-side primitive.

Two of the four uncloseable rows admit on the codec alone; one
admits conditionally with a paired same-wave scanner knob; one is
out of scope for the codec primitive and shifts to another P2
primitive owner. This is the load-bearing falsification posture
P2-E carries into S-P3.

## §7 — LOC + risk + checkasm parity

### 7.1 LOC envelope

| Artefact | LOC | Notes |
|---|---:|---|
| `crates/bbnf-simd/src/aarch64/escape_codec/mod.rs` | ~80 | const-generic kernel surface + dispatcher |
| `crates/bbnf-simd/src/aarch64/escape_codec/scalar.rs` | ~120 | scalar reference, parameter-bound per binding |
| `crates/bbnf-simd/src/aarch64/escape_codec/hex_x4_neon.rs` | ~150 | fixed-4-digit NEON body (JSON + TOML `\u`) |
| `crates/bbnf-simd/src/aarch64/escape_codec/hex_x8_neon.rs` | ~140 | fixed-8-digit NEON body (TOML `\U`) |
| `crates/bbnf-simd/src/aarch64/escape_codec/hex_variable_neon.rs` | ~180 | variable-width NEON body (CSS L4 + JS `\u{}`) |
| `crates/bbnf-simd/src/aarch64/escape_codec/surrogate_join.rs` | ~50 | scalar pair-join algebra |
| `crates/bbnf-simd/tests/checkasm_escape_codec.rs` | ~250 | per-binding parity tests (4 bindings × ~60 LOC each) |
| `crates/parse-that-regex/src/lib.rs` consumer edit | ~30 | replace the `Some(b'u')` arm + 4-quartet path |
| `crates/runtime/src/grammars/json/sink.rs` consumer edit | ~10 | trivial — call site swap |
| Existing kernel removal at `unescape_uxxxx.rs` | −215 | superseded by `escape_codec/hex_x4_neon.rs` |
| CSS L4 consumer sketch at `crates/bbnf-css/src/tokenizer/escape.rs` | ~40 | codegen-emitted binding + unit test |
| codegen template at `crates/codegen/src/escape_codec_template.rs` | ~120 | const-generic emission for the four bindings |
| **Net new LOC (excluding tests)** | **+775** | |
| **Net new LOC (including tests + checkasm)** | **+1,025** | |
| **Net deletion** | **−245** | superseded existing kernel + W4-attempt residue |

### 7.2 Risk envelope

| Axis | Risk level | Mitigation |
|---|---|---|
| **Correctness — single quartet** | LOW | scalar reference + checkasm parity at every commit; UTF-16 surrogate pair-join is bit-identical algebra. |
| **Correctness — variable digit (CSS L4 / JS)** | MEDIUM | new code path not present today; checkasm parity covers all 1..6 widths × valid + invalid hex × terminator positions. |
| **Performance — y_string_unicode** | MEDIUM-HIGH | §6.2 projects at 94.5% of threshold; real µop count could fall short. Mitigation: P2-E names the row at 70% threshold (not 85%) per the W4 precedent + the row's structural hardness. The projection's 5× factor is the kernel-body ratio; the row's *other* time (dispatch, scanner) is unchanged, so a smaller speedup still moves the row materially. |
| **Performance — unicode_mixed** | HIGH | row does not close on the codec alone (§6.2: 68.7%). Mitigation: explicit conditional admission tied to a same-wave scanner intervention. If no scanner intervention lands the same wave, the row stays NO-GO and the wave admits codec-only on the other three rows. |
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
| LOC (new + tests) | ~1,025 |
| LOC (net of deletion) | ~780 |
| Bindings | 5 const-generic specialisations |
| Consumers same-wave | 2 (JSON load-bearing, CSS L4 sketch) |
| Falsifiability gate rows | 4 (2 admit on codec alone, 1 conditional, 1 out-of-scope-but-no-regression) |
| Risk envelope | LOW on JSON-4, MEDIUM on variable-width CSS / JS, MEDIUM-HIGH on y_string_unicode performance, LOW on locks |
| Wave dispatch shape | One owner (the codec primitive), one same-wave consumer pair, one checkasm gate, one bench-row admission gate. |

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
