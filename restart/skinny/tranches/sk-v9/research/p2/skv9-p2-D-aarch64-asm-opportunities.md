---
tranche: SK-V9
phase: P2 research
artefact: P2-D
title: Host-targeted aarch64 ASM/SIMD opportunities for the four uncloseable rows
date: 2026-05-18
scope: research-only; no code edits; no commits
lens: host-cap — given the Apple M5 Max + ARMv9.2-A intrinsic surface, which
  per-uncloseable-row primitive class admits a wave-sized intervention that
  REDRESS 28/33/88/89 + 50-55 do not already block?
host: Apple M5 Max (aarch64-apple-darwin; 12P + 6E cores; ARMv9.2-A)
authority_inputs:
  - restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-CONVERGED.md
  - restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-B-xctrace-time-profiler.md
  - restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-D-structural-breakdown.md
  - restart/skinny/tranches/sk-v7/research/skv7-A3-dav1d-esoterica.md
  - restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md
  - restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md
  - skinny/REDRESS.md entries 28, 33, 50-55, 60-62, 64, 66-69, 82, 83, 84, 88, 89, 90
  - skinny/crates/bbnf-simd/src/aarch64/
  - skinny/crates/parse-that-regex/src/lib.rs (read_hex_unit_scalar, hex_nibble, match_string_at_quote_trusted_utf8, validate_string_escape)
---

# SK-V9 P2-D — Host-Targeted aarch64 ASM/SIMD Opportunities

The S-P1 convergent diagnosis ledger: four "uncloseable" rows
(`unicode_mixed`, `unicode_escapes`, `y_string_unicode`, `gsoc-2018`) carry
throughput gaps exceeding 130-460% of the OLS per-byte delimiter budget;
the wave that closes them is not a per-delimiter cost reduction but a
*new primitive class* drop. SK-V7 W10 PMULL prefix-XOR and W10b CSSC CTZ
bulk-consumer are both REJECTED (REDRESS 88, 89): the host-cap shape
that worked on x86 (PMULL/PMULL2 prefix-XOR as the default
`bitmap_prefix_xor_64` body) regressed escape-heavy rows by 12-15% and
the more conservative CSSC CTZ bulk consumer regressed six other rows
by 2-8%. Any SK-V9 P2 proposal that lights up an aarch64 intrinsic must
therefore carry: (a) a class not already foreclosed by 28/33/88/89,
(b) a same-wave consumer (no orphan primitive — dav1d/FFmpeg rule per
SK-V7 A3 §8), and (c) a falsification gate on the four uncloseable
rows.

This report enumerates the host surface, diagnoses the four rows,
designs the unicode-escape and string-block opportunities, surveys the
dead-SIMD-scanner wiring substrate that the union-substrate makes live,
audits the DAV1D process discipline against current bbnf-simd state,
and concludes each opportunity against its REDRESS material
differential.

---

## §1 — Host-cap survey (M5 Max + ARMv9.2-A)

### §1.1 — Feature inventory (confirmed available on M5 Max)

Host detection (per SK-V7 A3 §2 and `restart/skinny/tranches/sk-v6/
research/skv6-A6-host-asm-instruction-map.md` §1.1):
`hw.optional.arm.FEAT_PMULL=1`, `FEAT_DotProd=1`, `FEAT_CSSC=1`,
`FEAT_SME=1`/`FEAT_SME2=1`/`FEAT_SME2p1=1`, AES + SHA3 + SHA512
default-cfg under `RUSTFLAGS=-C target-cpu=native`.
Ordinary non-streaming SVE/SVE2 **unavailable** (M5 Max exposes
streaming-mode SME but not the unprivileged SVE2 vector pipe at the
user-mode ISA layer).

The intrinsic classes that admit at all on M5 Max:

| Feature  | Hot intrinsics                                                                  | Hot µop count       | bbnf-relevant primitives                                            |
| -------- | ------------------------------------------------------------------------------- | ------------------- | ------------------------------------------------------------------- |
| NEON     | `vld1q_u8`, `vqtbl1q_u8`, `vqtbl4q_u8`, `vceqq_u8`, `vandq_u8`, `vshrn_n_u16`   | 1                   | TBL byte classification; 16-byte block scan; shrn-fused movemask    |
| CSSC     | `ctz` (replaces `rbit` + `clz`) under `-C target-cpu=native`                    | 1                   | `bitmap_next_set_bit` first-set extract; tail handling              |
| FEAT_AES | `vaeseq_u8` (AES single-round; 1-cycle on Apple cores; doubles as a fast TBLX) | 1                   | byte-class shuffles when class predicate fits the AES S-box shape   |
| PMULL    | `vmull_p64`, `vmull_high_p64`                                                   | 1-2                 | carryless multiply; prefix-XOR; bit-mask propagation                |
| SHA3     | `veor3q_u8` (3-way XOR), `vbcaxq_u8`, `vrax1q_u64`, `vxarq_u64`                 | 1 (vs 2 EOR chain)  | XOR-tree collapse in mask carry; ternary AND-NOT for class predicates |
| DotProd  | `vdotq_u32`, `vusdotq_s32`                                                       | 1                   | digit MAC (4-digit and 8-digit horizontal accumulation)             |
| FlagM2   | `axflag`, `xaflag`, `setf8`/`setf16`                                            | 1                   | not a parser primitive (NZCV manipulation)                          |
| FRINTTS  | rounding with saturation                                                        | not relevant        | parser doesn't round                                                |
| SME/SME2 | streaming SVE matrix; ZA tile; predicated MAD                                   | streaming-mode only | rejected per SK-V7 A3 §2 (transition cost > parser hot loop length) |
| SVE/SVE2 | `svld1`, `svtbl`, `svcmpeq`, `svbsl`                                            | n/a                 | **unavailable on M5 Max** (host blocks)                             |

### §1.2 — What's blocked, and why

1. **SVE2** is unavailable on M5 Max at the user-mode ISA. Any
   proposal citing SVE2 BSL-based mask compression is host-blocked.
2. **SME2** streaming-mode is available, but the parser hot loop is too
   short to amortise the ZA-tile / Streaming-mode transition cost. The
   SME ZA tile is 256 bytes; entering streaming mode flushes the
   non-streaming SVE state and re-binds vector predicates. Per SK-V7 A3
   §2 the streaming-mode entry/exit budget exceeds the per-parse loop
   length. Rejected before measurement.
3. **PMULL** as a default-body for `bitmap_prefix_xor_64` is
   pre-rejected by REDRESS 88 — the candidate emitted `pmull.1q`
   correctly under `cargo asm` proof and the SIMD-scan microbench was
   neutral, but four named JSON rows regressed 4-15% on the production
   JSON parse benchmark (`instruments`, `numbers`,
   `unicode_escapes/track1`, `unicode_escapes/track2`). The host-cap
   shape works in isolation; the production-path wiring regressed
   escape-heavy rows specifically.
4. **CSSC CTZ as a bulk consumer** is pre-rejected by REDRESS 89 — the
   `bitmap_next_set_bit` aarch64 body using native `ctz` was correct,
   passed checkasm, but six rows dropped 2-8% on the W10b candidate.

### §1.3 — What's measured-but-orphaned

The substrate ledger from SK-V7 A3 §8 lists four "blocked_no_consumer"
primitives: `BULK_EMIT_COMPRESSED`, `FRAME_PUSH_BOUNDED`,
`FRAME_POP_BOUNDED`, `FSM_DISPATCH_THREADED`. These are gated on a
CollapsedStage codegen consumer — the V9.5 PSI excavation rejected
Rust-codegen-of-automata as a CollapsedStage shape (only hand-written
NASM admissible), so on aarch64 these unlock only through a per-grammar
`.S` wrapper consuming the macros. **Not in scope for SK-V9 P2-D** — the
SK-V9 wave-class is parse_only substrate, not CollapsedStage codegen.

### §1.4 — The wide-issue ceiling argument

CPI < 0.4 across 17 corpora means the M5 Max is **not** retire-bound on
any row. The 1.18-5.95 c/B range is *work-content* bound: the parser
performs O(N) ALU operations per byte and N is large on the LOSS rows.
A wave that adds **fewer µops per byte** lifts throughput; a wave that
adds **wider µops** (i.e. a SIMD primitive doing 16-way work in 1-3
µops) lifts throughput by ~16/3 ≈ 5×. Per-byte hex_nibble's scalar
6-7 µops/digit × 4 digits × 1 per-`\uXXXX` becomes ~28 µops/quartet;
a single `vqtbl1q_u8` + a `vandq_u8` + a vector range-test fold runs
the same 4-digit work in **3-4 µops total**. The host-cap arithmetic
admits a 6-8× reduction on the per-quartet primitive — large enough to
close the 38-44% self-time `y_string_unicode` Time Profiler row.

---

## §2 — Per-uncloseable-row diagnosis

For each of the four LOSS rows that the OLS regression cannot close
with a delimiter-only intervention, this section identifies the hot
leaves from P1-V3-B + C and names the ASM/SIMD primitive class whose
admission would close the residual gap.

### §2.1 — `y_string_unicode` (q_frac 1.000, 32 bytes/span, Δ_p −54.1%)

**Hot leaves (Time Profiler P1-V3-B §2):**

| Rank | %self  | Class                | Symbol                                                  | Source                                          |
| ---: | -----: | -------------------- | ------------------------------------------------------- | ----------------------------------------------- |
|    1 | 19.2%  | `unicode_escape_hex` | `parse_that_regex::hex_nibble`                          | `crates/parse-that-regex/src/lib.rs:959`        |
|    2 | 19.0%  | `unicode_escape_hex` | `parse_that_regex::read_hex_unit_scalar`                | `crates/parse-that-regex/src/lib.rs:945`        |
|    3 | 10.6%  | `string_tiny_scan`   | `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` | `crates/runtime/src/grammars/json/generated.rs:178` |
|    4 |  5.5%  | `simd_movemask`      | `bbnf_simd::aarch64::movemask::movemask_u8x16`           | `crates/bbnf-simd/src/aarch64/movemask.rs:22`   |
|    5 |  5.1%  | `dispatch_value`     | `runtime::generated_json::generated::dispatch_value`     |                                                 |

**Class diagnosis:** `escape_codec_hex_unit` carries 38.2% (track 1)
and 43.9% (track 2) self-time. The corpus is 99%+ short 6-byte
`\uXXXX` strings; the per-quartet primitive completely dominates.

**Primitive class to close the gap:** vectorised `\uXXXX` codec body.
The NEON kernel `unescape_uxxxx_neon` at
`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:74` is already
implemented (per Wave 1 admission kernel), and the four-quartet variant
`unescape_uxxxx_x4_neon` at `:125` exists. **Neither is wired into the
parse-that-regex hot path.** REDRESS 82 rejected a previous wiring
attempt as a *parser-owned per-quartet helper* under W4
falsifiability. The substrate change in SK-V9 P2 (union substrate /
P2-A scope) is what shifts the wiring framing: see §3 and §7.

### §2.2 — `unicode_escapes` (q_frac 0.750, 373 bytes/span, Δ_p −33.6%)

**Hot leaves (P1-V3-B §2):**

| Rank | %self  | Class                | Symbol                                                  |
| ---: | -----: | -------------------- | ------------------------------------------------------- |
|    1 | 23.7%  | `unicode_escape_hex` | `parse_that_regex::read_hex_unit_scalar`                |
|    2 | 20.9%  | `dispatch_value`     | `runtime::generated_json::generated::dispatch_value`     |
|    3 | 19.5%  | `string_full_scan`   | `parse_that_regex::match_string_at_quote_trusted_utf8`   |
|    4 |  9.9%  | `unicode_escape_hex` | `parse_that_regex::hex_nibble`                          |
|    5 |  4.8%  | `string_escape`      | `parse_that_regex::validate_string_escape`              |

**Class diagnosis:** Mixed `escape_codec_hex_unit` (33.6% combined) +
`string_full_scan` (19.5%) + `string_escape` (4.8%). The long-span
shape (373 B/span) shields the per-delimiter scan but exposes the
per-`\uXXXX` decode and the in-string escape validation as cumulative
costs across the span.

**Primitive classes to close the gap:**
- vectorised `\uXXXX` codec (same as §2.1) — closes 33.6%.
- escape-validation TBL collapse — `validate_string_escape` currently
  branches on byte class; a single `vqtbl1q_u8` lookup over the 16-byte
  escape-set (`b`, `f`, `n`, `r`, `t`, `\\`, `\"`, `/`, `u`) replaces
  the scalar match ladder.

### §2.3 — `unicode_mixed` (q_frac 0.750, 42 bytes/span avg, Δ_p −53.1%)

**Hot leaves (P1-V3-B §2):**

| Rank | %self  | Class                | Symbol                                                  |
| ---: | -----: | -------------------- | ------------------------------------------------------- |
|    1 | 24.9%  | `dispatch_value`     | `runtime::generated_json::generated::dispatch_value`     |
|    2 | 20.1%  | `string_escape`      | `parse_that_regex::validate_string_escape`              |
|    3 | 15.2%  | `string_full_scan`   | `parse_that_regex::match_string_at_quote_trusted_utf8`   |
|    4 |  9.7%  | other                | `<u16>::trailing_zeros`                                  |
|    5 |  9.5%  | `simd_movemask`      | `bbnf_simd::aarch64::movemask::movemask_u8x16`           |
|    6 |  5.7%  | `string_tiny_scan`   | `match_tiny_plain_string_with_cap::<16>`                 |

**Class diagnosis:** Distinct profile — **no `escape_codec_hex_unit`**
in the top eight (different from §2.1, §2.2). The hot cost is
`string_escape` (20.1%, the per-byte `validate_string_escape` branch
ladder) + `string_full_scan` (15.2%) plus the
`<u16>::trailing_zeros` cost (9.7%, the bitmap consumer for
`scan_string_special_block`). The string spans carry validated UTF-8
non-ASCII bytes rather than `\uXXXX` escapes — the per-byte non-ASCII
classification fires on every span byte.

**Primitive classes to close the gap:**
- escape-validation TBL collapse (same as §2.2).
- 32-byte block scan widening (see §4) — current
  `scan_string_special_block` processes 16 bytes per call; a 32-byte
  variant halves the call rate and amortises the per-block mask
  computation across two 16-byte vector loads + a concatenated u32
  mask. The span average is 42 bytes; a 32-byte block fits one full
  call per string with a small scalar tail.
- CSSC CTZ direct on the consumer side — the
  `<u16>::trailing_zeros` 9.7% is `rbit + clz` on the existing baseline
  (the kernel doesn't have a CSSC CTZ body for the call site). REDRESS
  89 rejected this exact change on the production hot path; the
  material differential is that the union-substrate wave consumes the
  mask in a *different code path* (P2-A scope, the union event/tape
  loop), so the wiring is not the same as the W10b candidate.

### §2.4 — `gsoc-2018` (q_frac 1.000, 195 bytes/span, Δ_p −51.0%)

**Hot leaves (P1-V3-B §2):**

| Rank | %self  | Class                | Symbol                                                  |
| ---: | -----: | -------------------- | ------------------------------------------------------- |
|    1 | 30.9%  | `simd_movemask`      | `bbnf_simd::aarch64::movemask::movemask_u8x16`           |
|    2 | 20.8%  | `string_tiny_scan`   | `match_tiny_plain_string_with_cap::<16>`                 |
|    3 | 10.5%  | other                | `<u16>::trailing_zeros`                                  |
|    4 |  5.3%  | `whitespace_skip`    | `parse_that_regex::skip_ascii_whitespace`               |
|    5 |  4.8%  | `string_block_scan`  | `parse_that_regex::skip_string_plain_trusted`            |
|    6 |  4.0%  | `string_full_scan`   | `parse_that_regex::match_string_at_quote_trusted_utf8`   |
|    7 |  3.5%  | `string_dispatch`    | `parse_string`                                          |
|    8 |  3.2%  | other                | `<u16 as core::convert::From<u8>>::from`                 |

**Class diagnosis:** The `simd_movemask` symbol at 30.9% is the
**string-block scanner's** mask helper (the movemask of the 16-byte
`scan_string_special_block` result). Adding `<u16>::trailing_zeros`
(10.5%) and `string_block_scan` (4.8%) the mask-+-CTZ pipeline is
*46.2% self-time*. The corpus has many long-span (195 B avg) strings;
each span pays one `scan_string_special_block` call per 16 bytes of
content, then a `trailing_zeros` extract per interesting bit, then a
loop iteration.

**Primitive classes to close the gap:**
- 32-byte block scan (halves the call rate).
- CSSC CTZ direct in the consumer (the 10.5% `trailing_zeros` is
  scalar RBIT+CLZ; native CTZ saves one µop per extract).
- `shrn`-fused movemask (replaces the `vshrn_n_u16::<4>`-then-pack
  pattern at `movemask.rs:22` with a single `shrn` writing directly to
  the consumer's mask register; saves the intermediate
  vand-shift-pack chain).

---

## §3 — Unicode-escape codec SIMD design

The class that dominates `y_string_unicode` (38-44%) and
`unicode_escapes` (33%) and is *generalisable* across grammars per
P1-V3-B §3.5 (CSS L4, JS `\u{...}`, TOML `\U`).

### §3.1 — Scalar reference (the parity oracle)

Current scalar at `parse-that-regex/src/lib.rs:945`
(`read_hex_unit_scalar`) + `:959` (`hex_nibble`):

```rust
#[inline(always)]
fn hex_nibble(byte: u8) -> u8 {
    match byte {
        b'0'..=b'9' => byte - b'0',
        b'a'..=b'f' => byte - b'a' + 10,
        b'A'..=b'F' => byte - b'A' + 10,
        _ => 0xff,
    }
}

#[inline(always)]
fn read_hex_unit_scalar(hex: &[u8]) -> Option<u16> {
    let n0 = hex_nibble(hex[0]);
    let n1 = hex_nibble(hex[1]);
    let n2 = hex_nibble(hex[2]);
    let n3 = hex_nibble(hex[3]);
    if (n0 | n1 | n2 | n3) & 0xf0 != 0 {
        return None;
    }
    Some(((n0 as u16) << 12) | ((n1 as u16) << 8) | ((n2 as u16) << 4) | n3 as u16)
}
```

Per-quartet µop count (approximation under `-C target-cpu=native`,
LLVM IR -> aarch64): 4 × (load + 3-way branch + sub + or) =
roughly 28 µops + 4 conditional branches; the conditional branches
are the dominant cost because the four hex bytes are *independently*
mispredictable.

### §3.2 — NEON kernel (already in tree, unwired)

`bbnf-simd/src/aarch64/unescape_uxxxx.rs:74`
(`unescape_uxxxx_neon`) is implemented per Wave 1 admission. The
intrinsic shape:

```rust
let bytes = vld1q_lane_u8::<0..3>(...);          // 4-byte load into low 4 lanes
let low_nibbles = vandq_u8(bytes, vdupq_n_u8(0x0f));
let lut = vld1q_u8(HEX_NIBBLE_LUT.as_ptr());
let base = vqtbl1q_u8(lut, low_nibbles);         // TBL: low-nibble -> 0..9 or poison

// Three range tests give the digit / upper / lower hex class:
let is_digit = vandq_u8(vcgeq_u8(bytes, '0'), vcgeq_u8('9', bytes));
let is_upper = vandq_u8(vcgeq_u8(bytes, 'A'), vcgeq_u8('F', bytes));
let is_lower = vandq_u8(vcgeq_u8(bytes, 'a'), vcgeq_u8('f', bytes));
let is_alpha = vorrq_u8(is_upper, is_lower);
let is_hex   = vorrq_u8(is_digit, is_alpha);
let alpha_adjust = vandq_u8(is_alpha, vdupq_n_u8(9));
let nibbles = vaddq_u8(base, alpha_adjust);

// Single early-out if any lane is non-hex (vminvq_u8 over is_hex):
if any-is-hex-bit-clear { return None; }

// Horizontal fold over four lanes -> u16 codepoint
```

Per-quartet µop count: 4 × LOAD-LANE (1 µop each, but they retire in
parallel as wide loads) + 1 AND + 1 TBL + 6 CMP/AND + 2 OR + 1 ADD =
**~15 µops, no data-dependent branches**, plus the early-out branch
which is *uniformly taken* on valid input (predicted perfectly). The
`vminvq_u8` horizontal-reduce takes the place of the 4-way OR of the
scalar version.

### §3.3 — The x4 variant (already in tree)

`unescape_uxxxx_x4_neon` at `:125` decodes **four consecutive
quartets** in one 16-byte vector: a single `vld1q_u8` loads 16 bytes,
the same TBL + range-tests fire once over all four quartets, and the
output is `[u32; 4]`. Per-quartet µop count: 1 LOAD + 1 AND + 1 TBL +
6 CMP/AND + 2 OR + 1 ADD + 1 STORE = **~13 µops total for FOUR
quartets**, i.e. ~3.25 µops per quartet. This is the load-bearing
amortisation: `y_string_unicode`'s 38% self-time class would land at
~10-12% on a wired x4 path, with the remaining cost being the loop
plumbing.

### §3.4 — Cross-grammar generalisation (per P1-V3-B §3.5)

The class parameters are:

| Parameter             | JSON     | CSS L4              | JS `\u{}`           | TOML `\U` / `\u`    |
| --------------------- | -------- | ------------------- | ------------------- | ------------------- |
| `hex_digit_count`     | 4 fixed  | 1..6 range          | 1..6 in `{}`        | 8 fixed / 4 fixed   |
| `surrogate_join`      | pair     | none                | range-check         | none                |
| `terminator_policy`   | fixed-w  | whitespace-or-non-hex | `}`-delimiter     | fixed-w             |
| `target_encoding`     | utf-8    | utf-8               | utf-8               | utf-8               |

The current `unescape_uxxxx_neon` is the *fixed-width 4* leaf. A
class-general primitive `escape_codec_hex_unit{N, join, term, enc}`
parameterises:
- the load width (4-lane / variable-lane via a length predicate);
- the surrogate-join postprocess (admission predicate against
  `0xD800..=0xDFFF`);
- the terminator policy (consume-N-fixed vs. detect-non-hex-via-TBL
  poison vs. `}`-delimiter).

The aarch64 primitive body is **identical** across the three (the TBL +
range fold doesn't change); the differences live in the surrounding
wrapper and the per-grammar `.data` slot. Lock 14 admissible: per
SK-V7 A3 §1 the data-vs-code split puts class-LUTs and terminator
policy in codegen-emitted `.data` tables, the macro body stays
grammar-neutral.

### §3.5 — Wiring proposal (material differential vs REDRESS 82)

REDRESS 82's W4 candidate moved the scalar `\uXXXX` decoder into
`unicode/escape_decode.rs`, reused the existing
`unescape_uxxxx_neon` for **one quartet at a time**, and wired it into
`decode_json_unicode_escape` and `unescape_json_string`. The
falsifiability gate failed on `unicode_escapes/direct_to_struct`
(39.4% of sonic) and `y_string_unicode/direct_to_struct` (regressed
6.6% on track 2).

The SK-V9 proposal differs structurally on three axes:

1. **x4 batching, not per-quartet.** The wired call site uses
   `unescape_uxxxx_x4_neon` over four consecutive `\uXXXX` quartets at
   the call site — the corpus-realistic shape on
   `y_string_unicode` where keys/values are dominated by 4-6 quartet
   runs. The W4 candidate only used the per-quartet kernel.
2. **Same-wave union-substrate consumer (P2-A scope).** The wiring
   lives in the union-substrate's typed event/tape consumer, not in
   the parser-owned scalar string materialiser. Per SK-V7 A3 §8 and
   REDRESS 82 the "parser-owned per-quartet helper" was the rejected
   shape; the union-substrate primary write path is materially
   different (per SC-6 §1.3 the structural projection becomes the
   substrate; the codec runs **once per retained tape cell**).
3. **Falsification gate against `direct_to_struct`.** REDRESS 82
   failed because `unicode_escapes/direct` regressed. The SK-V9
   proposal **must** include the direct-route digest gate on
   `unicode_escapes` + `y_string_unicode` + `unicode_mixed` as a
   blocking precondition — REDRESS 82's blocking rows become SK-V9's
   admission rows.

### §3.6 — Three-ops-per-nibble floor

Per the module docstring at `unescape_uxxxx.rs:1-29`, the floor on
aarch64 is **3 NEON µops per nibble** (TBL → shift → OR), per Lemire
"Parsing short hex strings with SIMD" (2022). The current
`unescape_uxxxx_neon` is *above* the floor (4-5 µops/nibble) because
of the explicit `vcgeq_u8` range tests — the LUT collision with
digit ASCII bits is disambiguated via three explicit class checks.
The x4 variant amortises these across 4 quartets, recovering the
floor at the per-quartet level. A further-optimised variant could
fold the range tests into a *single* TBL via a 64-entry low-6-bit
table (the Class A `match_tiny_plain_string` shape per
`match_tiny_plain_string.rs:79`), but that's a Wave 2+ optimisation;
the Wave 1 admission is the existing x4 NEON body.

---

## §4 — String-block scanner widening

The `simd_movemask` symbol carries 30.9% (gsoc-2018/t1), 29.9% (t2);
adding `<u16>::trailing_zeros` (10.5%, gsoc-2018/t1) and
`string_block_scan` (4.8%) the mask-+-CTZ pipeline carries
46.2% combined self-time on the most string-heavy long-span corpus.

### §4.1 — Current 16-byte path

`bbnf-simd/src/aarch64/string_block.rs:57`
(`scan_string_special_block`) loads 16 bytes via `vld1q_u8`, runs four
parallel `vceqq_u8`/`vcltq_u8`/`vcgeq_u8` against `terminator`,
`escape`, `<0x20`, `>=0x80`, then `movemask_u8x16` each result to a
16-bit mask, returning a `StringSpecialBlock { terminator_mask,
escape_mask, control_mask, non_ascii_mask }`.

```rust
#[inline(always)]
pub unsafe fn scan_string_special_block(
    ptr: *const u8, terminator: u8, escape: u8, control_limit: u8,
) -> StringSpecialBlock {
    let chunk = vld1q_u8(ptr);
    StringSpecialBlock {
        terminator_mask:  movemask_u8x16(vceqq_u8(chunk, vdupq_n_u8(terminator))),
        escape_mask:      movemask_u8x16(vceqq_u8(chunk, vdupq_n_u8(escape))),
        control_mask:     movemask_u8x16(vcltq_u8(chunk, vdupq_n_u8(control_limit))),
        non_ascii_mask:   movemask_u8x16(vcgeq_u8(chunk, vdupq_n_u8(0x80))),
    }
}
```

Per-block µop count: 1 LOAD + 4 CMP + 4 MOVEMASK = 9 µops + 4 broadcast
ops (`vdupq_n_u8`, hoisted out of the loop by LLVM). The
`movemask_u8x16` itself is `vshrn_n_u16::<4>` + a 4-stage AND-OR-pack
chain (≈ 4-6 µops).

### §4.2 — 32-byte widening (feasibility)

Two `vld1q_u8` loads (consecutive 16-byte chunks) load 32 bytes. The
*same four* `vceqq_u8`/`vcltq_u8`/`vcgeq_u8` test patterns fire twice,
producing two 16-bit masks per class. The natural fusion is:

```rust
// Pseudo-code; aarch64 intrinsic body
let chunk_lo = vld1q_u8(ptr);
let chunk_hi = vld1q_u8(ptr.add(16));

let term_lo = vceqq_u8(chunk_lo, vdupq_n_u8(terminator));
let term_hi = vceqq_u8(chunk_hi, vdupq_n_u8(terminator));
let esc_lo  = vceqq_u8(chunk_lo, vdupq_n_u8(escape));
let esc_hi  = vceqq_u8(chunk_hi, vdupq_n_u8(escape));
let ctl_lo  = vcltq_u8(chunk_lo, vdupq_n_u8(control_limit));
let ctl_hi  = vcltq_u8(chunk_hi, vdupq_n_u8(control_limit));
let n80_lo  = vcgeq_u8(chunk_lo, vdupq_n_u8(0x80));
let n80_hi  = vcgeq_u8(chunk_hi, vdupq_n_u8(0x80));

// SHA3 EOR3 collapses 2-stage XOR; vector OR is the dual:
// Fold the four "interesting" classes into ONE mask per chunk:
let interesting_lo = vorrq_u8(vorrq_u8(term_lo, esc_lo), vorrq_u8(ctl_lo, n80_lo));
let interesting_hi = vorrq_u8(vorrq_u8(term_hi, esc_hi), vorrq_u8(ctl_hi, n80_hi));

// 32-bit mask via shrn-pack (lo + hi << 16):
let mask_lo: u32 = movemask_u8x16(interesting_lo) as u32;
let mask_hi: u32 = movemask_u8x16(interesting_hi) as u32;
let mask: u32    = mask_lo | (mask_hi << 16);
```

Per 32-byte block µop count: 2 LOAD + 8 CMP + 8 OR (4 OR-pairs ×
2-input each, or **4 EOR3 instructions under SHA3**: `veor3q_u8` does
3-way XOR; for OR we'd use `vorrq` chained, or `vbcaxq_u8` for the
"OR AND-NOT" interior of the predicate) + 2 MOVEMASK = ~20 µops per
32 bytes vs. 9 µops per 16 bytes (= 18 µops per 32 bytes equivalent).

**The widening is roughly µop-neutral per byte unless** the
`interesting`-class fold collapses *first-only* extraction work. The
real win lives in the consumer: under the `interesting_mask =
term|esc|ctl|non_ascii` shape, the *single* 32-bit mask carries one
`trailing_zeros` extract per "interesting" event, vs. the current
shape that produces four parallel 16-bit masks and computes
`interesting_mask = t|e|c|n` *inside the consumer* (per
`StringSpecialBlock::interesting_mask` at `string_block.rs:14-17`).
Fusing the OR-fold into the producer halves the mask-handling work in
the consumer.

### §4.3 — Material differential vs REDRESS 83 (StringBlock16 tiny probe)

REDRESS 83 rejected the W5 generated-retained StringBlock16 tiny
probe. The candidate added a JSON-specific 16-byte wrapper over
`scan_string_special_block` and wired it ONLY into the generated
retained `match_tiny_plain_string_with_cap::<16>` helper. The
falsifiability gate failed: zero of six named parse rows crossed
threshold, six regressed >3%. Per the REDRESS narrative the failure
mode was hot-leaf cost: "the existing AArch64 `string_block` movemask
shape is too expensive for the already-tiny generated retained
quote-pair probe."

Structural differential of the SK-V9 proposal:

1. **Target call site is different.** REDRESS 83 wired into
   `match_tiny_plain_string_with_cap::<16>` (the *tiny* path, 16-byte
   cap). SK-V9's widening targets `match_string_at_quote_trusted_utf8`
   (the *full* path, called on `unicode_mixed/escapes/gsoc-2018` once
   the 16-byte cap is exceeded). The TP table shows
   `match_string_at_quote_trusted_utf8` at 15-20% on the four LOSS
   rows, the wired consumer for the wider scan.
2. **The widening lives in the existing 16-byte primitive's
   *successor*, not a wrapper.** REDRESS 83 was a JSON-specific 16-byte
   wrapper layered on top; SK-V9's proposal is a 32-byte primitive
   replacing the 16-byte primitive at the producer site (or a
   `scan_string_special_block_32` variant called by the existing
   16-byte producer when span is long).
3. **Same-wave consumer is the existing
   `match_string_at_quote_trusted_utf8`, not a new wrapper.** No
   sidecar primitive, no parallel substrate (Lock 1 compliance per
   SC-6).

### §4.4 — Consumer-side CSSC CTZ admission gate

The `<u16>::trailing_zeros` 10.5% on gsoc-2018/t1 is the consumer-side
mask extract. Under `-C target-cpu=native + cssc`, LLVM emits a
single `ctz` instruction; under the production rustc baseline (which
may not detect FEAT_CSSC), it emits `rbit + clz`. SK-V7 A3 §2 names
the admission gate: `cargo asm` proof that the intended CTZ sequence
appears under target-cpu=native.

REDRESS 89 rejected the W10b CSSC CTZ body for
`bitmap_next_set_bit` on six-row regression (canada, citm_catalog,
instruments, marine_ik, mesh, numbers all dropped 3-8%). The
material differential for the SK-V9 P2 proposal:

1. **Different call site.** REDRESS 89's target was
   `bulk_emit_positions_64_neon` (the structural-scan bulk consumer).
   SK-V9's target is the string-block scanner consumer's per-mask
   first-set extract.
2. **Different failure profile.** The W10b failure was a 2-8% drop on
   *currently-winning* rows (`canada`, `mesh`, `numbers` — the
   numeric-token-heavy WIN block). SK-V9's proposal targets the LOSS
   rows; the winning rows are guarded by the falsification gate.
3. **Same-wave consumer is the union-substrate string-mask consumer**
   (P2-A scope), not the structural-scan bulk-emit pipeline. The W10b
   regression was scoped to the bitmap consumer; the string-mask
   consumer is a separate call site with its own µop budget.

---

## §5 — Dead-SIMD-scanner wiring

P1-V3-B §3.1 confirms `scan_structurals` is **0.00% self-time on every
(corpus, track)** — the SIMD structural index that bbnf-simd produces
is *discarded*. The recursive-descent parser re-derives every
structural byte. SK-V9 P2-A scope is the union substrate that
*consumes* `scan_structurals` as the substrate's primary write path
(per SC-3 / SC-6: "the structural projection IS the tape").

### §5.1 — What scan_structurals already implements

Per the file inventory at `bbnf-simd/src/aarch64/`, the implemented
primitive bodies are:
- `bitmap_next_set_bit.rs` — sparse mask first-set extract (delegates
  scalar; CSSC CTZ rejected W10b)
- `bitmap_prefix_xor_64.rs` — prefix-XOR (delegates scalar; PMULL
  rejected W10)
- `bulk_emit_positions_64.rs` — dense mask compress-store
- `byte_class_from_eq_set_64.rs` — equality-set membership (1-byte
  class via union-of-`vceqq_u8` chain)
- `byte_class_from_table_64.rs` — TBL-driven class lookup
- `classify_tbl4.rs` — 4-register `vqtbl4q_u8` classifier
- `string_block.rs` — 16-byte string-special-block scanner
- `unescape_uxxxx.rs` — TBL-driven `\uXXXX` decoder
- `digit_mac.rs` — DotProd-based digit MAC
- `utf8/` — UTF-8 validation

The aarch64 substrate has the **primitive bodies** for a branch-free
structural classifier. They are not wired.

### §5.2 — Structural bitmap chain (branch-free)

For each 64-byte chunk (four `vld1q_u8` loads):

```
chunk_0..3 = vld1q_u8(ptr + 0..16..32..48)

# Per-chunk class mask via 4-register TBL (existing classify_tbl4):
class_0    = vqtbl4q_u8(structural_table, chunk_0)
class_1    = vqtbl4q_u8(structural_table, chunk_1)
class_2    = vqtbl4q_u8(structural_table, chunk_2)
class_3    = vqtbl4q_u8(structural_table, chunk_3)

# Movemask each 16-byte class to 16 bits, concat to 64-bit:
mask_struct = concat4(movemask(class_0), movemask(class_1),
                      movemask(class_2), movemask(class_3))

# Quote/escape state resolution (REJECTED PMULL path; need alternative):
# - Compute escape_mask: backslash positions cancel escaped quotes
backslash_mask = concat4(movemask(vceqq(chunk_i, vdupq('\\'))))
# - quote_mask (raw '"' positions):
quote_mask = concat4(movemask(vceqq(chunk_i, vdupq('"'))))

# The hard step: turn quote_mask into "inside-string" mask.
# simdjson uses PMULL prefix-XOR. PMULL rejected here (REDRESS 88).
# Alternative paths in §5.3.

# Final structural mask (inside-string is masked off):
in_string  = derive_inside_string(quote_mask, backslash_mask)
structural = mask_struct & !in_string

# Bulk-emit positions via bulk_emit_positions_64 (already implemented)
```

### §5.3 — The PMULL alternative chain

PMULL prefix-XOR as the default body is REDRESS 88 — escape-heavy
JSON regressed 12-15% on the production benchmark even though the
SIMD-scan microbench was stable. The proposal here is therefore not
to re-admit PMULL but to design an alternative chain.

#### §5.3.1 — VEXT-based prefix mask (the original simdjson alternative)

Before VPCLMULQDQ was available on x86, simdjson used a Hamming-weight
parity computation via a 6-stage shift-XOR ladder. The aarch64
analogue is the scalar reference at
`bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs` — the body that PMULL
was replacing. Per REDRESS 88, *the scalar prefix-XOR is the production
default*. The chain (6 shifts + 6 XORs over the 64-bit mask) runs at
about 12 µops total per chunk and **passes** the JSON benchmark gate
on M5 Max. The PMULL rejection means: keep the scalar prefix-XOR.

The aarch64-specific consideration: SHA3 `veor3q_u8` collapses 2-stage
XOR chains to 1 µop. The scalar prefix-XOR is implemented over u64
words, so SHA3 doesn't apply directly. A *vector* prefix-XOR variant
over uint8x16_t (the byte-position mask in a vector register) admits
EOR3:

```
# Shift-XOR ladder over a 16-byte vector representing a 128-bit mask:
m1 = veor3q_u8(m0, vshrq_n_u8::<1>(m0), vshrq_n_u8::<2>(m0))  # 1 EOR3 vs 2 XOR
m2 = veor3q_u8(m1, vshrq_n_u8::<3>(m1), vshrq_n_u8::<4>(m1))
m3 = veor3q_u8(m2, vshrq_n_u8::<5>(m2), vshrq_n_u8::<6>(m2))
```

This is **3 EOR3 ops total** vs 6-stage XOR; reduction from ~12 µops
to ~6 µops. Material differential vs PMULL: this is a fold of the
existing scalar ladder via SHA3, not a substitution with a 64-bit
carryless multiply. The hot-leaf failure mode that broke PMULL on
`unicode_escapes` is the *PMULL retire latency on the M5 Max P-core*
(PMULL.1Q is reported as 4-cycle latency, 1/cycle throughput; the
SHA3 EOR3 is 1-cycle latency); the proposal here trades 6 cheap µops
for 3 1-cycle µops, monotonically faster.

#### §5.3.2 — AESE-based byte-class shuffle

`vaeseq_u8` (FEAT_AES) is the AES single-round S-box transform; on
Apple cores it's 1-cycle latency, 1/cycle throughput. The S-box is a
fixed 256-byte LUT (the AES Rijndael S-box) — *not* useful as a
generic byte class. But the `vaeseq_u8(input, zero_key)` shape is a
1-cycle byte-shuffle gadget; the S-box output, however, is the AES
substitution and bears no structured relation to JSON classes.

**Disposition: not useful for structural classification.** The
AES S-box is the wrong shape; we already have `vqtbl1q_u8` (a true
TBL primitive) at 1-cycle, which is the canonical 1-µop byte-class
LUT. AES is mentioned in SK-V7 A3 §2 as "repurposable for
hash/mixing" but not for class predicates. Rejected for §5.3 use.

#### §5.3.3 — VEXT for cross-chunk carry propagation

`vextq_u8(a, b, N)` concatenates two 16-byte registers and extracts a
16-byte window starting at byte N. The natural use:
**cross-chunk carry-in for the quote/escape state**.

The quote-mask computation requires that an opening `"` in chunk i
flows into chunk i+1 as "in-string state." The current scalar shape
uses a `bool carry_in` argument to `bitmap_prefix_xor_64_scalar`. The
aarch64 vector analogue: after computing the per-chunk quote mask,
the carry_in for chunk i+1 is `extract_high_bit(in_string_mask_i)` —
a `vextq_u8(in_string_i, vdupq_n_u8(0), 15)` extracts byte 15 of the
previous chunk into the lane-0 position of the next chunk's carry
register.

This is the standard simdjson cross-block-carry pattern and admits
under FEAT_NEON baseline (no extension needed). The material
differential vs REDRESS 88: this *replaces* the rejected PMULL chain
with a vector shift-XOR fold using SHA3 EOR3 (per §5.3.1) plus
VEXT-based carry (this section), neither of which is the PMULL body
that REDRESS 88 measured.

### §5.4 — The wiring is the same-wave consumer

Per SC-3 / SC-6, the union-substrate write path is:

```
byte input
  -> SIMD chunked scanner (this section)
  -> structural bitmap + quote-mask + escape-mask
  -> typed event cursor (consumes the bitmap)
  -> OffsetTape (the cursor's retained projection)
```

The wave-class change is **substrate**, not parser: the
recursive-descent parser is *replaced* by the cursor-over-bitmap
walker. Per Lock 1 §1.3 sentence "if structural offsets are retained,
the structural projection IS the tape" — the union-substrate
collapses the bitmap and the tape into one queryable object. This is
the *same-wave consumer* for `scan_structurals` that has been
"blocked_no_consumer" since SK-V3.

### §5.5 — Material differential against REDRESS 28 + 33

REDRESS 28 admitted the host aarch64 primitive kernels (Class A
`match_tiny_plain_string` NEON, Class B `unescape_uxxxx_neon`) but
*rejected* active 16-byte tiny-string dispatch — wiring the kernel
into Track 1/Track 2 regressed `twitter` ~25%. REDRESS 33 refined
the diagnosis: the kernel-versus-call-site mismatch (active call site
was the 8-byte scalar early-out at `bbnf-simd/src/lib.rs:195`, not the
B1-attributed hot kernel) is what blocked the parse-G close.

Material differential for SK-V9 P2-D §5:

1. **Different consumer.** REDRESS 28/33's consumer was the *parser*'s
   tiny-string dispatch — the 16-byte kernel ran *inside* the parser
   hot loop, adding µops to a path that was already short. SK-V9's
   consumer is the *union substrate*'s structural-bitmap producer,
   running in a *different* code path (the bitmap is consumed by the
   typed event cursor, not by the parser).
2. **Different falsification scope.** REDRESS 28's gate was
   `twitter` parse_only Track 1/Track 2. SK-V9's gate is the
   union-substrate's combined parse + tape-build path on the four
   uncloseable rows; the substrate's own per-row cost includes the
   bitmap-write cost which was not measured by REDRESS 28.

---

## §6 — DAV1D process discipline gaps

Per SK-V7 A3 §1 every primitive ships with: (1) scalar oracle, (2)
forced feature masks, (3) ABI-checked-call shims, (4) recoverable
fault handling, (5) cycle-counter source-binding. Item (1) is the
binding pre-wiring gate: a primitive without a scalar reference and a
checkasm differential is not admissible.

### §6.1 — Current checkasm coverage

Checkasm tests in `bbnf-simd/tests/`:
- `checkasm_bitmap_next_set_bit.rs`
- `checkasm_bitmap_prefix_xor_64.rs`
- `checkasm_bulk_emit_positions_64.rs`
- `checkasm_byte_class_from_eq_set_64.rs`
- `checkasm_byte_class_from_table_64.rs`
- `checkasm_eob_pad_clamp.rs`
- `checkasm_parity.rs`
- `checkasm_structural_terminator_64.rs`
- `checkasm_utf8_block.rs`

Plus per-arch fixture tests:
- `aarch64_primitives.rs`
- `classifier_parity.rs`
- `corpus_parity.rs`

### §6.2 — Primitives lacking a checkasm gate

Aarch64 primitives present in `bbnf-simd/src/aarch64/` that are NOT
covered by a same-named checkasm test:

| Primitive                                                       | Source                                            | Checkasm status            |
| --------------------------------------------------------------- | ------------------------------------------------- | -------------------------- |
| `match_tiny_plain_string_neon` + `_scalar`                      | `aarch64/match_tiny_plain_string.rs`              | partial — covered by `classifier_parity.rs`? (no `checkasm_match_tiny_plain_string.rs` file) |
| `unescape_uxxxx_neon` + `_scalar` + `unescape_uxxxx_x4_neon`     | `aarch64/unescape_uxxxx.rs`                       | **missing** — no `checkasm_unescape_uxxxx.rs` (per REDRESS 82 the W4 patch added one and was rejected with the patch) |
| `scan_string_special_block` + `_scalar`                         | `aarch64/string_block.rs`                         | **missing** — no `checkasm_string_block.rs` (REDRESS 83's wave added one and was rejected) |
| `digit_mac` (DotProd)                                           | `aarch64/digit_mac.rs`                            | partial — covered by `checkasm_byte_class_from_eq_set_64`? no — separate primitive |
| `byte_context`, `cache_hints`, `eob_pad_clamp`, `quad_load`     | `aarch64/byte_context.rs` etc.                    | mixed — `eob_pad_clamp` covered |
| `movemask::movemask_u8x16`                                      | `aarch64/movemask.rs`                             | **implicit** (used by every other primitive's mask path); no standalone differential |

**P1-V3-E (legacy cleanup audit) likely surfaces this** (file
referenced in the convergence audit). The SK-V9 P2-D admission gate
is: **before wiring any new primitive into a hot path, the primitive
ships a `checkasm_<name>.rs` differential test**. The Wave 1 admission
kernel `unescape_uxxxx_x4_neon` is currently in-tree but has neither
a checkasm test (the REDRESS 82 wave attempted one but was rejected
with the whole patch — the test went out with it) nor an alignment
sweep.

### §6.3 — The five-invariant gate (SK-V7 A3 §1, restated)

| Invariant                  | bbnf-simd status (aarch64)                                                                                |
| -------------------------- | --------------------------------------------------------------------------------------------------------- |
| 1. Scalar oracle           | Present for all aarch64 primitives. Source-of-truth at `scalar/*.rs`.                                     |
| 2. Forced feature masks    | **Missing**. `BBNF_SIMD_FORCE` / `BBNF_SIMD_MASK` env controls named in skv6-B2:241 not implemented.       |
| 3. ABI-checked-call shim   | **Insufficient**. Per SK-V7 A3 §1, the current `callee_saved_register_then` wrapper at `checkasm_parity.rs:159-167` wraps a Rust closure; the replacement spec `tests/checkasm_ffi_aarch64.S` (AAPCS64 with `x19-x28`, `d8-d15`, 16-byte alignment, stack canary) is not in tree. |
| 4. Recoverable fault       | **Insufficient**. Current `checkasm_parity.rs:743` signal path panics from the handler (not async-signal-safe). skv6-B2:217 specifies `sigaction` + `sigsetjmp` trampoline; not implemented. |
| 5. Cycle-counter source    | **Insufficient**. Current bbnf-simd uses `Instant`-based timings and converts assuming 3.5 GHz constant. skv6-B2:295 binds every reading to one of `x86_rdtsc`, `aarch64_cntvct`, `instant_ns`, or `external_perf`. |

**SK-V9 P2-D dependency:** the §3 and §4 primitives can be admitted on
the existing checkasm infrastructure IF they ship their per-primitive
checkasm test (gap §6.2) AND the union-substrate wave provides the
same-wave consumer. The fuller invariant 2-5 closure is SK-V10+ work
per the SK-V7 A3 §2 menu and skv6-B2; deferring those does **not**
block §3/§4 admission because the existing checkasm parity harness
covers correctness for the same-class primitives already in tree
(REDRESS 88 + 89 demonstrate the harness works for asm-body
differentials).

### §6.4 — The orphan-rejection rule

Per SK-V7 A3 §1 invariant 6 + skv6-A2:154: benchmark potential does
not lift status. Every primitive admission carries a same-wave
runtime/generated consumer. The four currently `blocked_no_consumer`
primitives (`BULK_EMIT_COMPRESSED`, `FRAME_PUSH_BOUNDED`,
`FRAME_POP_BOUNDED`, `FSM_DISPATCH_THREADED`) stay blocked through
SK-V9 P2 — they unlock only with a CollapsedStage codegen consumer,
which is out of scope.

The §3, §4, §5 admission paths each carry a same-wave consumer:
- §3 `escape_codec_hex_unit` x4 NEON: consumer is the union-substrate
  string-content materialiser at the tape-cell projection layer.
- §4 32-byte string-block scan: consumer is the existing
  `match_string_at_quote_trusted_utf8` (just wider blocks; not a new
  call site).
- §5 structural-bitmap chain: consumer is the union-substrate's typed
  event cursor (P2-A scope).

---

## §7 — REDRESS material differentials (synthesis)

| Rejected predecessor                                                                 | SK-V9 P2-D proposal                                                                          | Structural difference                                                                                                                       |
| ------------------------------------------------------------------------------------ | -------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- |
| REDRESS 28 (16-byte tiny-string dispatch into parser hot loop, twitter −25%)         | §5 structural-bitmap chain (consumer is union substrate, not parser hot loop)                | Different code path; the union substrate replaces the recursive-descent parser body, not adding work to it.                                  |
| REDRESS 33 (Class A `match_tiny_plain_string` NEON at wrong call site)               | §3 `escape_codec_hex_unit` x4 NEON (correct call site: `unescape_json_string` materialiser)  | Different primitive class; the diagnosis-vs-call-site mismatch that broke Class A is the explicit admission gate for §3.                    |
| REDRESS 50-55 (parse-time aux side tables, event cursors, sink-local decoded stats)  | §5 union substrate (structural projection IS the tape, not a sidecar)                        | No retained side table; no parallel substrate; Lock 1 §1.3 amendment per SC-6 binds the union as a single substrate.                        |
| REDRESS 60-62 (boundary collapse, always/delayed-wide retained trusted scan)         | §4 32-byte string-block scan (widens the producer, not the retained boundary)                | The widening is producer-side per-block, not a retained scanner; same-row gate applies but the structural shape is different (per-block widening replaces per-call-pattern). |
| REDRESS 64 (retained Unicode-escape run validator)                                   | §3 `escape_codec_hex_unit` x4 (per-quartet batched, not a retained validator)                | No validator state; the x4 NEON kernel is pure functional (no carry across `\uXXXX` runs except surrogate join, which is scalar postprocess). |
| REDRESS 66-69 (direct source-hook field-folding, parser-owned decoded scratch, byte-output unescape, DirectBuild semantic strings) | not proposed in §3-§5; SK-V9 P2 stays on parse_only + retained-tape plane            | Per-row diagnosis (§2) attributes the gap to `escape_codec_hex_unit` and `string_block_scan`, not direct-route shape. P1-V3-D §6.4 routes any further direct-plane work to REDRESS 93's dedicated tranche. |
| REDRESS 82 (W4 single-quartet Unicode escape classifier)                             | §3 x4 batched + union-substrate consumer (not per-quartet parser-owned)                      | Three axes per §3.5: x4 batching, union-substrate consumer (not parser), direct-route falsification gate added.                              |
| REDRESS 83 (W5 generated-retained StringBlock16 tiny probe)                          | §4 32-byte at `match_string_at_quote_trusted_utf8` (long-span path, not tiny path)            | Different call site (full-scan, not tiny-probe); widening replaces the producer at the call site, not a wrapper layered on the consumer.    |
| REDRESS 84 (W6 object-pair value-byte control compaction)                            | not proposed                                                                                  | n/a — control-boundary economics, not in §3-§5 scope.                                                                                       |
| REDRESS 88 (W10 PMULL prefix-XOR default body)                                       | §5.3.1 SHA3 EOR3 fold (not PMULL; vector shift-XOR ladder over uint8x16 with EOR3 collapse)  | Different intrinsic (EOR3 1-cycle vs PMULL 4-cycle); different primitive shape (vector ladder vs 64-bit carryless mult); the failure mode (PMULL retire latency on M5 Max P-core escape-heavy rows) is structurally inapplicable. |
| REDRESS 89 (W10b CSSC CTZ bulk consumer)                                             | §4.4 CSSC CTZ at string-mask consumer (not bitmap bulk-emit consumer)                        | Different call site (string-block scanner's first-set extract, not `bulk_emit_positions_64`); different failure profile (W10b regressed WIN rows; §4.4 targets LOSS rows under guard).                            |
| REDRESS 90 (W10c canary hardening admitted)                                          | not relevant — test-harness hardening only.                                                  | n/a.                                                                                                                                        |

---

## §8 — Sources

- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-CONVERGED.md` — convergence ledger; four uncloseable rows; substrate-shape findings.
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-B-xctrace-time-profiler.md` — per-symbol self-time, primitive class taxonomy, hot-leaf attribution (§2 per-row, §3.2 string-scanner pair verdict, §3.5 `escape_codec_hex_unit` class).
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-D-structural-breakdown.md` — OLS regression, R²=0.371, four uncloseable rows (§5.3 reduction table; §6.2 unicode-row finding).
- `restart/skinny/tranches/sk-v7/research/skv7-A3-dav1d-esoterica.md` — DAV1D process discipline (§1), ARMv9.2 host inventory (§2), 5-primitive grammar-neutral gap (§4), admissibility predicate (§5), admission gates (§8).
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md` — simdjson two-stage architecture (§1.1), asmjson DPDA shape (§1.2), sonic-rs single-pass (§1.3); substrate-ceiling hypothesis.
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md` — Lock 1 amendment, union substrate, "structural projection IS the tape" sentence (§1.3); Lock 14 binding.
- `skinny/REDRESS.md` entries 28, 33, 50-55 (pre-blocks for cursors and parser-local SIMD), 60-62 (string-scanner widening), 64 (retained Unicode validator), 66-69 (direct-string materialisers), 82 (W4 unicode quartet classifier), 83 (W5 StringBlock16 tiny probe), 84 (W6 control compaction), 88 (W10 PMULL default body), 89 (W10b CSSC CTZ bulk consumer), 90 (W10c canary).
- `skinny/crates/bbnf-simd/src/aarch64/` — existing aarch64 primitive bodies (file list per §1.3 and §5.1).
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:74` (`unescape_uxxxx_neon`); `:125` (`unescape_uxxxx_x4_neon`); `:201` (`HEX_NIBBLE_LUT`).
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:57` (`scan_string_special_block`); `:14-17` (`interesting_mask`).
- `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:79` (`match_tiny_plain_string_neon` — low-6 TBL shape referenced in §3.6).
- `skinny/crates/parse-that-regex/src/lib.rs:284` (`validate_string_escape`); `:162` (`match_string_at_quote_trusted_utf8`); `:945` (`read_hex_unit_scalar`); `:959` (`hex_nibble`); `:547` (`skip_string_plain_trusted`).
- `restart/locks/LOCKS.md` Lock 1 (substrate union); Lock 14 (grammar generalisation); Lock 16 (grammar-neutral primitive surface).

End of report.
