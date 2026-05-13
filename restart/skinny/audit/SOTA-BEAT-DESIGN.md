# SOTA-BEAT-DESIGN — Structural-Index-Driven Codegen with SIMD Primitive Layer

Status: LIVE (post-Wave-2 redress, SK-V3 Wave 0/1 re-assay, 2026-05-12). Aligned with `GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md`. The sidecar structural-index *prepass* shape is rejected; the retained tape projection IS the structural index (per SK-V3 §3 and Lock 1 clarification). What remains live in this document: the structural-index-driven codegen *lowering shape* (cursor over `offsets[]`, single indexed byte read per dispatch, no whitespace re-scan), generated `SinkOnly` typed emission for direct-only APIs, the `bbnf-simd` primitive crate and its Lock-16-admissible kernel catalog, the AVX-512 esoteric-stack-on-asmjson story, and the Phase 0–4 falsifiability ladder — all rebased on Wave 2's two-pathology-class diagnosis (Class A `tiny_string_loop`, Class B `hex_decode`) plus the new `N-direct` finding.

The expanded corpus in `skinny/RESULTS.md` is the binding gate. The current SK-V3 Wave 0/1 run classifies parse/tape rows as G / NoGo on `twitter`, `random`, `unicode_mixed`, and `unicode_basic`; it classifies several other rows as A / GO or C / GO, so the substrate is shape-sensitive rather than uniformly refuted. The overall verdict is `N-direct / NoGo`: sink-only direct-to-struct correctness passes and no longer pays retained-view traversal in the timed rows. After removing duplicate UTF-8 validation and moving integer/non-integer classification into the scanner result, 6 of 17 direct workload rows pass the 1.10x sonic-rs time slack and 11 remain slower. The architecture is not closed by primitive admission alone; it needs real event-cursor consumption for parse rows and direct sink materialization work for exact float/string/Unicode-heavy rows.

Created: 2026-05-12 after V9.2 conditional refutation and six-agent comparative-profile cohort.
Anchor: this document is the executable architectural target for SOTA-BEAT against `sonic-rs` (2.32 GB/s twitter LazyValue), `simdjson` (~3.0 GB/s twitter DOM), and `yyjson` (~3.7 GB/s twitter scalar+force-inline) on the expanded corpus. `arm64` Apple Silicon is the primary host and gating environment; `x86_64` AVX-512 is the secondary acceleration target where asmjson territory (10.93 GiB/s DOM, Zen 4) is the aspirational ceiling.

## §1. Empirical premise

The earlier Wave 1 framing posited 5+ co-equal hot leaves bolted onto a sidecar SIMD scan. Wave 2 (per-corpus asm pathology + PMU/i-cache analysis + native sidecar comparison; `skinny/profile/wave2-asm/PROFILE-REPORT.md`, `skinny/profile/wave2-pmu/PMU-REPORT.md`, `skinny/profile/native-sidecars/PROFILE-REPORT.md`, `skinny/profile/reprofile-2026-05-12/`) collapsed the parse picture to **two distinct pathology classes inside one fused leaf** plus a credible win column. The SK-V3 Wave 0/1 re-assay adds a separate blocker: the first sink-only direct parser closes the view-walk penalty but still loses on numeric/string/Unicode materialization. The failing parse corpora are localized implementation gaps, while the direct workload requires `SinkOnly` to use the same primitive-quality string and number leaves as the parse path.

**Single dominant leaf**: `runtime::generated_json::generated::parse_value_at` carries 85.5–99.2% of whole-program self-time across every failing corpus (Wave 2 PMU §1.1). LTO has fused structural_scan / string_decode / view_material entirely into this 7,304-byte body (RVA 0x2460–0x40e8; PMU §1.5). The hot-leaf-count gate is already met — there is one leaf, not five. The remaining work is *inside* that leaf, not adding new ones.

**Pathology Class A — `tiny_string_loop` (3/5 failing corpora: github_events, update-center, random)**: the scalar 8-byte `match_tiny_plain_string` body at `crates/runtime/src/grammars/json/generated.rs:161-172` is inlined twice into `parse_value_at` (the key path at PC offset 0x02d4, RVA 0x2734; and the value path at PC offset 0x0cf8, RVA 0x3158), each iteration doing `ldrb / cmp #0x22 / b.eq / cmp #0x5c / b.eq / cmp #0x20 / b.hs` — four dependent ops per byte with no parallelism. Aggregate self-time on this band reaches 32.8%+17.3% = 50.1% on github_events, 35.5%+24.4% = 59.9% on update-center, 31.0%+20.5% = 51.5% on random (Wave 2 asm §(b.2)).

**Pathology Class B — `hex_decode` (2/5 failing corpora: unicode_escapes, y_string_unicode)**: the scalar `\uXXXX` decoder in `parse_that_regex`'s `unescape_json_string` materialises into `parse_value_at` as a `sub/csel/orr/lsl` cluster repeated × 4 nibbles, each nibble carrying a `cmp w15, #0xf` boundary check. Self-time: 22.2% on unicode_escapes, 13.9% on y_string_unicode (Wave 2 asm §(b.2)); whole-program mnemonic share `orr` 20.42% / `sub` 18.20% on unicode_escapes corroborates the decode-cluster signature.

**Credible win column** (current `skinny/RESULTS.md`, Mbps): skinny Track 1 / Track 2 already beat sonic-rs on `citm_catalog` (29185 / 29401 vs 24910), `canada` (16975 / 16675 vs 12658), `github_events` (25332 / 25794 vs 22182), `numbers` (19195 / 19050 vs 13567), and several other corpus shapes. The hard parse losses are `twitter` (16294 / 16068 vs 20810), `random` (7770 / 7677 vs 15370), `unicode_mixed` (7384 / 7300 vs 15892), and `unicode_basic` (6561 / 6889 vs 13304). The sink-only direct path now beats the old retained-view digest and passes `citm_catalog`, `apache_builds`, `github_events`, `update_center`, `instruments`, and `distinct_values`; 11 rows still miss sonic-rs direct, with float-heavy and Unicode-heavy rows the largest misses.

**Rejected routes** (REDRESS items 16-18, 25; SK-V3 synthesis §5): dispatch-table/function-pointer alternates, 12-byte/width churn, pair-token fusion, structural-index typed-parser **sidecar prepass** (the sidecar shape is invalidated; the retained tape projection IS the structural index per `GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md` §3), NEON no-escape string matcher, separator elision, generic SWAR whitespace skipper. Lazy-offset tape stands as the validated triad-and-credible-win substrate.

## §2. The architectural shape — structural-index-driven typed parse

The codegen template the generated parser must emit reads as follows. Scan stage produces a contiguous `offsets: &[u32]` indexing the input at every structural byte (whitespace already skipped at scan time); a parallel `flags: &[u8]` carries per-string `HasEsc` bits and other lazy-decode hints. Generated `parse_*` functions consume offsets via a `cursor: u32` advancing through the offset array; the source `&[u8]` is read only at `source[offsets[cursor] as usize]` and only inside `parse_string` and `parse_number` for the primitive's own bytes. Whitespace and structural delimiters are never re-scanned.

The contract reads:

```rust
fn parse_value(
    source: &[u8],
    offsets: &[u32],
    flags: &[u8],
    cursor: &mut u32,
    arena: &Arena,
) -> Result<ValueRef<'_, '_, V>, ParseError> {
    // ONE indexed byte read per dispatch — no skip_ws, no peek.
    let b = source[offsets[*cursor as usize] as usize];
    match b {
        b'{' => parse_object(source, offsets, flags, cursor, arena),
        b'[' => parse_array(source, offsets, flags, cursor, arena),
        b'"' => parse_string(source, offsets, flags, cursor, arena),
        b'-' | b'0'..=b'9' => parse_number(source, offsets, cursor, arena),
        b't' | b'f' | b'n' => parse_literal(source, offsets, cursor),
        _ => Err(ParseError::Unexpected(b)),
    }
}
```

Object and array bodies advance the cursor through the offset array; the loop terminator is detected by `source[offsets[cursor]]` matching `}` or `]`. String parse uses the next offset as the closing-quote position (computed at scan time); when `flags[cursor] & HAS_ESC == 0` the bytes between are borrowed directly without an escape-decode pass. Number parse uses the next offset minus current offset as the byte length.

This shape is what simdjson's stage2 already does (`stringparsing::parse_string`, `parse_digit`, `parse_decimal_after_separator` only touch source bytes inside primitives) and what sonic-rs achieves via LTO fusion (all SIMD primitives inline into the one `parse_object`/`parse_array` driver). Our skinny lacks both fusion and structural-index consumption; this design adopts the simdjson route mechanically and the sonic-rs route via Lock 15 (build-profile discipline).

The composition with the existing Lock 1 substrate is additive: `Tape<'input>` remains the eager canonical owner of the offset array, payload arena, and (for non-JSON grammars) the eager token stream. `ValueRef<'doc, 'input, K>` gains a `cursor: u32` field (already implied by `LAZY-TAPE-DESIGN.md` §3.3 for the lazy mode; here it is the canonical access path for *all* grammars on the structural-index route). The `DocumentView` trait shape is unchanged. No new substrate variant. No parallel substrate. Lock 1 stands.

## §3. SIMD primitive layer — `bbnf-simd` crate

The current skinny structural scan is co-located with the JSON grammar runtime. The structural-index-driven design separates this into a backend-agnostic primitive crate so multiple grammars and multiple SIMD targets compose. Per `feedback_no_god_modules` + `feedback_general_infra_crates` + `feedback_regex_generalized` (general-purpose constructs in their own crates).

### §3.1. Crate shape

```
crates/bbnf-simd/
├── src/
│   ├── lib.rs                — public surface: trait SimdClassifier, fn select_classifier()
│   ├── classifier.rs         — trait SimdClassifier { fn classify_chunk(&self, bytes: &[u8; 64]) -> ClassifyResult; ... }
│   ├── aarch64/              — primary host path
│   │   ├── mod.rs
│   │   ├── classify_tbl4.rs  — vqtbl4q_u8 4-table 64-byte classifier
│   │   ├── movemask.rs       — vshrn_n_u16 + vsri + zip1 (Validark interleaved)
│   │   ├── string_block.rs   — NEON quote/escape bitmask with HasEsc flag
│   │   └── prefetch.rs       — PRFM PLDL1KEEP / PLDL2STRM tuning (asm!)
│   ├── x86_64/               — secondary acceleration
│   │   ├── mod.rs
│   │   ├── avx512_vbmi2/     — Ice Lake+ / Zen 4+ path
│   │   │   ├── classify.rs   — vpermi2b 128-byte shuffle
│   │   │   ├── compress.rs   — vpcompressb one-shot offset emit
│   │   │   ├── mask_fuse.rs  — vpternlogd 3-input boolean fusion
│   │   │   └── carry.rs      — _mm512_alignr_epi8 cross-window
│   │   ├── avx2/             — Haswell+ / Zen 1+ fallback
│   │   │   ├── classify.rs   — _mm256_shuffle_epi8
│   │   │   ├── bmi2_emit.rs  — _pdep_u64 bits-to-indexes
│   │   │   └── prefix_xor.rs — _mm_clmulepi64_si128 string-bitmap
│   │   └── dispatch.rs       — CPUID-based selection at parser construction
│   └── scalar/               — SWAR fallback for portability
│       ├── mod.rs
│       └── swar_8byte.rs     — 8-byte chunked classify (asmjson #8 lineage; ~7 GB/s ceiling)
└── tests/
    ├── classifier_parity.rs  — exhaustive 256-byte-value parity across all 3 targets
    └── corpus_parity.rs      — twitter/citm/canada parity vs scalar reference
```

`crates/runtime/` consumes `bbnf-simd` via `Box<dyn SimdClassifier + Send + Sync>` or a generic-typed direct dependency depending on what the cost model in `crates/codegen/` selects per grammar.

### §3.2. arm64 NEON primary path (M-series Apple Silicon — the host)

This is the gating implementation; M-series is the dev box and the SOTA-BEAT gate runs here. The intrinsic catalog (Lock 16 allowlist; per the 2026-05-12 dav1d/ffmpeg/VLC ASM monolith research, the byte-classification + cross-lane permute + multiply-accumulate primitives that translate from dav1d kernel patterns to byte-stream JSON parsing):

| Primitive | Intrinsic | Source citation | Replaces | Abstract primitive (generalizes to ANY grammar) |
|---|---|---|---|---|
| 4-table 64-byte structural+whitespace+escape classify | `vqtbl4q_u8` | Lemire 2019 "Arbitrary byte-to-byte maps using ARM NEON" | sonic-rs's 1-table `vqtbl1q_u8` (saves ~16 c/64B per intrinsics agent) | Chunk-parallel byte classification with per-grammar alphabet LUT |
| Interleaved-vector movemask | `vld4q_u8` + `vshrn_n_u16` + `vsriq_n_u8` + `vzip1q_u8` | validark.dev/posts/interleaved-vectors-on-arm/ (Validark 2024) | sonic-rs's AND-OR tree (4× faster bitmap synthesis) | Mask reduction from N-byte chunks to 64-bit bitmap |
| Quad-load 64 bytes | `vld1q_u8_x4` | Arm A64 ISA | 4× separate `vld1q_u8` (frees 2 load-ports on M-series) | Single-instruction wide load |
| Cross-lane byte-shift extract | `vextq_u8` | Arm A64 ISA; **dav1d filter-overlap lineage** (loop-filter kernels heavily use this) | explicit prev-byte copy + shift | **1D sliding-window byte context** — cross-chunk quote-state, brace-depth carry, comment-state, RCDATA-state. Applies to ANY grammar with stateful chunk-spanning tokens (XML, YAML, SQL, Markdown). |
| 4-byte multiply-accumulate | `udot` / `sdot` | Arm A64 ISA Armv8.2-A; **dav1d FIR-filter MAC lineage** | scalar digit-block accumulation | **Multiply-accumulate over byte windows** — digit-block parsing (JSON `number`, CSS `<number>`, TOML/INI/SQL integer literals, Sheets formulas, BBNF `digit`). Applies to ANY grammar's number primitive. |
| Saturating add/sub | `vqaddq_u8` / `vqsubq_u8` | Arm A64 ISA | branch-on-overflow | **Branchless overflow-clamped accumulation** — i64 fast-path with deferred-decode flag set on overflow. Applies to ANY grammar's number primitive. |
| Branchless mask select | `vbslq_u8` | Arm A64 ISA | conditional emit/branch (used in `string_block.rs`) | Branchless predicate select |
| Byte popcount | `vcntq_u8` + `vaddvq_u8` | Arm A64 ISA | scalar `count_ones()` (saves GPR round-trip) | Chunk-level "any-true" / first-true reduction (any grammar's whitespace-skip optimization for whitespace-empty chunks) |
| Non-temporal pair-store | `STNP` (asm!) | kernel `clear_page` lineage; `feedback_no_polling_loops` not applicable here | normal cached store (write-allocate pollutes L1) | Tape-stream write-only: prevents L1/L2 eviction of input on inputs > L1 (3-8% cold-cache gain). |
| Streaming prefetch | `PRFM PLDL2STRM` (asm!) | Arm A64 ISA | generic `prefetch_read_data` | Tape walker (consumer) prefetch ahead-of-cursor for sequential offset stream. |

**Abstract primitive philosophy**: dav1d's pixel-arithmetic kernels (motion compensation, IDCT, loop filter, film grain — T14-T17 of the catalog at `skinny/profile/asm-string-unicode/`'s referenced dav1d-research) do NOT translate to JSON. But the *primitive operations* underneath them DO translate. The per-grammar selection is cost-model-derived from Grammar IR — alphabet size (≤16: `vqtbl1q_u8`; ≤64: `vqtbl4q_u8`; >64: SWAR or AVX-512 `vpermi2b`), number-token presence (triggers `udot`/`sdot`), string-token presence (triggers `vbslq_u8` + StringBlock), chunk-spanning-token presence (triggers `vextq_u8`). **No grammar-specific code in any generic crate** — Lock 14 verbatim.

The classifier kernel composes (1) + (3) into a single 64-byte block consumption per loop iteration. Quote/escape detection uses (1) + (7) with the `HasEsc` flag emitted into the parallel `flags` array. Movemask synthesis uses (2). Cross-chunk state uses (4). Number-block MAC uses (5)+(6).

#### §3.2.1. Class A kernel: NEON 16-byte `match_tiny_plain_string` scan

The current `match_tiny_plain_string` body at `crates/runtime/src/grammars/json/generated.rs:161-172` walks one byte per loop with four dependent compares (`cmp #0x22 / cmp #0x5c / cmp #0x20`) plus boundary check. Replace with a single 16-byte NEON pass per iteration:

1. `vld1q_u8` load 16 bytes from `source[cursor..]`.
2. `vqtbl4q_u8` against the JSON-string-body alphabet LUT (quote=0x22, backslash=0x5c, control < 0x20) — primitive (1) of the §3.2 catalog.
3. `vshrn_n_u16<4>` + `vorrq_u8` to compress lane match bits into a 64-bit movemask — primitive (2).
4. `rbit + clz` on the movemask (already present in current scalar tail; reuse) to locate the first interesting byte position.
5. Advance cursor by `tz_count` (or 16 if mask is zero) and re-enter.

The bulk-string body (96.5% of unicode-mixed input bytes per Wave 2 PMU §1.4; 70-83% of bytes for github_events / update_center) runs at 16 bytes per loop iteration instead of 1 byte per 4-op dependency chain. Projected throughput gain: 50–60% on Class A corpora (PMU §3 verdict). Each operation is already in the Lock 16 allowlist; no new primitive admission required. Citation: Wave 2 PMU §3.3 + Wave 2 asm §(c) "Fix 1 (materialize structural mask)".

#### §3.2.2. Class B kernel: NEON TBL-driven `\uXXXX` hex decoder

The current scalar `\uXXXX` decoder runs `(c >= b'0' && c <= b'9') ? c - b'0' : (c | 0x20) - b'a' + 10` per nibble, lowered as `sub w0, w0, #0x22 / sub w7, w4, #0x61 / csel w6, w24, w6, lo / orr w4, w4, w6 / lsl w4, w5, #8` — 11 dependent µops per nibble, 44 per `\uXXXX` sequence (Wave 2 asm §(b) mnemonic histogram: `csel` 16.25% on y_string_unicode).

Replace with a 16-byte LUT + shuffle (constant LUT `[0..9 at 0x30..0x39, a..f at 0x41..0x46 and 0x61..0x66, 0xff elsewhere]` stored as four `uint8x16x4_t` tables):

1. `vld1q_u8` load 16 bytes (covers four consecutive `\uXXXX` sequences or one with surrounding bytes).
2. `vqtbl4q_u8` against the hex-digit LUT — single µop maps ASCII hex character → 4-bit nibble value, with 0xff for non-hex.
3. `vshlq_n_u8` + `vorrq_u8` to pack `(hi << 4) | lo` byte pairs in-lane.
4. `vsri_n_u16` / `vzip1q_u8` to gather the four nibbles per quartet into the final u16 codepoint.

Three µops per nibble (load + table + pack) vs eleven scalar µops — projected 2–3× speedup on Class B corpora. The decoded u16 then flows to the existing UTF-8 emit path unchanged. Citation: Wave 2 asm §(c) "Fix 4 + dedicated NEON \uXXXX decoder"; Wave 2 PMU §3.3 (string-content-dominated corpora, `\u` share 61.3% of escapes on unicode_escapes).

Both kernels admit under Lock 16 row "arm64 NEON byte classify" + "arm64 NEON movemask" + "arm64 NEON loads + shifts" — no new lock entries. Both gate through the checkasm harness (§6) before landing.

Original projected impact (intrinsics agent + cycle-budget math, M5 Max): scan stage budget falls from current ~2.08 c/B (skinny twitter 41% of 5.07) to ~0.9 c/B (approaching yyjson's no-SIMD 0.91 c/B equivalent). Wave 0/1 supersedes the close claim: Class A/B primitives are admitted under strict checkasm, but the active 16-byte tiny-string parser route regressed `twitter`, so event-cursor / `parse_value_at` work is now the measured next target.

### §3.3. x86_64 AVX-512 secondary path (Ice Lake+ / Sapphire Rapids+ / Zen 4+)

The path **past asmjson** (not just past simdjson) on commodity Intel and AMD.
Verified by Wave 1 Agent 1 + Agent 3 (2026-05-12): asmjson's actual AVX-512
instruction footprint is minimal — only `vpcmpeqb`, `kmovq`, `vpcmpub`,
`korq`, `vmovdqu8`, and `tzcnt` in the inspected hot corpus. **Zero
`vpternlogq`, `vpclmulqdq`, `vpcompressb`, `vgf2p8affineqb`, `vpermb`,
`vpermt2b`, `vpmadd52`, `vpopcntb`, `vaes`, `movdir*`.** asmjson's 10.93
GiB/s comes from architecture (a 9-state DPDA finite-control fragment plus
bounded explicit stack, PC-as-state via `r10`, `tzcnt` seek, and EOB padding),
not from esoterica. Esoteric instructions are an x86 successor-tranche route,
not an SK-V3/SK-V4 M5 Max close condition.

The x86 strategy stacks: **adopt asmjson's architecture only where the
`CollapsedStage` admissibility predicate is satisfied** (DPDA finite control,
PC-as-state, `tzcnt` seek, EOB pad, bounded frame stack) AND **add esoteric
primitives strictly on top** as additive improvements. Each esoteric primitive
has a concrete "what asmjson does today" + "what the esoteric op replaces":

**Baseline (asmjson's primitives, adopted verbatim)**:

| Primitive | Intrinsic | Source citation | asmjson uses? |
|---|---|---|---|
| Byte-equality classify | `vpcmpeqb` | AVX-512BW | YES — 10× per chunk |
| Whitespace ≤ 0x20 | `vpcmpub` (cmp imm=2 ≤) | AVX-512BW | YES — 2× per chunk |
| Mask-OR fusion | `korq` | AVX-512F | YES — 6× per chunk |
| Mask → GPR | `kmovq` | AVX-512F | YES — 10× per chunk |
| Zero-masked load | `vmovdqu8 {k1}{z}` | AVX-512BW | YES — 2× per chunk (tail handling) |
| Next-event seek | `tzcnt` | BMI1 | YES — 18× per chunk |

**Esoteric additions (strict improvements over asmjson; each row names what asmjson does + what we replace it with)**:

| Esoteric primitive | What asmjson does today | What esoterica replaces it with | Gain | ISA gate |
|---|---|---|---|---|
| **GFNI `vgf2p8affineqb`** | 6× separate `vpcmpeqb` to classify `{}[],:` structural set + 3× `korq` to fuse (`parse_json_zmm_dom.S:275-284`) | Single µop classifies arbitrary 8-bit byte set via 8×8 GF(2) matrix encoding; structural alphabet = 6-byte set → single µop replaces 9 µops | ~3-4× elegant + measurably faster classify | AVX-512 GFNI (Ice Lake+, Zen 4+, Tremont) |
| **k-mask arithmetic family** (`_kandn_mask64`, `_kxor_mask64`, `_kxnor_mask64`, `_kshiftrq`, `_ktestq`) | 4× `mov [rbp+LOC_*], rax` stack-spill mask registers per chunk (`parse_json_zmm_dom.S:286-293`); GPR ping-pong via `kmovq` → integer-or → `kmovq` | Keep masks in k0..k7 across state transitions; spill only at EOB; merge in k-file (~1 cycle each, p0) | ~4 store+load eliminated per chunk = ~4 cycles | AVX-512F (Skylake-X+, Zen 4+) |
| **AVX-512 VPCLMULQDQ at 512-bit lane** | `cmp + branch on backslash` per byte inside string (no prefix-XOR primitive) | Adopt simdjson's prefix-XOR string-bitmap primitive at 4× lane width vs simdjson's 128-bit `_mm_clmulepi64_si128`; CRC-32C kernels measure 4× speedup at this width vs SSE4.2 | 4× throughput on string-bitmap computation; simdjson hasn't even adopted this 512-bit width upgrade | AVX-512 VPCLMULQDQ (Ice Lake+, Zen 3+) |
| **AVX-IFMA `vpmadd52luq`/`vpmadd52huq`** | Dispatches number tokens to a Rust `JsonWriter` vtable (zero number-parse asm code in asmjson) | 52-bit integer FMA for Eisel-Lemire fast-float mantissa multiplication; mantissa-mul stays in vector lanes; returns f64 directly without scalar callback | ~3× on number-heavy corpora (canada, mesh, marine_ik, numbers); pure win against asmjson which gives up on number parse | AVX-IFMA (Sapphire Rapids+, Zen 4+) |
| **AVX-512 VNNI `vpdpbusd`** | Same — no number-parse asm | Byte×byte→i32 dot-product, 4 bytes per i32 lane; 16-digit chunk → 4 lanes of `(d3*1000 + d2*100 + d1*10 + d0)` via one dot product. Lemire 2023 demonstrates exact pattern | ~3× scalar digit-block accumulation | AVX-512 VNNI (Cascade Lake+, Zen 4+) |
| **AVX-512 BITALG `vpshufbitqmb` + `vpopcntb`** | Per-state re-classification: each state re-runs `vpcmpub` + 7× `vpcmpeqb` over the same 64-byte chunk | Bit-gather 8 selected bits per 64-bit lane into k-mask in one µop (inverse of vpcompressb); per-state classification map becomes data, not code; per-byte popcount predicts branch density | One-µop multi-class classify; replaces `vptestmb + vpermb + vpmovb2m` triples | AVX-512 BITALG (Ice Lake+, Zen 4+) |
| **VBMI2 `_mm512_mask_compressstoreu_epi8`** | Scalar `tzcnt + shr + advance + cmp` per state hop to seek next interesting byte (`parse_json_zmm_dom.S:540-578`) | One-shot offset emission: emit structural-byte offsets from mask + index vector in one µop; eliminates per-byte scalar loop entirely | ~25 c/64B saved over tzcnt loop; simdjson explicitly leaves `vpcompressb` unused at `icelake/simd.h:157` for portability | AVX-512 VBMI2 (Ice Lake+, Zen 4+) |
| **VBMI2 `_mm512_ternarylogic_epi64`** | 3-way mask-OR via 3× `korq` per chunk | Single µop 3-input boolean (arbitrary truth table); collapses `(in-string ∧ ¬escaped) ∧ structural` to 1 µop | ~2-3 µops/chunk saved | AVX-512F (Skylake-SP+, Zen 4+) |
| **VBMI `vpermi2b` 128-byte byte-shuffle** | Limited 16-byte alphabet (per-state byte set ≤ 7); no path for grammars with larger alphabet | 128-byte byte-shuffle: arbitrary byte-class lookup table up to 128 bytes per pass; supports grammars with >7-byte first-set | Enables alphabet expansion past asmjson's design limit | AVX-512 VBMI (Ice Lake+, Zen 4+) |
| **`_mm512_alignr_epi8`** | Manual prev-bit propagation via shift + `bts` for cross-window quote-state carry | Cross-window byte-shifted concat in one instruction | ~2 cycles/chunk saved | AVX-512BW (Skylake-SP+) |
| **AVX-2 + BMI2 fallback** (`_pdep_u64`, `_pext_u64`, `_mm256_shuffle_epi8`, `_mm_clmulepi64_si128`) | asmjson is AVX-512 only; no AVX-2 fallback path | Bits-to-indexes (Mula 2018), AVX-2 classifier, 128-bit CLMUL prefix-XOR — non-VBMI2 host fallback (Haswell+, Zen 1+); gate Zen 1/2 slow-PEXT via CPUID | Enables non-AVX-512 host coverage | AVX-2 + BMI2 |
| **SWAR scalar fallback** | asmjson SWAR fallback at ~7 GiB/s per its docs | Same; vendored as `bbnf-simd/scalar/swar_8byte.rs`; ensures portability beyond AVX-2 | Correctness floor + ARM cross-platform | none |

Projected impact (cycle-budget math on Zen 4, applying esoteric stack on asmjson architecture; successor-tranche target only):
- asmjson baseline: 0.36 c/byte at 4 GHz (= 10.93 GiB/s)
- Add GFNI single-µop classify (replaces 9 µops): −0.05 c/B
- Add k-mask arithmetic (saves 4 cycles/chunk = 0.0625 c/B): −0.06 c/B
- Add AVX-IFMA + VNNI on number-heavy corpora: −0.10 c/B (canada-shape)
- Add VPCLMULQDQ-512 prefix-XOR + VBMI2 `vpcompressb` offset emit: −0.04 c/B
- Total target: ~0.11 c/B = **~14.0 GiB/s twitter on Zen 4** (= 1.28× asmjson), accepted only after equivalent-hardware measurement.

The path is: **admit a per-grammar `CollapsedStage` DPDA when all predicates
are satisfied + stack esoterica on top**. Each esoteric primitive is
independently falsifiable via the `bbnf-checkasm` differential harness
(Wave 1 Agent 2 + Wave 2 Agent 5); Lock 16 admissibility allowlist names each
row with citation + asmjson-doesn't-use evidence.

### §3.4. SWAR scalar fallback

For portability — non-SIMD hosts (rare in 2026+ but real: RISC-V hosts without RVV, embedded ARM without NEON, x86_64 without AVX-2). Implementation lineage: asmjson SWAR path at ~7 GB/s on commodity hosts; technique #8 from the DAVID/asmjson catalog.

```
swar_8byte.rs:
  fn classify_chunk_swar(bytes: &[u8; 8]) -> u8 {
      let word = u64::from_le_bytes(*bytes);
      let ws_mask = (word.wrapping_sub(0x2020202020202020)) >> 7;
      let quote_mask = word ^ 0x2222222222222222;
      ...
  }
```

This is the *correctness floor* — every grammar must parse correctly on every host even when no SIMD primitive is available. The portability story is corroborated by Wave 2 native-sidecar measurement: asmjson's M5 Max **arm64 native SWAR** baseline lands at **3315 / 2447 MiB/s on `string_array` / `string_object` synthetic corpora** (per `skinny/profile/native-sidecars/asmjson/NOTE.md`, native `cargo bench`). Skinny v3's twitter at 2631 MiB/s sits at ~80% of asmjson's SWAR ceiling on the same machine — comparable, not order-of-magnitude apart — confirming the SWAR-as-floor design is sound and the M-series scalar pipeline (~3.5 GHz integer width) bounds both implementations to the same plane. The NEON kernels in §3.2 lift skinny above this floor on Class A and Class B corpora.

## §4. Codegen template contract (lowering pattern; no new BIR variant)

**2026-05-12 amendment, corrected 2026-05-13: no new construct.** The
earlier draft proposed a `BirNode::CursorDispatch` variant. That was a
contrivance per the user's "no new directives, no contrivances" constraint.
The clean design is: the existing `Alt { mode: Dispatch }` BIR variant lowers
according to `LayoutFacts.backend_shape[rule_id]` (cost-model-derived per Lock
10 auto-detect; see ARCH §7.3). Same BIR; five materialization/access shapes.
No alphabet change.

### §4.1. Lowering matrix (one BIR variant, five access patterns)

```rust
// At crates/codegen/src/lower/rust.rs, when lowering Alt { mode: Dispatch }:
match layout_facts.backend_shape[rule_id] {
    BackendShape::EagerTape => {
        // emit: match source[pos] { byte_a => arm_a, byte_b => arm_b, _ => fallthrough }
        //       pos += consumed_bytes
        // Selected when: rule body or transitive uses include @error(recover) / @host fn
        //                parse-time-decoded / @layout scope, OR first-set has overlap.
    }
    BackendShape::OffsetTape => {
        // emit: match source[offsets[*cursor as usize] as usize] {
        //           byte_a => arm_a, byte_b => arm_b, _ => fallthrough
        //       }
        //       *cursor += 1
        // Selected when: byte-finite disjoint first-set; no payload-bearing tokens;
        //                no layout scope.
    }
    BackendShape::EventTape => {
        // emit: match event.byte() { byte_a => arm_a, byte_b => arm_b, _ => fallthrough }
        //       event.advance()
        // Selected when payload/recovery/layout side facts must be retained per cursor.
    }
    BackendShape::SinkOnly => {
        // emit: direct typed field writes during parse; no retained document identity.
        // Selected when the public API is direct-only and requires no post-parse
        // path/value traversal.
    }
    BackendShape::CollapsedStage => {
        // emit: asmjson-class AVX-512 VBMI2 DPDA with PC-as-state direct
        //       threading via r10 (Lock 16 "asmjson r10-direct-threading" admissibility).
        // Selected only when: target features admit, the rule is a byte-disjoint hub,
        // grammar stack discipline is bounded, a per-grammar .asm author exists, and
        // checkasm is green. Otherwise fall back to OffsetTape with
        // BBNF-COLLAPSEDSTAGE-NOT-VIABLE.
    }
}
```

The shape miner (Lock 10) detects hub candidacy and feeds `derive_backend_shape` per ARCH §7.3. No user-visible directive; no per-rule grammar annotation; per Lock 10 auto-detect mandate.

### §4.2. Generated parser body shape

Per-rule emission contract:

| Grammar shape | Eager-emit template (current) | OffsetTape/EventTape/SinkOnly template |
|---|---|---|
| Top-level dispatch (`parse_value` in JSON) | `skip_ws → match peek → recurse` | `match source[offsets[*cursor]] → dispatch → no whitespace work` |
| Open-close container (`{ pair* }`) | `expect b'{' → loop { peek → break if b'}' → parse_pair → expect b',' or b'}' }` | `cursor++ (consume open) → loop { peek source[offsets[cursor]] → break on close → parse_pair → cursor++ (consume separator) }` |
| Key-value pair (`string : value`) | `parse_string → skip_ws → expect b':' → skip_ws → parse_value` | `parse_string → cursor++ (consume colon offset) → parse_value` |
| String primitive | `expect b'"' → loop char-by-char with escape regex → expect b'"'` | `start = offsets[cursor] → cursor++ → end = offsets[cursor] → if flags[cursor-1] & HAS_ESC == 0 borrow source[start+1..end] else decode_path` |
| Number primitive | `loop digit-by-digit → SWAR or scalar accumulate` | `start = offsets[cursor] → cursor++ → end = offsets[cursor] → parse_digits(source[start..end])` |
| Literal (true/false/null) | `expect 4-byte memcmp` | `cursor++; verify source[offsets[cursor-1]..offsets[cursor-1]+4]` |

The dispatch arms within each `Alt { Dispatch }` lowered as `OffsetTape`,
`EventTape`, or `SinkOnly` should compile to inlined dispatch under LLVM's
normal `match` lowering. The cost model emits arm-density facts but does not
select a function-pointer table; that path was rejected at REDRESS-17 as
call-site indirection. Explicit PC-as-state dispatch is reserved for admitted
`CollapsedStage` NASM.

### §4.3. HasEsc flag at scan time

The scan emits a parallel `flags: &[u8]` (one byte per offset, or one bit per offset packed into the high bits of the next offset's u32 — implementation choice driven by cache pressure). For each string-quote offset, the scan sets `HAS_ESC` if any backslash byte was observed inside the string body during classify. The generated `parse_string` checks this flag; zero → borrow `source[start+1..end]` directly as `&str` after the scan-boundary UTF-8 policy has accepted the input. Non-zero → fall through to existing decode loop.

This is asmjson technique #7 + sonic-rs's `ParseStatus::HasEscaped` shape. It composes with the eager-tape canonical: `Tape<'input>` owns the offset array and the flags array; both are populated at scan time; both have known size bounds at scan-emission time so the offset Vec is pre-sized via `Vec::with_capacity(input.len() / 4)` per §6 step 2.

Post-SK-V3 correction: UTF-8 validation is a scan-boundary policy and
primitive contract, not a grammar directive. The grammar does not grow
`@validate_utf8`; `parse_bytes` validates before views are exposed, and
`parse(&str)` inherits Rust's already-valid UTF-8 input contract. The old
`set_len(0)` drop-bypass note is removed as a non-lever: offset vectors carry
plain `u32` elements, so there is no per-element destructor to bypass.

## §5. Phase 3 — Collapsed-stage AVX-512 backend (asmjson-class)

Optional second emitter for x86_64 AVX-512 VBMI2 hardware, behind feature flag `bbnf-runtime/avx512vbmi2`. The path past simdjson into asmjson territory. Per the DAVID research agent: "the architecturally consequential decision is technique #1+#3 together: collapsing Stage A and Stage B into one mask-driven FSM walk in the style of asmjson. This is the only way to actually approach 10+ GB/s on x86-64 AVX-512 hardware, but it requires giving up the structural-index abstraction in favour of mask-stream-with-FSM-state. Tractable as a parallel backend... not as a replacement for the existing pipeline."

The collapsed-stage backend is therefore a fifth `BackendShape`, not a
metadata-selected backend. The cost model derives it from existing Grammar IR
facts plus target-feature availability. There is no
`backend_shape = "collapsed-stage"` workspace key and no grammar annotation.
If the target lacks a green primitive vocabulary, NASM author, target silicon,
or grammar-specific parity harness, the compiler reports
`BBNF-COLLAPSEDSTAGE-NOT-VIABLE` and falls back to `OffsetTape`.

### §5.1. 9-state DPDA: 9-state finite control + direct-threaded dispatch via `r10` + hardware-bounded explicit stack (`open_buf[MAX_JSON_DEPTH=64]`) for container nesting

**Naming clarification (V9.5 PSI excavation, 2026-05-13)**: prior framing as "9-state FSM" was technically the finite-control fragment only. The asmjson reference (verified via direct WebFetch of `src/lib.rs`) carries `MAX_JSON_DEPTH = 64`, `frames_buf[]`, `open_buf[]`, `FrameKind { Object, Array }` — the bracket stack is load-bearing for nesting. Per the FSM-correctness audit at `/tmp/fsm-correctness-audit.md` §d, asmjson is a **Deterministic Pushdown Automaton (DPDA)**: finite-control 9 states + hardware-bounded explicit stack. A pure FSM cannot parse context-free grammars with nesting; any "FSM-shaped codegen" worth shipping is necessarily a DPDA. Codegen-emitted DPDA derivation from Grammar IR must derive the per-grammar stack discipline (bracket-pair set, depth bound, open-token validation) from Grammar IR facts — a derivation not yet audited for state-explosion bounds and the load-bearing residual risk per §5 below.

**V9.5 dispatch posture**: `CollapsedStage` as Rust codegen is rejected as the 1000-commit Era V failure mode in canonical form. The load-bearing observation (`LESSONS-LEARNED.md:17-26`): Rust-emitted explicit automaton overhead exceeds what LLVM can compile away, while Rust recursive descent compiles into an implicit automaton via the optimiser's call-stack-as-parse-state lowering. The four non-PSI shapes (`EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`) are materially different from `CollapsedStage` for precisely this reason: their parse state lives on the LLVM-managed call stack, not in a runtime-managed state-target table.

`CollapsedStage` is therefore admissible only via hand-authored NASM riding the two-layer reusable vocabulary established in §5.2. The admissibility predicate has two conjuncts: (a) the `ext/x86/bbnf.asm` primitive vocabulary is complete and `checkasm_parity` is green for the target ISA, AND (b) a per-grammar `.asm` source has been committed for the grammar × ISA pair under examination. If either conjunct fails for a given grammar × ISA, the cost model falls back to `OffsetTape` (Rust recursive descent over the offset stream, with the Lock 16 NEON / AVX-512 esoterica primitives still callable at hot inner loops via FFI shims). The fallback is recoverable; the diagnostic that names the condition is `BBNF-COLLAPSEDSTAGE-NOT-VIABLE`.

Per asmjson dev.md §1-39 and parse_json_zmm_sax.S analysis (DAVID research agent):

State alphabet: V (value), O (object body), K (key expected), D (colon expected), C (comma-or-close), S (string body), F (false literal), R (true literal), A (null literal — "null" rhymes with "a-z" close enough).

Each state has its own classifier mask set (e.g., state V wants `,]` for done, S wants `"\`). State transitions happen by jumping to the state's entry label after each chunk's classification, with `r10` holding the next-state target across chunk-refetch boundaries. No state-variable memory traffic; the program counter *is* the state.

### §5.2. Two-layer reusable vocabulary (dav1d / asmjson pattern)

The hand-authored ASM surface is structured as four layers; the lower two are grammar-neutral and shared across every grammar the cost model assigns to `CollapsedStage`, the upper two carry the per-grammar variation as data rather than code. The structure mirrors dav1d's split between `x86inc.asm` (ABI macros), `x86util.asm` (cross-pixel-format primitive macros), and the per-pixel-format kernel sources that compose those primitives. asmjson reimplements the ABI layer locally inside its 2,641-LOC monolith; the design here vendors dav1d's already-hardened ABI layer and adds one grammar-primitive layer on top, leaving roughly half the bytes of asmjson to author.

**Layer 0 — ABI and ISA-multiplexing macros (vendored).** `skinny/crates/bbnf-simd/ext/x86/x86inc.asm:1-1978` carries dav1d's `INIT_XMM` / `INIT_YMM` / `INIT_ZMM` register-width selectors, the `cglobal` / `cextern` ABI declarations, `WIN64_SPILL_XMM` for the Windows xmm6-15 callee-saved convention, and the SSE / AVX / AVX-512 instruction-encoding multiplexer that lets one `.asm` source compile across the three vector widths with no source duplication. BSD-2 license, attribution in `ext/x86/LICENSE-VENDOR`. No edits to this file; it is read-only vendor surface.

**Layer 1 — grammar-neutral primitive macros.** A new source `skinny/crates/bbnf-simd/ext/x86/bbnf.asm` carries the grammar-neutral macro vocabulary, sized at roughly 600 LOC when complete. HEAD `9eef728c` contains the skeleton plus the first end-to-end `BYTE_CLASS_FROM_EQ_SET_64` scalar/aarch64/x86/checkasm path. The primitives are the analog of dav1d's `x86util.asm`: each is a named macro with one ISA body per supported width, callable by per-grammar kernel sources without recompilation. The nine primitives:

- `BYTE_CLASS_FROM_TABLE_64` — consume 64 input bytes and a 256-byte classifier table, produce a 64-bit k-mask with one bit set per byte whose class is non-zero. NEON body: four `vqtbl4q_u8` against a 64-byte LUT (the alphabet fits per Lock 16 row "arm64 NEON byte classify"). AVX-2 body: four `vpshufb` against a 32-byte half-LUT with high-nibble fold. AVX-512 body: single-pass `vpermb` for arbitrary 64-byte LUT, or `vgf2p8affineqb` (GFNI, single µop) when the class set is encodable as an 8×8 GF(2) affine matrix — the asmjson-class structural set `{}[],:` falls in the affine-encodable subset per the Lock 16 GFNI admissibility row.
- `BYTE_CLASS_FROM_EQ_SET_64` — consume 64 input bytes and `k` byte constants, produce a k-mask with one bit set per byte equal to any constant. This is asmjson's actual primitive shape: the AVX-512 body is the `k`-way `vpcmpeqb` plus `korq` reduction (precisely the 10× `vpcmpeqb` + 6× `korq` instruction histogram observed in `parse_json_zmm_dom.S`). NEON body: per-constant `vceqq_u8` followed by `vorrq_u8` reduction; admissible under Lock 16 "NEON set-membership" (SVE2 `svmatch_u8` analog).
- `BITMAP_PREFIX_XOR_64` — consume a 64-bit bitmap of quote positions, produce a 64-bit bitmap of in-string regions via simdjson's carry-aware prefix-XOR construction. AVX-512 body: `vpclmulqdq` at 512-bit lane width (Lock 16 admissibility "AVX-512 VPCLMULQDQ"). NEON body: scalar carry chain (NEON has no PMULL64-over-bitmap analog that beats `clmul + eor` scalar at this width).
- `BITMAP_NEXT_SET_BIT` — locate the index of the next set bit in a 64-bit bitmap. x86 body: `tzcnt` (matches the 18× `tzcnt` site count in asmjson). aarch64 body: `vshrn_n_u16` movemask narrow plus `clz` on the bit-reversed scalar (per the Validark 2024 interleaved-movemask construction already in Lock 16).
- `BULK_EMIT_COMPRESSED` — given a 64-byte input and a 64-bit selection mask, emit the selected bytes contiguously to a destination cursor. x86 body: VBMI2 `vpcompressb {k1}` plus `vmovdqu8` store. NEON body: `vextq_u8` chain over the eight-of-sixteen subset followed by `vld1q`/`vst1q` writeback (the NEON port is materially slower than VBMI2; the macro abstracts the cost so per-grammar kernels do not hard-code the gap).
- `EOB_PAD_CLAMP` — asmjson's msac-style end-of-buffer discipline: over-allocate the input by one vector width, mask-zero the tail bytes past true input length so the classifier kernels see zeros (which lie outside every grammar's interesting-byte set) without scalar tail logic. x86 body: `vmovdqu8 {k1}{z}` zero-masked load. NEON body: `vandq_u8` with a tail-length-derived predicate vector.
- `FSM_DISPATCH_THREADED` — the `CollapsedStage`-only primitive. Implements `jmp [r10 + state*8]` against a per-grammar state-target table co-located in a `.data` section with the kernel. r10 carries the next-state-table base across chunk boundaries; per-state code paths each end with `mov r10, target_table_state_X; jmp BITMAP_NEXT_SET_BIT_continue`. This is the asmjson `parse_json_zmm_dom.S` PC-as-state architecture, distilled to a macro the per-grammar kernel can invoke without re-authoring the threading discipline.
- `FRAME_PUSH_BOUNDED` / `FRAME_POP_BOUNDED` — the bracket-stack discipline for DPDA-class grammars. asmjson's `MAX_JSON_DEPTH=64` carries `frames_buf[64]` and `open_buf[64]`; the macros encode the bounded-depth check, the open-token push, and the close-token pop with mismatch detection. Per-grammar kernels supply the bracket-pair set and the depth bound as macro arguments; the macros emit identical fault-on-overflow / fault-on-mismatch sequences regardless of grammar.

**Layer 2 — codegen bifurcation.** The cost model classifies each grammar into one of five `BackendShape` values (Lock 1 substrate union: `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage`). The four non-PSI shapes lower to Rust recursive descent that calls Layer-1 primitives via FFI shims at the hot inner loops — the parse state lives on the LLVM-managed call stack, the SIMD classification work happens inside the primitive bodies, and LLVM's optimiser fuses the call-stack-as-parse-state lowering through the recursive-descent driver in the same way that lets yyjson's force-inlined hot function fit in 18 KiB. `CollapsedStage` is the only shape that *also* requires the codegen to emit a per-grammar `.asm` source: a small kernel that supplies the classifier `.data` table, the state-transition `.data` table, the bracket-pair constants, and a thin entry/exit sequence that composes the Layer-1 macros into the grammar's specific 9-state-class DPDA. The per-grammar kernel is small precisely because every interesting primitive is already in `ext/x86/bbnf.asm`; the per-grammar variation is data (tables) plus a short composition (macro invocations), not new instruction-level code.

**Layer 3 — checkasm differential parity (admission gate).** Every primitive in `ext/x86/bbnf.asm` ships with a scalar Rust reference implementation in `bbnf-simd::scalar::*` and a differential parity closure in `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`. The Rust scalar reference *is* the executable specification — when the FFmpeg-discipline harness (randomised identical src0/src1 buffers, byte-equality between `call_ref` and `call_new`, alignment sweep 0..15, stack-clobber canary, SIGSEGV/SIGBUS/SIGILL guard) reports divergence, the primitive does not enter the codegen template. The harness has already earned its keep: the `escape_mask_64` NEON state-handoff bug (xorshift seed `0xCAFEF00DBAADF00D`, reported in `CHECKASM-REPORT.md` §d) was a chunk-boundary lookahead miscount that scalar review missed and the differential harness caught on first invocation. No primitive ships marketed as Lock-16 admissible until checkasm is green in strict mode (`BBNF_SIMD_STRICT=1`).

### §5.3. Generalization across arbitrary grammars

The primitives in Layer 1 are grammar-neutral by construction: each consumes 64 input bytes plus a parameter (a classifier table, a constant set, a bitmap) and produces a bitmap, a count, or an emitted byte range. Nothing inside `ext/x86/bbnf.asm` mentions a JSON-specific byte, a CSS-specific keyword, a BBNF-self-specific operator, or a Sheets-specific formula token. Per-grammar variation lives entirely in two `.data` sections that the codegen emits alongside the per-grammar kernel: a 256-byte classifier table (input byte → class) and a state-transition table (current state × input class → next state, sized `9 × |class_set|` for asmjson-shape DPDAs and grammar-derived for others).

This is the same factoring that lets dav1d share `x86util.asm` plus `filmgrain_common.asm` across every pixel format while the per-pixel-format motion-compensation kernels live in separate sources whose differences are constant tables and macro composition order, not new instructions. The executable spine — register conventions, prefetch discipline, mask-fusion sequences, k-mask retention across state transitions, EOB padding — is identical across JSON, CSS L4, BBNF-self, and Sheets. The per-grammar bytes are the contents of two `.data` sections.

Lock 14 (zero overfitting) is preserved unconditionally: no grammar-specific instruction lives in any generic crate. `ext/x86/x86inc.asm` is dav1d-vendored and grammar-agnostic by origin; `ext/x86/bbnf.asm` is grammar-neutral by macro-API discipline; the per-grammar kernel sources are codegen-emitted from Grammar IR via the same `LayoutFacts.backend_shape` derivation that selects between the five `BackendShape` values for every other lowering decision. The cost model decides which grammars receive a `CollapsedStage` kernel and which receive `OffsetTape` recursive descent; the audit surface for "is this grammar overfit" is the codegen-emitted `.data` table plus the macro composition list — both fall out mechanically from Grammar IR facts (alphabet, first-set partitions, bracket pairs, depth bound).

### §5.4. Size budget

The hand-authored ASM line count is bounded by the layer structure:

- Layer 0: `ext/x86/x86inc.asm` — 1,978 LOC, vendored from dav1d, BSD-2. Already in tree at `skinny/crates/bbnf-simd/ext/x86/x86inc.asm`. Zero authoring cost.
- Layer 1: `ext/x86/bbnf.asm` macro layer — ≈ 600 LOC. Nine primitive macros plus shared register conventions, label discipline, and `.data` section helpers.
- Layer 1: per-ISA primitive bodies — ≈ 800 LOC. Nine primitives × three ISAs (AVX-2, AVX-512, NEON) × roughly 30 LOC per body, with `BITMAP_PREFIX_XOR_64` and `BULK_EMIT_COMPRESSED` carrying the largest NEON bodies due to the absence of direct VPCLMULQDQ / VBMI2 analogs.
- Layer 2: per-grammar `CollapsedStage` `.asm` files — small, codegen-emitted, dominated by `.data` (classifier table + state-transition table) plus a macro composition sequence on the order of 100 LOC per grammar × ISA pair.

Total hand-authored ASM: ≈ 1,400 LOC across the macro layer and the per-ISA primitive bodies. Compare asmjson's 2,641-LOC AVX-512 monolith, which reimplements the ABI macro discipline locally and inlines every primitive at each call site — the dav1d vendor + macro-factor saves roughly half the bytes while admitting the same primitives plus the Lock 16 esoterica additions (GFNI, VPCLMULQDQ-512, AVX-IFMA, VNNI, BITALG) that asmjson does not exploit.

### §5.5. Classifier kernel per chunk

```asm
; Apple Silicon analog: load 64 bytes via vld1q_u8_x4, classify via 4× vqtbl4q_u8,
; reduce via vshrn_n_u16 movemask. x86_64 VBMI2 below.

vmovdqu64       zmm0, [rdi]              ; 64 bytes
vpcmpub         k1, zmm0, zmm_ws,   2    ; <= 0x20  → whitespace mask
vpcmpeqb        k2, zmm0, zmm_quote      ; "
vpcmpeqb        k3, zmm0, zmm_bs         ; \
vpcmpeqb        k4, zmm0, zmm_open       ; { or [ — fused via vpermi2b
korq            k5, k1, k2               ; structural-or-string-boundary
korq            k5, k5, k4               ; full delimiter mask
; downstream: vpcompressb of indices via mask k5, single-store to offset array
```

### §5.6. Dispatch table return + runtime selection

Per asmjson `src/lib.rs`:

```rust
pub fn build_parser(grammar: &str) -> Box<dyn TypedParser> {
    let backend = select_backend_for_cpu();
    match backend {
        BackendShape::CollapsedStageAvx512 if has_cpu_feature("avx512vbmi2") => {
            Box::new(CollapsedStageParser::for_grammar(grammar))
        }
        BackendShape::OffsetTape => Box::new(OffsetTapeParser::for_grammar(grammar)),
        BackendShape::EagerTape => Box::new(EagerTapeParser::for_grammar(grammar)),
    }
}
```

The selection happens once per parser construction; per-parse-call dispatch is inlined.

### §5.7. Projected impact

Twitter on Zen 4 AVX-512 VBMI2:

| Stage | c/B | Source |
|---|---:|---|
| Classify (vpcompressb + vpermi2b + vpternlogd fused) | ~0.15 | intrinsics agent estimate |
| FSM dispatch + emit | ~0.10 | asmjson published 10.93 GiB/s ÷ 4.5 GHz |
| Strings (mask-driven body skip; HasEsc-conditional decode) | ~0.10 | asmjson #7 |
| Numbers (SIMD digit-block fast-float on canada-shape only) | varies | |
| **Total twitter** | **~0.35** | **~12.8 GB/s twitter ~= 100K Mbps** |

This is a successor-tranche x86 target, not a local SK-V4 close claim. The
collapsed-stage backend is the only realistic route past asmjson on x86_64,
but it is accepted only on equivalent hardware with strictness and output plane
matched. On arm64, M5 Max close remains `OffsetTape` / generated `SinkOnly`
plus admitted NEON primitives; no arm64 `CollapsedStage` claim is made here.

## §6. Falsifiability + gates

Each phase carries an empirical gate; failure to land the gate routes back to re-profile and re-attribute, not to substrate amendment.

### §6.1. checkasm admission gate (precondition to all phases)

Every Lock 16 primitive used by any kernel in this design — including the two new kernels in §3.2.1 / §3.2.2 — must pass the `bbnf-simd` differential parity harness before it is marketed as a SOTA-BEAT lever. The harness lives at `skinny/crates/bbnf-simd/tests/checkasm_parity.rs` (516 LOC; verified prototype per `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`); it adapts FFmpeg's `checkasm.h` discipline (randomized identical src0/src1 buffers, `call_ref` vs `call_new` byte-equality, alignment sweep 0..15, 1 KiB stack-clobber canary, SIGSEGV/SIGBUS/SIGILL guard, robust outlier filter). It already caught the `escape_mask_64` NEON state-handoff bug on first run (CHECKASM §d, xorshift seed `0xCAFEF00DBAADF00D`) — proof the gate has teeth.

Admission protocol: every new `core::arch::*` use-site and every `asm!` block in `crates/bbnf-simd/` lands behind a pair of checkasm closures (`call_ref = scalar reference`, `call_new = NEON / AVX-512 candidate`) before the kernel is wired into the generated parser. Invocation: `cargo test -p bbnf-simd --release --test checkasm_parity` (strict mode: `BBNF_SIMD_STRICT=1`). A primitive that fails strict parity does not enter the codegen template — full stop. Wave 0/1 has cleared the `escape_mask_64` divergence and admitted the aarch64 Class A/Class B primitives, but parity admission is not a throughput close by itself.

### §6.2. Phase gates

| Phase | LOC budget | Twitter T1 gate | Hot-leaf count gate | Twitter c/B gate | Cite |
|---|---:|---|---|---|---|
| Phase 0 (Lock 15 enforcement: `lto=fat` + force-inline + ≤20 KiB i-cache budget; checkasm harness wired into CI) | ~15 LOC + Cargo.toml | T1 ≥ 950 MiB/s (catches `lto=thin` regression; yyjson lever) | ≤ 4 hot leaves | ≤ 3.5 c/B | Lock 15 enforcement; `skinny/profile/wave2-pmu/PMU-REPORT.md` §1.5 |
| Phase 1a (Class A NEON primitive: 16-byte `vqtbl4q_u8 + vshrn` movemask `match_tiny_plain_string`; §3.2.1) | admitted; parser route inactive | strict checkasm green; parser wiring must not regress `twitter` | 1 hot leaf after event-cursor | ≤ 2.5 c/B on Class A corpora | Wave 0/1 redress; `skinny/profile/wave2-asm/PROFILE-REPORT.md` §(c) Fix 1 |
| Phase 1b (Class B NEON primitive: TBL-driven `\uXXXX` hex decoder; §3.2.2) | admitted | strict checkasm green; close requires event-cursor / `parse_value_at` reprofile | 1 hot leaf after event-cursor | n/a (corpus-shape dependent) | Wave 0/1 redress; `skinny/profile/wave2-asm/PROFILE-REPORT.md` §(c) Fix 4 |
| Phase 2 (`LayoutFacts.backend_shape` cost-model + `Alt { Dispatch }` per-shape lowerer + HasEsc flag + lazy borrow; lowering pattern only, no new BIR variant) | ~470 LOC | **T1 ≥ 2375 MiB/s (SOTA-BEAT sonic-rs Value-DOM 2438 MiB/s; approaches simdjson 1.142 c/B)** | 1 hot leaf | ≤ 1.4 c/B | §4 |
| Phase 3 (AVX-512 VBMI2 backend; GFNI `vgf2p8affineqb` classifier) | ~200 | T1 ≥ 3325 MiB/s on x86_64 AVX-512 hardware (BEAT simdjson DOM 2923 MiB/s) | 1 hot leaf on x86_64 | ≤ 1.0 c/B on x86_64 | §3.3 + §5 |
| Phase 4 (collapsed-stage AVX-512BW backend; auto-selected via CPUID, not opt-in) | ~600 | T1 ≥ 7400 MiB/s on x86_64 (asmjson 10.93 GiB/s parity territory) | 1 hot leaf | ≤ 0.45 c/B | §5 |

### §6.3. Wave 2 re-baseline against S-anchors

The failing-corpus picture had collapsed from 5 to ~3 corpora in the Wave 2 projection. Wave 0/1 then admitted Class A + Class B NEON primitives (§3.2.1 / §3.2.2), but the current parse plane still has four G rows because parser wiring and `parse_value_at` still dominate. The direct plane is separately `N-direct / NoGo` after the first sink-only rewrite:

| Corpus | Wave 2 throughput (Mbps) | S-anchor (Mbps) | Δ vs anchor | Pathology class | Wave 0/1 status |
|---|---:|---:|---:|---|---|
| github_events | 20,709 (Wave 2 asm) | 19,678 (sonic-rs) | **+5.2%** | Class A | current report A / GO |
| update-center | 18,538 (Wave 2 asm) | 16,299 (sonic-rs) | **+13.7%** | Class A | current report C / GO |
| random | 12,373 (Wave 2 asm) | 11,586 (sonic-rs) | −6.8% | Class A | primitive admitted; event-cursor still required |
| unicode_escapes | 17,079 (Wave 2 asm) | 17,854 (sonic-rs) | −4.3% | Class B | current report C / GO; direct sink still fails |
| y_string_unicode | 11,120 (Wave 2 asm) | 13,343 (sonic-rs) | −16.7% | Class B | current report C / GO; direct sink still fails |

`github_events` has crossed its S-anchor in the current report; `update_center`,
`unicode_escapes`, and `y_string_unicode` are C / GO rather than hard failures.
Wave 0/1 then admitted the Class A/Class B primitives under strict checkasm, but
the active 16-byte tiny-string parser route regressed `twitter` and is disabled.
The binding parse residual is therefore no longer "admit the kernels"; it is
`parse_value_at` / event-cursor consumption plus string/Unicode projection on
the four remaining G rows. The binding direct residual is sink-only
numeric/string/Unicode materialization.

### §6.4. Comparator anchors

On M5 Max twitter DOM-class (verified at `skinny/profile/native-sidecars/PROFILE-REPORT.md`):

| Parser | Twitter c/B | Twitter MiB/s |
|---|---:|---:|
| **yyjson** (no SIMD; force-inline + ~18 KiB hot function) | 0.91 | 3687 |
| **simdjson DOM** | 1.142 | 2923 |
| **sonic-rs Value-DOM** | ~2.3 | 2438 |
| **skinny current** (lto=thin regression) | 5.07 | 658 |
| **RapidJSON floor** | 7.30 | 479 |
| **serde_json floor** | 7.80 | 449 |

The cycle-per-byte gate is the host-clock-invariant metric. The hot-leaf-count gate enforces fusion-quality parity with comparators (yyjson 1 leaf; sonic-rs Value-DOM 1-2; simdjson 2). A parser carrying 5+ leaves at the same wall-clock is structurally bolted-on, regardless of throughput.

**Per-corpus generalization gates** (no overfit to twitter): for each Phase, the gate must hold on `unicode_escapes.json` within 2× of twitter c/B (the escape-pathology bound; simdjson's 4.97 c/B on unicode_escapes vs 1.14 on twitter is the upper-bound ratio our parser must not exceed). JSONTestSuite conformance bundle (95 `y_string_*` files) must exit 0 with zero `BBNF-UTF8-INVALID-AT-PARSE` panics per BENCH §7.9 Gate 1. Float-bit-exact parity must hold on canada/numbers/mesh/marine_ik per BENCH §7.9 Gate 4.

Each gate falsifies a distinct architectural claim:
- **Phase 0** falsifies "build profile cannot meaningfully affect throughput" (yyjson 0.91 c/B without SIMD proves force-inline + i-cache residency alone closes 30-40% of the gap).
- **Phase 1** falsifies "NEON intrinsic upgrades are second-order" (Lemire 2019 `vqtbl4q_u8` + Validark 2024 movemask measured 4-16 c/64B savings).
- **Phase 2** falsifies "the recursive-descent driver is unavoidable" (simdjson stage2 `&buf[*(next_structural++)]` proves typed dispatch reads source only at primitive boundaries).
- **Phase 3** falsifies "AVX-512 VBMI2 buys nothing simdjson doesn't already exploit" (`icelake/simd.h:157` explicitly leaves `vpcompressb` unused; GFNI `vgf2p8affineqb` 2× over PSHUFB is genuinely unused in JSON literature).
- **Phase 4** falsifies "asmjson's collapsed-stage architecture is not
  portable to a meta-grammar" only after a per-grammar NASM author and parity
  harness exist. Until then `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` is the correct
  result, not a partial close.

## §7. Implementation sequence

The current receiver is `IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT.md`; the
older SK-V3 packet remains historical context. The summary sequence:

1. **Step 0** (~5 LOC, 2 min) — Lock 15 enforcement: `skinny/Cargo.toml [profile.release] lto=true codegen-units=1 panic="abort" debug=true` (LANDED per Wave 2 PMU).
2. **Step 1** (LANDED) — capacity Plan D `grow-only`: replace sampled heuristic with `Vec::with_capacity(256)` + geometric grow (per `skinny/profile/wave2-capacity/CAPACITY-REPORT.md` §6); 23–64% capacity reclaim, +10.2% github_events / +4.8% random throughput.
3. **Step 2** (~25 LOC, next) — event-cursor consumption over the tape projection; reduce `parse_value_at` self-time before revisiting parser-wired SIMD helpers.
4. **Step 3a** (ADMITTED, inactive parser route) — Phase 1a NEON Class A primitive in `bbnf-simd/aarch64/`; strict checkasm passes, but parser wiring of the 16-byte tiny-string helper regressed `twitter`, so the 8-byte scalar recognizer remains active.
5. **Step 3b** (ADMITTED) — Phase 1b NEON Class B primitive: TBL-driven `\uXXXX` decoder in `bbnf-simd/aarch64/`; same checkasm-first discipline.
6. **Step 3c** (LANDED) — close the `escape_mask_64` NEON state-handoff bug surfaced by checkasm strict mode (CHECKASM §d).
7. **Step 4** (~470 LOC, after event-cursor proof) — Phase 2 `LayoutFacts.backend_shape` cost-model + `Alt { Dispatch }` per-shape lowering + HasEsc flag emission + lazy borrow. **No new BIR variant** (per §4 amendment); same Alt, multiple lowerings.
8. **Step 5** (~1 hr) — comparative re-profile: produce `skinny/profile/skinny-v3-implemented/` and update `skinny/profile/native-sidecars/PROFILE-REPORT.md` deltas.
9. **Step 6** (~200 LOC, conditional, x86_64 only) — Phase 3 AVX-512 VBMI2 path in `bbnf-simd/x86_64/avx512_vbmi2/` (GFNI classify + k-mask arithmetic 5-pack from Lock 16).
10. **Step 7** (~600 LOC, conditional, post-Phase-3-validation) — Phase 4 collapsed-stage AVX-512BW asmjson-class backend.

Steps 0–5 must land before Steps 6–7; arm64 host gating discipline. Every SIMD primitive use-site passes checkasm before merge.

## §8. Locks affected

| Lock | Status | Rationale |
|---|---|---|
| Lock 1 (Tape substrate) | UNCHANGED | Structural-index-driven is a codegen template shape; the substrate (Tape + ValueRef + arena + DocumentView) is unchanged. ValueRef's cursor shape was already implied by `LAZY-TAPE-DESIGN.md`; the final gate keeps lazy-offset tape as the measured winning substrate, not as a refuted route. |
| Lock 10 (cost model + shape miner) | EXTENDED-MECHANICAL | Shape miner detects dispatch-hub patterns from grammar shape; cost model selects backend (`EagerTape` / `OffsetTape` / `EventTape` / `SinkOnly` / `CollapsedStage` per §4.1) per grammar. Same `Alt { mode: Dispatch }` BIR variant, multiple lowerings — no new BIR variant. Existing Lock 10 surface absorbs. |
| Lock 14 (grammar generalisation; zero overfitting) | UNCHANGED | The `OffsetTape` / `EventTape` / `SinkOnly` lowering family applies to *any* grammar with a dispatch hub (JSON, CSS L4, BBNF-self, Sheets); no grammar-specific code in `bbnf-simd` or the codegen template. |
| **Lock 15** (build-profile discipline + i-cache residency) | LANDED — i-cache budget already met | Every generated runtime crate declares `lto=true`, `codegen-units=1`, `panic="abort"` (or `unwind` if grammar declares recovery), `debug=true` in `[profile.release]`. Wave 2 PMU (`skinny/profile/wave2-pmu/PMU-REPORT.md` §1.5) confirms current fused `parse_value_at` body is **7,304 bytes ≈ 7.13 KiB**, well below the ≤20 KiB i-cache budget and ~7% of the M5 Max L1i (192 KiB per P-core). The `BBNF-ICACHE-BUDGET-EXCEEDED` diagnostic is currently un-fireable on JSON; the budget could be relaxed to ≤32 KiB or held as a forward-looking safety net for grammars with larger fused hot loops (CSS L4 typed-emit, BBNF-self pratt). Keep ≤20 KiB as the conservative default; no Lock 15 amendment required. See `14-LOCKS.md` §15. |
| **Lock 16** (SIMD/ASM admissibility allowlist; extended AVX-512 5-pack + NEON 3-pack) | EXTENDED (post-Wave-1) | Admissible SIMD primitives are an explicit allowlist with citations; current corpus per `14-LOCKS.md` §16 includes the AVX-512 5-pack (`k-mask arithmetic family`, `VPCLMULQDQ-512`, `AVX-IFMA vpmadd52`, `VNNI vpdpbusd`, `BITALG vpshufbitqmb`) and the NEON 3-pack (`LD4-interleaved 4-channel classifier`, `vbcaxq_u8 / veor3q_u8` ternary bitwise, `vceqq_u8 + vorrq_u8` set-membership = NEON port of SVE2 `svmatch_u8`). Every `core::arch::*` use-site and every `asm!` block in `crates/bbnf-simd/` must trace to a Lock 16 row + pass the checkasm parity harness (§6.1) before entering the codegen template. See `14-LOCKS.md` §16. |

## §9. Open residues

- **R1**: TS/WASM backend disposition for SIMD primitives. V2-deferred per Lock 8, but the `bbnf-simd` crate's types must be backend-agnostic at the trait boundary so V2 emitters can adopt WASM SIMD128 or TS WebAssembly SIMD without re-architecting. The trait surface in §3.1 already admits this; verification deferred to V2.
- **R2**: CSS L4 + Sheets + BBNF-self adoption of the `OffsetTape` /
  `EventTape` / `SinkOnly` lowering family. The shape applies where each
  grammar has a byte-disjoint dispatch hub; per-grammar measurement deferred to
  the future-grammar-onboarding test (`MASTER-PLAN §12`). No architectural
  amendment expected; only codegen template wiring.
- **R3**: Apple SME (Scalable Matrix Extension) on M4+ — flagged by intrinsics agent as not applicable for hot JSON parsing (entry/exit cost dwarfs per-block gain). Re-examine if M-series hardware gains SVE2 in future generations.
- **R4**: AMD Zen 4/5 VBMI2 `vpcompressb` store gotcha (Lemire 2025) — prefer `maskz_compress` + separate `storeu` on Zen 4 hosts; gate via runtime CPUID. Documented in `bbnf-simd/x86_64/avx512_vbmi2/compress.rs`.
- **R5**: The 18552 Mbps sonic-rs reference number is the LazyValue path which uses prefix-XOR; our skinny's `from_slice::<Value>` driver shape exercises the typed-Value-DOM path at ~2782 Mbps. The 7K Mbps gap framing remains correct against the LazyValue reference; honest comparison plane is documented in `BENCH.md` §6.

## §10. Verdict

This design is the empirical synthesis of (a) four research streams from Wave 1 (DAVID/asmjson; handwritten ASM 2024–2026 papers; SIMD intrinsics beyond sonic-rs/simdjson; comparative samply profiles of all three parsers) and (b) the Wave 2 redress (per-corpus asm pathology; PMU/i-cache analysis; capacity-plan probe; native sidecar M5 Max measurement; checkasm parity harness). The substrate is validated by the credible win column on citm/canada/mesh/numbers and by Track 1/Track 2 movement together. Wave 0/1 landed Plan D, fixed strict checkasm, admitted the grammar-neutral Class A/Class B primitives, replaced the timed direct view walk with a sink-only digest parser, removed duplicate UTF-8 validation, and moved integer classification into the scanner result. The measured full gate remains `N-direct / NoGo`: parse has four G rows, and direct sink remains below sonic-rs direct on 11 of 17 rows. Lock 1 stands. Lock 15 i-cache budget is empirically met (7,304-byte fused hot leaf < 20 KiB). Lock 16 stands extended with the AVX-512 5-pack + NEON 3-pack. The Layer 1 vocabulary canon stands.

The path to BEAT sonic-rs on the M5 Max expanded gate now starts with typed event-cursor consumption over the tape projection and a fresh `parse_value_at` profile, then reuses the admitted primitives only where the parser route is non-regressing. In parallel, direct sink needs number and Unicode materialization primitives rather than another retained-view change. The path past simdjson into asmjson territory remains Phase 4 (`CollapsedStage` AVX-512BW + esoteric stack from Lock 16), aspirational and gated on Phase 3 measurement.

Hereupon: execute `IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` against the skinny workspace, with each SIMD primitive landing through checkasm before it is wired into the codegen template.
