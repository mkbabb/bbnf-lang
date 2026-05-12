# SOTA-BEAT-DESIGN — Structural-Index-Driven Codegen with SIMD Primitive Layer

Status: ACTIVE proposal. Supersedes `LAZY-TAPE-DESIGN.md` (refuted at outcome G).
Created: 2026-05-12 after V9.2 conditional refutation and six-agent comparative-profile cohort.
Anchor: this document is the executable architectural target for SOTA-BEAT against both `sonic-rs` (2.32 GB/s twitter, the LazyValue reference at 18552 Mbps in `bbnf-bench`) and `simdjson` (~3.0 GB/s twitter, the simdjson-DOM reference). Both must be beaten; `arm64` Apple Silicon is the primary host and gating environment, `x86_64` AVX-512 is the secondary acceleration target.

## §1. Empirical premise

The lazy-offset tape amendment was implemented per `LAZY-TAPE-DESIGN.md` §10 and re-benched at outcome G: twitter T1 = 11780 Mbps = 1.47 GB/s, below the 13K Mbps refutation threshold and below the 14K Mbps validation threshold. Cite: `skinny/RESULTS.md:5-7`; `skinny/REDRESS.md` item 20.

The six-agent comparative cohort (skinny samply + asm + sonic-rs samply + simdjson samply + DAVID/asmjson research + handwritten ASM catalog + SIMD intrinsics catalog beyond sonic-rs) returned coherent attribution:

| Source | Finding | Cite |
|---|---|---|
| `skinny/profile/PROFILE-REPORT.md` | parse_value 35.58% self + scan 36.36% + consume_structural 10.28% + parse_string 9.13% = 5+ hot leaves | samply attribution |
| `skinny/profile/ASM-REPORT.md` | "the typed parser is bolted on top of the SIMD scan, not driven by it. `parse_value` does `skip_ws` and `peek` against raw bytes for every value boundary." | cargo-show-asm dumps; 689-instruction `parse_value` with 22.5% branch density |
| `skinny/profile/sonic-rs-v2/PROFILE-REPORT.md` | Hot-leaf count INLINED: twitter=1 (80%), citm=2 (72%+15%), canada=1 (88%). Sonic-rs fuses entire SIMD kernel into one parse driver via `lto=true codegen-units=1`. | comparative anchor |
| `skinny/profile/simdjson-v2/PROFILE-REPORT.md` | Twitter cycle budget total 1.142 c/B; stage1 0.629 c/B; stage2 0.377 c/B; `json_iterator::advance()` is a single u32-indexed pointer add; **whitespace and structural delimiters are never re-scanned in stage2**. | architectural verification |

The verdict converges across all four streams: the substrate is not the bottleneck; the codegen template shape is. Our generated parser ignores the structural index it computes. Sonic-rs fuses scan + dispatch into one stage via LTO; simdjson keeps scan + dispatch separate but stage2 reads `source[offsets[cursor]]` and never re-scans bytes the indexer already classified. We do neither — we run a sidecar SIMD scan, then a recursive descent that re-scans the same bytes for whitespace and value boundaries.

Four substrate routes are now measured-and-rejected: dispatch-table (REDRESS-17), 12-byte token (REDRESS-18), pair-token fusion (REDRESS-16), lazy-offset tape (REDRESS-20). The lever is architectural template shape, not substrate representation.

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

This is the gating implementation; M-series is the dev box and the SOTA-BEAT gate runs here. The intrinsic catalog (Lock 16 allowlist; sources tracked in `restart/audit/RESEARCH-SIMD-INTRINSICS.md`):

| Primitive | Intrinsic | Source citation | Replaces |
|---|---|---|---|
| 4-table 64-byte structural+whitespace+escape classify | `vqtbl4q_u8` | Lemire 2019 "Arbitrary byte-to-byte maps using ARM NEON" | sonic-rs's 1-table `vqtbl1q_u8` (saves ~16 c/64B per intrinsics agent) |
| Interleaved-vector movemask | `vld4q_u8` + `vshrn_n_u16` + `vsriq_n_u8` + `vzip1q_u8` | validark.dev/posts/interleaved-vectors-on-arm/ (Validark 2024) | sonic-rs's AND-OR tree (4× faster bitmap synthesis) |
| Quad-load 64 bytes | `vld1q_u8_x4` | Arm A64 ISA | 4× separate `vld1q_u8` (frees 2 load-ports on M-series) |
| Branchless mask select | `vbslq_u8` | Arm A64 ISA | conditional emit/branch (used in `string_block.rs`) |
| Byte popcount | `vcntq_u8` + `vaddvq_u8` | Arm A64 ISA | scalar `count_ones()` (saves GPR round-trip) |

The classifier kernel composes (1) + (3) into a single 64-byte block consumption per loop iteration. Quote/escape detection uses (1) + (4) with the `HasEsc` flag emitted into the parallel `flags` array. Movemask synthesis uses (2).

Projected impact (intrinsics agent + cycle-budget math): scan stage budget falls from current ~0.9 c/B to ~0.55 c/B (approaching simdjson's 0.629 c/B stage1 baseline). Twitter T1 lifts from 11780 → ~15400 Mbps. This alone validates the Phase 1 gate (≥14K Mbps).

### §3.3. x86_64 AVX-512 VBMI2 secondary path (Ice Lake+ / Zen 4+)

The path past simdjson on commodity Intel and AMD. Available only on hardware exposing `avx512vbmi2` (Ice Lake / Tiger Lake / Sapphire Rapids / Zen 4 / Zen 5). Dispatch at parser construction via CPUID; AVX-512 absent → fall to AVX-2 path; AVX-2 absent → fall to scalar SWAR.

| Primitive | Intrinsic | Source citation | Replaces / enables |
|---|---|---|---|
| One-shot structural-offset emission | `_mm512_mask_compressstoreu_epi8` | felixcloutier VPCOMPRESSB; Lemire 2022 "Parsing JSON faster with AVX-512"; simdjson `icelake/simd.h:157` explicitly leaves this unused for portability | replaces tzcnt+blsr scalar loop (~25 c/64B saved) |
| 3-mask boolean fusion | `_mm512_ternarylogic_epi64` | WikiChip AVX-512F; Sneller "Branchless Code With AVX-512" | collapses (in-string ∧ ¬escaped) ∧ structural in 1 µop instead of 2-3 |
| 128-byte byte-shuffle classify | `vpermi2b` | WikiChip AVX-512_VBMI | replaces 2× `vpshufb` lane-restricted lookups (one-pass classify) |
| Cross-window quote-state carry | `_mm512_alignr_epi8` | felixcloutier | replaces explicit prev-bit propagation |
| Bits-to-indexes (AVX2 fallback) | `_pext_u64` (BMI2) | Mula branchfree.org "Bits to indexes in BMI2 and AVX-512" | replaces tzcnt+blsr scalar loop on non-VBMI2 hosts (Zen 1/2 PEXT is slow; gate via CPUID) |
| String-bitmap prefix-XOR | `_mm_clmulepi64_si128` (CLMUL) | simdjson original; sonic-rs `src/util/arch/x86_64.rs` | baseline simdjson primitive; we adopt rather than reinvent |

Projected impact on x86_64 AVX-512 hardware (intrinsics agent quantification): scan stage at ~0.3 c/B (below simdjson's 0.629 c/B by emitting indexes one-shot instead of scalar-iterating); combined with stage2 (Phase 2 codegen template) at ~0.4 c/B → total ~0.7 c/B → ~5.7 GB/s twitter → ~46K Mbps. **Beats simdjson DOM (3.0 GB/s) by 1.9× and approaches asmjson (10.9 GB/s) territory on Zen 4 with the collapsed-stage option (§5).**

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

This is the *correctness floor* — every grammar must parse correctly on every host even when no SIMD primitive is available. Throughput on twitter: ~3 GB/s projected (asmjson SWAR hits 7 GB/s on heavier hardware; M-series scalar limited to ~3.5 GHz integer pipeline width).

## §4. Codegen template contract

The IR-side change is one new `BIR` variant; the codegen-side change is the template generator rewrite in `crates/codegen/src/lower/rust.rs`.

### §4.1. BIR variant

```rust
pub enum BirNode {
    // ... existing variants
    /// Dispatch on the byte at the current cursor position in the structural-offset array.
    /// `arms` covers all dispatch bytes; `fallthrough` is the default (error) branch.
    /// Generated lowerer emits: `match source[offsets[*cursor as usize] as usize] { ... }`.
    /// No skip_ws, no peek, no raw byte-position advancement.
    CursorDispatch {
        arms: Vec<(DispatchByte, BirId)>,
        fallthrough: BirId,
    },
    // ... existing variants
}
```

The `CursorDispatch` variant is the codegen primitive corresponding to a typed-parse hub (e.g., `parse_value` in JSON, `parse_declaration` in CSS, `parse_expression` in BBNF-self). The shape miner (Lock 10) detects this pattern from grammar shape — any rule whose body is `[ws] (first_set_byte_a → branch_a | first_set_byte_b → branch_b | ...) [ws]` lowers to `CursorDispatch` when the structural-index-driven mode is selected for the grammar.

### §4.2. Generated parser body shape

Per-rule emission contract:

| Grammar shape | Eager-emit template (current) | Structural-index-driven template (new) |
|---|---|---|
| Top-level dispatch (`parse_value` in JSON) | `skip_ws → match peek → recurse` | `match source[offsets[*cursor]] → dispatch → no whitespace work` |
| Open-close container (`{ pair* }`) | `expect b'{' → loop { peek → break if b'}' → parse_pair → expect b',' or b'}' }` | `cursor++ (consume open) → loop { peek source[offsets[cursor]] → break on close → parse_pair → cursor++ (consume separator) }` |
| Key-value pair (`string : value`) | `parse_string → skip_ws → expect b':' → skip_ws → parse_value` | `parse_string → cursor++ (consume colon offset) → parse_value` |
| String primitive | `expect b'"' → loop char-by-char with escape regex → expect b'"'` | `start = offsets[cursor] → cursor++ → end = offsets[cursor] → if flags[cursor-1] & HAS_ESC == 0 borrow source[start+1..end] else decode_path` |
| Number primitive | `loop digit-by-digit → SWAR or scalar accumulate` | `start = offsets[cursor] → cursor++ → end = offsets[cursor] → parse_digits(source[start..end])` |
| Literal (true/false/null) | `expect 4-byte memcmp` | `cursor++; verify source[offsets[cursor-1]..offsets[cursor-1]+4]` |

The dispatch arms within each `CursorDispatch` should compile to a jump table; cost-model heuristic in `crates/codegen/` emits arm density to give LLVM the strongest hint. Per `feedback_pluggable_components`: the dispatch-shape strategy is pluggable (match-density-tuned vs explicit jump table via `asm!` indirect branch on nightly). The function-pointer dispatch table previously rejected at REDRESS-17 is *not* the same; that was call-site indirection. This is jump-table dispatch with inlined targets.

### §4.3. HasEsc flag at scan time

The scan emits a parallel `flags: &[u8]` (one byte per offset, or one bit per offset packed into the high bits of the next offset's u32 — implementation choice driven by cache pressure). For each string-quote offset, the scan sets `HAS_ESC` if any backslash byte was observed inside the string body during classify. The generated `parse_string` checks this flag; zero → borrow `source[start+1..end]` directly as `&str` (one UTF-8 validation pass via simdutf8 if `@validate_utf8` directive is on, else trust). Non-zero → fall through to existing decode loop.

This is asmjson technique #7 + sonic-rs's `ParseStatus::HasEscaped` shape. It composes with the eager-tape canonical: `Tape<'input>` owns the offset array and the flags array; both are populated at scan time; both have known size bounds at scan-emission time so the offset Vec is pre-sized via `Vec::with_capacity(input.len() / 4)` per §6 step 2.

### §4.4. Set_len(0) drop bypass

asmjson technique #6. When `Tape<'input>` drops and the summary `any_string_has_escape` flag is false (tracked across the parse), the tape's offset Vec calls `set_len(0)` before drop so the deallocator frees in one call without per-element Drop. ~20 LOC; pure win at no risk.

## §5. Phase 3 — Collapsed-stage AVX-512 backend (asmjson-class)

Optional second emitter for x86_64 AVX-512 VBMI2 hardware, behind feature flag `bbnf-runtime/avx512vbmi2`. The path past simdjson into asmjson territory. Per the DAVID research agent: "the architecturally consequential decision is technique #1+#3 together: collapsing Stage A and Stage B into one mask-driven FSM walk in the style of asmjson. This is the only way to actually approach 10+ GB/s on x86-64 AVX-512 hardware, but it requires giving up the structural-index abstraction in favour of mask-stream-with-FSM-state. Tractable as a parallel backend... not as a replacement for the existing pipeline."

The collapsed-stage backend is therefore a *third* generated-parser shape (alongside (a) the eager-tape canonical for grammars with recovery/layout/typed-payload needs and (b) the structural-index-driven template for SOTA-class grammars). It is grammar-opt-in via metadata:

```toml
[workspace.metadata.bbnf.grammars.json.runtime]
backend_shape = "collapsed-stage"  # default = "structural-index"; "eager-tape" for non-JSON
target_features = ["avx512vbmi2"]  # required for collapsed-stage
```

### §5.1. 9-state FSM and PC-as-state

Per asmjson dev.md §1-39 and parse_json_zmm_sax.S analysis (DAVID research agent):

State alphabet: V (value), O (object body), K (key expected), D (colon expected), C (comma-or-close), S (string body), F (false literal), R (true literal), A (null literal — "null" rhymes with "a-z" close enough).

Each state has its own classifier mask set (e.g., state V wants `,]` for done, S wants `"\`). State transitions happen by jumping to the state's entry label after each chunk's classification, with `r10` holding the next-state target across chunk-refetch boundaries. No state-variable memory traffic; the program counter *is* the state.

### §5.2. Classifier kernel per chunk

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

### §5.3. Dispatch table return + runtime selection

Per asmjson `src/lib.rs`:

```rust
pub fn build_parser(grammar: &str) -> Box<dyn TypedParser> {
    let backend = select_backend_for_cpu();
    match backend {
        BackendShape::CollapsedStageAvx512 if has_cpu_feature("avx512vbmi2") => {
            Box::new(CollapsedStageParser::for_grammar(grammar))
        }
        BackendShape::StructuralIndexNeon => Box::new(StructuralIndexParser::for_grammar(grammar)),
        BackendShape::EagerTape => Box::new(EagerTapeParser::for_grammar(grammar)),
    }
}
```

The selection happens once per parser construction; per-parse-call dispatch is inlined.

### §5.4. Projected impact

Twitter on Zen 4 AVX-512 VBMI2:

| Stage | c/B | Source |
|---|---:|---|
| Classify (vpcompressb + vpermi2b + vpternlogd fused) | ~0.15 | intrinsics agent estimate |
| FSM dispatch + emit | ~0.10 | asmjson published 10.93 GiB/s ÷ 4.5 GHz |
| Strings (mask-driven body skip; HasEsc-conditional decode) | ~0.10 | asmjson #7 |
| Numbers (SIMD digit-block fast-float on canada-shape only) | varies | |
| **Total twitter** | **~0.35** | **~12.8 GB/s twitter ~= 100K Mbps** |

This beats simdjson (3.0 GB/s) by ~4× and approaches asmjson's published 10.93 GiB/s (12.8 GB/s) on equivalent hardware. The collapsed-stage backend is the only realistic >SOTA route past simdjson on x86_64. On arm64 the equivalent leverage (PC-as-state + LD4-interleaved classify) is plausible but bounded by NEON's lack of `vpcompressb` analog; arm64 stays on the structural-index-driven template for V1.

## §6. Falsifiability + gates

Each phase carries an empirical gate; failure to land the gate routes back to re-profile and re-attribute, not to substrate amendment.

| Phase | LOC budget | Twitter T1 gate | Hot-leaf count gate | Twitter c/B gate | Cite |
|---|---:|---|---|---|---|
| Phase 0 (build profile + micro-cleanups) | ~15 | T1 ≥ 12K Mbps (delta from baseline) | n/a | n/a | Lock 15 enforcement |
| Phase 1 (NEON intrinsic upgrade in `bbnf-simd/aarch64/`) | ~70 | **T1 ≥ 14K Mbps (validation)** | ≤ 4 hot leaves | ≤ 1.9 c/B | §3.2 |
| Phase 2 (structural-index-driven codegen template; `BirNode::CursorDispatch`) | ~400 | **T1 ≥ 17K Mbps (SOTA-BEAT sonic-rs LazyValue)** | ≤ 3 hot leaves | ≤ 1.4 c/B | §4 |
| Phase 3 (AVX-512 VBMI2 backend + collapsed-stage option) | ~600 | T1 ≥ 25K Mbps on x86_64 AVX-512 hardware (BEAT simdjson) | ≤ 2 hot leaves on x86_64 | ≤ 0.9 c/B on x86_64 | §3.3 + §5 |
| Aspirational Phase 4 (collapsed-stage AVX-512BW asmjson-class) | ~800 | T1 ≥ 50K Mbps on x86_64 (asmjson parity) | 1 hot leaf | ≤ 0.45 c/B | §5 |

The cycle-per-byte gate is the load-bearing comparator-anchored metric; wall-clock Mbps depends on host clock speed and can drift across dev hardware (M1 Pro vs M5 Max vs Zen 4). The hot-leaf-count gate enforces fusion-quality parity with sonic-rs (1-2 leaves) and simdjson (2 leaves) — a parser carrying 5+ leaves at the same wall-clock is structurally bolted-on, regardless of throughput.

Each gate falsifies a distinct architectural claim:
- Phase 1 falsifies "the NEON scan kernel is already at ceiling."
- Phase 2 falsifies "the recursive-descent driver is unavoidable."
- Phase 3 falsifies "AVX-512 VBMI2 buys nothing simdjson doesn't already exploit."
- Phase 4 falsifies "asmjson's collapsed-stage architecture is not portable to a meta-grammar."

## §7. Implementation sequence

The full edicts live in `restart/skinny/IMPLEMENTATION-PACKET-SOTA-BEAT.md` (forthcoming, dispatched from this document). The summary sequence:

1. **Step 0** (~5 LOC, 2 min) — Lock 15 enforcement: `skinny/Cargo.toml [profile.release] lto=true codegen-units=1 panic="abort" debug=true`.
2. **Step 1** (~8 LOC, 10 min) — pre-size offset Vec at `input.len()/4`; delete `TapeAssembler::finish` shrink-to-fit pair.
3. **Step 2** (~25 LOC, 20 min) — `#[inline(always)]` on `JsonNodeKind::at_cursor`; fuse byte→kind into iterator `next`.
4. **Step 3** (~70 LOC, 4-6 hr) — Phase 1 NEON intrinsic upgrade in `bbnf-simd/aarch64/`: `vqtbl4q_u8` classifier + `vshrn_n_u16` movemask + `vld1q_u8_x4` quad-load.
5. **Step 4** (~50 LOC IR + 350 LOC codegen, 2-3 days) — `BirNode::CursorDispatch` variant + rust template generator rewrite + `HasEsc` flag emission + `set_len(0)` drop bypass.
6. **Step 5** (~1 hr) — comparative re-profile: produce `skinny/profile/skinny-v3-implemented/` and `skinny/profile/COMPARISON.md` against sonic-rs-v2 + simdjson-v2.
7. **Step 6** (~200 LOC, conditional, x86_64 only) — Phase 3 AVX-512 VBMI2 path in `bbnf-simd/x86_64/avx512_vbmi2/`.
8. **Step 7** (~600 LOC, conditional, post-Phase-3-validation) — Phase 4 collapsed-stage AVX-512BW asmjson-class backend.

Steps 0-5 must land before Steps 6-7; arm64 host gating discipline.

## §8. Locks affected

| Lock | Status | Rationale |
|---|---|---|
| Lock 1 (Tape substrate) | UNCHANGED | Structural-index-driven is a codegen template shape; the substrate (Tape + ValueRef + arena + DocumentView) is unchanged. ValueRef gains a `cursor: u32` field but this was already implied by `LAZY-TAPE-DESIGN.md` and survives the lazy-tape refutation as canonical for the structural-index route. |
| Lock 10 (cost model + shape miner) | EXTENDED-MECHANICAL | Shape miner detects `CursorDispatch` patterns from grammar shape; cost model selects backend (eager-tape / structural-index / collapsed-stage) per grammar. No new lock primitive; existing Lock 10 surface absorbs. |
| Lock 14 (grammar generalisation; zero overfitting) | UNCHANGED | The structural-index-driven template applies to *any* grammar with a dispatch hub (JSON, CSS L4, BBNF-self, Sheets); no grammar-specific code in `bbnf-simd` or the codegen template. |
| **Lock 15 NEW** (build-profile discipline) | NEW LOCK | Every generated runtime crate declares `lto=true`, `codegen-units=1`, `panic="abort"` (or `unwind` if grammar declares recovery), `debug=true` in `[profile.release]`. Evidence: sonic-rs hot-leaf count = 1 confirms LTO fuses entire SIMD kernel into parse driver. Without this lock, the codegen template inversion yields half its gain. See `14-LOCKS.md` §15. |
| **Lock 16 NEW** (SIMD/ASM admissibility allowlist) | NEW LOCK | Admissible SIMD primitives are an explicit allowlist with citations (Lock 16 enumerates them); hand-tuned undocumented intrinsic loops without architectural name = forbidden as magic. Handwritten `asm!` admissible only for intrinsics absent from `core::arch::*` (e.g., arm64 `ldp`/`stp`/`stnp`, `PRFM PLDL2STRM`). See `14-LOCKS.md` §16. |

## §9. Open residues

- **R1**: TS/WASM backend disposition for SIMD primitives. V2-deferred per Lock 8, but the `bbnf-simd` crate's types must be backend-agnostic at the trait boundary so V2 emitters can adopt WASM SIMD128 or TS WebAssembly SIMD without re-architecting. The trait surface in §3.1 already admits this; verification deferred to V2.
- **R2**: CSS L4 + Sheets + BBNF-self adoption of the structural-index-driven template. The shape applies (each has a dispatch hub); per-grammar measurement deferred to the future-grammar-onboarding test (`MASTER-PLAN §12`). No architectural amendment expected; only codegen template wiring.
- **R3**: Apple SME (Scalable Matrix Extension) on M4+ — flagged by intrinsics agent as not applicable for hot JSON parsing (entry/exit cost dwarfs per-block gain). Re-examine if M-series hardware gains SVE2 in future generations.
- **R4**: AMD Zen 4/5 VBMI2 `vpcompressb` store gotcha (Lemire 2025) — prefer `maskz_compress` + separate `storeu` on Zen 4 hosts; gate via runtime CPUID. Documented in `bbnf-simd/x86_64/avx512_vbmi2/compress.rs`.
- **R5**: The 18552 Mbps sonic-rs reference number is the LazyValue path which uses prefix-XOR; our skinny's `from_slice::<Value>` driver shape exercises the typed-Value-DOM path at ~2782 Mbps. The 7K Mbps gap framing remains correct against the LazyValue reference; honest comparison plane is documented in `BENCH.md` §6.

## §10. Verdict

This design is the empirical synthesis of four research streams (DAVID/asmjson; handwritten ASM 2024-2026 papers; SIMD intrinsics beyond sonic-rs/simdjson; comparative samply profiles of all three parsers) and survives the lazy-tape refutation by relocating the architectural lever from substrate representation to codegen template shape + per-target SIMD primitive selection. The eager-tape canonical (Lock 1) stands. The 14-lock corpus stands. The two new locks (15 + 16) add discipline without re-architecting.

The path to BEAT sonic-rs on arm64 (the host) is Phase 1 + Phase 2 (~470 LOC). The path to BEAT simdjson on x86_64 is Phase 1 + Phase 2 + Phase 3 (~670 LOC). The path past simdjson into asmjson territory is Phase 4 (collapsed-stage AVX-512BW), aspirational at ~800 LOC, gated on Phase 3 measurement.

Hereupon: dispatch `IMPLEMENTATION-PACKET-SOTA-BEAT.md` against the skinny workspace.
