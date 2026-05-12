# IMPLEMENTATION-PACKET-V2 — Multi-Wave SOTA-BEAT Implementation

Status: READY for dispatch as multi-wave sequence.
Supersedes: `restart/skinny/audit/IMPLEMENTATION-PACKET-SOTA-BEAT.md` (V1 packet predated the 2026-05-12 corpus-expansion + dav1d/ASM lift + yyjson force-inline lever + tape-union clarification; V2 carries all of these without contrivances).
Anchor spec: `restart/skinny/audit/SOTA-BEAT-DESIGN.md` (post-2026-05-12-amendments).
Empirical anchors: `skinny/profile/{skinny-expanded,sonic-rs-expanded,simdjson-expanded,yyjson,rapidjson,serde_json,asm-string-unicode}/PROFILE-REPORT.md`.

This packet carries verbatim implementation edicts as five waves, each independently measurable, each committable as one or more atomic commits. Boundary discipline: implementation lives entirely under `skinny/`; no `restart/` files modified during execution. The user has explicitly mandated **no contrivances, no new directives, no deferrals**.

## §0. Pre-flight

1. **Comparator anchors** (M5 Max twitter DOM-class, per `skinny/profile/`):
   - yyjson: 0.91 c/B = 3687 MiB/s (no SIMD; force-inline lever)
   - simdjson DOM: 1.142 c/B = 2923 MiB/s (two-stage SIMD)
   - sonic-rs Value-DOM: ~2.3 c/B = 2438 MiB/s (LTO fusion + NEON StringBlock)
   - **skinny current**: 5.07 c/B = 658 MiB/s (= 5521 Mbps per skinny formula `bytes*8000/ns`)
   - **Closure required**: 5.6× cycle reduction to beat yyjson; 4.4× to beat simdjson; 2.2× to beat sonic-rs Value-DOM.

2. **Memory disciplines (binding per `~/.claude/projects/.../memory/`)**:
   - `feedback_no_inline_tests`: all tests in `tests/` directory, never `#[cfg(test)]` in `src/`.
   - `feedback_test_output_to_file`: long cargo runs redirect to file once, grep/tail over file.
   - `feedback_no_polling_loops`: long-running bash via `run_in_background=true`; do not sleep-poll.
   - `feedback_iter_profile_always`: every iteration-loop carries `--profile ax-iter` if applicable.
   - `feedback_single_cargo_per_target`: at most one cargo invocation in flight per `CARGO_TARGET_DIR` at any instant.
   - `feedback_clean_regen_discipline`: generated files always come from fresh regen; never hand-patch.
   - `feedback_no_workarounds`: zero tolerance for stubs/fallbacks/legacy code.
   - `feedback_no_deferrals`: integrate everything into the current pass; no "phase X unlocks the performance" hedging.
   - `feedback_dispatch_hard_cap`: each wave caps explicitly per §1-§5 below.

3. **No new constructs**:
   - No new BBNF directives (Lock 10 auto-detect mandate stands).
   - No new BIR variants (20-variant alphabet stays; `Alt { Dispatch }` lowers to multiple access patterns based on `LayoutFacts.backend_shape`).
   - No new workspace metadata keys beyond `[workspace.metadata.bbnf.grammars.<name>.profile]` (per-grammar build profile overrides) — already established.
   - No grammar-specific code in any generic crate (Lock 14 verbatim).

## §1. Wave 1 — Lock 15 enforcement + tape-union migration (Phase 0)

**Effort**: ~200 LOC delete + ~50 LOC migrate + ~5 LOC Cargo.toml = ~255 LOC net delta. **Cap**: 4 hours.

**Goals**:
- Land `lto = "fat"` (explicit; verify via `cargo build --release -v 2>&1 | grep '\-C lto=fat'`).
- Land force-inline discipline on hot-path emitted functions (yyjson lever).
- Land i-cache budget verification (≤ 20 KiB target for hot fused function).
- Delete the three-Vec parallel-buffer pathology per the tape-union research agent.

### §1.1. Lock 15 Cargo.toml enforcement

Edit `skinny/Cargo.toml`:

```toml
[profile.release]
lto = "fat"               # NOT "thin"; verify via `cargo build --release -v`
codegen-units = 1
panic = "abort"
debug = true              # samply symbol resolution

[profile.bench]
inherits = "release"
debug = true

# Optional per-bench profile for fast iteration; do NOT use for SOTA-BEAT gates
[profile.ax-iter]
inherits = "release"
lto = "thin"              # fast iteration only; SOTA-BEAT gates run on `release`
codegen-units = 16
```

Verification:
```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo build --release -v 2>&1 | tee /tmp/lto-verify.log | grep -E '\-C lto=fat' | wc -l
# Must return ≥ 1 per workspace member (count via grep + sort -u)
```

### §1.2. Force-inline emission for hot rules

Edit `skinny/crates/codegen/src/lower/rust.rs` to emit `#[inline(always)]` on rules where `LayoutFacts.hot_call_graph[rule_id].is_hot()` is true. Initial cost-model implementation in `skinny/crates/passes/src/recognizers/hot_path.rs`:

```rust
pub fn derive_hot_path(grammar_ir: &Grammar, profile_hints: Option<&PriorBenchProfile>) -> HashMap<RuleId, HotPathFact> {
    // Static analysis: rules transitively reached from `parse_value` / `Entry` rule
    // with maximum depth ≤ 5 are hot-path candidates. Profile hints (when available
    // from prior bench runs in skinny/profile/skinny-expanded/) refine by self-time
    // threshold ≥ 1% of total parse cycles.
    // Returns HotPathFact { force_inline: bool, max_inline_size_hint: usize }.
    todo!()  // ~50 LOC
}
```

Codegen template at `skinny/crates/codegen/src/lower/rust.rs`:

```rust
fn lower_rule(&mut self, rule_id: RuleId, body: &BirNode) -> TokenStream {
    let inline_attr = if self.layout_facts.hot_call_graph[&rule_id].force_inline {
        quote! { #[inline(always)] }
    } else {
        quote! {}
    };
    let body_tokens = self.lower_node(body);
    quote! { #inline_attr fn parse_#rule_id<'i>(...) -> Result<..., ParseError> { #body_tokens } }
}
```

### §1.3. Tape-union migration

Per the tape-union research agent (`task #100`), delete the three-Vec parallel-buffer pathology:

(a) `skinny/crates/runtime/src/grammars/json/parser.rs`: delete `ParserState.structural_offsets: Vec<u32>` field; scan emits directly into `tape: TapeBuilder` via write-through.

(b) `skinny/crates/runtime/src/tape/mod.rs`: rename `TapeAssembler` → `TapeBuilder`; delete dead `TapeBuilder` (the eager-Vec<TapeToken> carrier at lines 223-292) before the rename; delete `TapeToken`, `NodeKindId`, `TokenFlags` PAYLOAD_CLASS constants (unused after eager-path retirement).

(c) `skinny/crates/runtime/src/tape/mod.rs`: fold three `Box<[u32]>` sidecars (`offsets`, `string_escape_offsets`, `string_control_offsets`) into one `offsets: Box<[u32]>` + one packed `flags: Box<[u8]>`. HAS_ESC = 0x01; HAS_CONTROL = 0x02.

(d) `skinny/crates/runtime/src/tape/scan.rs`: scan kernel writes through `TapeBuilder::push_offset(u32, u8)`; no intermediate Vec.

Net LOC: −180 delete + 30 flags fold + 20 write-through = ~−130 LOC. Eliminates one Vec<u32>-sized allocation + one `Vec::into_boxed_slice` shrink-copy per parse.

### §1.4. Verification

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo fmt --all
cargo test --workspace --release 2>&1 | tee /tmp/wave1-test.log
cargo run -p xtask --release -- bench-json 2>&1 | tee /tmp/wave1-bench.log
samply record --save-only --unstable-presymbolicate -o /tmp/wave1-twitter.json.gz -- ./target/release/profile-lazy twitter
```

**Wave 1 gate**: twitter T1 ≥ 950 MiB/s (~7600 Mbps per skinny formula); hot-leaf count ≤ 4; c/B ≤ 3.5.

**Commit**: `perf(skinny): Wave 1 — Lock 15 enforcement (lto=fat + force-inline + i-cache budget) + tape-union migration (delete ParserState.structural_offsets + dead TapeBuilder + three-Vec fold)`.

## §2. Wave 2 — arm64 NEON intrinsic upgrade (Phase 1)

**Effort**: ~150 LOC + tests. **Cap**: 8 hours.

**Goal**: New `crates/bbnf-simd/` crate replacing the JSON-overfit `crates/simd-scan/`. Per-target NEON kernels for byte classification, movemask, quad-load, cross-chunk byte-shift, digit-block MAC.

### §2.1. Crate scaffold

```
skinny/crates/bbnf-simd/
├── Cargo.toml
├── ext/x86/                  # vendored x86inc.asm + x86util.asm from ffmpeg libavutil
│   ├── x86inc.asm           # 1978 LOC, verbatim from ffmpeg HEAD
│   └── x86util.asm
├── src/
│   ├── lib.rs               # pub trait SimdClassifier; pub fn select_classifier()
│   ├── classifier.rs        # trait def + ClassifyResult struct
│   ├── aarch64/
│   │   ├── mod.rs
│   │   ├── classify_tbl4.rs   # vqtbl4q_u8 4-table classifier (Lemire 2019)
│   │   ├── movemask.rs        # vshrn_n_u16 + vsri + zip1 Validark trick
│   │   ├── quad_load.rs       # vld1q_u8_x4 wrapper
│   │   ├── string_block.rs    # quote/escape with HasEsc emission
│   │   ├── byte_context.rs    # vextq_u8 cross-chunk byte-context (NEW per dav1d lift)
│   │   ├── digit_mac.rs       # udot/sdot for digit-block accumulation (NEW per dav1d lift)
│   │   └── cache_hints.rs     # STNP + PRFM PLDL2STRM asm! wrappers
│   ├── x86_64/                # stubs at Wave 2; populated at Wave 4
│   │   ├── mod.rs
│   │   ├── avx512_vbmi2/      # empty modules
│   │   └── avx2/              # empty modules
│   ├── scalar/
│   │   ├── mod.rs
│   │   └── swar_8byte.rs      # SWAR fallback (asmjson #8 lineage; portable correctness floor)
│   └── dispatch.rs            # CPUID select_classifier()
└── tests/
    ├── classifier_parity.rs   # 256-byte parity vs scalar (per feedback_no_inline_tests)
    └── corpus_parity.rs       # 14-corpus parity vs scalar reference
```

### §2.2. Intrinsic implementation

Each kernel module per `SOTA-BEAT-DESIGN.md` §3.2 intrinsic catalog. Specific files:

- `aarch64/classify_tbl4.rs` (~30 LOC): single `vqtbl4q_u8` per 64-byte block; per-grammar 64-byte alphabet LUT loaded from `&'static [u8; 64]` const generated by `passes::recognizers` from Grammar IR's first-set union (no grammar names in source).
- `aarch64/movemask.rs` (~12 LOC): Validark `vshrn_n_u16` + `vsriq_n_u8` + `vzip1q_u8`.
- `aarch64/quad_load.rs` (~8 LOC): `vld1q_u8_x4` wrapper with `#[inline(always)]`.
- `aarch64/byte_context.rs` (~25 LOC): `vextq_u8` cross-window state propagation; abstract primitive applies to ANY grammar with chunk-spanning tokens.
- `aarch64/digit_mac.rs` (~40 LOC): `udot`/`sdot` 4-byte MAC for digit-block parsing. Abstract primitive lift from dav1d FIR-filter pattern; applies to ANY grammar's number primitive.
- `aarch64/cache_hints.rs` (~20 LOC): `asm!` blocks for STNP + PRFM PLDL2STRM (intrinsics absent from `core::arch::aarch64`).

### §2.3. parse-that integration trait

New file `skinny/crates/parse-that/src/integration/simd_scan_hook.rs` (~120 LOC) per parse-that audit (task #102):

```rust
pub trait SimdScannerHook {
    fn classify_chunk(&self, bytes: &[u8; 64]) -> ClassifyResult;
    fn alphabet(&self) -> &'static [u8; 64];
}

// Hooks into parse-that's regex DFA self-loop acceleration at accel.rs:108
// Extends the existing 1/2/3/4-8/9-64 byte-set fast paths with a 65-256 SIMD path
// when SimdScannerHook is provided.
```

### §2.4. Verification

```bash
cargo test -p bbnf-simd --release       # parity tests on all 14 corpora
cargo run -p xtask --release -- bench-json
samply record --save-only -o /tmp/wave2-twitter.json.gz -- ./target/release/profile-lazy twitter
```

**Wave 2 gate**: twitter T1 ≥ 1330 MiB/s; hot-leaf count ≤ 3; c/B ≤ 2.5; **parity tests pass on all 14 corpora + 95 JSONTestSuite y_string conformance tests**.

**Commits** (one per atomic step):
- `feat(bbnf-simd): crate scaffold with per-target submodules + vendored x86inc.asm`
- `feat(bbnf-simd/aarch64): vqtbl4q_u8 classifier + Validark movemask + vld1q_u8_x4 + StringBlock with HasEsc`
- `feat(bbnf-simd/aarch64): vextq_u8 cross-chunk byte-context + udot/sdot digit-block MAC (dav1d primitive lifts)`
- `feat(parse-that): integration/simd_scan_hook.rs trait; regex DFA self-loop accel extended for SIMD alphabets`

## §3. Wave 3 — Structural-index-driven lowering + corpus expansion (Phase 2 + BENCH §3.1)

**Effort**: ~50 LOC cost-model + ~400 LOC codegen lowerer + ~80 LOC HasEsc emission + ~20 LOC drop bypass + corpus-expansion LOC. **Cap**: 3 days.

### §3.1. LayoutFacts.backend_shape derivation

New module `skinny/crates/passes/src/recognizers/backend_shape.rs` (~50 LOC) per ARCH §7.3 derivation algorithm:

```rust
pub fn derive_backend_shape(grammar_ir: &Grammar, rule_id: RuleId, target_features: &TargetFeatures) -> BackendShape {
    // Algorithm per ARCH §7.3 §7.4:
    // 1. If transitive uses include ErrorDirective => EagerTape
    // 2. Else if rule body contains Call { kind: Host } parse-time decoded => EagerTape
    // 3. Else if rule body contains LayoutDirective => EagerTape
    // 4. Else if rule's Alt first-set has overlap => EagerTape (lowers as Speculative)
    // 5. Else if target_features.has("avx512vbmi2") AND hub with ≥4 byte-disjoint arms => CollapsedStage
    // 6. Else => StructuralIndex
}
```

Wired into existing `passes::layout::lower(...)` to extend `LayoutFacts` with the new field.

### §3.2. Codegen template lowering matrix

Edit `skinny/crates/codegen/src/lower/rust.rs` `lower_alt_dispatch` function. Read `LayoutFacts.backend_shape[rule_id]`; emit one of three access patterns per `SOTA-BEAT-DESIGN.md` §4.1. ~400 LOC.

### §3.3. HasEsc emission + lazy borrow + drop bypass

Per `SOTA-BEAT-DESIGN.md` §4.3 + §4.4. ~100 LOC.

### §3.4. Corpus expansion to 14 corpora

Per BENCH.md §3.1 (post-amendment). Download missing corpora + synthesize unicode_mixed + unicode_escapes per `xtask/src/bin/corpus_gen.rs`. Per `skinny/crates/test-fixtures/corpus/json/manifest.toml`.

### §3.5. UTF-8 + non-character codepoint correctness fixes

Per BENCH.md §7.9 Gate 1 + Gate 2:

- `skinny/crates/parse-that-regex/src/lib.rs:352`: replace `char::from_u32` with manual codepoint construction admitting non-characters; emit `BBNF-UNICODE-NONCHAR-CODEPOINT` warning.
- `skinny/crates/runtime/src/grammars/json/view.rs:203, 229`: replace `from_utf8().expect()` with scan-time validation via `simdutf8::basic::from_utf8` at parse boundary; emit `BBNF-UTF8-INVALID-AT-PARSE` on failure.

### §3.6. Verification

```bash
cargo test -p runtime -p codegen -p passes --release
cargo run -p xtask --release -- bench-json
cargo run -p xtask --release -- check-conformance   # 95 JSONTestSuite y_string files
samply record --save-only -o /tmp/wave3-twitter.json.gz -- ./target/release/profile-lazy twitter
```

**Wave 3 gate**: twitter T1 ≥ 2375 MiB/s (BEAT sonic-rs Value-DOM 2438 MiB/s); hot-leaf count ≤ 2; c/B ≤ 1.4; **all 14 corpora pass parity**; **95/95 JSONTestSuite y_string tests pass**; **float-bit-exact parity on canada/numbers/mesh/marine_ik**.

**Commits**:
- `feat(passes/recognizers): derive_backend_shape cost-model for LayoutFacts (Lock 10 auto-detect; no new directive)`
- `feat(codegen): Alt { Dispatch } lowering matrix (EagerTape / StructuralIndex / CollapsedStage access patterns)`
- `feat(runtime): HasEsc flag + lazy borrow in parse_string + set_len(0) drop bypass`
- `feat(test-fixtures): 14-corpus expansion + JSONTestSuite y_string conformance bundle`
- `fix(parse-that-regex): admit non-character codepoints per RFC 8259; BBNF-UNICODE-NONCHAR-CODEPOINT warning`
- `fix(runtime): scan-stage UTF-8 validation via simdutf8; BBNF-UTF8-INVALID-AT-PARSE diagnostic`

## §4. Wave 4 — x86_64 AVX-512 VBMI2 path (Phase 3)

**Effort**: ~200 LOC. **Cap**: 1-2 days. **Conditional**: AVX-512 VBMI2 hardware (Ice Lake+ / Zen 4+).

Per `SOTA-BEAT-DESIGN.md` §3.3. Populate `bbnf-simd/x86_64/avx512_vbmi2/{classify,compress,mask_fuse,carry}.rs`. CPUID dispatch at `bbnf-simd/x86_64/dispatch.rs`. AMD Zen 4 `vpcompressb` store gotcha (Lemire 2025): prefer `maskz_compress` + separate `storeu` on Zen 4.

GFNI `vgf2p8affineqb` classifier addition (Lock 16 addition per task #99 re-examination); 2× over PSHUFB; ~40 LOC.

**Wave 4 gate** (on x86_64 AVX-512 VBMI2 host): twitter T1 ≥ 3325 MiB/s (BEAT simdjson DOM 2923 MiB/s); hot-leaf count ≤ 2; c/B ≤ 1.0.

## §5. Wave 5 — Collapsed-stage AVX-512BW backend (Phase 4)

**Effort**: ~600 LOC. **Cap**: 3-5 days. **Conditional**: Wave 4 outcome A/B; cost-model auto-selects `CollapsedStage` via CPUID + grammar hub-arity.

Per `SOTA-BEAT-DESIGN.md` §5. 9-state FSM (V/O/K/D/C/S/F/R/A) with PC-as-state direct threading via `r10` in `asm!`. Feature-gated under `bbnf-runtime/avx512vbmi2`. Vendored `x86inc.asm` macros from `crates/bbnf-simd/ext/x86/`.

**Wave 5 gate**: twitter T1 ≥ 7400 MiB/s (asmjson 10.93 GiB/s parity territory); hot-leaf count = 1; c/B ≤ 0.45.

## §6. Closure posture

If Wave 3 misses the gate (twitter T1 < 2375 MiB/s), per `feedback_no_workarounds` + `feedback_redispatch_empty_return`: re-profile, re-attribute, do NOT amend Lock 1. The codegen template inversion + Lock 15 enforcement + bbnf-simd primitive layer are the levers; the substrate is bounded by the four-perturbation rejection cluster.

If Wave 3 lands the gate, Waves 4-5 are dispatchable in parallel on x86_64 hardware. Aspirational Phase 4 (Wave 5) is NOT on the V1 close gate but its cost-model selection lands at Wave 3 (the `CollapsedStage` arm in `derive_backend_shape`); Wave 5 is the implementation completion.

The grand-synthesis spec at `restart/skinny/audit/SOTA-BEAT-DESIGN.md` + Lock 15 + Lock 16 + this implementation packet are the **complete** specification surface for SOTA-BEAT closure across arbitrary grammars on host arm64 + x86_64. Dispatch hereupon.
