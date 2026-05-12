# IMPLEMENTATION-PACKET-SOTA-BEAT — Skinny v3 Structural-Index-Driven Codegen + SIMD Primitive Layer

Status: READY for dispatch.
Audience: implementation agent dispatched against `/Users/mkbabb/Programming/bbnf-lang/skinny/`.
Anchor spec: `restart/skinny/audit/SOTA-BEAT-DESIGN.md` (read end-to-end before dispatch).
Empirical anchor: `skinny/profile/{PROFILE-REPORT,ASM-REPORT,sonic-rs-v2/PROFILE-REPORT,simdjson-v2/PROFILE-REPORT}.md`.

This packet carries verbatim edicts in execution order. Each step is independently measurable; commit after each. Boundary: implementation lives entirely under `skinny/`; no `restart/` files modified during execution (spec amendments are upstream and pre-ratified per the grand-synthesis 2026-05-12 pass).

## §0. Pre-flight

1. **Baseline bench**: `cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo run -p xtask -- bench-json` — record outcome G (twitter T1 ≈ 11780 Mbps expected). Snapshot `skinny/RESULTS.md` content. The bench tool writes to `skinny/RESULTS.md` per its current shape; preserve the pre-Phase-0 row for delta computation.

2. **Falsifiability gates** (`BENCH.md` §6 + §7.9; SOTA-BEAT-DESIGN.md §6):

| Gate | Twitter T1 | Hot-leaf count | Cycle-per-byte | Phase |
|---|---|---|---|---|
| Phase 1 validation | ≥ 14000 Mbps | ≤ 4 | ≤ 1.9 c/B | NEON intrinsic upgrade alone |
| Phase 2 SOTA-BEAT sonic-rs | ≥ 17000 Mbps | ≤ 3 | ≤ 1.4 c/B | + structural-index-driven codegen |
| Phase 3 SOTA-BEAT simdjson on x86_64 | ≥ 25000 Mbps | ≤ 2 | ≤ 0.9 c/B | + AVX-512 VBMI2 path |
| Phase 4 asmjson-class on x86_64 | ≥ 50000 Mbps | 1 | ≤ 0.45 c/B | + collapsed-stage backend (aspirational) |

3. **Memory discipline** (binding): `feedback_no_inline_tests` (all tests in `tests/` directory, never `#[cfg(test)]` in `src/`); `feedback_test_output_to_file` (long cargo runs redirect to file once, grep/tail over the file); `feedback_no_polling_loops` (long-running bash via `run_in_background=true`; do not sleep-poll); `feedback_iter_profile_always` (every iteration-loop carries `--profile ax-iter` if applicable; bare forms are heavy-surface); `feedback_single_cargo_per_target` (at most one cargo invocation in flight per `CARGO_TARGET_DIR` at any instant); `feedback_clean_regen_discipline` (generated files are always output of fresh regen; never hand-patch); `feedback_no_workarounds` (zero tolerance for stubs/fallbacks/legacy code).

4. **HARD CAP** per `feedback_dispatch_hard_cap`: Steps 0-3 cap at 1 day total; Step 4 caps at 3 days with mandatory sub-step commits; Step 5 caps at 4 hr; Steps 6-7 conditional and dispatched separately when AVX-512 VBMI2 hardware is available.

5. **Triumvirate trigger** per `feedback_triumvirate_auto_trigger`: JSONL quiet > 15 min OR first-pass no-commit triggers 3-agent triumvirate (research / plan / redress); no user prompt required.

## §1. Step 0 — Lock 15 enforcement

**Effort**: ~5 LOC, 2 min.
**Lock**: 15 (build-profile discipline).
**Reference**: SOTA-BEAT-DESIGN.md §3.1 + §7 step 0; `sonic-rs-v2/PROFILE-REPORT.md` §(d) hot-leaf count = 1 empirical evidence.

**Edit**: `skinny/Cargo.toml` add to `[profile.release]`:

```toml
[profile.release]
lto = true
codegen-units = 1
debug = true        # samply symbol resolution per feedback_samply_symbols
panic = "abort"     # JSON has no @error(recover), so abort is safe; if a recovery-bearing grammar joins the workspace, override per-grammar via [workspace.metadata.bbnf.grammars.<name>.profile]
```

**Verification**:
```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo build --release
cargo run -p xtask -- bench-json 2>&1 | tee /tmp/skinny-bench-step0.log
grep -E "twitter|citm|canada" /tmp/skinny-bench-step0.log
```

**Expected delta**: twitter T1 may rise 5-15% from LTO fusion alone (sonic-rs's NOINLINE wall-clock falls 2.1-3.2× on twitter/citm; reverse-projected, the LTO gain is bounded but non-zero on the skinny). Record the delta in `skinny/RESULTS.md` under a new "step0_lto_build_profile" row.

**Commit**: `perf(skinny): Lock 15 enforcement — lto=true codegen-units=1 in release profile`.

## §2. Step 1 — Pre-size offset Vec + delete shrink-to-fit pair

**Effort**: ~8 LOC, 10 min.
**Reference**: `skinny/profile/ASM-REPORT.md` §3 `TapeAssembler::finish` 2× `__rust_realloc` finding; ASM agent suspicious-functions #2 (`simd_scan::scan_json_parse_index` calls `grow_one`+`__rust_alloc`+`__rust_dealloc` in inner loop).

**Edits**:

(a) `skinny/crates/runtime/src/tape/scan.rs` (or wherever `scan_json_parse_index` lives; grep `pub fn scan_json_parse_index` in `skinny/crates/runtime/src/`): replace initial `Vec::new()` with `Vec::with_capacity(input.len() / 4)`. Tight bound: 4-byte JSON token (`null`/`true`) is the minimum structural unit; the Vec capacity is an over-estimate that grows by at most one realloc on pathological corner cases.

```rust
// Before:
let mut offsets: Vec<u32> = Vec::new();
// After:
let mut offsets: Vec<u32> = Vec::with_capacity(input.len() / 4);
```

Same for the escape candidate Vec at the same call site.

(b) `skinny/crates/runtime/src/tape/assembler.rs::TapeAssembler::finish()`: delete the two `shrink_to_fit()` calls (one for the offsets Vec, one for the escapes Vec). Per ASM-REPORT.md, each generates `__rust_realloc` per parse with zero benefit at our scale.

```rust
// Before:
pub fn finish(self) -> Tape<'input> {
    let mut offsets = self.offsets;
    offsets.shrink_to_fit();
    let mut escapes = self.escapes;
    escapes.shrink_to_fit();
    Tape { offsets, escapes, ... }
}
// After:
pub fn finish(self) -> Tape<'input> {
    Tape { offsets: self.offsets, escapes: self.escapes, ... }
}
```

**Verification**: re-bench; expect 2-5% twitter T1 gain. Record delta as "step1_presize_no_shrink".

**Commit**: `perf(runtime): pre-size offset Vec at input.len()/4; delete TapeAssembler::finish shrink-to-fit pair`.

## §3. Step 2 — Force-inline at_cursor + fuse dispatch

**Effort**: ~25 LOC, 20 min.
**Reference**: `skinny/profile/ASM-REPORT.md` §3 finding (a): `JsonNodeKind::at_cursor` is NOT inlined; carries the only indirect branch (byte→kind jump table) + 2 redundant bounds checks per call.

**Edits**:

(a) `skinny/crates/runtime/src/grammars/json/view.rs`: add `#[inline(always)]` to `JsonNodeKind::at_cursor`.

(b) Verify via `cargo asm`:
```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo asm -p runtime --rust runtime::grammars::json::view::JsonNodeKind::at_cursor > /tmp/at_cursor.s
# Expected: function does not appear (fully inlined into call sites)
grep -c "JsonNodeKind.*at_cursor" /tmp/at_cursor.s  # ideally 0
```

(c) If `at_cursor` still appears (LLVM didn't honour the hint due to dispatch density), hoist the byte→kind dispatch into `JsonArrayValues::next` / `JsonObjectPairs::next` bodies directly (10-15 LOC of duplication; acceptable per `feedback_pluggable_components` because the dispatch is the hot site).

**Verification**: re-bench; expect 1-3% twitter T1 gain. Record delta as "step2_inline_at_cursor".

**Commit**: `perf(runtime): #[inline(always)] on JsonNodeKind::at_cursor; fuse byte→kind dispatch into iterators`.

## §4. Step 3 — Phase 1 NEON intrinsic upgrade

**Effort**: ~70 LOC, 4-6 hr.
**Lock**: 16 (SIMD admissibility allowlist).
**Reference**: SOTA-BEAT-DESIGN.md §3.1 (`bbnf-simd` crate shape) + §3.2 (arm64 NEON primary path); MASTER-PLAN.md §13.1 admissible primitives table.

### §4.1. Scaffold `crates/bbnf-simd/`

New crate path: `skinny/crates/bbnf-simd/`. Add to `skinny/Cargo.toml` workspace members.

```
skinny/crates/bbnf-simd/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── classifier.rs       — pub trait SimdClassifier + ClassifyResult
│   ├── aarch64/
│   │   ├── mod.rs          — pub fn new_neon_classifier() -> impl SimdClassifier
│   │   ├── classify_tbl4.rs  — vqtbl4q_u8 classifier
│   │   ├── movemask.rs     — vshrn_n_u16 + vsri + zip1 Validark trick
│   │   ├── load.rs         — vld1q_u8_x4 quad-load helpers
│   │   └── string_block.rs — NEON quote/escape with HasEsc emission
│   ├── x86_64/             — stubs only at Step 3; populated at Step 6
│   │   ├── mod.rs
│   │   └── avx512_vbmi2/   — empty modules until Step 6
│   ├── scalar/
│   │   ├── mod.rs
│   │   └── swar_8byte.rs   — SWAR fallback (asmjson #8 lineage)
│   └── dispatch.rs         — CPUID-style runtime selector
└── tests/
    ├── classifier_parity.rs    — exhaustive 256-byte parity vs scalar (per feedback_no_inline_tests)
    └── corpus_parity.rs        — twitter/citm/canada parity vs scalar reference
```

`Cargo.toml`:
```toml
[package]
name = "bbnf-simd"
edition = "2021"

[features]
default = []
arm64-neon = []  # default-on for arm64 hosts via target_arch detection
x86_64-avx2 = []  # default-on for x86_64 hosts with AVX2
x86_64-avx512-vbmi2 = []  # opt-in; CPUID gated
```

### §4.2. NEON `vqtbl4q_u8` 4-table classifier

`src/aarch64/classify_tbl4.rs`:
```rust
use core::arch::aarch64::*;

#[target_feature(enable = "neon")]
#[inline(always)]
pub unsafe fn classify_64bytes(bytes: &[u8; 64]) -> ClassifyMasks {
    // Load 64 bytes in ONE instruction (vld1q_u8_x4)
    let q = vld1q_u8_x4(bytes.as_ptr());
    
    // 4-table classifier; the 64-byte class table covers all of:
    //   structural (0x7B '{', 0x7D '}', 0x5B '[', 0x5D ']', 0x2C ',', 0x3A ':', 0x22 '"')
    //   whitespace (0x20, 0x09, 0x0A, 0x0D)
    //   escape (0x5C '\\')
    //   other (default class)
    // ONE vqtbl4q_u8 per 16-byte input lane = 4 instructions for 64 bytes
    static CLASS_TABLE: [u8; 64] = build_class_table();  // const-fn
    let table = vld1q_u8_x4(CLASS_TABLE.as_ptr());
    
    let c0 = vqtbl4q_u8(table, q.0);
    let c1 = vqtbl4q_u8(table, q.1);
    let c2 = vqtbl4q_u8(table, q.2);
    let c3 = vqtbl4q_u8(table, q.3);
    
    ClassifyMasks { c0, c1, c2, c3 }
}
```

Citation: Lemire 2019 "Arbitrary byte-to-byte maps using ARM NEON" (Lock 16 allowlist row 1). Replaces sonic-rs's 1-table `vqtbl1q_u8` classifier; saves ~16 c/64B per intrinsics agent quantification.

### §4.3. Validark interleaved-vector movemask

`src/aarch64/movemask.rs`:
```rust
use core::arch::aarch64::*;

#[target_feature(enable = "neon")]
#[inline(always)]
pub unsafe fn movemask_64bytes_interleaved(masks: ClassifyMasks) -> u64 {
    // Validark's "interleaved-vector movemask" trick:
    //   - vshrn_n_u16 shifts each 16-bit lane right and narrows to 8-bit
    //   - vsri inserts the shift result into the destination's low bits
    //   - vzip1 interleaves; final low 64 bits of the resulting q-reg is the movemask
    // 3 SIMD insns vs Mula/Lemire's 6-insn pmovmskb-substitute
    let s0 = vshrn_n_u16(vreinterpretq_u16_u8(masks.c0), 4);
    let s1 = vshrn_n_u16(vreinterpretq_u16_u8(masks.c1), 4);
    let s2 = vshrn_n_u16(vreinterpretq_u16_u8(masks.c2), 4);
    let s3 = vshrn_n_u16(vreinterpretq_u16_u8(masks.c3), 4);
    
    let merged_lo = vsriq_n_u8(s0, s1, 4);
    let merged_hi = vsriq_n_u8(s2, s3, 4);
    let zipped = vzip1q_u8(merged_lo, merged_hi);
    
    vgetq_lane_u64(vreinterpretq_u64_u8(zipped), 0)
}
```

Citation: validark.dev/posts/interleaved-vectors-on-arm/ (Validark 2024). Lock 16 allowlist row 2. Replaces sonic-rs's AND-OR tree; 4× faster bitmap synthesis.

### §4.4. Quad-load `vld1q_u8_x4`

`src/aarch64/load.rs`:
```rust
use core::arch::aarch64::*;

#[target_feature(enable = "neon")]
#[inline(always)]
pub unsafe fn load_64bytes(ptr: *const u8) -> uint8x16x4_t {
    vld1q_u8_x4(ptr)
}
```

Citation: Arm A64 ISA. Lock 16 allowlist row 3. Single-instruction 64-byte load (vs 4× separate `vld1q_u8`); frees 2 M-series load-ports during dependent classification.

### §4.5. NEON StringBlock with HasEsc

`src/aarch64/string_block.rs`:
```rust
use core::arch::aarch64::*;

#[target_feature(enable = "neon")]
#[inline(always)]
pub unsafe fn scan_string_block(
    bytes: &[u8; 64],
    in_string: bool,
) -> StringBlockResult {
    let q = load_64bytes(bytes.as_ptr());
    let quote_mask = vceqq_u8(q.0, vdupq_n_u8(b'"'));    // bit per quote byte
    let bs_mask    = vceqq_u8(q.0, vdupq_n_u8(b'\\'));   // bit per backslash
    
    // ... combine via vbslq_u8 for branchless quote/escape resolution
    // Set HAS_ESC flag in result if any bs_mask bit is non-zero inside string body
    
    StringBlockResult {
        ends_at: /* offset of closing quote */,
        has_escape: /* boolean */,
    }
}
```

Citation: Arm A64 ISA + sonic-rs `src/util/arch/aarch64.rs` lineage (adopt baseline, add HasEsc flag emission). Lock 16 allowlist row 4.

### §4.6. Tests

`skinny/crates/bbnf-simd/tests/classifier_parity.rs`:

Exhaustive 256-byte parity vs scalar reference: for every byte value 0x00..=0xFF, build a 64-byte block of that byte, run the NEON classifier + scalar reference, assert equal classifyResult bitmasks. Boundary cases: all-quote block; all-whitespace block; all-backslash; mixed structural.

`skinny/crates/bbnf-simd/tests/corpus_parity.rs`:

Load `skinny/test_data/{twitter,citm_catalog,canada}.json`; run NEON scan + scalar scan; assert offsets and HasEsc flags are identical.

### §4.7. Wire into runtime

`skinny/crates/runtime/src/tape/scan.rs`: replace the existing NEON path with calls into `bbnf_simd::aarch64::{classify_tbl4, movemask, load, string_block}`. The scalar fallback remains via `bbnf_simd::scalar::swar_8byte` for non-NEON hosts (rare in 2026+ but real per Lock 16 SWAR baseline).

**Verification**: re-bench; expect twitter T1 ≥ 14000 Mbps (Phase 1 validation gate). If gate misses, run samply via:
```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
samply record --save-only --unstable-presymbolicate -o /tmp/skinny-step3.json.gz \
    ./target/release/profile-lazy
```
Compare hot-leaf count + scan class self-time against pre-Phase-1 baseline.

**Commit**: `perf(skinny): Phase 1 NEON intrinsic upgrade — vqtbl4q_u8 + vshrn_n_u16 movemask + vld1q_u8_x4`.

## §5. Step 4 — Phase 2 codegen template inversion

**Effort**: ~50 LOC IR + 350 LOC codegen + 80 LOC HasEsc + 20 LOC drop bypass = ~500 LOC total, 2-3 days.
**Reference**: SOTA-BEAT-DESIGN.md §2 + §4; COMPILER.md §3.3 + §3.4 normative codegen contract.

### §5.1. Add `BirNode::CursorDispatch` variant

`skinny/crates/codegen/src/ir/bir.rs` (grep `enum.*Bir` or `enum BirNode` in `skinny/crates/codegen/src/`): add the new variant per ARCH §7.2 amendment table:

```rust
pub enum BirNode {
    // ... existing 20 variants
    CursorDispatch {
        arms: Vec<(DispatchByteSet, BirId)>,
        fallthrough: BirId,
    },
}

#[derive(Clone, Debug)]
pub struct DispatchByteSet {
    /// A single byte value or a range (e.g., b'0'..=b'9').
    /// Multiple discrete bytes lower to multiple match arms; ranges lower to range patterns.
    pub bytes: SmallVec<[u8; 8]>,
    pub range: Option<(u8, u8)>,
}
```

Wire into pattern-match exhaustiveness in every consumer of `BirNode` (the rust lowerer, the VM, the snapshot tests).

### §5.2. Codegen template rewrite

`skinny/crates/codegen/src/lower/rust.rs`: emit cursor-walk shape when `[workspace.metadata.bbnf.grammars.<name>.runtime] backend_shape = "structural-index"` is set (default for JSON post-Step-4):

```rust
// Pseudocode for the lowerer:
fn lower_cursor_dispatch(&mut self, arms: &[(DispatchByteSet, BirId)], fallthrough: BirId) -> TokenStream {
    let dispatch_byte = quote! { source[offsets[*cursor as usize] as usize] };
    let arm_tokens = arms.iter().map(|(set, body_id)| {
        let pattern = lower_dispatch_byte_set(set);
        let body = self.lower(*body_id);
        quote! { #pattern => { #body } }
    });
    let fallthrough_body = self.lower(fallthrough);
    quote! {
        match #dispatch_byte {
            #(#arm_tokens),*
            _ => { #fallthrough_body }
        }
    }
}
```

Emit per-rule per-shape:

| Grammar shape | Emit |
|---|---|
| Top-level `parse_value` | `parse_value` matches on cursor byte; no `skip_ws`; arms call `parse_object`/`parse_array`/`parse_string`/`parse_number`/`parse_literal` |
| `parse_object` | `*cursor += 1` (consume `{`); loop: peek `source[offsets[*cursor]]`; break on `}`; parse_pair; consume separator via `*cursor += 1` |
| `parse_array` | Same shape with `[`/`]`. |
| `parse_pair` | `parse_string_cursor` → consume `:` via `*cursor += 1` → `parse_value` |
| `parse_string` | Read open quote at `offsets[*cursor]`; advance; read close quote at `offsets[*cursor]`; check `flags[*cursor-1] & HAS_ESC`; borrow or decode |
| `parse_number` | Read start at `offsets[*cursor]`; advance; read end at `offsets[*cursor]`; lazy borrow span |
| `parse_literal` | Read 4-byte memcmp at `offsets[*cursor]..+4`; advance |

### §5.3. HasEsc flag at scan time

`skinny/crates/runtime/src/tape/assembler.rs`: extend `TapeAssembler` to emit a per-string-boundary flag.

```rust
pub struct TapeAssembler {
    offsets: Vec<u32>,
    flags: Vec<u8>,   // 1 byte per offset; FLAG_HAS_ESC = 0x01 on closing-quote-of-string entries
    // ...
}

const FLAG_HAS_ESC: u8 = 0x01;

impl TapeAssembler {
    pub fn push_string_close(&mut self, offset: u32, has_escape: bool) {
        self.offsets.push(offset);
        self.flags.push(if has_escape { FLAG_HAS_ESC } else { 0 });
    }
    // ... other push_* methods set flags = 0
}
```

In the generated `parse_string_cursor` (emitted by §5.2):

```rust
fn parse_string<'i>(source: &'i [u8], offsets: &[u32], flags: &[u8],
                   cursor: &mut u32, arena: &Arena) -> Result<JsonString<'i>, ParseError> {
    let start_off = offsets[*cursor as usize];
    *cursor += 1;
    let end_off = offsets[*cursor as usize];
    let has_esc = flags[*cursor as usize] & FLAG_HAS_ESC != 0;
    *cursor += 1;
    let body = &source[(start_off as usize + 1)..(end_off as usize)];
    if !has_esc {
        Ok(JsonString::Borrowed(unsafe { std::str::from_utf8_unchecked(body) }))
    } else {
        Ok(JsonString::Decoded(decode_escapes(body, arena)?))
    }
}
```

### §5.4. Set_len(0) drop bypass

`skinny/crates/runtime/src/tape/mod.rs`: in `Tape::drop` (or equivalent), when the running summary `any_string_has_escape` is false (track this in the assembler and persist to `Tape`), call `self.offsets.set_len(0)` before drop to bypass per-element Drop iteration on u32 offsets.

```rust
impl<'input> Drop for Tape<'input> {
    fn drop(&mut self) {
        if !self.any_string_has_escape {
            // Safety: u32 has no Drop impl; set_len(0) is sound and avoids per-element Drop
            unsafe { self.offsets.set_len(0); }
        }
        // Vec free path now sees an empty vec; one dealloc call
    }
}
```

### §5.5. Parity testing

`skinny/crates/runtime/tests/cursor_dispatch_parity.rs`:

Fuzz-corpus parity test: 10K randomly-generated JSON documents; old recursive-descent parser output vs new cursor-walk parser output must match byte-for-byte (or structurally for floating-point) on the entire skinny test corpus + the fuzz corpus.

`skinny/crates/runtime/tests/check_json_parity.rs`: exists per current state; re-run after Step 4 lands; must pass.

### §5.6. Dispatch density tuning

The `match source[offsets[*cursor]] { ... }` emitted by the codegen template should compile to a jump table when LLVM judges arm density sufficient. Per `feedback_pluggable_components`: the dispatch-shape strategy is pluggable:

- Default (stable Rust): `match` with `core::hint::likely` on hot arms (`b'"'` and `b'{'`/`b'['` on twitter; `b'-'`/`b'0'..=b'9'` on canada).
- Nightly opt-in: `asm!` with indirect branch through `&[unsafe extern "C" fn]` table. This is NOT the same as the function-pointer dispatch table previously rejected at REDRESS-17 (which was call-site indirection); this is jump-table dispatch with inlined targets.

Start with the stable shape; profile via samply post-Step-4; if dispatch is > 5% self-time, escalate to nightly + `asm!`.

**Verification**: re-bench; expect twitter T1 ≥ 17000 Mbps (Phase 2 SOTA-BEAT sonic-rs gate). Run samply and verify hot-leaf count ≤ 3 at ≥10% self-time on twitter. Run `cargo run -p xtask -- check-json` (parity oracle) — must pass.

**Commits** (one per sub-step):
- `feat(codegen): add BirNode::CursorDispatch variant (ARCH §7.2 amendment)`
- `feat(codegen): emit cursor-walk shape for backend_shape = "structural-index" grammars`
- `feat(runtime): HasEsc flag at scan time + lazy borrow in parse_string`
- `perf(runtime): set_len(0) drop bypass when any_string_has_escape is false`
- `test(runtime): cursor-walk parser parity fuzz corpus (10K docs)`

## §6. Step 5 — Comparative re-profile

**Effort**: ~1 hr.
**Reference**: BENCH.md §7.9 comparative-profile primitive.

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
mkdir -p profile/skinny-v3-implemented

# Profile the new build
samply record --save-only --unstable-presymbolicate \
    -o profile/skinny-v3-implemented/twitter.profile.json.gz \
    ./target/release/profile-lazy twitter

samply record --save-only --unstable-presymbolicate \
    -o profile/skinny-v3-implemented/citm.profile.json.gz \
    ./target/release/profile-lazy citm

samply record --save-only --unstable-presymbolicate \
    -o profile/skinny-v3-implemented/canada.profile.json.gz \
    ./target/release/profile-lazy canada
```

Write `skinny/profile/skinny-v3-implemented/PROFILE-REPORT.md` with sections (a)-(f) per BENCH.md §7.9.

Write `skinny/profile/COMPARISON-v3.md` (new file) with the cross-parser hot-leaf table and cycle-per-byte table:

| Parser | Twitter c/B | Hot-leaf count | Twitter Mbps | Source |
|---|---:|---:|---:|---|
| simdjson DOM | 1.142 | 2 | ~24500 | `simdjson-v2/PROFILE-REPORT.md` |
| sonic-rs LazyValue | ~1.5 | 1 | 18552 | `sonic-rs-v2/PROFILE-REPORT.md` (reference) |
| **skinny-v3-implemented** | _measured_ | _measured_ | _measured_ | this run |
| skinny pre-Phase-1+2 | ~2.5 | 5+ | 11780 | `PROFILE-REPORT.md` (baseline) |

**Commit**: `docs(skinny/profile): comparative re-profile post-Phase-1+2; SOTA-BEAT-status documented`.

## §7. Step 6 — Phase 3 AVX-512 VBMI2 path (conditional)

**Effort**: ~200 LOC, 1-2 days on x86_64 hardware.
**Conditional**: AVX-512 VBMI2 hardware availability (Ice Lake / Tiger Lake / Sapphire Rapids / Zen 4 / Zen 5).
**Reference**: SOTA-BEAT-DESIGN.md §3.3; MASTER-PLAN.md §13.1 admissible primitives table for x86_64 AVX-512 VBMI2.

### §7.1. Populate `bbnf-simd/x86_64/avx512_vbmi2/`

Four files per SOTA-BEAT-DESIGN.md §3.1:

- `classify.rs` — `vpermi2b` 128-byte byte-shuffle classifier
- `compress.rs` — `_mm512_mask_compressstoreu_epi8` one-shot offset emission (the simdjson-leaves-on-the-table primitive per `icelake/simd.h:157`)
- `mask_fuse.rs` — `_mm512_ternarylogic_epi64` 3-mask boolean fusion
- `carry.rs` — `_mm512_alignr_epi8` cross-window quote-state carry

Plus `bbnf-simd/x86_64/avx2/`:

- `classify.rs` — `_mm256_shuffle_epi8` AVX-2 fallback
- `bmi2_emit.rs` — `_pdep_u64` bits-to-indexes (Mula 2018) for non-VBMI2 hosts
- `prefix_xor.rs` — `_mm_clmulepi64_si128` (simdjson baseline; adopt rather than reinvent)

CPUID dispatch at `bbnf-simd/x86_64/dispatch.rs`:

```rust
pub fn select_classifier() -> Box<dyn SimdClassifier> {
    if is_x86_feature_detected!("avx512vbmi2") && is_x86_feature_detected!("avx512bw") {
        Box::new(avx512_vbmi2::Classifier::new())
    } else if is_x86_feature_detected!("avx2") && is_x86_feature_detected!("bmi2") {
        Box::new(avx2::Classifier::new())
    } else {
        Box::new(scalar::Classifier::new())
    }
}
```

### §7.2. AMD Zen 4 `vpcompressb` store gotcha

Per `R4` in SOTA-BEAT-DESIGN.md §9 (Lemire 2025): on AMD Zen 4, prefer `maskz_compress` + separate `storeu` over `mask_compressstoreu_epi8`. Gate via CPUID vendor check:

```rust
fn compress_store_avx512_vbmi2(mask: __mmask64, data: __m512i, dst: *mut u8) {
    if is_amd_zen4_or_later() {
        let compressed = _mm512_maskz_compress_epi8(mask, data);
        _mm512_storeu_si512(dst as *mut __m512i, compressed);
    } else {
        _mm512_mask_compressstoreu_epi8(dst, mask, data);
    }
}
```

### §7.3. Verification on x86_64

```bash
# On Ice Lake / Zen 4 host:
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo build --release --target x86_64-unknown-linux-gnu
cargo run -p xtask -- bench-json
```

**Expected**: twitter T1 ≥ 25000 Mbps (SOTA-BEAT simdjson DOM 24500 anchor). Hot-leaf count ≤ 2.

**Commit**: `perf(bbnf-simd): Phase 3 x86_64 AVX-512 VBMI2 path — vpcompressb + vpternlogd + vpermi2b`.

## §8. Step 7 — Phase 4 collapsed-stage backend (aspirational)

**Effort**: ~600 LOC, 3-5 days.
**Conditional**: H.W4 outcome A/B validation + AVX-512 VBMI2 hardware.
**Reference**: SOTA-BEAT-DESIGN.md §5.

Implementation lives at `skinny/crates/runtime/src/backends/collapsed_stage_avx512/`:

- 9-state explicit FSM (V/O/K/D/C/S/F/R/A) per asmjson `doc/dev.md:1-39`
- PC-as-state direct threading via `asm!` indirect branch on `r10`
- Feature-gated: `bbnf-runtime/avx512vbmi2`
- Per-grammar opt-in via `[workspace.metadata.bbnf.grammars.<name>.runtime] backend_shape = "collapsed-stage"`

This is aspirational. Not on the V1 close gate. Dispatched as a parallel-emitter dispatch only after Phase 3 measurement validates the AVX-512 VBMI2 path against the simdjson anchor.

## §9. Closing posture

Implementation closure ordering:

1. ✅ Steps 0-3 (Phase 1 prep + NEON intrinsic upgrade) — arm64 host primary.
2. ✅ Step 4 (Phase 2 structural-index-driven codegen) — arm64 host SOTA-BEAT sonic-rs.
3. ✅ Step 5 (comparative re-profile) — empirical validation.
4. ⚠️ Step 6 (Phase 3 AVX-512 VBMI2) — x86_64 SOTA-BEAT simdjson; conditional on hardware.
5. ⚠️ Step 7 (Phase 4 collapsed-stage) — aspirational asmjson-class; conditional on Step 6 validation.

If Step 4 misses the Phase 2 gate (twitter T1 < 14K Mbps), per `feedback_redispatch_empty_return` + `feedback_no_workarounds`: re-profile, re-attribute, do **not** amend Lock 1. The codegen template inversion is the lever, not the substrate. Open a new SOTA-BEAT-DESIGN.md amendment cycle (`SK-V3-AMENDMENT-XXX`) if a new architectural insight emerges; the four-perturbation substrate-rejection cluster bounds the substrate-amendment search space empirically and is not to be re-litigated.

The grand-synthesis spec at `restart/skinny/audit/SOTA-BEAT-DESIGN.md` + Lock 15 + Lock 16 + this implementation packet are the **complete** specification surface for the SOTA-BEAT closure. Dispatch hereupon.
