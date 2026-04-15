# Research 01 — Grammar-Parameterised Structural Bitmap (SIMD beyond memchr)

*Verbatim deliverable from architecture research agent, April 2026.
Informs AU.2.7 (scanner-only v2) and AV.2 (stage-1 primitive of the
Dispatch Tape Automaton).*

---

## AU Architecture Research — Higher-Dimensional SIMD for Parser Kernels

### 1. Angle headline

**Grammar-parameterised structural-bitmap pre-pass**: a codegen-emitted, per-grammar SIMD scanner that produces, in a single vectorised pass over the padded input, a 64-bit-per-chunk structural bitmap whose set bits are exactly the grammar's structural alphabet — then a scalar driver consumes the bitmap via `CTZ`/`PEXT`, never re-scanning, never bounds-checking. One kernel per grammar, not per needle count.

### 2. Motivation tied to wave-2 hotspots

- `scan_ws_block_comments_slow` is 11.1–13.1% of CSS self-time across all three stylesheets (wave-2 matrix, `docs/tranches/AU/profiling-2.md:144`). The SIMD inner loop short-circuits early on dense inputs; samples pile up in the scalar tail.
- `memchr::memchr::{closure#0}` is 7–19% on string-heavy JSON (`twitter`, `data`, `data_xl` — wave-2 table, profiling-2.md:133). One needle per invocation, one 16-byte vector cycle per needle — expensive when the grammar naturally asks four structural questions at once.
- BBNF `__big_comment` pairs `memchr(b'*')` + 2-byte pointer-cast to detect `*/` (`docs/tranches/AU/profiling-2.md:222`); cost is 9.3–14.9% of leaf samples. Each `*` miss re-scans. A structural bitmap identifies every `*/` joint in one pass.
- Current codegen primitives live at `crates/core/src/generate/regex/emit/simd.rs` and top out at **8 targets** via nibble-LUT, **3 needles** via memchr3 — beyond that it degrades to a byte loop (`simd.rs:46,112`). CSS's structural alphabet is `{}();:,` plus `/*` and `*/` digraphs — already at the nibble-LUT ceiling and about to exceed it once attr selectors and `@`-rules land.

### 3. Novel idea — `scan_structural_<grammar>` kernel sketch

At codegen time, walk the grammar IR (`Lit`, `CharClass`, negated-class, punct-cluster) and collect two sets:
- `S` = structural bytes (the union of all non-whitespace terminal-starter bytes that cross a rule boundary),
- `D` = 2-byte structural digraphs (`/*`, `*/`, `-->`, `<!--`, `||`, `:=`, etc.).

Emit a per-grammar NEON kernel (plus an AVX-512 sibling, same shape):

```rust
// Padded input: [u8; N + 64] — prerequisite AU.6.1.
// For each 64-byte stripe we emit a 64-bit "structural word".
// AArch64 path, shown for |S| ≤ 16 and |D| ≤ 4:
#[target_feature(enable = "neon")]
unsafe fn scan_structural_css(src: &[u8], bits: &mut [u64]) {
    // Static nibble-LUT pair derived from S at codegen time.
    let lo = vld1q_u8(STATIC_LO.as_ptr());
    let hi = vld1q_u8(STATIC_HI.as_ptr());
    let mut i = 0;
    while i + 64 <= src.len() {
        // Four 16-byte loads = one 64-byte row.
        let v0 = vld1q_u8(src.as_ptr().add(i));
        let v1 = vld1q_u8(src.as_ptr().add(i + 16));
        let v2 = vld1q_u8(src.as_ptr().add(i + 32));
        let v3 = vld1q_u8(src.as_ptr().add(i + 48));

        // vqtbl1q_u8: byte → lo/hi nibble membership test (one insn each).
        let m0 = vandq_u8(vqtbl1q_u8(lo, vandq_u8(v0, vdupq_n_u8(0x0F))),
                           vqtbl1q_u8(hi, vshrq_n_u8(v0, 4)));
        // ...m1,m2,m3 identical...

        // Digraph detection: shifted compare with next byte.
        //   `/*` → compare v0 == b'/' AND (v0<<1 byte-shifted) == b'*'
        // NEON vextq_u8 produces the shifted neighbour without reload.
        let nxt0 = vextq_u8(v0, v1, 1);
        let d_slash = vceqq_u8(v0, vdupq_n_u8(b'/'));
        let d_star  = vceqq_u8(nxt0, vdupq_n_u8(b'*'));
        let digraph0 = vandq_u8(d_slash, d_star);

        // Fold each 16-byte lane into 16 bits via the shrn-narrow trick
        // (simdjson "vector → bitmask" idiom on AArch64, ~4 insns/lane).
        let b0 = neon_movemask_16(vorrq_u8(m0, digraph0)) as u64;
        let b1 = neon_movemask_16(vorrq_u8(m1, digraph1)) as u64;
        let b2 = neon_movemask_16(vorrq_u8(m2, digraph2)) as u64;
        let b3 = neon_movemask_16(vorrq_u8(m3, digraph3)) as u64;

        bits[i >> 6] = b0 | (b1 << 16) | (b2 << 32) | (b3 << 48);
        i += 64;
        // One `prfm pldl2keep, [src, #512]` per stripe — warm L2 a
        // stripe ahead without polluting L1. (Apple M-series P-core
        // LD bandwidth is ~3 per cycle; prfetch is on the LD pipe.)
    }
}
```

Phase 2 (scalar driver) consumes `bits` with `trailing_zeros`:

```rust
let mut word = bits[k];
while word != 0 {
    let off = (k << 6) | word.trailing_zeros() as usize;
    dispatch(src[off], src[off+1]);   // branchless via vpshufb
    word &= word - 1;                  // clear lowest bit
}
```

The driver never re-reads bytes it already classified as non-structural. On x86-64, replace `trailing_zeros` with `_pext_u64(word, mask)` to unpack positions into a compact tape directly — simdjson's `flatten_bits`, but with a codegen-chosen mask.

**Codegen plumbing.** Extend `crates/core/src/generate/regex/emit/simd.rs` with a `emit_structural_bitmap_kernel(grammar_id, S, D)` call that:

- Lifts `build_nibble_luts` (currently capped at 8 targets by `lo_lut |= 1 << i`; flip to a presence bitmap — any non-zero is "structural" — which takes the ceiling to the full 16 per nibble and enables `|S| ≤ 16`).
- Emits digraph pairs from the character-pair graph already computed by `scanner_plan.rs`.
- Removes the `_slow` tail entirely: with AU.6.1 tail padding in place, the last partial stripe is loaded unconditionally, structural bits beyond `src.len()` are masked off by `word &= (1 << (src.len() & 63)) - 1`.

### 4. Applicable grammars and hotspot targets

| grammar | structural alphabet | targeted profile frame | current share |
|---|---|---|---|
| **CSS L4** | `{ } ( ) ; : , @ #` + `/*`, `*/` | `scan_ws_block_comments_slow` | 11–13% |
| **JSON** | `{ } [ ] " : , \` | `memchr::closure#0`, `trim_leading_whitespace_scan_and_cache` | 7–19% + 4–12% |
| **Sheets** | `+ - * / ^ ( ) , : $ & =` | inside precedence tower calls to scanner | embedded in 56–86% tower |
| **BBNF** | `@ : ; , ( ) { } | -> /* */ (* *) $` | `__big_comment`, `__directive` dispatch | 9–15% + 7–19% |

Because the emitter derives `S` from the grammar, each parser gets its own tight LUT — JSON doesn't pay for `@` or `->`, Sheets doesn't pay for `"`, and BBNF's `(* *)` digraph participates without a hand-written second memchr path.

### 5. Measurement plan

- **Artifact:** `cargo expand -p bbnf-core --bench css_l4_bench` must show a single `scan_structural_css_l4` function replacing every `scan_ws_block_comments_slow` tail call and every `memchr2`/`memchr3` site where the needle set is a subset of `S`. Diff against the AU.3 expand baseline under `.profiles/samply/prebuild/expand/css_l4/expand.rs`.
- **Asm check:** `cargo asm --rlib bbnf-core scan_structural_css_l4` must contain `tbl` (NEON) or `vpshufb` (x86) and `clz`/`ctz` in the driver, and zero `bl memchr`.
- **Profile frames:** after a fresh `samply record` over bootstrap/tailwind/normalize, `scan_ws_block_comments_slow` frame share must drop from 11–13% toward <2%; the new `scan_structural_css_l4` frame replaces it with smaller absolute cycles (structural density of bootstrap is ~7%, so one LD per 14 bytes dispatched).
- **Benches:** `cargo bench -p bbnf-core --bench css_l4_bench -- --save-baseline au-struct`. Gate: bootstrap ≥ 720 MB/s (the AU.2 hard gate was 600; the structural bitmap should clear it by +20%). Canada JSON should post +5–8% (whitespace skip between pairs is currently 4–12% `trim_leading_whitespace_scan_and_cache`, which the bitmap subsumes).
- **Parity:** all existing tape-walker tests and the 22/22 tape parity fixtures must pass unchanged — this is a scanner-only shift, no tape-layout touch.

### 6. Honest portability cost

- **AArch64 (Apple Silicon):** primary target. `vqtbl1q_u8` is one cycle on every P-core since A13 / M1; the narrow-to-bitmask trick (`vshrn_n_u16 #4` → 64-bit mask register) is three insns. P-core load/store throughput (three LDs per cycle) makes the 4×16 stripe land in two cycles; the `prfm pldl2keep` sits on the same pipe without backpressure.
- **x86-64 AVX-512:** one emitter variant uses `VPSHUFB` + `VPMOVMSKB` + `_pext_u64`, shrinking the driver to one `pdep`/`pext`-per-word; BMI2 is universal on the Ice Lake+ floor we target.
- **x86-64 without AVX-512, SSE4.2 only:** structurally identical code with `_mm_shuffle_epi8` + `_mm_movemask_epi8`; 32-byte stripes instead of 64, two `u32` halves per bitmap word.
- **SVE2 gated off.** Samply profiling on M4-class cores shows SVE2 is not yet reachable from `target_feature` on stable Rust 2026; we do not ship an SVE2 path. Emitter reserves the identifier but codegen raises at request time.
- **E-cores (Apple Silicon):** SIMD throughput is ≈1/3 of P-core; the kernel stays a net win because the scalar baseline saturates an E-core load port. No fallback needed — the NEON path is universally available on AArch64.
- **WASM:** `simd128` target-feature emits a third variant using `i8x16.swizzle` and `i8x16.bitmask`; the driver is identical. No WASM fallback required.
- **One kernel per role.** No legacy memchr path retained in parallel; once the structural-bitmap scanner is wired, `emit_memchr1`/`2`/`3` and `emit_nibble_lut_scan` are deleted — they are strict subsets of the bitmap path and carrying both violates the "no-orthogonal-codepaths" invariant.

Key file refs: `crates/core/src/generate/regex/emit/simd.rs:14-131` (current memchr/nibble LUT emitters to subsume), `crates/core/src/backend/kernels/comment_ws.rs:11-13` (one-line call site that becomes the dispatch seat for `scan_structural_css`), `crates/core/src/backend/kernels/punct_ws_region.rs:91-151` (ws-padded cluster path that also collapses into the bitmap), `docs/tranches/AU/profiling-2.md:144,133,222` (the three frames this kernel erases).

## Prior-attempt context (added during peer review)

This proposal resurrects a capability that existed in the codebase before and was deleted. The archaeology trail:

- `4114695` feat(ir): add `compute_structural_bytes` pass (AO.0.1)
- `7198c97` feat(codegen): structural pre-scan dispatch + whitespace elision (AO.0.4-0.6)
- `2fa3172` feat(codegen): synchronized peek-only structural dispatch (AP.1b)
- `4417f8a` perf(codegen): gate structural dispatch behind WS elision (AP.1b)
- `2a8af08` fix(codegen): disable structural dispatch + restore WS trim (AP.1)
- `2f7c1bd` feat(parse-that,ir,core): delete structural dispatch infrastructure (AQ.5)

The deletion rationale (AQ.5, commit message `2f7c1bd`) was an economic one: post-AP.3.1's SIMD WS bitmap captured the savings the pre-scan was designed to provide, leaving a -190 µs citm regression. Four integration bugs drove most of that cost (`AQ-audit.md` lines 38–43): scalar `filter_quote_parity`, duplicated Alt match arms, unsaved `structural_cursor` on checkpoint, disabled WS elision. v2 avoids all four by shipping scanner-only, SIMD-filter-quote-parity from day one, no cursor, no hybrid dispatch, old kernels deleted in the same commit. The `compute_structural_bytes` pass and its alphabet computation can be recovered from commit `4114695` as a starting point; the codegen wiring is a smaller rewrite because v2 has no dispatch-coupling requirement.
