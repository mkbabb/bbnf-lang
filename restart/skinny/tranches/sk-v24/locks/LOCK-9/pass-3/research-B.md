# LOCK-9 · Family 9B — NATIVE-REBIRTH (parse-that keeps its own kernels)

*Pass-3 family research. Charter: parse-that keeps its own scan/kernel layer, rewritten in
idiomatic stable `core::arch::aarch64`, each kernel carrying a scalar parity oracle plus a
checkasm-liveness test; NO cross-repo coupling; the sole gate is measured no-regression per the
PROVE-09 refutation law. This report develops 9B alone to its strongest form: it does not survey
one-home siblings and it takes the two-home layout as its thesis, arguing it on measured
divergence and toolchain-decoupling grounds, then names exactly what a prototype must test.*

All parse-that paths are absolute-relative to `/Users/mkbabb/Programming/parse-that/`; all
bbnf-simd paths to `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/`. parse-that is
READ-ONLY; every landing rides a scratch copy (R3 §8) and the BBNF-PT-G letter.

---

## 0. The refutation is law, restated as the design frame

The de-nightly port of parse-that's combinator core is **performance-neutral**, not faster:
the headline 1.34–1.57× of PROVE-09/REPORT was REFUTED on fresh paired windows (VERIFY: window A
uniformly 0.81–0.98×, two windows disagree in sign → inside the noise floor). Two mechanical facts
survive the refutation and bind 9B:

1. **The naive `to_bitmask` is a codegen trap.** `otool -tv` of the refuted patch showed **2271
   `addv.8b` vs baseline 509**; `tbl` (swizzle) identical at 516=516. rustc's `portable_simd`
   `Mask::to_bitmask` on the pinned nightly, and the PROVE-09 shim's hand `vand`+two-`vaddv`
   reproduction of it, both do ~4× the reduction work per pack. **The law: stable NEON must be
   measured no-regression; the mask narrows via `vshrn`, never the double-`addv`.**
2. **The port is alloc-neutral.** `smallbox!` ≡ `SmallBox::new` (same S32 inline / heap-spill):
   construct `json_parser()` = 6 allocs/5960 B; parse twitter.json = 5390 allocs (Vec growth in
   `many`/`sep_by` + string arena, untouched by vtable boxing). An arena moves only the 6.

9B therefore makes **zero speedup claim**. Its deliverable is a stable, aarch64-only kernel layer
whose correctness is machine-proven per-kernel and whose no-regression floor is a gate that can
fail. This is the mission PT-2 already charters; 9B is its architectural center — *own* kernels,
proven by *own* oracles, decoupled from skinny's toolchain.

---

## 1. The mechanism made concrete

### 1.1 The kernel home — `parse_that/src/simd/` (own, colocated, cfg-pivoted)

Productionize PROVE-09's flat `src/simd128.rs` into a directory-module split by concern
(colocation edict; PT-2 §4 W1):

```
rust/parse_that/src/simd/
  mod.rs     (~40)  public u8x16 / mask8x16 vocabulary + re-exports; #[cfg] pivot
  neon.rs    (~210) #[cfg(target_arch="aarch64")] core::arch::aarch64 impl
  scalar.rs  (~130) #[cfg(not aarch64)] [u8;16] parity oracle — NO target intrinsics
```

Each of the eight scanner sites swaps one `use` line (`std::simd::prelude::*` → `crate::simd::*`),
zero call-site churn *except* the movemask consumers (§1.2). The `scalar.rs` body is the
**executable specification**: every `neon.rs` primitive is byte-parity-checked against it — the
same admission discipline bbnf-simd already runs ("the scalar reference is the executable
specification per the checkasm admission gate", `src/lib.rs:247-248`).

### 1.2 The movemask, the one load-bearing fix — the LEAN nibble-`vshrn` producer

Parse-that's iterating masks today go through `simd_eq(..).to_bitmask()` at
`scanners.rs:163`, `ws_comment.rs:81,150,247`, `quote_parity.rs:81-82`, `quoted_simd.rs:45-46`,
`decode.rs:82-83,189-190`, and `.trailing_zeros()`-style CTZ at `scanners.rs:208,286`,
`span_parser/scan.rs:152`, `structural_bitmap.rs:43`. That `to_bitmask` is the double-`vaddv`
trap. The LEAN producer replaces it:

```rust
// mask8x16::from_cmp(cmp: uint8x16_t) -> u64   (4-bit-per-lane stride)
let narrowed = vshrn_n_u16::<4>(vreinterpretq_u16_u8(cmp)); // uint8x8_t, one addv-free op
let mask = vget_lane_u64(vreinterpret_u64_u8(narrowed), 0);  // 64-bit, 4 bits per lane
```

`vshrn_n_u16::<4>` narrows sixteen 8-bit lanes into eight bytes at a **4-bit-per-lane stride** in a
single reduction-free instruction (Lemire, "Bit hacks with NEON movemasks"). Iterating consumers
migrate `tz` → `index = tz >> 2`; presence-only consumers (`any()`) lower to `vmaxvq_u8` (cheap,
unchanged). This is real, bounded churn (~+80/−40) at the ~5 CTZ sites — the honest cost the
zero-churn shim traded away for a regressing reduction tree. Note: bbnf-simd's own
`src/aarch64/movemask.rs` uses `vshrn_n_u16::<4>` but then a **scalar `for pair in 0..8` repack**
(lines 17-24) to force a dense 1-bit-per-lane `u16` — that dense repack is *not* the LEAN stride and
should NOT be copied; 9B keeps the 4-bit stride and pushes `>>2` to the consumer.

### 1.3 The x86 purge (the abrogation reaches parse-that source)

Exactly two files carry live x86 arms (rest is `portable_simd`):
`number_simd.rs:145-235` (SSE4.2 `_mm_*` + `is_x86_feature_detected!("sse4.2")`) — excise, keep the
NEON `:29-136` + scalar `:239` (−~90); `quote_parity.rs:42-49` (`pclmulqdq` `prefix_xor`) — excise,
keep aarch64 `vmull_p64` `:51-56` + scalar `:58-68` (−~30). `decode.rs`'s two `_mm_storeu_si128`
tokens are in doc comments (`:168,:221`) — scrub. After: `rg -E '_mm_|x86_64::|is_x86_feature'
rust/*/src` → 0.

**Digit-scan guard (SPEC §0/02 NO_GO).** `number_simd.rs` is *ported, never optimized*: keep its
existing NEON digit path byte-for-byte; do NOT pursue Mula/Lemire in-vector accumulation (the
latency-bound CPI inversion 0.13→0.30 regresses the product lane). Its NEON already uses the
right idiom — `vaddv_u8` of masked lo/hi halves (`number_simd.rs:54-55`) is a *horizontal-sum
digit accumulate*, a legitimate reduction, distinct from the movemask double-`addv` anti-pattern.

---

## 2. The substrate — own kernels, own oracle, no cross-repo coupling

### 2.1 The differential parity oracle (transliterated from bbnf-simd's checkasm, owned by PT)

bbnf-simd's `tests/checkasm_common.rs` is the template and is **self-contained** (~190 LOC, no
external deps) — it copies cleanly into `parse_that/tests/`:

- **`Xorshift64` + `guarded_call`** (`checkasm_common.rs:3-72`): a 1 KiB stack canary filled from a
  fixed seed, xor-folded before/after the candidate call, `black_box`-fenced; a clobber panics with
  the first bad byte. This is the dav1d checkasm liveness idea in Rust.
- **`callee_saved_register_then`** (`checkasm_common.rs:83-192`): reads/writes x19–x28 via
  `core::arch::asm!` sentinels and asserts the NEON candidate preserved every AArch64 callee-saved
  register — catches a kernel that clobbers a saved reg (a class of hand-NEON bug portable_simd
  hid).
- **The per-kernel differential test** follows `tests/checkasm_byte_class_from_table_64.rs`
  verbatim in shape: a **density sweep** `[0,1,2,4,8,16,32,64,128,255,256]` × an **alignment window
  sweep** `for align in 0..64` (slides the block across a 128-byte backing so every stripe offset is
  exercised) × xorshift-random fills, each asserting `guarded_call(neon) == scalar_reference`.

**INJECT_BUG.** The harness *is* the mutation oracle: flip one comparator or one lane in a scratch
copy of a `neon.rs` primitive and confirm the density/alignment sweep and the canary catch it — a
concrete self-test that the oracle has teeth (the checkasm design's whole point). Add one committed
`#[should_panic]` fixture that XORs a lane to prove the sweep is not vacuous.

### 2.2 Why own — the two-home divergence census (F1, answered as data)

Even after authoring both canonical producers into a scratch shared home and purging the refuted
idiom there, the residual substrate divergence across ALL membership + dense scan sites is
**irreducible to ~two producers + two clamp conventions + two shuffle builders regardless of
home** — the only genuinely shared surface is the ~190-LOC checkasm harness and the scalar oracle
bodies. Concrete deltas:

| Axis | parse-that | bbnf-simd | Delta if merged |
|---|---|---|---|
| **Block width** | `u8x16` 16-byte stripes → **u16** masks (`scanners.rs:163`, `ws_comment.rs:81`) | fixed **`[u8;64]`** blocks → **u64** masks (`byte_class_from_table_64`, `bulk_emit_positions_64`) | one side rewrites every inner loop (widen PT to 64B, or narrow bbnf-simd's CTZ-emit model to 16B) |
| **Pad/clamp** | `PaddedView`, `INPUT_PAD_BYTES=64` trailing NUL, `STRIDE≤64`, no per-chunk guard (`state.rs:57,111-114`) | `eob_pad_clamp` / `EobBlock` tail-clamp primitive (`dispatch.rs:55`, `lib.rs:276`) | two incompatible EOB conventions; unifying forces a convention rewrite on the loser |
| **Shuffle model** | **two-table** nibble-LUT `swizzle_dyn` lo/hi (`scanners.rs:281-282`, `structural_bitmap.rs:40-41`) | **single** lo6 `tbl4` with an admissibility gate (`classifier.rs:100-113` `lo6_table_admissible`) | different classifier algebra; not one shuffle |
| **Emit capacity** | u16 lane mask; `.trailing_zeros()` scalar CTZ | u32 positions, 64-base, `bulk_emit_positions_64` bulk store (`lib.rs:227-242`) | different position-emit width + contract |

Two facts sharpen the case:

- **The DENSE dense-64 producer is genuinely ABSENT from bbnf-simd** — `byte_class_from_table_64_neon`
  is a **scalar STUB**: `src/aarch64/byte_class_from_table_64.rs:3` calls
  `byte_class_from_table_64_scalar` directly. So "share bbnf-simd today" imports a scalar delegate
  for the dense classifier — a consumer-less/perf-less share, forbidden by Constitution viii. The
  canonical DENSE `vpaddq`-tree movemask must be authored *once into any home regardless of
  ownership* (the F1 premise), so the shared home saves nothing on the producer that does not yet
  exist.
- **Toolchain decoupling is the maintenance basis.** PT-2 takes parse-that to the **stable** channel
  (off the ICE-pinned `nightly-2026-04-11`, adopted to dodge 93 recorded `on_disk_cache` ICEs,
  R3 §1). bbnf-simd rides skinny's workspace toolchain and `blake3` dep (`Cargo.toml`). A shared
  rlib forces a common MSRV/feature floor and a **sync gate** — coupling PT's stable cutover to
  skinny's cadence. 9B's own-home keeps the two release trains independent; the drift-cost it pays
  is duplicating the scalar oracle bodies, mechanically kept in parity by the shared harness shape.

### 2.3 Where `#[inline]` actually fails to cross a seam (F1)

Cross-crate `#[inline]` across the rlib boundary is measured-live (0 in-loop indirect) — the crate
seam is *not* where inlining dies. The real non-inlinable boundary is **the fn-pointer dispatch
table**, not the crate wall: bbnf-simd's `PrimitiveKernels` (`classifier.rs:50-56`) is a struct of
`fn(...)` pointers resolved once through `OnceLock` (`classifier.rs:58-61`); every `prim::*` call
(`lib.rs:255-273`) is a **runtime indirect call by construction**, uninlinable at any LTO setting.
It would surface concretely under `codegen-units>1` + `lto=false/thin` on any non-generic `pub fn`
lacking `#[inline]`, but bbnf-simd converts that into a *guaranteed* indirect via the OnceLock
table. 9B avoids the table entirely: direct `#[cfg(target_arch="aarch64")]` static dispatch (the
shape bbnf-simd already uses for `byte_class_from_eq_set_64`, `lib.rs:285-288` — a monomorphic
`return neon(...)` with a scalar `#[allow(unreachable_code)]` tail). Own-home = zero indirect,
zero seam question.

### 2.4 Scanner-file mapping census (F1, owed)

The ~8 portable_simd files (R3 §2a), classified 1:1-kernel | vector-tier | no-go:

| File | Class | Basis |
|---|---|---|
| `parsers/scan/structural_bitmap.rs` (13) | **1:1-kernel** | single nibble-LUT classify → u16 mask; checkasm-able |
| `parsers/scan/quote_parity.rs` (4+x86) | **1:1-kernel** | `prefix_xor` (pclmul/`vmull_p64`) + `classify_stripe_64`; two named primitives |
| `parsers/scan/number_simd.rs` (17+x86) | **1:1-kernel** (port-only) | digit accumulate; SPEC §0/02 NO_GO on optimization |
| `parsers/scan/decode.rs` (17) | **vector-tier** | string decode-to-arena scan loop + `store_chunk` (`:227`) |
| `scanners.rs` (22) | **vector-tier** | `find_first_of` nibble-LUT dispatch + `trim_leading_whitespace` |
| `parsers/scan/ws_comment.rs` (16) | **vector-tier** | ws/comment multi-primitive classify loop |
| `parsers/scan/quoted_simd.rs` (7) | **vector-tier** | `odd_parity_backslashes` + quote/bs scan |
| `span_parser/scan.rs` (7) | **vector-tier** (dup of `scanners.rs`) | span-parser inline nibble-LUT scan |
| `state.rs` (2) | **no-go** | `PaddedView` — comments only, no live SIMD |

Split: **3 kernel-tier / 5 vector-tier / 1 no-go** (state.rs). The kernel-tier three are the
checkasm targets; the vector-tier five consume `simd/` primitives inside scan loops and are proven
by the ~400 integration tests + the paired bench (their consumer, Y.13).

---

## 3. F2 — native aarch64 rebirth: the stable-build blast radius

Four nightly enablers (R3 §2), by removal cost:

1. **`portable_simd`** (8 files / ~88 hits) → `simd/` NEON per §1. Widest surface; every hot
   scanner. This is 9B's body.
2. **`smallbox[nightly]`** — one type field `SmallBox<dyn ParserFn+'a, S32>` (`parse.rs:61`), one
   construction site `SmallBox::new` → `smallbox!` (`parse.rs:74`), one Cargo line
   (`Cargo.toml:97` drop `features=["nightly"]`). Blast radius is **narrow and alloc-neutral**
   (PROVE-09: 6 construct / 5390 twitter unchanged); the S32 inline/heap threshold is identical.
3. **`stmt_expr_attributes`** — dead after the port (`lib.rs:3`); delete.
4. **`fmt_helpers_for_derive`** — the **sole residual** and the only cross-repo edge (G3). It is
   forced NOT by parse-that source but by `@generated` `regex/generated.rs`, which emits the
   *expanded* `#[derive(Debug)]` (`#[automatically_derived] impl` calling the perma-unstable
   `core::fmt::Formatter::debug_tuple_field1_finish`). Fix at the **generator** (emit the
   `#[derive(Debug)]` attribute, not its expansion) + regen via `scripts/bootstrap-regex.sh`;
   generated.rs net **shrinks** (P53). On stable, all 20–21 errors localize to `generated.rs`; the
   12 hand-ported files carry zero (VERIFY attack #5's nuance: the retained feature attr is itself
   an `E0554`, so it must fall for the build to be truly stable-clean).

**Does a naive nibble-stride corrupt dense-64 parity? YES — the named counterexample.** The LEAN
4-bit-per-lane `vshrn` producer (§1.2) is correct only for the u8x16→u16 membership sites where the
consumer applies `tz>>2`. Applied to a **64-byte DENSE** site and then read as a true
1-bit-per-lane mask, the indices misalign (`tz>>2` vs `tz`) — silent position corruption. The dense
site needs a **`vpaddq`-tree true-1-bit producer** authored separately; the nibble producer is
LEAN-only. This is the dual of the movemask law: *neither* the double-`addv` (wrong for perf) *nor*
the nibble stride (wrong for dense parity) is universal. The parity harness (§2.1) must sweep BOTH
producers against scalar, and the two must never be substituted for each other.

---

## 4. Prior art (cited)

- **dav1d checkasm** (VideoLAN) — the callee-saved-register-clobber + stack-canary liveness harness;
  bbnf-simd's `checkasm_common.rs` is a faithful Rust transliteration (x19–x28 sentinels via
  `asm!`). The gold standard for hand-SIMD correctness gating.
- **simdjson stage-1** (Langdale & Lemire, 2019, *Parsing Gigabytes of JSON per Second*) — nibble-LUT
  structural classification + prefix-XOR quote parity; the `structural_bitmap.rs` / `quote_parity.rs`
  kernels descend from it.
- **Lemire, "Bit hacks with NEON / movemask on ARM"** — the `vshrn_n_u16::<4>` narrow trick (the
  blessed LEAN idiom) and the double-`vaddv` anti-pattern; the empirical spine of the refutation law.
- **rustc `portable_simd`** aarch64 `Mask::to_bitmask` lowering — the measured double-`addv`
  (2271 vs 509, PROVE-09/VERIFY attack #2); the reason a hand port can beat/tie std but must be
  instruction-audited.
- **Mula & Lemire, SIMD integer parsing** — cited as the NO_GO baseline for `number_simd.rs`
  (SPEC §0/02): the in-vector digit accumulation 9B deliberately does *not* adopt.
- **regex-automata / `aho-corasick`** — parse-that's `scanners.rs` already routes 1–3 targets to
  `memchr` and caches Aho-Corasick automata (`scanners.rs:35-52`); the membership tier's prior art
  and the boundary where a hand kernel stops paying.

---

## 5. Named risks & counterexamples

- **R1 — movemask regresses again.** A botched nibble-stride migration re-introduces the double-`addv`
  or corrupts `tz>>2` arithmetic. *Counterexample:* the 2271 `addv.8b`. *Guard:* `otool -tv`
  reduction-insn (`addv.8b`+`shrn`) ≤ baseline 509, `tbl`=516 unchanged.
- **R2 — window-asymmetry noise floor.** VERIFY's core finding: two arms sampled under different
  load fake a delta; two windows disagreeing in sign = inside noise. *Guard:* the no-regression gate
  requires **same-sign ≥0.98× across two opposite-order windows**, not a single window.
- **R3 — the digit-scan trap.** Someone "optimizes" `number_simd.rs` into in-vector accumulation.
  *Counterexample:* CPI 0.13→0.30. *Guard:* port-only; the file's NEON path is behaviour-preserved.
- **R4 — smallbox! ≠ SmallBox::new threshold drift.** *Guard:* the alloc probe asserts 6/5390
  byte-identical to baseline.
- **R5 — cross-repo half-landing (G3).** The generator Debug fix and the parse-that regen land out of
  sync; generated.rs lags. *Guard:* both halves + idempotent-regen (`git diff --stat` empty on the
  second run) before the stable build is declared green.
- **R6 — sharing a stub.** Adopting bbnf-simd's dense classifier today imports the scalar delegate
  (`aarch64/byte_class_from_table_64.rs:3`). *Guard:* 9B authors its own dense producer against its
  own oracle — no consumer-less import.

---

## 6. Exactly what a prototype must test to move family 9B

1. **The movemask, instruction-audited.** Author the LEAN nibble-`vshrn` u8x16→u64 producer; migrate
   the ~5 CTZ consumers (`scanners.rs:208,286`, `structural_bitmap.rs:43`, `ws_comment.rs`,
   `quote_parity.rs`, `decode.rs`) to `tz>>2`. Build baseline + patched `parse_that_combinator`
   binaries; `otool -tv` the hot classify/movemask functions: reduction-insn ≤ 509, `tbl` = 516.
   Then the paired divan gate: two windows, opposite arm order, N≥200, per corpus, patched median
   **≥0.98× on EVERY corpus in BOTH windows** — *the exact gate PROVE-09's shim failed* (window A
   0.81–0.98×).
2. **The oracle has teeth.** Port `checkasm_common.rs` into `parse_that/tests/`; write per-kernel
   differential tests (density × alignment 0..64 × random) for `structural_bitmap`, `quote_parity`,
   `ws_comment`; add the INJECT_BUG `#[should_panic]` lane-flip fixture proving the sweep is not
   vacuous; assert the aarch64 callee-saved check passes for every `neon.rs` primitive.
3. **The dense-parity counterexample.** Prove the nibble producer misindexes a synthetic 64-byte
   dense block, and that a separate `vpaddq`-tree producer matches scalar — the two producers are
   non-substitutable (§3).
4. **Full stable build.** After the generator Debug fix + regen, `cargo +stable build --workspace`
   in the scratch copy → EXIT 0, zero `E0554`. The de-nightly completeness proof.
5. **Alloc-neutrality.** `alloc_probe`: 6 construct / 5390 twitter unchanged after `smallbox!`.

**Move condition for 9B:** clauses 1–2 green (no-regression + machine-proven per-kernel) with the
own-home substrate and zero cross-repo coupling establishes NATIVE-REBIRTH as a landable core;
clause 4 closes the stable cutover; clause 3 is the correctness invariant that keeps the two
producers honest.
