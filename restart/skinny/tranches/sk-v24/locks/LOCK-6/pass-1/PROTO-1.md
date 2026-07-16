# PROTO-1 — LOCK-6 / pass-1 / probe t1 (family 6C, WASM-CORE)

**The NEON-crossing decider (6C R-A).** Does the aarch64 NEON structural-scan win
cross to wasm SIMD128, or is `crates/simd-scan/src/wasm.rs` a second implementation
carrying materially lower throughput?

---

## Question

FAMILY-SPECS §3 pins 6C-as-primary as BLOCKED-candidate with its performance premise
gated on **R-A**: the certified stage-1 kernel is aarch64 NEON (nibble-LUT swizzle +
`vextq_u8` digraph + PMULL/CLMUL prefix-XOR). `simd-scan` already ships a *separate
hand-written `wasm.rs` SIMD128 twin* whose CLMUL path degrades to a 6-op shift-XOR
(`parity::shift_xor_prefix`) because wasm has no CLMUL. Whether that twin keeps the
NEON throughput was **UNMEASURED**. §3: "If Brief 1 shows the wasm kernel matches
native NEON, this softens [6C] to ADVANCED-scoped." This probe measures it.

## Method (cold paired, same kernel, same corpus)

One shared entry — `scan_structural(input, JSON_ALPHABET)`, the exact
`benches/stage1_throughput.rs` alphabet (`{}[]:,` + `"`) — compiled two ways and
timed identically (cold-per-parse: every sample is an independent scan that allocates
a fresh `StructuralIndex`, matching the divan bench and the no-warm-benches law):

- **Native** — `cargo build --release` (opt3/lto/cgu=1) for aarch64-apple-darwin
  (default target-cpu → `aes`+`neon` on, so the native path uses the **PMULL/CLMUL**
  prefix-XOR). A Rust bin times N=200–300 scans, reports best + median MB/s.
- **wasm** — same crate, same source, `--target wasm32-unknown-unknown` with
  `RUSTFLAGS="-C target-feature=+simd128"` (the `core::arch::wasm32` intrinsics do not
  compile without it, so a successful build proves SIMD128 is live; ~3 GB/s confirms it
  is not the scalar fallback). Driven from **node v26 / V8** — the ecologically valid
  runtime, since a 6C tape crosses to a browser/JS consumer. Corpus copied into linear
  memory once; N=200–300 `scan_json` calls timed with `performance.now()`.

Harness: out-of-tree crate `probe6c` (path-dep on the worktree's `simd-scan`,
`CARGO_TARGET_DIR` inside the copy — no writes to tracked source). Bundled below.
Corpus: `skinny/crates/test-fixtures/corpus/json/{twitter,citm_catalog,canada}.json`.
loadavg recorded per run (6.4–7.1, 1-min); interleaved re-run confirms stability.

## What ran, and the numbers

Runtime: node v26.0.0 (V8). rustc 1.96.0-nightly. aarch64 (Apple Silicon). loadavg
6.42 / 7.06 / 7.12 across the native / wasm / interleaved passes.

**Correctness parity (per-scan structural-index length — byte-identical requirement):**

| corpus | native /scan | wasm /scan | parity |
|---|---|---|---|
| twitter (631 KB) | 30 318 | 30 318 | OK |
| citm (1.73 MB) | 93 731 | 93 731 | OK |
| canada (2.25 MB) | 223 235 | 223 235 | OK |

**Throughput (best-of-N MB/s; median in parens):**

| corpus | native NEON | wasm SIMD128 | wasm/native | native/wasm |
|---|---|---|---|---|
| twitter | 8905 (8843) | 3054 (2906) | 0.34 | **2.92×** (med 3.04×) |
| citm    | 8062 (7915) | 3220 (3090) | 0.40 | **2.50×** (med 2.56×) |
| canada  | 6015 (5948) | 2811 (2685) | 0.47 | **2.14×** (med 2.22×) |

Interleaved citm confirmation (N=200, alternating native/wasm, loadavg 7.1):
native best 8092 → wasm 3208 → native 8173 → wasm 3347 MB/s. Ratio holds ~0.40.

## Reading

- **The NEON win does NOT cross.** wasm SIMD128 runs at **0.34–0.47× native NEON**
  throughput — native is **2.1–3.0× faster** on the stage-1 structural scan. The gap
  is largest on the string-heavy twitter/citm (where the shift-XOR prefix-XOR replaces
  PMULL) and narrows on the number-heavy canada.
- **But the wasm twin is not slow in absolute terms** — it clears the W5 stage-1 hard
  gate (≥ 2 GB/s on ~1 MB JSON) on its own: 2.8–3.3 GB/s. It is a *correct, real* SIMD
  kernel (parity exact), just a materially slower second implementation.
- Some of the 2–3× is intrinsic to wasm/V8 (bounds checks, linear-memory indirection,
  cranelift/turbofan codegen vs LLVM+NEON), not only CLMUL→shift-XOR. That is not a
  confound to subtract out: "does the win cross to wasm" *is* the question of the whole
  wasm delivery stack, and V8 is the deployment target.

## Verdict

**RAN_MEASURED. R-A resolves NEGATIVE: the aarch64 NEON win does not cross to wasm
SIMD128 (native 2.1–3.0× faster, wasm ~0.4× native).** The "one wasm implementation
earns a performance claim on the hot kernel" premise is **REFUTED** — `simd-scan/wasm.rs`
is a genuine second implementation whose throughput is 2–3× below native, exactly the
dual-maintenance 6C exists to abolish, re-instantiated on the hottest surface. Per
FAMILY-SPECS §3 this keeps **6C-as-primary BLOCKED-candidate** (it does NOT soften to
ADVANCED-scoped). 6C survives only as the **zero-copy crossing mechanism** for a
6B-generated / 6D-verified tape — the tape *layout* crosses once as typed-array views
(that is F2/F3, not this kernel) — confined to the bbnf side. This probe anchors Q1:
6C is not a real independent primary family; it collapses into the wasm-crossing of a
generated/mirrored tape, with the hot scan kernel staying a dual implementation.
