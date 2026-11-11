# PROTO-P5 — Hand-asm vs NEON intrinsics on the 64-byte structural-classify kernel

Instrument, not artifact: a worktree-isolated checkasm-style harness that binds three bit-exact
bindings of bbnf's live 64-byte no-space classifier and measures the asm-vs-intrinsics delta on
Apple silicon. Nothing here lands on master; the decisive code is inlined below so the finding is
resurrectable after the worktree is discarded.

## The question, and the answer

Falsifiable: on OUR kernel, does hand aarch64 asm beat well-shaped NEON intrinsics by more than the
literature's 0–5% bound (report `25:263-271`), justifying an asm emission path in TW4?

**Answer: YES, the bound is exceeded on this kernel — measured +9% to +12% for asm over intrinsics —
and the cause is load-independent and named.** But the cause is *instruction selection*, not
scheduling or register pressure: LLVM lowers the classify mask with 8 surplus SIMD-ALU ops over 64
bytes and cannot be coerced out of it from intrinsics. The door is justified, *conditionally* on a
quiet re-capture, because every timing here was taken under loadavg 17–21 (the discipline mandate is
< 2). The static op-count delta (from `objdump`, machine- and load-independent) is what carries the
verdict; the wall-clock percentage is directional.

## The kernel under test

`crates/core/src/grammar/generated/json.rs:815` `nospace_bitmap_64_neon` (and its `chunk_ns_mask16`
helper, `json.rs:837-859`): classify 64 bytes against `{space, tab, nl, cr}`, invert, emit a
1-bit-per-byte `u64` (bit *i* set iff byte *i* is non-space). Algorithm per 16-byte chunk: OR the
four `vceqq_u8` compares → `vmvnq_u8` → `vandq_u8` with a positional `bit_vec`
`[1,2,4,…,128,1,2,…,128]` → two `vaddv_u8` horizontal reductions (low half, high half). Four chunks,
so **8 ADDV + 8 SIMD→GPR crossings** per 64-byte block — exactly the "one crossing per 16-byte block"
shape report `25:341-343` warns is the dominant cost.

## Method

Three bit-exact bindings behind one `fn(&[u8;64]) -> u64` signature (checkasm `declare_func`):

1. `nospace_scalar` — the reference (`call_ref`).
2. `nospace_intrin` — the shipping intrinsics kernel, **verbatim** from `json.rs`.
3. `nospace_intrin_ilp` — *well-shaped* intrinsics: the same algorithm rewritten straight-line,
   4-wide (all four chunks classified before any reduction, `vbicq_u8(bit_vec, ws)` in place of
   `mvn`+`and`), with no helper call — the honest test of the literature's "*well-shaped*
   intrinsics" clause.
4. `nospace_asm` — hand `asm!`, the **identical instruction stream** (same 8 ADDV + 8 UMOV
   crossings), hand-scheduled 4-wide.

Harness: 200 000 randomized 64-byte inputs (`xorshift64*`, an 8-symbol whitespace/non-whitespace
alphabet so masks are non-trivial) + 5 adversarial edges (all-zero, all-space, all-`x`, boundary
tab/space, mid-block transition), byte-exact parity of all four; a callee-saved-clobber + stack-canary
guard around the asm; a cold-per-process throughput micro-bench iterating BOTH SIMD variants + ILP
alternately (checkasm liveness), min-of-200 passes over a 1 MiB buffer, loadavg captured start and
end. Machine: Apple M5 Max, `rustc 1.96.0-nightly`, `-C target-cpu=native`, `lto=fat`,
`codegen-units=1`, `panic=abort`, `opt-level=3`.

## Results

Parity **GREEN**: `scalar == intrin == intrin_ilp == asm` across all 200 005 inputs.
Clobber/canary **GREEN**: no callee-saved corruption, canary intact.

Five fresh-process runs, min-of-200 ns per 64-byte block (loadavg beside each set):

| variant        | ns/block | GB/s  | vs asm     |
|----------------|----------|-------|------------|
| scalar         | 24.7–36.6| 1.7–2.6| —         |
| intrin (ship)  | 4.39–5.31| 12.0–14.6| +9.1…+11.8% |
| intrin (ILP)   | 5.15–5.32| 12.0–12.4| +9.3…+9.4% |
| asm            | 3.87–4.82| 13.3–16.5| —          |

Deltas (positive = asm faster), stable across runs:

- **asm vs intrin(ship): +9.1% … +11.8%**
- **asm vs intrin(ILP, well-shaped): +9.3% … +9.4%** — the honest bound.
- **reshape ship→ILP: −0.2%** — reshaping the source to expose 4-wide ILP changed *nothing*.
- cycle estimate @ 4.0 GHz: intrin ≈ 20.6 cyc/64B, asm ≈ 18.7 cyc/64B (≈ 2 cyc/64B).

## Root cause (load-independent — from `objdump -d`)

Reshaping the intrinsics did not help, so the cause is **not** scheduling order. The per-function
opcode histograms name it exactly:

```
CLASSIFY STAGE (per 64B):
  intrin(ship & ILP) : 16 cmeq + 8 mvn + 8 bic + 8 and  = 40 SIMD-ALU ops
  asm                : 16 cmeq + 12 orr        + 4 bic   = 32 SIMD-ALU ops   (8 fewer)
REDUCTION STAGE (identical crossing count):
  both               : 8 addv + 8 SIMD→GPR crossings
  intrin: 4 ext.16b + 7 fmov + 1 umov      asm: 4 mov.d + 8 umov.b
```

LLVM lowers `bit_vec & ~(sp|tab|nl|cr)` by De Morgan into **6 logic ops/chunk**
(`mvn`,`bic`,`mvn`,`bic`,`and`,`and`) instead of the minimal **4** (`orr`,`orr`,`orr`,`bic`). Over
four chunks that is **8 surplus SIMD-ALU ops**. On Apple's four symmetric SIMD pipes at 4 ops/cycle,
40 vs 32 ops ≈ 10 vs 8 cycles of classify — ≈ 2 cyc on a ~20 cyc/64B throughput-bound kernel ≈ the
measured ~10%. The op-count delta is a fact of the binary; it holds at any loadavg.

**The intrinsic cannot escape it.** `nospace_intrin_ilp` writes `vbicq_u8(bit_vec, ws_orr_tree)`
explicitly — the minimal form — and LLVM's canonicalizer rewrites it *back* to the 8-op-surplus
`mvn`/`bic`/`and` shape: its histogram is byte-identical to the shipping kernel (8 mvn + 8 bic + 8
and, 0 orr). This is report `25:246-252`'s "idiom the compiler will not keep," realized: the emitter
author has no intrinsic spelling that survives the missed peephole. The reduction tail differs too
(`ext`+`fmov` vs `mov.d`+`umov.b`) but is second-order — same crossing count, and it did not move
when the classify was reshaped.

## The decisive code

Hand-asm classify+reduce (the winning form — the tight `orr`-tree + single `bic`, 4-wide batched):

```rust
// constants: v4=space v5=tab v6=nl v7=cr, v16=bit_vec; chunks v0..v3 loaded up front
"cmeq v17.16b, v0.16b, v4.16b"  ... (16 cmeq, 4 chunks x 4 consts, batched)
"orr  v17.16b, v17.16b, v18.16b" ... (12 orr: 3-op OR-tree per chunk -> ws)
"bic  v17.16b, v16.16b, v17.16b" ... ( 4 bic: weighted = bit_vec & ~ws)   // <-- LLVM won't keep this
"mov  v18.d[0], v17.d[1]"        ... ( 4 mov.d: high half down for the 2nd ADDV)
"addv b17, v17.8b"               ... ( 8 addv)
"umov w9, v17.b[0]"              ... ( 8 umov.b crossings; orr/bfi assemble the u64)
```

Full source (crate `p5-kernel`, ~330 LOC): three bindings + xorshift harness + clobber/canary guard +
alternating min-of-200 bench. `options(nostack, readonly, pure)` on the asm; all v0–v7,v16–v28 and
x9–x16 declared clobbered. Rebuild: `cargo build --release --manifest-path <crate>/Cargo.toml`.

## Verdict and TW4 recommendation

- **The 0–5% bound is falsified for THIS kernel** (~9–12% measured; ~10% predicted by an
  8-op static delta that is load-independent). This is the *mildest* of report `25`'s
  asm-justifying cases — a missed instruction-selection peephole, not register spill (§3.2) or a
  missing intrinsic — and it is real and un-coercible from intrinsics (verified: explicit `vbicq`
  is canonicalized away).
- **Two cheaper doors should be priced before hand-asm.** (a) The delta lives *entirely* in the
  classify logic-op count; a crossing-reducing *algorithm* (single `vshrn_n_u16::<4>` movemask as
  already used at `json.rs:927`, or an `addp`-tree reduction) is intrinsic-expressible, removes ADDV
  crossings, and would beat all four variants — the "algorithmic win belongs in the kernel library"
  posture of `25:266-268`. (b) An `orr`-tree written so LLVM cannot De-Morgan it (e.g. materializing
  `ws` through an opaque boundary) may or may not hold — untested here.
- **If neither cheaper door holds under a quiet re-capture, an `asm!` emission path for this one
  classify sequence is warranted in TW4**, narrowly scoped, gated by the checkasm harness that went
  green here (parity + clobber + liveness). It is precisely dav1d's rule: measure, prove the
  compiler's selection is suboptimal, then and only then emit asm.
- **Blocking caveat, honestly:** every wall-clock number was taken at loadavg 17–21 — a gross
  violation of the cold-quiet mandate (`SPEC-DRAFT-P1.md:11-13`, `< 2`). The *direction* and the
  *static op-count cause* are robust; the *magnitude* (whether it is 5%, 10%, or 12% quiet) is
  DIRECTIONAL until the TW0 quiet session (`32 U1`) re-captures it. **The spec answer: keep the
  asm door in TW4's design, do not open it until the quiet re-capture confirms > 5% with this same
  op-count cause; default remains intrinsics.** loadavg beside every figure above.

## Limits (P5-specific)

Single kernel (the no-space classifier — the one 64B block-classify on the hot JSON path); single
uarch (M5 Max, not the M1/M3 the cost table will also key); no PMU cycle counter (mach-timebase
24 MHz → ns only; cycles are an estimate at assumed 4.0/4.6 GHz); heavy contention. The harness
itself — randomized bit-exact parity + clobber/canary + liveness iteration over every variant — is
the reusable asset and is the concrete TW4 checkasm template regardless of the asm decision.
