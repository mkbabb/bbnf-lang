# PROVE-02 — SIMD NUMERICS (F1): Mula/Lemire digit-run → Eisel-Lemire

**Verdict: NO_GO (measured refutation).** The kernel is byte-exact and retires
1.36–1.91× fewer instructions on canada (PB-6 reproduced end-to-end, load-invariant),
but it is **latency-bound on the live cold parser** and *loses* on cycles/wall: the
lazy product lane (`parse_tape`) regresses **+50% cyc/B** and paired `tape/sonic`
canada goes **0.70 → 1.09**; the gate lane `fused/sonic` stays **flat (~1.18 → 1.20)**.
The gate — *material movement toward ≥0.95× fused/sonic on canada* — is **not met**,
and the direction on the product lanes is a regression. The canada `fused/sonic` loss
is **not** digit-scan-instruction-bound; it is the eager `materialize_f64` float tax,
which this change does not touch.

Host: Apple aarch64, NEON always-on. Workspace root `skinny/`, `--release`
(fat-LTO, cgu=1, debug=true). All numbers cold-per-parse; ratios certified,
absolutes directional. Instructions/byte (retired-instruction PMU counter) is the
load-invariant certified column; cyc/B absolutes drift with the (heavy) machine, so
every cyc/B comparison below is an **adjacent-run same-load A/B** of two separately
built binaries (`jcb_swar` = SWAR baseline, `jcb_neon` = candidate).

---

## 1. What I built

Re-authored SK-V23 PROTO-PB-6's two kernels **into the live skinny
`parse-that-regex` crate** (PB-6's own scratch code is wiped; R5 maps it). Colocated
as a directory-module sibling of its sole consumer:

- **`crates/parse-that-regex/src/number/simd.rs`** (new, 106 LOC) — one public
  `scan_block(input, cursor) -> (count, value)` over a single 16-byte NEON block:
  - *count* = vshrn nibble-domain movemask: `(b-'0')>9 ⇒ 0xFF`, `vshrn_n_u16::<4>`
    narrows each u16 lane to a nibble so input byte `k` maps to bit `4k`; first
    non-digit ⇒ `tz = 4k ⇒ k` after `>>2`, all-digit ⇒ `nd==0 ⇒ tz=64 ⇒ 16`,
    branchless.
  - *value* = Mula/Lemire: `SHUF[count]` right-aligns the digits via `vqtbl1q_u8`
    (wrapped-negative indices ⇒ zero-fill), then a 3-stage `vmul`/`vpaddlq` pairwise
    fold ([10,1]→[100,1]→[10000,1]) + a `wrapping_mul(1e8)+add` lane combine → u64.
  - `const fn build_shuf()` builds the 17×16 shuffle table at compile time.
- **`crates/parse-that-regex/src/number/mod.rs`** (+39 LOC) — `scan_digit_run` gains
  an aarch64 NEON prelude: while `cursor+16<=len` and the running `digit_count<=3`,
  one `scan_block` resolves the leading run (canada's coordinate shape), then the
  existing 8/4/2/1 SWAR ladder handles the tail (>16-digit runs, near-EOF, and the
  exact 19-digit overflow semantics — unchanged). Two `NumberParts` methods
  (`can_push_block`/`push_block`) fold the block value with `mantissa *
  POW10[count] + value`; the `digit_count<=3` gate keeps one ≤16-digit block within
  the 19-digit mantissa budget so `u64` never overflows.
- **`crates/parse-that-regex/tests/digit_simd_parity.rs`** (new, 160 LOC) — the float
  byte-parity oracle (per `no-inline-tests`): every numeric token of canada
  (111,080), twitter, citm swept and materialized through the NEON path, asserting
  `to_bits()` equality vs libstd `f64::from_str`; plus integer widths 1..=20 and
  fractional runs 1..=17 forcing every kernel count 1..=16.

No x86, no hand-asm (pure `core::arch::aarch64` intrinsics), no stub/shim. The SWAR
ladder is retained as the non-aarch64 path and the executable spec.

## 2. Correctness — full float byte-parity FIRST (gate precondition, PASS)

`cargo test -p parse-that-regex` → **30 passed, 0 failed** (26 lib + 4 integration).
The canada sweep alone materializes 111,080 float tokens through the NEON kernel;
every one is bit-identical to libstd. Integer widths 1..=20 (mantissa + i64/u64
round-trip) and fractional counts 1..=17 exact. Parity is unconditional — the
verdict below is purely a performance finding.

## 3. Visual verification — the NEON kernel IS emitted (not scalarized)

Mnemonic counts, `objdump --macho -d` of each binary (kernel is `#[inline]`, so it
folds into ~18 `match_number_span` inline sites):

| mnemonic | swar | neon | delta (the kernel) |
|---|--:|--:|--:|
| `uaddlp` (vpaddlq, the Mula fold) | 1 | 55 | **+54** |
| `tbl` (vqtbl1q right-align) | 150 | 168 | +18 |
| `shrn` (vshrn_n::<4> count) | 200 | 228 | +28 |
| `cmhi` (vcgtq/vcleq) | 58 | 104 | +46 |

One inlined `scan_block`, exactly PB-6's dataflow (LLVM CSE'd the two `vsubq` into a
single `add.16b`, and lowered `trailing_zeros` to `rbit`/`clz`):

```asm
ldr    q6, [x1, x21]        ; vld1q_u8(chunk)
add.16b  v6, v6, v0         ; chunk + (-'0')        (shared: count + accumulate)
cmhi.16b v7, v6, v1         ; (adj) > 9  -> non_digit
shrn.8b  v7, v7, #0x4       ; vshrn_n_u16::<4>
fmov   x15, d7              ; vget_lane_u64         (SIMD -> GPR cross-domain)
cmhi.16b v7, v2, v6         ; 9 >= adj   -> is_digit
and.16b  v6, v7, v6         ; digits = adj & is_digit
rbit   x15, x15            ; \
clz    x15, x15            ;  } trailing_zeros
lsr    w17, w15, #2         ; >> 2  == count
ldr    q7, [x12, w17, uxtw #4] ; SHUF[count]        (LOAD DEPENDENT ON count)
tbl.16b  v6, { v6 }, v7     ; vqtbl1q_u8 right-align
mul.16b  v6, v6, v3         ; * [10,1,..]
uaddlp.8h  v6, v6           ; vpaddlq_u8   -> t1
mul.8h   v6, v6, v4         ; * [100,1,..]
uaddlp.4s  v6, v6           ; vpaddlq_u16  -> t2
mul.4s   v6, v6, v5         ; * [10000,1,..]
uaddlp.2d  v6, v6           ; vpaddlq_u32  -> t3
mov.d  x3, v6[1]            ; \  vgetq_lane_u64 x2   (SIMD -> GPR cross-domain)
fmov   x4, d6              ; /
```

This straight-line, **~15-deep serial dependency chain** with two SIMD→GPR moves and
a `count`-dependent table load is the root cause of section 5.

## 4. Load-invariant win: instructions/byte (canada, adjacent PMU A/B, both windows agree)

| lane | swar ins/B | neon ins/B | ratio | reduction |
|---|--:|--:|--:|--:|
| parse_only | 14.38 | 7.54 | 0.524 | **1.91x fewer** |
| parse_tape (product) | 16.31 | 10.33 | 0.634 | **1.58x fewer** |
| fused | 27.64 | 20.26 | 0.733 | **1.36x fewer** |

PB-6's 2.28x kernel-instruction win reproduced end-to-end (diluted by non-number
work). The kernel does exactly what PB-6 said **at the instruction level**. That the
parse_only drop is ~half confirms digit-scan dominates canada — which is *why* its
latency matters (section 5).

## 5. The refutation: cyc/B and CPI (canada, adjacent same-load PMU A/B, window 1 load≈11–14)

| lane | swar cyc/B | neon cyc/B | delta cyc/B | swar CPI | neon CPI |
|---|--:|--:|--:|--:|--:|
| parse_only | 1.873 | 2.399 | **+28.1%** | 0.130 | 0.318 |
| parse_tape (product) | 2.090 | 3.143 | **+50.4%** | 0.128 | 0.304 |
| fused | 3.683 | 3.583 | -2.7% | 0.133 | 0.177 |
| sonic (unchanged ref) | 2.818 | 2.892 | (drift) | 0.143 | 0.147 |

**Fewer instructions, MORE cycles.** CPI inverts 0.13→0.30 (IPC 7.7→3.3): the SWAR
ladder is ILP-rich GPR arithmetic; the NEON kernel is a latency-bound serial chain
(cross-domain `fmov` x3 + a `count`-dependent `ldr` mid-chain). On the live cold
parser each number sits on the parse critical path — a **latency** regime — so
fewer-but-serially-dependent NEON instructions lose to more-but-parallel SWAR ops.
The `fused` lane is ~flat only because eager `materialize_f64` + digest dominate it,
masking the scan penalty.

## 6. Gate: paired wall ratios (canada, load-certified, 3 windows load≈13–15)

| ratio | swar (mean of 3) | neon (mean of 3) | movement |
|---|--:|--:|--:|
| **fused/sonic** (gate lane) | **1.182** | **1.196** | none → away from 0.95x |
| tape/sonic (product) | 0.703 | 1.089 | **regression** |

R2-quoted baseline `fused/sonic` canada 1.199 (0.83x) — my same-machine SWAR
baseline 1.182 confirms it. **Gate >=0.95x not met; no material movement; product
lanes regress.** twitter/citm `fused/sonic` also flat (0.62→0.62 / 0.57→0.56):
short integer / `0`-heavy runs never engage the <=16-block win and pay the same latency.

## 7. Why the canada fused/sonic loss survives

R2 attribution already located the canada loss in **eager `materialize_f64`** (the
Clinger/Eisel-Lemire float tax, +1.48 cyc/B on canada), *not* in digit-run
instruction count. PROVE-02 targets the mantissa **accumulation** (span scan); the
`fused` lane's bottleneck is downstream (float conversion + digest), so cutting scan
instructions leaves `fused/sonic` flat. The canada redress must attack
`materialize_f64`, not `scan_digit_run`.

## 8. Diff summary (production code = 145 LOC; +test 160; the bench bin is R2-restored infra)

```
 crates/parse-that-regex/src/number/mod.rs          | +39
 crates/parse-that-regex/src/number/simd.rs         | +106 (new)
 crates/parse-that-regex/tests/digit_simd_parity.rs | +160 (new, parity oracle)
 crates/bbnf-bench/src/bin/json_canon_bench.rs      | +298 (R2 sec0 restore, not new logic)
```

## 9. Integration cost / salvage direction

Integration cost is **near-zero and clean**: the seam is a 39-line aarch64 prelude in
`scan_digit_run` + one 106-line colocated module; no generator/@generated churn
(parse-that-regex is hand-written), no API change, exact overflow semantics preserved
by falling through to the retained SWAR tail. It is landable as-is — but **should not
land**, because it regresses the product (`parse_tape`) on the corpus it targets.

Salvage hypothesis (unproven, out of scope here): the loss is the dependency
chain, not the algorithm. A count-only NEON (vshrn) feeding the *existing* SWAR
accumulate would drop most of the chain and both cross-domain `fmov`s — but PB-6's
own decomposition puts only -4/-6% of the instruction win on the count and -40/-61%
on the Mula accumulate, so a count-only variant salvages little. The honest campaign
signal: **the Mula/Lemire digit kernel is not the canada redress**; PB-6's 53.8%
was a warm throughput microbench and does not survive the cold latency-bound parser
(CRIT-PB-6 residual #2, now measured).

## Appendix — raw capture pointers

- baseline paired+PMU: `scratchpad/logs/bench_baseline.txt` (loadavg 16.09)
- candidate paired+PMU: `scratchpad/logs/bench_candidate.txt` (loadavg 19.09)
- same-load paired A/B (3 windows): `scratchpad/logs/ab_paired.txt`
- same-load PMU A/B (2 windows): `scratchpad/logs/ab_pmu.txt`
- parity oracle run: `scratchpad/logs/parity_test.log` (30 passed)
