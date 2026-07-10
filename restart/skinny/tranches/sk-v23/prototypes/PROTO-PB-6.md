# PROTO-PB-6 — SIMD mantissa accumulation probe

**Analysis instrument (never landed).** Target: parse-that `scan/number_simd.rs`. Host: Apple
M5 Max, aarch64, NEON always-on. Feeds SK-V23 **PT-R2.W4** ("SIMD mantissa + vshrn — the highest
materialization-leverage item in the whole SIMD axis", SPEC-DRAFT.md:486-490; research/02:210-231).

## Question & verdict

*Does Mula/Lemire in-vector digit accumulate + vshrn digit-count beat the SWAR
`parse_eight_digits` path with exact mantissa parity on coordinate-heavy input?*

**YES — decisively, on all three success criteria.** Exact u64 mantissa parity on 3.8M random
runs + the full canada corpus; **53.8% cyc/B win** on real coordinate-heavy canada.json (**69.6%**
on the fractional-mantissa runs alone), every window's 95% CI excluding 1.0 the same way; the
nibble-domain `tzcount>>2` trap demonstrated and handled.

## Method

Standalone scratch crate (`/private/tmp/pb6`, non-clobber: no parse-that working-tree contact),
LTO+`codegen-units=1`+`target-cpu=native`. Baseline kernels copied verbatim from
`number_simd.rs:31-102`; candidate re-authored. PMU via `proc_pid_rusage(RUSAGE_INFO_V5)`
`ri_cycles`/`ri_instructions` (data[30]/data[29]). Corpus: every maximal digit run in
`data/json/canada.json`, split into ≤16-digit stripes as the scanner does → 222,206 stripes,
1.86M digit bytes, avg 8.4/stripe (bimodal: ~2-digit integer parts + ~15-digit fractional
coordinates). Paired **interleaved** per sample (baseline then candidate, same loop), N=300,
inner=40 passes; mean ± 95% CI; 3 separated windows; per-block loadavg recorded. `ins/byte` is
the load-invariant check. Ratios certified; absolutes directional (steady-state kernel microbench,
not a full cold-parse figure — stated honestly).

## Candidate signatures (the decisive diffs)

**vshrn nibble-domain movemask** (replaces the `vaddv` AND-weights anti-pattern at
`number_simd.rs:41-59`):
```rust
let non_digit = vcgtq_u8(vsubq_u8(chunk, vdupq_n_u8(b'0')), vdupq_n_u8(9)); // 0xFF non-digit
let nd: u64 = vget_lane_u64(vreinterpret_u64_u8(
    vshrn_n_u16(vreinterpretq_u16_u8(non_digit), 4)), 0);   // 1 nibble / input lane
(nd.trailing_zeros() >> 2) as u32   // >>2 = THE TRAP; all-digit nd=0 → tz=64 → 16, no branch
```
**Mula/Lemire in-vector accumulate** (replaces SWAR `parse_eight_digits` folds + scalar tails,
`number_simd.rs:73-101`) — branchless across counts 1..=16 via a right-align shuffle then a fixed
4-stage pairwise fold:
```rust
let digits  = vandq_u8(adjusted, is_digit);          // non-digit lanes → 0
let aligned = vqtbl1q_u8(digits, vld1q_u8(SHUF[count].as_ptr())); // right-align, zero-pad low lanes
let t1 = vpaddlq_u8 (vmulq_u8 (aligned, [10,1,..]));  // →u16 lanes 0..99
let t2 = vpaddlq_u16(vmulq_u16(t1,      [100,1,..])); // →u32 lanes 0..9999
let t3 = vpaddlq_u32(vmulq_u32(t2,      [10000,1,..]));// →u64 lanes 0..1e8
vgetq_lane_u64(t3,0).wrapping_mul(100_000_000).wrapping_add(vgetq_lane_u64(t3,1))
```
`SHUF[c][j] = (j - (16-c)) as u8`: negatives wrap ≥128 → out-of-range → `vqtbl` yields 0. This
uniformly handles partial counts with ZERO scalar tail — no division, no `count<=8` byte-loop, no
`9..=15` SWAR+tail branch. (The ≤32-byte `vqtbl1q` single-table bound is respected: one 16-byte
table.)

## Results

**Correctness.** `property`: 3,800,000 runs (200k × counts 1..16 leaf kernel + 200k × 17..19
multi-stripe composition matching `scan_number_mantissa`) — **0 failures, EXACT PARITY**. Real
canada corpus: every stripe's `accumulate_baseline == accumulate_mula` and both counts == truth.

**PMU cyc/byte** (paired interleaved, N=300, inner=40):

| Corpus | load | baseline cyc/B | candidate cyc/B | ins/B base→cand | ratio (95% CI) | win |
|---|---|---|---|---|---|---|
| full canada (avg 8.4) W1 | 11.4 | 1.186 ±.013 | 0.551 ±.006 | 6.83→2.99 | 0.468 [.462,.473] | 53.3% |
| full canada W2 | 11.4 | 1.296 ±.021 | 0.574 ±.008 | 6.83→2.99 | 0.446 [.442,.449] | 55.5% |
| full canada W3 | 24.5 | 1.087 ±.005 | 0.505 ±.003 | 6.82→2.99 | 0.465 [.462,.468] | 53.5% |
| **long runs ≥9 digits (avg 14.8)** | 15.5 | 0.975 ±.007 | 0.296 ±.003 | 5.26→1.69 | 0.303 [.301,.306] | **69.6%** |
| synthetic all-15-digit | 15.5 | 0.656 ±.008 | 0.282 ±.002 | 5.27→1.67 | 0.432 [.429,.435] | 56.8% |
| synthetic all-2-digit (worst) | 15.4 | 2.588 ±.042 | 2.351 ±.022 | 18.0→12.5 | 0.924 [.910,.939] | 7.6% |

`cyc/B` absolutes wander with the (heavily loaded) machine; `ins/B` is rock-stable across windows
(baseline 6.82-6.83, candidate 2.99) — **2.28× fewer instructions**, the load-invariant truth. On
the long fractional runs, **3.11×** fewer (5.26→1.69). Every decision-grade window's CI excludes
1.0 the same way.

**Attribution** (cyc/B, `PB6_DECOMP`, N=200):

| corpus | vaddv+swar (base) | vshrn+swar | vaddv+mula | vshrn+mula (cand) |
|---|---|---|---|---|
| full canada | 1.114 | 1.066 (−4%) | 0.667 (−40%) | 0.514 (−54%) |
| long ≥9 | 0.994 | 0.935 (−6%) | 0.386 (−61%) | 0.299 (−70%) |

The **Mula accumulate is the dominant lever** (−40% full / −61% long); the vshrn count is a smaller
real contribution (−4-6%); they compose. The leverage is exactly where the claim placed it — in
mantissa *materialization*, not digit *counting*.

## The nibble-domain trap

`trap` mode: a 15-digit run through the correct kernel returns 15; the same kernel omitting `>>2`
returns **60 = 4×** (each input lane occupies a nibble of the 64-bit `vshrn` result; raw
`trailing_zeros` counts bit positions, not lanes). Asserted `correct==15 && raw==60`. Handled by
the `>>2` and by the identity that all-digit `nd==0` gives `tz==64 → 16` with no branch.

## Honesty caveats

- **Kernel microbench, warm/steady-state** — isolates the digit kernel; the live scanner
  interleaves memory traffic, dot/exponent handling, tape emission. Absolutes are directional; the
  RATIO and the 2.28-3.11× ins/B reduction are the certified findings. Loadavg was high (11-24)
  throughout; ins/B stability is what validates the ratios under that load.
- **Win is leverage-gated.** On ≤2-digit runs the candidate is only ~8% faster (CI [.910,.939],
  still excludes 1.0) — the fixed load+shuffle+fold cost barely beats a 2-iteration scalar loop.
  Real canada's ~54% comes from its long fractional runs; a purely small-integer corpus would see
  a near-wash. This is the correct, honest shape of a "materialization-leverage" win.
- **Not landed.** Scratch-only; no parse-that tree touched.

## Recommendation for PT-R2.W4

**GO.** Adopt both changes as one kernel: `count_leading_digits_neon` → the vshrn movemask;
`accumulate_digits_neon` → the branchless `vqtbl`-align + 4-stage Mula fold. The rewrite also
*deletes* the three-way `count==16 / <=8 / 9..15` branch structure (one path, KISS). Feed the
resulting u64 straight to Eisel-Lemire (`compute_f64`) unchanged — parity is exact, so no oracle
churn. Bind the close-gate to: (1) exact-value parity vs the scalar reference across random runs
(SPEC close-gate class c, PT-R2:494-497) — reproduced here at 3.8M runs; (2) a planted-trap
falsifier asserting raw-nibble `tz` == 4× (the `>>2` guard cannot silently regress); (3) a paired
canada cyc/B window. `checkasm`-liveness binds the kernel per the dav1d discipline.
