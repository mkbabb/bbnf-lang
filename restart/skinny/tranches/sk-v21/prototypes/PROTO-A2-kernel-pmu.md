# PROTO-A2 — kernel PMU session: P5 doors + P4-v2 live ladder

SK-V21 convergence pass 2, solo session A2. Uarch **Apple M5 Max**. PMU:
`proc_pid_rusage RUSAGE_INFO_V5.ri_cycles` (the counter the cold harness reads,
`skinny/crates/bbnf-bench/src/bin/profile_direct.rs:44-45`). Robust estimator:
**min-of-7** (under load extra cycles only add, so min is least-perturbed).
Both instruments are standalone non-workspace crates built with
`RUSTFLAGS="-C target-cpu=native"`, `lto=fat`, `codegen-units=1`, `panic=abort`;
raw logs beside this file (`p5-run1.log`, `p5-run2.log`, `p4v2-run1.log`,
`p4v2-run2.log`).

**Measurement honesty.** Host baseline loadavg was **8-16** all session (other
user processes; the under-2 mandate is unachievable this window per the session
protocol). Absolutes are DIRECTIONAL; **same-run ratios are authoritative**.
loadavg is recorded beside every run block below. True quiet certification
remains a TW0 gate.

---

## Deliverable 1 — P5 DOORS: does an intrinsic spelling obviate the asm lane?

### Setup

The 64-byte no-space classifier, verbatim shape from
`crates/core/src/grammar/generated/json.rs:815` `nospace_bitmap_64_neon` +
`:837` `chunk_ns_mask16`: per 16-byte chunk `ws = orr-tree(4× vceqq)` →
`vmvnq` → `vandq(bit_vec)` → **2× `vaddv`** (low/high half). Four chunks =
**8 `vaddv` SIMD→GPR crossings per 64-byte block**. Four bit-exact bindings,
parity-gated byte-exact against a scalar reference over 4096 blocks
(`PARITY OK` both runs):

- **(1) intrin baseline** — the shipping shape. LLVM De-Morgans `bit_vec & ~ws`
  into `mvn`/`bic`/`and` = ~40 classify SIMD-ALU ops (PROTO-P5's finding).
- **(2) door (a) vshrn** — the crossing-reducing movemask, `vshrn_n_u16::<4>` +
  `vget_lane_u64` (the idiom already in-tree at `json.rs:928`): **1 crossing per
  chunk = 4 crossings/block**. Natural output is the 4-bits-per-byte nibble
  mask; parity checked via its implied 1-bit mask (`nibble != 0 ⇒ non-space`).
- **(3) door (b) opaque barrier** — `ws` through `black_box`, then
  `vbicq_u8(bit_vec, ws)`: forces LLVM to keep the minimal `orr`+`bic` classify
  (32 ops, no De-Morgan blowup) while KEEPING the 8-`vaddv` reduction. Isolates
  the classify-ALU lever from the crossing lever.
- **(4) hand asm** — `core::arch::asm!`, the winning classify form of PROTO-P5
  (16 `cmeq` + 12 `orr` + 4 `bic` = 32 ops), `vaddv` reduction, `options(nostack,
  readonly)`, full v/w clobber list. Parity GREEN.

### Results (cyc/64B, min-of-7)

| variant | run1 (loadavg 10.3) | run2 (loadavg 8.6) | crossings | classify |
|---|---|---|---|---|
| intrin baseline | 15.774 | 16.870 | 8 vaddv | De-Morgan 40 ops |
| **door (a) vshrn** | **11.558** | **12.538** | **4** | (crossing lever) |
| door (b) barrier | 13.649 | 15.130 | 8 vaddv | orr+bic 32 ops |
| hand asm | 14.786 | 15.284 | 8 vaddv | orr+bic 32 ops |

Same-run speedups over baseline: door (a) **+36.5% / +34.6%**; door (b)
**+15.6% / +11.5%**; hand asm **+6.7% / +10.4%**. Door (a) is **~20-28% faster
than the hand asm** (`asm/door_a` ratio 0.78 / 0.82).

### Answers to the two P5 questions

**Which resource binds — the 8 crossings or the classify ALU ops? The 8
crossings.** The two levers are cleanly separated by doors (a) and (b): door (a)
halves crossings (8→4) and keeps the De-Morgan classify → **+35%**; door (b)
fixes only the classify op count (40→32) and keeps 8 crossings → **+13%**. The
crossing lever is worth **~2.6×** the classify-ALU lever. This resolves CRIT-P5's
open binding-resource question (CRIT-P5:36-40) in favor of the SIMD→GPR
crossings that report `25:224-227,341-343` names as the dominant Apple-NEON cost
— the movemask reduction, not the classify logic, is the bottleneck.

**Does an intrinsic spelling obviate the asm lane? YES, emphatically.** Door (a)
is a pure NEON **intrinsic** (`vshrn_n_u16::<4>`, already shipping at
`json.rs:928`) and it beats the hand asm by ~20-28% because the asm only pulled
the smaller (classify-ALU) lever while keeping 8 crossings, whereas door (a)
pulls the bigger (crossing) lever. This is exactly CRIT-P5:47-54's door-(a)
prediction confirmed: *a suboptimal algorithm in asm loses to a good algorithm
in intrinsics*. The hand asm reproduces PROTO-P5's +9-12% band (my +6.7-10.4%,
and my reduction serializes on one `v25` accumulator so it is a lower bound on
asm), but door (a)'s +35% dominates any asm figure. **Door (b) also beats the
asm** (+13% vs +6-10%) and is intrinsic-expressible — so *both* cheaper doors
CRIT-P5 asked to price obviate the asm lane.

**TW4 disposition.** Keep intrinsics as default; the asm emission path is **not
warranted for this kernel**. The win is an algorithm swap the emitter can make
in intrinsics: replace the `vaddv` movemask (`json.rs:843-844`,
`simd-scan/src/neon.rs:690`) with the `vshrn_n_u16::<4>` movemask, moving all
downstream mask arithmetic into the nibble domain (`tzcount >> 2`, per research
`25:196-201`). PROTO-P5's asm-door recommendation is superseded: the door-(a)
intrinsic retires the question.

---

## Deliverable 2 — P4-v2 LIVE LADDER: re-run against the totality kernels

### What changed from PROTO-P4 (per CRIT-P4)

CRIT-P4:44-49 dinged PROTO-P4 for measuring skinny `bbnf-simd` proxies and a
tests-only `crates/ir` selector. This re-run measures the **LIVE totality
kernels** feeding `KernelShape::select` (`crates/simd-scan/src/alphabet.rs:118-124`):
`classify_stripe_nibble` (`neon.rs:193`), `classify_stripe_wide` (`neon.rs:317`),
`classify_stripe_multi` (`neon.rs:435`), sharing the LIVE `movemask_u8x16`
(`neon.rs:690`). All three are copied **verbatim** (source lines cited in
`p4v2/src/main.rs`), so the measured code is byte-identical to what dispatches;
LUTs built by the live `NibbleLut`/`WideLut::from_singletons`
(`alphabet.rs:146,193`). Parity byte-exact vs scalar across the N-sweep
(`PARITY OK` both runs).

### Results (cyc/64B, min-of-7; run1 loadavg 13.9, run2 loadavg 14.3)

| kernel | run1 | run2 | note |
|---|---|---|---|
| LIVE nibble LUT (const) | 13.838 | 13.326 | `KernelShape::NibbleLut`, O(1) in N |
| LIVE wide LUT (const) | 18.222 | 17.232 | `KernelShape::WideLut`, O(1) — 4 vqtbl |
| LIVE MultiCmp N=1 | 10.107 | 9.728 | 0.73× nibble |
| LIVE MultiCmp N=2 | 13.300 | 12.955 | **0.96-0.97× nibble (still cheaper)** |
| LIVE MultiCmp N=3 | 17.926 | 16.190 | 1.22-1.30× nibble |
| LIVE MultiCmp N=4 | 21.605 | 20.412 | 1.53× nibble |
| LIVE MultiCmp N≥6 | ~32-33 | — | ~2.3-2.4× nibble (plateaus, pipe-bound) |

**LIVE nibble/multi crossover N\* = 3, stable across both runs** (N=2 is
multi-cheaper 0.96×; N=3 flips to nibble-cheaper 1.22×).

### The flip, and the fleet answer the brief demands

**The measured live table flips the ladder for N ∈ {1, 2}, and NO current-fleet
grammar reaches that band.** The ladder (`alphabet.rs:120-123`) assigns
`NibbleLut` for `1..=8`, but the LIVE `MultiCmp` is measured cheaper at N=1
(0.73×) and N=2 (0.96×). The current fleet: JSON structural `|S| ≈ 6-7`
(`{ } [ ] : , "`) → NibbleLut, N≥3, correctly selected (nibble wins 2.3×); CSS →
NibbleLut/WideLut, also above crossover. **So no current-fleet extraction
decision changes.** The flip band N≤2 is exactly a **single-delimiter** (CSV row
split on `,`, N=1) or **delimiter+newline** (CSV `,`+`\n`, N=2) grammar — the
**third language**, scheduled for TW6 (SPEC §9). The alphabet size that flips the
ladder is **|S| ≤ 2**.

**Two substantive corrections over PROTO-P4** (both from measuring live, not
proxy, kernels):

1. **The crossover moved from N\*=2 to N\*=3.** The live nibble path is heavier
   than PROTO-P4's single-`vqtbl4q` proxy (it does 2× `vqtbl1q` + `vtstq` +
   `vaddv` movemask = 13.3-13.8 cyc), so the flip band **widens to include N=2**.
   PROTO-P4 flipped only N=1 (single delimiter); the live measurement flips N=1
   AND N=2, i.e. it now also covers **CSV comma+newline**, not just a lone
   delimiter. The consumer for the cost table is still TW6's third language, but
   the flip it produces is one needle broader than PROTO-P4 claimed.

2. **FLIP 3 (movemask vshrn→vaddv) does NOT apply to the live simd-scan path.**
   PROTO-P4/FLIP 3 flagged a vshrn-vs-vaddv movemask inconsistency, but the LIVE
   `crates/simd-scan/src/neon.rs:690` `movemask_u8x16` **already uses the cheaper
   `vaddv`**. That inconsistency lives in skinny `bbnf-simd` and in `json.rs`'s
   quote scan (`json.rs:928`, `vshrn`), not in the totality classify ladder.
   Note the tension with Deliverable 1: for the *no-space* kernel the `vshrn`
   movemask is the CHEAPER door (fewer crossings) because it needs no per-half
   `vaddv`; for the multi/nibble classify the live `vaddv` movemask is already in
   place. The movemask choice is genuinely kernel-shape-dependent and belongs in
   the cost model as a pluggable cell, not a global default — which is the
   `pluggable-components` posture, and reconciles the two deliverables.

### Verdict for the SPEC §5 KernelCostTable

The null ("thresholds already match measurement") is refuted on the LIVE
kernels: `1..=8 → NibbleLut` mis-selects at N∈{1,2}. But CRIT-P4:50-59's
consumer-reality holds and is now measured precisely: **no current-fleet
decision changes; the table's only live flip requires |S|≤2, which only the
third language (CSV) provides.** The TW4-KernelCostTable-before-TW6-third-language
sequencing tension CRIT-P4 named stands — the cost table's live consumer must be
bound to the CSV wave (TW6), or it flips only a dead/hypothetical branch until
then. WideLut (18.2 cyc) beats MultiCmp for all N it is used for (9-16), so
`WIDE_LUT_MAX` remains a collision-feasibility boundary, not a cost one.

---

## Convergence contribution

- **P5:** the asm door is retired for the classify kernel — an intrinsic
  (`vshrn` movemask) beats hand asm by ~25% by pulling the binding lever
  (crossings, not classify-ALU); default stays intrinsics; TW4 needs no asm
  emission path here, only the movemask-algorithm swap. Resolves CRIT-P5's four
  open items (binding resource; door (a); door (b); the asm-vs-algorithm
  framing). Confidence **86%** (both doors' sign + the crossing-lever ranking are
  ratio-robust across two windows; absolute cyc directional under loadavg 8-16).
- **P4-v2:** the flip is confirmed on the LIVE kernels with the crossover
  relocated to N\*=3 (widening the flip to CSV's N=2 case) and FLIP 3 shown
  inapplicable to the live path; the honest fleet answer is **no current-fleet
  selection changes, the flip needs |S|≤2 = the third language**. Confidence
  **84%** (crossover N\*=3 identical across both runs; parity exact; the
  no-flip-until-CSV conclusion is structural, not load-sensitive).
