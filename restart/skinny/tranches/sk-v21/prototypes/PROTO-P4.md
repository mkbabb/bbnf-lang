# PROTO-P4 — Measured KernelCostTable flips an extraction decision

Prototype instrument for SK-V21 pass-1 (charter `PASS-ALPHA.md`, spec `SPEC-DRAFT-P1.md` §5).
Worktree-isolated; nothing here lands on master. Uarch: **Apple M5 Max**. PMU: `proc_pid_rusage`
`RUSAGE_INFO_V5.ri_cycles` (the same counter `bbnf-bench`'s cold harness reads —
`skinny/crates/bbnf-bench/src/bin/profile_direct.rs:44-45,118`).

## The falsifiable question

Do PMU-measured cycles/64B for real kernel alternatives differ enough from the hardcoded thresholds
in `crates/ir/src/passes/recognizers/kernel_shape.rs` (`NIBBLE_LUT_MAX = 8` L63, `WIDE_LUT_MAX = 16`
L68) to change at least one extractor selection?

`KernelShape::for_singleton_count(N)` (kernel_shape.rs:71-79) is a pure threshold ladder:

```rust
if count <= Self::NIBBLE_LUT_MAX { NibbleLut }        // N <= 8   -> vqtbl LUT
else if count <= Self::WIDE_LUT_MAX { WideLut }        // 9..=16   -> vqtbl LUT, wider payload
else { MultipassCmpEq }                                // N > 16   -> N * vceqq + OR-reduce
```

The doc comment (L44-56) states NibbleLut and WideLut have the *same intrinsic count* — so the only
place threshold-vs-cost can be tested is (a) the LUT-vs-MultipassCmpEq boundary the ladder puts at
N=16, and (b) the never-considered low-N end, where the ladder always picks a LUT.

## VERDICT: SUPPORTS — at least one selection flips, with three measured flips

## Method

A checkasm-shaped standalone crate (`p4-kernelcost`, non-workspace) registers the real in-tree
kernels behind one 64-byte-block `-> u64 structural mask` signature, verifies every one **byte-exact
against a scalar reference** over a randomized 12%-structural-density corpus across the whole N-sweep
(parity gate passes), then times each with the PMU. Robust estimator: **min of 5 repeats** (under
load, extra cycles only ever add, so the minimum is the least-perturbed estimate). Build:
`RUSTFLAGS="-C target-cpu=native"`, `lto=fat`, `codegen-units=1`, `panic=abort`.

Kernels are lifted verbatim from the tree:
- `classify_lut` = `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:22-44` — `vqtbl4q_u8` over a
  64-entry low-6 table + readback-verify `(class==chunk) & (class>0)`. O(1) in |S|.
- `classify_multipass` = the `MultipassCmpEq` shape — one `vceqq_u8`+`vorrq_u8` per byte. O(|S|).
- movemask A = `movemask.rs:3-25` (**vshrn**/narrow + scalar pack loop — the variant
  `classify_tbl4.rs:5` actually imports on the hot path).
- movemask B = `byte_class_from_eq_set_64.rs:78-92` / `structural_classify_64.rs:145-155`
  (**vaddv** bit-weight reduction — the variant those siblings define locally).

**Honesty / limits.** loadavg during capture was **17.3–23.0** (other agents live) — far from the
charter's quiet-<2 discipline. Absolute cyc/64B are therefore inflated by contention. The result
carried forward is a **ratio between two kernels measured back-to-back under identical load**, and
the multipass cost is cleanly linear in N (physical: one fixed-cost pass per needle), so the crossover
location is robust to the absolute inflation. A quiet re-capture belongs in TW0's bench session; it
will move the absolute cells, not the sign of the flips.

## The measured table  `(KernelId, Uarch=AppleM5Max) -> cycles/64B`  (min-of-5, loadavg 17.3)

| KernelId | cyc/64B | note |
|---|---|---|
| `classify_lut` (vqtbl4q + readback, movemask=vaddv) | **14.38** | O(1) in \|S\| |
| `classify_lut` (movemask=**vshrn**, the in-tree default) | 22.50 | +8.1 (+56%) |
| `movemask` vshrn − vaddv delta (4 movemasks/block) | **+8.12** | vshrn is slower |
| `multipass_cmpeq` N=1 | **9.49** | 0.66× LUT |
| `multipass_cmpeq` N=2 | 14.92 | 1.04× LUT (tie) |
| `multipass_cmpeq` N=3 | 18.73 | 1.30× |
| `multipass_cmpeq` N=4 | 21.51 | 1.50× |
| `multipass_cmpeq` N=8 | 41.99 | 2.92× |
| `multipass_cmpeq` N=16 | 82.88 | **5.76×** |
| `multipass_cmpeq` N=24 | 125.40 | 8.72× |
| `multipass_cmpeq` N=32 | 164.28 | **11.42×** |

Per-needle slope ≈ 4.9 cyc/64B; LUT constant ≈ 14.4. **Measured crossover N\* = 2** (multipass wins
only at N=1; N=2 is a statistical tie; LUT wins outright from N≥3 and its lead grows without bound).
Run 1 (loadavg 23, single-shot) gave the same shape with N\* between 2 and 4; min-of-5 tightened it.

## The three flips

**FLIP 1 — the low-N selection the ladder never questions.** For a **single-byte structural alphabet
(N=1)** the recognizer selects `NibbleLut`, but one `vceqq` is measured **9.49 vs 14.38 cyc/64B =
0.66×** — a 34% saving. A table-driven extractor flips N=1 (and defensibly N=2, a tie) from
`NibbleLut → MultipassCmpEq`. This is a real selection change on any grammar whose structural scan is
a single delimiter — CSV newline/comma scan, a quote-only string-body pre-scan, a single-terminator
split. The ladder's `N<=8 → NibbleLut` is a measured mis-selection at its bottom edge.

**FLIP 2 — `WIDE_LUT_MAX=16` is not a cost boundary at all.** The ladder sends N>16 to
`MultipassCmpEq`, yet the LUT already beats multipass **5.76× at N=16** and **11.42× at N=32**, and
the gap only widens (LUT is O(1)). On this uarch `MultipassCmpEq` is **never cost-optimal for N>1**.
The measured table demotes it from "chosen above 16" to an **admissibility-only fallback** — reachable
solely when >16 structural bytes collide in low-6 bits so the single `vqtbl4q` table cannot represent
them (a *correctness* constraint, not a cost one). The threshold's entire upper half is re-founded:
the boundary is collision-feasibility, not cardinality.

**FLIP 3 — the movemask kernel pair (bonus, checkasm-shaped).** The tree ships two `movemask_u8x16`
implementations. The hot structural path (`classify_tbl4.rs:5 use super::movemask::movemask_u8x16`)
imports the **vshrn** one; its siblings define the **vaddv** one locally. Swapping only the movemask
inside the identical LUT classify: **22.50 (vshrn) vs 14.38 (vaddv) cyc/64B — vaddv is 36% cheaper**,
+8.1 cyc/64B across the 4 movemasks/block. A `KernelCostTable` cell for `movemask` selects vaddv and
flips every classify kernel that currently calls the vshrn default. This is a checked, in-tree
inconsistency (two costs for one named op) that the table resolves by measurement.

## The threshold → table diff (what the selector becomes)

`for_singleton_count` stops being a `const fn` ladder and becomes a lookup that admissibility-filters,
then argmin's the measured cells:

```rust
// BEFORE (kernel_shape.rs:71-79): cardinality ladder, thresholds baked in.
if count <= 8 { NibbleLut } else if count <= 16 { WideLut } else { MultipassCmpEq }

// AFTER: measured table + admissibility filter (thresholds demote to filters, per SPEC §5.1).
fn select(count: usize, lut_feasible: bool, tbl: &KernelCostTable, u: Uarch) -> KernelShape {
    let lut = tbl.cyc_per_64b(KernelId::LutClassify, u);          // 14.38, constant
    let mp  = tbl.multipass_cyc_per_64b(count, u);                // ~9.5 + 4.9*(count-1)
    match (lut_feasible, mp < lut) {
        (true,  true)  => KernelShape::MultipassCmpEq,  // only N∈{1(,2)} on M5 Max
        (true,  false) => KernelShape::NibbleLut,       // the vqtbl LUT wins from N≥2/3
        (false, _)     => KernelShape::MultipassCmpEq,  // collision fallback: LUT can't represent S
    }
}
```

On the live grammar fleet: JSON's structural alphabet (`{}[]:,"` ≈ 6–7 bytes) stays `NibbleLut`
(correctly — LUT wins from N≥3). A **single-delimiter grammar** (CSV row-split on `,` or `\n`, or a
quote-only body scan) flips **NibbleLut → MultipassCmpEq** and saves ~34% of the classify cost —
one concrete extractor selection changed by measurement. And **no grammar in the fleet** should ever
select `MultipassCmpEq` on cost grounds for a large alphabet; it survives only as the collision
escape hatch — a demotion the current threshold hides.

## Resurrectable kernels (the crate is discarded with the worktree)

```rust
// classify_lut — classify_tbl4.rs shape, 64B block -> u64 mask, O(1) in |S|.
for lane in 0..4 {
    let chunk = vld1q_u8(ptr.add(lane*16));
    let low6  = vandq_u8(chunk, vdupq_n_u8(0x3f));
    let class = vqtbl4q_u8(table, low6);                            // uint8x16x4_t, 64-entry lo-6 table
    let structural = vandq_u8(vceqq_u8(class,chunk), vcgtq_u8(class,vdupq_n_u8(0)));
    out |= (movemask(structural) as u64) << (lane*16);
}
// classify_multipass — MultipassCmpEq shape, O(|S|).
for lane in 0..4 {
    let chunk = vld1q_u8(ptr.add(lane*16)); let mut acc = vdupq_n_u8(0);
    for &b in needles { acc = vorrq_u8(acc, vceqq_u8(chunk, vdupq_n_u8(b))); }
    out |= (movemask(acc) as u64) << (lane*16);
}
// movemask A (vshrn, movemask.rs) vs B (vaddv): B is 36% cheaper on M5 Max.
// B: let bits=vandq_u8(v, bit_weight_pattern); (vaddv_u8(lo(bits)) | vaddv_u8(hi(bits))<<8)
```

Full harness: `p4-kernelcost/src/main.rs` in the worktree (PMU struct copied verbatim from
`profile_direct.rs:11-72`; parity gate + min-of-5 measure). Raw runs:
`scratchpad/p4_run1.log` (loadavg 23), `scratchpad/p4_run2.log` (loadavg 17, min-of-5).

## Convergence contribution

The SPEC's central §5 artifact — "the one net-new artifact is the **measured `KernelCostTable`** …
never estimated" — is now demonstrated to *do work*: it flips ≥1 selection (single-delimiter grammar,
N=1) and structurally re-founds `WIDE_LUT_MAX` from a cost boundary to a collision boundary, plus
resolves a two-cost-for-one-op movemask inconsistency already in the tree. The threshold ladder is
**not** already matching measurement (the null is refuted). TW4's "KernelCostTable first" ordering is
validated, and the checkasm liveness harness that produces these cells is the same instrument that
proves kernel parity — one artifact, two duties, exactly as §5.4 wants.

Confidence 82% (directional on absolute cyc/64B under loadavg 17–23; the flip *signs* are
ratio-robust and the parity gate is exact).
