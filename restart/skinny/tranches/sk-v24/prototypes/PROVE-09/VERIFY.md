# PROVE-09 — ADVERSARIAL VERIFY

**VERDICT: REFUTED.** The headline claim — *portable_simd→NEON shim + smallbox
de-nightly = **1.34–1.57× FASTER** median across 6 corpora* — does **not**
reproduce on a fresh, paired, interleaved rebuild. The de-nightly is
performance-**neutral** (≈parity ±15% at load), and where any signal survives
the noise it leans the **wrong way** (patched marginally slower). The report's
own flagged mechanism ("portable_simd's suboptimal `to_bitmask`/`swizzle_dyn`")
is disproven at the instruction level.

Host aarch64-apple-darwin (M-series, 18 CPU), Darwin 25.4.0. Fresh scratch copy
of parse-that per R3 §8; `.cargo/config.toml` patch table repointed to absolute
sibling repos; `CARGO_TARGET_DIR` inside the copy; toolchain **nightly-2026-04-11**
(the pin); `[profile.bench]` = `lto=false / cgu=16 / O3` applied **identically to
both arms** (matches report A3 D1). Baseline built on the **pristine** copy
FIRST; patch.diff then applied to the same pristine copy (clean `-p1`, portable);
patched built; both `parse_that_combinator` binaries saved and run back-to-back.

## Reproduction method
- One variable only: patch.diff (`std::simd`→`crate::simd128` + `smallbox` de-nightly).
- Both binaries: same nightly, same profile, same target dir. Sizes differ
  (2 275 808 vs 2 276 096 B) → genuinely distinct codegen.
- Divan `--bench --sample-count 200`, 2 independent windows, opposite arm order.

## Fresh numbers — median (µs/ms), N=200, r = base/patched (>1 ⇒ patched faster)

| corpus | claim | A base med | A patch med | **A r** | B base med | B patch med | **B r** | best-min r |
|---|---|---|---|---|---|---|---|---|
| apache | 1.34× | 114.0 µs | 119.9 µs | **0.95** | 117.4 µs | 99.41 µs | 1.18 | 1.03 |
| canada | 1.57× | 2.621 ms | 3.140 ms | **0.83** | 2.543 ms | 2.589 ms | 0.98 | 0.98 |
| citm   | 1.54× | 1.175 ms | 1.369 ms | **0.86** | 1.088 ms | 1.061 ms | 1.03 | 1.03 |
| data   | 1.51× | 27.66 µs | 34.12 µs | **0.81** | 26.83 µs | 25.83 µs | 1.04 | 1.04 |
| data_xl| 1.42× | 32.08 ms | 34.60 ms | **0.93** | 27.81 ms | 30.66 ms | 0.91 | 0.97 |
| twitter| 1.56× | 541.9 µs | 553.9 µs | **0.98** | 478.3 µs | 553.3 µs | 0.86 | 0.84 |

Window A (baseline first): patched **uniformly slower** (0.81–0.98×). Window B
(patched first): mixed, straddling 1.0 (0.86–1.18×). best-min (least-preempted
sample across both windows): 0.84–1.04×, centered at **parity**; twitter favors
baseline. loadavg 10–17 throughout (recorded per run). **No corpus, in any
window or metric, approaches 1.34×, let alone 1.57×.** The two windows even
disagree in *sign* on apache/citm/data → the effect is inside the noise floor.

## Attack list & findings
1. **Magnitude/direction (PRIMARY) — REFUTED.** Paired rebuild shows neutral-to-
   slower, not 1.34–1.57× faster. The report's baselines roughly match mine
   (apache ~114–117 vs its 136.7; canada ~2.5 vs its 3.96) but its *modified*
   numbers (apache 102, canada 2.53, twitter 470) are faster than **anything** I
   measure for the patched arm — a classic two-arm **window asymmetry** (arms
   measured under different load / cherry-picked quiet window for "modified").
2. **Mechanism claim ("portable_simd `to_bitmask` suboptimal on old nightly") —
   DISPROVEN.** `otool -tv`: patched emits **2271 `addv.8b`** (shim's two
   `vaddv` movemask halves) vs baseline **509** — the shim does *more* reduction
   work per `to_bitmask`, consistent with it being ≥ as costly, not cheaper.
   `tbl` (swizzle_dyn) identical at **516 = 516** → swizzle is codegen-equivalent,
   contributes zero delta. The proposed source of the win is absent.
3. **Correctness/parity — PASS (run-level).** Shim audited faithful: `to_bitmask`
   (bit-position `vand` + `vaddv` halves, no carries, lane i→bit i) == `Mask::to_bitmask`;
   `swizzle_dyn`=`vqtbl1q_u8` (OOB→0, matches); `simd_ne`=`vmvnq∘vceqq`;
   `>>`=`vshlq` by −n; `any`/`all`=`vmaxvq`/`vminvq`. Both arms parse all 6
   corpora across ~800 parses/arm with `unwrap` Ok, exit 0. (Full `cargo test`
   suite not re-run under the 45-min cap; not needed — verdict rests on perf.)
4. **Edict audit — CLEAN (no violations).** simd128.rs is aarch64-NEON + a
   `[u8;16]` scalar fallback; **no x86 intrinsics/tokens introduced**; no
   grammar-name gates; parsimonious (276-L shim + 7 one-line `use` swaps); the
   bump-slab arena is **honestly carried as not-done**, not stubbed. smallbox
   `smallbox!`≡`SmallBox::new` (alloc-neutral). The de-nightly *port itself* is
   legitimate and compiles on nightly; it is only the **perf win** that is false.
5. **Stable-compile sub-claim — not independently reproved** (deferred under cap;
   secondary to the refuted headline). Note the retained `#![feature(fmt_helpers_for_derive)]`
   in the touched lib.rs is itself a stable `E0554`, so "my 12 files: zero stable
   errors" is at best nuanced; irrelevant to the perf refutation.

## Rationale
The de-nightly is a faithful, alloc-neutral port whose *expected* outcome is
cyc/B parity — and that is exactly what a fresh paired measurement shows. The
report's 1.34–1.57× is not a property of the patch; it is an artifact of
comparing two arms sampled under different conditions. Direction of the claimed
large speedup fails to reproduce (window A is uniformly the *other* way), so this
is REFUTED, not CONFIRMED_DELTA.
