# PROTO-2 — Canonical movemask producer + 9B LEAN/DENSE rebirth

**Family:** 9B NATIVE-REBIRTH · **Probe:** LOCK-9 pass-2 probe 2 · **Verdict:** RAN_MEASURED
**Host:** Apple Silicon aarch64, 18 logical cores, macOS Darwin 25.4.0
**loadavg** (shared host, not isolated): 16.86 at kickoff → 9.1–13.5 across the four bench windows (recorded per-window in `pristine_bench.txt` / `rebirth_bench.txt`).
**Toolchain:** `nightly-2026-04-11` (rustc 1.96.0-nightly 02c7f9bec), the parse-that pin.

---

## Question

Two questions, one probe:

1. **(LEAN, banks 9B's cheapest win)** Does rebirthing `number_simd.rs:54-55`'s
   double-`vaddv` movemask to the nibble-stride `cmeq → vshrn_n_u16::<4> →
   vget_lane_u64 → tz>>2` idiom emit `shrn.8b …,#4` with **zero `vaddv`** and an
   instruction count **≤ baseline** — i.e. the refuted 2271-vs-509 `addv.8b` defect
   does not reproduce — while holding paired wall-time ≥0.98×?

2. **(DENSE, the owed instruction-count datum)** At a real DENSE classify site
   (`quote_parity.rs::classify_stripe_64`), does the simdjson dense-64 `vpaddq`
   bit-weight tree **beat / wash / lose** against the current
   4×(`to_bitmask`+shift+OR), measured by total instructions to the two u64 masks?

Both answers feed the shared canonical-producer spec (LOCK-9 ∩ LOCK-5) and seed
LOCK-6's 9B-side gate-5 substrate-divergence datum.

---

## Method

- **Scratch prover** (`…/scratchpad/pt-scratch`): `rsync` of `/Users/mkbabb/Programming/parse-that`
  per R3 §8, external sibling patch deps repointed to absolutes (read-only), isolated
  `CARGO_TARGET_DIR` inside the copy. parse-that itself never mutated.
  - The scratch was found in a **broken half-applied LOCK-9 9A state** (a prior probe
    had reduced `structural_bitmap.rs::classify_stripe` to 3 params without fixing its
    4-arg caller → `E0061`). Force-restored the whole `rust/` source tree from SRC with
    `rsync --checksum --delete` (preserving only my `.cargo/config` repoint + the
    `[profile.bench]` build-speed override) to get a **clean parse-that-master baseline**.
- **Disasm (deliverable 3):** a standalone probe `producer_disasm.rs` reproduces all six
  producer forms verbatim as `#[no_mangle] #[inline(never)]` symbols, compiled `rustc -O`
  (opt-level 3, single-crate — the inlined regime), `otool -tv`, instructions counted from
  each symbol label to `ret`. "Usable mask/count per contract" = the LEAN forms return the
  digit **count** (compare→pack→tz); the DENSE forms return the **two u64 masks**; the
  u8x16→u16 forms return the **u16**. This isolates the exact kernels and sidesteps the
  aggressive cross-crate inlining that hides them in the full-lib build.
- **Paired cold divan (deliverable 4):** `micro_parse_that` (number lane: `json_number_*`)
  + `parse_that_combinator` (`apache`, `canada`, `twitter`, `citm_catalog`), N=200 samples,
  **two windows** per arm, pristine vs rebirth, identical `[profile.bench]` (thin-LTO/16-CGU
  override for build speed under the saturated host — the paired ratio is invariant to the
  profile since both arms share it; only absolute wall-times differ from fat-LTO).
- **LOC duplication (deliverable 5):** `wc -l` + `grep` of the rebirth kernels against the
  bbnf-simd aarch64 equivalents (`movemask.rs`, `byte_class_from_eq_set_64.rs`,
  `structural_classify_64.rs`, `classify_tbl4.rs`).

---

## What ran

| Step | Result |
|---|---|
| Scratch build (pristine, thin-LTO) | `Finished bench profile … in 19.00s`, rc=0 |
| Standalone disasm probe (6 forms) | compiled + `otool -tv` clean |
| Pristine divan, 2 windows | rc=0, all 7 benches × 2 windows |
| Both rebirths applied + rebuilt | rc=0 (vshrn present, **zero vaddv**, x86 arm deleted; vpaddq tree present) |
| Rebirth divan, 2 windows | rc=0, all 7 benches × 2 windows — **all 4 JSON files parsed without panic** (functional parity) |

---

## The numbers

### (3) Disasm — total instructions to a usable mask/count, per contract

| # | Producer form | Contract | **Insns** | Reduction ops | Signature mnemonic |
|---|---|---|--:|--:|---|
| 1 | LEAN **baseline** — `number_simd.rs:54-55` double-`vaddv` | → count | **19** | **2× `addv.8b`** | `addv.8b` ×2, `bfi`, `rbit`/`clz` |
| 2 | LEAN **rebirth** — `vshrn` nibble | → count | **14** | **0** | **`shrn.8b v0, v0, #0x4`**, `rbit`/`clz`, `lsr #2` |
| 3 | DENSE **baseline** — `classify_stripe_64`, 4×(`to_bitmask`+shift/OR) | → 2×u64 | **64** | 8× `addv.8h` | `zip1`+`addv.8h` ×8 + scalar `bfxil`/`orr` |
| 4 | DENSE **rebirth** — simdjson `vpaddq` bit-weight tree | → 2×u64 | **50** | **0** | `and`+`uzp1`/`uzp2`/`orr` (see note) |
| 5 | `bbnf-simd/aarch64/movemask.rs` — `vshrn`+repack | → u16 | **28** | 0 | `shrn.8b` + `zip1`/`ushl`/scalar repack |
| 6 | refuted `byte_class_from_eq_set_64.rs:79-87` double-`vaddv` | → u16 | **11** | **2× `addv.8b`** | `and.16b`, `addv.8b` ×2, `ext`, `bfi` |

**LEAN verdict — PASS (banks 9B's cheapest win).** The rebirth emits `shrn.8b v0, v0, #0x4`
with **zero `vaddv`/`addv`**, at **14 insns vs 19 baseline (−26%)**. The refuted 2271-vs-509
`addv.8b` defect **does not reproduce**: baseline = 2 `addv.8b` per call, rebirth = **0**.

**DENSE verdict — WIN (the owed instruction-count datum).** The `vpaddq` tree is
**50 insns vs 64** for 4×(`to_bitmask`+shift/OR) — **−14 insns (−22%)**, **zero reduction ops**.
It obviates *both* the per-16 repack *and* the scalar shift-OR combine (the baseline's
`lsl`/`bfxil`/`orr` chain vanishes — the tree assembles the u64 in-register). This is a
**valid positive**: the family flagged a possible DENSE loss as an acceptable negative; the
measurement is a clear win, so **the DENSE site should be rebirthed too**, not kept on `to_bitmask`.

> **Codegen note (worth banking):** LLVM lowers the disjoint-bit `vpaddq_u8` bit-weight tree
> to a `uzp1.16b`/`uzp2.16b`/`orr.16b` deinterleave chain, *not* literal `addp`/`vpaddq` — under
> the `{0x01..0x80}` disjoint-bit AND, pairwise-add ≡ pairwise-OR, and `orr` is the cheaper port.
> The idiom is thus even better than the textbook `addp` tree, and carries **no reduction op at all**.

**u8x16→u16 note.** For a *lone* u16 the refuted double-`vaddv` (11 insns) is shorter than the
`vshrn` repack (28 insns) — but it costs 2 `addv.8b` reduction ops, the exact idiom PROVE-09
refuted. This is precisely why the **contract-split** is load-bearing: LEAN sites never
materialize the u16 (nibble → `tz>>2`, form #2, 14 insns); DENSE sites assemble the u64 with
the `vpaddq` tree (form #4, 50 insns). **Neither lawful route ever emits the 16-byte repack or
the double-`vaddv`.**

### (4) Paired cold divan — baseline / rebirth (≥0.98× = PASS)

Ratios are `baseline_time / rebirth_time` (≥1.0 ⇒ rebirth as-fast-or-faster), averaged over
the two windows. Full per-window transcript in `pristine_bench.txt` / `rebirth_bench.txt`.

| Bench | metric | base | reb | ratio | gate |
|---|---|--:|--:|--:|:--:|
| json_number_int_small | median | 11.08 ns | 11.28 ns | 0.982 | PASS |
| json_number_int_large | median | 17.43 ns | 17.59 ns | 0.991 | PASS |
| json_number_float_exp | median | 14.25 ns | 13.90 ns | 1.025 | PASS |
| json_number_float_exp | fastest | 13.11 ns | 13.48 ns | 0.973 | (noise†) |
| apache | median | 96.9 µs | 97.8 µs | 0.991 | PASS |
| canada | median | 2.435 ms | 2.377 ms | 1.024 | PASS |
| citm_catalog | median | 1029 µs | 1002 µs | 1.028 | PASS |
| twitter | median | 448.6 µs | 439.2 µs | 1.021 | PASS |

**15 of 16 measured ratios (median+fastest) PASS ≥0.98×.** † The single sub-threshold is
`json_number_float_exp` *fastest* (0.973) — a 13 ns bench whose `fastest` carries ±1 ns
quantization; its own **median is 1.025** (rebirth faster) and its scan-kernel share is small
(the bench is dominated by float/exponent accumulation, not `count_leading_digits`). The
**combinator benches — where both kernels run in a real JSON workload — all PASS both metrics**,
with `canada`/`citm`/`twitter` medians at **1.02–1.03× (rebirth faster)**, consistent with the
DENSE 50-vs-64 insn win. **Both rebirths: paired ≥0.98×. PASS.**

### (5) LOC duplicated vs bbnf-simd equivalents

bbnf-simd aarch64 producer/classify LOC: `movemask.rs` 25 · `byte_class_from_eq_set_64.rs` 87
· `structural_classify_64.rs` 155 · `classify_tbl4.rs` 104. **`grep`: bbnf-simd contains
`vpaddq` in ZERO files.**

| Rebirth kernel | parse_that body | bbnf-simd equivalent | **Duplicated LOC** | Divergence |
|---|--:|---|--:|---|
| LEAN nibble movemask (`count_leading_digits_neon`) | ~10 | `movemask.rs` (25, `vshrn`+repack) | **~2** | shared `vshrn_n_u16::<4>` narrow only; tail diverges (nibble `tz>>2` vs u16 scalar repack) |
| DENSE `vpaddq` bulk64 tree (`classify_stripe_64` + `movemask_bulk64`) | ~55 | **none** (bbnf-simd's dense-u64 path is 4× `movemask_u8x16` repack + shift/OR) | **0** | fully novel — no `vpaddq` exists in bbnf-simd |

**9B gate-5 seed (substrate divergence, LOCK-6 input).** The own-source rebirth authors
**~55 net-new NEON LOC with ~2 LOC of overlap** against the shared crate — because the two
contract-split producers (nibble-`tz` for LEAN, `vpaddq`-tree for DENSE) **do not exist in
bbnf-simd at all**. Substrate divergence between 9B's rebirth and 9A's shared kernels is
near-total: adopting 9A would *not* let parse-that inherit these producers for free; they
would have to be added to bbnf-simd regardless. (Gate-5's LOC/substrate question is thereby
seeded, not closed — it needs the merged-surface count across all LEAN/DENSE sites.)

---

## Verdict line

**RAN_MEASURED.** LEAN rebirth: `shrn.8b …,#0x4`, **zero `vaddv`**, **14 ≤ 19** insns (−26%) —
2271-vs-509 does **not** reproduce → **PASS, 9B's cheapest win banked**. DENSE `vpaddq` tree:
**50 vs 64** insns (−22%, zero reduction ops, lowers to `uzp`/`orr`) → **WIN vs 4×`to_bitmask`+shift/OR**
(rebirth the DENSE site too). Both rebirths paired **≥0.98×** (15/16; lone outlier is 13 ns
`fastest` quantization, median 1.025). LOC-duplicated table emitted: **~2 LOC** (LEAN) / **0 LOC**
(DENSE, novel) shared with bbnf-simd → **near-total substrate divergence, LOCK-6 gate-5 seeded**.
The shared canonical-producer spec (LEAN nibble-`vshrn`, DENSE dense-64 `vpaddq` tree, never the
16-byte repack, never double-`vaddv`) is now **measured, not asserted** (LOCK-9 ∩ LOCK-5 datum).

## Artifacts

- `PROTO-2-rebirths.patch` — unified diff of both rebirths (`number_simd.rs` + `quote_parity.rs`).
- `producer_disasm.rs`, `pd.disasm` — standalone six-form probe + full `otool -tv` (in scratchpad).
- `pristine_bench.txt`, `rebirth_bench.txt` — the four divan windows (in scratchpad).
