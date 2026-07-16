# PROVE-01 — Adversarial Verification (VERIFY.md)

**Verdict: CONFIRMED** (with a magnitude caveat: the headline skip-heavy peaks are
optimistic single-run values; reproducible central tendency is lower but the same
order, all gate adjudications hold, parity 3/3, no edict violations).

Verifier tree: prover worktree `/.../.claude/worktrees/wf_386900ed-462-1` (patch
applied, 9 files staged, base `d2b32f6e3`). Prebuilt bench reused for timing;
parity test rebuilt (`cargo test -p bbnf-simd --test prefilter_parity --release`).

## Fresh numbers — 2 independent windows (cold paired PMU, prebuilt binary)

Metric of record = PMU cyc/B (proc_pid_rusage V5 ri_cycles; per-process, so
load-robust). speedup = scalar / fused.

| lane | W1 cyc (N=200, la~8) | W2 cyc (N=250, la~9) | claimed | prover run-2 range |
|---|--:|--:|--:|--:|
| skip_heavy_shufti  | **8.04x** (0.130→1.042) | **6.87x** (0.156→1.070) | 8.90x | 7.1–9.1x |
| skip_heavy_truffle | **4.67x** (0.224→1.044) | **4.78x** (0.217→1.038) | 5.56x | 4.6–5.8x |
| twitter (dense)    | **1.044x** | **1.026x** | 1.05x | 1.04x |
| bootstrap (dense)  | **1.094x** | **1.081x** | 1.15x | 1.17x |

- All four **directions reproduce**. Dense-neutrality gate (the actual MET claim)
  is **robustly MET** in both windows (both >1.0x, ≥0.95 satisfied with margin).
- Skip-heavy magnitude lands **inside/at the low end of the prover's own disclosed
  ranges**. Headline peaks 8.90x/5.56x are optimistic single-run; central values
  are ~7.5x shufti / ~4.7x truffle. bootstrap 1.15x→~1.09x (still MET).
- Fair scalar baseline measured **1.04–1.07 cyc/B** (report says ~1.03) — confirmed.
- Fused floor measured **0.13–0.16 cyc/B** shufti (report claims 0.115 — its
  best-case; the small floor is proportionally noise-sensitive at load).

## Parity oracle — rebuilt, 3/3 PASS
```
test classify_block_alignment_and_set_sweep ... ok   (NEON==scalar, set 1..=16, align 0..64)
test find_member_fused_matches_scalar_oracle ... ok  (both backends; dense/skip/boundary)
test corpus_parity_twitter_full_scan ... ok          (byte-identical position vectors)
test result: ok. 3 passed; 0 failed
```

## Attack list
1. **Opt-profile asymmetry** — none. Both lanes one binary, one `--release`.
   Loops byte-for-byte symmetric (`scan_all_fused`/`scan_all_scalar` identical
   shape, identical output, parity-asserted before timing).
2. **Warm contamination** — PMU loops 400× over the same buffer (L2-warm). But
   warm is symmetric AND *conservative for the headline*: on skip-heavy the scalar
   streams every byte while fused skips, so a cold run would penalize scalar more →
   warm UNDERSTATES the skip-heavy ratio. Not a refutation. (Wall lane is cold
   paired per-sample.)
3. **Load confound** — prover ran la=12.7, I ran la~8–9. Per-process PMU +
   per-sample interleaving null load drift; lower load did NOT inflate (my ratios
   came in below the peaks). Robust.
4. **Cherry-picked window** — two windows reproduce; no inflation beyond disclosed
   variance.
5. **x86 tokens** — none. All NEON under `#[cfg(target_arch="aarch64")]`; scalar
   fallback elsewhere. aarch64-only edict respected.
6. **Grammar-name gate** — none. Backend is pure cardinality (`members.len()`:
   ≤8 nibble, ≤16 wide, else scalar). No name gates.
7. **Patch-vs-report** — source matches patch.diff. LOC **+893/-0**, report says
   901 (8-LOC overclaim, immaterial; additive confirmed).
8. **Stub/dead-surface** — `SimdMode::Prefilter` (+4) is an unconsumed enum
   variant; the live scan still emits `Exact`. **Disclosed** in §6 as the
   not-yet-wired seam ("~1-day integration"), matching the honest PARTIAL verdict.
   A declared seam, not a hidden stub.
9. **Disasm claim** — verified via `otool -tV`: reject path reduces to
   `cmtst.16b`+`umaxv.16b`; `shrn` (movemask) appears only in the classify/locate
   path. Matches §4.

## The 15x-miscalibration reasoning — SOUND, with one caveat
Arithmetic checks: 0.115×15 = 1.725 cyc/B. Against a measured 1.03–1.07 cyc/B
membership scan, the ceiling is 1.03/0.115 ≈ 8.96x — **15x is physically
unachievable against this baseline**, so the inherited 15x bar did presuppose a
costlier consumer. The reframe is arithmetically correct and my measurements
corroborate both operands (baseline ~1.04, floor ~0.13–0.16). **Caveat:** the
">=15x against a ≥1.75 cyc/B consumer" projection is *hypothetical* — no such
consumer is wired (§6 concedes the live path emits Exact). Minor prose imprecision:
"1.03 cyc/B near memory-streaming speed" is loose (1 cyc/B is compute-bound
~3–4 GB/s, not DRAM bandwidth); immaterial to the ceiling arithmetic.

## Rationale
Direction reproduces on all four lanes; dense-neutrality gate MET with margin in
both windows; parity 3/3 on rebuild; no edict violations (aarch64-only, no
name-gate, additive, disclosed seam). The prover's verdict is honestly PARTIAL
(15x UNMET, stated). Headline peaks are optimistic but bracketed by the prover's
own disclosed ranges. Nothing inflates the claim; the mechanism and its gates
survive attack. → **CONFIRMED.**
