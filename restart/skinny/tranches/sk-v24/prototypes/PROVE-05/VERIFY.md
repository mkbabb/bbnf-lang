# PROVE-05 — ADVERSARIAL VERIFICATION

**Target claim:** CSS shufti tokenizer wired into the generated CSS scan.
track1_rich cyc/B −20.4 / −20.8 / −13.1 % (bootstrap/tailwind/material); lcss
ratio 2.20→2.90 and 3.11→4.12; animate +9.8 % regression; scalar-parity suite
passes; generated.rs via clean regen.

**Verdict: CONFIRMED_DELTA.** Direction reproduces on all 4 corpora (3 wins that
widen the lcss band + the animate regression). Parity passes; no edict violation.
But two headline magnitudes are materially off: **bootstrap −14.6 % (not −20.4 %)**
and **animate +13.4 %, structural (not the ~+5 % "residual after noise" the report
argues)**. Tailwind and material reproduce closely.

---

## Method

Prover worktree `wf_386900ed-462-5` present with built artifacts. Rebuilt BOTH
arms myself (fat-LTO release, nightly-2026-04-11): **wired** = staged tree;
**baseline** = `git stash` of the 5-file patch (symmetric to the prover's own
A/B). Distinct binary hashes (base `f6f722a6…`, wired `d008b767…`), so genuinely
different code. Ran them **interleaved back-to-back, N=6 windows**, `CSS_CANON_PMU=1
… 800`, all 4 corpora, loadavg 14–25 (higher than the prover's 11–17, i.e. a
harder load environment). cssparser/lightningcss are unchanged code = controls.
`cycles_per_byte` is via `proc_pid_rusage` PMU; `instructions` is deterministic
and load-immune — I lead with it where the two disagree.

## Fresh numbers — median of N=6 interleaved windows

| corpus | lane | base cyc/B | wired cyc/B | Δ% (mine) | Δ% (claim) | **insΔ%** |
|---|---|--:|--:|--:|--:|--:|
| bootstrap | track1_rich | 16.855 | 14.387 | **−14.6** | −20.4 | **+2.8** |
| bootstrap | track1_full | 16.582 | 14.168 | −14.6 | −15.3 | +3.3 |
| tailwind  | track1_rich | 14.356 | 10.974 | **−23.6** | −20.8 | −6.7 |
| tailwind  | track1_full | 14.340 | 11.161 | −22.2 | −18.9 | −8.6 |
| material  | track1_rich | 15.831 | 13.186 | **−16.7** | −13.1 | −1.6 |
| material  | track1_full | 14.585 | 12.174 | −16.5 | −14.4 | −2.2 |
| animate   | track1_rich | 14.718 | 16.683 | **+13.4** | +9.8 | **+29.9** |
| animate   | track1_full | 16.200 | 18.010 | +11.2 | +10.2 | +33.9 |

**Controls (Δ cyc/B, ins ≈ 0 as expected):** cssparser +2.8 / −6.4 / −3.4 / −1.4 %;
lightningcss +7.6 / −2.3 / −4.2 / −3.0 %. Residual window drift is ±3–7 %, so any
single-corpus cyc/B number carries a few points of load slop — exactly the confound
the report flags. Load-correct each win by its cssparser control and the three big
corpora cluster at **−15 to −18 %**.

**lcss ratios (median lcss cyc/B ÷ median skinny cyc/B), track1_rich:**

| corpus | base ×lcss | wired ×lcss | claim |
|---|--:|--:|---|
| bootstrap | 2.124 | 2.677 | 2.20→2.90 |
| tailwind  | 3.254 | **4.157** | 3.11→**4.12** (≈exact) |
| material  | 2.107 | 2.423 | 1.95→2.41 |
| animate   | 2.151 | 1.841 | 2.11→1.88 |

## Attack list

1. **Baseline-window inflation (bootstrap).** The report's base of 16.094 matches
   my *high-load* base windows (16.16–16.86), but its wired 12.805 is well below any
   wired window I saw (13.8–14.6). My paired median gives −14.6 %, not −20.4 %; the
   claimed −20.4 % looks like a favorable base × favorable wired pairing. Bootstrap
   lcss endpoint reproduces at 2.68, not 2.90.
2. **Animate regression understated AND mis-attributed.** The report says +9.8 % with
   "~half is window noise, residual ~5 % real." The **deterministic instruction count
   is +29.9 %** (track1_rich) / +33.9 % (full) — the regression is *structural*, not
   noise: the per-call LUT build + un-amortized 64-byte classify genuinely retires ~30 %
   more instructions on keyframe-dense short spans. My measured cyc/B regression is
   +13.4 %, larger than claimed. The "half noise" framing is refuted by load-immune data.
3. **Gains are mostly IPC, not work reduction.** Only tailwind actually reduces
   instructions (−6.7 %). bootstrap (+2.8 %) and material (−1.6 %) win purely on SIMD
   IPC (CPI drops). True, but it means the effect is fragile to microarchitecture —
   the report frames all four as "scan cost falls," eliding that bootstrap does *more*
   instructions.
4. **Edicts — clean.** No x86/SSE/AVX tokens in the diff; kernel lives under
   `src/aarch64/` and is `#[cfg(target_arch="aarch64")]`-gated with a scalar parity
   twin (checkasm anchor). No grammar-name gate — significant set is caller data
   (`fixed ∪ delimiters`), LUTs derived generically. No stub/fallback: uses the shared
   `movemask_u8x16` + real `vqtbl1q_u8`. Parsimony: +175/−22, 5 files, one cohesive
   kernel added to the existing tbl-family file. **No violations.**

## Parity oracles (re-run, wired tree)

- `neon_significant_skip_matches_scalar` — **PASS** (exhaustive: 5 corpora × 4 delim
  sets × every offset, shufti == scalar `significant_ref`). Membership algebra proven
  exact: `hi_lut[h]=1<<h`, `lo_lut[l]` bit-h set iff a member equals the query byte;
  non-ASCII high nibbles (b≥0x80) map to zero entries → correctly non-significant.
- `css_l4_*` route/projection suite — **PASS** (8 passed, 0 failed).
- Clean-regen: **PASS** — ran `cargo xtask regen-css`; the freshly emitted
  generated.rs added-line set is byte-identical to the committed patch (const +
  two `find_css_significant` call sites), 0 deletions, no other file touched. Not
  hand-patched.

## Verdict rationale

Direction is real and reproduces under a *harder* load than the prover used: the
shufti wiring speeds bootstrap/tailwind/material and regresses animate, and the
tailwind lcss band widens to ×4.16 (≈ the claimed 4.12). Parity and route oracles
pass; the transform is exactly semantics-preserving. **Not CONFIRMED** because two
load-immune facts contradict the report's specific numbers: bootstrap is −14.6 %
(claim −20.4 %; lcss 2.68 not 2.90), and the animate regression is a **+30 %
instruction** structural cost, not the ~+5 % post-noise residual the report asserts —
which matters because animate "gates landability." **CONFIRMED_DELTA.**
