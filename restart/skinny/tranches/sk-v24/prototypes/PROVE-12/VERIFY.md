# PROVE-12 — ADVERSARIAL VERIFICATION

**Verdict: CONFIRMED.** Const-folded delimiter-mask residualization reproduces on
every attacked lane. Disassembly, parity, and bit-for-bit regen all hold; no edict
violations. cyc/B carries load noise (my loadavg 25-26 > prover's 13-19) but the
deterministic ins/B floor lands within/at the claimed band, favorable to the claim.

Verifier host: aarch64, loadavg 15-26 during measurement (heavier than prover's).
Base commit `d2b32f6e3`, prover worktree present with built artifacts.

---

## 1. Fresh interleaved PMU (my own baseline, 2 windows x 800 iters, back-to-back)

I rebuilt the baseline from the unpatched `generated.rs` (fat-LTO, opt3, cgu=1).
**My baseline binary is BIT-FOR-BIT identical (sha `a1cddf3e…`) to the prover's
preserved baseline** — deterministic build, genuine A/B pair. Modified arm = prover's
preserved `.mod` binary (sha `c24e37c0…`), whose disasm I verified independently.

| corpus | lane | base cyc/B | mod cyc/B | cyc ratio (mine / report) | base ins/B | mod ins/B | ins ratio (mine / report) | ins %chg |
|---|---|--:|--:|--:|--:|--:|--:|--:|
| bootstrap | track1_full | 17.759 | 14.753 | **0.831** / 0.848 | 55.779 | 52.552 | **0.9421** / 0.9475 | -5.79% |
| bootstrap | track1_rich | 18.773 | 15.073 | **0.803** / 0.812 | 67.149 | 63.956 | **0.9525** / 0.9561 | -4.75% |
| tailwind  | track1_full | 15.470 | 12.764 | **0.825** / 0.954 | 53.370 | 49.765 | **0.9325** / 0.9364 | -6.75% |
| tailwind  | track1_rich | 15.308 | 13.905 | **0.908** / 0.934 | 68.993 | 65.578 | **0.9505** / 0.9501 | -4.95% |

(cyc/B, ins/B = mean of 2 windows; bytes*iters = bootstrap 186.2M, tailwind 143.7M.)

All four lanes win on ins/B (load-proof) and cyc/B (aggregate). My ins/B ratios track
the report's to within 0.005. The claimed ins/B band was -3.7…-6.4%; my measured
-4.75…-6.75% — bootstrap/tailwind rich match nearly exactly; tailwind full is 0.35pp
past the low edge (bigger win, favorable). cyc/B strongest on bootstrap (least noisy
corpus), exactly as the report predicts.

**Noise honesty:** at my elevated load one per-window cyc/B inversion appeared
(W1 MOD tailwind_rich 15.939 > W1 BASE 13.760), but ins/B that same window was cleanly
lower (65.735 < 69.009). Inter-window ins/B spread ≈0.9% (report claimed <0.2%);
irrelevant — the 4.75-6.75% reduction dwarfs it.

## 2. Parity (both binaries, XOR-fold checksum)

| lane | my-baseline | prover-mod | prover-baseline |
|---|---|---|---|
| track1_full_parse | cksum=1 | cksum=1 | cksum=1 |
| track1_rich | cksum=17839 | cksum=17839 | cksum=17839 |

Byte-identical output. Matches REPORT §5.

## 3. regen-css bit-for-bit

`cargo xtask regen-css` from the patched template: generated.rs sha
`883aa898…` before == after. **Bit-for-bit identical.** Clean-regen discipline holds.
928 LOC (report's 910→928 = +18 confirmed).

## 4. Disassembly (otool -tvV, built binary)

- **Modified** out-of-line monomorph symbol carries the mask in its mangled name:
  `…find_component_delim` + `Ko28000000000000000800000000000000_` (= `0x2800…0800…` =
  bits {59 `;`, 123 `{`, 125 `}`} = the `";{}"` set). Body: `mov x27,#0x2800000000000000`
  (mask immediate), `and x10,x10,#0x800000000000000` (`;` half), `ldrsb`, `tbnz w9,#0x1f`
  (byte<128 guard), `lsl x9,x26,x8` + `csel`/`csel` (u128 half select), `orr`/`cbnz`.
  **No slice `ldrb [x22,#N]` loads, no length `cmp x21,#2/#3` guards.**
- **Baseline** symbol is the non-generic slice version and its body shows exactly the
  interpretive dispatch: `ldrb w9,[x22,#0x1]`, `cmp x21,#0x2`, `ldrb w9,[x22,#0x2]`,
  `cmp x21,#0x3`, `ldrb w9,[x22,#0x3]`.

The AFTER disasm in REPORT §3 is faithful. Mask-as-immediate claim CONFIRMED.

## 5. Edict audit (patch.diff)

- **aarch64-only:** no `x86|sse|avx|_mm_|target_arch|cfg(target*)` tokens. Both binaries
  `Mach-O arm64`, non-fat.
- **No name-gates:** no grammar-name / rule-name string dispatch; delimiter set is a
  const-generic parameter (pluggable), not a per-site hardcoded branch.
- **No stubs:** no `todo!/unimplemented!/unreachable!/FIXME/stub/placeholder`.
- **Parsimony:** +18 generated LOC; identical hunks in template + generated file.
- **Semantics:** for byte<128, `(DELIMS>>byte)&1` == `contains`; byte>=128 guarded false
  and all delimiters ASCII — provably preserving (parity corroborates).

## 6. Attack list (all failed to refute)

1. Opt-profile asymmetry between arms → REFUTED: my baseline sha == prover baseline sha
   (same fat-LTO profile, deterministic).
2. Warm contamination → n/a: PMU mode is cold per-parse; ins/B deterministic floor used.
3. Cherry-picked window → REFUTED: 2 fresh windows, both directional on ins/B.
4. Hidden x86 / name-gate / stub tokens → REFUTED: none present.
5. Patch-vs-report mismatch → REFUTED: worktree diff == patch.diff byte-for-byte.
6. Regen drift → REFUTED: generated.rs regenerates bit-for-bit.
7. Fabricated disasm → REFUTED: mask immediate + absent slice loads confirmed by otool.

Residual caveat (non-fatal): cyc/B magnitudes are load-sensitive and mine diverge from
the report on tailwind (0.825 vs 0.954) — but in the claim's favor and inside cyc/B's
acknowledged noise; the certified ins/B floor is the reproduced invariant.
