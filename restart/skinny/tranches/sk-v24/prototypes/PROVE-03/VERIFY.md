# PROVE-03 — ADVERSARIAL VERIFICATION

**Verdict: CONFIRMED.** Target: fused single-pass trusted string scan.
Host Apple Silicon aarch64. Base HEAD `d2b32f6e3`. Prover worktree
`wf_386900ed-462-3` present with built artifacts; BASE arm rebuilt by
stashing patch.diff (`git stash` → `cargo build --release` → restore).
Both arms `[profile.release]` opt-level 3 / fat-LTO / codegen-units 1 —
symmetric profile, no asymmetry. Verifier ~35 min.

## Method

- ARM binary = prover's `target/release/json_canon_bench` (its ins/B matched
  the report exactly → confirmed patched artifact). BASE = same source with
  patch stashed, rebuilt `--release`. Binaries differ (726608 vs 710080 B).
- Re-ran `json_canon_bench pmu 400` interleaved BASE↔ARM back-to-back, 6
  windows, **both orders** (to neutralize the "ARM in worse slot" claim).
- Re-ran `paired 300` sonic-anchored (sonic = identical code both binaries),
  3 interleaved windows.
- Re-ran the 12 parity tests.

## Fresh numbers — twitter (631,515 B)

### Deterministic ins/B (load-independent) — N=200 iters

| lane | BASE | ARM | ratio | report BASE→ARM |
|------|-----:|----:|------:|-----------------|
| parse_only | 6.233 | 5.076 | 0.814× | 6.241→5.089 |
| parse_tape | 9.966 | 9.055 | **0.909×** | 9.970→9.065 (0.909×) |
| fused | 12.378 | 10.912 | **0.882×** | 12.393→10.920 (0.881×) |

Matches report within 0.1%. Instruction reduction is real and load-free —
the patch genuinely does less work on the trusted string path.

### Interleaved PMU cyc/B — 6 windows × 400 iters, loadavg 21–28

ARM/BASE per window (BASE-first / ARM-first alternated):

- parse_tape: 0.939, 0.831, 0.821, 0.916, 0.926, 0.881 → **median 0.898×**
- fused: 0.936, 0.801, 0.794, 0.875, 0.881, 0.814 → **median 0.845×**
- parse_only: all <1 (0.90–0.95), mean ~0.93×

**ARM won on both lanes in 6/6 windows regardless of interleave slot.** Report
claimed 0.931× (tape) / 0.843× (fused): fused near-exact; parse_tape win is
*larger* than claimed here (lower ratio). My load was higher than the report's.

### Paired sonic-anchored — 3 windows × N=300, loadavg 15–16

| ratio | BASE | ARM | relative | CIs |
|-------|------|-----|---------:|-----|
| tape/sonic | 0.503, 0.488, 0.524 | 0.444, 0.421, 0.448 | ~0.866× | non-overlap all 3 |
| fused/sonic | 0.649, 0.631, 0.667 | 0.549, 0.549, 0.537 | ~0.841× | non-overlap all 3 |

Report: tape ~0.87×, fused ~0.865× — reproduced. Every window CI non-overlapping.

## Parity — 12/12 pass, zero failures

- `checkasm_string_terminal_block`: 5 passed (incl. 200k-block NEON==scalar fuzz).
- `prove03_string_kernels`: 7 passed (incl. 20k-body kernel==reference fuzz,
  multibyte / surrogate / malformed / invalid-UTF-8).

## Edict audit — clean

- **aarch64-only**: both new sites `#[cfg(target_arch = "aarch64")]`. No x86
  intrinsics; sole `movemask`/`x86` token is a doc comment describing what the
  `vshrn` narrow *avoids*. SWAR-8 fallback is pre-existing scalar.
- **no name-gates**: zero corpus/grammar-name conditioning in the diff.
- **parsimony**: drops `memchr` dep, deletes `first_control_byte` +
  `control_byte_mask`; net small (+62 kernel LOC, lib.rs net −1). Deletes more
  than it adds on parse-that side.
- **no stubs**: kernel fully implemented; no todo/unimplemented/fallback.

## Attack list & outcome

1. Opt-profile asymmetry between arms → **refuted**: both `--release` fat-LTO.
2. Warm contamination → **refuted**: harness touches once then N cold per-parse;
   PMU/paired both cold.
3. "ARM ran in worse slot" cherry-pick → **neutralized**: interleaved both
   orders; ARM wins in every slot.
4. Load confound → **controlled**: paired sonic anchor (invariant code) +
   back-to-back interleave; deterministic ins/B is load-free and matches.
5. Hidden x86 / name-gate / stub → **none found**.
6. Parity gap → **refuted**: 12/12 incl. large fuzz; bit-identical by construction.

## Rationale

Direction reproduces on every metric and every window; magnitude matches
(fused, paired) or exceeds (parse_tape cyc/B) the claim; the load-independent
ins/B ratios match the report to 0.1%; parity is clean; no edict violation. The
central claim (fused string scan yields a measurable twitter cyc/B win on
parse_tape and fused) stands under attack. **CONFIRMED.**
