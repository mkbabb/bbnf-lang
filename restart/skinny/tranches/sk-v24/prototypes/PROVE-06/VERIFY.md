# PROVE-06 — ADVERSARIAL VERIFY

**Verdict: CONFIRMED.** Skinny stop-early `get` beats sonic-rs `get_from_str` on
2/3 path classes (shallow + missing win, deep loses) — direction reproduces on both
fresh windows, byte-exact parity holds on all 4 paths, stop-early resolves at 0.032 %
of the doc, no edict violations. Magnitudes within ~5 % of the report medians.

Target base `7f3be27c2` (recon HEAD, matches worktree HEAD). Binary mtime newer than
all sources; `cargo build --release` was a 0.28 s no-op → binary matches patched source.
arch = arm64/aarch64. Fixture twitter.json = 631515 B (matches report exactly).

## Fresh numbers — paired per-sample wall ratio (mine/sonic; <1 ⇒ skinny faster)

| class | W1 N=200 la≈8.0 | W2 N=300 la≈8.0 | claimed median | direction |
|---|--:|--:|--:|:--:|
| shallow `statuses[0]` | 0.793 ±0.014 | 0.781 ±0.011 | 0.755 | **WIN** |
| deep `..user.screen_name` | 2.014 ±0.024 | 2.025 ±0.027 | 2.108 | loss |
| missing `..nonexistent` | 0.681 ±0.008 | 0.675 ±0.008 | 0.688 | **WIN** |

## PMU cyc/get corroboration (HW counter, iters=20000)

| class | W1 ratio | W2 ratio | vs get_unchecked (W1/W2) |
|---|--:|--:|--:|
| shallow | 0.752 | 0.810 | 0.735 / 0.812 |
| deep | 1.964 | 2.021 | 3.055 / 3.290 |
| missing | 0.695 | 0.683 | 1.055 / 1.045 |

PMU direction matches paired on every class, both windows. (Against `get_unchecked`,
missing narrows to a tie — the report discloses this; the gate targets checked
`get_from_str`.)

## Parity oracle (re-run, both windows)
- shallow object → `{\n "metadata"...` byte-identical
- deep string → `"ayuu0123"` byte-identical (quotes included on both sides)
- missing → `None` == `None`
- prefix number → `505874924095815681` byte-identical
- gate `assert!(ok)` passed (no panic) both windows; tape_present agrees.

## Stop-early proof
Prefix `statuses[0].id` resolves at byte **204 / 631515 = 0.032 %** — exact, both windows.

## Attack list
1. **Run-2 tie outlier (the flagged risk).** Report's shallow run-2 = 1.008 at loadavg 16.
   At loadavg ~8 both my windows give a clean win (0.781/0.793) with 95 % CI upper bounds
   0.792/0.807 — never crossing 1.0. The tie is load-induced (shallow's ~1.28× margin is
   the thinnest of the three), not a false win. Honest disclosure by prover; not a refutation.
   **CAVEAT: shallow is the load-fragile lane — under heavy load it degrades to a tie.**
2. **Opt-profile asymmetry.** Both arms run in the same `--release` binary, interleaved per
   sample (load hits both sides of every ratio). No asymmetry.
3. **Warm contamination.** One out-of-band XOR fold pages in the source for BOTH sides before
   timing; each `get`/`get_from_str` re-walks from scratch (no memoized tape). Symmetric.
4. **Parity-vs-timing mismatch.** Parity uses `get_unchecked`, timing uses `get_from_str`;
   both return identical values for these 4 valid paths on valid JSON. Meaningful.
5. **x86 / hidden tokens.** No x86/avx/sse/target_feature in impl; skip engine is
   `#[cfg(target_arch="aarch64")]` NEON with a scalar `skip_tail` portable fallback. aarch64-clean.
6. **Name-gates.** `get` is fully generic over `Seg`; zero branching on grammar/fixture/path
   name. None found.
7. **Stubs/workarounds.** `skip_value` string `.unwrap_or(at+1)` is defensive (never fires on
   valid JSON); `block_emit_mask` is a DRY factor-out of the proven scan kernel (shared, not
   duplicated); scalar fallback is legitimate. No stubs on the hot path.
8. **Patch-vs-report.** Numbers reproduce within ~5 % (missing/deep ~4 %; shallow ~4 % weaker,
   still <1). All fall inside the report's disclosed per-run spread.

## Magnitude note
Shallow measured ~0.79 vs claimed 0.755 (win holds, ~4 % weaker); deep ~2.02 vs 2.108
(loss holds, less severe); missing ~0.68 vs 0.688 (near-exact). Within report's own
per-run ranges → CONFIRMED, not DELTA.
