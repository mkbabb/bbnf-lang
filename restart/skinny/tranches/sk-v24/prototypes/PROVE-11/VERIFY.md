# PROVE-11 — ADVERSARIAL VERIFICATION

**Verdict: CONFIRMED_DELTA.** The lever is real and the mechanism reproduces
exactly (emitted-JS eager-alloc census 6→0; fused alloc −60% dead-on). The two
light-op headline lanes reproduce at or ABOVE the claimed magnitude
(fused/unfused both clear the ≥10% gate). The **first-char dispatch lane does
NOT reproduce its claimed magnitude** — I measure **~+5%**, not the reported
**+10.2%** (task-sheet **+11.3%**), across 24 paired rounds in two independent
load windows, so dispatch **misses the ≥10% gate**. Parity is clean (124/124
both arms), no edict violations, no unguarded consumer of the widened field.

Reconstructed a FRESH scratch copy per R4 (pristine `parse-that/typescript`
v1.0.0 @ upstream HEAD `ef10d5b`, node modules symlinked). Baseline built on
pristine FIRST, then patched, interleaved. `patch.diff` applied CLEANLY to the
pristine copy (`patch -p1`, exit 0, all 6 hunks) — portability confirmed.

---

## 1. Fresh paired numbers — single build per process, baseline-first interleaved

cold per-op median ns (median of 13 batches × 300k/150k/80k ops), pristine and
modified back-to-back within each round. **N=24 rounds** across **two windows**
(W1 loadavg 13–17, W2 loadavg 10–12 — W2 matches the report's stated conditions).
`ratio = pristine/modified` (>1 = modified faster). Two statistics reported: the
aggregate median-of-medians (report's method) and the **load-immune per-round
paired median ratio** (correct paired statistic).

| lane | pri(med) ns | mod(med) ns | aggregate ratio | mod win% | per-round med ratio | claim (report / task) | reproduces? |
|---|--:|--:|--:|--:|--:|--:|:--:|
| **unfused then-chain** | 24.00 | 20.73 | **1.158×** | **+13.6%** | 1.188 (+15.8%) | +13.2% / +13.2% | **YES ≥10%** |
| **fused all(a,b,c)** | 19.97 | 16.86 | **1.184×** | **+15.6%** | 1.181 (+15.3%) | +10.1% / +11.2% | **YES ≥10%** (exceeds) |
| **first-char dispatch** | 136.75 | 130.10 | 1.051× | **+4.9%** | 1.050 (+4.8%) | +10.2% / +11.3% | **DELTA — misses gate** |
| sequential any (no-reg) | 334.44 | 329.85 | 1.014× | +1.4% | 1.049 (+4.7%) | +5.3% | no regression |
| json-comprehensive (no-reg) | 1846.08 | 1846.19 | 1.000× | −0.0% | 1.028 (+2.7%) | +2.3% | no regression |
| **fused alloc B/op** | 1.84 | 0.74 | **2.504×** | **−60.1%** | 2.496 | −60.5% | **YES — exact** |
| json alloc B/op | 8.40 | 5.04 | 1.667× | −40% | noisy (min .08) | GC-timing noise | not certifiable |

Per-window cross-check (both windows agree on the pattern):
- **W1** (load 13–17): fused +20.9%, unfused +19.9%, dispatch +2.6%.
- **W2** (load 10–12, ≈ report conditions): fused +14.9%, unfused +12.1%, dispatch **+5.6%**.

The dispatch shortfall is **not** a load artifact — at matched load (W2) it is
still ~half the claimed figure. The dispatch per-round ratio is a wide/noisy band
(min 0.695, max 1.420); the report's +10.2% sat near the top of that band and
does not hold up over 24 rounds.

## 2. Mechanism — confirmed by fresh census + alloc rate

Emitted-JS eager-alloc census across all dist chunks (fresh `vite build`, both arms):

| | eager `= []` | `void 0` resets | `??= []` (gated) |
|---|--:|--:|--:|
| **dist-pristine** | **6** | 0 | 0 |
| **dist-modified** | **0** | 4 | 2 |

Reproduces the report's census exactly. The −60.1% fused alloc-B/op corroborates
that 2 real array allocations per `ParserState` are removed — the causal claim is
sound.

## 3. Parity oracles (fresh)

- `npm test` — **124/124 pass, 13/13 files on BOTH pristine and modified**
  (identical pass-set). Parity: **PASS**.
- `tsc --noEmit` — **core source (state/utils/debug.ts) CLEAN** (pristine tree
  compiles 0-error). The only 2 tsc errors are TS7016 in the *added* harness
  `test/benchmarks/pt-prove11-paired.bench.ts`, which **hardcodes the prover's
  absolute scratch paths** `…/PROVE-11-work/dist-{pristine,modified}/parse.js`.
  Non-shipping measurement file; a **patch-portability wart**, not a core defect.
  Report's "tsc clean" is true only for the shipped change.
- Proof gates: `manifest`, `no-css-surface`, `subpath`, `packrat-{cross-input,
  reentrant,large-offset,armed}`, `no-dead-combinator`, `no-span-surface` all
  **PASS**. (`no-span-surface` first FAILed on a missing `../docs/` fixture in the
  scratch — environmental; PASS once `docs` symlinked. Unrelated to the patch.)
- `proof:perf` clause C trips on BOTH arms: pristine json 2574ns, modified 2439ns
  vs a checked-in **cross-machine** 1742ns baseline — a machine-relative absolute-ns
  gate, not a patch regression. Modified was FASTER than pristine here.

## 4. Consumer / shape-stability audit (the widen-to-undefined attack)

Grepped every read of `.suggestions`/`.secondarySpans` in modified src+test and
in all three consumers (value.js, latex-paper, keyframes.js): **zero unguarded
reads.**
- `debug.ts:180,185` (`statePrint` on a `ParserState`) — guarded `?? []`.
- `debug.ts:216–221` reads a **`Diagnostic`** (always arrays from
  `collectDiagnostic`) — safe by construction.
- `parser.ts:63–64` (**not touched by the patch**) copies `state.suggestions` →
  `errorState.suggestions`; now may pass `undefined`, but the sink is only
  `toString()`→`statePrint` (guarded) and the public `parser.state`. Safe.
- Consumers import `ParserState` but never read these fields (R4 §7 confirmed).

Hidden class stable: all three fields are native own-properties declared at
construction (`undefined` at birth), never transition. Correct.

## 5. Edict audit

- **aarch64-only**: no `x86`/`target_arch`/SSE/AVX tokens — pure TS. CLEAN.
- **no name-gates**: no grammar-name conditionals. CLEAN.
- **parsimony**: source net +19/−12; 7 of the +19 are doc-comment; ~12–14
  functional lines as claimed. CLEAN.
- **no stubs/workarounds**: genuine lazy-init refactor making
  `suggestions`/`secondarySpans` symmetric with the pre-existing lazy `expected`.
  No TODO/FIXME/stub/shim. CLEAN.

## Attack list & outcome
1. Opt-profile asymmetry between arms → both built with identical `vite.config`
   (minify:false), same command. No asymmetry.
2. Warm contamination → cold per-op, single build per process. Clean.
3. Load confound → 2 windows, load recorded per round; per-round paired ratio.
   Pattern holds in both windows. Cannot explain dispatch shortfall.
4. Cherry-picked window → dispatch reproduces LOW in both windows (2.6% / 5.6%).
5. Hidden x86 / name-gate / stub → none found.
6. Patch-vs-report mismatch → **found two**: (a) task-sheet inflates dispatch
   (+11.3) and fused (+11.2) vs the report's own table (+10.2 / +10.1);
   (b) dispatch does not reproduce at ≥10% at all. Also: "tsc clean" holds only
   for core, not the added hardcoded-path bench.
7. Unguarded consumer of widened field → none (§4).

## Verdict rationale
Direction reproduces on every lane; the removed-allocation mechanism is proven
exactly (census 6→0, −60% B/op). Fused and unfused clear ≥10% at or above the
claim. **But the report's "three core benches clear ≥10%" is not reproducible:
the dispatch lane lands at ~+5%, materially below the claimed +10.2%/+11.3% and
below the gate.** Parity clean, no edict violations. → **CONFIRMED_DELTA**
(2 of 3 headline lanes CONFIRMED and the alloc mechanism exact; dispatch
magnitude halved).
