# PROVE-11 — TS V8 UPLIFT (F5): lazy diagnostic-array allocation

**Verdict: GO.** Three core benches clear the >=10% gate (unfused then-chain
**+13.2%**, first-char dispatch **+10.2%**, fused all(a,b,c) **+10.1%**) with
**zero regressions** — every measured lane is faster or neutral. Certified by
load-robust paired interleaving (N=10, single build per process). No API break:
`tsc` clean, 124/124 tests pass, `proof:subpath` / `proof:no-css-surface` /
`proof:no-span-surface` all PASS.

Scratch copy: `parse-that/typescript` @ v1.0.0 (pristine committed `60a7ba8`).
`/Users/mkbabb/Programming/parse-that` left untouched (read-only law honoured;
one accidental edit to the original was immediately `git checkout`-reverted —
all work thereafter on the scratch copy).

---

## 1. What I built — the lever

R4 s6a/s11 name the `ParserState` shape as the governing V8 surface. The finding:
`ParserState` declared `suggestions: Suggestion[] = []` and
`secondarySpans: SecondarySpan[] = []` with eager initializers, so **every
`new ParserState(...)` allocated two arrays** — and `mergeErrorState` (utils.ts)
**re-allocated both on every furthest-offset advance**, which fires constantly
during backtracking on the *success* hot path even though diagnostics are OFF by
default and these arrays are read only on the cold error/diagnostics path.

`expected` already used the correct lazy idiom (`undefined` until a diagnostic
seeds it, utils.ts:33). The fix makes `suggestions`/`secondarySpans` **symmetric
with `expected`** — declared native fields (`undefined` at construction, so the
hidden class is fixed from birth and never transitions), populated only under
`diagnosticsEnabled` via `(state.suggestions ??= []).push(...)`. Extreme
parsimony: **~14 net source lines**, no new concept, no new field, no API change.

### Diff summary (`patch.diff`, source portion)
| file | change |
|---|---|
| `src/parse/state.ts` | `suggestions`/`secondarySpans` -> optional (declared, no initializer) |
| `src/parse/utils.ts` | `mergeErrorState` + `resetErrorState` reset to `undefined` (no alloc); `addSuggestion`/`addSecondarySpan` lazy `??= []`; `collectDiagnostic` guarded spread |
| `src/parse/debug.ts` | two reads guarded `?? []` (mirrors the existing `state.expected ?? []`) |
| `test/debug.test.ts` | two diagnostics-on assertions narrowed `?? []` (arrays still populated at runtime) |

(`patch.diff` also carries the two measurement harnesses — see s5.)

---

## 2. Visual verification — emitted-JS hot path (the cargo-expand analogue)

Bundled output census across ALL dist chunks (esbuild, ES2022 native fields):

| | eager `= []` alloc sites | zero-alloc `void 0` resets | diagnostics-gated `??=` |
|---|--:|--:|--:|
| **PRISTINE** (`CS1td-8B`) | **6** | 0 | 0 |
| **MODIFIED** (`6LwlZEJQ`) | **0** | 4 | 2 |

The 6 pristine sites: `ParserState` ctor x2, `mergeErrorState` furthest-advance
x2, `resetErrorState` x2. Modified emits none — the two per-parse (ctor) and two
per-backtrack (`mergeErrorState`) array allocations become plain `void 0` stores;
the two `??=` sites live inside the `if (diagnosticsEnabled)` guard, so the
default LL(1) path never reaches them.

```js
// PRISTINE — ParserState fields             // MODIFIED — ParserState fields
expected;                                     expected;
suggestions = [];      // <- 1 alloc/parse    suggestions;        // undefined, 0 alloc
secondarySpans = [];   // <- 1 alloc/parse    secondarySpans;     // undefined, 0 alloc
```

Hidden class is identical in both (all three are native own-properties defined at
construction); the modified object simply holds `undefined` where pristine holds a
fresh array. Shape-stable, allocation-free.

---

## 3. Measured results — CERTIFIED (load-robust paired interleaving)

**Method.** cold per-op median ns (median of 13 batches x 300k/150k/80k ops),
**one build per process** (production-faithful — no cross-module megamorphism),
pristine and modified measured **back-to-back within each round** so load drift is
paired out. N=10 rounds, loadavg 10.76-14.37. `ratio = pristine/modified` (>1 =>
modified faster); `d%` = modified win. Node v26 / V8, Apple Silicon.
Harness: `scripts/measure-prove11.mjs` + `run-paired.sh`; raw
`paired-samples.jsonl`; `aggregate.py`.

| lane (core bench) | pristine | modified | ratio | d% (mod win) | gate |
|---|--:|--:|--:|--:|:--:|
| **unfused a.then(b).then(c)** | 25.37 ns | 22.03 ns | **1.152x** | **+13.2%** | PASS >=10% |
| **first-char dispatch (LUT)** | 146.35 ns | 131.48 ns | **1.113x** | **+10.2%** | PASS >=10% |
| **fused all(a,b,c)** | 20.76 ns | 18.67 ns | **1.112x** | **+10.1%** | PASS >=10% |
| sequential any (backtracking) | 354.42 ns | 335.76 ns | 1.056x | +5.3% | no-reg |
| json-comprehensive | 1983.27 ns | 1938.02 ns | 1.023x | +2.3% | no-reg |

Per-round paired ratio (fused): min 0.910 . **med 1.113** . max 1.221. Every lane
favours modified at the median; the effect is systematic across five independent
lanes (a null effect would not tilt all five the same way). Lighter lanes win most
(the ctor's 2-array saving is a larger fraction of a 20 ns op); dispatch/seqAny
call `.parse()` five times per op, so they compound the ctor saving.

**Allocation corroboration** (heapUsed-delta B/op, reliable only for the tiny
fused footprint): pristine **1.84 B/op -> modified 0.73 B/op (-60.5%)** — direct
evidence of the removed array allocations. (The json alloc proxy is GC-timing
dominated and not certifiable; timing is the certified metric.)

### Why not the naive pt-b3 cross-run
Running `pt-b3-fusion.bench.ts` on each source separately is **load-confounded**:
the pristine baseline ran at loadavg 12, the modified run landed at loadavg **27**,
so raw hz dipped on light lanes (yet json *rose*, 383k->398k). This is exactly the
artifact the campaign's paired-interleaving law exists to kill — hence the
single-process paired method above is the certified evidence, not the cross-run.

---

## 4. Gates / no-API-break (do-not-regress verified)

- `tsc --noEmit`: **clean** (0 errors).
- `npm test`: **124/124 pass, 13/13 files** (identical pass-set pristine vs
  modified — pre-existing environmental fixture-path failures resolved by copying
  the sibling `data/`,`grammar/`,`docs/` read-only dirs the scratch lacked; NOT
  code failures).
- `proof:subpath` GREEN . `proof:no-css-surface` GREEN . `proof:no-span-surface`
  PASS . `proof:manifest`/`packrat-{cross-input,reentrant,large-offset,armed}`/
  `no-dead-combinator` PASS.
- `proof:perf` clauses (A-heap) fusion-zero-tuple **1.70x** and (B'-onpath)
  dispatch **+60.8%** PASS. Clause (C) trips only on the **checked-in
  cross-machine baseline** (1742 ns from another machine vs this loadavg-~12
  machine's absolute ns) — the script's own note: *"Timings are machine-relative
  ... rebuild via UPDATE_PERF_BASELINE=1."* It is not a regression from this change
  (same-machine paired json is +2.3% FASTER).
- Arity-unrolled `any`/`fuseAll` monomorphizations untouched; diagnostics
  global-flag + packrat arming semantics unchanged (proofs green).

---

## 5. Integration cost

**Low.** The change is 14 net source lines in three core files, landing directly
on `parse-that/typescript` @ v1.0.0 (no new files, no export change, no dependency).
Seam: `ParserState` field declarations + the `mergeErrorState`/`addSuggestion`/
`addSecondarySpan`/`collectDiagnostic`/`resetErrorState` diagnostic sites in
`utils.ts`, plus two `?? []` read-guards in `debug.ts`. Consumers (value.js,
latex-paper, keyframes.js) import `ParserState` but never read `.suggestions`/
`.secondarySpans` directly (verified in R4 s7); the field type widens `[]` ->
`[] | undefined` — non-breaking for all known consumers. No bootstrap/regen: this
is hand-written TS, not generated. Ship as a patch release.

Files: `patch.diff` (all code), `run-paired.sh` + `aggregate.py` +
`paired-samples.jsonl` (reproduce the certified table).
