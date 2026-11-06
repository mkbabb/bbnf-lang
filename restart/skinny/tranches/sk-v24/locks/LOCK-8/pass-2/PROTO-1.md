# PROTO-1 — LOCK-8 pass-2, family 8B, probe t1

**Family 8B — Type-Level Totality.** The full typed algebra (removing the enumerated
`Parser<any>` INPUT leaks, PT-E2 / 8B-G3) + `DropUndefined` wired into `all` (8B-G1'/8B-G2),
measured for the instantiation budget and precision on parse-that's own real self-use +
a synthetic corpus reproducing latex-paper's dominant type shapes.

---

## Question

Does wiring the spec's full typed algebra — `then→[T,S]` / `or→T|S` / `chain→S` /
`opt→T|undefined` with the unsound `S|T` INPUT dropped, `all→Parser<DropUndefined<Values<Ps>>>`,
`any→AnyValue`, and the two duplicate `ExtractValue` defs collapsed into `src/parse/types.ts` —
(1) stay within the TS2589 / instantiation budget, (2) type-precisely (both directions), and
(3) leave parse-that's own combinator surface non-broken? And does the runtime `all()`
drop-undefined match the `DropUndefined` type as a *documented widening*?

## Method (cold, deterministic; checker plane)

- **Scratch copy** per PROVE-11 / R4 §8: `pt-scratch-t1/` = a copy of
  `parse-that/typescript/src` + a controlled `tsconfig.probe.json` (include `src/parse` +
  `src/probe` only, `noEmit`, else identical strict/bundler config), `node_modules` symlinked.
  **parse-that/typescript itself untouched (READ-ONLY).**
- **Corpus** = parse-that's real self-use (`src/parse/parsers/json.ts`, `csv.ts`, whole
  combinator core) + `src/probe/stress.test-d.ts` reproducing latex's DOMINANT shapes
  (R4 §7): a **12-deep `.then()` chain** (latex `commands.ts` reaches 12), **wide `all()`
  sites with `opt()` members** (the DropUndefined crux), **`Parser.lazy()` mutually-recursive
  grammar** at the `Parser<Node>` seam, plus `.or/.chain/.opt/.then` precision assertions
  (`Expect<Equal<…>>` both ways + two `@ts-expect-error` negatives).
- **Instrument** = `tsc 5.9.3 --extendedDiagnostics` (node 26). Instantiation/Types counts
  are **checker-deterministic** — independent of loadavg. Check-time (ms) is directional
  only; loadavg disclosed per run.
- **Baseline** = same corpus against the *current leaky* signatures. **Tightened** = after the
  diff (`PROTO-1.diff`). Same tsc, same corpus.
- **Runtime differential** (gate 8B-#5): esbuild-bundle `src/probe/runtime-diff.ts` and run
  under node to observe `all(a, opt(b), c)` on present vs absent `b`.

## What ran

Three `tsc --extendedDiagnostics` invocations (baseline / tightened-clean / tightened+negatives)
and one node runtime differential. loadavg was **high and volatile throughout** (8→44, see per-run
lines); the deterministic instantiation counts are the load-bearing figures, check-time is noted
directional-only.

## The numbers

| corpus | signatures | Instantiations | Types | TS2589 | errors | check time |
|---|---|---:|---:|---:|---:|---:|
| self-use + stress | **BASELINE** (leaky `S\|T`, no DropUndefined) | **4280** | 3052 | 0 | 1 *(intended RED: DropUndefined absent)* | 0.24s @ load 44 |
| self-use + stress | **TIGHTENED** (full algebra) | **4404** | 3034 | 0 | **0** (all precision GREEN) | 0.18s @ load 15 |
| + 2 `@ts-expect-error` negatives | TIGHTENED | 4482 | 3054 | 0 | **0** (wrong shapes correctly rejected — no unused-directive) | 0.19s @ load 14 |

- **Budget delta (apples-to-apples, same corpus):** `4280 → 4404` = **+124 instantiations
  (+2.9%)**, Types `3052 → 3034` (−18). **Zero TS2589 introduced.** No instantiation
  explosion; the tightened algebra is comfortably in budget on this corpus.
- **Precision RED→GREEN:** baseline is RED exactly at `_t_wideopt_TIGHTENED` (line 55) —
  `all(a, opt(b), c)` yields `[…, number|undefined, …]` without DropUndefined. Tightened is
  GREEN there and everywhere; the two `@ts-expect-error` negatives fire (a genuinely wrong
  `string`/`[number,string]` shape is still rejected — precision **both ways**).
- **Non-breaking:** the whole `src/parse` core (parser/leaf/packrat/debug) + `json.ts`/`csv.ts`
  self-use compiled with **0 errors** under the tightened signatures. Tightening
  `then/or/chain` inputs from `Parser<S|T>` to `Parser<S>` broke **no** real call site here;
  it *improves* inference (no union muddle: `ident.then(num)` infers `[string, number]` directly).
- **Runtime drop-undefined differential (gate #5):** `all(a, opt(b), c)` →
  `b` present `["a","b","c"]` len 3 (matches type `[string,string,string]`); `b` absent
  `["a","c"]` **len 2** (runtime shorter than the type-claimed 3). **Documented widening
  confirmed** — sound when opts present, a labeled widening when absent, shippable because no
  surviving consumer exercises the length-variable case (research-B §2).

## Erasures that lawfully stay (verified, NOT touched)

`ParserFunction<T>` (`parser.ts` — the invariant IC function-pointer, `any` required for
variance) and `MemoCell.value: unknown` (heterogeneous packrat memo). Per research-B §3.

## Scope honesty / what this probe did NOT run

- **latex-paper NATIVE (8B-G4' step-ii) was NOT run.** latex resolves parse-that **0.7.1**
  (predates the 1.0.0 API cut: `*Span`, old `chain`). Compiling latex against the tightened
  1.x-shape src conflates the 0.7.1→1.x **migration** delta (8D-owned, step-i) with the
  type-only tightening (step-ii). Isolating step-ii requires the migration first — out of a
  single parsimonious probe's budget, and it belongs to a follow-on (t2 / 8D coordination).
  This probe therefore measures the budget on **parse-that self-use + a latex-shape stress
  corpus**, NOT on latex native. The 12-deep-`.then` + lazy-recursion shapes are reproduced
  faithfully, but the *instantiation figure is on the synthetic corpus, not latex's 8 grammar
  files*. Disclosed per the binding rule (signature + tsc version + corpus stated on every figure).
- **`skip`/`sepBy` input leaks NOT tightened.** Found (`skip<S>(parser: Parser<T|S>)`,
  `sepBy<S>(sep: Parser<S|T>)`) but outside the named 4-combinator algebra; left for completeness
  in a follow-on.
- Emit-identity (`dist/` step-ii byte-empty) NOT run — it is 8B-G4', a separate gate on the
  actual vite `dist/` toolchain, not a checker measurement.

## Verdict

**RAN_MEASURED — PASS (scoped).** The full typed algebra + `DropUndefined` compiles GREEN with
**+2.9% instantiations / zero new TS2589** on parse-that self-use + a latex-shape stress corpus;
precision holds both ways; the tightening is non-breaking for parse-that's own surface; the
runtime drop-undefined matches the documented widening. **The remaining 8B-G4' figure — the
instantiation budget on latex-paper NATIVE (step-ii isolated from the 0.7.1→1.x migration) — is
un-run and is the sharpest residual**, because it requires the API migration first and would
otherwise conflate two deltas.

Artifacts: `PROTO-1.diff` (the tightening + new `types.ts`), `pt-scratch-t1/` (the runnable
scratch copy: `tsconfig.probe.json`, `src/probe/stress.test-d.ts`, `src/probe/runtime-diff.ts`).
