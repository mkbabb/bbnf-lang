# LOCK-8 · pass-1 · PROTO-2 — Tightened type plane (family 8B; guards 8D non-regression R-8D3)

**Plane:** THE TS BACKBONE PLANE (parse-that/typescript)
**Probe:** precision uplift + instantiation/TS2589 budget + zero-cost emit on value.js's REAL grammar
**Verdict line:** **RAN_MEASURED — 8B PASS · R-8D3 HOLDS.** The tightened plane infers strictly more precisely (literal-headed sequence tuples, literal alternation unions), compiles value.js's real parsing subgraph with **0 new TS2589** and a **byte-identical error set**, at **1.042× instantiations / ~1.0× check-time** (both ≤1.5×), the `.test-d.ts` is **GREEN on candidate / RED on 1.0.0**, and emitted JS is **byte-identical (SHA match)**. No consumer site rejected.

---

## Question

Does the 8B tightened type plane (a) infer strictly more precisely, (b) compile value.js's real grammar within a TS2589 / instantiation budget (≤1.5× baseline), (c) turn a `.test-d.ts` GREEN that is RED on 1.0.0, AND (d) leave emitted JS byte-identical? A plane that blows the checker (TS2589) or rejects a real consumer site is a failed 8B and a broken R-8D3.

## Method

Scratch copies (parse-that is READ-ONLY): `pt-scratch/` = parse-that/typescript @ HEAD (v1.0.0) src, node_modules symlinked; `vjs-scratch/` = value.js 4.0.0 (4.0.0) src + tsconfigs, node_modules symlinked except a **real, swappable** `@mkbabb/parse-that/dist` so baseline-1.0.0 vs candidate `.d.ts` can be exchanged in place.

- **Baseline (1.0.0)** = value.js consuming the pristine published 1.0.0 `.d.ts`.
- **Candidate (tightened)** = same, consuming the patched `.d.ts`.
- **Probe surface** = `src/parsing/**` (18 `.ts` files; tsc pulls the transitive graph → 259/260 files). This is value.js's entire parse-that consumer surface: **61 `all()` sites** + the `dispatch()` CSS-function-name LUT (`parsing/index.ts` `Function_`, `parsing/color/color.ts` `Value`).
- Metrics: `tsc --extendedDiagnostics` (Instantiations, Check time — 2 cold runs each), `error TS` set diff, `TS2589` count. Emit: `esbuild` transpile of `leaf.ts` both trees, SHA compare. `loadavg ~8–12` recorded at each run (machine under load; check-time is noise-dominated, instantiation count is exact/stable).

### The tightening (patch in `PROTO-2.patch.md`)

- **NEW `src/parse/types.ts`** — `ValueOf`, `Values<Ps>` (variadic mapped tuple), `Union<Ps>`, `DropUndefined` (tail-recursive accumulator). Pure `type` module → **0 emitted bytes**.
- `all` → `Parser<Values<T>>`, `any` → `Parser<Union<T>>`, both with a `const` type param, routed through the shared helpers (precision defined once).
- `string<const S extends string>(str: S): Parser<S>` — the genuine new precision: `string("a")` is now `Parser<"a">`, which is what lifts `all(...)` from `Parser<[string, string]>` to `Parser<["a", string]>`. **Runtime bodies untouched** (the `as Parser<S>` cast erases).

## What ran, and the numbers

| Metric (value.js `src/parsing/**`) | Baseline 1.0.0 | Candidate | Ratio |
|---|---|---|---|
| Files | 259 | 260 (+`types.d.ts`) | — |
| Types | 16195 | 16412 | 1.013× |
| **Instantiations** | **24524** | **25546** | **1.042×** ✅ ≤1.5× |
| Check time (cold, loadavg~12) | 0.41–0.49s | 0.30–0.35s | ~1.0× (noise) ✅ ≤1.5× |
| **TS2589** | **0** | **0** | ✅ none new |
| `error TS` count | 21 | 21 | **error set BYTE-IDENTICAL** |

The 21 errors are pre-existing and environmental — `TS2339` `import.meta.env` (missing `vite/client` types) in `units/color/spaces.ts` and `TS2345` `number[][]` tuples in `easing.ts`. **None are at a parse-that consumer site; none are `TS2589`; the set is identical baseline↔candidate** (`comm` empty both directions). → criterion (a) PASS: every real consumer site is clean, no new TS2589, **no consumer rejected (R-8D3 holds)**.

**`.test-d.ts`** (`vjs-scratch/test-types/inference.test-d.ts`, imported as the published surface exactly like value.js):
- `all(string("a"), regex(/\d/))` ≡ `Parser<["a", string]>` — candidate GREEN.
- `any(string("a"), string("b"))` ≡ `Parser<"a" | "b">` — candidate GREEN.
- `string("px")` ≡ `Parser<"px">` — candidate GREEN.
- wrong-shape `all("not a parser", …)` STILL errors under both (`@ts-expect-error` satisfied).
- **Candidate: exit 0 (GREEN).** **Baseline 1.0.0: 3× `TS2344 'false' does not satisfy 'true'`** on the three equality assertions (the positive-control RED — proves the assertions discriminate precision, not vacuous). → criterion (c) PASS.

**Zero-cost emit:** `esbuild` transpile of `leaf.ts`, original vs tightened → `leaf.js` **SHA `89022179…` identical**, 7854 bytes both. `types.ts` → **0 emitted bytes**. value.js src is byte-identical across arms and `.d.ts` never emits, so its `dist/` is trivially identical. → criterion (d) PASS.

**Secondary corpus — latex-paper's 7 grammar modules** (`src/grammar/{primitives,text,math,commands,environments,document,compile}.ts`): overlaying candidate vs baseline-1.0.0 dist → **0 new errors, 0 TS2589** (4 errors present in *both* arms are 0.7.1→1.0.0 API-drift artifacts of the pin, orthogonal to the tightening). Corroborates R-8D3 non-regression. *(Caveat: latex-paper pins parse-that `^0.7.1`; the overlay is 1.0.0-family, so this is a bounded non-regression check, not a native compile.)*

## Finding (design constraint discovered)

`dispatch<const K extends string, T>(table: Record<K, Parser<T>>)` is **infeasible without touching the body**: with a generic `const K`, `Object.entries(table)` collapses the value to `unknown` (the `Record<K,…>` has no string index signature), producing `TS2345` inside `dispatch`'s own loop. Since the value-return type is `Parser<T>` either way, the key-literal precision buys the consumer nothing, so `dispatch` was reverted to `Record<string, Parser<T>>` — keeping the "runtime bodies untouched" invariant intact. The precision uplift is carried entirely by `string`'s `const S` feeding the already-tuple `all`/already-union `any`.

## Verdict

**RAN_MEASURED. 8B PASS. R-8D3 HOLDS.** (a) 0 new TS2589, error set byte-identical, no consumer rejected; (b) 1.042× instantiations, ~1.0× check-time (both ≤1.5×); (c) `.test-d.ts` GREEN on candidate / RED on 1.0.0; (d) emitted JS byte-identical. The family's signature falsifier — "type-level totality that blows the checker or rejects a real consumer" — did **not** fire.

*Files: `pt-scratch/src/parse/{types.ts,leaf.ts}`, `vjs-scratch/{tsconfig-probe.json,tsconfig-testd.json,test-types/inference.test-d.ts}`; measurement logs `vjs-scratch/{baseline,candidate}-tsc.out`, traces `.trace-base/`, `.trace-cand/`. Full patch in `PROTO-2.patch.md`.*
