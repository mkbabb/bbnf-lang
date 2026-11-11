# PROTO-PB-5 — V8 shape-normalization microbench

Analysis instrument (never landed). Target repo: parse-that (READ-ONLY; all work on
scratch copies under the worktree scratchpad). Date: 2026-07-10.

## Question

Does the uniform 3-field `Parser.context` plus `mapState` retirement keep every
dispatch-combinator site monomorphic (`%HaveSameMap` true, no dictionary mode) while
holding or improving the json floor?

## What was actually under test (the two levers)

Read of the live source pins the exact divergence the normalization closes:

- `typescript/src/parse/parser.ts:31` — `constructor(public parser, public context:
  ParserContext = {})`. The default is an **empty-object literal** — hidden class with
  **0 fields**.
- `typescript/src/parse/state.ts:179-189` — `createParserContext` returns one object
  literal `{ name, parser, args }` from a **single** source site; every combinator
  (`then/or/map/skip/many/sepBy/dispatch/lazy/…`, all 20 call sites in `parser.ts` +
  6 in `leaf.ts` + `lazy.ts:40`) routes through it → **3 fields**, one shared map.

So a Parser built by any combinator carries a 3-field context; a Parser built by the
raw `new Parser(fn)` API path carries the 0-field `{}`. Two maps at any site that reads
`parser.context.name` — e.g. the `trim()` guard `parser.context?.name === "whitespace"`
(`parser.ts:488`).

`mapState` (`parser.ts:162-187`) is the sole combinator that builds `Object.create(state)`
— a ParserState-**prototyped** view that is not a ParserState, a latent polymorph for any
callback receiving both. Grep across `typescript/src` + `parsers/` shows **zero callers**
(only its own def + the `parserNames` string). Dead; retiring it removes the latent
deopt vector and 422 B of bundle.

Normalization applied to the scratch `norm` copy (decisive diff):

```
// parser.ts constructor
-        public context: ParserContext = {},
+        public context: ParserContext = createParserContext(
+            undefined as never,
+            undefined,
+        ),
// parser.ts  — mapState method (26 lines incl. Object.create(state)) DELETED
// state.ts   — "mapState" removed from parserNames[]
```

## Method

`cp -R typescript/src/parse` → scratch `base/` and `norm/`; edits above to `norm/` only.
esbuild-bundle each (`--bundle --format=esm --platform=node`) → `base.mjs` / `norm.mjs`
(norm 40171 B vs base 40593 B, the mapState delta). One `node --allow-natives-syntax`
driver imports **both** realms and runs, per copy: an 18-parser population spanning every
combinator plus a raw `new Parser(fn)`; `%HaveSameMap` over all Parser instances and over
all `.context` objects; `%HasFastProperties` (dictionary-mode audit); `%GetOptimizationStatus`
on `jsonParser.parser` after 2000 warm parses; and a cold paired-interleaved json bench
(83196-byte nested corpus, 400 reps × 12 windows, alternating order, 3 whole-run repeats).
node v26.0.0.

## Results

**Shape (`%HaveSameMap`, `%HasFastProperties`) — the core finding:**

| metric | BASE | NORM |
|---|---|---|
| all Parser instances same map | true | true |
| all context objects same map | **false** (idx 17 diverges) | **true** |
| raw-default vs combinator context same map | **false** | **true** |
| context key-counts observed | **[3, 0]** | **[3]** |
| raw-default context keys | `[]` | `[name, parser, args]` |
| dictionary-mode Parsers / contexts | 0 / 0 | 0 / 0 |

BASE confirms the latent 2-map divergence is real (not vacuous): the raw `{}` context is a
distinct hidden class. NORM collapses it — every context, raw path included, is the one
3-field map. Zero dictionary-mode objects in either copy.

**Parity:** `base==native`, `norm==native`, `base==norm` all `true` on the corpus. The
edits are behavior-preserving.

**Optimization status:** both `jsonParser.parser` reach raw `%GetOptimizationStatus` == **25**
(kOptimized/TurboFan tier), byte-identical (`base==norm optStatus true`). No persistent deopt,
no divergence.

**Cold paired bench** (norm/base ratio; absolutes directional only, loadavg 15-26 so paired
ratio is the certified quantity):

| run | loadavg | base ns/byte | norm ns/byte | median ratio | min ratio |
|---|---|---|---|---|---|
| 1 | 19.1 | 16.24 | 16.47 | 1.0142 | 0.9934 |
| 2 | 15.5 | 16.76 | 16.67 | 0.9946 | 1.0289 |
| 3 | 26.3 | 20.83 | 20.88 | 1.0026 | 0.9814 |

Median ratio mean 1.0038; min ratio mean 1.0012. Both straddle 1.0 inside the ±1.4%
run-to-run swing. The floor holds — delta ≈ 0.

## Interpretation

Why parity, not a floor gain: the only site that reads `context.name` (the `trim()` guard)
fires at **construction** time, not in the per-parse hot loop; the dispatch hot path
(`leaf.ts:134` `parsers[idx].parser(state)`) never touches `.context`, and Parser instances
were already same-map in both copies. So the normalization is a **construction-time /
general-API monomorphism** win — it closes the raw-`new Parser(fn)` 2-map divergence and
retires the dead `mapState` `Object.create` polymorph — with the json floor unmoved.

## Verdict vs success criteria

- Same-map true across constructed parsers — **MET** (NORM contextSameMap + rawVsCombinator
  both true; BASE false, so the fix is non-vacuous).
- Zero dictionary-mode objects — **MET** (0/0 both copies).
- Bench delta ≥ 0 — **MET within noise** (median mean +0.38%, min mean +0.12%, both inside
  ±1.4% swing; floor held). Recorded as non-regression: no run showed a load-independent
  slowdown; the paired min ratio favored norm in 2 of 3 runs.

Recommendation: land the uniform 3-field context + mapState retirement as a **robustness /
shape-hygiene** change (not a perf lever). It is safe for PT-T4.W3 — floor-neutral, parity-
exact, and it removes a latent polymorphic seam on the general Parser API surface. No
regression to record.
