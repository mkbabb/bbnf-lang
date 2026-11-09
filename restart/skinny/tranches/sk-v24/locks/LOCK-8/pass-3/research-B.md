# LOCK-8 · Family 8B — THE TYPE PLANE (parse-that/typescript, TYPE-LEVEL TOTALITY)

*The TS type system carries the grammar: full positional inference, a typed combinator
algebra with no `Parser<any>` in a user-facing position, zero runtime cost. All paths cite
`parse-that/typescript` (READ-ONLY; any landing rides letter BBNF-PT-G). Prototyping is on
scratch copies. This family develops the type plane alone.*

---

## 0. The plane's frame

Constitution clause v (the type plane costs zero at runtime) is the family's north star: every
tightening here is a compile-time judgment that must leave the emitted `dist/*.{js,cjs}`
byte-identical. That invariant is not asserted — it is the falsifiable close gate (§4). The
type surface is small and hand-written (2650 LOC, no codegen), so the plane is a signature
problem, not a generator problem: five method/leaf signatures carry every leak.

---

## 1. Consumer liveness (P1/P2) — the decisive finding, answered early

**value.js has DIVESTED parse-that entirely; it is no longer a consumer.** The recon baseline
(value.js 3.1.0 importing `Parser ParserState all any dispatch regex string whitespace
mergeErrorState` across 9 sites, "59 `all()` sites") is STALE. Current state:

- `value.js/package.json` is at **4.0.0** and contains **no `@mkbabb/parse-that` dependency**
  (only a `devDependencies` block; zero `parse-that` string anywhere in `package.json`).
- **Zero** `@mkbabb/parse-that` imports in `value.js/src` (grep of `*.ts/*.js/*.json`, excl.
  `node_modules`/`dist`, returns nothing).
- Git commit `470818c` (*"fix(demo): color picker bug fixes, slider gradients, and parser
  migration"*) is the `-S'@mkbabb/parse-that'` removal from `package.json`. value.js now
  **vendors its own parser** under `value.js/src/parsing/{color,stylesheet,timeline}`.
- The `node_modules/@mkbabb/parse-that@1.0.0` still on disk is a **stale install artifact**,
  not a live edge. value.js's HEAD is fresh (commit 2026-07-15), so this is a completed,
  intentional migration — **not a mid-migration WIP branch**. P2 answer: the 4.0.0 import
  posture is INTENTIONAL and TERMINAL for parse-that.

**latex-paper is the ONLY live external consumer**, and it does not touch the inference
surface. Its imports are uniformly `Parser, ParserState, regex, string, any` (7 grammar
sites: `src/bibtex/parser.ts`, `src/grammar/{primitives,commands,math,text,compile,document,
environments}.ts`). It imports **`any` — never `all`, `skip`, or `sepBy`** — and pins
`^0.7.1` (installed 0.7.1, `<0.8.0`), predating the 1.0.0 `*Span`/`chain()` cut. Commit
cadence is low (HEAD 2026-07-02), so it is effectively frozen relative to the backbone. P1
answer: one live external consumer, on an old pin, exercising `any`/`regex`/`string` only.

**keyframes.js is off parse-that**: no `@mkbabb/parse-that` in `keyframes.js/src` and none in
its `package.json`. Its former transitive edge ran through value.js, which has now divested —
so keyframes.js is transitively-detached too.

**Consequence for 8B (the load-bearing one):** the `all`/`sepBy` deep-tuple inference surface
this family tightens has **no live external consumer**. The 59-`all()`-site "native corpus"
that a prior spec named as the instantiation-budget falsifier no longer exists. The type plane
is now a **first-party** concern — self-use + type-tests are the legitimate consumer (this
routes to 8D's consumer-set adjudication, and it directly decides §5's Q3 route).

---

## 2. Spec fidelity — divergences a re-cut must absorb

| spec assumption (extant PT-3 codex) | measured truth | disposition |
|---|---|---|
| value.js 3.1.0, dep `^1.0.0`, 59 `all()` sites, re-lock `1.0.0→1.1.0` at cut | value.js **4.0.0, no parse-that dep, 0 imports**, vendored parser (`470818c`) | supersede: value.js dropped from the consumer set |
| "three consumer suites green" gates the cut | one live consumer (latex-paper) + one detached (keyframes) + one divested (value.js) | re-cut to **one** external suite + first-party type-tests |
| inference tightening's falsifier = value.js's `all()` sites | latex-paper imports `any` only; no external `all`/`sepBy` consumer | falsifier = first-party `*.test-d.ts` + self-use, not a consumer corpus |
| latex-paper migrates 0.7→1.1 (W0 dry-run) | latex-paper frozen (HEAD 2026-07-02, `^0.7.1`); touches only still-exported symbols | migration is low-risk but **consumer-initiated, not campaign-forced** |

---

## 3. Q1 — the `as unknown as` erasure inside a tightened `all` signature

**The current `all` does not contain `as unknown as` — it hides the same erasure behind a
`Parser<any>` bound.** `leaf.ts:154-164`:

```ts
export function all<T extends Array<Parser<any>>>(...parsers: T) {
    type ExtractValue<T extends ReadonlyArray<Parser<unknown>>> =
        { [K in keyof T]: T[K] extends Parser<infer V> ? V : never };
    type Result = ExtractValue<T>;
    return makeParser(
        parsers.length === 1 ? parsers[0].parser : fuseAll<Result>(parsers),
        createParserContext("all", undefined, ...parsers),
    ) as Parser<Result>;                 // ← single cast today
}
```

`makeParser<T>(parser: ParserFunction<T>): Parser<T>` (`leaf.ts:7`). Because the constraint is
`Array<Parser<any>>`, `parsers[0].parser` is `ParserFunction<any>`; the ternary's union
collapses toward `any` (any absorbs), so `makeParser` infers `T = any`, returns `Parser<any>`,
and `as Parser<Result>` is trivially legal — **a single cast suffices only because the leak is
present.** Duplicate `ExtractValue` also lives at `parser.ts:9-11` (DRY defect — consolidate to
one `src/parse/types.ts`).

**What tightening does.** Replacing the bound with `ReadonlyArray<Parser<unknown>>` makes
`parsers[0].parser: ParserFunction<unknown>`, the union `makeParser<unknown> → Parser<unknown>`,
and `Parser<unknown> as Parser<Result>` is no longer legal as a single `as`: `Parser<T>` is
**invariant** in `T` (T sits in input positions via `map<S>(fn:(v:T)=>S)`,
`then<S>(next: Parser<S|T>)`, `restore(saved:{value:T})` and in output via `parse():…T`), so
`Parser<Result>` and `Parser<unknown>` are not mutually assignable and TS demands
`as unknown as Parser<Result>`. **The double-cast is therefore not new unsoundness — it is the
exact erasure the `Parser<any>` bound performed silently, made explicit and auditable.** That
is the lawful-erasure verdict with teeth: removing `any` surfaces the honest cast.

**The genuine soundness gap is elsewhere — drop-undefined, not the cast.** `all`/`fuseAll`
(`leaf.ts:179-273`) trim `undefined`-yielding arms to the live count (`out.length = w`,
`leaf.ts:209,248,270`). So `all(a, opt(b), c)` types as `ExtractValue = [A, B|undefined, C]`
but the runtime array is `[a, c]` when `b` is absent — positionally misaligned and one element
short. `ExtractValue`'s positional tuple **over-describes** the runtime for any arm whose value
type includes `undefined`. This is the real, documentable soundness concern; the `as unknown
as` masks it but does not cause it. A faithful type filters `undefined` arms positionally, but
that is a semantic change to the public type — gate it on the consumer set (now first-party).

**A readonly-variant that removes the double-cast** is possible but is an API addition, not a
free tightening: introduce a covariant output view `interface ParserOut<out T> { parse(...): …T }`
that `Parser<T>` implements, have combinator returns be `ParserOut<Result>` (covariant → single
`as` legal), and keep the invariant `Parser<T>` only where input methods are needed. This
removes the `as unknown as` down to one cast **and** is checkable (TS 4.7 `in`/`out` variance
annotations enforce it). It does **not** fix drop-undefined. It adds no `TS2589` (variance
annotations are declaration-level, zero instantiation depth). Recommendation: classify the
`as unknown as` as **LAWFUL-ERASURE (invariance bridge, documented)**; treat drop-undefined as
a separate named soundness row; offer `ParserOut<out T>` as the optional single-cast route.

The same invariance-bridge pattern recurs and is lawful at: `parser.ts:485,520`
(`all(...)/wrap(...) as unknown as Parser<T[]>` in `trim`), `parser.ts:556,629`
(`state as unknown as ParserState<T[]>` in `many`/`sepBy` — `ParserState<T>` is likewise
invariant). Each is a construction-boundary erasure, not a value-level lie; each is
documentable in one line.

---

## 4. Q2 — does the tightened algebra emit a BYTE-IDENTICAL `dist/`?

Inference tightening is **pure type-level**: generic bounds, `ExtractValue`, `infer`, variance
annotations. vite/esbuild strips all types before emit, so the JS/CJS bytes cannot change. The
gate is a two-step isolation on the REAL toolchain (vite ^7 + `vite-plugin-dts` + typescript
^5.8, per the manifest — **not** an esbuild proxy, whose type-strip and helper injection differ):

1. **Version-migration delta first (attribution isolation).** If the cut bumps
   vite/tsc/`vite-plugin-dts`, build 1.0.0 source under the OLD toolchain and under the NEW
   toolchain and diff `dist/**/*.{js,cjs}`. Any byte delta here is the toolchain's (minifier,
   helper injection, banner), and must be attributed and accepted BEFORE tightening is judged.
2. **Tightening delta second, on the FIXED toolchain.** Build tightened-signature source vs
   pristine source under the *same* toolchain and diff `dist/**/*.{js,cjs}`. This diff **must be
   byte-empty**. Only `dist/**/*.d.ts` may change (the tighter declared types). An eager-alloc
   census (`= []` occurrences per chunk) is the fast structural cross-check — for a type-only
   change it must be census-identical (the certified W1 alloc lever was `6→0`; a pure W3
   tightening must be `N→N`).

If any `dist/*.js` byte moves under step 2, the "tightening" smuggled a runtime change (a cast
that altered emit, an added helper) — that is a zero-cost-law violation and halts the wave. This
gate is the concrete proof of constitution clause v for the TS side.

---

## 5. Q3 — the instantiation-budget figure

**P2 resolves the gate:** value.js divested, so there is **no migrating consumer** whose native
`all`-corpus can carry the number; latex-paper exercises `any`, not the deep-tuple inference.
Per the directive's else-branch, the figure is taken on **self-use + type-tests under the EXACT
tightened signature**, disclosing signature + tsc version (typescript ^5.8) + corpus.

- **Metric:** `tsc --extendedDiagnostics` "Instantiations" and "Types" counts, plus
  `--generateTrace` + `@typescript/analyze-trace` on the hot mapped-type (`ExtractValue`),
  measured over `test/*.test.ts` + a purpose-built `test/types/inference.test-d.ts` that
  constructs `all`/`any`/`sepBy` at arities 2..8 (the band the unrolled arms cover,
  `leaf.ts:39-57,183-251`).
- **Hazard:** `ExtractValue` is a homomorphic mapped type over an inferred tuple; deep nesting
  (`all(all(all(...)))`) can raise instantiations superlinearly and, at the limit, trip **TS2589**
  ("excessively deep and possibly infinite"). Budget gate: tightened Instantiations ≤ pristine ×
  small-margin AND **zero new TS2589** across the arity band + a nested-depth stress case.
- Because the corpus is first-party, the number is honest only if the test-d corpus mirrors the
  real historical usage shape. Reconstruct the divested value.js distribution (mostly arity-2/3
  `all()`) from its now-vendored parser as the realistic synthetic, and **label it synthetic**.

---

## 6. Q4 — the `skip` / `sepBy` input-type leaks

Both type a **discarded-value** parser as `Parser<T | S>`, leaking the receiver's `T` into an
input position for a value that is thrown away, and over-constraining the argument:

- **`skip<S>(parser: Parser<T | S>)`** (`parser.ts:189`) returns `Parser<T>` — it keeps `this`'s
  value and **discards** the skipped parser's. The skipped value type is irrelevant, so
  `Parser<T | S>` both (a) leaks `T` pointlessly and (b) rejects a `Parser<Z>` whose `Z` is
  unrelated to `T`. **Fix:** `skip(parser: Parser<unknown>): Parser<T>` — strictly more
  permissive, deletes the phantom `S`.
- **`sepBy<S>(sep: Parser<S | T>, min?, max?)`** (`parser.ts:569`) returns `Parser<T[]>` and
  **discards** the separator's value (only element values are collected — `matches[len] =
  state.value` from `this.parser`, `parser.ts:583-617`). Same leak. **Fix:**
  `sepBy(sep: Parser<unknown>, min?, max?): Parser<T[]>` — deletes `S`.

Both fixes **remove** a type variable and a union computation, so they **cannot introduce
TS2589** (they reduce instantiation work). They are source-compatible supersets for well-typed
callers (any prior `Parser<T|S>` argument is already a `Parser<unknown>`). No `dist/` byte moves
(§4).

**Siblings (analyze separately, not the same fix):** `then<S>(next: Parser<S|T>)`
(`parser.ts:81`, returns `[T,S]` — KEEPS both, so `|T` is over-widening of `S`'s inference, not a
discard-leak; tighten to `next: Parser<S>`) and `or<S>(other: Parser<S|T>)` (`parser.ts:105`,
returns `T|S` — `|T` redundant since `S|T` already covers `T`; tighten to `other: Parser<S>`).
`next<S>(parser: Parser<S>)` (`parser.ts:212`, returns `Parser<S>`) is already clean — the
correct template the others should match.

---

## 7. The mechanism, concrete — tightened signatures + integration seams

| seam | path:line | change | risk |
|---|---|---|---|
| duplicate `ExtractValue` | `parser.ts:9-11`, `leaf.ts:155-157` | lift to one `src/parse/types.ts`, import both sides | none (DRY) |
| `all` bound | `leaf.ts:154` | `Array<Parser<any>>` → `ReadonlyArray<Parser<unknown>>`; cast becomes explicit `as unknown as Parser<Result>` (or `ParserOut<Result>` single cast) | §3 invariance bridge |
| `fuseAll` param | `leaf.ts:179` | `Array<Parser<any>>` → `ReadonlyArray<Parser<unknown>>` | internal only |
| `any` bound | `leaf.ts:28` | `Array<Parser<any>>` → `ReadonlyArray<Parser<unknown>>` + `infer` union | latex-paper consumes `any` |
| `skip` input | `parser.ts:189` | `Parser<T\|S>` → `Parser<unknown>`, drop `<S>` | none (super) |
| `sepBy` sep | `parser.ts:569` | `Parser<S\|T>` → `Parser<unknown>`, drop `<S>` | none (super) |
| `then`/`or` input | `parser.ts:81,105` | `Parser<S\|T>` → `Parser<S>` | inference-narrowing |
| `restore` value | `state.ts:79` | `{value: any}` → `{value: T}` | none |
| `ParserFunction<T>` | `parser.ts:14-16` | KEEP the implementation-boundary erasure (megamorphic IC needs the type-erased pointer); document, NOT counted as a leak | intentional |

`ParserFunction<T>` is the one lawful internal `any`: it is the erased function-pointer the
central megamorphic call site dispatches through (`this.parser(state)`, e.g. `parser.ts:53,82`;
`leaf.ts:44,49`). It never appears in a user-facing return; the type-test suite must whitelist
it explicitly, or it will read as an open leak.

---

## 8. Prior art (cited)

- **TypeScript variance annotations** (`in`/`out`, TS 4.7): the mechanism that makes an output
  view `ParserOut<out T>` enforce covariance and legalize a single cast — the principled route
  out of `as unknown as` (TS handbook, "Variance Annotations").
- **Method-parameter bivariance** (strictFunctionTypes checks function-type *properties*, not
  *methods*): why `then`/`skip`/`sepBy` — being methods — are bivariant in their param, which is
  exactly why `Parser<T>` reads as invariant and the cast is forced. Load-bearing for §3's verdict.
- **Homomorphic mapped types over inferred tuples** (`{[K in keyof T]: … infer V}`): the
  `ExtractValue` idiom; the TS2589 depth guard is the known failure mode (io-ts, Zod, and
  `ts-toolbelt` all document deep-instantiation blowups — Zod's recursive schemas are the
  canonical TS2589 case study).
- **Type-level testing:** `tsd` / `expect-type` / `@ts-expect-error` under `tsc --noEmit` — the
  standard falsifier for "infers precisely both ways" (rejects over-widening AND catches true
  mismatch). fp-ts and Effect ship exactly this `*.test-d.ts` style.
- **`tsc --extendedDiagnostics` / `--generateTrace` + `@typescript/analyze-trace`**: the
  instantiation-budget instrument (§5).
- **esbuild/vite type-erasure guarantee**: types never reach emit — the ground of §4's byte-empty
  claim; contrast an esbuild-only proxy whose helper injection differs from real vite/`vite-plugin-dts`.
- Combinator-typing precedents: **Parsimmon**/**arcsecond** (`any`-heavy — the anti-pattern this
  family removes) vs **@typed/parser** / **Effect Schema** (precise positional inference — the target).

---

## 9. Risks & counterexamples

- **R1 — the `all` type sharpens and a first-party site breaks.** With no external `all`
  consumer, the only falsifier is `test/types/inference.test-d.ts`; if it under-covers the arity
  band, a real narrowing regression ships silently. *Cure:* the test-d corpus reconstructs the
  divested value.js arity distribution (mostly 2/3) plus a deep-nest stress case.
- **R2 — drop-undefined stays unsound after tightening.** Tightening the bound does not fix the
  positional-tuple-vs-trimmed-array mismatch (§3). *Counterexample:* `all(string("a"), opt(b),
  string("c"))` types `[string, B|undefined, string]`, runs `["a","c"]`. *Disposition:* name it a
  distinct soundness row; the `undefined`-filtering type is a separate, consumer-gated change
  (first-party now — low blast radius, but still a public-type change).
- **R3 — `ParserOut<out T>` over-reaches into an API redesign.** The covariant view removes the
  double-cast but adds a type; parsimony must bound it to the return position only. *Cure:*
  prototype both — plain `as unknown as` (documented) vs `ParserOut<out T>` — and keep the smaller
  one unless the variance annotation pays for itself in measurable leak reduction.
- **R4 — TS2589 under the tightened `ExtractValue`.** Deep-nested `all` can blow instantiation
  depth. *Cure:* §5's nested stress case is a gate, not an afterthought; if tripped, the fix is a
  depth-bounded conditional (`T extends [infer H, ...infer R]` linear recursion) measured against
  the mapped-type form.
- **R5 — the stale `node_modules/@mkbabb/parse-that@1.0.0` in value.js misleads a census.** A
  naive per-tree import census trusting `node_modules` would wrongly count value.js as live.
  *Cure:* census on `package.json` deps + `src` imports (as §1 did), never on installed trees.

---

## 10. What a prototype must test to move this family

1. **The invariance verdict, mechanically.** On a scratch copy, tighten `all`'s bound to
   `ReadonlyArray<Parser<unknown>>` and confirm `tsc` forces `as unknown as Parser<Result>`
   (single `as Parser<Result>` errors "neither sufficiently overlaps"). Then implement
   `ParserOut<out T>` and confirm a single cast type-checks. Settles §3's route empirically.
2. **Byte-empty `dist/` (the zero-cost proof).** The §4 two-step isolation on the REAL
   vite/`vite-plugin-dts` toolchain: version-migration delta measured and attributed, then
   tightening delta on the fixed toolchain diffed to **zero JS/CJS bytes** (only `.d.ts` moves).
3. **The type-test falsifier.** `test/types/inference.test-d.ts`: `all(string,regex(/\d/))` ⇒
   `Parser<[string,string]>` not `Parser<any>`; `any(string,string)` ⇒ `Parser<string>`; a
   `@ts-expect-error` on a genuinely wrong shape still fires. RED on 1.0.0, GREEN after.
4. **skip/sepBy supersets.** Confirm `skip(parser: Parser<unknown>)` / `sepBy(sep:
   Parser<unknown>)` accept every prior call site AND a `Parser<Z>` the old union rejected, with
   no new TS2589.
5. **Instantiation budget, disclosed.** `tsc --extendedDiagnostics` Instantiations pristine vs
   tightened over arity-2..8 + nested stress; report the counts, the signature diff, and
   typescript ^5.8 — the number lives with its corpus, since no live external `all` corpus exists.
6. **The drop-undefined row.** A runtime + type differential on `all(a, opt(b), c)` documenting
   the positional-tuple-vs-trimmed-array gap, so the re-cut carries it as a named soundness
   decision rather than an invisible cast.
