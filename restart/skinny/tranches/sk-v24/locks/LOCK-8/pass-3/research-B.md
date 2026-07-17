# LOCK-8 · Family 8B — THE TYPE PLANE — pass-3 research

*TYPE-LEVEL TOTALITY: the TS type system carries the grammar — full positional inference, a
typed combinator algebra with no `Parser<any>` anywhere, diagnostics typed by position, all at
the zero-runtime-cost law. All paths are `parse-that/typescript` (READ-ONLY; any landing rides
letter BBNF-PT-G). Prototyping is scratch-copy only. Every `path:line` verified against HEAD.*

---

## 0. The type surface as it stands (the substrate)

The public combinator algebra is typed at four homes, and the family's whole subject is the gap
between what these signatures *promise* and what the runtime *delivers*:

- `ExtractValue<T>` — the positional projection, **defined twice**: `parser.ts:9-11` (module) and
  **re-declared byte-identically inside `all()`** `leaf.ts:155-157`. Both read
  `{ [K in keyof T]: T[K] extends Parser<infer V> ? V : never }`. DRY defect; one exported home
  (`src/parse/types.ts`) imported by both is the fix — a pure refactor, zero type-behaviour change.
- `ParserFunction<T=string>` `parser.ts:14-16` — the megamorphic-IC pointer. Its **type parameter
  `T` is entirely phantom**: the body is `(val: ParserState<any>) => ParserState<any>`; `T` never
  appears. Every `as ParserFunction<[T,S]>` / `as ParserFunction<S>` cast strewn through
  `parser.ts` (lines 100,119,141,157,184,207,229,246,...) is therefore a **type-level no-op** —
  documentary only. The real typing is minted solely by the `makeParser`/`new Parser` return casts.
  This is lawful (§1) but it means the "tightening" cannot touch `ParserFunction`; it lives in the
  return-cast layer.
- The `Parser<any>` leak sites: `any<T extends Array<Parser<any>>>` `leaf.ts:28`;
  `all<T extends Array<Parser<any>>>` `leaf.ts:154`; `fuseAll(parsers: Array<Parser<any>>)`
  `leaf.ts:179`; `restore(saved: {…; value: any})` `state.ts:79`.
- The value-slot erasure choke points: `state.ok/err/from` each `return this as unknown as
  ParserState<S>` `state.ts:59,66,73`; `unsafeSetValue`/`unsafeCall`/`unsafeCallRaw`
  `state.ts:87-99`. `fuseAll` produces its tuple through exactly this door: `return state.ok(out)
  as ParserState<Result>` `leaf.ts:210,249,271`.

`Parser<T>` is **invariant in `T`**: `T` occurs covariantly in `value:T` (`parser.ts:26`,
`state.ts:49`) and in `state:ParserState<T>|undefined`, and — nominally — in the `ParserFunction<T>`
field, but that erases to `any`. So the only real `T`-dependence at runtime is the `value` slot,
which is genuinely `unknown` until a parse populates it.

---

## 1. Directive-1 audit — the `as unknown as` erasure in the `all` value path

**Where the erasure actually lives.** `all()` itself (`leaf.ts:154-164`) carries a **single** cast,
`... as Parser<Result>`, not `as unknown as`. Under the *current* loose bound the ternary
`parsers.length===1 ? parsers[0].parser : fuseAll<Result>(parsers)` has type
`ParserFunction<any> | ParserFunction<Result>`; `makeParser<T>` infers `T=any`, returns
`Parser<any>`, and `any → Result` is a plain narrowing — one `as` suffices. The **`as unknown as`**
form appears (a) in the value producer `state.ok` (`state.ts:59`) that `fuseAll` funnels through,
and (b) in the two `all()`-consuming combinators: `trim` — `whitespace as unknown as Parser<S>`
`parser.ts:481`, `all(parser,this,parser) as unknown as Parser<T[]>` `parser.ts:485`,
`this.wrap(parser,parser) as unknown as Parser<T>` `parser.ts:520` — and `wrap`'s `discard` arm
`all(start as Parser<unknown>, this as Parser<unknown>, end as Parser<unknown>)` `parser.ts:394`.

**Verdict on the erasure itself: LAWFUL variance-bridging, documentable.** Each `as unknown as
ParserState<S>` / `as Parser<Result>` bridges the invariant `value`-slot: the runtime object is one
`ParserState`/`Parser` whose slot genuinely holds the value the target type names. `unknown` is the
honest intermediary for an invariant reconstruction (`Parser<unknown> → Parser<Result>` is neither a
widening nor narrowing, so the double-step is *required*, not a smell). This is an
implementation-boundary erasure of exactly the kind `ParserFunction`'s eslint-disable already
documents. A written justification suffices; it is **not** a soundness leak *per se*.

**BUT there is a genuine, cast-independent soundness leak underneath it.** `fuseAll` **compacts
`undefined`**: `if (state.value !== undefined) out[w++] = state.value; ... if (w!==n) out.length=w`
(`leaf.ts:199,207,209,230,238,246,248,267,270`). The type `ExtractValue<T>` promises a fixed-arity
positional tuple; the runtime yields a **shorter, index-shifted array** whenever any element parser
produces `undefined`. Concretely `all(string("a"), whitespace, string("b"))` types as
`Parser<[string,string,string]>` but, when `whitespace` empty-matches (`regex(/\s*/)` sets
`unsafeSetValue(undefined)` `leaf.ts:355`), yields `["a","b"]` at length 2. Same for any
`Parser<X|undefined>` element (notably `opt()` `parser.ts:234-249`, whose type is
`Parser<T|undefined>`). **No cast form fixes this** — `as`, `as unknown as`, or a perfectly precise
mapped type all sit *atop* a type→runtime arity contract that `fuseAll` breaks. The erasure merely
conceals it.

**Disposition (the tension the directive is really probing).** Three routes, only one of which is
both sound and TS2589-free:
1. **Keep the O(N) mapped type + document the compaction caveat.** Tighten the bound (§4) so
   `all<const T extends readonly Parser<unknown>[]>`, keep `as unknown as Parser<Result>` as a
   documented boundary erasure. TS2589-free (mapped type is non-recursive, O(arity)). Positional
   type stays *nominally* unsound for `undefined`-typed elements — labelled LAWFUL-NON-MIRROR with
   the named caveat. **Recommended**: it is the only route that neither risks TS2589 nor breaks
   consumers.
2. **Make the type sound by filtering `undefined` positions.** Requires a *recursive* tuple
   conditional (drop-element is not expressible as a mapped type) — the exact TS2589 hazard the
   directive fences. Rejected: trades a documented caveat for a compile-time-depth cliff on deep
   `all()` arities.
3. **Make the runtime match the type — stop compacting (`out[i]=state.value` unconditionally).**
   Sound and TS2589-free, but a **breaking runtime change**: every consumer that today receives the
   compacted array (value.js's `all()` destructures, `wrap`/`trim` which rely on the discard) would
   see extra `undefined` slots. A semver-major move; out of scope for a 1.1.0 tightening.

The prototype must therefore *exhibit* the compaction divergence (a paired type-vs-runtime test:
assert the inferred tuple length ≠ observed array length for an `undefined`-yielding element) and
adopt route 1 with the caveat written into the `all` doc-comment.

---

## 2. Directive-4 — the `skip`/`sepBy` (and `then`/`chain`) input-type leaks

These are **input-position** leaks, distinct from the `all` output leak, and every one is **shallow
and non-recursive — tightening cannot introduce TS2589**:

- `skip<S>(parser: Parser<T | S>): …ParserFunction<T>` `parser.ts:189-210`. `S` appears **only**
  inside the argument union `Parser<T|S>` and nowhere in the result. Because `S` is free, `Parser<T
  |S>` unifies with *any* `Parser<X>` (`S:=X`), so the parameter is a **disguised `Parser<unknown>`**
  that pretends to constrain. `skip` discards the argument's value (returns `T`), so the honest,
  precise signature is `skip(parser: Parser<unknown>): Parser<T>` — drop `S` entirely.
- `sepBy<S>(sep: Parser<S | T>, min, max)` `parser.ts:569`. Identical pattern: `S` phantom, `sep`'s
  value is discarded (result is `T[]`). Tighten to `sepBy(sep: Parser<unknown>, min?, max?):
  Parser<T[]>`.
- `then<S>(next: Parser<S | T>): …ParserFunction<[T,S]>` `parser.ts:81`. The `| T` widens what is
  accepted and muddies `S` inference. Precise form: `then<S>(next: Parser<S>): Parser<[T,S]>` —
  drop the `| T`. `then` builds a literal `[T,S]` tuple (`parser.ts:100`), so it is NOT subject to
  the §1 compaction; its type is sound once the `|T` is removed.
- `chain<S>(fn: (value:T)=>Parser<S | T>): …ParserFunction<S>` `parser.ts:124`. The `S|T` return
  union is loose; precise: `chain<S>(fn: (value:T)=>Parser<S>): Parser<S>`.
- `or<S>(other: Parser<S>)` `parser.ts:105` is **already precise** (`ParserFunction<T|S>`, correct).
- `restore(saved:{…; value:any})` `state.ts:79` → `restore(saved:{offset:number; value:T}): this`.

**No-new-TS2589 check.** All six edits remove a union or a phantom generic from a **depth-1** signature
— no conditional recursion, no accumulation over a tuple. Instantiation cost per call *drops*
(one fewer type var to solve; `skip`/`sepBy` lose `S` entirely). The type-test artefact
(`test/types/inference.test-d.ts`, `@ts-expect-error`/`expect-type` under `tsc --noEmit`) asserts:
`pA.skip(pB)` infers `Parser<A>` regardless of `B`; `pA.then(pB)` infers `Parser<[A,B]>`;
`pA.sepBy(comma)` infers `Parser<A[]>`; and a genuinely wrong shape still `@ts-expect-error`s
(precision proven both directions).

---

## 3. Directive-2 — does the tightened algebra emit a BYTE-IDENTICAL `dist/`?

**Claim: yes, provably, and the two-step isolation is mandatory because the toolchain moved.**
Every §1–§2 edit is type-only: generic-bound changes, union removals, `as`/`as unknown as` casts,
and the `ExtractValue` de-duplication — all erased by the compiler. TS emits **no** JS for a type
annotation, a generic parameter, a type alias, or an `as` cast. The one class of type-syntax that
*does* emit (enum / namespace / parameter-property / `import =`) is untouched. So step-(ii) is
byte-empty **by construction of what is being changed**.

The **isolation is non-negotiable** because a naive before/after `dist/` diff would conflate two
independent deltas — the build toolchain is `vite ^7 / vitest ^3.2 / typescript ^5.8`
(`typescript/package.json` devDeps, R4 §4), and the pinned 1.0.0 `dist/` was cut under an earlier
toolchain:
- **Step (i) — the version-migration delta.** Build the *unchanged* 1.0.0 source under the pinned
  toolchain, then again under the candidate toolchain; diff. Vite/esbuild/tsc bumps legitimately
  perturb emitted JS (helper injection `__spreadArray`/`__assign`, comment handling, sourcemap
  offsets — note `minify:false, sourcemap:true` in `vite.config.ts`, R4 §4, so emission is
  human-readable and diffs are meaningful). This delta is **documented, not zero**.
- **Step (ii) — the tightening delta.** At a *fixed* toolchain, build 1.0.0 source vs the tightened
  source; diff `dist/*.{js,cjs}`. This **must be byte-empty** (sourcemaps `.map` may differ only if
  line counts shift, which type-only edits do not cause). Any non-empty `.js` diff is a red flag
  that a *value* changed — the wave halts and the offending edit is found.

**Named hazard for step (ii).** The `ExtractValue` de-dup moves a type alias out of `all()`'s body
into a module import. This is still type-only (aliases erase), but it changes an *import graph* — if
the build inlines per-module, verify the moved import is `import type` so it does not pull a
value-side symbol. The tape-crossing / decode-speed numbers are LOCK-6's fence, not this family's;
note only that the byte-diff harness is `vite build` on the **real** toolchain, never an esbuild
proxy (esbuild's transform ≠ vite's rollup pipeline and would give a false-clean).

---

## 4. Directive-3 — the instantiation-budget figure, and the CONSUMER-LIVENESS pivot (P2)

**The live consumer corpus has moved out from under the R4/PT-3 assumptions — this is the family's
sharpest new fact.** A fresh per-tree census:

- **value.js is now `4.0.0`, on branch `tranche-u`** (HEAD `86a9a17`, 2026-07-15 — a paused WIP
  campaign, "THE PAUSE — halted at owner order"). Its **`package.json` declares NO
  `@mkbabb/parse-that` dependency** (grep-empty). Yet **two source files still import it**:
  `src/subpaths/math.ts`, `src/subpaths/transform.ts`. The R4 snapshot (value.js 3.1.0, `^1.0.0`,
  9 import sites, "59 `all()` sites") is **superseded**: the live tree has an **undeclared-dep /
  residual-import mismatch** — the classic signature of a **mid-migration WIP artifact**, not an
  intended posture. value.js has largely migrated *off* the combinator surface between 3.1.0 and
  4.0.0, retaining only two subpaths, and even those resolve against a dependency no longer in the
  manifest. **Answer to P2: WIP-branch artifact, not a settled contract.** Carry both hypotheses
  only until the owner confirms; the evidence (undeclared dep + 2 residual imports + paused campaign
  branch) weighs heavily toward "not a stable floor."
- **latex-paper is unchanged**: `0.2.1`, dep `@mkbabb/parse-that ^0.7.1` → resolves **0.7.1**
  (`node_modules` confirmed), last commit 2026-07-02 (stale ~2wk). It imports only
  `Parser, ParserState, regex, string, any` (R4 §7) — **none of which are `all`/`then`/`skip`/
  `sepBy`**. So latex-paper exercises **zero** of the tightened combinators and is **useless as a
  tuple-inference budget corpus**, and it still predates the 1.0.0 cut (a 0.7→1.1 jump).

**Consequence for the instantiation budget (gated on P2 = WIP):** neither live consumer is a valid
native corpus for the tightened `all`/`then`/`skip` inference. value.js-4.0.0 is un-runnable in its
current WIP state (undeclared dep); latex-paper touches none of the tightened combinators. **Record
the native measurement as un-runnable and take the figure on parse-that self-use** — the `test/`
suite + `test/benchmarks/*.bench.ts` (`json-comprehensive`, `pt-b3-fusion` exercise dense `all()`
nests) + the new type-test suite — **under the EXACT tightened signature, disclosing signature +
`tsc` version + corpus set**. Instrument via `tsc --extendedDiagnostics` ("Instantiations" count)
or `--generateTrace traceDir` before/after; the expected direction is a **decrease** (§2 removes
free type vars). Budget: tightened Instantiations ≤ baseline, the mapped-type `all` (§1 route 1)
contributing O(arity) per site and **no TS2589 at the deepest observed nest** — the falsifiable
number that gates the tightening.

**Plane-wide P1 (consumer liveness, forward-looking):** the only unambiguously *live, importing*
consumer of the combinator surface today is value.js's two residual subpaths — and those are on a
paused branch with a broken dep declaration. keyframes.js is transitive-through-value.js (R4 §8) and
inherits the same instability. **The forward consumer set is thin and in flux**; the API-plane (8D)
and semver decisions must not over-fit to the R4 "59 all() sites" figure, which no longer describes
HEAD.

---

## 5. Prior art (cited) — the type-level combinator canon

- **HKT-free positional tuple inference via variadic tuple types + `infer` in mapped types** is the
  established pattern; TS 4.0's variadic tuples (Microsoft/TypeScript#39094) and the
  `{ [K in keyof T]: … infer … }` idiom are exactly `ExtractValue`. parse-that's form is canonical;
  the only defect is duplication + the loose `Parser<any>` bound, not the technique.
- **`ReadonlyArray` + `const` type parameters** (TS 5.0 `const` modifiers on type params, TS#51865)
  are the tightening lever: `all<const T extends readonly Parser<unknown>[]>(...parsers: T)` captures
  literal tuple structure without a call-site `as const`, fixing inference precision at the source.
- **The undefined-compaction / type-vs-runtime arity mismatch** mirrors a known hazard in
  fp-ts / Effect Schema tuple decoders and in Zod's `.tuple()` vs `.array()` split: a decoder that
  *filters* cannot honestly type as a fixed tuple. Effect's resolution is an explicit
  "filtered → array" widening; the analogue here is route-1's documented caveat or a distinct
  `allExact` that never compacts. This is the SOTA adjudication, not a novel gap.
- **`--generateTrace` instantiation budgeting** is the Microsoft-sanctioned method for guarding
  compile-time cost (the `@typescript/analyze-trace` tool), and TS2589 ("excessively deep") is the
  hard wall recursive tuple filters hit — directly validating §1's route-2 rejection.
- **Zero-runtime-cost type erasure** is a language guarantee (TS emit strips types), the formal
  backing for §3's byte-empty step-(ii) claim; the isolation discipline (separate the toolchain
  delta) is standard reproducible-build hygiene.

---

## 6. Integration seams (path:line, where extant code serves)

- De-dup home: new `src/parse/types.ts` exporting the single `ExtractValue`; imported by
  `parser.ts:9` and `leaf.ts:155` (delete the inner copy).
- `all` bound tighten: `leaf.ts:154` `Array<Parser<any>>` → `readonly Parser<unknown>[]` with
  `const`; `fuseAll` param `leaf.ts:179` likewise; keep `as unknown as Parser<Result>` return with
  a doc-comment naming the compaction caveat (leaf.ts:160-163).
- Input-leak tightens: `skip` `parser.ts:189`, `sepBy` `parser.ts:569`, `then` `parser.ts:81`,
  `chain` `parser.ts:124`, `restore` `state.ts:79`, `any` `leaf.ts:28`.
- `ParserFunction` `parser.ts:14-16` — **leave as-is** (phantom `T` is lawful variance; the
  `as ParserFunction<…>` casts are no-ops, do not churn them).
- Byte-diff harness: `vite build` on `vite.config.ts` (R4 §4), two-step per §3.
- Type-test project: `test/types/inference.test-d.ts` run under `tsc --noEmit`; instantiation budget
  via `tsc --extendedDiagnostics`/`--generateTrace`.

---

## 7. Named risks & counterexamples

- **R-B1 (the real leak): compaction-arity mismatch.** `all(a, opt(b), c): Parser<[A, B|undefined,
  C]>` runtime-yields length 2 when `opt(b)` fails. Cast-independent; route-1 documents it, route-3
  would break consumers. The prototype MUST exhibit it (assert inferred-arity ≠ observed-length).
- **R-B2: TS2589 on the sound route.** A recursive `undefined`-filtering tuple type blows the
  instantiation depth on deep `all()` nests. Counterexample to route-2; the budget test at the
  deepest observed nest is the falsifier.
- **R-B3: byte-diff false-clean via esbuild proxy.** Diffing an esbuild transform instead of the real
  vite/rollup `dist/` hides an emission change. Cure: §3's harness is `vite build` only.
- **R-B4: consumer over-fit.** Designing the tightened surface to value.js's R4-era "59 all() sites"
  when HEAD (4.0.0/tranche-u) has migrated off and declares no dep. Cure: §4's re-census is the
  authority; carry the owner's P2 answer before pruning/keeping combinators.
- **R-B5: phantom-generic churn.** "Tightening" `ParserFunction`'s phantom `T` would break the
  megamorphic-IC variance the eslint-disable protects (R4 §6c) with zero type gain. Do not touch it.

---

## 8. What a prototype must test to move this family

1. **The compaction oracle** — a paired type-vs-runtime test proving `all` with an
   `undefined`-yielding element diverges in arity (fixes route-1 vs route-3 as a *decision*, not an
   assumption).
2. **The tightened type-test suite** RED-on-1.0.0/GREEN-after for `all`/`any`/`then`/`skip`/`sepBy`,
   with `@ts-expect-error` proving precision both ways.
3. **The two-step byte-diff** on the real vite toolchain: step-(i) documented toolchain delta,
   step-(ii) **empty** tightening delta.
4. **The instantiation budget** via `tsc --generateTrace` on self-use, under the exact tightened
   signature, with the deepest-nest TS2589-free assertion — disclosing signature + tsc version +
   corpus (native corpus recorded un-runnable per §4).
5. **A no-new-TS2589 sweep** across all six §2 edits (they are depth-1; the assertion is that
   instantiation count does not rise).
