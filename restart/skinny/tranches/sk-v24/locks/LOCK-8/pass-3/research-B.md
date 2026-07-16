# LOCK-8 · Family 8B — TYPE-LEVEL TOTALITY — pass-3 research

*The TS type plane of parse-that: the typed combinator algebra, the `Parser<any>` question,
the `as`-erasure audit, the byte-identical-`dist` invariant, and the instantiation budget —
re-founded on measured tsc behaviour, not the R4/PT-3 narrative. Every claim below is a run,
not an assertion. HEAD `ef10d5b` (`@mkbabb/parse-that` 1.0.0), tsc 5.9.3, vite 7.*

---

## 0. The reversal that governs this family

The R4/PT-3 premise — *"the `Parser<any>` inference leak"*, *"combinator return types infer
`Parser<any>`"* — **does not reproduce at HEAD.** A tsc reveal-test (assign each result to
`null` and read the TS2322 message) over `typescript/src/parse/index.ts` at 1.0.0, tsc 5.9.3:

| expression | inferred type at HEAD | verdict |
|---|---|---|
| `all(string("a"), regex(/\d/))` | `Parser<[string, string]>` | **precise tuple** |
| `any(string("a"), string("b"))` | `Parser<string>` | **precise** |
| `string("a").skip(string("b"))` | `Parser<string>` | precise |
| `string("a").sepBy(string(","))` | `Parser<string[]>` | precise |
| `string("a").many()` | `Parser<string[]>` | precise |

The `any` in `all<T extends Array<Parser<any>>>` (`leaf.ts:154`) and `any<…>` (`leaf.ts:28`) is
a **constraint bound, not a return-type leak**. `...parsers: T` triggers variadic-tuple
inference, so `T` binds to the *actual* argument tuple `[Parser<string>, Parser<string>]`; the
mapped `ExtractValue<T>` (`leaf.ts:155-157`) then recovers per-position `infer V` = `string`.
The constraint's `Parser<any>` only widens what `all` *accepts*, never what it *returns*. Family
8B is therefore not "fix a soundness hole" but "**delete a cosmetic `any` from the public
`.d.ts` at provable zero cost, and prove the algebra was already total.**" SPEC-fidelity divergence
row #1: PT-3 §3 / R4 §5 overstate the defect; the re-cut must reclassify it MED→cosmetic.

---

## 1. The `all` / phantom-cast erasure audit (directive 8B-q1)

`all` (`leaf.ts:160-163`) closes with `makeParser(… fuseAll<Result>(parsers) …) as Parser<Result>`;
`makeParser<T>` returns `Parser<T>` (`leaf.ts:7`). The pervasive siblings —
`as Parser<unknown>` throughout `parser.ts` (100,119,141,…), `as unknown as Parser<T>`
(`parser.ts:485,520`), `state.ok/err/from` `as unknown as ParserState<S>` (`state.ts:59,64,72`),
`many`/`sepBy` `as unknown as ParserState<T[]>` (`parser.ts:556,629`).

**Verdict: LAWFUL phantom erasure, documentable — not a soundness leak.** `Parser<T>`'s `T` is
a **phantom** parameter: the sole `T`-typed field is `parser: ParserFunction<T>`, and
`ParserFunction<T> = (val: ParserState<any>) => ParserState<any>` (`parser.ts:14-16`,
`dist/parser.d.ts:2`) — `T` is *already erased to `any`* inside its own definition. No runtime
value ever flows through a `T`-parameterised slot, so no `as Parser<X>` cast can make a value
inhabit a type it does not have; the casts re-label a phantom. This is the standard
newtype/phantom-erasure pattern (cf. Rust `PhantomData`, Haskell `coerce`/`Data.Coerce` over a
representationally-equal newtype — the GHC `Coercible` law is the exact analogue: coercion is
sound iff the parameter is representationally irrelevant, which a phantom always is). The one
place a bounded `any` is *load-bearing* rather than cosmetic is `ParserFunction`'s
`ParserState<any>` argument: the central megamorphic IC (`this.parser(state)`, R4 §6c) threads
heterogeneously-typed states through one indirect call; narrowing it would demand
variance/HKT that TS lacks. **Keep `ParserFunction`'s `any` (document it as an
implementation-boundary variance requirement); it is not counted as an open leak.**

---

## 2. The tightened signature — constructed, and TS2589-clean (8B-q1, 8B-q4)

Minimal tightening (bound only, `any`→`unknown`, drop dead generics), tested standalone under
tsc 5.9.3 `--strict`:

```ts
any<T extends ReadonlyArray<Parser<unknown>>>(...parsers: T): …   // was Array<Parser<any>>
all<T extends ReadonlyArray<Parser<unknown>>>(...parsers: T): …   // was Array<Parser<any>>
fuseAll(parsers: ReadonlyArray<Parser<unknown>>): …              // was Array<Parser<any>>
skip(parser: Parser<unknown>): Parser<T>                          // was skip<S>(parser: Parser<T|S>)
sepBy(sep: Parser<unknown>, …): Parser<T[]>                       // was sepBy<S>(sep: Parser<S|T>, …)
```

Measured outcomes:
- **Inference preserved exactly.** Non-`const` `all_min(string,regex)` ⇒ `Parser<[string,string]>`
  (mutable tuple) — byte-identical to HEAD. `skip`/`sepBy` ⇒ `Parser<string>`/`Parser<string[]>`.
- **No TS2589.** A 12-arm `all_tight(...)` stress instantiates `Parser<[string × 12]>` clean; the
  mapped-type `ExtractValue` is non-recursive (one homomorphic pass), so instantiation depth is
  O(arity), never the exponential that trips TS2589.
- **`const`-variant caveat (do NOT ship by default):** `<const T extends …>` yields
  `Parser<readonly [string,string]>` — a **semver-visible** change (breaks index-assignment /
  spread-mutation at consumer sites). The re-cut ships the **non-`const`** form; `const` is a
  named future option gated on a consumer that wants literal-tuple narrowing.

The `skip`/`sepBy` "input-type leaks" (8B-q4) are the spurious generics `<S>` + the union
`Parser<T|S>`/`Parser<S|T>`. Since both combinators **discard** the skipped/separator value, the
honest type is `Parser<unknown>`; the `<S>` was inference theatre. Dropping it removes the
generic and the union in one move, no new error. **latex-paper is a live consumer of `.skip`**
(`commands.ts:37,56,73,…` `.skip(ws)`) — `Parser<unknown>` accepts `ws` trivially (a
`Parser<string>` is structurally assignable to `Parser<unknown>` — identical erased shape), so
the tightening is non-breaking for it; the migration crater (8D) confirms.

Substrate / seams: `leaf.ts:28,154,179`; `parser.ts:14-16,189-210,569-636`; `state.ts:79`
(`restore(saved:{…; value:any})` → `value: T`, on the state's own `T`). Consolidate the twin
`ExtractValue` (`parser.ts:9-11` ≡ `leaf.ts:155-157`) into one `src/parse/types.ts` export (DRY).

---

## 3. Byte-identical `dist` — the two-step isolation, RUN on the real toolchain (8B-q2)

Two scratch copies of `typescript/` (node_modules symlinked), `vite build` each, HEAD vs the §2
minimal tightening:

- **Step (ii) — the tightening delta:** aggregate `shasum` over **all emitted `dist/*.js` +
  `*.cjs`** is **identical**: `ef2a829379f0d97f7a0c77c71f0e7edab1e6aa9c` on both trees. Per-file
  diff of every `.js`/`.cjs`: **empty.** Only `dist/leaf.d.ts` and `dist/parser.d.ts` change, and
  only in the four signatures of §2 (captured deltas: `Array<Parser<any>>`→
  `ReadonlyArray<Parser<unknown>>`; `skip<S>(parser: Parser<T|S>)`→`skip(parser: Parser<unknown>)`;
  `sepBy` likewise).

This is the constitution's clause-v invariant (type plane costs zero at runtime) **proven for TS
by construction**: vite transpiles TS via esbuild under `isolatedModules`, which erases types
before rollup ever sees them, so a type-only edit *cannot* perturb the JS. The directive's "not an
esbuild proxy" caveat is satisfied — this is the package's own `vite build`, rollup bundle +
`vite-plugin-dts`, not a bare `esbuild` call. **Step (i) — version-migration delta** (bumping
tsc/vite before tightening) is the *only* place `.js` could move (esbuild codegen/minify changes
across versions); it is a separate, attributable diff and must be taken FIRST so any `.js` churn is
charged to the toolchain bump, never to the type work. Since the shipping build is `minify:false`
(R4 §4), even that delta is expected near-empty. **The tightening's own `dist/*.js` diff is
byte-empty — measured, not asserted.**

---

## 4. The instantiation budget — corpus collapsed; take it on self-use (8B-q3, plane-wide P1/P2)

**Consumer census re-run at HEAD (2026-07-16) — the decisive liveness fact:**

| consumer | ver | parse-that today | evidence |
|---|---|---|---|
| **value.js** | **4.0.0** | **REMOVED** | `package.json.dependencies == {}`; zero `src` imports; comments "parse-that-FREE" (`subpaths/math.ts:2`, `subpaths/transform.ts:4`); last commit 2026-07-15 |
| **keyframes.js** | 6.0.0 | **REMOVED** | `leaves.ts:9-10` "0 parse-that … VERIFIED"; transitive dep gone with value.js; last commit 2026-07-15 |
| **latex-paper** | 0.2.1 | **LIVE**, pinned `^0.7.1` (installed 0.7.1) | 8 import sites; `Parser ParserState regex string any` + `.skip`; last commit 2026-07-02 |

**P2 answered: value.js 4.0.0's posture is INTENTIONAL, not a WIP artifact** — a completed,
documented excision (`dependencies:{}` + explicit "parse-that-FREE" annotations across two
subpath modules), reached one major version past R4's 3.1.0 snapshot. **The 59-`all()`-site
value.js corpus that R4/PT-3 leaned on as the arity histogram and the instantiation/crater target
NO LONGER EXISTS.** SPEC-fidelity divergence row #2: PT-3 §W0/§W5's "three consumer suites green"
gate is now **one** external suite (latex-paper); value.js/keyframes are non-consumers whose
"suites" test nothing parse-that. **P1 answered: latex-paper is the sole live external consumer**
of the combinator surface, and it exercises `any` + `.skip` + `regex`/`string` — **not** `all`,
**not** `sepBy`.

Consequence for the budget (directive: gated on P2): a *native* value.js corpus is **un-runnable**
(record it so). The instantiation budget is taken on **(a) first-party `test/*.test.ts` +
`test/benchmarks/`** and **(b) latex-paper's `any(...)`/`.skip(...)` grammar sites** under the
**exact** §2 tightened signature, disclosing signature text + tsc 5.9.3 + the corpus enumerated.
Method: `tsc --diagnostics --generateTrace` on each tree, compare `Instantiations` and
`Types` counts HEAD vs tightened; the §2 mapped-type is O(arity) so the delta is expected within
noise. The `all`/`sepBy` tightenings have **only first-party consumers** — the type-test suite
(`test/types/inference.test-d.ts`) is their legitimate Y.13 consumer; adjudicated, not pruned
(they remain public API the first-party benches import).

---

## 5. Substrate, seams, prototype obligations

**Concrete mechanism (the family, minimized):** one `src/parse/types.ts` holding the single
`ExtractValue` + tuple helpers; `leaf.ts:28,154,179` and `parser.ts:189,569` swap `any`→`unknown`
in bounds and drop the dead `<S>`; `state.ts:79` `restore` types on `T`; `ParserFunction`'s `any`
kept + documented. Net: type-only; emitted JS proven byte-identical (§3).

**Prior art / citations:**
- Phantom-type erasure soundness: GHC `Data.Coerce`/`Coercible` (representational role ⇒ zero-cost
  coercion) — the exact justification for the §1 casts; TS has no roles, so the phantom is enforced
  by discipline + the §3 byte-diff, not the compiler.
- Variadic-tuple + `const` type params: TS 4.0 variadic tuples (the `...parsers:T` tuple inference
  this family relies on) and TS 5.0 `const` type parameters (Microsoft/TypeScript#51865) — the
  `const`-variant caveat (§2) is a direct application.
- Mapped-type recursion / TS2589: the homomorphic `{[K in keyof T]:…}` form is the canonical
  TS2589-safe shape (vs. tail-recursive conditional accumulation, which trips the
  50-depth/5M-instantiation limiter) — why §2 stays clean at arity-12+.
- Type-level test tooling: `expect-type` / `tsd` `@ts-expect-error` assertions run under
  `tsc --noEmit` — the born-RED artefact class for the re-cut's `test/types/`.

**What a prototype must still test to move 8B:**
1. **The generateTrace budget numbers** on the real (a)+(b) corpora — the one figure not yet taken
   (this pass proved inference + byte-identity + no-TS2589; the *count* delta remains).
2. **latex-paper 0.7→1.1 crater** — install a packed tightened-candidate tarball, `npm test` +
   full `tsc`; confirm `.skip`/`any` sites stay green (expected green; §2 shows non-breaking).
   Run the minor-step alone for attribution (PT-3 §W0 method, now against the *sole* consumer).
3. **`restore(value:any)`→`value:T`** — verify no new error at its call sites (`state.ts` internal
   only; no external consumer), and that it does not force a cast elsewhere.

**Named risks / counterexamples:**
- **R-A (`const` semver trap):** shipping `<const T…>` silently narrows `all`'s return to
  `readonly` tuples and breaks any mutate-the-result consumer. Cure: ship non-`const` (§2 proves
  it preserves the mutable tuple exactly).
- **R-B (empty-corpus self-deception):** claiming "three consumer suites green" when
  value.js/keyframes no longer consume parse-that is a hollow gate. Cure: §4 re-scopes the gate to
  latex-paper + first-party; the census is the evidence.
- **R-C (`ParserFunction` over-tightening):** narrowing its `ParserState<any>` to satisfy a
  "zero-any" absolutist reading reintroduces variance TS cannot express and would perturb the IC.
  Cure: §1 documents it as lawful; the "no `Parser<any>` anywhere" charter phrasing is refined to
  "no `any` in *public return types*" (achieved) — SPEC-fidelity divergence row #3.
