# LOCK-8 family 8B — TYPE-LEVEL TOTALITY (research, pass 2)

*The TS type system carries the grammar: full `ExtractValue` inference, a typed combinator algebra
with no `Parser<any>`, diagnostics typed by position, compile-time inference mirroring the
zero-runtime-cost law. This is parse-that's OWN inference story — distinct from the Rust HM/CSP
solver. The plane is `type`/signature-only; emitted JS is byte-identical, so clause-v (zero runtime
cost) holds by construction and is proven by an empty `dist/` semantic diff.*

Pass 1 established the mechanism (the phantom-`ParserFunction` OUTPUT leak, the typed algebra, the
`opt`-in-`all` parity hole, the instantiation budget). Pass 2 was ordered to answer with EVIDENCE
against the *actual* consumer graph. The evidence overturns the premise every prior figure rested
on. That supersession is the load-bearing finding; the four tagged questions are answered under it.

---

## 0. THE SUPERSESSION — the anchor consumer has divorced parse-that

Ground truth from the working trees of record (READ-ONLY, verified by `grep`/`package.json`):

- **value.js is `4.0.0`** (`/Users/mkbabb/Programming/value.js`, git HEAD `86a9a17`, tags stop at
  `v3.1.0` — 4.0.0 is the unreleased tree of record). Its `package.json` `dependencies` is **empty**;
  no `@mkbabb/parse-that` in deps OR devDeps. `grep` of `src/` for `@mkbabb/parse-that` returns
  **zero import sites**. The CSS grammar is now a **hand-written recursive-descent parser** at
  `src/v4/css/grammar.ts` over value.js's own `Result`/`ok`/`err` (`src/v4/result.ts`); the subpath
  docs assert it verbatim — `src/subpaths/math.ts:2` "parse-that-FREE", `src/subpaths/transform.ts:4`
  "zero parse-that". The tree is **24 TS files**, not the directive's "259-file graph" (that figure
  counted a stale build/node_modules graph). The R4 "59 `all()` sites / dispatch c-bucket / 61
  `all()`" surface **no longer exists**.
- **keyframes.js is `6.0.0`**, `dependencies` = `{ "@mkbabb/value.js": "4.0.0" }` only — i.e. it
  consumes the parse-that-FREE value.js. `src/animation/internal/leaves.ts:9-10` documents its
  subpath as "`parse-that`-FREE … 0 parse-that / 0 engine modules — VERIFIED". Its stray
  `node_modules/@mkbabb/parse-that` at `1.0.0` is an unused transitive artifact.
- **latex-paper is `0.2.1`** — the **only surviving live parse-that type-level consumer**. Pinned
  `@mkbabb/parse-that@^0.7.1`, installed **0.7.1** (`node_modules/.../package.json`). Eight grammar
  files import from parse-that (`src/grammar/{commands,text,math,environments,document,compile,
  primitives}.ts`, `src/bibtex/parser.ts`), each importing exactly `Parser, regex, string, any`
  (some subsets). **183** real combinator call sites in `src/grammar/`+`src/bibtex/`
  (`.then/.or/.opt/.many/.sepBy/.map/.chain/.skip/.next/.trim/.wrap`).

**Consequence (a named supersession, constitution clause ix + viii).** The 8B type-level consumer
contract re-homes: the anchor is **latex-paper native** (the directive's budget corpus, and now the
*sole* real corpus) + the `.test-d.ts` suite + parse-that's own `parsers/json.ts`/`csv.ts` self-use.
Every prior 8B/PT-E2 figure quoted "against value.js's 59 `all()` sites" is **unmeasurable** — those
sites are gone. This is not fatal to 8B (Y.13 is satisfied by latex-paper + the type-test suite), but
it *is* fatal to any figure claimed on value.js, and it must be surfaced to the gestalt before a
number is banked. **Owner confirmation is required** that value.js 4.0.0's divorce is intended (vs a
mid-migration tree); the family develops against the evidence as it stands.

---

## 1. Q1 — `Parser<DropUndefined<Values<T>>>` instantiation budget "at all value.js `all()` sites"

**There are zero parse-that `all()` sites in value.js 4.0.0** (the three `grep` hits — 
`src/v4/css/grammar.ts:535,549,551` — are a local `isTransformCall()` function, not the combinator).
**latex-paper imports `Parser, regex, string, any` and uses NO `all()` at all** (verified: no `all(`
combinator call in any grammar file). So the signature `all<const Ps extends readonly
Parser<unknown>[]>(...ps: Ps): Parser<DropUndefined<Values<Ps>>>` has **no live consumer call site**
against which to blow the checker.

The budget question therefore does not disappear — it **relocates**. It must be re-measured on:
(i) the `.test-d.ts` suite's deliberately-wide `all(...)` cases (synthetic, but the checker is real),
and (ii) parse-that's own `parsers/json.ts` / `csv.ts` which do use `all`. And the *dominant* real
TS2589 risk in the surviving corpus is not `all`'s variadic tuple at all — it is **latex-paper's
`.then()` chains** (up to 12 `.then(` in `src/grammar/commands.ts` alone), each nesting a fresh
2-tuple one level deeper, and its **mutually-recursive grammar** (document→environments→commands→
math→text). The recursion is *bounded* structurally: every rule is annotated `const x: Parser<Node>`
(23 `Parser<…>` annotations in `commands.ts`, 21 in `text.ts`) or built via `lazy(() => …): Parser<T>`,
so the type checker **terminates recursion at the nominal `Parser<Node>` seam** — it never expands the
grammar into an infinite type. A 12-deep `.then` chain is 12 nested tuple instantiations, far under the
~1000 tail-recursion / ~50 non-tail conditional cap (TS 4.5, microsoft/TypeScript #49459).

**Disclosure rule (binding, per the directive).** Because value.js can no longer supply the number,
every TS2589/instantiation/check-time figure for 8B MUST state: *taken on latex-paper native, under
signature `<the exact generic>`, tsc `<version>`, `--extendedDiagnostics` + `--generateTrace`*. A
figure attributed to "value.js `all()` sites" is now unproduceable and must be struck.

## 2. Q2 — `all(a, opt(b), c)` soundness — the length-variable tuple

**First-confirm result (the directive's precondition): opt-in-`all` typed sites DO NOT EXIST in any
surviving consumer.** value.js divorced parse-that; latex-paper uses `.opt()` at **11 sites**, every
one via `.then(X.opt())` / `.skip(X.opt())` / `.next(X.opt())` (`commands.ts:108,178,192,196,230,245,
271,273`; `environments.ts:44,148`), **none inside `all()`** — and latex-paper has no `all()` anyway.

So the runtime differential the directive asks for **has no consumer input to run**. The parity hole
(`all` drops `undefined` at runtime — `leaf.ts:199,207,230,246,267` — so `all(a,opt(b),c)` returns
`[A,C]` when b fails but `[A,B,C]` when b matches, a length the static type cannot know) is real *in
principle* but **exercised by no live grammar**. The resolution is thus a **spec-hygiene decision, not
a consumer-forced one**:

- **ADOPT: type-level `DropUndefined<Values<Ps>>` + documented widening.** Sound to ship precisely
  BECAUSE no consumer exercises the length-variable case — the documented caveat ("`opt` inside `all`
  types to the b-absent shape; use `.then(x.opt())` for a length-stable optional") costs nothing live.
- **REJECT redefine-`all`-to-keep-undefined**: breaking, and there is no consumer to benefit.
- **REJECT `allOptional`**: a consumer-less substrate (Y.13 violation) — no grammar would call it.

**The falsifier that MUST still run** (even absent a consumer): a parse-that-side **self-test
differential** — construct `all(string("a"), string("b").opt(), string("c"))`, feed `"ac"` and
`"abc"`, assert runtime output `["a","c"]` vs `["a","b","c"]`, and assert the *typed* result matches
the documented widening (`Parser<["a","c"]>` accepted for both, with the caveat annotated). This
proves the typing is honest without needing a consumer, and belongs in the `.test-d.ts` + a runtime
sibling test.

## 3. Q3 — the full algebra + the `Parser<any>` INPUT leaks + per-combinator instantiation risk

The INPUT leaks (PT-E2's enumerated set) and the deeper OUTPUT leak (pass-1's systemic phantom-
`ParserFunction` finding) close together. Signatures + per-combinator instantiation cost:

| combinator | site | precise signature | per-node instantiation risk |
|---|---|---|---|
| `then` | parser.ts:81 | `then<S>(p: Parser<S>): Parser<[T,S]>` (DROP the `S\|T` widening) | O(1)/node; **chains nest linearly** — a 12-deep chain = 12 nested tuples (< cap) |
| `or` | parser.ts:105 | `or<S>(p: Parser<S>): Parser<T\|S>` (drop `S\|T`) | O(1); union flattens |
| `chain` | parser.ts:124 | `chain<S>(fn:(v:T)=>Parser<S>): Parser<S>` (drop `S\|T`) | O(1) |
| `opt` | parser.ts:234 | `opt(): Parser<T\|undefined>` | O(1) |
| `map` | parser.ts:146 | `map<S>(fn:(v:T)=>S): Parser<S>` | O(1) |
| `skip`/`next` | 189/212 | `skip<S>(p:Parser<S>):Parser<T>` / `next<S>(p:Parser<S>):Parser<S>` | O(1) |
| `many`/`sepBy` | 523/569 | `many():Parser<T[]>` / `sepBy<S>(s:Parser<S>):Parser<T[]>` | O(1) |
| `wrap`/`trim` | 392/480 | `wrap<L,R>(l,r):Parser<T>` / `trim():Parser<T>` | O(1) |
| `any` | leaf.ts:28 | `any<const Ps extends readonly Parser<unknown>[]>(...ps:Ps): Parser<ValueOf<Ps[number]>>` | O(N) union distribution — **cheap** |
| `all` | leaf.ts:154 | `all<const Ps …>(...ps:Ps): Parser<DropUndefined<Values<Ps>>>` | O(N) mapped tuple + O(N) tail-recursive filter |
| `fuseAll` | leaf.ts:179 | `(parsers: readonly Parser<unknown>[]): ParserFunction<Result>` | internal; `unknown` not `any` |
| `restore` | state.ts:79 | `restore(saved:{offset:number; value:T}): this` | O(1) |

The two erasures that STAY (documented, NOT counted leaks): (a) `ParserFunction<T>` remains the
type-erased IC pointer `(s:ParserState<any>)=>ParserState<any>` (parser.ts:14-16) — the megamorphic
`.parser(state)` call genuinely needs one invariant shape; the cure is **precise return annotations on
every public method** so inference flows through *signatures* while the *pointer* stays monomorphic
(types and codegen decoupled). (b) `MemoCell.value: unknown` (see §6) — the memo table is heterogeneous
across parser ids; erasure is lawful. Home the single `ExtractValue`/`Values`/`ValueOf`/`Union`/
`DropUndefined` in **`src/parse/types.ts`** (today duplicated identically at parser.ts:9-11 and inside
`all()` at leaf.ts:155-157). The arity-unrolled runtime arms (leaf.ts:39-57,183-251) are UNTOUCHED —
they gain positional types only; the V8 monomorphizations are preserved by measurement (LOCK-8's shape
audit owns that gate, not 8B).

## 4. Q4 — latex-paper NATIVE emit-identity (the budget corpus, done right)

The corpus is latex-paper compiled by **its own toolchain** (`npm test` = `vitest run`; its own `tsc`/
vite `dist`), NOT a single-file transpile proxy and NOT a 1.0.0-family overlay. The complication the
directive names is real: **latex-paper is on 0.7.1**, which predates the 1.0.0 API cut (`*Span`
excision, `chain()` C-16). So a tightened-signature candidate is an **0.7→candidate** jump that mixes
an *API-shape* delta with the *type-only* tightening — and emit-identity must isolate them:

- **Step (i): the API delta.** Build latex-paper `dist/` against installed 0.7.1 vs against a
  1.1.0-baseline (tightening OFF). This diff may be **non-empty** (the 0.7→1.x cut) and is *not* an 8B
  claim — it is the migration surface LOCK-8's consumer face owns. latex-paper imports only
  `Parser, regex, string, any` (all still exported at 1.x), so the expected API break is small, but it
  must be enumerated, not assumed.
- **Step (ii): the zero-cost proof (the 8B claim).** Build latex-paper `dist/` against 1.1.0-baseline
  vs 1.1.0-**tightened**. This diff MUST be **byte-empty** — the type plane is erased, so precise
  signatures cannot change latex-paper's emitted JS. A non-empty step-(ii) diff is a **defect** (a leak
  closed by a runtime cast, violating clause-v) and halts the family.

Mechanics (scratch only; non-clobber; parse-that READ-ONLY, lands via letter BBNF-PT-G): scratch-copy
parse-that at 1.1.0-baseline and 1.1.0-tightened, `npm pack` each, `npm install` the tarball into a
latex-paper scratch, run `tsc --noEmit --extendedDiagnostics --generateTrace ./trace` (record
instantiation count + check-time on latex-paper's real recursive grammar — this is the ONE surviving
real instantiation-budget number), `npm test`, and `diff -r` the two `dist/` trees.

---

## 5. Diagnostics typing — the `buildDiagnostic` deciding experiment (shared 8B/8D)

Pass-1's verdict (result-union ADOPT, `Parser<T,E>` expected-label BANK) is **reinforced** by the
supersession. The compile-time-label consumer pass-1 hedged on — keyframes' `ResolvedKeyframes.
diagnostics` — reached parse-that *through value.js*, which is now parse-that-free; so that downstream
**evaporated**, removing the last hypothetical demand for `Parser<T,E>`. It stays BANKED as a named
negative result: no consumer wants compile-time expected-labels, and it is the prime TS2589 offender.

The two surfaces are **not competitors**: the **field-triple** (`furthest`, `expected: string[]`,
`getLineAndColumn` — carried on the per-parse-scoped failed `ParserState`, populated into the W1 lazy
`suggestions`/`secondarySpans` fields under a scoped flag) is the **runtime carrier**; the
**result-union** `ParseResult<T> = {ok:true; value:T; offset:number} | {ok:false; diagnostics:
readonly Diagnostic[]}` is the **typed surface** whose failure arm *wraps* the triple. The deciding
experiment: after per-parse scoping, run `buildDiagnostic` against a returned failed state and assert
(a) `.ok===false` narrowing exposes exactly the field-triple, (b) **zero hot-path allocation when
diagnostics are off** (the lazy fields stay `undefined`; the scope is a null-or-field read on the
default path — no per-parse allocation), (c) `%HaveSameMap` holds between a diagnostics-off and a
diagnostics-populated state (the fixed-from-birth field claim). The result-union's Y.13 consumer is now
**latex-paper's runtime error-message construction** + the `.test-d.ts` narrowing suite, NOT a
compile-time label reader.

## 6. Plane-wide — packrat homing (the type of a memoized parser)

From `packrat.ts` (verified): memoization attaches at **`makeMemoized`** wrapping a `Parser<T>` into a
`Parser<T>` — **transparent to the type system**; a memoized parser's type is identically `Parser<T>`,
introducing **zero type-level surface and no `Parser<any>`**. The memo cell is shape-stable:
`interface MemoCell { ans: Answer | LR }` (packrat.ts:124-127), holding either a finished
`Answer{offset:number; value:unknown; isError:boolean}` (103-107) or an in-progress `LR` marker
(109-115). The **memo key is a primitive `number`** — `getCijKey = id*2^32+offset` (packrat.ts:79-100),
float64-safe with a fail-loud RangeError at the mantissa ceiling — so the key carries no hidden-class
churn and the table is `Map<number, MemoCell>` (packrat.ts:133). The one erasure — `Answer.value:
unknown` — is lawful: the memo table is heterogeneous across parser ids (one table serves every
`Parser<X>`), so a per-cell precise `T` is impossible and `unknown` (not `any`) is correct.

**Arming is opt-in and must be disclosed in EVERY perf table**: `PACKRAT_ARMED` (packrat.ts:156) is a
true no-op — `packratEnter`/`packratExit` are reference no-ops — until the FIRST `makeMemoized`
construction arms it permanently (a later memoized parser could be invoked anywhere). The default
LL(1) path pays nothing; no throughput gate rides arming. For 8B this closes the plane-wide question:
**the type of a memoized parser is `Parser<T>` unchanged; packrat is type-invisible.**

---

## 7. Prior art (citations, refined for pass 2)

- **TS 4.0 variadic tuple types** (microsoft/TypeScript #5453) — enables `Values<Ps>` length- and
  position-preserving mapped tuples; without it N-ary `all`/`then`-chain cannot type positionally.
- **TS 4.5 tail-recursion elimination in conditional types** — the ~1000 tail-recursive instantiation
  cap (#49459); non-tail nesting caps ~50. `DropUndefined` must be a tail-recursive accumulator.
- **TS 5.0 `const` type parameters** — literal inference (`string<const S>(s:S):Parser<S>`) so
  `string("+")→Parser<"+">`; a non-`const` default path retained for `string(runtimeVar)`.
- **ArkType** — existence proof that a full parser runs in the TS type system AND the cautionary tale
  (hand tail-recursive state machines to dodge TS2589; obsess over hover/editor perf). **Zod v4** — its
  rewrite targeted *checker* instantiation count; the direct precedent for 8B's instantiation-budget
  gate. **HotScript / Yallop-White "Lightweight higher-kinded polymorphism" (FLOPS 2014)** — the
  defunctionalized-HKT escape hatch IF naive signatures hit limits; over-engineering for latex-paper's
  scale (verdict: not needed — recursion is bounded at the `Parser<Node>` seam).

## 8. Named risks & counterexamples (pass-2)

- **R0 (NEW, dominant) — the consumer-divorce voids every value.js figure.** *Counterexample:* a
  synthesis session re-quotes "instantiation ≤1.5× at value.js's 59 `all()` sites." Those sites do not
  exist (value.js 4.0.0, empty deps). Cure: re-home every figure to latex-paper native + `.test-d.ts`;
  strike unproduceable value.js numbers; get owner confirmation the divorce is intended.
- **R1 — `.then`-chain nesting, not `all`, is the TS2589 surface.** latex-paper has no `all()`; its
  risk is 12-deep `.then` chains + recursive grammar. Mitigation is already structural (recursion
  breaks at `Parser<Node>` annotations); the budget measurement must target `.then` depth, not `all`
  arity.
- **R2 — 0.7→candidate emit diff misattributed as a zero-cost failure.** Cure: the two-step diff (§4)
  isolates the API cut (step i, allowed non-empty) from the type-only tightening (step ii, MUST be
  empty).
- **R3 — `S|T` removal / precise `any()` rejects a latex-paper site that leaned on the slop.** The
  falsifier is latex-paper's `npm test` + `tsc` against the candidate; a real break escalates
  minor→major, decided not assumed. latex-paper uses `any()` (union) heavily — precise `ValueOf<Ps
  [number]>` is a *superset* for well-typed sites but must be run, not asserted.
- **R4 — `const` over-narrowing** breaks `string(dynamicVar)`; a non-`const` default overload is
  mandatory. **R5 — hover/IntelliSense explosion** on nested-tuple types; mitigate with display
  aliases, not mandatory literal precision. **R6 — non-clobber**: parse-that READ-ONLY; scratch-copy
  prototype; land only via letter BBNF-PT-G.

## 9. What a prototype MUST test (to move family 8B)

1. **P1 — precision + zero-cost + budget on the REAL corpus.** Apply the tightened plane to a scratch
   parse-that; `npm pack`→install into scratch **latex-paper**; assert (a) strictly more precise
   inferred types at its `any()`/`.then()`/`.opt()` sites, (b) **step-(ii) `dist/` diff byte-empty**
   (zero runtime cost), (c) `tsc --noEmit` clean, (d) `--extendedDiagnostics`/`--generateTrace`
   instantiation count ≤ named ceiling and check-time ≤ named ms — **the one surviving real
   instantiation number**, taken on latex-paper's recursive grammar, signature disclosed.
2. **P2 — the `opt`-in-`all` self-test differential** (§2): parse-that-side, no consumer — prove the
   drop-undefined runtime for b-present/b-absent matches the documented `DropUndefined` widening.
3. **P3 — the `.test-d.ts` suite** (`expect-type` + `@ts-expect-error`): `any(string("a"),string("b"))
   →Parser<string>` (or literal union); a `@ts-expect-error` on a genuinely wrong shape STILL errors
   (precision both ways); RED on 0.7.1/1.0.0, GREEN after.
4. **P4 — the divorce confirmation.** Independently re-verify value.js 4.0.0 imports zero parse-that
   and keyframes.js is transitively parse-that-free, so the gestalt does not re-inherit a dead consumer
   premise. This is a *disqualifying precondition* check, run before any 8B number is banked.

## 10. Consumer contract (Y.13 — restated under the supersession)

The type plane's consumers are now precisely: **latex-paper 0.2.1** (`any`/`.then`/`.opt`/`.map`/
`.skip`/`.next`/`.many`/`.sepBy` across 8 grammar modules, 183 sites — the live budget corpus),
**parse-that's own `parsers/json.ts`+`csv.ts`** (the only surviving `all()`/`dispatch` self-use), and
the **`.test-d.ts` suite**. value.js and keyframes are NO LONGER type-level consumers of parse-that.
8B remains the strongest form of PT-E2: it closes the enumerated INPUT `Parser<any>` leaks AND the
systemic OUTPUT leak at the phantom-`ParserFunction` boundary, threads literal precision, and types the
diagnostic result-union — carrying the grammar in the type system while the emitted JS and the V8 hot
shapes stay byte-for-byte unchanged. The falsifier is latex-paper native green against a packed
candidate + the empty step-(ii) `dist/` diff — not a value.js suite that no longer imports the library.
