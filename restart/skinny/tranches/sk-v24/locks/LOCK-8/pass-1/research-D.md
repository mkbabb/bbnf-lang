# LOCK-8 family 8D — CONSUMER-CONTRACT-FIRST (pass 1, research D)

The TS backbone designed backward from its three consumers. The thesis of 8D is that the API,
the diagnostics shape, the semver contract, and the migration artifact are all *derivable* from
what value.js, latex-paper, and keyframes.js actually import and read — not from parse-that's
internal ambitions. This report pins the ground-truth consumer surfaces (all verified by
`grep`/`Read` against the live repos, not R4's snapshot), reads them backward into an API+semver
contract, and names the counterexamples a prototype must defeat. Ground truth diverges materially
from R4 §7 and from PT-3's W0 assumptions — those divergences are the first deliverable.

## 0. Ground-truth supersessions (constitution clause ix — named, not silent)

R4 §7 and PT-3.md §1.2/§3-W0 encode a consumer census that no longer holds. The live repos say:

- **value.js is 4.0.0, not 3.1.0** (`/Users/mkbabb/Programming/value.js/package.json:3`). R4's
  "value.js 3.1.0 pins `^1.0.0`" is stale.
- **value.js declares NO parse-that dependency.** `dependencies: {}`, no devDep/peer/optional
  parse-that (`package.json` — verified empty). Yet `node_modules/@mkbabb/parse-that` is present
  at **1.0.0** and `src/` imports `@mkbabb/parse-that` at ≥9 sites. The reconciliation:
  **value.js *bundles* parse-that into its own `dist/`.** `vite.library.ts:13`
  `libraryExternal: string[] = []` — value.js externalizes *nothing*; `rollupOptions.external`
  (`vite.config.ts:188`) points at that empty list. parse-that is compiled into value.js's
  published library, tree-shaken (`sideEffects:false`). The dep was dropped from `package.json`
  because it is a *build-time* input, not a runtime dependency. **This is the single most
  consequential 8D fact and it inverts PT-3's semver model** (§4 below).
- **latex-paper is 0.2.1 pinned `^0.7.1`, installed 0.7.1** (`node_modules` verified). The
  0.7→1.1 jump is real and untested — PT-3 W0 has this right; 8D sharpens the exact break surface
  (§5).
- **keyframes.js 6.0.0 has no direct dep** — transitive via value.js, and because value.js bundles
  parse-that, keyframes reaches 1.1.0 only after value.js *rebuilds and republishes* (§4).

These three supersessions are the W0 handshake's actual work; PT-3's "value.js re-locks 1.0.0→1.1.0
`npm test`" (§4-W5) is not a valid gate against a bundled dependency (§4.3).

## 1. The public API the consumers actually use (the whole contract)

Read backward across all three, the *entire* semver-critical surface is nine symbols. Everything
else parse-that exports (packrat, debug, split, lazy, csv/json domain parsers) is **unused by any
consumer** and is therefore free to churn.

| symbol | value.js | latex-paper | evidence |
|---|:-:|:-:|---|
| `Parser` (class + `new Parser((state)=>…)`) | ✓ | ✓ | value.js `parsing/utils.ts:149,159,237,451,484,501`; latex `grammar/math.ts:10`, `text.ts:19,31,48,63`, `primitives.ts:38,71,113,144,162` |
| `ParserState` (`new ParserState<T>(source)`) | ✓ type | ✓ **constructs** | value.js `parsing/utils.ts:2` (type); latex `grammar/document.ts:170` `new ParserState<LatexNode\|null>(source)`, `primitives.ts:5` |
| `all` | ✓ (≥59 sites) | — | value.js `parsing/{index,math,units,color,timeline/*}.ts` |
| `any` | ✓ | ✓ | value.js `parsing/index.ts:1`; latex `grammar/{text,environments,commands,document}.ts`, `bibtex/parser.ts:7` |
| `dispatch` | ✓ | — | value.js `parsing/index.ts:1,394` (CSS function-name LUT) |
| `regex` | ✓ | ✓ | ubiquitous both |
| `string` | ✓ | ✓ | ubiquitous both |
| `whitespace` | ✓ | — | value.js `parsing/{math,index,units,color,timeline}.ts` |
| `mergeErrorState` | ✓ **deep** | — | value.js `parsing/utils.ts:1,502` — calls `mergeErrorState(state, message)` directly |

**Two consumers reach *below* the combinator layer.** value.js and latex-paper both construct
`new Parser((state)=>…)` and `new ParserState<T>(source)` by hand, and value.js drives via
`parser.parseState(input)` then reads state fields *post-hoc* (`parsing/utils.ts` — 3 `.parseState(`
sites vs 1 `.parse(`). So the contract is not "combinators only"; it includes the **raw
`ParserState` constructor arity, the `ParserFunction` callback signature `(state)=>state`, and the
`mergeErrorState(state, msg)` free function.** Any 8C-style "minimize hand-written TS / regenerate
from a spec" plan must preserve these three low-level seams verbatim or it breaks both consumers.

## 2. The diagnostics contract — consumers already own the shape

The pivotal 8D finding for PT-E1: **parse-that's internal diagnostic vocabulary
(`Suggestion`/`SecondarySpan`/rich `Diagnostic`) is not what any consumer wants.** value.js has
independently authored two diagnostic contracts and maps *down* to them from a handful of raw
`ParserState` fields.

**value.js's post-hoc reader** (`parsing/utils.ts:531-570`, `buildDiagnostic`) reads exactly:
`state.offset`, `state.furthest`, `state.expected` (`string[]`), `state.getLineAndColumn(offset)`,
plus `state.isError`, `state.value`. It converts these into `ParseDiagnostic`
`{message, offset, line, column, expected?: string[], input}` and a pluggable `OnParseError` sink
(`utils.ts:527`, `tryParse`/`parseResult` at `:560-600`). It emits `expected` **only** when
`state.expected` is present (exactOptional guard, `utils.ts:552`).

**value.js's v4 target shape** (`src/v4/css/types.ts:9-27`): `ParseIssue`
`{code, start, end, expected: readonly string[], actual: string|null}` and
`ParseResult<T> = {ok:true,value,diagnostics:[]} | {ok:false,diagnostics:[ParseIssue,...]}`,
constructed in `v4/css/grammar.ts:44-70`. `diagnostics` carries a **flat coded issue with a
`string[]` expected set** — never a `Suggestion` or a `SecondarySpan`.

**keyframes.js** (`src/animation/validate.ts:99-169`) has its own `Diagnostic[]` with a `code`
field (`EMPTY_PARSE`, `:155`), populated by the `resolveKeyframes` adapter from value.js's channel —
a *third-order* flat-coded diagnostic.

**The consumer-derived verdict for PT-E1 (W2):**
1. The per-parse diagnostic surface consumers consume is the **`ParserState` field triple**
   (`furthest`, `expected: string[]`, `getLineAndColumn`) *readable on the returned state after
   `parseState()` returns* — not a new rich object. PT-E1 must make *these* per-parse-scoped
   (today gated by the module-global `diagnosticsEnabled`, R4 §5 / `utils.ts:6`), keeping the
   field names and types byte-stable.
2. The natural public form is a parse option — `.parse(src, {diagnostics:true})` or a scoped
   `withDiagnostics(fn)` — whose **returned `ParserState` still exposes `.expected/.furthest/
   .getLineAndColumn`** so value.js's existing `buildDiagnostic` keeps working unedited. A design
   that returns a *fresh* `{ok,value,diagnostics}` result object and tears the fields off the state
   breaks value.js's post-hoc reader (R-8D2).
3. **Do not** promote `Suggestion`/`SecondarySpan` into the consumer-facing diagnostic — they are
   the W1 lazy-alloc substrate (PROVE-11), read only by `debug.ts`; no consumer touches them
   (VERIFY §4). They stay internal.

This is Y.13 satisfied *by the real consumer*: W1's lazy `expected`-symmetric fields are the
substrate; value.js's `buildDiagnostic` → `ParseIssue` → keyframes `Diagnostic` chain is the live
consumer, not a synthetic test.

## 3. The hard behavioral contract (a born-RED gate consumers already encode)

value.js's `parsing/color/color.ts:720-740` records a **behavioral invariant with a version
history**: parse-that ≤0.8.2 fired `console.error(state.toString())` on every *expected* failure
(a per-parse console-I/O leak on every custom color name); the `^0.9` re-pin fixed it because
"diagnostics are OFF by default … only emit to console under an explicit `enableDiagnostics()`
value.js never calls." So the contract PT-E1/W2 must not regress:

- **Default parse path: zero console I/O, zero diagnostic allocation, `diagnosticsEnabled`
  effectively off.** value.js *never* opts in. This is exactly PROVE-11's lazy-field win (default
  path allocates nothing) plus a no-console guarantee.
- W2 *deletes* the module-global `diagnosticsEnabled` (PT-3 precept #6). The replacement per-parse
  scope must default to **off**, and the "off" path must be a null-scope field read — no allocation,
  no console (R-8D2/R2). value.js's own comment is the oracle: a regression here reintroduces the
  0.8.2 leak value.js explicitly re-pinned away from.

## 4. Semver + migration as first-class deliverables (the 8D backbone)

### 4.1 The bundling inversion
Because value.js **bundles** parse-that (`libraryExternal=[]`, §0), the propagation graph is:

```
parse-that 1.1.0 tarball
    └─(build-time bundle)→ value.js dist rebuild → value.js republish
                               └─(build-time bundle)→ keyframes.js (transitive) picks up 1.1.0
```

A bundled dependency means **`npm test` in value.js against its checked-in `dist/` tests the stale
bundled 1.0.0**, not the 1.1.0 candidate. PT-3's W5 gate "value.js re-locked 1.0.0→1.1.0 `npm test`"
(PT-3 §4-W5) is therefore under-specified. The correct 8D gate:

### 4.2 The corrected consumer-suite gate
1. Pack the 1.1.0 candidate (`npm pack` in `parse-that/typescript`).
2. In the value.js scratch: `npm install` the tarball **into a declared dep or the bundle input
   path**, then **`npm run build`** (rebuild `dist/` so the new parse-that is compiled in), *then*
   `npm test`. Testing without the rebuild is a false green.
3. keyframes.js: install the *rebuilt* value.js tarball, then `npm test`. It never sees parse-that
   directly.
4. latex-paper: it *does* declare parse-that (`^0.7.1`, runtime), so its gate is a genuine version
   bump + `npm test` (§5).

### 4.3 The semver verdict, per consumer
- **value.js (bundled):** parse-that 1.0→1.1 is *invisible to value.js's public semver* (it is an
  internal build input). The break risk is entirely at value.js *build* time: (a) `mergeErrorState`
  must remain exported from the top barrel after W2 lifts diagnostics into `diagnostics/`
  (R-8D1); (b) `ParserState.expected/furthest/getLineAndColumn` must stay readable post-parse
  (R-8D2); (c) inference tightening (W3) must not reject value.js's ~59 `all()` sites or its
  `new Parser<T>((state: ParserState<any>)=>…)` hand-constructions (R-8D3). If all three hold,
  value.js absorbs 1.1.0 with **no source edit and no semver bump of its own** — the ideal outcome
  the 8D gate proves.
- **latex-paper (runtime dep):** a real `^0.7.1 → ^1.1.0` bump; semver-visible; migration artifact
  required (§5).
- **keyframes.js:** no semver surface; rides value.js's republish.

### 4.4 The migration artifact (deliverable, not a note)
The tranche ships a `MIGRATION.md` fragment enumerating, per consumer, the exact symbol delta and
the mechanical steps: value.js = rebuild-only (no code change) *if* §4.3 holds; latex-paper =
version-bump + the 0.7→1.0→1.1 break table (§5); keyframes = value.js-republish-only. This is 8D's
"semver and migration are first-class deliverables" made concrete.

## 5. The latex-paper 0.7→1.1 jump — exact break surface

latex-paper imports **only** `Parser, ParserState, regex, string, any` (7 grammar sites, §1). The
1.0.0 breaking cut was `*Span` excision (S.H2) + `chain()` C-16 falsy-seed fix. None of latex-paper's
imports is `*Span` or `chain`, so the *likely* result is GREEN — but three seams need the W0
dry-run because they are *constructor/callback* seams, not combinator calls:

1. **`new ParserState<LatexNode|null>(source)`** (`document.ts:170`) — the 0.7 `ParserState`
   constructor arity. If the `*Span` excision changed the ctor signature (e.g. removed a span arg),
   this breaks at the **0.7→1.0** step and would be *mislabeled* a 1.1.0 break. PT-3 R4 counterexample;
   8D pins the exact line.
2. **`new Parser((state)=>…)`** callback shape (`math.ts:10` et al.) — the `ParserFunction<T>`
   signature. W3's inference tightening (`parser.ts:14`) must keep the *implementation-boundary*
   `(state)=>state` callable un-narrowed, or latex's untyped `(state)=>` callbacks fail to check.
3. **`any(...)` return shape** — C-16 touched `chain`, not `any`; expected safe, but the dry-run
   confirms.

W0 must run the **three-step** matrix (0.7→1.0 alone, then 1.0→1.1) so any red is attributed to the
cut that owns it (PT-3 R4). 8D adds: the dry-run installs a *packed* tarball at each step and runs
latex-paper's real `vitest run`, not a synthetic import.

## 6. Prior art (consumer-contract-first API & migration discipline)

- **Semantic Versioning 2.0** + **npm bundledDependencies** semantics: a bundled dep changes the
  break surface from "consumer's declared range" to "consumer's build inputs" — the exact value.js
  situation. cf. npm docs on `bundleDependencies` and tree-shaking (`sideEffects:false`).
- **rustc/rust-lang crater**: run the *real* downstream test suites against a candidate before
  release; the 8D consumer-suite gate is a three-repo crater run. The discipline (measure against
  actual consumers, enumerate breaks, attribute to the owning change) is directly transposable.
- **TypeScript's own `@ts-expect-error` / `expect-type` type-level testing** (DefinitelyTyped
  `dtslint`): the W3 inference gate should be a `*.test-d.ts` suite that pins value.js's *actual*
  `all(...)`/`any(...)`/`dispatch(...)` shapes, not synthetic ones.
- **Parsimmon's `.parse()` result shape** (`{status, value}`) — value.js explicitly mirrors it
  (`parsing/utils.ts:587` "Equivalent to Parsimmon's `.parse()`"). The consumer-facing result idiom
  is already Parsimmon-shaped; PT-E1's `{ok,value,diagnostics}` option should compose with, not
  replace, that idiom.
- **Elm/`elm-review` upgrade guides** and **semantic-release**: migration-as-artifact (machine-
  checked, per-symbol) is the SOTA for library backbones with pinned downstreams.

## 7. Named risks & counterexamples (each a prototype obligation)

- **R-8D1 — the `mergeErrorState` re-export vanishes.** W2 lifts diagnostics into
  `src/parse/diagnostics/collect.ts` (PT-3 §4-W2) and forgets to re-export `mergeErrorState` from
  the top barrel (`index.ts`). value.js `parsing/utils.ts:1` `import { mergeErrorState } from
  "@mkbabb/parse-that"` breaks — **silently, only at value.js build time** (bundled). *Gate:* a
  barrel-surface proof that `mergeErrorState` (and the eight other symbols of §1) still resolve from
  the package root `.d.ts` after the split.
- **R-8D2 — the per-parse scope tears down state fields.** W2 opens/closes the DiagnosticScope in
  `parseState`'s try/finally (PT-3 §4-W2) and nulls `state.expected` on close. But value.js drives
  via `parser.parseState(input)` and reads `state.expected/furthest/getLineAndColumn` **after** the
  call returns (`utils.ts:531-570`). If close tears the fields off, `buildDiagnostic` reads
  `undefined` → value.js emits diagnostics with no `expected`. *Counterexample:* scope-close must
  *finalize* the returned state's fields, not clear them. *Gate:* run value.js's actual
  `buildDiagnostic` (copied into the prototype) against a returned failed state and assert a
  populated `ParseIssue`.
- **R-8D3 — inference tightening rejects a consumer.** W3 narrows `all<Array<Parser<any>>>` →
  `ReadonlyArray<Parser<unknown>>` (PT-3 §4-W3). value.js's ~59 `all()` sites and its
  `new Parser<T>((state: ParserState<any>)=>…)` hand-constructions rely on the `any` variance.
  A narrowing that rejects them is a *value.js build break* mislabeled a parse-that "improvement."
  *Gate:* the W3 `*.test-d.ts` suite pins value.js's real call shapes; the value.js `tsc` (in the
  rebuild of §4.2) is the falsifier.
- **R-8D4 — the false-green consumer gate.** CI runs `npm test` in value.js against the checked-in
  `dist/` (stale 1.0.0) and reports GREEN without ever compiling 1.1.0 in. *Counterexample:* the
  gate omitted the `npm run build` step (§4.2). *Gate:* the consumer-suite script must rebuild
  value.js `dist/` from the packed tarball before `vitest`, and assert the bundled version string
  in `dist/` is 1.1.0.
- **R-8D5 — the console-leak regression.** W2's per-parse scope defaults *on*, or its "off" path
  still hits a `console.error(state.toString())` sink, reintroducing the ≤0.8.2 leak value.js
  re-pinned away from (`color.ts:728`). *Gate:* a test that parses a deliberately-failing custom
  color name with diagnostics unset and asserts **zero** `console.*` calls (spy) and zero array
  allocation on the default path.
- **R-8D6 — the undeclared-dependency hazard.** value.js `package.json` declares no parse-that dep
  at all (§0); the semver contract is *implicit* in the bundle. A W0 that assumes a declared
  `^1.x` range (PT-3 §3-W0) tests a fiction. *Gate:* W0 first *establishes* how value.js resolves
  parse-that at build time (bundle input path / phantom install) and pins the actual mechanism
  before any suite runs.

## 8. What a prototype must test to move family 8D

A single `prototypes/PROVE-8D` scratch (parse-that `typescript/` @ 1.0.0 pristine + scratch copies
of all three consumers, node_modules symlinked — the PROVE-11 recipe) proving:

1. **Consumer-shaped diagnostics oracle.** Add the per-parse `withDiagnostics`/`{diagnostics:true}`
   scope (W2). Copy value.js's `buildDiagnostic` (`utils.ts:531-570`) *verbatim* into the harness,
   run it against a returned failed `ParserState`, and assert it yields a populated `ParseIssue`
   (`{code,start,end,expected:string[],actual}`). The oracle is the *consumer's own mapper*, not a
   synthetic assertion (defeats R-8D2, proves §2.1-2).
2. **The isolation test using value.js's real nesting.** Reproduce value.js's speculative-parse
   pattern (`color.ts` map-first + rich fallback, a nested parse) and prove the inner parse's
   diagnostics do not contaminate the outer under per-parse scope (PT-3 §4-W2 born-RED).
3. **The console/allocation default-path gate** (R-8D5): failing parse, diagnostics unset →
   zero `console.*`, zero diagnostic alloc (a `--expose-gc` B/op read, PROVE-11 method).
4. **The barrel-surface proof** (R-8D1): after the `diagnostics/` split, all nine §1 symbols —
   especially `mergeErrorState`, `ParserState` ctor, `Parser` ctor — still resolve from the package
   root and value.js/latex `tsc` cleanly.
5. **The three-repo bundled-crater run** (§4.2, defeats R-8D4): pack 1.1.0 → rebuild value.js dist →
   `vitest` (value.js) → rebuild keyframes → `vitest` (keyframes); separately, latex 0.7→1.0→1.1
   three-step tarball dry-run with the `document.ts:170` ctor and `math.ts:10` callback as the named
   watch-points (§5, defeats R-8D5/R-8D6).
6. **The type-level inference suite** (`*.test-d.ts`, W3) pinning value.js's actual `all()`/`any()`/
   `dispatch()` shapes and its `new Parser<T>((state: ParserState<any>)=>…)` construction — precision
   must be a *superset* for these real sites, not a rejection (defeats R-8D3).

The family advances when (1)-(6) are GREEN in one scratch and the migration artifact (§4.4)
enumerates a zero-source-edit path for value.js and a pinned break table for latex-paper. That
artifact — three real suites green against a rebuilt-from-candidate world plus a per-consumer symbol
delta — *is* the 8D deliverable: the backbone proven by the survival of its consumers.
