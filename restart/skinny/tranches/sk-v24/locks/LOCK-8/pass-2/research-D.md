# LOCK-8 pass-2 — research-D — family 8D CONSUMER-CONTRACT-FIRST

*The TS backbone designed backward from its consumers. Every figure below is a static
fact of the checked-out trees (grep/AST/lockfile), not a runtime measurement; where a
number would be runtime it is named as un-run and handed to the prototype list. parse-that
is READ-ONLY; prototyping is against scratch copies; the only sanctioned artefact into the
repo is coordination letter BBNF-PT-G.*

---

## 0. The decisive inversion — the consumer census has flipped

The consumer set the 8D charter and the PT-3 tranche were designed around no longer exists.
Two of the three named consumers have **migrated off parse-that**; the third is the sole
survivor, frozen two minor-cuts behind.

**value.js — MIGRATED OFF (parse-that-FREE).**
- `value.js/package.json` (name `@mkbabb/value.js`, version **4.0.0**) has **no
  `dependencies` key at all** — only `devDependencies`. parse-that is absent from its
  runtime graph (`python3 -json`: `'dependencies' in d == False`).
- `src/parsing/{color,stylesheet,timeline}/` are **empty stub directories** (64 B each).
- CSS parsing was re-homed **hand-written** into `src/v4/css/grammar.ts` (29 972 B) +
  `src/v4/css/stylesheet.ts` (44 745 B), importing only `../result`, `../color/model`,
  `../value`, `./types`, `./named-colors` — zero combinators (`grammar.ts:1-36`).
- `src/subpaths/math.ts:2` "`@mkbabb/value.js/math` … parse-that-FREE"; `src/subpaths/
  transform.ts:4` "a pure leaf (zero parsing, zero parse-that)".
- parse-that's **own** source records the departure: `typescript/src/parse/utils.ts:150-156`
  — the imperative byte-scanners were "Kept after the CSS grammar itself left for value.js
  (D2/D3)." The migration is settled on both sides.

**keyframes.js — MIGRATED OFF (transitively severed).**
- `keyframes.js/package.json`: no `@mkbabb/parse-that` dep (never had a direct one).
- `src/animation/internal/leaves.ts:9-10`: "The subpath is `parse-that`-FREE (a 2-module /
  ~1.4KB graph, 0 CSS-grammar / 0 parse-that / 0 engine modules — VERIFIED)." Its only route
  to parse-that was transitive through value.js; value.js dropping it severs the chain.

**latex-paper — SOLE surviving consumer, frozen at 0.7.1.**
- `latex-paper/package.json:38` `"@mkbabb/parse-that": "^0.7.1"` → `node_modules/@mkbabb/
  parse-that/package.json` resolves **0.7.1** (predates the 1.0.0 cut).
- **8 import sites**, surface = exactly `{Parser, ParserState, regex, string, any}`:
  `src/grammar/commands.ts:9`, `environments.ts:6`, `text.ts:5`, `document.ts:5`
  (`+ParserState`), `primitives.ts:5` (`Parser, ParserState, regex, string`), `math.ts:5`
  (`Parser, regex, string`), `compile.ts:7` (`type Parser`), `bibtex/parser.ts:7`.
- **Dual-version hazard:** latex-paper also carries a **nested parse-that 0.6.0** via
  `@mkbabb/bbnf-lang@^0.1.1` (`package.json:37`; `package-lock.json:582-589` →
  `node_modules/@mkbabb/bbnf-lang/node_modules/@mkbabb/parse-that@0.6.0`). Two parse-that
  instances coexist in one tree.

**Consequence for the family.** The "three-repo bundled-crater harness" the directive treats
as the deliverable is over-specified against a dead census. It collapses to **one positive
consumer gate (latex-paper) + two negative guards (value.js / keyframes.js must remain
parse-that-free)**. The API is no longer "defined backward from value.js's 59 `all()` sites";
it is defined by latex-paper's **5-symbol combinator-layer surface** — the sharpest possible
contract. This is both the family's simplification and its Y.13 crisis (§9).

---

## 1. Build-time resolution — PINNED (directive #1)

The premise "value.js bundles parse-that; `libraryExternal=[]`" is now **false at its root**:
- value.js's production build (`vite.config.ts`, `mode === "production"`) is Rolldown-based
  with `rolldownOptions.external = libraryExternal`, and `vite.library.ts:14` declares
  `export const libraryExternal: string[] = []`. So value.js bundles **every** import into
  its seven-subpath `dist/` — nothing externalized. **But it imports zero parse-that**, so
  there is no bundled parse-that to be stale. The "checked-in bundle is a false green" hazard
  is **moot for value.js**: grepping value.js's checked-in `dist/` for parse-that yields a
  *true-green* absence, not a false one.
- keyframes.js: no dependency; nothing to resolve.
- latex-paper: `build: "tsup && vue-tsc"` (`package.json:30`); the suite is `vitest run`
  over `tests/**/*.test.ts`, which resolves parse-that from `node_modules` (**source-
  resolution, not bundled**). latex-paper does ship a checked-in `dist/` (`chunk-*.js`), but
  its own suite runs against `src/`, so that dist is not the parse-that gate surface.

**The genuine false-green** is therefore singular and lives in latex-paper's node_modules:
the pinned **0.7.1 tarball**. A crater that does not reinstall the candidate tests 0.7.1, not
the cut. The mandatory rebuild step is real, but it is a parse-that-dist rebuild + a
latex-paper `npm i <candidate-tarball>`, **not** a value.js dist rebuild.

---

## 2. The crater — re-scoped, with the mandatory rebuild re-homed (directive #2)

`scratch/pt-consumer-crater.sh` (harness only; never landed in any repo). Three legs:

- **(a) latex-paper — POSITIVE gate.** Scratch-copy latex-paper. Pack parse-that's working
  tree: `(cd scratch/parse-that/typescript && npm run build && npm pack)`. **Mandatory
  step:** `rm -rf node_modules/@mkbabb/parse-that && npm i ../parse-that-<candidate>.tgz`
  (skipping this is the false green — the pinned 0.7.1 silently passes). Then `npm test`
  (`vitest run`) **and** `npx vue-tsc --noEmit` (the type-break axis — §5). Also decide the
  nested-0.6.0 dedupe (§5, R3).
- **(b) value.js — NEGATIVE guard.** No suite against a candidate (nothing to break).
  Mechanical tripwire: `! grep -rq "@mkbabb/parse-that" src/` **and** `python3 -c
  "'dependencies' in json.load(...) and 'parse-that' in json.dumps(...)"` == false. Guards
  against silent re-introduction.
- **(c) keyframes.js — NEGATIVE guard.** Same shape; assert the `leaves.ts:9-10`
  parse-that-free invariant holds.

The crater is **constructable now** — resolution is pinned. It is single-legged on the
positive axis, which is the honest state of the world.

---

## 3. Mechanical import oracle — RAN (directive #3)

`grep -rnhoE "from ['\"]@mkbabb/parse-that[^'\"]*['\"]"` + symbol extraction over `src`+`dist`
of all three repos:

| repo | hits (src+dist) | subpath imports | symbol union |
|---|---|---|---|
| value.js | **0** | — | ∅ |
| keyframes.js | **0** (only prose comments) | — | ∅ |
| latex-paper | **8** (all top-level `@mkbabb/parse-that`) | **none** (`/core`,`/diagnostics`,`/packrat`,`/utils` never reached) | `{Parser, ParserState, regex, string, any}` — **5** |

**Findings, mechanical not prose:**
- **No consumer reaches below the combinator layer.** All 8 latex-paper imports hit the root
  entry; zero reach the `./core`/`./diagnostics`/`./packrat`/`./utils` subpaths.
- **No consumer imports an unlisted symbol.** The live surface is exactly 5 symbols, all
  combinator-layer.
- **The R4 nine-symbol census is STALE.** `{Parser, ParserState, all, any, dispatch, regex,
  string, whitespace, mergeErrorState}` was **value.js's** surface; value.js is gone. `all`,
  `dispatch`, `whitespace`, `mergeErrorState` now have **zero external consumers**.
- The entire packrat / diagnostics / dispatch / `all`/`fuseAll` / `sepBy` / `many` / `chain`
  surface is **consumer-less** from outside parse-that (§9 Y.13).

---

## 4. PT-E1 diagnostics mechanism — DESIGNED (directive #4)

**The field triple already lives per-parse on the returned failed state.** The parse epoch
builds it in `parseStateInner` (`parser.ts:56-65`): `const errorState = new ParserState(val,
undefined, furthest, true); errorState.expected = state.expected; errorState.furthest =
furthest;`. `getLineAndColumn(offset)` is an instance method (`state.ts:127`). `furthest` is
a constructor field, **always populated**. So two legs of the triple (`furthest`,
`getLineAndColumn`) are **already per-parse and always-on**; only `expected`/`suggestions`
**population** is gated.

**The only module-global residue** is the populate-gate boolean: `utils.ts:6`
`let diagnosticsEnabled = false`, read in `mergeErrorState` (`utils.ts:33`),
`addSuggestion` (`utils.ts:51`), `addSecondarySpan`. The furthest-offset / suggestions /
secondarySpans **storage** is already threaded onto the per-parse `ParserState`
(`state.ts:39-46`; `utils.ts:19-27` documents the intent as reentrant/interleave-safe).

**Mechanism (zero hot-path allocation when off).** Replace the module global with a
**primitive boolean field on the already-per-parse `ParserState`** — `state.diag: boolean`
— set once from a parse option (`.parse(src, { diagnostics: true })`) and threaded to child
states. `mergeErrorState(state, label)` reads `state.diag` instead of `diagnosticsEnabled`.
Cost when off: **one boolean field read** already in the state's monomorphic hidden class —
no allocation, no new object, no hidden-class transition (a boolean field fixed at
construction cannot transition). The `expected`/`suggestions` arrays allocate lazily only
under `state.diag` (PROVE-11's optional-field discipline supplies the shape-stable lazy
fields; note they are `= []` **non-optional at HEAD** `state.ts:44-45`, so that lever is
still un-landed).

**Why a boolean field beats a scope object.** A `DiagnosticScope` object hung off
`ParserContext` or allocated per `.parse()` re-introduces a per-parse allocation on the
diagnostics-off path — the exact regression named as a risk. A boolean field on a state that
is *already* allocated per-parse adds nothing. The "scope" is the state; no new lifetime is
needed. Deletion proof for the global (precept #6): `rg 'let diagnosticsEnabled' src/` → 0.

---

## 5. latex-paper migration matrix 0.7.1 → 1.0 → 1.1 — ATTRIBUTED (directive #5)

latex-paper imports **only** `{Parser, ParserState, regex, string, any}` (§3). The
three-step attribution, each break traced to the cut that owns it:

- **0.7.1 → 1.0.0.** Breaking cuts: `*Span` excision (S.H2, commit `043c4d1`) + `chain()`
  falsy-seed C-16 fix. latex-paper imports **no** `*Span` symbol and **no** `chain` — so the
  1.0.0 cut touches **zero** of its 5 symbols. **Expected GREEN, but must be RUN** (an
  untested 0.7→1.0 jump: a behavioral change in the shared `regex`/`string`/`any`/`Parser`/
  `ParserState` semantics between 0.7.1 and 1.0.0 could still bite). **Attribution rule:**
  run 0.7.1→1.0.0 **alone** first so a 1.0.0-era break is never mislabeled a 1.1.0
  regression.
- **1.0.0 → 1.1.0.** The 1.1.0 changes (lazy diag fields, per-parse diagnostics API,
  inference tightening) touch: (i) the diagnostics surface — latex-paper reads **none** of
  it, GREEN at runtime; (ii) the **inference tightening** (`any<T extends Array<Parser<any>>>`
  → precise, `leaf.ts:28`). latex-paper calls `any(...)` at `commands.ts:9`,
  `environments.ts:6`, `text.ts:5`, `document.ts:5`, `bibtex/parser.ts:7`. **If any of those
  sites relied on the `any` widening to typecheck, the tightening reddens `vue-tsc`.** This
  is the **one real 1.1.0 consumer risk** — a *type* break, not runtime. It is caught only by
  running `tsup && vue-tsc` in the crater, never by `vitest` alone.
- **Dual-instance hazard (owns a MIGRATION.md row).** Bumping the direct dep to 1.1.0 leaves
  the nested **0.6.0** (via bbnf-lang) in place — two parse-that instances. If a `Parser`
  value or an `instanceof` crosses the latex-paper ↔ bbnf-lang boundary, this mirrors the
  dual-instance fragility value.js deduped for Vue/reka-ui (`value.js/vite.config.ts` dedupe
  block, ~lines 120-135). MIGRATION.md must either dedupe or confirm bbnf-lang's parse-that
  never crosses latex-paper's boundary.

**MIGRATION.md rows to populate:** (i) 0.7→1.0 symbol audit — 5/5 survive, confirm by run;
(ii) 1.0→1.1 inference audit — 5 `any()`/generic sites re-typechecked under `vue-tsc`;
(iii) nested-0.6.0-via-bbnf-lang dedupe decision; (iv) note: latex-paper's checked-in `dist/`
is NOT the gate — its `src` suite is.

---

## 6. Packrat homing (PLANE-WIDE) — already lawful; and consumer-less

Memoization attaches at the closure core via `memoize()` / `mergeMemos()` wrapping a
recursive `Parser` (`packrat.ts`). **Memo key:** `getCijKey(p, offset) = id * MEMO_OFFSET_SPAN
+ offset`, float64-safe, `MEMO_OFFSET_BITS = 32`, fail-loud `RangeError` (`packrat.ts:71-100`).
**MemoCell** holds either a finished `ParserState` **or** an in-progress `LR` marker
(Warth-Douglass-Millstein packrat-with-left-recursion, PEPM '08; `packrat.ts:1-40`). **Type
of a memoized parser:** still `Parser<T>` — `makeMemoized` (`packrat.ts:~290`) returns a
`Parser` wrapping the same `ParserFunction<T>`; the Parser shape is unchanged (shape-stable,
no new field). **Arming:** module latch `PACKRAT_ARMED = false` (`packrat.ts:156`), armed at
**construction** of the first `memoize()` (`packrat.ts:290`) — a **true no-op** until armed;
`packratEnter`/`packratExit`/`resetPackrat` do nothing until then; the default LL(1) path
(CSS/JSON/CSV/LaTeX) pays no `MEMO.clear()` tax; the armed path is byte-identical thereafter;
the latch never disarms.

**8D-specific:** the sole live consumer (latex-paper) does **not** import `memoize`
(§3 oracle: 5 symbols, no packrat subpath). Packrat is therefore **UNARMED across the entire
live consumer set**. Every 8D perf table states **UNARMED (no consumer arms it)** — not a gap,
the measured reality. Arming stays opt-in, consumer-less, and disclosed.

---

## 7. `buildDiagnostic` — result-union vs field-triple (shared 8B/8D) — DECIDED on the
   consumer axis

The failed `ParserState` returned by `parseStateInner` (`parser.ts:56-65`) **already carries
the field triple per-parse**: `furthest` (always), `expected` (gated), `getLineAndColumn`
(method, `state.ts:127`). A result-union `{ ok: true; value: T } | { ok: false; furthest;
expected; getLineAndColumn }` is a **type-plane (8B) refinement**, not a consumer need.

**The deciding experiment, decided by the import oracle:** latex-paper — the only consumer —
reads **neither** `.expected` **nor** any result-union; it consumes `.parse()` → value and
handles failure through the existing `Parser`/`ParserState` API (§3: 5 combinator symbols,
no diagnostics subpath). So **field-triple-on-returned-state is sufficient for the consumer
contract**; the result-union has **zero 8D pull**. The returned state *is* the diagnostic — no
separate `buildDiagnostic` allocation is needed on the hot path; a lazy `.diagnostic()`
accessor formats from the already-present triple only when asked (latex-paper never asks).
**Adjudication:** field-triple wins on the consumer axis; the result-union is 8B's call on
type ergonomics, gated by no consumer.

---

## 8. Integration seams (path:line)

- value.js parse-that-free: `value.js/package.json` (no `dependencies`); `src/parsing/
  {color,stylesheet,timeline}/` empty; `src/v4/css/grammar.ts:1-36`; `src/subpaths/math.ts:2`,
  `transform.ts:4`.
- value.js resolution: `vite.config.ts` production `rolldownOptions.external = libraryExternal`;
  `vite.library.ts:14` `libraryExternal: string[] = []`.
- latex-paper: `package.json:37-38` (bbnf-lang + parse-that deps); 8 import sites (§0);
  `package-lock.json:582-589` nested 0.6.0; `package.json:30` build `tsup && vue-tsc`.
- keyframes.js: `src/animation/internal/leaves.ts:9-10`.
- diagnostics: `utils.ts:6` global; `mergeErrorState` `utils.ts:29-49`; `addSuggestion`
  `utils.ts:51`; `parseStateInner` `parser.ts:56-65`; state fields `state.ts:39-46`;
  `getLineAndColumn` `state.ts:127`.
- packrat: key `packrat.ts:71-100`; latch `packrat.ts:156`; arm `packrat.ts:290`.

---

## 9. The Y.13 reckoning the family must state

With value.js and keyframes.js gone, **most of parse-that's public surface is consumer-less**:
packrat, diagnostics, dispatch, `all`/`fuseAll`, `sepBy`, `many`, `chain`, `whitespace`,
`mergeErrorState` — none reached by any external consumer (§3). Constitution viii forbids
consumer-less substrate. The honest 8D disposition:
- The combinator **primitives** are exempt under constitution i (grammar/primitives/
  combinators are the floor). `Parser`, `regex`, `string`, `any` are consumed; they stand.
- But `dispatch`, `all`, packrat, diagnostics are **not primitives** and have **no external
  consumer**. 8D must either (a) declare parse-that's **own test/bench corpus** the first-party
  consumer that justifies them (Y.13 satisfied by an internal, disclosed suite), or (b)
  prune the surface to the 5-symbol live contract + primitives. This is the central
  consumer-contract-first decision the family now owns — and it can only be made honestly
  because the oracle (§3) proved the census, rather than assuming R4's nine symbols.

---

## 10. Prior art (cited)

- **crater (rust-lang/crater)** is the canonical prior art for the directive's own "crater"
  framing: build every downstream against a candidate compiler and classify regressions. Our
  re-scoped harness is a **1-consumer crater + 2 negative guards** — the degenerate but
  honest instance.
- **Consumer-driven contract testing (Pact)** and **tsd / expect-type / `$ExpectType`
  (DefinitelyTyped's type-test convention)** supply the mechanism for the 1.1.0 inference-break
  gate: a type-level suite run under `tsc --noEmit` / `vue-tsc` is the artefact (§5(ii)).
- **Error-carrying result vs error-in-state:** nom's `IResult`/`VerboseError` (Rust) is the
  result-union model; parsec/megaparsec carry the error on the returned `State`/`ParseError` —
  the field-triple model parse-that already implements (§7). The consumer axis being empty
  (latex-paper reads neither) is precisely why the choice is 8B's, unconstrained by 8D.
- **Dual-instance package hazard:** the standard npm nested-version failure mode (two copies
  of a stateful/`instanceof`-bearing package); value.js's own `dedupe` block is the in-tree
  precedent for the fix (§5).

---

## 11. What a prototype MUST test to move this family

1. **RUN the re-scoped crater (the un-run gate).** Pack parse-that HEAD; `npm i` the tarball
   into a latex-paper scratch **over** the pinned 0.7.1 (mandatory reinstall); `npm test` +
   `vue-tsc --noEmit`. GREEN, or enumerate the exact symbol/type break.
2. **RUN 0.7.1 → 1.0.0 ALONE** first (attribution isolation) so no 1.0.0-era break is blamed
   on 1.1.0.
3. **Type-break probe (the only real 1.1.0 consumer risk):** does the PT-E2 inference
   tightening change the inferred type at latex-paper's 5 `any()`/generic sites such that
   `vue-tsc` reddens?
4. **Dual-instance probe:** does latex-paper's nested parse-that 0.6.0 (via bbnf-lang) cross
   latex-paper's own parse-that boundary (instanceof / shared `Parser` type) at runtime? If
   yes → dedupe or document.
5. **Negative-guard probe:** mechanical grep gate asserting value.js + keyframes.js stay
   parse-that-free — a permanent tripwire against silent contract re-expansion.
6. **Boolean-field diagnostics probe:** replace `diagnosticsEnabled` (`utils.ts:6`) with a
   `state.diag` boolean field; prove `%HaveSameMap(diagOnState, diagOffState) === true`
   (`node --allow-natives-syntax`) and the diagnostics-off hot path holds its floor — no
   allocation, no hidden-class transition.

## 12. Named risks / counterexamples

- **R1 (headline): the design target is a phantom.** PT-3 gated on "value.js's 59 `all()`
  sites + `dispatch` bucket + `mergeErrorState`." value.js now has **zero** `all()` sites and
  no parse-that. Any 8D perf table citing "value.js's `all()` sites" measures nothing.
- **R2: the bundling premise is false, not just moot.** `libraryExternal=[]` bundles nothing
  *of parse-that* because value.js imports none. A crater grepping value.js's checked-in dist
  for a stale bundled parse-that finds a **true-green** absence.
- **R3: latex-paper's dual parse-that (0.7.1 + nested 0.6.0).** Bumping the pin without dedupe
  leaves two versions; the migration matrix owns this (§5).
- **R4: the sole positive gate may be unreliable.** latex-paper is itself at 0.2.1 and may be
  dormant; if its suite is red independent of parse-that, the only positive consumer gate is
  meaningless. It MUST be baselined GREEN at 0.7.1 first (a born-verified before-state), or the
  whole crater is uninterpretable.
- **R5: pruning vs corpus (Y.13).** If 8D keeps packrat/dispatch/all with no external
  consumer, it violates viii unless parse-that's own suite is declared their first-party
  consumer — that declaration must be explicit, not implicit (§9).
