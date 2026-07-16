# LOCK-8 pass-3 — research-D — family 8D CONSUMER-CONTRACT-FIRST

*The TS backbone designed backward from its live consumers. Every figure below is a
static fact of the checked-out trees (git/grep/AST/lockfile) captured this pass; where a
number would be a runtime it is named un-run and handed to §11. parse-that is READ-ONLY;
prototyping is against scratch copies; the sole sanctioned artefact into the repo is
coordination letter BBNF-PT-G.*

---

## 0. Headline — the census has flipped a SECOND time; value.js is DUAL-STATE

The prior-pass verdict ("value.js migrated off, latex-paper sole survivor") was measured
on value.js's **`tranche-u` working branch** and is only half true. The fresh per-tree,
per-branch census:

- **value.js is DUAL-STATE.** Its **shipping line — `master` — is 3.1.0 and STILL depends
  on `@mkbabb/parse-that: ^1.0.0`**, importing the FULL surface across 8 sites:
  `{Parser, ParserState, all, any, dispatch, regex, string, whitespace, mergeErrorState}`
  (`git show master:package.json`; `git grep …@mkbabb/parse-that master`). The
  parse-that-free rewrite (version 4.0.0, hand-written CSS in `src/v4/css/`) lives ONLY on
  branch **`tranche-u`, 137 commits ahead of master, and that campaign is PAUSED at owner
  order** (branch tip `86a9a17`, prior tip commits read "THE PAUSE — V campaign halted at
  owner order post-pass-4", "THE HANDOFF OF RECORD"). So value.js is neither cleanly a
  consumer nor cleanly gone: **released = live full-surface consumer at ^1.0.0; forward =
  paused, unmerged, parse-that-free.** This is precisely the XL-G "carry both hypotheses"
  state — only the owner collapses it.
- **latex-paper — LIVE sole DIRECT-source consumer, semi-dormant.** version 0.2.1, branch
  master, last commit 2026-07-02 (≈2 weeks stale; cadence gaps to May/March — active but
  slow, not frozen-dead). Pinned `^0.7.1` → resolves **0.7.1** (predates the 1.0.0 cut). 8
  import sites, surface exactly `{Parser, ParserState, regex, string, any}` (5 symbols).
- **keyframes.js — parse-that-FREE, confirmed.** version 6.0.0, active (2026-07-15), zero
  `@mkbabb/parse-that` in package.json or src. Its only route was transitive through
  value.js; on value.js-master (3.1.0) that transitive route may re-open — see §1.

**Consequence.** The consumer-set adjudication (directive 8D-Q1) does NOT reduce to
latex-paper's 5 symbols as the prior pass concluded. It **bifurcates on the value.js
master-vs-tranche-u owner question** and must be carried as two dispositions (§2).

---

## 1. Consumer liveness — the plane-wide P1/P2 answers (fresh)

| repo | line | version | parse-that dep | surface | posture |
|---|---|---|---|---|---|
| value.js | **master (shipping)** | 3.1.0 | `^1.0.0` | 9 symbols (R4 set) | **LIVE full consumer** |
| value.js | tranche-u (137 ahead) | 4.0.0 | **none** | ∅ | paused, unmerged, PT-free |
| latex-paper | master | 0.2.1 | `^0.7.1`→0.7.1 | 5 symbols | LIVE, semi-dormant, frozen behind |
| keyframes.js | master | 6.0.0 | none (transitive via value.js) | ∅ direct | PT-free (direct) |

- **P1 (which repos are live forward-looking consumers?).** DIRECT live consumers today:
  **value.js-master (^1.0.0) and latex-paper (^0.7.1)**. The forward-looking *intent*
  (value.js tranche-u) is to leave, but it is unmerged and paused — the released contract
  is what binds a cut. keyframes.js is not a direct consumer on either value.js line.
- **P2 (is value.js 4.0.0's posture intended or a WIP artifact?).** **BOTH, and only the
  owner disambiguates.** It is intended *direction* (137 deliberate commits, CSS re-homed
  hand-written in `src/v4/css/grammar.ts` + `stylesheet.ts`, `src/subpaths/math.ts:2`
  "parse-that-FREE") but a WIP *branch artifact* in that it is unmerged, PAUSED, and master
  still ships parse-that. The installed-vs-pinned lag is stark: master pins `^1.0.0` and the
  published parse-that is frozen at 1.0.0, so value.js-master is *at* its floor, not behind;
  latex-paper pins `^0.7.1` and is **two minor cuts (0.7→1.0→1.1) behind**. **Disposition:
  a 1.1.0 cut CANNOT assume value.js has left.** Carry value.js-master as a live gate until
  the owner confirms the tranche-u merge; if merged, re-census collapses to latex-paper.

---

## 2. Consumer-set adjudication (directive 8D-Q1) — TWO dispositions, keep/drop

parse-that's OWN gate already answers the abstract half: `scripts/proof-no-dead-combinator.mjs`
declares a combinator dead iff it has **zero consumers across `{parse-that/src, parse-that/test,
value.js/src, keyframes.js/src}`** (`:63-68`) — i.e. **the first-party `test/` tree is already
an enshrined first-party consumer** (Y.13/constitution-viii is discharged for first-party
combinators by the shipped gate, not a new assertion). Two flaws in that gate, both fixable:
(a) it **omits latex-paper/src** from `consumerTrees` — the sole surviving external DIRECT
consumer is not swept; (b) its **vacuity guard hard-codes `dispatch` as "known-live"** (`:49-58`),
but `dispatch`'s ONLY external consumer is value.js-master — if tranche-u merges, the vacuity
guard's own anchor dies. Both are §11 prototype items.

**Disposition A — value.js-master is authoritative (owner keeps the shipping line).** The R4
nine-symbol surface is fully live: **KEEP** `{Parser, ParserState, all, any, dispatch, regex,
string, whitespace, mergeErrorState}`. Nothing prunes; `all`/`dispatch`/`whitespace`/
`mergeErrorState` each have value.js-master as an external consumer. Packrat/diagnostics remain
first-party-only (test/ corpus is the declared consumer, per the shipped gate).

**Disposition B — tranche-u is destiny (owner confirms value.js leaves).** External DIRECT
contract shrinks to latex-paper's **5 symbols** `{Parser, ParserState, regex, string, any}`.
Then **externally-consumer-less:** `all`, `dispatch`, `whitespace`, `mergeErrorState`, plus the
never-external `sepBy/many/chain/wrap/trim/peek/lookAhead/minus/not/opt/recover`, packrat, and
diagnostics. **Do NOT prune them** — declare parse-that's `test/*.test.ts` (13 files, 124 cases)
+ `test/benchmarks/` the first-party consumer, exactly as the shipped gate ALREADY does. The
public API is a *library* contract; a combinator algebra with a first-party conformance corpus
is a legitimate Y.13 consumer (the shipped gate is the precedent). **Pruning is the wrong move**:
it amputates a coherent algebra to fit one transiently-frozen downstream. The keep/drop is:
KEEP ALL; RECLASSIFY `all/dispatch/whitespace/mergeErrorState` from "external-consumed" to
"first-party-consumed"; ADD latex-paper/src to the gate's `consumerTrees` so the classification
is measured, not asserted.

**Net 8D verdict on Q1:** *no combinator is dropped under either disposition.* The first-party
test suite IS a legitimate first-party consumer (parse-that's own gate enshrines it); the public
surface does not prune to the live external contract. The only edit the census forces is a gate
fix (add latex-paper; re-anchor the vacuity guard off value.js-master).

---

## 3. The migration CRATER (directive 8D-Q2) — re-scoped to the true census

`scratch/pt-consumer-crater.sh` (harness only; never landed). The crater now has **two positive
legs** (value.js-master resurfaced as a real gate), not one:

- **Leg 1 — value.js-master, POSITIVE (resurrected).** Scratch-copy value.js at **master**
  (3.1.0). Pack parse-that working-tree: `(cd scratch/parse-that/typescript && npm run build &&
  npm pack)`. **Mandatory reinstall** (the false-green tripwire): `npm i
  ../parse-that-<cand>.tgz` over the pinned 1.0.0. Then `npm test` (`vitest`, jsdom) **and**
  the full type-check (`tsc --noEmit` per its config). value.js-master exercises
  `all`/`any`/`dispatch`/`mergeErrorState` — the LARGEST inference-tightening blast radius (59
  `all()` sites per R4). This leg is what makes 8D-Q4 answerable on a NATIVE corpus.
- **Leg 2 — latex-paper, POSITIVE.** Scratch-copy latex-paper. `rm -rf
  node_modules/@mkbabb/parse-that && npm i ../parse-that-<cand>.tgz`. Run `npm test`
  (`vitest run`) **and** `npm run build` = `tsup && vue-tsc` (the type axis — §5). Do NOT touch
  the nested `bbnf-lang/node_modules/@mkbabb/parse-that@0.6.0` (dual-instance — §5).
- **Leg 3 — keyframes.js, NEGATIVE guard.** `! grep -rq "@mkbabb/parse-that" src/` + package.json
  assertion; a permanent tripwire against silent re-introduction.

**Minor-step-alone attribution (mandatory, directive 8D-Q2 + PT-3 W0).** Run **0.7.1→1.0.0
ALONE** for latex-paper, and **1.0.0→1.1.0 ALONE** for value.js-master, BEFORE any combined jump,
so a 1.0.0-era break (the `*Span` excision S.H2 / `chain()` C-16 fix) is never mislabeled a 1.1.0
regression. For value.js-master the isolating step IS the pure 1.0.0→1.1.0 minor (it already sits
at 1.0.0-compatible `^1.0.0`), giving the cleanest possible single-cut attribution of the PT-E2
inference tightening. **Baseline-GREEN-first is a hard precondition:** if a consumer suite is red
at its pinned version independent of the candidate, the crater is uninterpretable (halt, report
upstream). The crater is **constructable now**; it is un-RUN (§11).

---

## 4. Per-parse diagnostic mechanism + disabled-path cost (directive 8D-Q3)

**The residue is a single module-global boolean**, `utils.ts:6 let diagnosticsEnabled = false`
(setters `enableDiagnostics`/`disableDiagnostics` at `:8-13`), read in `mergeErrorState` (`:33`),
`addSuggestion` (`:51`), `addSecondarySpan`. The *storage* (`furthest`, `expected?`,
`suggestions`, `secondarySpans`) is ALREADY per-parse on `ParserState` (`state.ts:43-46`) and
`furthest`/`getLineAndColumn` (`state.ts:127`) are already always-on per-parse. Only the
*populate-gate* is global.

**Mechanism (candidates, disabled-path cost measured on the hidden class):**

1. **Boolean field on `ParserState` — `state.diag: boolean` (FAVORED).** Set once from a parse
   option (`.parse(src, { diagnostics: true })`) and threaded to child states; `mergeErrorState`
   reads `state.diag` instead of `diagnosticsEnabled`. **Disabled cost = ONE boolean field read
   already in the state's monomorphic hidden class** — zero allocation, zero new object, zero
   hidden-class transition (a `boolean` field fixed at construction never transitions). The
   `ParserState` ctor (`state.ts:41-53`) already has 5 positional fields + 3 diagnostic fields;
   adding one boolean initialized in the ctor keeps the shape fixed-from-birth.
2. **`DiagnosticScope` object hung off the parse epoch (PT-3 W2's literal proposal).** Opens/
   closes in `parseState`'s existing try/finally (`parser.ts:34-49`). **Disabled cost = a
   per-`.parse()` object allocation** UNLESS the scope is `null` when off — and a nullable scope
   reference on `ParserState` is then just candidate 1 wearing a heavier coat. **Rejected on the
   disabled path:** it re-introduces exactly the R2 per-parse allocation PT-3 names as a risk.
3. **`ParserContext` field (grammar-construction lifetime).** WRONG lifetime — context is built
   once per combinator at grammar-build (R4 §2), not per-parse; a flag there is effectively
   global-per-grammar, re-creating the leak.

**Verdict:** candidate 1. The "scope" PT-3 W2 wants IS the already-per-parse `ParserState`; no
new lifetime is needed. Deletion proof for the global (precept #6): `rg 'let diagnosticsEnabled'
src/` → 0. **The disabled-path oracle (§11): `%HaveSameMap(diagOnState, diagOffState) === true`
under `node --allow-natives-syntax`, and the W1 paired harness re-run with diagnostics off holds
its floor** — proving the flag read is free. NOTE the un-landed dependency: `suggestions`/
`secondarySpans` are STILL eager `= []` at HEAD (`state.ts:45-46`) — the PROVE-11 lazy-field lever
(optional, `undefined`-at-birth) is a PRECONDITION for the diagnostics-off path to be allocation-
free; W1 must land before W2's scoping is genuinely zero-cost.

---

## 5. Inference tightening × consumer type-check + the dual-instance hazard (directive 8D-Q4)

**The leaks and the tightening (verified against HEAD).** `any<T extends Array<Parser<any>>>`
(`leaf.ts:26`) with `Result = T[number] extends Parser<infer V> ? V : never`; `all<T extends
Array<Parser<any>>>` (`leaf.ts:154`) re-defining `ExtractValue` identically to `parser.ts:9-11`
(DRY violation — one shared `types.ts` is the fix); `fuseAll(parsers: Array<Parser<any>>)`
(`leaf.ts:179`); `restore(saved: {…; value: any})` (`state.ts:79`); `ParserFunction` erases to
`ParserState<any>` (`parser.ts:14-16`, documented as a legitimate variance requirement at the
implementation boundary — NOT a leak to close).

**Does the tightening redden a consumer's type-check?** The blast radius is the `any()`/`all()`
call sites where an EXPLICIT annotation absorbs a currently-`any`-widened union:

- **latex-paper (5 `any()` sites).** `document.ts:81 const inlineNode: Parser<LatexNode | null>
  = any(...)` is the single highest-risk site — an explicit annotation absorbing a currently-`any`
  union; under tightening the precise arm-union must be assignable to `LatexNode | null` or `tsc`
  reds. `text.ts:145 accent: Parser<TextNode> = any(bracedAccent, unbracedAccent, namedAccent)`
  (positional tuple) and `text.ts:154 any(...alts)` (spread of `Parser<TextNode>[]`) — both should
  survive (readonly-widening is covariant; a mutable array satisfies `ReadonlyArray`). But this is
  a **0.7→1.1 jump**, so the `any`/`Parser` signatures differ across THREE cuts, not just the
  tightening delta — the type break, if any, may be owned by 1.0.0, not 1.1.0. **Caught only by
  `tsup && vue-tsc`, never by `vitest` alone.**
- **value.js-master (59 `all()` sites + `dispatch` + `any`).** The LARGE surface — this is why
  Leg 1 of the crater (value.js-master native corpus) is the *native* answer to directive 8D-Q3's
  "instantiation-budget on a live consumer's native corpus." Its 59 `all()` sites are the real
  stress of the `ExtractValue` tightening; a `Parser<T[]>` widened to a precise tuple can exceed
  tuple-instantiation depth and trip **TS2589** (type instantiation excessively deep). The
  no-new-TS2589 check (directive) MUST be run on value.js-master's 59 sites, not a synthetic one.

**The dual-instance hazard — CONCRETE and on a LIVE path (resolve: DOCUMENT, do not force-dedupe).**
latex-paper carries TWO parse-that instances: direct `0.7.1` and nested `0.6.0` via
`@mkbabb/bbnf-lang@^0.1.1` (`node_modules/@mkbabb/bbnf-lang/node_modules/@mkbabb/parse-that@0.6.0`).
It is not hypothetical: `src/grammar/compile.ts:6` imports `BBNFToParser` from bbnf-lang (a
**0.6.0-built** `Parser` factory); `compile.ts:19,81 const [nt] = BBNFToParser(…)` yields
`Record<string, Parser<any>>` where every value is a **0.6.0 `Parser` instance**; latex-paper then
drives them via `.map()` on a live path (`compile.ts:22-40` — `nt.emDash.map(...)` etc.) while
typing them as the **direct 0.7.1** `Parser` (`compile.ts:7,15`). **Bumping the direct dep to
1.1.0 does NOT touch the nested 0.6.0** — so post-bump, 0.6.0-built objects are type-checked
against the 1.1.0 `Parser`/`.map` signature: a THREE-cut type↔runtime skew on a live path.

- **Why NOT dedupe:** forcing bbnf-lang onto parse-that 1.1.0 is a 0.6.0→1.1.0 jump for
  bbnf-lang's *internals* (which predate `*Span` excision + the `chain()` fix) — it could break
  bbnf-lang itself. Dedupe is unsafe unless bbnf-lang is co-re-cut.
- **Why DOCUMENT suffices:** the crossing is defensively `Parser<any>` on the latex-paper side
  (`Nonterminals = Record<string, Parser<any>>`, `escapedSpecial.map((parts: any[]) => …)`), so
  the tightening's `any`→precise does NOT bite here (`any` absorbs). Only `.map`'s structural
  presence must hold across 0.6.0↔1.1.0 (it does). **Resolution: a MIGRATION.md row stating the
  crossing drives 0.6.0 Parsers via `.map()` only, typed `Parser<any>`, no `instanceof` — gated by
  a runtime pass of latex-paper's suite after the bump.** Dedupe deferred to a bbnf-lang re-cut.

---

## 6. SPEC-fidelity divergence list (plane-wide) — 8D rows feeding a spec re-cut

1. **PT-3 §1.1 / R4 §7 "value.js migrated / three consumer suites."** DIVERGES: value.js-master
   (3.1.0) is a LIVE ^1.0.0 consumer; only tranche-u (paused, unmerged) is PT-free. The "three
   consumer suites green" gate must gate on **value.js-master + latex-paper** (2 positive), not a
   phantom-migrated value.js. Re-cut PT-3 W0/W5's consumer list.
2. **R4 §7 "value.js dep `^1.0.0` (lock→1.0.0), 3.1.0."** CONFIRMED for master; R4 read master.
   The prior-pass "value.js 4.0.0 gone" over-corrected to the branch. Re-cut to name the branch.
3. **PT-3 W0 "latex-paper 0.7.1-floor, migration adjudicated."** CONFIRMED (0.7.1, `^0.7.1`); ADD
   the nested-0.6.0 dual-instance row (§5) which PT-3 §4 mentions but does not resolve.
4. **`proof-no-dead-combinator.mjs` consumerTrees.** DIVERGES from the live census: omits
   latex-paper/src; vacuity-anchors on `dispatch` (value.js-master-only). Re-cut the gate.
5. **PT-3 W1 dispatch lane "~+5%."** CONFIRMED honest (PROVE-11 VERIFY halved +10.2%→~+5%,
   CONFIRMED_DELTA). No divergence — carry ~+5% no-regression, never the inflated figure.

---

## 7. Integration seams (path:line)

- value.js dual-state: `master:package.json` dep `^1.0.0`; `master` src 8 sites (full surface);
  `tranche-u` `package.json` no deps, `src/v4/css/grammar.ts`+`stylesheet.ts`, `src/subpaths/
  math.ts:2`; `git rev-list --count master..tranche-u` = 137; branch tip `86a9a17` (PAUSE).
- latex-paper: `package.json:37-38` (bbnf-lang `^0.1.1` + parse-that `^0.7.1`); 8 import sites
  (`grammar/{commands,environments,text,math,document,primitives,compile}.ts`, `bibtex/parser.ts`);
  nested 0.6.0 at `node_modules/@mkbabb/bbnf-lang/node_modules/@mkbabb/parse-that`; crossing
  `src/grammar/compile.ts:6-7,15,19,81`; build `package.json:30` `tsup && vue-tsc`.
- keyframes.js: `package.json` no parse-that; `src/animation/internal/leaves.ts` PT-free.
- diagnostics: global `utils.ts:6`, reads `utils.ts:33,51`; per-parse storage `state.ts:43-46`;
  `getLineAndColumn` `state.ts:127`; epoch try/finally `parser.ts:34-49`; eager `= []` still at
  HEAD `state.ts:45-46` (PROVE-11 lever un-landed).
- inference: `any` `leaf.ts:26`; `all`+dup `ExtractValue` `leaf.ts:154-157` vs `parser.ts:9-11`;
  `fuseAll` `leaf.ts:179`; `restore` `state.ts:79`; `ParserFunction` `parser.ts:14-16`.
- gate: `scripts/proof-no-dead-combinator.mjs:63-68` (consumerTrees), `:49-58` (vacuity=dispatch).

---

## 8. Packrat homing (plane-wide) — lawful, and consumer-state disclosed

Memoization attaches at the closure core (`packrat.ts`); key `getCijKey = id*MEMO_OFFSET_SPAN +
offset`, float64-safe, fail-loud RangeError (`packrat.ts:71-100`); `PACKRAT_ARMED=false`
(`packrat.ts:156`), arms at first `memoize()` construction (`packrat.ts:290`) — a TRUE no-op
until armed; `Parser` shape unchanged (no new field). **8D disclosure: NO live consumer imports
`memoize` on either value.js line or latex-paper** — packrat is UNARMED across the entire live
consumer set. Every 8D perf table states **UNARMED (no consumer arms it)** — the measured reality.

## 9. `buildDiagnostic` — result-union vs field-triple (shared 8B/8D)

The failed `ParserState` from `parseStateInner` (`parser.ts:56-65`) ALREADY carries the field
triple per-parse (`furthest` always, `expected?` gated, `getLineAndColumn` method). A result-union
`{ok:true;value:T} | {ok:false;furthest;expected;getLineAndColumn}` is a type-plane (8B)
refinement. **Consumer axis:** neither latex-paper (5 combinator symbols, no diagnostics subpath)
nor value.js-master (imports `mergeErrorState` but reads no result-union) consumes a result-union —
both handle failure through `.parse()`→value + `ParserState`. **Field-triple-on-returned-state
suffices for the consumer contract; the result-union has zero 8D pull** (it is 8B's call on type
ergonomics). No `buildDiagnostic` hot-path allocation; a lazy `.diagnostic()` accessor formats from
the present triple only when asked.

## 10. Prior art (cited)

- **crater (rust-lang/crater)** — build every downstream against a candidate compiler, classify
  regressions. Our harness is a **2-positive-leg + 1-negative-guard** crater (value.js-master +
  latex-paper positive; keyframes.js guard).
- **Consumer-driven contract testing (Pact); tsd / `expect-type` / `$ExpectType`
  (DefinitelyTyped's `.test-d.ts` convention)** — the mechanism for the 1.1.0 inference-break gate:
  a type-level suite under `tsc --noEmit`/`vue-tsc` is the artefact (§5).
- **TS2589 "type instantiation excessively deep"** — the concrete failure mode for tightened
  variadic-tuple `ExtractValue` over value.js-master's 59 `all()` sites; the no-new-TS2589 check is
  the guard (recursive conditional-type depth, microsoft/TypeScript).
- **npm dual-instance / `instanceof` hazard; `npm dedupe` / `overrides`** — the standard nested-
  version failure mode; value.js's own `dedupe` block (`vite.config.ts`) is the in-tree precedent.
  Here dedupe is DEFERRED (bbnf-lang co-re-cut required) and the crossing is DOCUMENTED (§5).
- **Error-on-returned-state (parsec/megaparsec) vs error-carrying-result (nom `IResult`)** — the
  field-triple vs result-union axis (§9); the consumer axis being empty makes it 8B's call.
- **SemVer minor-non-breaking discipline** — additive diagnostics API + inference *superset* +
  `[] → []|undefined` field widening are all minor-safe IF no consumer type reddens; the crater is
  the falsifier (a real type break escalates minor→major, decided not assumed).

## 11. What a prototype MUST test to move this family (un-run gates)

1. **RUN the 2-leg crater.** Pack parse-that working tree; `npm i` the tarball over the pin in a
   value.js-**master** scratch AND a latex-paper scratch (mandatory reinstall — skipping it silently
   tests the pinned old tarball). `npm test` + full type-check (`vue-tsc`/`tsc --noEmit`) each.
   GREEN, or enumerate the exact symbol/type break.
2. **RUN the minor step ALONE:** value.js-master 1.0.0→1.1.0 alone (isolates the PT-E2 tightening);
   latex-paper 0.7.1→1.0.0 alone (isolates the 1.0.0-era `*Span`/`chain` cut) before any combined
   jump.
3. **TS2589 probe on the NATIVE corpus:** does the `all`/`any` tightening exceed tuple-
   instantiation depth at value.js-master's 59 `all()` sites or latex-paper's `document.ts:81`?
   Report the tsc version + exact signature (directive 8D-Q4 native-corpus instantiation number).
4. **Dual-instance runtime probe:** run latex-paper's suite after the 1.1.0 bump WITHOUT deduping
   the nested 0.6.0; confirm `compile.ts` drives the 0.6.0-built Parsers (`nt.*.map()`) without
   runtime failure and no `instanceof Parser` crosses the boundary. GREEN → document; RED → dedupe
   (with bbnf-lang co-re-cut) or pin.
5. **Boolean-field diagnostics probe:** replace `diagnosticsEnabled` (`utils.ts:6`) with
   `state.diag`; prove `%HaveSameMap(diagOn, diagOff) === true` (`node --allow-natives-syntax`) and
   the diagnostics-off hot path holds the W1 floor (zero alloc, no hidden-class transition) — GATED
   on the PROVE-11 lazy-field lever landing first (`state.ts:45-46` still eager `= []`).
6. **Gate-fix probe:** add `latex-paper/src` to `proof-no-dead-combinator.mjs` consumerTrees; re-
   anchor the vacuity guard off value.js-master (`dispatch` dies if tranche-u merges); re-run the
   gate green under both value.js-line hypotheses.
7. **Negative-guard tripwire:** mechanical grep asserting keyframes.js (and, if tranche-u merges,
   value.js) stay parse-that-free — a permanent guard against silent contract re-expansion.

## 12. Named risks / counterexamples

- **R1 (headline): the census flips with the branch you read.** A crater run against
  value.js-`tranche-u` measures a phantom (nothing to break — it's PT-free); a crater against
  value.js-`master` measures the real full-surface consumer. The crater MUST name its branch; the
  prior "value.js gone" was a branch artifact.
- **R2: the sole no-owner-answer state.** Until the owner rules on the tranche-u merge, the consumer
  set is genuinely two-valued; any single-disposition perf/API claim is over-committed. Carry both.
- **R3: the dual-instance crossing is LIVE, not hypothetical** — `compile.ts` drives 0.6.0-built
  Parsers typed as 1.1.0. Force-deduping breaks bbnf-lang (a 4-cut internal jump); the safe move is
  DOCUMENT + runtime-gate, dedupe deferred to a bbnf-lang re-cut.
- **R4: latex-paper may be dormant.** ≈2 weeks stale, gaps to March — if its suite is red at 0.7.1
  independent of the candidate, that positive gate is uninterpretable; baseline-GREEN-first is a
  hard precondition.
- **R5: the tightening's native blast radius is value.js-master's 59 `all()` sites** (TS2589), not
  latex-paper's 5 — the crater's value.js-master leg is where the real instantiation-budget number
  lives; measuring only latex-paper under-tests the risk.
- **R6: a bbnf agent edits parse-that.** The non-clobber law (HANDOFF §6.9) binds: 8D work is
  scratch-copy prototyping + the BBNF-PT-G letter only; parse-that is READ-ONLY.
