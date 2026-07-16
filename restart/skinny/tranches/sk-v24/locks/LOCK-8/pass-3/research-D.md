# LOCK-8 · Family 8D — CONSUMER-CONTRACT-FIRST (the backbone formulation)

The parse-that TypeScript backbone designed backward from its real consumers: their
usage patterns define the API, perf gates ride their suites, semver and migration
are first-class deliverables. This family's charter is the consumer contract itself —
so its first duty is to re-establish, by measurement, who the consumers actually are.
Every path is absolute; parse-that/typescript is READ-ONLY (any landing rides letter
BBNF-PT-G); prototypes run only on scratch copies.

---

## 0. THE SEISMIC FINDING — the flagship consumer is leaving

The recon baseline (R4) recorded value.js at 3.1.0 importing nine parse-that symbols.
That is still true of **`master`**. It is **NOT** true of the live working tree.

- **`/Users/mkbabb/Programming/value.js` @ `master`** = version **3.1.0**, dependency
  `@mkbabb/parse-that: ^1.0.0`, and `src/parsing/{color,stylesheet,timeline,units,utils}.ts`
  all `import … from "@mkbabb/parse-that"`. Surface (git-grep of `master:src`): **9 symbols** —
  `Parser`, `ParserState` (type), `all`, `any`, `dispatch`, `regex`, `string`, `whitespace`,
  `mergeErrorState`. This is the published/registry reality (tranche R shipped 2.0.0/3.0.0 here).

- **`/Users/mkbabb/Programming/value.js` @ HEAD (branch `tranche-u`)** = version **4.0.0**,
  **137 commits ahead of `master`** (`git rev-list --left-right --count master...HEAD` → `0  137`),
  last commit **2026-07-15** (`origin/tranche-u` live). `package.json` has **`dependencies: {}`** —
  the entire block is gone; `JSON.stringify(pkg).includes("parse-that")` → **false**. `src/` contains
  **zero** `@mkbabb/parse-that` imports (only two doc-comments in `src/subpaths/{math,transform}.ts`
  that assert the property **"parse-that-FREE"**, a named O.W2 goal). `src/parsing/{color,stylesheet,
  timeline}/` are now **empty of `.ts`**. In their place: **`src/v4/`** — a bespoke recursive-descent
  CSS/color parser (13 files, **3180 LOC**) over a hand-rolled `Result<T>` monad
  (`src/v4/result.ts` `ok`/`err`), with its own `src/v4/css/grammar.ts` and `src/v4/grammar`. It uses
  **no combinators at all** — no `Parser`, no `all`, no `dispatch`.

**Adjudication of P2 (intended vs WIP artifact): BOTH, and the distinction is the deliverable.**
It is a **WIP feature-branch artifact** — unmerged, unpublished; `master` and the npm registry still
ship value.js 3.x on `@mkbabb/parse-that ^1.0.0`; `node_modules` on the branch still resolves a stale
`parse-that@1.0.0`. AND it is a **deliberate forward architecture** — "parse-that-FREE" is a *named*
subpath property, and value.js's own multi-pass "V campaign" (git log: `V · pass-4 · AGGLOMERATION`,
thesis **78%**, **PAUSED at owner order**) is actively building the v4 replacement parser. Per the
constitution's carry-both-hypotheses law (XL-G, consumer confirmations pending & non-blocking): **8D
must design for the world where value.js ships 4.0.0 and contributes ZERO to the parse-that consumer
contract, while keeping the 3.1.0/`master` contract green until that merge lands.** The owner question
to route: *does tranche-u/4.0.0 supersede master, or is v4 an experiment that may be abandoned?*

**keyframes.js** (6.0.0) confirms the cascade risk: it holds **no direct** `@mkbabb/parse-that` dep
(consumes transitively through value.js; `src/animation/compile/parse-flatten.ts` comment records the
direct-dep removal). If value.js 4.0.0 drops parse-that, keyframes.js's **transitive edge vanishes with
it** — two of three consumers gone in one merge.

---

## 1. The live external contract, after value.js — and CONSUMER-SET ADJUDICATION

Post-4.0.0, the **only** live external consumer is **latex-paper** (`/Users/mkbabb/Programming/latex-paper`,
0.2.1). Its parse-that surface is narrow and stable: **`{Parser, ParserState, regex, string, any}`** across
`src/bibtex/parser.ts` + `src/grammar/*.ts` (8 import sites). It pins `^0.7.1`, `node_modules` resolves
**0.7.1** — it does **NOT track 1.0.0** (predates the `*Span` excision S.H2 and the `chain()` C-16 fix).
Its recent commits are *feature* work (grammar extensions, vue windowing, VIRT tranche) — a **live but
narrow** consumer whose parse-that dependency is effectively frozen at 0.7.1.

So the external contract collapses to a spectrum:
- **latex-paper (live-narrow)**: `Parser`, `ParserState`, `regex`, `string`, `any` — leaf/primitive tier only.
- **value.js `master` (live-until-merge)**: adds `all`, `dispatch`, `whitespace`, `mergeErrorState`.
- **value.js `tranche-u` (leaving)**: nothing.

Everything else in the 26-method core (`chain`, `sepBy`, `many`, `not`/`minus`, `peek`/`lookAhead`,
`wrap`, `trim`, `recover`, `mapState`, the entire `packrat` and `diagnostics` subpaths) has **no live
external consumer** on any branch.

**CONSUMER-SET ADJUDICATION — is the first-party test/bench suite a legitimate consumer, or prune to the
external contract?** Two dispositions, developed to their strongest forms:

- **(A) KEEP-BROAD (first-party suite is the consumer).** The constitution forbids consumer-less substrate
  (clause viii / Y.13) — but parse-that's **13 `test/*.test.ts`** + the **competitor bench matrix**
  (`test/benchmarks/{arcsecond,chevrotain,nearley,ohm,parjs,parsimmon,peggy}.ts` + `json-comprehensive` +
  `pt-b3-fusion`) ARE that consumer: they exercise `chain`/`sepBy`/`many`/`packrat` against seven rival
  libraries, which is precisely the "general parser-combinator library" contract. Nine machine-checkable
  **proof gates** (`proof:{manifest,subpath,no-css-surface,no-span-surface,no-dead-combinator,packrat-*,
  perf}`) already fence this surface. `proof:no-dead-combinator` is the load-bearing gate: a combinator with
  neither an external consumer nor a first-party test/bench IS dead and MUST be pruned; one that has a
  differential bench is live. **Verdict for the backbone: KEEP-BROAD.** The public surface is defined by
  the proof-gate-fenced first-party contract, not by the fluid, mid-migration external set. Pruning to an
  external contract that is actively shrinking (value.js leaving) is fragile and would delete precisely the
  surface the forward asks target.

- **(B) PRUNE-TO-EXTERNAL (public = live external contract).** Would keep only
  `{Parser, ParserState, regex, string, any}` + the `master` four (`all`, `dispatch`, `whitespace`,
  `mergeErrorState`) on `.`; demote `diagnostics`/`packrat` subpaths to internal. This is the honest
  minimal-blast-radius read IF the owner confirms value.js 4.0.0 supersedes `master` and latex-paper stays
  frozen — but it **contradicts the PT-E letter** (below), which records value.js *asking* for diagnostics
  (PT-E1) and inference (PT-E2) as forward 1.1.0+ surface even as its own consumption evolves. Disposition
  B is the **fallback** the prototype must be able to justify with a census, not the default.

The 8D backbone therefore keeps the broad surface, but **each combinator carries a consumer tag** in a
committed ledger: `EXTERNAL-LIVE` (the 5 latex-paper + 4 master symbols), `FIRST-PARTY-BENCH`
(chain/sepBy/many/not/minus/…, cited to their bench line), or `DEAD` (→ `proof:no-dead-combinator` deletes
it). This ledger is the machine-checkable form of "consumer-contract-first."

---

## 2. The migration CRATER — semver & migration as first-class artifacts

Named after rustc's **crater** (rebuild every downstream against a candidate compiler; classify
regressions) — the mechanism, made concrete for parse-that:

**Subjects & baselines (run each GREEN at its pinned version first, then swap the candidate tarball):**
1. **value.js `master`** — pinned `@mkbabb/parse-that ^1.0.0` (installed 1.0.0). Baseline: `npm test`
   (`vitest run`, ~1996 tests across R-era) + `npm run typecheck` (**`vue-tsc -p tsconfig.lib.json` AND
   `tsconfig.demo.json`** — the full type-check, not just unit tests). This is the consumer that will catch
   an inference-tightening break (§4).
2. **latex-paper** — pinned `^0.7.1` (installed 0.7.1). Baseline: `npm test` (`vitest run`) +
   `npm run build` (**`tsup && vue-tsc -p tsconfig.vue.json`** — build IS the type-check here).
3. **value.js `tranche-u`** — records `un-runnable-against-candidate` (no parse-that dep); its presence is
   itself the crater datum (a consumer that removed the dep).

**Attribution isolation (the directive's minor-version-ALONE rule):** the candidate for latex-paper is a
**0.7.1 → 1.0.0** jump that crosses two breaking events — `*Span` excision (S.H2) and `chain()` falsy-seed
fix (C-16). Run the ladder **one minor at a time** (0.7.1→0.8.0→0.9.0→1.0.0), `npm test` + build at each
rung, so a red is attributed to the exact cut. **Prediction (must be tested, not assumed):** latex-paper
imports only `{Parser, ParserState, regex, string, any}` — none of which the `*Span` excision or `chain()`
touch — so the crater SHOULD be green end-to-end; a green result *retires* the version-skew ⚠ that R4 flags.
value.js `master`'s four extra symbols (`all`/`dispatch`/`whitespace`/`mergeErrorState`) are likewise all
still exported at 1.0.0. The crater's job is to convert "should be green" into "measured green."

**Substrate:** a scratch harness that (i) `npm pack`s the candidate `parse-that/typescript` build, (ii)
`npm i <tarball>` into a throwaway clone of each consumer, (iii) runs the consumer's own `test` + full
type-check script, (iv) diffs pass-set vs baseline. Reuses PROVE-11's scratch-copy discipline
(`/Users/mkbabb/Programming/parse-that` untouched; work on a copy).

---

## 3. Per-parse diagnostic enablement — mechanism + DISABLED-path cost (PT-E1)

**The ask** (`VALUEJS-PT-E-2026-07-05.md`, PT-E1 HIGH): make diagnostics **per-parse scoped**, not gated by
a **module-global** flag. Today `diagnosticsEnabled` lives at `.../typescript/src/parse/utils.ts:6`; it seeds
`expected` only inside `mergeErrorState` (`utils.ts:28-49`). The dead field is `ParseDiagnostic.expected`
(`utils.ts:89`); value.js authors messages that today reach nothing; kf's `ResolvedKeyframes.diagnostics` is
the downstream target. value.js's half is DONE (it routes its own messages via `fail()`→`mergeErrorState`;
it keeps the `expected` field as the forward seam) — the entire remaining gap is parse-that-side.

**Candidate mechanisms (the prototype must weigh all three on the DISABLED-path cost):**
- **(a) Context-scoped flag.** A `diagnostics: boolean` on `ParserContext`/`ParserState`
  (`state.ts:36-53`), set at `parseState` entry (`parser.ts:34-49`), read only where a diagnostic would
  seed. Cost on disabled path: one extra monomorphic field on an already-8-field shape; the read sits
  **inside** the existing cold `if` that guards seeding, so the LL(1) success path never executes it.
- **(b) Closure-captured mode.** Capture the mode into a `parseState`-local at epoch entry (mirrors
  `createLazyCached`'s closure-local cache at `lazy.ts:19-25`, explicitly built to avoid megamorphic-IC
  pollution). Zero new object field; the mode rides the call frame.
- **(c) Two compiled entry variants** — `parse()` vs `parseWithDiagnostics()` — selected at the public
  boundary; the disabled entry is byte-identical to today.

**The disabled-path cost is already largely bought.** PROVE-11 (GO; adversarially CONFIRMED_DELTA) made
`suggestions`/`secondarySpans` **symmetric with the lazy `expected`**: declared native fields, `undefined`
at construction (hidden class fixed from birth, never transitions), populated only under
`diagnosticsEnabled` via `(state.x ??= []).push(...)`. Emitted-JS census: **6 eager `= []` alloc sites → 0**
(reproduced exactly by the adversary); fused-path alloc **1.84 → 0.73 B/op (−60%)**, dead-on. So whichever
enablement mechanism ships, the **disabled path is proven zero-alloc and shape-stable** — the PT-E1
prototype's job is only to move the *enable signal* from module-global to per-parse **without** re-introducing
a hidden-class transition or a hot-path branch. Mechanism (a) or (b) both preserve monomorphism; (c) is the
zero-cost-by-construction fallback if a probe finds any IC churn.

**Caveat carried from PROVE-11 verification:** the headline "three benches clear ≥10%" did **not** fully
reproduce — the **first-char dispatch lane lands ~+5%, missing the ≥10% gate** (fused/unfused clear it). Any
PT-E1 perf claim must state armed-vs-unarmed and cite the per-round paired ratio, not the aggregate.

---

## 4. Inference tightening — consumer redness + the dual-instance hazard (PT-E2)

**The ask** (PT-E2 MED): tighten the `Parser<any>` leaks so consumer barrels stop widening. Leak sites:
`ParserFunction<T>` uses `any` (`parser.ts:14-16`); `any<T extends Array<Parser<any>>>` (`leaf.ts:28`);
`all<T extends Array<Parser<any>>>` (`leaf.ts:154`); `fuseAll(parsers: Array<Parser<any>>)` (`leaf.ts:179`);
`restore(…value: any)` (`state.ts:79`); pervasive `as Parser<unknown>` casts.

**The consumer-redness hazard is the whole point** — a tightening can be unit-test-green yet **redden a
consumer's full type-check** at its `any()`/`all()` sites. Both live external consumers call the tightened
combinators: value.js `master` uses `any`+`all`+`dispatch` (59 `all()` sites per pt-b3 notes; `dispatch` on
the CSS function-name bucket); latex-paper imports `any`. So the prototype **must** run, at those exact sites:
- **value.js `master`**: `vue-tsc -p tsconfig.lib.json` (the `typecheck` script) — the break-detector.
- **latex-paper**: `tsup && vue-tsc -p tsconfig.vue.json` (its `build`).
- parse-that's own **`tsc --noEmit`** with a **no-new-`TS2589`** ("type instantiation excessively deep")
  gate — the tightening's failure mode is instantiation-budget blowup, exactly the risk the directive flags.

**The nested-version dual-instance hazard, resolved.** If a consumer's tree resolves **two** parse-that
copies (e.g. latex-paper pins 0.7.1 while a transitive path pulls 1.x, or value.js's `file:`-dep history
leaves a second copy), TS treats the two `Parser<T>` declarations as **distinct nominal types** →
structurally-identical parsers fail to assign. Resolution: (i) **dedupe** via a single hoisted version
(npm `overrides`/`resolutions`) in the crater harness, and (ii) **document** the single-instance requirement
in the migration note. NOTE: value.js `tranche-u` has **no** parse-that dep, so the dual-instance risk there
is nil; it is live only for latex-paper + any future re-adder. The prototype must construct the two-copy
tree deliberately and show the redness, then show the dedupe cures it.

**`skip`/`sepBy` input-type leaks** (adjacent, 8B-shared): tighten and re-run the same no-new-TS2589 +
consumer-type-check gate; latex-paper/value.js don't call `sepBy` externally, so this is a first-party-only
tightening (lower risk, but still gated).

---

## 5. Prior art (cited)

- **rustc `crater`** — the ecosystem-wide rebuild-against-candidate + regression-classification tool; the
  directive's "migration CRATER" is its direct analogue. The minor-version-ALONE isolation mirrors crater's
  per-PR attribution runs.
- **`@microsoft/api-extractor` + `.api.md` review files** — machine-checkable public-surface contracts with
  CI drift gates; parse-that's `proof:{manifest,subpath,no-span-surface,no-dead-combinator}` are the
  hand-rolled equivalent. The consumer-tag ledger (§1) is an api-report specialized to *consumer* provenance.
- **`tsd` / DefinitelyTyped `dtslint` / `dts-critic`** — type-level regression harnesses that assert
  inference results and catch `TS2589` deep-instantiation; the model for the PT-E2 no-new-TS2589 gate run
  against a consumer's real sites rather than synthetic fixtures.
- **semver + npm `overrides`/`resolutions`** — the dedupe mechanism for the dual-instance hazard; and the
  discipline that "migration is an artifact" (a published upgrade note per breaking cut), which parse-that's
  1.0.0 (`*Span` excision, `chain()` C-16) already needed and latex-paper never consumed.
- **V8 hidden-class / IC monomorphism** (as in R4 §6 and PROVE-11) — the disabled-path cost model for the
  diagnostics-enablement mechanism.

---

## 6. Named risks & counterexamples

1. **The contract evaporates mid-design.** If 8D designs the API "backward from value.js" and value.js 4.0.0
   ships parse-that-free, the backbone is anchored to a departing consumer. Mitigation: anchor to the
   **proof-gated first-party contract + latex-paper's live-narrow surface**, treat value.js `master` as
   live-until-merge, and re-run the import census each pass (it changed between R4 and now).
2. **Pruning a combinator that value.js `master` still ships.** Deleting `all`/`dispatch`/`whitespace`/
   `mergeErrorState` because `tranche-u` dropped them would break the *published* consumer. The consumer tag
   must key on `master`+latex-paper (live), not `tranche-u` (WIP).
3. **latex-paper crater red at an intermediate rung.** Predicted green, but the 0.9→1.0 jump is untested;
   a red at `chain()` C-16 or a transitive `*Span` type would surface only at `vue-tsc`, not `vitest`.
4. **PT-E2 tightening green in isolation, red in `vue-tsc` at value.js's 59 `all()` sites** — the classic
   unit-test-blind break. Only the full consumer type-check catches it.
5. **Dual-instance nominal mismatch** silently degrading a consumer's inference to `any` (masking, not
   erroring) — worse than a hard red. The two-copy tree probe must assert *both* the hard-error and the
   silent-widen cases.
6. **PROVE-11 dispatch-lane over-claim** — do not carry "+10% dispatch" into any 8D perf gate; the reproduced
   figure is ~+5%.

---

## 7. What a prototype MUST test to move this family

1. **Consumer census re-run (blocking, cheap):** machine-emit the per-branch, per-symbol import table for
   value.js(`master`,`tranche-u`), latex-paper, keyframes.js; produce the **consumer-tag ledger** (one row
   per exported combinator: `EXTERNAL-LIVE` / `FIRST-PARTY-BENCH:<file:line>` / `DEAD`). This is the concrete
   artifact that adjudicates KEEP-BROAD vs PRUNE-TO-EXTERNAL. Feeds `proof:no-dead-combinator`.
2. **Migration crater (blocking):** the §2 ladder — latex-paper 0.7.1→…→1.0.0 one minor at a time, value.js
   `master` at `^1.0.0`; `npm test` **and** full type-check (`vue-tsc`/`tsup+vue-tsc`) at each rung; report
   pass-set diff + the exact cut of any red. Retires (or confirms) the R4 version-skew warning.
3. **PT-E1 mechanism bake-off:** implement (a) context-flag, (b) closure-captured, (c) two-entry on a scratch
   copy; measure DISABLED-path per-parse allocation (heapUsed-delta) + IC/hidden-class stability (the
   PROVE-11 emitted-JS census method) for each; confirm the enable-signal moves off the module global with
   zero hot-path regression. Certify with load-robust paired interleaving, armed-vs-unarmed stated.
4. **PT-E2 tightening + consumer-redness gate:** apply the tightened `all`/`any`/`ParserFunction` signatures;
   run parse-that `tsc --noEmit` (no-new-TS2589), then `vue-tsc` on value.js `master` at its 59 `all()` sites
   and `vue-tsc` on latex-paper. Separately construct a **two-copy** consumer tree and show the dual-instance
   mismatch + that npm `overrides` dedupe cures it.
5. **Owner routing (non-blocking, carried):** the P2 question — does value.js 4.0.0/`tranche-u` supersede
   `master`? — decides whether the backbone plans for a two-consumer world (latex-paper + first-party) or a
   three-consumer one. Design both; do not block on the answer.
