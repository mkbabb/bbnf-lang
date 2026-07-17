# LOCK-8 pass-3 — research-D — family 8D CONSUMER-CONTRACT-FIRST

*The TS backbone designed backward from its living consumers. Pass-3 replaces the static
grep census with RAN gates: the consumer suites were executed, the type baseline was executed
under the project's own config AND under a full-scope config, and the divergence between the
two is the family's decisive new fact — the project's own type gate is a FALSE GREEN that
never checks the parse-that consumers. parse-that is READ-ONLY; every run was against the
checked-out consumer trees at their pinned parse-that, never a write. The single sanctioned
artefact into `~/Programming/parse-that` is coordination letter BBNF-PT-G. Figures marked RAN
are executed on the trees; UN-RUN figures carry the exact command that closes them.*

---

## 0. CONSUMER LIVENESS — RESOLVED by commit cadence + import census (PLANE-WIDE P1/P2/P3)

The census flip is now confirmed as a *settled, actively-maintained* state, not a
mid-migration WIP artifact. Fresh `git log -1` + `package.json` + `grep` over each tree:

| consumer | version | HEAD commit date | direct parse-that dep | src parse-that imports |
|---|---|---|---|---|
| **value.js** | **4.0.0** | **2026-07-15** (1 day pre-today) | **none** (no `dependencies` key at all) | **0** |
| **keyframes.js** | **6.0.0** | **2026-07-15** (1 day pre-today) | none (consumes `@mkbabb/value.js@4.0.0`) | **0** |
| **latex-paper** | 0.2.1 | 2026-07-02 (2 wks stale) | `^0.7.1` → resolves **0.7.1** | **8 sites** |

- **P1 — value.js posture is INTENTIONAL, not WIP.** value.js `4.0.0` (HEAD `86a9a17`)
  committed one day before today with an *entirely absent* `dependencies` key and zero
  `@mkbabb/parse-that` src imports. A tree this actively worked carrying no runtime dep is a
  decided architecture. CSS parsing was re-homed hand-written into `value.js/src/v4/css/
  {grammar.ts,stylesheet.ts}` (zero combinators); `src/subpaths/math.ts:2` and
  `transform.ts:4` self-label "parse-that-FREE". parse-that's own `typescript/src/parse/
  utils.ts:150-156` records the departure. **Settled on both sides.**
- **P2 — keyframes.js is transitively severed and live.** `6.0.0`, committed 2026-07-15,
  depends on `value.js@4.0.0`; its only historical route to parse-that was through value.js's
  CSS grammar, now gone. `src/animation/internal/leaves.ts` self-certifies parse-that-free.
- **P3 — latex-paper is the SOLE surviving consumer: stale, not frozen.** HEAD 2026-07-02
  (two weeks behind), still committing, pinned `^0.7.1`. It **will** need migration to consume
  a 1.1.0 cut; no evidence it is being actively re-pointed. Its 5-symbol surface is the
  sharpest possible live contract.

**Owner posture is the only remaining unknown (XL-G, non-blocking).** Carry both hypotheses:
(H-prune) the live external contract is latex-paper's 5 symbols and the surface should shrink;
(H-firstparty) parse-that's own RAN suite legitimizes the wider surface. §1 adjudicates.

---

## 1. CONSUMER-SET ADJUDICATION — live external contract vs first-party suite (8D-1)

**The live external combinator surface is exactly 5 symbols** (RAN, §3): latex-paper's 8
import sites reach only `{Parser, ParserState, regex, string, any}` (`grammar/math.ts:5`,
`text.ts:5`, `commands.ts:9`, `environments.ts:6`, `document.ts:5`, `primitives.ts:5`,
`compile.ts:7` type-only, `bibtex/parser.ts:7`). Zero sites reach `./core`, `./diagnostics`,
`./packrat`, `./utils`. R4's nine-symbol census was **value.js's** and is dead.

**The consumer-less remainder** (zero external consumer): `all`, `fuseAll`, `dispatch`,
`whitespace`, `mergeErrorState`, `sepBy`, `many`, `chain`, `not`/`minus`, `wrap`, `trim`,
`peek`, `lookAhead`, `recover`, `opt`, `next`, `skip`, `then`, `or`, `map`, `mapState`, `eof`,
the entire **packrat** subsystem, the entire **diagnostics** subsystem.

**Adjudication (Constitution viii — no consumer-less substrate). 8D recommends H-firstparty
with a named floor**; H-prune is destructive to a proven-fast core for no offsetting benefit:

- **KEEP — first-party consumer = the RAN test/bench corpus, declared explicitly.** The
  combinator *primitives and algebra* (`all`/`fuseAll`, `dispatch`, `then`/`or`/`map`/`chain`/
  `sepBy`/`many`/`opt`/`skip`/`next`/`wrap`/`trim`/`peek`/`lookAhead`/`not`/`minus`/`eof`,
  `whitespace`) are exercised by parse-that's own `test/*.test.ts` (13 files, 124 tests) and
  `pt-b3-fusion.bench.ts`. Under Constitution i these are "combinators… the floor"; the RAN
  suite is their *disclosed* first-party consumer — Y.13 satisfied by an executable oracle,
  **not** by liveness assertion. The tranche must state this explicitly: *"the parse-that
  conformance corpus is the first-party consumer of the combinator algebra; its green suite is
  the Y.13 discharge."*
- **QUARANTINE, don't delete — packrat + diagnostics.** Not primitives, zero consumer of any
  kind today. packrat is a true no-op until first `memoize()` (`packrat.ts:156,290`);
  diagnostics is structurally dead on the default path (`utils.ts:6` global off). PT-3.W2
  gives diagnostics its FIRST consumer (per-parse scope; keyframes' future
  `ResolvedKeyframes.diagnostics`) — the W1→W2 Y.13 pairing. **packrat has NO pending
  consumer**; its Y.13 discharge is its own `proof:packrat-{cross-input,reentrant,large-offset,
  armed}` suite — declare that the discharge, or mark packrat SPEC-ONLY-until-armed.

**Kept:** the 5 live symbols + the primitive algebra behind the RAN suite. **Dropped:**
nothing deleted; packrat/diagnostics labelled substrate-with-pending-or-suite-consumer.
Pruning the algebra orphans the RAN suite and the bench floor for no consumer gain — H-prune
rejected on evidence.

---

## 2. THE MIGRATION CRATER — RAN, and the baseline hides a FALSE GREEN (8D-2)

Pass-2 specified the crater; pass-3 executed both axes against the pinned 0.7.1 world, and the
result overturns the crater's own gate definition.

**Runtime axis — GREEN, interpretable.** `cd latex-paper && npx vitest run` →
**127/127 tests pass, 14/14 files, 1.05 s** at parse-that 0.7.1. Clean before-state.

**Type axis — the decisive finding: the project's own type gate DOES NOT CHECK THE PARSE-THAT
CONSUMERS.** latex-paper's build type gate is `vue-tsc -p tsconfig.vue.json`
(`package.json:30`). Run verbatim it reports **0 errors** — an apparent green baseline. But
`tsconfig.vue.json` declares `include: ["src/vue*.ts","src/vue*.vue","src/types*.ts"]` — it
**excludes `src/grammar/**` and `src/bibtex/**` entirely**, i.e. every file that imports
parse-that. The project's green type gate is green *because it never type-checks the combinator
consumers.* The base `tsconfig.json` additionally sets `skipLibCheck:true`, suppressing
cross-instance `.d.ts` errors. **A crater trusting `vue-tsc -p tsconfig.vue.json` — the
consumer's own gate — is a FALSE GREEN: it cannot see the tightening or the dual-instance.**

Forcing a full-scope check (`npx vue-tsc --noEmit` over the whole tree) reddens with the
**dual-instance hazard, empirically confirmed**:
```
src/grammar/compile.ts(76,5): error TS2322: Type '…@mkbabb/bbnf-lang/…/Nonterminals'
  is not assignable to type '…src/grammar/compile").Nonterminals'.
    Type 'Parser<any>' is missing … : flags, minus, call, recover
```
`compile.ts:6` imports `BBNFToParser` from `@mkbabb/bbnf-lang` (a **runtime value** import,
called at `:19,:81`), whose nested parse-that **0.6.0** exports a `Parser` structurally
divergent from latex-paper's direct **0.7.1** `Parser` (0.6.0 lacks `flags/minus/call/
recover`). Two parse-that instances coexist; their `Parser` types do not unify.

**Consequences (the family's sharpest new law):**
1. **The consumer's own `build` type gate is scope-blind to the parse-that files.** The honest
   crater type leg must **force-check `src/grammar/** + src/bibtex/**` with `skipLibCheck:false`**
   (a dedicated `vue-tsc --noEmit` over those files), or it measures nothing on the type axis.
2. **The dual-instance divergence is a pre-existing baseline red, PT-3-independent.** Any red
   after a 1.1.0 bump must be attributed against this baseline, not blamed on the tightening.
3. **The 1.1.0 bump WIDENS the 0.6.0↔1.1.0 divergence.** The mask (scope-exclusion +
   `skipLibCheck`) holds it dormant; the crater must run the unmasked check to prove the state.

**Attribution-isolated crater (the RUN order):**
- **(a) baseline, pinned 0.7.1:** vitest GREEN 127/127 [RAN]; force-scope vue-tsc records the
  dual-instance/`.bbnf` baseline [RAN — §5].
- **(b) 0.7.1 → 1.0.0 ALONE:** pack the 1.0.0 tree, `rm -rf node_modules/@mkbabb/parse-that &&
  npm i ../parse-that-1.0.0.tgz`, re-run both axes. Isolates the `*Span`-excision / C-16 cut so
  a 1.0.0-era break is never mislabeled 1.1.0. latex-paper imports no `*Span`/`chain` →
  expected GREEN, must be RUN.
- **(c) 1.0.0 → 1.1.0:** install the 1.1.0 candidate tarball; re-run. The 1.1.0 candidate does
  not yet exist (parse-that HEAD is 1.0.0 at `ef10d5b`); the crater is constructable the moment
  PT-3.W5 produces it.

**value.js / keyframes.js — NEGATIVE guards only** (nothing to break): tripwires
`! grep -rq "@mkbabb/parse-that" src/` + `'dependencies' absent`, guarding against silent
re-expansion of the parse-that-free invariant.

---

## 3. THE IMPORT ORACLE — RE-RAN (8D + PLANE-WIDE SPEC FIDELITY)

`grep -rEn "from ['\"]@mkbabb/parse-that"` over `src` of all three trees:

| repo | src hits | subpaths reached | symbol union |
|---|--:|---|---|
| value.js 4.0.0 | **0** | — | ∅ |
| keyframes.js 6.0.0 | **0** | — | ∅ |
| latex-paper 0.2.1 | **8** | none (all top-level) | `{Parser, ParserState, regex, string, any}` = **5** |

**SPEC FIDELITY divergences (feed the re-cut):**
- PT-3 §4/§0 and R4 §7 assume **value.js is a live consumer** ("value.js's 59 `all()` sites",
  "value.js re-locked 1.0.0→1.1.0"). **Divergence:** value.js has zero parse-that; the W5
  "three consumer suites green" gate is really **one** positive suite (latex-paper) + two
  negative guards. Re-cut W0/W5.
- PT-3.W0 treats latex-paper's `tsup && vue-tsc` as the type gate. **Divergence:** that gate
  excludes the parse-that consumers; the crater's type leg needs an explicit force-scope run,
  which no PT-3 wave currently specifies. **New requirement.**
- PROVE-11's dispatch lane is carried in PT-3.W1 at "~+5%" honest (CONFIRMED_DELTA halved the
  prover's +10.2%). Faithful — no divergence.

---

## 4. PT-E1 DIAGNOSTIC MECHANISM — cost on the DISABLED path (8D-3)

**Mechanism: a primitive boolean field on the already-per-parse `ParserState`**, not a scope
object. `ParserState` is constructed per-parse in `parseStateInner` (`parser.ts:56-65`); the
furthest/`getLineAndColumn` legs of the diagnostic triple are already always-on there. The
only module-global residue is `utils.ts:6 let diagnosticsEnabled = false` (read in
`mergeErrorState :33`, `addSuggestion :51`, `addSecondarySpan`).

Replace it with `state.diag: boolean`, set once from a parse option (`.parse(src,
{diagnostics:true})`), threaded to child states. `mergeErrorState(state,label)` reads
`state.diag`. **Cost on the disabled path: one boolean field read inside the state's existing
monomorphic hidden class — zero allocation, zero new object, zero hidden-class transition** (a
boolean declared at construction cannot transition).

**Why a boolean field beats a `DiagnosticScope` object** (the PT-3.W2 candidate): a scope hung
off `ParserContext` (grammar-construction lifetime) is the *wrong lifetime*; a scope allocated
per `.parse()` re-introduces the exact per-parse allocation named as risk R2. The state is
*already* allocated per-parse — the boolean adds nothing. **The scope IS the state.** MEASURE,
don't assert: the disabled-path probe is `%HaveSameMap(stateDiagTrue, stateDiagFalse) === true`
under `node --allow-natives-syntax`, plus a re-run of the PROVE-11 paired harness with
diagnostics off holding the W1 then-chain/fused floors. Deletion proof (precept #6):
`rg 'let diagnosticsEnabled' src/` → 0.

**Substrate note:** the PROVE-11 lazy `suggestions`/`secondarySpans` fields are `= []`
non-optional at HEAD (`state.ts:44-45`) — that lever is **un-landed**; W1 lands it, W2's
`state.diag` gate populates them lazily. Y.13 discharged W1(substrate)→W2(consumer).

---

## 5. INFERENCE TIGHTENING vs the consumer's type-check (8D-4) — RAN, and INVISIBLE to the
   consumer's own gate

latex-paper has **3 live `any()` call sites** (RAN): `grammar/text.ts:145`
`any(bracedAccent, unbracedAccent, namedAccent)` → `Parser<TextNode>`; `text.ts:154`
`any(...alts)` — a **spread of a homogeneous `Parser[]`**; `document.ts:81` `any(…)` →
`Parser<LatexNode|null>`. All three are **typed-target assignments or homogeneous spreads** —
precisely where PT-E2's tightening (`any<T extends Array<Parser<any>>>` →
`ReadonlyArray<Parser<unknown>>` with `infer V`) is a **superset for well-typed sites**: a
check-position `Parser<TextNode>` target accepts a precisely-inferred `Parser<TextNode>`
unchanged; a homogeneous spread infers cleanly. **Low tightening risk at all three sites.**

**But the risk is UNOBSERVABLE from latex-paper's own type gate** — `text.ts`/`document.ts`
live in `src/grammar/**`, which `tsconfig.vue.json` excludes (§2). The tightening could redden
these sites and latex-paper's `build` would still pass green. **The crater's type leg MUST
force-check `src/grammar/**` under `skipLibCheck:false`, or the inference axis is a null
measurement.** This is the single most important correction to the PT-3 crater design.

**The nested-version dual-instance hazard — RESOLVED (runtime-benign, type-latent-MASKED):**
- **Runtime: BENIGN.** `BBNFToParser` (bbnf-lang's nested parse-that 0.6.0) is instantiated at
  runtime (`compile.ts:19,81`) alongside the direct 0.7.1 instances, yet **vitest is GREEN
  127/127** — latex-paper consumes the `Nonterminals` record structurally (`.parse()`), never
  `instanceof`-crosses the boundary. No runtime collision.
- **Type: DIVERGENT but MASKED** by two mechanisms — `tsconfig.vue.json` scope-exclusion of the
  grammar files + base `tsconfig.json skipLibCheck:true`. A full-scope, `skipLibCheck:false`
  check reveals TS2322 (§2).
- **Disposition (dedupe OR document):** a 1.1.0 bump of the *direct* dep leaves the nested
  0.6.0 in place and widens the type divergence. **DEDUPE** = also bump `@mkbabb/bbnf-lang`'s
  nested parse-that pin so both resolve one instance (removes divergence at source);
  **DOCUMENT** = a MIGRATION.md row stating the divergence is runtime-benign + type-masked and
  the mask is intentional. Dedupe is the clean route; document the honest fallback if bbnf-lang
  cannot be re-pinned. Either way the crater must run the unmasked check to prove the choice.

---

## 6. INTEGRATION SEAMS (path:line)

- Consumer state: `value.js/package.json` (no `dependencies`), `src/v4/css/{grammar,
  stylesheet}.ts` (hand-written, zero combinators), `src/subpaths/{math,transform}.ts`;
  `keyframes.js/package.json` (`value.js@4.0.0`), `src/animation/internal/leaves.ts`;
  `latex-paper/package.json:37-38` (bbnf-lang `^0.1.1` + parse-that `^0.7.1`).
- latex-paper imports: `grammar/{math:5,text:5,commands:9,environments:6,document:5,
  primitives:5,compile:7}.ts`, `bibtex/parser.ts:7`; `any()` sites `text.ts:145,154`,
  `document.ts:81`.
- Dual-instance: `compile.ts:6` (`BBNFToParser` value import), `:19,:81` (runtime calls), `:15`
  (`Nonterminals = Record<string, Parser<any>>`); nested 0.6.0 at
  `node_modules/@mkbabb/bbnf-lang/node_modules/@mkbabb/parse-that`; base `tsconfig.json`
  `skipLibCheck:true`; `tsconfig.vue.json include` excludes `src/grammar/**`.
- Diagnostics: `utils.ts:6` global; `mergeErrorState utils.ts:29-49`; `parseStateInner
  parser.ts:56-65`; state fields `state.ts:39-46`; `getLineAndColumn state.ts:127`.
- packrat: key `packrat.ts:71-100`; latch/arm `packrat.ts:156,290`.

---

## 7. PRIOR ART (cited)

- **rust-lang/crater** — build every downstream against a candidate compiler, classify
  regressions. Our instance is a **1-consumer crater + 2 negative guards** — the honest form
  for a collapsed census.
- **The `include`-scope false-green** mirrors the known **tsc project-scope pitfall**
  (TypeScript handbook: files outside `include`/`files` are silently unchecked) and
  **`skipLibCheck` masking cross-version type divergence** (the canonical "two copies of a
  typed package" failure, Microsoft/TypeScript #38538 lineage).
- **tsd / expect-type / `$ExpectType`** (DefinitelyTyped's type-test convention) — the
  mechanism for the inference-break gate: `@ts-expect-error`/`expectType` under `--noEmit`.
- **Dual-instance package hazard** — the standard npm nested-version failure; value.js's own
  Vite `dedupe` block (`value.js/vite.config.ts`) is the in-tree precedent for the dedupe route.
- **Error-in-state vs error-in-result** — parsec/megaparsec carry the error on the returned
  `State` (the field-triple parse-that implements, `parser.ts:56-65`); nom's `IResult` is the
  result-union. The consumer axis is empty (latex-paper reads neither), so the choice is 8B's,
  unconstrained by 8D.

---

## 8. WHAT A PROTOTYPE MUST TEST TO MOVE THIS FAMILY

1. **RUN the force-scope type crater** — `vue-tsc --noEmit` over `latex-paper/src/grammar/** +
   src/bibtex/**` with `skipLibCheck:false`, at 0.7.1 (baseline), then 1.0.0, then the 1.1.0
   candidate. The project's own `vue-tsc -p tsconfig.vue.json` gate is a FALSE GREEN and must
   NOT be the crater's type leg.
2. **RUN 0.7.1→1.0.0 ALONE** (attribution isolation) before the 1.1.0 step.
3. **Inference-tightening probe** at the 3 `any()` sites (`text.ts:145,154`, `document.ts:81`)
   under the force-scope check — does the tightened `ReadonlyArray<Parser<unknown>>`+`infer`
   signature redden any? (Predicted: no — all typed-target/homogeneous — but must be RUN
   because latex-paper's own gate cannot see it.)
4. **Dual-instance disposition probe** — attempt the dedupe (re-pin bbnf-lang's nested
   parse-that) and confirm the force-scope TS2322 clears; else write the MIGRATION.md
   runtime-benign/type-masked row.
5. **Boolean-diagnostics probe** — `state.diag` field; `%HaveSameMap(on,off)===true`; PROVE-11
   paired harness with diagnostics off holds the W1 floors (no alloc, no transition).
6. **Negative-guard tripwires** — mechanical grep asserting value.js + keyframes.js stay
   parse-that-free (a permanent contract-re-expansion sentinel).

---

## 9. NAMED RISKS / COUNTEREXAMPLES

- **R1 (headline) — the crater's green is a false green.** The consumer's own type gate
  (`vue-tsc -p tsconfig.vue.json`) excludes the parse-that-importing files; a crater trusting
  it "passes" while never checking the tightening or the dual-instance. Cure: force-scope
  `src/grammar/**` + `skipLibCheck:false`. **CONFIRMED empirically (§2, §5).**
- **R2 — the design target (value.js) is a phantom.** PT-3.W5 gates on "value.js re-locked
  1.0.0→1.1.0"; value.js has zero parse-that (committed 1 day pre-today). Any table citing
  value.js's `all()` sites measures nothing. Cure: re-cut W0/W5 to 1-positive+2-negative.
- **R3 — the 1.1.0 bump widens the dual-instance divergence.** Bumping direct parse-that while
  bbnf-lang stays on nested 0.6.0 grows the type gap; the mask holds but the crater must prove
  it. Cure: dedupe or documented mask (§5).
- **R4 — the sole positive gate could be dormant/red-independent.** latex-paper is 2 weeks
  stale at 0.7.1. **Refuted for now:** its suite is RAN GREEN (127/127) and its build type gate
  is GREEN — the before-state is interpretable. But it MUST be re-baselined GREEN before every
  candidate step, or the crater is uninterpretable.
- **R5 — Y.13 for packrat has NO pending consumer.** diagnostics gets one in W2; packrat does
  not. Cure: declare the packrat proof-gate suite its first-party consumer, or mark
  SPEC-ONLY-until-armed (§1) — explicitly.
- **R6 — a bbnf agent edits parse-that.** All pass-3 runs were against consumer trees at their
  pinned parse-that; `~/Programming/parse-that` was never written. The 1.1.0 landing rides
  letter BBNF-PT-G, owner/parse-that-side only.
