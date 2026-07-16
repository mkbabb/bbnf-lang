# LOCK-8 · FAMILY 8D — THE CONSUMER-CONTRACT-FIRST PLANE (pass-3 research)

*The TS backbone designed backward from its actual live consumers: their usage patterns
define the retained API, perf gates ride their suites, and semver + migration are the
first-class deliverables. This report re-censuses the live graph from source of record and
finds the pass-2 consumer model materially stale; the family's strongest form is rebuilt on
the measured graph, not the assumed one.*

Package under contract: `@mkbabb/parse-that`, TS root `~/Programming/parse-that/typescript`,
frozen at `1.0.0` (HEAD `ef10d5b`, the VALUEJS-PT-E ask letter; tags `v1.0.0` = `7eab78c`
S.H4 + `043c4d1` S.H2 `*Span` excision). READ-ONLY; any landing rides letter BBNF-PT-G.

---

## 1. THE CONSUMER LIVENESS LEDGER (the decisive re-census)

Prior family text (R4 §7, PT-3 §4) modeled three live consumers — value.js (9 imported
symbols incl. `dispatch`, "59 `all()` sites"), latex-paper (5 symbols, `^0.7.1`),
keyframes.js (transitive via value.js). A fresh per-tree census from source of record
contradicts that model on two of three rows:

| consumer | version @ tip | parse-that dep (pkg.json) | lock entries | src import sites | disposition |
|---|---|---|---|---|---|
| **value.js** `master` | **3.1.0** | `^1.0.0` | **3** | (published posture) | LIVE-published |
| **value.js** `tranche-u` (working tip) | **4.0.0** | **absent** | **0** | **0** | **SHED — migrating off** |
| **latex-paper** | 0.2.1 | `^0.7.1` → installed `0.7.1` | present | **8 sites, 5 symbols** | **LIVE — sole undisputed** |
| **keyframes.js** | 6.0.0 | absent (direct) + value.js `4.0.0` | — | **0** | **DEAD — no path** |

**Evidence, per row:**

- **value.js is bifurcated.** The published/default branch is `origin/HEAD → master`, version
  **3.1.0**, retaining `@mkbabb/parse-that: ^1.0.0` (three lock entries). The active
  development tip is `tranche-u`, version **4.0.0**, which has **deleted** the dependency
  from `package.json`, carries **zero** parse-that entries in its `package-lock.json`, and is
  source-clean of parse-that imports (`grep -rE "from ['\"]@mkbabb/parse-that" src` = 0). Its
  subpath modules now self-declare the severance: `src/subpaths/math.ts:2` "parse-that-FREE",
  `src/subpaths/transform.ts:4` "pure leaf … zero parse-that". The `node_modules/@mkbabb/
  parse-that@1.0.0` still on disk is an **orphan** — absent from tranche-u's lock, a residue
  of the pre-migration install. value.js still parses CSS (`src/parsing/{color,stylesheet,
  timeline}` survive), but with a hand-rolled parser, not this combinator core.

- **The P2 adjudication (carry BOTH per XL-G):** value.js's parse-that-free posture is a
  **WIP campaign-branch artifact**, not (yet) the published contract. tranche-u is an advanced
  but **paused** campaign (git log: "THE PAUSE — V campaign halted at owner order post-pass-4,
  thesis 78%"). It has **not** merged to master. So:
  - **H1 (forward intent — shed):** tranche-u lands → value.js **exits** the consumer set;
    the arity-N / `all()` specialization loses its cited live corpus entirely.
  - **H2 (published posture holds):** master 3.1.0 remains a live `^1.0.0` consumer.
  Owner confirmation is the discriminator and is NON-blocking; both hypotheses are carried
  into every downstream disposition below. The single fact that IS settled: **the "59
  `all()` sites" live corpus no longer exists at value.js's working tip** — it is a
  frozen-master figure at best, a dead figure under H1.

- **latex-paper is the sole undisputed live external consumer.** Pinned `^0.7.1`, installed
  `0.7.1`. Eight import sites across `src/grammar/{primitives,commands,math,text,document,
  environments}.ts`, `src/grammar/compile.ts` (type-only `import type { Parser }`), and
  `src/bibtex/parser.ts`. Its **measured** contract is narrower than the algebra:
  - Free functions actually imported+called: `regex` (30×), `string` (24×), `any` (3×),
    `Parser`, `ParserState`. It does **not** import or call parse-that `all`, `dispatch`,
    `whitespace`, `eof` (the 3 bare `all(` hits are a `.all()` map method in
    `src/transform/{sections,labels}.ts`, not this package).
  - Parser instance methods used: `.map` (98×), `.skip` (43×), `.trim` (31×), `.then` (23×),
    `.next` (16×), `.opt` (11×), `.chain` (2×), `.not` (1×). It uses **none** of `.or`,
    `.many`, `.sepBy`, `.wrap`, `.peek`, `.lookAhead`, `.minus`, `.recover`, `.mapState`,
    `.debug` (n-ary alternation is expressed via free `any()`).
  - Toolchain: `build = tsup && vue-tsc -p tsconfig.vue.json`, `test = vitest run` — the
    `vue-tsc` leg is precisely the full type-check surface the 8D reddening question targets.

- **keyframes.js is off the graph.** v6.0.0 has no direct parse-that dep and no src import
  (`src/animation/internal/leaves.ts:9` self-certifies "parse-that-FREE"). Its only path was
  transitive via value.js — and value.js `4.0.0` (the version keyframes depends on,
  `dependencies['@mkbabb/value.js'] = 4.0.0`) is itself parse-that-free. Its
  `node_modules/@mkbabb/parse-that@1.0.0` is a stale orphan of the same kind. **keyframes is
  not a gate under any hypothesis.**

**Net:** the live external consumer set has collapsed from three to **one certain
(latex-paper) + one contingent (value.js master under H2)**. Every pass-2 deliverable that
gated on "three consumer suites green" (PT-3 W0/W5) is built on a stale graph and must be
re-cut against this ledger.

---

## 2. CONSUMER-SET ADJUDICATION — first-party suite vs. live external contract

**The question:** for combinators with no external consumer, is the first-party test/bench
suite a legitimate consumer (Y.13 satisfied), or should the public surface prune to the live
external contract?

**The governing distinction:** parse-that is a **published standalone library**, not an
app-internal module of value.js. For an app-internal module, Y.13 ("consumer-less substrate
forbidden") bites hard — dead surface is deletion-bait. For a **published combinator algebra**,
the product *is* the algebra, and its own conformance + bench suite (13 `test/*.test.ts` files,
4 `test/benchmarks/*.bench.ts`) is a **legitimate first-party consumer** — a downstream user
who installs the package is the latent external consumer the suite stands in for. Pruning to
latex-paper's 5-symbol slice (`regex/string/any/Parser/ParserState`) would gut the library to
a regex+string+alternation front and destroy its reason to exist. **Therefore the algebra is
kept; only genuinely dead demo/legacy surface is pruned.** Concrete disposition:

- **TIER-A — KEEP (hard external floor).** `Parser` (class + methods `map skip trim then next
  opt chain not`), `ParserState`, `regex`, `string`, `any`. These have a live external
  consumer (latex-paper) under BOTH hypotheses. Any signature change here rides latex-paper's
  `vue-tsc` gate (§5). Under H2 add `all`, `dispatch`, `whitespace`, `mergeErrorState` (the
  value.js-master surface).

- **TIER-B — KEEP as library algebra (first-party-consumer legitimate).** The rest of the
  combinator algebra: `.or .many .sepBy .wrap .peek .lookAhead .minus .negate .mapState
  .recover .eof`, `eof/trimStateWhitespace`, `lazy` family (`getLazyParser createLazyCached
  lazy`), packrat (`memoize mergeMemos resetPackrat`), `createParserContext spanToString
  mergeSpans`. Each is exercised by a first-party test (chain/reentrancy/validate-parsers/
  memoize/json/csv/math). For a published algebra this is a sound Y.13 discharge — the
  conformance suite is the consumer of record. Do **not** prune; but every such row is
  labeled FIRST-PARTY-ONLY in the divergence ledger so the gestalt sees it is not
  externally load-bearing.

- **TIER-C — PRUNE / RE-HOME candidates (no external + demo-grade or being-deleted).**
  - The module-global diagnostics quintet `enableDiagnostics disableDiagnostics
    collectDiagnostic getCollectedDiagnostics clearCollectedDiagnostics` — **being deleted**
    by the per-parse-scope rework (§4); they DROP from the public barrel, replaced by the
    scoped API. This is a real surface change (see semver, §5).
  - `jsonParser` / csv domain parsers (`parsers/*`, "domain showcase") and `containsDelimiter`
    / `splitBalanced` (`split.ts`, BBNF `toDoc()` format-time internals) — no external
    consumer, first-party-test-only, arguably example/internal rather than public API.
    Candidate to move behind a `./examples` subpath or unexport, **but** the change is
    surface-breaking and buys little; recommend RETAIN-BUT-LABEL over prune, deciding at the
    gestalt with the owner, not unilaterally.

**Adjudication verdict:** the disposition is KEEP-THE-ALGEBRA (Tier-A hard, Tier-B
first-party-legitimate) + DROP only the diagnostics module-globals that the scope rework
already deletes. The "prune to live external contract" route is **rejected** — it is the
right rule for an app-internal module and the wrong rule for a published library; misapplying
it would delete the product.

---

## 3. THE MIGRATION CRATER (re-cut against the measured graph)

The directive asks: baseline the live consumer GREEN at its pinned version, reinstall the
candidate tarball, run tests + full type-check, and **isolate the minor-version step alone**.
On the measured graph the crater collapses and re-shapes:

- **latex-paper — the real crater, two legs (attribution isolation mandatory).** Its pin is
  `^0.7.1`, which resolves `<0.8.0` and **predates the 1.0.0 breaking cut** (`*Span` excision
  S.H2, `chain()` C-16 falsy-seed fix). A 1.0→1.1 "minor step alone" is therefore
  **not directly runnable** for latex-paper — it never crossed 0.7→1.0. The crater must be
  two legs so a break is attributed to the cut that owns it:
  1. **Leg A (0.7 → 1.0, MAJOR):** install a packed `1.0.0` tarball into a latex-paper scratch,
     `npm test` (`vitest run`) + `vue-tsc -p tsconfig.vue.json`. Because latex-paper touches
     only `regex/string/any/Parser/ParserState` + methods `map/skip/trim/then/next/opt/
     chain/not` — all still exported and behaviorally intact at 1.0.0 — the expected result
     is GREEN; any RED here is a **1.0.0** break, not a 1.1.0 one, and must be labeled as such.
  2. **Leg B (1.0 → 1.1, the actual PT-3 delta, MINOR):** from Leg A's green tree, install the
     `1.1.0` candidate, re-run both. This is the isolated minor step the directive wants.
- **value.js — crater is hypothesis-dependent.** Under H1 (tranche-u lands): **no crater**
  (no dependency). Under H2 (master 3.1.0 live): a **clean single minor leg** 1.0→1.1
  (master already sits at `^1.0.0`), `npm test` (`vitest`, jsdom). Its `dist/` type-check is
  the full-type-check surface for the H2 arm.
- **keyframes.js — crater MOOT** (no dependency path under any hypothesis).

**The re-cut gate:** "three consumer suites green" (PT-3 W5) is stale. The correct close gate
is **latex-paper Leg-B green (+ Leg-A attribution recorded)**, plus **value.js-master Leg
green IF H2**, with keyframes struck. This is a named supersession of the pass-2 gate.

---

## 4. PER-PARSE DIAGNOSTIC ENABLEMENT — mechanism + disabled-path cost

**The defect (measured):** enablement is a **module-global mutable** flag `diagnosticsEnabled`
(`utils.ts:6`, toggled by `enableDiagnostics`/`disableDiagnostics`) feeding a **module-global
buffer** (`utils.ts:138-146`). Nested/interleaved parses cross-contaminate; the `expected`
set is structurally dead on the default path because the global is off.

**Candidate mechanisms and their disabled-path cost (the 8D comparison):**

| mechanism | per-parse correct? | disabled-path alloc | hidden-class impact |
|---|---|---|---|
| (a) module-global flag+buffer (current) | **NO** (cross-contaminates) | zero | none, but wrong scope |
| (b) scope object allocated **per top-level `.parse()`** | yes | 1 obj / top-level parse (amortized over whole tree) | stable if shape-fixed |
| (c) **nullable scope field on the parse epoch, read lazily** | yes | **zero** (null read) | **none** (field fixed from birth) |

**Recommended mechanism — (c), the shape-stable per-parse scope.** Enablement is carried as a
**parse option** (`.parse(src, { diagnostics: true })` or a scoped `withDiagnostics(fn)`),
never a module-global write. A `DiagnosticScope` opens/closes on the **parse epoch** — the
`parseState` try/finally at `parser.ts:34-49`, the same seam that already brackets
`packratEnter`/`packratExit` — and threads onto the per-parse `ParserState` (constructed in
`parseStateInner`, already per-parse). The scope is **null on the disabled path**; the lazy
`suggestions`/`secondarySpans` fields (declared `undefined` at construction, native class
fields) populate only under `scope !== null && scope.enabled`.

**Disabled-path cost, with evidence to demand:**
- **Allocation:** ZERO on the diagnostics-off path — no array birth (the lazy-field mechanism
  removes eager `= []`, an emitted-JS census of 6→0 eager allocations and a measured **−60%
  fused alloc B/op**), and the scope itself is a `null` field read, not an object, when off.
  The one legitimate allocation is a single scope object per **top-level** `.parse()` when
  diagnostics are ON — never per inner combinator-parse (the hazard in §7-R2).
- **Hidden class / monomorphism:** a `ParserState` that never touches diagnostics and one that
  populates `suggestions` must satisfy `%HaveSameMap` = TRUE (the fields are `undefined` vs a
  fresh array at the SAME map). This is the falsifiable shape oracle: run under `node
  --allow-natives-syntax`, assert same-map; the fixed-from-birth field claim is the invariant.
- **Regression floor:** re-run the paired cold-per-op harness (single build/process, A,B,A,B
  interleave, median-of-13-batches × ≥300k ops, N≥12 rounds, loadavg recorded — NOT `vitest
  bench`, which is warm/cross-run and load-confounded) with diagnostics OFF; the then-chain
  and fused lanes must hold their landed floors (then-chain ≥ +11%, fused ≥ +13% over the
  eager-alloc baseline). If the scope open/close costs anything on the off path, this catches
  it. The dispatch lane is carried at the **honest ~+5%** (verified halved from a claimed
  +10.2%), a no-regression lane, NOT a ≥10% gate — over-claiming it is the exact self-deception
  the refutation laws outlaw.

---

## 5. INFERENCE TIGHTENING — does it redden the live consumer's type-check?

**The tightening (measured leak sites):** duplicated `ExtractValue` (`parser.ts:9-11` and
re-defined in `all()` at `leaf.ts:155-157`); `Parser<any>` leaks at `ParserFunction<T>`
(`parser.ts:14-16`), `any<T extends Array<Parser<any>>>` (`leaf.ts:28`), `all<…>`
(`leaf.ts:154`), `fuseAll(Array<Parser<any>>)` (`leaf.ts:179`), `restore({… value: any})`
(`state.ts:79`). The tightening replaces `Array<Parser<any>>` with `ReadonlyArray<Parser<
unknown>>` + `infer V`.

**Where it can redden — latex-paper is the ONLY live external site, and it IS in the blast
radius:** latex-paper imports and calls parse-that `any()` at 3 sites
(`src/grammar/{environments,text,commands,document}.ts`, `src/bibtex/parser.ts`) — precisely
the `leaf.ts:28` signature being tightened. Its full type-check is `vue-tsc -p
tsconfig.vue.json` (its build's second leg), and unit tests (`vitest run`) will **not** catch
a pure type break — only the `vue-tsc` run will. **The falsifiable test:** apply the tightened
`any`/`all` signatures to a scratch 1.1.0 tarball, install into latex-paper, run `vue-tsc -p
tsconfig.vue.json`, diff the diagnostic set against the 0.7.1 (or Leg-A 1.0.0) baseline. The
tightening is a *superset for well-typed sites* — precise types accept everything `any`
accepted — so the expected result is CLEAN; a new TS2589 (excessive-depth instantiation) or a
rejected `any()` call is a **real semver signal** escalating minor→major, decided by this
measurement, not assumed. Under H2, value.js-master's `all()`/`dispatch()` sites get the same
`vue-tsc`/`tsc` treatment; under H1 they are moot.

**The nested-version dual-instance hazard — DISSOLVED in the live graph, documented.** The
hazard (two parse-that instances in one tree from a nested dependency) required a repo
depending on two parse-that-bearing packages at different pins. In the measured graph: value.js
tranche-u = none, keyframes = none, latex-paper = 0.7.1 standalone. **No repo nests two
instances.** Under H2, value.js-master (1.0.0) and latex-paper (0.7.1) are separate installs
in separate repos — no shared `node_modules`, no dedupe needed. The hazard is therefore
**documented as not-present** (not deduped, because there is nothing to dedupe); it would only
re-appear if a future repo took both latex-paper and value.js as deps — absent today. Record
this as the disposition, with a watch-note for the gestalt.

**Zero-runtime-cost invariant (constitution v):** the tightening is compile-time only; the
emitted JS must be byte-identical. Prove it with a `vite build` `dist/` diff that is
semantically empty (the arity-unrolled arms at `leaf.ts:39-57,183-251` keep their runtime
shape; only the generic bounds sharpen). A leak that closes only via a runtime cast or an
altered `dist/` is a defect, not a fix.

---

## 6. SPEC-FIDELITY DIVERGENCES (feeding the re-cut)

Machine-checkable deltas between the family-spec text and the measured prototype/consumer graph:

1. **value.js liveness (HIGH).** Spec cites value.js as a live consumer with 9 imported symbols
   incl. `dispatch` and "59 `all()` sites" as the arity-specialization corpus. **Measured:**
   working tip (tranche-u, 4.0.0) is parse-that-FREE; only frozen master (3.1.0) retains the
   dep. The 59-`all()` corpus is stale (master) or dead (tranche-u). → the arity-N/`all()`
   justification must re-cite master-frozen OR first-party bench, disclosed as such; it cannot
   claim a live value.js corpus.
2. **keyframes.js gate (HIGH).** Spec treats keyframes as a transitive consumer gate.
   **Measured:** keyframes 6.0.0 has zero parse-that (direct or via value.js 4.0.0). → strike
   keyframes from the consumer-suite gate.
3. **"three consumer suites green" close gate (HIGH).** → re-cut to **latex-paper Leg-B green +
   Leg-A attribution**, plus value.js-master green **iff H2**, keyframes struck.
4. **latex-paper contract width (MED, confirmatory).** Spec says "only Parser/ParserState/
   regex/string/any." **Measured-confirmed** for imports; additionally the live *method*
   contract is exactly `{map skip trim then next opt chain not}` and the live *free* contract
   excludes `all/dispatch/whitespace/or/many/sepBy`. This is the precise blast-radius surface.
5. **latex-paper migration framing (MED).** Spec's "0.7→1.1 jump" is right; the fix is the
   two-leg attribution (§3), else a 1.0.0-owned break is mislabeled 1.1.0.

---

## 7. PRIOR ART / SOTA (citations for the mechanism)

- **Crater methodology.** Rust's `crater` (rust-lang/crater) is the canonical "install candidate
  across the real downstream corpus, diff green/red, attribute per-release" tool; its
  per-crate before/after diffing and regression triage is the pattern the §3 two-leg crater
  mirrors at library scale. The attribution-isolation discipline (bisect the breaking release)
  is crater's core lesson.
- **Semver for type-level APIs.** DefinitelyTyped's `dtslint`/`@definitelytyped/dts-critic` and
  the TypeScript team's `@ts-expect-error`-based type tests (also `tsd`, `expect-type`,
  `vitest`'s `expectTypeOf`) are the SOTA for asserting inference precision both ways —
  rejecting over-widening AND catching true mismatches — which is exactly the W3 type-test
  shape. "A more-precise type is source-compatible for well-typed consumers" is the DT
  minor-bump rule; a rejected previously-accepted call is the major signal.
- **Deep instantiation (TS2589).** The tightening's risk class is TypeScript's
  recursion/instantiation-depth limit; `ReadonlyArray<Parser<unknown>>` + `infer V` variadic
  tuple inference is the idiom (TS 4.0 variadic tuple types) that keeps depth bounded, versus
  naive recursive conditional types that trip TS2589.
- **V8 hidden-class discipline.** The "fields declared at construction, never transition" rule
  (monomorphic maps, `%HaveSameMap`) is standard V8 optimization guidance (V8 team's "hidden
  classes" / IC-state literature); the lazy-`undefined`-field-vs-array-at-same-map trick is the
  allocation-avoidance form of it. Node's `--allow-natives-syntax` + `%HaveSameMap` is the
  established probe.

---

## 8. NAMED RISKS & COUNTEREXAMPLES

- **R1 — designing to a dead corpus.** *Counterexample:* the family keeps citing value.js's 59
  `all()` sites to justify arity-N specialization, but tranche-u has shed them. *Cure:* cite the
  arity histogram from the ACTUAL live graph — latex-paper uses `any()` (n-ary, 3 sites) and NO
  `all()`; the arity justification, if it survives, rests on first-party bench + master-frozen,
  disclosed, not a live value.js claim.
- **R2 — the scope adds a per-inner-parse allocation.** *Counterexample:* the DiagnosticScope
  is allocated inside `parseStateInner` (per combinator-parse) rather than at the top-level
  epoch, regressing the diagnostics-off hot path. *Cure:* scope opens once per top-level
  `.parse()`, null on the off path; the off-path paired harness must hold the landed floor or
  the wave does not close.
- **R3 — reddening latex-paper's `vue-tsc` silently.** *Counterexample:* the tightened `any()`
  signature trips a TS2589 at latex-paper's 3 call sites, invisible to `vitest run`. *Cure:* the
  crater's type-check leg is `vue-tsc -p tsconfig.vue.json`, not unit tests; a new diagnostic
  escalates semver, decided by measurement.
- **R4 — mis-attributing a 1.0.0 break to 1.1.0.** *Counterexample:* latex-paper's `^0.7.1` pin
  means the 0.7→1.1 jump crosses the `*Span`/`chain()` cut; a break gets blamed on PT-3. *Cure:*
  the two-leg crater isolates Leg A (0.7→1.0) first.
- **R5 — treating tranche-u as settled.** *Counterexample:* the family assumes value.js has
  permanently shed parse-that and prunes the surface accordingly, then tranche-u never merges
  and master stays the `^1.0.0` consumer. *Cure:* carry H1/H2 to owner confirmation; the surface
  disposition (§2) is stable under BOTH because it keeps the algebra either way.
- **R6 — pruning the library to the app contract.** *Counterexample:* the surface is pruned to
  latex-paper's `regex/string/any` slice, deleting the published algebra. *Cure:* the §2
  adjudication — first-party suite is a legitimate consumer for a published library; keep the
  algebra, drop only the being-deleted diagnostics globals.

---

## 9. WHAT A PROTOTYPE MUST TEST TO MOVE THIS FAMILY

1. **The owner liveness confirmation (P2), as a gate not a probe:** is tranche-u the forward
   posture (value.js exits) or a WIP branch (master stays)? Until answered, the crater and the
   arity justification carry both arms. *Non-blocking but decision-shaping.*
2. **The two-leg latex-paper crater:** scratch install of a packed 1.0.0 then 1.1.0 candidate,
   `vitest run` + `vue-tsc -p tsconfig.vue.json` at each leg, diagnostic-set diff vs the 0.7.1
   baseline. GREEN both legs = the migration is real; any RED = the exact symbol + the owning
   release.
3. **The `any()` tightening reddening test:** the tightened `leaf.ts:28`/`154` signatures on a
   scratch tree, `vue-tsc` over latex-paper's 3 `any()` sites, assert zero new TS2589 / zero new
   rejection. This is the falsifiable semver classifier.
4. **The disabled-path scope oracle:** `%HaveSameMap` TRUE for diagnostics-off vs -populated
   `ParserState`, and the off-path paired harness holding the landed then-chain/fused floors —
   proving the per-parse scope costs zero when off.
5. **The empty `dist/` semantic diff:** `vite build` before/after the tightening, proving the
   zero-runtime-cost invariant (compile-time-only inference).
6. **The re-cut close gate itself:** latex-paper Leg-B GREEN (+ Leg-A recorded) [+ value.js-
   master GREEN iff H2], keyframes struck — the superseded "three suites" gate replaced by the
   measured-graph gate.
