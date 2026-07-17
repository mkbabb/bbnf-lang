# LOCK-8 pass-3 — research-D — family 8D CONSUMER-CONTRACT-FIRST

*The TS backbone designed backward from its live consumer contract. This pass RUNS the
gates the prior pass could only design: the latex-paper baseline is captured GREEN, the
census is re-verified against the checked-out trees, and the dual-instance boundary is
traced to the exact line where it is crossed. parse-that is READ-ONLY; all prototyping is
scratch-copy; the single sanctioned artefact into the repo is coordination letter
BBNF-PT-G. Figures marked RAN are executed on the trees; figures marked UN-RUN are handed
to the prototype list with the exact command that closes them.*

---

## 0. The census — RE-VERIFIED, and the surviving consumer is LIVE (RAN)

The flip recorded earlier holds exactly, re-checked against the current trees:

- **value.js — parse-that-FREE (confirmed).** `value.js@4.0.0`, branch `tranche-u`. No
  `dependencies` key at all (`python3`: `'dependencies' in pkg == False`, `'parse-that' in
  json.dumps(pkg) == False`). `src/parsing/{color,stylesheet,timeline}/` are empty stub
  dirs (64 B). CSS is hand-written in `src/v4/css/{grammar,stylesheet,types,named-colors}.ts`
  — zero combinator imports (`grep '@mkbabb/parse-that' src/` → **0 hits**).
- **keyframes.js — severed (confirmed).** `keyframes.js@6.0.0`. `grep '@mkbabb/parse-that'
  src/ package.json` → **0 hits**. Its only route was transitive through value.js; that
  route is cut.
- **latex-paper — the SOLE surviving consumer, and it is LIVE, not dormant.** `@0.2.1`,
  branch `master`, HEAD `c140473` — recent, active commits (the VIRT tranche, W2-SKELETON
  15f/136t). This answers PLANE-WIDE P3: latex-paper is a **forward-looking** consumer that
  will migrate onto a new cut, **not** a frozen tree. Its parse-that pin, however, is
  stale relative to its own activity: `package.json:38` `"@mkbabb/parse-that": "^0.7.1"`,
  resolving `node_modules/@mkbabb/parse-that@0.7.1` (predates the 1.0.0 breaking cut).

**Baseline captured (RAN).** `cd latex-paper && npx vitest run` → **127 passed (127),
14 files, 1.08s, exit 0**. This is the falsifiable before-state the crater needs: latex-paper
is GREEN at 0.7.1 *today*. Any red after a candidate bump is attributable, because the green
baseline is banked. (Note: 127 tests, grown past the R4-era count — the suite is active.)

**Live combinator surface — RE-RAN import oracle.** `grep -rn '@mkbabb/parse-that' src/`,
8 sites, symbol union across all of them = exactly **`{Parser, ParserState, regex, string,
any}`** (5 symbols): `grammar/math.ts:5` `{Parser,regex,string}`; `grammar/commands.ts:9`,
`environments.ts:6`, `text.ts:5`, `bibtex/parser.ts:7` `{Parser,regex,string,any}`;
`grammar/document.ts:5` `{Parser,regex,string,any,ParserState}`; `grammar/primitives.ts:5`
`{Parser,ParserState,regex,string}`; `grammar/compile.ts:7` `type {Parser}`. **No subpath
reached** (`/core`,`/diagnostics`,`/packrat`,`/utils` are all untouched). No consumer
imports `all`, `dispatch`, `whitespace`, `mergeErrorState`, `memoize`, or any diagnostic
symbol from outside parse-that.

---

## 1. CONSUMER-SET ADJUDICATION — the repo already decided it, mechanically (directive 8D#1)

The question: for combinators with no external consumer, is the first-party test/bench
suite a legitimate first-party consumer, or should the public surface prune to the live
external contract? **The disposition, in three tiers:**

**Tier A — the external floor (un-prunable under any circumstance).** `{Parser, ParserState,
regex, string, any}` — latex-paper's live 5-symbol surface. These may never be removed,
renamed, or type-narrowed in a way that breaks the 8 import sites. This is the hard contract.

**Tier B — the combinator algebra (KEEP; first-party corpus is a *legitimate* consumer).**
`then or map chain skip next opt many sepBy all dispatch trim wrap peek lookAhead not minus
recover` and the leaf set. **Verdict: KEEP, and the first-party suite is a lawful Y.13
consumer** — because parse-that is a *published combinator library*, not an internal helper
of three downstream repos. Its contract is the algebra as a standalone npm package; pruning
to latex-paper's 5 symbols would gut the product to a token-parser, contradicting the
constitution-i floor (primitives + combinators ARE the exempt base). **The decisive
evidence that this is the repo's own settled posture, not an assertion:** parse-that already
ships `proof:no-dead-combinator` (`package.json`, in the `proof:all` chain) — a mechanical
gate asserting *every* combinator is exercised by the first-party corpus. The repo has
already operationalized "the first-party suite is the consumer of the algebra." 8D
**ratifies** this: Y.13 for Tier B is discharged by the 13 `test/*.test.ts` + 4
`*.bench.ts` + `proof:no-dead-combinator`, disclosed as the first-party contract.

**Tier C — accreted machinery with neither external nor honest first-party consumer (PRUNE
/ collapse).** The **module-global diagnostics latch** (`utils.ts:6` `diagnosticsEnabled`,
buffer `utils.ts:138-146`) is consumer-less *and* defective (cross-parse contamination, §3);
it is deleted-not-flagged regardless (W2). This is the honest prune target — not the
algebra. `proof:no-dead-combinator` does **not** cover the diagnostics globals (they are not
combinators), which is exactly why they escaped the gate and accreted; 8D's contribution is
to name them Tier C and delete them.

**One-line rule for the gestalt:** *the public surface is justified by the first-party
conformance corpus (the library is the product); latex-paper's 5 symbols additionally pin an
un-prunable external floor; only non-combinator accreted globals prune.* No combinator is
dropped; `proof:no-dead-combinator` is the standing enforcement.

---

## 2. The migration crater — baseline leg RAN, candidate leg specified (directive 8D#2)

`scratch/pt-consumer-crater.sh` (harness only; never landed). The crater is a **1-consumer
crater + 2 negative guards** — the honest degenerate instance of rust-lang/crater.

- **Leg (a) latex-paper — POSITIVE gate. Baseline RAN GREEN (127/127).** Remaining candidate
  steps: scratch-copy latex-paper; pack the parse-that working tree
  `(cd scratch/parse-that/typescript && npm run build && npm pack)`; **the mandatory
  reinstall** `rm -rf node_modules/@mkbabb/parse-that && npm i ../parse-that-<cand>.tgz`
  (omitting this silently re-tests the pinned 0.7.1 — the false green); then **both axes**:
  `npx vitest run` (runtime) **and** `npx vue-tsc -p tsconfig.vue.json` (the type axis —
  §4). **Attribution isolation (directive: run the minor step ALONE):** run **0.7.1→1.0.0
  first, in isolation**, so a 1.0.0-era break (the `*Span` excision S.H2, `chain()` C-16) is
  never mislabeled a 1.1.0 regression; only then 1.0.0→1.1.0.
- **Leg (b) value.js — NEGATIVE guard.** `! grep -rq '@mkbabb/parse-that' src/` **and** the
  `dependencies`-absent assertion. A permanent tripwire against silent re-introduction.
- **Leg (c) keyframes.js — NEGATIVE guard.** Same shape; assert 0 hits in `src/`+`package.json`.

The baseline half is now **evidence, not plan**: latex-paper is provably GREEN at 0.7.1, so
the crater is interpretable. The candidate half is UN-RUN only because it requires a built
1.1.0 tarball (the W5 tree), which does not yet exist.

---

## 3. Per-parse diagnostic enablement — mechanism + disabled-path cost (directive 8D#3)

**The storage is already per-parse; only the *gate* is a module-global.** The failed
`ParserState` returned by `parseStateInner` (`parser.ts:56-65`) already carries the diagnostic
triple per-parse: `furthest` (constructor field, always populated), `expected`
(`state.ts:43`, gated), `getLineAndColumn(offset)` (instance method, `state.ts:127`). The
sole module-global residue is the populate-gate boolean `utils.ts:6` `let diagnosticsEnabled
= false`, read in `mergeErrorState` (`utils.ts:33`), `addSuggestion` (`utils.ts:51`),
`addSecondarySpan`.

**Candidate mechanisms, compared on the DISABLED-path cost (the directive's axis):**

| mechanism | scoped? | per-parse alloc when OFF | hidden-class risk | verdict |
|---|---|---|---|---|
| module-global boolean (current) | **NO** (cross-parse leak) | 0 | none | defective |
| `DiagnosticScope` object hung off the parse epoch | yes | **1 object / `.parse()`** even when off | new shape | **rejected** (R3 regression) |
| flag on `ParserContext` | no (wrong lifetime) | 0 | none | **rejected** — `ParserContext` is per-*combinator* (grammar-construction, `parser.ts:29`), not per-parse; would leak across parses exactly like the global |
| **boolean field `state.diag` on `ParserState`** | **yes** | **0** | **none** | **WINNER** |

**The winner and its zero-cost proof.** `ParserState` is *already* allocated per-parse.
Add a primitive boolean field `diag`, **initialized in the constructor** (`state.ts:36-53`)
so the hidden class is fixed from birth and can never transition. It is set once from a parse
option — `.parse(src, { diagnostics: true })` (or a scoped `withDiagnostics(fn)`) — never a
module-global write. `mergeErrorState(state, label)` reads `state.diag` in place of the
global. **Disabled-path cost = one boolean read on a field that is part of the state's fixed
monomorphic map — no allocation, no new object, no hidden-class transition.** A
`DiagnosticScope` object, by contrast, re-introduces the exact per-parse allocation the risk
list names; the "scope" *is* the state, so no new lifetime is needed. Deletion proof
(precept #6): `rg 'let diagnosticsEnabled' src/` → 0.

**The one real threading obligation (hand to the prototype).** `state.diag` must propagate to
**every** child/derived state that `mergeErrorState` can see. parse-that mutates a cursor
state and also mints new states via `state.from`/`unsafeSetValue` (`state.ts:55-99`); the
prototype must confirm `diag` copies across every mint site, or a child state silently reads
`false`. Oracle: `node --allow-natives-syntax` asserting `%HaveSameMap(diagOnState,
diagOffState) === true` (same map with/without diagnostics) AND a nested-parse isolation test
(outer off, inner on → outer collects zero) that is **RED on the current global** and GREEN
after — the born-RED gate.

---

## 4. Does the inference tightening redden latex-paper's `any()` sites? (directive 8D#4)

**The three sites, RAN out of the tree, classified:**

1. `text.ts:154` `return any(...alts)` where `alts: Parser<TextNode>[]` (from
   `names.map(() => regex(...).map(() => textNode(...)))`). Spread of a homogeneous array.
   Under the tightened `any<T extends ReadonlyArray<Parser<unknown>>>` with mapped-tuple
   `ExtractValue`, a `Parser<TextNode>[]` maps to element `TextNode`; return `Parser<TextNode>`
   is assignable to the function's `Parser<TextNode>` annotation. **SAFE.**
2. `text.ts:145` `export const accent: Parser<TextNode> = any(bracedAccent, unbracedAccent,
   namedAccent)`. If all three args are `Parser<TextNode>`, the inferred union is `TextNode`
   → assignable to the `Parser<TextNode>` annotation. **SAFE unless one arg is mistyped.**
3. `document.ts:81` `const inlineNode: Parser<LatexNode | null> = any(comment, paragraphBreak,
   displayMathDollar, …)` — a large **heterogeneous** union assigned to an explicit
   annotation. `LatexNode` is a **16-member union** (`types/ast.ts:103-140`: TextNode,
   MathNode, CommandNode, EnvironmentNode, …). **This is the sole real risk site.**

**The mechanism of the risk, precisely.** The *current* `any<T extends Array<Parser<any>>>`
has a hidden failure mode: when tuple inference does not fire cleanly, `T` falls back to the
constraint `Array<Parser<any>>`, so the result degrades to `Parser<any>` — assignable to
`Parser<LatexNode | null>` **unconditionally**, whatever the args actually are. The tightened
signature's fallback is `Parser<unknown>`, which is **not** assignable to a covariant
`Parser<LatexNode | null>`. So the tightening **unmasks** any latent mistype: if even one
inline parser's value type is genuinely outside `LatexNode | null`, `document.ts:81` reddens
under `vue-tsc`. If every inline parser is honestly typed within the 16-member union (plus
`null`), the tightening is **safe and strictly better** (subtype → supertype assignment holds).

**Verdict.** The tightening is **probably safe** for latex-paper — its `any()` args are
node-typed parsers and the annotations are supertypes — but the verdict is **UN-DECIDABLE by
unit tests** (the 127/127 baseline says nothing about it) and **UN-DECIDABLE statically here**
because it depends on the inferred union of ~20 heterogeneous inline parsers. It is decided
only by building the 1.1.0 candidate and running **`npx vue-tsc -p tsconfig.vue.json`** at the
migrated tree — exactly the "break caught only by full type-check, not unit tests" the
directive names. This is the crater's mandatory type axis, not an optional add-on.

*Reassuring boundary fact:* latex-paper's `nt.*` nonterminals are typed `Parser<any>`
(`compile.ts:15` `Record<string, Parser<any>>`); `Parser<any>` **stays assignable** to the
tightened `Parser<unknown>` params (any → anything), so the cross-instance composition of §5
does **not** newly redden under W3. The type risk is confined to the annotated-assignment
sites (1–3 above), not the `any()`-argument-passing sites.

---

## 5. The dual-instance hazard — TRACED to its crossing line; RESOLVED as a migration gate

The nested dual-instance hazard is **not hypothetical** — it is structurally crossed, and the
1.1.0 migration *widens* it. Traced:

- `compile.ts:6` imports `BBNFToParser` from `@mkbabb/bbnf-lang`; that package bundles its own
  **nested parse-that 0.6.0** (`node_modules/@mkbabb/bbnf-lang/node_modules/@mkbabb/parse-that
  @0.6.0`, confirmed on the tree).
- `compile.ts:18` `const [nt] = BBNFToParser(LATEX_TOKENS_BBNF)` — `nt` is a bag of `Parser`
  objects **constructed by parse-that 0.6.0**.
- `compile.ts:15` types them `Nonterminals = Record<string, Parser<any>>` where `Parser` is
  latex-paper's **own** import from parse-that **0.7.1** (`compile.ts:7`). The `any` masks the
  version mismatch at the type level.
- `compile.ts:22-40+` then calls `nt.emDash.map(...)` — 0.6.0's `.map`, on a 0.6.0 object,
  benign — but the resulting `nt.*` parsers **flow into latex-paper's grammar** where they are
  composed with **0.7.1** combinators (`any`, `.then`, `.skip`, etc., across `grammar/*.ts`).

**The runtime mechanism of failure.** A 0.7.1 combinator wrapping `nt.emDash` builds a 0.7.1
closure that calls `nt.emDash.parser(state)` with a **0.7.1 `ParserState`**, while
`nt.emDash.parser` is a **0.6.0 `ParserFunction`** expecting a **0.6.0 `ParserState`**. Today
this works by structural luck (compatible field layout at 0.6↔0.7). The 1.0.0 cut removed
fields (`*Span` excision) and 1.1.0 adds lazy fields (`suggestions`/`secondarySpans`) — so
**bumping latex-paper's direct dep to 1.1.0 while bbnf-lang stays 0.6.0 drives 1.1.0
combinators over 0.6.0-shaped parser states across the widest shape gap in the package's
history.** This is a **runtime** hazard, distinct from the §4 type hazard.

**Resolution (dedupe-or-document, decided not assumed):**
- The lazy fields are **additive/optional** (constructed `undefined`, never required by a
  reader); the `*Span` excision removed fields latex-paper's token grammar never touches. So
  the *most likely* outcome is **compatible-by-structure → DOCUMENT** as a benign nested
  version in a `MIGRATION.md` row.
- **But the falsifier exists and must be run:** the crater's runtime leg (`vitest run`, whose
  `document.test.ts` (19t) + `integration.test.ts` (8t) + `paper.test.ts` (19t) exercise the
  compiled bbnf tokens end-to-end) is exactly the test that goes RED if the 1.1.0-over-0.6.0
  state-shape boundary breaks. **If red → dedupe** via a latex-paper `package.json`
  `overrides: { "@mkbabb/parse-that": "1.1.0" }` (requires bbnf-lang to be 1.1.0-compatible)
  **or** a coordinated bbnf-lang bump; **if green → document** the benign nested pin.
- This makes the dual-instance decision **gated on the crater's runtime leg**, not on
  speculation — the honest disposition.

---

## 6. Integration seams (path:line)

- Census: value.js `package.json` (no `dependencies`), `src/v4/css/grammar.ts`;
  keyframes.js `package.json` (0 hits); latex-paper `package.json:38` `^0.7.1`, 8 import
  sites (§0), baseline `npx vitest run` → 127/127.
- The crossing: `latex-paper/src/grammar/compile.ts:6` (`BBNFToParser` ← bbnf-lang/0.6.0),
  `:7` (`Parser` ← parse-that/0.7.1), `:15` (`Record<string, Parser<any>>`), `:18`
  (`BBNFToParser(...)`); nested pin `node_modules/@mkbabb/bbnf-lang/node_modules/@mkbabb/
  parse-that@0.6.0`.
- Type-break sites: `document.ts:81` (heterogeneous union → `Parser<LatexNode|null>`, the risk
  site), `text.ts:145`, `text.ts:154`; `LatexNode` at `types/ast.ts:103-140`.
- Diagnostics: global `utils.ts:6`; reads `mergeErrorState` `utils.ts:33`, `addSuggestion`
  `utils.ts:51`; per-parse state fields `state.ts:36-53`, mint sites `state.ts:55-99`;
  `getLineAndColumn` `state.ts:127`; failed-state build `parser.ts:56-65`.
- parse-that gates: `any` signature `leaf.ts:28`; `ExtractValue` `parser.ts:9-11`;
  `Parser` covariant class `parser.ts:24-32`; `proof:no-dead-combinator` + `proof:all`
  (`package.json`).
- Packrat (consumer-less across the live set — no consumer imports `memoize`): arm latch
  `packrat.ts:156/290` — every 8D perf table states **UNARMED (no consumer arms it)**.

---

## 7. What a prototype MUST test to move this family

1. **The candidate crater (the one UN-RUN gate).** Build a 1.1.0 tarball; reinstall over
   latex-paper's 0.7.1 (mandatory `rm -rf … && npm i`); run **`vitest run` AND `vue-tsc -p
   tsconfig.vue.json`**. Run **0.7.1→1.0.0 alone first** (attribution). The 0.7.1 baseline is
   already banked GREEN (127/127) — the before-state exists.
2. **The type axis at `document.ts:81`** — the sole real §4 risk: does the tightened `any()`
   union stay assignable to `Parser<LatexNode|null>`, or does it unmask a latent mistype? Only
   `vue-tsc` decides.
3. **The dual-instance runtime leg** — does 1.1.0-over-0.6.0 break `document/integration/
   paper.test.ts`? Green → document; red → dedupe via `overrides`.
4. **The `state.diag` diagnostics prototype** — replace `diagnosticsEnabled` with a
   constructor-set boolean field; prove `%HaveSameMap(on,off) === true` and the nested-parse
   isolation oracle (RED on the global, GREEN after); confirm `diag` propagates across every
   `state.from`/`unsafeSetValue` mint site.
5. **The negative-guard tripwires** — mechanical `grep` gates asserting value.js and
   keyframes.js stay parse-that-free (permanent, against silent re-expansion).

---

## 8. Named risks / counterexamples

- **R1 — the migration widens the version gap at the worst moment.** Bumping latex-paper to
  1.1.0 puts 1.1.0 (direct) beside 0.6.0 (nested, via bbnf-lang) — the widest ParserState-shape
  gap the package has spanned. *Counterexample to "just bump it":* the compiled-token tests
  drive 0.6.0 parsers through 1.1.0 combinators. Cure: §5's gated dedupe-or-document.
- **R2 — the type break hides behind `any` and unit-green.** latex-paper is 127/127 GREEN and
  every `any()` arg is `Parser<any>`, so *both* unit tests and argument-passing type-check
  silently; the break, if any, lives only at `document.ts:81`'s annotated assignment and only
  `vue-tsc` finds it. *Counterexample:* a migration that runs `vitest` and declares success
  ships a `vue-tsc`-red tree. Cure: the type axis is mandatory in the crater.
- **R3 — `DiagnosticScope` re-adds the disabled-path allocation.** *Counterexample:* a scope
  object per `.parse()` regresses the diagnostics-off hot path below floor. Cure: the boolean
  field on the already-per-parse state (§3); the W2 gate re-runs the disabled-path harness.
- **R4 — the design target was a phantom.** Any 8D table citing "value.js's 59 `all()` sites"
  measures a repo that imports zero parse-that. The contract is latex-paper's 5 symbols +
  the first-party corpus (Tier B), nothing more.
- **R5 — pruning the algebra to satisfy Y.13 would gut the library.** *Counterexample:*
  removing `all`/`dispatch`/packrat because no *external* consumer imports them destroys a
  published combinator library. Cure: Tier B — the first-party corpus + `proof:no-dead-
  combinator` is the lawful consumer; only Tier C (non-combinator globals) prunes.
