# LOCK-8 — THE TS BACKBONE PLANE — pass-2 FAMILY SPECIFICATIONS

*Synthesis, pass 2 (Fable-tier judgment). One cogent spec per family while routes remain
incompatible, then the incompatibility map and the BLOCKED-candidate adjudication. Source:
pass-2 research-{A,B,C,D,priorart}, the pass-1 REGISTRY, PROTO-1/PROTO-2, CRIT-1/CRIT-2.
Binding facts unchanged: parse-that/typescript is READ-ONLY to the bbnf campaign — every family
lands only via letter BBNF-PT-G; prototypes run on scratch copies (PROVE-11 recipe). Two engines
govern and must never be conflated: the V8 JIT (8A) and the TS checker (8B). Withheld/settled
facts from the pass-1 registry (−14% runtime-switch KILL; constant-capture inlines; `.parser`
LOAD monomorphic; packrat latch; don't-split-`parser.ts`) are NOT re-litigated.*

---

## 0. THE LOAD-BEARING SUPERSESSION — THE CONSUMER DIVORCE (CONFIRMED this pass)

Three of the four pass-2 researchers (A, B, D) independently reported that parse-that's anchor
consumer has divorced it. **The synthesis verified the census mechanically** (not SPEC_ONLY):

| repo | version / ref | actual `import … from "@mkbabb/parse-that"` (src) | `all()` sites | disposition |
|---|---|--:|--:|---|
| **value.js** | 4.0.0, HEAD `86a9a17` (branch `tranche-u`, 139 commits **past** tag `tranche-t-close`) | **0** | **0** | MIGRATED OFF; `package.json` has **no `dependencies` key**; CSS re-homed hand-written at `src/v4/css/grammar.ts` over its own `Result`/`ok`/`err` |
| **keyframes.js** | 6.0.0 | **0** | — | transitively severed; `dependencies = {@mkbabb/value.js: 4.0.0}` only |
| **latex-paper** | 0.2.1, pin `^0.7.1`, installed **0.7.1** | **8** (5 symbols: `Parser, ParserState, regex, string, any`) | **0** | **SOLE surviving live consumer**, frozen two minor-cuts behind, uses `any()` at 3 sites / 2 files, `.opt()` at 11 sites (none inside `all`) |

**A reconciliation the synthesis is obliged to record.** research-A's premise ("corpus of
record: value.js at HEAD `86a9a17`" carrying **61 `all()` / 93 `any()`** sites) is **refuted by
measurement**: at HEAD `86a9a17` there are **0** `all()` and **0** `any()` combinator sites and
**0** parse-that imports (the 12 grep hits research-A/B partially saw are prose comments —
`"parse-that-FREE"`). `86a9a17` is 139 commits *past* the `tranche-t-close` tag research-A
conflated it with; the tag itself also has 0 `all()`. Every research-A figure attributed to
"value.js's real `all()` arguments" (`color.ts:147,151,…`), and its "corpus evidence the
predicate has bite," was **measured on a tree that does not exist at HEAD** and is **struck** —
exactly as research-B struck its own value.js figures. The *mechanisms* research-A designed (the
SFI predicate, the arity-N specializer) survive; their *justifying corpus* does not.

**Consequences the whole lock now carries (constitution viii/ix, Y.13):**
1. Every pass-1 8B PASS metric (1.042× instantiations, byte-identical error set, "259-file
   graph") was taken on value.js and is **unproduceable** — the 259-file figure counted a stale
   build/`node_modules` graph; the real tree is 24 TS files with zero parse-that.
2. 8A's arity-N fusion specializer (center-B) has **lost its principal justification** — the
   value.js `all()`/`any()` mass it was scoped to is gone.
3. 8D's "three-repo bundled-crater" collapses to **1 positive gate (latex-paper) + 2 negative
   guards** (value.js/keyframes must *stay* parse-that-free).
4. **Most of parse-that's public surface is now consumer-less** (`all`, `dispatch`, `whitespace`,
   `mergeErrorState`, packrat, the rich diagnostics vocabulary) — a plane-wide Y.13 reckoning
   that 8D now owns and 8A's specializer is coupled to.
5. **OWNER CONFIRMATION REQUIRED** that value.js 4.0.0's divorce is *intended* (tree of record)
   vs a mid-migration WIP branch. The families develop against the evidence as it stands; no
   number is banked on value.js.

The sole surviving real consumer for BOTH the runtime and type planes is **latex-paper native**
(+ parse-that's own `parsers/json.ts`/`csv.ts` self-use + the `.test-d.ts` suite). This is the
pivot on which pass-3's sharpest briefs sit.

---

## FAMILY 8A — THE SHAPE-STABLE CORE (the V8 runtime plane)

**Thesis (unchanged).** Shape stability and monomorphism are structural invariants provable with
V8 natives (`%HaveSameMap` / `%GetOptimizationStatus` / `--trace-turbo-inlining` /
`--trace-maglev-inlining`), not adjectives. PROVE-1 (pass-1) proved constant-capture inlines and
the `.parser` LOAD is monomorphic. Pass-2 resolves the two "new source change" defects and
answers the redirect's SFI question — but demotes center-B's specializer to a Y.13 candidate.

**What converged this pass (banked):**
- **`mapState` has ZERO consumers (research-A §0, grep-CONFIRMED across all three trees + the
  parse-that core).** `git grep mapState` over value.js/latex/keyframes `src` → 0; the only core
  hits are the definition + registry; **no combinator routes through it**. This DISSOLVES the two
  pass-1 defects (8A-G1 masked-field-drop, 8A-G2 unsound cast) at the root: the field set cannot
  break a consumer that does not exist.
- **The clean typed boundary (GRAFT 2 realized, no `as unknown as`).** research-A supplies a
  shared read-surface interface `ReadonlyState<T> {readonly src, value:T, offset, isError,
  furthest}`; `ParserState<T>` already declares all five publicly, so
  `class ParserState<T> implements ReadonlyState<T>` type-checks with **no runtime change and no
  cast**. `mapState`'s view becomes a `ReadonlyState<T>` fixed-shape literal (five fields, decl
  order) — one stable hidden class, no `Object.create`, no `as unknown as ParserState<T>`. The
  pass-1 falsely-declared 8A⟂8B "non-conflict" is now a **genuine** non-conflict by mechanism.
- **The SFI-heterogeneity predicate is ANSWERED (the redirect's open question, research-A §2).**
  A site is SFI-**homogeneous** (already inline in the general loop; unrolling wins nothing, emits
  dead bytes) iff all argument parsers share one constructing factory; **heterogeneous** (the
  general-loop call IC goes poly→megamorphic; positional unroll gives each position a monomorphic
  IC) iff factories mix. Build-time rule: `unroll(site) ⟺ d ≥ 2 ∧ arity ≤ CEILING`, where
  `d = |distinct SFIs|` computed **either** at construction (`new Set(parsers.map(p =>
  p.context.name)).size`, `context.name` a *faithful* SFI proxy → **no false negatives**) **or**
  build-time from the AST. False positives cost only wasted bytes, never a perf regression.
- **Structural vs temporal legs must report SEPARATELY (research-A §4).** Structural (the lazy
  diagnostic-array lever: alloc `1.84→0.74 B/op`, −60.1%, exact under `--expose-gc`) reproduces
  deterministically; temporal (ns) is load-noisy — the **dispatch lane HALVED on re-measure**
  (+10.2% claimed → ~+4.9% reproduced). Every 8A claim carries BOTH; the temporal leg is the
  paired per-round median over N≥12 rounds in ≥2 load windows. This closes pass-1's 8A-G5.
- **Packrat homed (research-A §5).** Memoized parser type = `Parser<T>` (shape-identical; memo
  logic inside the `.parser` closure). Key = float64-safe `id·2³² + offset`, module-global
  `Map<number, MemoCell>`, fail-loud `RangeError`. The one shape hazard is `MemoCell.ans:
  Answer|LR` polymorphism — **cold-path-quarantined behind arming** (opt-in latch, first
  `makeMemoized`, never disarms; default LL(1) pays nothing; every perf table discloses
  armed/unarmed; no throughput gate on arming).

**Exact open gaps (pass-2):**
- **8A-G-CORPUS (NEW, the load-bearing gap — center-B is now consumer-less).** The arity-N
  specializer was scoped to value.js's `all()`/`any()` mass, which is **gone** (§0). Its surviving
  justification is latex-paper (`any()` ×3, `all` ×0) + parse-that self-use (`json.ts`/`csv.ts`) +
  `.test-d.ts`. Building an arity-N fuser for a divorced corpus is consumer-less substrate
  (Y.13/viii). **Center-B's fate is COUPLED to 8D's Y.13 reckoning**: if 8D declares parse-that's
  own suite the first-party consumer, the specializer is justified against `json.ts`/`csv.ts`; if
  not, center-B **BANKS**. This must be decided by a census of surviving heterogeneous arity≥4
  sites, not asserted.
- **8A-G-MAGLEV (carried).** PROTO-1 forced synchronous TurboFan (`--no-maglev`); Node 26's
  default production tier is Maglev, whose inlining is more conservative. The specializer must
  verify inlining under `--trace-turbo-inlining` AND `--trace-maglev-inlining` at each arity —
  un-run.
- **8A-G-CEILING (carried, now thin).** `CEILING` reads from the *surviving* corpus's actual
  arity histogram (not value.js's, which is void). With the corpus this thin the emitted-size
  budget is easy — which itself is evidence center-B may not be worth building.
- **8A-G-mapState-Y.13 (minor).** A consumer-less `mapState` is itself a parsimony question
  (survives only for `proof:no-dead-combinator`). Not a correctness gap; a pruning candidate for
  8D's reckoning.

**Integration seams.** `parser.ts:162-187` (`mapState`; `Object.create` at `:177` is the fix);
`state.ts:36-53` (`ParserState` → `implements ReadonlyState<T>`); `state.ts:44-45` (lazy
diag-array lever); `leaf.ts:28-79` (`any`, arity-2 only), `leaf.ts:154-273` (`all`/`fuseAll`);
`leaf.ts:62`,`:261` (the general-loop call); `state.ts:141-189` (`context.name`/`parserNames` =
the runtime SFI proxy); `packrat.ts:79-99/103-127/156/280-487`.

**Gates it carries (born-RED).** (1) `%HaveSameMap(view_i, view_j)` TRUE across `mapState` after
the `ReadonlyState` fix; `Object.create` view ≠ real state on 1.0.0 (falsifiable). (2)
Structural leg (alloc B/op, ins/B) reported *separately* from temporal (paired ns, N≥12, ≥2
windows). (3) `--trace-deopt` zero steady-path deopts on the **surviving real corpus** (latex +
self-use), not synthetic triples. (4) Specializer inlines under **both** TurboFan and Maglev, or
is refuted. (5) Dispatch lane ~+5% no-regression, never a ≥10% gate. (6) **Center-B ships only
if a Y.13 consumer survives the §0 census** (coupled to 8D).

**Disposition: ADVANCE (mapState/StateView front) / center-B → Y.13-CANDIDATE.** The two pass-1
defects are CLOSED cleanly; the SFI question is answered; packrat is homed. But the family's
terminal deliverable (the arity-N specializer) has lost its corpus and is a consumer-less-substrate
candidate pending 8D's reckoning — not BLOCKED (the mechanism and every primitive exist; what is
missing is a *consumer*, not a primitive).

---

## FAMILY 8B — TYPE-LEVEL TOTALITY (the type plane)

**Thesis (unchanged).** The TS type system carries the grammar — full `ExtractValue` inference, a
typed combinator algebra with no `Parser<any>`, diagnostics typed by position — while emitted JS
stays byte-identical (clause-v proven by an empty `dist/` semantic diff). parse-that's OWN
inference story, distinct from LOCK-1's Rust HM/CSP solver.

**What pass-2 established (and struck):**
- **STRIKE: every value.js figure (§0).** The pass-1 PASS (1.042×, byte-identical errors,
  259-file graph) is unproduceable — value.js imports zero parse-that. Re-label the pass-1 PASS
  "literal-precision subset, measured on a now-divorced consumer." This is a **regression in
  banked evidence**, honestly recorded.
- **The budget corpus RE-HOMES to latex-paper native** (0.7.1; 8 grammar files; **183** real
  combinator call sites: `.then/.or/.opt/.many/.sepBy/.map/.chain/.skip/.next/.trim/.wrap`) +
  parse-that's own `parsers/json.ts`/`csv.ts` (the only surviving `all()`/`dispatch` self-use) +
  the `.test-d.ts` suite.
- **The dominant real TS2589 surface is NOT `all` — it is `.then()`-chain nesting** (up to 12
  `.then(` in `commands.ts`) + latex's mutually-recursive grammar. Bounded **structurally**: every
  rule is annotated `const x: Parser<Node>` or built via `lazy(() => …): Parser<T>`, so the
  checker terminates recursion at the nominal `Parser<Node>` seam — a 12-deep chain is 12 nested
  tuples, far under the ~1000 tail-recursive / ~50 non-tail caps (TS 4.5, #49459).
- **`opt`-in-`all` (8B-G2) is DISCHARGED on the consumer axis (research-B §2).** First-confirm
  result: opt-in-`all` typed sites **do not exist** in any surviving consumer (latex's 11 `.opt()`
  are all `.then/.skip/.next(X.opt())`, none inside `all`; latex has no `all`). The runtime
  differential has **no consumer input**. Resolution is therefore **spec-hygiene, not
  consumer-forced**: ADOPT `Parser<DropUndefined<Values<Ps>>>` + a documented widening (sound to
  ship precisely because no consumer exercises the length-variable case); REJECT redefine-`all`
  (breaking, no beneficiary) and REJECT `allOptional` (Y.13 substrate). **A parse-that-side
  self-test differential MUST still run** to prove the typing is honest.

**Exact open gaps (pass-2):**
- **8B-G1' (the budget number RELOCATES, still un-run on the real corpus).** `DropUndefined`
  wired into `all` has no live consumer call site; the budget must be re-measured on the
  `.test-d.ts` wide cases + `json.ts`/`csv.ts` self-use, and the *dominant* real number taken on
  **latex's `.then`-depth + recursive grammar** under `tsc --extendedDiagnostics --generateTrace`.
  **Disclosure rule (binding):** every TS2589/instantiation/check-time figure states the exact
  signature, tsc version, and corpus; any figure attributed to "value.js `all()` sites" is struck.
- **8B-G3 (the headline is STILL untouched).** "No `Parser<any>` anywhere" got zero of PT-E2's
  enumerated INPUT leaks removed; the full algebra (`then→[T,S]` dropping the unsound `S|T`, `or→
  T|S`, `chain→S`, `opt→T|undefined`) is **unprototyped**. research-B §3 supplies the precise
  per-combinator signature table + O(1)/O(N) instantiation costs, and names the two erasures that
  lawfully STAY (`ParserFunction<T>` = the invariant IC pointer; `MemoCell.value: unknown` =
  heterogeneous memo table). Home the type utils in **`src/parse/types.ts`** (kills the duplicate
  `ExtractValue` at `parser.ts:9-11` + `leaf.ts:155-157`). This is the family's load-bearing
  un-run work.
- **8B-G4' (emit-identity, un-run, now a TWO-step isolation).** latex is on 0.7.1, predating the
  1.0.0 cut, so a tightened-candidate jump mixes an API-shape delta with the type-only tightening.
  Step (i): latex `dist/` 0.7.1 vs 1.1.0-baseline (may be non-empty — the migration surface,
  8D-owned). Step (ii): 1.1.0-baseline vs 1.1.0-**tightened** MUST be **byte-empty** (a non-empty
  step-ii is a clause-v defect that halts the family).
- **8B-G6 (result-union settled toward secondary).** research-B §5 + research-D §7 CONVERGE:
  keyframes' compile-time-label downstream **evaporated** (it reached parse-that through the now-
  divorced value.js), removing the last hypothetical demand for `Parser<T,E>` — it stays BANKED as
  a named negative result. Field-triple = runtime carrier; result-union = the typed *wrapper* of
  the failure arm, consumer = latex runtime error-msg + `.test-d.ts` (Y.13-thin, additive).

**Gates it carries (born-RED).** (1) Instantiation budget ≤ named ceiling + check-time ≤ named ms
on **latex native** under the *exact tightened signature*. (2) `dist/` step-ii diff BYTE-EMPTY.
(3) `.test-d.ts` RED on 0.7.1/1.0.0, GREEN after, `@ts-expect-error` on genuinely wrong shapes
still errors (precision both ways). (4) No compiling consumer becomes uncompilable (latex
`vue-tsc` GREEN — shared with 8D-G4's type-break axis). (5) opt-in-`all` self-test differential
proves the drop-undefined runtime matches the documented widening.

**Disposition: ADVANCE, evidence re-homed.** All primitives ship (variadic tuples, `const`
params, tail-recursion elimination). The pass-1 PASS is struck to a subset measured on a divorced
consumer; the crux (`opt`-in-`all`) is discharged on the consumer axis; the full algebra + the ONE
surviving real budget number remain un-run and are the sharpest brief. No missing primitive — a
re-measurement + the untouched algebra, both runnable now.

---

## FAMILY 8C — THE ISOMORPHIC MIRROR (the correspondence plane)

**Thesis (RE-SCOPED as directed).** 8C does **not** own the general mirror machinery. The linter
(→ **LOCK-6A**), the single-spec→two-artifact generator (→ **LOCK-6B**), and the behavioral
differential oracle (→ **LOCK-6D**) are all LOCK-6's. 8C's sole deliverable is the genuine
parse-that residue: the **DIVERGENCE LEDGER** — one machine-checkable row per combinator/leaf/
subsystem, classified `MIRRORED` / `LAWFUL-NON-MIRROR(reason)` / `DRIFT(defect)`, each non-mirror
carrying a behavioral-parity assertion 6D's harness runs and 6A's linter whitelists. **8C's
deliverable is a PARSE-THAT face, not a bbnf-bench face** (research-C §0): the divergences live
entirely in parse-that's own rust↔ts split; the offset-tape crossing is a bbnf-bench/LOCK-6 face,
adjudicated OUT.

**What converged this pass (the double-home RESOLVED by cession — pass-1's 8C-G1):**
- **The mirror is DENSE (research-C §1).** A near-1:1 method census (`then/skip/next/or/chain/map/
  opt/many/sepBy` Rust↔TS, modulo the `not`↔`minus` name drift and the `Option`↔`undefined`
  totality skew) makes the divergences legible as *exceptions*, not chaos. These are the
  `MIRRORED` (spec-emittable) rows.
- **FOUR structural divergences classified (not three):**
  1. **SpanParser** — Rust-only (~1264 LOC, enum-dispatched zero-boxing); TS excised in the 1.0.0
     cut with a standing `proof:no-span-surface` gate forbidding reintroduction. `LAWFUL-NON-
     MIRROR(rust-zero-boxing; V8 has no boxed-combinator cost; a TS twin is pure dead weight —
     more shapes through the one megamorphic IC)`. Parity floor: a span-triple corpus asserting
     `(matched, start, end)` == TS `(matched, offset-delta)` (incl. trim-boundary + zero-width,
     R-C2).
  2. **regex** — Rust bespoke NFA→DFA vs TS native sticky `RegExp`; no NFA/DFA under
     `typescript/src` at all. `LAWFUL-NON-MIRROR(two-engines; HIR-isomorphic per LOCK-5)`. Parity
     floor: a differential corpus **including ordered-alternation + sticky-anchor cases** (R-C1 —
     the single highest-value assertion, below).
  3. **diagnostics** — Rust compile-time `#[cfg(feature)]` erase vs TS per-parse runtime scope
     (PT-E1). `LAWFUL-NON-MIRROR(compile-feature vs per-parse-scope) + MIRRORED(behavioral
     field-triple)`. Parity floor: failing-input corpus asserting identical `{furthest_offset,
     sorted expected, line, column}`; `found`/`suggestions` allowed to widen (Rust-richer).
  4. **packrat (the hidden 4th, grep-confirmed TS-only — 0 Rust hits).** `LAWFUL-NON-MIRROR(TS-
     only; Rust uses borrow-scoped recursion)`. The MemoCell shape-stability question is *intra-TS*
     (8A/LOCK-4), not a correspondence question — there is no Rust twin to stay stable against.
- **The consumer divorce SIMPLIFIES R-C3.** The "diagnostics `found`/`suggestions` widening is
  safe" claim previously risked a keyframes reader; keyframes is now parse-that-free, so no
  consumer reads a Rust-richer field. The widening is unconditionally safe.

**Exact open gaps (pass-2):**
- **8C-G-LEDGER (the deliverable is un-built).** A single machine-readable ledger file
  (AST/grep census over `parse-that/rust/parse_that/src/` + `typescript/src/parse/`), authored
  scratch-side, landed only via BBNF-PT-G. This is the residue; it is not yet written.
- **8C-G-REGEX (the sharpest un-run correspondence uncertainty — R-C1).** Rust-DFA leftmost-
  **longest** vs JS-RegExp leftmost-**first** (backtracking) provably disagree on ordered
  alternation (`/a|ab/` on `"ab"` → DFA `"ab"`, RegExp `"a"`). This is a *known* hazard
  (regex-automata vs Irregexp), not speculative. **If any grammar regex leaf uses ordered
  alternation with a shorter-first alternative, the regex row is DRIFT, not a lawful mirror** —
  and 8C's central regex classification is currently unfalsified. This is Brief 2.
- **8C-G-REGEN (ceded to 6B, un-run).** regen∘regen byte-identity is un-provable until 6B stands
  up a generator; 8C's contribution is only the classification bounding what the spec may contain
  (spec-emittable = the `MIRRORED` rows; must-not-symmetric-emit = the four divergences).
- **8C-G-TAPE (quarantine standing).** PROVE-14's stop-early **4.78×** (updated from 4.4×; CI
  [4.20, 5.37]; full-walk 0.98× parity) is a **pure-JS decode** number; the wasm→JS export cost is
  UNMEASURED. It must NEVER migrate onto a wasm-boundary claim (SPEC §0 shim-perf law). The
  tape-crossing artifact physically lives at `crates/bbnf-bench/ts-tape/` — a LOCK-6/6C face;
  8C neither homes it nor claims the number. Adjudicate the single path with LOCK-6.

**Gates it carries (born-RED).** (1) The ledger enumerates every combinator/leaf/subsystem with a
classification + parity assertion; zero unclassified rows. (2) Span-triple parity corpus GREEN
(incl. trim-boundary/zero-width). (3) **Regex differential GREEN incl. ordered-alternation +
anchoring, OR the diverging leaves are enumerated and the row re-classified DRIFT.** (4)
Diagnostics field-triple parity GREEN Rust-on vs TS-on. (5) The 4.78× stays labelled pure-JS-
decode until the export cost is separately measured.

**Disposition: ADVANCE (materially, via re-scope).** The pass-1 headline gap (double-home across
three strata + linter) is RESOLVED by cession to LOCK-6; the ambitious runtime-dispatch arm stays
BANKED (−14% refuted). What remains is genuine parse-that residue: the ledger (un-built) and its
parity corpora (un-run), with the regex alternation-order probe the one uncertainty whose answer
could flip a `MIRRORED` row to `DRIFT`. No missing primitive — a census + differential corpora.

---

## FAMILY 8D — CONSUMER-CONTRACT-FIRST (the API plane)

**Thesis (unchanged, sharpened).** The API, diagnostics shape, semver contract, and migration
artifact are derivable from what consumers actually import and read. The consumer census FLIPPED
(§0): the contract is now defined by latex-paper's **5-symbol combinator-layer surface** — the
sharpest possible contract — and two negative guards.

**What converged this pass (the strongest family this pass — nearly every pass-1 gap resolved):**
- **Build-time resolution PINNED (pass-1's 8D-G1, the blocked harness — research-D §1).** The
  "value.js bundles parse-that; stale bundle = false green" hazard is **moot**: value.js imports
  zero parse-that, so grepping its `dist/` yields a *true-green* absence. The genuine false-green
  is singular — latex-paper's pinned **0.7.1 tarball** in `node_modules`. Mandatory step =
  `rm -rf node_modules/@mkbabb/parse-that && npm i ../parse-that-<candidate>.tgz`, a latex reinstall,
  NOT a value.js dist rebuild. **The crater is constructable now.**
- **The import oracle RAN (pass-1's 8D-G2, prose census → mechanical — research-D §3).** latex =
  8 hits / 5 symbols / **none below the combinator layer** (zero reach `./core`/`./diagnostics`/
  `./packrat`/`./utils`); value.js/keyframes = 0. The R4 nine-symbol census is **STALE** (it was
  value.js's surface). `all`/`dispatch`/`whitespace`/`mergeErrorState` now have **zero external
  consumers**.
- **PT-E1 DESIGNED (pass-1's 8D-G3, contract-without-mechanism — research-D §4).** The field
  triple already lives per-parse on the returned failed state (`parseStateInner`, `parser.ts:56-65`):
  `furthest` (always) + `getLineAndColumn` (instance method) are always-on; only
  `expected`/`suggestions` *population* is gated. **Mechanism:** replace the module-global
  `diagnosticsEnabled` (`utils.ts:6`) with a **primitive boolean field `state.diag`** on the
  already-per-parse `ParserState`, set once from a parse option and threaded to children. Cost when
  off = **one boolean read** in the state's monomorphic hidden class — no allocation, no new object,
  no hidden-class transition. This BEATS a scope object (which re-introduces per-parse alloc on the
  diagnostics-off path). Deletion proof for the global: `rg 'let diagnosticsEnabled'` → 0.
- **latex migration matrix ATTRIBUTED (pass-1's 8D-G4 — research-D §5).** 0.7→1.0 touches **zero**
  of latex's 5 symbols (no `*Span`, no `chain`) — expected GREEN but must be RUN alone first
  (attribution isolation). 1.0→1.1: the diagnostics surface latex reads none of (GREEN at runtime);
  the **ONE real 1.1.0 risk is the inference tightening reddening `vue-tsc` at latex's `any()`
  sites** — a *type* break, caught only by `tsup && vue-tsc`, never by `vitest`. Dual-instance
  hazard (nested parse-that **0.6.0** via `bbnf-lang`) owns a MIGRATION.md row.
- **buildDiagnostic DECIDED on the consumer axis (pass-1's 8D-G5 / the 8B⟂8D collision —
  research-D §7).** latex reads **neither** `.expected` **nor** any result-union; field-triple-on-
  returned-state is sufficient; the result-union has **zero 8D pull** (it is 8B's type-ergonomics
  call, gated by no consumer). Field-triple wins on the consumer axis.

**Exact open gaps (pass-2):**
- **8D-G-Y13 (NEW, the central family decision — research-D §9).** With value.js + keyframes gone,
  `dispatch`, `all`, packrat, and the rich diagnostics vocabulary have **no external consumer**
  (constitution viii forbids consumer-less substrate). `Parser`/`regex`/`string`/`any` are consumed
  and stand as primitives. 8D must decide, explicitly: **(a)** declare parse-that's own test/bench
  corpus the first-party consumer (Y.13 satisfied by an internal disclosed suite), OR **(b)** prune
  the surface to the 5-symbol live contract + primitives. **This adjudication also decides 8A
  center-B's fate** (the specializer's only surviving consumer would be that internal suite).
- **8D-G-CRATER (un-run, but constructable).** The 1-positive + 2-negative crater is specified,
  never run. R4 caveat: latex is at 0.2.1 and may be dormant — it MUST be baselined **GREEN at
  0.7.1 first** (a born-verified before-state) or the only positive gate is uninterpretable.
- **8D-G-DUAL (a MIGRATION.md row).** Bumping latex's direct pin leaves the nested 0.6.0 (via
  bbnf-lang) in place — two parse-that instances. If a `Parser` value or `instanceof` crosses the
  latex ↔ bbnf-lang boundary, dedupe (value.js's own dedupe block is the precedent) or document
  that it never crosses.

**Integration seams.** `utils.ts:6` (global → `state.diag`), `mergeErrorState` `utils.ts:29-49`,
`parseStateInner` `parser.ts:56-65`, `state.ts:39-46/127`; latex `package.json:37-38`, 8 import
sites, `package-lock.json:582-589` (nested 0.6.0); value.js/keyframes parse-that-free markers.

**Gates it carries (born-RED).** (1) Crater: latex `npm test` + `vue-tsc --noEmit` GREEN against a
reinstalled candidate tarball, OR the exact symbol/type break enumerated; baselined GREEN at 0.7.1
first. (2) 0.7→1.0 run ALONE before 1.0→1.1 (attribution isolation). (3) The `vue-tsc` type-break
probe at latex's `any()` sites (the only real 1.1.0 risk — shared with 8B-G4). (4) Boolean-field
diagnostics: `%HaveSameMap(diagOn, diagOff) === true`, zero alloc / zero hidden-class transition
when off. (5) Negative-guard tripwires: value.js + keyframes stay parse-that-free (mechanical grep).
(6) The Y.13 disposition (prune vs internal-corpus) is stated explicitly, not implied.

**Disposition: ADVANCE (materially).** Nearly every pass-1 open gap is resolved or given a concrete
mechanism (crater constructable, oracle ran, PT-E1 designed, matrix attributed, buildDiagnostic
decided). The remaining work is a runnable crater + one explicit Y.13 adjudication — pure npm/tsc/
vitest construction, no missing primitive.

---

## THE INCOMPATIBILITY MAP (pass-2 — materially cleaner than pass-1)

The four families remain orthogonal planes over one closure core. The consumer divorce **collapsed
two pass-1 collisions** and settled the sharp one; it opened one constitution-level tension.

| pair | pass-1 state | pass-2 disposition | precise reason |
|---|---|---|---|
| **8A ⟂ 8D** (StateView drops the field triple) | ADDED as a missed collision (registry) | **EVAPORATED** | `mapState` has **zero consumers** (grep-CONFIRMED); the 8D field triple is read on the RETURNED failed state (`parseStateInner`), never through `mapState`. There is nothing to collide. The `ReadonlyState<T>` view drops no field a consumer reads, because no consumer reads it. |
| **8A ⟂ 8B** (the `as unknown as` unsound cast) | pass-1 agglom REFUTED the declared "non-conflict" | **RE-RESOLVED to a genuine non-conflict** | research-A replaces `oldView as unknown as ParserState<T>` with a structural `ReadonlyState<T>` interface `ParserState implements`; `mapState`'s callback takes `oldState: ReadonlyState<T>`. No cast. GRAFT 2 realized by mechanism, not assertion — 8B's soundness discipline satisfied. |
| **8B ⟂ 8D** (the sharp one: result-union vs field-triple) | additive-but-UNADJUDICATED | **ADJUDICATED: field-triple is the contract** | The sole surviving consumer (latex) reads **neither** `.expected` nor a result-union (import oracle, research-D §3/§7). Field-triple-on-returned-state suffices; result-union is 8B's *additive* typed wrapper of the failure arm, Y.13-thin (consumer = latex runtime error-msg + `.test-d.ts`). Both coexist non-competitively; the `buildDiagnostic` deciding experiment now merely *confirms* a decision already made on the consumer axis. |
| **8C ⟂ 8D** (below-combinator seams must survive regeneration) | binding constraint | **SHRUNK to the 5-symbol combinator surface** | The below-combinator seams (raw `new Parser((state)=>…)`, direct `mergeErrorState`) were **value.js's**; value.js is gone. latex reaches none of them. 8C's regeneration constraint relaxes to preserving only the 5 combinator-layer symbols verbatim. |
| **8A ⟂ 8C-ambitious** (runtime TS dispatch) | 8C-ambitious BANKED | **UNCHANGED — BANKED** | −14% runtime-switch KILL is law (C1, measured). Only door is a build-time-generated specialized dispatcher (LOCK-4B), not a runtime VM. |
| **8C ⟂ 8D** (diagnostics: should TS *also* compile-time-erase?) | conceded (behavioral, not mechanism) | **UNCHANGED — conceded** | Compile-time erasure kills the live per-parse surface; behavioral mirror satisfies clause-v both ways. The divorce removed the last consumer (keyframes) that even read it. |
| **8A-center-B ⟂ Y.13** (NEW, family-vs-constitution) | — | **COUPLED to 8D-G-Y13** | The arity-N specializer's corpus (value.js `all()`/`any()`) is gone (§0). It is consumer-less substrate UNLESS 8D declares parse-that's own suite the first-party consumer. This is not a family-vs-family clash but a *shared disposition*: 8A center-B ships iff 8D's Y.13 reckoning declares the internal corpus a consumer. |

**Net.** No two families are globally mutually exclusive. Pass-2 is *cleaner* than pass-1: the two
pass-1 collisions (8A⟂8D, 8A⟂8B) evaporate via the consumer-less `mapState` + the `ReadonlyState`
interface; the sharp 8B⟂8D is adjudicated toward the field-triple; 8C⟂8D shrinks. The single new
tension is a **shared Y.13 disposition** (8A center-B ∪ 8D surface pruning), resolved by one census.

---

## BLOCKED-CANDIDATE ADJUDICATION

*"A family stalled at a missing primitive equal in difficulty to the original problem is
BLOCKED-candidate. Elegance earns nothing on its own."*

**No family is BLOCKED-candidate.** Every central mechanism is constructible on current substrate;
what the divorce removed is *consumers* and *banked evidence*, not primitives.

- **8A** — the `ReadonlyState` view, the SFI predicate, and packrat homing are concrete. Center-B's
  arity-N specializer is a **Y.13-CANDIDATE** (missing a consumer, not a primitive) — it BANKS if
  8D's reckoning declines to declare the internal corpus a consumer. A bounded disposition, not a
  stall.
- **8B** — variadic tuples, `const` params, tail-recursion elimination all ship. The struck value.js
  evidence is a *re-measurement need* on latex native, not a missing tool; the full algebra is
  un-written, not impossible.
- **8C** — re-scoped to the ledger + parity corpora; the machinery is ceded to LOCK-6. The only
  route lacking a lawful V8 primitive (runtime dispatch) is BANKED, not the family. The regex
  alternation-order probe could re-classify one row DRIFT — a *result*, not a block.
- **8D** — pure npm/tsc/vitest construction; the crater is constructable and the census ran.

**The nearest thing to a block — a Y.13-SHADOW, not a mark.** The consumer divorce has made a large
fraction of parse-that's public surface consumer-less. This does not block any family; it forces one
explicit adjudication (8D-G-Y13) whose two lawful outcomes (declare the internal suite the consumer,
or prune) are both terminal answers. It is a decision the campaign can make, not a missing primitive.

**Owner dependency (not a block, a precondition).** value.js 4.0.0's divorce must be confirmed
*intended* before any 8B/8A/8D figure is banked. This is a lookup/confirmation, not a stall.

---

## THE TWO SHARPEST UNCERTAINTIES (prototype briefs)

Pass-1's two briefs measured the two EASIEST uncertainties (does inlining fire — PROTO-1 PASS; does
the thin type slice compile — PROTO-2 swerved onto a divorced consumer). Pass-2 has since ANSWERED
the SFI predicate (research-A §2), DESIGNED PT-E1 (research-D §4), RAN the import oracle, and made
the crater constructable. What remains genuinely un-run AND genuinely uncertain — and now everything
rides on **latex-paper as the sole surviving consumer** — are these two:

**Brief 1 — 8B (pt-scratch): the full typed algebra + `DropUndefined` budget + zero-cost, on
latex-paper NATIVE.** The untouched headline (Parser<any> INPUT leaks), the re-homed budget (the ONE
surviving real instantiation number), and the emit-identity proof — one runnable probe against the
sole surviving real type-level consumer, which no pass has run natively. Also closes 8D's `vue-tsc`
type-break axis (latex's `any()` sites under the tightened plane).

**Brief 2 — 8C (pt-scratch): the regex alternation-order behavioral differential.** The single
highest-value correspondence experiment; discriminates whether the regex row is a lawful behavioral
MIRROR or silent DRIFT. A *known* hazard (regex-automata leftmost-longest vs Irregexp leftmost-first)
whose answer could flip a `MIRRORED` classification; currently unfalsified.

*(Full self-contained briefs returned in the structured output.)*
