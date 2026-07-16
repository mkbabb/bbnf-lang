# LOCK-8 (THE TS BACKBONE PLANE) — pass-3 prior-art research

Problem class: the parse-that TypeScript combinator backbone as a first-class design
surface — V8 discipline (hidden-class stability, IC monomorphism, allocation-zero hot
path), TYPE-LEVEL inference totality (ExtractValue, no `Parser<any>`), packrat semantics,
the diagnostics plane (per-parse scoping), and the API/semver/consumer contract toward
1.1.0. No family allegiance; this is a neutral sweep of what exists, what won and lost
elsewhere, the constraints our own evidence base already imposes, and the traps named in
comparable systems. Citations are `path:line` (codebase) or URL (web).

---

## 1. Comparable systems — the field for THIS problem class

**The scannerless-combinator peers (runtime perf).** parse-that's own measured field
(`parse-that/docs/perf-optimization-ts.md:306-329`) is the definitive apples-to-apples
table (all producing `JSON.parse`-equal output, `deepEqual`-validated):

- **Parsimmon** (jneen — monadic LL(∞), immutable state) is the *architectural peer* and
  loses hard: 985 ops/s on data.json vs parse-that 4,779 (BBNF) / 5,480 (hand). Root cause
  named: immutable-state combinators allocate a `ParserState` per success/failure
  (`perf-optimization-ts.md:57-80,335`). **Lesson (binds 8A):** immutable state is the
  single biggest perf sink; parse-that already won this by threading one mutable
  `ParserState` — the org to preserve, not relitigate.
- **Chevrotain** (lexer+parser, `IToken` allocation) — the strongest non-native competitor
  (4,029-4,100 ops/s honest), but 2-phase: it materializes token objects. parse-that
  scannerless beats it 1.17-1.93× and the advantage *grows with file size*
  (`perf-optimization-ts.md:319-329`). **Trap it named:** its own benchmark was dishonest
  twice — recognizer-only (`outputCst:false`) and `.slice(1,-1)` instead of `JSON.parse`
  on escapes (`perf-optimization-ts.md:20-26`). The correctness-parity-before-measurement
  law came from catching this.
- **Peggy** (PEG→JS codegen), **Nearley+moo** (Earley) — 3-12× slower; codegen/Earley pay
  for generality parse-that does not need for LL(1) grammars.
- **arcsecond** (francisrstokes — zero-dep Parsec clone) and **parjs** (GregRos — TS,
  fluent OOP) — Parsec-style, immutable-state, no V8-tuned hot path; parjs has decent
  types but no allocation-zero discipline. Neither is a measured threat.
  https://github.com/GregRos/parjs · https://github.com/francisrstokes/arcsecond

**The type-level peers (inference totality — the 8B problem).**
- **typescript-parsec / ts-parsec** (spakhm): infers syntax-tree types from combinators —
  "beautiful and fun" — but the author's own verdict is the cautionary datum: *"every time
  I do semi-advanced type hackery like this, I end up spending more time dealing with weird
  type errors than actually working on my grammar,"* and recommends hand-coded parsers for
  serious work. https://www.spakhm.com/ts-parsec **Lesson (binds 8B / PT-3 W3):** type-level
  totality carries a real cognitive/maintenance tax; the win must be *inference of return
  types* (extinguishing `Parser<any>`), NOT a type-level grammar DSL.
- **parsebox** (sinclairzx81): parser combinators implemented ENTIRELY in the type system
  (`Static.Tuple`, `Static.Parse`). The extreme end of 8B — and the ceiling that names the
  boundary: pure type-level parsing hits TS instantiation-depth / tuple-recursion limits
  and type-checker slowdown. https://github.com/sinclairzx81/parsebox **Trap:** do not let
  8B drift toward compile-time *parsing*; the charter is compile-time *typing of a runtime
  parser*. The zero-runtime-cost law (constitution v) is satisfied by inference, not by
  moving parsing into types.

**The packrat SOTA reference.**
- **Ohm** (ohmjs, Dubroy/Warth) — incremental packrat, SLE2017. Its memo table is
  *instance/per-parse scoped*, not a module global.
  https://ohmjs.org/pubs/sle2017/incremental-packrat-parsing.pdf This directly validates
  PT-E1's move: per-parse scoping is the established form; a module-global latch is the
  anomaly.

---

## 2. What won and lost elsewhere — the mechanisms

**V8 discipline (won, and its hard ceiling).** The hidden-class / inline-cache model is
settled prior art: declare all fields in the constructor in fixed order → one transition
path → monomorphic ICs; a second object shape at a site → polymorphic; >4 shapes →
megamorphic (generic hashtable dispatch, no inlining).
https://medium.com/@yashschandra/hidden-v8-optimizations-hidden-classes-and-inline-caching-736a09c2e9eb
· https://richardartoul.github.io/jekyll/update/2015/04/26/hidden-classes.html

- **Won:** parse-that already applies the whole playbook — arity-unrolled `any`/`fuseAll`
  (leaf.ts:39-57,183-251), `Int8Array(128)` FIRST-set dispatch (leaf.ts:100-151), sticky
  `regex.test()` zero-alloc (leaf.ts:344-358), `RegExpMatchArray` elimination via `test()`
  (`perf-optimization-ts.md:178-201`), and PROVE-11's lazy-field fix (fields `undefined` at
  birth so the hidden class is fixed from birth, `PROVE-11/REPORT.md:29-71`).
- **Lost / structurally unfixable at 8A altitude:** the **megamorphic IC at the central
  `.parser(state)` dispatch** (R4 §6c; `perf-optimization-ts.md:202-207` measured
  `CallFunction_ReceiverIsNotNullOrUndefined` at **5.9% CPU**). parse-that's own conclusion:
  *"structural to the closure-based combinator pattern… A future architecture using tagged
  unions + switch dispatch could eliminate it entirely, but that's a fundamental API
  redesign"* (`perf-optimization-ts.md:207,338`). **Binding lesson:** 8A ("shape-stable
  core") can deliver allocation-zero + hidden-class stability, but it CANNOT monomorphize
  the central dispatch — that is a LOCK-4 concern (4A CLOSURE-TO-GRAPH / 4C VM-CORE). An
  8A design that claims IC monomorphism at the dispatch site over-reaches into a problem the
  perf doc already named as out of closure-core reach.

**The tagged-union / runtime-switch escape (LAW-refuted on V8 — the most binding
adjudication).** The obvious answer to §2's structural megamorphic IC — replace closure
dispatch with a discriminated union + `switch(tag)` (the Rust `enum`-beats-`Box<dyn>` move) —
was *implemented, measured, falsified, and KILLED* in parse-that. The `SpanParser` tagged
union with `callSpan()` switch-dispatch ran **~10–14% SLOWER** than the closure span lane on
an 8-arm CSS-value alt-token scan, reproduced across three workloads with an independent
adversarial re-run at −14% (`parse-that/docs/future-research.md:71-115`, Tranche A.W3
falsified → B.W0 P-inv-28 KILLED, zero in-realm consumers). The verdict verbatim: *"V8's
monomorphic-per-call-site closure dispatch with inlining beats the recursive switch — the
OPPOSITE of the Rust enum-vs-Box<dyn> regime… The jump-table speedup premise does not
transfer from Rust to V8/TS."* **Binding on every route:** any 8A/8C proposal to escape the
IC via a runtime discriminant is dead by measured refutation; the one carve-out (verbatim,
future-research.md:112-114) is a *build-time-generated specialized dispatcher* (a flat
array-of-ops, NOT a recursive runtime switch) — i.e. LOCK-4B's build-time fuser, never a
runtime VM loop. This directly constrains **LOCK-4C's TS face**: a bytecode VM interpreted by
one `switch(op)` is the exact shape refuted; 4C survives on TS only if dispatch is
generated-and-specialized per grammar. The win lives in the COMPILER (fewer/FIRST-set
monomorphic call sites), never in a cleverer runtime tag.

**Packrat + global mutable state (the named pitfall that IS PT-E1).** Packrat's core
invariant: *a nonterminal invocation at a given position must always return the same
result* — this is precisely what precludes context-sensitive / stateful parsing under
naive memoization (arxiv 1609.05365 "Principled Stateful Parsing";
https://en.wikipedia.org/wiki/Packrat_parser). A **module-global mutable diagnostics flag**
(`diagnosticsEnabled`, utils.ts:6) violates exactly this under nested/concurrent parses:
an inner parse that enables diagnostics leaks its flag+buffer into an outer parse (R4 §5;
PT-3 W2 defect analysis). The literature's answer is the State-monad / per-parse-context
discipline (avoid shared mutable index;
https://www.sderosiaux.com/articles/2018/06/15/a-simple-way-to-write-parsers-using-the-state-monad/).
Left-recursion support in packrat (Warth PEPM08; multiple-LR JIP29) is a *separate* axis —
parse-that's packrat is opt-in and arms only at first `memoize()` (packrat.ts:290); nothing
in LOCK-8 needs to reopen it, and the honest default-path "pays nothing" claim is R4-verified.

**Semver for type-level tightening (the external LAW).** SemVer-for-TS's governing rule is
**"no new red squiggles"**: a minor release must not introduce new consumer type errors.
https://www.semver-ts.org/ · https://www.semver-ts.org/formal-spec/index.html Tightening a
generic from `any` → precise is, per this spec, potentially a **breaking (major)** change,
because any consumer relying on the `any` widening gets a new error (cf. the TS 4.7
`extends object` inference tightening that broke `Maybe.of(42)` —
https://www.learningtypescript.com/articles/why-typescript-doesnt-follow-strict-semantic-versioning).
**Binding on PT-3 W3/W5:** "precise types are a superset for well-typed consumers, minor
bump justified" is TRUE only if the three consumer suites produce zero new red squiggles;
the falsifier is a real consumer `tsc` (value.js's 59 `all()` sites, latex-paper's grammar).
The plan already flags this (PT-3 R3, §W5 semver review) — the prior-art authority to cite
in the adjudication is semver-ts.org, not an internal assertion.

---

## 3. Constraints our own evidence base already imposes

- **The honest-number law (SPEC §0/09, binds even pure-TS work — PT-3 §1.3).** PROVE-11 is
  **CONFIRMED_DELTA**, not GO: then-chain +13.6%, fused +15.6%, fused alloc −60.1% (exact,
  census 6→0 emitted `=[]` sites), but the **first-char dispatch lane is ~+5%, not the
  claimed +10.2%** — halved, and misses ≥10% in BOTH load windows (`PROVE-11/VERIFY.md:1-46,
  118-125`). Any LOCK-8 design MUST carry dispatch at ~+5% no-regression, never gate it ≥10%.
  Inflating it is the exact §0/09 self-deception (PT-3 R1's named counterexample).
- **Paired-interleaving mandatory; warm benches are disinformation (SPEC §0/09 standing
  lesson).** The naive `vitest bench` cross-run is load-confounded — PROVE-11 measured the
  baseline at loadavg 12 and the modified run at loadavg **27** (`PROVE-11/REPORT.md:103-108`).
  The certified method is single-build-per-process, A,B,A,B per-sample interleaving, N≥12
  rounds, min AND median, per-round loadavg. A two-arm window asymmetry is the classic trap.
- **Type plane costs zero at runtime (constitution v).** W3 inference tightening must emit
  byte-identical `dist/` — proven by an empty *semantic* diff, not asserted (PT-3 §W3 gate).
- **Consumer-less substrate forbidden (Y.13 / constitution viii).** PROVE-11's lazy fields
  are a substrate whose consumer is PT-E1 per-parse scoping — the pair must land together
  (PT-3 §W1→W2). A shipped lazy field with no live populator is a Y.13 violation.
- **Colocation grand edict + no god modules (constitution vi).** `utils.ts` (186 LOC) mixes
  the diagnostics latch+accumulator+collector (6-146) with whitespace/block-comment scanners
  (159-186) — a god module in gestation (R4 §5; PT-3 §W2 split into `diagnostics/` + `scan.ts`).
- **Consumer contract as gate (8D / SPEC §2b).** The close gates are the THREE real suites:
  value.js 3.1.0 (`^1.0.0`), latex-paper 0.2.1 (**`^0.7.1` — version skew**), keyframes.js
  6.0.0 (transitive). No design closes on liveness alone (R4 §7).

## 4. The traps, named (from comparable systems + our verifiers)

1. **The dispatch-inflation trap.** PROVE-11's VERIFY halved a headline number in fresh
   windows; a +8% reading is not a pass to celebrate when the honest carry is ~+5%
   (`VERIFY.md:118-125`; PT-3 R1).
2. **The megamorphic-IC-is-structural trap.** Shape stability (8A) does NOT monomorphize the
   central `.parser(state)` dispatch (measured 5.9% CPU, `perf-optimization-ts.md:202-207`) —
   and neither does a runtime tagged-union/`switch`, which was measured **−10–14% SLOWER** and
   KILLED (`future-research.md:71-115`, §2 above). Only a *build-time-generated specialized*
   dispatcher (LOCK-4B's fuser) is a live escape; a runtime VM `switch(op)` inherits the
   refutation.
3. **The type-level-friction trap.** ts-parsec's author and parsebox's recursion-depth ceiling
   warn that type-hackery can cost more than the grammar and slow the type-checker; 8B must
   type a *runtime* parser, not move parsing into types.
4. **The stricter-inference-is-breaking trap.** SemVer-for-TS "no new red squiggles":
   `any`→precise can be a MAJOR change; the consumer `tsc` is the only falsifier (§2 above).
5. **The version-skew migration trap.** latex-paper pins `^0.7.1`, predating the 1.0.0
   `*Span` excision (S.H2) and `chain()` C-16 cut; a 1.1.0 bump is an untested 0.7→1.1 jump
   whose breaks could be mislabeled as 1.1.0's (R4 §7; PT-3 R4/§W0 — dry-run the 0.7→1.0 leg
   in isolation first).
6. **The per-parse-scope allocation trap.** Threading a scope object allocated per `.parse()`
   would regress the diagnostics-off hot path below the W1 floor; the scope must be a null-or-
   field read on the default path (PT-3 R2).
7. **The god-module-split over-reach trap.** Splitting `utils.ts` must lift diagnostics only;
   the scanners move by rename, no logic change (PT-3 R5).
8. **The dishonest-benchmark trap (Chevrotain's lesson):** every competitor/arm must produce
   `deepEqual` output before timing, or the numbers are meaningless
   (`perf-optimization-ts.md:20-26,339`).

## 5. Net standing for the design passes

The mechanism base is unusually settled: parse-that already holds the runtime-perf field
(measured, mutable-state + FIRST-set dispatch + arity-unroll + zero-alloc regex), and
PROVE-11's lazy-alloc lever is CONFIRMED_DELTA with an exact allocation mechanism. The OPEN
design questions LOCK-8 must resolve, each with prior-art bearing: (a) how far 8A's
"shape-stable core" honestly reaches given the structural megamorphic ceiling (answer: not to
the central dispatch — that is LOCK-4); (b) whether 8B's type-level totality is inference-only
(safe, zero-cost) vs type-level parsing (parsebox ceiling — refuse); (c) the semver class of
the inference tightening (SemVer-for-TS says consumer `tsc` decides minor vs major); (d)
per-parse diagnostics scoping as the packrat-consistent, Ohm-validated cure for the
module-global latch. The honest-number, paired-interleaving, and zero-runtime-cost laws are
non-negotiable constraints, not design choices.

### Sources
- Codebase: `parse-that/docs/perf-optimization-ts.md` (megamorphic 5.9% CPU; Chevrotain fair-bench
  law; regex `test()` alloc cure); `parse-that/docs/future-research.md:71-115` (SpanParser
  tagged-union refutation −10–14%, KILLED B.W0/P-inv-28 — the binding LAW); R4-parse-that-ts.md;
  PROVE-11 REPORT/VERIFY (dispatch ~+5% not +10.2%; then +13.6%/fused +15.6%/alloc −60.1%);
  PT-3.md; VALUEJS-PT-E-2026-07-05.md; SPEC §0.
- Web: semver-ts.org; learningtypescript.com (TS 4.7 inference break); spakhm.com/ts-parsec;
  github.com/sinclairzx81/parsebox; github.com/GregRos/parjs; ohmjs SLE2017 incremental packrat;
  en.wikipedia.org/wiki/Packrat_parser; V8 hidden-class/IC references (yashschandra, artoul);
  Warth PEPM08 packrat-left-recursion; arxiv 1609.05365 principled stateful parsing.
