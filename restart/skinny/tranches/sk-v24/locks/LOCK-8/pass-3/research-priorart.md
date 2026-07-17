# LOCK-8 (THE TS BACKBONE PLANE) — pass-3 prior-art research

Problem class: the parse-that TypeScript combinator backbone as a first-class design surface —
**8A** V8 discipline (hidden-class stability, IC monomorphism, allocation-zero hot path), **8B**
TYPE-LEVEL inference totality (ExtractValue, no `Parser<any>` — TS's own inference story, distinct
from LOCK-1's Rust HM solver), **8C** isomorphic mirror of the Rust core (consumes LOCK-6), **8D**
consumer-contract-first (value.js / latex-paper / keyframes; semver; migration as artifact). No
family allegiance; a neutral sweep of what exists, what won and lost elsewhere, the constraints our
own evidence already imposes, and the traps named in comparable systems. Citations are `path:line`
(codebase) or URL (web).

**The single most binding datum:** the Rust-shaped tagged-union / jump-table dispatch was
implemented on V8, measured **10–14% SLOWER**, and **KILLED** (§2, `future-research.md:71-115`).
Every family that proposes "mirror the Rust instruction/enum/VM shape into TS" (8C, and the
LOCK-4C VM-core temptation) must clear that refutation, not reopen it.

---

## 1. Comparable systems — the field for THIS problem class

**Scannerless-combinator peers (runtime perf).** parse-that's own measured table
(`parse-that/docs/perf-optimization-ts.md:306-329`, all producing `JSON.parse`-equal output,
`deepEqual`-validated) is the definitive apples-to-apples field:

- **Parsimmon** (jneen — monadic LL(∞), immutable state) is the *architectural peer* and loses
  hard: 985 ops/s on data.json vs parse-that 4,779 (BBNF) / 5,480 (hand). Root cause: immutable
  state allocates a `ParserState` per success/failure (`perf-optimization-ts.md:57-80,335`).
  **Lesson (binds 8A):** immutable state is the single biggest perf sink; parse-that already won
  this by threading one mutable `ParserState` — an org to preserve, never relitigate.
- **Chevrotain** (lexer+parser, `IToken` allocation) — strongest non-native competitor (4,029-4,100
  ops/s honest), but 2-phase and materializes token objects; parse-that scannerless beats it
  1.17-1.93× and the advantage *grows with file size* (`:319-329`). **Trap it named:** its own
  benchmark was dishonest twice — recognizer-only (`outputCst:false`) and `.slice(1,-1)` instead of
  `JSON.parse` on escapes (`:20-26`). The correctness-parity-before-measurement law came from
  catching this. Chevrotain's two documented V8 laws bracket 8A: (i) *"define all cache properties
  in the constructor"* to hold one hidden-class path — exactly PROVE-11's fixed-from-birth fix, so
  parse-that already banks it; (ii) *"do NOT create a Parser instance for each input — re-use a
  single instance and reset its state between iterations"*
  (<https://chevrotain.io/docs/guide/performance.html>). Law (ii) is UNPAID: parse-that allocates a
  fresh `ParserState` per `.parse()` (R4 §6a; PROVE-11 removed the two inner arrays, not the
  object). 8A's "allocation-zero hot path" is thus not reached by PROVE-11 alone — the
  chevrotain-proven remedy is instance-reuse + state-reset, un-adopted here.
- **Peggy** (PEG→JS codegen), **Nearley+moo** (Earley) — 3-12× slower; codegen/Earley pay for
  generality parse-that does not need for LL(1) grammars.
- **arcsecond** (francisrstokes — zero-dep Parsec clone), **parjs** (GregRos — TS, fluent OOP),
  **parser-ts** (gcanti) — Parsec-style, immutable state, no V8-tuned hot path; parjs/parser-ts
  have decent inferred types but no allocation-zero discipline. Not measured threats.
  <https://github.com/GregRos/parjs> · <https://github.com/francisrstokes/arcsecond> ·
  <https://github.com/gcanti/parser-ts>

**Type-level peers (inference totality — the 8B problem).**
- **ts-parsec** (spakhm): infers syntax-tree types from combinators — "beautiful and fun" — but the
  author's own verdict is the cautionary datum: *"every time I do semi-advanced type hackery like
  this, I end up spending more time dealing with weird type errors than actually working on my
  grammar,"* recommending hand-coded parsers for serious work. <https://www.spakhm.com/ts-parsec>
  **Lesson (binds 8B / PT-3 W3):** type-level totality carries a real cognitive/compile-time tax;
  the win must be *inference of return types* (extinguishing `Parser<any>`), NOT a type-level DSL.
- **parsebox** (sinclairzx81): parser combinators implemented ENTIRELY in the type system
  (`Static.Tuple`, `Static.Parse`, `this['input']`→`output` static mapping). The extreme end of 8B —
  and the ceiling that names the boundary: pure type-level parsing hits TS instantiation-depth /
  tuple-recursion limits and type-checker slowdown. <https://github.com/sinclairzx81/parsebox>
  The ceiling is concrete: the **instantiation-depth limit is ~50** (since 4.1; a separate ~500
  recursion cap), and exceeding it throws **TS2589 "Type instantiation is excessively deep and
  possibly infinite"** (<https://oneuptime.com/blog/post/2026-01-24-fix-type-instantiation-excessively-deep-typescript/view>;
  <https://herringtondarkholme.github.io/2023/04/30/typescript-magic/>). A *full* `ExtractValue`
  over deeply-nested `all()`/`any()` tuples is exactly the shape that trips it; the literature's
  mitigations are **tail-recursive types**, **deferred instantiation** (recurse through an object
  property / array element, which TS defers past the depth counter), and **loop unrolling** (≈2×
  reachable depth) (<https://dev.to/susisu/how-to-create-deep-recursive-types-5fgg>). Convergent
  insight for the gestalt: parse-that's arity-unrolling (leaf.ts:39-57,183-251) is a *perf*
  monomorphization for 8A/W4 AND the literature's depth mitigation for 8B — the two families are
  ALIGNED on the unrolled arms, not in tension.
  **Trap:** do not let 8B drift toward compile-time *parsing*; the charter is compile-time *typing
  of a runtime parser*. The zero-runtime-cost law (constitution v) is satisfied by inference, not by
  moving parsing into types.

**Packrat SOTA reference.**
- **Ohm** (ohmjs, Dubroy/Warth — incremental packrat, SLE2017): its memo table is *instance /
  per-parse scoped*, not a module global.
  <https://ohmjs.org/pubs/sle2017/incremental-packrat-parsing.pdf> This directly validates PT-E1's
  move — per-parse scoping is the established form; a module-global latch is the anomaly.

**Industrial rust↔ts mirror pattern (8C/LOCK-6).** The dominant "one core, two languages" pattern
is **wasm-core + thin TS bindings** (LOCK-6C). Our own measured boundary: PROVE-14 proved a
**byte-exact tape crossing** and **4.4× stop-early get in TS**, but **full-walk in TS is ~0.91× —
native parse won, an honest non-goal** (`SPEC.md §0 row 14`; `PORTFOLIO.md:148-149,156-161`).
Isomorphism that WINS is *data-layout* (the offset tape) + *behavioral oracle*; isomorphism that
LOSES is *runtime control-flow shape* (§2 below). PROVE-14's full-walk non-goal is the local
instance of an external general law: a systematic WASM-vs-JS review finds WASM wins overwhelmingly
on **small inputs with few boundary crossings** (26.99× XS, 8.22× small) but the advantage
**collapses as input grows and JS↔WASM back-and-forth rises** (~1.12× medium; some large cases JS
edges it) because of boundary-crossing cost (<https://benchmarkingwasm.github.io/BenchmarkingWebAssembly/>;
<https://medium.com/@hashbyt/webassembly-vs-javascript-the-performance-battle-b2936d4c6c03>).
Verdict for 8C/6C: a wasm-core-as-sole-impl claim must be bounded exactly as PROVE-14 already
bounds it — the full-walk lane is a *named non-goal per the boundary law*, TS-native survives where
V8 measurably wins; the boundary is real, input-size- and crossing-frequency-dependent, and must
be measured (6D's null hypothesis defeated by cost, not asserted).

---

## 2. What won and lost elsewhere — the mechanisms

**V8 discipline (won, and its hard ceiling).** The hidden-class / inline-cache model is settled
prior art: declare all fields in constructor order → one transition path → monomorphic ICs; a
second shape → polymorphic; **>4 shapes → megamorphic**, generic hashtable dispatch, no inlining,
TurboFan refuses to optimize the function, **10–50× degradation**.
<https://dl.acm.org/doi/pdf/10.1145/3428242> (An Inline Caching Design for TypeScript) ·
<https://medium.com/@yashschandra/hidden-v8-optimizations-hidden-classes-and-inline-caching-736a09c2e9eb>

- **Won:** parse-that applies the whole playbook — arity-unrolled `any`/`fuseAll`
  (leaf.ts:39-57,183-251), `Int8Array(128)` FIRST-set dispatch (leaf.ts:100-151), sticky
  `regex.test()` zero-alloc (leaf.ts:344-358), `RegExpMatchArray` elimination via `test()`
  (`perf-optimization-ts.md:178-201`), mutable single-threaded `ParserState`, and PROVE-11's
  lazy-field fix (fields `undefined` at birth so the hidden class is fixed from birth, census of
  eager `=[]` sites **6→0**, `PROVE-11/REPORT.md:29-71`, VERIFY reproduced exactly).
- **Lost / structurally unfixable at 8A altitude:** the **megamorphic IC at the central
  `.parser(state)` dispatch** (R4 §6c; `perf-optimization-ts.md:202-207` measured
  `CallFunction_ReceiverIsNotNullOrUndefined` at **5.9% CPU**). parse-that's own conclusion:
  *"structural to the closure-based combinator pattern… A future architecture using tagged unions +
  switch dispatch could eliminate it entirely, but that's a fundamental API redesign"*
  (`:207,338`). Secondary deopt: `mapState`'s per-invocation `Object.create(state)` — the *only*
  prototype-chain object on the hot path (R4 §6b). **Binding lesson:** 8A ("shape-stable core") can
  deliver allocation-zero + hidden-class stability but CANNOT monomorphize the central dispatch —
  that is a LOCK-4 concern. An 8A design claiming IC monomorphism at the dispatch site over-reaches
  into a problem the perf doc already named as out of closure-core reach. (Closures avoid
  per-instance hidden-class churn but are not a megamorphism cure:
  <https://www.mail-archive.com/v8-users@googlegroups.com/msg14936.html>.)

**The tagged-union / runtime-switch escape — LAW-refuted on V8 (the most binding adjudication).**
The obvious answer to the structural megamorphic IC — replace closure dispatch with a discriminated
union + `switch(tag)` (the Rust `enum`-beats-`Box<dyn>` move) — was *implemented, measured,
falsified, and KILLED* in parse-that. The `SpanParser` tagged union with `callSpan()`
switch-dispatch ran **~10–14% SLOWER** than the closure span lane on an 8-arm CSS-value alt-token
scan, reproduced across three workloads with an independent adversarial re-run at −14%
(`future-research.md:71-115`: Tranche A.W3 falsified → B.W0 P-inv-28 KILLED, zero in-realm
consumers; the 15 `*Span` combinators were themselves removed in the 1.0.0 breaking cut,
`proof:no-span-surface` guards reintroduction). Verdict verbatim: *"V8's monomorphic-per-call-site
closure dispatch with inlining beats the recursive switch — the OPPOSITE of the Rust
enum-vs-Box<dyn> regime… The jump-table speedup premise does not transfer from Rust to V8/TS."*
**Binding on every route:** any 8A/8C proposal to escape the IC via a *runtime* discriminant is
dead by measured refutation. The ONE carve-out (verbatim, `future-research.md:107-114`) is a
*build-time-generated specialized dispatcher* — a flat array-of-ops, NOT a recursive runtime switch
— i.e. LOCK-4B's build-time fuser / bbnf codegen, never a runtime VM loop. **This directly
constrains LOCK-4C's TS face:** a bytecode VM interpreted by one `switch(op)` is the exact shape
refuted; 4C survives on TS only if dispatch is generated-and-specialized per grammar. The win lives
in the COMPILER (fewer FIRST-set monomorphic call sites), never in a cleverer runtime tag. **And it
constrains 8C:** LOCK-6's "reasonable point" must be *behavioral / data-layout* isomorphism here
(tape crossing, oracle corpus, wasm-core), not runtime control-flow-shape mirroring.

**Packrat + global mutable state (the named pitfall that IS PT-E1).** Packrat's core invariant — *a
nonterminal invocation at a given position must return the same result* — is what precludes
context-sensitive parsing under naive memoization (<https://en.wikipedia.org/wiki/Packrat_parser>;
"Principled Stateful Parsing", arXiv:1609.05365). A **module-global mutable diagnostics flag**
(`diagnosticsEnabled`, utils.ts:6) + module-global collected buffer (utils.ts:138-146) violates
exactly this under nested/concurrent parses: an inner parse that enables diagnostics leaks its
flag+buffer into an outer parse (R4 §5; PT-3 W2 defect analysis). The literature's answer is the
State-monad / per-parse-context discipline (avoid shared mutable index). Left-recursion in packrat
(Warth PEPM08) is a *separate* axis — parse-that's packrat is opt-in and arms only at first
`memoize()` (packrat.ts:290); nothing in LOCK-8 needs to reopen it, and the honest default-path
"pays nothing" claim is R4-verified.

**Semver for type-level tightening (the external LAW).** SemVer-for-TS's governing rule is **"no
new red squiggles"**: a minor must not introduce new consumer type errors.
<https://www.semver-ts.org/> Tightening a generic `any`→precise can therefore be a **breaking
(major)** change, because any consumer relying on the `any` widening gets a new error (cf. the TS
4.7 `extends`-inference tightening that broke `Maybe.of(42)` —
<https://www.learningtypescript.com/articles/why-typescript-doesnt-follow-strict-semantic-versioning>).
**Binding on PT-3 W3/W5:** "precise types are a superset for well-typed consumers, minor bump
justified" is TRUE only if the three consumer suites produce zero new red squiggles; the falsifier
is a real consumer `tsc` (value.js's 59 `all()` sites, latex-paper's grammar). PT-3 already flags
this (R3, §W5 semver review) — the prior-art authority to cite in the adjudication is
semver-ts.org, not an internal assertion.

---

## 3. Constraints our own evidence base already imposes

- **The honest-number law (SPEC §0/09, binds even pure-TS work — PT-3 §1.3).** PROVE-11 is
  **CONFIRMED_DELTA**, not GO: then-chain +13.6%, fused +15.6%, fused alloc −60.1% (exact, census
  6→0), but the **first-char dispatch lane is ~+5%, not the claimed +10.2%** — halved, misses ≥10%
  in BOTH load windows (`PROVE-11/VERIFY.md:1-46,118-125`). Every LOCK-8 design MUST carry dispatch
  at ~+5% no-regression, never gate it ≥10%. Inflating it is the exact §0/09 self-deception
  (PT-3 R1's counterexample). This is the concrete local instance of "no stable-port perf claims."
- **Paired-interleaving mandatory; warm benches are disinformation (SPEC §0/09).** Naive
  `vitest bench` cross-run is load-confounded — PROVE-11 measured baseline at loadavg 12, modified
  at loadavg **27** (`PROVE-11/REPORT.md:103-108`). Certified method: single-build-per-process,
  A,B,A,B per-sample interleaving, N≥12 rounds, min AND median, per-round loadavg. NEON-free TS so
  the digit-scan / block-scheduling / tape-navigation / shim laws do not bind by subject (PT-3 §1.3).
- **Type plane costs zero at runtime (constitution v).** W3 inference tightening must emit
  byte-identical `dist/` — proven by an empty *semantic* diff, not asserted (PT-3 §W3 gate). A leak
  closed with `as any` or altered emitted JS is a **defect**, not a win.
- **Consumer-less substrate forbidden (Y.13 / constitution viii).** PROVE-11's lazy fields are a
  substrate whose consumer is PT-E1 per-parse scoping — the pair lands together (PT-3 §W1→W2). A
  shipped lazy field with no live populator is a Y.13 violation.
- **Colocation grand edict + no god modules (constitution vi).** `utils.ts` (186 LOC) mixes the
  diagnostics latch+accumulator+collector (6-146) with whitespace/block-comment scanners (159-186)
  — a god module in gestation (R4 §5; PT-3 §W2 split into `diagnostics/` + `scan.ts`).
- **Non-clobber law (constitution; SPEC §2b; PT-3 §2).** `~/Programming/parse-that` is READ-ONLY to
  the bbnf campaign; designs land only via letter **BBNF-PT-G-2026-07-16** (supersedes the
  never-delivered BBNF-PT-F), placed by the owner or a parse-that-side session; prototyping is
  scratch-copy only. Any family assuming bbnf-side edits to parse-that is dead on arrival.
- **Consumer contract as gate (8D / SPEC §2b).** Close gates are the THREE real suites: value.js
  3.1.0 (`^1.0.0`), latex-paper 0.2.1 (**`^0.7.1` — version skew**, transitive keyframes.js 6.0.0).
  No design closes on liveness alone (R4 §7).

## 4. The traps, named (from comparable systems + our verifiers)

1. **Dispatch-inflation.** PROVE-11's VERIFY halved a headline number in fresh windows; a +8%
   reading is not a pass to celebrate when the honest carry is ~+5% (`VERIFY.md:118-125`; PT-3 R1).
2. **Megamorphic-IC-is-structural.** Shape stability (8A) does NOT monomorphize the central
   `.parser(state)` dispatch (5.9% CPU) — and neither does a runtime tagged-union/`switch`, measured
   **−10–14% SLOWER** and KILLED (`future-research.md:71-115`). Only a *build-time-generated
   specialized* dispatcher (LOCK-4B fuser) is a live escape; a runtime VM `switch(op)` inherits the
   refutation.
3. **Layout-isomorphism-into-TS.** Mirroring the Rust instruction/enum/VM shape at runtime is the
   killed encoding; the isomorphism that wins is the offset tape + behavioral oracle (PROVE-14) or
   wasm-core + thin bindings (LOCK-6C) — measured, not asserted.
4. **Type-level friction / DX cliff.** ts-parsec's author warning + parsebox's recursion-depth
   ceiling: type-hackery can cost more than the grammar and slow the type-checker; 8B must type a
   *runtime* parser, not move parsing into types.
5. **Stricter-inference-is-breaking.** SemVer-for-TS "no new red squiggles": `any`→precise can be a
   MAJOR change; the consumer `tsc` is the only falsifier (value.js 59 `all()`, latex-paper grammar).
6. **Version-skew migration.** latex-paper pins `^0.7.1`, predating the 1.0.0 `*Span` excision
   (S.H2) and `chain()` C-16; a 1.1.0 bump is an untested 0.7→1.1 jump whose breaks could be
   mislabeled as 1.1.0's (R4 §7; PT-3 R4/§W0 — dry-run the 0.7→1.0 leg in isolation first).
7. **Per-parse-scope allocation.** Threading a scope object allocated per `.parse()` regresses the
   diagnostics-off hot path below the W1 floor; the scope must be a null-or-field read on the
   default path (PT-3 R2).
8. **God-module-split over-reach.** Splitting `utils.ts` must lift diagnostics only; scanners move
   by rename, no logic change (PT-3 R5).
9. **Dishonest-benchmark (Chevrotain's lesson):** every competitor/arm must produce `deepEqual`
   output before timing, or the numbers are meaningless (`perf-optimization-ts.md:20-26,339`).
10. **Non-clobber breach.** A session with full file-level specs writing into
    `~/Programming/parse-that` violates HANDOFF §6.9; bbnf-side is scratch-copy + letter only.

## 5. Disposition per family (net standing for the design passes)

- **8A SHAPE-STABLE** — alloc-zero / hidden-class-stable is ~80% pre-won and re-verified (PROVE-11
  CONFIRMED_DELTA; census 6→0; fixed-from-birth shape; mutable state; arity-unrolls; sticky-`test()`;
  `dispatch()` LUT are the do-not-regress floor). Honest residual: the `.parser` megamorphic IC
  (10–50× at >4 targets; 5.9% CPU) is **structural and unsolved by dispatch**, and the tagged-union
  escape is a measured V8 trap. A design claiming "monomorphic by construction" at the dispatch site
  contradicts measured evidence — that ceiling belongs to LOCK-4.
- **8B TYPE-LEVEL TOTALITY** — externally feasible (parsebox proves type-level tuple inference);
  internally scoped to zero runtime cost with a RED-born type-test gate (`inference.test-d.ts`,
  proving precision both ways). Risks: DX/compile-time blowup (refuse type-level *parsing*) and
  consumer type breakage (SemVer-for-TS says consumer `tsc` decides minor vs major).
  `ParserFunction<T>` erasure is a *legitimate* bounded internal `any` (the IC needs variance at the
  implementation boundary), not counted as an open leak.
- **8C ISOMORPHIC MIRROR** — runtime-shape isomorphism into TS is **measured-hostile** (SpanParser
  −14%; PROVE-14 full-walk 0.91× non-goal). The isomorphism that wins is data-layout (offset tape,
  byte-exact) + behavioral oracle (LOCK-6 6D-leaning) or wasm-core + thin bindings (6C). Consumes
  LOCK-6's verdict; must not reopen the killed tier.
- **8D CONSUMER-CONTRACT** — the VALUEJS-PT-E asks (PT-E1 HIGH scoped diagnostics / PT-E2 MED
  inference / PT-E3 record — Pratt stays dormant, does not clear KISS/DRY); latex-paper `^0.7.1`
  skew; keyframes transitive; additive-minor semver gated on packed-tarball consumer suites;
  migration-as-artifact. PT-E1's per-parse scoping is a reentrancy-correctness fix (global latch
  cross-contaminates), Ohm-validated, not a perf claim. The recorded 1.0.0 non-goals
  (token streams, incremental, LR, SpanParser resurrection) must not be re-litigated
  (`future-research.md:196-227`).

The honest-number, paired-interleaving, zero-runtime-cost, and non-clobber laws are non-negotiable
constraints, not design choices.

### Sources
- Codebase: `parse-that/docs/perf-optimization-ts.md` (megamorphic 5.9% CPU; Chevrotain fair-bench
  law; regex `test()` alloc cure; the full 746→5,480 optimization history + peer table);
  `parse-that/docs/future-research.md:71-115,196-227` (SpanParser tagged-union refutation −10–14%,
  KILLED B.W0/P-inv-28 — the binding LAW; recorded 1.0.0 non-goals); `R4-parse-that-ts.md` (V8
  surfaces §6, PT-E1/E2 seams §5, consumers §7); `prototypes/PROVE-11/{REPORT,VERIFY}.md` (dispatch
  ~+5% not +10.2%; then +13.6% / fused +15.6% / alloc −60.1%); `docs/tranches/sk-v24/PT-3.md`;
  `parse-that/docs/tranches/A/VALUEJS-PT-E-2026-07-05.md`; `SPEC.md §0/§2b`; `PORTFOLIO.md`
  (Constitution + LOCK-6/PROVE-14 isomorphism seed).
- Web: <https://www.semver-ts.org/> + <https://www.semver-ts.org/formal-spec/2-breaking-changes.html>
  (widening-return / narrowing-still-errors) ; learningtypescript.com (TS 4.7 inference break);
  <https://www.spakhm.com/ts-parsec> ; <https://github.com/sinclairzx81/parsebox> ;
  <https://github.com/GregRos/parjs> · <https://github.com/gcanti/parser-ts> ;
  <https://github.com/francisrstokes/arcsecond> ; Ohm SLE2017 incremental packrat
  (ohmjs.org/pubs/sle2017) ; <https://en.wikipedia.org/wiki/Packrat_parser> +
  <https://bford.info/packrat/> (memory-cost / linear-only-with-memory) ;
  <https://dl.acm.org/doi/pdf/10.1145/3428242> (TS inline caching) ;
  V8 hidden-class/IC 4-shape megamorphic ceiling (yashschandra medium; ACM DL) ;
  v8-users closures-&-megamorphism thread ;
  <https://chevrotain.io/docs/guide/performance.html> (constructor-props + instance-reuse laws) ;
  TS2589 depth-50 wall + mitigations (oneuptime ; herringtondarkholme ; dev.to/susisu) ;
  WASM↔JS boundary-collapse systematic review (benchmarkingwasm.github.io ; medium/@hashbyt) ;
  Warth PEPM08 packrat left-recursion ; arXiv:1609.05365 principled stateful parsing.
