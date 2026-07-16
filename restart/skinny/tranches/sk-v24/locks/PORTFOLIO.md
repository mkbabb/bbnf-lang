# SK-V24 STAGE 6 — THE LOCKS: round-zero portfolio (minted by Fable, 2026-07-16)

Seven core locks, each resolved by the convergent multiagent design loop (≥3 passes, earned
convergence, fresh non-author audit, two consecutive clean passes), then THE GESTALT: one
agglomeration across both repos producing the perfected tranche set with proper DAG ordering —
conscious of the extant tranches (docs/tranches/sk-v24/ GN-0..6 + PT-1..3, and parse-that's
native series: VALUEJS-PT-*, KF-TO-PARSETHAT-*, its A/B/Q/S lineage). Families within a lock
are genuinely orthogonal architectural centers; two routes sharing a mechanism share a family.

## THE CONSTITUTION (binding on every lock, every pass)

i. **Grammar totality**: everything is grammar-driven, here and there, save primitives and
   combinators — the regex grammar IS bootstrapped and bbnf-defined.
ii. **True isomorphism**: the regex engine's HIR has true isomorphism to the bbnf HIR/IR
   system (LOCK-5 decides the lawful form; every other lock consumes its verdict).
iii. **rust/ts isomorphism to a reasonable point** — in layout and in code (LOCK-6 decides
   where "reasonable" ends; measured, not asserted).
iv. The refutation laws (SPEC §0): no digit-scan SIMD, no block-scheduling pursuit, no
   stable-port perf claims, fold-over-cursor dead, warm benches are disinformation, x86 dead.
v. The type plane costs zero at runtime (verified invariant).
vi. The colocation grand edict + extreme parsimony; no god modules; no name-gates.
vii. Concrete artifacts or the SPEC-ONLY label — running prototypes, measured numbers, named
   counterexamples; status reports and vague optimism are rejected.
viii. Consumer-less substrate is forbidden (Y.13) — in designs as in code.
ix. Extant-tranche consciousness: the GN/PT codex is the baseline the gestalt re-cuts, never
   silently contradicts — every divergence is a named supersession.

## LOCK-1 — THE SOLVER SPINE (type inference & solving, bbnf)

Task: the full type system for the grammar plane — `->` projections, document shapes, sums
and products, capacities, numeric widths, decode classes — inferred, solved, and erased to
zero runtime cost, with grammar-sourced diagnostics. GN-3's three walls are the floor, not
the system. Evidence seeds: PROVE-08 (HM core 221 LOC, derivation proven), R6 (the discard
sites), csp-solver crate (sibling csc411, vendored at crates/csp-solver).

- **1A UNIFICATION-FIRST**: the HM core grows — algebraic data types, row polymorphism for
  document shapes, let-generalization over rules; CSP appears only where unification is
  undecidable (widths, overloads) as a bounded post-solve.
- **1B ONE-CONSTRAINT-STORE**: every judgment (equality, width, capacity, decode class,
  cost) is one constraint system; unification is equality propagation inside the global
  store; local propagators + global search; the csp-solver crate is the engine.
- **1C BIDIRECTIONAL-LOCAL**: bidirectional checking, rule-local (synthesize/check against
  declared projections); global solving only at cross-rule seams; diagnostics-first design —
  every failure names its grammar span.
- **1D TYPES-AS-ECLASSES**: the type lattice lives in an e-graph; unification is congruence
  closure; projections canonicalize by saturation; extraction picks representative types by
  cost. (Shares the engine with LOCK-2; the FAMILY is the type-theoretic center.)

## LOCK-2 — THE GRAPH ENGINE (e-graph + CSP optimization, global and local)

Task: where and how graph optimization governs the pipeline — HIR canonicalization, IR
rewriting, kernel/backend selection, capacity and schedule decisions; global vs local CSP;
profile-fed cost models; deterministic, reproducible regen. Evidence seeds: egraph +
egraph-derive crates (in-tree), csp-solver, PROVE-12 (residualization), the KernelCostTable
lineage, R1's cost/BackendShape seam.

- **2A EGRAPH-ISLANDS**: a directed pass pipeline with e-graph islands at named decision
  points (regex HIR canon, scan-plan selection, projection canon); every island has a
  same-wave consumer and a cost model; CSP selects over extracted candidates.
- **2B IR-IS-EGRAPH**: the middle IR is an e-graph end to end; passes are rewrite groups;
  equality saturation under a schedule; extraction emits BackendShape directly.
- **2C CSP-CENTRAL**: the decision driver IS a CSP — variables are per-site lowering choices
  (kernel, capacity plan, prefilter tier, residualization depth), constraints are legality +
  the generated-size budget, the objective is a cost lattice fed by PMU facts; e-graphs only
  generate candidates.
- **2D PROFILE-CLOSED-LOOP**: optimization as measured feedback — cost coefficients fitted
  from canon-bench PMU captures, committed as fact tables (determinism preserved); the
  solver re-runs at regen; the loop is the architecture.

## LOCK-3 — THE IR TRANSPOSITION SPINE

Task: the IR tower and its transpositions — grammar AST → typed HIR → mid-IR → backend
shapes → emission; lowering contracts and invariants; residualization (PROVE-12)
generalized; the decision driver's seat; multi-backend (Rust now, TS later) over a
backend-agnostic middle. Standing laws: no value-discard, no silent epsilon, preserve rich
AST. Evidence seeds: R1 (ir/lib.rs, dispatch), R6 (DirectBuild IR), PROVE-12.

- **3A CONTRACT-TOWER**: a small fixed tower (three levels), each lowering a total function
  with a checkable contract — round-trip/parity oracles per level, CI-enforced.
- **3B SINGLE-IR-MULTIPASS**: one rich IR refined in place by annotating passes (the
  LLVM shape); BackendShape as attributes; the fewest transpositions that suffice.
- **3C GRAMMAR-HOMOIOUSIA**: the IR is itself grammar-specified — a bbnf-defined IR grammar;
  transpositions are grammar-to-grammar maps; the generator eats itself (bootstrap
  alignment with LOCK-7).
- **3D STAGED-RESIDUAL**: the IR is an explicitly two-stage program (comptime/runtime split
  in the types); residualization is erasure of stage 0 — PROVE-12 becomes a corollary;
  per-site staging solved by LOCK-2's engine.

## LOCK-4 — THE COMBINATOR CORE (parse-that, Rust + TS)

Task: the combinator algebra at beyond-SOTA — allocation model (the arena/ownership question
PT-2 carries), fusion (arity-unrolling proven in TS), packrat semantics, diagnostics
(PT-E1), and rust/ts semantic parity. Combinators remain lawful hand-written primitives
under the constitution — but their compilation and analysis need not be naive. Evidence
seeds: R3 (SmallBox core, alloc hotspots), R4 (V8 surfaces, deliberate monomorphizations),
PROVE-11, PT-2/PT-3 tranche docs.

- **4A CLOSURE-TO-GRAPH**: combinators construct a static parser graph (analyzable nodes,
  not opaque nested closures); fusion, packrat placement, and diagnostics points become
  graph analyses; the graph shape is isomorphic across rust/ts.
- **4B COMPTIME-FUSION**: Rust — monomorphized fusion chains, ownership redesigned so the
  vtable/SmallBox question dissolves; TS — a build-time fuser emits specialized chains (the
  arity-unroll generalized); no runtime graph at all.
- **4C VM-CORE**: combinators lower to a compact bytecode VM — one instruction set shared by
  rust and ts (layout isomorphism by construction); packrat and diagnostics are VM
  facilities; measured against the closure core, not assumed.
- **4D GRAMMAR-SUGAR**: combinators are a construction API over the same HIR the grammar
  plane uses (LOCK-5's continuum); parse-that keeps its public API and inherits the
  optimizer; the combinator engine is the generator's runtime in disguise.

## LOCK-5 — THE REGEX CONTINUUM (the grammar-driven regex engine)

Task: the regex engine reborn grammar-first — the regex grammar bbnf-defined and
BOOTSTRAPPED; an HIR truly isomorphic to bbnf's HIR/IR (one system, or a lawful functor);
NFA→DFA→minimization→prefilter derivation recast as solver problems (CSP/e-graph) where that
is measured to win; PROVE-10's prefilter tier, the Teddy tier, and the lazy DFA folded; the
1148-LOC classify re-derived or re-homed; the existing engine's test suite and consumers as
parity floor. Evidence seeds: R3 (bbnf-regex 6742 LOC, stable-clean), PROVE-10, regex-bootstrap
crate, PT-1 tranche doc.

- **5A ONE-HIR**: regex HIR is a sub-language of bbnf HIR — a single type in a single home;
  regex parsing rides the bootstrapped grammar; automata construction consumes bbnf IR;
  classify becomes an IR analysis pass.
- **5B LAWFUL-FUNCTOR**: two HIRs, one total functor each way, tested by round-trip oracles
  — isomorphism to a reasonable point; each side keeps domain-fit representations; the
  e-graph rewrite library is the shared part.
- **5C AUTOMATA-AS-EXTRACTION**: determinization, minimization (Hopcroft as
  rewrite-to-normal-form), and prefilter-tier selection as e-graph saturation + CSP
  extraction — measured head-to-head against the classical pipeline; wins stay, losses are
  named negative results.
- **5D GRAMMAR-FRAGMENT**: regex literals in grammars lower DIRECTLY into the grammar's own
  IR — no separate engine on the generator path at all; the standalone parse-that regex API
  survives as a thin front over the same machinery.

## LOCK-6 — THE ISOMORPHISM PLANE (the rust/ts split)

Task: proper isomorphism to a reasonable point, in layout and in code — what is mirrored
(module tree, names, oracle tables, tape layout, bench protocol), what stays idiomatic
per-language; V8 laws vs LLVM laws; the TS backend seed (PROVE-14) and the parse-that TS
backbone in one coherent plane. Evidence seeds: PROVE-14 (byte-exact tape crossing, 4.4×
stop-early, full-walk non-goal), R4, PT-3 tranche doc.

- **6A MIRROR-TREE**: hand-maintained mirrored module trees with an isomorphism LINTER (CI
  verifies tree correspondence and a name map); code idiomatic per language.
- **6B SPEC-GENERATED-SKELETON**: a shared machine-readable spec (grammar-driven, per the
  constitution) generates both skeletons — types, tape decoders, oracle tables; hand code
  fills idiomatic bodies; drift is impossible by construction where generated.
- **6C WASM-CORE**: the Rust core compiled to wasm is the one implementation; TS is bindings
  plus an idiom layer; TS-native code exists only where V8 measurably wins (the PROVE-14
  full-walk lesson is the proof such boundaries exist).
- **6D ORACLE-ISOMORPHISM**: behavioral isomorphism only — a shared conformance corpus and
  differential tests; layout free per language. (The null hypothesis: it must be DEFEATED by
  measured maintenance/drift cost, or it wins.)

## LOCK-7 — THE GRAMMAR TOTALITY & BOOTSTRAP

Task: the constitution's clause (i) as a design problem — bbnf-in-bbnf self-hosting depth,
the regex-grammar bootstrap (staging, fixpoint, committed stage-0), grammar-specified planes
(IR? emitter templates? oracle tables?), typed host functions for context-dependence
(hybrid-grammar-host law), what "primitive" lawfully means, and xtask/regen as the bootstrap
engine. Standing law: self-hosting uses language features — augment BBNF if features are
missing, never work around.

- **7A STAGED-BOOTSTRAP**: classic two-stage — committed stage-0 artifacts, fixpoint gate
  (regen∘regen = regen, byte-identical); minimal bootstrap surface, maximal auditability.
- **7B TOTAL-SELF-HOST**: bbnf grammar, regex grammar, IR grammar, emitter templates — all
  grammar-specified; the generator becomes mostly data; hand code shrinks to kernels and
  primitives; measured against build-time and debuggability budgets.
- **7C HOST-FUNCTION-LATTICE**: totality bounded by a TYPED host-fn interface — context-
  dependent and recursive semantics live behind declared signatures; totality is a measured
  percentage of surface behind grammars, with a named floor per subsystem.
- **7D META-CIRCULAR-ORACLE**: self-hosting as verification first — the bbnf-defined
  bbnf/regex grammars serve as differential ORACLES against the hand parsers; promotion to
  production parser is gated on measured parity + performance, subsystem by subsystem.

## Pass protocol bookkeeping

Registry per lock at locks/LOCK-N/REGISTRY.md (families ADVANCED / BANKED / BLOCKED /
RETIRED, convergence % per family, exact open gaps, next-pass directives). Pass artifacts at
locks/LOCK-N/pass-K/. Researchers in pass K receive the task statement and their family
charter ALONE — the registry and the currently favored approach stay withheld from research;
synthesis, critique, and agglomeration see everything. ≥3 passes per lock before convergence
talk; 100% = zero enumerated gaps + fresh non-author audit + two consecutive clean passes.
THE GESTALT runs only when every lock has converged or been returned as a rigorously
converged core with its exact remaining gap — it produces the perfected dual-repo tranche
set with DAG ordering, born-RED gates, π obligations, and a disposition for every banked
route.
