# LOCK-4 — THE COMBINATOR CORE · pass-3 · PRIOR-ART SWEEP

*parse-that Rust + TS. The problem class: a combinator algebra at beyond-SOTA — allocation
model (the arena/ownership question), fusion, packrat placement, diagnostics, and rust/ts
semantic parity. Families under judgment: **4A** closure-to-graph, **4B** comptime-fusion,
**4C** VM-core, **4D** grammar-sugar. No family allegiance; this is what exists, what won and
lost elsewhere, and the traps.*

---

## 0. The problem, stated as the field states it

Naive combinator parsers are slow for ONE reason the literature agrees on: *host-language
abstraction overhead*. Combinators build deeply nested closure objects on the heap plus the
intermediate allocations at composition boundaries, and dispatch through them per byte — this
is exactly what R3/R4 measured in parse-that (Rust `SmallBox<dyn ParserFn>` per combinator;
TS `.parser(state)` as "THE megamorphic IC of the engine", R4 §6c). Every family in LOCK-4 is
a different answer to "remove the closure tower's cost without losing the algebra." The field
has settled answers, and they disagree with each other in instructive ways.

---

## 1. Family 4A — CLOSURE-TO-GRAPH (analyzable nodes, not opaque closures)

**The dominant modern research line, and it WON where it was tried.**

- **Parsley** (Willis & Wu, *Staged Selective Parser Combinators*, ICFP 2020;
  [paper](https://mpickering.github.io/papers/parsley-icfp.pdf),
  [hackage](https://hackage.haskell.org/package/parsley),
  [scala port](https://github.com/j-mie6/parsley)) is the direct precedent for 4A. Parsers are
  reified as an AST — the **Combinator Tree** — analysed and optimised by static passes BEFORE
  code is emitted, then lowered to an **abstract machine in CPS form**. The binding finding:
  *"parser combinator hierarchies can be replaced by a finite state automaton, which can be
  implemented more efficiently than a recursive top-down parser."* That is 4A's thesis, already
  validated in production Haskell + Scala. The **selective** functor (static choice/branch
  structure exposed to analysis) is *why* the graph is analyzable — free monads over-hide
  control flow; selectives expose it. Lesson for 4A: the node algebra must expose branch
  structure statically or the graph analyses (fusion, packrat placement, diagnostics points)
  have nothing to chew on.

- **Combinator-tree = the AST-is-combinator law we already hold.** MEMORY `no-ts-ir` ("AST IS
  the combinator") and 4A's own charter ("static parser graph, isomorphic across rust/ts") are
  the *same* structure Parsley calls the Combinator Tree. The prior art says: do not invent a
  fresh IR under the combinators; the reified combinator graph IS the IR.

- **Where 4A costs**: the graph must be built once and analysed — the staging boundary. In a
  library (not a generator) that boundary is fragile: Parsley pays it via Typed Template Haskell
  (compile-time). parse-that-TS has no macro stage, so a 4A "runtime graph then analyse" risks
  paying analysis per grammar-construction. R4 already records the mitigation the TS core uses
  instead — arity-unrolled monomorphic arms + an `Int8Array(128)` dispatch LUT (leaf.ts) — a
  *hand-applied* graph optimisation. 4A's question is whether to systematize that into a real
  node graph or keep it as point monomorphizations.

---

## 2. Family 4B — COMPTIME-FUSION (specialized chains, no runtime graph)

**Won decisively where a staging mechanism exists; the strongest measured numbers in the field.**

- **Staged parser combinators** (Jonnalagedda et al., *Staged Parser Combinators for Efficient
  Data Processing*, OOPSLA 2014;
  [ACM](https://dl.acm.org/doi/10.1145/2714064.2660241),
  [author](http://manojo.github.io/2015/09/02/staged-parser-combinators)). The number to cite
  honestly: a staged parser runs **~50% faster than a hand-written recursive-descent parser**
  and **>25× faster than the un-staged combinator version**. Mechanism = 4B exactly: staging
  dissociates parsing from composition, CPS-encodes the combinators so *fusion reduces to
  function composition*, and partial evaluation removes it at stage time — emitted code "looks
  like a hand-written descent parser." This is 4B's TS "build-time fuser emits specialized
  chains" and Rust "monomorphized fusion chains."

- **flap** (Boruch-Gruszecki et al., *A Deterministic Parser with Fused Lexing*, PLDI 2023;
  [pdf](https://arxiv.org/pdf/2304.05276)) extends the line: combinators → typed automaton with
  lexing FUSED into the parse, deterministic, hand-quality codegen. Relevant to 4B *and* to the
  LOCK-5 continuum (scanner/regex fusion): the winning modern shape is combinators-to-automaton
  with the lexer folded in, not a separate scan pass.

- **The parse-that TS core already ships 4B's kernel, hand-rolled.** R4 §6d: `fuseAll`
  arity-2/3 unrolled arms (leaf.ts:183-251) collapse N-1 closures into one specialized chain;
  Rust `seq!`/`alt!` macros collapse N-1 boxes into ONE `Box<dyn ParserFn>` (R3 §5,
  macros.rs:2,80). Comptime-fusion at N=2,3 by hand. 4B's real question: generalize the unroll
  (a build-time fuser) vs. the exponential specialization blow-up that killed naive template
  metaprogramming. **Named trap**: full monomorphization of arbitrary chains explodes code size
  and, on V8, *pollutes* the megamorphic IC rather than curing it (R4 §6c names `.parser(state)`
  as the one megamorphic IC — arity unroll narrows it, does not remove it). Parsley chose a
  *staged interpreter* over a fully-unrolled emitter for exactly this reason: it bounds code
  size while keeping fusion.

---

## 3. Family 4C — VM-CORE (compact bytecode, one instruction set rust+ts)

**Won for portability and space; the classic answer, trade-offs fully known.**

- **The LPEG parsing machine** (Medeiros & Ierusalimschy, *A Parsing Machine for PEGs*, DLS
  2008; [ACM](https://dl.acm.org/doi/10.1145/1408681.1408683)) is the canonical VM-core. Each
  pattern compiles to a program for a virtual parsing machine; programs compose dynamically. The
  load-bearing motivation is space: *"Packrat has guaranteed worst-case linear time but also
  linear space with a rather large constant"* — the VM sidesteps the packrat memo table. That is
  4C's pitch: layout-isomorphism-by-construction (one instruction set) AND packrat as an
  *optional VM facility* (a memo instruction), not a default cost.

- **gpeg** (Yedidia, *Fast Incremental PEG Parsing*, SLE 2021;
  [pdf](https://people.seas.harvard.edu/~chong/pubs/gpeg_sle21.pdf),
  [repo](https://github.com/zyedidia/gpeg)) proves the VM extends to *incremental* parsing —
  directly relevant to LOCK-9's 9D lazy/streaming surface. Ports of LPEG's bytecode to Java
  (pegless) and Go (gpeg) show the instruction set travels across languages — the empirical
  evidence FOR 4C's rust↔ts layout-isomorphism claim.

- **The measured caution the constitution demands (vii, artifacts-or-SPEC-ONLY).** A VM is an
  *interpreter*: it trades the closure indirect-call for a bytecode dispatch loop. On a modern
  OoO core that loop can cost as much as the vtable it replaced — the precise reason SPEC §0/04
  refuted block-scheduling (the M-class core already pipelines) and why 4C's charter says
  "measured against the closure core, NOT assumed." **No prior-art number says a PEG VM beats a
  staged/fused emitter on throughput** — LPEG's win is space + dynamic composability, not raw
  speed vs. hand code. 4C stands on the isomorphism/packrat/streaming dividend, gated by a
  throughput floor, or it loses to 4B on the hot path.

---

## 4. Family 4D — GRAMMAR-SUGAR (combinators as an API over the same HIR)

- The **generator-eats-itself** position and the union's stated telos (SPEC §1): parse-that's
  combinator API becomes a thin front over the same HIR the grammar plane / LOCK-5 continuum
  uses, inheriting the optimizer for free. Nearest prior art is again **Parsley** (the combinator
  tree IS the grammar representation) and **flap** (combinators and lexing share one automaton).
  Internal precedent: MEMORY `grammar-authoritative-status` + `hybrid-grammar-host` (leaf
  semantics via grammar, context-dependence behind typed host fns). 4D's risk is the **consumer
  contract**: value.js/latex-paper/keyframes (R4 §7) import a concrete `Parser<T>` combinator
  surface; a "combinators are sugar over HIR" reformulation must preserve that public API or it
  breaks three consumers (LOCK-8 / PT-3 W5 gate). Prior art offers no free lunch — Parsley/flap
  are greenfield APIs, not retrofits over a shipped 1.0.0 library with external semver consumers.

---

## 5. Constraints our own evidence base already imposes (binding on every family)

- **§0/11 — TS lazy diagnostic alloc is CONFIRMED_DELTA, not CONFIRMED.** PROVE-11 VERIFY:
  fused/unfused clear ≥10% (fused +15.6%, then-chain +13.6%, alloc −60.1% exact), but the
  **dispatch lane is ~+5%, not the claimed +10.2%** — it MISSES the ≥10% gate in both load
  windows. Binding (PT-3 §1.3): dispatch is a *no-regression* lane carried at ~+5%; inflating it
  is the §0/09 self-deception. Any family leaning on a dispatch-lane speedup over-claims against
  our own re-measure.

- **§0/09 — no shim perf claims; paired interleaving mandatory even for "obvious" wins.** The
  portable_simd→NEON shim's 1.34–1.57× did NOT reproduce (two-arm window asymmetry). PT-2 §1.1:
  the de-nightly port makes **ZERO** perf claim; value = stability. Any 4A/4B/4C throughput claim
  must be cold, paired, single-build-per-process, N≥200 / ≥12 rounds, two windows (PT-2 §3.4,
  PT-3 §1.2) — the naive `vitest bench`/`divan` cross-run is refused as evidence.

- **PT-2 W4 — the arena is HIGH-RISK, LOW-REWARD by measurement.** Named unsoundness trap:
  `ParserFn<'a>` is parameterized by the *input* lifetime; a `'static`/thread-local arena holding
  `'a`-closures is **unsound** (the arena lifetime must be ≤ `'a`, threaded explicitly, never
  smuggled). Reward ceiling is *known and small*: the arena moves only the **6 construction
  allocs** (5960 B), never the **5390 per-parse allocs** (Vec growth in `many`/`sep_by` + string
  arena — the real mass). PT-2 W4 is "sound-or-refuted, never sound-appearing"; a measured
  refutation is a valid close (P4/P39). **Any 4B "ownership redesign so the vtable/SmallBox
  question dissolves" must confront this: the vtable box is NOT where the allocation mass lives.**
  The mass is in the *collection* combinators, which 4A/4C also do not touch.

- **The type plane costs zero at runtime (constitution v).** 4D's HIR-inheritance and PT-3 W3's
  `Parser<any>`-leak closure are compile-time only — emitted JS/Rust byte-identical (proven by an
  empty `dist/` semantic diff). No family may buy inference precision with a runtime cast or an
  altered emit.

- **Consumer contract is the terminal falsifier (LOCK-8 / PT-3 W5).** value.js (59 `all()` sites,
  `dispatch()`), latex-paper (`^0.7.1` SKEW — untested 0.7→1.1 jump), keyframes.js (transitive).
  Any reformulation ships only if all three suites stay green; the placement law (bbnf never
  writes `~/Programming/parse-that`; owner/parse-that-side + letter BBNF-PT-G) is unchanged.

---

## 6. Traps named in comparable systems (the negative corpus)

1. **Full monomorphization blows up code size AND can worsen the IC.**
   [winnow](https://epage.github.io/blog/2023/07/winnow-0-5-the-fastest-rust-parser-combinator-library/) /
   [chumsky](https://github.com/zesterer/chumsky): winnow is the *fastest* Rust combinator library
   and it got there by **removing performance cliffs**, NOT maximal monomorphization — "being the
   fastest was a non-goal; fast enough" (chumsky). The field's fastest libraries chose
   ergonomics + cliff-removal over a fully-fused emitter. A 4B chasing total specialization is
   pursuing a target the field abandoned as diminishing.

2. **Packrat is a *cost*, not a default win** (LPEG's founding motivation; Ford's original
   [packrat](https://bford.info/packrat/)): linear time, linear space with a large constant.
   parse-that already ships this correctly — packrat OFF by default, arms permanently at first
   `memoize()` (R4 §6g, packrat.ts:290), LL(1) path pays nothing. 4A/4C "packrat placement as a
   graph/VM facility" must preserve *pay-nothing-when-unused*; the prior-art failure mode is a
   memo probe on the hot path.

3. **Left recursion is a real but bounded hazard** (Warth et al.,
   [PEPM 2008](https://web.cs.ucla.edu/~todd/research/pepm08.pdf); Pika parsing, Hutchison,
   [arXiv 2005.06444](https://arxiv.org/abs/2005.06444)). Packrat extends to (even mutual) left
   recursion via depth-limited memo growth, super-linear only on pathological grammars, not
   typical use. Pika reformulates packrat *bottom-up*, getting left recursion + error recovery
   for free — a design point for a 4C VM that wants recovery as a facility. Not a blocker for the
   combinator families, but a named axis if diagnostics (PT-E1) grows toward recovery.

4. **V8 prototype-chain views deopt callers** (R4 §6b): `mapState` builds `Object.create(state)`
   per invocation — slower access, deopt risk. Any TS-side graph/VM introducing per-step view
   objects reproduces this. Shipped mitigation: monomorphic own-property shapes (PROVE-11's
   fixed-from-birth hidden class); a family adding fields/views must prove `%HaveSameMap` holds
   (PT-3 W4 shape oracle).

5. **The interpreter-dispatch tax** (4C's specific hazard). No PEG-VM paper claims a throughput
   win over hand/staged code; the wins are space, dynamic composition, incrementality (gpeg),
   portability. Judged purely on the hot loop, a bytecode VM re-introduces an indirect dispatch —
   the exact cost the closure tower had. 4C earns its place only via the isomorphism +
   packrat/streaming dividend, gated by a throughput floor vs. the closure core.

---

## 7. Verdict for the synthesis fleet (no allegiance, the field's balance of evidence)

- **4A (closure-to-graph): strongest external validation** (Parsley, production Haskell + Scala)
  and matches our "AST IS the combinator" law — but its staging boundary is cheap only where a
  compile-time stage exists (Rust macros / the generator; NOT TS runtime).
- **4B (comptime-fusion): strongest NUMBERS** (staged combinators 50% over hand-written, 25× over
  unstaged; flap fused-lexing) — but the fastest *libraries* (winnow) declined total
  specialization, and our own arena evidence shows the vtable box is not the alloc mass. 4B must
  fuse the *collection* combinators, not just the sequence chain, to touch the real 5390-alloc mass.
- **4C (VM-core): owns isomorphism + packrat-facility + streaming** (LPEG, gpeg) — but no prior
  art gives it a throughput win; it stands or falls on the layout-isomorphism dividend measured
  against the closure core, never assumed.
- **4D (grammar-sugar): the frontier and the union's telos** — Parsley/flap show
  combinators-over-one-representation is coherent, but no prior art retrofits it over a shipped
  library with three semver consumers; the consumer contract is the gate.

The families are genuinely orthogonal centers: 4A and 4C *share* a reified representation (graph
vs. bytecode of the same tree) and could co-home; 4B is the emitter strategy that could serve
either (extract-to-fused-chains from 4A's graph); 4D is the API-homing question composing with
all three. The evidence base forbids any of them a *speed* claim without cold paired re-measure,
forbids leaning on the dispatch lane (§0/11 halved it), and warns that the allocation prize the
arena chases (PT-2 W4) is 6 allocs, not 5390 — the mass is in the collection combinators every
family currently leaves untouched.

---

### Sources
- Willis & Wu, Staged Selective Parser Combinators, ICFP 2020 — https://mpickering.github.io/papers/parsley-icfp.pdf · https://hackage.haskell.org/package/parsley · https://github.com/j-mie6/parsley
- Jonnalagedda et al., Staged Parser Combinators for Efficient Data Processing, OOPSLA 2014 — https://dl.acm.org/doi/10.1145/2714064.2660241 · http://manojo.github.io/2015/09/02/staged-parser-combinators
- Boruch-Gruszecki et al., flap: A Deterministic Parser with Fused Lexing, PLDI 2023 — https://arxiv.org/pdf/2304.05276
- Medeiros & Ierusalimschy, A Parsing Machine for PEGs, DLS 2008 — https://dl.acm.org/doi/10.1145/1408681.1408683
- Yedidia, Fast Incremental PEG Parsing (gpeg), SLE 2021 — https://people.seas.harvard.edu/~chong/pubs/gpeg_sle21.pdf · https://github.com/zyedidia/gpeg
- Ford, Packrat Parsing and PEGs — https://bford.info/packrat/
- Warth et al., Packrat Parsers Can Support Left Recursion, PEPM 2008 — https://web.cs.ucla.edu/~todd/research/pepm08.pdf
- Hutchison, Pika parsing, arXiv 2005.06444 — https://arxiv.org/abs/2005.06444
- Winnow 0.5 / chumsky — https://epage.github.io/blog/2023/07/winnow-0-5-the-fastest-rust-parser-combinator-library/ · https://github.com/zesterer/chumsky
- Internal seeds: SPEC §0 (11 CONFIRMED_DELTA, 09 REFUTED, 02/04 laws), PT-2.md W4 (arena soundness trap, 6-vs-5390 alloc split), PT-3.md (dispatch honest ~+5%, consumer gates), R3 §5 (SmallBox/alloc hotspots), R4 §6 (V8 surfaces), PROVE-11 REPORT+VERIFY.
