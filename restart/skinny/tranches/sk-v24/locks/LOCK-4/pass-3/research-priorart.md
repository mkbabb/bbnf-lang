# LOCK-4 · THE COMBINATOR CORE — PASS-3 PRIOR-ART RESEARCH

*parse-that Rust + TS combinator algebra, beyond-SOTA. No family allegiance — a sweep of what
exists, what won and lost elsewhere, which constraints our evidence base already imposes, and the
traps named in comparable systems. Pass-3 tilt: the directives (Q1–Q8) pivot the search toward the
runtime-graph→comptime bridge, the arbitrary-host-closure / opaque-barrier boundary, the
monomorphization blowup budget, the one-plan-two-lowerings isomorphism oracle, and the
interpreter-setup tax. Every claim is an external URL or a verified local `path:line`. No builds run.*

---

## 0. THE PROBLEM CLASS (restated at the pass-3 seams)

A combinator core is hand-written primitives + a composition algebra. The universal seam, named in
every source: **combinators are ergonomic but the composition machinery is opaque — closures hide the
static grammar, and every SOTA answer recovers it, then stages / lowers / analyses it.** The pass-3
directives sharpen this into five *mechanism* questions the literature has already partly adjudicated:

- **Q1 runtime-graph → comptime**: how (if at all) a value-level combinator graph reaches
  monomorphized code.
- **Q2/Q3 the opaque barrier**: the `->` value plane vs the arbitrary host closure — how large the
  un-analyzable surface is, and what a serializable descriptor must carry.
- **Q4 fusion / blowup budget**: named caps on inlining before code-size / compile-time explode.
- **Q5/Q6 one plan, two lowerings + the isomorphism-of-record oracle**.
- **Q7 setup vs value-plane vs recognizer cost**: interpreter/VM state allocation and pooling.

The four LOCK-4 families (4A closure-to-graph, 4B comptime-fusion, 4C VM-core, 4D grammar-sugar) are
the four historical answers; the literature has priced most of their trade-offs.

---

## 1. Q1 — RUNTIME GRAPH → COMPTIME: THE BRIDGE IS *STAGING*, NOT A FREE LOWERING

The decisive prior-art fact for Q1: **a value-level combinator graph never reaches monomorphized code
"for free."** Monomorphization is driven by *static types*; a runtime `IrNode` carries no type
information the compiler can specialize on. The only two bridges the literature knows are:

1. **Build-time codegen over a statically-known graph** — the *First Futamura Projection*. The
   combinator library is an interpreter of the grammar; specialization (staging / macro / quotation)
   produces direct code for that grammar. This is the *only* path from a grammar *value* to
   monomorphized output, and it requires the graph be known at generation time (grammar-text → IR →
   emit), not merely at runtime.
   - **Jonnalagedda et al., "Staged Parser Combinators for Efficient Data Processing"** (OOPSLA 2014,
     Scala LMS) — staging "dissociates input parsing from parser composition and eliminates the
     intermediate data structures and computations associated with parser composition at staging
     time" — explicitly "closely mirroring the First Futamura Projection."
     <https://2014.splashcon.org/details/oopsla2014/17/Staged-Parser-Combinators-for-Efficient-Data-Processing>
   - **Moleike, "Staged Parser Combinators in Scala: Have Your Cake and Eat It (Too)"** — Scala-3
     quotation CPS: "continuations are fully evaluated at compile time," splicing the next parser's
     AST directly, fusing the whole grammar. Reported: **~50% faster than hand-written recursive
     descent, >25× faster than the unstaged Cats implementation, and allocates *less* than the
     hand-written parser** (the composition-boundary closures are gone).
     <https://moleike.github.io/blog/staged-parser-combinators/>
   - **flap (Yallop, Xie, Krishnaswami, PLDI 2023)** — staged, deterministic-GNF combinators generate
     "specialized token-free code that runs **two to six times faster**" and hit **~1.4 GB/s (~2.3
     cyc/B) on JSON**, beating ocamlyacc/asp. Fusion is "strikingly simple" *because* the normal form
     is deterministic. <https://arxiv.org/abs/2304.05276>, <https://github.com/yallop/ocaml-flap>

2. **Defunctionalization** — Reynolds's 1972 transform that replaces higher-order closures with a
   first-order data tag + one `apply`. **A reified combinator IR (`bbnf_ir::IrNode`, the 14-variant
   near-bijection — R3/Hand-forward) IS a defunctionalized parser**; the CPS-to-abstract-machine route
   (defunctionalize the continuation → a first-order transition function) is exactly the closure-graph
   → VM lowering of 4C. <https://en.wikipedia.org/wiki/Defunctionalization>,
   <https://dl.acm.org/doi/10.1145/773184.773202> (Danvy & Nielsen, "Defunctionalization at work").

**Verdict for Q1 (report, not resolve):** the runtime combinator API cannot reach comptime
monomorphization absent a codegen-reflection step; the only mechanism trace the literature supports is
*grammar-known-at-generation → staged emit*. A monomorphizing lowering therefore *demands* the graph be
a serializable plan the emitter consumes (this is why Q1 and Q5 are the same question wearing two hats).

---

## 2. Q2/Q3 — THE OPAQUE BARRIER IS THE APPLICATIVE/SELECTIVE vs MONADIC BOUNDARY

This is the most load-bearing pass-3 prior-art result. The literature has a *precise* name for "which
`->` sites an analysis can see through": the **applicative/selective vs monadic** split.

- **Applicative / selective** effects declare their structure *statically*: all arguments are known at
  construction, only the *choice among a finite set of branches* is deferred to runtime. Fully
  analyzable. **Mokhov et al., "Selective Applicative Functors"** (ICFP 2019) inserted `Selective`
  between `Applicative` and `Monad` precisely so that "all effects are declared statically but which
  to execute is chosen dynamically." <https://dl.acm.org/doi/10.1145/3341694>
- **Monadic bind (`>>=` / `flat_map`)** is the *opaque barrier*: the continuation is an arbitrary
  function, so "static analyzers treat all functions as black boxes and are unable to analyze anything
  that happens past a monadic bind" (Morpheus, arXiv:2305.07901 — carried from pass-1).
- **Willis, Wu, Pickering, "Staged Selective Parser Combinators"** (ICFP 2020 → the **Parsley**
  library) is the direct application: it compiles combinators to hand-written quality *by leveraging
  the static grammar tree*, and it **deliberately restricts to selective (not monadic) combinators** so
  the tree survives to be optimized and staged. Monadic `flatMap` in Parsley is the escape hatch that
  *forfeits* the static analysis. <https://mpickering.github.io/papers/parsley-icfp.pdf>

The Rust ecosystem confirms the same boundary *as an allocation fact*: chumsky's `flat_map` closure
"returns different parser types, so type erasure via `Box<dyn Parser>` is required, incurring heap
allocation and dynamic dispatch," whereas the applicative `zip` combinator "is built on the stack as a
concrete type … allowing monomorphization → inlining → LLVM optimization end-to-end."
<https://docs.rs/chumsky/latest/chumsky/trait.Parser.html> (via search synthesis).

**Mapping to Q2/Q3 (report):** a `->` that is a *pure finite projection* (MapExpr-expressible) is an
applicative/selective node — analyzable, fusable, monomorphizable. An *arbitrary host closure* is a
monadic bind — the `Opaque(HostFnId)` / needed `HostClosure` descriptor. The prior art's counsel is
uniform: **count and cap the monadic sites; they are the ceiling on every downstream analysis.** The
`≤2% barrier floor` the directive names is exactly the "keep the grammar selective" discipline Parsley
enforces. A serializable descriptor must carry, for a monadic site: the input-type it consumes, the
output-type it produces, and an *opaque identity* (host-fn id) — because its body is by construction
not reifiable. Selective encoding is the tool to *shrink* that set (turn "closure that picks among N
parsers" into a static branch node), not to eliminate it.

---

## 3. Q4 — THE FUSION / BLOWUP BUDGET IS A *MEASURED* CAP IN EVERY MONOMORPHIZING SYSTEM

Every system that fuses/monomorphizes hits the same wall and installs the same kind of cap; the
directive's pluggable {inline | enum-node | box} cost model has direct precedent.

- **Staged code duplication (the exponential trap).** The Moleike staging blog names it exactly: "if
  `k.succ` is a massive block of generated code, the compiler literally duplicates that code into your
  bytecode" — exponential growth across branches. **The fix is *join points*: extract the duplicated
  continuation into a local function.** That is the concrete blowup-budget mechanism — box/share at the
  point where inline duplication would explode. <https://moleike.github.io/blog/staged-parser-combinators/>
- **Rust monomorphization, measured in practice.** chumsky's own docs warn that "long chains of
  `Parser::or` … tend to produce exponential behaviour in Rust's trait solver," and prescribe the
  `choice()` combinator (identical behaviour, flat type) as the cap — a *named fanout threshold* past
  which you switch encoding. Chumsky also breaks the recursive-type knot with **boxed recursion**:
  `Box<dyn Parser>` *only* at the recursion point, concrete everywhere else.
  <https://docs.rs/chumsky/latest/chumsky/guide/_00_getting_started/index.html>,
  <https://github.com/zesterer/chumsky>
- **nom / winnow** monomorphize fully → fastest on the JSON micro-benchmarks (winnow leads, nom next,
  both clearly ahead of chumsky/pom — carried pass-1) *but* the type complexity "leaks into client
  code … not too dissimilar to early C++ template metaprogramming," and deep nesting explodes both
  types and compile time. The trade is measured, not asserted.
- **Parsley / FastParse** cap by *lowering to a VM / bytecode* instead of unbounded inline — a bounded
  instruction array cannot exponentially duplicate. This is the structural alternative to a fuse-arity
  cap: fuse to a ceiling, then emit ops.

**Verdict for Q4 (report):** the caps the directive asks for (fuse-arity, dispatch-fanout,
boxing-depth) are *the* levers real systems tune. The pluggable cost model is well-precedented; the
firing numbers must be *measured* on CSS **and** value.js (the directive's own requirement), because
the blowup is grammar-shape-dependent (or-chain fanout, recursion depth) — no universal constant
exists. The three encodings map cleanly: inline-monomorphize (nom/winnow, staged CPS) below the cap →
enum-node (chumsky `choice`, a flat dispatch) at fanout → box/join-point (chumsky boxed recursion,
Moleike join points) at recursion / duplication.

---

## 4. Q5/Q6 — ONE PLAN, TWO LOWERINGS + THE ISOMORPHISM ORACLE

**One plan → two lowerings** is the staging/LMS model generalized: one combinator AST, staged to
target A or target B. The *serializable* form of that plan already exists in the VM family — **a
bytecode program is a portable artifact both a Rust and a TS decoder can execute** (4C's isomorphism
dividend). The canonical instruction set:

- **LPeg parsing machine (Medeiros & Ierusalimschy, "A Parsing Machine for PEGs")** — PEGs compile to
  a VM with `Char/Jump/Call/Return/Choice/Commit/Fail/…` over a PC register, a subject register, and a
  single stack of call/backtrack frames; **the transformation PEG→machine is formally proven
  semantics-preserving.** <https://www.inf.puc-rio.br/~roberto/docs/ry08-4.pdf> This proof is the
  template for "one plan, two lowerings, byte-identical parses": if both faces decode the same proven
  ISA, identity is by construction, not by luck. GPeg (Yedidia, SLE 2021) extends the same machine to
  incremental parsing. <https://people.seas.harvard.edu/~chong/pubs/gpeg_sle21.pdf>

**The isomorphism-of-record oracle** (Q6) is **differential testing** — the settled method:

- "Two or more independent implementations of a specification are exercised on identical inputs, and
  discrepancies are automatically flagged." <https://en.wikipedia.org/wiki/Differential_testing>
- **Cross-Language Differential Testing of JSON Parsers** (ACM AsiaCCS 2024) is the exact shape of the
  dual oracle: same input, N language implementations, flagged divergence.
  <https://dl.acm.org/doi/10.1145/3634737.3657003>
- **Ali et al., "A Survey of Parser Differential Anti-Patterns"** (LangSec 2023) escalates the stakes:
  parser differentials are a *security* class, not a cosmetic one. <https://langsec.org/spw23/papers/Ali_LangSec23.pdf>
  → the Rust/TS byte-identity contract is a correctness/safety obligation, and the directive's insistence
  on a **live Rust golden (not a hand-faked blob)** is precisely the anti-pattern this literature warns
  against — a fabricated oracle hides exactly the divergences differential testing exists to catch.

**Verdict for Q5/Q6 (report):** the two-level oracle the directive specifies — *opcode-stream identity*
(the serialized plan decodes identically) **and** *Value-tree identity* (against the live Rust
interpreter) — is the standard differential-testing dual: structural equivalence of the plan + semantic
equivalence of the output. A bytecode/VM plan makes the first level *free* (bit-compare the op array);
the second must run both faces on a shared conformance corpus (JSON/CSS/value.js grammars). Note the
XL-D fact: the fast TS face is the build-time emitter — so the *fast* lowering need not decode the plan
at runtime at all; the runtime decoder survives as the isomorphism/correctness oracle, which is exactly
where opcode-stream identity earns its keep.

---

## 5. Q7 — SETUP vs VALUE-PLANE vs RECOGNIZER: POOL THE MACHINE, BOUND THE MEMO

The parsing-VM literature is precise about the three-way cost split the directive asks for.

- **Recognizer cost = the VM loop.** LPeg's machine is a tight PC/stack loop; the recognizer state is a
  *reusable* stack, not a per-parse allocation. This is the answer to `Interpreter::new`'s 8-Vec +
  FxHashMap setup: **the machine state is set up once and reset, not reallocated.** LPeg, Parsley
  (reusable instruction array + operand stack), and every production VM pool their state.
  <https://www.inf.puc-rio.br/~roberto/docs/ry08-4.pdf>
- **Setup tax is amortizable to ~0 by pooling.** The staged/hand-written comparison (Moleike) shows the
  win is *allocation elimination* at composition boundaries — the same principle applied to the
  interpreter itself: a pooled / `clear()`-reused scratch (the LPeg model) removes setup from per-parse
  cost. This mirrors PROVE-11's own certified TS result: making `ParserState` arrays lazy (`undefined`
  at construction, `??=` under a guard) cut alloc **−60.5%** and won +10–13% on light lanes precisely
  because the *setup* allocation was a large fraction of a small op (PROVE-11 REPORT §3). The lesson
  ports to Rust: the setup share dominates *single-parse* cost and vanishes under amortization.
- **Value-plane cost = the semantic actions / captures**, and its mass is the Vec growth in
  `many`/`sep_by` + the string arena — **5390 per-parse allocs vs only 6 construction allocs on
  twitter** (PROVE-09 REPORT, cited in PT-2 W4). This is the decisive measured split: the vtable/box
  question is 6 allocs; the value plane is 5390. **Pooling the setup and the machine state attacks the
  right mass; the arena-over-vtable-boxes does not** (PT-2 W4 is high-risk/low-reward *by measurement*
  — its reward ceiling is those 6 allocs, and its named trap is the unsound `'static` arena holding
  `'a`-closures).
- **The memo table is the other mass, and it must stay bounded + opt-in.** Packrat is O(n) space and
  "the cost of memoization can outweigh its benefits." The settled mitigations:
  **elastic sliding-window packrat** (Kuramitsu, IPSJ 2015 — heap upper-bounded by a runtime-sized
  window) <https://www.jstage.jst.go.jp/article/ipsjjip/23/4/23_505/_pdf>; **cut operators** that
  deallocate memo storage when an alternative is committed
  <https://www.researchgate.net/publication/221292729> (Redziejowski, "mostly constant space"). parse-that
  already obeys the strongest form: **packrat is OFF by default and arms only at first `memoize()`**
  (`PACKRAT_ARMED`, R4 §6g / packrat.ts:290) — the default LL(1) path pays nothing, which the
  literature endorses as the correct default.

**Verdict for Q7 (report):** the clean split is *recognizer (VM loop, cheap) / value-plane (captures +
Vec/arena growth, the 5390-alloc mass) / setup (the machine state, pool it to ~0)*. Prior art says pool
the setup, bound/opt-in the memo, and do **not** chase the 6-alloc vtable arena as the lever.

---

## 6. TRAPS NAMED IN COMPARABLE SYSTEMS (pass-3 checklist)

1. **Runtime-switch VM is dead on V8 — MEASURED.** parse-that's own A.W3 `SpanParser` tagged-union
   `switch` dispatcher ran **10–14% SLOWER** than monomorphic closure dispatch, re-run adversarially at
   −14%, then killed (`parse-that/docs/future-research.md:71-109`, carried pass-2). The recorded escape
   is *not* a recursive runtime switch but a **flat array-of-ops with a generated specialized
   dispatcher** — i.e. LPeg-style, not enum-recursion. Any 4C VM-core on the TS face must obey this;
   Chevrotain confirms the mechanism — its speed comes from shape-stable monomorphic objects and IC
   hits, not from a dispatch loop <https://chevrotain.io/docs/guide/performance.html>.
2. **Monomorphization exponential trait-solve blowup** (chumsky `.or` chains → use `choice()`); deep
   concrete nesting explodes compile time (nom/winnow). Cap fanout; box recursion.
3. **Staged code duplication** — massive continuations duplicate per branch; extract *join points*.
4. **Monadic bind = analysis barrier** — every un-selective `->` blinds fusion/prefetch/diagnostics;
   count them (Q3) and prefer selective encodings.
5. **Unsound `'static` arena holding `'a`-parameterized closures** (PT-2 W4's named trap) — the sound
   arena must be `'a`-bounded and threaded, never smuggled; MIRI-clean or refuted.
6. **Packrat O(n) memory** — keep opt-in; bound with sliding window / cut if ever default.
7. **Two-arm window asymmetry in benches** — PROVE-09's 1.34–1.57× was a *measurement artifact*
   refuted by paired interleaving; paired interleaving is mandatory even for "obvious" wins (SPEC §0/09,
   now law).
8. **Fabricated oracle blobs** — a hand-faked golden hides the very parser differentials differential
   testing exists to catch (LangSec anti-patterns); the golden must be a *live* Rust run (Q6).

---

## 7. Q8 — PREDECESSOR SHAPES (report only, per directive; not resolved here)

- **LOCK-5 (regex continuum).** XL-C already **retired the ONE-HIR functor**: the settled shape is
  *two domain-fit types* — an opaque `Regex(StringId)` grammar tier and a rich `Hir` engine tier — with
  a *cheap bridge*, not a lawful functor between two HIRs. The shared-substrate question no longer
  hinges on a single HIR. Open sub-question for LOCK-4: whether the combinator IR and the regex `Hir`
  share the e-graph rewrite *library* (they can) without sharing the *type* (they need not). This is an
  input; LOCK-4 does not settle it.
- **LOCK-7 (grammar totality & host-fn lattice).** The unresolved shape is the **typed host-function
  interface** that bounds totality — context-dependent / recursive semantics behind declared
  signatures, with a measured "% of surface behind grammars" and a named floor per subsystem. This is
  the *exact* home of the Q2 monadic-bind / arbitrary-host-closure descriptor: a `HostClosure` /
  `Opaque(HostFnId)` is a LOCK-7 host-fn-lattice entry, and *which* lock owns the internal-descriptor
  evaluator (Hex/Number/Enum) — LOCK-3 IR-VM lowering vs LOCK-7 host dispatch — is a genuine open seam
  the directive flags (Q6), reported here as unsettled.

---

## 8. SOURCES

- Mokhov et al., *Selective Applicative Functors*, ICFP 2019 — <https://dl.acm.org/doi/10.1145/3341694>
- Willis, Wu, Pickering, *Staged Selective Parser Combinators* (Parsley), ICFP 2020 —
  <https://mpickering.github.io/papers/parsley-icfp.pdf>
- Jonnalagedda et al., *Staged Parser Combinators for Efficient Data Processing*, OOPSLA 2014 —
  <https://2014.splashcon.org/details/oopsla2014/17/Staged-Parser-Combinators-for-Efficient-Data-Processing>
- Moleike, *Staged Parser Combinators in Scala* — <https://moleike.github.io/blog/staged-parser-combinators/>
- Yallop, Xie, Krishnaswami, *flap: A Deterministic Parser with Fused Lexing*, PLDI 2023 —
  <https://arxiv.org/abs/2304.05276>, <https://github.com/yallop/ocaml-flap>
- Reynolds defunctionalization — <https://en.wikipedia.org/wiki/Defunctionalization>; Danvy & Nielsen,
  *Defunctionalization at work* — <https://dl.acm.org/doi/10.1145/773184.773202>
- Medeiros & Ierusalimschy, *A Parsing Machine for PEGs* (LPeg) —
  <https://www.inf.puc-rio.br/~roberto/docs/ry08-4.pdf>; GPeg (Yedidia, SLE 2021) —
  <https://people.seas.harvard.edu/~chong/pubs/gpeg_sle21.pdf>
- chumsky docs (trait-solver blowup, `choice`, boxed recursion) —
  <https://docs.rs/chumsky/latest/chumsky/guide/_00_getting_started/index.html>,
  <https://github.com/zesterer/chumsky>
- Chevrotain runtime-performance guide (V8 monomorphism / hidden classes) —
  <https://chevrotain.io/docs/guide/performance.html>
- Differential testing — <https://en.wikipedia.org/wiki/Differential_testing>; *Cross-Language
  Differential Testing of JSON Parsers*, AsiaCCS 2024 — <https://dl.acm.org/doi/10.1145/3634737.3657003>;
  Ali et al., *Parser Differential Anti-Patterns*, LangSec 2023 —
  <https://langsec.org/spw23/papers/Ali_LangSec23.pdf>
- Kuramitsu, *Packrat Parsing with Elastic Sliding Window*, IPSJ 2015 —
  <https://www.jstage.jst.go.jp/article/ipsjjip/23/4/23_505/_pdf>; Redziejowski, *mostly constant
  space* — <https://www.researchgate.net/publication/221292729>
- Local: R3 (`.../sk-v24/research/R3-parse-that-rust.md`), R4 (`R4-parse-that-ts.md`), PT-2.md,
  PROVE-09/11 REPORT+VERIFY, DIRECTIVES-RESEARCH.md (pass-3), SPEC §0 (refutation laws).
