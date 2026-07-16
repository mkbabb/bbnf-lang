# LOCK-4 (THE COMBINATOR CORE) — pass-3 PRIOR-ART RESEARCH

*Prior-art researcher, pass 3. No family allegiance. Registry withheld by protocol.
Scope: the combinator algebra at beyond-SOTA — allocation model, fusion, packrat,
diagnostics, rust/ts semantic parity — across parse-that (Rust + TS).*

Task statement: PORTFOLIO.md LOCK-4 (lines 97-117). Families 4A CLOSURE-TO-GRAPH,
4B COMPTIME-FUSION, 4C VM-CORE, 4D GRAMMAR-SUGAR. Constitution binds every judgment
(PORTFOLIO.md §18-34). Evidence seeds R3, R4, PROVE-11 (+VERIFY), PROVE-09 (+VERIFY,
the refuted perf lesson), PT-2, PT-3.

---

## 1. THE PROBLEM CLASS — what "beyond-SOTA combinators" means in the field

The combinator-performance literature has one settled thesis: **naive combinators are
slow because abstraction (nested closures) is opaque to the compiler; the cure is to make
the combinator a DATA STRUCTURE you can analyze and specialize, then partially-evaluate it
against the grammar (First Futamura Projection).** Every winning system in the field does
exactly this. The four LOCK-4 families are the field's four settled answers, and the
external record already tells us which won, which lost, and where each breaks.

### The canonical wins (external, measured)

- **flap (Yallop/Dolan/Kammar/Krishnaswami, PLDI/OOPSLA 2023)** — typed parser combinators
  + derivative lexers, normalized into a *deterministic Greibach Normal Form* (single-char
  lookahead, no backtracking), then **fused** so lexing and parsing become one pass that
  "branches only on individual characters, not intermediate structures" and never
  materializes tokens. **~1.4 GB/s on JSON = a little over 2.3 cyc/B**, 3-9× ocamlyacc.
  This is the strongest external number for the exact problem class and the direct
  ancestor of family 4B. [flap]
- **Parsley / staged selective parser combinators (Willis, Wu, Pickering, ICFP 2020)** —
  combinators are an *analyzable lazily-constructed AST*; static passes do **left-factoring**
  and **first-set dispatch**; **packrat memoization is placed SELECTIVELY by static analysis,
  not uniformly**; final compilation is a staged interpreter emitting a **stack-based bytecode
  VM** or specialized JVM code. Approaches hand-written speed. This is the direct ancestor of
  families 4A (analyzable graph) and 4C (VM). [parsley]
- **Scala-3 staged combinators (Moleike; and Béguet/Jonnalagedda "Accelerating Parser
  Combinators with Macros")** — a staged combinator via CPS + macros runs **~50% faster than
  a hand-written recursive-descent parser and >25× faster than the non-staged version**;
  control flow made explicit, continuations fully evaluated at compile time. Ancestor of 4B. [staged-scala]
- **"Obliteratingly Fast Parser Combinators" (Krishnaswami, 2021)** — defunctionalize the
  combinator into a first-order normal form + first/follow-set analysis so the dispatch is a
  character switch, not a closure chain; the simpler 2021 IR made "optimization invariants
  lexically apparent," dropping the complex staging pipeline. [oblit]

The through-line: **the analyzable representation is the enabling substrate (4A); fusion/
staging is the payoff (4B); the VM is one lowering target among two, measured against staged
codegen (4C); the public combinator API is preserved as sugar over the analyzed core (4D).**
These are not four rival architectures — the literature runs them as ONE pipeline
(construct → analyze → specialize → lower), which is a strong signal for the gestalt.

### The Rust field (parse-that's actual competitors)

- **winnow** (nom fork) — measured **the fastest Rust combinator library, <½ nom's parse
  time**; "toolbox" model, batteries-included, `&mut Input` cursor (not closure-returning).
  [winnow]
- **chumsky** — "framework" model; uses **GATs so the caller communicates to sub-parsers that
  a produced value can be DISCARDED** (the check-vs-emit split) — a compile-time fusion of the
  "do I need the value?" question directly relevant to 4B and to bbnf's own no-value-discard
  law. [winnow, lib.rs]
- **nom** — small core, function-combinators (`Fn(I)->IResult`), leans on external crates.

parse-that's Rust core (`SmallBox<dyn ParserFn+'a, S32>`, R3 §5) is the **closure/vtable**
model that ALL of winnow/chumsky/flap have moved away from — winnow to a cursor, flap/Parsley
to an analyzed graph. That is the pass-relevant tension: parse-that keeps the vtable core
(constitution i: "lawful hand-written primitives"), so beyond-SOTA here means *compiling*
those primitives (4A/4B/4C), not abandoning them.

---

## 2. FAMILY-BY-FAMILY: what won, what lost, what our evidence already fixes

### 4A CLOSURE-TO-GRAPH — analyzable nodes, not opaque nested closures

**External verdict: WON, and it is the prerequisite for everything else.** Parsley and
"obliteratingly fast" both make the combinator a reified AST; only then can first-set dispatch,
left-factoring, and *selective* packrat placement be graph analyses. chumsky's GAT
discardability is the same idea in the Rust type system.

**Our evidence base already has the seam.** R4 §6c names the TS engine's central megamorphic
IC — `this.parser(state)` flowing many closure shapes through one indirect call
(parser.ts:53,82,…; leaf.ts:44,49,62,140). R3 §5 names the Rust equivalent —
`SmallBox<dyn ParserFn>` vtable dispatch, plus the `span_parser/` sub-tree which is *already*
enum-dispatched, vtable-free (R3 §5 last ¶). **The `span_parser/` enum dispatch is a
half-built 4A inside parse-that's own tree** — a graph of tagged nodes rather than boxed
closures. That is the concrete anchor 4A must weave, not a greenfield.

**Constraint our base imposes:** constitution iii demands the graph shape be *isomorphic across
rust/ts* (4A says so explicitly), and LOCK-6 owns "reasonable point." An analyzable node enum
is trivially mirrorable (unlike closures); this is 4A's strongest cross-language argument.

### 4B COMPTIME-FUSION — the payoff lever (Rust monomorphized chains; TS build-time fuser)

**External verdict: WON, biggest measured wins in the class** (flap 2.3 cyc/B JSON; Scala
staged 25× over non-staged, 1.5× over hand-RD). This is First Futamura Projection applied to
parsing. It is also where the constitution bites hardest.

**Traps our evidence base already names — this is the load-bearing section:**

1. **The stable-port perf-claim refutation (SPEC §0/09, PROVE-09 VERIFY — LAW).** PROVE-09
   claimed 1.34-1.57× from the `portable_simd`→NEON shim; adversarial paired re-measure got
   **neutral-to-slower**; the 1.34× was two-arm **window asymmetry**. `otool -tv` disproved the
   *mechanism* (patched emitted **2271 `addv.8b` vs baseline 509** — the shim did MORE reduction
   work). **Lesson for 4B: no fusion/monomorphization perf claim survives without cold, paired-
   interleaved, N≥200, two-window measurement — and the mechanism must be verified at the
   instruction level, not asserted.** PT-2 §1.1 makes "faster" appear in *zero* gates; a 4B that
   claims speed must gate it exactly as PT-2.W1 does (insn-count ≤ baseline + paired ≥0.98×).
2. **Code-size / compile-time explosion (Parsley, Scala staged — named in the field).**
   "If a parser nests a few `|` and `~`, generated code grows exponentially until the JVM refuses
   to compile it." Our analogue: the **generated-size budget** + **extreme parsimony** edicts
   (constitution vi, MEMORY generated-size-budget), and rustc/LLVM compile time. R4 §6d shows
   parse-that already does *bounded* arity-unrolling (`any`/`fuseAll` arity-2/3 only, general
   `for`-arm beyond) — the field's warning is exactly why the unroll is capped at 3. **4B must
   inherit that cap discipline; a "fuse everything" chain is the named trap.**
3. **The Rust arena reward ceiling (PROVE-09 §Arena, PT-2.W4 — measured).** 4B says "ownership
   redesigned so the vtable/SmallBox question dissolves." The evidence: parse-that construction =
   **6 allocs / 5960 B**; parse twitter = **5390 allocs** (Vec growth in `many`/`sep_by` + string
   arena). **An arena over the 86 `Parser::new` vtable boxes moves only the 6, never the 5390 —
   high-risk (the `'a` input-lifetime unsoundness trap: a `'static` arena holding `'a` closures is
   unsound), low-reward.** PT-2.W4 is explicitly "sound OR refuted." So 4B's "dissolve the vtable"
   is *real work with a known-small payoff*; the actual alloc mass lives in the per-parse Vec
   growth, which is a `many`/`sep_by` codegen problem (pre-size + reuse), NOT a vtable problem.
   4B should target the 5390, not fixate on the 6.

**TS side of 4B (build-time fuser).** R4 §6d: arity-unroll already applied (value.js's 59
`all()` + hot 3-tuples). This IS a hand-written build-time fuse. Generalizing it = a code
generator emitting specialized `all`/`any` arms — the Scala-macro approach. The V8 constraint
(§below) is that the emitted arms must stay monomorphic.

### 4C VM-CORE — one bytecode instruction set shared rust/ts

**External verdict: WON on the JVM (Parsley), but with a NAMED, DECISIVE trap, and Parsley
itself moved PAST pure-VM to staged codegen.** Parsley compiles to a stack-based bytecode VM;
but the **JVM 64KB per-method bytecode limit** and code-size explosion forced strategic method
decomposition, and the *final* Parsley pipeline is a **staged interpreter** — i.e. the field's
own leading VM system uses staging (4B) as the terminal stage and the VM (4C) as the analyzable
middle. **This is direct evidence that 4C and 4B are not rivals but sequential: the VM is the
IR, staged codegen is the emit.** That collapses toward 4A (the graph) + 4B (the emit).

**Constraint our base imposes:** 4C's headline promise is "**layout isomorphism by
construction**" across rust/ts — a genuine, unique advantage under constitution iii/LOCK-6
(a bytecode buffer is trivially byte-identical across languages; PROVE-14's byte-exact tape
crossing is the existence proof that this works). **But** 4C explicitly says "measured against
the closure core, not assumed," and our own laws forbid warm/unpaired benches. The V8 cost of
a bytecode *interpreter loop* (a megamorphic switch over opcodes) is precisely the megamorphic-
dispatch trap V8 punishes at ~3.5× (§below) — a TS bytecode VM could REGRESS the very IC that
arity-unrolling was built to protect (R4 §6d). **4C on the TS side is the highest-variance
family; its win is isomorphism, its risk is re-megamorphizing the V8 hot loop.**

### 4D GRAMMAR-SUGAR — combinators as construction API over the shared HIR

**External verdict: this is the "toolbox vs framework" axis (winnow vs chumsky), and it is
where LOCK-4 meets LOCK-5.** 4D says combinators are a construction API over "the same HIR the
grammar plane uses (LOCK-5's continuum)" — "the combinator engine is the generator's runtime in
disguise." The field analogue: flap's typed combinators normalize into the *same* deterministic
NF the generated parser uses; there is one core, two front-ends (combinator API + grammar).

**Constraint our base imposes — the sharpest one:** constitution i says "**everything is
grammar-driven … save primitives and combinators**" — combinators are explicitly carved OUT of
the grammar-totality mandate as "lawful hand-written primitives." 4D's "combinators inherit the
optimizer / are the generator's runtime" must therefore preserve the **public parse-that API
unmodified** (SPEC §1: "consumed not duplicated"; PT-3 §1.1: "bbnf grows no combinator surface
and consumes this backbone unmodified"). value.js (3.1.0, 9 import sites), latex-paper (0.7.1
SKEW), keyframes.js are the do-not-break gates (R4 §7). **4D is an internal re-homing (combinator
API → shared HIR) that must be API-invisible to three consumers — a semver + isomorphism problem,
not a rewrite.** LOCK-6 adjudicates how far the HIR-sharing goes (6C WASM-core would make the
Rust HIR the one truth and TS a binding; 6A/6D keep TS idiomatic).

---

## 3. THE V8 / RUST DISPATCH TRAP (governs 4A/4B/4C on both sides)

External confirmation of R4's V8 census, and the single most-cited trap for TS combinators:

- **Megamorphic dispatch is a cliff, not a slope.** A 4-way (polymorphic) IC is ~1.4× slower
  than monomorphic; **>4 shapes (megamorphic) falls to a ~3.5× slower cache and TurboFan
  ABANDONS optimization for that site.** [v8-ic, ic-design-paper] parse-that's `this.parser(state)`
  is exactly a megamorphic site (R4 §6c: "many distinct closure shapes flowing through one
  indirect call"). **This is the governing constraint for the whole TS lock:** any family that
  ADDS shapes to that call (a richer node graph 4A, a VM opcode switch 4C) risks the cliff;
  4B's arity-unrolling exists to *remove* shapes from it.
- **The canonical fix is on record: monomorphic AST nodes.** The TypeScript compiler itself
  (microsoft/TypeScript #59198) made `Node` monomorphic — one shape with `kind/pos/end`, other
  data hung off a side object — precisely to cure megamorphic property access. [ts-mono] This is
  the template for a TS 4A: a **single node shape with a `kind` tag** (an enum-dispatched graph,
  mirroring parse-that's own vtable-free `span_parser/`), NOT a class-per-combinator hierarchy.
- **PROVE-11 CONFIRMED_DELTA is the honesty calibration.** The lazy-diagnostic-alloc lever is
  real (then-chain **+13.6%**, fused **+15.6%**, fused alloc **−60.1% exact**) — but the prover's
  **dispatch +10.2% did NOT reproduce; it lands ~+5%** across 24 rounds in two windows. **Trap:
  light-op lanes (20 ns ctor-dominated) sit in a wide/noisy ratio band; a single favorable
  window reads high.** PT-3 §1.3 carries dispatch at ~+5% everywhere and refuses to gate it ≥10%.
  Any 4B/4C TS number must be paired-in-one-process (PROVE-11's certified method), never a
  `vitest bench` cross-run (load-confounded: pristine loadavg 12 vs modified 27, PROVE-11 §3.1).

**Rust dispatch:** the mirror trap is `SmallBox<dyn ParserFn>` vtable indirection. flap/Parsley/
winnow all removed it (graph/cursor). 4A/4C would remove it for Rust too (enum tag → match, or
opcode → jump-table); 4B keeps closures but monomorphizes chains. All three are measured against
the current vtable core, cold/paired (PT-2.W1 protocol), or they do not land.

---

## 4. PACKRAT & DIAGNOSTICS (the two remaining charter clauses)

- **Packrat: selective placement WON (Parsley); blanket memoization LOST.** Parsley memoizes
  only nodes a static analysis shows benefit. parse-that already matches this: R4 §6g — packrat
  is **OFF by default, arms permanently at first `memoize()`** (`PACKRAT_ARMED`, packrat.ts:156/
  290), default LL(1) pays nothing; the Rust side has a packrat epoch too (R4 head: "934b2fa S.H1
  packrat epoch"). **The 4A analyzable graph is the natural home for "which nodes to memoize" —
  it becomes a graph analysis exactly as in Parsley, rather than the user hand-calling
  `memoize()`.** Warth left-recursion (the standard citation) adds real per-call overhead and its
  indirect-LR form is complex; experiments show super-linear blowup is rare in practice but the
  cost is genuine — so selective, analysis-driven placement (not blanket packrat) is the settled
  answer. [warth, parsley]
- **Diagnostics: per-parse scope is the field norm; global latches are the anti-pattern.**
  R4 §5 + PT-3.W2: parse-that's diagnostics are gated behind a **module-global** `diagnosticsEnabled`
  (utils.ts:6) + a module-global collected buffer (utils.ts:138-146) — two concurrent/nested
  parses cross-contaminate. PT-E1 makes it per-parse scoped (hung off the parse epoch, like the
  packrat epoch). PROVE-11 already made `suggestions`/`secondarySpans` **lazy** (undefined at
  birth, fixed hidden class) — the substrate PT-E1 consumes. **The diagnostics-point placement
  is a 4A graph property** (every alternative/label node is a diagnostic site), unifying with the
  first-set analysis — chumsky's error-recovery framework is the comparable system, and its lesson
  is that recovery *strategies* must be first-class graph nodes, not bolted-on.

---

## 5. CONSTRAINTS THE EVIDENCE BASE IMPOSES (the binding set for every LOCK-4 pass)

1. **No stable-port / fusion perf claim without cold-paired-N≥200-two-window + instruction-level
   mechanism proof** (SPEC §0/09, PROVE-09 VERIFY; the 2271-vs-509 counterexample). Window
   asymmetry is the classic self-deception.
2. **Light-op lanes over-read; carry the honest number** (PROVE-11 VERIFY: dispatch +10.2%→~+5%).
   Paired-in-one-process, never `vitest bench` cross-run.
3. **Megamorphic-dispatch cliff (>4 shapes ≈ 3.5×, TurboFan bails)** governs every TS family;
   monomorphic tagged-node graph (TS-compiler #59198 template) is the on-record cure; do not
   regress the arity-unroll (R4 §6d).
4. **Arena reward ceiling: 6 construction allocs, not the 5390 per-parse** (PROVE-09 §Arena).
   The `'a`-input-lifetime unsoundness trap; sound-or-refuted (PT-2.W4). Real alloc mass is
   `many`/`sep_by` Vec growth + string arena — a codegen target, not a vtable target.
5. **Generated-size / compile-time budget + extreme parsimony** (constitution vi; the field's
   code-size-explosion trap, Parsley/Scala). Bounded unrolling (cap ≈3) is deliberate.
6. **Combinators are carved OUT of grammar-totality** (constitution i) — they stay lawful
   hand-written primitives; 4D re-homes them over the HIR but keeps the public API invisible to
   value.js / latex-paper (0.7.1 SKEW) / keyframes.js (R4 §7; PT-3 W0/W5 consumer gates).
7. **rust/ts isomorphism to a reasonable point, MEASURED** (constitution iii; LOCK-6 decides).
   4A's tagged-node graph and 4C's bytecode buffer are the two families whose layout is trivially
   mirrorable; closures (current core) are not. PROVE-14 proves byte-exact crossing is achievable.
8. **Non-clobber / placement law**: parse-that is READ-ONLY to bbnf agents; work is scratch-copy
   only (R3 §8 recipe), lands via letter BBNF-PT-G by owner/parse-that-side session (PT-2/PT-3 §1.4).
9. **x86 dead, warm benches disinformation, type plane zero runtime cost** (constitution iv/v).

---

## 6. THE FIELD'S GESTALT SIGNAL (for the eventual weave)

The single most important external finding: **flap, Parsley, and the Scala staged line all run
ONE pipeline — reify (4A) → analyze/normalize → specialize/stage (4B) → optionally via a VM IR
(4C) — under one public combinator API (4D).** They are not four architectures to choose among;
they are four *stages* of the one architecture the field converged on. Parsley's own evolution
(pure VM → staged final interpreter) is the proof that 4C is the analyzable middle and 4B the
terminal emit. **The pass-relevant question for LOCK-4 is therefore not "which family wins" but
"where does parse-that's existing seam (`span_parser/` enum dispatch, arity-unroll, packrat/
diagnostics epochs) already sit on that pipeline, and how far along it does the constitution let
us push before the API/consumer/isomorphism gates bind."** flap's 2.3 cyc/B JSON is the external
ceiling the fused path aims at; parse-that's own JSON tape already claims 2.2× sonic (SPEC §1) on
the *generated* side — the combinator side is the gap flap shows is closable.

---

## Sources

- [flap] flap: A Deterministic Parser with Fused Lexing, Yallop/Dolan/Kammar/Krishnaswami, PLDI/OOPSLA 2023. https://arxiv.org/abs/2304.05276 · https://www.cl.cam.ac.uk/~nk480/flap.pdf
- [parsley] Staged Selective Parser Combinators, Willis/Wu/Pickering, ICFP 2020. https://mpickering.github.io/papers/parsley-icfp.pdf · https://dl.acm.org/doi/10.1145/3409002 · https://github.com/j-mie6/parsley
- [staged-scala] Staged Parser Combinators in Scala. https://moleike.github.io/blog/staged-parser-combinators/ · Accelerating Parser Combinators with Macros (Béguet/Jonnalagedda), https://dl.acm.org/doi/10.1145/2714064.2660241
- [oblit] Obliteratingly Fast Parser Combinators, Krishnaswami, 2021. https://semantic-domain.blogspot.com/2021/12/obliteratingly-fast-parser-combinators.html
- [winnow] Winnow 0.5: The Fastest Rust Parser-Combinator Library? https://epage.github.io/blog/2023/07/winnow-0-5-the-fastest-rust-parser-combinator-library/ · https://docs.rs/winnow/latest/winnow/_topic/why/index.html
- [lib.rs] Parser tooling — Rust crates. https://lib.rs/parsing
- [v8-ic] The V8 Engine: Inline Caching. https://braineanear.medium.com/the-v8-engine-series-iii-inline-caching-unlocking-javascript-performance-51cf09a64cc3
- [ic-design-paper] An Inline Caching Design for TypeScript Performance, OOPSLA 2020. https://dl.acm.org/doi/pdf/10.1145/3428242
- [ts-mono] Performance: Monomorphic AST Nodes, microsoft/TypeScript #59198. https://github.com/microsoft/TypeScript/issues/59198
- [warth] Packrat Parsers Can Support Left Recursion, Warth/Douglass/Millstein, PEPM 2008. https://web.cs.ucla.edu/~todd/research/pepm08.pdf
- Internal: SPEC.md §0-§1, PORTFOLIO.md §18-34 (Constitution), R3-parse-that-rust.md, R4-parse-that-ts.md, PROVE-09 REPORT+VERIFY, PROVE-11 REPORT+VERIFY, PT-2.md, PT-3.md.
