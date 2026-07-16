# LOCK-4 · THE COMBINATOR CORE — PASS-3 PRIOR-ART RESEARCH

Scope: the combinator algebra at beyond-SOTA. Pass-3 is a **delta over pass-2** (which mapped
the four families onto the reify-and-stage / monomorphize / parsing-VM / grammar-sugar literature
split and banked the measured constraints). Pass-3 sweeps prior art for the SPECIFIC mechanisms
the pass-3 directives interrogate: runtime→comptime staging (Q1), host-closure descriptors (Q2/Q3),
fusion/blowup budgets (Q4), one-plan-two-lowerings + isomorphism-of-record (Q5/Q6), and per-parse
setup amortization (Q7). No family carries a verdict. Every citation is an external URL or a
verified local `path:line`. No builds run.

---

## 0. THE BINDING CONSTRAINTS CARRIED FORWARD (do not re-litigate — pass-2 §1)

Restated by pointer so a pass-3 design cannot silently reopen them:
- **Runtime-`switch` VM is DEAD on V8, measured in THIS codebase** (SpanParser −10..14%, killed;
  `parse-that/docs/future-research.md:71-109`). Only a *build-time-specialized* decoder survives.
- **De-nightly/arena shim perf claim REFUTED and LAW** (SPEC §0-09; `PROVE-09/VERIFY.md`).
  Paired A,B,A,B interleaving mandatory even for "obvious" wins.
- **Arena reward ceiling is 6 allocs of 5390** (construction vs per-parse Vec/string mass) —
  vtable arena aims at 0.1% of the mass; soundness-blocked (`'a`-closure lifetime), sound-or-refuted.
- **Lazy diagnostics = CONFIRMED_DELTA** (fused −60.1% alloc exact; dispatch honest **~+5%**, not
  +10%; `PROVE-11/VERIFY.md`). Over-claiming trips the W6 audit.
- **Constitutional floors:** combinators stay lawful hand-written primitives; type plane costs zero
  at runtime; no god modules / name-gates; concrete-artifact-or-SPEC-ONLY; consumer-less substrate
  forbidden (Y.13); rust/ts isomorphism only *to a measured reasonable point* (LOCK-6 owns it).
- **Settled substrate (DIRECTIVES):** node set is a 14-variant near-bijection with `bbnf_ir::IrNode`
  carrying fusion passes + a path-dep-**consumed** e-graph; a comptime-monomorphization mechanism
  (Rust) and a build-time-emitter mechanism (TS) each exist as **two distinct mechanisms** — infer
  no election from either; the regex functor is **retired** (XL-C: two domain-fit types + cheap
  bridge); the TS fast face is the build-time emitter (XL-D).

---

## 1. Q1 — RUNTIME GRAPH → COMPTIME MONOMORPHIZATION: the staging literature is the answer

The directive asks whether the runtime combinator API (it builds a runtime `IrNode`) reaches
comptime type-monomorphization at all, or only via a build-time codegen step over a comptime-known
graph. **The canonical prior art answers decisively: NO runtime combinator value reaches
monomorphization without a staging/codegen step, and the payoff of adding that step is quantified.**

- **Staged Parser Combinators (Jonnalagedda, Sarkar, Rompf; LMS; ACM SIGPLAN 2014/2015)** —
  staging "dissociates input parsing from parser composition, and eliminates intermediate data
  structures and computations associated with parser composition at staging time." Combinators are
  **turned into parser generators**; the measured result: a staged parser runs **~50% faster than a
  hand-written recursive-descent parser and >25× faster than the non-staged version**
  ([dl.acm.org/10.1145/2714064.2660241](https://dl.acm.org/doi/10.1145/2714064.2660241);
  [manojo.github.io/2015/09/02/staged-parser-combinators](http://manojo.github.io/2015/09/02/staged-parser-combinators)).
  LMS "creates an intermediate representation of the staged code" (the LMS graph IR) and guarantees
  the composition abstractions are removed at staging — "abstraction without regret"
  ([cacm.acm.org lightweight-modular-staging](https://cacm.acm.org/research/lightweight-modular-staging/)).
- **Mechanism trace this imposes on the API (the directive's Q1 evidence ask):** the combinator API
  must become a **two-stage program** — its composition happens in the generator stage; only the
  residual (specialized recognizer) survives to runtime. In LMS this is the `Rep[T]` staging-type
  discipline; in Rust it is `const`/codegen over a comptime-known graph; the runtime `IrNode` a
  combinator builds is a **stage-0 value**, and monomorphization is reachable ONLY after a
  reflection/codegen lowering reads that graph at build time. This is exactly LOCK-3's **3D
  STAGED-RESIDUAL** shape (comptime/runtime split in the types; residualization = erasure of stage-0)
  and PROVE-12's const-fold generalized. **Prior-art verdict on Q1:** the runtime API does NOT reach
  comptime monomorphization directly; a build-time codegen step over the comptime-known graph is the
  only lawful route, and staged-combinator literature is the proof + the payoff figure.

---

## 2. Q2/Q3 — HOST-CLOSURE SURFACE + OPAQUE BARRIER: this is the defunctionalization problem

`FnDescriptor` has no `HostClosure` variant (code-verified gap). The directive asks what a
serializable descriptor must carry and how large the `Opaque(HostFnId)` analysis barrier is. **The
exact prior art is defunctionalization** — the standard transform for turning arbitrary host
closures into a first-order, serializable, analyzable representation.

- **Defunctionalization (Reynolds 1972;
  [en.wikipedia.org/wiki/Defunctionalization](https://en.wikipedia.org/wiki/Defunctionalization)).**
  A program contains finitely many function abstractions; each is assigned a unique tag; the function
  space becomes a **sum type** (one summand per abstraction, carrying its captured environment);
  every application becomes a call to one global `apply` that dispatches on the tag. → A `->` host
  closure is serializable as `(tag, captured-env)` **iff** its abstraction is enumerable at build
  time. This is precisely the typed shape a `HostClosure` descriptor must carry: a closed tag + a
  typed captured-environment record.
- **Better Defunctionalization through Lambda Set Specialization (PACMPL/OOPSLA 2023; the Roc
  compiler)** — the modern refinement: enumerate the **lambda set** flowing to each call site and
  specialize the dispatcher per site, so the first-order program is "greatly amenable to classical
  optimizations"
  ([dl.acm.org/10.1145/3591260](https://dl.acm.org/doi/10.1145/3591260);
  [dspace.mit.edu/…/3591260.pdf](https://dspace.mit.edu/bitstream/handle/1721.1/150980/3591260.pdf)).
  Roc's own open problem — defunctionalization "in the presence of a specializing compiler"
  ([github.com/roc-lang/roc/issues/5969](https://github.com/roc-lang/roc/issues/5969)) — is the same
  Rust-monomorphization interaction LOCK-4 faces.
- **The Q3 barrier-ratio consequence (prior-art framing):** a defunctionalized closure is **NOT an
  opaque barrier** — it is an analyzable tag an analysis sees through. The irreducible
  `Opaque(HostFnId)` barrier is exactly the set of closures whose **lambda set is unbounded or
  escapes** — value-dependent continuations that capture arbitrary host values (`chain`/`mapState`,
  the monadic core; R4 §2 `parser.ts:124-187`). So the ≤2% barrier floor is a claim about how few
  `->` sites are genuinely non-enumerable; defunctionalization theory says everything else reduces to
  a tag. **The census the directive demands (per-corpus non-projection counts) is a lambda-set
  enumerability audit**, not a raw closure count.
- **Selective functors are the priced escape for the value-dependent middle** (Parsley; Willis, Wu,
  Pickering, ICFP 2020) — `branch`/`select` (`(<*?) = select`) recover conditionals **analyzably**
  where monadic bind would defeat static structure; "statically analysed (by reporting all possible
  effects) and efficiently executed (by skipping unnecessary effects)"
  ([mpickering.github.io/papers/parsley-icfp.pdf](https://mpickering.github.io/papers/parsley-icfp.pdf);
  [dl.acm.org/10.1145/3409002](https://dl.acm.org/doi/10.1145/3409002)). **Trap (carried, sharpened):**
  every `->` that stays monadic is a barrier; the design choice is which sites to force into a
  selective/defunctionalized shape (analyzable, serializable) vs admit as typed `HostClosure`
  descriptors behind LOCK-7's host-fn lattice.

---

## 3. Q4 — FUSION-POLICY / BLOWUP BUDGET: monomorphization bloat is a documented, budgeted cost

The pluggable {inline-monomorphize | enum-node | box-here} cost model needs named caps (fuse-arity,
dispatch-fanout, boxing-depth, `.rlib`/`dist` + incremental-compile budget). **Prior art establishes
the budget is real and names the standard levers — the caps are not invented, they are the
literature's mitigations parameterized.**

- **Monomorphization code bloat is the well-documented dark side of Rust zero-cost generics:** each
  instantiation emits fresh machine code; "heavy use of generics is one of the primary reasons large
  Rust projects take a long time to compile"; binary size grows with the instantiation count
  ([medium.com The Dark Side of Monomorphization](https://medium.com/@theopinionatedev/the-dark-side-of-monomorphization-9999b97732d4);
  [rustfaq.org monomorphization](https://www.rustfaq.org/en/what-is-monomorphization-and-how-does-it-affect-performance/)).
- **The named mitigations MAP ONTO the three cost-model choices:** (a) **limit the number of
  instantiations** = the fuse-arity / dispatch-fanout cap (beyond it, don't monomorphize);
  (b) **specialization** = the enum-node route for a bounded closed set; (c) **LTO dedup** merges
  duplicate `Vec<i32>`-style instantiations across crates — the boxing-depth fallback's linker-side
  recovery. rustc's **explicit-monomorphization / polymorphization** discussions are the upstream
  precedent for capping instantiation to reduce compile time
  ([internals.rust-lang.org explicit-monomorphization](https://internals.rust-lang.org/t/explicit-monomorphization-for-compilation-time-reduction/15907)).
- **The directive's grounding surface (value.js 59 `all()` + hot 3-tuples, R4 §6d):** the budget must
  be measured against the FULL consumer surface, not JSON alone — the arity-unrolled arms already
  specialize arity-2/3 (`leaf.ts:39-57,183-251`) and STOP (general `for`-arm beyond), which is
  precisely the "cap the fanout, fall back to a general arm" policy the literature prescribes. The
  pass-3 number owed: measured `dist`/`.rlib` growth vs fuse arity, so the caps are fitted, not
  asserted — the Constitution's "generated-size budget" instantiated for combinators.

---

## 4. Q5/Q6 — ONE PLAN, TWO LOWERINGS + ISOMORPHISM-OF-RECORD: retargetable IR + differential oracle

The directive asks whether a fused region is derivable from ONE serializable plan that drives both
the Rust lowering and the TS emitter (⇒ one plan, two lowerings, byte-identical parses), and how to
define isomorphism-of-record evidence (opcode-stream identity vs a **live** Rust golden AND
`Value`-tree identity vs the Rust interpreter). **Both are established patterns.**

- **One serializable IR → multiple backends is the retargetable-IR pattern.** MLIR is the canonical
  modern statement: a "structured and retargetable approach" where one IR lowers to multiple targets
  via composable passes
  ([arxiv.org/pdf/2202.03293](https://arxiv.org/pdf/2202.03293)); the general principle — "a back-end
  for a given target accepts a string in the IR language and produces a program in the target
  language, and in principle back-ends for any language could be implemented." parse-that already has
  the substrate: `bbnf_ir::IrNode` is serializable (MessagePack bus, pass-2 §1) — the plan exists; the
  open question is whether the TS emitter consumes THAT plan or a re-derived one. **The distinguishing
  observable the directive wants (one plan driving both lowerings to identical output) is exactly a
  differential-testing setup**, below.
- **The isomorphism-of-record dual oracle IS differential testing against a live golden** — and
  recent systems build precisely this shape:
  - **CrossLangFuzzer (2026)** — "introduces a unified IR to decouple test generation from
    language-specific syntax… serializes the generated program into the unified IR… behavioral or
    **bytecode mismatches** indicate bugs"
    ([arxiv.org/html/2606.28132](https://arxiv.org/html/2606.28132v1)). This is the **opcode-stream
    identity** oracle, serialized-IR form.
  - **CLIR (Cranelift, 2026)** — an **intersection-based IR for differential testing across all
    backends** ([arxiv.org/html/2606.26977](https://arxiv.org/html/2606.26977)) — the multi-backend
    (Rust/TS) parity oracle, generalized.
  - **The oracle-problem lesson the directive already encodes:** "differential testing bypasses the
    oracle problem… by running on multiple compilers and identifying defects when they yield differing
    results." → Q6's insistence on a **live Rust golden, not a hand-faked blob**, is the exact
    discipline these systems enforce: the golden must be produced by running the real Rust lowering,
    and the TS decode is checked against it (opcode-stream) AND the `Value`-tree checked against the
    Rust interpreter. Two oracles, one corpus — the CrossLangFuzzer/CLIR posture.
- **Q6 report-only (internal-descriptor evaluator home):** the Hex/Number/Enum descriptor evaluator
  is a dispatch over a closed tag set — defunctionalization's global `apply` (§2). Prior art leans it
  toward an **IR-VM lowering (LOCK-3)** home (a decode-and-dispatch facility co-located with the
  already-live `bbnf_ir` VM `vm/{bytecode,compiler,interpreter}`) rather than scattered LOCK-7 host
  dispatch — but this is a cross-lock disposition to REPORT, not settle here (Constitution viii: a
  second consumer-less VM is forbidden, so any new evaluator must ride the extant one).

---

## 5. Q7 — PER-PARSE SETUP TAX: object-pool / arena-per-request reset is the amortization

`Interpreter::new` (8 Vecs + FxHashMap) per parse is a classic **per-invocation setup tax**. Prior
art names the mechanism and its shape exactly.

- **Object pool with reset-on-return** — the standard pattern: `allocate`, `reset` (return object to
  initial state), `is_valid`; thread-local pools backed by a global to minimize mutex contention
  (the RecycleContainer pattern)
  ([github.com/fereidani/opool](https://github.com/fereidani/opool)).
- **Arena-per-request / arena-per-parser-pass** — "each execution call checks out a clean arena, runs
  a closure, resets it at block exit, and returns the arena to the pool — useful for request
  handlers, worker jobs, **parser passes**"; "when parsing multiple documents individually using
  arenas, the same memory is reused for each document, and overall memory usage never exceeds the
  largest tree size"
  ([medium.com Arena Allocators](https://medium.com/@sgn00/high-performance-memory-management-arena-allocators-c685c81ee338);
  [protobuf.dev C++ Arenas](https://protobuf.dev/reference/cpp/arenas/)). → The `Interpreter`'s 8 Vecs
  + map become a pooled scratch reset (`.clear()` keeps capacity) between parses, not re-`new`'d.
- **The Q7 measurement owed:** the setup-allocation share of single-parse cost and the amortization
  curve as the pool warms. Prior art frames the win as **capacity-preserving reset** (the Vecs keep
  their backing store; only length resets to 0) — a `clear()`-and-reuse, NOT a free, so the clean
  split the directive asks for is: recognizer cost (kernels) / value-plane cost (`->` descriptors) /
  setup cost (the 8-Vec + map churn the pool removes). This dovetails with the arena reward-ceiling
  fact: the per-parse mass is Vec growth (5390 allocs), and pooling the setup Vecs attacks *that*
  mass — the opposite end from the refuted vtable arena.

---

## 6. Q8 — PREDECESSOR SHAPES (report only, per directive)

- **LOCK-5 (regex substrate):** XL-C has RETIRED the ONE-HIR/functor question — the settled shape is
  **two domain-fit types** (opaque `Regex(StringId)` grammar tier; rich `Hir` engine tier) with a
  cheap bridge; the shared-IR question no longer hinges on a single type. Open sub-question feeding
  LOCK-4: none blocking — the combinator engine consumes the bridge, not the HIR. (Do not settle.)
- **LOCK-7 (host-fn lattice):** the typed `HostClosure` descriptor and the ≤2% opaque barrier
  (§2/Q2/Q3) live behind LOCK-7's host-fn lattice — a TYPED interface bounding totality with a named
  floor per subsystem (PORTFOLIO 7C). LOCK-4's defunctionalization census (which `->` sites are
  lambda-set-enumerable vs genuine `HostClosure`) is the INPUT to that lattice; the lattice's shape
  is LOCK-7's to fix. (Do not settle.)

---

## 7. TRAPS NAMED IN COMPARABLE SYSTEMS (pass-3 additions to pass-2 §3)

Carried from pass-2 (window-asymmetry; Rust↔V8 law inversion; vtable-aimed allocation; monad tax;
full-packrat over-memoization; naive VM interpretation; generated-size blowup; unsound lifetime
smuggling). **New for pass-3:**

9. **Staging leak — a "runtime graph" that never gets staged pays the composition tax forever.** The
   staged-combinator result is contingent on the codegen step actually firing; a design that builds
   the `IrNode` graph at runtime and *interprets* it (no build-time residual) reproduces the >25×
   non-staged penalty. The residual must be emitted, not walked (Q1).
10. **Defunctionalization escape — an unbounded lambda set defeats the first-order transform.** If a
    `->` closure's captured set can't be enumerated at build time, it stays a genuine `Opaque` barrier;
    forcing it into a tag anyway (a catch-all "generic closure" summand) collapses the analysis back to
    a megamorphic `apply` — the very indirection defunctionalization was meant to remove (Q2/Q3).
11. **Oracle self-forgery — a hand-faked golden blob is not isomorphism-of-record.** CrossLangFuzzer/
    CLIR require the golden be produced by the REAL other-backend run; a checked-in synthetic opcode
    stream proves nothing about the live Rust lowering (Q6, the directive's explicit "not a hand-faked
    blob").
12. **Pool aliasing / stale-state bleed — a reset that forgets a field cross-contaminates parses.**
    The object-pool `reset` must be total (mirrors the PT-3 W2 per-parse-scope isolation oracle for
    diagnostics globals); a partial reset is the setup-pooling analog of the module-global latch bug
    (Q7).

---

## 8. THE DECISIVE UNBUILT NUMBER (carried, now with prior-art expectations attached)

Pass-2's two keystones stand: (a) the **common-baseline runtime fork** (graph/bytecode/monomorphized/
closure over one grammar, one leaf substrate) and (b) the **first cross-language TS paired number**.
Pass-3's prior art now says *where the numbers should land*: staged combinators predict a large
build-time-residual win over any interpreted graph (Q1); defunctionalization + selective functors
predict the barrier ratio is a lambda-set-enumerability property, expected small (Q2/Q3);
monomorphization-bloat literature predicts a real `dist`/`.rlib` cost that caps fuse-arity (Q4);
retargetable-IR + differential-testing practice validates one-plan-two-lowerings as buildable and
names the dual-oracle discipline (Q5/Q6); object-pool/arena-per-parse predicts a capacity-preserving
setup-reset win against the true per-parse Vec mass (Q7). The numbers themselves remain owed — but
every pass-3 directive now has a prior-art anchor for its expected result and its named failure mode.
