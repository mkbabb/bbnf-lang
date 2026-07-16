# LOCK-4 — THE COMBINATOR CORE · pass-1 · ADVERSARIAL CRITIC 2 of 2

*Critic 2, cross-lock-weighted. Read: PORTFOLIO.md (LOCK-4 charter + CONSTITUTION), FAMILY-SPECS.md,
research-{A,B,C,D,priorart}, PROTO-1 (+patch, +the running scratch tree), PROTO-2. Substrate claims
spot-checked against live code (bbnf_ir `vm/`, in-tree `egraph`/`egraph-derive`, parse-that
`span_parser/`, and the pt-scratch prototype). Charter is applied verbatim; convergence is EARNED, and
this is pass 1 — no family is near converged, and the two prototypes that ran do not touch the two
decisions that actually pick the terminal runtime.*

---

## 0. THE HEADLINE FINDING (weighted for the pass-2 hinge)

**Two prototypes ran and both are genuine, refutable, concrete artifacts (constitution vii satisfied).
Neither resolves the family selection.** The synthesis frames Brief 1 (value-plane) and Brief 2
(chain-census) as "the two numbers pass-2 cannot pick without" (FAMILY-SPECS §5, §6). That framing is
the **elegant-reduction trap**: the two briefs clear *preconditions* (single-tier is Rust-viable; the
monad is not pervasive), then the load-bearing "pick the terminal runtime on Q-runtime × Q-ownership"
step reads *and-then-pick* — but the pick requires three numbers that were **not** produced this pass:

1. **The 4B inlining number** (does removing the `SmallBox` indirect call buy anything). FAMILY-SPECS
   §6 explicitly *defers* it ("yields to the two cross-family cruxes"). This is the one measurement that
   decides whether the terminal runtime is "none / monomorphized" (4B) vs "a runtime representation"
   (4A/4C/4D). It is the axis on which 4A vs 4B — the top incompatibility (§5) — turns, and it is unrun.
2. **The TS-execution number** for 4C/4D (and the TS graph-interpreter number for 4A-P4). Every
   prototype ran Rust-only. PROTO-1's verdict text says "4A/4C/4D single-tier VIABLE" **unqualified**;
   it is Rust-only viable. The constitution's clause iii (rust/ts iso, non-negotiable) is untouched by
   any artifact this pass.
3. **LOCK-5's substrate verdict and LOCK-6's iso verdict** — both external, both load-bearing for the
   pick (see §6), neither available.

So the honest pass-1 state is: **the two prototypes de-risked the shared analyzability crux (good, real
progress), but the family that becomes THE combinator core cannot be named**, because the 4A-vs-4B
number is deferred and the TS half is unmeasured. Pass-2 must not read the two green briefs as a
mandate to pick; it inherits an *unconverged* selection with the deciding numbers still open.

---

## 1. FAMILY 4A — CLOSURE-TO-GRAPH

**Convergence: 55%.** Strongest prototype evidence in the lock; but its two *novel* claims (passes;
rust/ts iso) have zero running evidence, and PROTO-1 does not test the comparison 4A actually has to win.

### What holds
- Substrate is real and consumed (verified: `span_parser/` is 10 files with live JSON/CSS consumers) —
  Y.13 satisfied by construction, not asserted.
- PROTO-1 is a genuine, paired, cold, two-window, N=100 artifact that RAN and could have failed (canada
  floor 0.986× is a real refutable margin). The value-plane parity result on Rust is earned.

### Exact open gaps
1. **The pass architecture — 4A's ONE novel claim — is untested (verified defect in the evidence).**
   4A's charter is "cleverness that today is smart-constructors becomes passes, each a total
   `Graph→Graph`" (FAMILY-SPECS §1). PROTO-1's `json_graph.rs` **hand-writes the fully-fused arena**
   (`let nodes = vec![JsonNode::Dispatch{…}, JsonNode::Object{…}, …]`, line 163) — there is **no `fuse`
   pass, no SCC packrat placement, no dispatch synthesis**. The keystone gate P1 ("`fuse(Graph)→Graph`
   produces a byte-identical optimized graph to the hand-fused constructors") was **not run**. PROTO-1
   proves a hand-optimized arena runs at parity; it proves nothing about whether the fusion is
   *separable, iterable-to-fixpoint, and byte-identical as a pass*. The entire "fusion-as-a-pass" thesis
   is SPEC-ONLY. This is prototype-evidence misuse: the running artifact is credited to a claim it does
   not exercise.
2. **PROTO-1 measures the wrong baseline for the 4A-vs-4B decision.** The proto compares the graph
   against the *current* closure core (`SmallBox` + `lazy` `UnsafeCell` + `dispatch_byte_multi` Vec
   indirection) — and PROTO-1 §"Why the graph wins" concedes the win comes from *retiring those known
   inefficiencies*, not from analyzability. But the terminal-runtime decision (§5 axis Q-runtime, 4A vs
   4B) is graph-vs-**monomorphized-4B-chain**, which PROTO-1 never built. A graph that beats the old
   `SmallBox` core says nothing about whether it beats a `Then<A,B>`/`Map<A,F>` monomorphized chain. The
   number that decides 4A's terminal viability against its true rival is absent.
3. **TS is entirely unmeasured (gate P4 unrun).** The rust/ts-iso claim — "optimized graph node-for-node
   identical across languages" — has no artifact. R4's own finding (dispatch-LUT under-delivered ~+5% vs
   +10% claimed) is a live warning that a `switch(node.tag)` interpreter may go megamorphic on V8; the
   synthesis flags it (open-question d) but nothing tests it. The constitution-iii proof is 0% built.
4. **Recursion-API break is designed-around, not measured.** The `lazy()`→`RuleId` auto-mint via cache
   identity (R3 guard) is asserted as an "observable-sharing fallback"; no consumer grammar with
   anonymous mutual recursion was run through it to prove the public API survives. This is a *masked
   fallback* until exercised — the guard's cost ("measure the identity-recovery cost", research-A R3) is
   unpaid.
5. **Cross-lock double-home with LOCK-2 (unnamed by the synthesis).** 4A proposes to "mirror
   `rust/regex/src/egraph/`" for its fusion pass — i.e. stand up a *second* equality-saturation rewrite
   system private to parse-that's combinator graph. LOCK-2's charter is ONE graph-engine home; the
   standing memory `general-infra-crates` ("e-graphs in own crate, not stuffed into domain crates") and
   `system-cohesion` ("fold into existing systems; no orthogonal subsystems") both cut against a
   parse-that-private fusion e-graph. 4A's fusion passes must be adjudicated as consumers of LOCK-2's
   engine, not a private clone — this belongs in the gaps, not deferred to gestalt.

---

## 2. FAMILY 4B — COMPTIME-FUSION

**Convergence: 35%.** No running prototype; the one measurement that decides the family was explicitly
deferred; the honest prior is a possible null. Disposition "ADVANCE-candidate, REWARD-AT-RISK" is the
right label but the % must reflect that its charter value is *unmeasured on the axis it names*.

### Exact open gaps
1. **The deciding number is unrun and the charter targets an axis never measured.** 4B's charter value
   is "the vtable/SmallBox question dissolves." Priorart §4 (grounding on PROVE-09/PT-2) establishes the
   arena/alloc axis is low-reward (6 vs 5390 allocs) — so 4B pivots to a *different* axis, the
   indirect-call/inlining barrier, which "PT-2/PROVE-09 never measured" (research-B §7). That pivot is
   honest, but it means 4B advances on an axis with **zero internal evidence in either direction**, and
   the prototype that would produce it (generic-struct JSON parser + `cargo asm` + paired bench) was
   **deferred this pass** (FAMILY-SPECS §6). A family whose entire thesis rests on an unmeasured axis,
   with its deciding experiment postponed, is not 50%+ converged.
2. **The honest prior is NEUTRAL and the family may correctly close on a refutation.** The `SmallBox`
   call is a single fixed-target branch the Apple-M BTB predicts perfectly on the steady loop (research-B
   R1) — the exact PROVE-09 shape (an "obvious" win that evaporated under paired interleaving). 4B's own
   spec concedes "a filed refutation is a valid family outcome." Correct — but this means the family's
   *expected* pass-2 outcome may be "does not advance as a terminal runtime," and the % should encode
   that, not the elegance of the mechanism.
3. **PROVE-11 says the TS win is allocation-shaped, not dispatch-shaped — the TS half may be a
   ~+5% ceiling.** The certified delta (dispatch lane ~+5%, the double-digits from removed tuple allocs)
   means a 4B-TS fuser that isolates dispatch-elimination reproduces only the small number. The gate
   ("isolate dispatch-elimination from allocation-elimination") is correct but unrun; until it runs,
   "beyond-SOTA" for 4B-TS is unsupported.
4. **Self-contradiction in the spec text (spec-cites-itself / masked fallback).** The architecture line
   asserts "**No runtime graph at all** (the axis that separates 4B from 4A/4C)"; open-question (d) then
   concedes "'no runtime graph at all' is false for dynamic grammars… the runtime interpreter graph
   remains the sound fallback." The headline separator and the fallback are in direct tension. 4B is
   *fused-sites-only + a retained interpreter graph* — which is a 4A/4C runtime under another name for
   the unfused remainder. The claim must be scoped in the disposition, not just the footnote, or the
   Q-runtime incompatibility map (§5, "4B eliminates any runtime program") is false as stated.
5. **Generated-size budget is a named gate but the budget is not quantified.** R3 (blowup) and the
   `generated-size-budget` memory bind a per-tranche line-count ceiling; 4B names it but no number, no
   `.boxed()` depth-cap policy value, is given. A gate with no threshold is a gate that cannot fail.

---

## 3. FAMILY 4C — VM-CORE

**Convergence: 30%.** Mechanism substantiated (LPeg proven-correct, seed in-tree); but throughput is a
"loss-to-be-recovered" with no artifact, and the family **double-homes a bytecode VM against bbnf_ir's
existing `vm/`** — a cross-lock contradiction the synthesis under-weights.

### Exact open gaps
1. **Throughput is an open measurement whose honest prior is a loss — and nothing ran.** Every gate in
   research-C §6 ("Span-tier VM vs SpanKind tree-walker", "threaded-code recovery", "value-tier honesty
   check") is unrun. The honest prior (0.7–0.9× on twitter/citm/CSS; value-tier possibly 0.6×) is a
   *loss to recover*, not a win. Advance-candidate is defensible only as "isomorphism + facilities"; the
   % must not borrow speed the paired bench has not certified (the §0/09 LAW applied to 4C's own claims).
2. **CROSS-LOCK DOUBLE-HOME (verified, and the strongest cross-lock finding in the lock).** 4C proposes
   a *new parse-that-private* bytecode instruction set + two interpreters. **bbnf_ir already ships a
   bytecode VM** — verified live: `crates/ir/src/vm/{bytecode.rs, compiler/, interpreter/}` (interpreter
   has `control.rs`, `memo.rs`, `repeat.rs`, `value.rs`; compiler has `emit.rs`, `compound.rs`,
   `rule.rs`). That is LOCK-3's IR-lowering territory (contract-tower / single-IR-multipass) **and** the
   engine 4D inherits for free. A parse-that-private VM is therefore a *second* bytecode VM alongside the
   one bbnf already owns — precisely the double-homed work the gestalt exists to collapse, and a
   `system-cohesion`/`no-orthogonal-codepaths` violation. The incompatibility map (§5) treats 4C as
   fully orthogonal to 4D on ownership only; it does not name that 4C-private and the extant `vm/` are
   the *same artifact class in two homes*. This must be an enumerated gap: 4C is only lawful if it
   *is* bbnf_ir's VM (which collapses it into 4D) or if a private VM is justified against LOCK-3's
   verdict.
3. **The iso "strictly stronger than any LOCK-6 mirror-linter" claim pre-empts LOCK-6.** 4C asserts its
   byte-identical-program oracle is "strictly stronger than any LOCK-6 mirror-linter" (FAMILY-SPECS §3;
   §5 repeats "4C's is strictly stronger"). That is a LOCK-6 verdict rendered inside LOCK-4. If LOCK-6
   independently rules 6C (wasm-core: one Rust impl, TS is bindings), the byte-identical-*two-decoder*
   program is moot — there is one decoder. LOCK-4 cannot certify its own iso mechanism as the winner;
   this is a cross-lock ordering hazard (see §6), not a settled strength.
4. **Value-tier collapse to 4B is an admitted escape the family cannot own.** 4C §"open questions" (b)
   and research-C risk 2 concede that if value-tier hits 0.6× the honest verdict is "recognition VM yes,
   value VM needs staging → 4B." A family whose value plane resolves by *becoming another family* has not
   converged as a terminal runtime; it has an unowned boundary. PROTO-1 showed a *tree-walk* value graph
   survives — but a *bytecode* value-tier (indirect `Action` per projection) is a different animal and
   was not measured. Do not import PROTO-1's tree-walk parity as bytecode-VM parity.
5. **Left-recursion seed-growth parity across rust/ts is the one genuinely subtle facility and is
   untested.** Named (research-C risk 4) with a concrete counterexample shape; no artifact.

---

## 4. FAMILY 4D — GRAMMAR-SUGAR

**Convergence: 25%.** Rust-side is the most elegant, near-free parsimony story in the lock and the
substrate is real (verified: bbnf_ir `vm/interpreter/mod.rs`, `vm/compiler/`, `bytecode.rs`, `egraph/`,
`passes/` all exist). **But elegance earns nothing** — 4D is BLOCKED on a TS-execution primitive of
equal-or-greater difficulty than the original problem, and PROTO-1 (Rust-only) cannot touch the block.

### Exact open gaps
1. **The BLOCK is real, correctly diagnosed, and unmoved this pass.** Both TS horns are constitution-i
   traps: (i) full port of `GrammarIR` + interpreter + optimizer passes to TS = re-implementing the
   grammar plane's engine in a second language (a primitive of equal difficulty); (ii) the WASM-core
   crossing, where **PROVE-14 measured full-walk value materialization at ~0.91× — a LOSS on exactly the
   workload a value-producing combinator parser IS**. Neither horn was tested this pass. The "exact gate
   to unblock" (measure the WASM full-walk for a value-producing combinator parse; and/or LOCK-5's
   substrate ruling) is fully external and open. 4D cannot converge above blocked until one resolves.
2. **`FnDescriptor::HostClosure` is a required NEW primitive with an unmeasured poison surface.** The IR
   has no opaque runtime-closure variant (`FnDescriptor` has only transparent `Expr{MapExpr}`, verified
   in the spec's cited seam). Adding `HostClosure` aligns with LOCK-7 — but LOCK-7's host-fn lattice is
   itself unconverged, so 4D depends on *two* external unresolved locks (LOCK-5 substrate + LOCK-7
   host-fn). PROTO-2 mitigates by showing the opaque set is tiny (|M|=1), but see §5 — PROTO-2 asserts
   reformulability, it does not demonstrate it.
3. **State-effecting combinators (`peek`/`lookAhead`/`recover`/`mapState`) are outside the IR
   vocabulary.** research-D risk 3: `IrNode` models only `Negate`/`Minus`, not the `ParserState`
   mutations these perform. "The minimal lawful set must be measured, not guessed" — unmeasured. This is
   a missing-primitive gap distinct from `HostClosure`.
4. **Two-runtimes-behind-one-API is an honest but unresolved bifurcation.** research-D risk 5: compile
   mode needs the grammar at build time; interpret mode serves runtime grammars. The "inherit the
   optimizer / codegen" value is strongest only for the build-time subset; the runtime-grammar path is
   the interpret-mode regression lane (the evaporated 1.34–1.57× shim is the standing caution). No number
   distinguishes them.
5. **Parsimony cuts FOR 4D and the portfolio should say so.** Under constitution vi (extreme parsimony,
   fold-into-existing) 4D is the maximum: one IR, one optimizer, one iso bus, all verified to exist.
   4A/4C stand up NEW private subsystems (a node union + passes; an instruction set + two interpreters)
   that *duplicate* bbnf_ir's `vm/` + `egraph/` + `passes/`. The synthesis records 4D's parsimony but
   does not weigh that choosing 4A/4C *over* 4D is choosing the less-parsimonious, duplication-heavy path
   the standing memories forbid. If 4D's TS block resolves, the constitution actively favors it; that
   conditional dominance (§5 already states "if LOCK-5 rules bbnf_ir the shared substrate, 4D dominates")
   should govern the pass-2 ordering, not sit as a footnote.

---

## 5. THE TWO PROTOTYPES — HARDENED

### PROTO-1 (VALUE-PLANE-SURVIVAL) — RAN, real, but over-scoped in its verdict
- **Genuine and refutable** (paired, cold, two-window, byte-identical parity gate green, real 0.986×
  floor). Concrete-artifact clause satisfied. Good.
- **Gap A — untested passes (see §1.1):** hand-writes the fused arena; the "fusion-as-a-pass" keystone
  is not exercised. The verdict "4A/4C/4D single-tier VIABLE" credits the running artifact with a claim
  it does not test.
- **Gap B — wrong rival:** beats the *old closure core*, not a monomorphized 4B chain; cannot adjudicate
  the 4A-vs-4B terminal decision it is cited toward.
- **Gap C — Rust-only:** "4C/4D single-tier VIABLE" is unqualified in the verdict but the artifact is
  Rust; 4C-bytecode-value-tier and all-TS are untested. Do not let a tree-walk Rust parity result stand
  in for a bytecode-VM or a V8 result.
- **Requested correction:** re-label the verdict "single-tier Rust tree-walk VIABLE vs the *current*
  closure core; the pass-architecture, the monomorphized-4B rival, the bytecode-VM value-tier, and all
  TS remain unmeasured."

### PROTO-2 (CHAIN-MONADIC-CENSUS) — RAN, honest, but classification is asserted not demonstrated
- **Genuine census; |M|=1 is a real, refutable finding** and the poison-scope reasoning on the LaTeX
  `environment` rule is sound. Good — this materially de-risks the shared analyzability gate.
- **Gap A — S-reformulability is asserted.** The 4 value.js sites are *classified* "S selective-
  reformulable"; none was actually rewritten as `branch`/`select` and shown to preserve the parse. "Sees
  through every one" is a design assertion, not a demonstration. A pass-2 that builds a selective algebra
  must prove these four, not inherit the label.
- **Gap B — "uniformly analyzable" over-reaches for the value plane.** `map` is declared "A by type"
  because its signature can't return a `Parser` — TRUE for *structure*/FIRST/dispatch. But an opaque
  `map` closure IS opaque to *value-plane* analyses (chumsky-style discard/liveness, cost modeling) that
  4A/4C/4D also want. "The analyzable subset is effectively the WHOLE grammar" is true for FIRST/dispatch
  and false for value-liveness; the synthesis inherits the strong phrasing. Scope it.
- **Gap C — census surface is narrow.** Covers three consumer suites + parse-that's own json/csv/css. It
  does not cover parse-that's full test surface or the bbnf grammar plane's generated combinator usage;
  |M| is a lower bound over shipped consumers, presented close to a universal.

---

## 6. CROSS-LOCK VIEW (critic-2 weight) — where LOCK-4 contradicts or double-homes the codex

1. **4C (and 4D's compile mode) vs LOCK-3 + bbnf_ir's live `vm/`: a bytecode VM double-home.** Verified:
   `crates/ir/src/vm/` already contains `bytecode.rs`, a compiler, and a value-producing interpreter.
   4C-private stands up a *second* bytecode VM; that is LOCK-3's IR-lowering seat and the very engine 4D
   inherits. Enumerated resolution owed by the gestalt: **one bytecode home.** Either 4C *is* bbnf_ir's
   VM (collapsing 4C into 4D) or a parse-that-private VM is justified against LOCK-3's contract-tower —
   it cannot be silently orthogonal.
2. **4A vs LOCK-2's one-graph-engine edict: a fusion-e-graph double-home.** 4A "mirrors
   `rust/regex/src/egraph/`" for fusion; `egraph`/`egraph-derive` exist as in-tree crates that LOCK-2
   owns. A parse-that-private fusion e-graph violates `general-infra-crates` + `system-cohesion`. 4A's
   passes must consume LOCK-2's engine.
3. **LOCK-4's terminal choice pre-decides LOCK-6, and vice versa — a circular ordering hazard.** 4A
   commits to 6A (mirror-tree + CI linter); 4C claims its byte-identical-program is "strictly stronger
   than any LOCK-6 mirror-linter"; 4D's block hinges on 6C (wasm-core, PROVE-14 0.91×). These are three
   *different* LOCK-6 verdicts embedded in three LOCK-4 families. LOCK-4 cannot converge independently of
   LOCK-6: if LOCK-6 rules 6C, 4A's mirror discipline and 4C's two-decoder oracle are both moot, and 4D's
   block either lifts (wasm wins full-walk) or hardens (it doesn't). **The two locks must co-converge in
   GESTALT-P; neither can be frozen alone.** The synthesis does not surface this mutual constraint.
4. **4D depends on LOCK-5 (substrate ruling) AND LOCK-7 (host-fn lattice) — both unconverged.** 4D's
   entire "for free" claim is contingent on LOCK-5 ruling `bbnf_ir` the genuine shared substrate; its
   `HostClosure` primitive is contingent on LOCK-7's lattice. Correctly flagged in-family, but the
   cross-lock consequence is that **4D cannot be selected until two other locks land** — a DAG-ordering
   fact for the gestalt, not a within-LOCK-4 resolution.
5. **PROVE-14 (LOCK-6/6C seed) is load-bearing against 4D and is a *measured non-goal*, not a bug to
   fix.** The 0.91× full-walk is the constitution's refutation-law territory (§0/14). Any pass-2 attempt
   to "unblock 4D via wasm" must beat a measured non-goal on the exact workload — that is a high bar the
   ordering must respect, not assume away.

---

## 7. CONSTITUTION & FAILURE-MODE LEDGER

- **Vacuous convergence:** avoided by both prototypes (real refutable gates). But the *lock-level*
  convergence framing (§0) is vacuous-adjacent: "two numbers pass-2 cannot pick without" implies the two
  numbers enable the pick; they do not (§0.1–0.3).
- **Spec-cites-itself:** 4B architecture ("no runtime graph at all") vs 4B open-question d (fallback
  graph) — §2.4. 4C's "strictly stronger than LOCK-6" cites a LOCK-6 verdict LOCK-4 has no standing to
  render — §3.3/§6.3.
- **Gates that cannot fail:** 4B's generated-size budget has no quantified threshold (§2.5); PROTO-2's
  poison-scope "test" is a reasoning argument, no analyzer run (§5, PROTO-2 Gap A).
- **Elegant-reduction trap:** §0 (the pick reduces to two briefs + "and-then-pick"); 4C's value-tier
  reduces to "→ 4B" (§3.4); 4D's whole Rust elegance reduces to "and-then the TS primitive" (§4.1).
- **Legacy alias / masked fallback:** 4A `lazy()`-as-sugar unexercised (§1.4); 4B retained interpreter
  graph (§2.4). Both flagged, neither exercised.
- **Consumer-less substrate (Y.13):** families preserve real consumers (verified). The live risk is the
  inverse — 4A/4C build *duplicate* substrates against bbnf_ir's existing consumed ones (§6.1/§6.2).
- **Unverified gestalt:** the "4A/4B/4C compose as a Parsley pipeline" claim (priorart §1) is used to
  argue the incompatibilities are "terminal-runtime only" — but no artifact composes them here; it is
  imported prior art, correctly cited, not locally verified. Acceptable as prior art; must not be read as
  a local result.
- **Prototype-evidence misuse:** PROTO-1 credited to the untested pass architecture (§1.1) and to
  non-Rust/non-4B decisions (§5). This is the sharpest single misuse to correct before pass-2.
- **Zero-runtime-cost types (v):** not engaged by any family this pass; TS type-level story is LOCK-8,
  correctly out of LOCK-4 scope.

---

## 8. VERDICT & PASS-2 DIRECTIVES

No family is converged; this is pass 1 and correctly so. The two prototypes are real and advance the
*shared* analyzability crux — but the *selection* crux is untouched. Pass-2 must:

- **Run the deferred 4B inlining prototype** (generic-struct JSON parser vs `SmallBox` vs `span_parser`
  enum tier; `cargo asm`/`otool` mechanism proof + paired bench). Without it the 4A-vs-4B terminal axis
  is undecidable and the whole §5 incompatibility map is un-actionable.
- **Build the 4A `fuse(Graph)→Graph` pass** and run gate P1 (byte-identical to the hand-fused arena) —
  the claim PROTO-1 was credited with but did not test.
- **Produce ONE TS number** (a TS tag-interpreter or fuser, cold-paired vs today's closures) — the
  constitution-iii half is 0% built.
- **Adjudicate the bytecode double-home** (4C-private vs bbnf_ir `vm/` vs 4D-inherited) and the
  fusion-e-graph double-home (4A-private vs LOCK-2) BEFORE electing a terminal runtime — these are
  cross-lock, and picking a private-substrate family without the ruling reopens the very duplication the
  gestalt collapses.
- **Treat LOCK-4 and LOCK-6 as co-converging**; do not freeze LOCK-4's terminal runtime ahead of
  LOCK-6's iso verdict and LOCK-5's substrate verdict.

**Per-family convergence: 4A 55% · 4B 35% · 4C 30% · 4D 25% (BLOCKED, Rust-elegant, TS-blocked).**
