# LOCK-4 — THE COMBINATOR CORE · pass-1 · CRIT-1 (Adversarial Critic 1 of 2 — WITHIN-LOCK view)

*Weighting: internal soundness of each family spec, and whether the two prototypes actually exercised the
sharpest uncertainty they are cited for. Every load-bearing path:line was spot-checked against the
read-only `parse-that` tree; `PROTO-2-4B-FUSION.patch` was read in full and its faithfulness to the
substrate verified (`parse.rs:41-53` blanket `impl ParserFn for F`; `parse.rs:61` `Parser =
SmallBox<dyn ParserFn,S32>`; `lazy.rs:20-64` `UnsafeCell` + `Box<dyn LazyParserFn>` + cached `Parser`;
the fused arm impls `ParserFn` on concrete `Seq2/Seq3/Alt2` structs — all confirmed).*

**This CRIT-1 supersedes the earlier CRIT-1 (timestamp 04:09) verbatim.** That file critiqued a
now-abandoned probe set (`PROTO-1.md` VALUE-PLANE-SURVIVAL hand-fused `json_graph.rs`). The CURRENT
`FAMILY-SPECS.md` (04:34) re-synthesised the lock around two fresh briefs — **PROBE-1** (monadic-wall
census) and **PROBE-2** (Rust runtime-substrate fork) — which then RAN as
`PROTO-1-MONADIC-WALL-CENSUS.md` (04:41, GREEN) and `PROTO-2-4B-FUSION.md` (04:53, 4B CONFIRMED). The
prior registry numbers (4A 50 / 4B 32 / 4C 35 / 4D 30) predate both and are stale. This critique reprices
against the current evidence base.

**Verdict up front.** The pass's decisive event is real and I credit it: **PROBE-2 ran the exact 4B
inlining experiment the prior pass called "deferred", and it PASSED its own stated gate** — zero vtable
`blr` on the fused hot chain, ≥1.10× cold-paired in all 12 cells, both load windows, both arm orders.
That is a genuine, refutable, could-have-failed artifact (constitution vii satisfied). But the win it
certifies is **narrower than "beyond-SOTA"**: the baseline is a *synthetic* `SmallBox` closure core, not
the shipped tuned `json_parser()`; it is **Rust-only** (constitution iii is still 0% built for every
family); the grammar is JSON (the smallest, least blowup-prone); and the fused form was reached by
**hand-written structs**, not the public builder API — whose `seq!`/`alt!` macros do not even compile on
the toolchain (E0282 / HRTB warts, conceded in the probe). The terminal runtime **still cannot be named
inside LOCK-4**: Fork I (graph HOME) is untouched and the isomorphism half of Fork II is unmeasured.

---

## 0. Cross-family findings that reprice every family this pass

### F-1 — PROBE-2's "FORK II routes to 4B" is a TWO-arm measurement generalised to a THREE-way fork (prototype-evidence over-reach).

FORK II is 4A (extracted closures) ⊗ 4B (monomorphized straight-line) ⊗ 4C (bytecode VM). PROBE-2 measured
exactly one pair: `fused` (4B) vs a hand-reconstructed `SmallBox` **closure core**. It never built 4A's
*extracted-fused-region* runtime, and it never built 4C's dispatch-loop VM. So "routes to 4B for Rust"
(PROTO-2-4B-FUSION verdict line) is, against 4A and 4C, an **inference** — reasonable (4A keeps a box per
fused region, 4C's honest prior is a loss), but not a measurement. The one datum that IS direct is
4B-beats-closures; everything else in the routing claim is deduced. Pass-2 must state it as inference
until a three-way bench exists.

### F-2 — PROBE-2 does not just confirm 4B; it measures a NEW disadvantage for 4A's own runtime, unsurfaced by the synthesis.

`FAMILY-SPECS §4A` fixes 4A's runtime as **"extraction to a fused closure tree (one closure per fused
region … closures remain the execution substrate)."** PROBE-2 just measured that the closure-dispatch tax
is **16–27%** on the JSON structural spine (zero-`blr` fused vs 60-`blr` closure). Therefore 4A's runtime
substrate is now **measured-inferior to 4B on the very axis PROBE-2 tested**, unless 4A's fused regions
are large enough to amortise the per-region box. FORK II is no longer symmetric: PROBE-2 puts a thumb on
the scale *toward 4B and away from 4A-as-terminal-runtime*. The FAMILY-SPECS incompatibility map still
treats 4A/4B/4C as three co-equal Fork-II positions; that is now false on the throughput axis for Rust.

### F-3 — "Beyond-SOTA" is unestablished; the win is over a synthetic baseline, not the shipped parser.

PROBE-2 is explicit and honest that it is **not** a comparison against `json_parser()` (which layers
`dispatch_byte_multi` + SIMD string scanning). Both arms share identical leaf scanners as direct `bl`, so
the measured 1.16–1.27× is *purely the box-dispatch tax on structural combinators*. That confirms the 4B
**mechanism** (box dispatch is removable and costs ~20% of the structural traversal). It does **not**
confirm the 4B **deliverable** (that 4B beats the tuned incumbent parse-that ships). "Those optimizations
… would ride on top of 4B" is asserted, not measured — and `dispatch_byte_multi` removes exactly the
kind of per-node dispatch 4B also removes, so an interaction that shrinks the marginal win is a live,
untested possibility. The LOCK-4 charter is *beyond-SOTA*; PROBE-2 clears *beyond-the-naïve-boxed-core*.

### F-4 — PROBE-1 greenlights the STRUCTURAL reification floor, not the VALUE-plane one — which is 4D's sharpest risk.

PROBE-1's GREEN ("analyzable subset is effectively the WHOLE grammar") is a claim about
**structure/FIRST/dispatch** analyzability: `chain`/`mapState` are the only structure-manufacturing
opaque nodes, and they are 0.525% and hot-path-absent. That is exactly what 4A dispatch/packrat, 4B
fusion, and 4C control-flow lowering need — good, real, credited. But `map`-over-an-opaque-closure is
tallied bucket (b) "analyzable" **because its signature cannot return a `Parser`** — true for structure,
**false for value-plane analyses** (discard/liveness/cost, and the tape-materialization bridge). 4D's
staged-lane crux (does arbitrary `.map` output reconcile cheaply with bbnf's tape-first runtime?) is a
value-plane question PROBE-1 does not touch. So PROBE-1 lifts the floor for 4A/4B/4C structurally and does
**little for 4D's blocking risk**. The synthesis's uniform "green floor for all four" over-reads it.

*(All four findings are within-lock: they concern whether the probes tested the sharpest uncertainty of
the families they are cited for, independent of any other lock.)*

---

## 1. FAMILY 4A — CLOSURE-TO-GRAPH — **convergence 45%**

**Sound substrate, but the current synthesis dropped 4A's one running artifact and no new probe touches
its mechanism.** The substrate is real and consumed (`SpanKind`, live `parsers/json.rs` + `parsers/css/*`
— Y.13 by construction) and the passes-as-rewrites framing is coherent with an in-tree precedent
(`rust/regex/src/egraph/`). But the re-synthesis no longer leans on the old `json_graph.rs` tree-walk
parity result, and **neither PROBE-1 nor PROBE-2 exercises a single 4A gate** (G-A1…G-A6). PROBE-1 lifts
4A's *analyzability precondition*; PROBE-2 (F-2) lowers 4A's *runtime-substrate standing* vs 4B. Net: 4A
is the best-substantiated family on substrate and the least-tested on its own novel claim.

**Enumerated open gaps:**
1. **[keystone] Fusion-as-a-separable-pass (G-A2) is unbuilt and unproven.** No `fuse(Graph)→Graph`
   exists; the thesis that "hand `seq!`/`fuseAll` fusion becomes a derived total pass, byte-identical to
   the hand form, for arbitrary arity" is SPEC-ONLY. This is 4A's *reason to exist* and it has zero
   running evidence.
2. **General-union value-plane dispatch cost (G-A1/open-Q) untaken.** Whether a `match` over the ~25-node
   `Node` union — widened by the `Map` payload — stays competitive with monomorphized straight-line was
   not measured this pass; PROBE-2 measured the *opposite* substrate (monomorphized, no union match).
3. **[from F-2] 4A's extracted-closure runtime is now measured-inferior to 4B on Rust throughput.** The
   "one box per fused region" runtime pays the exact 16–27% box tax PROBE-2 just certified, unless regions
   are large. No measurement of how large 4A's fused regions get on real grammars, nor whether extraction
   recovers 4B-level inlining. 4A must either concede the throughput axis to 4B or prove region-size
   amortisation — untested either way.
4. **Packrat-SCC placement (G-A5) + FIRST-set dispatch synthesis (G-A4) + diagnostics-annotation pass
   unbuilt.** All three "derived, not hand-maintained" claims are asserted; no rule-map, no SCC, no
   synthesised LUT, no labels pass.
5. **One-path collapse unproven.** The claim that the whole combinator library folds to a single graph
   tier (not a parallel second codepath alongside the closure `Parser`) is untested; parsimony (vi) is at
   risk until a real fold, not an added arm, is shown.
6. **Rust↔ts isomorphism (G-A6) 0% built.** No `Node` union or passes in TS; R4's live warning (a
   `switch(node.tag)` interpreter may go megamorphic on V8; the dispatch-LUT lever under-delivered ~+5%
   vs +10% claimed) is unaddressed. This is a LOCK-6 dependency 4A cannot resolve alone.
7. **Recursion-seam guard unexercised.** The `lazy()`→`Ref(RuleId)` auto-mint-via-cache-identity is a
   masked fallback until a consumer grammar with anonymous mutual recursion is run through it; `lazy.rs`
   ships `UnsafeCell` + `Box<dyn LazyParserFn>` + cached `Parser` (verified) — the identity-recovery cost
   is unpaid.

---

## 2. FAMILY 4B — COMPTIME-FUSION — **convergence 50%**

**The pass's biggest single lift (from ~32% to 50%): its deciding throughput experiment RAN and passed
its stated gate.** PROBE-2 is a genuine refutable artifact — zero `blr` on the fused chain (hard disasm
gate), 1.16–1.27× cold-paired in all 12 cells, both windows, both arm orders, load-symmetric, equivalence
gate green, recursion proven (one box on the closure arm, zero on the fused arm). On the axis 4B names as
its reason to exist — the indirect-call/inlining barrier — 4B now has **positive Rust evidence**, not the
NEUTRAL prior the prior pass recorded. That is the correct verdict for the mechanism. But 50%, not higher,
because the win is scoped and the family's *other three axes are 0% built*.

**Enumerated open gaps:**
1. **[from F-3] The win is over a synthetic `SmallBox` baseline, not the shipped `json_parser()`.**
   "Beyond-SOTA" — the charter word — is unestablished. The production optimizations riding "on top" is
   asserted; the `dispatch_byte_multi` interaction that could shrink the marginal win is untested.
2. **Rust-only; isomorphism (constitution iii) 0% built — and 4B is the WEAKEST of the four on this
   axis.** Rust monomorphized generics have **no TS analog**; the TS face is an *emitted-code* build-time
   fuser (G-B3), unbuilt. "Isomorphism in layout AND in code" (iii, non-negotiable) is 4B's softest
   position, and the family whose throughput case is strongest is the one whose iso case is weakest — an
   awkward tension for the lock the synthesis does not resolve.
3. **Ergonomic realizability unproven — the public API path does not compile on the toolchain.** The
   probe concedes `seq!`/`alt!` hit E0282 / "FnOnce is not general enough" on nightly-2026-04-11; both
   arms were **hand-written** (fused structs; the closure arm to the macros' expansion). So it is
   unproven that `a.then(b).then(c)` monomorphizes to `Seq2<Seq2<A,B>,C>` *through the public API* rather
   than requiring per-grammar hand-written structs. This is a real, current friction, not hypothetical.
4. **Monomorphization / generated-size blowup untested at real grammar scale.** JSON is the smallest,
   least-nested grammar. The blowup-prone consumers (CSS, value.js — hundreds of productions, deep
   Seq∘Alt nesting) are unmeasured. The `generated-size-budget` memory binds a per-tranche line ceiling;
   the fusion-policy question (fuse arity 2–4 + first-byte dispatch; table long fan-out) is open.
5. **Generated-size budget is a gate that cannot fail (no threshold).** PROBE-2 "reports" 6.4/7.0 s
   incremental compile and a binary-size figure but against **no budget** — "no size regression concern
   at this grammar scale" is a judgment, not a pass/fail. A gate with no number cannot refute.
6. **PROVE-11 caps the TS half at ~+5% dispatch.** The certified TS delta is allocation-shaped, not
   dispatch-shaped; a 4B-TS fuser that isolates dispatch-elimination reproduces the small number.
   "Beyond-SOTA for 4B-TS" is unsupported until the dispatch-vs-allocation isolation gate runs.
7. **[latent] 4B-as-terminal vs 4B-as-build-time-backend-of-4A/4D unresolved.** In a Parsley-style
   pipeline 4B is the *staging endpoint* of a reify→specialise flow, not a fourth co-equal terminal.
   Whether 4B is elected as THE runtime or subsumed as the compile-mode of a graph family (Fork I) is
   undecided and is a gestalt question.

---

## 3. FAMILY 4C — VM-CORE — **convergence 28%**

**No artifact touched its mechanism this pass, and PROBE-2 hardened the case against its throughput bid.**
`SpanKind`/`CompiledDfa(Arc<Dfa>)` show half the leaf vocabulary is already a compiled machine — real. But
not one line of a choice-point bytecode VM was written; PROBE-2 confirming 4B for Rust throughput means
4C's own honest prior (a 0.7–0.9× loss, value-tier possibly 0.6×) is now *dominated on the axis it
concedes*. 4C's remaining case is entirely on the non-throughput axes (iso-by-construction,
TS-megamorphism-kill, one-register diagnostics, streaming) — and its cheapest, highest-signal,
throughput-independent claim was skipped.

**Enumerated open gaps:**
1. **The byte-identical-program iso oracle — 4C's differentiator — is unbuilt.** Nothing serializes a
   program or decodes it in TS. This is the single cheapest thing 4C could have proven (needs no
   throughput win), and it was not done. Its "strictly stronger than any LOCK-6 mirror-linter" claim is a
   LOCK-6 verdict rendered inside LOCK-4 (spec-cites-itself / cross-lock over-reach) — and unexercised.
2. **[cross-lock, verified] Bytecode-VM double-home.** `bbnf_ir` already ships a value-producing bytecode
   VM (`crates/ir/src/vm/{bytecode.rs,compiler/,interpreter/}`). A parse-that-private LPeg ISA + two
   interpreters is a *second* bytecode home — `no-orthogonal-codepaths` / `system-cohesion` forbid it. 4C
   is only lawful if it *is* bbnf_ir's VM (which collapses it into 4D) or a private VM is justified
   against LOCK-3's contract-tower. Unresolved.
3. **Throughput is a loss-to-be-recovered with zero artifact.** The threaded-code recovery, the Span-tier
   vs tree-walker paired bench, and the value-tier honesty check are all unrun. Do not import PROBE-2's
   *closure-core* comparison, nor any tree-walk parity, as bytecode-VM evidence — different overhead
   regime.
4. **Value-tier collapse into 4B is an admitted, unowned escape.** If every `Action k` is an
   un-inlinable indirect call, the value-tier resolves by *becoming 4B* (staging). A family whose value
   plane converges by turning into another family has not converged as a terminal runtime.
5. **Left-recursion seed-growth parity across rust/ts — the one genuinely subtle facility — untested.**

---

## 4. FAMILY 4D — GRAMMAR-SUGAR — **convergence 25% (BLOCKED; the block is high-confidence and unmoved)**

**Rust-side near-bijection verified and elegant; the block equally verified and unmoved; PROBE-1 slightly
de-risks the poison surface but PROBE-2 mildly weakens 4D's perf-differentiation story.** `IrNode` (14
variants) matches the near-bijection table; `FnDescriptor` has `EnumWrap/BoxWrap/NumberConvert/HexConvert/
SpanCapture/Expr{MapExpr}` and **no opaque host-closure variant** — so the central wall (must add
`HostClosure`) is code-verified real; `parse_with_ir` exists at `interpreter/mod.rs:373`. On Rust alone,
4D is the parsimony maximum — but elegance earns nothing on its own.

**Enumerated open gaps:**
1. **The TS-execution primitive is unresolved (the block), and neither horn moved.** Horn (i) full port
   of the grammar-plane engine to TS = a primitive of equal-or-greater difficulty than the original LOCK-4
   problem; horn (ii) the WASM crossing, where PROVE-14 measured **~0.91× on full-walk value
   materialization** — a LOSS on exactly the workload a value-producing combinator parser is (a measured
   non-goal, not a bug). Neither was tested. The 4C-graft third horn (a small TS switch-loop decoder over
   a serialized bbnf program) is unbuilt.
2. **[F-4] PROBE-1 does NOT de-risk 4D's sharpest risk.** The value-model materialization tax (arbitrary
   `.map` output ↔ tape-first codegen) is a value-plane question; PROBE-1 greenlit only structural
   analyzability. 4D's blocking uncertainty is untouched by this pass's only census.
3. **[from F-3, adverse] PROBE-2 mildly weakens 4D's differentiating perf claim.** 4D's staged-lane value
   is "inherit bbnf's optimizer/prefilter for free." PROBE-2 just showed a **native monomorphization
   (4B) buys 1.2× Rust with no bbnf IR at all** — so 4D's "for free" throughput advantage now competes
   with a proven native route that needs none of bbnf's coupling. Fork-I evidence, gestalt-level, but it
   cuts against 4D's headline.
4. **Cannot converge inside LOCK-4.** Hard dependency on LOCK-5 (is `bbnf_ir` *genuinely* the shared
   substrate? else "for free" evaporates) AND LOCK-7 (host-fn lattice for `HostClosure`, itself
   unconverged). 4D's ceiling is bounded by two external verdicts.
5. **`FnDescriptor::HostClosure` + state-effecting combinators (`peek/lookAhead/recover/mapState`)
   outside the IR vocabulary; decisive `parse_with_ir`-vs-closure number unmeasured.** The minimal lawful
   variant set "must be measured, not guessed" — it was not.

---

## 5. The two probes — hardened (within-lock)

### PROBE-2 / PROTO-2-4B-FUSION — RAN, genuine, gate PASSED; four scope limits
- **Credited.** Refutable (could have shown <1.10× or window/order asymmetry — the PROVE-09 failure
   shape), and did not; the disasm gate is a hard, timing-independent proof (0 `blr` fused vs 60 closure).
   The patch is faithful to the substrate (verified). The equivalence gate and recursion proof are real.
- **Limit A (F-3):** baseline is a *synthetic* `SmallBox` core, not `json_parser()`; beyond-SOTA
   unestablished.
- **Limit B (F-1):** measured 4B-vs-closure only; "routes to 4B over 4A/4C" is inference, not a three-way
   bench.
- **Limit C:** Rust-only (iso 0% built); three corpora, not the six the gate names; median-only, no
   dispersion/CI (the citm cells at 1.161–1.186× clear ≥1.10× by a thin ~5% margin under loadavg 8–17).
- **Limit D (refutation-law edge):** the manual harness does a **3-rep, fused-only warmup** (`manual()`,
   patch line 488–489) before the paired loop — asymmetric and mildly against the cold-only discipline,
   though each timed rep is a genuinely cold full-document parse. Immaterial to the ≥1.10× verdict given
   N=250 + median, but it should not be waved through as "purely cold."
- **Requested correction:** re-label the verdict *"4B Rust throughput MECHANISM confirmed vs the boxed
   closure core; beyond-SOTA (vs the tuned parser), the 4A/4C three-way, all-TS/iso, large-grammar
   blowup, and the public-API realizability remain unmeasured."*

### PROBE-1 / PROTO-1-MONADIC-WALL-CENSUS — RAN, honest, GREEN; robust where the prior census was soft
- **Credited, and improved over the prior census.** The GREEN verdict **does not depend on the S/M
   tally** — counting all 5 opaque sites, 5/952 = 0.525% ≤ 2%, and hot-path `chain`/`mapState` = 0. The
   prior CRIT worry (the |M|≤3 pass rested on optimistic S-tagging) is largely dissolved: the threshold
   holds even tagging every site opaque.
- **Edge A (F-4):** greenlights the STRUCTURAL floor only; the value-plane floor (which 4D needs) is
   untouched. "Analyzable subset is effectively the WHOLE grammar" over-reaches for value-liveness/cost.
- **Edge B:** snapshot of today's grammars, not a property of the algebra (acknowledged). Surface is the
   three consumer suites + parse-that json/csv/css — not parse-that's full test surface nor bbnf's
   generated combinator usage. |M| is a lower bound over shipped consumers.
- **Edge C:** the `S`-reformulability of the 4 value.js sites is *classified*, not *demonstrated* — none
   was rewritten as `branch`/`select` and shown to preserve the parse; and such a rewrite is a public-API
   change + read-only-consumer migration (unowned). Immaterial to GREEN (poison-scope holds regardless).

---

## 6. Constitution & failure-mode ledger (within-lock)

- **Vacuous convergence:** avoided — both probes are refutable gates that could have failed. But the
   *lock-level* framing ("PROBE-1 + PROBE-2 clear the preconditions") is **elegant-reduction-adjacent**:
   PROBE-2 resolved only the *throughput half of Fork II for Rust*; the terminal-runtime pick still reads
   "and-then pick", and the hard part (Fork I graph HOME; the iso half of Fork II) is unconverged.
- **Spec-cites-itself:** 4C's "byte-identical program strictly stronger than any LOCK-6 mirror-linter"
   renders a LOCK-6 verdict inside LOCK-4 (§3.1). Milder than last pass on 4B (the "no runtime graph at
   all" vs fallback tension is reduced but the TS opaque-site fallback is still unconceded).
- **Gates that cannot fail:** 4B's generated-size/blowup gate has no quantified threshold (§2.5); PROBE-2
   "reports" deltas against no budget.
- **Prototype-evidence misuse:** PROBE-2's "routes to 4B" over-reaches a two-arm to a three-way fork
   (F-1); "beyond-SOTA" is claimed of a synthetic-baseline win (F-3). The single sharpest correction for
   pass-2.
- **Masked fallbacks:** 4A's `lazy()`-recursion-seam guard unexercised (§1.7); 4B's public macro API does
   not compile (E0282) and was side-stepped by hand-written structs (§2.3).
- **Isomorphism (iii):** **0% built for every family** — the sharpest structural gap in the lock. 4B (the
   Rust throughput winner) is weakest here; 4C claims the strongest form and left it unbuilt; 4A's is
   mirror+linter (TS unbuilt); 4D's is blocked. No family has *demonstrated* iso this pass.
- **Refutation laws (iv):** PROBE-1 correctly measurement-free; PROBE-2 cold-paired/two-window/both-order
   (good) with the one warmup edge above (§5, Limit D). No stable-port / warm-throughput claim survives.
- **Zero-runtime-cost types (v):** not engaged by LOCK-4 pass-1 (LOCK-1/8 territory). N/A.
- **Consumer-less substrate (viii):** satisfied — probes are scratch/read-only; 4A/4C/4D preserve live
   consumers. The inverse risk (4A/4C stand up substrates duplicating bbnf_ir's live `vm/`+`egraph/`) is
   a Fork-I / gestalt matter, correctly flagged, not a within-lock resolution.

---

## 7. Disposition for pass-2 (within-lock)

- **What is newly SETTLED (do not re-litigate):** on Rust, removing `SmallBox<dyn>` box dispatch from the
   structural combinator traversal via monomorphized `Seq/Alt` structs buys **16–27%** over the boxed
   closure core, with the vtable `blr` provably gone. The 4B *mechanism* is confirmed.
- **What is NOT settled (do not inherit):** that 4B beats the *shipped tuned parser* (beyond-SOTA); that
   Fork II "routes to 4B" over 4A/4C (only 4B-vs-closure was measured); anything TS / isomorphic; 4B's
   behaviour at real-grammar scale; and the terminal-runtime pick (Fork I is untouched).
- **Cheapest high-signal pass-2 moves, in order:** (a) the **three-way Rust bench** — 4B monomorphized vs
   4A extracted-fused-region closures vs 4C bytecode VM, over the same JSON grammar, so Fork II is
   *measured* not inferred (F-1/F-2); (b) run 4B against the **production `json_parser()`** (dispatch +
   SIMD on both arms) to convert mechanism→deliverable (F-3); (c) produce **ONE TS number** — the
   constitution-iii half is 0% built for all four; (d) build 4A's `fuse(Graph)→Graph` and run G-A2 — the
   claim 4A rests on and has never exercised.
- **Do not let the gestalt inherit "FORK II routes to 4B" as a three-way result** — it is a two-arm
   measurement plus an inference. And do not freeze LOCK-4's terminal runtime ahead of LOCK-6 (iso) and
   LOCK-5 (substrate); Fork I remains the deepest, wholly-untouched incompatibility.

**Per-family convergence: 4A 45% · 4B 50% · 4C 28% · 4D 25% (BLOCKED, Rust-elegant, TS-blocked).**
