# LOCK-4 — THE COMBINATOR CORE · REGISTRY

*Agglomerator record (Fable-tier judgment). One row per family; convergence is the **critics'
conservative floor** unless a floor is named as erring. The lock is not four features — it is four
compilation strategies for one combinator algebra, decided on two axes: **Q-runtime** (what survives to
runtime: graph 4A | bytecode 4C | none 4B) × **Q-ownership** (whose representation `.then()` builds:
parse-that-private 4A/4B/4C | shared `bbnf_ir` 4D). A single terminal runtime must be chosen under a
value-plane policy chosen once.*

> **CURRENT STATE = PASS 2 (2026-07-16).** Inputs folded this pass: pass-2 FAMILY-SPECS, research-{A,B,C,D,
> priorart}, DIRECTIVES-RESEARCH, **PROTO-1 (TS execution face, RAN/MEASURED/PASS)**, **PROTO-2
> (cycle-break + value-plane floor, RAN — one PASS, one MEASURED-NEGATIVE)**, CRIT-1 (within-lock), CRIT-2
> (cross-lock). Pass-1's re-folded body is preserved in PASS HISTORY. **Two prototypes RAN this pass and
> both hit the two sharpest pass-1 uncertainties** — PROTO-1 the fast-isomorphic-TS face (INCOMPAT-2),
> PROTO-2 the cycle-break gate-0 + the value-plane floor (INCOMPAT-1 + F-4). Both are refutable; both moved
> the lock. This is the strongest pass the lock has had. **The finding is a decided axis, not a named
> terminal:** the TS-fast face is settled to *build-time emitter, not runtime decoder* (×1.23 vs −22%), and
> gate-0 is proven mechanically severable — but the terminal remains un-electable inside LOCK-4 (two
> irreducible forks are exported to LOCK-5 and LOCK-6), and three reason-to-exist keystones remain 0% built
> after two–three passes.

---

## THE PASS-2 EVENTS (what RAN, what it decided, where it over-reached)

**PROTO-1 — TS execution face (RAN / MEASURED / PASS).** One projection-dense arity-7 `rgb(n,n,n)` region,
three TS runtimes, paired-cold sample-interleaved A,B,C, N=240×2 windows, 3 cold processes, both orders,
M5 Max / Node v26 / vitest 3.2.4. Result: **(B)** build-time-emitted specialized closure = **×1.23** over
the `.parser`-IC baseline (×1.231/×1.180/×1.231, every run > the ×1.05 gate), with **0** `.parser(` in the
emitted body (grepped; 7 lexical `f0..f6` + inline mapfn). **(A)** runtime switch-VM over 98-byte
`bbnf_ir`-shaped msgpack bytecode (sticky RegExp re-derived) = **−22%** vs B (A÷B 1.217/1.227/1.219, pinned
within 0.8%, inside ±2%) and merely **ties** the naive baseline (A÷C ×0.990, ±4%). **Decides INCOMPAT-2 by
measurement:** a fast isomorphic TS terminal EXISTS, but *only* as a build-time emitter; the runtime-decoder
face is refuted as the fast face. The SpanParser law (parse-that's own killed `−10..−14%` experiment)
**survives and sharpens**. *Scope honestly bounded (both critics):* the emitter is a hand-rolled `new
Function` over ONE arity-7 region (not a grammar-driven fuser); the projection was MapExpr-expressible (it
**dodged GAP-1**, arbitrary host closures); the msgpack blob was hand-encoded in-harness and self-compared
3-lane (perf answered; the **cross-language iso-of-record oracle was NOT built**).

**PROTO-2 — cycle-break + value-plane floor (RAN — one PASS, one NEGATIVE).** **Gate-0 = PASS.** The
`parse_that ↔ bbnf_ir` Cargo cycle (`crates/ir/Cargo.toml:13 parse_that = "0.4"`) is severed by moving the
4 `crates/ir` `parse_that` usages onto `bbnf_regex` (which already `pub use bbnf_regex::*`-re-exports them);
`cargo build -p bbnf-ir` exit 0; `cargo tree` acyclic (`parse_that → bbnf-ir → bbnf-regex`); **negative
control** (unmodified repo) reproduces `error: cyclic package dependency` — gate is load-bearing.
**Value-plane floor = RAN, MEASURED-NEGATIVE on its own ≥0.98× rubric.** Value completion built
(`Op::ApplyFn(FnId)` at the Map-drop site, `fns: Vec<FnDescriptor>` `#[serde(default)]` on `BytecodeProgram`,
`Value` += F64/U32/Str/Variant, `apply_fn.rs` MapExpr evaluator); **oracle byte-identical (GREEN)** — VM
`ApplyFn→Value::U32` == closure `parse_hex` for all 64 tokens, spans match. Paired cold N=400×2, both orders:
projection-dense **0.043×**, recognizer-only **0.021×**, amortized single-parse **0.462×**. **The FAIL
trigger fired.** Honest decomposition: the value-plane *tax proper* (ApplyFn over recognizer) is **~5%**
(near noise); the dominant gap is `Interpreter::new` per-parse **setup allocation** (8 Vecs + FxHashMap),
which amortizes ~22× (0.021×→0.46×) on a single parse. Even amortized, the *best-case zero-alloc* closure
stand-in stays **~2.2×** ahead.

**Two corrections the pass-2 specs under-state (folded from the critics — both load-bearing):**
- **[CRIT-1 RF-1, the sharpest new gap] The tower's `4A→4B` "Rust lowering" arrow is SPEC-ONLY and possibly
  incoherent.** 4A/4D build a **runtime** value (`.then() ≡ Seq(vec![…])`, an `IrNode`; PROTO-2 ran it as a
  runtime value — `Interpreter::new` per parse). 4B's confirmed win is **type-monomorphization** (`.then() ≡
  Seq2<A,B>`, a concrete comptime type) — which **cannot** be derived from a `Vec<Op>`/`IrNode` that exists
  only at runtime. "The graph erases into AOT Rust" is realizable only via a build-time codegen step reading
  a *comptime-known* graph — i.e. the **generator** path (grammar text → IR → monomorphized Rust), **not**
  parse-that's live combinator API. The tower composes cleanly for the generator path; for the combinator
  API it delivers only the *interpret* terminal (measured ~2.2× adverse) or today's closures — **not** a
  4B-fast Rust terminal. **A confirmed win (4B-Rust) on a mechanism the tower's own front (4A/4D runtime
  graph) cannot feed is a standing incoherence, not a lowering.** This is the pass-2 elegant-reduction trap:
  "the tower converges the mechanisms" reads the load-bearing "and-then make the runtime combinator graph
  reach comptime monomorphization" step as free. It is 0% built and, for a runtime API, not obviously
  possible without an unspecified codegen-reflection step.
- **[CRIT-2 §0.1/§4.3] PROTO-2's value floor RAN NEGATIVE on its own rubric; "should still be built —
  unblocks the Rust story" over-reads it.** Value-completion unblocks **correctness** (byte-identical,
  proven) — it does **not** unblock a competitive Rust runtime (the built-and-measured interpret terminal
  LOST). "Viability turns on amortizing/eliminating `Interpreter::new` setup" is a **NEW, unmeasured
  hypothesis for pass-3**, not a discharged gap. F-4's floor is a **LOSS of record**. The masked-fallback
  risk: relabeling a measured loss by its cause (setup, not value plane) does not convert it to a win.

---

## STATUS TABLE (pass 2)

| Family | Status | Conv. | One-line |
|---|---|---:|---|
| **4A CLOSURE-TO-GRAPH** | **ADVANCED** | **50%** | Role clarified — *source front, not terminal* (PROTO-2's adverse interpret number supports erasing the graph rather than running it); gate-0 retires route-b's cycle hazard. But its reason-to-exist keystone `fuse(Graph)→Graph` on a **combinator-constructed** graph is 0% built for the THIRD pass; the RF-1 `4A→4B` lowering arrow is incoherent for a runtime API; PROTO-1's runtime-switch refutation also refutes 4A-TS (a `switch(node.tag)` interpreter), unstated by the specs. |
| **4B COMPTIME-FUSION** | **ADVANCED · terminal-role BANKED→compile-backend + TS-emitter** | **58%** | The pass's biggest lift. Now has running evidence on **both** faces: Rust monomorphization confirmed (pass-1, 16–27%, 0 `blr`) AND the TS-emitter *mechanism-class* confirmed (PROTO-1, ×1.23, 0 `.parser`). The only family with a live artifact per language. Capped: the two faces are **different mechanisms** (monomorphization / emission — no shared isomorphic terminal); the emitter is one hand-rolled arity-7 region (general fuser + GAP-1 host closure unbuilt); production dilution + G-B4 budget still unmeasured/unset. |
| **4C VM-CORE / PORTABLE-PROGRAM ISO** | **BANKED** | **32%** | Terminal-runtime-on-V8 **measured-refuted twice** (PROTO-1 switch-VM −22%; PROTO-2 interpret 0.021–0.46×). Value-completion built + byte-identical (shared with 4D). But the contribution that "RISES" — the byte-identical portable-program **iso-of-record oracle** (G-C1) — is UNBUILT-of-record (PROTO-1 hand-faked the blob + 3-lane self-identity, never the dual oracle over six corpora). Folds into the LOCK-3/`bbnf_ir` VM line. |
| **4D GRAMMAR-SUGAR / SHARED-IR** | **BLOCKED (confirmed)** | **30%** | Gate-0 PASSED (strongest single 4D result; negative control load-bearing) and value-completion byte-identical — but the decisive Rust number is now a **LOSS of record** (0.46× even amortized), and the fast-isomorphic-TS block is *sharpened*: PROTO-1 shows the only fast TS face is the per-language emitter, refuting "one VM, two sides, for free" for the fast path. Correctness/parsimony story kept; performance story lost. Cannot converge inside LOCK-4 (LOCK-5 + LOCK-7 predecessors). |

Net: **no family is converged; the terminal runtime cannot be named inside LOCK-4.** Both irreducible forks
are externalized — **Q-ownership → LOCK-5** (5A shared type vs 5B lawful functor; gate-0 *compiles*
mechanically but the substrate *semantics* are LOCK-5's) and **the TS-fast terminal → LOCK-6** (the confirmed
no-V8-analog Rust win forces 6C/6D). The pass-2 shape-change: **the TS execution face is now measured-decided
(emitter, not decoder)** and **gate-0 feasibility is proven** — so the two things that pass-1 said "0% built /
unresolved" are answered *as mechanism/feasibility*. What remains is the *election*, which is not LOCK-4's to
make, plus the three unbuilt keystones below.

---

## 4A — CLOSURE-TO-GRAPH · ADVANCED · 50%

**Floor rationale.** CRIT-1 50 / CRIT-2 50 (independent agreement). Take **50**. Role-clarity is a real,
modest gain — the reframe fixes 4A as *source front, not terminal*, and PROTO-2's adverse interpret number
actively supports it (erase the graph into AOT rather than run it); gate-0 retires route-b's Cargo-cycle
hazard. It does not rise further because 4A's *reason to exist* was untouched by any pass-2 prototype for the
third pass running, and two new adverse cross-family findings landed on it (RF-1, the runtime-switch
refutation).

**What is EARNED (do not re-litigate).** The graph 4A wants is not greenfield: `bbnf_ir::IrNode` is a live,
serializable 14-variant near-bijection with the combinator set, already carrying fusion passes
(`passes/transform/`), a **consumed** (not cloned) e-graph (`Cargo.toml:19` path-dep — the LOCK-2 double-home
is retired *on the shared route*), and two runtimes. 4A's task is to make parse-that *construct* it and prove
the analyses ride cheaply — not to invent it.

**Exact open gaps.**
1. **[keystone, 0% built — THIRD pass] G-A1 `fuse(Graph)→Graph` byte-identical to a hand-fused arena is
   SPEC-ONLY on a combinator-constructed graph.** The specs' "the passes EXIST, not hypothetical" upgrade is a
   re-description: `fuse.rs`/`fuse_token`/`pattern_dedup`/`inline.rs` operate on the *generator's* `IrNode`
   from `.bbnf` text, not on a combinator front. The spec itself concedes the keystone is "point the existing
   passes at a combinator-constructed graph and oracle byte-identity" — exactly what is unbuilt. 4A's one
   novel claim still has zero running evidence.
2. **[RF-1, NEW] The `4A→4B` "Rust lowering" arrow is incoherent for a runtime combinator graph.** How a
   runtime-constructed `IrNode` reaches comptime monomorphization is unspecified (SPEC-ONLY; likely requires
   the generator's codegen-reflection path, not the combinator API). The tower's middle is asserted, not
   built.
3. **[RF-5 / INCOMPAT-3, THIRD pass] The three-way common-baseline Rust fork is STILL unbuilt.** 4A graph-
   interpret vs 4B `Seq2` mono vs 4C VM on ONE leaf substrate + ONE grammar. PROTO-2 ran 2 arms (VM vs a
   closure *stand-in*, not 4B). "4B beats 4A" remains inference across two non-comparable measurements. The
   lock's oldest untouched keystone.
4. **[Q-A2, unmeasured] The `Opaque(HostFnId)` barrier surface size on value.js/latex-paper** — the number
   that decides whether the analysis story holds. PROTO-1's projection was MapExpr-expressible, so it dodged
   GAP-1 and says nothing about the barrier. G-A3 untouched.
5. **[cross-family, under-applied by the specs] PROTO-1's runtime-switch refutation ALSO refutes 4A-TS.** A
   4A-TS graph-walking `switch(node.tag)` interpreter is the same runtime-tag-switch shape PROTO-1 measured at
   −22%. The specs apply the refutation to 4C/4D but leave 4A-TS "unmeasured." By its own evidence, 4A-TS must
   *also* lower through the build-time emitter (4A-source → 4B-emitter) — collapsing 4A-as-terminal on the TS
   side exactly as for 4C/4D. Unstated consequence, folded here.

**Pass-3 directives.**
- *Prototype (the keystone, owed three passes):* build `fuse(Graph)→Graph` over a **combinator-constructed**
  `bbnf_ir::IrNode` (not `.bbnf`-text IR) and run G-A1 — byte-identical to a hand-fused arena (structural-
  equality oracle) AND JSON/CSS cold-paired ≥0.98×. This is the claim 4A has been credited with but never ran.
- *Prototype (the three-way fork, owed three passes):* 4A graph-interpret vs 4B `Seq2` vs 4C VM, **same leaf
  substrate, same grammar**, sample-interleaved, N≥200, two windows. Until it exists, Q-runtime is inference.
- *Research must answer:* (a) does parse-that's **runtime** combinator API reach 4B monomorphization at all,
  or only via the generator's codegen path (RF-1)? — the tower's middle. (b) How large is the `Opaque` barrier
  surface on value.js/latex-paper (Q-A2)? (c) Apply PROTO-1's refutation to 4A-TS explicitly and state the
  emitter-lowering consequence.
- *Withheld from researchers:* do not carry "4A is the Rust lowering source of 4B" as settled — it is
  SPEC-ONLY and RF-1-suspect. Do not carry the three-way fork as measured. Hand them the narrow settled fact
  (graph is a live near-bijection, e-graph consumed not cloned) and the open keystone.

---

## 4B — COMPTIME-FUSION · ADVANCED · terminal-role BANKED→compile-backend + TS-emitter · 58%

**Floor rationale.** CRIT-1 60 / CRIT-2 58. Take **58** (conservative floor). CRIT-1's +2 credits the
mechanism-class win on both faces; CRIT-2 holds it at 58 for the same reason it is capped — the two faces are
*different mechanisms* and the terminal cannot freeze ahead of LOCK-6. 58 is the defensible floor: the biggest
single family lift in the campaign, but three named caps keep it below convergence.

**What is NEWLY EARNED — do not re-litigate.** 4B now has measured support on **both** language faces:
- **Rust (confirmed pass-1):** monomorphized `Seq2/Alt2` structs remove `SmallBox<dyn>` box-dispatch →
  **16–27%** over the boxed closure core, vtable `blr` **provably gone** (0 vs 60, disasm-gated), N=250 A,B,A,B
  interleaved, two windows, both orders, recursion proven (0 boxes via monomorphic free-fn recursion).
- **TS (confirmed pass-2, PROTO-1):** a build-time-emitted specialized closure beats the `.parser`-IC baseline
  **×1.23** (three cold processes, both orders, **0** `.parser(` in the emitted body), and in the same
  experiment refuted the runtime-decoder rival at **−22%**. This validates the *mechanism-class* of the TS
  fast face and resolves INCOMPAT-2 in the emitter's favour.

**The reframe (unchanged, now doubly evidenced).** The Rust win is *monomorphization* (no V8 analog); the TS
win is *emission* (no Rust analog) — **two different mechanisms**. So 4B is **not** a cross-language isomorphic
terminal; it is a Rust compile-backend PLUS a per-language TS emitter. A confirmed win on an un-mirror-able
mechanism *sharpens* constitution-iii rather than resolving it. **Terminal formulation BANKED by evidence;
the live formulation is 4B-as-Rust-compile-backend + 4B-TS-as-the-shared-TS-execution-face** (since every TS
terminal in the lock must lower through the emitter — §CROWDING).

**Exact open gaps.**
1. **[general emitter UNBUILT] PROTO-1 hand-emitted `new Function` for ONE arity-7 region.** The deliverable —
   a grammar-driven build-time fuser specializing *arbitrary* combinator regions (recursion, alt-dispatch,
   packrat, diagnostics, **arbitrary host closures**, the full value.js/latex-paper/keyframes surface) — is
   SPEC-ONLY. The gap between "one hand-emitted closure ×1.23" and "the TS backbone's automatic specializer"
   is the actual work. G-B3 met for one region only; three-consumer parity / bundle-size / tsc-clean untouched.
2. **[GAP-1 dodged] The emitter probe's projection was MapExpr-expressible.** It never crossed the
   arbitrary-host-closure wall (`FnDescriptor` has **no** `HostClosure`, code-verified) — the whole lock's
   value-plane hazard on the TS side. Unmeasured.
3. **[F-3, unmoved] Production dilution unmeasured.** G-B1 (fused struct vs the *shipped* `json_parser()` with
   `dispatch_byte_multi` + SIMD on **both** arms) is unbuilt; the 16–27% stays an upper bound on a *stripped*
   grammar; "beyond-SOTA" vs the tuned incumbent is unestablished.
4. **[G-B4, gate-that-cannot-fail, unset across TWO passes] The fusion-policy / monomorphization-blowup budget
   has no threshold.** A *pluggable* {inline-monomorphize | enum-node | box-here} cost model with named
   fuse-arity / dispatch-fanout / boxing-depth caps and a named `.rlib`/`dist` + incremental-compile budget on
   CSS **and** value.js — restated verbatim, number still unset. A gate that cannot fail is not a gate.
5. **[Q-B2, the plan artifact] Is the fused region derivable from a *serializable plan* that also drives the
   TS emitter (⇒ 4B is the Rust face of a crossing terminal), or intrinsic to the Rust type system (⇒
   standalone)?** The deciding evidence (one plan driving both Rust variant-selection and the TS emitter to
   byte-identical parses) was NOT built. Without it, "4B is the Rust lowering of the IR-plan" is a working
   disposition, not a demonstration — and it collides with RF-1 (the combinator API's runtime IR cannot feed
   monomorphization directly).
6. **[extant-tranche, un-reconciled] 4B's confirmed Rust perf claim vs PT-2's PROVE-09 "ZERO perf claim"
   posture.** 4B files a *new* perf claim on a *different* axis (indirect-call, not SIMD/arena); it must be
   reconciled against PT-2's stability-only ledger as a *named* supersession. No PT-tranche home named. Owed.

**Pass-3 directives.**
- *Prototype (the general emitter — the deliverable, not one region):* generalize `fuseAll`/`new Function`
  past the hand-rolled arity-7 into a **grammar-driven vite/ts-transform** that specializes arbitrary
  combinator regions from the shared op-list; grep every emitted region for `.parser(` (must be 0); isolate
  dispatch- from allocation-elimination (PROVE-11 bounds dispatch ~+5%); 124/124 parity; bundle-size + tsc-clean.
- *Prototype (through GAP-1, the wall both probes dodged):* run the emitter over a region containing an
  **arbitrary host closure**, not a MapExpr-expressible projection. This is the value-plane hazard the whole
  lock must survive on the TS side.
- *Prototype (production dilution — mandatory, converts mechanism→deliverable):* fused struct vs the *shipped*
  `json_parser()` with `dispatch_byte_multi` + SIMD on **both** arms; report the shrink (G-B1).
- *Prototype (the three-way Rust bench):* 4B `Seq2` vs 4A extracted-closures vs 4C VM, SAME grammar + leaf
  substrate — measure FORK II, don't infer (shared with §4A directive).
- *Research must answer:* the fusion-policy value that bounds monomorphization blowup on the full consumer
  surface (give G-B4 a number); and Q-B2's plan-artifact observable (does one serializable plan drive both
  lowerings?).
- *Withheld from researchers:* do not seed "both faces confirmed ⇒ pick 4B as the terminal" — the two faces
  are different mechanisms (no isomorphic terminal); the emitter is one hand region, not a production fuser;
  production dilution + beyond-SOTA are unestablished. Hand them the confirmed *mechanism-class on both faces*,
  the compile-backend + TS-emitter reframe, and the open axes.

---

## 4C — VM-CORE / PORTABLE-PROGRAM ISOMORPHISM · BANKED · 32%

**Floor rationale.** CRIT-1 48 / CRIT-2 32. Take **32** (conservative floor; CRIT-1's 48 is generous on the
same facts). *Why the floor, not 48:* both critics agree the terminal is BANKED and both agree the *risen*
contribution — the byte-identical portable-program **iso-of-record oracle** (G-C1) — has **zero running
evidence for its actual claim** (crossing correctness). CRIT-1 §RF-2 states this as sharply as CRIT-2 does
("the axis that survives has zero running evidence … the axis that dies got a gold-plated measurement"). A
family whose one differentiating survivor is unbuilt-of-record does not converge to 48 on a
value-completion-was-built credit alone. 32 is the honest floor.

**What is EARNED (do not re-litigate).** (a) The terminal-runtime-on-V8 bid is now **measured-refuted twice**
— PROTO-1 switch-VM −22% vs emitter (ties naive), PROTO-2 interpret 0.021–0.46× vs closures — moving 4C from
inferred-BANKED to *evidence*-BANKED. (b) The value-completion (G-C2) is **built + byte-identical** (the same
`Op::ApplyFn` + `fns` + `Value`-scalar fix 4C shares verbatim with 4D; oracle GREEN). (c) The dependency-homing
spike (G-C3) is satisfied by PROTO-2's gate-0 + negative control. (d) 4C's bytecode VM **is** `bbnf_ir`'s live
`vm/` (a private second ISA is an abrogate-before-patch violation, code-verified) — the LOCK-3 double-home is
retired on route.

**Why BANKED (the crowding, hardened by measurement).** As a *fast TS runtime* 4C is refuted-prior→refuted-
datum: the switch-decoder survives only as a **portability/size** face (98 bytes, ties naive), never the fast
face. Its mechanism *is* the extant VM. So 4C does not advance as an independent terminal-runtime family — its
value-completion + iso-layer fold into the LOCK-3/`bbnf_ir` VM line.

**What is BANKED (preserved, RISEN, and now the largest unbuilt-cheap experiment in the lock).** The
byte-identical **portable-program isomorphism** — the program crosses *as data* (serialize in Rust, decode in
TS), so isomorphism is a property of the representation, not a mirrored-source linter. This RISES because the
4B reframe makes isomorphism the binding constraint: it is the correctness bridge *every* isomorphic terminal
needs. **But PROTO-1 did NOT build it** (hand-faked blob, 3-lane self-identity — perf answered, isomorphism-of-
record unanswered). This is the pass-2 evidence-misuse to guard: PROTO-1's PASS is a *perf* result and must
not be read as iso-layer evidence.

**Residual open gaps (carried into the banked line).**
1. **[RF-2, the surviving axis is UNTESTED] G-C1 the dual iso oracle is UNBUILT** — decoded `Op[]`
   structurally == a **live Rust `to_vec_named` golden** (not a hand-faked blob) AND TS `Value` tree
   node-for-node == Rust `Interpreter` over six corpora. 4C's risen contribution has zero running evidence for
   crossing correctness. Now the *most* valuable unbuilt-cheap experiment in the lock.
2. **[G-C4] Wire-contract regen-idempotence** (encode∘decode∘encode = encode, variant tags pinned) unbuilt.
3. **[cross-lock double-home, NEW, un-adjudicated] The value-completion PROTO-2 built lands LOCK-3/LOCK-7 work
   inside `crates/ir` under a LOCK-4 probe.** `Op::ApplyFn` + `apply_fn.rs` MapExpr evaluator + `Value` scalar
   arms directly dispatches internal descriptors (Hex/Number/Enum) — is that LOCK-3's IR-VM lowering contract
   or LOCK-7's host-fn lattice? GESTALT-Ω must name the home; the specs leave it home-ambiguous.
4. Left-recursion seed-growth parity rust/ts (research-C R4) untested — relevant only if the VM line proceeds
   via `bbnf_ir`'s memo.

**Pass-3 directive.** Do NOT spin an independent 4C terminal thread. **Build the iso oracle for real (G-C1) —
the single owed 4C artifact:** a *live* Rust `to_vec_named` serialize → TS ~200-LOC switch-loop decode → dual
oracle (opcode-stream identity + `Value`-tree identity vs the Rust `Interpreter`) over the six R3 corpora. This
is simultaneously the 4D-TS **correctness** graft (not the fast graft — that is the emitter). Then GESTALT
adjudicates the value-plane home (LOCK-3 vs LOCK-7, gap 3).

---

## 4D — GRAMMAR-SUGAR / SHARED-IR OWNERSHIP · BLOCKED (confirmed) · 30%

**Floor rationale.** CRIT-1 45 / CRIT-2 30. Take **30** (conservative floor; CRIT-1's 45 errs generous).
*Why the floor:* CRIT-1's 45 counts "characterization near-complete" as high convergence for a BLOCKED family
— but the decisive Rust number is a **measured LOSS**, and its viability rests on the *unmeasured* "amortize
`Interpreter::new` setup" hypothesis, which CRIT-2 correctly refuses to credit ("a re-attribution, not a
rescue; a NEW unmeasured claim for pass-3"). A BLOCKED family with a loss-of-record and an unmeasured rescue
hypothesis converges at CRIT-2's 30, not CRIT-1's 45.

**What ADVANCED this pass (real).** Gate-0 (the Cargo cycle break) RAN and **PASSED** with a negative control —
the strongest single 4D result in the campaign; value-completion is **built + byte-identical**; the value-plane
floor (F-4, 4D's sharpest pass-1 risk) is now **measured** (negative, but attributed).

**The block (confirmed, unmoved, sharpened).** 4D's constitutional rust/ts isomorphism rests on a TS-execution
primitive with three horns, all now hardened:
- **Horn (i):** full `GrammarIR` + interpreter + optimizer port to TS = re-implementing the grammar plane's
  engine in a second language — a primitive of *equal-or-greater difficulty than the original LOCK-4 problem*.
  BLOCKED-candidate by the rule's own text.
- **Horn (ii):** the wasm crossing — PROVE-14 measured full-walk value materialization at **0.91×, a LOSS** on
  exactly the workload a value-producing combinator parser IS. A measured non-goal.
- **Horn (iii):** the shared-decoder graft — now measured-adverse (PROTO-1 switch-decoder −22% V8); its *fast*
  form needs the build-time emitter, i.e. the same per-language primitive. So the shared program crosses as
  data (correct, isomorphic) but is **slow to run on V8**; the fast form is per-language emission. **4D's
  entire thesis "one VM, two sides, for free" is refuted for the fast path.**

**Exact open gaps.**
1. **[block, unmoved + sharpened] The fast-isomorphic-TS primitive is the per-language emitter (=4B-TS).** All
   three horns stand/hardened. 4D keeps its correctness/parsimony story and loses its performance story.
2. **[RF-3 / INCOMPAT-1 framing corrected] Q-ownership is UNBLOCKED, not DECIDED — and gate-0 is *mechanical*,
   not "HARD BUILD FAILURE until LOCK-5 rules."** CRIT-2 §4.2: `parse_that::regex` is literally `pub use
   bbnf_regex::*`, so the `Dfa` already lives in `bbnf_regex`; gate-0 is a **4-line re-export swap, DONE**, and
   needs no LOCK-5 ruling. What LOCK-5 actually decides is the *semantic* question — is `bbnf_ir` genuinely the
   shared HIR (**5A**) or a lawful functor across separate HIRs (**5B**, which evaporates "inherit for free")?
   Real and unconverged (Q-D2). **Split the two: gate-0-compiles = mechanical, done; shared-substrate-semantics
   = LOCK-5, open.** (This corrects the pass-1/spec "entire axis decided/HARD FAILURE" overstatement.)
3. **[RF-4 / F-4, LOSS of record] The interpret terminal is measured-dead** (~2.2× even amortized, even vs a
   best-case zero-alloc closure). "The value plane rides ~free / dominant gap is setup allocation" is honest
   *attribution* but NOT a rescue — "viability turns on amortizing setup" is unmeasured. `Op::ApplyFn`'s real
   consumer is the slow interpret path (LSP/DAP, dynamic grammars) — legitimate but slow-only. Q-D1 closes
   NEGATIVE at these sizes.
4. **[GAP-1 uncrossed] `FnDescriptor::HostClosure` typed shape unbuilt.** PROTO-2 built a `MapExpr` evaluator
   over *existing analyzable* descriptors (hex→U32, MapExpr-expressible) — it dodged the arbitrary-host-closure
   barrier. LOCK-7 owns the typed shape; never inline a `Box<dyn Fn>` into the node (breaks the msgpack bus).
5. **[hard predecessors] LOCK-5 (5A vs 5B) AND LOCK-7 (host-fn lattice for GAP-1/4/5) both unconverged.** DAG
   fact: 4D cannot be *selected* until both land, even with gate-0 done and the correctness story built.

**Pass-3 directives.**
- *Prototype (test the rescue hypothesis — do NOT carry "viable" without it):* eliminate/pool `Interpreter::new`
  per-parse setup (arena-reuse the 8 Vecs + FxHashMap) and re-run the value-plane floor. Measure whether the
  0.46× amortized ratio actually closes toward 0.98× — the unmeasured and-then-the-hard-part PROTO-2's
  re-attribution rests on.
- *Prototype (the correctness graft, NOT the fast graft):* the 4C iso oracle (G-C1) drives 4D's shared
  `bbnf_ir` program through a TS decoder byte-identically — this unblocks **correctness/parity**, not throughput
  (the fast TS face is the emitter). Name the distinction explicitly.
- *Prototype (through GAP-1):* extend the value plane with a `FnDescriptor::HostClosure(FnId)` + `fn_table`
  variant and a paired regression lane — the arbitrary-host-closure wall both pass-2 probes dodged.
- *Research must answer:* LOCK-5's substrate ruling (5A vs 5B) and LOCK-7's host-fn lattice shape — hard DAG
  predecessors; report their shape, do not resolve inside LOCK-4.
- *Withheld from researchers:* do not seed "4D dominates on parsimony/isomorphism-readiness ⇒ pick it" —
  dominance is *conditional* on LOCK-5 ruling 5A, and its Rust runtime is a **measured loss**. Do not carry
  "value plane rides free ⇒ interpret is viable" (the measured floor LOST). Hand them the block (three horns),
  the loss-of-record, the corrected gate-0 framing (mechanical, done), and the two predecessors.

---

## CROWDING & REDIRECT (pass 2)

The pass-2 measurements collapse the apparent four-way rivalry further and add ONE decisive new convergence:

- **[NEW, decided by PROTO-1] EVERY TS terminal converges onto the build-time emitter (4B-TS).** The runtime-
  switch decoder is refuted on V8 (−22%, ties naive). Both critics extend this: it refutes not only the
  4C-decoder and 4D-decoder as fast faces but **also 4A-TS's `switch(node.tag)` graph interpreter** (same
  runtime-tag-switch shape). **Redirect:** the *runtime-decoder-as-fast-TS-face* is BANKED-by-measurement
  across 4A/4C/4D; the single live TS execution face is 4B-TS's emitter. The decoder survives only as an
  isomorphism/correctness oracle, never the fast runtime.
  **Underexplored formulation to OPEN in its place (the wall every winning formulation must survive):** the
  **general grammar-driven emitter *through an arbitrary host closure* (GAP-1)**. Both PROTO-1 and PROTO-2
  dodged it (MapExpr-expressible projections). No family has emitted, or run a value plane through, an
  arbitrary host closure. This is the value-plane hazard on the TS side and the untested core of the emitter's
  production form.
- **[NEW, RF-1 — the tower's incoherent middle to OPEN, not close] Does the combinator API reach 4B
  monomorphization at all?** The confirmed 4B-Rust win is comptime type-monomorphization; the combinator API
  builds a runtime `IrNode`. The `4A/4D-graph → 4B-mono` arrow is SPEC-ONLY and possibly realizable only via
  the generator's codegen-reflection path. **Open this as an explicit pass-3 question** — it is the load-bearing
  step the tower reads as free.
- **[carried] The bytecode VM is ONE mechanism in three homes** (4C-private ≈ 4D-compile-mode ≈ `bbnf_ir`'s
  live `vm/`). Redirect stands: 4C's *private-VM-as-terminal* is the redundant one → BANKED; opened in its
  place is the byte-identical iso oracle over `bbnf_ir`'s bytecode (RISEN; still the 4D-TS **correctness** graft).
- **[carried] The tree-walk graph is ONE mechanism in two homes** (4A-private-node-union ≈ `parse_with_ir`).
  Redirect: run 4A's experiments on `bbnf_ir::IrNode` where the seam allows; hold the private union pending
  LOCK-5. **The e-graph double-home is RETIRED on route** (code-verified path-dep, not a clone) — a genuine
  pass-2 gain, conditional on electing the shared route.

## CROSS-POLLINATION (named grafts — both families developed enough; graft justified)

1. **4C ⊗ 4D — the portable-program TS *correctness* oracle (PRIMARY for parity; scope corrected this pass).**
   Drive 4D's shared `bbnf_ir` bytecode program through 4C's small TS switch-loop decoder — the program crosses
   as msgpack data (confirmed at `bbnf_ir::lib.rs`). **Corrected scope:** this graft supplies *isomorphism-of-
   record + parity*, NOT the fast TS terminal (PROTO-1 refuted the decoder as the fast face). Justified: 4C's
   iso mechanism ≡ 4D's crossing need; the value-completion is *identical* for both (same `Op::ApplyFn` + `fns`
   + `Value`-scalar fix). Highest-leverage *correctness* graft; it does NOT unblock 4D's performance story.
2. **4B-TS emitter ⊗ {4A, 4C, 4D} — the shared TS execution face (NEW, forced by PROTO-1).** Every TS terminal
   must lower through the build-time emitter. Justified: PROTO-1 measured the runtime-decoder −22% and the
   emitter ×1.23; the emitter is the only V8-survivable fast face, so whichever isomorphic terminal wins adopts
   4B-TS as its TS lowering — exactly as 4B-Rust is the Rust lowering.
3. **4A ⊗ 4B — graph front, monomorphized Rust lowering (CARRIED, now RF-1-gated).** 4A's graph is the
   isomorphic front; 4B's confirmed monomorphization is its Rust lowering. **Gate:** RF-1 must first answer
   whether the *runtime* combinator graph can feed *comptime* monomorphization without the generator's codegen
   path. Until answered, this graft is coherent only for the generator front, not the combinator API.
4. **4A ⊗ 4D — fusion-as-a-pass on the shared union (SECONDARY).** 4A's `fuse(Graph)→Graph`, IF it must exist,
   runs on `bbnf_ir::IrNode` and consumes `bbnf_ir`'s `egraph/`+`passes/`, not a private clone. Justified:
   `IrNode` is the verified near-bijection of 4A's node set.

The grafts converge on a small pass-3 keystone set (below).

## THE PASS-3 KEYSTONE SET

1. **4A's `fuse(Graph)→Graph` byte-identity oracle (G-A1)** — the one differentiator SPEC-ONLY for three
   passes; on a *combinator-constructed* graph.
2. **The three-way common-baseline Rust fork (INCOMPAT-3 / RF-5)** — 4A-interpret vs 4B-`Seq2` vs 4C-VM, one
   leaf substrate, one grammar. INFERRED for three passes; the Q-runtime fork is undecided until it runs.
3. **The 4C cross-language iso-of-record oracle (G-C1)** — a *live* Rust `to_vec_named` → TS decode → dual
   oracle over six corpora. PROTO-1's perf PASS is not this.
4. **The general grammar-driven emitter, through GAP-1** — past one hand region, through an arbitrary host
   closure (4B-TS deliverable + the wall both probes dodged).
5. **4B production dilution (G-B1) vs the shipped `json_parser()`; and give G-B4 a number.**
6. **The 4D value-plane rescue test** — eliminate `Interpreter::new` per-parse setup and re-measure; do NOT
   carry "viable" without it.
7. **RF-1 answered** — does parse-that's combinator API reach 4B monomorphization, or only via the generator's
   codegen path? The tower's middle.

## CROSS-LOCK DAG ORDERING (for GESTALT — pass 2)

- **[FORCED, re-confirmed] LOCK-4 ⊗ LOCK-6 are one joint decision.** PROTO-1's confirmed monomorphization/
  emitter split (Rust mechanism ≠ TS mechanism, no shared isomorphic terminal) is a **standing counterexample
  to source-level rust/ts isomorphism for the combinator core** — proper *evidence fed to* LOCK-6, forcing
  **6C (wasm-core)** or **6D (behavioral/oracle iso, layout free per language)** and away from 6A/6B source-
  mirroring. The terminal cannot freeze ahead of LOCK-6's verdict.
- **[CORRECTED framing] LOCK-4 ⊗ LOCK-5 — Q-ownership.** Split: **gate-0-compiles = mechanical, DONE** (4-line
  re-export swap; PROTO-2 + negative control). **Shared-substrate-semantics = LOCK-5, OPEN** (5A shared type vs
  5B lawful functor; if 5B, 4D's "inherit for free" evaporates). The pass-1 "HARD BUILD FAILURE until LOCK-5
  rules" is overstated and corrected here.
- **[NEW double-home, un-adjudicated] LOCK-4 ⊗ LOCK-3/LOCK-7 — the value-plane home.** The value-plane
  completion PROTO-2 built (`Op::ApplyFn` + `apply_fn.rs` MapExpr evaluator + `Value` scalar arms) is IR-VM
  lowering (LOCK-3 3A contract-tower) + host-fn dispatch (LOCK-7 7C lattice) landed in `crates/ir` under a
  LOCK-4 probe. GESTALT-Ω owes an adjudication of *which lock homes the value plane*; the specs assign
  HostClosure's typed shape to LOCK-7 but leave the internal-descriptor evaluator (Hex/Number/Enum) ambiguous.
- **[NEW, route to avoid re-derivation] LOCK-4 ⊗ LOCK-8 — the emitter's V8 discipline.** PROTO-1's "fast TS
  terminal = build-time emitter" (hidden classes, IC elimination, `new Function` lexical-name dispatch) is
  LOCK-8/8A/8C territory. 4B-TS and LOCK-8's core are one implementation (a double-home GESTALT-P must
  collapse); route PROTO-1's result to LOCK-8 so it is not re-derived.
- **[soft predecessors] LOCK-9 leaf substrate** underlies every "same leaf on both arms" gate (three-way fork,
  G-B1 production dilution). **PROVE-14** (LOCK-6/6C seed) is a measured non-goal against 4D's wasm horn.
- **[extant-tranche, owed] 4B's confirmed perf claim vs PT-2's PROVE-09 zero-claim posture** — un-reconciled;
  the value-plane completion has no named PT-tranche home. GESTALT hand-off owes the supersession.

---

## PASS HISTORY

### Pass 2 (2026-07-16) — TWO prototypes RAN; the sharper ran NEGATIVE; the TS face is measured-decided

**Events.** PROTO-1 (TS execution face) RAN/MEASURED/**PASS** — build-time emitter ×1.23 over `.parser`-IC
baseline (0 `.parser(` emitted), runtime switch-decoder −22% (ties naive); INCOMPAT-2 decided *by
measurement* → **emitter, not decoder**; SpanParser law survives. PROTO-2 (cycle-break + value floor) RAN —
**gate-0 PASS** (cycle mechanically severable, negative control load-bearing) and value-plane floor
**MEASURED-NEGATIVE** (0.021–0.46×; oracle byte-identical GREEN; the loss attributed to per-parse setup
allocation, not the value plane).

**Two corrections folded from the critics.** (i) CRIT-1 RF-1 — the tower's `4A→4B` "Rust lowering" arrow is
SPEC-ONLY and possibly incoherent (monomorphization needs comptime types; the combinator API builds a runtime
`IrNode`); the confirmed 4B-Rust win is on a mechanism the tower's own front cannot feed. (ii) CRIT-2 §0.1 —
PROTO-2's value floor RAN NEGATIVE on its own rubric; value-completion unblocks *correctness*, NOT a
competitive Rust runtime; "viability turns on amortizing setup" is a NEW unmeasured hypothesis, not a
discharged gap. Also folded: gate-0 is *mechanical/done*, not "HARD FAILURE until LOCK-5" (CRIT-2 §4.2); the
runtime-switch refutation also refutes 4A-TS (CRIT-1 gap 5 / CRIT-2 §6.4); a NEW LOCK-3/LOCK-7 value-plane
double-home (CRIT-2 §6.2); route the emitter finding to LOCK-8 (§6.5).

**Critic floors folded.** CRIT-1 (within-lock): 4A 50 / 4B 60 / 4C 48 / 4D 45. CRIT-2 (cross-lock): 4A 50 /
4B 58 / 4C 32 / 4D 30. **Dispositions (conservative floor):** 4A **ADVANCED 50** (role clarified; keystone
still 0% built three passes; RF-1 + 4A-TS refutation new-adverse); 4B **ADVANCED 58 · terminal-role
BANKED→compile-backend + TS-emitter** (both faces mechanism-confirmed; two faces are different mechanisms —
no isomorphic terminal; general emitter/dilution/budget open); 4C **BANKED 32** (terminal measured-refuted
twice; value-completion built + byte-identical; but the RISEN iso-oracle is unbuilt-of-record — floored at
CRIT-2's 32, not CRIT-1's generous 48); 4D **BLOCKED 30** (gate-0 PASS + value-completion byte-identical, but
the Rust number is a LOSS of record and the fast-TS block is sharpened — floored at CRIT-2's 30, not CRIT-1's
45).

**Corrected disposition.** No family converged; the terminal is un-electable inside LOCK-4. Shape-change from
pass 1: the TS execution face is now **measured-decided** (emitter) and gate-0 is **feasibility-proven** — the
two "0% built / unresolved" items pass-1 named are answered as mechanism/feasibility. What remains: the
*election* (exported to LOCK-5 ownership-semantics + LOCK-6 iso-verdict) and three unbuilt keystones (4A
fusion pass; the three-way common-baseline fork; the 4C iso-of-record oracle) — plus the newly-opened wall
(the general emitter *through GAP-1*) and the newly-opened incoherence (RF-1: does the combinator API reach
monomorphization at all?).

### Pass 1 (2026-07-16) — agglomerated, then RE-FOLDED after PROTO-2-4B-FUSION + MONADIC-WALL-CENSUS

**Re-fold event.** The first agglomeration (4A 50 / 4B 32·REWARD-AT-RISK / 4C 30·BANKED / 4D 25·BLOCKED) was
written before the lock's deciding throughput experiment ran, on the premise that 4B's inlining number was
"deferred / zero evidence." PROTO-2-4B-FUSION then RAN and CONFIRMED (0 fused `blr` vs 60 closure, 1.16–1.27×
cold-paired A,B,A,B interleaved N=250, two windows, both orders, load-symmetric, equivalence + recursion
proven); PROTO-1-MONADIC-WALL-CENSUS RAN GREEN (opaque-dynamic 0.525% ≤ 2%; hot path monad-free — a
*structural* floor only, the value plane untouched, F-4). Both refreshed critics re-folded them.

**Pass-1 re-folded floors.** CRIT-1: 4A 45 / 4B 50 / 4C 28 / 4D 25. CRIT-2: 4A 48 / 4B 50 / 4C 28 / 4D 27.
**Dispositions:** 4A **ADVANCED 45** (charter runtime measured-inferior to 4B — F-2; keystone unbuilt; leads
iso-readiness); 4B **ADVANCED 50 · terminal-role BANKED→compile-backend** (mechanism confirmed; un-isomorphic;
throughput axis closed and never the deciding one); 4C **BANKED 28** (terminal doubly crowded; iso-oracle
grafts to 4D); 4D **BLOCKED 25** (TS-execution primitive; value-model tax untouched by the census).

**Pass-1 corrected disposition.** The throughput half of FORK II resolved (→ 4B for Rust, as *inference* over
a three-way fork), so the deciding open axis became **isomorphism (0% built) + the Q-ownership/substrate
ruling (LOCK-5)** — NOT throughput. The reframe (not the number) was the finding: 4B's win is Rust-
monomorphization with no V8 analog, re-casting 4B as the Rust *compile backend* of an isomorphic terminal and
forcing LOCK-6 away from source-mirroring toward 6C/6D. Pass-2 (above) then measured the TS face and the
value-plane floor, confirming the reframe and hardening the block.
