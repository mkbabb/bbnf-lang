# LOCK-4 — THE COMBINATOR CORE · PASS-2 · ADVERSARIAL CRITIC 2 of 2 (cross-lock weight)

*Critic 2, pass 2, cross-lock-weighted. Read: PORTFOLIO.md (LOCK-4 charter + CONSTITUTION),
pass-2 FAMILY-SPECS, research-{A,B,C,D,priorart}, PROTO-1 (TS execution face, RAN), PROTO-2
(cycle-break + value-plane floor, RAN), pass-1 CRIT-2. Four substrate claims INDEPENDENTLY
re-verified against live HEAD this pass (not taken from the specs): (1) `crates/ir/Cargo.toml:13`
= `parse_that = "0.4"` — the cycle is real; (2) `vm/compiler/node.rs:31` = `IrNode::Map { inner,
.. } => self.compile_node(inner, ir)` — the Map-drop is real; (3) `fn_descriptor.rs` carries
EnumWrap/BoxWrap/NumberConvert/HexConvert/SpanCapture/Expr and **no** HostClosure/Opaque; (4)
`value.rs:54-64` = Span/Tagged/Array/Nil, **no** scalar arm on HEAD; (5) `Cargo.toml:19` path-deps
`egraph`, `csp-solver = "0.1"`. The specs' code facts hold — this is NOT a spec-cites-itself
substrate. Convergence EARNED; pass 2; no family converged.*

---

## 0. HEADLINE — TWO ARTIFACTS RAN; THE SHARPER ONE RAN *NEGATIVE*, AND THE REFRAME SPINS IT

Pass-2 is a genuine advance: unlike pass-1's stale-number problem, both PROTO-1 and PROTO-2 RAN,
are refutable, and are disciplined (paired-cold, interleaved, both orders, negative controls). The
tower reframe (4D=ownership+value-completion, 4A=analyzable front, 4B=Rust lowering, 4C=portable
crossing) is a **real mechanism convergence**, not a rhetorical one — the double-homes it retires
(4C-VM≡bbnf_ir-VM, 4A-fusion consumes the path-dep e-graph) are code-verified, and I confirm both.

**But the reframe carries a load-bearing inversion the specs under-state:**

1. **PROTO-2's value-plane floor RAN NEGATIVE on its own rubric.** BRIEF-2's pass/fail: *"FAIL-and-
   name-negative if the projection-dense ratio is <0.98× (closures win — 4D's honest close, F-4)."*
   Measured: **0.021×–0.46×.** The FAIL trigger **fired.** This is 4D's honest close on the value
   plane, per the brief's own text — yet the specs re-narrate it as a *mechanical debt that "should
   be built" because it "unblocks the Rust + correctness story."* The Rust interpret story PROTO-2
   measured is **2.2×–48× BEHIND closures.** Value-completion unblocks *correctness* (byte-identical,
   proven) — it does **not** unblock a competitive Rust runtime, and the disposition "should be
   built" reads a measured loss as a mechanical win (§4, elegant-reduction).

2. **The two irreducible forks are BOTH externalized to other locks.** The tower "converges the
   mechanisms; the two forks are Q-ownership (LOCK-5) and the TS-fast terminal (LOCK-6)." Restated
   plainly: **LOCK-4 cannot elect a production terminal internally** — the two decisions that pick
   it are exported. That is honest DAG accounting, but it means LOCK-4's convergence ceiling is
   *bounded by two external verdicts*, and the lock-level "tower converged" framing must not be read
   as terminal-convergence (§6.1, vacuous-adjacency).

PROTO-1 is a clean PASS and I do not dispute it; but what it validated is narrower than the specs'
"converges 4C-TS and 4D-TS onto 4B-TS" (§1.1 below).

---

## 1. FAMILY 4A — CLOSURE-TO-GRAPH · **convergence 50%** (pass-1 crit-2: 48%)

**Why essentially flat.** The reframe to "source representation, not terminal" is sound and the
substrate story strengthens (e-graph consumed via `Cargo.toml:19` path-dep — verified; fusion passes
exist at `passes/transform/`). But **no pass-2 prototype touched 4A's one novel keystone.** PROTO-1
built a TS emitter; PROTO-2 built the VM value plane. Neither ran `fuse(Graph)→Graph`. So 4A's
single load-bearing claim is where pass-1 left it: an existence-argument from the passes existing,
never a run.

### Exact open gaps
1. **[keystone, STILL UNRUN] G-A1 `fuse(Graph)→Graph` byte-identical to a hand-fused arena is
   SPEC-ONLY after pass-2.** The specs upgrade the framing from "build a fusion pass from scratch"
   to "point the *existing* passes at a combinator-constructed graph and oracle byte-identity." No
   prototype pointed them. The upgrade is a re-description, not evidence. 4A's ONE differentiator
   remains unmeasured two passes in.
2. **[cross-family evidence UNDER-applied] PROTO-1 arguably REFUTES 4A's TS face and the specs do
   not apply it.** PROTO-1 measured a runtime `switch`-VM at **−22% vs the emitter, ties naive
   baseline** on V8. 4A's TS terminal is a graph-walking `switch(node.tag)` interpreter — *the same
   runtime-tag-switch shape.* The specs apply PROTO-1's refutation to 4C/4D but leave 4A-TS labeled
   "unmeasured." By its own evidence, a 4A-TS runtime tag-interpreter inherits the SpanParser
   penalty; 4A-TS must ALSO lower through the build-time emitter (i.e. 4A-source → 4B-emitter), which
   collapses 4A-as-terminal on the TS side exactly as it does 4C/4D. This consequence is unstated.
3. **[unmeasured] Opaque host-closure barrier surface (Q-A2).** The number that decides whether the
   analysis story holds on value.js/latex-paper — still uncounted. PROTO-1's projection was a
   *MapExpr-expressible* `{r,g,b}` inline, i.e. it dodged GAP-1 (arbitrary host closure), so it says
   nothing about the barrier.
4. **[unbuilt] The three-way common-baseline fork (INCOMPAT-3) was NOT built.** 4A-interpret vs
   4C-bytecode vs 4B-monomorphized on one leaf substrate / one grammar — the experiment pass-1 CRIT-2
   §1.2 demanded — is still unbuilt. PROTO-2 ran 2 arms (VM vs a closure *stand-in*, not 4B). The
   Q-runtime fork stays INFERRED.
5. **[cross-lock, conditional] The e-graph and node-union double-homes are retired ONLY on the shared
   route (b).** I verify `egraph` is a path-dep client, not a clone — good — but that retirement is
   contingent on electing INCOMPAT-1's shared route, itself gated. On route (a) 4A re-clones both.
   The convergence cannot bank the retirement unconditionally.

---

## 2. FAMILY 4B — COMPTIME-FUSION · **convergence 58%** (pass-1 crit-2: 50%)

**Why up.** 4B now has measured support on *both* faces: pass-1's disasm-gated Rust win (16–27%, 0
`blr`) AND pass-2's PROTO-1 confirming the TS-emitter *mechanism-class* beats the runtime switch on
V8 (×1.23, zero `.parser(` in the emitted body). 4B is the only family with a running artifact on
each language. It remains the strongest-substantiated route.

**Why capped at 58% — the reframe's own crisis, unresolved.** 4B's Rust win is *monomorphization*
(no V8 analog); its TS win is *emission* (no Rust analog). These are **two different mechanisms**, so
4B is **not** a cross-language isomorphic terminal — it is a Rust compile-backend PLUS a per-language
TS emitter. A confirmed win on an un-mirror-able mechanism *sharpens* constitution-iii rather than
resolving it (pass-1 §6.1, unchanged and correct). The terminal cannot freeze ahead of LOCK-6.

### Exact open gaps
1. **[PROTO-1 scope] The measured emitter is a hand-rolled `new Function` over ONE arity-7 region,
   not a grammar-driven build-time fuser.** PROTO-1 validates the mechanism *class* (build-time
   specialization > runtime switch on V8) — a real, decisive result — but NOT that a production
   emitter exists. The gap between "a `new Function` beats `.parser`-IC on one 7-tuple" and "a
   vite/ts-transform that handles recursion, alt-dispatch, packrat, diagnostics, **arbitrary host
   closures**, and the full value.js/latex-paper/keyframes surface" is the entire unbuilt 4B-TS.
   PROTO-1's projection was MapExpr-expressible — it dodged the host-closure wall (GAP-1) that is the
   whole lock's value-plane hazard.
2. **[G-B1, still unmeasured] Production dilution.** The 16–27% is an upper bound on a *stripped*
   grammar (no `dispatch_byte_multi`, no SIMD). The end-to-end shrink vs the shipped `json_parser()`
   with production kernels on both arms is still unquantified — pass-1 §2.2, un-addressed by either
   pass-2 prototype. Q-B1 (beyond-SOTA) is therefore unestablished.
3. **[G-B4, gate-cannot-fail, UNFIXED across two passes] The fusion-policy budget still has no
   threshold.** Pass-1 CRIT-2 §2.6 flagged it; pass-2 specs still say "currently a gate with no
   threshold." A pluggable {inline-monomorphize | enum-node | box-here} model with named arity /
   fan-out / boxing-depth caps and a named `.rlib`/`dist` + incremental-compile budget on CSS **and**
   value.js — unset. The `generated-size-budget` edict is cited, no number produced. A gate that
   cannot fail until a value is named.
4. **[Q-B2, the plan artifact] "Is the fused region a serializable plan (⇒ 4B is the Rust face of a
   crossing terminal) or intrinsic to the Rust type system (⇒ standalone)?"** — the deciding evidence
   (a working plan that drives both the Rust variant selection and the TS emitter to byte-identical
   parses) was NOT built. Without it, "4B is the Rust lowering of the IR-plan" is a working
   disposition, not a demonstrated one.
5. **[extant-tranche, un-reconciled] 4B's confirmed Rust perf claim vs PT-2's PROVE-09 "ZERO perf
   claim" posture.** Pass-1 §7 flagged that 4B files a *new* perf claim on a *different* axis
   (indirect-call, not SIMD/arena) and must be reconciled against PT-2's stability-only ledger as a
   *named* supersession. Pass-2 specs do not name the tranche home for it. Still owed (ix).

---

## 3. FAMILY 4C — VM-CORE / PORTABLE-PROGRAM ISO · **convergence 32%** (pass-1 crit-2: 28%)

**Why marginally up.** The value-completion RAN in PROTO-2 and its byte-identical `Value`-output
oracle PASSED — a real, high-signal correctness result. That is the ADVANCE.

**Why still low — the terminal is doubly refuted and the surviving contribution is UNBUILT-of-record.**
As a fast TS runtime, 4C is now measured-adverse twice: PROTO-1 (switch-VM −22% vs emitter, ties
naive) *and* PROTO-2 (shared-VM interpret 0.021×–0.46× vs closures in Rust). The specs correctly BANK
the terminal. The contribution that "RISES" — the byte-identical portable-program isomorphism layer —
is the honest survivor, but:

### Exact open gaps
1. **[the RISEN contribution is SPEC-ONLY of record] PROTO-1 did NOT build the cross-language iso
   oracle.** PROTO-1's blob was **hand-encoded in-harness** to the `to_vec_named` shape ("a live Rust
   serialize round-trip was outside the cap"), and its only equivalence check was **3-lane
   self-identity on one input** — not the dual oracle 4C's iso-layer needs (decoded `Op[]` ==
   Rust-emitted golden; `Value` tree == Rust `Interpreter`). So the one thing 4C keeps — isomorphism
   *of record* — remains unproven. The perf question was answered with a faked blob (legitimate for
   perf, as PROTO-1 argues); the isomorphism question was not answered at all. G-C1/G-C4 unbuilt.
2. **[cross-lock double-home, NEW, un-adjudicated] The value-completion PROTO-2 built lands
   LOCK-3/LOCK-7 work inside `crates/ir` under a LOCK-4 probe.** `Op::ApplyFn` + `apply_fn.rs`
   MapExpr evaluator + `Value` scalar arms is a new value-plane engine. LOCK-3 owns the IR-VM
   lowering contract (3A contract-tower); LOCK-7 owns the host-fn lattice (GAP-1 HostClosure typed
   shape). PROTO-2's evaluator directly handles HexConvert/NumberConvert/EnumWrap — is that LOCK-3's
   or LOCK-4's home? The specs note LOCK-7 "owns the typed shape" for HostClosure but the *built*
   evaluator already dispatches the internal descriptors. GESTALT-Ω must adjudicate the home; the
   specs do not name it.
3. **[terminal refuted, per §7] Do not import PROTO-2's Rust interpret ratio as a fatal fact for the
   *iso-layer* — but do not import PROTO-1's PASS as iso-layer evidence either.** The iso-layer is
   cheap and worth building; its existence proof is simply not among the pass-2 artifacts.
4. **[left-recursion seed-growth parity rust/ts] untested** (research-C R4) — relevant only if the VM
   line proceeds via bbnf_ir's memo; unmoved.

---

## 4. FAMILY 4D — GRAMMAR-SUGAR / SHARED-IR OWNERSHIP · **convergence 30%** (pass-1 crit-2: 27%) · BLOCKED confirmed

**Why marginally up.** Gate-0 (the Cargo cycle break) RAN and PASSED in PROTO-2 with a *negative
control* — the strongest single result for 4D in the campaign. I confirm the cycle is real on HEAD
(`Cargo.toml:13`), and PROTO-2's break (repoint `parse_that::regex::Dfa → bbnf_regex::Dfa`, delete
the dep) is sound. The value-completion also runs byte-identical (correctness floor met).

**Why still BLOCKED and low.** 4D's decisive Rust number is now MEASURED and it is a **LOSS**: the
value-completed interpret terminal is 0.46× closures even amortized (~2.2× behind a best-case
closure). The brief's FAIL-and-name-negative fired. And the block — the fast-isomorphic-TS primitive
— is *unmoved and now sharpened*: PROTO-1 shows the only fast TS face is the per-language emitter
(=4B-TS), so 4D's entire thesis ("inherit the VM for free on both sides") is refuted for the *fast*
path. The shared program crosses as data (correct, isomorphic) but is **slow to run on V8** (PROTO-1
A ties naive baseline); the fast form is per-language emission, not a shared decoder. 4D keeps its
correctness/parsimony story and loses its performance story.

### Exact open gaps
1. **[the block, unmoved and sharpened] The fast-isomorphic-TS primitive is the emitter, which is
   per-language — so 4D's "one VM, two sides, for free" is refuted for the fast path.** Horn (i)
   equal-difficulty engine port unretired; horn (ii) wasm 0.91× unretired; horn (iii) the decoder
   graft is now measured-adverse (PROTO-1). BLOCKED stands, correctly.
2. **[INCOMPAT-1 framing OVERSTATED] "The entire Q-ownership axis is decided by LOCK-5's substrate
   ruling" conflates two different things.** PROTO-2 shows the *build cycle* breaks with a **4-line
   mechanical swap** — because `parse_that::regex` is literally `pub use bbnf_regex::*`, the `Dfa`
   already lives in `bbnf_regex`; parse_that only re-exports it. So "gate-0 compiles" is DONE and
   needs no LOCK-5 *ruling.* What LOCK-5 actually decides is the *semantic* question — is `bbnf_ir`
   genuinely the shared HIR (5A) or a lawful functor across separate HIRs (5B, which evaporates
   "inherit for free")? That is real and unconverged (Q-D2). But it is softer than the specs'
   "HARD BUILD FAILURE until LOCK-5 rules." Separate: gate-0-compiles = mechanical, done;
   shared-substrate-semantics = LOCK-5, open.
3. **[value plane MEASURED-LOSS, re-narrated] The "dominant gap is per-parse setup allocation, not
   the value plane" decomposition is a re-attribution, not a rescue.** It may be true that
   `Interpreter::new`'s 8 `Vec`s + `FxHashMap` dominate, and that eliminating them would move the
   ratio — but that is a NEW, unmeasured claim. What is measured: the value-completed shared VM loses
   to closures. "Viability turns on amortizing setup" is a hypothesis for pass-3, not a discharged
   gap. F-4's floor is a LOSS of record.
4. **[hard predecessors] LOCK-5 (5A vs 5B) AND LOCK-7 (host-fn lattice for GAP-1/4/5) both
   unconverged.** DAG fact unchanged: 4D cannot be *selected* until both land.
5. **[GAP-1 unbuilt of typed record] `FnDescriptor::HostClosure` typed shape.** PROTO-2 built a
   `MapExpr` evaluator but did NOT add a HostClosure variant for arbitrary closures — it handled only
   the existing analyzable descriptors. The arbitrary-host-closure barrier (the specs' GAP-1) is
   still uncrossed; the projection PROTO-2 measured was hex→U32 (MapExpr-expressible), dodging it.

---

## 5. THE PROTOTYPES — HARDENED

### PROTO-1 (TS execution face) — RAN, PASS, disciplined; scope it precisely
- **Genuine and well-run.** Interleaved paired-cold, both orders, N=240×2 windows, 3 cold processes,
  A÷B pinned to ±0.8% — the SpanParser law survives, INCOMPAT-2 is answered. I do not dispute the
  ×1.23 / −22% numbers.
- **Scope gap A (§2.1):** the emitter is a hand-rolled `new Function` over one arity-7 region, not a
  grammar-driven fuser; "converges 4C-TS/4D-TS onto 4B-TS" is validated at the *mechanism-class*
  level only, not at production scale.
- **Scope gap B (§3.1):** the iso oracle (dual: opcode-stream identity + Value-tree identity vs a
  *real* Rust `BytecodeProgram`) was NOT built — the blob was hand-faked and self-compared. Perf:
  answered. Isomorphism-of-record: unbuilt. The specs' "the iso-layer RISES … unbuilt and cheap" is
  correct that it is *unbuilt*; PROTO-1 did not close it.
- **Scope gap C:** the projection was MapExpr-expressible, so PROTO-1 says nothing about GAP-1
  (arbitrary host closures), the exact wall 4B-TS/4D-TS must survive.

### PROTO-2 (cycle-break + value-plane floor) — RAN, one PASS one NEGATIVE; the negative is the finding
- **Gate-0 PASS is solid** (negative control proves it load-bearing) — and *cheaper than framed*
  (mechanical re-export swap, §4.2).
- **Value-plane floor RAN NEGATIVE (0.021×–0.46×).** Per BRIEF-2's own rubric this is 4D's honest
  close / F-4 negative. The specs' "should still be built — unblocks the Rust story" over-reads it:
  correctness is unblocked; a competitive Rust interpret runtime is not (§0.1, §4.3).
- **Honest caveat, correctly disclosed:** closure arm B is a hand-written zero-alloc *stand-in*, a
  best-case lower bound, not parse-that's actual generated combinator. Cuts both ways — the VM's loss
  might narrow against real closures, but that is unmeasured; the artifact establishes a LOSS against
  an idealized rival, which is the conservative direction for a "VM wins" claim.

---

## 6. CROSS-LOCK VIEW (critic-2 weight)

### 6.1 — [sharpest] The terminal is JOINTLY OWNED by LOCK-4 ⊗ LOCK-5 ⊗ LOCK-6 — LOCK-4 cannot converge a terminal alone.
The tower folds the mechanisms but exports both deciding forks: Q-ownership → LOCK-5's 5A/5B ruling
(shared type vs functor), TS-fast terminal → LOCK-6's 6C/6D verdict (wasm vs oracle-iso). PROTO-1's
confirmed monomorphization/emitter split is a **standing counterexample to source-level rust/ts
isomorphism for the combinator core** — proper *evidence fed to* LOCK-6, and the specs correctly do
not *render* the 6-verdict inside LOCK-4 (an improvement over pass-1's 4C "strictly stronger than any
LOCK-6 linter" over-reach, now absent). But the consequence is that LOCK-4's convergence ceiling is
bounded below 100% by construction until both external verdicts land. The lock must be *returned as a
rigorously converged core with its exact external gaps*, per the pass protocol — not declared
converged.

### 6.2 — [NEW double-home] The value-plane completion is LOCK-3/LOCK-7 work landed in `crates/ir` under a LOCK-4 probe.
`Op::ApplyFn` + `apply_fn.rs` MapExpr evaluator + `Value` scalar arms (built in PROTO-2) is IR-VM
lowering (LOCK-3 3A contract-tower) plus host-fn dispatch (LOCK-7 7C lattice). GESTALT-Ω owes an
adjudication of *which lock homes the value plane*; the specs assign HostClosure's typed shape to
LOCK-7 but leave the internal-descriptor evaluator (Hex/Number/Enum) home-ambiguous. Un-named.

### 6.3 — [retired, but conditionally] 4C-VM≡bbnf_ir-VM and 4A-fusion-consumes-e-graph are code-verified retirements.
I confirm both: the bytecode VM IS `bbnf_ir`'s live `vm/`; the e-graph is a `Cargo.toml:19` path-dep
client, not a clone. Pass-1's two strongest double-home flags (§6.2, §6.3) are genuinely retired **on
the shared route** — a real pass-2 gain. Caveat: conditional on INCOMPAT-1 electing the shared route.

### 6.4 — [under-applied] PROTO-1's runtime-switch refutation also refutes 4A-TS, and the specs don't say so.
A 4A-TS graph-walking `switch(node.tag)` interpreter is the same runtime-tag-switch shape PROTO-1
measured at −22%. The specs apply the refutation to 4C/4D but leave 4A-TS "unmeasured." By its own
evidence the reframe should conclude 4A-TS must *also* lower through the emitter — i.e. every TS
terminal in the lock is the emitter, full stop. This tightens the incompatibility map and the specs
leave it loose.

### 6.5 — [cross-lock overlap] The "fast TS terminal = build-time emitter" finding is LOCK-8 territory.
The emitter's V8 discipline (hidden classes, IC elimination, `new Function` lexical-name dispatch) is
squarely LOCK-8/8A/8C. PROTO-1's result should feed LOCK-8 as the load-bearing TS-face evidence; the
specs mention LOCK-8 barely. 4B-TS and LOCK-8's core are one implementation — a double-home GESTALT-P
must collapse. Flag it now so LOCK-8 doesn't re-derive the emitter finding.

### 6.6 — [unmoved] LOCK-9 leaf substrate underlies every "same leaf on both arms" gate.
The three-way common-baseline fork (INCOMPAT-3) and G-B1 production dilution both depend on LOCK-9's
kernels being held identical across arms. Not touched pass-2; a soft predecessor.

---

## 7. CONSTITUTION & FAILURE-MODE LEDGER

- **Vacuous convergence:** both prototypes have refutable gates; PROTO-2's value floor *could* have
  passed and did not — not vacuous at the probe level. **But the lock-level tower framing is
  vacuous-adjacent:** "four families are one tower" can read as "everything converged" when in truth
  the terminal is un-electable (two external forks), two of four families are BANKED/BLOCKED, and
  4D's decisive number is a LOSS. Convergence % must reflect terminal-blockage, not mechanism-folding.
- **Spec-cites-itself / circularity:** the substrate facts are independently code-verified (I re-ran
  them) — NOT self-citing. The "three of four families independently reframe toward the tower" is
  mild over-narration: the researchers shared the same substrate facts (bbnf_ir exists, VM exists),
  so their convergence is partly *forced by the shared substrate*, not four independent minds — real
  signal, slightly over-sold as independence.
- **Gates that cannot fail:** 4B G-B4 fusion-policy budget still has NO threshold (two passes running);
  G-A2 fused-region histogram carries a "or the counter-finding" escape that blunts a clean
  fail-negative.
- **Elegant-reduction trap (TWO live forms):** (i) the terminal reduces to "fold the mechanisms
  [done] and-then LOCK-5 + LOCK-6 decide the fork [unconverged, external]" — the load-bearing step is
  someone else's lock; (ii) 4D's value plane reduces to "build the mechanical value-completion [done,
  byte-identical] and-then it's viable" — but the built-and-measured value plane LOST to closures, and
  "viability turns on amortizing setup" is an unmeasured and-then-the-hard-part.
- **Masked fallback:** PROTO-2's "dominant gap is setup allocation, not the value plane" re-attributes
  a measured loss to a not-yet-fixed substrate cost, functioning as a fallback narrative for a
  fired FAIL trigger.
- **Legacy alias:** none new. 4A `lazy()`-as-sugar recursion guard still unexercised (pass-1 §1.4).
- **Consumer-less substrate (viii):** the reframe *avoids* new consumer-less substrate by reusing the
  live VM — genuine compliance. The PROTO-2 `ApplyFn` value plane has a consumer path (parse_with_ir
  → LSP DAP + json bench) — not consumer-less. Compliant.
- **Prototype-evidence misuse:** the sharpest this pass — PROTO-1's PASS (perf) is being read as
  evidence for the 4C *iso-layer* (isomorphism-of-record), which it did not build (hand-faked blob,
  3-lane self-equality). And PROTO-2's byte-identical *correctness* oracle is being read toward 4D/4C
  *viability*, which the same probe's throughput arm refuted.
- **Zero-runtime-cost types (v):** N/A to LOCK-4 (LOCK-8 territory). Unchanged.
- **Extant-tranche consciousness (ix):** 4B's new perf claim vs PT-2's PROVE-09 zero-claim posture
  still un-reconciled; the value-plane completion has no named PT-tranche home. Owed.

---

## 8. VERDICT & PASS-3 DIRECTIVES

No family converged; pass 2. The reframe is a genuine mechanism convergence and both prototypes RAN —
a real advance over pass-1's staleness. But the sharper prototype ran NEGATIVE on its own rubric, the
terminal is jointly owned by two external locks, and three keystones (4A fusion pass, the 4C
cross-language iso oracle, the 4B production-dilution + policy-budget numbers) are unbuilt after two
passes.

**Per-family convergence (critic-2, pass-2, cross-lock): 4A 50% · 4B 58% · 4C 32% · 4D 30% (BLOCKED).**
Leadership by axis, unchanged in shape from pass-1 but now *measured on both faces*: **4B leads
running evidence (Rust win + TS-emitter mechanism-class); 4D leads isomorphism-of-representation but
its Rust runtime is a measured loss; 4A leads analyzability-readiness but its keystone is still
unrun.** The confirmed emitter does NOT license naming a terminal — it fixes the TS *mechanism* (must
lower through a build-time emitter) while leaving the terminal *election* blocked on LOCK-5 (ownership
semantics) and LOCK-6 (iso verdict).

Pass-3 must:
- **Build 4A's `fuse(Graph)→Graph` byte-identity oracle (G-A1)** — the one differentiator still
  SPEC-ONLY two passes in; and apply PROTO-1's refutation to 4A-TS explicitly (§6.4).
- **Build the 4C cross-language iso oracle for real** — a *live* Rust `to_vec_named` serialize →
  TS decode → dual oracle (opcode-stream identity + Value-tree identity vs the Rust `Interpreter`).
  PROTO-1's perf answer does not substitute for the isomorphism-of-record (§3.1, §5).
- **Build the three-way common-baseline fork (INCOMPAT-3)** — 4A-interpret vs 4C-bytecode vs
  4B-monomorphized, one leaf substrate, one grammar. Still INFERRED after two passes.
- **Test whether eliminating `Interpreter::new` per-parse setup closes 4D's value-plane loss** — the
  unmeasured and-then-the-hard-part PROTO-2's re-attribution rests on; do NOT carry "viable" until it
  is measured.
- **Give G-B4 a number** and measure G-B1 production dilution vs the shipped `json_parser()`.
- **Extend the emitter probe past arity-7 to a grammar-driven fuser AND through an arbitrary host
  closure (GAP-1)** — the wall both PROTO-1's projection and PROTO-2's projection dodged.
- **GESTALT hand-off:** name the value-plane's home (LOCK-3 vs LOCK-7, §6.2); reconcile 4B's perf
  claim against PT-2's zero-claim ledger as a named supersession; route the emitter finding to LOCK-8
  so it is not re-derived (§6.5). Return LOCK-4 as a **converged core with exact external gaps
  (LOCK-5 ownership-semantics; LOCK-6 iso-verdict)** — not a terminal.
