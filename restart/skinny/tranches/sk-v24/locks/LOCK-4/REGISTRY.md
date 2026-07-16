# LOCK-4 — THE COMBINATOR CORE · REGISTRY

*Agglomerator record (Fable-tier judgment). One row per family; convergence is the **critics'
conservative floor** unless a floor is named as erring. Inputs folded: FAMILY-SPECS, research-{A,B,C,D,
priorart}, PROTO-1 (+patch +scratch tree), PROTO-2, CRIT-1 (within-lock), CRIT-2 (cross-lock). The lock
is not four features — it is four compilation strategies for one combinator algebra, decided on two axes:
**Q-runtime** (what survives to runtime: graph 4A | bytecode 4C | none 4B) × **Q-ownership** (whose
representation `.then()` builds: parse-that-private 4A/4B/4C | shared `bbnf_ir` 4D). A single terminal
runtime must be chosen under a value-plane policy chosen once.*

---

## STATUS TABLE (after pass 1)

| Family | Status | Conv. | One-line |
|---|---|---:|---|
| **4A CLOSURE-TO-GRAPH** | **ADVANCED** | **50%** | Leader; ships substrate + the one running paired artifact — but the artifact tests a hand-fused Rust tree-walk, not 4A's charter (fusion-as-a-pass, general union, iso). |
| **4B COMPTIME-FUSION** | **ADVANCED · REWARD-AT-RISK** | **32%** | Deciding experiment (inlining / `cargo asm`) *deferred this pass*; zero evidence on its sole axis; honest prior NEUTRAL — may correctly close on a filed refutation. |
| **4C VM-CORE** | **BANKED** | **30%** | Terminal-runtime bid crowded out (throughput dominated by 4A's measured parity; private bytecode VM double-homes bbnf_ir's live `vm/`). Its one novel, untested, cheap contribution — the byte-identical portable-program iso layer — is re-opened as a graft onto the shared VM / 4D's TS unblock. |
| **4D GRAMMAR-SUGAR** | **BLOCKED** | **25%** | Rust near-bijection real and verified; parsimony maximum. BLOCKED on a TS-execution primitive (horn i equal-difficulty re-port; horn ii wasm PROVE-14 0.91× measured loss). The 4C graft is the concrete third horn that could unblock it. |

Net: **no family is converged; the terminal runtime cannot be named on pass-1 evidence.** The three
numbers the pick requires — 4B inlining-attributable win, one TS number (constitution-iii is 0% built),
LOCK-5's substrate ruling — were all unproduced. Pass-2 must not read the two green briefs as a mandate
to pick (CRIT-2's elegant-reduction trap).

---

## 4A — CLOSURE-TO-GRAPH · ADVANCED · 50%

**Floor rationale.** CRIT-1 50 / CRIT-2 55. Take **50**: CRIT-2's +5 credits the real substrate and the
genuine paired artifact (both real), but CRIT-1 correctly discounts for the keystone (P1 fusion-as-a-pass)
being unbuilt and PROTO-1 testing a *hand-fused 5-variant arena*, not the ~25-node general union 4A's
charter specifies. No reason to go below 50: `span_parser/` ships (29 variants, 10 files) with live
JSON/CSS consumers (Y.13 satisfied by construction), and one refutable paired artifact ran (canada floor
0.986× is a real margin that could have failed).

**What is EARNED (do not re-litigate).** A runtime tree-walk value graph, Rust-only, hand-fused, runs at
parity-or-better vs the *current* closure core; the value-plane `Map` `Box<dyn Fn>` indirection **confines
to scalar leaves** (objdump: 647 direct `bl` vs 45 indirect `blr`, spine fully inlined). Narrow and real.

**Exact open gaps.**
1. **[keystone] P1 fusion-as-a-separable-pass is UNBUILT.** PROTO-1 hand-writes the fused arena
   (`json_graph.rs:163` literal `vec![...]`); there is no `fuse(Graph)→Graph`, no SCC packrat placement,
   no FIRST-set dispatch synthesis. 4A's ONE novel claim ("cleverness becomes total `Graph→Graph`
   passes") has zero running evidence and is SPEC-ONLY.
2. **General-union value-plane number UNTAKEN.** P2's real question — does a *general* `match` over the
   wide union (widened by the `Map` payload) stay ≥0.98× the monomorphized chain — was reduced to bespoke
   fused arms. Rerun with the actual `Node` union + generic `Seq/Alt/Many/SepBy` dispatch, or state the
   result is limited to hand-fused grammars.
3. **Wrong rival.** PROTO-1 beats the *old* closure core (`SmallBox` + `lazy` `UnsafeCell` +
   `dispatch_byte_multi` Vec indirection) — and concedes the win is *retiring those inefficiencies*, not
   analyzability. The 4A-vs-4B terminal decision is graph-vs-**monomorphized-4B-chain**, never built.
4. **One-path collapse unproven.** `json.rs` (closure) still ships *alongside* `json_graph.rs`; the parity
   test needs both. §2.4.2's "parallel second codepath" violation is still open — an arm was added, not a
   tier folded.
5. **Rust-only; P4 iso 0% built.** No `ParserNode` union/passes in TS. R4 warning stands: dispatch-LUT
   under-delivered (~+5% vs +10% claimed); a `switch(node.tag)` interpreter may go megamorphic on V8. The
   Rust win is assumed to transfer, not shown.
6. **Recursion-seam guard (R3) unexercised.** PROTO-1 sidesteps `lazy()` (index recursion). The
   `lazy()`→`NonTerminal(RuleId)` auto-mint-via-cache-identity guard is a *masked fallback* until a
   consumer grammar with anonymous mutual recursion runs through it.
7. **Packrat SCC placement (P3) + diagnostics-annotation pass UNBUILT.**
8. **[cross-lock] Double-home hazards.** (a) 4A "mirrors `rust/regex/src/egraph/`" — a *second*
   equality-saturation system private to the combinator graph; standing memory (`general-infra-crates`,
   `system-cohesion`) says fusion passes must *consume* LOCK-2's engine, not clone it. (b) 4A's private
   node union duplicates `bbnf_ir::IrNode` + `parse_with_ir` (a 4A-shaped tree-walk) — the Q-ownership
   crowding vs 4D, gated on LOCK-5.

**Pass-2 directives.**
- *Prototype (keystone):* build `fuse(Graph)→Graph` and run P1 — byte-identical to the hand-fused arena
  (structural-equality oracle) AND JSON/CSS cold-paired ≥0.98×. This is the claim PROTO-1 was *credited
  with but did not test*.
- *Prototype (value-plane redo):* re-run P2 with the **general** union + generic dispatch arms,
  **sample-interleaved** `A,B,A,B` (not divan's per-bench adjacency — see F-CRUX-3), N≥200 configured
  (not divan defaults), so 4A's keystone is earned not assumed.
- *Redirect (parsimony):* run P1/P2 on `bbnf_ir::IrNode` where the seam allows (research-A §0 / research-D
  §1 confirm the near-bijection), so the private-vs-shared ownership question is *measured*, not assumed;
  hold the private node union in reserve, opened only if LOCK-5 rules `bbnf_ir` is NOT the shared
  substrate. 4A's fusion pass must be adjudicated as a consumer of LOCK-2's e-graph.
- *Research must answer:* does the general interpreter (graph) tie the closure core on a projection-heavy
  grammar (R1 counterexample: value.js `all()`-of-mapped-parts), or only on structure-dominated work?
- *Withheld from researchers:* the "1.06× graph faster / single-tier VIABLE" headline (fusion artifact +
  wrong-rival + not sample-interleaved). Hand them only the settled narrow fact (Map indirection confines
  to scalar leaves) and the open question (does the *general* analyzable interpreter tie the closures).

---

## 4B — COMPTIME-FUSION · ADVANCED · REWARD-AT-RISK · 32%

**Floor rationale.** CRIT-1 32 / CRIT-2 35. Take **32**: the family's deciding experiment (generic-struct
JSON + `cargo asm` + paired bench) was *explicitly deferred* this pass (FAMILY-SPECS §6); it has zero
empirical evidence in either direction on the one axis that is its entire reason to exist (the
indirect-call / inlining barrier). A family whose central lever is unmeasured and whose deciding
experiment is postponed cannot sit above the low-30s.

**Exact open gaps.**
1. **The deciding number is unrun and PROTO-1 cuts *against* it.** A runtime analyzable graph already
   ties/beats the closure core (F-CRUX-2); if closures are 4B's target and a graph that *keeps*
   analyzability already matches them, 4B's headroom to win by *erasing* the representation is narrower
   than the spec implies. Not surfaced as a within-lock tension; pass-2 must.
2. **`cargo asm`/`otool` mechanism proof not produced** — no demonstration the `blr` vtable calls fold to
   direct/inlined in a generic-struct build (the timing-independent proof the family rests on).
3. **Predicted-branch trap (R1) unrebutted.** The `SmallBox` indirect call is a single fixed-target branch
   the Apple-M BTB predicts perfectly on the steady loop; inlining across it may buy nothing — the exact
   PROVE-09 shape (an "obvious" win that evaporated under paired interleaving). Honest prior NEUTRAL.
4. **Weakest isomorphism of the four, and the synthesis did not flag it.** priorart §9: 4B's Rust and TS
   fusers are *different codegen over a shared plan* — the softest position vs constitution iii
   ("isomorphism in layout AND in code"). Enumerated here as a gap (it was not in FAMILY-SPECS).
5. **Spec self-contradiction (masked fallback).** Architecture asserts "No runtime graph at all"; open-Q
   (d) concedes the runtime interpreter graph is retained as the sound fallback for dynamic grammars. 4B
   is *fused-sites-only + a retained interpreter graph* — a 4A/4C runtime under another name for the
   unfused remainder. The Q-runtime incompatibility map ("4B eliminates any runtime program") is false as
   stated; scope the claim to fused sites in the disposition, not the footnote.
6. **PROVE-11 says the TS win is allocation-shaped (~+5% dispatch).** The build-time fuser (generalize
   `fuseAll` past arity-3 to a vite/ts-transform plugin), the dispatch-vs-allocation isolation gate, and
   bundle-size/`tsc`-clean/three-consumer parity are all untouched. "Beyond-SOTA" for 4B-TS is unsupported.
7. **Generated-size budget is a gate with no threshold** — R3 blowup + `generated-size-budget` memory bind
   a per-tranche line ceiling; 4B names it but gives no number and no `.boxed()` depth-cap policy value. A
   gate that cannot fail.

**Pass-2 directives.**
- *Prototype (the deferred one — now mandatory):* generic-struct algebra (`Then/Or/Map/Opt/Many` +
  `Boxed`) with `Parser` trait + associated `Output`, `#[inline]` throughout; reimplement the JSON grammar
  three ways — SmallBox core, generic-struct, `span_parser` enum tier; `cargo asm`/`otool` mechanism proof
  (blr folded?) + paired cold N≥200 two-windows over six corpora. Expect single-digit % and **be ready to
  file a refutation** (PT-2 W4 arena precedent — a valid family outcome, not a failure to bury).
- *Research must answer:* is 4B a *rival terminal* or the *build-time compile backend* of 4D/4A? Parsley
  shows 4B is the staging endpoint of the 4A→4C→4B pipeline, not a fourth terminal — this latent crowding
  (4B-as-terminal vs 4B-as-compile-mode) must be resolved before 4B is elected.
- *Withheld from researchers:* do not seed "the vtable dissolves = speed." The axis is unmeasured; the
  honest prior is NEUTRAL; the Rust core reform makes ZERO perf claim (§0/09 LAW). Hand them the paired-
  interleave discipline and the null-is-valid framing, not an expected win.

---

## 4C — VM-CORE · BANKED · 30%

**Floor rationale.** CRIT-1 35 / CRIT-2 30. Take **30**: CRIT-2 correctly weights that the family's
*cheapest, highest-signal, throughput-independent* claim (the byte-identical-program oracle) was **skipped**
this pass, and that the private bytecode VM double-homes bbnf_ir's live `vm/`. CRIT-1's +5 credits the
proto-VM substrate reality, which is real but does not lift the terminal bid.

**Why BANKED (the crowding redirect).** As a *terminal runtime*, 4C is crowded out on both flanks:
- **Throughput bid dominated by 4A.** 4C's honest prior is a throughput *loss* (0.7–0.9× on twitter/citm/
  CSS; value-tier possibly 0.6× because every `Action` is an un-inlinable call the closure core inlined),
  recovered only by threaded code. PROTO-1 measured a *tree-walk* runtime-analyzable representation (4A) at
  **parity-or-better** — a faster runtime representation that is *also* analyzable. A bytecode VM that is
  slower and also analyzable is dominated. (Do not import PROTO-1's tree-walk parity as bytecode-VM
  parity — different overhead regime; F-CRUX-1.)
- **Mechanism redundant with the extant VM (strongest cross-lock finding, CRIT-2 §6.1).** `bbnf_ir` already
  ships a value-producing bytecode VM — verified live: `crates/ir/src/vm/{bytecode.rs, compiler/{emit,
  compound,rule}.rs, interpreter/{control,memo,repeat,value}.rs}`. That is LOCK-3's IR-lowering seat and
  the engine 4D inherits *for free*. A parse-that-private instruction set + two interpreters is a *second*
  bytecode VM — the exact double-home the gestalt exists to collapse; `no-orthogonal-codepaths` /
  `system-cohesion` forbid it.

**What is BANKED (preserved, not discarded).** 4C's ONE genuinely novel, cheap, **untested** contribution:
the **byte-identical portable-program isomorphism** — a language-neutral opcode/operand stream that
*crosses as data* (serialize in Rust, decode in TS), so isomorphism is a property of the representation, not
a mirrored-source linter. This is re-opened as a directed graft (see Cross-Pollination) onto the shared
VM and 4D's TS unblock. BANKED means: **4C does not advance as an independent terminal-runtime family; its
instruction-set design and iso-oracle fold into the LOCK-3 / bbnf_ir VM line and the pass-2 iso experiment.**

**Residual open gaps (carried into the banked line).**
1. The byte-identical-program oracle is unbuilt — nothing serializes a program or decodes it in TS. This
   is the cheapest highest-signal thing the family could prove (needs no throughput win) and it was skipped.
2. The `no-span-surface` non-reopening argument ("internal lowering target ≠ public surface") is asserted,
   not exercised against a VM introduction.
3. Left-recursion seed-growth parity across rust/ts (research-C risk 4) — the one genuinely subtle
   facility — untested. Relevant only if the VM line proceeds via bbnf_ir's memo.

**Pass-2 directive.** Do NOT spin an independent 4C research thread. Fold the byte-identical-program oracle
into the 4D/VM experiment: serialize bbnf_ir's `BytecodeProgram` (MessagePack, already at `lib.rs:6`),
decode it in a **small TS switch-loop interpreter (~200 LOC)**, assert identical opcode stream + identical
parse over a shared corpus. If a private VM is ever to be resurrected, it must first be adjudicated against
LOCK-3's contract-tower verdict (is a second bytecode home justified?) — abrogate-before-patch.

---

## 4D — GRAMMAR-SUGAR · BLOCKED · 25%

**Floor rationale.** CRIT-1 30 / CRIT-2 25. Take **25**: both agree the *block itself is high-confidence
and well-converged*, but a family that cannot converge above BLOCKED inside LOCK-4 (it needs LOCK-5's
substrate ruling and LOCK-7's host-fn lattice) is correctly held at the lower floor. CRIT-1's +5 credits
the Rust-side elegance, which is genuine but "earns nothing on its own."

**What is verified (code-checked by both critics).** `IrNode` carries 14 variants matching the
near-bijection table; `FnDescriptor` has `EnumWrap/BoxWrap/NumberConvert/HexConvert/SpanCapture/
Expr{MapExpr}` and **no opaque host-closure variant** — so the central wall (must add `HostClosure`) is a
real, code-verified gap; `parse_with_ir` exists at `interpreter/mod.rs:373`. On Rust alone, 4D is the
parsimony maximum: one IR, one optimizer (`passes/` + `egraph/`), one MessagePack iso bus — all verified to
exist, all *duplicated* by 4A/4C's private subsystems.

**The block (correctly diagnosed, unmoved this pass).** Constitutional rust/ts isomorphism (clause iii,
non-negotiable) rests on a TS-execution primitive with two horns, both constitution-i traps:
- **Horn (i):** full port of `GrammarIR` + interpreter + optimizer passes to TS = re-implementing the
  grammar plane's engine in a second language — a primitive of *equal-or-greater difficulty than the
  original LOCK-4 problem*.
- **Horn (ii):** the WASM-core crossing — **PROVE-14 measured full-walk value materialization at ~0.91×, a
  LOSS on exactly the workload a value-producing combinator parser IS** (the 4.4× stop-early tape win does
  NOT cover full materialization; SPEC §0/14). This is a *measured non-goal*, not a bug to fix.

**Exact open gaps (to unblock + intra-family unbuilt).**
1. **The TS-execution primitive is unresolved (the block).** Neither horn retired. Horn-(ii)'s gate (does
   the wasm crossing beat the PROVE-14 full-walk non-goal for a *value-producing* parse?) unmeasured; horn-
   (i) conceded equal-difficulty **— but see the 4C graft, which is the un-conceded third horn.**
2. **Cross-lock: cannot converge inside LOCK-4.** Hard dependency on LOCK-5 (is `bbnf_ir` *genuinely* the
   shared substrate? else the "for free" optimizer/iso claim evaporates) AND LOCK-7 (host-fn lattice for
   `HostClosure`, itself unconverged). 4D's ceiling this pass is bounded by two other locks' verdicts.
3. **4D's decisive number unmeasured** — no `parse_with_ir`-vs-closure paired bench (interpret-mode
   regression lane, the evaporated-shim caution) nor compile-mode win.
4. **`FnDescriptor::HostClosure` + scoped-opacity guard unbuilt.** PROTO-2 argues current sites don't
   poison siblings, but the variant, fn-table, and applicative/selective/monadic tiering are unbuilt.
5. **State-effecting combinators (`peek`/`lookAhead`/`recover`/`mapState`) outside the IR vocabulary** —
   the minimal lawful set of new variants vs `HostClosure`-with-state "must be measured, not guessed."

**Pass-2 directives.**
- *Prototype (parity + decisive number):* a minimal Rust combinator front building `GrammarIR` for the JSON
  grammar via `parse_with_ir` — byte-identical trees vs hand + closure JSON parsers; then the paired cold
  N≥200 two-window of interpret AND compile modes vs the closure core over six corpora. This ALSO takes
  4A's untaken general-union value-plane number (interpret = 4A-shaped tree-walk over the shared union) —
  one prototype, three families' numbers.
- *Unblock experiment (the graft — highest leverage in the lock):* build the ~200-LOC TS switch-loop
  decoder over bbnf_ir's serialized `BytecodeProgram` (the 4C iso mechanism). If it drives a value-
  producing JSON parse in TS at a stated no-regression floor vs today's TS closures, horn-(i) collapses
  from "engine re-port" to "small decoder over a shared program" and horn-(ii)'s wasm loss is sidestepped —
  4D unblocks. This is the single pass-2 experiment that could move 4D off BLOCKED.
- *Research must answer:* LOCK-5's substrate ruling and LOCK-7's host-fn lattice shape — flag both as hard
  DAG predecessors; 4D cannot be *selected* until they land, even if the graft lifts the TS block.
- *Withheld from researchers:* do not seed "4D dominates on parsimony so pick it." The dominance is
  *conditional* on LOCK-5 ruling `bbnf_ir` the shared substrate; if LOCK-5 keeps HIRs separate (a lawful
  functor, not one type), 4D loses its "for free" claim and 4A/4C's private representation is the honest
  choice. Hand researchers the block and the graft, not a verdict.

---

## CROWDING & REDIRECT (this pass)

The critiques converge families onto shared mechanisms along **Q-ownership**, not Q-runtime:

- **The bytecode VM is ONE mechanism in three homes** (CRIT-2 §6.1, strongest cross-lock finding):
  4C-private-VM ≈ 4D-compile-mode ≈ bbnf_ir's live `vm/`. **Redirect:** 4C's *private-VM-as-terminal*
  formulation is the redundant one → **BANKED**. The underexplored formulation opened in its place: the
  **byte-identical portable-program iso layer over bbnf_ir's bytecode** (4C's genuinely novel bit), which
  is simultaneously the 4D-TS-unblock graft.
- **The tree-walk graph is ONE mechanism in two homes:** 4A-private-node-union ≈ bbnf_ir's `parse_with_ir`
  (a 4A-shaped tree-walk; FAMILY-SPECS §5). **Redirect (not a bank — 4A ships the only running artifact):**
  run 4A's value-plane/fusion experiments on `bbnf_ir::IrNode` where the seam allows; hold the private union
  in reserve pending LOCK-5.
- **The e-graph fusion system is ONE mechanism in two homes:** 4A-private-fusion-egraph vs LOCK-2's
  in-tree `egraph`/`egraph-derive`. **Redirect:** 4A's fusion pass consumes LOCK-2's engine; no private
  clone (`general-infra-crates`, `system-cohesion`).
- **4B is NOT crowded on Q-runtime** (it is the opposite pole — "none survives to runtime"), but carries a
  *latent* crowding: 4B-as-terminal vs 4B-as-the-build-time-compile-backend of 4D/4A (Parsley's staging
  endpoint). Resolve before electing 4B a terminal.

## CROSS-POLLINATION (named grafts — both families developed enough; graft justified)

1. **4C ⊗ 4D — the portable-program TS decoder (PRIMARY, highest leverage).** 4C is developed enough (LPeg-
   proven instruction set + the byte-identical-program design); 4D is developed enough (verified near-
   bijection + a precisely-diagnosed TS block). **Graft:** drive 4D's shared `bbnf_ir` bytecode program
   through 4C's small TS switch-loop interpreter — the program crosses as MessagePack data (already at
   `bbnf_ir::lib.rs:6`), NOT a wasm runtime. This makes 4D's horn-(i) *not* an engine re-port (a ~200-LOC
   decoder) and sidesteps horn-(ii)'s measured 0.91× wasm loss. Justified because both families independently
   land on "a shared program + a small per-language decoder" as the iso mechanism.
2. **4A ⊗ 4D — fusion-as-a-pass on the shared union (SECONDARY).** 4A's `fuse(Graph)→Graph` thesis, IF it
   must exist, runs on `bbnf_ir::IrNode` and consumes bbnf_ir's `egraph/`+`passes/` (which 4D inherits),
   not a parse-that-private node union + private fusion e-graph. Justified because `IrNode` is the verified
   near-bijection of 4A's node set (research-A §0, research-D §1).

Both grafts converge on ONE pass-2 keystone prototype: **a single combinator front onto `bbnf_ir::IrNode`,
exercised via `parse_with_ir` (interpret = 4A-shaped) AND the bytecode compiler + TS decoder (compile =
4C-shaped), measured against the closure core.** That one prototype takes 4A's untaken general-union value-
plane number, 4C's untested iso oracle, and 4D's decisive number simultaneously — the parsimony-respecting
collapse the constitution wants.

## CROSS-LOCK DAG ORDERING (for GESTALT)

- **LOCK-4 cannot freeze ahead of LOCK-6** (iso verdict) or **LOCK-5** (substrate verdict). Three different
  LOCK-6 verdicts are embedded in three LOCK-4 families (4A→6A mirror-linter; 4C→byte-identical program;
  4D→6C wasm). If LOCK-6 rules 6C, 4A's mirror discipline and 4C's two-decoder oracle are moot and 4D's
  block either lifts or hardens. **LOCK-4 ⊗ LOCK-6 co-converge in GESTALT-P; neither freezes alone.**
- **4D depends on LOCK-5 (substrate) AND LOCK-7 (host-fn lattice)** — both unconverged. DAG fact: 4D cannot
  be *selected* until both land.
- **4C/4A private substrates must be adjudicated against LOCK-3 (bytecode double-home) and LOCK-2 (fusion
  e-graph double-home) BEFORE a terminal runtime is elected** — picking a private-substrate family without
  the ruling reopens the very duplication the gestalt collapses.

---

## PASS HISTORY

### Pass 1 (2026-07-16) — agglomerated

- **Artifacts:** FAMILY-SPECS (synthesis, 4 family specs + incompatibility map + 2 briefs); research-{A,B,
  C,D,priorart}; PROTO-1 VALUE-PLANE-SURVIVAL (RAN, Rust-only); PROTO-2 CHAIN-MONADIC-CENSUS (RAN,
  measurement-free); CRIT-1 (within-lock: 4A 50 / 4B 32 / 4C 35 / 4D 30); CRIT-2 (cross-lock: 4A 55 / 4B
  35 / 4C 30 / 4D 25).
- **PROTO-1 disposition:** genuine, refutable, paired, cold, two-window, N=100 — but **misattributed across
  three families**. Tests a hand-fused Rust tree-walk arena, not: 4A's general interpreter / fusion-as-a-
  pass (F-CRUX-1, verified — `json_graph.rs:163` hand-writes the arena); 4C's choice-point bytecode VM
  (different overhead regime); 4D's `bbnf_ir` mechanism (never touches it). The 6% "graph faster" is a
  **fusion artifact + wrong-rival**, and divan runs each bench to completion (adjacent, NOT sample-level
  A,B,A,B interleaved — F-CRUX-3) at a distrust-band effect size below PROVE-09's refuted magnitude.
  **SETTLED (narrow):** the value-plane `Map` indirection confines to scalar leaves. **NOT SETTLED (do not
  inherit):** "4A/4C/4D single-tier VIABLE"; "graph is faster."
- **PROTO-2 disposition:** load-bearing result credited — every `chain`/`mapState`/`recover` site fires
  after a fixed-FIRST prefix, so no site poisons an enclosing alternation's FIRST/dispatch (poison-scope
  passes independent of the S/M tally); `mapState`=0, `recover`=0 call sites; `map` applicative-transparent
  by signature. Two soft edges: the |M|≤3 pass rests on *optimistic S-tagging* of currently-`chain`-written
  sites (reformulability asserted, not demonstrated — and reformulation entails a public-API change +
  read-only-consumer migration); the census is a snapshot of today's grammars, not a property of the
  algebra. Verdict rests on poison-scope (holds for all 5 sites), not the S/M arithmetic.
- **Dispositions set:** 4A ADVANCED 50; 4B ADVANCED·REWARD-AT-RISK 32; 4C BANKED 30 (terminal bid crowded
  out; iso-mechanism re-opened as graft); 4D BLOCKED 25 (TS-execution primitive; 4C graft is the candidate
  unblock).
- **Pass-2 keystone:** ONE combinator front onto `bbnf_ir::IrNode` — interpret (4A) + compile+TS-decoder
  (4C) vs closure core — taking three families' numbers at once. Plus the deferred 4B inlining/`cargo asm`
  prototype (mandatory; the 4A-vs-4B terminal axis is undecidable without it). Plus produce ONE TS number
  (constitution-iii is 0% built).
