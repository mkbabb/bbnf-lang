# LOCK-4 — THE COMBINATOR CORE · pass-1 · ADVERSARIAL CRITIC 2 of 2 (cross-lock) — REFRESH

*Critic 2, cross-lock-weighted, second reading. Supersedes the 04:07 CRIT-2 draft: it folds an
artifact none of the prior adjudication files saw. Read: PORTFOLIO.md (LOCK-4 charter + CONSTITUTION),
FAMILY-SPECS.md, REGISTRY.md, research-{A,B,C,D,priorart}, PROTO-1 (VALUE-PLANE, +patch, +scratch tree),
PROTO-2 (CHAIN-MONADIC-CENSUS), PROTO-1-MONADIC-WALL-CENSUS, **PROTO-2-4B-FUSION (+patch)**. Substrate
claims spot-checked against live code (bbnf_ir `vm/`, in-tree `egraph`/`egraph-derive`, parse-that
`span_parser/`). Charter applied verbatim; convergence EARNED; this is pass 1 — no family is converged.*

---

## 0. THE HEADLINE FINDING — THE PASS-1 ADJUDICATION IS STALE ON ITS ONE DECIDING NUMBER

**Filesystem timeline (all 2026-07-16):** PROTO-2 03:54 · PROTO-1 04:01 · CRIT-2(old) 04:07 · CRIT-1
04:09 · REGISTRY 04:16 · research-B/C/D 04:23–04:25 · FAMILY-SPECS 04:34 · MONADIC-WALL-CENSUS 04:41 ·
**PROTO-2-4B-FUSION.patch 04:52 · PROTO-2-4B-FUSION.md 04:53 (NEWEST).**

Every adjudication artifact — the REGISTRY, CRIT-1, the prior CRIT-2, and FAMILY-SPECS §6 — was written
declaring 4B's deciding experiment **not run**:

- REGISTRY 4B row: *"Deciding experiment (inlining / `cargo asm`) deferred this pass; zero evidence on
  its sole axis."* Floor 32%.
- CRIT-1 §2: *"zero empirical evidence this pass, on the one axis that is its entire reason to exist …
  the make-or-break … was not run."*
- CRIT-2(old) §0.1 + §2.1: *"The 4B inlining number … is unrun"; "No running prototype; the one
  measurement that decides the family was explicitly deferred."*
- FAMILY-SPECS §6 / PROBE-2: filed as a **brief for pass 2.**

**It ran.** `PROTO-2-4B-FUSION.md` is a genuine, RAN, refutable artifact that:

1. **Passes the hard disasm gate** — fused hot chains (`fused::value/object/array/parse`) carry **0
   `blr`** (arm64 vtable indirect); the closure core carries **60**. This is *exactly* the
   timing-independent `cargo asm`/`otool` mechanism proof CRIT-1 §2 gap 2 said "was not produced." It
   was produced.
2. **Passes the throughput gate** — cold, paired, **A/B/A/B sample-interleaved**, N=250, two load
   windows, both arm orders, mimalloc, fat-LTO: **1.16×–1.27×** fused ÷ closure, **min cell 1.161×,
   every one of 12 cells ≥ 1.10×,** load-symmetric (no PROVE-09 window/order asymmetry).
3. **Rebuts the predicted-branch trap** (CRIT-1 §2 gap 3 / research-B R1): the BTB-predicts-the-fixed-
   target null hypothesis is empirically false at this grammar scale — removing the boxed dispatch buys
   16–27%, not zero, in both windows.

**Three of 4B's five REGISTRY/CRIT gaps are thereby closed by an artifact the adjudication never saw.**
The REGISTRY's terminal sentence — *"the terminal runtime cannot be named on pass-1 evidence … the 4B
inlining-attributable win [was] unproduced"* — is factually overtaken. The FORK-II throughput half is
**resolved for Rust: it routes to 4B**, precisely as PROBE-2's own pass/fail rubric specifies.

**But this does NOT license picking 4B, and the reframe is the real finding (below §6.1): the confirmed
win is a *Rust monomorphization* win, and monomorphization has NO V8 analog (T1, priorart §1). So the
one axis 4B just won (throughput) is the one axis it was always going to win, on the mechanism
(concrete generic `Seq2<P0,P1>` structs = the nom/winnow route) that is structurally un-mirror-able in
TS.** The binding constraint of the lock is not throughput — it is isomorphism (constitution iii,
non-negotiable), on which 4B is the *worst* family, and that half remains 0% built. The stale
adjudication mis-ranked the families by treating throughput as open; with throughput now closed, the
correct reading is that **throughput was never the deciding axis** — a confirmed-fast, un-isomorphic
Rust path re-casts 4B as the Rust *compile backend* of an isomorphic terminal (4A-graph or 4D-IR), not
a fourth terminal. That is the registry's own latent "4B-as-terminal vs 4B-as-compile-mode" crowding
(REGISTRY CROWDING §4), now **resolved by evidence toward compile-backend.**

**Process obligation for the agglomerator:** re-fold PROTO-2-4B-FUSION and re-open the 32% floor and the
"terminal cannot be named" disposition. The pick is still not makeable — but for the *opposite* reason
the registry gives (isomorphism unbuilt + provably harder), not (throughput unmeasured).

---

## 1. FAMILY 4A — CLOSURE-TO-GRAPH · **convergence 48%** (was 55; re-priced down)

**Why down.** The confirmed 4B number reprices PROTO-1's headline. PROTO-1's "graph faster ~1.06×"
was already diagnosed (CRIT-1 F-CRUX-2) as a *boxing-retirement* artifact, not an analyzability result.
PROTO-2-4B now isolates that exact effect cleanly — identical direct-`bl` leaf scanners on both arms, so
the *only* variable is the combinator substrate — and attributes **16–27%** to eliminating the boxed
dispatch alone. So the effect PROTO-1 credited to "keeping the representation analyzable" is, more
precisely and more largely, "stop boxing the spine," which **4B captures by erasing the representation**
and 4A captures by reifying-then-extracting. PROTO-1's 1.06× is a *weaker, less-isolated* measurement of
the same lever a rival family measured at 1.16–1.27×. 4A keeps the lead on isomorphism-readiness (its
graph-as-data is mirror-able where 4B's monomorphized structs are not) — but loses the "graph is
intrinsically faster" narrative it was over-credited with.

### What still holds
- Substrate real and consumed: `span_parser/` (29 `SpanKind` variants, 10 files) with live JSON/CSS
  consumers — Y.13 by construction.
- PROTO-1 is a genuine paired cold two-window artifact (canada floor 0.986× a real refutable margin).

### Exact open gaps
1. **[keystone, unmoved] `fuse(Graph)→Graph` is UNBUILT.** PROTO-1 hand-writes the fused arena
   (`json_graph.rs:163` literal `vec![…]`); no separable fusion pass, no SCC packrat placement, no
   FIRST-set dispatch synthesis. 4A's ONE novel claim has zero running evidence and is SPEC-ONLY.
2. **[sharpened by new evidence] The 4A-vs-monomorphized-4B head-to-head is STILL not built on a
   common baseline.** PROTO-1's graph beat the *production* `json_parser` (with `dispatch_byte_multi` +
   SIMD leaf scanning) at 1.06×; PROTO-2-4B's fused struct beat a *stripped* boxed-closure core (leaf
   scanners held identical, no `dispatch_byte_multi`, no SIMD) at 1.16–1.27×. **These two numbers share
   neither baseline nor grammar and cannot be subtracted.** "4B (1.16–1.27×) beats 4A (1.06×)" is a
   category error. The Q-runtime decision (graph vs monomorphized chain) is undecided until *one*
   experiment runs both against the *same* leaf substrate on the *same* grammar. Pass-2 must build it.
3. **TS entirely unmeasured (P4 unrun).** No `ParserNode` union/passes in TS; R4's dispatch-LUT
   under-delivery (~+5% vs +10% claimed) and V8-megamorphism risk on `switch(node.tag)` are live and
   untested. Constitution-iii proof 0% built — and, per §6.1, the confirmed 4B result makes this the
   binding gate for the whole lock, not a footnote.
4. **Recursion-seam guard (R3) is a masked fallback.** `lazy()`→`NonTerminal(RuleId)` auto-mint via
   cache identity is asserted; no consumer grammar with anonymous mutual recursion exercises it.
5. **[cross-lock] Fusion-e-graph double-home vs LOCK-2 (unmoved).** 4A "mirrors
   `rust/regex/src/egraph/`" = a second equality-saturation system private to the combinator graph.
   `general-infra-crates` + `system-cohesion` require it to *consume* LOCK-2's engine, not clone it.
6. **[cross-lock] Node-union double-home vs bbnf_ir `parse_with_ir` (unmoved).** The private union
   duplicates `IrNode`; the Q-ownership crowding vs 4D, gated on LOCK-5.

---

## 2. FAMILY 4B — COMPTIME-FUSION · **convergence 50%** (was 35; re-priced UP on the confirmed artifact)

**Why up, and why not higher.** PROTO-2-4B closes the three gaps that pinned 4B to the low-30s
(deciding number, mechanism proof, predicted-branch trap) with a *better-isolated* experiment than
PROTO-1 (leaf scanners identical → the 1.16–1.27× is attributable purely to boxed-dispatch removal) and
a *timing-independent* disasm gate (0 vs 60 `blr`). It even repairs PROTO-1's F-CRUX-3 defect: the
measurement of record is a manual **A,B,A,B sample-interleaved** harness, the interleaving the PROVE-09
law demands and divan's per-bench adjacency lacks. On its own axis, 4B is now the **strongest running
Rust artifact in the lock.** It does not go above 50% because its *entire remaining case* is the
isomorphism half — 0% built and, uniquely among the four, provably *harder* after this result.

### Exact open gaps
1. **[the reframe, not a defect in the artifact] The confirmed win is Rust-monomorphization, which has
   no TS analog (T1).** PROTO-2-4B built concrete generic `Seq2/Seq3/Alt2` structs — the nom/winnow
   route priorart §1 certifies "has no TypeScript analog; V8 does not monomorphize generics; there is
   no concrete `Then<A,B>` type to emit." So 4B's proven throughput mechanism **cannot cross to TS**;
   the TS face must be a *different* mechanism (a build-time emitter over a shared plan), which
   FAMILY-SPECS itself concedes is "the weakest isomorphism of the four." **A confirmed Rust win on an
   un-mirror-able mechanism sharpens, rather than resolves, the constitution-iii crisis.**
2. **[unenumerated by prior adjudication] The isolated number is an UPPER BOUND on the production win,
   and production dilution is unmeasured.** PROTO-2-4B's own honest caveat: the comparison strips
   `dispatch_byte_multi` + SIMD string scanning ("those optimizations are orthogonal and would ride on
   top"). But those kernels absorb the bulk of production runtime (canada is number-bound on
   `scan_number_strict_f64`); once they reabsorb the time, the combinator-glue share — hence the 4B
   speedup — **shrinks below 1.16–1.27×** by an unmeasured amount. The headline is the tax on a
   *simplified* grammar; the end-to-end win against the *shipped* `json_parser()` is unquantified.
3. **[soft edge in the artifact] The closure baseline is a hand-reconstruction, not the shipped macro
   output.** PROTO-2-4B: the `alt!`/`seq!` macros hit nightly-2026-04-11 HRTB/`E0282` warts, so Arm B
   "was written to their exact expansions." The disasm (60 `blr`) corroborates that boxing is present,
   but *exact* fidelity to the macro expansion (arity, ordering, `SmallBox` inlining thresholds) is
   asserted, not compiler-verified. A one-line-off reconstruction could inflate the delta.
4. **TS build-time fuser UNBUILT; PROVE-11 bounds it at ~+5% dispatch.** Generalizing `fuseAll` past
   arity-3 to a vite/ts-transform plugin, isolating dispatch- from allocation-elimination, and the
   bundle-size/`tsc`-clean/three-consumer parity gates are all untouched. "Beyond-SOTA" for 4B-TS is
   unsupported by any artifact.
5. **Self-contradiction unresolved (masked fallback).** Architecture: "No runtime graph at all";
   open-Q (d): the runtime interpreter graph is retained for dynamic grammars. 4B is *fused-sites-only +
   a retained interpreter graph* — a 4A/4C runtime under another name for the unfused remainder. The
   Q-runtime incompatibility map ("4B eliminates any runtime program") is false as stated; scope it.
6. **Generated-size budget is a gate with no threshold.** `generated-size-budget` memory binds a
   per-tranche line ceiling; 4B names it, gives no number, no `.boxed()` depth-cap policy value.
   PROTO-2-4B reports "6.4/7.0 s incremental, negligible size at JSON scale" — true for one grammar, but
   the *policy value* (fuse arity 2–4, table long fan-out) that bounds blowup on the full consumer
   surface is still unset. A gate that cannot fail until a number is named.

---

## 3. FAMILY 4C — VM-CORE · **convergence 28%** (was 30; crowded harder on throughput, iso-value up)

**Why slightly down net.** The confirmed 4B Rust throughput win (1.16–1.27×) dominates 4C's honest
throughput prior (0.7–0.9× loss) by an even wider relative margin than 4A's parity did — 4C's
terminal-runtime bid is now doubly crowded out. Its ONE surviving contribution (the byte-identical
portable-program iso layer) *rises* in value precisely because §6.1 makes isomorphism the binding
constraint — but that contribution is a graft onto the shared VM / 4D unblock, not an independent
terminal, exactly as the REGISTRY BANKED it. Net: marginally down as a family, its iso mechanism
re-homed.

### Exact open gaps
1. **Throughput prior is a loss and nothing ran** — every research-C §6 gate unbuilt. Do not import
   PROTO-1's tree-walk parity as bytecode-VM parity (different overhead regime; a choice-point/value
   stack the tree-walk did on the native stack for free).
2. **[cross-lock, strongest — unmoved] Bytecode VM double-home vs bbnf_ir's live `vm/`.** Verified:
   `crates/ir/src/vm/{bytecode.rs, compiler/{emit,compound,rule}.rs, interpreter/{control,memo,repeat,
   value}.rs}` already ships a value-producing bytecode VM — LOCK-3's IR-lowering seat, the engine 4D
   inherits free. A parse-that-private ISA + two interpreters is a *second* VM;
   `no-orthogonal-codepaths` / `system-cohesion` forbid it. 4C is lawful only if it *is* bbnf_ir's VM
   (collapsing into 4D) or is justified against LOCK-3's contract-tower.
3. **The byte-identical-program oracle — the cheapest, highest-signal, throughput-independent thing the
   family could prove — was SKIPPED.** Nothing serializes a program or decodes it in TS. Per §6.1 this
   is now the *most* valuable unbuilt experiment in the lock (it is the iso mechanism that could rescue
   4B's un-isomorphic Rust win). Fold it into the 4D graft per REGISTRY, do not spin an independent 4C
   thread.
4. **"Strictly stronger than any LOCK-6 mirror-linter" pre-empts LOCK-6.** A LOCK-6 verdict rendered
   inside LOCK-4; if LOCK-6 rules 6C (one wasm impl), the two-decoder oracle is moot.
5. **Left-recursion seed-growth parity rust/ts untested** (research-C risk 4) — the one genuinely subtle
   facility; relevant only if the VM line proceeds via bbnf_ir's memo.

---

## 4. FAMILY 4D — GRAMMAR-SUGAR · **convergence 27%** (was 25; BLOCKED, iso story relatively stronger)

**Why marginally up.** The block is unmoved — but the confirmed 4B result *strengthens 4D's relative
position on the binding axis.* 4D's MessagePack data-crossing (`bbnf_ir::lib.rs:6`) is a language-neutral
representation; 4B's monomorphized structs are Rust-only. If isomorphism is the constraint that actually
decides the lock (§6.1), then 4D's "one IR, two producers, program crosses as data" is the *most*
isomorphism-ready architecture, and 4B is at best its Rust compile backend. That does not unblock 4D — it
still needs a TS execution primitive — but it re-weights the parsimony-plus-isomorphism case in 4D's
favor, which the stale registry (writing before the 4B win) could not see.

### Exact open gaps
1. **[the block, unmoved] The TS-execution primitive is unresolved.** Horn (i) full `GrammarIR` +
   interpreter + passes port = equal-difficulty re-implementation; horn (ii) PROVE-14 wasm full-walk
   ~0.91× — a *measured non-goal* on the value-producing workload. Neither retired. The 4C-decoder graft
   is the un-conceded third horn and is unbuilt.
2. **[cross-lock] Cannot converge inside LOCK-4** — hard dependency on LOCK-5 (is `bbnf_ir` genuinely
   the shared substrate? else "for free" evaporates) AND LOCK-7 (host-fn lattice for `HostClosure`,
   itself unconverged). DAG fact: 4D cannot be *selected* until both land.
3. **4D's decisive number unmeasured** — no `parse_with_ir`-vs-closure paired bench (interpret-mode
   regression lane) nor compile-mode win.
4. **`FnDescriptor::HostClosure` + scoped-opacity guard unbuilt.** Verified: `FnDescriptor` has
   `EnumWrap/BoxWrap/NumberConvert/HexConvert/SpanCapture/Expr{MapExpr}` and **no opaque host-closure
   variant** — a real code-verified gap. PROTO-2 shows the opaque set is tiny (|M|=1) but asserts
   reformulability of the 4 S-sites; it does not demonstrate it.
5. **State-effecting combinators (`peek/lookAhead/recover/mapState`) outside the IR vocabulary** — the
   minimal lawful variant set vs `HostClosure`-with-state "must be measured, not guessed."

---

## 5. THE PROTOTYPES — HARDENED (with the new artifact)

### PROTO-2-4B-FUSION (4B) — RAN, CONFIRMED, the strongest running Rust artifact — but see the reframe
- **Genuine, refutable, well-disciplined.** Disasm gate is a timing-independent mechanism proof (the
  thing CRIT-1 §2 gap 2 demanded); throughput is A,B,A,B sample-interleaved (repairs PROTO-1's
  F-CRUX-3), N=250, two windows, both arm orders, load-symmetric. Concrete-artifact clause satisfied at
  a higher rigor bar than PROTO-1.
- **Gap A — the win is un-isomorphic (§2.1).** Rust monomorphization, no V8 analog; confirming it
  sharpens the constitution-iii problem rather than solving the lock.
- **Gap B — isolated ⇒ upper bound (§2.2).** Strips `dispatch_byte_multi` + SIMD; production dilution
  unmeasured; not a comparison vs shipped `json_parser()`.
- **Gap C — baseline is a hand-reconstruction (§2.3).** Macro warts forced Arm B to be written to the
  believed expansion; fidelity asserted, corroborated by disasm but not compiler-verified.
- **Requested framing correction:** re-label the verdict "4B's Rust *combinator-substrate tax* is
  16–27% and its mechanism (0 `blr`) is proven — an isolated upper bound on a stripped grammar, on a
  monomorphization mechanism with no TS analog; 4B is confirmed as a Rust *compile backend*, not a
  cross-language terminal runtime."

### PROTO-1 (VALUE-PLANE) — RAN, real, over-scoped, and now partly superseded
- Genuine paired cold artifact; the narrow settled fact (Map indirection confines to scalar leaves; 45
  `blr` on leaves, spine inlined) stands.
- Its "graph faster" headline is now doubly discounted: F-CRUX-2 already called it a fusion artifact,
  and PROTO-2-4B isolates the same lever more cleanly at a larger magnitude. Do not carry "4A/4C/4D
  single-tier VIABLE" as settled; carry only "Rust value-plane box confines to leaves."

### PROTO-2 / MONADIC-WALL-CENSUS — RAN, honest, load-bearing; classification asserted
- |M|=1, poison-scope on the LaTeX `environment` rule sound; materially de-risks the shared
  analyzability floor for all four families (opaque = 0.525% ≤ 2%; hot path monad-free).
- Gap A — S-reformulability of the 4 value.js sites is *classified*, not *demonstrated*; reformulation
  entails a public-API change (`branch`/`select`) + read-only-consumer migration (unowned). Verdict
  rests on poison-scope (holds for all 5), not the S/M arithmetic.
- Gap B — "uniformly analyzable" holds for FIRST/dispatch, **not** for value-plane liveness/discard
  analyses (an opaque `map` fn is opaque to those); scope the strong phrasing.
- Gap C — snapshot of shipped consumers, not a property of the algebra.

---

## 6. CROSS-LOCK VIEW (critic-2 weight) — contradictions, double-homes, and the confirmed-4B fallout

### 6.1 — [NEW, the sharpest cross-lock finding this refresh] The confirmed 4B win FORCES a LOCK-6 verdict away from source-mirroring.
4B's throughput is now proven, on concrete monomorphized generic structs, which **have no V8 analog**
(T1; priorart §1). This is the strongest empirical evidence yet produced in the campaign that
**6A (hand-mirrored module trees + linter) and 6B (spec-generated skeletons) cannot express the fast
Rust path** — you cannot mirror a `Seq2<P0,P1>` monomorphization into TS source, because the TS side
has no monomorphization. The lawful isomorphism for a 4B-fast Rust core is therefore **6C (wasm-core:
the Rust impl IS the implementation, TS is bindings)** or **6D (behavioral/oracle iso only; layout free
per language)**. LOCK-6 must consume this: *a measured, confirmed Rust monomorphization win is a
standing counterexample to source-level rust/ts isomorphism for the combinator core.* Correspondingly,
whichever family becomes the **isomorphic** terminal (4A-graph-as-data or 4D-IR-as-data) can adopt 4B
purely as its **Rust lowering** — 4B is the compile backend, not a rival terminal. This resolves the
REGISTRY's latent "4B-as-terminal vs 4B-as-compile-mode" crowding **by evidence**, toward compile-mode.

### 6.2 — Bytecode VM double-home: 4C (and 4D compile-mode) vs LOCK-3 + bbnf_ir's live `vm/` (unmoved).
Verified live `vm/` above (§3.2). Gestalt owes ONE bytecode home: 4C *is* bbnf_ir's VM (collapse into
4D) or a private VM is justified against LOCK-3's contract-tower. Cannot be silently orthogonal.

### 6.3 — Fusion-e-graph double-home: 4A vs LOCK-2's one-graph-engine edict (unmoved).
4A "mirrors `rust/regex/src/egraph/`" for fusion; `egraph`/`egraph-derive` are in-tree crates LOCK-2
owns. 4A's passes must consume LOCK-2's engine; a private fusion e-graph violates `general-infra-crates`
+ `system-cohesion`.

### 6.4 — LOCK-4 ⊗ LOCK-6 co-convergence is now *forced by data*, not just structurally implied.
Before this refresh, the LOCK-4/LOCK-6 mutual dependency was an abstract ordering hazard (three
different 6-verdicts embedded in three families). The confirmed 4B win makes it concrete: **the fastest
Rust runtime is un-mirror-able, so the terminal cannot be named until LOCK-6 rules how rust/ts parity is
achieved** (6C wasm vs 6D oracle). LOCK-4's terminal choice and LOCK-6's iso verdict are now a single
joint decision. Neither freezes alone; both co-converge in GESTALT-P.

### 6.5 — 4D depends on LOCK-5 (substrate) AND LOCK-7 (host-fn lattice) — both unconverged (unmoved).
DAG fact: 4D cannot be *selected* until both land, even if the 4C-decoder graft lifts the TS block.

### 6.6 — PROVE-14 (LOCK-6/6C seed) is a measured non-goal against 4D's wasm horn (unmoved).
0.91× full-walk is refutation-law territory (§0/14); any "unblock 4D via wasm" must beat a measured
non-goal on the exact value-producing workload — a high bar the ordering must respect.

---

## 7. CONSTITUTION & FAILURE-MODE LEDGER

- **Vacuous convergence:** both census probes have real refutable gates; PROTO-2-4B's throughput gate
  could have failed (min-cell rubric) and did not. Not vacuous. **But the lock-level framing was
  vacuous-adjacent and is now falsified:** the REGISTRY's "terminal cannot be named because the 4B
  number is unproduced" rests on a false premise — the number was produced (§0).
- **Spec-cites-itself / pre-empts another lock:** 4C's "strictly stronger than any LOCK-6 mirror-linter"
  renders a LOCK-6 verdict inside LOCK-4 (§3.4, §6.2). 4B architecture ("no runtime graph at all") vs
  open-Q (d) fallback graph (§2.5).
- **Gates that cannot fail:** 4B's generated-size budget still has no quantified threshold (§2.6);
  PROTO-2's poison-scope "test" is a reasoning argument, not an analyzer run.
- **Elegant-reduction trap:** the OLD trap (pick = two briefs + "and-then-pick") is *discharged* on the
  throughput half — 4B's number now exists. A **NEW** elegant-reduction trap replaces it: "throughput is
  confirmed for 4B ⇒ pick 4B" reads the load-bearing "and-then make it isomorphic and cross to TS" step
  as free, when it is 0% built and, for 4B specifically, provably harder (§6.1). 4C's value-tier still
  reduces to "→ 4B"; 4D's Rust elegance still reduces to "and-then the TS primitive."
- **Legacy alias / masked fallback:** 4A `lazy()`-as-sugar unexercised (§1.4); 4B retained interpreter
  graph for the unfused remainder (§2.5). Both flagged, neither exercised.
- **Consumer-less substrate (Y.13):** families preserve real consumers. The live risk remains the
  inverse — 4A/4C build *duplicate* substrates against bbnf_ir's existing consumed ones (§6.2/§6.3).
- **Prototype-evidence misuse:** the sharpest misuse is now a *staleness* misuse — the REGISTRY,
  CRIT-1, and FAMILY-SPECS attribute "unrun / zero evidence" to an experiment that ran and confirmed
  (§0). Secondary: PROTO-1 still credited toward the untested fusion pass and toward non-Rust/non-4B
  decisions; PROTO-2-4B still credited toward a *production* win it explicitly did not measure (§2.2).
- **Zero-runtime-cost types (v):** not engaged by any family this pass (LOCK-8 territory). N/A.
- **Extant-tranche consciousness (ix):** research-B §7 names PT-2 W4 / PT-3 W4 supersessions; the 4B
  confirmed win must be reconciled against PT-2's PROVE-09 "ZERO perf claim" posture — 4B is a *new*
  perf claim on a *different* axis (indirect-call, not SIMD/arena), lawfully admissible because it is
  cold/paired/interleaved/two-window/disasm-gated, but it must be filed as a *named* new claim, not
  folded silently into PT-2's stability-only ledger.

---

## 8. VERDICT & PASS-2 DIRECTIVES

No family is converged; this is pass 1. The refresh changes the *shape* of the non-convergence, not the
fact of it: the throughput half of FORK II is now resolved (→ 4B for Rust), so the deciding open axis is
no longer throughput — it is **isomorphism (0% built) and the Q-ownership substrate ruling (external).**

Pass-2 must:
- **Re-fold PROTO-2-4B-FUSION into the REGISTRY** and re-price 4B off its stale 32% "deferred" floor;
  re-open the "terminal cannot be named" disposition with the corrected reason (isomorphism, not
  throughput).
- **Build the ONE common-baseline 4A-vs-monomorphized-4B experiment** (graph interpreter vs `Seq2`
  structs vs the SAME leaf substrate on the SAME grammar) — the head-to-head neither PROTO-1 nor
  PROTO-2-4B ran (§1.2). Until it exists, Q-runtime is undecided.
- **Produce the FIRST TS number** — a TS tag-interpreter (4A), a TS fuser (4B), or the ~200-LOC
  TS switch-loop decoder over bbnf_ir's serialized `BytecodeProgram` (the 4C⊗4D graft). Constitution-iii
  is 0% built and is now the binding constraint of the lock (§6.1).
- **Measure 4B's production dilution** — fused struct vs the *shipped* `json_parser()` (with
  `dispatch_byte_multi` + SIMD), so the 1.16–1.27× isolated upper bound gets an end-to-end floor (§2.2).
- **Adjudicate the double-homes** (4C-VM vs bbnf_ir `vm/`; 4A-fusion-egraph vs LOCK-2) BEFORE electing a
  terminal — and **treat LOCK-4 ⊗ LOCK-6 as one joint decision**, now forced by the confirmed
  un-mirror-able 4B win (§6.4): the terminal runtime cannot be frozen ahead of LOCK-6's iso verdict and
  LOCK-5's substrate verdict.

**Per-family convergence (critic-2 refresh): 4A 48% · 4B 50% · 4C 28% · 4D 27% (BLOCKED).** Leadership
now splits by axis: **4B leads Rust-throughput-and-mechanism; 4A leads isomorphism-readiness;** neither
is converged, and — because isomorphism is the binding constraint and 4B is its weakest — the confirmed
4B number does **not** license a pick. It re-casts 4B as the Rust compile backend of an isomorphic
terminal yet to be built.
