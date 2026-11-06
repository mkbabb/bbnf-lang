# LOCK-4 — THE COMBINATOR CORE · PASS-2 · CRIT-1 (adversarial, within-lock weighting)

*Critic 1 of 2, pass 2. Weighting: internal soundness of each family spec + whether the pass-2
prototypes tested the sharpest uncertainty. Inputs read in full: PORTFOLIO/CONSTITUTION,
FAMILY-SPECS (pass-2), PROTO-1.md + PROTO-2.md, research-{A,B,C,D,priorart} headers, the pass-1
REGISTRY + pass-1 CRIT-1/CRIT-2 (for carry-over accounting). Four load-bearing code facts
re-verified live this pass (all GREEN, my own grep): `crates/ir/Cargo.toml:13 parse_that = "0.4"`
(the cycle); `crates/ir/src/vm/compiler/node.rs:31 IrNode::Map{inner,..} => compile_node(inner,ir)`
(the Map-drop); `FnDescriptor` = EnumWrap/BoxWrap/NumberConvert/HexConvert/SpanCapture/Expr, **no**
HostClosure (`types/fn_descriptor.rs:18`); `IrNode` = 13 top-level variants (`types/node.rs:30`).*

**Verdict up front.** Pass-2 is the strongest pass this lock has had, and I credit its central
event twice: the two prototypes hit the two sharpest uncertainties pass-1 named — PROTO-1 the
fast-isomorphic-TS face (INCOMPAT-2), PROTO-2 the cycle-break + value-plane floor (INCOMPAT-1 gate-0
+ F-4) — and **both RAN, both are refutable, both moved the lock.** INCOMPAT-2 is now *decided by
measurement* (build-time emitter ×1.23; runtime decoder −22%, ties naive; SpanParser law survives).
Gate-0 is now *proven feasible* (cycle severed, `cargo tree` acyclic, negative control confirms
load-bearing). The value-plane is *built and byte-identical* (Op::ApplyFn + fns + Value scalars,
oracle GREEN). This is real convergence, not status prose.

**But the reframe over-reaches at one seam, and it is load-bearing.** The pass-2 "single tower over
`bbnf_ir::IrNode`" is verified at its *ends* (gate-0, value-completion, TS-emitter) and asserted at
its *middle* (4A analyzable front → 4B Rust lowering). The middle hides a comptime-vs-runtime
coherence gap that no prototype touched and no spec disambiguates (RF-1 below). Three families'
reason-to-exist keystones remain 0% built (4A fusion-on-a-combinator-graph; the three-way common
baseline; the general grammar-driven emitter). The numbers below reflect both the genuine lift and
the untouched cores.

---

## THE REFRAME / TOWER — cross-cutting findings

### RF-1 (SHARPEST) — The tower conflates parse-that's *runtime* combinator API with a *comptime* monomorphization it never builds. "4B is the Rust lowering of the shared IR" is SPEC-ONLY and possibly incoherent.

The reframe's spine is a claim of composition: *4A constructs the graph → 4B is its Rust lowering →
4C is its portable crossing.* Two of those arrows are measured; the 4A→4B arrow is neither measured
nor coherent as stated.

- 4D/4A build a **runtime value**: `.then()` ≡ `Seq(vec![...])`, an `IrNode` value. PROTO-2 ran it as
  a runtime value — `Interpreter::new` per parse, 8 `Vec`s + an `FxHashMap` allocated at runtime.
- 4B's confirmed win (PROTO-2-4B, pass-1) is **type-monomorphization**: `.then()` ≡ `Seq2<A,B>`, a
  distinct *concrete type* the Rust compiler specializes at compile time. Monomorphization requires
  the types to be known at compile time; it cannot be derived from a `Vec<Op>`/`IrNode` that exists
  only at runtime.
- Therefore "the graph erases into AOT Rust (4A-source-of-4B)" (FAMILY-SPECS §4A reframe; Q-A3) is
  only realizable via a **build-time codegen step that reads a comptime-known graph and emits
  concrete structs** — which is precisely what the *generator* already does from `.bbnf` grammar
  text, and is **not** what parse-that's live combinator API does. The tower silently assumes the
  combinator API can be a comptime construction; the measured evidence (PROTO-2) is that it is a
  runtime one.

Consequence: the tower composes cleanly for the **generator path** (grammar text → IR → monomorphized
Rust), but for **parse-that's own combinator API** it delivers only the *interpret* terminal (measured
2.2× adverse, PROTO-2) or keeps today's closure core — **not** a 4B-fast Rust terminal. The specs never
draw this line. This is the pass-2 elegant-reduction trap: "the tower converges the mechanisms" reads
the load-bearing "and-then make the runtime combinator graph reach comptime monomorphization" step as
free, when it is 0% built and, for a runtime API, not obviously possible without a codegen reflection
step nobody has specified. **A confirmed win (4B-Rust) on a mechanism the tower's own front (4A/4D
runtime graph) cannot feed is a standing incoherence, not a lowering.**

### RF-2 — Prototype tested 4C's REFUTED axis exhaustively and its SURVIVING axis not at all.

PROTO-1 measured the bytecode-decoder-as-TS-runtime (4C's *terminal* bid) to ±0.8% across three cold
processes and refuted it (−22% vs emitter). Excellent. But 4C's *surviving* contribution — the one the
spec says "RISES" — is the **byte-identical portable-program isomorphism layer** (G-C1: decoded `Op[]`
structurally == Rust golden; TS `Value` tree node-for-node == Rust `Interpreter` over six corpora).
That is the correctness bridge every isomorphic terminal needs, and PROTO-1 **did not build it**: its
lane-A msgpack was *hand-encoded in-harness* (the report's own honest provenance note), not a live Rust
`to_vec_named` round-trip, and only one region's dispatch cost was measured — never the six-corpora
Value-tree equivalence. So the axis that survives has **zero running evidence for its actual claim
(crossing correctness)**; the axis that dies got a gold-plated measurement. The sharpest uncertainty
*for 4C's risen role* was not the one tested.

### RF-3 — "Q-ownership axis DECIDED" over-reads gate-0: feasibility ≠ decision (internal inconsistency).

PROTO-2's verdict line and FAMILY-SPECS both say gate-0 "decides the entire Q-ownership axis." But
INCOMPAT-1's own text says the resolution "is **LOCK-5's call**," and Q-D2 remains open: *is `bbnf_ir`
genuinely the shared type (5A) or a lawful functor across separate HIRs (5B, which evaporates "inherit
for free")?* Gate-0 proved the cycle is **breakable in a scratch by moving `Dfa` to `bbnf_regex`** — a
*feasibility* result. Whether that move is the lawful substrate form, and whether the two ends actually
share one type vs. two functored types, is external and unresolved. The axis is *unblocked*, not
*decided*. The spec should say feasibility-proven; it says decided.

### RF-4 — The value-plane decomposition is honest attribution but risks reading as viability.

PROTO-2 measured the shared-VM interpret terminal at 0.021×–0.46× closures — a clear negative — then
decomposed it: value-plane tax proper (Op::ApplyFn) ~5%, dominant gap = per-parse `Interpreter::new`
setup allocation (a tape-substrate cost). The attribution is legitimate and useful. But note the
residue the decomposition does *not* dissolve: even amortized over a realistic single parse (0.46×),
and even against an admittedly *best-case zero-alloc* closure baseline (not parse-that's real
combinator), the shared-VM interpret terminal is **still ~2.2× slower**. "The value plane rides ~free"
is true and must not be misread as "the interpret terminal is viable" — it is measured-dead as a fast
path regardless of which layer owns the cost. FAMILY-SPECS mostly holds this line (calls it 4D's honest
negative), but the "≈free" framing is one inference away from a masked-fallback: relabeling a loss by
its cause does not convert it to a win.

### RF-5 — INCOMPAT-3 (three-way Rust runtime fork) is still INFERRED, never measured on a common baseline (carry-over, third pass).

PROTO-2 measured VM-interpret vs closure (VM loses). PROTO-2-4B (pass-1) measured monomorphized vs
closure (mono wins). The *implied* ranking (mono > closure > VM-interpret) is strong — but the single
experiment that would make it a measurement (4A graph-interpret vs 4B `Seq2` vs 4C VM on **one** leaf
substrate + **one** grammar) is unbuilt for the third consecutive pass. The spec correctly labels the
fork "INFERRED"; it is nonetheless the lock's oldest untouched keystone.

---

## 4A — CLOSURE-TO-GRAPH — **convergence 50%** (was 45)

**Why up, only modestly.** Role-clarity is a real gain: the reframe fixes 4A as *source front, not
terminal*, which PROTO-2's adverse interpret number actively supports (erase the graph into AOT rather
than run it). Gate-0 retires 4A-route-b's Cargo-cycle hazard. But 4A's *reason to exist* — fusion as
`Graph→Graph` passes over a **combinator-constructed** graph — was not exercised by any pass-2
prototype, for the third pass running.

**Enumerated open gaps.**
1. **[keystone, 0% built — third pass] G-A1 `fuse(Graph)→Graph` byte-identical to a hand-fused arena
   is UNBUILT on a combinator-constructed graph.** The spec's "the passes EXIST, not hypothetical"
   upgrade is an evidentiary sleight: `fuse.rs`/`fuse_token`/`pattern_dedup`/`inline.rs` operate on the
   *generator's* `IrNode` from `.bbnf` text, not on a combinator front. The spec itself concedes the
   keystone is "point the existing passes at a combinator-constructed graph and oracle byte-identity" —
   which is exactly what is unbuilt. 4A's one novel claim still has zero running evidence.
2. **[RF-1] The 4A→4B "Rust lowering" arrow is incoherent for a runtime combinator graph.** How a
   runtime-constructed `IrNode` reaches comptime monomorphization is unspecified (SPEC-ONLY; possibly
   requires a codegen-reflection step = the generator path, not the combinator API).
3. **[RF-5] The three-way common-baseline Rust fork (4A-interpret vs 4B-mono vs 4C-VM) is STILL not
   built.** "4B beats 4A" remains an inference across two non-comparable measurements.
4. **[Q-A2] The `Opaque(HostFnId)` barrier surface size on value.js/latex-paper is UNMEASURED** — the
   number that decides whether the analysis story holds. G-A3 untouched.
5. **[cross-lock, unmoved] Isomorphism (G-A6) 0% built in TS; the `switch(node.tag)` interpreter's V8
   megamorphism risk (R4) is now *confirmed adverse* by PROTO-1's lane-A −22% — the graph-survives-to-TS
   route is measured-slow. LOCK-6 dependency.**

## 4B — COMPTIME-FUSION — **convergence 60%** (was 50)

**Why up — the pass's biggest single family lift.** PROTO-1 produced the **first TS number** on the
axis pass-1 recorded as 0% built: a build-time-emitted specialized closure beats the `.parser`-IC
baseline **×1.23** (three cold processes, both orders, zero `.parser(` in the emitted body), and in the
same experiment refuted the runtime-decoder rival at −22%. This validates the *mechanism* of the TS
fast face and resolves INCOMPAT-2 in 4B-TS's favour. The Rust half was already CONFIRMED (pass-1). 4B
now has positive evidence on **both** language faces' mechanisms — no other family does.

**Why not higher.**
1. **[general emitter UNBUILT] PROTO-1 hand-emitted `new Function` for ONE arity-7 region.** The
   deliverable — a grammar-driven build-time fuser that specializes *arbitrary* combinator regions from
   the shared op-list — is SPEC-ONLY. The gap between "one hand-emitted closure ×1.23" and "the TS
   backbone's automatic specializer" is the actual work, and it is untouched (G-B3 partially met for one
   region only; three-consumer parity / bundle-size / tsc-clean untouched).
2. **[F-3, unmoved] Production dilution unmeasured.** G-B1 (fused struct vs the *shipped* `json_parser`
   with `dispatch_byte_multi` + SIMD on **both** arms) is unbuilt; the 16–27% Rust win stays an upper
   bound; "beyond-SOTA" vs the tuned incumbent is unestablished.
3. **[G-B4, gate-that-cannot-fail, unmoved] The generated-size / monomorphization-blowup budget is "a
   gate with no threshold."** The spec restates this verbatim without setting the number. A gate that
   cannot fail is not a gate. Unset across passes.
4. **[RF-1 / structural] Two mechanisms sharing one name.** The Rust fast face (type-monomorphization,
   no V8 analog) and the TS fast face (build-time source emission) are *divergent by construction* —
   PROTO-1 §8 says so honestly. 4B therefore cannot itself satisfy constitution-iii's "isomorphism in
   code" clause for the execution face; it forces LOCK-6 to 6C/6D. Correctly named, not resolvable
   inside LOCK-4.

## 4C — VM-CORE / PORTABLE-PROGRAM ISOMORPHISM — **convergence 48%** (was 28)

**Why up sharply.** Three things resolved: (a) the terminal-runtime-on-V8 bid is now
**measured-refuted** (PROTO-1 lane-A −22% vs emitter, ties naive; the honest loss prior is no longer a
prior but a datum), moving 4C's disposition from inferred-BANKED to evidence-BANKED; (b) the
value-completion (G-C2) is **built and byte-identical** (PROTO-2 oracle GREEN — the same Op::ApplyFn +
fns + Value fix 4C and 4D share); (c) the dependency-homing spike (G-C3) is **satisfied** by PROTO-2's
gate-0 + negative control.

**Why not higher.**
1. **[RF-2, the surviving axis is untested] G-C1 (the byte-identical crossing correctness — decoded
   `Op[]` == Rust golden AND TS `Value` tree == Rust Interpreter over six corpora) is UNBUILT.** The
   prototype hand-encoded msgpack and measured only dispatch speed. 4C's *risen* contribution has zero
   running evidence for its actual claim.
2. **[G-C4] Wire-contract regen-idempotence (encode∘decode∘encode = encode, variant tags pinned)
   UNBUILT.**
3. **[dissolution accounting] 4C-as-a-family is largely RETIRED-into-tower.** Its ISA/VM *is* `bbnf_ir`'s
   live VM (a private second ISA would be an abrogate-before-patch violation); its value-completion ≡
   4D's; its surviving iso-layer folds into the LOCK-3/`bbnf_ir` line. The 48% measures a well-characterized
   contributor with narrow cheap remaining gaps, not an independent terminal.

## 4D — GRAMMAR-SUGAR / SHARED-IR OWNERSHIP — **convergence 45%** (was 25) · **BLOCKED (confirmed)**

**Why up substantially, still BLOCKED.** The hardest predecessor is proven feasible: **gate-0 PASSED**
(G-D0 — cycle severed, `cargo build -p bbnf-ir` compiles without `parse_that`, `cargo tree` acyclic,
negative control shows the gate load-bearing). Value-completion is **built + byte-identical**. The
value-plane floor (F-4, 4D's sharpest pass-1 risk) is now **measured** — negative, but the negative is
attributed (setup allocation, not the value plane). A BLOCKED family's convergence measures how fully
the block + salvage is characterized; that characterization is now near-complete.

**Why still BLOCKED, and enumerated gaps.**
1. **[block confirmed, fast-TS horn now measured-HARDER] The fast-isomorphic-TS face 4D needs is the
   lock's shared unbuilt primitive, and PROTO-1 sharpened horn-(iii) from prior to datum** (−22% V8
   decoder). Horn-(i) (full engine re-port = equal-difficulty) and horn-(ii) (wasm 0.91×, PROVE-14)
   stand. The block is more evidenced, not lifted.
2. **[RF-3] Q-ownership is unblocked, NOT decided.** Q-D2 (5A shared type vs 5B lawful functor) is
   LOCK-5's ruling; gate-0 proved feasibility only. If 5B, "inherit the optimizer/VM/bus for free"
   evaporates and 4D's entire value proposition weakens.
3. **[RF-4] The interpret terminal is measured-dead (~2.2× even amortized, even vs a best-case
   closure).** `Op::ApplyFn`'s only fast-path consumer would be a terminal that loses; its real consumer
   is the slow interpret path (LSP/DAP, dynamic grammars) — a legitimate but slow-only consumer. The
   value-plane-vs-closure throughput question (Q-D1) closes NEGATIVE at these sizes.
4. **[Q-D3, unmoved] LOCK-7 host-fn lattice shape for GAP-1/4/5** (`map` with arbitrary host closure —
   there is no `FnDescriptor::HostClosure`, code-verified; `recover`/`mapState`) is external and open.

---

## CONVERGENCE-PACING NOTE (within-lock)

This lock cannot reach 100% inside LOCK-4: **both** irreducible forks are externally gated — Q-ownership
by LOCK-5's substrate ruling (5A vs 5B), the execution-face isomorphism by LOCK-6's verdict (6C/6D
forced by the confirmed no-V8-analog Rust win). The honest ceiling for a *converged core with named
gaps* is: INCOMPAT-2 measured-decided (emitter, not decoder); gate-0 feasibility-proven;
value-completion built + byte-identical; the terminal-election deferred to LOCK-5⊗LOCK-6 with exact
predecessors named. The three within-lock items that CAN still close and have not — and should drive
pass-3 — are: **(1)** the general grammar-driven build-time emitter (4B-TS deliverable, not one hand
region); **(2)** the three-way common-baseline Rust fork (RF-5 / INCOMPAT-3), unbuilt for three passes;
**(3)** the RF-1 comptime-vs-runtime coherence question — does parse-that's combinator API reach 4B
monomorphization at all, or only via the generator's codegen path? Until RF-1 is answered, "the tower
converges the mechanisms" is asserted at its middle.

### Two consecutive clean passes: NOT YET. Fresh non-author audit: this is it (pass-2). Gaps remain enumerated and non-empty for every family. No family converged.
