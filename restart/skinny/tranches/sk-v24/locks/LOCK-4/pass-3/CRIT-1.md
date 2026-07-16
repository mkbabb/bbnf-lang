# LOCK-4 — THE COMBINATOR CORE · PASS-3 · CRIT-1 (adversarial critic 1 of 2, within-lock weighting)

*Read: PORTFOLIO/CONSTITUTION, CROSS-LOCK (XL-A..H), FAMILY-SPECS.md (15:22), research-{A,B,C,D,priorart},
PROTO-1.md, PROTO-1-numeric-leaf.md, PROTO-2.md, PROTO-2-vmvalue.md. Tree-of-record spot-checks run on the
main repo HEAD `c43126422` (the correction's cited tree). Critic-1 weights internal soundness of each family
spec and whether the prototypes tested the sharpest uncertainty.*

---

## THE FINDING THAT REORGANIZES THIS CRITIQUE — the FAMILY-SPECS is STALE against its own two follow-on probes

FAMILY-SPECS.md was frozen at **15:22**. It closes with "PROTOTYPE BRIEFS FOR THE NEXT PASS" — BRIEF 1
(numeric-leaf election lever, 4B) and BRIEF 2 (4C value plane + live dual iso-oracle). **Both briefs were
then executed THIS pass**, after the spec froze:

- **PROTO-1-numeric-leaf.md (15:38)** ran BRIEF 1 → **FAIL.** Canada stays 1.56× (gate ≤1.05×); the lever
  "did NOT fire."
- **PROTO-2-vmvalue.md (15:54)** ran BRIEF 2 → **BUILT + 8/8 GREEN** (in a worktree).

So the FAMILY-SPECS's net disposition is stale in **both** directions: 4B's election lever, named "the
single lever between mixed and elect," **failed**; 4C's "central UNBUILT claim" **got built**. Neither is
folded into any family's convergence framing. This is the load-bearing correction CRIT-1 owes the pass —
every family % below is computed against the probes, not against the frozen spec.

The correction the spec *did* fold (4C value plane UNBUILT on HEAD) is **verified accurate**: on
`c43126422`, `Op`=30 variants (no `ApplyFn`), `Value`=4 shapes (`Span/Tagged/Array/Nil`),
`IrNode::Map{inner,..}=>compile_node(inner)` at `compiler/node.rs:31` (fn_id dropped), vm=1772 LOC,
`rg ApplyFn crates/ir/src`=0. The spec's honesty on this is real and creditable.

---

## 4A — CLOSURE-TO-GRAPH · **70%**

**Sound.** Role correctly demoted to runtime-front reifier + pre-codegen pass host (NOT a terminal; RF-1
refutes the terminal reading, and open-Q3 folds pass-2 PROTO-1's −22% switch-VM refutation onto 4A-TS). The
substrate is real and consuming (IrNode 14-variant, GrammarDag hash-cons, passes/{transform,sets,lr}).

**The elegant-reduction trap, unresolved for a FOURTH pass.** 4A's ONE novel claim — G-A1: `fuse(Graph)→Graph`
on a *combinator-constructed* graph byte-identical to a hand-fused arena — has **zero running evidence across
four passes**, and the spec says so plainly. "The passes exist for the generator's IrNode; pointing them at a
combinator front and proving byte-identity is the unbuilt claim" is a load-bearing step that reads *and-then-
the-hard-part*. The reduction ("the passes already exist") is true for the grammar-text front and **unbuilt**
for the combinator front — the exact thing 4A is chartered to prove. This keeps 4A from converging regardless
of how settled the role is.

**Open gaps (enumerated):**
1. **G-A1 unbuilt (4th pass):** `fuse(Graph)→Graph` on a combinator-constructed graph byte-identical to a
   hand-fused arena — no prototype, no number, four passes. The single novel 4A claim.
2. **Reify→serialize→same-emitter equivalence unbuilt.** PROTO-1 proved grammar-TEXT→emitter generalizes;
   the *runtime-graph → serialize → same emitter → graph-of-record parity* path (open-Q2) is untested. 4A-as-
   source-front is coherent only if this round-trips, and it never ran.
3. **The "analyzable graph" thesis is a repo aggregate, not an invariant.** PROTO-2 PART 1: 1.857% opaque
   nodes repo-wide (K=13.78, 7.7% cushion) but **4.84% math-only (FAIL)**. In closure-dense sub-grammars 4A
   reads as "skeleton with opaque leaves," weakening its own central claim exactly where projection lives.

---

## 4B — COMPTIME-FUSION · **75%** (strongest family; election is further off than the spec frames)

**Sound + built.** G-B0 PASS (recursive JSON, 0 SmallBox/dyn/fn-ptr, value recursion → direct byte-dispatched
`fn` calls). G-B3 PASS (arbitrary host closure fused as 1 named call-out, bbnf-ir compile-proven). G-B1
measured (mixed). Architecturally this is the only family with a running fast face on both languages.

**But the election lever the spec bets on FAILED — and the spec does not know it.** FAMILY-SPECS 4B open-Q1
and INCOMPAT-3 both name the numeric-leaf fast-path as "the single lever between 'mixed' and 'elect 4B as the
Rust fast face'" and "the concrete open number." **PROTO-1-numeric-leaf RAN it and it did not fire**: the
SWAR 8-digit fold + integer fast-path is *inert on canada* (its floats have short digit runs that straddle
the delimiter, so the guarded 8-byte probe always falls to the scalar tail). Canada stays **1.56×,
unchanged**. So:
- The "single lever" framing is **refuted by 4B's own follow-on probe.** The election gate ("beats SOTA
  everywhere") stays uncleared; 4B is electable "for everything EXCEPT number-dominated workloads."
- The residual ceiling is now *named but unbuilt and unmeasured*: the 16-byte SIMD digit stripe +
  `get_unchecked` indexing, both gated on giving the emitted parser a **padded input buffer** — "a larger
  architectural move than a leaf swap... the next probe, not this one." **This is SPEC_ONLY**: the claim that
  padding closes canada is a diagnosis of where the gap lives, not a measured result. 4B's path to election
  runs through an un-prototyped `parse()` input-contract re-architecture.

**The gate-that-cannot-fail, unset for THREE passes (checklist hit).** G-B4 (firing budget: fuse-arity,
dispatch-fanout, boxing-depth, `.rlib`/`dist` + incremental-compile) has anchors (fuse-arity 8, `try_factor_alt`
fires at 4, one box per SCC back-edge) but **no measured number**. The spec labels it "the gate-that-cannot-
fail, still UNSET across THREE passes." A firing budget with no number does not gate the monomorphization-
blowup risk it exists to bound — and that risk is the whole reason 4B needs a budget (constitution vi,
generated-size). Owed on CSS **and** value.js, three passes running.

**Open gaps (enumerated):**
1. **Election lever FAILED (unfolded):** canada 1.56× after the numeric-leaf probe; 4B is workload-
   conditional, NOT a uniform Rust fast face. Not reflected in the frozen spec.
2. **Canada ceiling is SPEC_ONLY:** the padded-input re-architecture (SIMD stripe + `get_unchecked`) that
   would close canada is unbuilt and unmeasured — a hypothesis, not a result.
3. **G-B4 firing budget: no number, three passes.** The gate cannot fail as written.
4. **G-B0 alloc sub-claim un-instrumented:** construction allocs <6 / per-parse =1 are source-census only,
   not dhat-isolated.
5. **Even discounting canada, the win is narrow:** data/twitter WIN, citm is a **TIE not a win**; "beyond-
   SOTA" (charter) is met only on string/structure/small. G-B3's core emission arms (`driver/map.rs` +
   `emit_host_closure` default) are "patch-plausible, NOT cold-built" — the PASS rides partly on an
   uncompiled patch (constitution vii, concrete-artifact honesty; disclosed but load-bearing).
6. XL-E supersession has no PT-tranche home named.

---

## 4C — VM-CORE / PORTABLE-PROGRAM ISOMORPHISM · **70%**

**The value plane got BUILT (uplift over the spec's "0% built") — but the probe tested self-consistency, not
the sharpest uncertainty.** PROTO-2-vmvalue landed the 4 edits in a worktree (Op 30→31 with `ApplyFn`, Value
4→8, Map fn_id emitted, `fns` side table), 8/8 tests, Oracle A byte-identical, Oracle B crossing GREEN. Credit
where due: this discharges the *mechanism-buildability* of G-C0 and is honest that it is a worktree probe
(`rg ApplyFn` on the main HEAD is still 0 — **not landed on record**).

**But G-C0's actual gate was not tested — the oracle-self-forgery trap the research explicitly warned of.**
FAMILY-SPECS G-C0's gate is: "the VM produces value-projected output **byte-identical to the closure core's
`.map`** on a projection-dense grammar (differential oracle vs parse-that's closure parse)." PROTO-2-vmvalue's
Oracle A asserts identity against **"an independently specified golden"** — an author-written expected value,
**not** parse-that's live `.map`. Oracle B asserts Rust-VM ≡ TS-decoder (crossing self-consistency), **not**
VM ≡ closure core. So the semantic-correctness gate — *does the VM compute the SAME values the shipped
combinator does?* — **never ran.** research-C §4/§10-R3 and priorart §7.11 cite McKeeman/Csmith precisely on
this: "a fabricated golden hides exactly the divergences the oracle exists to catch." Oracle A's golden is
that fabricated golden for the semantic ground truth. The value plane is built + self-consistent + crossing-
identical; it is **not** proven byte-identical to the incumbent `.map`, which is the gate as written.

**Worse, the hardest value case is a pass-through stub.** PROTO-2-vmvalue states its `apply_fn.rs` "handles
`HostClosure` as a pass-through — its body is opaque." So for arbitrary-closure `->` sites the VM carries the
value structurally and evaluates **nothing** — G-C0's "byte-identical to `.map`" is not merely untested for
HostClosure, it is *un-testable* by construction there. The value plane computes the closed descriptors
(Number/Hex/Enum/MapExpr); the open one it forwards.

**G-C1's mandatory adversarial member was not crossed.** FAMILY-SPECS G-C1 requires the dual oracle "over the
six R3 corpora + one projection-dense grammar," with "**canada the mandatory adversarial member**" (PROTO-1's
f64 leaf is where routes diverge). PROTO-2-vmvalue's own BOUNDARY: the six JSON corpora were **NOT** executed
by an independent TS interpreter — blocked on porting `parse_that::regex::Dfa` to TS. So the recognizer-plane
Value-tree crossing over the corpus — the half that includes canada — is **unrun**, and it carries an unowned
regex-divergence risk (bespoke Dfa vs V8 `RegExp`, research-C R5) the moment the TS port lands.

**Open gaps (enumerated):**
1. **G-C0's real gate untested:** VM output vs parse-that's live closure `.map` never differentially checked;
   the probe substituted an author-specified golden (the McKeeman trap). Semantic correctness of record open.
2. **HostClosure value case is a pass-through stub** — the VM evaluates no arbitrary closure; byte-identity
   there is un-testable by construction.
3. **G-C1 canada / full-corpus crossing unrun**, blocked on a TS `Dfa` port (bounded engineering + latent
   Dfa-vs-RegExp divergence risk).
4. **G-C4 msgpack idempotence unexercised:** the probe used `serde_json`, not `rmp_serde::to_vec_named`; the
   born-RED guard against MessagePack externally-tagged-variant reorder (research-C R4) was never fired on the
   bus it ships on.
5. Value-plane home double-home (LOCK-3 internal-descriptor vs LOCK-7 host-closure) un-adjudicated → correctly
   exported to GESTALT-Ω.

---

## 4D — GRAMMAR-SUGAR / SHARED-IR OWNERSHIP · **68%** (honestly BLOCKED/split; bounded by predecessor DAG)

**Sound disposition.** 4D is correctly dispositioned: terminal role refuted three horns over (horn i = full
grammar-engine TS port is a primitive of equal-or-greater difficulty → BLOCKED-candidate *and* XL-D retires
the need; horn ii = PROVE-14 wasm full-walk 0.91× loss on exactly the value-producing workload; horn iii =
shared decoder −22% on V8). Gate-0 (Cargo cycle) severed mechanically; GAP-1 (HostClosure type-plane)
discharged compile-proven (PROTO-2 PART 2). research-D's split-the-barrier finding (47% of latex-paper's
non-projection sites recoverable as analyzable `Record`/`Array`/`Select` data; single-digit true-opaque
residue) is a genuine sharpening, and the spec correctly self-guards against "4D dominates on parsimony ⇒
elect it" (dominance is conditional on LOCK-5 ruling 5A, and 4D's Rust runtime is a measured loss).

**Residual soundness note (not a defect, a watch-item).** The spec's open-Q1 keeps the 5A-vs-5B
shared-substrate-semantics question live and DAG-blocking on LOCK-5. XL-C retired the regex **functor** (5B for
the *regex* axis) — but the 4D ownership question (is parse-that's runtime IR literally `bbnf_ir`?) is a
distinct axis, correctly still-open. The export is clean; the risk is only that a reader conflates XL-C's
regex-functor retirement with the grammar-IR-ownership question. The spec keeps them separate — good.

**Open gaps (all predecessor-owned / report-only per Q8):**
1. **INCOMPAT-1 (Q-ownership) → LOCK-5:** 5A shared type vs 5B lawful functor for the grammar IR; DAG-blocking,
   un-resolvable inside LOCK-4.
2. **HostClosure lattice → LOCK-7:** typed shape + per-subsystem floor (value.js's ~5 Pratt folds; the tier-1
   Record/Array/Select boundary).
3. **Node-ratio thin (1.857% repo / 4.84% math):** is fusion-inheritance safe on closure-dense sub-grammars,
   or only in aggregate? Open (shared with 4A gap 3).
4. **Value-plane home split → GESTALT-Ω** (recommended, not settled).

---

## CROSS-CUTTING FAILURE-MODE AUDIT

- **Elegant-reduction trap — TWO hits.** (i) 4A's G-A1 ("the passes already exist") is true for the
  grammar-text front and unbuilt for the combinator front — four passes. (ii) 4B's "single lever" reduction
  read *and-then-the-numeric-leaf*, and the numeric leaf turned out unconverged: the follow-on probe failed
  and the real ceiling (padded-input re-architecture) is unbuilt.
- **Gate-that-cannot-fail — TWO hits.** (i) G-B4 firing budget: no number, three passes. (ii) G-C0's gate as
  *tested* was weakened from "byte-identical to the closure core" to "byte-identical to an author-specified
  golden" — a gate that cannot catch a VM-vs-incumbent divergence.
- **Oracle self-forgery / faked golden — ONE hit.** 4C Oracle A's "independently specified golden" is exactly
  the hand-authored semantic ground truth the McKeeman/Csmith discipline (cited in research-C §4 and priorart
  §7.11) forbids. Oracle B's live golden proves only Rust↔TS crossing consistency, not correctness of record.
- **SPEC_ONLY anchoring a claim — TWO hits.** (i) 4B's canada-parity-via-padding is an unmeasured hypothesis
  presented as the known ceiling. (ii) G-B3's core emission arms are "patch-plausible, not cold-built" — the
  PASS rides partly on an uncompiled patch (disclosed).
- **Stale-evidence / unfolded follow-on probes — the governing hit.** FAMILY-SPECS predates and folds
  neither PROTO-1-numeric-leaf (FAIL) nor PROTO-2-vmvalue (BUILD). Its "next-pass" briefs were run *this* pass.
- **Consumer-less substrate (constitution viii / Y.13) — CLEAN.** The VM has live consumers (DAP adapter, JSON
  bench); the `ApplyFn` completion's consumer is the iso-oracle + interpret/LSP lane. No violation.
- **Isomorphism clause (constitution iii) — honestly handled but half-tested.** INCOMPAT-2 (Rust
  monomorphization ≠ TS emission ⇒ no shared isomorphic terminal ⇒ 6C/6D, away from 6A/6B) is a genuine
  refutation of source-level rust/ts iso, correctly exported to LOCK-6/LOCK-8. 4C's competing "iso by
  construction (parser crosses as data)" is proven for the value-plane descriptors (9 crossed GREEN) and
  **unproven for the recognizer plane over the corpus** (canada unrun). The lock's strongest iso claim is
  half-tested.
- **Base-law (XL-H) — CLEAN but note the split-tree honesty burden.** Protos base-pin `ce51c36fc`; the
  correction is tree-verified on `c43126422`. The PROTO-2-vmvalue "build" is worktree-local (not on the main
  HEAD — verified `rg ApplyFn`=0). "8/8 tests pass" must read as *provably buildable + self-consistent in a
  scratch worktree*, NOT *landed + validated-against-incumbent*. The spec's "0% built" for the record is
  correct; the probe's discharge is buildability, not convergence.
- **Minor accuracy flag.** FAMILY-SPECS says "Pass-3 built it: grammar/json/json.bbnf ... emitted." PROTO-1
  states json.bbnf + json.rs were "already realized in-tree at base-pin"; pass-3 *measured* a pre-existing
  artifact. Evidence is valid either way; the "built this pass" diction overstates.

---

## NEXT-PASS DIRECTIVES (within-lock)

1. **Refold the disposition against the two follow-on probes.** 4B is workload-conditional (election lever
   FAILED); 4C's value plane is buildable-in-worktree but its correctness-vs-incumbent gate is untested.
   Update every family % and the net disposition; the "single lever" and "0% built" framings are both dead.
2. **4C — run G-C0's REAL gate.** Differential oracle: VM `ApplyFn` output vs parse-that's live closure `.map`
   on a projection-dense grammar. An author-specified golden does not discharge it. Land at least one
   HostClosure evaluation (not a pass-through) so the hard value case is testable.
3. **4C — cross canada.** Port the TS `Dfa` (or restrict to LOCK-5's certified grammar-lowerable regex class)
   and run G-C1 over the six corpora incl. canada; fire G-C4 on `rmp_serde::to_vec_named`, not `serde_json`.
4. **4B — the padded-input probe or the named ceiling.** Either prototype the padded `parse()` input contract
   (SIMD stripe + `get_unchecked`) and re-measure canada, or record canada 1.56× as 4B's measured ceiling and
   stop calling it "a single lever." Produce G-B4's number on CSS + value.js.
5. **4A — build G-A1 once.** `fuse(Graph)→Graph` on a combinator-constructed graph, byte-identical to a
   hand-fused arena, over one grammar. Four passes without it is the family's whole risk.
