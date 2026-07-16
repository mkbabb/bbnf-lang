# LOCK-5 · Family 5C — AUTOMATA-AS-EXTRACTION (pass-3 research-C)

Determinization, minimization, and prefilter-tier selection recast as congruence closure + CSP
extraction, **measured** head-to-head against the classical pipeline. Wins stay; losses are named
negative results. This report answers Q5 (minimization-as-congruence vs a *correct* Hopcroft over a
grown acyclic/cyclic corpus) and Q6 (tier selection: table vs CSP; coupling-witness or none). It is
grounded in a bake-off that **ran** — the numbers below are measured, not banded.

Engine home: `parse-that/rust/regex` (`bbnf-regex`, 6742 LOC confirmed, READ-ONLY canonical). Shared
substrate: `parse-that/rust/regex/src/egraph` (HIR e-graph, congruence closure over `HirENode`) +
the bbnf-side `csp_solver` crate consumed at `skinny/crates/passes/src/decision_csp.rs`. Base-pin:
parse-that HEAD, scratch-copied per R3 §8; the canonical tree is untouched.

---

## 0. The measured verdict (both questions answered)

**Q5 — minimization-as-congruence RETIRES as an e-graph centerpiece; Revuz survives as a table
optimization; the `:358` restart-delete is mandatory but pays nothing at regex scale.**
**Q6 — tier selection is LOCAL; the monotone table wins; the CSP is a consumer-less substrate
absent a shared-LUT budget that does not exist in-tree.**

Both are negative results in the charter's sense (5C's mechanism, honestly measured, mostly loses),
and both are load-bearing: they tell GESTALT-P to keep `derive_feasible_engines` as a table and to
route minimization through a **correct** classical control, not an e-graph, with a Revuz single-pass
fast path dispatched on acyclicity.

---

## 1. The load-bearing defect that framed Q5

`parse-that/rust/regex/src/automata/dfa.rs:291` `fn hopcroft_minimize` **is not Hopcroft.** It is
Moore partition refinement with a full **restart on every split**: `dfa.rs:358`
`break; // restart refinement from the beginning`. Each split rescans all blocks × classes × members
from scratch — no worklist, no "process the smaller half", no predecessor lists. Worst case is
**~O(n³·k)**. The header claims Hopcroft; the body is a naive restart-refiner. The `flat_transitions`
table (`dfa.rs:50`) is never inverted into predecessor lists — the exact datum a real Hopcroft needs.

This is the crux the charter forced: **any earlier "acyclic fast path beats Hopcroft" claim was
measured against this restart-refiner, not against O(n·k·log n).** Q5 cannot be adjudicated until a
correct control exists — so one was built.

---

## 2. Mechanism — minimization as congruence, one engine, two schedules

**Core identity.** Two DFA states are equivalent iff they are congruent: same accept bit and, for
every byte-class, targets that are themselves congruent. That is exactly the fixpoint an e-graph
computes by congruence closure. The DFA-state e-node is isomorphic to the existing `HirENode`
(`egraph/node.rs`, children projected to `egraph::Id`, inserted via `egraph/translate.rs:19`
`insert_hir`):

```
DfaSig { accept: bool, targets: Box<[Id]> }   // len == num_classes; DEAD → a reserved dead Id
```

Hash-cons over `(accept, targets)` under union-find merges congruent states — the single mechanism.
It admits two schedules, and acyclicity dispatch is a *scheduling* decision on the **same** engine,
not an orthogonal codepath (no-orthogonal-codepaths law):

- **Acyclic (Revuz single pass).** With no back-edge, process states in reverse-topological order;
  targets are canonical when visited, so **one** congruence pass suffices — O(n·k) with a radix sort
  of signatures. Revuz, *Minimization of acyclic DFAs in linear time*, TCS 1992.
- **Cyclic (iterated congruence / saturation).** With back-edges, iterate signature-refinement to a
  fixpoint (LLVM-style `Changed` bool). This is Moore expressed as saturation; worst case
  **O(n²·k)** — the SAME class as the naive refiner, so the e-graph earns nothing here.

---

## 3. Q5 — the reference control and the bake-off that ran

Five lanes over one **unminimized** DFA per pattern (`Dfa::compile_with`, `minimize:false`), each
timed sample on a fresh clone (clone excluded), median of 401 samples, single process, cold/paired:

| lane | what | complexity |
|---|---|---|
| **L0 incumbent** | `dfa.rs:291` verbatim — Moore + full restart on every split | ~O(n³·k) |
| **L1 null** | incumbent **minus** `:358` — fall through, loop to a clean fixpoint pass | O(passes·n·k) |
| **L2 valmari** | Hopcroft **worklist** (inverse-image split) + explicit virtual DEAD sink completing the partial DFA (the `dfa.rs:330/:340/:423` DEAD edges made a real trap state) | O(n·k·log n) |
| **L3 revuz** | acyclic single **reverse-topological** congruence pass; acyclic-tag only | O(n·k) |
| **L4 congruence** | `DfaSig` **hashcons rebuild to fixpoint**; passes-to-fixpoint counted | O(passes·n·k) |

**Corpus (Q5 manifest).** `COMPILE_PATTERNS` grown **26 → 56**; tag **computed at runtime** by an
iterative back-edge DFS (`is_acyclic`), not asserted → **12 acyclic / 44 cyclic** (≈21% acyclic).
Acyclic = bounded/DAG leaves (`[01]{8}`, `\d{4}-\d{2}-\d{2}`, `#[0-9a-fA-F]{6}`, bounded IPv4,
bushy alternations `(?:Mon|…|Sun)`). Cyclic = star/plus (`[a-z]+`, `\w+`, `[^"\\]*`, `.*?lit`).
`num_classes` (`dfa.rs:54`, u16, byte-equiv-compressed at `dfa.rs:80` `compute_byte_classes`) is
≤8 for grammar leaves, so `DfaSig.targets` is a tiny radix-sortable `[Id; ≤8]`.

**Correctness oracle FIRST.** Every lane's output is reduced to a BFS-canonical form
(renumbering-invariant: accept + per-class canonical target, DEAD = −1) and compared byte-for-byte
**before any timing**:
```
corpus: 56 patterns  acyclic=12  cyclic=44  divergences = 0
```
All four minimizers + incumbent emit **byte-identical minimal DFAs** on every one of the 56 patterns.
The incumbent's *result* is correct; only its *cost model* was ever in question. Zero divergence also
proves the sentinel-Moore / virtual-sink-Hopcroft / topological-Revuz partial-DFA relations agree on
this corpus (no trap-merge disagreement).

**The decisive, allocation-independent measure — congruence passes-to-fixpoint:**
```
max passes = 8  at n=9   ([01]{8})      worst passes / log2(n) = 2.00
n=2 → 1 pass    n=20 → 3 passes (bushy, shallow)
```
Passes track DFA **diameter**, not log n. On the **chain-structured** acyclic DFAs that *are* the
fast-path's target, passes ≈ depth ≈ n (`[01]{8}` n=9 → 8 passes; `\d{4}-\d{2}-\d{2}` n=11 → 6;
IPv4 n=16 → 6). `log n` held only for the **bushy alternations** (shallow). The `DfaSig` rebuild is
therefore **O(n²·k)** on exactly the class it was meant to accelerate — refuting the O(n·k·log n)
claim. Revuz does all of them in **one** reverse-topological pass by construction.

**Timings (Σ median ns over the 12 acyclic patterns):**
```
incumbent=24124  null=24376  valmari=86669  revuz=17168  cong=37664
null   vs revuz : null/revuz = 1.42×   (delete-:358 does NOT fully recover the acyclic win)
incumbent ≈ null: 24124 ~ 24376  (<1% — the strawman is not slow at n≤20)
```
Two findings fall out: (i) `cong` (37664) is the **slowest correct lane** on acyclic — the e-graph
indirection loses outright; (ii) `incumbent ≈ null` proves the `:358` restart was **never** the
source of any "acyclic-beats-Hopcroft" gap at regex scale (n ≤ 20; the O(n³) restart costs nothing
until n ≫ 20). The e-graph centerpiece had no cost-model foe to defeat and is itself the slowest
lane — it retires on both grounds.

**Dispatch net-positivity** (fold `is_acyclic` across all 56): dispatch cost Σ = 6786 ns; against
the fair `null` control the acyclic saving Σ(null−revuz) = 7208 ns > 6786 ns — **marginally**
net-positive. This is the weakest surviving conjunct even at its best.

**Verdict.** `RAN · oracle 0/56 divergences · cong passes≈n (O(n²k)) · incumbent≈null (<1%) · Revuz
single-pass 1.42× over bug-fixed Moore · CENTERPIECE RETIRES → 5C = table-tier + :358 fix; Revuz
kept as a no-egraph acyclic fast-path.`

**What survives / dies.** DIES: minimization-as-congruence as an e-graph centerpiece (slowest
correct lane; O(n²·k) on target class; no strawman gap to close). SURVIVES: a single
reverse-topological **Revuz** pass — genuinely the fastest correct lane on acyclic, the only lane
whose passes stay at **1** as n grows; a structural/table optimization needing no e-graph. MANDATORY
regardless: delete `dfa.rs:358` — a real ~O(n³) latent bug, correctness-neutral (0 divergences),
free to remove even though it pays nothing until n ≫ 20.

**The honest cyclic control for production** is Valmari–Lehtinen 2008 (partial-DFA native, no dead
padding) or classic Hopcroft with predecessor lists over an explicitly dead-completed DFA — NOT the
e-graph, and NOT the restart-refiner. On this corpus the Valmari lane's allocation constant factors
(`Vec<Vec<Vec<u32>>>` inverse image, HashMap per splitter) swamp its asymptotic edge at n ≤ 20 —
its timing is confounded and the FAIL verdict does not rest on it; the load-bearing signals are the
scale-free **passes** count and the `incumbent`/`null`/`revuz`/`cong` quartet that shares one
lightweight partition-vector constant factor.

---

## 4. Q6 — tier selection: table vs CSP; the coupling-witness search

**Inputs `(cardinality, literal-prefix, byte-rarity)` — two present, one missing.**

- *Cardinality* = start-state entry-set size, read O(256) from the start row (the `Prefilter::from_dfa`
  mechanism from PROVE-10; NOTE: prefilter has **not** landed in canonical `dfa.rs` — `grep Prefilter
  src/automata/dfa.rs` = 0 hits — it is a proven-GO seam, not merged). ≤3 → memchr1/2/3; larger → shufti/Teddy.
- *Literal-prefix* = `info/literal_prefix.rs:8` `extract_literal_prefix` (fixed-byte prefix/suffix).
- *Byte-rarity* = **the missing input.** No frequency/rank oracle exists in-tree (`grep rank|rare
  src/automata src/info` = 0 substantive hits). regex-automata ships a static ASCII-rank table
  (BurntSushi's `rank` heuristic); without a port, "rare byte" selection is guesswork — a named gap.

**The feasibility substrate is a monotone TABLE, not a CSP.** `info/mod.rs:61` `EngineSet` (u16
bitset: MEMCHR1/2/3, NIBBLE_LUT, ONE_PASS, SMALL_DFA, DFA, FAMILY_HELPER) is derived at
`info/mod.rs:344` `derive_feasible_engines` — its own doc comment (`info/mod.rs:342`) calls it a
"monotone derivation … from already-computed" fields: a pure per-pattern function, a few branch
comparisons, zero analysis cost. The bbnf-side recognizer CSP
(`skinny/crates/passes/src/decision_csp.rs:3` `csp_solver::domain::CostFiniteDomain`,
`decision_csp.rs:49` `csp.add_variable(CostFiniteDomain::new(values, costs))`) would consume this
`EngineSet` as the *domain* of a per-pattern engine variable.

**Coupling-witness search — NONE found (the Q6 negative result).** A grep for any global
codegen/LUT-budget coupling (`lut_budget`, `codegen_budget`, `shared_lut`, `table_budget`) over
`skinny/crates/ir/` + `skinny/crates/` returns **ZERO**. The only budgets that exist are orthogonal
to tier *legality*:
- `decision_csp.rs:90` `node_budget` — a bound on CSP **search nodes** (`decision_csp.rs:103`
  `budget-exceeded`), i.e. solver effort, not a shared physical LUT.
- `generated_loc` (`bbnf-bench/src/report.rs:3244`) — a **reporting** field measuring emission size,
  not a constraint that makes one pattern's tier deny another's.

Each pattern's tier is derived from its **own** start-state row and **own** literal prefix. There is
no shared resource over which two patterns' tier choices compete. **Therefore tier selection is
LOCAL, the monotone table wins, and a CSP over it is unearned overhead** (kiss-perf-bias,
extreme-parsimony) — a consumer-less substrate (constitution viii).

**The exact condition that would flip it to CSP (stated so it is testable).** A global coupling
appears only if the generator imposes a *bounded shared budget* the tiers consume — e.g. a
per-generated-parser cap on distinct **shufti nibble-LUTs** (32 bytes each) or **Teddy buckets**, so
choosing shufti for pattern A denies it to pattern B. That budget does not exist today. CSP earns its
keep **iff** such a shared-LUT cap is introduced; the crossover can only be exhibited by
*synthetically constructing* the cap. This is the one live experiment that could overturn the
table verdict — and it requires a codegen feature that GN-2's `generated_loc` budget (size, not tier
legality) does not supply.

---

## 5. Substrate — integration seams (path:line, canonical parse-that unless noted)

- **Minimize swap point:** `dfa.rs:89-90` (`if opts.minimize && dfa.states.len() > 1 { dfa =
  hopcroft_minimize(&dfa) }`) — the single call site for the correct control + Revuz fast path.
- **The bug to delete:** `dfa.rs:358` `break; // restart refinement from the beginning`.
- **Partial-DFA data:** `dfa.rs:330/:340/:423` DEAD-sentinel handling — where Valmari's
  partition-over-transitions (or an explicit virtual sink) replaces the `usize::MAX` block hack.
- **Predecessor lists (absent):** `dfa.rs:50` `flat_transitions` must be inverted per class for any
  real Hopcroft; the incumbent never builds this.
- **Byte-equiv classes (done):** `dfa.rs:80` `compute_byte_classes` (`sets/equiv.rs`) keeps
  `num_classes` ≤ ~8 → signature width tiny, radix-sortable.
- **Acyclicity dispatch:** a new `info` predicate beside `info/dfa_size.rs:10` `estimate_dfa_size`
  (back-edge DFS on the DFA, or lifted from HIR: unbounded `Repetition` reaching itself ⇒ cyclic).
- **E-graph reuse (for the retired lane / cost precedent):** `egraph/node.rs` `HirENode`,
  `egraph/translate.rs:19` `insert_hir`, `egraph/cost.rs` `CostModel` — the `DfaSig` node would have
  derived congruence for free, but the lane retires.
- **Tier table:** `info/mod.rs:344` `derive_feasible_engines` (extend for byte-rarity + shufti/Teddy
  bits). CSP consumer (if ever earned): `skinny/crates/passes/src/decision_csp.rs`
  (`CostFiniteDomain`, 278 LOC).
- **Prefilter tier (extraction target, NOT yet merged):** PROVE-10's `automata/prefilter.rs` +
  `Dfa::find` — 9.6×–29× match-throughput, 0.99× on non-prefilterable controls, compile-neutral.

---

## 6. Prior art (citations)

- **Revuz 1992**, *Minimization of acyclic DFAs in linear time*, TCS 92(1) — the surviving acyclic
  single-pass congruence; O(n) for the acyclic half, measured fastest correct lane here.
- **Hopcroft 1971**, *An n log n algorithm for minimizing states* — the correct control (predecessor
  lists + smaller-half). **Knuutila 2001**, *Re-describing an algorithm by Hopcroft*, TCS 250 — the
  implementation subtleties the incumbent skipped.
- **Valmari–Lehtinen 2008**, *Bisimilarity minimization in O(m log n) time* — SOTA control handling
  **partial** automata natively (matches this engine's DEAD transitions); the honest cyclic control.
- **Moore 1956** partition refinement — what the incumbent actually is, minus the restart bug.
- **Bonchi–Pous 2013**, *Checking NFA equivalence with bisimulations up to congruence*, POPL — the
  theoretical backing for minimization-as-congruence (which nonetheless loses at regex scale here).
- **Brzozowski 1962** (double-reversal + determinization ⇒ minimal DFA) — a "minimize as extraction"
  control worth a future timing lane; exponential worst case, often competitive.
- **egg / egglog** (Willsey et al., POPL 2021) — congruence closure = equality saturation; the
  `egraph` substrate. **Cranelift ægraphs** — the acyclic-e-graph one-pass shape (which Revuz
  realizes without the e-graph machinery).
- **regex-automata** (BurntSushi) — `ByteClasses` (mirrors `sets/equiv.rs`), DFA minimization, the
  memchr/rare-byte prefilter ladder + the static ASCII-rank table (the byte-rarity oracle to port).

---

## 7. Named risks + counterexamples

- **Cyclic congruence is a loss (measured).** Iterated hashcons is O(n²·k); the chain automaton
  `[01]{8}` needs 8 passes at n=9. The e-graph must NOT be sold as any minimizer; it is the slowest
  correct lane on acyclic and asymptotically worse on cyclic. A negative result, not a footnote.
- **Acyclicity-detection cost is near the win.** The back-edge DFS is O(n·k); against the fair `null`
  control the net saving (7208 ns) barely clears the dispatch cost (6786 ns). *Counterexample:* an
  acyclic-but-large DFA where detect+Revuz ≈ direct-minimize. The dispatch survives only marginally.
- **Partial-DFA under-minimization.** Classic Hopcroft on the DEAD-sentinel DFA under-merges without
  dead-completion. *Counterexample the corpus lacks:* two states differing only in *which* class goes
  DEAD, or a genuine non-accepting all-DEAD trap the virtual-sink lane would drop but sentinel-Moore
  keeps. 0 divergences confirm the corpus has none; a trap-bearing corpus must re-run the oracle
  before trusting Valmari.
- **Byte-rarity guesswork.** Without a rank oracle the tier may pick a common entry byte (`e`,`h`) as
  accel — legal but useless (PROVE-10: `h` in prose wins 15.8× vs `0`/`#` absent wins 29×). The
  table's rarity dimension needs the ASCII-rank port before it is sound.
- **CSP without a coupling is theater.** Absent a shared-LUT budget, a CSP over per-pattern tiers is
  consumer-less (Y.13). *The counterexample the prototype could not build:* two patterns whose tier
  choices contend for a shared codegen resource — it does not exist; that absence IS the finding.
- **Small-n regime.** Regex DFAs here are n = 2–20; asymptotic separations sit near the noise floor
  on a loaded host. The scale-free **passes** signal and the direction (growing with n) are the
  trustworthy evidence; no absolute throughput claim is made.

---

## 8. What remains to move this family (residual obligations)

The centerpiece bake-off has RUN; these are the residual, prioritized:

1. **Deconfound Valmari at scale.** Re-time L2 with flat arena allocation (no per-splitter HashMap /
   `Vec<Vec<Vec<u32>>>`) and grow the corpus toward n ≫ 100 (synthetic chains, generated-grammar
   leaves) to confirm the O(n·k·log n) edge emerges where the O(n³) restart finally bites — the one
   regime where deleting `:358` visibly pays. Until then, ship the correct control on principle
   (correctness-neutral) with the fast path, not on a measured large-n win.
2. **Land the Revuz fast path** as a `derive_feasible_engines`-adjacent structural dispatch: `is_acyclic`
   ⇒ single reverse-topological congruence pass; else the correct Hopcroft/Valmari control. No e-graph.
3. **Port the ASCII byte-rank oracle** into `info/` so the tier table's rarity dimension is sound;
   re-verify the PROVE-10 skip-heavy corpus improves accel-byte choice (the `h`-vs-`0` spread).
4. **Q6 crossover, only if GN-2 ships a shared-LUT cap.** Synthetically introduce a per-parser shufti
   nibble-LUT byte budget and show `decision_csp.rs`'s CSP beats the monotone table **only** under
   that cap. Absent the cap, the table is law and the CSP stays retired — name it.
