# LOCK-5 · pass-3 · research-C — family 5C: AUTOMATA-AS-EXTRACTION

Family charter: recast determinization, minimization (Hopcroft as rewrite-to-normal-form),
and prefilter-tier selection as e-graph saturation + CSP extraction — measured head-to-head
against the classical pipeline; wins stay, losses are named negative results. This report
develops 5C to its strongest form and reports where the mechanism holds, where it has already
been measured to break, and the two prototypes that decide the family.

Settled ground it rides on (XL-C): two domain-fit HIRs joined by a cheap `simplify_hir`
bridge; the vendored engine is the measurement substrate; every number base-pinned to
parse-that HEAD. 5C touches nothing in the type plane — it lives entirely below the HIR, on
the automaton and on the tier-selection decision.

---

## 1. The extant substrate (path:line — what 5C would rewrite)

The engine already contains an e-graph, a minimizer, an acceleration tier, and a CSP. 5C's
claim is that the *first three* should collapse into the *fourth*. The concrete seams:

- **Determinization** — `parse-that/rust/regex/src/automata/dfa.rs:218 subset_construction`
  (classical powerset over byte-equivalence classes; `epsilon_closure:189`, `move_on_byte:205`).
  Byte classes computed at `dfa.rs:80` via `sets::equiv::compute_byte_classes`.
- **Minimization** — `dfa.rs:89` dispatch gate (`opts.minimize && states.len() > 1`),
  `dfa.rs:291 hopcroft_minimize`. This is **misnamed**: it is Moore partition-refinement with
  a **full restart on every split** (`dfa.rs:358 break; // restart refinement from the
  beginning`), asymptotically ~O(n³·k), not Hopcroft's O(n·k·log n).
- **The HIR e-graph** — `parse-that/rust/regex/src/egraph/mod.rs`. `HirEGraph =
  EGraph<HirENode, NoAnalysis>` (`mod.rs:56`); `simplify_hir` (`mod.rs:143`) build→saturate→
  extract; five retained rules (`default_hir_rules`); extraction cost `egraph/cost.rs:79
  RegexExtractionCost` (per-node structural + literal-per-byte + class/repeat/merged knobs).
  **Critical fact: this e-graph operates on HIR, never on DFA states.** There is no automaton
  e-graph in the engine. 5C's "minimization-as-extraction" would require a *new* DFA-state
  congruence engine — it cannot reuse `HirEGraph`.
- **Acceleration tier** — `automata/accel.rs:15 AccelStrategy` (`Memchr1/2/3` → `NibbleLut{
  lo_lut:[u8;16], hi_lut:[u8;16], exit_bytes}` for 4..8 exits → `None`), detected per
  self-loop state (`detect_accel:41`, 80% self-loop heuristic at `:65`).
- **Prefilter tier** (landed by PROVE-10) — `automata/prefilter.rs` `Prefilter{None,Byte1,
  Byte2,Byte3}` derived once from the start-state transition row; `Dfa::find` unanchored
  leftmost with a single prefilter branch. Certified 8.8–29.5× on prefilterable patterns,
  0.99× on controls, compile-neutral.
- **Tier-selection inputs** — `info/classify.rs:15 detect_accel_candidate` (literal-prefix
  first byte, else suffix first byte), `info/dfa_size.rs:10 estimate_dfa_size` (cardinality
  proxy = 3× NFA-state estimate), `info/mod.rs:61 EngineSet` (monotone feasibility bitset:
  MEMCHR1/2/3, NIBBLE_LUT, ONE_PASS, SMALL_DFA, DFA, FAMILY_HELPER).
- **The CSP** — `skinny/crates/passes/src/decision_csp.rs`. `Csp<CostFiniteDomain>`, one
  variable per rule, five constraints, one min-cost objective, `NODE_BUDGET = 10_000`
  (`:14`), `csp_candidate_scope = "rule-local-backend-candidates"` (`:145`). This is the
  extant realization of "CSP extraction" — and it is **rule-local**, a fact that decides Q6.

---

## 2. Q5 — minimization-as-congruence vs a reference Hopcroft

**The mechanism, stated at full strength.** DFA minimization *is* the coarsest congruence:
two states are equivalent iff they agree on acceptance and their per-class successors are
equivalent. Cast as extraction: hash-cons each state on a signature `DfaSig{is_accept,
targets:[classId → repId]}`, rebuild to fixed-point (states whose signatures collide merge,
which changes their predecessors' signatures, iterate). At fixpoint the distinct signatures
are the minimal states — the exact e-graph "rebuild-to-canonical" move applied to an
automaton instead of a term.

**This mechanism has been built and measured, and it FAILS.** The `DfaSig` congruence
rebuild does not achieve O(n·k·log n): passes-to-fixpoint track the DFA **diameter**, not
log n, because a signature change at depth d only propagates one hop per pass. On exactly the
acyclic chain-DFAs the fast path exists to serve — fixed-length dates, hex, IP, bitstrings —
the diameter *is* the state count, so the rebuild degenerates to **O(n²·k)**. Measured on a
runtime-tagged 56-pattern corpus (12 acyclic / 44 cyclic, `is_acyclic` by back-edge DFS):

| pattern | n | congruence passes | Revuz passes | ceil(log₂ n) |
|---|---:|---:|---:|---:|
| `[01]{8}` | 9 | **8** | 1 | 4 |
| `\d{4}-\d{2}-\d{2}` | 11 | 6 | 1 | 4 |
| `\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}` | 16 | 6 | 1 | 4 |
| `(?:Mon\|…\|Sun)` (bushy) | 20 | 2 | 1 | 5 |

`log n` held only for shallow bushy alternations; it is **refuted for every bounded chain**,
which is the acyclic class the centerpiece was meant to accelerate. The congruence lane was
additionally the **slowest correct lane on acyclic** (37.7 µs vs Revuz 17.2 µs vs bug-fixed
Moore 24.4 µs, Σ over 12 pats). A correctness oracle (BFS-canonical form, renumber-invariant)
confirmed **0 divergences across all 56 patterns × 5 lanes** — the incumbent's *result* is
correct; only the congruence *cost model* was ever the question, and it lost.

**The two survivors, and the two things that die.**

- **DIES:** minimization-as-e-graph-congruence. Wrong asymptotics on the target class, slowest
  correct lane, and — the deeper point — **there was never a cost-model foe to defeat**. The
  incumbent's `:358` full-restart bug, run against a null lane that simply deletes the restart,
  moves the total by **<1%** at n ≤ 20 (`incumbent ≈ null`). Every prior "acyclic-beats-Hopcroft"
  claim was measured against this strawman; at regex scale the strawman and its fix are
  statistically indistinguishable. The e-graph had nothing to beat.
- **SURVIVES (structural, no e-graph):** a single **reverse-topological Revuz pass** on acyclic
  DFAs — the only lane whose passes stay at **1** as n grows, 1.42× over bug-fixed Moore and
  growing with n. This is a *table/structural* optimization (2C-as-table territory), folded at
  the `dfa.rs:89` minimize dispatch behind an `is_acyclic` tag; it needs no congruence engine.
- **MANDATORY regardless:** delete `dfa.rs:358`. A real latent ~O(n³) restart, correctness-
  neutral (0 divergences), free to remove; it simply does not pay until n ≫ 20.

**The residual gap the pass-3 directive names, and the clean prototype it demands.** The
measured Hopcroft control was **allocation-confounded**: it built inverse-image as
`Vec<Vec<Vec<u32>>>` plus a `HashMap` per splitter plus a `vec![false;m]` per worklist pop,
so at n ≤ 20 it ran 3.5–5× slower than everything — its ">1.3× loss to Revuz" is an
allocation tax, not an algorithmic verdict. The FAIL verdict does **not** rest on that lane
(it rests on the allocation-independent passes count and on `incumbent ≈ null`), but the
directive's demand — "a REFERENCE O(n·k·log n) Hopcroft control, measured against congruence"
— is not yet cleanly met. The **prototype to run**: a *flat* Hopcroft/Valmari control with (i)
inverse transitions as a single CSR array (`starts:[k·(n+1)]`, `elems:[edges]`) built once,
(ii) partition as three parallel `Vec<u32>` (block-of-state, elems, first/last) — Valmari's
refinable-partition layout, **zero per-split heap allocation** — and (iii) partial-DFA DEAD
edges handled by Valmari's absent-transition rule (no virtual sink, removing the trap-merge
caveat). Predicted outcome, stated as a falsifiable claim: even a zero-allocation Hopcroft
will **not** beat the single Revuz pass on the acyclic slice at n ≤ 20 (the passes signal
already shows congruence needs n passes and Revuz needs 1), and will land within noise of
bug-fixed Moore on the cyclic slice — confirming 5C's minimization centerpiece is a **named
negative result** and the family reduces to {Revuz acyclic fast-path ∥ delete `:358` ∥ tier
selection}. If the clean Hopcroft *does* separate on the cyclic slice at the corpus's upper n,
that is a win for a classical worklist Hopcroft — still **not** for e-graph congruence.

---

## 3. Q6 — tier selection: is it a table or a CSP?

Tier selection maps `(cardinality, literal-prefix, byte-rarity)` to a
`memchr1/2/3 | shufti | Teddy | none` accelerator and a `Revuz | Moore | none` minimizer. The
directive asks: earn table-vs-CSP by exhibiting a **concrete global codegen/LUT-budget
coupling witness, or show none exists (then it is local)**.

**The witness search comes up empty in the extant solver.** `decision_csp.rs` solves
**one rule at a time** — one variable, rule-local candidate scope (`:145`), a per-solve
`NODE_BUDGET = 10_000` (`:14`) that bounds *search effort on that rule*, not a shared
resource across patterns. Its five constraints (`recognizer`, `substrate`, `simd`, `capacity`,
`selected`) are all **per-candidate legality predicates** — e.g. `capacity` is
`candidate.capacity_cost <= 1` (`:79`), a local bound, not a running sum. There is **no shared
variable** whose domain shrinks as other patterns consume it. Therefore, as the code stands,
tier selection is **provably local**: each pattern's accelerator is chosen independently, and
a **priority table keyed on `(EngineSet, cardinality bucket, literal-prefix length,
start-byte rarity)` is the correct and sufficient mechanism.** A CSP over local choices is
unearned overhead — it explores a product space that factors.

**Where a global coupling *could* arise, and why it is not modeled today.** The one candidate
for genuine coupling is the static-table footprint: each `NibbleLut` accelerator is
`2×[u8;16] = 32 B` (`accel.rs:23`); a shufti tier adds a nibble-LUT per wide-entry pattern; a
Teddy tier adds bucket masks; each DFA contributes `num_states × num_classes × 4 B` of
transition table. Sum these over all regex leaves in a grammar and you approach the
**per-tranche generated-size budget** (a standing law: generated code has a line/table budget;
overflow blocks the wave). *That* is a global knapsack: under a hard table ceiling, choosing
Teddy for pattern A can force pattern B down to memchr. **But it is not in the solver.** The
generated-size budget is a **post-hoc build gate**, not a shared constraint variable; every
pattern is sized independently and the aggregate is checked afterward. So the honest Q6
answer: **no global coupling is modeled; tier selection is local today; a table wins.** The
CSP earns its keep *only if* the generated-size budget is lifted into the solver as a shared
0/1-knapsack constraint (patterns as items, LUT/table bytes as weights, the per-tranche
ceiling as capacity, match-throughput gain as value) — at which point tier choices genuinely
couple and a table can no longer be optimal. That lifting is the single concrete thing that
would move Q6 from "table" to "CSP", and it does not exist in the tree. This is 5C's cleanest
*positive* opening: not minimization-as-extraction (dead), but **tier-selection-as-knapsack**,
earned by a measured budget-binding corpus.

**The prototype to run for Q6:** a grammar with N regex leaves whose independent
table-optimal choices *exceed* the per-tranche table budget; solve twice — (a) greedy
per-pattern table, then trim to fit (the current post-hoc gate), (b) a shared-budget knapsack
CSP over `(pattern → tier)` — and measure aggregate match-throughput of the emitted engine.
If (b) beats (a) by more than the CSP's build-time cost on a budget-binding corpus, the CSP is
earned; if no corpus binds the budget, tier selection is confirmed local and the table is law.

---

## 4. Prior art (cited)

- **Hopcroft 1971**, "An n log n algorithm for minimizing states in a finite automaton" — the
  O(n·k·log n) reference the directive demands as the control; the "process the smaller half"
  invariant is what the incumbent's full-restart Moore throws away.
  https://dl.acm.org/doi/10.5555/891883
- **Valmari & Lehtinen 2008 / Valmari 2012**, "Fast brief practical DFA minimization" — the
  refinable-partition, allocation-frugal Hopcroft that handles **partial** DFAs without a
  virtual dead state; the correct blueprint for the clean Q5 control that removes both the
  allocation confound and the trap-merge caveat.
  https://link.springer.com/chapter/10.1007/978-3-540-78163-9_9
- **Revuz 1992**, "Minimization of acyclic deterministic automata in linear time" — the single
  reverse-topological pass; the survivor of 5C's minimization audit. O(n) on ADFAs; the DAWG
  construction lineage.
  https://doi.org/10.1016/0304-3975(92)90142-3
- **Paige & Tarjan 1987**, relational coarsest partition — the theory under which "DFA
  minimization = coarsest congruence" is exact, and under which naive hash-cons rebuild is
  *provably* worse than a three-way worklist split (the reason congruence-by-rebuild degrades).
- **regex-automata / burntsushi "regex internals"** — the load-bearing SOTA counterpoint: the
  meta engine's dense-DFA path minimizes *optionally and rarely on the hot path*, and the lazy
  (hybrid) DFA **never minimizes** — states are built during search under a memory budget. This
  argues the entire "minimization-as-extraction" premise may be a **non-goal** for a match
  engine: production regex does not spend compile time minimizing. https://burntsushi.net/regex-internals/
- **Hyperscan / Teddy / shufti / truffle** (NSDI'19; Langdale) — the multi-literal and
  nibble-LUT tiers that make tier *selection* a real decision; the exact kernels the wide-entry
  tier would emit and the source of the LUT-byte weights in the Q6 knapsack.
  https://www.usenix.org/system/files/nsdi19-wang-xiang.pdf
- **egg / egglog** — the e-graph rebuild whose "rebuild is amortized cheap on terms" property
  does **not** transfer to automaton congruence: term children are fixed-arity and shallow;
  DFA successor-signatures form deep chains, so rebuild passes track diameter. Naming this
  non-transfer *is* the negative result.

---

## 5. Named risks and counterexamples

1. **Consumer-less substrate (Y.13).** A DFA-state e-graph has **no consumer** — the extant
   `HirEGraph` is HIR-level, extraction feeds `RegexInfo`, and nothing downstream wants a
   saturated automaton. Building a `DfaSig` e-graph to house minimization would erect a
   subsystem with one caller that a single Revuz function replaces. Forbidden by construction.
2. **The strawman trap.** Any future 5C claim that measures a new minimizer against
   `dfa.rs:291`-with-`:358` is measuring against a bug. `incumbent ≈ null` (<1% at n ≤ 20) means
   the only honest baselines are bug-fixed Moore and a zero-allocation Hopcroft. Delete `:358`
   *before* any comparison.
3. **Small-n regime.** Regex DFAs here are n = 2–20; asymptotic separations sit near the noise
   floor. Verdicts must lean on the allocation-independent **passes** count and on paired
   same-binary ratios, never on µs absolutes. A large-n synthetic (deep bounded repetitions)
   is the only way to make Hopcroft's log-factor visible — and it will still lose to Revuz on
   the acyclic slice.
4. **Partial-DFA trap states.** The corpus contains none (0 divergences), so virtual-sink
   Hopcroft and sentinel-Moore agree. A corpus with genuine all-DEAD non-accepting traps would
   split them; the clean Valmari control (absent-transition rule) avoids the virtual sink and
   removes this caveat — another reason the Q5 prototype must be Valmari-shaped, not
   virtual-sink-shaped.
5. **Q6 budget mirage.** If no realistic grammar's table footprint binds the generated-size
   budget, the knapsack CSP is a solution without a problem — the table is law and the CSP is
   consumer-less. The Q6 prototype must **first exhibit a budget-binding corpus**; absent one,
   report "local, table wins" as the terminal finding.

---

## 6. What a prototype must test to move this family

The family is already at a measured negative on its centerpiece; two prototypes decide what
remains:

- **P1 (Q5, clean control).** A zero-allocation Valmari Hopcroft (CSR inverse transitions,
  three parallel partition vectors, absent-transition partial-DFA rule) added as a sixth lane
  to the existing `regex_min_race` harness, over the 56-pattern tagged corpus plus a large-n
  synthetic slice. Gate: does clean Hopcroft beat Revuz on **any** slice? Predicted: no on
  acyclic (Revuz single-pass), within-noise on cyclic small-n, a visible log-factor only on the
  large-n synthetic — confirming minimization-as-congruence dead, Revuz retained as a
  no-e-graph structural fast-path, `:358` deleted. Seam: `dfa.rs:89` minimize dispatch gains an
  `is_acyclic` branch; nothing else changes.
- **P2 (Q6, earn the CSP).** A budget-binding grammar corpus + the two-lane solve (greedy-table
  then trim vs shared-budget knapsack CSP), measuring emitted-engine aggregate match
  throughput. Gate: does the knapsack beat greedy-table on a corpus that binds the per-tranche
  table budget, net of solve cost? If yes, tier-selection-as-knapsack is 5C's one earned CSP;
  if no corpus binds, tier selection is local and a priority table on `(EngineSet, cardinality,
  literal-prefix, byte-rarity)` is the law. Seam: `decision_csp.rs` gains one shared knapsack
  constraint over a per-pattern `tier` variable — the *only* concrete edit that turns Q6's
  answer from "table" to "CSP", and it must be earned by P2's corpus, not asserted.

Bottom line for 5C: **automata-as-extraction is refuted for minimization** (congruence rebuild
is O(n²·k) on the target class, slowest correct lane, no cost-model foe) and **survives only as
tier-selection-as-knapsack**, itself gated on a budget-binding corpus that does not yet exist
in the tree. The classical pipeline wins the automaton; the CSP's remaining claim is the global
table budget, and that claim is currently unmodeled — a post-hoc gate, not a solver constraint.
