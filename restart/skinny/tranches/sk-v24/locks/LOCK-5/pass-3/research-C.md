# LOCK-5 pass-3 · Family 5C — AUTOMATA-AS-EXTRACTION

Determinization, minimization, and prefilter-tier selection recast as e-graph
saturation + CSP extraction, measured head-to-head against the classical pipeline —
wins kept, losses named. Every path/line is `parse-that/rust/regex/src` (the canonical
one-regex-home, R3 §3) unless prefixed. The vendored `skinny/crates/parse-that-regex`
is the number/unicode slice only (`lib.rs`, `number/`, `unicode/`, `integration/`, 3022
LOC) — it carries **no automata**; the automata measurement substrate is the canonical
engine (6742 LOC). Base-pin every number to that engine at HEAD.

---

## 0. The substrate as it stands (measured facts, not estimates)

**Minimization.** `dfa.rs:291 fn hopcroft_minimize` is **mislabeled**. It is not
Hopcroft's O(n·k·log n) worklist algorithm. It is Moore-style partition refinement that
**restarts from scratch on every split** — `dfa.rs:358 break; // restart refinement from
the beginning`, re-collecting each block's members by a full O(n) scan every pass
(`dfa.rs:320 (0..n).filter(...)`). Each of up to n splits re-drives the whole
block×class product: worst case **O(n²·k)** (a right-linear chain DFA degenerates fully).
The "restart" Q5 names is literally `dfa.rs:358`. Dispatch is `dfa.rs:89` (`from_nfa`
Step 4, gated on `opts.minimize && states>1`). Consequence for Q5: measuring any
candidate against *this* incumbent is a rigged win — an honest control must be a real
Hopcroft.

**Determinization.** `dfa.rs:218 subset_construction` (classical powerset over byte
classes, `sets/equiv.rs compute_byte_classes` → `[u8;256]` `dfa.rs:52`). This is not an
e-graph fit — subset construction has no confluent rewrite normal form; leave it
classical. (5C touches minimization and tier-selection, not determinization.)

**Acyclicity signal is already free.** `RegexInfo.max_match_len: Option<usize>`
(`info/mod.rs:168`, from `info/width.rs compute_match_width`) is `Some` iff the pattern
has no unbounded `*`/`+` reaching accept — i.e. **`max_match_len.is_some()` ⟺ the DFA is
acyclic**. `is_acyclic` dispatch costs zero new analysis; it is a projection of an
extant field. The engine corpus is acyclic-heavy: `#[0-9a-fA-F]{3,6}`, bounded numeric,
fixed keywords are all acyclic; only `[a-z]+`/`\w+`/`https?://…`-tail are cyclic.

**Tier selection is already a local table, twice over.** `accel.rs:90` is a pure
per-state cardinality `match exit_count { 1→Memchr1, 2→Memchr2, 3→Memchr3,
4..=8→NibbleLut, _→None }`; `info/mod.rs:344 derive_feasible_engines` is a monotone bit
derivation from `(negated-class cardinality, accel_candidate, literal_prefix, one_pass,
dfa_size≤64, hir_walkable)` producing `EngineSet` (`info/mod.rs:61`, u16 newtype). Both
are **local** — each state/pattern decides independently, no cross-site term.

**The extraction engine that actually exists** is HIR-only: `egraph/mod.rs`
(`build_hir_egraph`/`saturate_hir_egraph`/`extract_canonical`/`simplify_hir`),
`egraph/node.rs HirENode` (variant-parallel to `Hir`, `egraph_derive::Language`),
`egraph/cost.rs RegexExtractionCost` (shared `egraph::CostWeights`), driven by
`egraph::CspScheduler`. There is **no DFA-state e-graph** and **no tier-selection CSP** in
tree; the CSP consumer `crates/ir/src/passes/csp_recognizers.rs` referenced at
`info/mod.rs:55` does **not exist** — it is the greenfield GN-2 consumer, planned not
built. `csp-solver` lives at `csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/src/lib.rs`.

---

## 1. Q5 — Hopcroft control + minimization-as-congruence

**The honest control (must be built).** A real Hopcroft: an inverse-transition
(predecessor) index per class, a worklist W of `(splitter-block, class)` pairs, each
splitter processed once, only *touched* blocks refined, the **smaller half** pushed to W
(the log n comes from a state never sitting in more than log n smaller-halves).
O(n·k·log n). Reference-grade alternatives for the control cohort: **Valmari 2012**
(refines states *and* transitions, handles partial DFAs natively — our `DEAD` sentinel
`dfa.rs:330` makes the DFA partial, so Valmari is the cleaner control), and **Revuz 1992**
for the acyclic lane (below).

**Minimization-as-congruence — the decisive analysis.** DFA-state equivalence is a
congruence (Myhill–Nerode): `p ≡ q` iff `is_accept(p)=is_accept(q)` and
`δ(p,c) ≡ δ(q,c)` for every class c. Congruence closure is *exactly* what an e-graph
`rebuild` computes. To do minimization *in* the e-graph you would add a second `Language`
— `StateNode { accept: bool, row: Box<[Id]> }` where `row[c]` is the target state's Id —
insert one node per DFA state, and let hash-cons + rebuild merge structurally-identical
rows to fixpoint. That fixpoint **is** the minimal DFA. **But**: a naïve
rebuild-to-fixpoint merges only rows whose children are *already* merged, so it needs one
pass per equivalence-depth level — the same **O(n²)** shape as the incumbent restart on a
chain DFA. The e-graph reproduces the pathology unless it embeds Hopcroft's smaller-half
worklist discipline — at which point the e-graph adds nothing over calling Hopcroft
directly, and violates the consumer-less-substrate law (Constitution viii / Y.13) with a
second Language nobody else consumes. **Predicted negative result: state-minimization does
not belong in the e-graph.** The e-graph stays HIR-only (canonicalization,
`simplify_hir`); minimization stays a classical function. This is the family's most
important finding — it *bounds* how far "automata-as-extraction" can honestly go: the
extraction/e-graph plane governs *HIR-shape* canonicalization; the automata plane stays
classical (Hopcroft/Revuz), and the two meet only at `simplify_hir` before NFA build.

**The acyclic fast path (Revuz) — does it beat correct Hopcroft?** Revuz 1992 minimizes
an acyclic DFA in **O(n·k)** linear time: bucket states by height (reverse-topological
distance to accept), then within each height process states in ascending order of a
signature `(accept, sorted (class→already-numbered-target) pairs)`, assigning equal
numbers to equal signatures — no partition refinement, no worklist, no log n. `is_acyclic`
= `max_match_len.is_some()` (free). Head-to-head prediction: on the engine corpus (DFAs
with n<64 — the `SMALL_DFA` threshold, `info/mod.rs:385`) Revuz beats a *correct* Hopcroft
by a **small constant** (single bucketed pass vs worklist bookkeeping), and both
**annihilate** the restart incumbent. But once the restart is deleted (`dfa.rs:358` → real
Hopcroft), the `log n` Revuz saves is `log 64 ≈ 6` on tiny automata — **sub-microsecond**.
So on the standard corpus **deleting the restart largely erases the acyclic path's reason
to exist**. Where it earns its keep is compile-time at scale: the W3 large-pattern corpus
(bounded-repetition blow-ups, large Unicode classes) where n is large and acyclic — there
O(n·k) vs O(n·k·log n) is a real multiplier and Revuz is the compile-time win the W3 gate
(≥5× lazy-vs-eager) reaches for on the *bounded* subset.

**Q5 evidence a prototype must produce.** Corpus manifest ≥50 patterns tagged
acyclic/cyclic (seed from the `regex_dfa_compile` corpus + bounded-repetition blow-ups);
three paired cold timings per pattern `{incumbent-restart, real-Hopcroft/Valmari,
Revuz-on-acyclic}`; a **byte-identity oracle** asserting all three emit the same minimized
transition table (`flat_transitions` equal after canonical state renumber, `dfa.rs:281`);
the acyclic fraction; and the congruence-rebuild pass-count measured against the
`n·k·log n` claim (§below).

---

## 2. Q6 — tier selection: earn table-vs-CSP; the global-coupling witness

**Restating the extant fact:** the memchr/nibble ladder is *already* a local table
(`accel.rs:90`, `info/mod.rs:344`). For a CSP to earn its place over a table it must model
a **global coupling constraint** — a term linking two sites' choices. Absent that, the CSP
degenerates to N independent argmins, which *is* a table (with worse constant factors and
a solver dependency — a kiss-perf-bias violation, MEMORY kiss-perf-bias).

**Hunt for the coupling witness — three candidates, adjudicated:**

- **(a) memchr / nibble cardinality ladder — NO coupling.** `exit_count → tier` is a pure
  per-state function of local exit-set cardinality (`accel.rs:80`). Two patterns never
  contend. **Local. Table wins. CSP unearned here.**

- **(b) generated-LUT / codegen-size budget — REAL but NARROW coupling.** Every
  `NibbleLut` selection emits two 16-byte tables per accelerated state
  (`accel.rs:120 build_nibble_luts`). Under the per-tranche generated-size budget (MEMORY
  generated-size-budget), NibbleLut proliferation across many patterns is a *shared*
  resource: selecting NibbleLut for site A can push the total past budget and force site B
  to a cheaper tier. That is a genuine global constraint — `Σ lut_cost(site) ≤ budget` — a
  covering/knapsack the CSP models and greedy-local cannot. **This is the concrete witness
  Q6 asks for.** It is narrow: it fires only when NibbleLut/Teddy sites are numerous
  enough to strain the budget.

- **(c) Teddy multi-literal co-bucketing — REAL coupling.** Teddy (R7 §3, ICPP21) shares
  one bucketed multi-literal automaton across *several* literal patterns; the choice of
  which literals co-bucket is inherently cross-pattern (a set-partition over
  `info/literal_prefix.rs extract_literal_prefix` outputs). This does not exist in tree
  (Teddy is W2-planned); when it lands it is a legitimate global decision.

**Q6 verdict.** The tier ladder is **local — keep it a table** (`accel.rs`,
`derive_feasible_engines`); model the cost as a *pluggable* input (MEMORY
pluggable-components) but do not wrap the ladder in a CSP. The CSP earns its seat **only**
at the two genuine global seams: (b) the generated-LUT-budget covering constraint and (c)
Teddy co-bucketing — both narrow, both budget/set-partition shaped. `EngineSet`
(`info/mod.rs:61`) is the right primitive: it is the per-pattern *domain* the CSP ranges
over; the CSP's *constraints* are exactly (b)+(c), and where neither binds the solve
collapses to the table it should have been.

---

## 3. Congruence-rebuild complexity vs the O(n·k·log n) claim (the number that decides §1)

The e-graph `rebuild` (egg-style congruence closure, Willsey et al POPL 2021) is
amortized near-linear in *edges processed per pass*, but the number of *passes* to reach
the minimal-DFA fixpoint equals the maximum Myhill–Nerode distinguishing depth — up to
O(n) on a chain. Naïve `saturate_hir_egraph`-shaped iteration (`egraph/mod.rs:87`, run to
fixed-point via `CspScheduler`) therefore costs **O(n) passes × O(n·k) work = O(n²·k)** on
the adversarial chain — identical to the incumbent restart. Hopcroft/Valmari beat this
precisely because the smaller-half worklist bounds total work to O(n·k·log n). The
prototype must instrument pass-count on a chain DFA of length n and plot it against
`n·log n`: if passes ≈ n (not log n), minimization-as-congruence is refuted as an
efficiency lever and banked as a named negative result — the e-graph is a *shape*
canonicalizer, not a state minimizer.

---

## 4. Prior art (citations)

- **Hopcroft 1971**, *An n log n algorithm for minimizing states in a finite automaton* —
  the worklist + inverse-transition + smaller-half control. Gries 1973 correctness proof.
- **Revuz 1992**, *Minimisation of acyclic deterministic automata in linear time*, TCS 92
  — height-bucketed O(n·k) acyclic minimization; the acyclic-lane control.
- **Valmari 2012**, *Fast brief practical DFA minimization*, Inf. Proc. Lett. — refines
  states and transitions, native partial-DFA support (matches our `DEAD` sentinel); the
  cleanest full control.
- **Brzozowski** double-reversal (reverse∘determinize twice) — the pure-algebra
  "minimization = canonical quotient" anchor; exponential worst case, not competitive, but
  the theoretical statement of minimization-as-normal-form.
- **egg** (Willsey, Nandi, et al, POPL 2021) — e-graph rebuild = congruence closure; the
  mechanism the 5C hypothesis leans on, and the reason §3's pass-count is the crux.
- **Cranelift ægraphs / acyclic e-graphs** — bounding saturation cost by acyclicity; the
  precedent for "extraction under a compile-time budget."
- **regex-automata** (BurntSushi) — lazy/hybrid DFA, ranked memchr→Teddy prefilter ladder,
  byte-class equivalence: the classical pipeline 5C measures against (R7 §4).
- **Hyperscan shufti/truffle/Teddy** (Wang et al; Teddy ICPP21, R7 §3) — the bucketed
  multi-literal tier; the *only* genuinely cross-pattern tier decision → the Q6(c) witness.

---

## 5. Named risks & counterexamples

- **R1 (the family's load-bearing negative).** Minimization-as-congruence over a generic
  e-graph reproduces the incumbent's O(n²) restart pathology unless it embeds Hopcroft's
  smaller-half worklist — at which point the e-graph is redundant. Counterexample: a
  right-linear chain DFA of n states, naïve rebuild does ~n passes. Disposition: keep
  minimization classical; the e-graph stays HIR-only. *This must be proven, not asserted.*
- **R2.** The acyclic Revuz path beats a *correct* Hopcroft by only a small constant on
  n<64; deleting the restart erases most of its reason. Counterexample: the ~10-state
  hex-color DFA — Revuz and Hopcroft both sub-µs, delta in the noise. It earns its keep
  only on the W3 large bounded-repetition corpus.
- **R3.** The tier CSP degenerates to N independent argmins (a table) absent (b)/(c).
  Counterexample: an isolated hex pattern and an isolated URL pattern — the "CSP solution"
  is each pattern's local best; no constraint links them. CSP unearned for the ladder.
- **R4.** Over-modeling the whole ladder as CSP violates kiss-perf-bias; the genuine
  coupling (LUT budget, Teddy co-bucketing) is narrow — model *only* it.
- **R5 (Constitution viii / Y.13).** A DFA-state e-graph with no downstream consumer is a
  forbidden consumer-less substrate; the §1 experiment must yield a *measured* verdict
  (win kept / loss named), never banked machinery.
- **R6.** Basing the Q5 comparison on the mislabeled incumbent (`dfa.rs:291`) is a rigged
  win — any correct algorithm beats a restart. The control must be real Hopcroft/Valmari.

---

## 6. What a prototype must test to move 5C

1. **Fix + control.** Replace `dfa.rs:291` with a real Hopcroft (worklist + predecessor
   index) *and* implement Revuz-on-acyclic (`is_acyclic = max_match_len.is_some()`). Corpus
   ≥50 tagged acyclic/cyclic. Emit three paired cold timings + a byte-identity oracle over
   `flat_transitions`. Report the acyclic fraction. **Decides Q5's "does the fast path beat
   correct Hopcroft / does deleting the restart erase its reason."**
2. **Congruence refutation.** Add a `StateNode` Language + rebuild-to-fixpoint minimizer;
   instrument pass-count on a length-n chain DFA; plot vs `n·log n`. **Confirms or refutes
   minimization-as-congruence as an efficiency lever (§3).**
3. **Tier coupling.** Keep the `accel.rs` table; add generated-LUT-budget accounting;
   construct a corpus where NibbleLut proliferation strains the budget and test whether a
   CSP trading one site to a cheaper tier beats greedy-local. If no corpus reproduces
   coupling → tier selection is local, table stays, CSP retired for the ladder (kept for
   Teddy co-bucketing only). **Decides Q6.**
4. **Where acyclic earns its keep.** Run Revuz vs Hopcroft on the W3 large
   bounded-repetition corpus at compile time; does linear-acyclic hit the ≥5× the W3 gate
   wants on the *bounded* subset? **Sites the acyclic path's real value.**
