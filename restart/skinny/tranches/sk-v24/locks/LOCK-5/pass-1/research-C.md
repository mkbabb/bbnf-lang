# LOCK-5 · Family 5C — AUTOMATA-AS-EXTRACTION (pass-1, research)

Charter: recast determinization, minimization (Hopcroft as rewrite-to-normal-form), and
prefilter-tier selection as e-graph saturation + CSP extraction; measure head-to-head against
the classical pipeline; wins stay, losses are named negative results.

The family's thesis in one line: **the regex engine already carries the exact substrate this
charter demands — an `EGraph<N,A>` with a CSP scheduler — but applies it only at the regex-HIR
algebra layer; 5C extends that same engine down one level, to the automaton itself, and asks
whether the automata algorithms survive the transposition.** The honest answer, developed
below, is *three different answers* for the three operations, and two of them are negative
results by construction.

---

## 1. The substrate that already exists (what 5C reuses, not invents)

The isomorphism the constitution demands (§ii) is already half-built. `bbnf-regex` runs
equality saturation over regex HIR on the **shared** `egraph` crate that `bbnf-ir` also uses:

- `crates/egraph/src/egraph.rs:23` `EGraph<N: Language, A: Analysis<N>>`; hash-cons `:155`
  `add`, union-find `:202` `union`, congruence `:251` `rebuild`, `:114/:119` `find`.
- `crates/egraph/src/extract.rs:85` `CostModel`, `:115` `Extractor` (`best_node`,
  `extract_tree`) — extraction under a cost lattice (`:36` `Lattice`).
- `crates/egraph/src/csp_scheduler.rs:175` `CspScheduler` — rewrite scheduling **already cast
  as a CSP**: `DirtyDomain` lattice (`:57`), `ParentDirtyProp` constraint (`:120`), driven
  through `Scheduler<N,A>` (`scheduler.rs:43`). This is the precedent that "e-graph saturation
  + CSP extraction" is not speculative in this tree — it is the running discipline, one layer up.
- The regex-HIR language: `parse-that/rust/regex/src/egraph/node.rs` `HirENode` (derives
  `egraph_derive::Language`; children projected `Box<Hir>→Id`, `Vec<Hir>→Box<[Id]>`),
  `translate.rs:22` `insert_hir` / `extract_hir`, five rewrite rules under `egraph/rules/`
  (`flatten`, `union`, `superset`, `redundant`, `repetition`), `mod.rs` `simplify_hir` (the one
  consumer, `RegexInfo::analyze_from_hir`). `egraph/mod.rs` states outright it *"Mirrors the
  grammar-tier e-graph in bbnf-ir."*

So the shared engine, the derive macro, the CSP scheduler, and the cost/extraction machinery
are all in hand. 5C's move is a **new `Language` node type one level below `HirENode`** — an
automaton coalgebra node — fed to the *same* `EGraph`, extracted by the *same* `Extractor`,
tier-selected by the *same* csp-solver. That is the constitution's "one system" made literal:
one e-graph engine, three node languages (grammar HIR, regex HIR, automaton).

The classical pipeline it races is `parse-that/rust/regex/src/automata/dfa.rs`:
`:218` `subset_construction` (BTreeSet powerset + `HashMap<BTreeSet<StateId>,u32>` memo),
`:291` `hopcroft_minimize`, `:189` `epsilon_closure`, `:205` `move_on_byte`; byte-class
partition at `sets/equiv.rs:94` `compute_byte_classes`; `dfa.rs:36` `state_limit: 4096` bail.

---

## 2. Three operations, three transpositions, three verdicts

### 2A. Minimization as congruence closure — **LAWFUL and provably exact (the safe GO)**

A DFA is a coalgebra: each state is `⟨accept: bool, succ: [StateId; num_classes]⟩`. Define an
e-node language `AutomatonENode { accept: bool, succ: Box<[Id]> }` (derive `Language`; `succ`
are the child Ids, exactly the `node.rs` `Box<[Id]>` field pattern). Insert every DFA state as
one e-node; `rebuild()` runs congruence closure: **two states merge iff they carry the same
accept flag and every successor is already in the same e-class.** The fixpoint of that merge is
the Myhill–Nerode quotient — the minimal DFA. Extraction (`Extractor`) then picks one
representative per e-class.

Why this is *exact* (unlike 2B): the automaton is **finite and total**, so congruence closure
(a least fixpoint, merging from singletons upward — the Moore direction) converges to the same
coarsest congruence that Hopcroft's partition refinement (greatest fixpoint, splitting from
`{accept, ¬accept}` downward) reaches. For a complete deterministic automaton the two fixpoints
coincide — this is the Myhill–Nerode / final-coalgebra theorem (Rutten, *Universal Coalgebra*,
TCS 249, 2000: minimization = quotient by the final-coalgebra bisimulation). The bridge from
e-graph congruence to automaton bisimulation is Bonchi & Pous, *Checking NFA Equivalence with
Bisimulations up to Congruence* (POPL 2013) — congruence closure **is** the up-to-congruence
bisimulation proof technique.

**Prerequisite (a named correctness trap):** the current `Dfa` is *partial* — missing
transitions use the `DEAD` sentinel (`dfa.rs:128`, `nfa.rs` `DEAD`). Congruence over a partial
automaton is not the same relation. The e-node must model `DEAD` as a distinguished absorbing
e-class (one shared sink node) so `succ` is total, or congruence merges states that differ only
in reachable dead transitions. This is cheap (one extra e-node) but must be in the prototype.

**Why it can win the head-to-head:** `hopcroft_minimize` at `dfa.rs:291` is **not** Hopcroft's
`O(n·k·log n)`. It is naive Moore refinement with a *global restart* — `dfa.rs:358`
`break; // restart refinement from the beginning`, wrapped in the `while changed` loop
(`:314`), re-scanning all blocks × all classes after every single split: `O(n²·k)` worst case.
Congruence closure via the shared union-find (`egraph.rs:202/:251`) is worklist-driven and does
not restart. So 5C's minimizer plausibly beats the *incumbent* minimizer on medium/large
automata — while producing a byte-identical state count (the correctness gate).

**Why it can lose (named negative result candidate):** against a *real* `O(n·k·log n)`
Hopcroft it may not win — and the e-graph carries hash-cons + rebuild + drop overhead the
tight `Vec`/`BTreeSet` path does not (`egraph/mod.rs` documents `drop_in_place<EGraph>`
dominating on small patterns; the whole `regex_dfa_compile` corpus is small — median ~55 µs per
`REPORT.md`). Prediction: **win vs the incumbent restart-Hopcroft; parity-or-loss vs a
reference real-Hopcroft on small patterns; win only on pathological blowups.** All three
outcomes are reportable per the "losses are named" clause.

### 2B. Determinization as derivative-saturation — **elegant, subsumes minimization, THEORY-BOUNDED (the honest negative-result risk)**

The deeper, more isomorphic move: skip subset construction entirely. Brzozowski derivatives
(Brzozowski, *Derivatives of Regular Expressions*, JACM 11(4), 1964; Owens–Reppy–Turon,
*Regular-expression derivatives re-examined*, JFP 19(2), 2009) build a DFA **directly from
regex terms**: each state is a derivative regex `∂_c(r)`, transitions are `c ↦ ∂_c`, accept iff
the term is nullable. Two derivative states are the *same* DFA state iff their regex terms are
equivalent. Brzozowski's theorem: if the equivalence is taken modulo a sound equational theory
(at least ACI of union), the derivative automaton is **finite**, and if the theory is *complete*
for regex equivalence, it is already **minimal** — determinization and minimization collapse
into one operation.

The e-graph decides that term equivalence: insert derivative terms as `HirENode`s, saturate the
**existing five rules** (`egraph/rules/`), and read one DFA state per e-class. This reuses
`translate.rs`, `node.rs`, and the entire rule set unchanged — the most constitution-aligned
casting possible (regex-HIR ⊂ bbnf-HIR; automata construction consumes the IR e-graph;
`classify` becomes an IR analysis — precisely LOCK-5's 5A phrasing, reached from the automata
side).

**The counterexample that makes this a negative result:** regex equivalence is
**PSPACE-complete** (Meyer–Stockmeyer). No *finite* rewrite rule set is complete for it (Kozen's
KAT axiomatization, *A Completeness Theorem for Kleene Algebras*, Inf. & Comp. 110, 1994, is
complete but is an infinite/schematic proof system, not a finite terminating egg-style rule
set). The five rules in `egraph/rules/` are sound but **incomplete**. Therefore the
saturation-derived DFA is minimal only *up to the rule theory*: wherever two derivative terms
are language-equal but not provably equal under `{flatten, union, superset, redundant,
repetition}`, they occupy **distinct e-classes → distinct DFA states → a non-minimal DFA.** The
family's honest position: derivative-saturation is a beautiful determinizer but not a reliable
minimizer, and a classical Hopcroft/congruence cleanup (2A) may still be owed on its output.

**What the prototype must measure to settle 2B:** the *minimality gap*
`ρ = |states(saturate-only)| / |states(true-minimal)|` across the 50-pattern
`regex_dfa_compile` corpus. If `ρ = 1` on the whole corpus, 2B is a landmark win (one
saturation replaces subset+Hopcroft). If `ρ > 1` on any pattern, the named negative result is:
*derivative-saturation is theory-bounded; it determinizes but under-minimizes; keep 2A as the
closer.* Second axis: end-to-end parse→DFA wall time vs `compile_with` (subset+Hopcroft),
cold paired N≥200.

### 2C. Prefilter / accel-tier selection as CSP extraction — **the clean CSP GO (no fixpoint hazard)**

This is the operation that is *genuinely* a discrete optimization and NOT a partition
refinement, so it escapes the fixpoint-direction trap of 2A/2B entirely.

The tier lattice already exists, spread across three sites: `automata/accel.rs` `AccelStrategy`
(`Memchr1/2/3`, `NibbleLut{lo_lut,hi_lut,exit_bytes}`, `None`); the prefilter tier
`Prefilter{None,Byte1,Byte2,Byte3}` with `from_dfa` reading the start-state row; and the wide
tier (shufti via `vqtbl1q_u8`, Teddy multi-literal) that PT-1.W2 specs as a *"cost model
(pluggable, not a hardcoded branch)"* fed by `info/literal_prefix.rs:85` and `sets/byteset.rs`.
Today the selection is an `if`-ladder (`accel.rs` exit-byte count; PROVE-10 `from_dfa` "≤3 →
memchr else None").

5C makes that ladder a real CSP over the csp-solver crate
(`crates/csp-solver`: `Csp`, `OptimizationMode`, `SolveConfig`, `solver`):
- **Variables**: per accelerable scan-site, `tier ∈ {Memchr1,2,3, Shufti, Teddy, None}`.
- **Domain constraints (legality)**: `Memchr{k}` legal iff exit-byte cardinality `= k`; `Shufti`
  iff cardinality ≤ 16 with a clean low/high-nibble LUT (`accel.rs` `NibbleLut`); `Teddy` iff a
  multi-byte literal prefix exists (`info/literal_prefix.rs`).
- **Objective**: a cost lattice `≈ skip-probability × kernel-cyc/B` extracted by the
  `Extractor`/`CostModel` machinery (`extract.rs:85`), the same lattice shape LOCK-2 wants.
- **The lazy-vs-eager decision** (LOCK-5 W3) folds in as a second CSP variable gated on
  `info/dfa_size.rs` `estimate_dfa_size` (already the pluggable size estimator) vs
  `dfa.rs:36` `state_limit`.

**The honest boundary (named risk, not a negative result yet):** with ≤~6 tiers and *per-site
independence*, a cost-sorted table beats a CSP solver's setup cost — the CSP earns its seat only
under a **global** constraint: a shared SIMD-LUT register/codegen budget across many sites, or
the generated-size budget (LOCK constitution §generated-size). The prototype must exhibit at
least one corpus where a global constraint makes the CSP choose *differently and better* than
greedy per-site; absent that, the verdict is "pluggable cost-sorted table" — which still honors
the pluggable-components edict, just without the solver. This is the safest concrete deliverable
of the family and the one most likely to ship.

---

## 3. Integration seams (path:line — where extant code serves)

| Seam | Location | Role in 5C |
|---|---|---|
| Shared e-graph engine | `crates/egraph/src/egraph.rs:23,155,202,251` | union-find + congruence rebuild reused verbatim for 2A/2B |
| Extraction + cost lattice | `crates/egraph/src/extract.rs:85,115`; `Lattice:36` | representative pick (2A), minimality (2B), tier cost (2C) |
| CSP scheduler (precedent) | `crates/egraph/src/csp_scheduler.rs:175` | proof that CSP-driven saturation already runs here |
| Node-language pattern | `regex/src/egraph/node.rs` `HirENode`; `translate.rs:22` | template for the new `AutomatonENode` (derive `Language`, `Box<[Id]>` succ) |
| Regex-HIR rules (2B reuse) | `regex/src/egraph/rules/{flatten,union,superset,redundant,repetition}.rs` | the (incomplete) equational theory deciding derivative-state equality |
| Classical baseline (race target) | `regex/src/automata/dfa.rs:218` `subset_construction`, `:291` `hopcroft_minimize` (naive restart `:358`) | the head-to-head incumbent |
| Byte-class partition | `regex/src/sets/equiv.rs:94` `compute_byte_classes` | a second partition-refinement candidate for congruence casting |
| Tier lattice | `regex/src/automata/accel.rs` `AccelStrategy`; PROVE-10 `Prefilter::from_dfa`; PT-1.W2 shufti/teddy | 2C CSP variables + domains |
| Cost inputs | `regex/src/info/literal_prefix.rs:85`, `info/dfa_size.rs` `estimate_dfa_size` | 2C legality + lazy/eager gate |
| CSP engine | `crates/csp-solver` `Csp/OptimizationMode/SolveConfig/solver` | 2C solver |
| Isomorphism anchor | `regex/src/egraph/mod.rs` ("mirrors bbnf-ir"); `crates/ir` `cost.rs` | the "one system" the constitution mandates |

Access discipline: `parse-that/rust/regex` is READ-ONLY; all measurement runs on the R3 §8
scratch-copy recipe (rsync working tree, repoint 7 external patch-deps, isolated
`CARGO_TARGET_DIR`), bench profile fat-LTO/cgu=1/O3, cold paired N≥200 — the PROVE-10 protocol.

---

## 4. Prior art (cited)

- **Brzozowski**, *Derivatives of Regular Expressions*, JACM 11(4), 1964 — derivative DFA.
- **Owens, Reppy, Turon**, *Regular-expression derivatives re-examined*, JFP 19(2), 2009 —
  similarity classes (ACI) give small/minimal derivative DFAs; the practical construction 2B copies.
- **Antimirov**, *Partial derivatives of regular expressions and finite automaton constructions*,
  TCS 155, 1996 — partial derivatives → NFA (the NFA-side alternative if 2B's DFA blows up).
- **Hopcroft**, *An n log n algorithm for minimizing states in a finite automaton*, 1971;
  **Moore** partition refinement, 1956 — the classical baselines 2A must beat/match.
- **Rutten**, *Universal Coalgebra: a theory of systems*, TCS 249, 2000 — DFA minimization =
  final-coalgebra quotient = the categorical justification that 2A is extraction.
- **Bonchi & Pous**, *Checking NFA equivalence with bisimulations up to congruence*, POPL 2013 —
  congruence closure IS the up-to-congruence bisimulation; the bridge for 2A.
- **Kozen**, *A Completeness Theorem for Kleene Algebras and the Algebra of Regular Events*,
  Inf. & Comp. 110, 1994; **Meyer & Stockmeyer** (regex equivalence PSPACE-complete) — the
  incompleteness bound that makes 2B a theory-bounded (negative) result.
- **Willsey et al.**, *egg: Fast and extensible equality saturation*, POPL 2021 — rebuild +
  extraction, the engine `crates/egraph` reimplements.
- **regex-automata / burntsushi** (lazy hybrid DFA, prefilter tiers), **Hyperscan** (Wang et al.,
  NSDI'19: shufti/truffle/teddy) — 2C's tier vocabulary and the lazy-DFA state-cap.

---

## 5. Named risks & counterexamples

1. **Fixpoint-direction hazard.** Congruence closure is a *least* fixpoint (merge from
   singletons); Hopcroft is a *greatest* fixpoint (split from coarse). They coincide for 2A
   (finite total automaton → same Myhill–Nerode quotient) but **NOT** for 2B (regex-term theory
   is incomplete → residual non-minimality). Counterexample class: any pair of language-equal
   regexes not related by the five rules (e.g. `(a*)* ≡ a*` is covered; `(ab)*a ≡ a(ba)*` is
   not — sliding equivalences outside the rule set). 2B under-minimizes there.
2. **E-graph overhead on small patterns.** The corpus is small (~55 µs compiles); hash-cons +
   `rebuild` + `drop_in_place<EGraph>` (documented cliff in `egraph/mod.rs`) can swamp the win.
   Counterexample: `[a-z]+`-class trivial patterns where subset+restart-Hopcroft is already
   microseconds. Mitigation: gate the e-graph path on `estimate_dfa_size` above a measured
   crossover; below it, keep the classical path (a pluggable cost decision, not a name-gate).
3. **Partial-transition congruence bug.** `DEAD` must become a real absorbing e-class or 2A
   merges states differing only in dead edges. Counterexample: `abc` (a chain whose non-accept
   states all lack most transitions) collapses wrongly if `DEAD` is not modeled.
4. **CSP over-engineering (2C).** With independent per-site tiers, a sorted cost table wins; the
   solver justifies itself only under a global budget constraint. Counterexample to *require* the
   CSP: a grammar with many scan sites sharing a bounded SIMD-LUT/codegen budget where greedy
   per-site over-allocates. If no such case measurably beats greedy → verdict is the table.
5. **Determinism / reproducibility.** e-graph extraction and CSP solving must be deterministic
   for regen bit-for-bit (constitution / generated-files law). csp-solver ordering
   (`ordering.rs`) and extraction tie-breaks must be total orders, asserted by a regen idempotency
   test.

---

## 6. What a pass-1 prototype MUST test (to move this family)

- **P1 (2A, the safe GO).** Add `AutomatonENode` (derive `Language`) + a congruence-closure
  minimizer on `crates/egraph`. Head-to-head on the `regex_dfa_compile` 50-pattern corpus, cold
  paired N≥200, vs (a) the incumbent `dfa.rs:291` restart-Hopcroft and (b) a reference
  `O(n·k·log n)` Hopcroft. **Correctness gate: minimized state count byte-identical to (b) on
  every pattern.** Metric: compile-time ratio + the small/large crossover. Expected: win vs (a),
  report the (b) crossover honestly.
- **P2 (2B, the negative-result probe).** Brzozowski-derivative determinization over the
  existing `HirENode` rules. Measure the minimality gap `ρ = states(saturate) / states(minimal)`
  per pattern and end-to-end parse→DFA time vs `compile_with`. GO only if `ρ=1` corpus-wide AND
  time competitive; else file the named negative result (theory-bounded under-minimization) and
  keep 2A as the closer.
- **P3 (2C, the CSP-vs-table adjudication).** Tier-selection CSP (csp-solver) vs a cost-sorted
  table, on both independent-site and shared-budget scenarios. GO for the CSP only if a global
  constraint makes it beat greedy measurably; else the verdict is the pluggable table (still
  edict-compliant). Correctness gate: identical match sets vs the scalar scan (PROVE-10's paired
  `count_scalar == count_prefilter` discipline, promoted to `tests/`).

The family converges when P1 gives a landing minimizer (win or a named crossover), P2 returns
either the collapse-win or its named theory bound, and P3 returns CSP-or-table with the evidence
that decides it — every outcome, win or loss, a certified head-to-head, never an assertion.
