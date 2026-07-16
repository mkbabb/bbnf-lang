# LOCK-5 · Family 5C — AUTOMATA-AS-EXTRACTION (pass-1, research)

Charter: recast determinization, minimization (Hopcroft as rewrite-to-normal-form), and
prefilter-tier selection as e-graph saturation + CSP extraction; measure head-to-head against
the classical pipeline; wins stay, losses are named negative results.

The family's thesis in one line: **the regex engine already carries the exact substrate this
charter demands — an `EGraph<N,A>` with a CSP scheduler — but applies it only at the regex-HIR
algebra layer; 5C asks whether that same engine can descend one level, to the automaton
itself.** The honest answer, developed below and sharpened against the source, is *three
distinct verdicts* for the three operations, and the sharpest of them is a correctness
boundary the elegant framing conceals: **the automaton is a cyclic graph and the e-graph's
native congruence is an inductive (least) fixpoint, whereas minimization is a coinductive
(greatest) fixpoint — the two do not coincide by computation, only by result.** That gap is
the family's load-bearing finding.

---

## 1. The substrate that already exists (what 5C reuses, not invents)

The isomorphism the constitution demands (§ii) is already half-built. `bbnf-regex` runs
equality saturation over regex HIR on the **shared** `egraph` crate that `bbnf-ir` also uses
(verified `path:line`, top-level tree):

- `crates/egraph/src/egraph.rs:23` `EGraph<N: Language, A: Analysis<N> = NoAnalysis>`;
  `:114` `find`, `:119` `find_ref`, `:155` `add` (hash-cons by canonical children),
  `:202` `union`, `:251` `rebuild` (deferred congruence, the egg discipline). The union-find
  proper is `crates/egraph/src/unionfind.rs`.
- `crates/egraph/src/extract.rs:36` `Lattice: Clone + PartialOrd`, `:85` `CostModel<N>`,
  `:115` `Extractor<'a,N,A,C>`, `:183` `best_node`, `:190` `extract_tree` — extraction under a
  cost lattice.
- `crates/egraph/src/csp_scheduler.rs` — rewrite scheduling **already cast as a CSP**:
  `:57` `DirtyDomain` (a `LatticeDomain`, `:96`), `:120` `ParentDirtyProp` (a
  `Constraint<DirtyDomain>`, `:141`, whose `:152` `revise` propagates dirtiness up parent
  edges), driven through `scheduler.rs`. This is the precedent that "e-graph saturation + CSP
  extraction" is not speculative in this tree — it is the running discipline one layer up.
- `crates/egraph/src/language.rs:16` `Language: Clone + Eq + Hash` — `:21` `children(&self) ->
  &[Id]`, `:25` `children_mut`, `:30` `matches` (structural, children excluded). This is the
  contract any new node language must satisfy.
- The regex-HIR language: `parse-that/rust/regex/src/egraph/node.rs:45` `HirENode` (`:44`
  `#[derive(…, Language)]` via `egraph_derive::Language`; `Box<Hir>→Id`, `Vec<Hir>→Box<[Id]>`,
  e.g. `:70` `Concat(Box<[Id]>)`), `translate.rs:19` `insert_hir` / `:60` `extract_hir`, five
  rewrite rules under `egraph/rules/{flatten,union,superset,redundant,repetition}.rs`, plus
  `saturation_cache.rs` and `cost.rs`. `egraph/mod.rs` states outright it *"Mirrors the
  grammar-tier e-graph in bbnf-ir."*

So the shared engine, the derive macro, the CSP scheduler, and the cost/extraction machinery
are all in hand. 5C's *proposed* move is a new `Language` node one level below `HirENode` — an
automaton coalgebra node fed to the *same* `EGraph`. §2A below is exactly where that proposal
meets its structural obstruction.

The classical pipeline it races is `parse-that/rust/regex/src/automata/dfa.rs`:
`:218` `subset_construction` (`BTreeSet` powerset + `HashMap<BTreeSet<StateId>,u32>` memo,
`:262` `state_limit`-4096 bail at `:36`), `:291` `hopcroft_minimize`, `:189` `epsilon_closure`,
`:205` `move_on_byte`; byte-class partition at `sets/equiv.rs:17` `compute_byte_classes`
(dense `0..num_classes`, small/large split `:36`/`:66`); `nfa.rs` `DEAD` sentinel.

---

## 2. Three operations, three transpositions, three verdicts

### 2A. Minimization as congruence closure — **GO as a non-restart refiner; the "for-free-via-rebuild" reading is REFUTED**

The elegant proposal: a DFA state is a coalgebra cell `⟨accept: bool, succ: [StateId;
num_classes]⟩`; model it as `AutomatonENode { accept: bool, succ: Box<[Id]> }` (derive
`Language`, `succ` the child Ids — exactly `node.rs`'s `Box<[Id]>` pattern); insert every state,
call `rebuild()`, read one representative per e-class via `Extractor`. The claim is that
congruence closure lands on the Myhill–Nerode quotient for free.

**This reading is false, and naming precisely why is the family's contribution.** Two
independent obstructions, both structural to e-graphs:

1. **Fixpoint direction.** E-graph congruence closure computes the *least* congruence
   consistent with the asserted equalities: two nodes merge only when their children are
   *already* merged (`egraph.rs:155/:251` hashcons-then-rebuild). DFA state equivalence
   (bisimilarity / Myhill–Nerode) is the *greatest* fixpoint — coinductive: `p ~ q` unless some
   word distinguishes them. Least ≠ greatest exactly when the transition graph has **cycles**.
   Counterexample (canonical): two accepting states `p, q`, each self-looping on every byte
   (`δ(p,·)=p`, `δ(q,·)=q`). They are Myhill–Nerode-equivalent and *must* merge. As e-nodes
   `p={accept, [P,P,…]}`, `q={accept, [Q,Q,…]}`: congruence fires only if `P` and `Q` are
   already one class — which is the very fact we want to derive. Standard congruence closure
   performs no speculative merge, so `p, q` stay distinct → **under-minimization on any starred
   sub-expression.** `[a-z]+`, `\w+`, `0x[0-9a-fA-F]+` (all in the PROVE-10 corpus) are cyclic
   and exhibit this.
2. **Cyclic construction.** `egraph.rs:155` `add(node)` requires each child `Id` to exist
   *before* the node — a term (DAG) discipline. A self-looping state has a child equal to
   itself; it cannot be `add`-ed bottom-up at all without a placeholder-then-patch protocol
   foreign to the egg contract. Automata are cyclic graphs; e-graphs are acyclic terms. The
   mismatch is structural, not merely a tuning knob.

**Where the elegant claim IS exact — a real, useful sub-class.** For an **acyclic** DFA (a
finite language: bounded `{m,n}` repeats, fixed-width literals — e.g. `#[0-9a-fA-F]{3,6}`) the
transition graph is a DAG, `add` succeeds bottom-up, and congruence closure *coincides* with
bisimulation (no cycle ⇒ least = greatest). So congruence-closure minimization is provably
exact and genuinely "free" **for acyclic patterns**, and only there. This is a landmark-shaped
sub-result the naive framing missed: a corpus-measurable partition of leaves into
*congruence-minimizable* (acyclic) and *refinement-only* (cyclic).

**The correct casting for the cyclic core.** Minimization is partition refinement (Moore /
Hopcroft / Paige–Tarjan / Valmari) — a greatest fixpoint reached by *splitting* from
`{accept, ¬accept}`. Union-find supports merge, not split, so refinement is implemented over
the block structure and the shared `unionfind.rs` stores only the *final* quotient (and the
acyclic sub-DAGs where congruence did fire). The e-graph's honest contribution is the shared
union-find + hashcons for representative extraction, **not** the term-congruence `rebuild`
loop. The alternative genuinely-congruence-powered route is Bonchi–Pous bisimulation *up to
congruence* (POPL 2013): explore state *pairs*, using congruence closure to prune — this DOES
use e-graph congruence, but as an equivalence *checker* over O(n²) pairs, not a one-shot
`rebuild`.

**Why it still wins the head-to-head (the GO).** The incumbent `hopcroft_minimize`
(`dfa.rs:291`) **is not Hopcroft.** It is naive Moore refinement with a **global restart**:
`dfa.rs:358` `break; // restart refinement from the beginning`, nested inside the block loop
(`:318`) which itself breaks (`:362`) back to `while changed` (`:315`). Every single split
re-scans all blocks × all classes from scratch: one split per outer pass ⇒ `O(n)` passes ×
`O(n·k)` scan = **`O(n²·k)`**. A worklist refinement with no restart (Hopcroft `O(n·k·log n)`,
or Valmari's refinable-partition `O(n·k)` in practice), sharing the union-find quotient store,
plausibly beats it on medium/large automata **while producing a byte-identical state count**
(the correctness gate). That is the shippable 2A: *a correct non-restart refinement minimizer,
congruence-exact on the acyclic sub-class, refinement on the cyclic core, sharing the e-graph
union-find* — not "the e-graph minimizes for free."

**Named correctness trap (independent of the above).** The current `Dfa` is *partial* —
missing transitions are `DEAD` (`nfa.rs`; `dfa.rs:330,340` special-case it as `usize::MAX`).
Refinement over a partial automaton must model `DEAD` as a distinguished absorbing block (one
shared sink) or it conflates states differing only in dead edges. Cheap; mandatory in the
prototype. `abc` (a chain whose interior states lack most transitions) is the witness.

### 2B. Determinization as derivative-saturation — **elegant, subsumes minimization, THEORY-BOUNDED (the honest negative-result risk)**

The deeper move: skip subset construction. Brzozowski derivatives (JACM 11(4), 1964;
Owens–Reppy–Turon *re-examined*, JFP 19(2), 2009) build a DFA **directly from regex terms** —
each state is a derivative regex `∂_c(r)`, transition `c ↦ ∂_c`, accept iff nullable. Two
derivative states are the *same* DFA state iff their terms are equivalent. Brzozowski's theorem:
modulo a sound equational theory containing at least ACI of union the derivative automaton is
**finite**; if the theory is *complete* for regex equivalence it is already **minimal** —
determinization and minimization collapse. Owens–Reppy–Turon's "smart constructors" (ACI
normalization) are *precisely* the shape of `egraph/rules/{flatten,union,redundant,…}`.

The e-graph decides that term equivalence: insert derivative terms as `HirENode`s, saturate
the **existing five rules**, read one DFA state per e-class. This reuses `translate.rs`,
`node.rs`, and the full rule set unchanged — the most constitution-aligned casting possible
(regex-HIR ⊂ bbnf-HIR; automata construction consumes the IR e-graph; `classify` becomes an IR
analysis — LOCK-5's 5A phrasing reached from the automata side).

**The counterexample that makes this a negative result:** regex equivalence is
**PSPACE-complete** (Meyer–Stockmeyer). No *finite* rewrite set is complete for it (Kozen's KAT
axiomatization, Inf.&Comp. 110, 1994, is complete but infinite/schematic — not a terminating
egg rule set). The five rules are sound but **incomplete**; the saturation-derived DFA is
minimal only *up to the theory*: wherever two derivative terms are language-equal but not
provably equal under `{flatten, union, superset, redundant, repetition}`, they occupy
**distinct e-classes → distinct states → a non-minimal DFA.** Sliding equivalences are the
witness class — `(a*)* ≡ a*` is covered by `repetition`, but `(ab)*a ≡ a(ba)*` is *not* related
by any of the five. The honest position: derivative-saturation is a beautiful determinizer, an
unreliable minimizer — a 2A-style refinement cleanup is still owed on its output. (Antimirov
*partial* derivatives, TCS 155, 1996 → an NFA, are the fallback if the DFA blows up.)

**What the prototype must settle:** the *minimality gap* `ρ = |states(saturate-only)| /
|states(true-minimal)|` across the 50-pattern `regex_dfa_compile` corpus. `ρ=1` corpus-wide ⇒
landmark (one saturation replaces subset+Hopcroft). Any `ρ>1` ⇒ the named negative result
*(theory-bounded; determinizes but under-minimizes; keep 2A as closer)*. Plus end-to-end
parse→DFA wall time vs `compile_with`, cold paired N≥200.

### 2C. Prefilter / accel-tier selection as CSP extraction — **the clean CSP GO (no fixpoint hazard)**

This is genuinely discrete optimization, not partition refinement, so it escapes the
fixpoint-direction trap of 2A/2B entirely — the safest shippable deliverable of the family.

The tier lattice already exists, spread across sites: `automata/accel.rs:15` `AccelStrategy`
(`:17` `Memchr1(u8)`, `:19` `Memchr2`, `:21` `Memchr3`, `:23` `NibbleLut{lo_lut,hi_lut,
exit_bytes: SmallVec<[u8;8]>}` — the shufti tier, built by `:95` `build_nibble_luts`, `:29`
`None`); the start-state entry-byte prefilter `Prefilter{None,Byte1,Byte2,Byte3}` with
`from_dfa` (PROVE-10, reading the state-0 transition row); the wide/multi-literal tier (truffle
`vqtbl`, Teddy) that PT-1.W2 specs as a *"cost model (pluggable, not a hardcoded branch)."*
Today selection is an `if`-ladder (`accel.rs:91-105`: exit-byte count `1/2/3 → Memchr{k}`, `≤16
→ NibbleLut`, else `None`).

5C makes that ladder a real CSP over `crates/csp-solver` (`builder/`, `constraint/`, `domain/`,
`solver/`, `ordering.rs`):
- **Variables**: per accelerable scan-site, `tier ∈ {Memchr1,2,3, Shufti(NibbleLut), Teddy,
  None}`.
- **Domain constraints (legality)**: `Memchr{k}` iff exit-byte cardinality `= k`
  (`accel.rs:88`); `Shufti` iff cardinality `≤16` with a clean low/high-nibble LUT
  (`build_nibble_luts`); `Teddy` iff a multi-byte literal prefix exists
  (`info/literal_prefix.rs:8` `extract_literal_prefix`).
- **Objective**: a cost lattice `≈ skip-probability × kernel-cyc/B`, extracted by the
  `Extractor`/`CostModel` machinery (`extract.rs:85/:115`) — the same lattice shape LOCK-2
  wants; cyc/B directional on macOS (PMU-restricted; ratios certified).
- **The lazy-vs-eager decision** folds in as a second CSP variable gated on `info/dfa_size.rs:10`
  `estimate_dfa_size` (verified: `nfa_estimate.saturating_mul(3)` — a pluggable heuristic) vs
  `dfa.rs:36` `state_limit`; `info/one_pass.rs:38` `check_one_pass_eligible` /`:19`
  `is_hir_walkable` are the ready-made legality predicates for the one-pass fast path.

**The honest boundary (named risk, not yet a negative result):** with ≤~6 tiers and *per-site
independence*, a cost-sorted table beats a CSP solver's setup cost. The CSP earns its seat only
under a **global** constraint — a shared SIMD-LUT register / codegen budget across many sites,
or the constitution's generated-size budget. The prototype must exhibit ≥1 corpus where a global
constraint makes the CSP choose *differently and better* than greedy per-site; absent that, the
verdict is "pluggable cost-sorted table" — still honoring the pluggable-components edict, just
without the solver.

---

## 3. Integration seams (path:line — where extant code serves)

| Seam | Location | Role in 5C |
|---|---|---|
| Shared e-graph engine | `crates/egraph/src/egraph.rs:23,114,155,202,251`; `unionfind.rs` | union-find + representative store; congruence `rebuild` reusable **only** on acyclic sub-DFAs (2A) / derivative terms (2B) |
| `Language` contract | `crates/egraph/src/language.rs:16,21,25,30` | trait the `AutomatonENode`/`HirENode`-derivative node must satisfy |
| Extraction + cost lattice | `crates/egraph/src/extract.rs:36,85,115,183,190` | representative pick (2A), minimality read (2B), tier cost (2C) |
| CSP scheduler (precedent) | `crates/egraph/src/csp_scheduler.rs:57,120,141,152`; `scheduler.rs` | proof CSP-driven saturation already runs here |
| Node-language pattern | `parse-that/…/regex/src/egraph/node.rs:44,45,70`; `translate.rs:19,60` | template for a new node (derive `Language`, `Box<[Id]>` succ) |
| Regex-HIR rules (2B theory) | `…/regex/src/egraph/rules/{flatten,union,superset,redundant,repetition}.rs` | the (incomplete) equational theory deciding derivative-state equality |
| Classical baseline (race target) | `…/regex/src/automata/dfa.rs:218` `subset_construction`, `:291` `hopcroft_minimize` (naive restart `:358`, `O(n²·k)`) | the head-to-head incumbent 2A must beat |
| Byte-class partition | `…/regex/src/sets/equiv.rs:17` `compute_byte_classes` | second partition-refinement candidate; dense class space for `succ` width |
| Tier lattice | `…/regex/src/automata/accel.rs:15,17-29,88-105` `AccelStrategy`/`build_nibble_luts`; PROVE-10 `Prefilter::from_dfa`; PT-1.W2 shufti/teddy | 2C CSP variables + domains |
| Cost / gating inputs | `…/regex/src/info/literal_prefix.rs:8`, `dfa_size.rs:10` `estimate_dfa_size`, `one_pass.rs:38` `check_one_pass_eligible` | 2C legality + lazy/eager + one-pass gate |
| CSP engine | `crates/csp-solver/src` (`builder/`, `constraint/`, `domain/`, `solver/`, `ordering.rs`) | 2C solver; `ordering.rs` = the determinism total-order |
| Isomorphism anchor | `…/regex/src/egraph/mod.rs` ("mirrors bbnf-ir"); `crates/ir/src/egraph`, `crates/ir/src/egraph/rules/regex.rs` (grammar-tier copy) | the "one system" the constitution mandates; the duplicate rule set 5A/5B target |

Access discipline: `parse-that/rust/regex` is READ-ONLY (6742 LOC total, verified); all
measurement on the R3 §8 scratch-copy recipe (rsync working tree, repoint external patch-deps,
isolated `CARGO_TARGET_DIR`), pristine bench profile fat-LTO / cgu=1 / O3, cold paired N≥200 —
the PROVE-10 protocol.

---

## 4. Prior art (cited)

- **Brzozowski**, *Derivatives of Regular Expressions*, JACM 11(4), 1964 — derivative DFA (2B).
- **Owens, Reppy, Turon**, *Regular-expression derivatives re-examined*, JFP 19(2), 2009 —
  ACI similarity classes ("smart constructors") give small/minimal derivative DFAs; the shape
  of the five HIR rules.
- **Antimirov**, *Partial derivatives…*, TCS 155, 1996 — partial derivatives → NFA (2B fallback).
- **Moore** (1956, refinement) / **Hopcroft** (*An n log n algorithm…*, 1971) / **Paige &
  Tarjan** (*Three partition refinement algorithms*, SICOMP 16(6), 1987) / **Valmari** (*Fast
  brief practical DFA minimization*, IPL 112, 2012) — the greatest-fixpoint minimizers 2A must
  match (state count) and beat (time) vs the incumbent restart-Moore.
- **Rutten**, *Universal Coalgebra*, TCS 249, 2000 — minimization = final-coalgebra quotient;
  the categorical statement that minimization is a **greatest** fixpoint (why native e-graph
  least-fixpoint congruence does not compute it on cyclic automata).
- **Bonchi & Pous**, *Checking NFA equivalence with bisimulations up to congruence*, POPL 2013 —
  the ONE way congruence closure genuinely powers bisimulation: pair-exploration up-to, not a
  one-shot `rebuild`.
- **Kozen**, *A Completeness Theorem for Kleene Algebras…*, Inf.&Comp. 110, 1994; **Meyer &
  Stockmeyer** (regex equivalence PSPACE-complete) — the incompleteness bound that makes 2B a
  theory-bounded negative result.
- **Hopcroft & Karp** (union-find NFA equivalence, 1971) — the classical near-linear equivalence
  test underlying the Bonchi–Pous accelerator (the 2A-alt substrate).
- **Willsey et al.**, *egg: Fast and extensible equality saturation*, POPL 2021 — deferred
  `rebuild` + extraction; the engine `crates/egraph` reimplements; its acyclic-term premise is
  precisely why §2A's cyclic obstruction bites.
- **regex-automata / burntsushi** (lazy hybrid DFA, quit-byte, prefilter ladder),
  **Hyperscan** (Wang et al., NSDI'19: shufti/truffle/teddy) — 2C's tier vocabulary + the
  lazy-DFA state cap.

---

## 5. Named risks & counterexamples

1. **Fixpoint-direction / cyclic-graph mismatch (2A, the load-bearing one).** E-graph
   congruence is a *least* fixpoint over acyclic terms; DFA minimization is a *greatest*
   fixpoint (bisimulation) over a cyclic graph. They agree only on the *result* for acyclic
   automata. Witness: two self-looping accept states never merge under `rebuild`
   (under-minimization); a self-looping state can't even be `add`-ed bottom-up. Consequence:
   the "e-graph minimizes for free" reading is refuted; the shippable 2A is a non-restart
   refinement minimizer sharing the union-find, congruence-exact only on the acyclic sub-class.
2. **E-graph overhead on small patterns.** The corpus is small (~55 µs compiles per PROVE-10);
   hash-cons + `rebuild` + `drop_in_place<EGraph>` (documented cliff in `egraph/mod.rs`) can
   swamp the win on trivial `[a-z]+`-class patterns. Mitigation: gate the refinement path on
   `estimate_dfa_size` above a measured crossover; below it, the classical path — a pluggable
   cost decision, not a name-gate.
3. **Partial-transition (`DEAD`) trap (2A).** `DEAD` must become a real absorbing block/e-class
   or refinement conflates states differing only in dead edges. Witness: `abc`.
4. **CSP over-engineering (2C).** With independent per-site tiers a sorted cost table wins; the
   solver justifies itself only under a global budget. Witness *to require* the CSP: many scan
   sites sharing a bounded SIMD-LUT/codegen budget where greedy over-allocates. Absent a
   measured such case → verdict is the table.
5. **Determinism / reproducibility.** Extraction tie-breaks and CSP variable/value ordering
   (`csp-solver/src/ordering.rs`) must be total orders for regen bit-identity (generated-files
   law), asserted by a regen idempotency test.

---

## 6. What a pass-1 prototype MUST test (to move this family)

- **P1 (2A, the GO + the refutation).** Add the automaton node + a **non-restart** refinement
  minimizer (Valmari or worklist-Hopcroft) sharing `crates/egraph` union-find, with a
  congruence-closure fast path taken **only** on acyclic DFAs. Head-to-head on the
  `regex_dfa_compile` 50-pattern corpus (cyclic *and* acyclic leaves present), cold paired
  N≥200, vs (a) the incumbent `dfa.rs:291` restart-Moore and (b) a reference `O(n·k·log n)`
  Hopcroft. **Correctness gate: minimized state count byte-identical to (b) on every pattern —
  including the cyclic ones the congruence path must NOT be trusted with.** Report: the win vs
  (a), the crossover vs (b), and the measured acyclic fraction where congruence-closure is
  exact-and-free (the landmark sub-result).
- **P2 (2B, the negative-result probe).** Brzozowski-derivative determinization over the
  existing `HirENode` rules. Measure `ρ = states(saturate) / states(minimal)` per pattern and
  parse→DFA time vs `compile_with`. GO only if `ρ=1` corpus-wide AND time competitive; else file
  the theory-bounded under-minimization result (`(ab)*a ≡ a(ba)*` as the named uncovered
  equivalence) and keep 2A as the closer.
- **P3 (2C, the CSP-vs-table adjudication).** Tier-selection CSP (`crates/csp-solver`) vs a
  cost-sorted table, on both independent-site and shared-budget scenarios. GO for the CSP only if
  a global constraint makes it beat greedy measurably; else verdict = the pluggable table.
  Correctness gate: identical match sets vs the scalar scan (PROVE-10's paired
  `count_scalar == count_prefilter`, promoted to `tests/`).

The family converges when P1 delivers a landing non-restart minimizer (win vs the incumbent,
honest crossover vs real-Hopcroft) *plus* the named acyclic/cyclic partition, P2 returns either
the collapse-win or its named theory bound, and P3 returns CSP-or-table with the evidence that
decides it — every outcome a certified head-to-head, never an assertion. 5C rides *below*
whichever type-plane family wins (it consumes their HIR, produces their DFA); its only internal
exclusion is that 2B-as-minimizer cannot replace the 2A refiner — 2B determinizes, 2A closes.
