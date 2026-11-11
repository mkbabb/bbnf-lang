# LOCK-5 · Family 5C — AUTOMATA-AS-EXTRACTION · pass-3 · research-C

Charter: determinization, minimization (as rewrite-to-normal-form / congruence), and
prefilter-tier selection recast as e-graph saturation + CSP extraction — measured
head-to-head against the classical pipeline; wins stay, losses are named negative results.
Settled ground (XL-C): grammar tier keeps `Regex(StringId)` opaque; engine tier keeps a rich
`Hir` (grown with `Class`/`Look`); the vendored skinny copy is the number/unicode substrate,
the automata engine is `bbnf-regex` (`parse-that/rust/regex`, READ-ONLY, base-pin HEAD). This
family answers Q5 (minimization) and Q6 (tier selection).

---

## 0. The substrate as it actually stands (base-pin, verified)

The determinize→minimize pipeline is `automata/dfa.rs`: `compile_with` (`:67`) →
`Nfa::from_hir` → `from_nfa` (`:77`), whose Step 3 is `subset_construction` (`:218`) and Step 4
is `hopcroft_minimize` (`:291`, gated `opts.minimize && states>1` at `:89`). Byte-equivalence
classes (`sets/equiv.rs::compute_byte_classes`) collapse 256 bytes to `num_classes` columns
(`:80`) BEFORE determinization — the one place the classical pipeline already does the right
thing, and it must be preserved by any e-graph reformulation (the transition alphabet is
`num_classes`, not 256; `DfaState.transitions: Vec<StateId>` `:17` is indexed by class).

**The mislabel that governs everything.** `hopcroft_minimize` (`dfa.rs:291`) is NOT Hopcroft.
It is Moore partition refinement (Moore 1956) with a **global restart on every split**: on the
first disagreeing member it splits, sets `changed=true`, and `break`s "restart refinement from
the beginning" (`:357-358`), then `break`s the block loop (`:362-363`), re-entering the outer
`while changed` (`:314`) from block 0. Each full sweep is O(n·num_cls) (Σ over blocks of
members×classes = n·num_cls); there are up to O(n) splits, each triggering a fresh sweep ⇒
**O(n²·num_cls)** worst case. True Hopcroft (Hopcroft 1971) is **O(num_cls·n·log n)** via a
worklist of (block, class) splitters that never rescans a stable block. The docstring's
"Hopcroft's algorithm" (`dfa.rs:1,4,290`) is aspirational. **grep `acyclic|is_acyclic` across
the whole crate = 0 hits** (only the `:358` restart comment): the "acyclic fast path" Q5
interrogates is a PROPOSED route, not extant code. This is the pivot of Q5.

The e-graph (`egraph/`, 1253 LOC) operates on **HIR**, not on DFA states: `build_hir_egraph`
(`egraph/mod.rs:68`) → `saturate_hir_egraph` (`:87`) → `extract_canonical` (`:119`) rewrites
the pattern algebra (`[a-c]|[d-f]`→`[a-f]`, `superset.rs`/`union.rs`/`flatten.rs`) under
`RegexExtractionCost` (`egraph/cost.rs:16`, sharing `egraph::CostWeights` with the bbnf tier).
It runs BEFORE `Nfa::from_hir`. There is today **no e-graph on the automata (state) plane** —
so "minimization-as-congruence" is net-new, and its lawfulness is the whole question.

---

## Q5 — Minimization: acyclic fast path vs a CORRECT Hopcroft control

### The mechanism made concrete

DFA minimization computes the **coarsest** partition compatible with (a) accept-status and (b)
transition-respecting refinement: p≡q iff `is_accept(p)=is_accept(q)` and ∀class c,
`δ(p,c)≡δ(q,c)`. This is a **greatest fixpoint** (bisimulation / Myhill-Nerode congruence):
start everything-equal, refine down. Three lawful realizations, measured head-to-head:

1. **Reference Hopcroft (the control Q5 demands).** Worklist `W` of (block, class) splitters;
   pop, for each class split every block by pre-image; push the smaller half (the "process the
   smaller half" invariant buys the log n). O(num_cls·n·log n), no restart — the correct
   baseline the incumbent `dfa.rs:291` fails to be. **~120 LOC, self-contained, replaces
   `:291-438`.** Port target: Valmari 2012 (refinable-partition structure: marked/unmarked queue,
   block-of-element index) because it handles the DEAD/incomplete transition (`DEAD` at
   `dfa.rs:9`, sentinels at `:330,340`) without a completion pass.

2. **Acyclic fast path (Revuz).** If the transition graph is a DAG (no cycle through a non-DEAD
   edge), minimization is a single **reverse-topological signature bucketing**: process states in
   reverse-topo order, give each the signature `(is_accept, [class→new_id_of_target])`,
   hash-cons; identical signatures ⇒ merged. **O(n·num_cls), no log factor.** Prior art: Revuz
   1992; this is the MADFA/DAWG construction (Daciuk et al. 2000). **The decisive observation:
   Revuz signature hash-consing IS bottom-up congruence closure** — literally what an e-graph's
   `rebuild()` does. So for the acyclic fragment, "minimization-as-e-graph-congruence" is not an
   analogy; it is the same computation, and it is optimal (linear).

3. **Minimization-as-congruence (the e-graph route), general case — the NEGATIVE RESULT.** The
   e-graph substrate (`egraph` crate, union-find + hash-cons `rebuild`) computes a **least**
   fixpoint: assert equalities, close congruence upward. DFA minimization is the **greatest**
   fixpoint. For a **cyclic** DFA these differ: the coarsest bisimulation needs
   *assume-equal-then-refute-on-a-witness*; upward congruence can only *merge on proven
   equality*, and two states on a cycle are never "proven equal" bottom-up because their
   signatures reference each other. Naive hash-consing of cyclic states either never converges or
   under-merges. **Therefore the e-graph can lawfully own minimization ONLY on the acyclic
   fragment** (where it coincides with Revuz). This hard boundary is the family's principal named
   result. (A theoretical bridge exists — Bonchi & Pous 2013 "bisimulations up to congruence"
   (HKC), coinduction *up to congruence* — but HKC is an equivalence *checker*, not a partition
   *builder*; grafting a coinductive upper bound onto a least-fixpoint union-find is a research
   project, not substrate reuse. Name it; do not promise it.)

### What the acyclic dispatch buys, and the trap Q5 sets

`is_acyclic` (a DFS back-edge check over the class-indexed table, O(n·num_cls)) routes DAG
patterns to Revuz (linear, = e-graph congruence) and cyclic patterns to reference Hopcroft
(n·log n). Regex leaves are acyclic exactly when `*`/`+`-free: literals, bounded reps unrolled
(`estimate_nfa_states` unrolls `(_,Some(m))` at `info/dfa_size.rs:32`), fixed char-class
concats, finite-word alternations. The `quantified_class`/`negated_class` families (`[^"\\]+`,
`\d*`, `[a-z]+`) are **cyclic** (the `+` self-loop is the accel target, `accel.rs:1-6`).

**The trap, stated plainly:** the incumbent's cost is the *restart* (`dfa.rs:358`), not the
cyclicity. If Q5's control is the CORRECT Hopcroft (route 1), the acyclic path competes against
n·log n, not O(n²·num_cls). On the actual corpus — `SMALL_DFA` is `states ≤ 64`
(`info/mod.rs:385`) and PROVE-10's patterns compile in ~55 µs total — log₂(64)≈6 and constant
factors dominate; Revuz-linear and Valmari-Hopcroft are **indistinguishable at n≤64**. So
"deleting the restart erases the acyclic path's reason to exist" is the *likely* outcome on the
incumbent corpus, and the family must not ship the acyclic path on a corpus that cannot separate
it. The acyclic path earns its place ONLY if the GROWN corpus exhibits DFAs large enough
(unrolled `{4,8}` × wide classes, deep literal alternations → hundreds of states) that the n/log
n gap and the missing log factor become measurable. **Honest disposition: conditional. Acyclic-
Revuz stays iff the crossover n is inside the real corpus; else it is a named negative result and
the deliverable is simply "replace restart-Moore at dfa.rs:291 with correct Valmari-Hopcroft" —
a pure win with no dispatch.**

### Congruence-rebuild complexity, measured against the O(n·k·log n) claim

- Acyclic congruence rebuild (Revuz / e-graph bottom-up): **O(n·num_cls)** — one reverse-topo
  pass, each state hashed on its num_cls-wide signature; hash-cons amortized O(num_cls) per
  state. No log factor. Strictly better than O(n·k·log n) on the DAG fragment.
- Cyclic: e-graph rebuild does **not** reach the coarsest partition (category error above);
  measuring it against O(n·k·log n) measures a wrong answer's speed. The only sound cyclic
  control is Valmari-Hopcroft; the prototype times *it*, and reports the e-graph route as
  "acyclic-only, N/A on cyclic" — not a slower-but-correct competitor.

---

## Q6 — Tier selection from (cardinality, literal-prefix, byte-rarity): table vs CSP

### The mechanism as it stands (all local, all monotone)

Feasibility is `derive_feasible_engines` (`info/mod.rs:344-412`): a straight-line, per-pattern
derivation of an 8-bit `EngineSet` bitmask (`info/mod.rs:61-105`: MEMCHR1/2/3, NIBBLE_LUT,
ONE_PASS, SMALL_DFA, DFA, FAMILY_HELPER) from **only per-pattern fields** — `negated_class`
cardinality (`128 - cs.len()` = exit-set size → memchr1/2/3 vs nibble-LUT, `:363-375`),
`literal_prefix`/`accel_candidate` (`:376-378`), `one_pass_eligible` (`:380`),
`dfa_size_estimate ≤ 64` (`:385` → SMALL_DFA), `classification` family membership (`:397-408`).
Own docstring: "No analysis cost beyond a few branch comparisons" (`:343`). Q6's three axes map
exactly: **cardinality** = negated-class exit count (`:363`); **literal-prefix** =
`extract_literal_prefix` (`info/literal_prefix.rs:8`, memcmp fast path `info/mod.rs:141`);
**byte-rarity** = `accel_candidate: Option<u8>` (`info/mod.rs:178`, "a single discriminating
byte that memchr can use") and, at match entry, PROVE-10's start-state entry-byte set
(`automata/prefilter.rs::from_dfa`, 1/2/3 → memchr tier). All three are **local functions of one
HIR**.

Final selection (pick ONE engine from the feasible set) is the bbnf-side CSP:
`crates/passes/src/decision_csp.rs` (278 LOC) — candidates shared via `Arc` (`:50`), constraints
`"recognizer"` (`:57`), `"simd"` (`:76`), `"capacity"` (`:79`), search bounded by `NODE_BUDGET`
(`:90,148`). Pipeline: `regex_facts->egraph_active_cost->csp->compile_codegen`
(`decision_csp.rs:166`); the e-graph HIR cost (`egraph/cost.rs`) feeds `egraph_active_cost`.

### The witness Q6 demands: does a global codegen/LUT-budget coupling exist?

**Searched, and it does not — the selection is LOCAL.** Every constraint in `decision_csp.rs`
reads only *this pattern's* candidate vector (`shared` = one pattern's candidates, `:50`); the
`NODE_BUDGET` (`:90`) bounds the CSP *solver's own search*, not a shared codegen artifact budget.
`derive_feasible_engines` reads no cross-pattern state. A per-site CSP whose variable domains and
constraints are all local is, by construction, **argmax over a cost-ranked feasible set** — a
table lookup with a tie-break. The CSP machinery is not *earning* its generality here; it is a
fixed-point/argmax dressed as constraint search.

**The CSP earns its keep iff a cross-pattern coupling is introduced, and exactly one is latent:**
the **generated-size budget** (a standing constitution law — generated code has a per-tranche
line/byte budget). NibbleLut accelerators embed two 16-byte tables *per accelerated state*
(`accel.rs:120` `build_nibble_luts`); SMALL_DFA embeds a full `num_states × num_classes`
transition table; FAMILY_HELPER reuses a shared kernel (zero marginal table). When a grammar has
K regex leaves each independently choosing its cheapest engine, the SUM of emitted tables can
exceed the codegen budget — and *then* the choice becomes a knapsack: demote some leaves from
SMALL_DFA/NIBBLE_LUT to a shared DFA/family-helper to fit, minimizing total match cost. **That is
a genuine global constraint (`Σ table_bytes(engine_i) ≤ BUDGET`, minimize `Σ match_cost_i`) — the
only thing that makes CSP-over-table non-vacuous.** It is ABSENT today (no cross-leaf term in
`decision_csp.rs`) but CONSTRUCTIBLE, and it is the concrete witness the family must build to
justify a CSP over a per-pattern table. Absent that term, **the honest verdict is: tier selection
is local; a cost-ranked table (feasibility bitmask + argmax) is the right substrate, and the CSP
is over-machined for the per-pattern case.**

---

## Prior art (citations)

- **Hopcroft 1971**, "An n log n algorithm for minimizing states in a finite automaton" — the
  worklist/smaller-half control the incumbent claims but is not.
- **Valmari 2012**, "Fast brief practical DFA minimization" (Inf. Proc. Lett.) — refinable-
  partition structure handling partial DFAs (DEAD transitions) without completion; port target
  for route 1.
- **Moore 1956** — the incumbent `dfa.rs:291` is Moore refinement; the `:358` restart is the
  pessimization.
- **Revuz 1992**, "Minimisation of acyclic deterministic automata in linear time"; **Daciuk,
  Mihov, Watson, Watson 2000** (incremental MADFA) — the acyclic linear route ≡ e-graph
  bottom-up congruence.
- **Bonchi & Pous 2013**, "Checking NFA equivalence with bisimulations up to congruence" (HKC) —
  the only bridge from congruence-closure machinery to bisimulation; a checker, not a builder;
  named, not promised.
- **regex-automata** (Gallant) — the memchr/rare-byte prefilter ladder PROVE-10 productionised
  (`prefilter.rs`); the Teddy multi-substring tier (SIMD shift-or) is the un-built 4-8-byte
  entry-set tier (PROVE-10 §5 caveat).
- **egg / egglog** (Willsey et al. 2021) — the e-graph substrate; its `rebuild()` is least-
  fixpoint congruence closure — the exact reason it cannot own cyclic minimization.

## Named risks & counterexamples

1. **Greatest-vs-least fixpoint (the killer).** e-graph congruence = least fixpoint; DFA
   minimization = greatest fixpoint. The e-graph is lawful for minimization ONLY on the acyclic
   fragment. Any design routing cyclic DFAs through the HIR e-graph's `rebuild` silently
   under-minimizes. Counterexample: `(ab)*` — the two loop states are bisimilar but never
   bottom-up-congruent.
2. **The restart trap (Q5).** Benchmarking the acyclic path against the incumbent restart-Moore
   (`dfa.rs:291`) is dishonest — it beats a strawman. The control MUST be correct Valmari-
   Hopcroft; against it, on n≤64 corpora, the acyclic path shows no separation. Ship the acyclic
   dispatch only if the grown corpus contains DFAs past the crossover.
3. **Small-DFA reality.** `SMALL_DFA ≤ 64 states` (`info/mod.rs:385`); most regex leaves are
   below it. Asymptotic minimization wins (linear vs n·log n) are invisible at this scale; the
   real minimization win is *deleting the O(n²) restart*, full stop.
4. **CSP vacuity (Q6).** Per-pattern CSP with local-only constraints = argmax; the CSP is
   unearned until the cross-leaf generated-size-budget term exists.
5. **Byte-class alphabet.** Any state-plane e-graph MUST key transitions on `num_classes`
   (`dfa.rs:54`), not 256 bytes, or it re-does what `compute_byte_classes` already did and
   inflates signatures.

## Exactly what a prototype must test to move this family

1. **Replace `dfa.rs:291-438` with correct Valmari-Hopcroft** (worklist, smaller-half,
   DEAD-aware). Assert byte-identical minimized DFAs vs the incumbent over `regex/tests/` (the 49
   dfa subtests) — pure win, no behavior change, removes O(n²).
2. **GROWN corpus manifest, ≥50 patterns, each tagged acyclic/cyclic** via `is_acyclic` DFS on
   the class-indexed table; include deliberately LARGE DFAs (unrolled `[0-9a-f]{4,8}`, deep
   literal alternations `(foo|bar|baz|...){N}`) to push past n=64. Report the acyclic/cyclic split
   and max/median state count.
3. **Three-way timing on that corpus:** (a) reference Valmari-Hopcroft, (b) `is_acyclic`→Revuz /
   else Hopcroft, (c) e-graph bottom-up congruence on the acyclic subset only. Cold, paired,
   same-binary (PROVE-10 protocol). Find the crossover n where (b) separates from (a). No crossover
   inside the corpus ⇒ record the negative result, ship only (a).
4. **Congruence-rebuild complexity** empirically: fit (a) to n·log n, Revuz to n·num_cls; confirm
   the log factor's presence/absence.
5. **Q6 witness:** implement the cross-leaf `Σ table_bytes ≤ BUDGET` knapsack in a scratch
   `decision_csp` variant over a K-leaf grammar; show a case where local argmax overflows the
   budget and the CSP demotes a leaf to fit — the ONLY evidence CSP beats table. If no such case is
   constructible on real grammars, declare tier-selection local and retire the CSP to a ranked
   table.
