# LOCK-5 · Family 5C — AUTOMATA-AS-EXTRACTION (pass-2 research)

Determinization, minimization, and prefilter-tier selection recast as e-graph saturation
plus CSP extraction, measured head-to-head against the classical NFA→DFA→Hopcroft pipeline.
Wins stay; losses are named negative results. Every path/line below is verified against the
live trees; every number names the tree it was produced in.

Two live snapshots hold incompatible substrate. The automata substrate this family owns lives
only in the parse-that tree (`parse-that/rust/regex`, READ-ONLY) — the vendored skinny copy
(`skinny/crates/parse-that-regex/src`) carries only `integration/ number/ unicode/ lib.rs`, no
`automata/`, no `egraph/`. So every measurement here is a parse-that-tree measurement and must
be re-run in the reconciled tree of record once the vendor is completed.

---

## 1. The incumbent minimizer is misnamed — and its badness is a global restart, not its class

`parse-that/rust/regex/src/automata/dfa.rs:291` `fn hopcroft_minimize` is **not** Hopcroft. It
is naive Moore partition refinement crippled by a **global restart**: on any single block split
it executes `break; // restart refinement from the beginning` (`dfa.rs:358`) and the enclosing
`if changed { break }` (`dfa.rs:362-364`), discarding all partition progress and re-scanning
from block 0. Worst case is O(n²·k·B) with B = final block count (cubic-ish); each of the B
splits re-does the full O(n²·k) scan. Correct Hopcroft is O(n·k·log n) via the worklist
"process the smaller half" invariant (Hopcroft 1971). The initial partition (accept vs
non-accept, `dfa.rs:299-311`), the split test (`dfa.rs:326-360`), and the rep-remap
(`dfa.rs:368-437`) are otherwise standard.

**Price the null first (directive Q2).** The incumbent's pathology is almost entirely the
restart. Two honest baselines bracket every 5C claim:
- **Null-A (delete the restart):** drop `dfa.rs:358` + `362-364`, let refinement complete a
  full pass over all blocks×classes before re-looping — that alone collapses O(n³) to proper
  Moore O(n²·k). Cheap, ~2-line delta.
- **Null-B (correct reference Hopcroft):** a worklist implementation, O(n·k·log n).

Any minimizer that is not the restart-crippled incumbent will look spectacular against HEAD.
**The family's soundness obligation: the acyclic fast path and the e-graph minimizer are
measured against Null-B, never against HEAD.** A crossover measured against HEAD is
disinformation. The likely honest verdict (stated up front as a possible negative result): on
a corpus dominated by `*`/`+` self-loops, correct Hopcroft wins and any acyclic fast path is
dead weight; the fast path earns its keep only on the acyclic fraction.

---

## 2. The centerpiece — DFA minimization *is* congruence closure *is* one e-graph rebuild

The family charter's "Hopcroft as rewrite-to-normal-form" is literal, not metaphor. Partition
refinement computes the **coarsest congruence** on states under the signature
`sig(s) = (s.is_accept, λclass. rep(δ(s,class)))`. An e-graph's `rebuild()` computes exactly
the coarsest congruence over its hash-cons table by construction (egg / Willsey et al., POPL
2021; the rebuild worklist *is* congruence closure). So:

> Insert each DFA state as an e-node keyed on `sig(s)`; equal signatures hash-cons-merge; the
> merge changes `rep(·)`, which re-dirties parents, which the rebuild worklist re-probes to
> fixpoint. The fixpoint partition is the minimal DFA.

This is the AUTOMATA-AS-EXTRACTION thesis made concrete and it unifies three things the
incumbent keeps separate:
- **Determinization** = hash-cons on the NFA-state-**set** signature (subset construction is
  congruence closure over `powerset` e-nodes; `dfa.rs:218` `subset_construction` is the
  hand-rolled form).
- **Minimization** = hash-cons on the transition signature (§above).
- **The acyclic special case** = when the DFA is a DAG, reverse-topological processing needs
  **zero fixpoint iterations** — every child's rep is final before its parent is hashed. That
  is exactly Revuz's linear-time acyclic minimizer (Revuz 1992) falling out as the terminating
  branch of the same rebuild loop.

The reused substrate already exists: `HirENode` (`regex/src/egraph/node.rs`) derives
`egraph_derive::Language`; the graph is `EGraph<HirENode, NoAnalysis>` (`egraph/mod.rs`);
saturation runs through `CspScheduler` (`crates/egraph/src/csp_scheduler.rs:175`, AC-3 dirty
closure per iteration) or `BackoffScheduler` (`crates/egraph/src/scheduler.rs:51`); extraction
by `RegexExtractionCost` (`egraph/cost.rs`). A minimizer-as-congruence-closure adds a **second**
`Language` — states, not HIR nodes — over the same `EGraph`/`Scheduler`/union-find machinery.
No new engine; a new node type on the existing one.

**Critical asymptotic risk (named).** Naive congruence iteration is O(n) rounds → O(n²·k),
i.e. it degenerates to Moore-minus-restart (Null-A), not to Hopcroft. To match Null-B the
rebuild must carry the Hopcroft worklist discipline (re-probe only the smaller side of each
split). The prototype must prove the e-graph minimizer hits O(n·k·log n), not merely "beats the
crippled incumbent."

---

## 3. Derivative saturation as determinizer + minimizer, and its completeness ceiling (Q3)

Over the **same derived `Language`** as the HIR rewrite theory, add a Brzozowski/Antimirov
derivative operator:
- **Antimirov partial derivatives** (Antimirov 1996) yield NFA states directly, avoiding the
  ACI-normalization blowup — the determinizer.
- **Brzozowski derivatives quotiented by the rewrite congruence** (Brzozowski 1964; Owens–
  Reppy–Turon, JFP 2009, "re-examined") yield DFA states directly; states = derivative classes
  under the smart-constructor laws. With strong enough smart constructors this builds the
  **minimal** DFA in one construction — determinizer and minimizer fused.

**The completeness ceiling — the decisive negative result.** `default_hir_rules`
(`regex/src/egraph/rules/mod.rs`) is exactly five: `FlattenAltConcat`, `DeduplicateAlternation`,
`SupersetAbsorbClass`, `UnionMergeClass`, `AbsorbRepetition`. That set is the **ACUI +
charclass-lattice + bound-arithmetic** fragment (associative-commutative-unit-idempotent Alt,
plus class union/superset, plus repetition-bound merge). It is **sound** (never merges
non-equivalent forms) but **incomplete** for regular-language equivalence. Concrete
counterexamples the five rules provably cannot reach — exhibit these in the prototype:
- `(ab)*a` ≡ `a(ba)*` (star-shift / sliding) — language-equal, no rule slides across the star.
- `(a|b)*` ≡ `(a*b*)*` (star-of-union vs union-of-stars) — language-equal, unreachable.

These are not bugs to fix. Kozen 1994 (completeness of Kleene algebra) proves the star
equational theory requires the **star-induction inference rule**, which is *not* a finite
ground rewrite — no finite e-graph rule set can decide star equivalence. Therefore:

> Derivative saturation is a legitimate **determinizer** and a **heuristic minimizer** whose
> quality is bounded by the ACUI+lattice fragment. It over-approximates the minimal DFA state
> count; classical Hopcroft on the determinized DFA is the complete backstop that closes the
> `(ab)*a≡a(ba)*` class. The lawful architecture is: derivative-saturation determinize →
> classical Hopcroft (or the §2 congruence-closure) minimize. The e-graph is not the decision
> procedure; the automaton is.

**Saturation ratio ρ.** Define ρ = merged-derivative-classes / distinct-syntactic-derivatives
across the corpus. ρ<1 exactly on the star-interleaving equivalences above. ρ measures how far
the smart-constructor set closes the gap to the minimal DFA; the residual (minimal-states(egraph)
− minimal-states(Hopcroft)) is the named quality debt.

**The gate/corpus mismatch — RE-CUT, measured.** The "50+ patterns" figure attaches to the
classify/HIR corpora, verified: `regex_classify` bench = **57** patterns, `regex_hir_egraph`
bench = **62**. The DFA-minimization bench that is *this family's home*,
`regex/benches/regex_dfa_compile.rs` `COMPILE_PATTERNS`, holds **26** (verified count). The
acyclic-vs-Hopcroft crossover is entirely pattern-shape-dependent, so a 26-pattern corpus of
unknown acyclic fraction cannot locate it. **Verdict: GROW `COMPILE_PATTERNS` to ≥50,
stratified and tagged** — an acyclic arm (bounded reps `a{3,6}`, literal alternations
`(alpha|gamma|delta)`, anchored fixed strings; no back-edges) and a cyclic arm (`\w+`, `[a-z]*`,
`https?://[...]+`; self-loops). Report every minimizer number against the tag, not a blended
mean. Do **not** import the 57/62 number into the DFA axis.

---

## 4. Prefilter/tier selection — table suffices per site; CSP only on a global budget (Q4)

The current tier logic is per-site and local: `Prefilter::from_dfa` reads the start-state
transition row (O(256)) and picks Byte1/2/3→memchr / else None — a hardcoded ladder that
reproduced **8.8–29.5×** on prefilterable patterns with **0.97–1.01×** controls, cold paired
N=200, in the parse-that tree. The wide tier (shufti via `vqtbl1q_u8`; Teddy fed by
`info/literal_prefix.rs`) extends the ladder for entry sets > memchr3.

The entry-set → tier map is a **local** function of `(cardinality, literal-prefix presence,
byte rarity)`. There is **no cross-site coupling** — hence a **pluggable cost-sorted table
suffices**: sort the legal tiers per site by expected cyc/byte (cardinality × corpus byte
frequency), pick the cheapest legal, done. It is deterministic for bit-identical regen by
construction: fixed tier ordinal `memchr1 < memchr2 < memchr3 < shufti < teddy < none` as the
primary key, lexicographic entry-set bytes as the tie-break.

Tier selection becomes a **genuine CSP only if a global constraint couples the sites** — the
witness would be a shared **codegen-size or L1-resident-LUT budget**: shufti needs a 32-byte
nibble LUT per site, Teddy a bucketed mask table; if the generated artifact caps total LUT
bytes (or the emitted-size budget the constitution imposes), then choosing shufti at site A
denies budget to site B → the per-site choices are coupled → CSP. Absent such a budget, a CSP
is over-engineering and violates the consumer-less-substrate law (Y.13): a CSP with no binding
constraint.

**This is the double-homed interlock the cross-lock notes flag** (prefilter/tier CSP double-homed
with LOCK-2's scan-plan CSP; a single home must be decided). 5C's verdict: home the *table* in
5C (it is a property of the compiled DFA), and escalate to the CSP home only when a prototype
exhibits the global-budget witness. The `_ => {}` wildcard concern from the sibling formulation
applies here too — any tier chooser must `select` explicitly on the entry-set class, never
silently fall through.

---

## 5. Substrate & integration seams (path:line)

| Concern | Where it lives now | 5C action |
|---|---|---|
| DFA type | `regex/src/automata/dfa.rs:45` `Dfa` (`byte_classes[256]`, flat_transitions) | consume unchanged |
| Minimizer (crippled) | `regex/src/automata/dfa.rs:291` `hopcroft_minimize`, restart `:358` | replace; new `automata/minimize/` dir-module: `hopcroft.rs` (correct worklist), `acyclic.rs` (Revuz), `congruence.rs` (e-graph closure), `mod.rs` (`is_acyclic` dispatch) |
| Determinizer | `regex/src/automata/dfa.rs:218` `subset_construction` | mirror as congruence closure over powerset e-nodes |
| Acyclic test | absent (`grep is_acyclic` = 0 hits) | new O(n·k) DFS, folded once, cost amortized across all patterns |
| e-graph engine | `EGraph<HirENode,NoAnalysis>` `egraph/mod.rs`; `CspScheduler` `crates/egraph/src/csp_scheduler.rs:175`; `BackoffScheduler` `scheduler.rs:51` | add a second `Language` (DFA states) on the same engine |
| Derived Language | `HirENode` derives `egraph_derive::Language` `egraph/node.rs` | reuse the derive for the derivative operator |
| Rewrite set | `default_hir_rules` (5 rules) `egraph/rules/mod.rs` | the completeness-ceiling set; sound, incomplete for star |
| Cost / extraction | `RegexExtractionCost` `egraph/cost.rs` | extraction picks the canonical minimal automaton |
| Prefilter ladder | `Prefilter::from_dfa` (start-row read, O(256)) | keep as the local cost-sorted table |
| One-pass predicate | `regex/src/info/one_pass.rs:38` `check_one_pass_eligible` | soundness reference for the derivative determinizer |

The colocation is the `accel.rs`/`nfa.rs`/`dfa.rs` sibling precedent under `automata/`; the new
minimizer is a directory-module, not a flat file — no god module.

---

## 6. Prior art (citations)

- **Hopcroft 1971**, "An n log n algorithm for minimizing states in a finite automaton" — the
  correct reference control (Null-B); process-the-smaller-half.
- **Moore 1956** — the naive partition refinement the incumbent approximates (badly).
- **Revuz 1992**, "Minimisation of acyclic deterministic automata in linear time" — the O(n)
  acyclic fast path (reverse-topological signature bucketing).
- **Valmari 2012**, "Fast brief practical DFA minimization" — partition refinement over partial
  DFAs; a clean reference implementation for the control.
- **Brzozowski 1964**, "Derivatives of Regular Expressions" — derivatives mod ACI are finite and
  are the DFA states.
- **Antimirov 1996**, "Partial derivatives…" — partial derivatives are the NFA states, no ACI
  blowup.
- **Owens, Reppy, Turon 2009** (JFP), "Regular-expression derivatives re-examined" — smart
  constructors build the minimal DFA directly; the practical determinizer+minimizer.
- **Kozen 1994**, "A completeness theorem for Kleene algebras…" — proves star equivalence needs
  the star-induction rule → **no finite ground rewrite set is complete** → the completeness
  ceiling is a theorem, not a tuning gap.
- **Bonchi & Pous 2013** (POPL), "Checking NFA equivalence with bisimulations up to congruence"
  — congruence-closure for automata; the formal tie between minimization and the e-graph.
- **Willsey et al. 2021** (POPL), egg — e-graph `rebuild()` = congruence closure; the substrate
  the §2 minimizer reuses.
- **regex-automata** (dense/sparse DFA, hybrid lazy DFA, memchr/Teddy/shufti prefilter ladder) —
  the classical-pipeline control the head-to-head measures against.
- **Teddy** (Hyperscan lineage, ICPP'21) / **shufti** (Muła) — the multi-byte prefilter tiers.

---

## 7. Named risks & counterexamples

1. **Acyclic fast path is a NULL on the real corpus.** If the engine corpus is self-loop-heavy
   (`\w+`, `[a-z]*`, `https?://[...]+` are all cyclic), `is_acyclic` is pure overhead. Must
   measure the acyclic *fraction* before claiming the path; report the fraction whatever it is.
2. **Congruence-closure minimizer degenerates to Moore.** Without the Hopcroft smaller-half
   worklist inside the rebuild, it is O(n²·k) — beats crippled HEAD, ties Null-A, loses to
   Null-B. The prototype must prove O(n·k·log n).
3. **Derivative determinization blows up.** Weak smart constructors → exponentially many
   equivalent-but-distinct derivatives → state explosion; ρ quantifies it. The 5-rule set may be
   too weak; measure ρ, don't assume.
4. **Completeness-ceiling quality gap.** `(ab)*a≡a(ba)*`, `(a|b)*≡(a*b*)*` leave the e-graph
   minimizer non-minimal vs Hopcroft. This is a theorem (Kozen), carried as a named quality debt,
   backstopped by classical Hopcroft — not "fixed."
5. **Tier CSP with no binding constraint.** Building a CSP where a cost-sorted table suffices
   violates Y.13. Escalate to CSP only on an exhibited global codegen/LUT-budget witness.

---

## 8. What the prototype must test (decisive experiments)

- **P1 — the minimizer head-to-head.** Implement Null-A (delete-restart), Null-B (correct
  Hopcroft worklist), Revuz acyclic, and the e-graph congruence-closure minimizer. Grow
  `COMPILE_PATTERNS` to ≥50, tagged acyclic/cyclic. Cold paired, per-parse, N≥200, in the
  reconciled tree. Report: (a) HEAD → Null-A delta (proves the restart is the pathology),
  (b) Revuz-vs-Null-B crossover as a function of acyclic fraction with the `is_acyclic` dispatch
  cost folded across ALL patterns, (c) whether the congruence minimizer hits O(n·k·log n) or
  degenerates. A tie is reported as a tie; an acyclic NULL on the cyclic corpus is reported as a
  negative result.
- **P2 — ρ and the completeness ceiling.** Build the derivative-saturation determinizer over
  `HirENode` + derivative op; measure ρ across the grown corpus; differential-test every merge
  against the classical DFA (soundness: zero non-equivalent merges); exhibit `(ab)*a≡a(ba)*`
  unreachable by the 5 rules; report minimal-states(egraph) − minimal-states(Hopcroft) per
  pattern (the quality debt).
- **P3 — tier: table or CSP.** Construct a grammar with N regex sites under a stated LUT/codegen
  budget; show whether greedy per-site cost-sorted tier selection ever overflows (→ CSP earns
  its keep, exhibit the reassignment) or never does (→ table suffices); prove bit-identical regen
  via the fixed tier-ordinal + lexicographic tie-break.
- **Byte-identity gate.** The congruence minimizer's output must be transition-identical to
  Null-B Hopcroft on every equivalence the rewrite set reaches (state-count-identical up to
  canonical numbering), and its find/scan throughput re-derived from that automaton — no
  inherited PROVE-10 band.
