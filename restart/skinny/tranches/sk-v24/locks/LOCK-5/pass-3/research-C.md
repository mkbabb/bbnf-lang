# LOCK-5 · Family 5C — AUTOMATA-AS-EXTRACTION (pass-3 research)

Determinization, minimization, and prefilter-tier selection recast as e-graph saturation
plus CSP extraction, measured head-to-head against the classical NFA→DFA→Hopcroft pipeline.
Wins stay; losses are named negative results. Every path/line is verified against the live
`parse-that/rust/regex` tree (the automata substrate this family owns; the vendored skinny copy
`skinny/crates/parse-that-regex/src` carries only `integration/ number/ unicode/ lib.rs` — no
`automata/`, no `egraph/` — so every automaton number re-runs in the reconciled tree of record).

The pass-3 obligation is sharper than pass-2's: the reference control is no longer the incumbent
but a **correct O(n·k·log n) Hopcroft**, and the decisive question is whether the acyclic fast
path survives once the incumbent's global-restart pathology is deleted for free.

---

## 0. The measurements already in hand (directional — wrong baseline, named)

A minimizer bake-off already ran on the 26-pattern DFA-compile corpus (`bench` profile, fat-LTO
cgu=1 O3, cold-paired N=500, interleaved, loadavg ≈7). It is EXACT and its verdicts hold — but it
measured against the **crippled incumbent**, so its ratios are the wrong-baseline directional
numbers pass-3 exists to correct:

| head-to-head | ratio vs **incumbent** | reading |
|---|---|---|
| worklist (correct-partition, DEAD-as-absorbing-block) whole corpus | **3.53× SLOWER** | alloc tax dominates at n≤16 |
| acyclic **congruence** fast path, acyclic subset (4 pats) | **0.80× (20% faster)** | the one shippable sliver |
| Brzozowski-derivative saturate-only (5 rules) | ρ_max **1.667**, organic ρ=1.5 | BLOCKED — under-minimizes |

Correctness was EXACT: 26/26 byte-identical minimized state counts and language-equivalence;
the acyclic congruence path matched on 4/4 acyclic patterns. Two facts from that run govern
everything below and are the crux pass-3 must resolve:

1. **The automata are tiny.** The full state census: every pattern minimizes to **2–16 states**
   (`\d+`=2 … `\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}`=16, the corpus maximum), over **k = num_classes
   ≤ ~10** byte-equivalence classes (`DfaState.transitions` is indexed by class, `dfa.rs:14`).
   At n≤16 the "O(n·k·log n) beats O(n²·k)" crossover **never activates** — the log-n worklist's
   predecessor-list / per-split HashMap / smaller-half bookkeeping is pure constant-factor loss.
   This is why the correct worklist ran 3.53× *slower*: asymptotic superiority is latent, and the
   incumbent's alloc-light array scan wins on constants at this scale.

2. **The incumbent's badness is one global restart, not its algorithmic class.** `dfa.rs:291
   hopcroft_minimize` is misnamed naive Moore; on any block split it executes `break; // restart
   refinement from the beginning` (`dfa.rs:358`) under an `if changed { break }` (`:362-364`),
   discarding all partition progress and re-scanning from block 0 — worst case O(n²·k·B). Deleting
   those ~3 lines (Null-A) restores proper Moore O(n²·k) with the incumbent's alloc-light constants
   intact.

---

## 1. Q5 — the reference control, and the acyclic path's reason to exist

**The bracket pass-3 mandates.** Four baselines, all measured against each OTHER, never against
HEAD (a crossover vs the crippled incumbent is disinformation):

- **Null-A — delete the restart.** Drop `dfa.rs:358` + `:362-364`; refinement completes a full
  pass over all blocks×classes before re-looping. ~3-line delta, O(n²·k), alloc-light. This is the
  honest general baseline.
- **Null-B — correct reference Hopcroft, O(n·k·log n).** Worklist with the process-the-smaller-half
  invariant (Hopcroft 1971). **Valmari 2012** ("Fast brief practical DFA minimization") is the
  clean reference implementation: partition refinement over *partial* DFAs, which handles the
  engine's DEAD-target trap natively (the corpus is full of partial transitions — every
  `transitions[class] == DEAD`), so no `usize::MAX` absorbing-block hack is needed.
- **Acyclic-congruence — Revuz.** `is_acyclic` (iterative DFS, self-loops = cycles) dispatches a
  DAG to one reverse-topological signature-bucketing pass: every target's representative is final
  before its source is hashed ⇒ the coarsest partition in a **single linear O(n·k) sweep, zero
  fixpoint rounds** (Revuz 1992).
- **e-graph congruence-closure — the general automata-as-extraction form.** Insert each DFA state
  as an e-node keyed on `sig(s) = (is_accept, λclass. rep(δ(s,class)))` over a **second `Language`**
  on the existing `EGraph`/union-find (`HirENode` already derives `egraph_derive::Language`,
  `node.rs:44`; the derive projects `Id`/`Box<[Id]>` children — a DFA-state node projects its
  `Box<[Id]>` transition row identically). `rebuild()` computes the coarsest congruence over the
  hash-cons table by construction (egg, Willsey 2021) — literally the minimal-DFA partition.

**Congruence-rebuild complexity, measured against the O(n·k·log n) claim (the pass-3 evidence
demand).** The e-graph rebuild worklist WITHOUT Hopcroft's smaller-half discipline runs O(n)
rounds → **O(n²·k)** — it degenerates to Null-A (Moore), *not* to Null-B. It reaches O(n·k·log n)
ONLY if the rebuild imports the smaller-half re-probe, which is re-implementing Hopcroft inside the
e-graph and inherits its alloc constants. The one exception is decisive: **on an acyclic DFA the
rebuild terminates in exactly ONE round** (reverse-topological order makes every child final before
its parent) — that is Revuz, O(n·k) linear, **strictly below Null-B's O(n·k·log n)** and with none
of the worklist alloc. So the e-graph congruence minimizer's honest asymptotic is *Revuz on the
acyclic fraction, Moore on the cyclic fraction* — it is the acyclic path in disguise, and its
general-case story is a named negative result, not a Hopcroft competitor.

**Does deleting the restart erase the acyclic path's reason to exist? — the predicted answer, with
mechanism.** The measured acyclic-congruence 0.80× (20% win) was taken against the **crippled
incumbent** on the acyclic subset. On acyclic tiny DFAs the incumbent's restart fires on nearly
every split (each of B blocks triggers a full re-scan) — so most of that 20% is the **restart
penalty, not a determinization/congruence advantage**. Against **Null-A** (proper Moore, alloc-light,
no restart) the acyclic subset already converges in ~diameter passes (≤16 states ⇒ ≤~4 passes) with
zero allocation, so the predicted crossover is **near-tie or Null-A wins** — i.e. **deleting the
restart erases most of the acyclic path's advantage**. Revuz's genuine residual edge is its
*single* pass vs Moore's diameter-many passes, which only pays when the acyclic fraction is large
AND diameters are non-trivial. The 26-corpus acyclic fraction is **4/26 = 15% (44/121 minimal
states)** — too small and too shallow to bank the path. The grown corpus decides whether that
fraction ever justifies the `is_acyclic` dispatch folded across ALL patterns.

**Corpus manifest (grow `COMPILE_PATTERNS`, currently 26 at `benches/regex_dfa_compile.rs:21`, to
≥50, stratified and tagged).** The crossover is entirely pattern-shape-dependent; a 26-pattern
corpus of 15% acyclic fraction cannot locate it. Tag each pattern:
- **Acyclic arm** (no back-edges, DAG): bounded reps `a{3,6}`, `[0-9a-fA-F]{4,8}`, `\d{4}-\d{2}-\d{2}`;
  literal alternations `(alpha|gamma|delta)`; anchored fixed strings; longer chains to raise diameter
  (`\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}` = the current 16-state max — add deeper ones so Revuz's
  single-pass edge over Moore's diameter-passes can actually show).
- **Cyclic arm** (self-loops): `\w+`, `[a-z]*`, `https?://[...]+`, `(?:foo|bar|baz)+` — where any
  acyclic fast path is dead weight and Null-A/Null-B fight it out.
- Report every minimizer number **against the tag**, never a blended mean; fold the `is_acyclic`
  DFS cost (O(n·k), one pass) across ALL patterns including cyclic ones (it fires and fails on every
  cyclic pattern — that overhead is charged to the dispatch, honestly).

**Predicted Q5 verdict (to be confirmed by the grown-corpus run, stated as a falsifiable negative
result).** (a) Null-A is the best *general* minimizer at this engine's automaton sizes — the
correct fix is delete-the-restart, not a worklist and not an e-graph; correct Hopcroft (Null-B)
loses on constants at n≤16 and its asymptotic win is unreachable here. (b) The acyclic-congruence
(Revuz) path wins its slice against a correct Hopcroft (O(n·k) < O(n·k·log n)) but its edge over
**Null-A** is near-tie unless the acyclic fraction and diameters are large — likely a *targeted*
dispatch, not a banked win. (c) The general e-graph congruence minimizer is a named negative result
on cyclic patterns (degenerates to Moore). (d) Derivative saturate-only stays BLOCKED (ρ_max 1.667;
`simplify_hir((ab)*a)` ≠ `simplify_hir(a(ba)*)` — the five `default_hir_rules`, `rules/mod.rs:64`,
provably cannot slide across a star; Kozen 1994 makes this a theorem, not a tuning gap).

---

## 2. Q6 — tier selection is LOCAL: the global-budget witness does not bind

The prefilter ladder (PROVE-10 landed a scratch-tree `Prefilter::from_dfa` reading the start-state
transition row, O(256), producing **9.6×–29×** match-throughput on prefilterable patterns with
**0.99× zero-regression** controls; NOTE — it is **not yet vendored into `parse-that/rust/regex`**:
`automata/` holds only `accel dfa mod nfa`, and only `info/literal_prefix.rs` — shufti/Teddy absent).
The tier vocabulary, cheapest→dearest: **memchr1 < memchr2 < memchr3** (no LUT) **< shufti**
(32-byte nibble LUT per site, `vqtbl1q_u8`) **< Teddy** (bucketed mask table, fed by
`info/literal_prefix.rs`) **< none**.

The entry-set → tier map is a **local** function of `(cardinality, literal-prefix presence, byte
rarity)` — no site's tier depends on another's. Q6 asks whether any *global* constraint couples the
sites into a genuine CSP. Two candidate witnesses, both refuted concretely:

- **Runtime L1-resident-LUT coupling — REFUTED.** shufti/Teddy LUTs are per-DFA data, and grammar
  regex leaves fire **one at a time** at different input positions (the generator dispatches to a
  single leaf per scan, not a tight loop over all N sites simultaneously). No shared hot LUT region,
  no simultaneous L1 pressure ⇒ no runtime coupling.
- **Codegen-size-budget coupling — REFUTED at realistic site counts.** The constitution's
  generated-size budget is a line/byte cap. shufti = 32 B/site, Teddy ≈ 256 B/site of `const` data —
  it counts, but choosing shufti at site A denies Teddy at site B only if total LUT bytes approach
  the cap, which needs *hundreds* of wide-tier sites; and even then the budget is soft (emitted
  size), not a hard L1 constraint that flips legality. No realistic grammar exhibits the
  reassignment where greedy per-site selection overflows and a global re-solve strictly helps.

**Verdict: tier selection is LOCAL — a pluggable cost-sorted table suffices.** Sort legal tiers per
site by expected cyc/byte (cardinality × corpus byte-frequency), pick the cheapest legal, done.
Determinism for bit-identical regen is by construction: fixed tier ordinal
`memchr1<memchr2<memchr3<shufti<teddy<none` as primary key, lexicographic entry-set bytes as
tie-break — no float/hash order. A CSP here builds a constraint system with **no binding
constraint**, violating the consumer-less-substrate law (Y.13). **Home the table in 5C** (it is a
property of the compiled DFA); escalate to **LOCK-2's scan-plan CSP** — with which this is
double-homed — ONLY when a prototype exhibits the global codegen/LUT-budget witness (a grammar
whose greedy per-site tiers overflow the stated budget and whose reassignment strictly wins). Until
then the CSP escalation is over-engineering. Any chooser must `select` explicitly on the entry-set
class — never a silent `_ => {}` fallthrough.

---

## 3. Substrate & integration seams (path:line, verified live)

| Concern | Where it lives now | 5C action |
|---|---|---|
| DFA type | `automata/dfa.rs:45` `Dfa` (`flat_transitions[state*num_classes+class]`, `byte_classes[256]`, `num_classes:u16`) | consume unchanged |
| DFA state | `automata/dfa.rs:14` `DfaState` (`transitions:Vec<StateId>` by class, `is_accept`) | e-node source for the congruence Language |
| Minimizer (crippled) | `automata/dfa.rs:291` `hopcroft_minimize`, restart `:358`, `if changed{break}` `:362-364` | **primary fix = delete restart (Null-A)**; then dir-module `automata/minimize/`: `moore.rs` (Null-A), `hopcroft.rs` (Valmari, Null-B), `acyclic.rs` (Revuz), `congruence.rs` (e-graph closure), `mod.rs` (`is_acyclic` dispatch) |
| Determinizer | `automata/dfa.rs:218` `subset_construction` | congruence closure over powerset e-nodes (same second Language) |
| Acyclic test | absent (`is_acyclic` = 0 hits) | new O(n·k) iterative DFS, self-loops=cycles, folded once across ALL patterns |
| e-graph engine | `EGraph<HirENode,NoAnalysis>` `egraph/mod.rs`; `HirENode` derives `Language` `egraph/node.rs:44` | add DFA-state `Language` on the same union-find/rebuild |
| Rewrite set | `default_hir_rules` (5 rules) `egraph/rules/mod.rs:64` — flatten/redundant/repetition/superset/union | the completeness ceiling; sound, incomplete for star (ρ_max 1.667) |
| Cost / extraction | `egraph/cost.rs` `RegexExtractionCost` | extraction picks the canonical minimal automaton |
| Prefilter (PROVE-10, un-vendored) | scratch `automata/prefilter.rs` `from_dfa` (start-row O(256)) | land as the LOCAL cost-sorted tier table |
| Literal-prefix (Teddy feed) | `info/literal_prefix.rs:8` `extract_literal_prefix` | wide-tier input; shufti/Teddy absent → future tier |
| Byte classes | `sets/equiv.rs` `compute_byte_classes` (`dfa.rs:80`) | k = alphabet granularity for every asymptotic |

Colocation precedent: `accel.rs`/`nfa.rs`/`dfa.rs` siblings under `automata/`; the minimizer is a
directory-module, not a flat file — no god module.

---

## 4. Prior art (citations)

- **Hopcroft 1971**, "An n log n algorithm for minimizing states" — Null-B, process-the-smaller-half.
- **Valmari 2012**, "Fast brief practical DFA minimization" — the clean reference Hopcroft over
  *partial* DFAs; handles the DEAD-target trap natively (no absorbing-block hack).
- **Moore 1956** — the naive partition refinement Null-A recovers; O(n²·k), alloc-light.
- **Revuz 1992**, "Minimisation of acyclic deterministic automata in linear time" — the O(n·k)
  single reverse-topological pass; the one genuine automata-as-extraction win on the acyclic fraction.
- **Brzozowski 1964** / **Antimirov 1996** — derivatives mod ACI are DFA states / partial
  derivatives are NFA states; the saturate-only determinizer.
- **Owens, Reppy, Turon 2009** (JFP), "Regular-expression derivatives re-examined" — smart
  constructors build the minimal DFA directly; the practical bound the 5-rule set falls short of.
- **Kozen 1994**, "A completeness theorem for Kleene algebras" — star equivalence needs the
  star-induction rule ⇒ **no finite ground rewrite set is complete** ⇒ ρ>1 is a theorem, not a gap.
- **Bonchi & Pous 2013** (POPL), "Checking NFA equivalence with bisimulations up to congruence" —
  the formal tie between minimization and congruence closure.
- **Willsey et al. 2021** (POPL), egg — `rebuild()` = congruence closure; the reused substrate.
- **regex-automata** (dense/sparse/hybrid-lazy DFA; memchr/Teddy/shufti ladder) — the classical
  pipeline the head-to-head measures against. **Teddy** (Hyperscan lineage) / **shufti** (Muła) —
  the wide prefilter tiers.

---

## 5. Named risks & counterexamples

1. **Acyclic path is a NULL once the restart is gone.** Predicted primary result: against Null-A
   the acyclic-congruence 20% edge is mostly the restart penalty, shrinking to near-tie at 15%
   fraction / shallow diameters. Report the fraction and the Null-A crossover whatever they are.
2. **e-graph congruence degenerates to Moore on cyclic patterns.** Without importing Hopcroft's
   smaller-half discipline the rebuild is O(n²·k), not O(n·k·log n) — a named negative for the
   general automata-as-extraction minimizer; the acyclic ONE-round case (Revuz) is its only win.
3. **Correct Hopcroft (Null-B) loses on constants at n≤16.** Its asymptotic superiority is
   unreachable at this engine's automaton sizes; the worklist alloc tax (measured 3.53× slower)
   is the constant that dominates.
4. **Derivative saturate-only stays BLOCKED.** ρ_max 1.667, organic miss ρ=1.5; theory-bounded by
   Kozen — carried as quality debt, backstopped by classical minimization, never "fixed."
5. **Tier CSP with no binding constraint (Y.13).** Building a CSP where the cost-sorted table
   suffices is over-engineering; escalate only on an exhibited global codegen/LUT-budget witness.
6. **Prefilter is un-vendored.** PROVE-10's `from_dfa` lives in a scratch copy; its 9.6×–29× band
   must be re-derived in the reconciled tree of record before it is banked.

---

## 6. What the prototype must test (decisive pass-3 experiments)

- **P1 — the four-baseline minimizer bake-off vs Null-A AND Null-B.** Implement Null-A
  (delete-restart Moore), Null-B (Valmari correct Hopcroft), Revuz acyclic-congruence, and the
  e-graph congruence minimizer. Grow `COMPILE_PATTERNS` to ≥50, tagged acyclic/cyclic per §1.
  Cold-paired, per-parse, N≥500, in the reconciled tree. Report, per tag: (a) HEAD→Null-A delta
  (proves the restart is the pathology); (b) **Revuz-vs-Null-A and Revuz-vs-Null-B** crossover as a
  function of acyclic fraction, `is_acyclic` DFS cost folded across ALL patterns; (c) the congruence
  rebuild **round-count instrumented** — must show ONE round on acyclic, O(n) rounds on cyclic,
  confirming it does NOT reach O(n·k·log n) generally. A tie is a tie; an acyclic NULL vs Null-A is
  the named negative result.
- **P2 — ρ and the completeness ceiling (confirm BLOCKED).** Re-run the derivative saturate-only
  determinizer over `HirENode`+derivative op on the grown corpus; differential-test every merge
  against the classical DFA (zero non-equivalent merges); exhibit `(ab)*a≡a(ba)*` unreachable by the
  5 rules; report minimal-states(egraph)−minimal-states(Hopcroft) per pattern (the quality debt).
- **P3 — tier: table or CSP.** Construct a grammar with N regex sites under a stated LUT/codegen
  budget; show whether greedy per-site cost-sorted tier selection **ever** overflows (→ CSP earns
  its keep, exhibit the reassignment) or never does at realistic N (→ table suffices); prove
  bit-identical regen via fixed tier-ordinal + lexicographic tie-break. Re-vendor PROVE-10's
  `from_dfa` and re-derive its throughput band in the tree of record.
- **Byte-identity gate.** Every minimizer's output transition-identical to Null-B on all patterns
  (state-count-identical up to canonical numbering); find/scan throughput re-derived from that
  automaton — no inherited PROVE-10 band.
