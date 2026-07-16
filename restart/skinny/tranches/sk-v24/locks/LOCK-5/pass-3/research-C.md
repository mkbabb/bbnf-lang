# LOCK-5 · Family 5C — AUTOMATA-AS-EXTRACTION (pass-3 research-C)

Determinization, minimization, and prefilter-tier selection recast as congruence closure +
extraction, measured head-to-head against the classical pipeline. Wins stay; losses are named
negative results. This report answers Q5 (minimization-as-congruence vs a *correct* Hopcroft over a
grown acyclic/cyclic corpus) and Q6 (tier selection: table vs CSP, coupling-witness or none).

Engine home: `parse-that/rust/regex` (`bbnf-regex`, 6742 LOC, READ-ONLY). Shared substrate:
`crates/egraph` (HIR e-graph, congruence closure) + `crates/csp-solver`. All numbers are
prototype obligations, not inherited bands (constitution vii).

---

## 0. The load-bearing defect (governs Q5)

`parse-that/rust/regex/src/automata/dfa.rs:291` `fn hopcroft_minimize` **is not Hopcroft.** It is
Moore partition refinement with a full **restart on every split**: `dfa.rs:358` `break; // restart
refinement from the beginning`, hoisted out again at `:362-364`. Each split rescans all `num_blocks`
blocks × `num_classes` × members from scratch. Worst case is O(n²·k) per split × O(n) splits =
**~O(n³·k)** — no worklist, no "process the smaller half", no predecessor lists. The header comment
(`dfa.rs:1,4,290`) claims Hopcroft; the body is a naive restart-refiner.

This is the crux the charter forces open: **any earlier "acyclic fast path beats Hopcroft" claim was
measured against this restart-refiner, not against O(n·k·log n).** Q5 cannot be adjudicated until a
correct control exists. The restart-refiner is the incumbent the charter tells us NOT to measure
against.

---

## 1. Mechanism — minimization as congruence, one engine, two schedules

**Core identity.** Two DFA states are equivalent iff they are congruent: same accept bit and, for
every byte-class, targets that are themselves congruent. That is exactly the fixpoint an e-graph
computes by congruence closure. Encode a DFA-state e-node isomorphic to the existing `HirENode`
pattern (`parse-that/rust/regex/src/egraph/node.rs:45`, children projected to `egraph::Id`):

```
DfaSig { accept: bool, targets: Box<[Id]> }   // len == num_classes; DEAD → a reserved dead Id
```

Hash-cons over `(accept, targets)` under union-find compaction merges congruent states. This is the
*single mechanism*. It admits two schedules, and the acyclicity dispatch is a scheduling decision on
the **same** engine — not an orthogonal codepath (MEMORY: no-orthogonal-codepaths):

- **Acyclic (Revuz single pass).** When the DFA carries no back-edge, process states in
  reverse-topological order. Targets are already canonical when a state is visited, so **one**
  congruence pass suffices — O(n·k) with a radix/lexicographic sort of signatures, no iteration.
  This is precisely a single e-graph rebuild with zero re-canonicalization loop (the acyclic-ægraph
  shape, Cranelift). Revuz, *Minimization of acyclic deterministic automata in linear time*, TCS
  1992.
- **Cyclic (iterated congruence / saturation).** With back-edges, iterate signature-refinement to a
  fixpoint (the LLVM-style `Changed` bool, MEMORY: changed-flag-convergence). This is Moore
  minimization expressed as saturation; worst case **O(n²·k)** (a chain automaton needs O(n)
  passes) — the SAME class as the naive refiner, so the e-graph earns nothing here vs a proper
  Hopcroft.

**Verdict shape (to be measured, not asserted).** The congruence framing is honest for the *acyclic*
half (one rebuild, genuinely linear) and a *loss* for the cyclic half (iterated hashcons is O(n²·k),
worse than O(n·k·log n)). Therefore the lawful design is: **dispatch on acyclicity; congruence for
acyclic, a correct Hopcroft/Valmari for cyclic.** The real, defensible win over the incumbent is
*deleting the restart* — not the acyclic special-case per se. Q5's sharpest form: once the restart
is deleted (correct control in place), does the acyclic path still earn its detection cost?

---

## 2. The reference control the charter demands (Q5)

Build a correct O(n·k·log n) control with **partial-DFA handling** — the incumbent's `DEAD`
transitions (`dfa.rs:330,340,423`) make this a *partial* automaton, and textbook Hopcroft
under-minimizes partial DFAs. Two named controls:

- **Hopcroft (1971) with predecessor lists + "process the smaller half"** worklist of `(block,
  class)` splitter pairs. Requires inverting `flat_transitions` (`dfa.rs:50`) into per-class
  predecessor lists — the data the incumbent never builds.
- **Valmari–Lehtinen (2008), *Bisimilarity Minimization in O(m log n) Time*** — the true SOTA
  control: handles partial/DEAD transitions natively (refinable-partition structure over both states
  and transitions), so it needs no dead-state padding. This is the honest reference for this engine's
  partial DFAs; Hopcroft-classic would need explicit dead-completion first.

Correctness oracle across all three (incumbent-restart, Revuz-acyclic, Valmari): the minimal DFA is
canonical, so **all must emit byte-identical state counts + transition tables** on every corpus
pattern. A divergence is a bug in one of them, caught before any timing is trusted.

---

## 3. The grown corpus (Q5 manifest)

≥50 patterns, tagged acyclic/cyclic, extending the existing regex benches
(`parse-that/rust/regex/benches/regex_dfa_compile.rs`, ~50 patterns per R3 §6). Tags derive from a
DFA back-edge test (a DFS marking SCCs; a self-loop on any live class ⇒ cyclic):

- **Acyclic (bounded, DAG-shaped):** `[0-9a-fA-F]{3,6}`, `#[0-9a-fA-F]{6}`, literal alternations
  `(alpha|gamma|delta|kappa)`, fixed-width dates `\d{4}-\d{2}-\d{2}`, bounded IPv4 octets. These
  arise from grammar leaves with bounded repetition — the common case (`info/dfa_size.rs:32` already
  distinguishes bounded-unroll from star).
- **Cyclic (star/plus/unbounded):** `[a-z]+`, `\w+`, `[^"\\]*`, `https?://[a-zA-Z0-9./-]+`,
  `.*?literal`, whitespace-with-block-comment. Back-edges from the Kleene ε-loop
  (`info/dfa_size.rs:30`).

Per pattern record: state count in/out (correctness), NFA/DFA sizes, minimize-time for all three
controls, and — for the congruence variant — **congruence-rebuild complexity measured**
(passes-to-fixpoint, hashcons ops) against the O(n·k·log n) claim. `num_classes` is typically ≤8 for
grammar leaves (`dfa.rs:54` `num_classes: u16`, byte-equiv-compressed via `sets/equiv.rs`
`compute_byte_classes`), so signatures are `[StateId; ≤8]` — tiny, radix-sortable.

---

## 4. Tier selection — table vs CSP (Q6)

**Inputs are already computed.** `(cardinality, literal-prefix, byte-rarity)`:

- *Cardinality* = start-state entry-set size, read O(256) from the start row by
  `Prefilter::from_dfa` (PROVE-10, `automata/prefilter.rs`; `REPORT.md:23`). ≤3 → memchr1/2/3;
  4..N → shufti; multi-byte literal prefix → Teddy (PT-1 W2 plan).
- *Literal-prefix* = `info/literal_prefix.rs:8` `extract_literal_prefix` (fixed-byte prefix/suffix).
- *Byte-rarity* = **the missing input.** No frequency oracle exists in-tree; regex-automata ships a
  static ASCII-rank table (BurntSushi's `rank` heuristic). Without it, "rare byte" selection is
  guesswork — a named gap for the prototype.

**The feasibility substrate is a monotone TABLE, not a CSP.** `info/mod.rs:61` `EngineSet` (u16
bitset: MEMCHR1/2/3, NIBBLE_LUT, ONE_PASS, SMALL_DFA, DFA, FAMILY_HELPER) is derived by
`info/mod.rs:344` `derive_feasible_engines` — "no analysis cost beyond a few branch comparisons"
(`:343`), a pure per-pattern function of the already-computed fields. The recognizer CSP
(`crates/ir/src/passes/csp_domains.rs`, `csp_strategy/mod.rs` 1361 LOC, `decision_csp.rs`
`CostFiniteDomain`) consumes `EngineSet` as the *domain* of a per-pattern `RegexEngine` variable.

**Coupling-witness search — none found (the Q6 negative result).** A grep for any global
codegen/LUT-budget coupling (`lut_budget`, `codegen_budget`, `shared_lut`, `table_budget`,
`generated_loc` used as a *tier* constraint) over `crates/ir/src/` returns **ZERO**. Each pattern's
tier is derived from its *own* start-state row and *own* literal prefix — `from_dfa` is O(256)
**local**; `derive_feasible_engines` reads only per-pattern fields. There is no shared resource over
which two patterns' tier choices compete. **Therefore tier selection is LOCAL, and the monotone
table wins; CSP over it is unearned overhead** (MEMORY: kiss-perf-bias, extreme-parsimony).

**The exact condition that would flip it to CSP (stated so it can be tested, not hand-waved).** A
global coupling appears only if the generator imposes a *bounded shared budget* the tiers consume:
e.g. a per-generated-parser cap on distinct **shufti nibble-LUTs** (32 bytes each) or **Teddy
buckets**, so choosing shufti for pattern A denies it to pattern B. That budget does not exist today
(the only global budget, `generated_loc_budget`, bounds emission *size*, not tier *legality* — it is
a GN-2/generator concern, orthogonal to automata-tier choice). CSP earns its keep **iff** such a
shared-LUT cap is introduced; absent it, the table is the answer and the CSP is a consumer-less
substrate (constitution viii). The prototype constructs the synthetic cap to exhibit the crossover.

---

## 5. Substrate — integration seams (path:line)

- Minimize site: `dfa.rs:89-91` (`if opts.minimize && dfa.states.len() > 1 { dfa =
  hopcroft_minimize(&dfa) }`) — the single swap point for the congruence engine + correct control.
- Partial-DFA data: `dfa.rs:330,340,423` (`DEAD` sentinel handling) — where Valmari's
  partition-over-transitions replaces the `usize::MAX` block hack.
- Byte-equiv classes (already done): `dfa.rs:80` `compute_byte_classes` (`sets/equiv.rs`), keeps
  `num_classes` ≤ ~8 → signature width tiny.
- E-graph reuse: `crates/egraph` + `egraph-derive::Language` (`egraph/node.rs:36,44`) — `DfaSig`
  e-node derives `children()`/congruence for free, isomorphic to `HirENode`. Extraction-cost
  precedent: `egraph/cost.rs` `RegexExtractionCost` (a `CostModel`).
- Acyclicity dispatch: new `info` predicate beside `info/dfa_size.rs` (an SCC/back-edge DFS on the
  DFA, or lifted from HIR: unbounded `Repetition` reaching itself ⇒ cyclic).
- Tier table: `info/mod.rs:344` `derive_feasible_engines` (extend for byte-rarity + shufti/Teddy
  bits). CSP consumer: `crates/ir/src/passes/csp_domains.rs`, `csp_strategy/mod.rs`,
  `decision_csp.rs`.
- Prefilter tier (extraction target): PROVE-10 `automata/prefilter.rs` + `Dfa::find` (`dfa.rs`),
  PT-1 W2/W3 (shufti/Teddy/lazy).

---

## 6. Prior art (citations)

- **Revuz 1992**, *Minimization of acyclic deterministic automata in linear time*, TCS 92(1) — the
  acyclic single-pass congruence; O(n) for the acyclic half.
- **Hopcroft 1971**, *An n log n algorithm for minimizing states in a finite automaton* — the
  correct control; predecessor lists + smaller-half. **Knuutila 2001**, *Re-describing an algorithm
  by Hopcroft*, TCS 250 — the implementation subtleties the incumbent skipped.
- **Valmari–Lehtinen 2008**, *Bisimilarity minimization in O(m log n) time* — SOTA reference that
  handles **partial** automata natively (matches this engine's DEAD transitions); the honest cyclic
  control.
- **Bonchi–Pous 2013**, *Checking NFA equivalence with bisimulations up to congruence*, POPL —
  congruence as the automata-equivalence primitive; the theoretical backing for minimization-as-
  congruence.
- **Brzozowski 1962** (double-reversal + determinization yields the minimal DFA directly) — a
  "minimization as extraction" control worth timing: exponential worst case, often competitive,
  reframes minimize as reverse-determinize.
- **egg / egglog** (Willsey et al., POPL 2021) — congruence closure = equality saturation; the
  `crates/egraph` substrate. **Cranelift ægraphs** — the acyclic-e-graph one-pass shape.
- **regex-automata** (BurntSushi) — `ByteClasses` (mirrors `sets/equiv.rs`), DFA minimization, the
  memchr/rare-byte prefilter ladder + the static ASCII-rank table (the byte-rarity oracle gap).

---

## 7. Named risks + counterexamples

- **Cyclic congruence is a loss.** Iterated hashcons is O(n²·k) (chain automaton ⇒ O(n) passes) —
  worse than O(n·k·log n). *Counterexample:* a long `a?a?a?…a{n}` chain. The e-graph must NOT be sold
  as the cyclic minimizer; it is honest only for the acyclic schedule. Report this as a negative
  result, not a footnote.
- **Acyclicity-detection cost cancels the win.** The back-edge DFS is itself O(n·k). If it equals the
  minimize cost, the dispatch saves nothing. *Counterexample:* an acyclic-but-large DFA where
  detect+Revuz ≈ Valmari-direct.
- **Partial-DFA under-minimization.** Classic Hopcroft on the DEAD-sentinel DFA under-merges.
  *Counterexample:* two states differing only in *which* class goes DEAD — the incumbent's
  `usize::MAX` block collapses them incorrectly under a naive port. Valmari or explicit
  dead-completion required; the byte-identical oracle catches it.
- **Byte-rarity guesswork.** Without a frequency oracle the tier may pick a common entry byte (e.g.
  `e`) as accel — legal but useless (PROVE-10 §2a: `h` in prose wins less than absent `0`/`#`). The
  CSP/table objective must weight by rarity; that needs the rank table.
- **CSP without a coupling is theater.** If no shared-LUT budget exists, a CSP over per-pattern tiers
  is a consumer-less substrate (Y.13). *Counterexample the prototype must fail to build:* two
  patterns whose tier choices actually contend for a shared resource in today's codegen — it does not
  exist; that absence IS the finding.

---

## 8. What a prototype must test to move this family

1. **The three-way minimize bake-off.** Implement (a) correct Valmari (partial-DFA), (b) Revuz
   acyclic single-pass, (c) the `DfaSig` congruence engine on `crates/egraph`, alongside the
   incumbent restart-refiner. Over the ≥50 acyclic/cyclic corpus: assert **byte-identical minimal
   DFAs** across all four (correctness), then time minimize per pattern. **Answers Q5:** does Revuz
   beat correct Valmari on the acyclic tag, and does the incumbent restart account for any prior
   "acyclic wins" claim? Report passes-to-fixpoint for the congruence engine vs the O(n·k·log n)
   claim.
2. **Does the e-graph pay for itself?** Measure `DfaSig` congruence rebuild vs direct
   partition-refinement — the honest negative result if the e-graph indirection loses (it likely does
   on cyclic; likely ties Revuz on acyclic). Keep only the winning schedule per tag.
3. **Q6 coupling probe.** Instrument the corpus for any global LUT/codegen-budget contention across
   patterns (expect none — confirm the negative result), then synthetically introduce a per-parser
   shufti-LUT byte cap and show the recognizer CSP (`decision_csp.rs`) beats the monotone table
   **only** under that cap. Absent the cap, the table wins — name it as law.
4. **Byte-rarity oracle.** Port a static ASCII-rank table; show tier selection improves accel-byte
   choice on the PROVE-10 skip-heavy corpus (the `h`-vs-`0` spread).
