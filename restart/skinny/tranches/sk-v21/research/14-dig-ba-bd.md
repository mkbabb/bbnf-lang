# Dig Shard 14 — BA / BB / BC / BD + PLAN-INPUT-2026-05-03: The Terminal Pre-Restart Planning Era

## Scope and headline finding

This shard excavates the four tranche directories `docs/tranches/{BA,BB,BC,BD}` and the
scope-allocation input `docs/tranches/PLAN-INPUT-2026-05-03.md`. These constitute the **terminal
pre-restart planning corpus**: a four-tranche, thirteen-lock, roughly fifty-wave edifice drafted
2026-05-03 that was never executed. Every commit touching these directories is a `docs(...)` commit;
the campaign shipped **zero lines of production code** before the project abandoned it wholesale for
the `restart/` topology.

The single most important fact: the BA-BD plan was the *response* to a diagnosis, and the diagnosis —
not the plan — is what survived. The `restart/corpora/` directory contains verbatim copies of the
BA-BD-era audit substrate (`CENSUS.md`, `MODULES.md`, `RESTART-SKETCH.md`, `SOTA.md`, timestamped
May 16), while the entire wave machinery, the thirteen locks, the carry-tag lattice, and the surgery
ledgers were dropped. The evidence was inherited; the plan was discarded.

## (a) What was attempted — the charters and waves

### The four-tranche arc (per `docs/tranches/PLAN-INPUT-2026-05-03.md:1-16`)

PLAN-INPUT is dated 2026-05-03 and addresses "the three Phase 2 plan agents (BA, BB, BC drafters)".
It is explicitly "scope allocation, gate specification, and carry-tag interface" — it forbids the
drafters from inventing scope (`PLAN-INPUT-2026-05-03.md:16`). The arc:

- **BA — Surgical Foundation** (`BA/BA.md:1-7`). Rebuild the substrate from first principles: excise
  grammar-specific leaks from generic crates, partition god modules per "Lock 13", land JSON
  direct-to-struct codegen, retire the `OpenFrame` stack + `Vec<OpenFrame>::clone` pathology. The BA
  flagship gate: `JsonParser::parse(twitter.json) ≤ 400 µs`, beating sonic-rs's 436 µs by ≥ 8%
  (`BA/BA.md:21`).
- **BB — Generality + Optimisation** (`BB/BB.md:1-9`). Generalise direct-to-struct across nine
  grammars; compress the five-grammar cohort (BNF/CSV/EBNF/CSS-Pretty/Math) from ~2000 LOC to ~250
  LOC via template emission; stand up the per-domain optimisers (CSP/e-graph/miners/cost-model) as
  path-dep crates; auto-detect Pratt + SIMD from grammar shape; land the sonic-class `pointer!` macro
  and lightningcss-class `Visitor`. CSS L4 to beat lightningcss's 4.16 ms bootstrap by ≥ 15% and
  43.37 ms tailwind by ≥ 18% (`BB/BB.md:19-20`).
- **BC — Backend ABI** (`BC/BC.md:1-7`). Formalise a backend-agnostic typed IR + per-backend lowerer;
  split `core` into `bbnf-parse`/`bbnf-codegen`/`bbnf-runtime`; land TS + WASM emitter *scaffolds*
  (production deferred); freeze and publish the sister crates. A 22-variant typed-IR alphabet
  (`BC/BC.md:149-166`).
- **BD — Multi-Backend Activation** (`BD/BD.md:1-11`). Activate TS + WASM to production, publish
  sister crates + path triplet to crates.io/npm, run a 9-grammar × 3-fixture × 3-backend parity
  matrix. "Hereupon BD has no successor… The plan ends here" (`BD/BD.md:9`, `BD/BD.md:178`).

### Wave inflation

BA was specified in PLAN-INPUT as a 7-wave tranche (`PLAN-INPUT-2026-05-03.md:64-72`, W0-W6). By the
final BA.md re-draft it had grown to **13 waves** (W0, W1, W2, W3a/b/c, W4a/b/c, W5a/b/c/d/e, W6 —
`BA/BA.md:43`). The driver is documented in `BA/audit/W5-substrate-identity-decision.md:8,26,84`: the
"option (a)" decision to honour "Lock 1" (retire `OpenFrame` across all nine grammars) *within* BA
rather than deferring it to BB. That single scope decision is stated to grow "the cumulative tranche
calendar… from 7-wave estimate to 13-wave", a "1.5-2x prior 7-wave estimate — accepted as the cost of
honouring Lock 1" (`W5-substrate-identity-decision.md:26`). The plan grew heavier as execution
receded.

The apparatus around the waves inflated in parallel: BA carries a **Phase-4 surgery ledger numbered
to 27** (`BA/BA.md:174-194`); BB references "surgery 25/30/35" and a lettered "HARDENING-PLAN"
lane-set (S04-x, C03-x, D08-x, G05-x, G06-x, BB02-x — `BB/BB.md:7-9,43-59`); BC carries a
sub-wave lattice W0a/b/c, W1a/b, W3a/b/c/d/e, W5a/b/c/d (`BC/BC.md:31-48`). Each tranche restates the
13-lock table, a per-wave generated-LOC budget table, a risks table, a sequencing-discipline table,
and a friction-forecast table. The four charters total ~860 lines of dense cross-referential prose
before a single wave was executed.

## (b) What shipped vs what died on the vine

**Nothing shipped.** `git log --oneline --all -- docs/tranches/{BA,BB,BC,BD}` returns only
documentation commits: `7c6cec96b docs(tranches/BA): redress`, `4f34144ba docs(tranches/BB):
phase-4 spec-depth re-draft`, `99556a9de docs(tranches/BC): phase-4 spec-depth re-draft`, `58e108ad0
docs(tranches/BD): phase-4 initial draft`, and the base `6098a4b52 docs(tranches): land BA/BB/BC
restart drafts (Phase 2)`. A grep of all commit subjects for `feat(BA|BB|BC|BD` or `BA.W|BB.W|BC.W|
BD.W` implementation tags returns matches only from the *predecessor* eras (AZ-IV, AW-V, cutover) —
never from a BA-BD wave. The BA-BD tranches have no `PROGRESS.md`, no `FINAL.md`, no wave-tagged
commits.

`era-VI-restart.md:44-46` (written earlier, 2026-04-22) already records the verdict for the first BA/
BB/BC scaffolds: "**Scaffold only** — `BA.md`, `PROGRESS.md`, `waves/`. No tranche-tagged commits.
**Not started.**" The May-3 re-draft rebuilt these scaffolds larger; they were still never started.

What genuinely shipped from this era is the **audit substrate itself** — the repo-root `audit/`
directory (tracked; 24 files dated 2026-05-03): `CENSUS-2026-05-03.md` (mechanical kill-list),
`MODULES-2026-05-03.md` (per-file fates + 17-step pipeline), `SOTA-2026-05-03.md` (competitor
anchors), `RESTART-SKETCH-2026-05-03.md` (JSON parse trace + post-restart sketch),
`HARDENING-SYNTHESIS-2026-05-03.md`, and eight `HARDENING-2026-05-03-0N-*` audit lanes plus a
matching eight `HARDENING-PLAN-2026-05-03-0N-*` lanes. This corpus is what fed the plan and what
outlived it.

### The restart inheritance

`restart/corpora/` contains `CENSUS.md`, `MODULES.md`, `RESTART-SKETCH.md`, `SOTA.md` — byte-for-byte
the same sizes as the `audit/` originals (e.g. CENSUS 47187 bytes in both), re-dated May 16. This is
the concrete proof that the restart **inherited the diagnostic corpus and dropped the prescription**.
The restart's own history begins at `b5eb4651c docs(restart): consolidate restart documentation` and
proceeds through PASS-A/B/C hardening and a MASTER-PLAN — a fresh clean-slate topology (the "skinny"
track, SK-V1…V21), not a resumption of BA-BD.

## (c) Root causes of divergence — with evidence

### Root cause 1: substrate-first / consumer-later (the Era V rut), never cured

The dominant failure mode of the whole pre-restart campaign is named in
`era-V-dta-psi-rut.md:7-10`: "every tranche ships the compile-time emission of constants, tables, and
shape dictionaries; no tranche fully activates the runtime consumer that reads them." Era V (AV
through AX, 2026-04-15→19) burned ~572 tranche-tagged commits across five days and closed with
**every bench entry below the AU baseline** — JSON twitter at 24.7% of baseline, CSS/Sheets/BBNF at
3-7% (`era-V-dta-psi-rut.md:5-6,177-186`). Seven distinct substrates (DTA, PSI, columnar tape,
ShapeRef, PHF/SIMD classifiers, Bloom/GADT dedup, shape-emitter) all shipped; none reached break-even
(`era-V-dta-psi-rut.md:30-46`).

The BA-BD May-3 audit **documented that this pattern had continued through AZ-I/II/III/IV** — the very
tranches that immediately preceded BA. The tranche-archaeology lane
(`audit/HARDENING-2026-05-03-06-tranche-archaeology.md`) walks BA's 24 hard gates and finds
**eighteen of twenty-four carry a "Failed and routed forward" verdict**, one "Never fully attempted",
several "Succeeded then regressed". Two gate families are named chronic:

- **Direct-projection (gates 5/8/9/13)**: "The failure was not lack of substrate; it was that the
  substrate was consumed through a runtime builder and arena, then deduped into templates."
  (`HARDENING-2026-05-03-06:` §A). `StructRegistry` and `project_types` landed repeatedly
  (`e0e0af30`, `43ea56bb`, `85cf83e7`); each left runtime `StructLayout`/`OpenFrame`/builder dispatch
  alive.
- **Lazy/SOTA-path (gates 4/7/10/11/12/14/15/16/17)**: "substrate-without-hot-path-consumer: eager
  parse remained the benchmarked value API, `LegacyPath` bridged typed paths back to runtime paths,
  `__path_plan` re-export wrappers survived, and `__EAGER_EMPTY_PATH<Json,_>` leaked a cross-grammar
  marker." (`HARDENING-2026-05-03-06:` §B).

The BA plan's answer was to *armor against its own predecessors' failure mode with more
gates and more sequencing discipline* — while replicating the same planning-heavy, execution-deferred
structure that produced the failure.

### Root cause 2: the defining measurement was diagnosed and never fixed pre-restart

The 4196× gap and the 86.07% pathology are the load-bearing motivation of the entire BA restart
(`PLAN-INPUT-2026-05-03.md:345`: "The 4196× gap is the BA-restart's defining motivation"). Per
`RESTART-SKETCH-2026-05-03.md:154-157,177,206`: on a 25,963-sample fat-LTO bench, `Vec<OpenFrame>::
clone` — the speculative parse checkpoint at `json/builder.rs:243/248` — consumed **86.07% inclusive
samples**; and `JsonDocument::get<T>(twitter.json)` ran **4196× slower** than sonic-rs's
`get_unchecked` because it eagerly parses and walks the materialized AST (`RESTART-SKETCH:39-40`). The
tranche-archaeology lane confirms this was profiled at DEEP-B (`d4b8de18`) but "**no code change
landed before BA**" (`HARDENING-2026-05-03-06:` gate 3). The single most important perf defect the
project ever identified was carried, unfixed, across every pre-restart tranche.

### Root cause 3: overfitting to JSON and per-grammar leakage

BA-G7 (`BA/BA.md:36`) is a census of grammar-specific leakage in the supposedly generic IR crate:
`crates/ir/src/registry/strategy.rs:130-185` hard-coded grammar arms, a `shape_dict_bbnf.rs`
recognizer keyed on BBNF, a `GrammarAuditTag` enum carrying grammar idents, CSS-L4 host functions
living in the core crate. The `__EAGER_EMPTY_PATH<Json,_>` marker
(`HARDENING-2026-05-03-06:` gate 7) is the sharpest overfitting artifact: a JSON-typed generic
parameter leaked into *every* grammar's generated module — "a dishonest Json marker in generated
modules". This is exactly the "hardcoded grammars, per-grammar backend files" divergence the restart
charter names as its founding grievance.

### Root cause 4: doc/code inversion and spec drift

The spec-drift lane (`audit/HARDENING-2026-05-03-03-spec-drift.md:20-292`) enumerates eight drift
findings (D1-D8) in the BA-BD plans *before execution even began*:
- **D3**: "BA.W0 and BC.W2 both claim closure of the same W6.2 worktree-fixture miss" — duplicate
  ownership.
- **D4**: "BB.W0 creates rank/tier skeletons with deferred W3 consumers, contradicting BB's
  substrate-with-consumer invariant" — the Era V anti-pattern reappearing inside the anti-Era-V plan.
- **D5/D6**: contradictory rewrite-storage location and split schema ownership between waves.
- **D8**: "GESTALT and codegen-paths still describe AZ-IV's two-entry parse path as current canon,
  while BA makes one parse path the audited mechanism" — the docs describe superseded reality while
  the plan assumes the new one. Doc/code inversion, verbatim.

### Root cause 5: gate dishonesty patterns

The archaeology names three recurring dishonesty modes the plan had to legislate against
(`HARDENING-2026-05-03-06:` chronic-risk items 3-4): (i) "Evidence gates repeatedly closed with named
misses" — perf gates closing on "existence, partial matrices, or environmental gating"; (ii)
"Deletion gates were often redirected into dedup or whitelisting" — AZ-IV.W5.4 made a "permanent
substrate audit" and W6.2 "routes 32 zero-caller substrates" rather than deleting them. The plan's
elaborate SOTA-anchoring discipline ("every parse-throughput gate cites a specific competitor +
dataset + platform per Lock 8", `BA/BA.md:15`) is a direct reaction to prior "AU baseline" and
"≥ pre-W3" self-referential gates.

## (d) Deferred / abandoned work worth resurrecting

### Positive designs

1. **The competitor-anchored SOTA gate table** (`SOTA-2026-05-03.md`, cited throughout). Concrete,
   dataset+platform-pinned targets on M1 Pro: sonic-rs twitter 436 µs, citm_catalog 854 µs, canada
   3.144 ms; lightningcss bootstrap 4.16 ms, tailwind 43.37 ms, animate 1.97 ms; the sonic-rs
   `get_unchecked ~0.1×` and `pointer!` surface; the lightningcss `Visitor<'i,T>` + `VisitTypes`
   bitflag. This is the cleanest honest benchmark scaffold the project ever assembled and should
   anchor SK-V21 gates directly.
2. **The checkpoint redesign** (`RESTART-SKETCH-2026-05-03.md:206`): replace `Vec<OpenFrame>::clone`
   with an O(1) tuple `Checkpoint = (stack_depth, arena_arrays, arena_objects, root_set)` where
   rollback *truncates* instead of *replaces*, and elide the checkpoint entirely on byte-disjoint
   Alts (`RESTART-SKETCH:233`). This is the concrete fix for the 86.07% pathology and was never
   executed.
3. **`Document::get<T>` routing directly through `parse_with(input, path)`** so narrow-path access
   never materializes the full AST — the sonic-class lazy `get` (`BA/BA.md:54`, gate BA-G9). The
   substrate (typed paths, `parse_with`, `path!` macro) all landed in AZ-IV; only the API collapse
   was missing.
4. **The generated-LOC census** (`PLAN-INPUT-2026-05-03.md:363-374`): 168,750 LOC across 9 generated
   files, with `css_l4.rs` at 107,138 LOC the dominant mass. A hard budget with per-wave windows is a
   sound discipline worth keeping (in lighter form).
5. **Substrate-with-consumer sequencing rule**: every wave's output must have a same-wave or next-wave
   consumer (`PLAN-INPUT-2026-05-03.md:265-321`). The rule is correct; the failure was that the plan
   asserted compliance in a table rather than enforcing it in code.

### Negative results that must not be re-learned

1. **Columnar / SoA tape LOSES to AoS + `sib_skip`.** Tranche Y split the tape into 7 structural
   Vecs; AY-I.W1 reverted to a single `Vec<TapeRec>` + parallel `sib_skip` because "the 7-column AoS
   lost to cache-locality of a single AoS record" (`era-VI-restart.md:59-64,76-81`). Called "the most
   important architectural reversal of Era VI" and "direct evidence that an Era IV 'durable' decision
   (columnar split) was not durable." Do not re-attempt columnar storage.
2. **The DTA/PSI table-driven interpreter is a dead end.** ~78,500 LOC of dispatch-automaton
   interpreter, deleted wholesale at AX.W0b (`era-V-dta-psi-rut.md:190-231,246`), never once beat the
   `fn __<rule>` recursive-descent baseline — "The DTA walker carries structural overhead the
   `fn __<rule>` path did not" (`era-V-dta-psi-rut.md:98-100`). The shape-emitter thesis
   ("auto-derive the sonic-rs-class inner loop from any BBNF grammar") was "demonstrated exactly once,
   on JSON, at W3 close… and lost by W6" (`era-V-dta-psi-rut.md:174-188`).
3. **The single hardest lesson**, verbatim (`era-V-dta-psi-rut.md:310-314`, AX.md proposition 4):
   "Novel levers compound only when they share a substrate AND a demonstrable floor. V's
   substrate-first-consumer-later anti-pattern must not recur."

## (e) Meta-process pathologies

1. **Planning became a substitute for execution.** The terminal artifact of a project trying to build
   a fast parser was four charters, an 8-lane hardening audit, an 8-lane hardening *plan*, a
   PLAN-INPUT scope-allocator, per-tranche surgery ledgers, and a carry-tag lattice — and no parser.
   The plan's own sophistication (13 locks, ~50 waves, 27+ surgeries) is inversely correlated with
   any code landing.

2. **Letter-recycling destroyed archaeological legibility.** The BA/BB/BC letters were reused across
   incompatible plans: `a028054ad docs(tranches): rename BA/BB/BC per tape-abrogation pivot`,
   `2f1589b2c docs(ba+bb): recycle BA letter for rule discovery; subsume BB`, then `9dde66ab1 chore
   (tranches): archive pre-restart BA/BB/BC; clean slate for re-draft`. The same letter names three
   different plans, violating the "new-tranche-new-doc" discipline and making the corpus hard to read
   even for its own authors. (The memory `feedback_new_tranche_new_doc` post-dates and codifies this
   lesson.)

3. **FINALs were written for waves that raised viability questions, then routed forward anyway.**
   AW-II closed "honestly on what landed… with the viability question raised for the successor
   tranche" (`era-V-dta-psi-rut.md:122-126`) — "the first hard signal that the DTA arc is not
   recovering the AU baseline. It is written into a FINAL and then routed forward for another 200+
   commits." Honest close-out prose became a mechanism for *continuing* a failing path rather than
   stopping it.

4. **The audit apparatus grew to police failures it could not prevent by policing.** The
   tranche-archaeology lane exists precisely because gates kept "closing on named misses" and
   "deletions redirected into whitelisting". Each new discipline (SOTA-anchoring, substrate-audit
   denominators, surgery ledgers) was a reaction to the prior discipline being gamed. This is the
   classic escalation of process where the process itself is the load the project cannot lift.

## How the restart decision was reached, and what it inherited vs dropped

The chain: Era V's DTA/PSI campaign (~600 commits) closed 0/17 above baseline and deleted its own
78K-LOC interpreter (AX.W0b). Era VI (Apr 20-22) was a *first* restart — a tranche-discipline reset
("Stop executing until the dev loop is truthful", `era-VI-restart.md:15-33`) — that produced the B0/
B1 infra prelude, the meta-audit, and the first (empty) BA/BB/BC scaffolds. Execution then continued
into AZ-IV/cutover, which the May-3 tranche-archaeology lane shows kept "failing and routing forward"
the same gates. On 2026-05-03 the project ran a full 8-lane hardening audit, distilled it into the
`audit/` substrate + `PLAN-INPUT`, and drafted the maximal BA-BD plan. **That plan was never
executed.** Instead the project performed the *actual* topological restart: `restart/` — a clean-slate
skinny→totality strategy — that copied the `audit/` corpus into `restart/corpora/` and began anew at
SK-V1.

**Inherited by the restart:** the diagnostic corpus (CENSUS/MODULES/SOTA/RESTART-SKETCH); the
`OpenFrame`-clone / 4196× diagnosis; the direct-to-struct thesis; the competitor-anchored SOTA gates;
the substrate-with-consumer principle; the "no columnar / no DTA" negative results; and the lock
*ideas* (which reappear folded into the skinny track's Lock 1…N).

**Dropped by the restart:** the four-tranche BA-BD wave structure; the 13-lock-as-numbered-contract
apparatus; the carry-tag lattice (C1'…C8, BA→BB→BC→BD); the surgery ledgers; the per-grammar
`OpenFrame` migration sequencing (W5a-e); the TS/WASM/publication tranche (BD) as a concrete plan; and
the recycled BA/BB/BC letters (the restart uses SK-Vn numbering with fresh docs per pass).

## Bottom line for the totality plan

The BA-BD era is the project's clearest object lesson that **a heavier plan is not a safer plan**. The
diagnosis was correct and durable; the prescription was a fifty-wave contract that its own drift-audit
found contradictory before a wave ran, and that never survived contact with a keyboard. SK-V21 should
harvest the concrete artifacts (SOTA table, checkpoint redesign, lazy-`get` collapse, negative
results) and consciously refuse the meta-structure (lock-as-numbered-contract, carry-tag lattices,
surgery ledgers) that turned planning into a terminal activity.
