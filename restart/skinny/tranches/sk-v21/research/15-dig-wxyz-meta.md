# Dig Shard 15 — W / X / Y / Z + archive/ + meta-audit/

Scope of this excavation: the four single-letter tranche plan docs
`docs/tranches/{W,X,Y,Z}/*.md`, the pre-restart archive bundle
`docs/tranches/archive/pre-restart-{BA,BB,BC}/`, and the corpus self-audit
`docs/tranches/meta-audit/` (eight numbered assays plus the five-era archaeology
under `meta-audit/archaeology/`). The self-audit is the earliest formal
retrospective the project produced; its conclusions on process pathology are the
prior art SK-V21 must not re-derive. Evidence is cited as `file:line` or git sha.

---

## 0. Executive summary

Two distinct bodies of work carry these directory names, and they must be read
separately:

1. **The W→X→Y→Z arc is a coherent, git-confirmed, EXECUTED CSP-optimizer-
   activation continuum** (V built the substrate; W/X/Y/Z activated it). It is the
   single most *disciplined* body of work in the dig — it invented the exact
   anti-ghost-substrate machinery (consumer-invariant test, freezing guards,
   truth-based profiling attribution) that the rest of the corpus lacked. It is a
   resurrection goldmine, not a cautionary tale. Tagged commits run 2026-04-09
   (`f86be0045` post-W) through 2026-04-10 (`6eeac0cc1` post-Z).

2. **The meta-audit is the corpus self-audit dated 2026-04-22**: eight assays
   (session-friction, instruction-adherence, tranche-drift, toolchain-pain,
   validation, commit-archaeology, appurtenant-assay, abrogation-catalog) plus a
   five-era archaeology. It already diagnosed the dominant failure mode of the
   whole campaign — **substrate-first-consumer-later** — and named the toolchain
   wall, the orchestration friction, and five gaps it never closed. SK-V21 should
   treat its findings as settled and skip re-deriving them.

The earliest project intent (era-II archaeology): *grammar is a standalone input
language; the Rust compiler is one backend among several; grammar authorship and
codegen are decoupled by a canonical interner-backed IR from which every backend
emits* (`archaeology/era-II-foundations.md:27-47`). That thesis — IR as single
source of truth — is the one invariant that survived every era and was re-anchored
every time it drifted. SK-V21's re-founding is faithful to it.

A cross-cutting naming pathology: the meta-audit's own era-IV/era-VI archaeology
attributes tranches **Y and Z to tape-column / cursor work**
(`archaeology/era-IV-tape-first.md:38`, `era-VI-restart.md:77-82`), while the
git-tagged `Tranche Y.-1 … Y.13` and `Tranche Z.0 … Z.7` commits are the CSP
ghost-elimination and sonic-parity arc described in the plan docs. The letters
collided (or the self-audit mis-reconstructed) — a concrete instance of the
letter-recycling disease the corpus repeatedly suffered (`archive/pre-restart-BA/
historical/BA-pre-recycle.md:1` — "LETTER RECYCLED"; `archive/pre-restart-BA/
BA.md:4` — "un-recycled at master 40092b28"). **SK-V21 must adopt a monotonic,
collision-free tranche identity scheme.**

---

## 1. What the W→X→Y→Z arc attempted (charters and waves)

Every one of the four plan docs opens with the identical carried edict —
verbatim, word for word — quoted as "Original edict (carried forward)"
(`W/W.md:5-7`, `X/X.md:3-9`): *"Devise a path forward… NO quick solutions, NO
workarounds… NO legacy code… Are all of our optimizations (CSP, egraph,
structural pattern matching) properly wired in and activating?"* The repetition of
the same 200-word edict across four tranche openings is itself a process artifact
(the "devise a path forward ceremony" the meta-audit flagged as Pattern 5,
`meta-audit/01-session-friction.md:114`).

The arc's architectural spine:

- **V (referenced, not in this shard)** built the substrate: csp-solver
  `ImplicationConstraint` + `CardinalityConstraint`; `RegexClass` variants;
  `NodeFacts.recognizer`; the `mine_recognizers` pass with eight miners;
  `backend/kernels/` directory; `docs/benchmarks/post-V.json`. V's own honest
  self-assessment: *"spent without gain — the substrate exists but no emission
  consumer reads `ir.recognizer_decisions`"* (`W/W.md:450`). **V is the archetype
  of substrate-first-consumer-later at the tranche granularity.**

- **W — Activation, Hot-Path Demolition, Three Distinct Cliffs**
  (`W/W.md`). Thesis: *"Tranche V was a substrate; Tranche W is its activation,
  demolition, and hot-path elimination"* (`W/W.md:19`). Six phases: (0)
  `factor_literal_prefixes` rewrite, (1) grammar-tier e-graph lifecycle, (2) HIR
  e-graph caching, (3) activate the optimizer layers, (4) hot-path clone
  elimination, (5) parse-time SIMD/`&[u8]`/perfect-hash. Eleven architectural
  commitments as gates (`W/W.md:106-116`).

- **X — Activation Truth, Type-Pass Demolition, Cost-Sharing Gestalt**
  (`X/X.md`). Post-W profiling surfaced two new cliffs hidden behind W's demolished
  91.73% cliff: `find_node_id_for_var` linear scan (23.13% self on css_l4,
  `X/X.md` §1) and per-compile HIR re-saturation. X is "the full activation tranche
  for the W substrate plus compile-cliff demolition." 23 architectural commitments.

- **Y — Activation Truth II: Load-Bearing Emission, Cross-Rule Optimization,
  Parse-Time SIMD** (`Y/Y.md`). Opens with a brutal honest ledger: *"Of the bench
  gates X set, only one landed: `compile_css_l4 −30.1%`. Every parse-time gate
  missed, most as regressions (`json_canada −3.9%`, `css_tailwind −5.6%`,
  `css_bootstrap −7.4%`)"* (`Y/Y.md:5`). Y's job: make every X decision load-bearing,
  install freezing guards, delete ghost substrate. 17 hard commitments.

- **Z — Sonic-rs Parity, Universal Cost Model, Gestalt Compile Wins**
  (`Z/Z.md`). Opens rebased on ground truth because *"several of the draft's numbers
  were inherited from earlier conversations and no longer matched source"*
  (`Z/Z.md:7`). Eight phases toward sonic-rs parity: single-walk recognizer miner,
  explicit SIMD intrinsics, single-variant enum inlining, structural bitmap pre-pass,
  checkpoint reduction, Ref/Repeat CSP lift, universal cost knobs.

---

## 2. What actually shipped vs what died on the vine

Git confirms the arc executed end to end (subjects grepped from
`git log --all`):

- **W landed**: `19ad65af1 feat(kernels): wire 6/8 family modules through
  scanner_plan + dispatch (W.3d/W.5d)`, `38ab0e10a fix(rust): manual Clone+Copy
  impls (W.5b)`, `f86be0045 docs(bench): post-W baseline`. Note **6/8** kernels, not
  8/8 — two family modules never found a production consumer (the seed of Y.4's
  match-or-delete).

- **X landed** phases 0-6 and 9-12 (`f86cb88be docs(bench): post-X baseline
  (Tranche X phases 0-6, 9-12 committed)`), including `48403b4bc perf(types): O(1)
  var→node reverse map (X.0)` — the one gate Y confirms actually landed
  (`compile_css_l4 −30.1%`). **X.6 global-CSP batching was attempted and reverted**:
  `5e329edf9 perf(strategy): compile-scoped CSP batching (X.6)` immediately followed
  by `ae05b4698 revert(strategy): per-rule CSP solves (Tranche X.6 batching pulled)`.
  X.6 hung `compile_css_l4` at 94ms — a 10× blowup — *"because branch-and-bound
  explored the cross-product with no guard"* (`Y/Y.md:13`). This is the load-bearing
  negative result of the arc (see §5).

- **Y landed** the full ghost-substrate purge: `1260466fc feat(ir,bench): Tranche
  Y.-1 freezing guards`, `b0721ca7f Tranche Y.2 — delete SharedHelper ghost
  substrate`, `c8d9eab8f Tranche Y.4 — delete ghost family recognizers`, `6b4ee642e
  Y.3 — fold AltMode::TokenDispatch into ByteDispatch`, `9e85eeb35 Y.1 — delete
  backend/patterns/`, `3539b5ee6 test(bbnf): Y.13 — consumer-invariant enforcement`,
  `debcbdb0d docs(bench): post-Y baseline`. Z's context confirms the outcome:
  *"~830 lines of dead infrastructure deleted, AltMode 5→3, WrapMode 5→4, every
  surviving variant verified by the Y.13 consumer-invariant test"* (`Z/Z.md:5`).
  **The two zero-match kernel families W shipped were deleted, not kept as
  "future substrate."** Y's gates still missed: *"Compile-time gates missed by
  2.3–9.6%; parse-time gates by 0.6–10.6%"* (`Z/Z.md:5`).

- **Z landed** the sonic-parity arc: `ed0b02e60 single-walk recognizer mining
  (Z.0)`, `5692ffd52 delete CallStrategy::InlineFusion ghost (Z.5)`, `d03e5654d
  delete WrapMode::DelimScan ghost (Z.5b)`, `6d584972d consolidate strategy cost
  knobs (Z.6)`, `6eeac0cc1 post-Z baseline (Z.7)`. Z continued deleting ghost
  variants (`InlineFusion`, `DelimScan`) that earlier tranches had introduced —
  the ghost-substrate treadmill never fully stops.

Died on the vine within the arc: X.6 global CSP (reverted → replaced by Y.5
connected-components); the SharedHelper backend emission was planned in X (X.9c/d)
but *not shipped* until Y.2; JSON authoritative `RegexEngine` end-to-end
(X.11a) dissolved and had to be re-declared as an explicit Y.6b deliverable
because it "must not dissolve into cost-feedback side effects" (`Y/Y.md:233-237`).

**The recurring pattern across all four: ambitious multi-percent bench gates,
most missed, a handful of large compile-time wins, parse-time repeatedly
regressing before recovering.** SK-V21 should calibrate its own gate promises
against this: the arc's biggest real win was a single O(1)-reverse-map change
(`X.0`, −30%), and its biggest real cliff was one per-call hashmap rebuild
(`W` Phase 0, 91.73%). Structural fixes dwarfed the sweeping CSP theatre.

---

## 3. Root causes of divergence (with file:line evidence)

The charter frames the pre-restart era as ending in *"total divergence: hardcoded
grammars, per-grammar backend files, spec drift."* The dig locates each:

### 3a. Substrate-first-consumer-later — the master pathology

Named explicitly and repeatedly. Era V's signature: *"every tranche ships the
compile-time emission of constants, tables, and shape dictionaries; no tranche
fully activates the runtime consumer that reads them"* (`archaeology/
era-V-dta-psi-rut.md:8-10`). Seven substrate pieces (DTA, PSI, columnar tape,
ShapeRef, PHF+SIMD, Bloom+GADT, Shape-emitter) all shipped; *"None reach break-
even parse throughput with the AU baseline before Era V ends"* (`era-V:45-46`). At
Era V close every bench was **below** the AU baseline — JSON twitter 486 MB/s =
24.7% of baseline; CSS/Sheets/BBNF at 3-7% (`era-V:180-186`). The cost: ~600
tranche-tagged commits across five days (`era-V:302-308`). AX proposition 4
crystallized the lesson: *"Novel levers compound only when they share a substrate
AND a demonstrable floor. V's substrate-first-consumer-later anti-pattern must not
recur"* (`era-V:310-314`). The W→X→Y→Z arc is the *tranche-granular* version of the
same disease (V builds, W/X/Y/Z chase the consumer) — and is the corpus's only
worked example of curing it, via the Y.13 consumer-invariant test.

### 3b. JSON overfitting / hardcoded per-grammar backends

- AW-V's thesis "auto-derive the sonic-rs-class inner loop from any BBNF grammar"
  was *"demonstrated exactly once, on JSON, at W3 close (`c1e86ab3`), and lost by
  W6"* (`era-V:174-188`). The generality claim held for one grammar for a few
  commits.
- The `has_w4_classified` gate at `crates/core/src/backend/rust/emitter/
  grammar.rs:718` *"over-restricts JSON's visitor-path"* — CSS/Sheets/BBNF fell
  back to the DTA walker (`era-V:181-185`).
- **The literal "hardcoded grammars, per-grammar backend files":** AX.W1
  landed hand-coded `bbnf::json::Value` / `bbnf::css::StyleSheet`, deleted at W1r.0:
  `3429aaba Revert W1.A/W1.B (−6,128 LOC); sonic-rs runtime → dev-dep`
  (`era-V:277-279`). Over six thousand lines of per-grammar hand-coding, reverted.

### 3c. Two decision surfaces for one semantic (orthogonal codepaths)

The `EmissionTier` axis (`MustTape`/`MustFn`/`MayInline`) was an independent axis
layered over `PayloadKind → TypeDesc`; deleted wholesale at AQ.5 (`2f7c1bd4
refactor(ir,codegen): delete structural dispatch`, `era-IV-tape-first.md:60-76`),
subsuming ~two weeks of AF/AG/AH/AI/AJ/AK tier work. This is the
`no-orthogonal-codepaths` memory's origin. The same disease recurs in the W-Z arc:
`CallStrategy::InlineFusion` was *"decided via @token + fuse_token_dispatch and
coalesced post-hoc — a ghost decision point exactly of the kind Y.13 was built to
prevent"* (`Z/Z.md:260`), deleted at Z.5.

### 3d. "Durable" decisions that were not durable

Era IV's Tranche-Y 7-column tape split (per the archaeology's attribution) was
reverted in AY-I.W1: *"columns reverted from 7 structural Vecs to 1 `Vec<TapeRec>`
+ parallel `sib_skip`… the 7-column AoS lost to cache-locality of a single AoS
record… direct evidence that an Era IV 'durable' decision was not durable"*
(`era-VI-restart.md:63-82`). SK-V21 lesson: columnar SoA is not a free win; cache
locality of a single record can dominate.

### 3e. CSP "optimization" theatre

Pre-W, the entire cost-optimization side of csp-solver was **dead API**: *"There
is no cost-driven optimization in the entire workspace… `OptimizationMode::
MinimizeCost`, `solve_optimized()`, `CostDomain` — the entire optimization side of
csp-solver — is dead API"* (`W/W.md:72`). Files named "csp" did no solving:
`csp_recognizers.rs` had *"zero references to `csp_solver::` — a pattern-match
cascade misnamed csp"* (`W/W.md:13, :65`). `CostWeights` was *"shared by type
only"* — both tiers called `::default()` independently, *"the 'shared substrate'
rhetoric in CLAUDE.md is structural fiction"* (`W/W.md:76`). Backend strategy
solvers were priority cascades ignoring `ir.recognizer_decisions` (`W/W.md:66-70`).
The gap between the CLAUDE.md narrative and the code was total. **This is the
doc/code inversion the charter warns of, at its purest.** W was the first
production `MinimizeCost` invocation in project history (`W/W.md:106, gate at :326`
— "csp_solver::Csp::solve invocation count: ≥1, currently 0").

### 3f. The toolchain wall (measured)

`meta-audit/04-toolchain-pain.md` measured it directly on-host:
`cargo check -p bbnf-bootstrap --lib` cold = **>130 s, single-core, single-rustc,
inside one derive expansion** (`04:20-27, :32-40`); target-lock contention
serializes parallel `-p A` + `-p B` checks (`04:76`); `scripts/bootstrap-bbnf.sh:28`
deletes `target/.bbnf-cache/` **every run** so every bootstrap pays full cost
(`04:141-156`); incremental-cache ICE from `bbnf-analysis` (`04:157`). The single
26 GB-RSS `derive(Parser)` aggregate link is the close-proof surface routinely
(mis)used for iteration (`02:15-47`). This is the build wall the charter's
Rust-stack-abrogation directive targets — it is real, measured, and unresolved at
corpus close.

---

## 4. Meta-process pathologies the self-audit already concluded

`meta-audit/01-session-friction.md` mined the five most recent long sessions
(1078 Bash calls, 147 Agent dispatches). SK-V21 should treat these as settled:

- **Pattern 1 — Bash-tail polling instead of Monitor**: 362 `tail`-log
  invocations vs 40 Monitor/ScheduleWakeup, 29 of those in the single most recent
  session (`01:36-56`). (Origin of the `no-polling-loops`, `bg-then-monitor`
  memories.)
- **Pattern 3 — zombie/runaway sub-agents the orchestrator does not notice**
  (`01:75`). (Origin of `reconcile-task-census`.)
- **Pattern 4 — orchestrator hand-patches `generated.rs`**, violating the
  clean-regen invariant (`01:98`). (Origin of `generated-size-budget`,
  `clean-regen-discipline`.)
- **Pattern 5 — "devise a path forward" ceremony / tranche-scope churn**
  (`01:114`) — the exact edict repeated verbatim atop W/X/Y/Z.
- **Pattern 6 — cargo build/check runtime catastrophe** (the B0 regression,
  `01:134`).
- **Pattern 9 — mid-tranche scope pivot without a new tranche letter**
  (`01:193`). (Origin of `new-tranche-new-doc`.)
- **Pattern 10 — context-overflow guardrails learned, then lost** (`01:207`).

`meta-audit/02-instruction-adherence.md` found edicts violated in practice:
heavy-surface `cargo check -p bbnf --tests` run for routine iteration without
`--profile ax-iter` (29 bare invocations verified, `05:34-47`); large-file Read
without prior `wc -l` (`02:105`); concurrent cargo on shared `target/` lock
(`02:90`). These minted the `iter-profile-always`, `read-size-preflight`,
`single-cargo-per-target` memories.

`meta-audit/03-tranche-drift.md` is a catalog of plan↔PROGRESS↔wave-spec drift:
wave-spec `W0p.md` omitting every landed d-sub-phase (`03:24`), scope creep of
W0'.d4-d7 into B0's territory (`03:151`), undocumented deferrals (d2 named
nowhere, `03:205`), cross-tranche debt incoherence (`03:249`). It is cited **live**
as the meta-cause for the chronic-deferral pattern (`meta-audit/ARCHIVE.md:3-4`).

`meta-audit/05-validation.md` verified 01-04 and then named **five gaps the
self-audit itself never closed** (`05:252-341`) — these are directly in SK-V21's
mandate and should be treated as inherited open work:
- **G1** — no first-principles SOTA toolchain investigation.
- **G2** — no ~2000-commit retrospective synthesis (the era archaeology partially
  answered this).
- **G3** — no BA/BB/BC competitor-parity synthesis.
- **G4** — no instruction-layer streamlining pass.
- **G5** — no orchestrator dispatch-cost analysis.

`meta-audit/07-appurtenant-assay.md` and `08-abrogation-catalog.md` are the
modernization substrate: a 16-repo posture record written *"for the fleet
modernization tranche that will abrogate `bencher`/`criterion`, pin the rust
toolchain, adopt `divan` + `nextest`"* (`07:4-6`); and a 63-item KEEP/MODERNIZE/
REPLACE/ABROGATE/FOLD classification of every script/Makefile/CI/inline hack, net
−1,420 LOC of ad-hoc tooling flagged for removal (`08:1-40`). **These two files are
a ready-made input to SK-V21's Rust-stack-abrogation census** — the abrogation
targets are already enumerated with named upstream replacements.

---

## 5. Worth resurrecting for the totality plan (algorithms, designs, negatives)

The W→X→Y→Z arc is the corpus's most reusable body of optimizer work. Resurrect:

**Algorithms / designs:**

1. **`factor_literal_prefixes` as `FactorCtx` with a once-built dedup map**
   (`W/W.md:124-153`). The single largest compile win in the dig (7-10× on
   compile_css_l4). The dedup map is built once per pass and threaded as
   `&FxHashMap<&[u8], u32>` keyed against `&[String]` — never cloned per call;
   `IrNode` clones replaced with `std::mem::replace`/`strip_leading_take`. Keep the
   trie recursion; fix only the map and clone behavior.

2. **The consumer-invariant test (Y.13)** (`Y/Y.md:326-333`, `Z/Z.md:119`). An
   exhaustive-match test over every `AltMode`/`WrapMode`/`RegexEngine` variant that
   fails to compile until each variant has an observable consumer (cargo-expand or
   bench evidence). This is the *structural antidote to substrate-first-consumer-
   later.* SK-V21 should make an equivalent a standing gate on every emitted
   decision variant — no variant lands without its consumer in the same commit.

3. **Freezing guards (Y.-1)** (`Y/Y.md:19-71`): a `SolveConfig::node_budget`
   (default 1M) on csp-solver's backtracking/branch-and-bound with a structured
   `BudgetExceeded` fallback to a trivial per-variable pick; bench wall-clock
   guards; nextest `slow-timeout`/`terminate-after`. Born from the X.6 94ms freeze.
   **These belong in the csp-solver coordination the charter calls out** — check
   whether the re-vendored `crates/csp-solver` still carries the budget.

4. **Connected-components CSP decomposition (Y.5)** (`Y/Y.md:207-223`): union-find
   partitions strategy variables by `ImplicationConstraint` edges; per-component
   `MinimizeCost` solve; zero-cross-constraint components degenerate to trivial
   picks. This is the *correct* shape that X.6's naive cross-product blowup should
   have been.

5. **Single-walk recognizer miner (Z.0)** (`Z/Z.md:138-174`): collapse 9
   independent DAG descents into one orchestrator walk over a `RecognizerMiner`
   trait (`inspect(node, id, ctx, out)`). Note the deliberate exclusion: CSP
   constraint-generation stays a separate walk (different computation model,
   facts→CSP boundary).

6. **Structural bitmap pre-pass (Z.3b)** (`Z/Z.md:223-240`): the *grammar-agnostic*
   generalization of sonic-rs structural indexing — `RecognizerShape::
   StructuralBitmap { bytes }` detected when a hot Alt's FIRST sets are all subsets
   of ≤8 single-byte literals above a density threshold. Explicitly "zero overfit;
   CSS selectors, SQL keywords, EBNF token grammars would opt in automatically."
   This is the SOTA-parity technique done *generically* — exactly SK-V21's mandate.

7. **Universal cost model** (`Z/Z.md:299-308`, `W/W.md:196-215`): `CostConfig`
   with per-grammar tunables via `BBNF_COST_*` env; lift `StrategyConfig` to the
   shared `egraph` substrate so grammar-tier and HIR-tier read one struct. The
   design is sound; the earlier "shared by type only" implementation was the fiction
   (§3e) — resurrect the design, not the fiction.

8. **Truth-based profiling methodology (Z)** (`Z/Z.md:30-106`): the exact
   samply/dSYM recipe — build with `debug=true`, resolve newest binary via
   `ls -t | head -1` (never glob), verify UUID match via `xcrun dwarfdump --uuid`
   before recording, presymbolicate, run parse benches from `crates/core` for cwd-
   relative data. And the discipline: *every "+X%" claim must cite a samply profile
   line (symbol + self-time delta), not just a bench delta.*

**Interpreter/deletion discipline (from Era V/VI):**

9. **AX.W0b interpreter deletion** — ~78K LOC reclaim (`era-V:210-232`) — the
   worked example of "can we delete before we patch" (`abrogate-before-patch`).
10. **AX's 21 invariants** (`era-V:252-256`): bench-checkpoint mid-wave, wire-
    contract compile-gate, ledger-review at handoff, frozen-contract for gate
    predicates. The `gate_predicate_wire_contract.rs` 21-assertion freeze surface is
    salvageable.
11. **The view layer** `crates/core/src/backend/rust/view/` (`NodeView`,
    `TapeCursor`, IR-derived named-type resolver, `era-V:283-288`) and the
    canonical-parity harnesses against sonic-rs/lightningcss/simdjson/serde_json/
    cssparser (`tests/*_parity.rs`, `era-V:287-288`).

**Negative results that MUST NOT be re-learned:**

- **Unbounded CSP branch-and-bound freezes** — X.6 hung css_l4 at 94ms (10×)
  exploring the cross-product with no guard (`Y/Y.md:13`). Never solve a global CSP
  without a node budget and a component decomposition.
- **Per-call hashmap rebuild in a recursive pass** = 91.73% cliff, 44.5%
  libsystem_malloc (`W/W.md:29, :137`). Build shared scratch once, thread by
  reference.
- **7-column tape AoS loses to a single `Vec<TapeRec>` + `sib_skip`** on cache
  locality (`era-VI:63-82`). Do not re-split into columns without a cache-locality
  measurement.
- **Hand-coded per-grammar `Value` types** cost 6,128 LOC and were reverted
  (`3429aaba`). Generality is the whole point; per-grammar hardcoding is anti-thesis.
- **Bench dishonesty via inherited numbers** — Z had to rebase because *"several
  of the draft's numbers were inherited from earlier conversations and no longer
  matched source"* (`Z/Z.md:7`; "4 allocs"/"9× walks" were both wrong,
  `Z/Z.md:32`). Only direct source inspection caught them. Re-profile; never inherit.
- **"Shared substrate" that shares only a type** — CostWeights was structural
  fiction (`W/W.md:76`). A shared struct with two independent `::default()` calls
  is not coordination.
- **Two zero-match kernel families W shipped** (6/8 consumers) — substrate with no
  consumer must be *deleted* (Y.4 did), not preserved as "future" (`Z/Z.md` context).

---

## 6. The archive/ bundle and the letter-recycling disease

`docs/tranches/archive/pre-restart-{BA,BB,BC}/` preserves pre-recycle tranche
plans with explicit SUBSUMED banners. BA-pre-recycle was *"typed path/query macro,
lazy bail-out parse, host bindings"* absorbed into AZ-IV; the Python binding was
dropped (`archive/pre-restart-BA/historical/BA-pre-recycle.md:1-9`). BB was
*"E-graph rewrite rule inference — Ruler-style CVC enumeration"* subsumed into the
recycled BA + AZ-IV (`archive/pre-restart-BB/historical/subsumed/BB-subsumed-
banner.md:1-6`). The canonical post-AZ-IV sequence had to be re-declared
explicitly: **AZ → BA (direct-projection) → BB (rule-discovery) → BC (cleanup) →
BD+** with "letter discipline: un-recycled at master `40092b28`"
(`archive/pre-restart-BA/BA.md:4`).

The lesson is unambiguous: **the corpus repeatedly reused tranche letters, then
had to write banners, `historical/` subdirs, and "un-recycled" notes to dig itself
out.** Combined with the Y/Z collision in §0, this is the strongest single argument
for SK-V21 adopting a *monotonic, never-recycled, collision-free* tranche identity
scheme (the SK-V## sequence already does this — preserve it rigorously).

Notably, the BA direct-projection plan (`archive/pre-restart-BA/BA.md:16-40`)
carries a still-live architectural defect worth resurrecting as a requirement:
*"Type inference output is thrown away at the parse boundary… nine emission sites
construct `__layout: StructLayout { rule_type: TypeDesc::Span, fields: vec![] }` —
codegen-known type info is re-derived at runtime per leaf push"* and *"`Vec<OpenFrame>
::clone` dominates samply at 86.07%… checkpoint deep-clones the in-flight stack on
every speculative branch entry."* These are precisely the `typed-materialization-
invariant` and speculative-checkpoint-clone problems; SK-V21's emitter must carry
codegen-known types to the tape and avoid deep-cloning the frame stack on every
byte-dispatch checkpoint.

---

## 7. Bottom line for SK-V21

The pre-restart corpus is not uniformly a wreck. The W→X→Y→Z arc is a *model* of
disciplined optimizer activation — it invented the consumer-invariant gate, the CSP
freezing guards, the truth-based profiling attribution, and the grammar-agnostic
structural-bitmap generalization. The wreckage (Era V's 600-commit substrate rut,
the 6,128-LOC hand-coded Value revert, the CSP theatre, the 130s bootstrap wall)
was diagnosed in full by the 2026-04-22 self-audit, which SK-V21 should cite rather
than re-derive. The three things to carry forward as *non-negotiable structure*:
(1) every emitted decision variant lands with its consumer and its exhaustive-match
gate in the same commit; (2) every global search is budgeted and component-
decomposed; (3) every performance claim cites a fresh profile line. And the one
thing to fix that the corpus never did: a monotonic, collision-free tranche
identity, so the totality plan never needs an "un-recycled" banner.
