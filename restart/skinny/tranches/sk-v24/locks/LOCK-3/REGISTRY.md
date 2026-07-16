# LOCK-3 — THE IR TRANSPOSITION SPINE · REGISTRY

*Living adjudication ledger across the ≥3-pass convergence loop. The AGGLOMERATOR folds each
pass (research + FAMILY-SPECS + PROTOs + CRIT-1 + CRIT-2) into this file. Convergence % is the
**critics' floor** unless the agglomerator can name why a critic erred; no family approaches 100%
until it has zero open gaps + a fresh audit + two consecutive clean passes. Status is one of
ADVANCED / BANKED / BLOCKED / RETIRED.*

Charter: the IR tower and its transpositions (grammar AST → typed HIR → mid-IR → backend shapes →
emission); lowering contracts and invariants; residualization (PROVE-12) generalized; the decision
driver's seat; multi-backend (Rust now, TS later) over a backend-agnostic middle. Standing laws:
`no-value-discard`, `no-silent-epsilon`, `preserve-rich-ast`.

---

## VERIFIED BASELINE (re-anchored by the agglomerator against live — supersedes pass-1)

Live master **HEAD is now `skinny@c43126422`** (`git rev-parse HEAD` = `c43126422423…`, confirmed
verbatim this pass). This is **exactly the pin FAMILY-SPECS §0 and both critics used** — the pass-1
registry's `b3cb8c16b` is a 10-commit-behind ancestor (ancestor-of-HEAD ✓). The source tree lives
at top-level `skinny/crates/` (NOT `restart/skinny/`, which holds docs/tranches only). The spine is a
four-edge transposition chain:

```
.bbnf ──parse──▶ GrammarIr ──compile/lower_expr──▶ BackendIr ──sink_only──▶ SinkOnlyProgram ──lower_to_rust/emit──▶ .rs
```

**Base-drift is a NON-issue for code (agglomerator ran the stronger check CRIT-2 §0.1 opened).**
`git diff --stat c93fc43143..c43126422 -- skinny/crates` and `…ce51c36fc..c43126422 -- skinny/crates`
are **both empty** — the 10 intervening commits touched only `restart/.../tranches/` (the research
corpus), never `skinny/crates`. So the *code tree* every LOCK-3 prototype measured is **byte-identical
to HEAD**; the PROTO-1/PROTO-2/PROTO-1-t1 byte-identity floors and the PROTO-2-3A localization matrix
**transfer to HEAD verbatim**. XL-H wrong-artifact-risk is **discharged** for every LOCK-3 prototype
this pass. This is recorded as a positive reconciliation, not an unproven assertion.

Anchors re-greped by hand on `c43126422` this pass (all CONFIRMED):

| Fact | Live truth (`c43126422`, verified) |
|---|---|
| `->` ty-drop | `passes/src/lib.rs:204` `ExprKind::Project { body, .. } => self.infer_expr(*body)?` (ty/target via `..`) |
| `:203` | `ExprKind::Annotation { .. } => Type::Builtin(BuiltinTy::Unit)` — a **non-defect** (`@ws`/`@cap` legit untyped) |
| name-gate | `has_rule("value")` @ `codegen/src/runtime_generator.rs:98` **survives** (clause-vi name-gate) |
| schema sum-wall | `DirectTypeKind` @ `codegen/src/direct_schema.rs:52` is **Struct-only** (single variant; no recursive `Sum`) |
| CSS emit | **IR-driven** (`css_scan_direct` + `ScanAlphabet::derive_alphabet` → `const … : &[u8]`); `find_component_delim` retired from `codegen/src` |
| stage plane | `binding_times`/`Stage` **absent** from live `crates/ir/`; `DirectBuildField.target` = `None` at every offset-tape descriptor |
| **XL-1 (verified)** | `TypeAnalysis`/`from_ir`/`analysis.rs` **absent from live `skinny/crates/`**; they exist ONLY in legacy `crates/core/src/backend/driver/analysis.rs` over the legacy `GrammarIR` |

**Two facts bind every family.** (1) The tower is **3B-shaped today** — one attribute-annotated
`LayoutFacts` bundle (`passes/src/lib.rs:32-62`, mutated in place at `:54-57`), `BackendExpr`/
`SinkOnlyProgram` a materialized plane, then a **fractured** emission stage; LOCK-3 either hardens
(3A/3B) or re-founds (3C/3D), every divergence from the GN codex a **named supersession**
(Constitution ix). (2) The **`Project.ty`@204 thread** is single-owner and **DISCHARGED for the fact
plane** (PROTO-1 EXECUTED RED→GREEN, zero footprint) — but the schema stays **starved**
(`DirectBuildTarget` real, `.target = None` everywhere; the sole schema producer is the hand file
`xtask/src/real_typed_schema.rs`), and both the CONSUME half and the consistency CONTRACT over the
threaded value remain open (see the cross-lock directives).

---

## CURRENT DISPOSITIONS (as of pass-2 — the fold of all four prototypes)

| Family | Status | Convergence | One-line |
|---|---|---|---|
| **3A · CONTRACT-TOWER** | **ADVANCED** (discipline-on-3B-substrate; standalone typed-multi-level-tower **BANKED**) | **60%** ↑55 | The distinctive **refinement oracle RAN and LOCALIZES** (PROTO-2-3A) — but the RAN prototype newly exposed that the *contract* (vs the *oracle*) is **presence-only/vacuous until LOCK-1** delivers name→Type; the load-bearing E1-infer non-vacuity is unrun. |
| **3B · SINGLE-IR-MULTIPASS** | **ADVANCED** (the convergent substrate) | **55%** ↑50 | One byte-identical **slice** of the fold RAN (PROTO-1-t1, 3/3) and refuted PROTO-2's `dispatch_alt_count` discriminator by execution — but the **killer whole-fold gate G-3B-1 is still UNRUN after two passes**, the −100 net-LOC rests on an **unrun re-walk**, and the ran slice used a Constitution-forbidden DRY duplicate. |
| **3C · GRAMMAR-HOMOIOUSIA** | **BLOCKED** (bank the gift; converts to ADVANCED on unblock) | **32%** ↑30 | The **C1/C2 split** is the real analysis progress (data-model-as-grammar vs transposition-as-grammar); C1 born-RED on `DirectTypeKind::Sum` + a LOCK-7 consumer, C2 refuted-on-live and ceded to LOCK-2. Distinctive content **0% convergence-eligible**; SPEC-ONLY this pass. |
| **3D · STAGED-RESIDUAL** | **BANKED** (stage-attribute subsumed into 3B; PROVE-12-`u128` flagship **RETIRED**) | **38%** =38 | **Nothing 3D-distinctive ran.** The unification thesis is 100% SPEC-only and spec-cites-itself (Constitution v restatement); leverage rests on a **retired-target** PROVE-12, an already-shipped `ScanAlphabet`, the shared `:204` thread, and a **proven-DEAD gate** (`admits_collapsed_stage` reads `avx512bw`, permanently false on aarch64). |

Floor rule applied: 3A floor(62,60,63)=**60**, 3B floor(60,55,63)=**55**, 3C floor(34,33,32)=**32**,
3D floor(42,38,40)=**38** (FAMILY-SPECS / CRIT-1 / CRIT-2). The agglomerator re-verified every crux
fact both critics rest on (HEAD=pin, `:204` ty-drop, `:203` non-defect, name-gate survival,
Struct-only schema, XL-1 `from_ir`-absence) — **the critics did not err, so the floor stands
unadjusted.** Convergence deltas from pass-1 credit the executed organs (3A oracle, 3B slice) while
withholding the load-bearing gates each left unrun.

**THE HEADLINE PROCESS CORRECTION (both critics, agglomerator-confirmed by mtime).** `FAMILY-SPECS.md`
= **15:10**; `PROTO-1-t1.md` = **15:34**; `PROTO-2-3A.md` = **15:38**. The synthesis was **frozen
24–28 min before its own two sharpest prototypes ran** — it names only "the two prototypes that RAN
(PROTO-1, PROTO-2)" and issues the two later probes as *pass-3 briefs* that were then **executed**.
This registry **re-folds all four artifacts** (PROTO-1 :204-thread + PROTO-2 fold-trace [pass-2 early];
PROTO-1-t1 seam-slice + PROTO-2-3A refinement-oracle [pass-2 late]); the FAMILY-SPECS SPINE table's
"3A oracle UNRUN" and "3B-G1 UNRUN" rows are corrected below.

---

## FAMILY 3A — CONTRACT-TOWER · ADVANCED (discipline) · 60%

**What is earned (RAN/PROVEN this pass — the distinctive organ ran).** PROTO-2-3A **executed** the
per-edge refinement oracle `differential(edge, corpus)` — the one thing distinguishing 3A from a
tautological `verify()`, and the "sharpest UNRUN uncertainty" the frozen spec still mislabels UNRUN:
4/4 tests, `EXIT_CODE=0`. The **E3 correspondence oracle** is reference-free (the sink map is a 1:1
arm renaming) and **GREEN on the live `json` BackendIr** (15 rules / 100 nodes) as well as a 13/13-arm
synthetic census. An injected single-arm E3 fault is **named to its arm with zero false-positive
witnesses** across a 5-fault matrix — *including a same-discriminant payload corruption a tag-only
check would miss* — and an **E2 input fault trips the E2 oracle while E3 stays GREEN**, so the runner
**localizes to the injected edge, never merely "a failure"** (open question (i) = **YES, by
execution**). The Y.13 witness-census discipline (zero-witness arm ⇒ `unverified`, never `sound`;
`ValueProject` Sheets-only) is proven. This is a genuine uplift the frozen synthesis does not credit.
Separately, PROTO-1 proved the `verify_project_types` predicate over `Project@204`: **RED (4 spans) →
GREEN, zero emit footprint** (four regen checks byte-identical predicate-in vs -out).

**Redirect (crowding) — reaffirmed and hardened.** 3A-as-standalone-typed-multi-level-tower stays
**BANKED**; 3A advances **as a discipline layered on the 3B single-IR substrate**. The two **compose**
(the cross-pollination below), now with running evidence on both sides: 3A's E3 oracle ran on the
*same live BackendIr* the 3B fold would fold — the graft is no longer merely named, it is partially
executed.

**Open gaps (re-cut against the RAN prototypes — the frozen gap-set was wrong).**
- **G-3A-α (the contract is a GATE-THAT-CANNOT-FAIL until LOCK-1 — newly exposed by the run).** The
  proven predicate checks, per `Project` with `ty=Some(d)`: `node_types[project]==Type::Declared(d)`
  **and** `node_types[project] != node_types[body]`. `Type::Declared(String)` is a **nominal** wrapper
  (distinct enum variant), never structurally equal to the **structural** `infer_expr(body)` — so the
  `≠ body` clause is **vacuously true whenever `ty=Some`**. The organ verifies **threading-presence**,
  not declared-vs-inferred **consistency**. True non-vacuity is **born-RED on LOCK-1** (G-3A-a
  name→`Type` resolution): until it lands the contract degrades to string-equality. The "PROVEN" organ
  is presence-only; the *contract* is unearned. (CRIT-1 G-3A-α, CRIT-2 §vacuity — both, verified.)
- **G-3A-β (E1-infer injected-fault matrix UNRUN — the load-bearing edge).** The oracle ran on the
  **free** edge (E3, a 1:1 renaming). PROTO-2-3A's own scope-limit (1): E1/E2 were grounded only as
  reference digests + one E2 structural oracle `O2`. The full E1(infer)/E2(materialize) injected-fault
  matrix — where the `ty`-contract's *meaning* lives — is unrun. The organ is proven where it is easy,
  unproven where it matters.
- **G-3A-γ (E3 is a lossy correspondence, not a round-trip).** The map **erases** `SpanMark.kind` +
  `TapeEmit.kind`, so `relift(lower_expr(x))==x` is false on those fields; the honest oracle *excludes*
  the two erased arms, and a fault flipping only an erased `kind` is **invisible to E3** (must be caught
  at E2). Correct edge-attribution, but "free round-trip" over-claims — a named property to fold in.
- **G-3A-δ (E4-as-projection unrun; CSS two-rooted tower undecided).** Open q (ii): is `render`
  (`runtime_generator.rs:78`) a pure projection of L2 (so "three levels" holds, E4 carries no oracle)?
  Open q (iv): the CSS `Err⇒SinkOnly / Ok⇒Scan` fork (`runtime_generator.rs:40-71`) forces either a
  first-class edge oracle or a declared two-root tower. Both unrun.
- **G-3A-ε (the runner is a probe, not the deliverable).** PROTO-2-3A is a `+607`-LOC probe test, not
  the shipping `differential(edge,corpus)` CI contract that is 3A's stated deliverable.

**Pass-3 directives.**
1. **Run the E1-infer injected-fault matrix** (mutate `infer`/`materialize`; show E3 stays GREEN under
   each, E1/E2 localize). This is the load-bearing widening PROTO-2-3A itself named — the edge the
   oracle did not run.
2. **Exhibit ≥1 Sheets projection where declared `ty` constrains BEYOND `infer_expr(body)`** (open q
   iii), else report the contract `unverified`. Carry **G-3A-a name→`Type` resolution as a born-RED
   dependency edge on LOCK-1** — the contract is presence-only until it lands; do not assert
   non-vacuity absent it.
3. Prove E4/`render` is a pure projection of L2 (open q ii); decide the CSS two-rooted-tower question
   (open q iv) — first-class edge oracle or declared two-root.
4. Promote the runner from probe to a **shipping `differential(edge,corpus)` CI contract** — every
   byte-identity gate in the lock becomes an instance of one edge oracle (3A's deliverable).

**Withheld from researchers:** do **not** re-litigate the standalone typed-multi-level-tower (BANKED);
do **not** re-argue that the oracle localizes an injected fault (PROVEN — E3 arm + E2↔E3 separation,
zero false positives); do **not** re-run the E3 correspondence on the shipping map (GREEN on live).

---

## FAMILY 3B — SINGLE-IR-MULTIPASS · ADVANCED (substrate) · 55%

**What is earned.** The cleanest structural argument in the lock, now with one **executed slice**.
PROTO-2 cleared the **fact-layer purity precondition by trace**: the four `SinkOnlyFacts`
accumulations read only `GrammarIr + backend_shape + descriptor`, so `materialize_rule`'s emit-order
framing is a **pure derivation recomputable inside one fold** (Q-3B-1 precondition = YES). PROTO-1-t1
then **executed** the emit-dispatch seam retirement — replacing the `has_rule("value")` name-gate with
a structural value-role census — **byte-identical GREEN 3/3** (`check-{json,sheets,real-typed}`), the
first *run* code on the fold path. It also **refuted PROTO-2's named discriminator by execution**:
`dispatch_alt_count == 0` turns `check-sheets` **RED** (`missing Number RegexProgram` — Sheets carries
dispatch Alts too); the honest discriminator is the *specific* **≥4-of-6 value-category census**
(`roles.value`), not any dispatch count.

**Open gaps.**
- **G-3B-1 (THE killer gate, still UNRUN after two passes — the elegant-reduction trap, live).**
  *Execute* the whole `emit(&GrammarIr,&Attributes)` fold; **regen×check × {json,css,sheets,real-typed}
  byte-identical**. Two passes in: trace (PROTO-2) + one slice (PROTO-1-t1), **no fold**. "Executable"
  ≠ "executed." This is the one falsifiable that earns the substrate the whole spine hangs on. Per the
  spec's own condition, the **demotion trigger is partly met**: run it next pass or demote 3B to
  "structurally-motivated, unproven."
- **G-3B-1a (the re-walk re-expression IS the unconverged hard part — CRIT-1's sharpest addition).**
  PROTO-2's trace states deleting `SinkOnlyExpr` **adds back** a comparable re-walk of `GrammarIr` in
  `json_sink_direct/dispatch.rs` (318 LoC) + `general_descent/view.rs` (431 LoC) — **structural
  tree-walk consumers**, not emission-only intermediates. The "one fold" byte-identity risk lives
  precisely in re-basing those two walkers onto `GrammarIr` (a reordered/differently-keyed walk breaks
  identity). **The −100 net-LOC is the reward IF the walkers re-base byte-identically — an unrun
  assumption, reported CONTINGENT, not a measured deletion.** The classic elegant-reduction tell.
- **G-3B-2 (relocation ≠ retirement; a DRY smell — carried born-RED).** PROTO-1-t1's slice ports
  `derive_materialization_roles.value` **into codegen**, duplicating passes-side logic — a
  `system-cohesion`/`no-god-modules`/DRY violation the probe itself flags. So the *name* is gone on
  that fork; the *clean single-home retirement* is not. The production home is threading the
  passes-computed `roles.value` onto the attribute bundle (`LayoutFacts → Attributes`), read by the
  fold via lookup. True **RETIREMENT** (vs relocation) stays **born-RED on `DirectTypeKind::Sum`**
  (GN-3.W1) + a value-role attribute — carried, never asserted retired absent the node. **The proven
  artifact is a byte-identically *relocatable* slice with a DRY home, NOT the single-home fold.**
- **G-3B-3 (materialization key unconfirmed).** per-`RuleId` suffices for JSON/Sheets; the outer tree
  keys by **`NodeId`** (one rule holds many distinct Seq/Alt nodes — CSS L4 inline alternatives the
  live candidate). Fleet check (json/css/sheets/csv): does any single rule need two distinct
  descriptors → per-ExprId forced? A partial migration is the `no-orthogonal-codepaths` defect. Unrun.
- **G-3B-4 (CONSUME correctly REFUTED-as-stated, honestly homed here).** `.target` is a **dead field**
  — `None` at all ~9 construction sites (`passes/src/lib.rs:1157,1177,1201,1208,1228,…`), `Some(..)`
  nowhere, never read (the typed render clones the hand `DirectSchemaSet`). The `target=Some(ty)`
  one-liner is refuted; true CONSUME sits behind three walls (type/wiring/corpus) **inside G-3B-1's
  fold**, not a separable move. A live Y.13 dead-substrate fact, honestly carried.
- **G-3B-5 (byte-identity incidental-vs-semantic; css fourth-level).** Open q (i): header comments,
  `BTreeMap` key order (`:110`), roster order must be shown incidental on the **executed** fold, every
  non-identical byte attributed. Open q (ii): does `css_scan` hide a fourth value level (grammar-name
  prefixes / entry-rule threading not in `GrammarIr`)? Both unrun.

**Pass-3 directives.**
1. **RUN G-3B-1** on live: full `emit(&GrammarIr,&Attributes)` fold; regen×check ×
   {json,css,sheets,real-typed} byte-identical. The one falsifiable — run it or demote.
2. **Re-base `dispatch.rs` (318) + `view.rs` (431) onto `GrammarIr` byte-identically** (G-3B-1a) — the
   real content of the gate. Report net-LOC as **contingent** on this, not banked.
3. **Single-home the value-role discriminator** — thread passes-computed `roles.value` onto the
   attribute bundle; retire the codegen-side DRY duplicate PROTO-1-t1 introduced. (Record the ran slice
   as "executable, single-home wiring deferred," not a discharged sub-gate.)
4. Attribute every non-identical byte incidental-vs-semantic on the executed fold; trace `css_scan` for
   a hidden fourth level.
5. Run the fleet key-check (per-RuleId vs per-ExprId) with the exact counterexample rule.

**Withheld from researchers:** do **not** build any `PassManager`/`PreservedAnalyses` invalidation
machinery — consumer-less until a mutating Transform exists (Stratum 2 only). Do **not** call the
name-gate "retired" — PROTO-1-t1 proved it byte-identically *relocatable*, not retired.

---

## FAMILY 3C — GRAMMAR-HOMOIOUSIA · BLOCKED · 32%

**Correctly the honest disposition; the C1/C2 split is the real analysis progress (research-C's
decisive move).** The family cleaves into two claims of very different strength:
- **C1 — the IR *data model* is grammar-specified** (enum/struct shells emitted from a bbnf grammar).
  Bounded born-RED: `DirectTypeKind::Sum` @ `direct_schema.rs:52` (verified Struct-only this pass;
  owner **GN-3.W1**, +420 LOC) + a consuming **LOCK-7·7B** wave (TOTAL-SELF-HOST literally enumerates
  "IR grammar"). On live there is **no `ir.bbnf`, no meta-grammar, one grammar (`json.bbnf`)** — C1's
  sole reader is an **unlanded LOCK-7 wave** (Y.13), admissible only as a born-RED edge under GESTALT-B.
- **C2 — the IR *transpositions* are grammar-to-grammar maps.** **REFUTED on live** (the sharpest
  counterexample): bbnf maps **bytes → tape**; `lower_expr`/`materialize_rule` are **tree → tree**
  folds with no byte stream, so no bbnf grammar *is* this map. Realizing it demands a
  term-rewriting/attribute-eval engine bbnf lacks → importing one is an orthogonal-subsystem breach OR
  a collapse into **LOCK-2's engine**. **BLOCKED-candidate** (missing primitive of difficulty *equal
  to* the original problem), **ceded to LOCK-2** — provisional on XL-B (WORKING, not final; see XL-2).

**Note — one pass-1 blocker DISCHARGED by XL-C.** Pass-1 listed three blockers; XL-C (RATIFIED)
settles the regex axis and states verbatim that **"LOCK-3 3C no longer hang[s] on ONE-HIR"** — the
LOCK-5 HIR-functor verdict is no longer a blocker. FAMILY-SPECS and both critics already absorbed this
(they list only Sum + LOCK-7 consumer), so it is priced into the 32%. Two external gates remain; the
family stays BLOCKED.

**The one win that stands alone (not distinctively 3C).** 3C's `->`-thread (P4) is the **same
single-owner `:204` thread** PROTO-1 ran; 3C **consumes** the threaded value, it does not author it.

**Open gaps (all externally owned).**
- **G-3C-1 (`DirectTypeKind::Sum`, born-RED, GN-3.W1).** The product-only subset excludes every `Alt`
  = excludes `ExprKind` (10-arm) / `BackendExpr` (13-arm) themselves — **must not be treated as
  standing for the formulation** (elegant-reduction trap by construction).
- **G-3C-2 (named consumer, born-RED, LOCK-7·7B).** No live `ir.bbnf`; sole reader unlanded. Y.13.
- **G-3C-3 (fixpoint, LOCK-7·7A).** `regen∘regen = regen` byte-identical over a grammar-generated
  `ir/src/lib.rs` carrying derives/doc-comments/impls — the sharpest engineering gate; likely forces
  per-derive/per-impl annotations.
- **G-3C-4 (C2 cession).** Ceded to LOCK-2, provisional on XL-B.

**Pass-3 directives.**
1. Hold **BLOCKED**. **Do not author `ir.bbnf`** until (a) GN-3.W1 lands `DirectTypeKind::Sum` and (b)
   a consuming LOCK-7·7B wave is named. Track both as born-RED dependency edges with owners.
2. Keep the C1/C2 costing apart — it is the family's honesty. C2 stays refuted-on-live/ceded-to-LOCK-2
   (provisional on XL-B); do not re-import a rewrite engine into 3C's charter.
3. *Only if* a consumer is named: run the product-only G1 emit-byte-identity + `regen∘regen` fixpoint
   probe as a **scoping probe** (proves the emitter is expressive enough for compiler-internal ADTs),
   explicitly labeling that it **excludes the sum**.

**Withheld from researchers:** do **not** re-derive the blockers (settled); do **not** propose
authoring the IR meta-grammar as pass-3 mechanism (consumer-less-forbidden); the cost/analysis
substrate (`ir/cost.rs`) stays hand-written primitives — do not god-modularize it into the
meta-grammar.

---

## FAMILY 3D — STAGED-RESIDUAL · BANKED · 38%

**Nothing 3D-distinctive ran this pass (Constitution vii).** 3D's content over "3B + a shipped
`ScanAlphabet` const" is exactly two claims: (i) `BindingTime ∈ {Stage0,Stage1}` as a typed IR fact,
and (ii) the unification "type-plane erasure = stage-0 erasure on the type sub-plane." **Both are
SPEC-only**, and (ii) **spec-cites-itself** — its warrant is Constitution v ("the type plane costs
zero at runtime") ⟺ "the type plane is entirely Stage0," a definitional restatement, not an
independent result. The stage mechanism **is architecturally a 3B move** (`Stage` is one attribute
slot); the separate-staging-spine formulation stays **BANKED into 3B's `Attributes` store**.

**The leverage is weaker than the FAMILY-SPECS 42% tick implies — four soft anchors.**
- **PROVE-12 (the "one executed instance") is a retired-target measurement.** Its target
  `find_component_delim` is **retired from `codegen/src` on live**; the numbers (ins/B −4.8…−6.8%,
  cyc/B 0.80–0.91×) stand on base `d2b32f6e3` against a site no longer on the emit path — and
  research-D itself flags the base moved (`d2b32f6e3→c43126422`) and the PMU **was not re-run**.
- **`ScanAlphabet`** (the surviving live residual) is **already shipped**, not 3D-authored (declared
  dependency, not re-authored) — so it is not 3D convergence.
- **G-3D-1 (the DEAD gate — strongest live fact, but evidence of a PROBLEM not a win).**
  `admits_collapsed_stage` (`passes/src/lib.rs:960`) = `target.avx512bw && matches!(…Entry(_))`, and
  `avx512bw` is `cfg!(target_arch="x86_64"…)` — **permanently false on aarch64** (SPEC §0: x86 dead).
  `CollapsedStage` is a **dead shape** (`CollapsedStageNotViable` fires whenever declared) — a genuine
  Y.13 consumer-less-substrate on the living tree. This is a problem 3D must fix, not a bank.
- The RETIRED sub-verdict holds: the **PROVE-12-`u128`-byte-identity flagship** stays **RETIRED** (its
  concrete artifact is gone from the emit path) — do not re-pose it.

**Open gaps.**
- **G-3D-i (unification unproven — the distinctive, 0% run).** Attach a binding-time fact to live IR
  and prove "type-plane = stage-0" beyond restatement — the same single-owner `:204` thread as an S0
  residual to a Rust *type* (3D **consumes** it).
- **G-3D-ii (`binding_times(CssScanProgram)` == PROVE-12 partition, unrun).** Runnable today via the
  PROVE-12 checksum (parity byte-identical iff the Stage0 set equals the hand-residualized set) —
  not run this pass.
- **G-3D-iii (CollapsedStage re-home, unrun).** Re-home admission off `avx512bw` onto a grammar-derived
  binding-time predicate → admissible on aarch64 via `decision_csp`, no stub, **no x86 smuggled back**;
  RED→GREEN on `CollapsedStageNotViable`.
- **G-3D-iv (JSON type-plane PMU delta, unmeasured).** JSON has no scan-side Stage0 (hot rules dispatch
  to hand kernels) → the JSON claim is *entirely* the type sub-plane; absent a measured cold paired-PMU
  delta it collapses to "typed façade, same speed" (acceptable only if declared; PROVE-09 law).

**Pass-3 directives (all under the banked banner; reactivate to ADVANCED only if the unification is
proven concretely — binding_times on real IR + CollapsedStage re-home + a measured type-plane delta).**
1. Run **`binding_times` on the REAL CSS IR**; prove the Stage0/Stage1 partition **equals** the set
   PROVE-12 hand-residualized (checksum parity oracle).
2. **Re-home `admits_collapsed_stage`** off the dead `avx512bw` gate onto a grammar-derived binding-time
   predicate — no stub, no x86 back.
3. **Prove the unification concretely** by threading `Project.ty`@204 as an S0 residual to a Rust
   *type* — the un-done half (consume the shared thread, do not re-own it).
4. **Re-confirm PROVE-12 on `c43126422`** (XL-H hygiene, base moved) + measure the type-plane JSON
   first-byte-dispatch cold paired-PMU delta — real ins/B or a named negative.

**Withheld from researchers:** the PROVE-12-`u128`-byte-identity claim is **RETIRED** — do not re-pose
it; do **not** re-author `css_scan_direct` (landed) — declare the dependency; per-site staging-form
scheduling is **LOCK-2's** seat (gated on a real consumer, provisional on XL-B), not a LOCK-3 hardcoded
emitter branch.

---

## REDIRECT / CROWDING DECISIONS (pass-2)

1. **3A-tower ⊗ 3B-single stays dissolved into the hybrid — now with running evidence on both sides.**
   The lock's one hard incompatibility (distinct typed levels vs one IR) is resolved toward **one IR +
   per-pass contracts**; 3A-as-standalone-typed-tower stays **BANKED**. 3A-discipline + 3B-substrate
   **compose**, and this pass PROTO-2-3A's E3 oracle ran on the *same live BackendIr* the 3B fold folds.
2. **3D's separate-staging-spine stays BANKED into 3B's attribute store.** `Stage` is an attribute, not
   a level; nothing 3D-distinctive ran to reopen the spine. Its unique unification thesis is re-opened
   scoped to live (G-3D-i…iv), not funded as an independent tower.
3. **The seam-dissolution is single-funded to 3B — now DECISIVELY, by execution (XL-3).**
   `has_rule("value")`@98 + `derive_materialization_roles`@1403 are the **same live hardcodes** 3C
   would "dissolve via grammar" and 3B would "retire via fold." PROTO-1-t1 **executed** 3B's byte-
   identical relocation of this exact seam → the funded route (3B) now has running evidence and 3C's
   grammar-dissolution alternative is further deprecated. The gestalt funds **one** (3B); 3C's route
   reactivates only if 3C unblocks. Funding both is a double-home breach.

**No new redundancy to bank this pass.** The critics show the two advancing families composing (not
converging to one mechanism), so no family is marked BANKED for redundancy this pass. The
**underexplored formulation to open** is not a rival mechanism but the *unrun half* of the proven 3A
organ: the **E1-infer injected-fault matrix + the `ty`-contract non-vacuity witness** (G-3A-α/β) — the
edge where the contract's meaning lives, born-RED on LOCK-1.

## CROSS-POLLINATION (the one named, justified — now partially EXECUTED)

**3A's refinement-oracle organ ⟶ 3B's `(GrammarIr, Attributes)` fold.** Upgraded from "named/justified"
(pass-1) to **named, justified, and partially executed**: PROTO-2-3A ran the E3 correspondence oracle
on the **live `json` BackendIr** (15 rules / 100 nodes) — the per-edge contract over the single IR the
3B fold would fold. The graft: each analysis/emit pass over the single IR carries a **checkable output
contract** — a `verify(grammar, attrs)` predicate (PROTO-1, proven) + a per-edge differential digest
(PROTO-2-3A, run on E3) — so the discipline of a nanopass tower is recovered **without a second tree**.
This *is* the convergent spine's "one IR (3B) + per-pass contracts (3A discipline)."
*Deferred graft:* 3D's residual-count oracle is a 3A-style refinement oracle, but 3D is BANKED and its
distinctive organ never ran — re-evaluate only after the 3D re-cut (G-3D-i…iii) lands on real IR.

## THE CONVERGENT SPINE (hypothesis; architecturally sound; rests on two UNRUN load-bearing halves)

**One IR (3B substrate) + per-pass contracts (3A discipline) + a stage attribute (3D erasure); 3C
banked** until GN-3.W1 (sums) + a named LOCK-7 wave unblock it. The families **compose**, not compete.
State after the four-prototype fold (correcting the frozen SPINE table):

| Component | State after pass-2 |
|---|---|
| `:204` thread + verify predicate (RED→GREEN, zero footprint) | **PROVEN** (PROTO-1) |
| Fold fact-layer purity (`materialize_rule` recomputable) | **PROVEN by trace** (PROTO-2) |
| **3A refinement oracle — localizes an injected fault to its edge** | **RAN** (PROTO-2-3A) — on the **free E3 edge**; E2↔E3 separation proven |
| **3A *contract* non-vacuity (E1-infer; declared-vs-inferred)** | **UNRUN + gate-cannot-fail until LOCK-1** (name→`Type`) — the organ that earns 3A's *contract* |
| Seam relocation (`has_rule("value")` → value-role census) | **RAN byte-identical** (PROTO-1-t1) — relocatable slice, DRY home; single-home + retirement deferred |
| PROTO-2's `dispatch_alt_count` discriminator | **REFUTED by execution** — working discriminator = ≥4-of-6 value-category census |
| **3B-G1: execute the WHOLE fold, regen×check byte-identical** | **UNRUN — the spine's load-bearing gate** (−100 net-LOC contingent on an unrun re-walk) |
| CONSUME (`.target` → reproduce `real_typed_schema.rs`) | **REFUTED-as-stated** (`.target` dead); behind the 3B fold |
| 3D binding_times on real IR + CollapsedStage re-home | UNRUN (BANKED) |
| 3C C1 (grammar-specified data model) | BLOCKED (Sum node + LOCK-7 consumer) |
| 3C C2 (grammar-to-grammar transposition) | **BLOCKED-candidate — refuted, ceded to LOCK-2 (provisional on XL-B)** |

**The two UNRUN load-bearing halves (both critics converge):** 3B rests on **G-3B-1, the whole-fold
regen×check** (one slice ≠ the fold; the slice used a Constitution-forbidden duplicated home); 3A rests
on the **E1-infer non-vacuity** (the oracle is proven on the 1:1 free edge, not on the edge that could
be vacuous). No family is convergence-eligible (pass 2 of ≥3; each carries ≥1 UNRUN load-bearing gate;
two consecutive clean passes + fresh non-author audit outstanding for all four).

---

## CROSS-LOCK / GESTALT DIRECTIVES (pass-2)

1. **XL-1 — the `TypeAnalysis::from_ir` consumer contradiction: route to GESTALT-B as a named-
   supersession challenge with evidence (the single most important cross-lock finding this pass).**
   CROSS-LOCK XL-F(1) settles as binding fact: *"TypeAnalysis::from_ir at backend/driver/analysis.rs is
   the real type consumer."* **Agglomerator-verified this pass:** `TypeAnalysis`/`from_ir`/`analysis.rs`
   are **absent from live `skinny/crates/`** — they exist only in **legacy `crates/core/src/backend/
   driver/analysis.rs`**, over the legacy `GrammarIR` (a reference-resolved grammar, not a typed IR
   with `node_types`). All four LOCK-3 researchers (A §0, C §5 caveat, D §3) independently found this,
   and FAMILY-SPECS re-homed the consumer to the `DirectSchemaSet` seam (`derive_materialization_plan`
   → `json_typed_direct::render`) **silently**. Two readings, both routable with evidence: **(a)** XL-F(1)
   describes the *greenfield tree-of-record* consumer (reconcilable under XL-A: skinny is the
   measurement substrate, designs target greenfield) — in which case it must be **labeled** as such,
   not a live-spine pointer; **(b)** it is a **stale pointer for the live spine** and XL-F(1) needs
   amendment. The live consuming edge on the measurement substrate is the `DirectSchemaSet` seam. Owed:
   an explicit named-supersession line (Constitution ix), not a quiet substitution.
2. **`Project.ty`@204 is single-owner; the thread is DISCHARGED, the CONSUME and the CONTRACT are
   not.** PROTO-1 EXECUTED the thread (RED→GREEN, zero footprint) — assign it to **exactly one owner**
   (a GN-3 wave, `no-value-discard`); 3A verifies, 3B threads as `attrs.node_type`, 3C declares, 3D
   erases as S0 — the other three **CONSUME**. Two entangled obligations remain and are **not** funded
   by funding the thread: **(i)** the CONSUME half — `.target` is a dead field; true CONSUME (delete
   the schema param, synthesize `DirectSchemaSet` from `sink_only`, resolve JSON's zero-arrow corpus)
   sits **inside G-3B-1's fold**; **(ii)** the consistency CONTRACT over the threaded value (3A) is a
   **separate born-RED obligation on LOCK-1** (name→`Type` resolution) — the thread is *presence*, the
   contract is *consistency*; do not let "PROTO-1 discharged the thread" read as "the contract is
   discharged." Entangled with XL-1: *what* consumes the value is the XL-1 dispute.
3. **Keep the decision-driver seat unmerged from LOCK-2 (XL-B, WORKING → provisional).** LOCK-3 owns
   *where* the driver sits (the fact contract at the L1→L2 edge); LOCK-2 owns *what engine*. 3A/3B
   respect this; 3D's per-site staging-form scheduling **stays LOCK-2's seat, gated on a real
   consumer** — a LOCK-3 hardcoded emitter branch for staging-form is incompatible with the boundary.
   The cessions to LOCK-2 (3C-C2, 3D staging-form) carry **"(provisional on XL-B)"** — XL-B is not
   final until GESTALT-B (XL-2).
4. **`no-ts-ir` holds** across 3C/3D — the meta-grammar emits TS *type declarations*, never a runtime
   TS IR; the backend-agnostic middle is **tape + schema** (PROVE-14), not a shared instruction IR.
5. **XL-B strengthened for GESTALT-B (evidence, non-blocking).** This pass's running evidence — the 3A
   oracle on live IR, the 3B fold slice byte-identical — strengthens LOCK-3's claim to the middle-IR
   tower's shape and the decision-driver seat; carry to GESTALT-B when XL-B finalizes.

---

## PASS HISTORY

### Pass 2 (folded by AGGLOMERATOR, Fable-tier)

- **Inputs:** research-{A,B,C,D,priorart} (pass-2 sweep, all pinned live `c43126422`), FAMILY-SPECS
  (frozen 15:10), four prototypes — PROTO-1 (`:204`-thread, RAN_MEASURED), PROTO-2 (fold fact-purity
  trace), **PROTO-1-t1** (seam-slice, RAN_MEASURED 15:34), **PROTO-2-3A** (refinement oracle,
  RAN_MEASURED 15:38) — CRIT-1 (within-lock), CRIT-2 (cross-lock).
- **Agglomerator verification (live `c43126422` = HEAD):** confirmed HEAD **is** the critics' pin
  (`git rev-parse` = `c43126422423…`); base-drift discharged (`git diff --stat` over `skinny/crates`
  empty for both prototype ancestors → prototype numbers transfer to HEAD verbatim); re-greped `:204`
  ty-drop, `:203` non-defect, `has_rule("value")`@98 survival, `DirectTypeKind` Struct-only; and
  **verified XL-1** — `TypeAnalysis`/`from_ir` absent from live skinny, present only in legacy
  `crates/core`. **The critics did not err; the FAMILY-SPECS was stale** (cut 24–28 min before its two
  sharpest prototypes ran). Floors taken unadjusted.
- **Process correction:** re-folded all four artifacts; corrected the SPINE table's stale "3A oracle
  UNRUN" (→ RAN on E3) and "3B-G1 UNRUN" (→ whole fold unrun; one slice ran, `dispatch_alt_count`
  refuted) rows; re-cut both advancing families' gap-sets against the RAN prototypes.
- **Dispositions set:** 3A ADVANCED-discipline **60%** (↑55; organ RAN, contract vacuous-until-LOCK-1);
  3B ADVANCED-substrate **55%** (↑50; one byte-identical slice, killer fold gate UNRUN, net-LOC
  contingent on unrun re-walk); 3C BLOCKED **32%** (↑30; C1/C2 split the analysis progress, distinctive
  0% eligible); 3D BANKED **38%** (=38; nothing distinctive ran, leans on retired-target/shipped/shared/
  dead-gate evidence).
- **Redirects:** hybrid dissolution reaffirmed (evidence both sides); 3D-staging-spine stays banked into
  3B; seam-dissolution single-funded to 3B **decisively by execution** (XL-3). No new redundancy banked;
  the underexplored formulation to open is the unrun E1-infer non-vacuity half of the 3A organ.
- **Cross-pollination:** 3A refinement-oracle ⟶ 3B attributes-fold upgraded to **partially executed**
  (E3 arm on live IR).
- **Cross-lock:** **XL-1** (`from_ir` consumer contradiction) surfaced as a named-supersession challenge
  to GESTALT-B with verified evidence; XL-2 (cessions provisional on WORKING XL-B) carried; the `:204`
  single-owner thread's CONSUME + CONTRACT halves separated (thread ≠ contract; both open).
- **Lock-wide:** no family convergence-eligible (pass 2 of ≥3; each carries ≥1 UNRUN load-bearing gate;
  two consecutive clean passes + fresh non-author audit outstanding for all four).

### Pass 1 (folded by AGGLOMERATOR, Fable-tier)

- **Inputs:** research-{A,B,C,D,priorart}, FAMILY-SPECS, PROTO-1 (level-count adjudication, RAN_MEASURED),
  PROTO-2 (PROVE-12 corollary, RAN_UNMEASURED/REFUTED), CRIT-1 (within-lock), CRIT-2 (cross-lock).
- **Agglomerator verification (live `b3cb8c16b`):** confirmed both critics' crux facts — 203=`Annotation`
  (non-defect) / 204=`Project` ty-drop; 51-commit staleness of the PROTO baseline; CSS IR-driven via
  `css_scan_direct`/`ScanAlphabet` (`CSS_GENERATED_RS` gone); `find_component_delim` retired from
  `codegen/src`; `binding_times`/`Stage` absent from live `ir/`; `has_rule("value")`@98 survives;
  `DirectTypeKind` Struct-only. **The critics did not err; FAMILY-SPECS §0 over-graded on stale/mis-probed
  evidence.** Floor taken unadjusted.
- **Dispositions set:** 3A ADVANCED-as-discipline (tower formulation BANKED) 55%; 3B ADVANCED-substrate 50%;
  3C BLOCKED 30%; 3D BANKED (PROVE-12-`u128` gate RETIRED) 38%.
- **Redirects:** 3A-tower⊗3B dissolved to hybrid; 3D-staging-spine banked into 3B attribute store;
  seam-dissolution single-funded to 3B. Cross-pollination: 3A refinement-oracle ⟶ 3B attributes fold.
- **Convergent-spine hypothesis (architecturally sound, rests on three unrun/refuted gates):** one IR
  (3B) + per-pass contracts (3A discipline) + a stage attribute (3D erasure), **3C banked** until
  LOCK-1 (sums) + LOCK-5 (functor) + a named wave unblock it. The gating gates: 3B-G1 byte-identity
  collapse (UNRUN), the 3A refinement oracle + `Project`@204 re-anchor (UNRUN/mis-run), the 3D live
  re-cut (REFUTED-as-posed).
- **Lock-wide:** no family convergence-eligible (pass 1 of ≥3; the baseline-staleness defect alone bars
  all four this pass).

<!-- Pass 3 appends below this line -->
