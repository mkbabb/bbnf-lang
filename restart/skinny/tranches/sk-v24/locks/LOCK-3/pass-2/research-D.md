# LOCK-3 · Family 3D — THE STAGED-RESIDUAL SPINE (pass-2 research)

The IR is an explicitly two-stage program. Every node carries a binding time — stage-0
(generator-time, *erased*) or stage-1 (runtime, *emitted*). Residualization is not a
special optimization bolted beside the tower; it **is** the erasure of the stage-0
sub-graph. Type-plane erasure and delimiter-mask const-folding are then the *same*
phenomenon witnessed on two sub-planes of one binding-time analysis, not two machineries.
That single-mechanism claim is this family's center of gravity, and its whole burden of
proof.

Measurement substrate: `skinny` @ `06e70053f` (live HEAD; the sole living implementation).
Every `path:line` below was opened and verified against that tree. Prototype/PMU deltas are
UNMEASURED on live except PROVE-12 (declared, not re-authored) — stated as obligations,
not claims (per the shim-refutation law).

---

## 1. The mechanism, made concrete

### 1.1 The two-level type over the grammar IR

Binding-time analysis (BTA) is, per Nielson & Nielson, *a form of type checking over a
two-level language* — the two-level λ-calculus splits every computation into an early
(compile-time) and a late (run-time) part, and BTA is the congruence that assigns those
levels ([Nielson & Nielson 1988](https://researchr.org/publication/NielsonN88:2);
[Nielson, PEPM'91](https://dl.acm.org/doi/10.1145/115865.115881)). Lightweight Modular
Staging makes the pragmatic move this family adopts wholesale: *use only types to
distinguish the two binding times*, no syntactic quotation
([Rompf & Odersky, CACM 2012](https://m-cacm.acm.org/magazines/2012/6/149801-lightweight-modular-staging/fulltext)).
The bbnf generator is, in Futamura's terms, the second projection — a compiler obtained by
specializing a parse interpreter with respect to a grammar
([Futamura projections, arXiv:1611.09906](https://arxiv.org/pdf/1611.09906)); MLIR's
progressive lowering is the same staged-residual shape at dialect granularity
([MLIR lowering paths](https://apxml.com/courses/compiler-runtime-optimization-ml/chapter-2-advanced-ml-intermediate-representations/mlir-lowering-paths)).

Concretely for LOCK-3: a lattice `Stage = { S0, S1 }` (S0 ⊑ S1; join = S1), and a pass
`binding_times(&GrammarIr) -> StageMap` keyed by `ExprId`. A node is **S0** iff every one
of its data inputs is a grammar constant (a `Literal`, a `Regex` pattern, a projected `ty`,
a derived alphabet) reachable without consuming input bytes; it is **S1** iff any input is
the parsed byte stream. The congruence rule is Nielson's: an operator is S0 only when all
operands are S0. Residualization = *evaluate the S0 sub-graph at generation time, delete it
from the emitted program, and emit only the S1 frontier — specialized w.r.t. the S0 values it
consumes.* That is program specialization after BTA, verbatim.

### 1.2 PROVE-12 is the corollary, not the theorem

PROVE-12 const-folded the CSS component-delimiter test: the delimiter set (`b";{}"`, …) is a
grammar constant threaded as a runtime `&[u8]` and re-scanned with `<[u8]>::contains` on
every byte; the fix makes `find_component_delim<const DELIMS: u128>` and bakes the mask as an
immediate. In binding-time terms: the delimiter set is **S0**; the per-byte membership loop
is **S1**; specializing the S1 loop with respect to the S0 mask *is* the residual. The Rust
`const`-generic monomorphization is the specializer, and the emitted immediate is the residual
program. Measured (declared): ins/B −4.8…−6.8% deterministic, cyc/B 0.80–0.91×, regen
bit-for-bit, +18 generated LOC (both REPORT and the non-author VERIFY hold, base `d2b32f6e3`).
The family's claim is that this hand-authored win is one instance of a general emit rule keyed
on the stage fact — not a bespoke CSS patch to be re-authored per site.

---

## 2. The substrate on live — enumerated stages and the stage-0 islands that already ship

### 2.1 The transposition levels (the edge-DAG the stage fact rides)

Live has a **two-level tower**, not three: L1 `GrammarIr` (`ir/src/lib.rs`, `ExprKind`
10-arm @ `:230`–`:271`) → decision driver → L2 `BackendIr`/`BackendExpr` (13-arm @
`ir/src/lib.rs:376`–`413`) → emit (Rust text, `codegen`). `BackendShape` is a 5-value
attribute on L2 (`EagerTape|OffsetTape|EventTape|SinkOnly|CollapsedStage`,
`ir/src/lib.rs:361`). The `materialize_rule` framing (SpanMark→body→SpanMark→TapeEmit→
DirectBuild→Return) is an *emit-order derivation over L2 arms*, not a distinct IR level.
Per the Boundaries note, LOCK-3 owns **where the driver sits** = the fact contract at the
**L1→L2 edge**: the `StageMap` is an attribute *produced over L1 and consumed at that edge*,
exactly where `derive_recognizers` (`passes/src/lib.rs:338`) and `derive_materialization_plan`
already read L1 to author L2. The stage fact is one more attribute on that same edge, not a
new pass pipeline.

### 2.2 Stage-0 island #1 — the grammar-derived stop-set (DECLARED, not re-authored)

This island already ships as a stage-0 residual on live; the family declares the dependency:

- CSS: `derive_alphabet(&ScannedGrammar) -> ScanAlphabet` (`codegen/src/lower/css_scan.rs:487`)
  harvests `{`/`}`/`;`/`:`/quotes/`/`/balanced-pairs from named rules at *generation time*,
  then `render_alphabet_consts` bakes them as `const … : &[u8]` (`codegen/src/css_scan_direct/mod.rs:112`)
  feeding the SIMD scan (`css_scan_direct`). The alphabet is computed early and emitted as a
  constant — a textbook S0 residual.
- JSON/structural: `derive_recognizers(&GrammarIr) -> Vec<Recognizer>`
  (`passes/src/lib.rs:338`) folds single-byte `Literal`s and quoted-string `Regex`es into
  `StructuralAlphabet { bytes }` (`:361`–`365`) → `Recognizer::SimdScan { mode, alphabet, site }`
  (`ir/src/lib.rs:416`). Same S0 harvest, different sink.

**The family's contribution is not to re-derive these** — it is to prove, via
`binding_times`, that these nodes are *provably S0 and the live emit path already realizes
the erasure*. A stage fact that merely re-labels an island the emitter already residualizes
is a consumer-less taxonomy (Constitution viii, Y.13) — forbidden. The consumer that makes
the fact load-bearing is §3.2.

### 2.3 Stage-0 island #2 — the type sub-plane (the `->` erasure)

There is no `Stage`/`binding_time`/`comptime`/`residual` construct anywhere on live (grep:
zero hits) — this substrate is entirely net-new, and must land with its consumer in the same
wave. The one stage fact that already flows unnamed is the type plane: Constitution (v)
mandates the type plane cost **zero at runtime**, which is precisely the statement *the type
sub-plane is entirely S0 — every type fact must be erased before the S1 frontier.*

---

## 3. Threading one node-type as a stage-0 residual (the Shared question, consumed not re-derived)

### 3.1 The wire, end-to-end on live

`ExprKind::Project { body, target, ty: Option<String> }` (`ir/src/lib.rs:261`–`265`) carries
the projected type as a stage-0 fact. It is dropped at `passes/src/lib.rs:204`
(`ExprKind::Project { body, .. } => self.infer_expr(*body)?` — `ty` discarded via `..`). The
end-to-end thread is:

1. `binding_times` marks the `Project` node **S0** (its `ty` is a grammar constant).
2. Inference already stores the *body's* structural type:
   `self.facts.node_types.insert(expr_id, ty.clone())` at `passes/src/lib.rs:212`. The
   threaded arrow reads this **existing** map entry — it does not stand up a parallel
   inference. The S0 `ty` string is unified against `facts.node_types[body]` once.
3. The single consumer on live is `derive_materialization_plan` (`passes/src/lib.rs` ~`:1136`–
   `:1290`), which fills `DirectBuildTarget` (`ir/src/lib.rs:483`–`490`) — whose
   `type_ref: DirectBuildTypeRef` (`:485`, `Named|Scalar` @ `:493`) is emitted as a Rust
   **type** (a struct-field type / enum arm), never a runtime value. Today that target is
   hardcoded `None` on the offset-tape path (`ir/src/lib.rs:470`,
   `pub target: Option<DirectBuildTarget>`). Threading = fill it from the resolved leaf.

**Consumption contract (must-state):** the staged family CONSUMES one threaded node-type at
step 3; it does not re-derive it. The stage annotation is the *wire* that carries `ty` from
`:204` to the `DirectBuildTarget` fill, and BTA is the *proof* that the wire is erasable
(S0). "Type-plane erasure = stage-0 erasure on the type sub-plane" is then literal: the S0
`ty` becomes the *name of a Rust type* in generated text (`DirectBuildTypeRef::Named`), which
by construction costs zero runtime instructions — the LMS "types distinguish the stages"
principle realized against a real emitter. The erasure is witnessed by a byte-identical regen
with the stage fact on/off (zero emit footprint on the runtime crate).

### 3.2 The general residual emit rule (the load-bearing consumer)

The stop-set island (§2.2) and the type island (§2.3) share one emit rule: *at every S0→S1
frontier, specialize the S1 node w.r.t. the S0 value.* PROVE-12 is that rule applied by hand
to one frontier (delimiter mask). The consumer that discharges Y.13 is the rule made general:
`binding_times` locates the frontier, and the emitter emits the const-generic / const-baked
specialization *without a per-site hand patch*. This is what lifts the fact above a relabel —
it fires at the JSON structural scan and general-descent stop-sets with the same mechanism,
and it is where a NEW measured site lives (§6.3).

### 3.3 Note on the withheld `TypeAnalysis::from_ir` consumer

The Shared question names `backend/driver/analysis.rs` `TypeAnalysis::from_ir` as the
downstream consumer. **That path does not exist on live** (grep: zero hits for `TypeAnalysis`
/ `from_ir` across `crates/`) — it belongs to a greenfield layout, not `06e70053f`. On live
the sole type-consuming sink is the `DirectBuildTarget` fill (§3.1). The family therefore
threads to the *real* consumer, and carries "a `TypeAnalysis::from_ir` consumer" as a
born-RED dependency edge on the greenfield port (owner: the GN-1 loom), not as a live claim.

---

## 4. Prior art (citations)

- **Binding-time analysis / two-level λ-calculus.** Nielson & Nielson, *Automatic binding
  time analysis for a typed λ-calculus* (Sci. Comp. Prog. 1988) — BTA over products, sums,
  lists, recursive types, expressed as type checking; the direct model for `binding_times`
  over `ExprKind`. [researchr](https://researchr.org/publication/NielsonN88:2) ·
  [Nielson PEPM'91](https://dl.acm.org/doi/10.1145/115865.115881) ·
  [two-stage splitting, ESOP'16](https://link.springer.com/chapter/10.1007/978-3-662-49498-1_11).
- **Types-as-stages.** Rompf & Odersky, *Lightweight Modular Staging* (CACM 2012) — stages
  distinguished by types only, ANF-preserved; the model for "type-plane erasure = stage-0."
  [CACM](https://m-cacm.acm.org/magazines/2012/6/149801-lightweight-modular-staging/fulltext).
- **Futamura projections.** The generator = 2nd projection; specializing the parse
  interpreter w.r.t. a grammar. [arXiv:1611.09906](https://arxiv.org/pdf/1611.09906).
- **Staged lowering at scale.** MLIR progressive dialect lowering as the industrial
  staged-residual tower. [MLIR lowering paths](https://apxml.com/courses/compiler-runtime-optimization-ml/chapter-2-advanced-ml-intermediate-representations/mlir-lowering-paths).
- **Templates-as-partial-evaluation.** The C++ template ↔ specialization correspondence,
  precedent for Rust const-generics as the specializer.
  [arXiv:cs/9810010](https://arxiv.org/pdf/cs/9810010).

---

## 5. Named risks and counterexamples

- **R-A — S0-ness is necessary, not sufficient, for a win (the sharpest).** A node being
  stage-0 does not mean residualizing it is faster. PROVE-04 refuted block-scheduling
  (the M-class OoO core already pipelines); PROVE-02 refuted the digit-scan (latency-bound
  inversion). PROVE-12 won *because it deleted interpretive dispatch* (slice loads + length
  guards), not merely because the set was constant. Counterexample: the JSON first-byte
  dispatch `match byte { … }` (`grammars/json/generated.rs:105`) is S0-keyed, but LLVM may
  already lower it to a jump table — residualizing it could be neutral. Therefore *which*
  frontier to specialize is a **cost decision owned by LOCK-2** (`ir/src/cost.rs`,
  `PriorityStep`); LOCK-3 owns only the stage *fact contract* at the L1→L2 edge. The two must
  stay distinct (Boundaries note) — the family does not re-author the engine.
- **R-B — consumer-less relabel.** The stop-set is *already* residualized (const baked). A
  stage fact that only annotates it is forbidden substrate. Mitigation is §3.2: the fact must
  drive the *general* emit rule that fires at un-residualized frontiers, or it is dead.
- **R-C — `ty` is a bare `Option<String>`, not a resolved type.** The projection vocabulary
  is unresolved text (`f64`/`Span`/`Nu8`, folded lossily to 4 categories on the general-descent
  path). BTA cannot unify strings. Threading therefore *consumes* LOCK-1's resolved type (HM
  output), it does not itself infer — a born-RED dependency edge, owner LOCK-1. Staged-residual
  is the erasure discipline over LOCK-1's lattice, not a rival solver.
- **R-D — net-new substrate, Y.13.** No stage construct exists on live; `binding_times` +
  `StageMap` + the emit rule must land with a consumer same-wave (the §3.2 rule + the §3.1
  `DirectBuildTarget` fill), never as a taxonomy pass ahead of its reader.
- **R-E — the emit-order derivation must be recomputable, not carry hidden data.** For the
  stage fact to ride the L1→L2 edge cleanly, `materialize_rule`'s SpanMark/TapeEmit/DirectBuild
  ordering must be a pure function of L1 + `StageMap`; any datum it carries that is absent from
  `GrammarIr` breaks the single-fold story and must be surfaced into the IR.

---

## 6. Exactly what a prototype must test to move this family

1. **Soundness of the stage fact on REAL IR.** Implement `binding_times(&GrammarIr)` and run
   it on the *live* css-l4 IR (roots `grammar/css/l4/stylesheet.bbnf`) and json IR
   (`skinny/grammars/json.bbnf`) — not a synthetic mirror. Assert: (a) the alphabet-deriving
   nodes feeding `derive_alphabet`/`derive_recognizers` come out **S0**; (b) the per-byte scan
   loop + `TapeEmit` come out **S1**; (c) the S0 set equals what the live emitter already bakes
   const (`render_alphabet_consts`) — proving the fact exposes a real stage-0 island the emit
   path realizes, not an invented one.
2. **The type sub-plane erasure, threaded on live.** Thread one JSON leaf `Project.ty` (a
   string leaf) as S0 → `DirectBuildTarget.type_ref` (`ir/src/lib.rs:485`, today `None`) →
   emitted as a Rust type. Gate: regen **byte-identical** with the thread on/off over
   `crates/runtime/` (zero runtime footprint — Constitution v proven, not asserted), and the
   `DirectBuildTarget` non-`None` for the threaded leaf.
3. **The general residual rule at a NEW site, MEASURED.** Generalize PROVE-12's const-fold as
   the S0-erasure emit rule and fire it at the JSON first-byte dispatch
   (`grammars/json/generated.rs:105`). Cold, paired, N≥200 PMU ins/B on the json canon bench
   first-byte lane. Report the delta *or the NO_GO* honestly — unmeasured plausibility does not
   count, and R-A predicts this specific site may be neutral (already a jump table). A neutral
   result is a legitimate negative that still validates the mechanism at site #1 (the
   stop-sets), where PROVE-12's −4.8…−6.8% ins/B is the measured floor.

The family advances iff (1) proves the fact sound on real IR, (2) proves the type erasure
byte-identical, and (3) yields a measured ins/B number (win or named negative) at one new
frontier — binding the stop-set residual and the type-plane erasure under one binding-time
pass with one emit rule, cost-gated by LOCK-2 and typed by LOCK-1.
