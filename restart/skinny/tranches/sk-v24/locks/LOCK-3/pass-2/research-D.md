# LOCK-3 · Family 3D — THE STAGED-RESIDUAL FORMULATION (pass-2)

Base pin: `skinny@061c79fa7581ec42f0d39f778c1f19dc7e65ed26` (live HEAD; the sole
measurement substrate). The legacy `bbnf-lang` `crates/core` tree is a **port-target
reference only**, never a measurement surface — this distinction is load-bearing below
(§3, R-C). All `path:line` verified at this HEAD unless prefixed `crates/core/`.

---

## 1. The thesis, made concrete

The IR carries a **binding-time (BT) fact per node**: every `BackendExpr`/`ExprId` is
either *stage-0* (its value is fixed by the grammar, known at generation) or *stage-1*
(its value depends on runtime input). Residualization is nothing but **erasing every
stage-0 node into a generator-time constant and emitting only the stage-1 residual**.
This is offline partial evaluation over the grammar IR: the two-level λ-calculus of
Nielson & Nielson applied to `GrammarIr`, with the generator as the `mix` specializer.

Two disciplines already ship on live as *un-labeled corollaries of this idea* — the
family's job is to make the label explicit and prove it accounts for both:

- **The alphabet/STOP-set island (the declared dependency, not re-authored).**
  `ScanAlphabet::derive_alphabet` (`crates/codegen/src/lower/css_scan.rs:487-570`)
  harvests the CSS structural bytes from *grammar rule terminals*
  (`ruleBlock`→`{}`, `atRuleBody`→`;`, declaration→`:`, `cssString`→quotes) at
  generation time and renders them as `const AT_RULE_STOP: &[u8]` … `COLON_BEFORE_STOP`
  (`crates/codegen/src/css_scan_direct/mod.rs:112-149`, dispatched by
  `css_scan_direct/drivers.rs:51-262`). JSON's analogue is `STRUCTURAL_ALPHABET`
  (`crates/runtime/src/grammars/json/scan.rs:19`, a `const StructuralAlphabet`). These
  consts **are** the stage-0 residual of a scan whose loop is the stage-1 program. The
  island is not synthetic: it is exposed on the live emit path.
- **PROVE-12 (residualization) is a strict corollary.** PROVE-12 const-folded the CSS
  delimiter-membership test into a `const`-generic `u128` bitmask
  (`find_component_delim::<{ delim_mask(b";{}") }>`), collapsing three runtime slice
  loads + two length guards into two immediate `and` masks — measured −3.7…−6.4% ins/B,
  0.81-0.98× cyc/B, byte-identical parity, +18 generated LOC. In BT terms: the
  delimiter set was a *stage-0 node still being interpreted at stage-1*; residualization
  moved it to where the BT fact already said it belonged. The framing predicts PROVE-12;
  it does not merely accommodate it.

The mechanism is therefore: **(a)** a `binding_times` analysis over the real
`BackendIr`; **(b)** a residualizer that specializes each stage-0 frontier node via the
host's own staging primitives — Rust `const fn` + const-generics (exactly PROVE-12's
seam) and monomorphization-as-specialization (the first Futamura projection in
miniature); **(c)** a BT fact on the type sub-plane whose stage-0 erasure *is* the
zero-runtime-cost type-plane invariant (Constitution v).

---

## 2. The substrate

### 2a. The stage fact — an attribute, not a new sum arm
Attach BT as a companion map over stable node ids, in the annotating-pass shape (do
**not** grow the `BackendExpr` sum, per §14/parsimony and no-value-discard). Home it on
`ShapeFacts` (`crates/ir/src/lib.rs:357`) or as a sibling of the recognizer/shape facts,
keyed the same way `admits_collapsed_stage` already keys per-rule decisions:

```
enum BindingTime { Static, Dynamic }          // two-level; Nielson & Nielson
struct StageFacts { node_bt: Vec<(NodeId, BindingTime)> }
```

Producer: a new `binding_times(&BackendIr) -> StageFacts` pass sited beside
`derive_backend_shape` (`crates/passes/src/lib.rs:398`) and `admits_collapsed_stage`
(`:959`). Rule of the analysis (offline BTA, Jones/Gomard/Sestoft): `ByteLiteral`,
`RegexProgram` pattern text, `SpanMark` labels, `TapeEmit.kind`, `DirectBuild.shape`,
and every alphabet/terminal that `derive_alphabet` reads are **Static**; the per-byte
loop over `source[..]` and any input-classified branch are **Dynamic**. A node is Static
iff all its inputs are Static (monotone lub — the standard BTA fixpoint, LLVM-SCCP-shaped).

### 2b. Relation to `CollapsedStage` — the live "stage" shape is DORMANT (a real gap)
`BackendShape::CollapsedStage` (`crates/ir/src/lib.rs:366`) is the closest extant
"stage" surface: a whole-rule decision (`P3CollapsedStage`,
`crates/ir/src/cost.rs:267,279`; rationale `CollapsedStageAdmissible`, `cost.rs:258`;
cost `LocalTempOnly`/`LocalLoop`, `cost.rs:131-135`), lowered by a 17-LOC emitter
(`crates/codegen/src/lower/collapsed_stage.rs`) through `tape_plan.rs:9-53`
(`fuse_span_stage`/`commit_fused_stage`). **But its admissibility predicate is
`target.avx512bw && matches!(rule_ir.expr, BackendExpr::Entry(_))`
(`crates/passes/src/lib.rs:959-961`)** — gated on an x86 feature that is abrogated
(refutation-law: x86 dead). On aarch64 live it **never fires**. So the extant "stage"
concept is (i) *coarse* (whole-rule, not per-node) and (ii) *inert*. The family's
substrate **generalizes CollapsedStage from a dead whole-rule shape into a live per-node
BT fact**, and either revives the shape under a real aarch64 admissibility predicate or
retires it in favor of the node-level residualizer — a named supersession, not silent
contradiction (Constitution ix).

### 2c. The type sub-plane as a BT sub-lattice
The `->` projection type is a Static value: `ExprKind::Project { body, target, ty }`
(`crates/ir/src/lib.rs:261-265`), where `ty: Option<String>` is a generation-time
token. "Type-plane erasure = stage-0 erasure on the type sub-plane" is literal: thread
`ty` as a Static residual whose emission is a **Rust type** (a `DirectTypeRef`), which by
construction costs zero at stage-1. This is Nielson's two-level *type* discipline and
LMS's `Rep[T]` marker (Rompf & Odersky): the binding time lives *in the type*.

---

## 3. The `->` thread as stage-0 residual (the Shared-directive edge)

**The discard.** `crates/passes/src/lib.rs:204`:
`ExprKind::Project { body, .. } => self.infer_expr(*body)?` drops `target`/`ty`; lowering
carries them verbatim into `BackendExpr::ValueProject` (`:1349`) but
`DirectBuildField.target` is filled `None` on the offset-tape path (R6 §5d).

**The consumption contract (how 3D consumes a threaded node-type rather than
re-deriving it).** The Static `ty` is read *once* by the direct-to-struct emitter as the
specializing constant of a stage-0 residual: `ty` → `DirectBuildField.target` →
`DirectTypeRef` → the emitted Rust `struct`/`enum` field type in
`crates/codegen/src/json_typed_direct.rs:10` over `direct_schema.rs` (`DirectTypeKind`,
Struct-only today at `:52`). The anti-pattern this retires is the **re-derivation** at
`crates/codegen/src/emit/general_descent/view.rs:20-46` (`classify_projection` folding
every arrow to `{Float,Bool,Tag,Span}`) — a stage-1-shaped re-classification of a
stage-0 fact. Erasure means the fold dies and the type is consumed once, at stage-0.

**The cross-tree caveat (R-C, sharp).** The Shared directive names the downstream
consumer as `TypeAnalysis::from_ir`. That symbol exists **only in the legacy tree** —
`crates/core/src/backend/driver/analysis.rs:16-31`, reading `ir.types` +
`ir.type_map.scratch_types()` over a real `bbnf_ir::TypeDesc`. **Skinny has no `TypeDesc`
and no `TypeAnalysis`** (R6 §5: `rg TypeDesc` → 0 hits on live). Therefore threading
`:204`'s `ty` "into that consumer" is a *port-target* statement, not a skinny seam. On
live skinny the honest stage-0 consumer is `direct_schema` + `json_typed_direct`, and
the wire is: (1) stop discarding at `:204`; (2) fill `DirectBuildField.target` in
`derive_materialization_plan` (`:1136-1391`, currently `None`); (3) emit as a
`DirectTypeRef`. Claiming the `crates/core` consumer as the skinny thread would be a
consumer-substitution error the family must not commit.

---

## 4. Integration seams (path:line, live HEAD)

- Stage fact home / producer: `crates/ir/src/lib.rs:357` (`ShapeFacts`); new pass beside
  `crates/passes/src/lib.rs:398` (`derive_backend_shape`) and `:959`
  (`admits_collapsed_stage`).
- Stage-0 island evidence (declared dependency): `crates/codegen/src/lower/css_scan.rs:487`
  (`derive_alphabet`); consts `crates/codegen/src/css_scan_direct/mod.rs:112-149`;
  drivers `crates/codegen/src/css_scan_direct/drivers.rs:51-262`; JSON const
  `crates/runtime/src/grammars/json/scan.rs:19`.
- Residualizer seam (PROVE-12 corollary): the `const fn mask` + const-generic
  monomorph pattern generalizes to any grammar-derived STOP/dispatch table; JSON
  first-byte dispatch site at `crates/codegen/src/json_sink_direct/dispatch.rs`
  (`derive_arms`, first-byte keys) — the NEW measurement site for Q3.
- Type sub-plane thread: `crates/passes/src/lib.rs:204` (discard), `:1349`
  (`ValueProject` carry), `:1136-1391` (`target` None-fill); `direct_schema.rs:52`
  (`DirectTypeKind`, Sum gate); `json_typed_direct.rs:10` (emitter); fold to retire at
  `emit/general_descent/view.rs:20-46`.
- Dormant stage shape: `crates/ir/src/lib.rs:366`; `cost.rs:131,258,267`;
  `passes/src/lib.rs:959`; `lower/collapsed_stage.rs`; `lower/tape_plan.rs:9-53`.
- Port-target consumer (NOT a skinny seam): `crates/core/src/backend/driver/analysis.rs:16-31`.

## 5. Boundary compliance

LOCK-3 owns **that** the BT fact exists and **where** it attaches (the fact contract at
the IR edge); LOCK-2's engine owns the per-site *decision of which* Static frontier
nodes to actually residualize (cost/CSP over monomorph-count vs ins/B — the
generated-size-budget is the constraint). Declared as a dependency edge, not
re-authored here. `no-ts-ir` holds: the staged split lives in the Rust generator; any
TS face emits type declarations (stage-0 residuals), never a runtime IR (XL-D).

---

## 6. Prior art (citations)

- **Jones, Gomard & Sestoft, *Partial Evaluation and Automatic Program Generation*
  (1993)** — offline partial evaluation, binding-time analysis, the `mix` equation, the
  Futamura projections. The generator specializing a scan loop to a grammar's STOP-set
  is the *first* Futamura projection; PROVE-12 makes the residual branchless.
- **Nielson & Nielson, *Two-Level Functional Languages* (1992)** — the two-level
  λ-calculus and *type-theoretic* BTA that grounds §2c: the stage lives in the type.
- **Taha & Sheard, MetaML / MSP (2000); MetaOCaml** — explicit staging annotations
  (`.< >.`, `.~`, `.!`); the model for making the stage boundary syntactically visible.
- **Rompf & Odersky, Lightweight Modular Staging (LMS, 2010) / Delite** — `Rep[T]` as
  the binding-time marker inside a host type system; the direct analogue of threading
  `ty` as a Static residual to a Rust type.
- **Zig `comptime`** — the production exemplar of a comptime/runtime split encoded in
  the types; residualization is automatic once a value is comptime-typed.
- **Typed Template Haskell (`Code Q a`)** — a well-typed splice: stage-as-type in a
  production compiler.
- **Rust `const fn` + const generics + monomorphization** — the host's own staging
  mechanism, already exploited by PROVE-12; monomorphization = specialization.
- **LLVM SCCP / MLIR `-sccp` + `-canonicalize`** — sparse conditional constant
  propagation is a binding-time-driven const fold; the BTA fixpoint is SCCP-shaped.
- **`regex-automata` `ByteClasses` / alphabet compression** — separating a *static*
  alphabet from *dynamic* input is the same stage split at the automata layer.

---

## 7. Named risks & counterexamples

- **R-A · Degenerate BTA.** Because the whole grammar is known at generation, a naive
  BTA marks *everything* except the input bytes Static — the split is trivial and buys
  nothing. The useful boundary is narrow: Static = grammar-derived constants (alphabets,
  dispatch tables, type tags); Dynamic = the per-byte loop. The family must show the
  *frontier* (Static nodes with Dynamic consumers — precisely the delimiter set PROVE-12
  hit) is non-empty and that residualizing exactly it (not more) wins.
- **R-B · Dormant `CollapsedStage`.** The one live "stage" surface is gated on
  `avx512bw` (`passes/src/lib.rs:960`) and never fires on aarch64. "The stage fact
  already lives on live" is false for the *shape*; it is true only for the *derived
  consts*. The family must revive-or-retire CollapsedStage explicitly.
- **R-C · No skinny consumer for `ty`.** `TypeAnalysis::from_ir`/`TypeDesc` exist only in
  `crates/core` (§3). Threading `:204` to that symbol is cross-tree; the skinny consumer
  is `direct_schema`. Consumer-less substrate is forbidden (Y.13 / Constitution viii):
  the BT fact and the threaded `ty` must land with their emitter consumer *same-wave*.
- **R-D · Monomorph explosion.** Over-residualizing (a distinct const-generic monomorph
  per site per grammar) bloats generated code; PROVE-12 saw 3 monomorphs collapse to 1
  by inlining — favorable, but not guaranteed. The generated-size-budget gate is the
  teeth; LOCK-2's cost model owns the trade.
- **R-E · Capacity hints are corpus facts, not grammar facts.** GN-3 R1: the hand schema
  carries `875`/`184`/`10_800`/`5_759` from *counting documents*. A grammar-sourced BTA
  cannot mark these Static — they are not derivable from grammar structure. Counterexample
  to "all Static facts derive from the grammar"; such hints must enter as explicit
  annotations or be excluded from the stage-0 set.
- **R-F · Stale PROVE-12 base.** PROVE-12 measured at `d2b32f6e3` (sk-v22). On live the
  CSS path already routes through `css_scan_direct` with const STOP arrays; the prototype
  must confirm whether the const-generic `u128` mask residual is *already live* or must be
  re-applied at the JSON dispatch site (XL-H: stale-base numbers are wrong-artifact).

---

## 8. What a prototype must test (the movers)

1. **`binding_times` on the REAL IR (not a mirror).** Run the pass over the live
   `css_l4_declaration_values` and `json` `BackendIr` (from `passes::compile`) and dump
   the per-node label. **Pass iff** the alphabet/STOP-set frontier lands Static and the
   per-byte scan loop lands Dynamic, with the `derive_alphabet` island provably a Static
   node the live emit path exposes. Evidence: the executed label map, keyed to real
   `NodeId`s.
2. **Type-plane erasure, concrete on skinny.** Fill `DirectBuildField.target` from
   `Project.ty` (kill the `None`), emit it as a `DirectTypeRef`, and show the type-plane
   costs zero: `git diff --name-only <base>..HEAD | grep crates/runtime/` **empty**. The
   consumption contract: the emitter specializes on the threaded type; the
   `classify_projection` re-derivation (`general_descent/view.rs:20-46`) is deleted.
3. **The JSON first-byte dispatch NEW site (Q3, PROVE-09 discipline).** Const-fold the
   dispatch table at `json_sink_direct/dispatch.rs` and produce **cold paired-PMU ins/B**
   on `json_parity` — the measured delta, re-pinned to `061c79fa7`. Unmeasured
   plausibility does not count; the number is the deliverable.
4. **PROVE-12 as corollary, re-pinned.** Show the delimiter-mask residual is exactly
   stage-0 erasure of a node the BTA already labeled Static; regen byte-identical, parity
   cksum unchanged, on live HEAD.
