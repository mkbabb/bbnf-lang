# LOCK-3 · Family 3D — STAGED-RESIDUAL · pass-2 research

Live substrate re-pinned to `skinny@3f2350f15` (HEAD). Every `path:line` below opened
against that tree. All paths under `/Users/mkbabb/Programming/bbnf-lang/skinny/` unless noted.

The formulation: the IR is an explicitly two-stage program — a comptime (stage-0)
sub-program the generator evaluates away, and a runtime (stage-1) sub-program over input
bytes. Residualization is *erasure of stage 0*. The mechanism reifies binding-time as a
first-class IR attribute so that "this is stage-0, erase it" is a checkable fact, not an
implicit consequence of which function the generator happens to run.

---

## 1. The residual island already ships — declared, not re-authored

The grammar-derived stop-set is live and is a **complete, executed instance of stage-0
erasure**, three-deep:

1. **Stage-0 value construction** — `derive_alphabet` (`crates/codegen/src/lower/css_scan.rs:487`)
   reads the STOP/SKIP bytes *off grammar rule terminals*: braces from `ruleBlock`
   (`:489`), `;` from `atRuleBody` (`:500`), `:` from the declaration split (`:508`),
   `()`/`[]` from selectors (`:515`), quotes from `cssString` (`:532`), `/` from the `@ws`
   regex (`:552`). The result `ScanAlphabet` (`css_scan.rs:38-57`) exists **only in the
   generator** — a pure stage-0 value, absent from the runtime binary.
2. **Erasure to a stage-1 const** — `render_alphabet_consts`
   (`crates/codegen/src/css_scan_direct/mod.rs:112-158`) `writeln!`s that stage-0 value into
   `const AT_RULE_STOP: &[u8] = b";{}"` / `QUALIFIED_STOP` / `DECLARATION_STOP` /
   `COLON_BEFORE_STOP` (emitted at `crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:29-35`).
   The stage-0→stage-1 boundary is literally this write of a byte literal.
3. **Erasure into the instruction stream** — PROVE-12 (CONFIRMED GO) drives residualization
   one level deeper: `find_component_delim<const DELIMS: u128>` monomorphizes the membership
   test to a const shift-and-test, so the delimiter set becomes an *immediate operand*
   (`mov x27,#0x2800…`) and the runtime slice loads + length guards vanish. Measured on
   `css_canon_bench`: ins/B −3.7…−6.4% deterministic, cyc/B 0.81–0.98×, +18 generated LOC,
   parity byte-identical (PROVE-12/REPORT §3-6).

The same island exists on **two more live paths**, so the pattern is already plural (not a
CSS one-off): general-descent emits `const LEAF_STOP: &[u8] = {stop_array}` from a
grammar-harvested stop-set (`crates/codegen/src/general_descent/emit.rs:94,101`;
materialized at `crates/runtime/src/grammars/sheets/generated.rs:44`), and the JSON direct
dispatch emits `match byte { b'{' => … }` arms keyed on a **generation-time first-set**
(`crates/codegen/src/json_sink_direct/dispatch.rs:52` `first_bytes(program,&callee.expr)`;
`first_set.rs`). Family 3D **depends on** these three derivations and re-authors none. Its
contribution is the *fact* that makes their stage-0-ness provable and uniform.

---

## 2. Mechanism: `binding_times` — offline BTA as a monotone IR fold

**Substrate.** A two-point binding-time lattice `BindingTime ∈ {Static, Dynamic}` (Static ⊑
Dynamic), attached as an `Attributes` side-table keyed by `ExprId`/`RuleId` over
`GrammarIr`. This is textbook **offline binding-time analysis** (Jones–Gomard–Sestoft,
*Partial Evaluation and Automatic Program Generation*; the two-level λ-calculus of Nielson &
Nielson): the Static/Dynamic *division* is exactly the split `derive_alphabet` performs by
hand today.

**The fold is isomorphic to the one already live.** `Infer::infer_expr`
(`crates/passes/src/lib.rs:180-213`) is already a monotone fold that writes
`facts.node_types.insert(expr_id, ty)` at `:212`. `binding_times` is the *same traversal*
with a 2-point codomain:

- `ExprKind::Literal` (`:193`), `ExprKind::Regex` (`:194`) → **Static** (grammar constants —
  these are the leaves `derive_alphabet` harvests).
- The single Dynamic source is the **input cursor**: the leaf a scan/decode reads from
  `source[offset]` (the byte at `find_component_delim`; the `match byte` in `dispatch.rs:201`).
- `Ref`/`Repeat`/`Optional`/`Seq`/`Alt`/`Discard` (`:191-208`) propagate by join: a node is
  Static iff all inputs are Static. `Ref` follows the referenced rule's binding time.

**Claim proven by running on the REAL IR** (not a synthetic mirror): on the live
`css_l4_declaration_values` IR, every byte feeding `ScanAlphabet` / `AT_RULE_STOP` folds to
Static; the scan cursor folds to Dynamic. So `binding_time(AT_RULE_STOP) = Static` — the
const *is* a stage-0 residual, and the property is now a checkable attribute rather than a
side effect of running `derive_alphabet` in the generator. On the live `json` IR, the
`first_set` feeding each dispatch arm is Static; the dispatched byte is Dynamic.

**Retirement, not relocation (the honest-consumer test, Constitution viii / Y.13).** The
BTA is consumer-less decoration unless it *subsumes* extant hand-divisions. It subsumes two:
- `derive_alphabet`'s hand-coded Static frontier (`css_scan.rs:487-572`) becomes the
  Static-node query — the emitted const must be byte-identical, proving BTA recomputes the
  hand division exactly.
- `derive_materialization_roles`'s byte-literal role classifier (`passes/src/lib.rs:1403-1490`,
  the `b"{"`/`b"["`/`b"true"` gates) is a stage-0 structural read — its Static literal frontier
  is a BTA query. (Full retirement of its *role* semantics is LOCK-1/type-plane work; BTA
  claims the binding-time half only.)

**This is NOT a new IR tier.** Staging adds an *orthogonal attribute axis* over the existing
`GrammarIr`, honoring "the fewest transpositions that suffice" — it composes with a
single-IR-multipass shape rather than inserting a fourth level between HIR and BackendShape.
The existing `BackendShape::CollapsedStage` arm (`ir/src/lib.rs:366`; cost `cost.rs:131`;
priority `P3CollapsedStage` `cost.rs:267`; decision-CSP seam `decision_csp.rs:72`; the
`collapsed_stage_author_declared` gate `passes/src/lib.rs:664`) shows the IR vocabulary
already carries a coarse *whole-grammar* "stage" notion; BTA is the **per-node** refinement it
lacks, and that decision-CSP seam is the natural hand-off to LOCK-2 (§4).

---

## 3. Type-plane erasure = stage-0 erasure on the type sub-plane

The `->` projected type at the infer edge is a stage-0 residual whose erasure target is a
Rust *type* rather than a const. This unifies the value sub-plane (PROVE-12) and the type
sub-plane (GN-3) under one erasure operation.

- **The node.** `ExprKind::Project { body, target, ty }` (`ir/src/lib.rs:261`). `ty` is
  discarded at `passes/src/lib.rs:204` (`Project { body, .. } => self.infer_expr(*body)?`) —
  the `:203` `Annotation→Unit` arm is a legitimate untyped directive and is not the target.
- **Binding time.** `binding_time(Project.ty) = Static`: the projected type is known entirely
  at generation time and, by Constitution v, contributes **zero runtime instructions**. It is
  the definitional stage-0 residual — its very cost signature (zero at runtime) *is*
  stage-0-ness on the type sub-plane.
- **The concrete thread (consumption contract, per the Shared directive).** Family 3D does
  **not** re-derive the type. At the infer edge, the `Project` arm records the node's binding
  time and its *residual value* — the resolved type — into the fact store alongside
  `facts.node_types` (`:212`). The single downstream consumer,
  `derive_materialization_plan` (`passes/src/lib.rs:1149-1287`), **reads that one entry** to
  fill `DirectBuildField.target` (hardcoded `None` today — R6 §5d;
  `DirectBuildTarget.type_ref` at `ir/src/lib.rs:483-541`). One write at the edge, one read at
  the plan; no independent re-derivation. This is exactly the wire GN-3.W2 threads
  (`passes/src/lib.rs:204` → `DirectBuild.type_ref`), reframed: threading `ty` *is* carrying a
  stage-0 residual to its erasure target, and the target is a Rust type in the emitted struct
  (`json_typed_direct::render`, `codegen/src/json_typed_direct.rs:10`).
- **Erasure symmetry.** Value residual → const/immediate (PROVE-12). Type residual → Rust type
  annotation (GN-3). Both are `binding_time = Static` nodes erased before stage-1; the only
  difference is the residual *target* (a `const` vs a `type`). The `crates/runtime/` diff is
  empty for the type case — the erasure that costs zero is the same erasure that PROVE-12
  measures shrinking the instruction stream; they are one law over two sub-planes.

---

## 4. LOCK-2 seam (per-site staging) — a born-RED dependency edge

The charter's clause "per-site staging solved by LOCK-2's engine" resolves at exactly one
place: **polyvariant BTA**. A rule used at both a Static and a Dynamic site forces a choice —
duplicate/specialize it (residualize twice) or conservatively join to Dynamic (lose the
residual). CSS avoids this: its stop-set rules are Static-only, which is *why* the island is
clean. The decision "specialize this site or not" is a cost/legality problem — variables are
per-site staging choices, the objective is the generated-size budget vs the ins/B win. That
is `BackendShape`/cost selection (`passes::derive_backend_shape` `passes/src/lib.rs:398`;
`DecisionCspFacts` `ir/src/cost.rs:203`; the CollapsedStage decision-CSP arm
`decision_csp.rs:72,128`). Family 3D **declares** this edge born-RED with owner LOCK-2; it
does not author a solver. The BTA lattice generates the candidate division; LOCK-2's engine
picks per-site depth.

---

## 5. PMU: what a prototype must actually measure (PROVE-09 discipline)

Honest accounting, because unmeasured plausibility does not count:

- **Type sub-plane (§3): zero runtime delta by construction.** GN-3 is compile-time only; the
  invariant gate is `git diff --name-only <base>..HEAD | grep crates/runtime/` empty. There is
  no PMU number to claim and claiming one would be refuted at the seam.
- **Value sub-plane, CSS: already measured** — PROVE-12's ins/B −3.7…−6.4%, cyc/B 0.81–0.98×.
  Family 3D inherits this; it is not a new claim.
- **JSON first-byte dispatch: the open, falsifiable measurement.** The JSON dispatch is
  *already* residualized — `first_set` is Static, and `dispatch.rs:201` emits a native
  `match byte { … }` that LLVM lowers to a jump table. So a BTA-driven re-expression (e.g. a
  const-generic first-set membership analogous to PROVE-12's `DELIMS`) has a strong **NEUTRAL**
  prior: the residual is already present in a form LLVM optimizes. R6 confirms "JSON's hot
  rules dispatch straight to hand-written kernels — nothing to residualize there."

  The prototype must therefore MEASURE, cold/paired/PMU (N≥200, `json_canon_bench` first-byte
  lane), whether staging finds a **new** JSON residual beyond the existing match — and report
  it as a *negative result* if neutral. PROVE-12 §7 asserts "the pattern generalizes to JSON's
  structural scan"; that assertion is **unmeasured on JSON** and is precisely what family 3D's
  prototype falsifies or confirms. Either way is a valid outcome under Constitution vii.

---

## 6. Prior art (citations)

- **Partial evaluation / offline BTA.** Jones, Gomard, Sestoft, *Partial Evaluation and
  Automatic Program Generation* (1993) — the Static/Dynamic division, the standard `binding
  times` analysis this family names literally. Nielson & Nielson, two-level λ-calculus (the
  `code`/`type` staging discipline).
- **Futamura projections.** The bbnf generator specialized against a grammar *is* the first
  Futamura projection: specializing the parse loop w.r.t. a fixed grammar yields the parser;
  the stop-set const is the residual. This makes "the generator is a partial evaluator" a
  precise, not metaphorical, claim.
- **Multi-stage programming in the types.** Taha & Sheard MetaOCaml (`.< >.`/`.~`, the `code`
  type); the charter's "comptime/runtime split *in the types*" is the MetaOCaml `code`
  discipline. Rompf & Odersky LMS (`Rep[T]` carries binding time in the Scala type system;
  the SQL/Spark query compilers) — the direct model for staging expressed as a type, and the
  precedent for "generality proven by construction, zero runtime cost."
- **Live analogues.** Zig `comptime` (stage-0 params + monomorphization = residualization,
  the closest production system); Rust const-generics + monomorphization (what PROVE-12 uses —
  a poor-man's two-stage without a binding-time *analysis*); Terra. GraalVM/Truffle PE is the
  *runtime-dual* Futamura projection (partial-evaluate an AST interpreter); family 3D is its
  AOT counterpart.

---

## 7. Named risks & counterexamples

- **Consumer-less substrate (sharpest).** If BTA's only consumer is the already-done
  `derive_alphabet`, it is relocation, not retirement — a Y.13 violation. Guard: the prototype
  must show the emitted CSS const is byte-identical *derived from the BTA Static frontier* (BTA
  subsumes the hand division) AND name the LOCK-2 polyvariant consumer (§4). Absent both, the
  family is decoration.
- **JSON has no new residual.** The likely negative result of §5 — staging finds nothing on
  JSON that the native `match` + LLVM did not already residualize. This does not sink the family
  (CSS + sheets + general-descent remain three real islands) but it **refutes the "generalizes
  to JSON" assertion**; the prototype must own that outcome, not bury it.
- **Polyvariant blow-up.** A Static-at-one-site / Dynamic-at-another rule forces specialize-or-
  join; unbounded specialization violates the generated-size budget. Guard: this is exactly the
  born-RED LOCK-2 edge (§4) — BTA must not decide it unilaterally.
- **Binding time ≠ the existing type fold.** `regex_type` collapses whitespace to `Unit`
  (`passes/src/lib.rs:224-230`); BTA must not conflate "erased type" (Unit) with "Static
  binding". They are orthogonal: a Dynamic node can carry a Unit type. The prototype must keep
  the two attribute planes distinct or it will mis-stage.

---

## 8. What the prototype must test (the moves that advance this family)

1. Implement `binding_times(&GrammarIr) -> Attributes<ExprId, BindingTime>` as a monotone fold
   mirroring `infer_expr` (`passes/src/lib.rs:180-213`); **run it on the REAL
   `css_l4_declaration_values` and `json` IR**. Assert: every `ScanAlphabet`/`AT_RULE_STOP`
   input is Static; the scan/dispatch cursor is Dynamic. (Evidence for "a provably stage-0 node
   the live emit path exposes.")
2. Derive the CSS const from the BTA Static frontier and diff against `generated.rs:29-35` —
   **byte-identical** proves BTA reproduces `derive_alphabet`'s hand division (retirement claim).
3. Thread `Project.ty` (`passes/src/lib.rs:204`) as a Static residual → populate
   `DirectBuildField.target` (`passes/src/lib.rs:1149-1287`), consuming the single edge-written
   node-type; assert `crates/runtime/` diff empty (type plane costs zero — the type-sub-plane
   erasure).
4. Attempt a const-generic first-set residual on the JSON dispatch (`dispatch.rs:201`) and
   **measure cold/paired/PMU ins/B** on `json_canon_bench`. Report the delta — including a
   NEUTRAL/negative result — against the PROVE-12 CSS baseline. This is the born-RED measurement
   that moves the "does staging find a JSON residual?" question from assertion to fact.

## 9. Pointer appendix

- Stop-set derivation (stage-0 value): `codegen/src/lower/css_scan.rs:38-57,487-572`
- Erasure to const (stage-0→stage-1): `codegen/src/css_scan_direct/mod.rs:112-158`;
  materialized `runtime/src/grammars/css_l4_declaration_values/generated.rs:29-35`
- PROVE-12 instruction-stream residual: `prototypes/PROVE-12/{REPORT,patch.diff}` (const-generic
  `find_component_delim<const DELIMS:u128>`, `delim_mask`)
- Second/third islands: general-descent `codegen/src/general_descent/emit.rs:94,101` →
  `runtime/src/grammars/sheets/generated.rs:44`; JSON first-set dispatch
  `codegen/src/json_sink_direct/{dispatch.rs:52,201,first_set.rs}`
- BTA attach point: `passes/src/lib.rs:180-213` (fold), `:212` (`facts.node_types` sibling)
- Type-sub-plane thread: `ir/src/lib.rs:261` (`Project`), discard `passes/src/lib.rs:204`,
  consumer `passes/src/lib.rs:1149-1287` + `ir/src/lib.rs:483-541` (`DirectBuildTarget.type_ref`)
- Existing coarse "stage" vocabulary + LOCK-2 seam: `ir/src/lib.rs:366`
  (`BackendShape::CollapsedStage`), `ir/src/cost.rs:131,203,267`,
  `passes/src/decision_csp.rs:72,128`, `passes/src/lib.rs:398,664`
- Hand-division to subsume: `passes/src/lib.rs:1403-1490` (`derive_materialization_roles`)
