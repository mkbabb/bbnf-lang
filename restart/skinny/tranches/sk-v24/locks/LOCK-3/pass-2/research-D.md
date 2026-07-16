# LOCK-3 · Family 3D — THE STAGED-RESIDUAL SPINE (pass-2 research)

Live substrate pinned to `skinny@c93fc4314` (working tip; the directive's re-pin
token `b3cb8c16b` names the lock-set frozen base, an ancestor of the tip). The
skinny tree is homed inside `bbnf-lang` with no separate `.git`, so the commit
names the repo. Every `path:line` below was opened on that tree.

---

## 0. The thesis in one line

The generator already performs partial evaluation at three unrelated sites, each
hand-rolled. A single **binding-time lattice** over the IR unifies them: a node
is *static* (stage-0, computable at generation time) or *dynamic* (stage-1,
depends on the runtime input byte-stream), and **residualization is the erasure
of every stage-0 node** into a compile-time constant, a `const`-generic, or a
Rust *type*. Under this reframing PROVE-12's delimiter mask, the CSS stop-set
constants, and the GN-3 type plane are not three features but one operation
applied to three sub-planes of the same IR. The constitution's clause (v) —
"the type plane costs zero at runtime" — is *identically* the statement "the
type sub-plane is all stage-0, hence fully erased."

## 1. The three residual islands already on live (the mechanism, retro-fitted)

**(a) CSS stop-sets — the live, grammar-derived stage-0 island.**
`crates/codegen/src/css_scan_direct/mod.rs:112-158` (`render_alphabet_consts`)
harvests every delimiter byte from the CSS L4 rule literals off
`lower::css_scan::ScanAlphabet` and emits `const AT_RULE_STOP: &[u8]`,
`QUALIFIED_STOP`, `DECLARATION_STOP`, `COLON_BEFORE_STOP`, and the fixed-9
`SKIP_ALPHABET: [u8; 9]`. This path is live: `runtime_generator.rs:57`
(`css_scan::derive_program`) → `:228` (`css_scan_direct::render`). The stop-set
is a **pure stage-0 value** — it depends only on grammar structure, never on
input — and it is already erased to a Rust `const` that feeds the SIMD scan.
This is a residualized stage-0 node in production; family 3D does not re-author
it, it *declares it the exemplar* and gives it a name.

**(b) PROVE-12's delimiter mask — stage-0 erasure to a `const`-generic.**
The delimiter set at each of 5 CSS call sites is a grammar constant; PROVE-12
const-evaluates it into a `u128` ASCII bitmask threaded as
`find_component_delim::<{ delim_mask(b";{}") }>` (PROVE-12/REPORT §2). The
membership test collapses from an interpretive `<[u8]>::contains` slice-scan
(runtime length guards + slice loads) to two immediate `and` masks
(PROVE-12/REPORT §3, verifier-CONFIRMED, ins/B −3.7…−6.7%). This is *exactly*
stage-0 erasure: a static value moved from a runtime slice into a
monomorphization parameter. **PROVE-12 becomes a corollary** the moment
`binding_times` labels the delimiter node stage-0 and the residualizer targets
`const`-generics — it is island (a) taken one erasure-step further (from
`const &[u8]` scanned at runtime to a `const`-generic beta-reduced away).

**(c) The JSON first-byte dispatch — a stage-0-*derived* residual on the live
direct path.** `crates/codegen/src/json_sink_direct/dispatch.rs:201` and
`recognize.rs:51` emit `match byte { ... }` whose arms are the alternation lead
bytes harvested from the grammar (`rule_has_byte`, `dispatch.rs:76`,
`b'{'`/`b'"'`/…). The *shape* of the dispatch (which bytes select which arm) is
stage-0; the *byte read* (`bytes.get(*cursor)`) is stage-1. This is the JSON
analogue of (b) and the site the PMU test in §5 targets.

Retro-fit: (a) is stage-0 residualized to `const`; (b) is stage-0 residualized
to `const`-generic + monomorph; (c) is a stage-0 dispatch table emitted as
`match`. One lattice subsumes all three.

## 2. The substrate — a binding-time attribute, one pass, one lattice

**Lattice.** Two points, `Stage0 ⊑ Stage1` (static ≤ dynamic), join = `Stage1`.
Congruence rule (the standard offline-PE monovariant BTA fixpoint): a node is
`Stage1` iff any of its data inputs is `Stage1`; the sole *seed* of `Stage1` is
the runtime input cursor (`bytes.get(*cursor)`). Grammar literals
(`ExprKind::Literal`), regex patterns (`ExprKind::Regex`), projection
targets/types (`ExprKind::Project{target, ty}`, `ir/src/lib.rs:261-264`), the
harvested `StructuralAlphabet{bytes}` (`ir/src/lib.rs:434-437`), delimiter sets,
and grammar-declared capacity hints are all `Stage0`.

**Substrate.** A `binding_times(&GrammarIr) -> BindingTimeMap` pass producing a
dense `IndexVec<ExprId, Stage>` — an *attribute annotation over the existing IR*,
not a new IR tier (this keeps 3D orthogonal to the tower-count question; it adds
one fact, not one level). It lives beside inference in `crates/passes/`. The
residualizer is a second pass, `residualize(&GrammarIr, &BindingTimeMap)`, that
lowers each stage-0 sub-tree to its compile-time form and hands the stage-1
remainder to the existing emit path — the emitter's input is a *specialized*
IR in which stage-0 is gone.

**Granularity — per-`ExprId`, never per-`RuleId` (a hard finding).** JSON's
`value` rule is recursive and carries the runtime cursor, so *as a whole rule*
it is `Stage1`; but its **first-byte dispatch table** and its **whitespace-skip
shape** are `Stage0` sub-nodes of the same rule. A per-`RuleId` binding-time
collapses every non-trivial rule to `Stage1` (the recursion poisons it) and the
residual islands vanish. The split is intra-rule; the attribute must key on
`ExprId`. **Answer to the per-key question: per-`ExprId` is mandatory**;
per-`RuleId` is unsound-by-coarsening for the recursive fleet grammars
(json, css, sheets).

## 3. Type-plane erasure = stage-0 erasure on the type sub-plane (the concrete thread)

The `->` projected type is a **stage-0 value by construction**: known at
generation time, dependent on nothing runtime. Threading it is therefore not a
new type system — it is the type sub-plane's instance of the *same* stage-0
erasure that (a)/(b)/(c) perform on the byte sub-plane. The erasure target for a
type is a **Rust type**, which has no runtime representation — so "the type plane
costs zero at runtime" is a *theorem* of the lattice (all stage-0 ⇒ fully
erased), not an independent invariant to police.

**The end-to-end thread on live** (each step consumes the *threaded* node, never
re-derives it):

1. `crates/ir/src/lib.rs:261-264` — `ExprKind::Project { body, target, ty:
   Option<String> }` already carries the arrow. `binding_times` labels it `Stage0`.
2. `crates/passes/src/lib.rs:204` — `ExprKind::Project { body, .. } =>
   self.infer_expr(*body)?` currently drops `target`/`ty`. **The consumption
   contract:** the residualizer reads the *stage-0 witness* `ty` here — when
   `ty = Some(t)`, `t` is placed **verbatim** into `DirectBuildField.target`
   (currently hardwired `None`, `passes/src/lib.rs:1149-1287`); when `ty = None`,
   and only then, HM stage-0 inference fills it. The threaded node is *consumed
   as a residual*, not re-inferred — inference is the fallback for the absent
   witness, not the primary path. This is the exact CONSUMES-not-re-derives
   contract the shared `->` question demands.
3. `crates/passes/src/lib.rs:1349-1353` — `Project → BackendExpr::ValueProject {
   body, target, ty }` already carries `ty` verbatim into the backend
   (`ir/src/lib.rs:407-410`); the residualizer routes it into the
   `MaterializationPlan` field rather than the current 4-way `ProjectionKind`
   fold (`emit/general_descent/view.rs:20-46`), which is a *stage-0 value thrown
   to 4 categories* — a lossy erasure the lattice replaces with a faithful one.
4. `crates/codegen/src/direct_schema.rs` (`DirectSchemaSet`) →
   `json_typed_direct` renders the Rust struct field type. The struct field's
   type IS the residual of the stage-0 `ty`.

**Boundary (honest, load-bearing):** a *struct* type erases with zero runtime
trace, but a **sum** (`DirectTypeKind::Sum`, the six-arm `value` alternation)
carries a runtime **discriminant** — the byte read selecting the variant is
`Stage1`. The lattice bisects the type plane cleanly: *type identity* is stage-0
(erased to a Rust type), *variant selection* is stage-1 (the live first-byte
`match` of §1c, `value.rs:28-47`). This reconciles the zero-cost claim with the
unavoidable discriminant and locates exactly where the runtime work sits.

## 4. The `verify` predicate and the differential runner

**A per-pass `verify` predicate.** The lattice supplies a *soundness* predicate
richer than a single-node check: **no stage-0 node may have a stage-1 input**
(the congruence invariant). Wired over the whole IR it reads GREEN on a
well-labeled tree, RED on any mislabeling; specialized to `Project@204` it reads
RED on HEAD (the `ty` is dropped, so the field it should residualize into is
`None`) and GREEN once the witness threads and `DirectBuildField.target` is
`Some(t)`. Emit footprint is zero when `ty` is present: the predicate reads the
attribute map only, so regen of the six outputs (regen/check × json/css/sheets)
is byte-identical with the predicate on or off — the idempotent-on-disk gate
GN-3.W3 already carries.

**A `differential(edge, corpus)` runner.** The lattice localizes mis-lowering
naturally: inject a fault on one stage-0 node (flip a delimiter byte, drop a
`ty`), residualize, and the XOR-checksum / `json_parity` digest that already
folds every parse result diverges *only* on the sub-plane that node feeds — the
edge is the injection site. Coverage metric: the fraction of `ExprId`s the
corpus exercises at least once; zero-witness stage-0 nodes (constructs the corpus
never reaches) are reported **unverified** (labeled stage-0 by congruence but
never residualized under test), distinct from **sound**.

## 5. What a prototype must test to move family 3D (the decisive experiments)

1. **Run `binding_times` on the REAL json.bbnf and css-l4 IR** (not a synthetic
   mirror). Assert: the CSS stop-set nodes and JSON alternation-lead nodes come
   out `Stage0`; every `bytes.get(*cursor)` node comes out `Stage1`; the
   recursive `value` rule is `Stage1` *as a whole* yet exposes `Stage0`
   sub-nodes. **Kill criterion:** if per-`ExprId` BTA cannot separate the
   dispatch shape from the byte read, the family is unsound at granularity.
2. **PROVE-12 subsumption.** Show `residualize` over the stage-0 CSS delimiter
   node **reproduces PROVE-12's `const`-generic `u128` mask emit byte-identically**
   (regen bit-for-bit vs the committed template). If it produces a *parallel*
   mechanism, PROVE-12 is not a corollary and the unification claim downgrades to
   "analogy" (R4).
3. **The type thread, zero-footprint.** Thread `Project.ty` (`:204`) into
   `DirectBuildField.target`; prove sheets + typed regen byte-identical when `ty`
   is present (witness path emits nothing new), and that HM fires *only* on
   `ty = None`. Consumer: the GN-3.W2 sheets accessor generation, re-framed as
   witness-consumption not re-classification.
4. **JSON first-byte PMU (the honest measurement, per PROVE-09 law).**
   Residualize `parse_value_direct`'s `match byte` (`dispatch.rs:201`) into a
   const dispatch table; measure cold paired ins/B on the JSON first-byte
   dispatch lane, N≥200. **Report the delta including a possible null/negative
   result:** LLVM likely already lowers a small dense `match` to a jump table, so
   the win may be ≈0 — a *named negative result* is the correct output, not a
   suppressed one. This is the family's most refutable claim.
5. **The soundness `verify` predicate** (§4) as a fail-closed CI test: RED on
   HEAD at `Project@204`, GREEN once threaded; plus the congruence invariant over
   the whole IR.

## 6. Prior art (citations)

- **Partial evaluation / Futamura projections.** Futamura (1971); Jones, Gomard,
  Sestoft, *Partial Evaluation and Automatic Program Generation* (1993) — the
  canonical BTA + specialization pipeline; offline PE = monovariant BTA then
  specialize. The generator here is a Futamura-1 specializer: grammar (static) +
  input (dynamic) residualizes to a recognizer; PROVE-12 removes the last
  interpretive dispatch — precisely PE's "residualize the interpreter."
- **Binding-time analysis / two-level languages.** Nielson & Nielson, *Two-Level
  Functional Languages* (1992); Similix, C-Mix (self-applicable PE for C) — the
  `Stage0 ⊑ Stage1` lattice and per-expression annotation are their core.
- **Multi-stage programming.** Taha & Sheard, MetaML (1997); MetaOCaml; Taha,
  *A Gentle Introduction to Multi-stage Programming* (2004) — brackets/escape/run
  as stage annotations; well-staged ⇒ stage-0 erases (the "stage in the types"
  framing of the 3D charter is theirs).
- **Lightweight Modular Staging.** Rompf & Odersky (2010) — `Rep[T]` vs `T` as
  the binding-time marker; Delite. The nearest engineering precedent for
  "type-carried stage" in a real compiler.
- **Terra** (DeVito et al., 2013) — two-language staging (Lua stages Terra);
  **Zig `comptime`** — a binding-time keyword, comptime values erased, exactly
  the stage-0 erasure claim.
- **GraalVM/Truffle partial evaluation** — the first Futamura projection realized
  in production (interpreter + PE = compiler); direct precedent for
  "residualize the interpretive dispatch loop" that PROVE-12 executed by hand.
- **Rust substrate.** `const fn` + const-generics + monomorphization ARE the
  stage-0 erasure engine on the emit side (PROVE-12's `u128` mask); MIR const-eval
  (miri) is the in-compiler partial evaluator. **MLIR** constant-folding /
  canonicalization + dialect lowering is the multi-level analogue.
- **Phase distinction.** Harper, *Practical Foundations for Programming Languages*
  — the static/dynamic phase distinction is the type-theoretic root of
  "type = stage-0."

## 7. Named risks and counterexamples

- **R1 — the JSON PMU may be a negative result.** `at_cursor`/`parse_value_direct`
  is already a tight first-byte `match`; LLVM may already emit a jump table, so
  const-table residualization can show ≈0 or negative ins/B. Under the PROVE-09
  refutation law this must be *measured and reported as null if so* — the family's
  runtime win rests on CSS (a/b), not JSON (c). Over-claiming JSON is the chief
  refutation vector.
- **R2 — monovariance vs recursion / capacity hints.** A rule referenced from
  both a stage-0 and a stage-1 context forces poly-variant duplication or a
  conservative join to stage-1. Resolved by per-`ExprId` granularity (§2), but a
  capacity computed from a *parsed count* genuinely IS stage-1: the hand schema's
  `875`/`5_759`/`184` (GN-3 R1) are runtime document facts, stage-1 unless
  declared as `@cap` grammar annotations. The lattice makes this precise — a
  capacity is stage-0 iff grammar-declared.
- **R3 — sum discriminant is stage-1.** "Type = stage-0 erased" is strictly true
  only for structs; sums carry a runtime discriminant. The family must state the
  bisection (type identity stage-0, variant selection stage-1, §3 boundary) or it
  over-claims zero-cost for sums.
- **R4 — subsumption vs parallel mechanism.** If `residualize` does not reproduce
  PROVE-12's exact emit byte-identically (experiment §5.2), PROVE-12 is *not* a
  corollary and the "one operation, three islands" thesis is analogy, not fact.
  This is the family's central falsifiable claim.
- **R5 — the JSON scan island is dormant.** `crates/runtime/src/grammars/json/scan.rs`
  (`scan_structurals`, `STRUCTURAL_ALPHABET`) is a stage-0 residual (grammar-derived
  alphabet → SIMD index) but is **not on the live JSON path** (neutralized). The
  strongest *live* stage-0 island is CSS; a claim that JSON already ships a stage-0
  SIMD residual is false — the live JSON residual is only the `match` dispatch (c)
  and `skip_ws`.

## 8. Integration seams (path:line, live tree)

- Binding-time attribute + residualizer home: `crates/passes/src/lib.rs` (beside
  `infer_expr@204`, `lower_expr@1340-1359`, `materialize_rule@1362-1389`).
- The `->` witness: def `crates/ir/src/lib.rs:261-264`; drop `passes/src/lib.rs:204`;
  backend carrier `ir/src/lib.rs:407-410`; lower `passes/src/lib.rs:1349-1353`;
  plan `None`-fill `passes/src/lib.rs:1149-1287`; lossy fold to replace
  `crates/codegen/src/emit/general_descent/view.rs:20-46`.
- Stage-0 island (a) CSS stop-set: `crates/codegen/src/css_scan_direct/mod.rs:112-158`;
  live wiring `runtime_generator.rs:57,228`.
- Stage-0 island (c) JSON dispatch: `crates/codegen/src/json_sink_direct/dispatch.rs:76,201`,
  `recognize.rs:51`.
- Stage-0 alphabet node in IR: `crates/ir/src/lib.rs:434-437` (`StructuralAlphabet`),
  `:400-406` (`TapeEmit`/`DirectBuild`), `:453-460` (`TapeKind`).
- The name-gate the lattice makes redundant: `runtime_generator.rs:98`
  (`has_rule("value")`) — a rule-name string test a stage/role attribute should
  retire (§2 congruence keys off structure, not the literal name `value`).
- PROVE-12 mask template (the emit to reproduce): `runtime_generator.rs`
  `CSS_GENERATED_RS` `find_component_delim`.
</content>
