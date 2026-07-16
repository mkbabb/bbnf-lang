# LOCK-1 pass-2 · family 1C — BIDIRECTIONAL-LOCAL, diagnostics-first

The type system for the grammar plane, formulated as **bidirectional checking**: every
rule is checked rule-locally against the type its `->` projections *declare*, synthesis is
used only where nothing is declared, and the sole global step is a fixed-point over the
cross-rule reference graph. The design center is *where a failure is reported*: a declared
projection that the body cannot satisfy must name the `->` span, never a synthesized
interior. This family grows no new engine — it re-plumbs the extant type pass into two
modes (synthesize / check) and lands a span on the obligation record that is presently
span-less.

---

## 1. The mechanism

### 1.1 Two judgments over the existing IR

Bidirectional typing (Pierce & Turner, *Local Type Inference*, TOPLAS 2000; Dunfield &
Krishnaswami, *Complete and Easy Bidirectional Typechecking*, ICFP 2013; *Bidirectional
Typing*, ACM Comput. Surv. 2021) splits one inference relation into two:

- **synth** `e ⇒ τ` — the node *produces* a type (used where no annotation constrains it).
- **check** `e ⇐ τ` — the node is *verified against* an expected type (used at every
  annotation / declared-projection boundary).

The mode-switch rule is the whole discipline: at an ascription the checker enters **check**
mode with the declared type, recurses into the body under that expectation, and the
annotation itself is where a mismatch localizes. This is the shape a grammar `->` wants. A
projection `body -> Target : Ty` is an ascription: it *declares* the result type, so the
body is *checked against* `Ty` and the projection node *synthesizes* `Ty` outward.
PROVE-08's `classify_arrow` already encodes exactly this split — "ascription `: T` checks,
bare target synthesizes" (`PROVE-08/REPORT.md §1`; core in `type_plane/hm.rs`, 221 LOC,
VERIFY-certified perf-neutral). 1C promotes that leaf lexer from a one-shot classifier into
the pass's control mode.

### 1.2 Rule-local, cross-rule-global

The charter's spine ("bidirectional checking, rule-local; global solving only at cross-rule
seams") maps onto the greenfield structure with no new tower:

- **Rule-local (check/synth):** each rule body is walked once. Structural nodes
  (`Seq/Alt/Repeat/Optional/Literal/Regex`) synthesize as today. A `Map`/`Project` node
  with a declared return type switches its body to check mode against that type. All of this
  is intra-rule and terminates in one post-order walk — no propagation needed for the
  declared fragment.
- **Cross-rule-global (fixed-point):** a `Ref` to another rule is the only place a
  rule-local walk cannot conclude, because the target's type may be undetermined (mutual
  recursion). That is precisely the seam the greenfield already treats specially:
  `RefConstraint` "suspends propagation while the target rule is unsolved" and rule-to-rule
  cycles are cycle-broken by grounding + a named `UnresolvedCompoundRef { cyclic: true }`
  obligation (`crates/ir/src/passes/types/mod.rs:68-80` doc; `generate.rs`). 1C keeps that
  fixed-point but re-frames it as the inherited/synthesized-attribute boundary (§4): a
  declared rule type is a **synthesized attribute** available to every referrer without
  iteration; only *undeclared* mutually-recursive rules need the bounded fixpoint. The
  declared surface drives the SCC iteration count to zero for any ascribed rule.

### 1.3 The localization property (the family's thesis)

Synthesis-first inference reports a clash *where the clash is discovered*, which for a
grammar is deep inside a rule body — a `Ref`, an `Alt` branch join, a fused-number node —
far from the `->` the author wrote. The extant pass makes this concrete: heterogeneous
`Alt` branches that disagree are surfaced as a `HetJoin`/`UnresolvedCompoundRef` obligation
at the **downstream** `Alt`/`Ref` `NodeId` (`crates/ir/src/types/type_desc.rs:52-60` doc,
`crates/ir/src/passes/types/obligation.rs:56-74`), not at the projection that demanded the
shape. Check mode inverts this: the expected type is *pushed down* from the `->`, so the
failure surfaces at the first body node that cannot meet it and — because the expectation
was introduced *by the projection* — the diagnostic's primary span is the `->` operator's
span, with the offending interior node as a secondary note. This is the rustc
obligation-cause model (a `FulfillmentError` carries an `ObligationCause` whose span is the
obligation's *introduction* site, not merely its failure site) and the Elm/Roc
"diagnostics-first" stance (name the annotation the author wrote; show expected-vs-actual
side by side).

---

## 2. The substrate — what must exist

### 2.1 A span on the obligation record (the one net-new datum)

The blocking gap is small and exact. The greenfield obligation is **span-less**: `enum
TypeObligation` carries `ref_node: Option<NodeId>`, `target_rule: RuleId`,
`structural_type: TypeDesc` — but no source offset
(`crates/ir/src/passes/types/obligation.rs:56-74`). It is collected into
`GrammarIR::type_obligations: Vec<TypeObligation>` (`crates/ir/src/types/grammar.rs:428`).
Spans exist one level up — `RuleMeta.source_span: Option<GrammarSpan>`
(`crates/ir/src/types/rule.rs:29`) — but do not reach the node that failed. Meanwhile
**skinny already declares the target record and leaves it empty**: `TypeObligation { span:
SourceSpan, expected_from: String, actual_from: String, solver_stage: &'static str }` at
`skinny/crates/passes/src/lib.rs:138-144`, held in `TypeFacts.obligations` (`:130`) and
populated *by nothing* — a consumer-less substrate (Constitution viii / Y.13 violation
standing open). 1C's substrate is the union of the two: give the greenfield obligation a
span field, sourced by threading the failing `IrNode`'s `NodeId → GrammarSpan` (the DAG
keys every node by stable `NodeId`; `RuleMeta.source_span` is the anchor, refined per-node
where the front-end retains offsets).

### 2.2 A check-mode arm at the projection node

The declared-vs-synthesized comparison *is* the check judgment, and it is absent today. The
greenfield `->` return type is taken **without verification**: "Map nodes with
`FnDescriptor::Expr` use the return type from the MapExpr, falling back to Span"
(`crates/ir/src/passes/types/mod.rs:44-47` doc; `generate.rs:369-399`). Nothing confirms
the body can *produce* that return type — a `-> Foo` over a body that synthesizes `Bar` is
silently accepted and grounded. 1C adds the check: at a `Map`/`Project` node with declared
type `τ`, unify the body's synthesized type against `τ`; on clash, emit a
`ProjectionMismatch { span: <-> span>, declared: τ, synthesized: body_ty }` obligation
rather than silently overwriting. In skinny the same arm replaces the discard at
`skinny/crates/passes/src/lib.rs:204` (`ExprKind::Project { body, .. } =>
self.infer_expr(*body)?` throws away `target` and `ty`) — the exact site R6 flags
(`R6 §5a`).

### 2.3 Reuse, not rebuild

No CSP or e-graph substrate is added for the declared fragment: leaf/sum
check-then-synthesize is decidable and covered by the proven 221-LOC union-find core. The
greenfield CSP (`project_types`, AC-3 `propagate()` over `TypeDomain`) stays as the
**global** solver for the undeclared cross-rule residue; 1C inserts only the check arm and
the span, leaving propagation untouched. This keeps the family inside Constitution vi
(extreme parsimony) and the KISS-perf bias — the delta is one arm, one field, one
population site.

---

## 3. Integration seams (path:line)

Greenfield (tree-of-record target):

- **Type pass entry / synthesis:** `crates/ir/src/passes/types/mod.rs:35`
  (`project_types`) — the check arm inserts inside constraint generation.
- **`->` return-type synthesis (no check today):**
  `crates/ir/src/passes/types/generate.rs:369-399` (`FnDescriptor::Expr` return-type read,
  "falling back to Span").
- **Obligation record (add `span`):** `crates/ir/src/passes/types/obligation.rs:56-74`;
  sink `GrammarIR::type_obligations` at `crates/ir/src/types/grammar.rs:428`.
- **Span anchor:** `crates/ir/src/types/rule.rs:29` (`RuleMeta.source_span:
  Option<GrammarSpan>`), keyed per-node by `crates/ir/src/dag` `NodeId`.
- **Cross-rule fixpoint seam:** `ProjectConstraint`/`RefConstraint` + cycle-break in
  `crates/ir/src/passes/types/mod.rs:68-80` (the SCC boundary, §4 / Open S).
- **LOCK-3 sink (the handoff the verdict must lower through):**
  `crates/core/src/backend/driver/analysis.rs:22` — `TypeAnalysis::from_ir` reads
  `ir.types: Vec<(RuleId, TypeDesc)>` + `ir.type_map`; second sink
  `crates/core/src/backend/rust/view/named_types.rs`.
- **TS mirror (every record design names it):** `crates/core/src/backend/ts/projection.rs`
  (`type_desc_to_ts` / MapExpr→JS). The verdict projects through `TypeDesc`, already
  backend-agnostic, so the TS face inherits the check for free.

Skinny (measurement substrate):

- **Discard site (the check arm's home):** `skinny/crates/passes/src/lib.rs:204`.
- **Unpopulated span record (Y.13 open):** `skinny/crates/passes/src/lib.rs:138-144` +
  `:130`.
- **Error path today (stringly-typed, span-less):** `skinny/crates/passes/src/lib.rs:197`
  (`PassError::Type(String)`) and `skinny/crates/passes/src/diagnostics.rs` — `PassDiagnostic`
  carries `rule: Option<RuleId>` + `message: String`, **no span** (Thread C.2's target).
- **Front-end span source:** every `Expr` is created with a `SourceSpan { start, end }`
  (`skinny/crates/ir/src/lib.rs:22-35`; `add_expr`/`add_rule` take a span); the `->` node's
  own span is live at lowering (`:1349`, `ExprKind::Project { body, target, ty } =>
  BackendExpr::ValueProject`).
- **Reused bidirectional lexer:** PROVE-08 `type_plane/hm.rs` `classify_arrow`
  (ascription-checks / bare-synthesizes).

---

## 4. Open S — the cross-rule seam as one attribute-grammar discipline

Open S asks whether the cross-rule dependency is one *seam discipline* rather than a fifth
engine. Bidirectional-local answers it natively. Cast the type pass as an attribute grammar
(Knuth 1968; circular/conditional attribute grammars with a monotone lattice — Farrow 1986,
Boyland 1996 — terminating by ascending chain on the finite `TypeDesc` lattice):

- **Synthesized attribute** = a rule's declared result type (from its `->`). Available to
  every referrer with zero iteration — this is what makes declared rules SCC-free.
- **Inherited attribute** = the *expected* type pushed down in check mode (the `->`'s
  declared `τ` flowing into the body). This is the engine of §1.3's localization.
- **Circular attribute (bounded fixpoint)** = only the undeclared mutually-recursive
  residue, solved by the existing AC-3 `propagate()` + cycle-break grounding, its
  termination lattice the finite `TypeDesc` height (`Span ⊑ … ⊑ BoxedEnum`).

The three items Open S names as one discipline — SCC-generalize, federation-sequencing,
bounded-fixpoint — are then the three attribute classes, not three subsystems: the SCC is
the circular class, federation-sequencing is the synthesized class' topological
availability, and the bounded fixpoint is the lattice height. GHC's `OutsideIn(X)`
(Vytiniotis, Peyton Jones, Schrijvers, Sulzmann, JFP 2011) is the load-bearing prior art
for the local-vs-global split done right: local constraints solved eagerly in place, only
the genuinely-cross-scope residue deferred to a global solver — exactly declared-rule-local
check vs undeclared-cross-rule fixpoint.

---

## 5. Prior art (citations)

- **Bidirectional typing:** Pierce & Turner, *Local Type Inference* (TOPLAS 2000); Dunfield
  & Krishnaswami, *Complete and Easy Bidirectional Typechecking* (ICFP 2013) and
  *Bidirectional Typing* survey (ACM CSUR 2021). The check/synth split and the
  ascription-drives-check rule are taken verbatim.
- **Diagnostic-cause model:** rustc `ObligationCause`/`FulfillmentError` — the reported
  span is the obligation's *introduction* site; rustc's primary-span + secondary-note
  `Diagnostic` structure. Elm and Roc error philosophy (name the author's annotation; show
  expected-vs-actual side by side).
- **Local-vs-global boundary:** GHC `OutsideIn(X)` (JFP 2011) — eager local solving,
  deferred cross-scope residue.
- **Attribute grammars:** Knuth (1968); circular/conditional attribute grammars with
  monotone-lattice fixpoints (Farrow 1986; Boyland 1996) — the Open S framing.
- **In-tree seeds:** PROVE-08 (`type_plane/hm.rs`, `classify_arrow`, VERIFY CONFIRMED_DELTA
  perf-neutral); GN-3 (the sum node + `->`-through-`infer` waves); R6 (`R6 §5a-5d`, the
  discard-and-fold census).

---

## 6. Named risks & counterexamples

- **R-C1 — per-node spans may not survive to the IR.** `RuleMeta.source_span` is
  rule-granular; if the front-end discards per-node offsets, the finest localizable span is
  the whole rule, not the `->`. **Counterexample:** two `->` in one rule — a rule-span
  diagnostic cannot say *which*. **Test:** confirm the front-end retains an offset on the
  `Project`/`Map` node (skinny's `Expr` does — `SourceSpan` per `add_expr`); if greenfield
  dropped it, the additive-span wave must re-thread it, a named cost.
- **R-C2 — check mode changes accepted programs.** Today `-> Foo` over a `Bar`-producing
  body is silently grounded to Span; adding a check turns that into an error. **Risk:** the
  corpus contains latent mismatches and the checker reddens green grammars.
  **Test:** run the check arm in *record-only* mode first (emit the obligation, do not fail
  the build) and count clashes across JSON / Sheets / CSS L4 before flipping to fail-closed
  — the honest migration, not a flag-day (§7.3).
- **R-C3 — the localization claim is unfalsifiable without a paired output.** The family
  rests on "check localizes at the `->`, synthesis reports at a synthesized interior." That
  must be *shown*, not asserted (§7.1).
- **R-C4 — TS-side divergence.** If the checker records anything richer than `TypeDesc`,
  the TS mirror cannot lower it. **Guard:** the obligation payload is `TypeDesc` + span
  only; `type_desc_to_ts` already covers every `TypeDesc` variant, so the verdict is
  TS-lowerable by construction.
- **R-C5 — cross-rule check ordering.** Check mode needs a referrer to know the target
  rule's declared type; a forward reference to an as-yet-unwalked declared rule must read
  the *declaration*, not wait for the walk. **Guard:** declared types are a synthesized
  attribute pre-seeded from the `->` before the body walk (mirrors skinny's
  `seed_rule_types`, `skinny/crates/passes/src/lib.rs:172-176`), so declaration
  availability is order-independent; only undeclared rules enter the fixpoint.

---

## 7. What a prototype must test (to move family 1C)

### 7.1 Thread C.1 — the side-by-side localization proof (make-or-break)

Author a typed `->` micro-fixture with a *genuine* declared/synthesized mismatch — e.g.
`frac = digits "." digits -> Ratio` whose body synthesizes a two-`Span` `Seq` while `Ratio`
is declared scalar, the mismatch reachable only through the projection. Run it twice:

1. **synthesis-first (baseline = the extant pass):** record the `NodeId`/span the failure
   (or the `HetJoin`/`UnresolvedCompoundRef` obligation) actually lands on.
2. **bidirectional check:** record the span the `ProjectionMismatch` obligation lands on.

**Pass criterion:** baseline reports at a *synthesized interior* (a body `Seq`/`Ref`/`Alt`
`NodeId`); check reports at the **`->` span**; the two outputs are printed side by side with
expected-vs-actual. If check does not move the span to the `->`, the thesis is refuted and
the family retires to synthesis-first. This single experiment decides 1C.

### 7.2 Thread C.2 — the additive span landing (the substrate proof)

Against the greenfield base: add `span` to the type obligation (`obligation.rs:56-74`),
source it from the failing node's `NodeId → RuleMeta.source_span`, and specify the
`Project`/`Map` sub-span enrichment (primary = `->` span; secondary = offending interior
node). **Gate:** the diff is purely additive (a new field + its population; no behavior
change when no clash exists), the full corpus stays green (JSON / Sheets / CSS L4 regen
byte-identical), and the runtime-surface `git diff` is empty (Constitution v — the type
plane costs zero at runtime; the obligation is generation-time only). Skinny mirror:
populate the already-declared-but-empty `TypeObligation.span`
(`skinny/crates/passes/src/lib.rs:138-144`) from the same source, closing the standing Y.13
consumer-less-substrate violation.

### 7.3 The record-only census (de-risks R-C2)

Before any fail-closed flip, run the check arm in record-only mode and report, per corpus,
how many declared `->` sites currently mismatch their body's synthesized type — the count
that says whether check mode is a clean addition or a corpus-reddening one. Pre-register the
expected count (ideally zero on honestly-authored grammars); treat any nonzero as a named
residual, not a silent regression.
