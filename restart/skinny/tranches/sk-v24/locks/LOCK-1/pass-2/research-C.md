# LOCK-1 · pass-2 · research-C — FAMILY 1C: BIDIRECTIONAL-LOCAL

Family thesis. Type checking for the grammar plane is **rule-local bidirectional**:
each `->` projection is a *check boundary* — when a rule carries a declared type the
body is **checked against** it; when it does not, the body **synthesizes** a type;
global (cross-rule) solving is confined to the seams where a `Ref` or an `Alt` forces
two rule-local judgments to meet. The design center is not the solver — it is the
*diagnostic*: every failure names a grammar span, and the span it names is the `->`
the author wrote, not a synthesized interior the author never sees. The rest of this
report shows the mechanism made concrete against the greenfield tree, the three seams
where extant code already serves, and exactly what a prototype must prove.

---

## 1. The mechanism: check vs. synthesize, localized at the arrow

Bidirectional typing (Pierce & Turner, *Local Type Inference*, TOPLAS 2000;
Dunfield & Krishnaswami, *Bidirectional Typing*, ACM CSUR 2021) splits one judgment
into two mutually-recursive modes: **synthesis** `Γ ⊢ e ⇒ A` (the term *produces* a
type) and **checking** `Γ ⊢ e ⇐ A` (the term is *verified against* an expected type
pushed down from context). The rule that makes it diagnostic-precise is the mode
switch: `(e : A)` ascription enters checking; a bare term stays in synthesis. The
localization win is structural — a checking failure is reported at the **subterm that
was checked against the expected type**, because that is where the two types first
disagree, whereas a pure-synthesis (Hindley–Milner / Algorithm W) failure surfaces at
whatever unification variable happened to be grounded last, which is frequently a
*synthesized interior node* far from the author's annotation (this is the classic
"error reported in the wrong place" pathology of unification-only inference, and the
motivation rustc gives for its `Expectation` machinery — see §6).

Mapped onto the grammar plane, the `->` arrow IS the ascription boundary. A rule
`unit = "px" -> 0u8` synthesizes `U8` from the literal `0u8`. A rule
`length = digits -> f64 | fraction -> f64` **checks** each branch's projection against
the shared declared leaf `f64`; if one branch synthesized `Span` and the other `f64`,
the disagreement is localized **at the branch arrow whose synthesized type failed the
check**, not at the `Alt` join that HM would blame. The proven bidirectional leaf
already exists: PROVE-08's `classify_arrow(target_text, type_text)`
(`prototypes/PROVE-08/patch.diff:374`) is exactly the mode switch — `type_text = Some(T)`
is the **checking** path (an ascription `: T` checks against the named type), a bare
`target_text` is the **synthesis** path (leaf inferred from literal shape:
`true`/`false`→Bool, `Nu8`→U8Tag, `f64`→F64). That 33-line function is the reusable
kernel of family 1C; the family's work is to (a) give the check path a *span* to blame
and (b) route the blame into a diagnostic stream a consumer reads.

---

## 2. The substrate at HEAD — what already serves

The greenfield already implements a projection type system as a **CSP over rule-output
types**, and it already distinguishes the two bidirectional modes *structurally* — it
simply never exploits the distinction for localization. The seams:

- **The check annotation.** `FnDescriptor::Expr { expr: MapExpr, return_type: Option<TypeDesc> }`
  (`crates/ir/src/types/fn_descriptor.rs:54`) is the lowered `->`. `return_type = Some(T)`
  is precisely an ascription (checking mode); `return_type = None` is synthesis. This is
  the whole bidirectional bit, already present in the IR.
- **The synthesis→bind site.** `crates/ir/src/passes/types/generate.rs:370` reads the
  `Expr` arm: `return_type.clone().unwrap_or(TypeDesc::Span)`, then
  `self.csp.add_constraint(MapConstraint::new(var, map_type))`. This is a **ground**, not
  a *check* — the declared type is asserted as the Map var's value; if the body's
  synthesized type disagrees, there is no comparison, no localized error: the annotation
  silently wins (or the CSP later clashes at a distant Ref/Alt). `fn_descriptor_return_type`
  (`crates/ir/src/passes/types/registry.rs:458`) is the projection reader for the same
  annotation. **This is where CHECK mode must be installed** — compare synthesized body
  type against `return_type` and, on mismatch, emit a localized obligation rather than
  overwrite.
- **The synthesized-interior blame site (the counter-example, live).**
  `join_types(branch_types)` (`crates/ir/src/passes/types/constraint/revise.rs:116`) is
  the `Alt`-branch LUB. When branches disagree and no homogeneity reduction fires
  (`:141`), it lifts to `TypeDesc::HeterogeneousAltJoin(...)` and records a
  `TypeObligation::HeterogeneousAltJoin { rule: Option<RuleId>, node: Option<NodeId>, branches }`
  (`crates/ir/src/passes/types/obligation.rs:92`). **This is synthesis-first localization
  in the wild**: the blame carries a *synthesized Alt-node* `NodeId`, not the arrow the
  author wrote — and it carries no span at all. It is the exact artifact Thread C #1 must
  put side-by-side against a checking-mode report.
- **The diagnostic stream, and its missing consumer.** `GrammarIR::type_obligations:
  Vec<TypeObligation>` (`crates/ir/src/types/grammar.rs:428`), drained at
  `crates/ir/src/passes/types/mod.rs:486`. `TypeObligation::diagnostic()`
  (`obligation.rs:113`) already renders human strings — but they name `node N` / `rule R`,
  never a source span, and **nothing in the `crates/analysis` LSP crate consumes
  `type_obligations`** (grep: zero hits in `crates/analysis/src`). The stream is a
  *consumer-less substrate* (Constitution viii / Y.13) — the diagnostics-first family's
  single largest standing debt.
- **The one span that exists.** `IrRule.source_span: Option<GrammarSpan>`
  (`crates/ir/src/types/rule.rs:29`; `GrammarSpan { start: u32, end: u32 }` at
  `crates/ir/src/types/node.rs:18`) is the **only** source span in the type-relevant IR.
  `IrNode` carries none (`node.rs:30-98`); `IrNode::Map { inner, fn_id }` (`node.rs:79`) —
  the lowered arrow — has no span; `FnDescriptor::Expr` has no span; `MapExpr`
  (`crates/ir/src/types/map_expr.rs:17`) has none. **Rule granularity is the current
  localization floor.** A grammar with three `->` on one rule can only ever blame the
  whole rule.

Net: the greenfield is a *synthesis-first* projection CSP with a rule-granular span
floor and a consumer-less obligation stream. Family 1C's mechanism — bidirectional
checking with arrow-localized, span-carrying, LSP-consumed diagnostics — is an
**additive enrichment of exactly these five seams**, not a new engine.

---

## 3. Thread C #1 — the micro-fixture: synthesis-first vs. checking, side by side

**Build the fixture first** (a genuinely under-determined `->` where the two modes blame
different spans). Author `crates/ir/tests/fixtures/bidir_width.bbnf`:

```
value = tag | measure ;
tag     = "row"    -> 0u8
        | "col"    -> 1u8 ;
measure = digits   -> f64
        | fraction -> f64 : Span ;      // (†) deliberate clash: body synthesizes f64,
                                        //     ascription checks against Span
digits   = /[0-9]+/ ;
fraction = /[0-9]+\.[0-9]+/ ;
```

Line (†) is the probe: the branch's bare target `f64` synthesizes `TypeDesc::F64`; the
ascription `: Span` checks against `TypeDesc::Span`. They disagree.

**Synthesis-first output (HEAD behavior, reconstructed from the live seams).** With no
checking arm, `generate.rs:370` grounds the Map var to the *annotation* (`Span`) via
`MapConstraint`, discarding the synthesized `F64`; the disagreement does not surface at
(†). It surfaces later, at the `measure` `Alt` join, where branch-1 `= F64` and
branch-2 `= Span` disagree → `join_types` (`revise.rs:116`) lifts
`HeterogeneousAltJoin([F64, Span])` and records
`TypeObligation::HeterogeneousAltJoin { rule: Some(measure), node: Some(<Alt NodeId>), branches: [F64, Span] }`.
`diagnostic()` renders:

```
heterogeneous Alt join: node 41 in rule 6 :: branches [F64, Span]
  (lifted to TypeDesc::HeterogeneousAltJoin; lowers to Box<Enum>)
```

Blame = a **synthesized Alt node** (`node 41`), no span, and it fingers the *join*, not
the arrow — the author reading this cannot see that the `: Span` on `fraction` is the
fault. Worse, it is not even an *error*: it degrades to a `Box<Enum>` layout silently.

**Checking-first output (family 1C).** CHECK mode at the arrow (`generate.rs:370` arm,
enriched): synthesize the branch body (`F64`), compare against the ascription (`Span`),
mismatch → emit a new obligation localized at the **arrow span**:

```
type mismatch at fraction:4:22..4:31  (`-> f64 : Span`)
  projection synthesizes F64 but is ascribed Span
  note: the surrounding `measure` alternation requires all branches to agree
```

`4:22..4:31` is the byte span of the `-> f64 : Span` arrow, carried from the frontend.
The side-by-side is the deliverable: **same fixture, same clash, two blame sites** —
synthesis blames `node 41` (an interior the author never wrote, span-less); checking
blames `fraction:4:22` (the arrow the author wrote). This is the family's central
evidence and it is fully constructible from the seams above.

---

## 4. Thread C #2 — the additive diff: span-carrying diagnostics + `Project` sub-span

Two enrichments, both additive, both naming their LOCK-3 sink and TS mirror per the
evidence bar.

**(a) A span on the diagnostic.** `TypeObligation` (`obligation.rs:56`) gains an optional
`span: Option<GrammarSpan>` on each variant (default `None`, so every existing
constructor and the `W3a-types-obligations.txt` archive stays byte-stable), plus a
third variant `ProjectionMismatch { rule, node, arrow_span: GrammarSpan, synthesized:
TypeDesc, ascribed: TypeDesc }` — the CHECK-mode failure. `diagnostic()` (`:113`) renders
the span when present. Additive: `+1 field`, `+1 variant`, `+~15 render LOC`; the drain
at `mod.rs:486` is unchanged.

**(b) The `Project` sub-span — carrying the arrow byte-range down.** The blocker is that
`IrNode::Map` / `FnDescriptor::Expr` carry no span. The minimal, colocation-clean carrier
is a **side table** on `GrammarIR`, isomorphic to the existing `node_facts:
HashMap<NodeId, NodeFacts>` (`grammar.rs:86`): add `map_spans: HashMap<NodeId,
GrammarSpan>` populated at lowering (`crates/core/src/lower/mod.rs`, beside the
`type_obligations: Vec::new()` init at `lower/mod.rs:243`) from the frontend AST's arrow
token range. This keeps `IrNode` serialization-stable (no per-node span bloat across the
WASM boundary) and gives CHECK mode a `NodeId → arrow-span` lookup at `generate.rs:370`.
The **frontend must capture the arrow span**: the bootstrap parser records the byte range
of each `->` (and its optional `: T`) as it produces the `Map`; today `dump_ir.rs:72`
shows the Map reaches IR with only `{inner, fn_id}`, so the span capture is a new
frontend field threaded through lowering — the one non-additive touch, isolated to the
parser→lower seam.

**LOCK-3 sink (XL-F, mandatory).** The enriched obligation must lower through
`TypeAnalysis::from_ir` (`crates/core/src/backend/driver/analysis.rs:22`) and the second
`from_ir` at `crates/core/src/backend/rust/view/named_types.rs:100`. `TypeAnalysis`
carries `rule_types: Vec<(RuleId, TypeDesc)>` and `scratch_types`; the obligation stream
rides alongside on `GrammarIR` and is already visible to both `from_ir` sites via `ir`.
The record design adds no runtime instruction (Constitution v — the type plane costs zero
at runtime): obligations are compile-time-only, drained before emission, consumed by the
LSP/analysis surface and CI, never by generated parser code. The `git diff` over
`crates/core/src/backend/rust/emitter` stays empty; the record touches only analysis +
diagnostics.

**TS mirror (mandatory).** `crates/core/src/backend/ts/projection.rs:40` already handles
`HeterogeneousAltJoin` identically to `BoxedEnum` and defers its evidence to
`ir.type_obligations` — so the TS backend is *already* an obligation-stream consumer by
reference. The span enrichment is mirror-symmetric: `type_desc_to_ts` needs no change
(spans are diagnostic metadata, not projected types), and the TS diagnostic surface reads
the same `ProjectionMismatch { arrow_span }` — the mirror is the *absence* of divergence,
which is the strongest form (LOCK-6 "reasonable point": diagnostics are behavioral, shared
by construction).

**Corpus green.** JSON (`grammars/json.bbnf`, zero `->`) produces zero obligations — the
enrichment is inert there. Sheets and CSS L4 (the `->`-bearing grammars) must regen
byte-identical: the obligation stream is compile-time metadata, so the generated parser is
untouched; the gate is `git diff` over generated output empty + the existing
`type_obligations` archive unchanged for the `None`-span default.

---

## 5. Open S — the cross-rule seam as an attribute grammar

The charter confines global solving to cross-rule seams; Open S asks whether those seams
are one **attribute-grammar** discipline rather than a fifth engine. They are, and the
bidirectional modes ARE the attribute directions (Knuth, *Semantics of Context-Free
Languages*, 1968; the synthesized/inherited split is original to it):

- **Synthesized attribute** = SYNTH mode. A rule's output type is computed *bottom-up*
  from its body — exactly `project_seq_type` (`revise.rs:46`) and `join_types` (`:116`)
  composing child types into the parent. No annotation, information flows up.
- **Inherited attribute** = CHECK mode. An *expected* type flows *top-down* from context
  into a subterm — the `return_type` ascription pushed into the branch bodies, the shared
  `Alt` leaf pushed into each arm. This is the direction HEAD lacks; installing it at
  `generate.rs:370` is literally adding the inherited-attribute edge.
- **Circular attribute** = the cross-rule fixpoint. Mutually recursive rules
  (`value → object → pair → value`) form an attribute cycle; the projection CSP already
  resolves it by fixpoint with a cycle-break ground (`obligation.rs` `cyclic == true`,
  `mod.rs` post-propagation path). This is a **bounded circular attribute grammar** in the
  Jones/Farrow sense (Farrow, *Automatic generation of fixed-point-finding evaluators for
  circular attribute grammars*, PLDI 1986; Magnusson & Hedin, *Circular Reference
  Attributed Grammars*, JVLC 2007 — the JastAdd basis).

The **termination lattice** is the discipline that makes the circular case total. The
type lattice is finite-height by construction: `TypeDesc` (`type_desc.rs:13`) has a fixed
scalar floor (Span, F64, Bool, I8..U64) and monotone constructors (Option/Vec/Tuple)
bounded by grammar nesting depth; the LUB (`join_types`) is monotone and idempotent; the
cycle-break grounds any unresolved var to the top of the lattice
(`HeterogeneousAltJoin`/`BoxedEnum`). The three whole-system phenomena the charter names
collapse onto one lattice discipline:

- **SCC-generalize** = compute each strongly-connected rule component's attributes to a
  local fixpoint before generalizing (let-generalization over the SCC, GHC-style).
- **federation-sequencing** = the topological order over the condensed rule-dependency
  DAG is the attribute-evaluation schedule; acyclic segments are one bottom-up pass,
  SCCs are the bounded-fixpoint islands.
- **bounded-fixpoint** = the finite-height lattice + monotone revise gives a guaranteed
  ascending-chain termination bound = lattice height × variable count.

Evidence claim: this is **not a fifth engine** — it is a *reading* of the existing
projection CSP (`passes/types/`) as an attribute evaluator, where SYNTH/CHECK are the two
attribute directions and the cycle-break is the circular evaluator. The prototype
obligation is to show the reading is faithful, not to build a new solver.

---

## 6. Prior art, with citations

- **Bidirectional typing** — Pierce & Turner, *Local Type Inference* (TOPLAS 22(1), 2000);
  Dunfield & Krishnaswami, *Bidirectional Typing* (ACM Computing Surveys 54(5), 2021, the
  canonical survey). The check/synth mode split and its localization property are their
  contribution; family 1C is a direct application to grammar projections.
- **rustc's `Expectation`** — `rustc_hir_typeck::expectation::Expectation`
  (`ExpectHasType` / `NoExpectation`) is production bidirectional checking: an expected
  type flows down into expression checking so errors localize at the expression, not at a
  distant inference variable. This is the exact model for CHECK mode at `generate.rs:370`,
  and rustc's own diagnostics literature cites wrong-place-blame as the motivation.
- **Attribute grammars** — Knuth 1968 (synth/inherited); Farrow PLDI 1986 and
  Magnusson & Hedin JVLC 2007 (circular AGs with fixpoint termination, the JastAdd
  reference attribute grammar system) — the Open S model.
- **Span-carrying diagnostics as first-class IR** — MLIR's `Location` attribute (every op
  carries a source location, diagnostics reference it; `mlir::Diagnostic` +
  `emitError(loc)`) and rust-analyzer's `TextRange`-anchored `Diagnostic` are the pattern
  for the `map_spans` side table: source location is metadata attached to IR nodes, never
  lost across lowering. The greenfield's `GrammarSpan` (owned `u32` pair, WASM-safe) is the
  right primitive; it is simply not yet attached below rule level.
- **Elm / Roc error localization** — the industry standard for "point at exactly the token
  the author wrote"; the design target for the §3 side-by-side.

---

## 7. Named risks and counterexamples

- **R1 — checking silently narrowing a legal synthesis.** If CHECK mode is installed as a
  hard equality (`synth == ascribed`), a legitimately-widening ascription
  (`digits -> f64` where the body synthesizes an integer scalar that *should* coerce to
  `f64`) becomes a false-positive error. **Counterexample:** `int -> f64` — body
  synthesizes `U32`, ascription `f64`, a lawful numeric widening. **Resolution:** CHECK is
  *subsumption*, not equality — the compare at `generate.rs:370` must run through a
  subtype/coercion lattice (the same numeric-width lattice LOCK-1's width sub-problem
  owns), and only emit `ProjectionMismatch` when no coercion exists. The check leaf must be
  the lattice `⊑`, not `==`. This is the sharpest risk: it decides whether CHECK mode is
  *sound* or merely *strict*.
- **R2 — no arrow span in the frontend.** If the bootstrap parser does not record the
  `->` byte range, the whole localization story reduces back to rule granularity. **The
  frontend span-capture is the one hard prerequisite** — verified absent at HEAD
  (`dump_ir.rs:72` shows `Map { inner, fn_id }` only). Prototype must land it or the
  §3 checking-mode span is unreachable.
- **R3 — the obligation stream has no LSP consumer.** Enriching `TypeObligation` with a
  span is inert if nothing renders it. **Constitution viii forbids the consumer-less
  substrate**: the diff MUST include an `analysis`-crate bridge
  (`type_obligations → lsp_types::Diagnostic` with `range` from the span). Without it the
  wave is a substrate-only landing and fails the bar.
- **R4 — CHECK mode changes projected types → regen not byte-identical.** If CHECK mode
  *alters* the resolved `TypeDesc` (e.g. rejecting a join that HEAD silently boxed), the
  generated parser moves and the "compile-time only / zero runtime" invariant breaks.
  **Resolution:** CHECK mode emits a *diagnostic* and leaves the projected type exactly as
  synthesis-first produced it (the obligation augments, never overrides — the same
  contract `HeterogeneousAltJoin` already honors: it records evidence but still lowers to
  `Box<Enum>`). Diagnostics-first ≠ type-changing.
- **R5 — cross-rule cycle non-termination under CHECK.** Adding the inherited (top-down)
  attribute edge can, naively, break the fixpoint's monotonicity. **Counterexample:** a
  recursive rule whose inherited expected type depends on its own synthesized type.
  **Resolution:** the termination lattice (§5) — inherited edges only *narrow* toward the
  lattice bottom and the cycle-break grounds to top; the ascending-chain bound holds
  because CHECK never re-widens. Prototype must exhibit a recursive `->` grammar and show
  the fixpoint still converges.

---

## 8. What a prototype must test to move this family

1. **The side-by-side (Thread C #1, primary).** On `bidir_width.bbnf` (§3), produce BOTH
   the synthesis-first obligation (`HeterogeneousAltJoin { node: <Alt NodeId>, [F64, Span] }`,
   span-less, blaming the join) AND the checking-mode obligation
   (`ProjectionMismatch { arrow_span: fraction:4:22..4:31, F64, Span }`, blaming the
   arrow). Diff the two blame locations. **Pass = the two modes name different spans and
   the checking span is the author's arrow.** This is the family's make-or-break.
2. **The additive span landing (Thread C #2).** Land `TypeObligation.span` +
   `ProjectionMismatch` + `GrammarIR.map_spans` + the frontend arrow-span capture + the
   `analysis`-crate LSP bridge. Gate: additive `git diff` (no `IrNode` variant reshaping),
   corpus regen byte-identical (JSON zero obligations; Sheets/CSS L4 generated output
   unchanged), and the LSP bridge renders a `Diagnostic` with the arrow `range`.
3. **Subsumption not equality (R1).** A fixture with a lawful widening ascription
   (`int -> f64`) must produce **zero** `ProjectionMismatch` — proving CHECK runs through
   the coercion lattice, not `==`.
4. **Circular termination (R5).** A recursive `->` grammar (`value → array → value`, each
   arm projected) must reach the projection fixpoint with CHECK edges installed, in bounded
   iterations (≤ lattice-height × var-count). Report the iteration count.
5. **Zero-runtime invariant (Constitution v).** `git diff` over
   `crates/core/src/backend/rust/emitter` and generated parser output = empty across all of
   1–4. The type plane's win is localization, never a runtime instruction.

The measurement substrate is skinny@HEAD for wall-time neutrality (the projection CSP runs
at generation time; the family adds compile-time diagnostic work only, which must not
regress derive wall-time beyond noise). The designs target greenfield structure —
`crates/ir/src/passes/types/` is the home, `TypeAnalysis::from_ir` the handoff, the
`analysis` crate the diagnostic consumer.
