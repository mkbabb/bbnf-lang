# LOCK-3 · Family 3C — GRAMMAR-HOMOIOUSIA · pass-2 research

*The IR is itself grammar-specified: a bbnf-defined IR grammar, transpositions expressed as
grammar-to-grammar maps, the generator eating itself.* Re-pinned to live `skinny@3f2350f15`
(HEAD; the directive's `b3cb8c16b` is stale — every seam below reopened against `3f2350f15`).
All `path:line` under `/Users/mkbabb/Programming/bbnf-lang/skinny/` unless prefixed.

---

## 0. The claim, stated so it can be falsified

Homoiousia asserts three things, each of which must be true on or near live for the family to
stand, and each of which the live tree currently only *partially* satisfies:

1. **The IR has a faithful grammar form.** `GrammarIr` (`ir/src/lib.rs:33`) and its `ExprKind`
   10-arm (`:233-271`) plus `BackendExpr` 13-arm (`:375-413`) already `derive(Serialize,
   Deserialize)` — the IR *has* a data form, but no `.bbnf` grammar describes that form and no
   round-trip is exercised anywhere (`serde_json` appears in the IR only inside emitted JSON
   value decoders, `json_templates/value.rs:94`, never over `GrammarIr` itself). The grammar
   form is **latent, not realized**.
2. **Transpositions are grammar-to-grammar maps.** Two live transpositions already *are* term
   rewrites in miniature — `lower_expr` (`passes/src/lib.rs:1318`, `ExprKind → BackendExpr`) and
   `materialize_rule` (`:1362-1389`, the `SpanMark(Start) · body · SpanMark(End) · TapeEmit ·
   DirectBuild · Return` splice). But they are hand-written Rust `match` arms, not declared
   rewrite rules, and the *decision* transpositions around them are **not** structural maps at all
   (§4).
3. **The generator eats itself.** It does not. `bbnf.bbnf` (`repo:grammar/bbnf/bbnf.bbnf`, 90
   lines) is a *descriptive* self-spec of the surface syntax; the live front-end
   `grammar::parse_grammar` (`grammar/src/lib.rs:29`) is 2142 LOC of hand-written Rust that never
   consumes `bbnf.bbnf`. Self-application is aspirational and belongs to the bootstrap lock.

The strongest honest form of 3C is therefore not "the IR becomes a grammar" as a monolith — it
is a **staged homoiousia**: realize (1) as a standalone round-trip witness that needs no gate;
realize the *syntax-directed* half of (2) as declared rewrites and let it **expose** the half
that cannot be a rewrite; and carry (3) plus the full-fidelity sum algebra as named born-RED
edges owned by sibling locks. The rest of this report makes each concrete.

---

## 1. The named downstream consumer (the directive's sharp question)

> *What named downstream wave would consume an IR expressed as a grammar, and what evidence
> establishes that consumer on or near live?*

**There is exactly one, and it is not inside LOCK-3.** The PORTFOLIO codex names it verbatim:
LOCK-7 **7B TOTAL-SELF-HOST** — *"bbnf grammar, regex grammar, **IR grammar**, emitter templates
— all grammar-specified; the generator becomes mostly data"* (PORTFOLIO LOCK-7, 7B). The gestalt
DAG places LOCK-7 downstream of LOCK-3 inside GESTALT-B (which "weaves LOCK-1/2/3/7"). So an
`ir.bbnf` is **consumer-less within LOCK-3's own live surface**; its sole consumer is a sibling
lock's not-yet-ratified family.

Under Constitution clause viii (Y.13 — *consumer-less substrate is forbidden, in designs as in
code*), this is dispositive: **3C is not independently landable.** It cannot ship an `ir.bbnf`
into the live tree with no consumer. Two lawful outcomes only:

- **(A) 3C lands AS a LOCK-7 co-wave** — the IR grammar and 7B's totality land together, one
  wave, one consumer reading the substrate the same wave it appears (the P8/P40 in-wave-consumer
  rule the GN codex enforces per wave, e.g. GN-3 §4 "Consumer in this wave (Y.13)").
- **(B) 3C is carried as a born-RED dependency edge** whose owner is LOCK-7/7B, with the
  near-live witnesses of §5 as the *only* thing 3C lands standalone.

Either way the disposition is honest and named: **3C's mechanism is real and prototype-provable,
but its production home is LOCK-7, not LOCK-3.** LOCK-3 owns the *transposition contracts*; 7B
owns the *grammar-of-the-IR itself*. This is the boundary the family must not blur.

The directive's warning — *"do not treat the product-only, sum-excluding subset as standing for
the formulation"* — bites here. One could show `DirectTypeKind::Struct` (`direct_schema.rs:52-58`,
the **only** variant today) round-tripping through a grammar and declare victory. That is a lie:
the grammar's natural output is a recursive **sum** (`value = object | array | string | number |
bool | null`), which the Struct-only schema IR cannot represent. The sum algebra is a **second
born-RED edge**, owner **GN-3.W1** (the Sum Node: `DirectTypeKind::Sum` + HM `unify`-over-sums,
GN-3 wave manifest W1). 3C's full form depends on both edges (7B consumer, GN-3.W1 sum node); its
partial form (§5 witnesses) depends on neither.

---

## 2. XL-C made concrete: the IR grammar keeps regex opaque

XL-C (RATIFIED) dissolves the ONE-HIR dependency — the grammar tier keeps `Regex(StringId)`
opaque. On live this is already true and load-bearing for 3C: `ExprKind::Regex { pattern: String
}` (`ir/src/lib.rs:250`) carries the pattern as an **opaque String**; nullability calls
`analyze(pattern)` (`ir/src/lib.rs:331`) through the `bbnf_regex` facade but never destructures
the regex. In the surface self-grammar the same opacity holds: `regex = ( "/" , /(\\.|[^\/])+/ ,
"/" ) -> Span` (`repo:grammar/bbnf/bbnf.bbnf:15`) — a regex literal is a `Span` leaf.

**Consequence for the IR grammar:** an `ir.bbnf` production for `Regex` is a single
String-carrying terminal. The IR grammar does **not** need LOCK-5's HIR to be lawful — regex is a
sealed leaf token, exactly as `bbnf.bbnf` already treats it. This is the precise reason the
directive says 3C "no longer hangs on a ONE-HIR verdict": the homoiousia map `ExprKind::Regex →
ir.bbnf::regex` is `String → String`, total and gate-free.

---

## 3. The enumerated transposition edge-DAG on live (the substrate)

The live spine, edge by edge — this is what a grammar-to-grammar formulation must reproduce:

| Edge | From → To | Live mechanism (path:line) | Grammar-map-able? |
|---|---|---|---|
| **E1** | `.bbnf` source → `GrammarIr` | `grammar::parse_grammar` (`grammar/src/lib.rs:29`), hand-written 2142 LOC | only via bootstrap (LOCK-7) |
| **E2** | `GrammarIr` → typed facts | `Infer::infer_expr` (`passes/src/lib.rs:184-213`); `node_types` fold | **partial** — structural fold yes, `Project.ty` **dropped** at `:204` |
| **E3a** | `ExprKind` → `BackendExpr` | `lower_expr` (`passes/src/lib.rs:1318-1359`) | **yes** — pure syntax-directed rewrite |
| **E3b** | rule → materialization splice | `materialize_rule` (`:1362-1389`) | **yes** — a fixed rewrite template |
| **E3c** | grammar → roles | `derive_materialization_roles` (`:1403-1490`), byte-literal + regex-kind heuristics | **no** — global analysis, not a rewrite |
| **E3d** | grammar → plan / shape | `derive_materialization_plan` (`:1139`); `BackendShape` 5-arm (`ir/src/lib.rs:360`) via cost facts | **no** — cost-directed decision (LOCK-2) |
| **E4** | `BackendIr` → Rust | `json_sink_direct` / `general_descent` / `css_scan_direct` emitters + `runtime_generator` static templates (`include_str!("json_templates/*.rs")`, `runtime_generator.rs:101-133`) | **partial** — JSON templates static, not grammar-derived |

The distinct **value levels** on the spine (reconciling the directive's `materialize_rule`
framing with the sink-only program): structural EBNF (E1 output, no value) → `SpanMark`-bracketed
spans → `TapeEmit{TapeKind}` coarse 9-arm tag (`ir/src/lib.rs:453-463`) → `DirectBuild{shape,
fields}` typed record (`ir/src/lib.rs:403-406`) → `Return`. Under homoiousia these five are not
five Rust enums but **five stages of one meta-grammar** — the same production set decorated by
successive rewrite passes (the Trees-That-Grow shape, §6). That reframing is the family's
aesthetic payoff; §4 is why it is only *half* achievable.

---

## 4. The counterexample the family must own: not every edge is a rewrite

The honest negative result — and the sharpest thing 3C can contribute — is that **E3c and E3d
are not grammar-to-grammar maps and never can be.**

- `derive_materialization_roles` (`passes/src/lib.rs:1403-1490`) recovers the JSON value taxonomy
  by *global* structural heuristics: a rule with `{`+`}` literals → container, `[`+`]` →
  sequence, `SpanKind::String` regex → string, a rule referencing ≥4 value targets → `value`
  (`:1462`). This is a whole-grammar analysis with a fixpoint flavor, not a local term rewrite. No
  rewrite-rule language (nanopass, PDL, Stratego) expresses "the rule that references ≥4 others."
- `BackendShape` selection (`EagerTape | OffsetTape | EventTape | SinkOnly | CollapsedStage`,
  `ir/src/lib.rs:360-367`) is a **cost-directed** decision fed by measured `CostFacts` /
  `DecisionCspFacts` (`ir/src/cost.rs`, re-exported `ir/src/lib.rs:8-13`) — LOCK-2's engine. A
  grammar-to-grammar rewrite is syntax-directed by construction; it cannot minimize a cost lattice
  over the whole document. **Counterexample:** OffsetTape-vs-SinkOnly for `json` depends on
  paired-PMU cost facts, not on any local production shape.

**Therefore homoiousia is partial by nature:** the grammar-to-grammar map covers the
*syntax-directed* lowering (E3a, E3b, the `Project.ty` thread of E2) and **exposes** the
cost-directed and heuristic edges (E3c, E3d) as the residue that stays host passes. This is not a
defect to hide — it is the family's precise contribution: **it draws the line between what the IR
grammar can absorb and what must remain the decision driver.** LOCK-3 owns *where the driver sits*
(the fact contract at the L1→L2 edge, per the directive Boundaries); homoiousia says the
syntax-directed transpositions move into `ir.bbnf`, and the cost decisions stay in the driver
LOCK-2 supplies. The two are kept distinct exactly as the directive demands.

The payoff of drawing that line: **E3c's heuristic recovery layer is retired, not relocated.**
When roles are *declared* productions in the IR grammar (because the surface grammar's `->`
projections already declare them — `mapped_factor = factor , ( "->" ?w , ( value_expr ,
type_annotation ? ) ) ?`, `repo:grammar/bbnf/bbnf.bbnf:42`), the byte-literal role classifier
(`:1403`) and the `has_rule("value")` name-gate (`runtime_generator.rs:98`; `rule.name ==
"value"` at `json_sink_direct/mod.rs:98`) become **deletable**. That cures Constitution clause vi
(*no name-gates*) by construction — the taxonomy is grammar-declared, not heuristically recovered.

---

## 5. How 3C consumes the threaded `->` type (the shared question), and what lands standalone

> *What does threading the `:204` `ty` require, and how does 3C CONSUME one threaded node-type
> rather than re-deriving it?*

On live: `ExprKind::Project { body, target, ty }` (`ir/src/lib.rs:261-265`) — `infer_expr` returns
`self.infer_expr(*body)?` at `passes/src/lib.rs:204`, **discarding** `target` and `ty`; `lower_expr`
carries them verbatim into `BackendExpr::ValueProject` (`:1349-1353`); the materialization plan
leaves `DirectBuildField.target` (`ir/src/lib.rs:466-471`) as `None` for every offset-tape
descriptor.

**The homoiousia consumption contract (the distinguishing answer):** in the homoiousia frame the
surface-grammar production `-> value_expr : type` (`bbnf.bbnf:42`) and the IR-grammar `Project`
production are the **same production under a refinement map** — the IR grammar is a decorated
refinement of the surface grammar, not an independent tree. So there is *nothing to re-derive*:
`ty` flows by **structural identity of the two grammars** into `DirectBuildTarget.type_ref`
(`ir/src/lib.rs:485`). Contrast the sibling formulations, which each re-infer a type at the
consumer (`infer_expr` re-runs a structural fold; `derive_materialization_roles` re-classifies by
byte-literal). 3C reads *one* node — the `Project.ty` the surface grammar already wrote — and
carries it, retiring both re-derivations. This is the "consume, don't re-derive" bar made exact:
**the transposition preserves the `ty` production; the role classifier and the 4-way
`ProjectionKind` fold that would otherwise re-derive it are deleted.**

**What 3C can land standalone (no gate), the near-live witnesses:**

- **W-3C-1 · Round-trip witness.** Author `ir.bbnf` whose productions are the 10 `ExprKind` arms
  (regex an opaque `Span` leaf per §2). Because `ExprKind`/`BackendExpr` already
  `derive(Serialize, Deserialize)` (`ir/src/lib.rs:33`), parse a *live* compiled `GrammarIr`
  (json + sheets + css) into `ir.bbnf`'s data form and back; assert **serde-equal** to the Rust
  value. Zero gate: the product+sum of `ExprKind` is already representable in serde. This proves
  claim (1) — the IR has a faithful grammar form — on live, today. Metric: byte-round-trip
  identity across the three live IRs.
- **W-3C-2 · Rewrite-as-data witness.** Express `materialize_rule`'s splice (`:1362-1389`) as a
  *declared* grammar-to-grammar rewrite (a data-defined rule, MLIR-PDL / nanopass form) and show
  the produced `BackendIr` is serde-identical to the hand-Rust output on `json.bbnf`. This tests
  claim (2)'s achievable half **and forces the §4 negative result into the open** — the attempt to
  also express E3c/E3d as rewrites fails, and that failure is the recorded evidence that draws the
  driver boundary.
- **W-3C-3 · Retirement witness.** With `Project.ty` threaded (`:204` no longer dropping it →
  `DirectBuildTarget.type_ref` filled, was `None`) and roles read as grammar-declared productions,
  **delete** `derive_materialization_roles` (`:1403`) and the `has_rule("value")` gate
  (`runtime_generator.rs:98`, `json_sink_direct/mod.rs:98`) and prove regen **byte-identical**
  (`xtask regen-json && check-json`; `regen-sheets && check-sheets`). RED on HEAD (name-gate
  present), GREEN when roles are declared. This is the "retirement, not relocation" bar and the
  clause-vi cure, both artefact-backed by regen byte-diff.

The full-fidelity form (sum node + 7B consumer) stays born-RED beyond these three: its gate is
**GN-3.W1** (`DirectTypeKind::Sum`) ∧ **LOCK-7/7B** (the IR-grammar consumer).

---

## 6. Prior art / SOTA (citations)

- **Nanopass framework** — Sarkar, Waddell, Dybvig, *"A Nanopass Infrastructure for Compiler
  Education"*, ICFP 2004; Keep & Dybvig, *"A Nanopass Framework for Commercial Compiler
  Development"*, ICFP 2013. Each IR is a **declared grammar** (`define-language`); passes are
  grammar-to-grammar transformers with auto-generated catamorphism boilerplate. **This is 3C
  exactly**, and it is production-proven: the Chez Scheme compiler is ~50 nanopass languages. It
  is also the direct evidence for §4's line — nanopass passes are syntax-directed; whole-program
  analyses (register allocation, cost decisions) are *not* nanopasses, they are ordinary functions
  over the IR. Chez keeps them separate, precisely as 3C proposes for E3c/E3d.
- **MLIR** — Lattner et al., *"MLIR: Scaling Compiler Infrastructure for Domain Specific
  Computation"*, CGO 2021. Dialects declared in ODS/TableGen; the IR has a textual generic form
  that round-trips (claim 1 industrialized); **PDL** (Pattern Descriptor Language) is an MLIR
  dialect that expresses *rewrites as IR data* — "transpositions are grammar-to-grammar maps"
  realized at industrial scale. PDL is the SOTA witness that declared rewrites over a
  grammar-defined IR scale; MLIR's separate `Pass`/`Analysis` machinery is the witness that
  cost-directed transforms stay imperative (§4).
- **Ott** — Sewell et al., *"Ott: Effective Tool Support for the Working Semanticist"*, ICFP
  2007 / JFP 2010. Write a language's grammar **once**; Ott generates the Coq/Isabelle/HOL/LaTeX
  definitions. The canonical "single grammar as the source of all downstream artifacts" — the
  "generator eats itself" ideal (claim 3), and the model for 7B's "generator becomes mostly data."
- **Spoofax / SDF3 + Stratego** — Visser et al. SDF3 grammar generates the AST signature;
  Stratego term-rewrites over that signature. Homoiousia (grammar *is* the signature) as a shipped
  language workbench.
- **K Framework** — Roșu & Șerbănuță, *"An Overview of the K Semantic Framework"*, JLAP 2010. A
  language = grammar + rewrite rules over a grammar-defined configuration; everything is a term
  rewrite. The maximalist form of claim (2).
- **Trees That Grow** — Najd & Peyton Jones, Haskell Symposium 2017. One AST datatype indexed by a
  phase, so each compiler phase is the **same grammar** decorated differently — the type-level
  witness for §3's "five value levels are five stages of one meta-grammar," now GHC's live AST.
- **egg / egglog** — Willsey et al., POPL 2021; Zhang et al., *egglog*, PLDI 2023. Rewrite rules as
  data over a declared sort with e-graph saturation — the bridge to LOCK-2, and the engine that
  *could* host the cost-directed edges 3C exposes (E3d), turning §4's residue into a saturation
  problem rather than a hand pass. Named as the interlock, not adopted here.
- **JetBrains MPS** — projectional editing where the concept *is* the grammar (no parse step). The
  editor-side proof that homoiousia removes the parser/AST gap entirely.

---

## 7. Named risks and counterexamples

- **R-3C-1 · Transposition non-expressibility (the load-bearing risk).** E3c (role heuristics,
  `:1403`) and E3d (cost-directed `BackendShape`, `ir/src/lib.rs:360` + `CostFacts`) are not
  structural rewrites. **Counterexample:** "the rule referencing ≥4 value targets is `value`"
  (`:1462`) and "OffsetTape iff measured cost wins" have no grammar-to-grammar expression.
  Mitigation is *not* to force them — it is to **name them as the driver residue** (LOCK-2/LOCK-3D
  home) and let W-3C-2 surface the boundary as evidence. If a synthesis pass later claims the whole
  spine is grammar-to-grammar, this risk refutes it.
- **R-3C-2 · The sum-node gate.** Without `DirectTypeKind::Sum` the IR grammar cannot faithfully
  carry `value = object|array|...`; the product-only subset is not the formulation (directive's
  explicit prohibition). Born-RED edge, owner **GN-3.W1**.
- **R-3C-3 · Consumer-less substrate (Y.13 / clause viii).** An `ir.bbnf` with no live consumer is
  forbidden. Only consumer is LOCK-7/7B. Born-RED edge, owner **LOCK-7**; 3C's standalone footprint
  is limited to the §5 witnesses.
- **R-3C-4 · Bootstrap regress / stage-0.** If the generator eats itself, the grammar that parses
  `ir.bbnf` must itself be compiled by a committed stage-0; unstaged, it is infinite regress. The
  fixpoint gate (`regen∘regen = regen`, byte-identical) is LOCK-7/7A's; 3C consumes that verdict,
  does not author it. **Counterexample:** editing `ir.bbnf` without re-pinning stage-0 silently
  forks the two.
- **R-3C-5 · Perf neutrality (clause v + GN-3 refutation guards).** Homoiousia is a generator-time
  restructuring; it must cost **zero** runtime instructions — the emitted parser byte-identical.
  Any grammar-to-grammar reformulation that perturbs emitted output is refuted at the
  `xtask check-json`/`check-sheets` byte-diff. The witness *is* byte-identity; there is no perf
  claim to make (a "faster because grammar-driven" assertion is refuted at the seam).
- **R-3C-6 · Generator-does-not-eat-itself today.** The live `parse_grammar` is 2142 LOC of hand
  Rust (`grammar/src/lib.rs:29`) and never reads `bbnf.bbnf`. Claiming self-application on live is
  false; 3C's honest state is "the surface self-spec exists as a document; the IR self-spec does
  not exist at all." Any report asserting the live generator is grammar-driven is refuted by the
  hand parser's LOC.

---

## 8. What a prototype must test to move this family

One prototype, three conjuncts, each independently gating:

1. **`ir.bbnf` round-trip (W-3C-1).** Ship an `ir.bbnf` of the 10 `ExprKind` arms; round-trip the
   live json/sheets/css `GrammarIr` through it via the derived serde form; assert value-equality.
   *Moves:* claim (1) from latent to realized, gate-free, on live. *Metric:* 3/3 IRs round-trip
   identical.
2. **`materialize_rule` as declared rewrite (W-3C-2).** Re-express the splice (`:1362-1389`) as a
   data-defined rewrite; assert the `BackendIr` is serde-identical to hand-Rust on `json.bbnf`;
   **attempt** the same for `derive_materialization_roles` (`:1403`) and record the failure. *Moves:*
   claim (2)'s achievable half to proven, and §4's boundary from assertion to evidence. *Metric:*
   E3a/E3b identical; E3c attempt produces a documented non-expressibility.
3. **Retirement under byte-identity (W-3C-3).** Thread `Project.ty` (`:204` → `DirectBuildTarget.
   type_ref`, was `None`); delete `derive_materialization_roles` (`:1403`) and the `has_rule
   ("value")` gate (`runtime_generator.rs:98`, `json_sink_direct/mod.rs:98`); prove `xtask
   check-json`/`check-sheets` byte-identical. *Moves:* the clause-vi name-gate cure and the
   "consume-not-re-derive" contract to artefact-backed. *Metric:* RED→GREEN with zero regen diff.

The full-fidelity landing (sum + self-hosting IR grammar + 7B consumer) is **not** a 3C prototype
target — it is the two born-RED edges (GN-3.W1, LOCK-7/7B) that 3C carries with named owners. The
family's deliverable to the gestalt is the trio above plus the drawn driver boundary: **the
syntax-directed spine becomes `ir.bbnf`; the cost-directed decisions stay the driver LOCK-2
supplies; the heuristic recovery layer is deleted, not moved.**
