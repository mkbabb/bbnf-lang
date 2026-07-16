# LOCK-3 · Family 3D — STAGED-RESIDUAL · pass-1 research

**Thesis.** The generator is *already* a partial evaluator; skinny simply refuses to
say so. Grammar → typed HIR → mid-IR → backend shape → emitted Rust is the **first
Futamura projection** run by hand: the grammar is the object program, the
recognizer-interpreter is the metaprogram, the generated `.rs` is the residual.
Family 3D makes the two-stage structure *explicit in the types* — every IR value
carries a binding time (Stage-0 = generation-time / static, Stage-1 = runtime /
input-dependent) — and defines **residualization as erasure of Stage-0**. Under that
discipline PROVE-12 (const-fold the CSS delimiter mask) stops being a clever manual
patch and becomes the *forced output* of a binding-time analysis; the standing laws
(no-value-discard, no-silent-epsilon, preserve-rich-AST) become type invariants the
compiler mechanically enforces rather than conventions reviewers police; and the
ambiguous per-site divisions (monomorphize-vs-thread, unroll-vs-loop, match-vs-table)
become decision variables handed to LOCK-2's cost/CSP engine under the generated-size
budget.

---

## 1. The mechanism, made concrete

### 1.1 The binding-time lattice and its law

Introduce a two-point lattice `Stage = Static ⊑ Dynamic`. A value is **Static** when
its value is fully determined by the grammar (known at `cargo xtask regen`), and
**Dynamic** when it depends on the input bytes (known only at parse time). The single
inference law (offline binding-time analysis, BTA): *an operation is Dynamic iff any
operand is Dynamic; otherwise Static.* This is a monotone fixpoint, structurally
identical to skinny's existing `nullability` at `skinny/crates/ir/src/lib.rs:320-349`
— same LLVM-style `changed` convergence (memory: changed-flag-convergence), same
`Vec<_>`-indexed-by-node SoA layout the tape already uses (R6 §1). The analysis is a
new sibling `pub fn binding_time(grammar: &GrammarIr) -> Vec<Stage>` seeding the
leaves:

- **Static leaves**: `ExprKind::Literal.bytes`, `Regex.pattern`, `Project.ty`,
  `Project.target`, `Repeat{min,max}`, `AltMode`, `SpanKind`, `CaseSensitivity`, the
  capacity plan constant (`GrowOnly→256`, R6 §1), the structural alphabet
  (`ir/src/lib.rs:435`), every delimiter/STOP set — *all grammar constants*.
- **Dynamic leaves**: the byte at a cursor, a matched span's contents, a parsed
  scalar (`materialize_f64`), the tape `offsets`/`flag_values`, the cursor position.
- **Propagation**: `Seq`/`Alt`/`Repeat`/`Discard` are Dynamic (they branch on input);
  a `Ref` inherits its callee's stage; a `Project` whose body is Dynamic but whose
  `ty`/`target` is Static is the canonical *mixed* node — a Dynamic residual carrying
  a lifted Static tag (this is exactly where the `->` discard bug lives, §3).

### 1.2 The two-level IR

`BackendExpr` (`skinny/crates/ir/src/lib.rs:376-413`) becomes a **two-level IR**: each
node is tagged with its stage (cleanest: a parallel `Vec<Stage>` indexed by node id,
SoA per the tape discipline — not a fat enum). Semantically the tower is unchanged
(constitution ix: the GN codex — `EagerTape|OffsetTape|EventTape|SinkOnly|
CollapsedStage` at `ir/src/lib.rs:361` — is re-cut, never contradicted); what changes
is that the tower now has a *typed seam* between the two stages. Borrowing MetaML's
notation, a Static subtree has type `T` and a Dynamic subtree has type `Code<T>` (a
generation-time-only handle to emitted Rust). `Code<T>` is a **compile-time-only
type**: it never exists in the residual (constitution v — the type plane costs zero at
runtime; verified by the standing `payloads().len()==0` / zero-alloc invariant at
`skinny/crates/runtime/src/lib.rs:42-44`).

### 1.3 Residualization = the specializer

The lowering `BackendIr → Rust` is re-expressed as a **specializer** that walks the
staged IR once:

- A **Static** node is *executed* at generation time; its result is a value (a mask, a
  match table, a byte count, an erased Unit) — nothing is emitted for it directly.
- A **Dynamic** node *emits residual Rust*.
- A **Static value in a Dynamic context** is **lifted** (MetaML "cross-stage
  persistence"): it becomes an immediate/const-generic in the residual. This lift IS
  PROVE-12: `delim_mask(b";{}")` is the Static evaluation, `const DELIMS: u128` is the
  lift, `(DELIMS >> byte) & 1` is the residual over the Dynamic `byte` (patch.diff at
  `runtime_generator.rs` CSS template; disasm `mov x27,#0x2800…`, no slice loads, +18
  LOC, byte-identical — SPEC §0 row 12 CONFIRMED). Under BTA this promotion is not
  authored: `delimiters` is a Static operand threaded through a Dynamic loop, so the
  specializer is *obligated* to lift it. PROVE-12 is a corollary.

### 1.4 CollapsedStage reclaimed

`BackendShape::CollapsedStage` (`ir/src/lib.rs:361`; admissibility at
`passes/src/lib.rs:555-664`) is a dormant, consumer-less emit path flagged
consumer-or-retire in the GN codex (Y.13 forbids consumer-less substrate). Family 3D
gives it its *literal* meaning: the shape where Stage-0 **collapses** Stage-1 — a rule
whose entire body residualizes to a straight-line, tape-free recognizer (maximal
specialization; the lattice endpoint where no interpretive dispatch survives).
CollapsedStage becomes the named home for a fully-residualized rule, retiring the
dormant surface by giving it staged semantics rather than deleting it.

---

## 2. Substrate & integration seams (path:line)

- **BTA fixpoint** — new `binding_time` beside `nullability`,
  `skinny/crates/ir/src/lib.rs:320-349` (verbatim convergence shape).
- **Stage carrier** — parallel `Vec<Stage>` over `BackendExpr` nodes,
  `skinny/crates/ir/src/lib.rs:376-413`.
- **The specializer** subsumes today's scattered lowering: `passes::lower_expr`
  (`Project → ValueProject` at `skinny/crates/passes/src/lib.rs:1349-1353`) and the
  emitter modules under `skinny/crates/codegen/src/lower/`. The ad-hoc const-folds
  (PROVE-12's mask, the ws-blanket `SpanKind::Whitespace → Unit` at
  `passes/src/lib.rs:225-231`, the capacity constant, the first-byte dispatch tables
  at `json_sink_direct/dispatch.rs`) unify into *one* Static-evaluation pass.
- **The extant baseline this supersedes (named, constitution ix).** The GN codex
  already generalizes PROVE-12 into a *shared const-mask module*
  `skinny/crates/codegen/src/residual/` (`delim_mask.rs` + `const_membership.rs`,
  GN-5.W3), routing every stop-set (`matches!(_b, 44u8|34u8|…)` CSV textdata, JSON
  structural scan, general-descent) through one `find_stop<const MASK:u128>` seam.
  **Family 3D's supersession**: that seam is a *mechanism-level* hoist of one
  transform; family 3D lifts it to a *principle-level* two-level IR where the const-
  mask seam, the ws-erasure, and the dispatch-table fold are all instances of one
  binding-time division — and where the division is *derived*, not hand-placed. The GN
  residual module becomes the specializer's Static-membership emitter, one lift rule
  among several, not the whole story.
- **The cost/CSP seam (LOCK-2, §4)** — `CostFacts`/`ActiveCostFacts`/
  `DecisionCspFacts`/`PriorityStep` at `skinny/crates/ir/src/cost.rs:5,173,203,264`;
  the existing CSP-selected lowering precedent is `passes::derive_backend_shape`
  (`skinny/crates/passes/src/lib.rs:398`) reading `BackendShapePlan.cost_facts`. The
  staging division is a *new decision variable* in this same driver — no orthogonal
  subsystem (memory: system-cohesion).
- **The budget constraint** — `xtask lint-loc` (LOC-budget gate,
  `skinny/xtask/src/main.rs:49`) is the hard constraint the specializer's CSP must
  satisfy (memory: generated-size-budget).
- **Determinism** — the specializer must regen bit-for-bit (memory:
  clean-regen-discipline); ties in the CSP need a total order on candidate divisions.

---

## 3. Why the staged types *are* the standing laws

The LOCK-3 standing laws stop being convention and become mechanical:

- **no-value-discard.** `Infer::infer_expr` throws away `Project.ty` —
  `skinny/crates/passes/src/lib.rs:203-204` returns `self.infer_expr(*body)?`,
  silently dropping the Static projection tag (R6 §5a; the same drop makes every
  `DirectBuildField.target` `None`, R6 §5c). In a two-level IR a Static value that is
  neither consumed nor lifted is a **binding-time error** — the specializer cannot
  erase a Static operand into nothing, it must fold it or lift it. The exact class of
  bug the law names is caught by construction.
- **no-silent-epsilon.** An unknown `rule_kind` at lowering is a *Static* computation
  with no value; the specializer must `panic` at generation (memory:
  no-silent-epsilon), never emit an epsilon residual — a Static failure is a
  generation-time failure, categorically not a runtime Unit.
- **preserve-rich-AST / diagnostics-first.** `SourceSpan` (`ir/src/lib.rs:21-25`) is
  marked *Dynamic-adjacent*: it rides into the residual as a lifted constant so error
  messages keep their grammar span even after Static folds erase surrounding
  structure. Over-eager folding that drops a span is, again, a binding-time violation.

---

## 4. The LOCK-2 seam — per-site staging as a solver problem

BTA gives a *safe* division (everything reachable from a Dynamic operand is Dynamic),
but not a *unique* one. Several sites are genuinely bistable, and the choice is a
classic partial-evaluation *poly-variance / specialization-depth* decision with a
runtime-cost vs code-size trade:

| Site | Static (residualize) | Dynamic (thread) | Trade |
|---|---|---|---|
| delimiter set (PROVE-12) | const-generic monomorph per site | one `&[u8]` fn | ins/B ↓ vs +N monomorphs |
| bounded `Repeat{min,max}` | unroll to straight-line | runtime loop | branch elision vs LOC blowup |
| first-byte dispatch (`json_sink_direct/dispatch.rs`) | `match` arms | lookup table | i-cache vs d-cache |
| recursive `Ref` | inline (DANGER §5.1) | `CallRule` | *must stay Dynamic* |

Each is a discrete variable; the constraint set is BTA legality (a Dynamic operand
forbids a Static result) plus the `lint-loc` budget; the objective is the cost lattice
fed by canon-bench PMU facts (`cost.rs`). This is exactly LOCK-2's engine: the e-graph
*generates* candidate residualizations (equality-saturation over the staged IR —
rewrites that move the Static/Dynamic frontier), CSP *extracts* the division
minimizing cyc/B subject to LOC. "Per-site staging solved by LOCK-2's engine" is
therefore concrete: the staging division is a first-class decision variable in the
same `DecisionCspFacts` driver that already selects `BackendShape`, not a new
subsystem.

---

## 5. Named risks & counterexamples (what a prototype must kill)

**5.1 Specialization non-termination / code explosion** — the canonical PE failure.
Fully specializing a *recursive* rule (JSON `value → object → pair → value`) unrolls
forever and blows `lint-loc`. Mitigation: BTA must keep rule-recursion **Dynamic**
(generalized to a `CallRule`, never inlined); the CSP bounds unroll depth against the
budget. *Counterexample to run*: nested-JSON (`data/json/citm`, `canada`) — confirm
the specialized `value` rule stays a call and LOC stays in budget.

**5.2 Stage machinery leaking into the residual** — if `Code<T>` has any runtime
footprint, constitution v is violated. *Oracle*: the standing `payloads().len()==0` /
zero-alloc assert (`runtime/src/lib.rs:42-44`) plus byte-identical parity vs the
hand-tuned emitter must both hold.

**5.3 Backend-dependent division (LOCK-6 / constitution iii)** — the optimal division
for Rust (lean on the monomorphizer: PROVE-12's `const DELIMS` yields 1 surviving
symbol + 2 fully-inlined) is *wrong* for the TS backend (V8 has no monomorphization;
Static values must be fully residualized at build time by the LOCK-4B/8 fuser — 5
specialized functions, not 1 generic). The CSP cost model must therefore be
**backend-parameterized**; a single division cannot be assumed isomorphic. *Named
tension*, unresolved — the first cross-backend risk this family owns.

**5.4 Binding-time improvement required** — naive BTA sometimes leaves too much
Dynamic; exposing more Static structure needs an IR rewrite first ("The Trick": bound
a Dynamic value to a Static enumeration). This is the LOCK-2 dependency: e-graph
rewrites reshape the IR so BTA sees more Static. Risk = 3D's residualization quality is
gated on LOCK-2 landing its rewrite library.

**5.5 Determinism under CSP ties** — non-deterministic tie-breaking breaks
clean-regen. Requires a total order over candidate divisions (lexical over site id).

---

## 6. Prior art (citations)

- **Futamura, "Partial Computation of Programs" (1971)** — the three projections; the
  first (specialize interpreter w.r.t. program → compiled target) is *precisely*
  skinny's generator. This family names what the generator already is.
- **Jones, Gomard & Sestoft, *Partial Evaluation and Automatic Program Generation*
  (1993)** — the canonical text: offline BTA, the two-point binding-time lattice, the
  Static/Dynamic congruence law, self-application, code-explosion control. §5.1's
  termination discipline is from here.
- **Nielson & Nielson, *Two-Level Functional Languages* (1992)** — the type-theoretic
  substrate for "comptime/runtime split in the types": binding-time *types*, the
  two-level λ-calculus. The literature for §1.2.
- **Taha & Sheard, "MetaML / Multi-Stage Programming with Explicit Annotations"
  (2000); MetaOCaml** — `Code<T>`, brackets/escape/run, cross-stage persistence (= the
  Static-value lift, §1.3). The engineering vocabulary for the two-level IR.
- **Rompf & Odersky, "Lightweight Modular Staging" (LMS, 2010); Delite** — the closest
  production analogue: `Rep[T]` in the *type* distinguishes Dynamic from Static, and a
  generative framework residualizes. Proof this is buildable in a typed host.
- **Andersen, C-Mix (1994)** — offline PE for *C*, the systems-language precedent (we
  emit systems Rust); binding-time-directed specialization of pointer code.
- **Wegman & Zadeck, Sparse Conditional Constant Propagation (SCCP)** — the classical-
  compiler twin of BTA: a Top/Const/Bottom lattice by sparse fixpoint. rustc's MIR
  const-prop and LLVM's SCCP/known-bits are BTA-in-disguise; the specializer is SCCP
  extended to *emit* rather than *rewrite*.
- **Zig `comptime`; Terra (Lua-staged C)** — the pragmatic single-language incarnation.
  Rust's `const fn` + const-generics + monomorphization is the same, weaker, offline
  two-level language — the exact substrate PROVE-12 exploits.
- **egg / egglog (Willsey et al., 2021)** — equality saturation + cost extraction for
  the §4 per-site division. **Denali** (Joshi–Nelson–Randall) is the goal-directed-
  superoptimizer precedent for e-graph-driven code selection.
- **MLIR (Lattner et al., 2021)** — the progressive-lowering dialect tower and the
  canonicalization/fold framework; the staged tower is MLIR-shaped (each stage a
  dialect, residualization a legalization). PDL/transform-dialect = the pluggable
  rewrite seam.
- **regex-automata (in-tree consumer)** — the compiled DFA is a *static* residual of
  the regex; the lazy DFA is *online* PE (residualize on demand) — both binding times
  in one family, relevant to LOCK-5's automata-as-extraction.
- **GHC Typed Template Haskell (`Code Q a`)** — a typed multi-stage system in a pure
  host; the direct MetaML descendant, a reference for making `Code<T>` sound.

---

## 7. What the pass-1 prototype must demonstrate (moves the family)

1. **Automate PROVE-12.** Feed the CSS `find_component_delim(pos, delimiters)` (5 call
   sites, each `delimiters` a grammar constant) through `binding_time`; the specializer
   must *derive* the const-mask residual (byte-identical to PROVE-12's hand patch —
   parity is the acceptance oracle) with the same ins/B win (−4.8/−6.8%, SPEC §0 row
   12) and no LOC regression beyond +18. Success = residualization *forced by BTA*, not
   authored.
2. **Type-catch the discard.** Add the `Stage` tag to `BackendExpr`; show the
   `Project.ty` drop at `passes/src/lib.rs:203` becomes a binding-time error (a Static
   value erased into nothing) — no-value-discard enforced by the type, not the
   reviewer.
3. **Bound the recursion.** On nested-JSON, confirm the specialized `value` rule stays
   a `CallRule` (Dynamic) and LOC stays in `lint-loc` budget (§5.1 killed).
4. **Prove the LOCK-2 seam is real.** Take one bistable site (dispatch `match` vs
   lookup table, or bounded-`Repeat` unroll vs loop), formulate it as a `DecisionCsp`
   variable with cost + LOC constraint, and show the engine picks the cheaper division
   — demonstrating "per-site staging solved by LOCK-2's engine" is load-bearing, not
   decorative.
