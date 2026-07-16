# LOCK-3 — THE IR TRANSPOSITION SPINE — pass-2 prior-art research

Author: PRIOR-ART RESEARCHER, pass 2. Constitution-bound (PORTFOLIO.md head): no family
allegiance; concrete artifacts or SPEC-ONLY; consumer-less substrate forbidden (Y.13);
extant-tranche consciousness. REGISTRY/pass-1 deliberately unread. Every codebase claim
cited `path:line`; every web claim carries its URL.

## 0. The problem class (what LOCK-3 actually is)

The IR tower for a **parser generator that emits code** (not a runtime): grammar AST → typed
HIR → mid-IR → backend shapes → emission, with lowering contracts, residualization
(PROVE-12), a backend-agnostic middle for multi-backend (Rust now, TS later), under three
standing laws — no value-discard, no silent epsilon, preserve rich AST (SPEC §1; PORTFOLIO
LOCK-3 head). This is the classic **compiler middle-end design axis** — how many IRs, single
rich vs many small, attributes vs distinct types, staged vs monolithic — specialized to the
fact that our "program" is a grammar and our "output" is generated Rust/NEON. The four family
poles map cleanly onto four decades of settled middle-end debate (below).

## 1. HEAD baseline — we already run a 2-level tower

`GrammarIr` (`ir/src/lib.rs:33`) with `ExprKind` (`:234`, 11 variants incl.
`Project{body,target,ty}` `:261`) → `BackendIr` (`:351`) with `BackendExpr` (`:376`),
`BackendShape` (`:361`, a 5-variant *attribute enum* EagerTape/OffsetTape/EventTape/SinkOnly/
CollapsedStage), `Recognizer::SimdScan` (`:416`), and the `DirectBuild{shape,fields}` schema
(`:403`, `DirectBuildTarget` `:483`). Lowering is `passes::lower_expr`
(R6 `passes/src/lib.rs:1349`); emission is `codegen`. So HEAD is **not** greenfield: it is a
two-IR spine where `BackendShape` already behaves like an LLVM-style attribute on a shared IR
(pole 3B) but the second IR (`BackendExpr`) is a *distinct type* (pole 3A). The families are
choosing which of these two truths to make total, plus 3C (grammar-specify the IR) and 3D
(stage it). This dual nature is the design's central live tension, not a clean slate.

## 2. What exists / won / lost elsewhere for THIS class

**Single-IR-multipass (pole 3B) — the LLVM shape.** One IR, refined in place by annotating
passes; backend facts are *attributes*, not new types. Won as the universal backend; the cost
it pays is that LLVM-IR is low-level, forcing every frontend to re-lower and duplicating work
([MLIR TargetLLVMIR](https://mlir.llvm.org/docs/TargetLLVMIR/)). Sea-of-nodes and RVSDG are
the graph incarnations of the same "one IR, many rewrites" thesis. Our memory precepts
**no-orthogonal-codepaths** and **one-codegen-path** are the local statement of 3B's virtue:
a singular collection/lowering strategy, no conditional Vec-vs-scratch or combinator-fallback
branching. `BackendShape` as attribute (`ir/src/lib.rs:361`) is already this shape.

**Many-small-IRs (pole 3A/nanopass) — the Chez Scheme shape.** Nanopass: dozens of formally
defined micro-languages, each pass one tiny total transformation, boilerplate auto-derived
from `define-language` deltas; shipped in a **commercial** compiler (Chez)
([nanopass.org](https://nanopass.org/); [Keep & Dybvig, ICFP'13](https://andykeep.com/pubs/np-preprint.pdf)).
Won on *maintainability and checkability* (each language is a checkable contract — exactly
LOCK-3's 3A "round-trip/parity oracle per level"). **Named trap: compile-time blow-up** from
the extra passes and repackaging — the framework's own acknowledged downside. This is the
direct evidence *against* an unbounded tower and *for* 3A's own "small fixed tower (three
levels)" hedge; the generated-size/build-time budget precept (generated-size-budget) is our
guard.

**MLIR / progressive lowering — the backend-agnostic-middle shape (bears on 3B+multi-backend).**
Multiple abstraction levels coexist as *dialects*, progressively lowered by rewrite passes to
LLVM ([mlir.llvm.org](https://mlir.llvm.org/docs/TargetLLVMIR/); Flang studies
[arXiv:2310.01882](https://arxiv.org/pdf/2310.01882)). This is the strongest argument for a
backend-agnostic middle that emits `BackendShape` and later a TS shape from one core. **Named
trap:** dialect proliferation and the `unrealized_conversion_cast` / TypeConverter
materialization machinery — abstraction tax that only pays off at MLIR's scale. Our
countervailing law is the memory precept **no-ts-ir** ("AST IS the combinator; no IR layer for
TS") — a measured refusal to grow a second-backend IR speculatively. Multi-backend here is
`RustEmitter`/`Emitter`-trait shaped (project_multi_backend, driver_unification), *not*
dialect-shaped.

**Staged metaprogramming / partial evaluation (pole 3D) — the residualization theory.**
Multi-stage programming = quote/splice control over evaluation timing; LMS, MetaOCaml, Terra
are the canonical systems ([pls-lab MSP](https://www.pls-lab.org/multi-stage-programming);
[Amin notes](https://www.cs.uoregon.edu/research/summerschool/summer25/_lectures/Amin-notes-3.pdf);
[okmij generative](https://okmij.org/ftp/meta-programming/)). The **Futamura projections**
formalize "specialize an interpreter on a grammar → a compiler"; the first projection is the
core, realized in Truffle/GraalVM and PyPy/RPython at *runtime*
([Wikipedia partial evaluation](https://en.wikipedia.org/wiki/Partial_evaluation);
[Würthinger et al. 2nd-projection](https://dl.acm.org/doi/10.1145/3359061.3361077)). **Our
PROVE-12 is a hand-rolled, ahead-of-time first-Futamura specialization**: it const-evals +
beta-reduces the CSS delimiter membership test at *generation* time to a const-generic `u128`
mask — CONFIRMED, cyc/B 0.80-0.91×, +18 LOC, disasm-verified immediate (PROVE-12/REPORT.md
§2-4; SPEC §0 row 12). 3D's claim — "residualization is erasure of stage 0; PROVE-12 becomes a
corollary" — is thus *grounded in shipped, verified code*, the constitution's concrete-artifact
bar. **Named trap:** staging soundness — scope extrusion / hygiene (the MetaOCaml literature's
central hazard); a two-stage type system (3D) must prove stage-0 erasure is total, or it leaks.

**Parabix / Pablo — the closest living comparable on our third axis.** A bitstream IR (Pablo)
with a genuine transposition spine PabloBuilder → PabloAST → **LLVM**, and separately to **GPU
CUDA** and **ARM NEON** — a real multi-backend middle over *vectorized* parsing
(sk-v21 research/26:206-210; ARM report 2026-02-01). This is the only prior art that combines
a backend-agnostic IR **and** SIMD emission — LOCK-3's exact conjunction. **Decisive
adjudication already banked (SPEC §1 differentiator; sk-v21/26:207-215): Parabix is
regex-specific, not a general BNF/EBNF grammar generator** — its delivered artifacts are
icgrep/wc/csv2json. It proves the transposition-spine-to-SIMD architecture is buildable and
names the gap we fill; it does *not* pre-empt us.

**IR-as-grammar / self-hosting (pole 3C).** nanopass's `define-language` is itself a meta-DSL
that *is* the IR specification — precedent that "grammar-specify the IR" is real and shipped
([nanopass.org](https://nanopass.org/)). Our LOCK-7 bootstrap (STAGED-BOOTSTRAP, committed
stage-0, fixpoint regen∘regen=regen) is the discipline 3C's "generator eats itself" must obey.
**Named trap (memory no-ts-ir, restated):** grammar-specifying the IR is only justified where
a consumer reads it — a bbnf-defined IR grammar with no second reader is a Y.13 violation
(consumer-less substrate). 3C must name its in-wave consumer or it is SPEC-ONLY.

**Contract/oracle verification (bears on 3A).** The 3A "checkable contract, CI-enforced
round-trip/parity oracle per level" is the well-trodden **translation-validation / differential-IR
** method: RandIR differentially tests an embedded compiler by round-tripping randomly composed
IR against an oracle ([Ofenbeck et al., Scala'16](https://www.cs.purdue.edu/homes/rompf/papers/ofenbeck-scala16.pdf));
translation validation checks each lowering preserves semantics
([Fang, NYU](https://cs.nyu.edu/media/publications/fang_yi.pdf)). We already own the primitive:
`GrammarIr::pretty` (`ir/src/lib.rs:124`) is a round-trip anchor, and PROVE-12's XOR-fold
checksum parity (REPORT §5) is a working differential oracle. 3A's CI gate is engineering, not
invention.

## 3. Constraints our evidence base imposes (the non-negotiables)

- **The type plane costs zero at runtime** (Constitution v; SPEC §0 row 08; GN-3 §1.4). Any IR
  level that carries types must erase to zero emitted instructions — a *staging* obligation
  (3D-flavored) binding on ALL families.
- **Wall 2 is an IR-expressiveness wall we own**: `DirectTypeKind` has one variant `Struct`
  (GN-3 §1.2; R6 §5d `schema/direct_schema.rs`); the grammar's natural output is a recursive
  **sum** (`value = object|array|...`). Any LOCK-3 tower must make the mid-IR represent sums or
  it cannot type JSON. This is a *concrete, measured* expressiveness floor.
- **Y.13 consumer-less-substrate is already being violated at HEAD**: `DirectBuildTarget`
  (`ir/src/lib.rs:483`) is a full type schema whose `target` field is `None` for every offset-
  path descriptor (R6 §5c, `passes/src/lib.rs:1149-1287`) — substrate built ahead of its
  consumer. LOCK-3's tower must *activate* it in-wave, not carry it dead. (typed-materialization-
  invariant: every `->` must reach the tape emitter.)
- **Standing lowering laws** (SPEC §1; memory): no-value-discard (`.map(|_| ())` forbidden —
  the infer-drop at `passes/src/lib.rs:203-204` that throws away `Project.ty` is the canonical
  violation, R6 §5a), no-silent-epsilon (lowering catch-alls panic on unknown rule_kinds),
  preserve-rich-ast (never flatten typed rules for speed).
- **Refutation laws bound the *justification*, not the mechanism** (SPEC §0): no perf claim for
  the type plane (row 08/09 shim refutation) — 3D/3C wins are *generality/erasure*, never
  speed; warm benches are disinformation; the only IR perf number in play is PROVE-12's
  residualization, already CONFIRMED cold-paired.
- **Extant-tranche consciousness**: GN-3 already scopes the tower's type story (three walls,
  HM 221-LOC core, sum node net-new); GN-4 consumes the derived `DirectSchemaSet`. LOCK-3's
  IR verdict must *re-cut* GN-3 as a named supersession, never silently contradict it.

## 4. The traps, named (for the synthesis fleet)

1. **Tower sprawl** (nanopass compile-time tax) vs **premature backend-agnosticism** (MLIR
   dialect tax / no-ts-ir) — the two failure modes are opposite; 3A risks the first, 3C/3B-as-
   MLIR the second. The parsimony edict + generated-size-budget + a *named second consumer*
   are the referees.
2. **Consumer-less substrate** (Y.13) — the `DirectBuildTarget=None` dead schema is the live
   proof this trap is already sprung; every proposed IR node needs an in-wave reader.
3. **Staging unsoundness** (scope extrusion) — 3D's two-stage types must prove total stage-0
   erasure; PROVE-12 shows a *point* residualization works, not that a *general* two-stage type
   system is sound.
4. **Distinct-IR vs attribute drift** — HEAD mixes both (`BackendExpr` distinct type +
   `BackendShape` attribute); a converged tower must pick, or the "fewest transpositions that
   suffice" (3B) degrades into two half-committed representations.
5. **Regex-specificity gap illusion** — Parabix proves the SIMD-transposition-spine is
   buildable; do not re-derive it, and do not overclaim generality it never delivered.

## 5. Family-to-prior-art crosswalk (settled anchors)

| Family | Strongest precedent | Won/Lost | Binding constraint |
|---|---|---|---|
| 3A CONTRACT-TOWER | nanopass (Chez, commercial) + translation validation | Won on checkability; **lost on compile-time** | small fixed tower only; generated-size-budget |
| 3B SINGLE-IR-MULTIPASS | LLVM / sea-of-nodes / RVSDG; `BackendShape` at HEAD | Won as universal middle | one-codegen-path, no-orthogonal-codepaths |
| 3C GRAMMAR-HOMOIOUSIA | nanopass `define-language`; LOCK-7 bootstrap | Real but niche | Y.13 in-wave consumer or SPEC-ONLY; no-ts-ir |
| 3D STAGED-RESIDUAL | Futamura/MSP (LMS,MetaOCaml,Terra); **PROVE-12 (ours, CONFIRMED)** | Won as theory; proven in-point | stage-0 erasure total; zero-runtime-cost law |
