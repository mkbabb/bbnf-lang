# LOCK-3 — THE IR TRANSPOSITION SPINE · PRIOR-ART (pass 2)

*What exists, what won and lost elsewhere for the IR-tower / lowering-contract / staged-residual
problem class, the constraints our own evidence base imposes, and the named traps. No family
allegiance; the four families (3A CONTRACT-TOWER, 3B SINGLE-IR-MULTIPASS, 3C GRAMMAR-HOMOIOUSIA,
3D STAGED-RESIDUAL) are the axes the external record is scored against.*

---

## 0. The problem class, stated

A parser **generator** whose spine is: grammar AST → typed HIR → mid-IR → backend shapes →
emission, with (a) lowering contracts/invariants, (b) residualization (PROVE-12) generalized,
(c) a decision-driver seat, (d) multi-backend (Rust now, TS later) over a backend-agnostic
middle. Standing laws bind: **no value-discard, no silent epsilon, preserve rich AST**
(MEMORY: no-value-discard, no-silent-epsilon, preserve-rich-ast; typed-materialization-invariant
— "every `->` reaches the tape emitter"). This is simultaneously a *compiler IR* problem and a
*staged program-generation* problem — the two literatures below are the load-bearing prior art.

---

## 1. What exists in-tree (the baseline the record is scored against)

The extant tower is real and small (R1 §1-2, R6 §5, `ir/src/lib.rs`, `passes/src/lib.rs`):

- **Levels**: `GrammarIr`/`ExprKind` (incl. `Project{body,target,ty}`, `ir/lib.rs:261`) →
  `BackendIr`/`BackendExpr` (incl. `ValueProject`, lowered `passes/lib.rs:1349`) →
  `BackendShape` (5 variants EagerTape|OffsetTape|EventTape|SinkOnly|CollapsedStage,
  `ir/lib.rs:361`; `all_backend_shapes()` `cost.rs:334`) → per-shape lowering in `codegen/lower/*`
  → emission (`runtime_generator`, `json_sink_direct/`, `css_scan_direct/`, `general_descent/`,
  `json_typed_direct.rs`).
- **Decision driver**: `passes::compile` = infer → shape → CSP → egraph (R1 §1); `derive_recognizers`
  (`passes/lib.rs:338`), `derive_backend_shape` (`:398`), cost model `ir/cost.rs` (`CostFacts`,
  `ActiveCostFacts` with egraph/CSP provenance `:173`, `DecisionCspFacts` `:203`, `PriorityStep`
  8-step `:264`). This is already a nascent LOCK-2-shaped island system — the two locks share this seam.
- **Residual precedent**: PROVE-12 (CONFIRMED) is the generator const-folding + beta-reducing a
  grammar-constant delimiter set into a `const fn delim_mask`+const-generic monomorph — a
  **first-Futamura specialization** of the recognizer to a known input, disasm-verified to erase
  3 slice loads + 2 length guards, +18 gen LOC, cyc/B 0.81–0.98× (PROVE-12 REPORT §2-6).
- **The rot the spine must fix (measured value-discard)**: `Project.ty` is thrown away at
  `infer_expr` (`passes/lib.rs:203-204`); `DirectBuildField.target` is hardcoded `None` on the
  offset-tape path (R6 §5c-d, `passes/lib.rs:1149-1287`); types collapse to a 4-way
  `ProjectionKind {Float,Bool,Tag,Span}` fold on the general-descent path (R6 §5b,
  `general_descent/view.rs:20-46`); the one honest type IR (`DirectSchemaSet`) is **hand-authored**
  outside the compiler (`xtask/real_typed_schema.rs`, ~1014-1020 LOC). GN-3 is the redress; LOCK-3
  owns the tower discipline that makes the discard structurally impossible.

**Verdict on the baseline**: a working but *lossy* spine — the transposition exists but violates
no-value-discard at three named sites. The prior-art question is which family shape closes those
sites without re-introducing IR proliferation or a premature-flattening trap.

---

## 2. Nanopass (Keep & Dybvig) — the strongest precedent for 3A + 3C

The **nanopass framework** ([Keep dissertation](https://andykeep.com/pubs/dissertation.pdf);
[nanopass.org](https://nanopass.org/); [ICFP'13](https://dl.acm.org/doi/10.1145/2500365.2500618))
is the canonical answer to "IR tower with per-level contracts, machine-checked." Two forms:
`define-language` (formally specifies an IL's **grammar**) and `define-pass` (a pass over one
language producing another), with **pattern-matchers and traversal auto-generated from the
language descriptions**, so each pass is statically constrained to valid-in→valid-out.

- **Scores 3C (GRAMMAR-HOMOIOUSIA) directly**: nanopass's ILs *are* grammar-specified — this is
  exactly "the IR is itself grammar-specified, transpositions are grammar-to-grammar maps."
  Precedent that it is not fantasy: Chez Scheme is a commercial nanopass compiler.
- **Scores 3A (CONTRACT-TOWER)**: the "each lowering is a total function with a checkable
  contract, round-trip/parity oracle per level" idea is nanopass's core value proposition —
  the framework *generates* the boilerplate that makes many total passes affordable
  ([SIGPLAN "My First Fifteen Compilers"](https://blog.sigplan.org/2019/07/09/my-first-fifteen-compilers/);
  [tutorial](http://wonks.github.io/keep/fall2012/2012/11/02/a-nanopass-framework.html)).
- **The WIN elsewhere**: education + Chez; 43-pass compilers built in 15 weeks; "single-task
  passes" catch bugs at pass boundaries by construction.
- **The LOSS / TRAP (binding constraint on 3A/3C)**: Keep measured compile time "within a factor
  of two" *despite* replacing 5 passes with 50+ nanopasses — i.e. **IR proliferation costs ~2×
  build time and per-level boilerplate**. Our constitution (extreme parsimony §vi; no god modules;
  GN generated-size budget) and 3A's own "a small **fixed** tower (three levels)" directly reject
  unbounded nanopass fan-out. The lesson: adopt nanopass's *contract-per-level + grammar-defined
  IL* discipline, reject its *many-tiny-ILs* multiplication. 3B (single-IR-multipass) is the
  antithesis pole precisely on this axis.

---

## 3. MLIR / LLVM — the 3B pole and the premature-flattening law

**MLIR** ([Cornell CS6120](https://www.cs.cornell.edu/courses/cs6120/2023fa/blog/mlir/);
[lowering paths](https://apxml.com/courses/compiler-runtime-optimization-ml/chapter-2-advanced-ml-intermediate-representations/mlir-lowering-paths))
and **LLVM** are the industrial answer to "one IR framework, progressive lowering."

- **Scores 3B (SINGLE-IR-MULTIPASS)**: MLIR is one IR *substrate* with **dialects** coexisting at
  multiple abstraction levels; passes are rewrites; lowerings can target *parts* of a program
  (partial lowering). LLVM's "one rich IR refined in place by annotating passes, BackendShape as
  attributes" is literally the LLVM pass-manager shape 3B names. Our `BackendShape`-as-selection +
  `ActiveCostFacts` provenance is already MLIR-attribute-shaped.
- **The LOAD-BEARING TRAP MLIR was built to escape** (cite this against any 3A/3B that lowers
  eagerly): *"High-level semantics — shapes, parallel loops, tensor semantics, sparsity,
  control-flow — must be lowered into LLVM IR early, **losing information** useful for later
  optimization."* This is the compiler-literature statement of our **preserve-rich-ast /
  no-value-discard** law. The extant three discard sites (§1) are exactly this failure: the tower
  flattens `->` before emission. MLIR's cure = keep the high-level dialect *live alongside* the
  low one and lower progressively. Direct constraint on LOCK-3: **the mid-IR must carry the typed
  projection to the emitter; a level that erases `->` is the premature-lowering anti-pattern by
  name.**
- **The COUNTER-TRAP (constrains 3B toward 3A)**: MLIR is frequently criticized as heavyweight —
  the [Quora](https://www.quora.com/I-recently-came-across-MLIR-projects-for-an-LLVM-compiler-Why-is-there-a-need-of-one-more-IR)
  "why one more IR" debate — dialect infrastructure is a large fixed cost only amortized by many
  backends. A single-grammar-family generator does **not** get MLIR's amortization; a full
  dialect framework would be a consumer-less substrate (constitution Y.13/viii). So 3B should mean
  *one rich IR with attribute-annotated levels* (LLVM shape), **not** an MLIR dialect zoo.

---

## 4. Staged metaprogramming / partial evaluation — the exact home of 3D + PROVE-12

This literature is where PROVE-12 *lives*, and it is the strongest formal backing for 3D
(STAGED-RESIDUAL).

- **Futamura projections** ([Wikipedia: partial evaluation](https://en.wikipedia.org/wiki/Partial_evaluation);
  [second-Futamura for interpreters](https://dl.acm.org/doi/10.1145/3359061.3361077)): specializing
  a program to known inputs yields a **residual program** — "leaner, faster, optimized for the
  inputs you don't know yet." PROVE-12 is *first Futamura projection applied to a recognizer*:
  the grammar constant (delimiter set) is the known input; the emitted const-folded scanner is the
  residual. This names PROVE-12 with a 50-year-old theorem and tells LOCK-3 how to **generalize**
  it: any grammar-constant reaching a runtime dispatch (STOP-sets, first-sets, capacity plans,
  decode classes) is a partial-evaluation target. GN-5.W4 ("12's const-mask generalized to every
  grammar-derived stop-set") is the Futamura generalization spelled out.
- **LMS — Lightweight Modular Staging** ([CACM](https://cacm.acm.org/research/lightweight-modular-staging/);
  [Rompf/Odersky GPCE'10](https://infoscience.epfl.ch/record/150347/files/gpce63-rompf.pdf)): the
  decisive precedent for 3D's "comptime/runtime split **in the types**." LMS "uses **only types**
  to distinguish the two stages" — `Rep[T]` = a value at the *second* (runtime) stage; everything
  else is stage-0 and evaporates. This is 3D verbatim: "the IR is an explicitly two-stage program;
  residualization is erasure of stage 0; PROVE-12 becomes a corollary." LMS proves the type-staged
  approach *ships* (SQL generation, GPU DSLs, ML). It also proves 3D is compatible with our
  **type-plane-costs-zero-at-runtime** invariant (constitution v) — stage-0 erasure *is* the
  zero-runtime-cost proof, mechanized.
- **The TRAP in staging** (constrains 3D): quasiquote-based staging (MetaOCaml, Terra) is powerful
  but the binding-time discipline is subtle — a mis-staged term silently *runs at the wrong stage*
  (a stage-0 value captured into stage-1 code, or vice versa). LMS's answer — *types* carry the
  stage, so mis-staging is a **type error** — is the guard 3D must adopt: the comptime/runtime
  split must be a checked type distinction, not a convention. Maps onto no-silent-epsilon: a stage
  boundary that can be silently crossed is the staging analogue of a silent Epsilon fallthrough.

---

## 5. Multi-backend (Rust now, TS later) — the backend-agnostic-middle record

The spine's fourth mandate (multi-backend over a backend-agnostic middle) has a mixed record.

- **ANTLR** ([antlr4](https://github.com/antlr/antlr4);
  [antlr-ng](https://www.antlr-ng.org/introduction.html)) emits 10+ targets (Java, C#, Python,
  JS, **TypeScript**, Go, C++, Swift, Dart, PHP) from one grammar — the existence proof that a
  parser generator can carry many backends. **The mechanism is the lesson**: ANTLR keeps backends
  tractable by making grammars **semantic-action-free at the core** and pushing language-specific
  logic into per-target StringTemplate groups
  ([building a target](https://theantlrguy.atlassian.net/wiki/spaces/ANTLR3/pages/2687399/How+to+build+an+ANTLR+code+generation+target)).
  SableCC goes further — **zero** embedded actions, visitors only. Constraint on LOCK-3: a
  backend-agnostic middle is *only* achievable if the middle carries **no host-language semantics**
  — the `->` projections must resolve to abstract `TypeDesc::Named`, each backend mapping to native
  types (MEMORY: backend-agnostic-types; hybrid-grammar-host). The extant `json_typed_direct.rs`
  emits *Rust* structs — that Rust-ness must sit *below* the agnostic middle, or the TS backend
  (LOCK-6, PROVE-14) inherits a Rust-shaped IR it cannot consume.
- **PROVE-14 (CONFIRMED)**: byte-exact offset-tape crossing to TS, stop-early get 4.4×, full-walk
  a *measured non-goal* (native parse won). This is the empirical boundary: the backend-agnostic
  middle is the **tape layout + value-surface contract**, not the emitter internals. LOCK-6 decides
  where "reasonable isomorphism" ends; LOCK-3 must present a middle whose *contract* (tape shape,
  typed schema, decode classes) is the crossing surface, per constitution iii (measured, not asserted).
- **The LOSS elsewhere**: retargetable generators that let semantic actions leak into the grammar
  (yacc/bison C actions, tree-sitter's C scanners) are effectively **single-backend** — the actions
  pin the target. Named trap: any `@host` function that returns a host-native value rather than an
  abstract typed value re-pins LOCK-3 to Rust (hybrid-grammar-host law is the escape: typed host-fn
  *signatures* are agnostic; only the bodies are native).

---

## 6. Constraints our own evidence base already imposes (binding)

From SPEC §0 (the certified swath + refutation laws) and MEMORY:

1. **The tower adds zero runtime instructions where it is a type/stage plane** (constitution v;
   GN-3 invariant "`crates/runtime/` untouched, `git diff` empty per wave"). Any LOCK-3 level that
   claims a *speed* win from the type plane is refuted by `git diff` — echoing refutation 09
   (shim perf claim REFUTED). 3D's win is *erasure-proven zero cost*, never speed.
2. **No orthogonal codepaths / one codegen path** (MEMORY: one-codegen-path, no-orthogonal-codepaths).
   The extant *three* materialization paths gated on `has_rule("value")` (R6 §0, §6) are exactly
   the name-gated fork the constitution forbids; PROVE-13 already cured `!has_rule("value")` with a
   structural predicate. LOCK-3's tower must **unify** the paths, not enshrine BackendShape as five
   parallel emitters — a single spine with attribute-selected lowering (LLVM/3B shape) satisfies both
   "one path" and "pluggable decision points" (MEMORY: pluggable-components).
3. **Residualization must be generator-time + monomorphization, "not deferred to LLVM luck"**
   (PROVE-12 REPORT §2) — the Futamura specialization is explicit in the generator, disasm-verified.
4. **No IR for TS** (MEMORY: no-ts-ir — "AST IS the combinator"). This bounds 3C/3B: the
   backend-agnostic middle is a **contract**, not a materialized second IR on the TS side; TS
   consumes the tape/schema contract directly. A LOCK-3 that mints a TS-side IR violates a standing edict.
5. **Extant-tranche consciousness** (constitution ix): GN-3's `type_plane::derive` (HM core, sum
   node, threaded arrow) is the *committed* redress of §1's discard sites. LOCK-3 supersedes it only
   by naming — the spine houses `type_plane` as its inference level, it does not silently re-invent it.

---

## 7. The trap table (named counterexamples for the pass-2 synthesis)

| Trap | Named in | Bites which family | Guard |
|---|---|---|---|
| **IR proliferation** (~2× build, per-level boilerplate) | nanopass (Keep §2) | 3A, 3C | "small **fixed** tower (3)"; generated-size budget; parsimony |
| **Premature flattening** (lower high-level semantics early, lose info) | MLIR (§3) | 3A, 3B | preserve-rich-ast; the mid-IR carries `->` to the emitter; no-value-discard |
| **Dialect-zoo fixed cost with no amortization** | MLIR criticism (§3) | 3B | single rich IR (LLVM shape), not a dialect framework; Y.13 consumer-less-substrate ban |
| **Silent mis-staging** (stage-0 value leaks into stage-1) | MetaOCaml/Terra vs LMS (§4) | 3D | stage carried in *types* (LMS); a crossable stage boundary = no-silent-epsilon violation |
| **Type-plane speed claim** | SPEC §0 refutation 09; GN-3 §1.4 | 3D, all | `git diff` over runtime empty; win is erasure, not speed |
| **Backend pinning via leaked host semantics** (yacc C actions) | ANTLR/SableCC record (§5) | 3B multi-backend | abstract `TypeDesc::Named`; typed host-fn signatures agnostic, bodies native |
| **Name-gated fork** (`has_rule("value")` three-path split) | R6 §0/§6; PROVE-13 cure | 3B "one path" | structural predicate; one attribute-selected spine |
| **Materialized TS IR** | MEMORY no-ts-ir | 3C | middle is a *contract* (tape/schema), not a second IR |

---

## 8. Family adjudication (evidence-scored, no allegiance)

- **3A CONTRACT-TOWER** — best external backing: **nanopass** (contract-per-level, grammar-defined
  IL, generated traversal, round-trip oracle). Adopt its discipline; **reject** its many-IL cost —
  cap at the "small fixed tower." Its round-trip/parity-oracle-per-level maps onto our
  typed-materialization-invariant (every `->` reaches emission = a parity oracle).
- **3B SINGLE-IR-MULTIPASS** — best external backing: **LLVM** (one rich IR, annotating passes,
  attributes-as-BackendShape). **MLIR** supplies the premature-lowering *warning* not the *shape*
  (its dialect framework over-amortizes for a few grammars). Strongest fit to "one codegen path"
  and the extant `ActiveCostFacts`-annotated `BackendShape`. Chief risk: keeping the IR "rich"
  enough to not flatten `->` (the very rot §1 documents).
- **3C GRAMMAR-HOMOIOUSIA** — backing: **nanopass `define-language`** (the IR *is* grammar-specified)
  + our own LOCK-7 bootstrap. Highest elegance, highest bootstrap cost; the no-ts-ir + parsimony
  edicts cap it to a *contract*-level homoiousia, not a fully materialized meta-circular IR grammar
  unless LOCK-7 pays the bootstrap.
- **3D STAGED-RESIDUAL** — backing: **LMS** (type-staged, ships, zero-cost by erasure) +
  **Futamura** (PROVE-12 *is* the first projection; GN-5.W4 is its generalization). The only family
  with a CONFIRMED in-tree prototype behind it (PROVE-12) and a mechanized zero-runtime-cost proof
  (erasure). Its residualization subsumes PROVE-12 as a corollary and per-site staging is exactly
  what LOCK-2's cost-driven engine selects — the natural 2/3 weave seam.

The families are not exclusive: the record most favors a **3B-shaped single rich IR** whose levels
carry **3A-nanopass contracts** (parity oracle per level = typed-materialization-invariant), with
**3D-LMS type-staging** as the erasure mechanism that makes PROVE-12 general and the type plane
provably zero-cost, and **3C** admitted only as a *contract*-level homoiousia bounded by LOCK-7/no-ts-ir.

---

## Sources

- Keep, *A Nanopass Framework for Commercial Compiler Development* — https://andykeep.com/pubs/dissertation.pdf ·
  https://dl.acm.org/doi/10.1145/2500365.2500618 · https://nanopass.org/ ·
  http://wonks.github.io/keep/fall2012/2012/11/02/a-nanopass-framework.html ·
  https://blog.sigplan.org/2019/07/09/my-first-fifteen-compilers/
- MLIR — https://www.cs.cornell.edu/courses/cs6120/2023fa/blog/mlir/ ·
  https://apxml.com/courses/compiler-runtime-optimization-ml/chapter-2-advanced-ml-intermediate-representations/mlir-lowering-paths ·
  https://www.quora.com/I-recently-came-across-MLIR-projects-for-an-LLVM-compiler-Why-is-there-a-need-of-one-more-IR
- Partial evaluation / Futamura — https://en.wikipedia.org/wiki/Partial_evaluation ·
  https://dl.acm.org/doi/10.1145/3359061.3361077
- LMS — https://cacm.acm.org/research/lightweight-modular-staging/ ·
  https://infoscience.epfl.ch/record/150347/files/gpce63-rompf.pdf ·
  https://www.cs.purdue.edu/homes/rompf/papers/rompf-wf16.pdf
- ANTLR / retargetable codegen — https://github.com/antlr/antlr4 ·
  https://www.antlr-ng.org/introduction.html ·
  https://theantlrguy.atlassian.net/wiki/spaces/ANTLR3/pages/2687399/How+to+build+an+ANTLR+code+generation+target
- In-tree: SPEC §0 + GN-1/GN-3; R1 §1-2/§8, R6 §0/§5/§6; PROVE-12 REPORT; `ir/src/lib.rs`,
  `passes/src/lib.rs`, `codegen/src/` (all absolute under `/Users/mkbabb/Programming/bbnf-lang/skinny/`).
