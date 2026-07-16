# LOCK-3 pass-1 · family 3D — STAGED-RESIDUAL: the IR as a two-stage program

**The thesis.** The IR tower is an explicitly *two-level* program. Every node carries a
binding time — **stage 0** (comptime: known once the grammar is fixed) or **stage 1**
(runtime: known only once input bytes arrive). Codegen is not "a lowering that happens to
constant-fold in places"; it is a **specializer** (a partial evaluator) that runs every
stage-0 subterm at generation time and emits only the stage-1 residual. Residualization —
the thing PROVE-12 did by hand at five CSS delimiter sites — stops being a per-pattern
trick and becomes the *definition of the emit spine*: emitting = erasing stage 0. Three
strong corollaries fall out, and they are the reason this family is not decoration:

1. **PROVE-12 is a corollary, not a feature.** `delim_mask(b";{}")` is a stage-0 fold; the
   `find_component_delim::<{…}>` residual is its stage-1 remainder. The family makes that
   one uniform pass rather than five hand-edits, and reaches the JSON structural scan and
   the general-descent STOP-set with the same mechanism.
2. **The zero-runtime-cost type-plane invariant (Constitution v; GN-3) *is* stage erasure.**
   "The type plane costs zero at runtime" is exactly "the type plane is stage-0, and stage 0
   is erased." `Project.ty`, `DirectSchemaSet`, `DirectBuildTarget` layout — all stage-0
   metadata that drives residual shape and then vanishes. GN-3's per-wave `git diff
   crates/runtime/ == empty` gate is the *stage-0-fully-erased* property stated operationally.
   The family unifies GN-3's erasure and PROVE-12's residualization under one law.
3. **The CSS-05 animate regression is a *mis-staging bug*, mechanically diagnosable.** GN-2.W3
   exists because feature 05 builds its shufti LUT per-call — a stage-0 value (the delimiter
   ∪ fixed set is grammar-fixed) computed at stage 1 (`find_css_significant`, per short span).
   Binding-time analysis flags a stage-0 value on a stage-1 path as *mis-staged*; re-staging
   it to a const table is the fix. The family turns GN-2.W3's hand-hoist into a consequence of
   the stage assignment.

The standing LOCK-3 laws — no-value-discard, no-silent-epsilon, preserve-rich-AST — recast as
*type invariants* under this discipline: discarding `Project.ty` (`passes/src/lib.rs:203`) is a
stage-0 value dropped before erasure could account for it; a silent epsilon is an unresolved
binding time; the rich AST is the stage-0 evidence the residualizer consumes. They stop being
conventions reviewers police and become properties the two-level IR mechanically enforces.

---

## 1. The mechanism made concrete

### 1.1 Binding-time analysis (BTA) — a Vec<Stage> fixpoint isomorphic to `nullability`

The whole apparatus reduces to one monotone analysis over the existing `GrammarIr`. Define

```
enum Stage { Comptime, Runtime }          // the 2-level lattice; Comptime ⊑ Runtime
```

with the rule: **a node is Comptime iff all of its data-flow inputs are Comptime; any
dependence on the input byte stream forces Runtime.** This is a least-fixpoint over the
`exprs` vector, structurally *identical* to `nullability(&GrammarIr)` at
`skinny/crates/ir/src/lib.rs:320-349` — same `loop { changed }` Changed-flag convergence
(Memory: changed-flag-convergence), same per-`ExprKind` match, same `Vec<bool>`-shaped
carrier. It lands as a sibling `pub fn binding_times(&GrammarIr) -> Vec<Stage>` in the same
file, ~50 LOC, no new type layer. Seeds and forcers:

- **Stage-0 seeds:** `ExprKind::Literal.bytes`, `ExprKind::Regex.pattern` (and its compiled
  DFA — see §1.4), `ExprKind::Annotation` (capacity hints, `@cap`), `Project.target/.ty`
  (`ir/src/lib.rs:185-193, 261-265`), `StructuralAlphabet.bytes` (`ir/src/lib.rs:434-437`),
  the CSS STOP/delimiter sets derived in `codegen/src/lower/css_scan.rs`.
- **Stage-1 forcers:** the cursor read (`source[offset]`), the byte at position, tape
  offsets/spans, `Repeat` trip counts, anything reached through a `Ref` into the input-driven
  driver. `Seq`/`Alt`/`Repeat`/`Discard` are Runtime as *control*, but their **operands'
  binding times are what residualize** (an `Alt` whose branch-selecting key is stage-0
  unrolls to straight-line dispatch; a `Repeat` whose bound is stage-0 unrolls).

Soundness is the standard PE condition: the lattice is **conservative** — unknown ⇒ Runtime.
A value that merely *looks* static but depends on input (e.g. a capacity keyed on document
length) must never be assigned Comptime, or the specializer would bake an input-dependent
constant. This is the one correctness obligation and it is monotone/decidable.

### 1.2 The specializer = the emitter (the first Futamura projection, made explicit)

bbnf's generator already *is* the compiler you get by specializing a generic parse-driver
with respect to a grammar (Futamura's 1st projection). The family names that seam and asks the
only question that matters per node: **is this subterm stage-0 (evaluate it now, splice the
result) or stage-1 (emit residual code)?** The emit spine
(`codegen/src/lib.rs:101` `emit_from_source` → `emit_with_layout` → `lower::lower_to_rust` →
`runtime_generator::emit_sink_only`) consumes the BTA-annotated IR and, at each node:

- **Comptime node** → run it at generation time; splice its value as a Rust `const`, a
  `const fn` call, a `const`-generic parameter, or a compile-time LUT/table.
- **Runtime node** → emit the residual straight-line Rust that consumes those constants.

PROVE-12 is exactly this at one node class: the delimiter membership test. `delim_mask` is
the Comptime evaluation (`patch.diff:75-88`), `find_component_delim::<{delim_mask(...)}>` is
the Runtime residual with the constant threaded as a monomorphizing `const` generic
(`patch.diff:45-60`). The family's claim: this is not special to CSS delimiters — it is the
general shape of *every* residualization, and BTA tells the emitter which nodes get it.

### 1.3 Residual forms (the stage-0 → Rust substrate)

Rust supplies the entire residual substrate; no MetaML-style quote/splice surface syntax is
introduced (staging stays *internal to the generator* — §5 risk). The Comptime→residual map:

| Stage-0 artifact | Residual form | Live proof-point |
|---|---|---|
| delimiter / STOP set | `const fn`-folded `u128` mask + `const`-generic monomorph | PROVE-12 (`patch.diff`) |
| shufti/classify LUT | `const [uint8x16_t; N]` table, built once | GN-2.W3 hoist (`runtime_simd.rs` `find_css_significant`) |
| small `Alt` with stage-0 keys | unrolled straight-line first-byte dispatch | `json_sink_direct/dispatch.rs` ("91.5% hot leaf") |
| `Repeat` with stage-0 bound | unrolled body | — |
| compiled regex DFA | `const` transition table / `dfa.find` | PROVE-10 (bbnf-regex) |
| capacity plan | `const` sizing (where grammar-fixed) | `tape/assembler.rs:13-40`, `scan.rs:64` |
| `Project.ty`, field layout, schema | drives struct/enum *shape*, then erased | GN-3 (`DirectBuildTarget`, `DirectSchemaSet`) |

### 1.4 Why not "just let LLVM const-fold it?" (the load-bearing distinction)

The obvious objection is that rustc/LLVM already do constant propagation. PROVE-12 refutes it
empirically and the refutation is the family's raison d'être: the delimiter set was threaded
as a runtime `&[u8]` across a function boundary, and LLVM did **not** prove it constant — the
`const` generic was required to *force* monomorphization; the `contains` loop with its length
guards survived otherwise (REPORT §3 before/after disasm: 3 slice loads + 2 length guards
gone). The grammar *knows* the binding-time separation; relying on LLVM to rediscover it
across the interpreter/data boundary is "deferred to LLVM luck" (REPORT §2). **Staging is the
discipline of making the separation the grammar already knows explicit, so the residual is
straight-line before LLVM ever sees it.** Where LLVM *does* already nail it (JSON string/number
already dispatch straight to hand kernels — R6 §3 confirms nothing to residualize), BTA finds
nothing stage-0-mislaid and emits no change: those are *named negative results*, not defeats.

### 1.5 Per-site staging as a LOCK-2 decision (the bounded-specialization knob)

Some nodes admit a genuine binding-time *choice* with a cost tradeoff, not a forced
assignment: a small `Alt` may unroll to straight-line dispatch (faster, larger) or stay a
runtime `match` (smaller); a LUT may be a `const` table (fast, +data) or an inline
computation; a `Repeat` may unroll or loop; a stage-0 constant may be monomorphized-in or
threaded. This is classic **polyvariant / bounded static variation** ("The Trick" in the PE
literature) — and aggressive unrolling blows the generated-size budget (Memory:
generated-size-budget). So the *aggressiveness of stage-0 specialization is exactly a CSP*:
variables = per-site stage assignment where a choice exists; constraints = legality (never
stage a Runtime-dependent value to Comptime) + the generated-LOC budget; objective = the cost
lattice. That is precisely LOCK-2's engine (`ir/src/cost.rs` `CostFacts`/`DecisionCspFacts`/
`PriorityStep`; `passes::derive_backend_shape` at `passes/src/lib.rs:398`). 3D hands LOCK-2 a
well-typed decision — "specialize here or not" — rather than a hardcoded branch (Memory:
pluggable-components). The forced assignments (the soundness-mandatory ones) never reach the
solver; only the free choices do.

---

## 2. The substrate

Three concrete pieces, each with a same-wave consumer (Constitution viii, Y.13 — no
consumer-less substrate):

- **`Stage` + `binding_times()`** in `crates/ir/src/lib.rs` beside `nullability` (§1.1).
  Consumer: the emit spine's residualizer.
- **The residualizer pass** in `codegen/src/lower/` — a directory-module `lower/residual/`
  (no god module; Memory: no-god-modules) that walks the BTA-annotated `BackendExpr`
  (`ir/src/lib.rs:376-413`) and rewrites Comptime subterms to their spliced form. It *owns*
  the `const fn` emission (generalizing `delim_mask`), the const-generic threading, and the
  LUT-hoist. Consumer: `runtime_generator` / `json_sink_direct` templates it feeds.
- **The stage-erasure oracle** — a test/gate that asserts no stage-0 value survives into the
  runtime residual, strengthening the existing runtime assert `runtime/src/lib.rs:42-44`
  (`payloads().len()==0`, zero writes/allocs after JSON parse) and GN-3's `git diff
  crates/runtime/ == empty` into a *property of the emitter*, not a spot-check. Consumer: CI
  + the GN-3 W5 challenge conjuncts.

The substrate is deliberately *thin*: `Stage` is a two-element enum, `binding_times` is a
50-LOC fixpoint, the residualizer generalizes an 18-LOC proven pattern. It is an **analysis +
an emit discipline**, not a new type-system layer — this is the answer to the over-substrate
risk (§5). The "types" are annotations the emitter reads; the specialization already happens
in codegen today, ad hoc — the family makes it uniform and named.

---

## 3. Integration seams (path:line)

- **BTA fixpoint home:** `skinny/crates/ir/src/lib.rs:320-349` — `nullability`; the exact
  structural template for `binding_times`. `ExprKind` at `:234-271`; `BackendExpr` at
  `:376-413`.
- **The archetypal residual (generalize this):** PROVE-12 `patch.diff` — `delim_mask` const
  fn (`:75-88`) + `find_component_delim<const DELIMS: u128>` (`:45-60`); template home
  `codegen/src/runtime_generator.rs` `CSS_GENERATED_RS`.
- **Delimiter/alphabet stage-0 sources to residualize uniformly:** `StructuralAlphabet`
  `ir/src/lib.rs:434-437`; CSS STOP-set `codegen/src/lower/css_scan.rs` (712 LOC); JSON
  structural scanner (neutralized) `runtime/src/grammars/json/scan.rs:34-48`; general-descent
  blanket `skip_ws` `codegen/src/general_descent/emit.rs:126-137`.
- **Mis-staged LUT (the animate diagnosis):** `runtime/src/runtime_simd.rs`
  `find_css_significant` builds `fixed ∪ delimiters → uint8x16_t` shufti tables per-call;
  GN-2.W3 hoists it. BTA flags it: a grammar-fixed (stage-0) LUT computed on a stage-1 path.
- **Type-plane erasure (the unification):** `Project.ty` discarded at `passes/src/lib.rs:203`
  (GN-3 W2 threads it); `DirectBuildField.target` always `None` on the offset-tape path
  (`passes/src/lib.rs:1149-1287`, R6 §5d); the honest type IR `DirectSchemaSet`/
  `DirectBuildTarget` at `ir/src/lib.rs:465-535` and `codegen/src/direct_schema.rs`; the
  zero-cost runtime assert `runtime/src/lib.rs:42-44`.
- **Per-site staging → LOCK-2:** cost facts `ir/src/cost.rs` (`CostFacts` `:24`,
  `DecisionCspFacts` `:203`, `PriorityStep` `:264`); shape/cost selection
  `passes/src/lib.rs:398` `derive_backend_shape`; `BackendShape` `ir/src/lib.rs:361-367`.
- **Regex DFA as stage-0 artifact:** `bbnf-regex` `analyze` (already consumed for nullability,
  `ir/src/lib.rs:331`); PROVE-10 `dfa.find` — "compile the automaton at gen time, match at run
  time" is the canonical staged product (ties LOCK-5).
- **Capacity as a staging candidate:** `CapacityPlan`/`GrowOnly` const-256 `tape/assembler.rs:13-40`,
  `scan.rs:64`.

---

## 4. Prior art (with citations)

- **Futamura projections** (Y. Futamura, *Partial Evaluation of Computation Process*, 1971;
  reprinted Higher-Order & Symbolic Comp. 1999). 1st projection: specialize an interpreter
  w.r.t. a program ⇒ a target program. **bbnf codegen is the compiler produced by the 1st
  projection over a generic parse-driver** — this family names that identity.
- **Partial evaluation & binding-time analysis** — Jones, Gomard & Sestoft, *Partial
  Evaluation and Automatic Program Generation* (Prentice Hall, 1993): the `mix` specializer,
  offline BTA, "The Trick"/bounded static variation, polyvariant specialization, code-size
  control. This is the direct theory of §1 and §1.5.
- **Two-level languages** — Nielson & Nielson, *Two-Level Functional Languages* (CUP, 1992):
  binding time as a *type-level* property (comptime vs runtime in the type), the exact
  formulation the charter asks for ("the split in the types").
- **Multi-stage programming** — Taha & Sheard, *MetaML and multi-stage programming with
  explicit annotations* (TCS 2000); Kiselyov's BER MetaOCaml (brackets `.<>.`, escape `.~`,
  `Runcode.run`). The reference semantics for staged code; the family adopts the *discipline*
  but deliberately *not* the surface syntax (residuals are Rust const-generics/const-fn).
- **Lightweight Modular Staging (LMS)** — Rompf & Odersky (GPCE 2010): `Rep[T]` marks stage-1;
  everything else is staged away. The closest architectural analog — staging by *type-directed*
  representation, no quasiquotes — mirrors "annotate, don't add syntax."
- **Zig `comptime`** — the closest *production* analog: a parameter/value is comptime or
  runtime, tracked in the type system, and the compiler partially evaluates. The bbnf target
  in a systems idiom: `const`-generic residuals are Rust's `comptime`.
- **Terra** — DeVito et al., *Terra: a multi-stage language for high-performance computing*
  (PLDI 2013): a metalanguage staging a low-level target — the generator/runtime split bbnf
  already has, made a language feature.
- **Typed Template Haskell** (`Code Q a`) and **Racket phase separation** — staging via typed
  quotes / hygienic phases; evidence that "the generator eats itself" (3C's homoiousia, LOCK-7
  bootstrap) is a solved shape.
- **Finally Tagless, Partially Evaluated** — Carette, Kiselyov & Shan (JFP 2009): staging
  *without* a GADT IR via typeclass-polymorphic interpretation — an alternative to a two-level
  ADT worth measuring against the annotation approach.
- **Collapsing Towers of Interpreters** — Amin & Rompf (POPL 2018): multi-level staging
  collapse; the theory behind aligning the generator's self-application with LOCK-7 bootstrap.
- **regex-automata / RE2 / Thompson** — "compile the DFA once, match many": the DFA is a
  textbook staged artifact (stage-0 build, stage-1 execute). Grounds §1.4's DFA row and the
  LOCK-5 tie.
- **Rust `const fn` / const generics / MIR const-eval** — the actual residual substrate
  (what PROVE-12 rides); the family's residuals are all expressible here today, no nightly.

---

## 5. Named risks and counterexamples

- **R-D1 — over-substrate (Y.13 / Constitution viii).** A full two-level *type system* would
  be heavier than the win at HEAD (a handful of const-foldable sites: delimiters, alphabets,
  one LUT). **Mitigation:** the family ships an *analysis* (`Stage` enum + 50-LOC fixpoint) +
  an *emit discipline*, not a type layer; the specialization already exists ad hoc in codegen.
  Named consumers: PROVE-12 delimiter sites, GN-2.W3 LUT, GN-3 type erasure, the JSON/general
  STOP-sets, the capacity plan. If BTA cannot be shown to subsume ≥2 hand sites into one pass,
  the family is refuted as premature abstraction — that is the prototype's job (§6.1).
- **R-D2 — code-size explosion (generated-size budget).** Aggressive stage-0 unrolling
  (per-context specialization, `Repeat` unroll) can blow the LOC budget. **Mitigation:** this
  is *why* per-site staging is a LOCK-2 CSP with the budget as a hard constraint (§1.5) —
  bounded specialization, never unbounded. Forced (sound) stagings don't hit the solver; only
  free choices do. Counterexample to watch: a grammar with a large stage-0 `Alt` that unrolls
  into a multi-KB dispatch — the budget constraint must prune it back to a runtime match.
- **R-D3 — LLVM already does it (the skeptic's null hypothesis).** For some sites rustc
  const-prop suffices and staging buys nothing. **Mitigation:** measure *per site*; PROVE-12
  proves the cross-boundary slice case LLVM misses, but the family must publish the sites where
  staging == LLVM as negative results, not hide them (Constitution vii). The win is only real
  where the residual beats letting LLVM try.
- **R-D4 — mis-staging is a *soundness* bug, not a perf miss.** If BTA wrongly marks an
  input-dependent value Comptime, the specializer bakes a wrong constant into every parser.
  **Mitigation:** conservative lattice (unknown ⇒ Runtime); a differential oracle (staged vs
  unstaged emit must be byte-identical output — PROVE-12's parity checksum, REPORT §5) is the
  standing gate. Counterexample: capacity keyed on document length must stay Runtime even
  though it "looks" like a grammar constant.
- **R-D5 — scope collision with GN-3 and GN-2.** The type-erasure framing overlaps GN-3
  (which owns `Project.ty` threading) and the LUT-hoist overlaps GN-2.W3. **Mitigation:** 3D
  is the *unifying account*, not a re-implementation — it re-cuts GN-2.W3 and GN-3's erasure
  invariant as instances of one law (a named supersession per Constitution ix), and the
  shared-file discipline (`passes/src/lib.rs`, serialized writes — GN-3 R3) is inherited.

---

## 6. What a prototype must test to move this family

Each is falsifiable, artefact-backed, cold/paired per the profiling standing law.

1. **BTA-uniform-residual (the generalization proof).** Implement `binding_times()` +
   `lower/residual/` and drive the *same* const-fold/const-generic residualizer over ≥2 site
   classes from one mechanism: (a) reproduce PROVE-12's CSS delimiter win byte-identically
   (parity checksum unchanged; cyc/B 0.81-0.98× reproduced on `css_canon_bench`), and (b)
   extend a *measurable* residual win to a **second** site — the JSON structural STOP-set or
   the general-descent delimiter scan. **Gate:** ≥1 new site wins, all sites regen bit-identical
   (`xtask regen-*` idempotent), generated-LOC within budget. **Refutes the family if:** the
   second site shows staging == LLVM (nothing to residualize) *and* no other stage-0 site
   exists — then 3D is a one-site hand-edit, not a plane.
2. **Re-staging mechanically fixes animate.** Show BTA flags `find_css_significant`'s per-call
   LUT as a stage-0 value on a stage-1 path, and that re-staging it to a `const` table (the
   residualizer's LUT-hoist form) erases the +13.4% cyc/B / +29.9% ins animate regression —
   i.e., GN-2.W3's hand-hoist *falls out of the stage assignment*. **Gate:** animate track1_rich
   ≤1.0× with bootstrap/tailwind/material wins retained (tailwind lcss ratio ≥4.0×), ins Δ ≤+2%.
3. **Zero-runtime-cost as an erasure *property*.** Prove via `cargo asm`/disasm that no
   `Project.ty`/`DirectSchemaSet`/schema value survives into the runtime residual — a stage-0
   erasure check strengthening `runtime/src/lib.rs:42` and GN-3's empty-`git diff` gate into a
   property the emitter guarantees, not a spot-assert. **Gate:** the erasure oracle is
   fail-closed and green; runtime binary byte-identical across a schema-shape change that alters
   no runtime bytes.
4. **Per-site staging is a real LOCK-2 decision.** Exhibit ≥1 site with a genuine binding-time
   choice (small `Alt` unroll-to-dispatch vs runtime `match`, or const-LUT vs computed) where
   the assignment is *selected by the cost model under the generated-size budget*, and show the
   solver picks differently when the budget is tightened. **Gate:** the decision is a CSP
   variable read from `DecisionCspFacts`, not a hardcoded branch; flipping the budget flips the
   emitted form (proving the clause "per-site staging solved by LOCK-2's engine" is load-bearing,
   not decorative).
