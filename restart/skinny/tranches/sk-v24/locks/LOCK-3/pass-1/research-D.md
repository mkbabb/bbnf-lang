# LOCK-3 · Family 3D — STAGED-RESIDUAL (pass 1, research D)

The IR is an explicitly two-stage program. Every node carries a **binding time** — stage 0
(comptime / generation time) or stage 1 (runtime / emitted parser). The generator IS a partial
evaluator: it specializes the grammar over the *type* of the input, evaluates the stage-0
program to values, and residualizes only the stage-1 program into Rust+NEON. Residualization is
the **erasure of stage 0**. PROVE-12's hand-authored const-fold ceases to be a patch and becomes
a corollary — the one instance a systematic law was going to emit anyway. Per-site staging
choices (monomorph vs shared const table) are cost decisions handed to the LOCK-2 engine.

This family's thesis, stated once: **the comptime/runtime split lives in the types, and
stage-0-costs-zero is the law that subsumes type-plane-costs-zero.**

---

## 1. The mechanism made concrete

### 1.1 The stage lattice and the binding-time analysis

Introduce a two-point lattice `Stage = { Comptime (S0), Runtime (S1) }` with `S0 ⊑ S1` (a value
known at generation time can always be re-materialized at runtime; the converse is a leak). A
node's stage is the join of its operands' stages against its own seed. This is textbook **offline
binding-time analysis** (Jones/Gomard/Sestoft, *Partial Evaluation and Automatic Program
Generation*, 1993): monotone, decidable, computed once by fixpoint.

The analysis is not a new subsystem — it is **isomorphic to the `nullability` fixpoint already in
the tree** at `skinny/crates/ir/src/lib.rs:320-349` (the same `loop { changed } ` monotone walk
over `grammar.exprs`, one `Vec<Stage>` parallel to the `Vec<bool>` nullable vector). Add
`fn binding_times(grammar: &GrammarIr) -> Vec<Stage>` beside `nullability`, seeded from the
grammar-constant nodes:

| IR node (`ir/src/lib.rs`) | Seed stage | Why |
|---|---|---|
| `Literal { bytes }` `:246` | **S0** | byte pattern is a grammar constant |
| `Regex { pattern }` `:250` | **S0** | pattern → NFA→DFA table is comptime-derivable |
| `Project { target, ty }` `:261` | **S0** | the projected type is a generation-time fact |
| `Annotation { name, value }` `:257` | **S0** | `@cap`, `@ws`, policy bits are directives |
| the input cursor / `source[pos]` | **S1** | the only genuine runtime seed |

Everything else joins: `Seq`/`Alt`/`Repeat`/`Optional`/`Ref` take the join of their children;
`Discard`, `Project` join body with their (S0) annotation. A rule is S1 iff a byte read reaches
it — i.e. essentially every recognizer rule is S1, but its *parameters* (delimiter sets,
alphabets, dispatch first-sets, DFA tables, capacity hints, field types) are S0 islands hanging
off an S1 spine. Residualization is the act of collapsing each S0 island to a constant before it
touches the S1 spine.

### 1.2 The residualizer: erasure of stage 0

Split the emitter's leaf handling in two:

- **`emit_const(node) -> Residual`** evaluates an S0 subtree to a stage-0 *value* — a `u128`
  membership mask, a `[u8; N]` alphabet, a DFA transition table, a `usize` capacity, a
  `DirectTypeRef` — and splices it as Rust `const` / `const fn` / a `const`-generic argument /
  an associated `const`. The S0 program disappears; its result remains as an immediate.
- **`emit_runtime(node)`** emits the S1 spine (byte compares, cursor advances, `TapeEmit`,
  `SpanMark`, sink callbacks) as ordinary Rust, reading the S0 constants by name.

The **stage-0 value plane** is a small `enum Residual { Mask(u128), Bytes(Vec<u8>), Dfa(..),
Capacity(usize), TypeRef(DirectBuildTypeRef), ... }`. It is the generation-time analogue of the
runtime value plane (`runtime/src/tape`) — and it is *consumed in the same pass that produces
it* (Y.13), so it is not a consumer-less substrate.

### 1.3 PROVE-12 as the corollary (the load-bearing claim)

PROVE-12 hand-wrote exactly one instance of `emit_const`. In its patch
(`prototypes/PROVE-12/patch.diff`): the delimiter set `b";{}"` is an S0 island; `delim_mask` is
`emit_const` folding it to a `u128`; `find_component_delim<const DELIMS: u128>` is the S1 spine
reading the S0 constant as a const generic; the per-byte `delimiters.contains` interpretive
dispatch — an S0 value threaded as a runtime `&[u8]` and re-scanned every byte — is precisely a
**binding-time error the analysis forbids**: an S0 value stuck at S1 because the template author
never lifted it. The disassembly in PROVE-12 §3 (`ldrb [x22,#N]` slice loads + `cmp x21,#..`
length guards → two immediate `and` masks) is the visible signature of stage-0 erasure.

Family 3D's claim is that a stage-annotated lowering emits `find_component_delim<const DELIMS>`
**automatically**, because BTA marks the delimiter set S0 and the residualizer splices it as a
const generic — no hand template edit. The SPEC's own forward ask makes this explicit: GN-5.W4
wants "12's const-mask generalized to every grammar-derived stop-set." That generalization *is*
this family. PROVE-05 (CSS shufti LUT) and the JSON structural alphabet (`StructuralAlphabet` at
`ir/src/lib.rs:435`, already computed S0 by `derive_recognizers`) are the same island class.

### 1.4 The unification with the type/value plane (the strong result)

R6 established that "the type plane costs zero at runtime" and that `->` is discarded at
`passes/src/lib.rs:203`. Under the stage lattice this is not a separate invariant — a projected
type is an **S0 value** (`Project.ty` is Comptime-seeded), and `DirectBuildTarget.type_ref`
(`ir/src/lib.rs:485`) residualizes to a Rust *type*, which has no runtime representation by
construction. **GN-3's ZERO-RUNTIME-COST invariant is the special case of stage-0 erasure applied
to the type sub-plane.** LOCK-1's whole type system and LOCK-3's stage plane are one erasure seen
from two angles: LOCK-1 types the S1 values, LOCK-3 erases the S0 program that computed the S1
program. This is exactly LMS's "using only types to distinguish the two stages of execution"
(Rompf & Odersky, CACM 2012) — the `Rep[T]` (S1) vs `T` (S0) discipline, but with the split made
an IR attribute rather than a Scala type constructor.

### 1.5 Per-site staging → the LOCK-2 seam

Not every S0 island *should* residualize as a monomorph. PROVE-12 itself shows the tension: of 5
delimiter sets, 3 collapsed to one shared monomorph and 2 were fully inlined into callers — a
size/speed trade. Const-generic monomorphization buys straight-line speed at a code-size cost;
a shared runtime `const` table is size-neutral but pays one indirection. Which residual form each
S0 site takes is a **per-site cost decision under the generated-size budget** — variables =
per-site staging form, constraints = the `lint-loc` budget + legality, objective = cyc/B. That is
the LOCK-2 decision driver verbatim; family 3D produces the *candidate* residual forms and hands
extraction to that engine via the existing `ir/src/cost.rs` `DecisionCspFacts` / `PriorityStep`
seam (`cost.rs:203,264`). Family 3D owns the staging *semantics*; LOCK-2 owns the staging
*schedule*.

---

## 2. The substrate

- **`Stage` + `binding_times` fixpoint** — `ir/` crate, mirroring `nullability`
  (`ir/src/lib.rs:320`). Monotone, `Changed`-flag convergence (the LLVM-style loop already used),
  content-hash only as `debug_assert`.
- **`Residual` value enum** — `ir/` (the stage-0 value plane); its constructors are the
  `emit_const` outputs. Colocated with `DirectBuildTypeRef` since a type-ref IS a residual.
- **`emit_const` / `emit_runtime` split** in `codegen`'s lowering — extend `LowerCtx` (which
  already carries `backend_shape` / `cost_facts` / `per_grammar_policy`,
  `codegen/src/lib.rs:177-184`) with a `stage_map: Vec<Stage>` field threaded from the BTA pass.
- **The BTA pass slot** — `passes::compile` (`codegen/src/lib.rs:101-103` → `passes`),
  after infer/shape (types resolved, so `Project.ty` is a known S0 value) and before
  `lower_to_rust`. It reuses the alphabets `derive_recognizers` already computes
  (`passes/src/lib.rs:338`) rather than recomputing them.
- **rustc as the residual emission target** — S0 residuals land as `const fn` + const generics +
  associated `const`s; MIR const-propagation + monomorphization complete the erasure LLVM-side.
  PROVE-12 proves this substrate is already load-bearing (mask-as-immediate, disasm-verified).

No new IR *tower level* is minted (this is the intra-family discipline, §5): `Stage` is an
LLVM-shape **attribute** on the existing `GrammarIr`/`BackendIr`, and erasure happens inside the
existing `lower_to_rust`, not in a new transposition. Family 3D is an *annotating pass + a split
leaf emitter*, the fewest moving parts that make the two-stage semantics real.

---

## 3. Integration seams (path:line)

- `ir/src/lib.rs:320-349` — `nullability`: the exact monotone-fixpoint template `binding_times`
  clones (same `Vec` shape, same `changed` loop, same per-`ExprKind` join).
- `ir/src/lib.rs:234-271` — `ExprKind`: the seed sites (`Literal:246`, `Regex:250`,
  `Project:261`, `Annotation:257` = S0; the cursor = S1).
- `ir/src/lib.rs:376-413` — `BackendExpr`: `ByteLiteral:388`, `RegexProgram:389`,
  `DirectBuild.{shape,fields}:403` are S0-residualizable; `TapeEmit:400`, `SpanMark:396`,
  `RepeatLoop:383` are the S1 spine.
- `ir/src/lib.rs:435` — `StructuralAlphabet`: already an S0 residual produced by
  `passes/src/lib.rs:338` `derive_recognizers`; family 3D *names* it as such and residualizes it
  the same way as the delimiter mask.
- `ir/src/lib.rs:483-541` — `DirectBuildTarget`/`DirectBuildTypeRef`/`DirectBuildDecode`: the
  type sub-plane, all S0, residualizing to Rust types (the R6 §5d "typed layer" wired zero-cost).
- `passes/src/lib.rs:203` — where `Project.ty` is currently discarded; under staging it is an S0
  value threaded into `emit_const`, not dropped (this is also GN-3.W2's Wall-3 cure — the two
  families agree on the seam).
- `codegen/src/lib.rs:177-184` — `LowerCtx`: add `stage_map`.
- `codegen/src/lower/css_scan.rs` (712 LOC, STOP-set derivation) + `json_sink_direct/dispatch.rs`
  (the first-byte dispatch, "91.5% hot leaf") — the two richest S0-island residualization sites
  beyond PROVE-12's CSS loop; the generalization targets.
- `ir/src/cost.rs:203,264` — `DecisionCspFacts` / `PriorityStep`: the per-site staging-form
  decision handed to LOCK-2.
- `prototypes/PROVE-12/patch.diff` — the reference implementation of one `emit_const`, to be
  reproduced byte-identically *from the annotated IR* as the corollary proof.

---

## 4. Prior art (citations)

- **Futamura projections** (Futamura 1971). The 1st projection: specializing an interpreter to a
  program yields a compiler. bbnf's generator specializing a grammar-interpreter over the grammar
  IS the 1st Futamura projection made concrete; the emitted parser is the residual program. This
  is the theoretical justification that "a parser generator" and "a partial evaluator of a
  universal parser" are the same artifact.
- **Jones, Gomard & Sestoft**, *Partial Evaluation and Automatic Program Generation* (1993). The
  canonical offline BTA + specialization: annotate binding times by fixpoint, then a
  specializer erases the static program. `binding_times` + `emit_const` is this scheme applied to
  the grammar IR. The "binding-time improvement" problem (§5 risk 2) is named here.
- **Taha & Sheard**, MetaML / MetaOCaml (POPL'97, TCS 2000; Kiselyov's BER MetaOCaml 2013). Staging
  via *explicit* brackets/escape/run rather than automatic BTA — "the programmer makes binding
  times explicit." bbnf's grammar surface already carries explicit stage-0 markers: `->`
  projections, `@` annotations, regex literals are the programmer's binding-time declarations.
  Offshoring (MetaOCaml code → C/LLVM) is the analogue of bbnf's emit-to-Rust+NEON.
- **Rompf & Odersky, LMS** (CACM 2012, "Lightweight Modular Staging"). Two stages distinguished
  *by type only* (`Rep[T]`). This is family 3D's exact thesis — the split lives in the types —
  and the reason the stage plane and the type plane unify (§1.4).
- **Jonnalagedda, Coppey, Stucki, Rompf, Odersky**, "Staged Parser Combinators for Efficient Data
  Processing" (OOPSLA 2014). *The* closest prior art: a staged parser runs ~50% faster than a
  hand-written recursive-descent parser and >25× faster than the unstaged combinator library, by
  evaluating the static grammar at comptime and quoting only runtime-dependent logic. bbnf is a
  manual, codegen-based instance of precisely this result; family 3D closes the gap between
  "manual codegen" and "systematic staging."
- **Kovács**, "Staged Compilation with Two-Level Type Theory" (2022, arXiv:2209.09729). The
  rigorous modern foundation: object-level (S1) and meta-level (S0) as two layers of one type
  theory, with a *provable* erasure to the object program. This gives family 3D its erasure a
  metatheorem rather than a convention — "stage-0-costs-zero" becomes a soundness property, not a
  measured hope, matching the constitution's clause (v) "type plane costs zero at runtime
  (verified invariant)."
- **Zig `comptime`** (production). A shipped language where the comptime/runtime split is a
  first-class type-system feature and `comptime` params generate specialized monomorphs — the
  direct production analogue of const-generic residualization. **Terra** (DeVito et al., PLDI
  2013) — Lua-staged low-level code, another production two-stage system.
- **rustc const-generics + const-eval + MIR const-propagation** — the emission substrate that
  completes the erasure LLVM-side (PROVE-12's `const fn delim_mask` + `<const DELIMS: u128>` is
  this exact mechanism, disasm-confirmed).

---

## 5. Named risks and counterexamples

1. **Monomorph explosion breaches the generated-size budget.** *Counterexample:* residualizing
   every regex DFA per-site as a const-generic table N-plicates generated code O(rules ×
   alphabet); PROVE-12 stayed +18 LOC only because there were 5 tiny masks. *Guard:* the per-site
   staging form is a LOCK-2 cost decision under the `lint-loc` budget (§1.5); the shared runtime
   `const` table is the size-neutral fallback residual. The staging law must never force
   monomorphization — it *offers* it as a candidate.

2. **BTA over-approximation silently forfeits the win.** *Counterexample:* a genuinely-static node
   marked S1 because a spurious dependency taints it (the classic binding-time-improvement gap) —
   no correctness bug, but the residual is never lifted and the speedup vanishes with no error.
   *Guard:* a **residual-count oracle** (number of S0 islands actually residualized) emitted as a
   regen artifact; a drop in the count fails the gate. This is the observability that makes the
   silent failure loud.

3. **Erasure unsoundness — an S0 node that secretly reads S1 data.** *Counterexample:* capacity
   hints (`@cap(875)`) are *corpus* facts, not grammar facts (GN-3 R1: `875`/`184`/`5_759` come
   from counting a specific document). Staging them S0 is sound only if the grammar *declares*
   them; deriving them from input is a leak. *Guard:* fail-explicit per the no-silent-epsilon law
   — an S0 node with an S1 operand is a `StageError::Leak`, never silently demoted to S1.

4. **The BTA becomes an orthogonal analysis subsystem.** *Counterexample:* a stand-alone stage
   inferencer that re-walks the IR duplicates `nullability`, `first_set`, and
   `derive_recognizers`' alphabet computation — a god-pass. *Guard (system-cohesion law):*
   `binding_times` shares the `nullability` fixpoint shape and *consumes* the alphabets
   `derive_recognizers` already computes; it adds one `Vec<Stage>`, not a new pass tower.

5. **The two-stage framing inflates into a third IR tower level.** *Counterexample:* modeling
   "stage 0" as a distinct transposition between HIR and mid-IR contradicts a minimal contract
   tower and the no-orthogonal-codepaths law. *Guard:* `Stage` is an attribute on the *existing*
   nodes (the LLVM annotating-pass shape), and erasure happens inside the existing
   `lower_to_rust`. This is the sharpest intra-family design constraint: family 3D is an
   annotation + a split leaf emitter, nothing structural.

---

## 6. What a prototype must test to move this family

- **P1 — the corollary proof (load-bearing).** Stage-annotate the CSS grammar IR, run
  `binding_times`, and have the residualizer emit `find_component_delim<const DELIMS: u128>` +
  `delim_mask` **from the annotated IR alone**, no hand template edit. *Gate:* `cargo xtask
  regen-css` produces output **byte-identical to PROVE-12's hand patch**; PROVE-12's parity
  checksum (track1_full=1, track1_rich=17839) MATCH; the residual is emitted, not authored. This
  proves the hand patch is a corollary of the law. Until this passes, the family is SPEC-only.

- **P2 — generalization beyond the one hand-picked loop.** Apply the identical stage-erasure to a
  site PROVE-12 never touched — the JSON structural first-byte dispatch
  (`json_sink_direct/dispatch.rs`) or the general-descent skip set — and measure the same class of
  deterministic ins/B reduction (PROVE-12's floor: −3.7…−6.4% ins/B). *Gate:* a NEW site
  residualizes with a measured, cold, paired win (per the profiling standing law), proving the
  law is general, not overfit to one CSS scan. This is GN-5.W4's explicit ask executed.

- **P3 — the per-site staging decision (the LOCK-2 seam).** Stage one set two ways — const-generic
  monomorph vs shared runtime `const` table — measure both cyc/B and generated LOC, and show the
  cost model *picks* per-site under the loc budget. *Gate:* the decision is a `DecisionCspFacts`
  output, not a hardcoded emitter branch (pluggable-components law).

- **P4 — the erasure invariant (stage-0-costs-zero).** A differential oracle proving the residual
  program's *output* is byte-identical to the un-staged interpreter across all corpora, and a
  `git diff` proving the residualization added zero runtime instructions beyond the erased
  constants. *Gate:* parity checksum MATCH (PROVE-12 §5 form) + the S0-erasure `Vec<Stage>`
  content-hash stable across regen (`Changed`-flag convergence, hash as `debug_assert`).

The family advances the instant P1 is green: it converts a certified hand patch into an emergent
property of a stage-annotated IR, at which point PROVE-05, the JSON alphabet, and every future
grammar-derived stop-set inherit residualization for free — the differentiator SPEC §1 names
("emits grammar-specialized kernels behind a cost model") reframed as one law: the generator is a
partial evaluator, and the parser is its residual.
