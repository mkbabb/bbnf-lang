# LOCK-3 — THE IR TRANSPOSITION SPINE — Prior-Art Research (pass 1)

Researcher: PRIOR-ART, pass 1. No family allegiance; registry withheld by protocol.
Problem class: the IR tower `grammar AST → typed HIR → mid-IR → BackendShape → emission`;
lowering contracts/invariants; residualization (PROVE-12) generalized; the decision driver's
seat; multi-backend (Rust now, TS later) over a backend-agnostic middle. Standing laws:
no value-discard, no silent epsilon, preserve rich AST (constitution iv/v/vi; SPEC §0/§4).

All `path:line` verified this pass. Web anchors cited inline; URLs in §7.

---

## 0. What our own evidence base ALREADY decides (the binding floor)

These are not open questions — they are measured facts the tower design must obey.

- **The value-discard trap is REALIZED at HEAD, measured.** `ExprKind::Project { body, target,
  ty: Option<String> }` (`ir/src/lib.rs:261-265`) carries `->` only as a bare string. Inference
  DISCARDS it: `Infer::infer_expr` for `Project` returns the body's type, dropping the
  projection (`passes/src/lib.rs:203`, cited R6 §5a). On general-descent `ty` folds to a
  4-way `ProjectionKind {Float,Bool,Tag,Span}` (`general_descent/view.rs:20-46`); on the
  offset-tape path `DirectBuildTarget.target` is `None` for every descriptor
  (`passes/src/lib.rs:1149-1287`). The rich schema IR (`DirectBuildTarget{type_ref, presence,
  cardinality, representation, decode}`, `ir/src/lib.rs:483-541`) EXISTS but is UNPOPULATED
  from the grammar. **This is precisely the MLIR "lowered too early → information loss" failure
  (§2) and the LLVM "brittle metadata" failure (§3), present in our own tree.** The
  typed-materialization-invariant ("every `->` reaches the tape emitter") is currently broken;
  the tower's first job is to close it. GN-3 confirms Wall 3 is half-fallen (`ExprKind::Project`
  exists at HEAD — thread into infer + `DirectBuild.type_ref`).
- **Zero-runtime-cost erasure is proven and must be preserved (constitution v).** SPEC §0 #08:
  HM core (221 LOC) derives a validated `DirectSchemaSet` in ~230µs compile-time, ZERO runtime
  cost (held under adversarial re-measure). Any staged/typed tower keeps stage-0 fully erased.
- **PROVE-12 is a working instance of staged residualization, not a one-off.** SPEC §0 #12 /
  PROVE-12 REPORT: `find_component_delim` const-generic over a `u128` delimiter mask;
  membership residualizes to two immediate `and`s (disasm-verified `REPORT.md §3`); regen
  bit-for-bit; +18 generated LOC; cyc/B 0.81-0.98×. The REPORT itself states the pattern
  "generalizes to any grammar-derived delimiter/STOP-set scan" (§7). This is 3D's seed.
- **The decision-driver seat already exists.** `ir/src/cost.rs`: `CostFacts` (:5),
  `ActiveCostFacts` (egraph/CSP selection provenance, :173), `DecisionCspFacts` (:203),
  `PriorityStep` 8-step (:264); `BackendShape` 5-variant (`ir/src/lib.rs:361`) selected by
  `passes::derive_backend_shape` (`passes/src/lib.rs:398`). LOCK-3's "decision driver's seat"
  is this seam, shared with LOCK-2.
- **Generated-size budget is law** (memory: generated-size-budget) + **no silent epsilon**
  (lowering catch-alls panic, never return Epsilon) + **fewest transpositions that suffice**
  (constitution vi, extreme parsimony). These directly constrain tower depth (§1) and forbid
  the nanopass "dozens of IRs" and MLIR "dialect proliferation" anti-patterns (§1, §2).

Current tower at HEAD (R1 §2): `parse_grammar → GrammarIr` (AST) `→ passes::compile` (infer →
shape → CSP → egraph) `→ BackendIr {recognizers, rules, shape_facts}` (`ir/src/lib.rs:352`)
`→ lower::lower_to_rust → runtime_generator`. Two mid-shapes coexist: `BackendExpr`
(`ir/src/lib.rs:376`) and the `DirectBuild*` schema. It is ALREADY a small tower — LOCK-3 is a
consolidation/contract problem, not a green build.

---

## 1. 3A CONTRACT-TOWER ancestor — the nanopass framework (Sarkar/Waddell/Dybvig 2004; Keep & Dybvig 2013)

The micropass structure — "a collection of many small passes, each performing a single task" —
is nanopass's thesis. **Wins that transfer to 3A:** each pass applies one transformation
between adjacent IRs, keeping invariants LOCAL and the lowering sequence easy to reason about;
per-pass testing/debugging. This is the intellectual parent of "each level a total function
with a checkable contract."

**The named trap (decisive for us):** naive micropass compilers "duplicate code for traversing
and rewriting ASTs" and "obscure the meaningful transformation." Nanopass's whole value is
*curing that trap* — `define-language` specifies each IR as a GRAMMAR, and the framework
GENERATES the traversal/catamorphism + enforces grammar conformance between passes (Keep
dissertation; nanopass.org; DeepWiki). **Consequence for LOCK-3: 3A and 3C are not rivals —
3C (grammar-specified IR) is the mechanism that makes 3A affordable.** A hand-written 3-level
contract tower re-pays the traversal-boilerplate tax nanopass explicitly eliminated. Also
decisive: nanopass runs DOZENS of IRs; our "fewest transpositions that suffice" + generated-
size budget forbid that. **The transferable core is the per-level grammar + generated traversal
+ conformance check, NOT the many-IRs count.**

## 2. The 3A-vs-3B tension IS the LLVM-vs-MLIR debate — progressive lowering vs single mid-IR

MLIR's progressive-lowering argument (multiple dialects) names our exact failure: "high-level
semantics must be lowered into LLVM IR early, losing information useful for optimization"; a
single mid-IR forces high-level constructs into "many intrinsics, conventions, and metadata,
which are brittle and hamper composability." **Our `Project.ty` discard (§0) and unpopulated
`DirectBuildTarget` (§0) are this failure, measured.** The MLIR remedy — keep each abstraction
at its own level, lower late — is the same principle as our standing law "preserve rich AST /
no value-discard."

**But MLIR's own cost is dialect proliferation** — which extreme parsimony rejects. So the
tower must be MLIR-in-principle (don't lower `->` intent early) yet LLVM-in-shape (few levels).
This favors **3B SINGLE-IR-MULTIPASS refined by annotating passes with BackendShape-as-
attributes** — provided the attributes are actually POPULATED (the HEAD bug is that they
aren't). The synthesis question handed forward: can one rich IR carry projection intent as a
live attribute to emission (3B) without the "brittle metadata" trap MLIR warns of? Nanopass's
answer (§1): only if the attribute is part of a CHECKED grammar, not ad-hoc metadata.

## 3. 3B / 3D killer anchor — Cranelift's acyclic e-graph (aegraph)

The single most load-bearing practical prior art, because it adjudicates "make the middle IR an
e-graph" (LOCK-2 2B, LOCK-3 3D) against real compile-speed constraints:

- **Classic egg-style e-graph + separate extraction did NOT fit an in-place SSA IR.** Full
  equality saturation's parent-pointer/canonicalization cost was "untenable" at JIT speed.
- **Fusion won:** IR and e-graph are ONE structure. Pure (side-effect-free) nodes float
  location-free in e-classes; side-effecting ops anchor a reduced "side-effect skeleton."
  Unions happen ONLY at node-creation; rewrites apply EAGERLY during construction (cascades-
  style), not in a separate saturate-then-extract phase.
- **The rewrite engine is GENERATED, not egg:** rewrites are driven by ISLE-compiled Rust
  "because... we need high control" and compile speed. ISLE extended with multi-extractors /
  multi-constructors to iterate e-nodes in an e-class.
- **Scoped elaboration** (dominator-tree traversal + scoped hashmap) subsumes GVN and does
  scheduling in one pass — one structure solves phase-ordering.
- **Rejected/failed, named:** per-block e-graphs (lose cross-block opt); PDG/VSDG predicate
  embedding ("relooping and recovering a CFG... yields both bugs and slow compilation");
  separate extraction phase; **optimization fuel is unsolved** ("a problem we will need to
  solve," deferred). Result on SpiderMonkey.wasm: +1% compile time, −13% runtime.

**Lessons for LOCK-3:** (a) if the mid-IR becomes an e-graph, use the ACYCLIC/eager-cascades
form, not full saturation — our generated-size + compile-budget laws (§0) make Cranelift's
verdict ours a fortiori; (b) the rewrite engine should be GENERATED from rules (ties 3C +
codegen-is-the-generator), echoing ISLE-over-egg; (c) "one structure carrying scheduling +
optimization + lowering" is the 3B "single rich IR, annotating passes" thesis, validated in
production. RVSDG/VSDG (regionalized value-state dependence graph) is explicitly the road
Cranelift REFUSED for the semantic-gap cost — a caution against any control-as-nested-
expression reframing of our mid-IR. (See also "E-Graphs as a Persistent Compiler Abstraction,"
arXiv 2602.16707, for the general in-place-e-graph position.)

## 4. 3C GRAMMAR-HOMOIOUSIA — ISLE term-rewriting + metacircular bootstrap

- **ISLE = "transpositions are grammar-to-grammar maps" in production.** Input IR is a tree of
  terms; rules rewrite to another tree of terms; all rules compile to ONE single-pass decision
  tree, sharing work, honoring per-rule priorities. Used for BOTH machine-independent opt
  rewrites AND instruction lowering across **four ISAs (x86-64, aarch64, RISC-V, s390x)** —
  ISLE already demonstrates ONE rewrite-rule DSL retargeting a backend-agnostic middle to many
  targets. **Direct evidence for LOCK-3's multi-backend clause (Rust now, TS later) over one
  middle.** Named practicality requirement: multi-extractors/multi-constructors + user-
  configurable priorities.
- **The bootstrap fixpoint oracle comes free with 3C.** Metacircular parser-generator practice
  (van Rossum PEG meta-grammar; nanopass `define-language`; Chez self-host) gives the standard
  correctness proof: regen∘regen byte-identical ("if compiled again the output is the same,
  proving the generated meta-parser works"). Our `xtask check-*` stale gate + PROVE-12 "regen
  bit-for-bit" already IS this. A grammar-specified IR grammar (3C) inherits nanopass's
  generated-traversal win (§1) AND the bootstrap fixpoint oracle (aligns with LOCK-7 7A/7D).

## 5. 3D STAGED-RESIDUAL — multi-stage programming / partial evaluation / Futamura

- **"Two-stage program with the split in the types" is LMS's `Rep[T]`, exactly.** Multi-stage
  programming annotates which code runs at generation time vs runtime; LMS (Rompf, Scala)
  realizes the first Futamura projection with `Rep[T]` binding-time types and reports >10× over
  interpretive baselines. **PROVE-12 is a hand-rolled instance:** const-eval the mask at
  generation time (stage 0) + monomorphize so each call site beta-reduces (stage-0 erasure).
  3D generalizes it into a principled binding-time discipline in the type plane.
- **Residualization = erasure of stage 0 = partial evaluation of the interpreter to the
  program** (first Futamura projection). The mechanism 3D needs is classical **binding-time
  analysis**: classify each IR site stage-0 (grammar-constant: delimiter sets, STOP-sets,
  first-sets, dispatch keys) vs stage-1 (input-dependent). Our decision-driver seat (§0,
  `cost.rs`) is where per-site staging would be solved by LOCK-2's engine (3D's own clause).
- **Trap:** MSP's power is bounded by what binding-time analysis can prove static; over-eager
  staging bloats generated code (collides with the generated-size budget). PROVE-12's +18 LOC
  is the disciplined shape; the generalization must keep monomorphs "compiler-emitted, not
  template-duplicated" (REPORT §6).

## 6. 3A oracles — translation validation & the incompleteness trap

Necula's translation validation (PLDI'00) is 3A's per-level contract mechanism: compare the IR
before/after each pass and verify semantics preserved; retrofittable at "about the effort of
one compiler pass"; if it succeeds the run is correct. **But the named trap is decisive:
benchmark-driven differential validation is INCOMPLETE — "would not identify errors... not
exercised by the benchmarks."** So 3A's CI round-trip/parity oracles MUST be backed by a
conformance corpus, not just the canon benches. We already have the primitive: PROVE-12's
`CSS_CANON_PROFILE` XOR-folds every parse into a checksum (byte-identical parity gate). The
tower generalizes this to a per-level parity cksum + a conformance corpus (ties LOCK-6 6D
oracle-isomorphism). The "no silent epsilon" law (lowering catch-alls panic) is the totality
half of the same contract.

### Cross-cutting traps (comparable systems, named)

| System | Named trap | Bearing on LOCK-3 |
|---|---|---|
| Nanopass | traversal boilerplate; dozens-of-IRs | 3A needs 3C's generated traversal; parsimony caps depth |
| MLIR | lower-too-early info loss; dialect proliferation | our `Project.ty` discard IS this; keep intent, few levels |
| LLVM single mid-IR | brittle intrinsics/metadata for high-level constructs | our unpopulated `DirectBuildTarget`; attributes must be checked-grammar |
| egg full saturation | parent-pointer cost untenable at compile speed; extraction phase | if mid-IR=e-graph, use acyclic/eager (Cranelift), generated rules |
| VSDG/RVSDG/PDG | relooping semantic gap → "bugs and slow compilation" | do NOT reframe control as nested-expression mid-IR |
| Translation validation | benchmark differential testing incomplete | 3A oracles need conformance corpus, not just benches |
| MSP/LMS | over-staging bloats code | 3D bounded by binding-time analysis + size budget |

---

## 7. Sources

Web:
- Nanopass: https://nanopass.org/ ; Keep dissertation https://andykeep.com/pubs/dissertation.pdf ;
  Sarkar/Waddell/Dybvig https://www.cs.tufts.edu/comp/150FP/archive/kent-dybvig/nanopass.pdf ;
  https://deepwiki.com/nanopass/nanopass-framework-scheme
- MLIR vs LLVM progressive lowering: https://www.lei.chat/posts/compilers-and-irs-llvm-ir-spirv-and-mlir/ ;
  https://mlir.llvm.org/docs/TargetLLVMIR/
- Cranelift aegraph RFC: https://github.com/bytecodealliance/rfcs/blob/main/accepted/cranelift-egraph.md ;
  RVSDG anchor https://github.com/egraphs-good/egg/discussions/106 ;
  E-graphs as persistent compiler abstraction https://arxiv.org/abs/2602.16707
- ISLE: https://cfallin.org/blog/2023/01/20/cranelift-isle/ ;
  language ref https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/isle/docs/language-reference.md
- MSP / Futamura / LMS: https://arxiv.org/pdf/1611.09906 (Futamura diagrammatic) ;
  LMS via https://continuation.passing.style/static/papers/oopsla20.pdf
- Translation validation: Necula PLDI'00 https://people.eecs.berkeley.edu/~necula/Papers/tv_pldi00.pdf ;
  VOC https://www.jucs.org/jucs_9_3/voc_a_methodology_for/Zuck_L.pdf
- Metacircular bootstrap: https://medium.com/@gvanrossum_83706/a-meta-grammar-for-peg-parsers-3d3d502ea332 ;
  https://en.wikipedia.org/wiki/Self-hosting_(compilers)

Codebase (verified `path:line`, HEAD): `ir/src/lib.rs:261-265,352-541` (Project/BackendIr/
BackendExpr/BackendShape/DirectBuild schema); `ir/src/cost.rs:5,173,203,264` (decision-driver
seat); `passes/src/lib.rs:203,398,1149-1287,1403-1490` (infer-discard, shape derive, plan,
roles); `codegen/src/lib.rs:101,130` + `lower/` (emit tower); PROVE-12 `REPORT.md §1-7`;
seeds R1 §2/§8, R6 §5/§8; GN-3.md (three walls, Wall 3 half-fallen); SPEC §0 #08/#12 + §4 laws.
