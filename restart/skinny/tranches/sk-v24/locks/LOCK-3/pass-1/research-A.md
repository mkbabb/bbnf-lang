# LOCK-3 · Family 3A — THE CONTRACT-TOWER (pass 1 research)

*A small fixed tower — three levels — where each lowering is a total function carrying a
checkable contract, and each level owns a well-formedness verifier plus a per-run refinement
oracle, all CI-enforced. The thesis: correctness of the transposition spine is not proven once
about the generator; it is validated on every regen run, per level, so a defect is localized to
the transposition that introduced it rather than surfacing as "the final output changed."*

---

## 1. The mechanism, made concrete

### 1.1 The tower that already exists (the substrate to formalize)

The generator today runs an implicit four-edge transposition chain. 3A does not invent a tower —
it names the levels, fixes their count, and hangs a contract on each edge. The extant chain:

| Level | Type (home) | Producer | Shape |
|---|---|---|---|
| **L0 · Grammar HIR** | `GrammarIr { rules, exprs }` (`ir/src/lib.rs:33-39`) | `grammar::parse_grammar` | arena-indexed `Rule`/`Expr(ExprKind)`, `RuleId`/`ExprId` newtypes (`ir/src/lib.rs:15-19`) |
| **L1 · Backend-agnostic mid-IR** | `BackendIr { recognizers, rules, shape_facts }` (`ir/src/lib.rs:351-358`) | `passes::compile` → `PipelineOutput` (`passes/src/lib.rs:32-68`) | `BackendExpr` (`ir/src/lib.rs:376-413`) + `ShapeFacts` + `Recognizer` + side-band `LayoutFacts` (`passes/src/lib.rs:90-100`) |
| **L2 · Backend shape** | `LoweredRust { rule_plans, sink_only_program }` (`lower/rust.rs:10-14`) | `lower::lower_to_rust` (`lower/rust.rs:32`) | per-rule shape-selected Rust bodies, cost-attributed |
| **(emission)** | `EmittedSource` text | `runtime_generator::emit_sink_only` (`codegen/src/lib.rs:210`) | generated `.rs` splice |

Between L1 and L2 sits a further total lowering, `sink_only::lower_program` /`lower_expr`
(`lower/sink_only.rs:126,174`) producing `SinkOnlyExpr` — evidence that the honest edge count is
already **four**, not three (see risk R3, §5). The multi-backend seat LOCK-3 names (Rust now, TS
later) is L1: `BackendIr`/`BackendExpr` are backend-agnostic; `LoweredRust` is the first
Rust-committed artifact, so the TS backend forks at the L1→L2 edge, not deeper.

### 1.2 What 3A adds: three organs per level

Each fixed level acquires a triad, uniform across the tower:

**(a) A well-formedness verifier — a total predicate, run after every pass.** This is the GHC
`-dcore-lint` / MLIR op-verifier / rustc `-Zvalidate-mir` discipline (§4). It is *not* the parser
of the level and *not* the lowering; it is an independent judge that reads a level's value and
returns `Ok(())` or an enumerated set of violations. The extant fragments to fold in:
- L0: `GrammarIr::resolve_refs` (`ir/src/lib.rs:84`), `validate`/`validate_resolved_refs`
  (`ir/src/lib.rs:110,300`), `nullability` fixpoint (`ir/src/lib.rs:320`). These are today
  *called ad hoc*; 3A unifies them under one `verify(&self) -> Result<(), Vec<ContractViolation>>`.
- L2: the W7 **fail-closed** block (`lower/rust.rs:32-92,112-170`) already verifies shape ==
  `cost.chosen`, decision-CSP `sat`/budget-`pass`, policy-facts coherence — this IS an L2 verifier,
  but expressed as `Err(String)` (weak; see R4). 3A retypes it into the `ContractViolation` lattice.

**(b) A round-trip / idempotence oracle.** L0 already has `GrammarIr::pretty` (`ir/src/lib.rs:124`)
whose fixed point is `parse ∘ pretty = id` (a tested surface — `pretty_prints_rules_deterministically`,
`ir/src/lib.rs:637`). L2's round-trip is **regen byte-identity**: `xtask check_target`
(`xtask/src/regen.rs:37`) fails if `emit` over unchanged `.bbnf` is not byte-for-byte the committed
`generated.rs` — the clean-regen law made a per-level idempotence contract.

**(c) A per-run refinement oracle — translation validation over the corpus.** Rather than prove
each lowering semantics-preserving once (CompCert's forward-simulation, §4), 3A validates each
*run*: a `Digest` fold over each level's observable output on every corpus grammar, asserting the
output at L_k and the output after `lower(L_k)` agree. The template exists and is proven: the
CSS `CSS_CANON_PROFILE` XOR-checksum that certified PROVE-12's residualization byte-identical
(the mask-fold `MATCH`, `track1_full=1`, `track1_rich=17839`), and the JSON family-B
`assert_direct_struct_parity` / `track1_digest`/`track2_digest` checksums
(`bbnf-bench/benches/json_parity.rs:19,202-218`). 3A generalizes the single end-to-end checksum
into a *per-edge* differential, so a mis-lowering is attributed to its edge.

### 1.3 The flagship contract — the typed-materialization-invariant as an L1 verifier

The sharpest, most load-bearing contract is the standing law that *every `->` in the grammar
reaches the tape emitter; inference composes types, never loses them.* Today it is **violated by
construction**: `Infer::infer_expr`'s `Project` arm discards the projected type —
`infer_expr(Project) = infer_expr(body)` (`passes/src/lib.rs:203-204`), so the type lattice never
sees `->`; and on the offset-tape path `DirectBuildField.target` is `None` for every descriptor
(`ir/src/lib.rs:469-471`; populated nowhere, per the value-plane cartography). The L1 verifier
that 3A mandates is exactly the predicate: *for every `ExprKind::Project` in the L0 grammar, the
L1 `BackendIr` contains a `ValueProject`/`DirectBuild` whose `type_ref` (`ir/src/lib.rs:483-496`)
is populated.* Wire it, and it turns RED on HEAD — proving the contract is non-vacuous (§6, P1).
This makes GN-3's Wall-3 cure (`docs/tranches/sk-v24/GN-3.md:43-51`) a **CI-checked contract**
instead of a hoped-for property; the two locks compose (3A supplies the check, LOCK-1/GN-3 supplies
the inference that satisfies it).

### 1.4 Totality — exhaustive match + explicit fail, never silent epsilon

"Total function" is enforceable at two grains. Structurally, the lowerings are already exhaustive
matches with no wildcard drop: `lower_expr: ExprKind → BackendExpr` covers all ten variants
(`passes/src/lib.rs:1318-1358`), `sink_only::lower_expr` covers all `BackendExpr` variants
(`lower/sink_only.rs:174-234`). This is the ISLE / no-silent-epsilon posture (§4) — the compiler's
own exhaustiveness checker is the totality proof for the *shape* map. What is *not* total is the
fact-lookup layer: `extract` panics on `missing active cost`, `missing rule`, `missing
materialization`, and one structural assertion `value is a sequence with explicit ws around alt`
(`passes/src/lib.rs:1783,1817,1862,1864`). Under 3A these become either (i) a proven-unreachable
discharged by an upstream verifier, or (ii) a typed `ContractViolation`, so the tower has no
`panic!` reachable from a well-formed input — the contract *is* the proof of unreachability.

### 1.5 Residualization positioned (the 3A answer vs 3D)

PROVE-12's const-fold-delimiter-mask residualization (`prototypes/PROVE-12/`, GO, CSS cyc/B
0.81–0.98×, +18 generated LOC) is, in the 3A framing, **not a distinct staging level**. It is a
semantics-preserving rewrite at the emission edge whose *only* obligation is the L2→text refinement
oracle: parity checksum `MATCH` + regen byte-identity. 3A refuses 3D's "IR as an explicit two-stage
program in the types" precisely because the contract-tower already discharges residualization
correctness with an oracle it must run anyway — adding a staging plane to the type system buys no
check the parity fold does not already give, and violates parsimony (Constitution vi, `no-deferrals`
/ `system-cohesion`). 3A's stance: **residualization is a validated rewrite, not a new tower level.**

### 1.6 The decision driver's seat

LOCK-3 names "the decision driver's seat." In 3A it sits *at the L1→L2 edge as a fact contract*,
not woven into an IR (that is 2C/3D's e-graph-in-the-IR route). `passes::compile` computes
`LayoutFacts { backend_shape, cost_facts, per_grammar_policy, same_substrate_union }`
(`passes/src/lib.rs:54-61`) via `derive_backend_shape_with_diagnostics`; `lower_to_rust` consumes
them behind the fail-closed gate (`lower/rust.rs:38-76`). 3A keeps the driver as an *external fact
table checked at the seam* — the CostFacts/DecisionCspFacts/ActiveCostFacts lattice
(`ir/src/cost.rs`) — because that keeps the IR a value with a decidable verifier, and keeps the
optimizer swappable (Constitution: pluggable-components). The contract at this edge is: no rule
lowers without cost facts whose `chosen` shape equals the backend shape and whose CSP is `sat`
within budget. That is the strongest contract in the tower today and the model for the rest.

---

## 2. The substrate (what to build)

Colocated per the grand edict — each level's verifier lives in the crate that owns the level:

```
crates/ir/src/verify/        # L0 + L1 IR value verifiers (GrammarIr, BackendIr)
    mod.rs                   # trait Contract { fn verify(&self) -> Result<(),Vec<ContractViolation>> }
    violation.rs             # enum ContractViolation (typed lattice, NOT String)
    grammar_hir.rs           # L0: folds resolve_refs+validate+nullability+pretty-roundtrip
    backend_ir.rs            # L1: totality of lower_expr coverage + typed-materialization-invariant
crates/codegen/src/verify/   # L2 verifier + refinement oracle
    lowered_rust.rs          # L2: the W7 fail-closed block, retyped from Err(String)
    refine.rs                # per-edge Digest fold (generalized PROVE-12 checksum + json_parity)
crates/xtask/src/gate/
    verify_each.rs           # runs every level verifier after its pass; the CI consumer (Y.13)
```

- `trait Contract` is the uniform organ; `ContractViolation` is a machine-enumerable enum
  (`UnresolvedRef`, `DroppedProjection { span }`, `ShapeCostDisagree`, `CspUnsat`,
  `NonIdempotentRegen`, `RefinementMismatch { edge, corpus }`, …) — every violation names its
  grammar span (Constitution: diagnostics-first, LOCK-1 echo).
- The refinement oracle is one generic `Digest` trait + a `differential(edge, corpus)` runner,
  fed the existing corpus loader (`test-fixtures`) and the canon fixtures.
- `xtask verify-each` is the consumer that satisfies the consumer-less-substrate law (Y.13):
  no verifier lands without the gate that reads its verdict.

Reuse map (no net-new logic where the check exists): `ir/src/lib.rs:84,110,300,320,124` → L0
verifier; `lower/rust.rs:32-170` → L2 verifier (retype only); `xtask/src/regen.rs:37` → L2
idempotence oracle; PROVE-12 checksum + `json_parity.rs:19,202-218` → refinement-oracle instances.

---

## 3. Integration seams (path:line where extant code serves)

- **L0 value + verifier fragments**: `ir/src/lib.rs:33-39` (GrammarIr), `:84` (resolve_refs),
  `:110`/`:300` (validate), `:320` (nullability fixpoint), `:124` (pretty — round-trip oracle).
- **L0→L1 lowering (total match)**: `passes/src/lib.rs:32` (compile), `:1318-1358` (lower_expr —
  the exhaustive shape map; `Project` arm at `:1349` carries `ty`, but infer drops it at `:203`).
- **L1 value + the type_ref carrier**: `ir/src/lib.rs:351-358` (BackendIr), `:376-413`
  (BackendExpr), `:465-535` (DirectBuildField/Target/TypeRef — the field the flagship contract
  guards), `:400-413` (TapeEmit/DirectBuild/ValueProject — the `->` sinks).
- **L1→L1.5→L2 lowerings**: `lower/sink_only.rs:126,174-234` (SinkOnlyProgram, total);
  `lower/mod.rs:19-27` (select_lowering — the 5-way shape dispatch); `lower/rust.rs:32-92`
  (lower_to_rust — the W7 fail-closed L2 verifier in embryo), `:112-170` (policy-facts check).
- **L2→text idempotence**: `codegen/src/lib.rs:101,130,194,210` (emit entries),
  `xtask/src/regen.rs:37` (check_target staleness = idempotence oracle).
- **Decision-driver facts**: `ir/src/cost.rs` (CostFacts `:5`, ActiveCostFacts `:173`,
  DecisionCspFacts `:203`), consumed `lower/rust.rs:38-76`; produced
  `passes/src/lib.rs:48-61`.
- **Refinement-oracle templates**: PROVE-12 `CSS_CANON_PROFILE` XOR-fold
  (`prototypes/PROVE-12/REPORT.md §5`); `bbnf-bench/benches/json_parity.rs:19,202-218`.
- **Whole-pipeline structural-lock gate 3A localizes**: GN-1.W1 gate
  (`docs/tranches/sk-v24/GN-1.md:270-289`) — regen byte-identity + checkasm — is an *end-to-end*
  oracle; 3A inserts the per-level contracts so a GN-1.W1/W2 failure points at an edge.

---

## 4. Prior art (with citations)

- **GHC Core Lint** (`-dcore-lint`, plus `-dstg-lint`, `-dcmm-lint`): the canonical per-level
  well-formedness verifier — type-checks the Core IR *after every optimization pass*. GHC's tower
  (Core → STG → Cmm → asm) is a *small fixed tower of typed IRs each with a lint*, the direct
  archetype for 3A. (Peyton Jones & Santos, "A transformation-based optimiser for Haskell.")
- **MLIR verifiers**: every op/trait carries a `verify()`; the pass manager runs verification
  after each pass (`mlir-opt --verify-each`); region/SSA/dominance invariants are the contract.
  IRDL/PDLL declare dialects and their invariants. (Lattner et al., "MLIR," CGO 2021.)
- **LLVM IR verifier**: `verifyModule` / the `Verifier` pass, run between passes under
  `-verify-each`; the "IR is verifiable at every point" invariant.
- **rustc MIR**: `-Zvalidate-mir` / `rustc_mir_transform::validate`, and crucially the **`MirPhase`**
  ladder — MIR advances through phases (Built → Analysis → Runtime) where *each phase strengthens
  the invariants a validator enforces*. This is literally the contract-tower: fixed phases, each
  with a checkable, monotonically stronger invariant set.
- **Cranelift**: the CLIF `verifier`; ISLE, the term-rewriting **lowering DSL** whose pattern
  coverage is a compile-time totality concern (the no-silent-epsilon posture as a language
  feature); regalloc2's **symbolic checker** validates each allocation *run* — translation
  validation shipped in production.
- **Translation validation** (the "validate each run, don't prove the pass once" school): Pnueli,
  Siegel & Singerman, "Translation Validation," TACAS 1998; Necula, "Translation validation for an
  optimizing compiler," PLDI 2000. This is 3A's theoretical spine: the refinement oracle validates
  each regen over the corpus rather than proving the generator correct.
- **Full verification (the opposite pole, deliberately not chosen)**: CompCert (Leroy, "Formal
  verification of a realistic compiler," CACM 2009) and CakeML prove per-pass forward simulation.
  3A rejects this by parsimony — the generator is a build tool, not a proof artifact; corpus-bounded
  translation validation buys the practical guarantee at CI cost, not a Coq development.
- **Nanopass** (Sarkar, Waddell & Dybvig, "A nanopass infrastructure for compiler education,"
  ICFP 2004; Keep & Dybvig 2013): the *anti-thesis* — many tiny passes, each with an
  auto-generated well-formedness check from a `define-language` form. 3A's "small fixed tower
  (three levels)" is the conscious opposite: the **fewest** IRs carrying the **strongest** per-level
  contracts. Naming both fixes 3A's position on the axis.

---

## 5. Named risks & counterexamples

- **R1 — vacuous verifier (the sharpest).** A verifier that checks only cheap invariants ("refs
  resolved") gives false confidence and misses the load-bearing defect. *Counterexample:* an L1
  verifier omitting the typed-materialization-invariant passes green on HEAD even though `->` is
  silently dropped at `passes/src/lib.rs:203`. *Guard:* the flagship contract (§1.3) must be in the
  L1 verifier, and the P1 prototype must *demonstrate it turns RED* on the current tree.
- **R2 — round-trip ≠ semantics-preservation.** `parse ∘ pretty = id` (L0) proves the printer, not
  the lowering; regen byte-identity proves the emitter is a function, not that it is *correct*. Only
  the differential refinement oracle tests preservation — and it is **corpus-bounded** (translation
  validation's known limit). *Counterexample:* PROVE-12's checksum covers 4 CSS corpora; a delimiter
  set never exercised at runtime could be mis-folded and never caught. *Guard:* the refinement
  oracle's coverage is a first-class metric; grammar constructs with zero corpus witnesses are
  reported as *unverified*, not assumed sound.
- **R3 — the tower is not three (count is asserted, not measured).** The extant chain has four
  transposition edges (L0→L1, L1→SinkOnly, SinkOnly→L2, L2→text). Declaring "three levels" may
  collapse two edges that deserve separate contracts, or pad two that are one. *Counterexample:*
  `BackendIr` and `LoweredRust` are close in shape today; forcing a contract between them may be
  ceremony, while `SinkOnlyProgram` (`lower/sink_only.rs`) is a real level with no verifier.
  Meanwhile LOCK-6's TS backend may *demand* a fourth level. *Guard:* the level count must be
  **measured** (Constitution vii), not decreed — the P3 prototype enumerates edges and labels each
  total/partial + its contract, and the "three" survives only if the enumeration supports it.
- **R4 — stringly-typed contracts.** The one real contract that exists (`lower_to_rust`) returns
  `Err(String)` (`lower/rust.rs:41-84`). A contract expressed as a formatted string is not
  machine-enumerable, not diff-stable, not span-bearing. *Guard:* `ContractViolation` is a typed
  enum before any new contract is added; retyping W7 is prerequisite work, not a follow-on.
- **R5 — CI cost / runtime bleed.** Verify-each adds regen/CI time; the risk is it creeps into the
  emitted parser (violating Constitution v, type-plane-costs-zero). *Guard:* verifiers run only at
  regen/CI; the P1 prototype gates on *regen byte-identity with the verifier on vs off* — the
  emitted `.rs` must be byte-identical, proving zero runtime footprint.

---

## 6. What a prototype must test to move family 3A

- **P1 — non-vacuity, localized.** Implement `Contract` for `GrammarIr`/`BackendIr`/`LoweredRust`,
  wire `xtask verify-each` after each pass, and prove the L1 verifier's typed-materialization-invariant
  **turns RED on HEAD** (where `->` is dropped at `passes/src/lib.rs:203`) while `cargo build`
  stays green — demonstrating a defect that build-green misses is caught, and *attributed to the
  L0→L1 edge*. Measure: verifier LOC, regen-time delta, and byte-identical emitted `.rs` with the
  verifier on/off (zero runtime footprint). *Falsifiable:* if no single edge localizes the defect,
  the tower cut is wrong.
- **P2 — the per-edge refinement oracle.** Generalize the PROVE-12 XOR-checksum and the json_parity
  digest into one `differential(edge, corpus)` runner; show it (a) reproduces PROVE-12's `MATCH` and
  the JSON `track1/track2` digests, and (b) *detects an injected mis-lowering* (e.g. a corrupted
  `delim_mask`) at the correct edge. *Falsifiable:* if an injected fault passes the oracle, the fold
  is not observing enough of the level's output.
- **P3 — settle the level count empirically.** Enumerate every transposition in the live pipeline
  (`grammar::parse_grammar`, `passes::compile`→BackendIr, `lower_expr`→BackendExpr,
  `sink_only::lower_program`→SinkOnlyProgram, `lower_to_rust`→LoweredRust, `emit_sink_only`→text),
  label each total/partial and name its contract, and report whether it collapses to exactly three
  contract-bearing levels or the honest number is four. Deliverable: the labeled edge-DAG. This is
  the measurement that either earns "three" or refutes it (R3).

---

*3A's claim in one line: the IR spine is correct because every level judges itself and every
lowering-run is validated against the level above it — a GHC-Core-Lint discipline over a
three-level (measured, not decreed) tower, with translation-validation refinement oracles standing
in for a CompCert-grade proof the generator does not need.*
