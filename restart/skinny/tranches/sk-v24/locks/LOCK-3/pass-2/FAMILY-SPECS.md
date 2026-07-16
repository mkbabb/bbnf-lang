# LOCK-3 — THE IR TRANSPOSITION SPINE · pass-2 FAMILY-SPECS

*Synthesis (Fable-tier), pass 2. One cogent specification per family while routes remain
incompatible, then the incompatibility map. Elegance earns nothing on its own; a family stalled on
a missing primitive of equal difficulty to the original problem is marked BLOCKED-candidate. Built
on the pass-1 registry dispositions (3A ADVANCED-discipline / 3B ADVANCED-substrate / 3C BLOCKED /
3D BANKED), re-cut against the fresh pass-2 research-{A,B,C,D,priorart} sweep, and bound by the
CROSS-LOCK ledger (XL-A…H) as settled fact.*

---

## 0. The settled baseline (pass-2, re-anchored to live; base-pin reconciled)

**Base-pin (XL-H).** The directive's re-pin token `b3cb8c16b` is a **stale ancestor meta-commit**
(the lock-set amendment), 9 commits behind live (`git rev-list --count b3cb8c16b..HEAD` = 9). Live
master HEAD is **`bbnf-lang@c93fc43143`** (confirmed this session: `git rev-parse HEAD` =
`c93fc43143743…`; skinny is a subtree sharing that git, no separate `.git`; `passes/src/lib.rs` is
**2096 LOC**, not the R6-seed 2750). All four pass-2 researchers pinned correctly to `c93fc43143`
and named it; the earlier draft of this synthesis anchored one ancestor back at `7790134675c6` (an
ancestor of HEAD — `git merge-base --is-ancestor` true — across which the cited anchors are
byte-stable). The three crux facts were **re-greped by hand on live `c93fc43143` this pass**:

- `passes/src/lib.rs:204` — `ExprKind::Project { body, .. } => self.infer_expr(*body)?` (the `->`
  ty-drop); `:203` — `ExprKind::Annotation { .. } => Type::Builtin(BuiltinTy::Unit)` (a **non-defect**,
  legitimately untyped directive). **Confirmed verbatim.**
- `crates/codegen/src/direct_schema.rs:52` — `enum DirectTypeKind { Struct { … } }`, **one variant**;
  no `Sum`/`Enum`. **Confirmed** — the sum-node algebra is un-emittable today.
- `crates/codegen/src/runtime_generator.rs:98` — `if !sink_only.has_rule("value")` name-gate.
  **Survives** (clause-vi-forbidden).

The spine at HEAD is a five-edge / three-IR-level chain (research-A/B enumerate identically):

```
E0 .bbnf ─parse─▶ GrammarIr ─E1 infer─▶ [LayoutFacts attrs] ─E2 single_plan/lower_expr─▶ BackendIr
   ─E3 lower_program─▶ SinkOnlyProgram ─E4 emit_sink_only─▶ .rs text
   (ir:33/234, 10-arm ExprKind)   (passes:1283/1318/1362, 13-arm BackendExpr)   (sink_only.rs, 13-arm SinkOnlyExpr)
```

**Three facts bind every family (pass-2 sharpened).**

1. **The tower is 3B-shaped today and the redundancy is measured.** `lower_expr` is a **near-1:1**
   structural re-projection (`Seq→Seq`, `Alt→Alt`, `Repeat→RepeatLoop`, `Project→ValueProject`);
   `lower_program` is a second near-1:1 (the only deltas: `TapeEmit{kind}`→bare `TapeEmit`,
   `DirectBuild{shape,fields}`→boxed `DirectShape`, `SpanMark` drops its kind). One AST sum,
   **re-declared twice**, with a five-node emit frame (`SpanMark·body·SpanMark·TapeEmit·DirectBuild·
   Return`) spliced by ONE function, `materialize_rule` (`:1362`), keyed by `RuleId`. LOCK-3 either
   **hardens** this (3A/3B) or **re-founds** it (3C/3D); every divergence from the GN codex is a
   **named supersession** (Constitution ix).

2. **The `Project.ty`@204 thread is QUADRUPLE-homed and pass-1 discharged it in NONE** (registry
   cross-lock directive 1). All four families' pass-2 research now converge on **one identical wire**:
   thread `ty` at `:204` → `type_facts.node_types[expr]` (the attribute table, `passes:93`, already
   sibling to `nullability`) → `DirectBuildField.target` (today hardcoded `None` on every offset-tape
   descriptor, `passes:1149-1287`) → the typed emitter. The families differ **only in what they do
   with the consumed value** — 3A *verifies the equality*, 3B *stores it as one attribute*, 3C *reads
   it as a sum-variant payload*, 3D *erases it as an S0 residual to a Rust type*. This convergence is
   the pass-2 result: **the thread is a single-owner born-RED gate; the other three consume, they do
   not re-derive.** The owner is a GN-3 wave (`no-value-discard`, XL-F names `TypeAnalysis::from_ir`
   as the fact-contract consumer — a greenfield/legacy deliverable, absent on live skinny: `rg
   TypeAnalysis` → 0 hits, so the consumer is itself a born-RED edge).

3. **The live residual already ships; PROVE-12's u128 flagship is RETIRED** (registry, XL). The
   grammar-derived stop-set ships as `ScanAlphabet::derive_alphabet` → `const … : &[u8]` feeding a
   SIMD scan (`css_scan_direct`). PROVE-12's patch targeted `find_component_delim`/`CSS_GENERATED_RS`,
   both **gone from the emit path**; its numbers stand, its patch is wrong-artifact. Residualization is
   a **live fact**, not a design; the open question is whether *naming* it (a `Stage` attribute) buys
   anything the imperative emitter does not.

**The three axes (unchanged, but two have collapsed at pass-2):**
- **Axis M — representation multiplicity** (3A distinct typed levels ⊗ 3B one IR): **the hard
  incompatibility of pass-1, now DISSOLVED** — research-A pass-2 no longer poses a tower ("the family's
  product is NOT new IR levels — it is a verifier lattice welded onto the edges that already exist").
- **Axis R — residualization ownership** (3D forced-BTA ⊗ 3C authored-rewrite): **the contested seat,
  now largely COLLAPSED** — the live residual ships imperatively (`ScanAlphabet`), neither theory owns
  it via artifact; 3D adds a *fact*, 3C needs a *born-RED engine*.
- **Axis S — IR substance** (hand-written enums ⊗ bbnf-specified node grammars): **isolates 3C; the one
  live incompatibility that remains, gated on `DirectTypeKind::Sum`.**

---

## FAMILY 3A — CONTRACT-TOWER · ADVANCED (as discipline on the 3B substrate) · registry-floor 55%

**Architecture.** NOT a new tower of distinct typed levels (that formulation is BANKED, pass-1;
do not re-litigate). 3A pass-2 is a **verifier lattice welded onto the five spine edges that already
exist** (E0–E4, §0), plus the one typed table the L1→L2 contract obliges the emitter to CONSUME
instead of re-derive. Each edge Eₖ is already a total function; the family ADDS a checkable contract
Cₖ (an executable oracle predicate) run at regen and CI-enforced. The three contracts:
- **C1 (E1 analysis)** — every `ExprId` reachable from an entry rule has a `node_types` entry, and
  every `Project` node's node-type is the RESOLVED projected type, not the bare body type
  (`no-value-discard`, `no-silent-epsilon`).
- **C2 (E2 materialize) — the CONSUME contract** — `DirectBuild.field.target == node_types[expr]` per
  `Project` ExprId; i.e. the struct stratum is *read from L1*, never re-derived by
  `derive_materialization_roles` (`:1403`, the byte-literal classifier). The equality **IS the edge**:
  re-derivation makes the two diverge on any grammar whose `->` disagrees with the byte heuristic →
  the oracle flags RED. Re-derivation becomes definitionally a contract violation, not a style choice.
- **C3 (E4 emit) — parity** — the emitted parser's tape columns are byte-identical to the independent
  hand-written track2 oracle and canonical-string equal. **This oracle already exists and CI already
  runs it**: `parity::assert_parity` (`bbnf-bench/src/parity.rs:23`) vs `track2/json.rs` (a genuine
  two-implementation differential, McKeeman/Csmith).

**Mechanism.** `ContractViolation` a typed enum (never `Err(String)`), the retyping being real
prerequisite work (pass-1 PROVEN). The distinctive organ — the one thing separating 3A from a
tautological `verify()` — is the **per-edge differential runner** `differential(edge, corpus) ->
DiffReport`, `edge ∈ {E0..E4}`: it (a) reproduces the three extant digest families (JSON tape-column
+ canonical-string at E4, the CSS `CSS_CANON_PROFILE` XOR-fold, node-type coverage at E1, stratum
consistency at E2) by selecting the oracle apt to the edge, and (b) localizes an **injected**
mis-lowering to the LOWEST edge whose Cₖ breaks. Coverage metric = **per-edge construct coverage**
(`covered_arms / total_arms` of the edge's sum, witnessed by the fleet `{json, css_l4, sheets}`); a
zero-witness arm reports **UNVERIFIED (sound-unknown)**, distinct from VERIFIED-SOUND, and is NAMED
(e.g. `TapeKind::Member/Element` if no fleet grammar emits them). Prior art is settled precedent, not
invention: rustc `-Zvalidate-mir` `Validator`, LLVM `-verify-each`, MLIR `--verify-diagnostics`
legalization + type-materialization hooks (the strongest structural match to C2), translation
validation (Pnueli/Necula), RandIR differential (Ofenbeck).

**Integration seams (path:line, live).** C1: `passes:204` (drop) / `:212` (insert) / `:93`
(`node_types`). C2: `:1362` (`materialize_rule`), `:1403` (`derive_materialization_roles` — retire on
typed path), `:1136` (`derive_materialization_plan` — populate `target`). C3: `bbnf-bench/src/
parity.rs:23`, `track2/json.rs`, the `gate.rs` scalar↔NEON `parity_ok`. Regen digest gate:
`xtask/src/regen.rs:37` (`check_target`), `:177-184` (hash). Legacy consume-pattern reference:
`crates/core/.../driver/analysis.rs` `TypeAnalysis::from_ir` reads `ir.types`, never re-derives — the
shape C2 mirrors (born-RED: absent on live skinny).

**Gates it carries (born-RED).**
- **G-3A-1** — `verify_project_types` reads **RED on HEAD** (for every projected ExprId,
  `node_types[project] == node_types[body]`) and **GREEN once `:204` threads**. Witness: the **Sheets**
  grammar (the only live grammar exercising `->`; JSON has zero — R1). EXECUTED, not mapped.
- **G-3A-2** — the PREDICATE ALONE has zero emit footprint: regen content-hash byte-identical with it
  compiled in vs out. (Stated caveat: *threading `ty`* changes typed-direct output where the byte
  heuristic and the true `->` disagree — that is C2's job, not the predicate's; the zero-footprint
  claim covers the predicate only.)
- **G-3A-3** — `differential(edge, corpus)` reproduces `assert_parity`'s tape digest AND the CSS
  XOR-fold, and an injected mis-lowered `TapeKind` (Element-for-Sequence) localizes to **E2** (C1
  green / C2 red / C3 red).
- **G-3A-4** — per-edge construct-coverage table over `{json, css_l4, sheets}`; every zero-witness arm
  NAMED as UNVERIFIED.

**Open questions.**
- **Q-3A-1** — Does C2's equality hold under **projection chains** (`a -> b`, `b -> c`)? Node-types
  must compose to a fixpoint, never silent Epsilon (research-A R4). Untested; a synthetic Sheets-style
  chain is the guard.
- **Q-3A-2** — **Vacuous CONSUME.** If `derive_materialization_roles` is kept as a "fallback," C2
  passes trivially (relocation, not retirement). C2 must make the node-type the SOLE producer on the
  typed path; the byte classifier survives ONLY as the JSON no-`->` structural-recovery fallback. Is
  that survival itself a clause-vi violation, or a lawful floor until JSON gains `->`? (Interlocks
  LOCK-7; see the incompatibility map.)
- **Q-3A-3** — **Who validates the validator?** A shared-interpreter bug passes the oracle falsely;
  mitigated by the divergent hand-written track2 + XOR-fold + NEON-parity being three independent
  algorithms, but residual common-mode risk is unquantified.

**Disposition: ADVANCED (discipline). The standalone typed-multi-level-tower is BANKED** (pass-1).
Nothing is missing; the flagship contract is buildable now and provably non-vacuous; the distinctive
organ (the differential runner) is fully specified but **UNRUN**. 3A composes with 3B, does not compete.

---

## FAMILY 3B — SINGLE-IR-MULTIPASS · ADVANCED (the convergent substrate) · registry-floor 50%

**Architecture.** One rich IR — the existing `GrammarIr` arena (`ir:234`, 10-arm `ExprKind`) — refined
in place by annotating passes; every derived plane is an attribute table keyed by `RuleId`/`ExprId`.
**Delete L1 (`BackendIr`/`BackendExpr`) and L2 (`SinkOnlyProgram`/`SinkOnlyExpr`).** Emission becomes
one generation-time fold `emit(&GrammarIr, &Attributes) -> String`. The live pipeline is **already
three-quarters this shape**: `LayoutFacts` (`passes:91`) is a bundle of seven per-`RuleId`/`ExprId`
attribute maps; the charter's "BackendShape as attributes" is literally true today
(`backend_shape: HashMap<RuleId, BackendShape>`, `:96`). What 3B removes is the *tower below the
attributes* — the two structurally-redundant transposition IRs. This is a **generation-time** fold
producing straight-line Rust, NOT a runtime lazy-cursor fold (the SPEC §0 "fold-over-cursor dead"
refutation governs the runtime and does not touch it).

**Mechanism (two strata, Y.13-honest).**
- **Stratum 1 (lands now).** Delete `single_plan`/`lower_expr`/`materialize_rule` (`passes:1283-1389`)
  and the second/third enums; the emitter walks `GrammarIr` directly, reading `attrs.backend_shape[id]`
  to pick the emit family and `attrs.materialization.descriptors[id]` to decide framing.
  `materialize_rule`'s splice is a **pure derivation** of `(grammar, descriptors)` — recomputed inside
  the fold (emit span-start, recurse body, emit span-end/tape/direct-build) without materializing
  `BackendExpr::Seq[…]`. Node arms become match arms in the fold visitor.
- **Stratum 2 (lands with its first Transform ONLY).** The `Analysis`/`Transform`/`PassManager`
  invalidation machinery is **consumer-less until a mutating pass exists** and is forbidden now (Y.13).
  Stratum 1 is honestly "delete a redundant tree"; the "LLVM shape" that names the family is Stratum 2.

**The honest data-flow caveat (research-B §2).** `SinkOnlyProgram` carries five facts collected by
side-effect during the L2 walk (`SinkOnlyFacts`: `direct_shapes`, `span_kinds`, `literals`,
`dispatch_alt_count`, `policy_summary`). `policy_summary` is pure attribute re-export; the other four
are traversal accumulations — recomputable, but they prove the transposition currently carries
**collection state, not just shape**. The fold must accumulate them in its single pass (LLVM's
one-walk-collects-all). None reads anything outside `GrammarIr` + `backend_shape` + the descriptor map
→ recomputable byte-identically, provable by a data-flow trace. **This is the specific obstacle the
byte-identity gate tests.**

**The `:204` consumption (single-IR form).** Record the declared type once into `proj_types:
HashMap<ExprId, Option<String>>` (or fold into `node_types`), populated at `:204`; every downstream
formulation READS it where it today re-derives (`classify_projection`'s 4-way `general_descent/
view.rs:20-46` fold; `derive_materialization_plan`'s `None`-fill). The single-IR shape is exactly
this: **the type is one more attribute table over the one IR**, consumed where materialization is
decided, never a second lattice re-derived from bytes.

**Integration seams.** `ir:234` (`ExprKind`); `passes:91` (`LayoutFacts` → `Attributes`), `:1283-1389`
(delete), `:204/1136` (thread + populate `target`); `sink_only.rs` (delete); `runtime_generator.rs:98`
(`has_rule("value")` — retire), `:57/228` (CSS `render`→`css_scan_direct`, the separate island);
`xtask/src/main.rs:39-52` (the regen×check dispatch).

**Gates it carries (born-RED).**
- **G-3B-1 (killer, UNRUN in pass-1).** Implement the fold for `{json, sheets, typed}`; EXECUTE
  `regen×check` — **all outputs byte-identical** (regen/check × json/sheets/typed; CSS declared as a
  separate terminal-emitter island, research-B route (b)). Mapped ≠ executed. This is the one
  falsifiable gate that earns the family.
- **G-3B-2.** `has_rule("value")`@98 **GONE** (deleted, replaced by the `backend_shape` attribute — a
  fact, not a name; research-B judges this a CLEAN retirement, testable).
- **G-3B-3.** `derive_materialization_roles`@1403 disposition NAMED: **RETIRED** (requires JSON to carry
  `->`/schema so the grammar is authoritative for materialization role) **or RELOCATED** (the floor, if
  JSON stays annotation-free) — carried as a born-RED edge to LOCK-7 grammar totality. Relocation
  smuggled silently is the failure mode.
- **G-3B-4.** Per-`RuleId` vs per-`ExprId` key: **counterexample sheets** carries `->` at multiple
  sub-expression positions within one rule (`google-sheets.bbnf:6,12,17-18,34-42,97`) → recommend
  per-`ExprId` (subsumes per-`RuleId`); enumerate every fleet rule with >1 materialization decision to
  confirm before committing.
- **G-3B-5.** Report net-LOC delta (**≈ −220**, one sum instead of three) and the emission-extensibility
  count (**5→2**: `BackendShape` 5 arms → the live fleet exercises {tape-building, sink} = a 2-valued
  attribute × orthogonal policy facts).

**Open questions.**
- **Q-3B-1** — Does byte-identity actually hold, or does a `SinkOnlyFacts` accumulation carry an
  emit-order fact the fold cannot reproduce? (The caveat above; the gate answers it.)
- **Q-3B-2** — `derive_materialization_roles` retirement is CONDITIONAL on JSON `->`. Is the LOCK-7
  grammar-totality obligation acceptable, or is relocation the honest floor for this pass?
- **Q-3B-3** — Attribute coverage has no *type-system* enforcement (Rust's exhaustive `match` covers
  node kinds, not "did every `Project` write its attribute"); the coverage verifier is a runtime
  debug-assert — does that re-import the vigilance 3B claims to abolish? (This is exactly where **3A's
  C1 grafts in** — the named cross-pollination.)

**Disposition: ADVANCED — the strongest structural claim and the convergent substrate.** The deletion
targets all exist on live; the collapse is executable today behind a byte-identity gate; no cross-lock
primitive blocks Stratum 1. The killer gate (G-3B-1) remains **UNRUN** — "executable ≠ executed."

---

## FAMILY 3C — GRAMMAR-HOMOIOUSIA · BLOCKED-candidate · registry-floor 30%

**Architecture.** The IR is of *like substance* to the grammars it lowers: its node algebra is
bbnf-specified in an `ir.bbnf` whose `->` sum-of-products projections author the node kinds and their
codec; the same direct-to-struct emitter that produces JSON/CSS parsers regenerates `ir/src/lib.rs`;
the generator eats itself. **This is not speculative for this codebase** (pass-2's strongest new
evidence): bbnf is **already self-hosted** — `grammar/bbnf/{bbnf,expressions,types}.bbnf` describe bbnf
in bbnf *with typed `->` projections in use* (`identifier … -> Span`; `mapped_factor` declares the
`-> : type` syntax itself), and `types.bbnf` carries the header hook "*Includes … arbitrary
identifiers (for IrNode, etc.)*" — the grammar tier **already anticipates `->`-projecting rule outputs
onto IR node types**. bbnf already describes ebnf/bnf/g4/regex as grammars. The grammar-of-grammars
capability is proven; the exact gap is that none projects the *alternation of node kinds* into a
single discriminated Rust **enum**.

**Mechanism & the crux gate.** `ExprKind` (10-arm) and `BackendExpr` (13-arm) are **sums**; the schema
IR is **Struct-only** (`direct_schema.rs:52`, confirmed live). So the IR-as-grammar can express only
the *product* subset today — and **the directive binds: the product-only, sum-excluding subset does
NOT stand for the formulation** (ten independent structs is not the IR; it has lost the algebra).
- **GATE-1 · `DirectTypeKind::Sum { variants, unknown }`** beside the Struct arm; `validate()` grows a
  sum arm (payloads resolve, tags duplicate-free); `json_typed_direct::render` grows `render_sum_arm`
  emitting a Rust `enum` + tag dispatch. **Owner: GN-3.W1** — which lands `DirectTypeKind::Sum` for
  `value = object|array|string|number|bool|null` **anyway**; that is *structurally the same construct*
  `expr = seq|alt|…` needs. 3C is the general consumer of the sum node GN-3 introduces for JSON —
  precisely how a substrate earns a consumer under Y.13. **A BOUNDED primitive, not equal-difficulty.**
- **GATE-2 · thread `Project.{target,ty}`@204 into the schema.** Owner: GN-3.W2 (= the §0 fact-2 wire).
  3C reads the threaded leaf as **sum-variant payloads**; because variants carry *declared* payloads,
  `derive_materialization_roles`@1403 is **RETIRED, not relocated** — homoiousia dissolves the
  heuristic taxonomy by making it *declared*. This is 3C's distinctive lever over 3B's floor.
- **GATE-3 · the E3 rewrite engine** (the transposition half). Recasting `materialize_rule`'s
  imperative splice as a declarative `expr→backend_expr` grammar-to-grammar rewrite needs an engine to
  consume it. Two born-RED routes: (a) **LOCK-2**'s IR-rewrite engine (2B/2C) reproduces the splice
  byte-identically; (b) **LOCK-7**'s meta-circular oracle consumes it as a differential check. **This
  primitive is "the transposition spine one meta-level up" — of equal-or-greater difficulty.** It
  blocks the transposition half ONLY; the node-algebra half (GATE-1/2) stands without it.

**The named consumer (Y.13).** Pass-2 STRENGTHENS the consumer story: (1) the generator's own IR
`Serialize/Deserialize` codec (today `#[derive]` at `ir:226/233/375`) is replaced by grammar-generated
definitions + a grammar-generated JSON codec — the IR becomes just another typed-JSON document; **but
no live regen site serde-round-trips `BackendIr`**, so this consumer is *compiled-but-unexercised*
(born-RED, owner GESTALT-B/LOCK-7). (2) **LOCK-7 · 7B TOTAL-SELF-HOST names the consumer verbatim** in
the constitution — "*bbnf grammar, regex grammar, **IR grammar**, emitter templates*." (3) GESTALT-B
weaves LOCK-1/2/3/7 and reads the IR grammar as the shared substrate. So: the node-algebra half has a
same-wave consumer via GATE-1's render arm; the transposition half is a born-RED edge owned by
LOCK-2/7. **No consumer-less substrate ships** — but the strongest consumer is a *design* consumer, not
yet a live loop.

**Integration seams.** `direct_schema.rs:52` (Struct-only, GATE-1) + `json_typed_direct.rs:74,313,435`
(`render_sum_arm` absent); `codegen/src/lib.rs:130` `emit_typed_from_source` (the organ the IR grammar
rides); `xtask/src/real_typed_schema.rs` (**1014 LOC hand file — the anti-pattern 3C dissolves**: a
human writing beside the grammar what the grammar should *be*); `ir:234-271/376-413` (the algebras to
generate); the self-host precedent `grammar/bbnf/*.bbnf`. Prior art: **MLIR ODS** (the closest peer —
declarative `.td` op-definitions generated to C++, "adding an op is data not code"); **tree-sitter
`node-types.json`** (a shipped sum-schema-of-node-kinds from a grammar — direct precedent for GATE-1);
LLVM TableGen ISel patterns (declarative grammar-to-grammar rewrites → matcher table — precedent AND
the risk lesson for GATE-3); nanopass `define-language`; GHC "Trees That Grow."

**Named risks.** R-1 **product-only mirage** (guard: acceptance asserts a top `DirectTypeKind::Sum`
with exactly 10/13 variants; Struct-only emit is RED). R-2 **arena recursion** — live IR is
`Seq(Vec<ExprId>)`, `ExprId=u32` index, NOT `Vec<Box<Expr>>`; the grammar must project `ExprId` as a
scalar (an **arena schema**); any boxed child fails closed. R-4 **generic-JSON stays heuristic** —
`json.bbnf` has zero `->`; homoiousia of the emit transposition is reachable only for grammars that
declare their `->` (the IR grammar is itself typed, so this does not block the node-algebra half, but
forbids claiming the generic-JSON path).

**Open questions.** Q-3C-1 — the product-only scoping probe (emit `Rule`/`Expr`/`DirectBuild*`
byte-identical) is STILL UNRUN; the family is SPEC-ONLY this pass and wears the label. Q-3C-2 — does
GN-3.W1's `Sum` construct, designed for JSON `value`, actually suffice for `ExprKind`'s arena-scalar
variants unchanged, or does it need an arena-ref extension? Q-3C-3 — is the compiled-but-unexercised
serde-codec consumer enough to satisfy Y.13, or must a live regen round-trip exist first?

**Disposition: BLOCKED-candidate** (converts to ADVANCED on unblock). Its foundational gate is one
schema construct externally owned but BOUNDED (`DirectTypeKind::Sum`, GN-3.W1); its transposition half
needs a primitive of equal difficulty (the rewrite engine, LOCK-2/7); its consumer is design-strong,
live-weak. Bank the gift: **3C is the sternest possible consumer for the type plane and the bootstrap
gate.** Hold BLOCKED — do NOT author `ir.bbnf` until GN-3.W1 lands `Sum` and a consuming wave is named
(both born-RED dependency edges).

---

## FAMILY 3D — STAGED-RESIDUAL · BANKED (re-cut to the live residual) · registry-floor 38%

**Architecture.** The IR is an explicitly two-stage program: each node carries a binding-time fact
`Stage ∈ {Comptime, Runtime}` (a two-point join-semilattice, Runtime ⊒ Comptime, joined by the same
monotone `Changed`-flag fixpoint as `nullability` — NOT a structural hash); **residualization is the
erasure of the Stage-0 sub-graph.** Architecturally this **is a 3B move** — `Stage` is one more
attribute slot on the single IR; the residualizer is a split leaf emitter, not a transposition. Per
the pass-1 redirect, the **separate-staging-spine formulation is BANKED**; its subsumed stage attribute
lands in 3B's `Attributes` store. What survives for independent evaluation is the **strong unification**
thesis: *type-plane erasure = stage-0 erasure applied to the type sub-plane.*

**Two S0 islands on live.** (1a) The CSS stop-set: `ScanAlphabet::derive_alphabet` →
`render_alphabet_consts` → `const SKIP_ALPHABET/AT_RULE_STOP/…` (`css_scan_direct/mod.rs:112-155`) —
**already ships** as a first-Futamura residual; 3D **declares** it, does not re-author it (registry
directive). (1b) The JSON first-byte dispatch: `json_sink_direct/dispatch.rs:31-59` walks the `value`
rule's `Alt{Dispatch}` first-byte keys (Stage-0, grammar-derived) but emits a **naive `match` in three
un-unified monomorphs** — the un-residualized NEW site the directive names.

**The re-cut (registry directive 3D-1/2).** The PROVE-12-`u128`-byte-identity flagship is **RETIRED**
(named negative: `find_component_delim` gone from the emit path; byte-identity fails structurally). The
thesis re-homes to the live residual (`const &[u8]`, not `u128`) and to the **one 3D contribution not
already shipped as `ScanAlphabet`**: threading `:204` as an S0 residual **to a Rust *type***. The
shared `->` question answered in staged terms — `ty: Option<String>` is comptime data whose *sink is a
Rust type*, erased at generation into a `DirectBuildField.target`, contributing zero runtime
instructions (Constitution v, by construction). GN-3.W2 (thread `Project` → `target`, kill the 4-way
`ProjectionKind` fold at `general_descent/view.rs:20-46`) **IS the type-plane instance of the
staged-residual mechanism** — 3D subsumes it as a corollary, not a parallel effort.

**Mechanism.** One pass `binding_times(&GrammarIr) -> StageFacts` (a side-table keyed by
`ExprId`/`RuleId`, colocated with `CostFacts` in `ir/cost.rs`). Rules: a fully-known `ByteLiteral`/
regex-`terminal_bytes` node = Comptime; an operand consumed only to select among Comptime alternatives
(a STOP set, a dispatch key set) = Comptime → emit as `const`; `source[pos]`-dependent = Runtime; a
`Project.ty` node = Comptime-on-the-type-sub-plane. Residualization: *every maximal Comptime sub-graph
is const-emitted; the Runtime frontier is the emitted code.* Prior art: Jones-Gomard-Sestoft offline
BTA + Futamura projection 1 (the generator as partial evaluator); MetaOCaml/LMS `Rep[T]` (the cleanest
match for "type-plane = stage-0"); Zig `comptime`.

**Integration seams.** `ir/cost.rs` (`Stage` + `StageFacts`); a `binding_times` pass in `passes/`
beside `infer`/`derive_materialization_plan`; the type residual at `:204`/`:1136`/`json_typed_direct`;
the JSON dispatch residual at `json_sink_direct/dispatch.rs`. **Dead "stage" to ABROGATE not extend
(`abrogate-before-patch`):** `BackendShape::CollapsedStage` (`ir:366`), `admits_collapsed_stage` =
`target.avx512bw && Entry(_)` (`passes:959`) — an existing per-rule "stage" fact gated on `avx512bw`
(x86, dead by refutation law iv), dormant on aarch64. 3D's `Stage` is a distinct live concept; the
CollapsedStage machinery should be **deleted**, not conflated (named risk).

**Gates it carries (all under the banked banner; reactivate to ADVANCED only if the unification is
proven concretely).**
- **G-3D-1** — `binding_times` run on the **REAL** CSS *and* JSON IR (not a synthetic mirror — pass-1's
  4/4 tests ran on a shaped mirror, vacuous); prove the delimiter island is an S0 node the live emit
  path actually exposes.
- **G-3D-2 (the distinctive contribution)** — thread `:204` as an S0 residual to a Rust type; emit it
  as `DirectBuildField.target`; `git diff crates/runtime/` **empty** (the type residual costs zero
  runtime instructions). This is the concrete proof of "type-plane = stage-0," un-done this pass.
- **G-3D-3** — if (1)-(2) land: residualize the JSON first-byte dispatch and measure a NEW-site
  **cold, paired, N≥200, PMU ins/B** win on twitter/citm/canada/bootstrap (floor −3.7…−6.4% ins/B). 3D
  is the family most prone to unmeasured plausibility (PROVE-09 law) — the prototype **must be allowed
  to return a named negative** (dispatch already residual-enough) rather than force a claim.

**Open questions.** Q-3D-1 — does *naming* the residual (a `Stage` fact) buy anything the imperative
`ScanAlphabet` emitter does not, beyond generalization to the JSON site? Q-3D-2 — **capacity hints are
corpus facts, not grammar facts** (`vec_with_capacity` 875/184/… are Stage-0 only if the typed grammar
carries `-> Vec<T> @cap(N)`; else not derivable) — decides whether type-plane byte-identity is honestly
reachable. Q-3D-3 — three un-unified dispatch monomorphs: residualizing one and not the others
reintroduces the drift the single fold is meant to kill.

**Disposition: BANKED** (stage-attribute subsumed into the 3B substrate; PROVE-12-`u128` flagship
RETIRED). The distinctive unification (G-3D-2) is **UNPROVEN**; the surviving live claim (grammar-
derived stop-set = BTA-liftable S0 island) is **already shipped** as `ScanAlphabet` under a different
name. Reactivates to ADVANCED only if G-3D-2 lands concretely on real IR.

---

## THE INCOMPATIBILITY MAP (pass-2)

The four families are **not four mutually-exclusive spines**. Pass-1 had one hard incompatibility
(Axis M) and one contested seat (Axis R); **pass-2 has dissolved the first and collapsed the second**,
leaving exactly one live incompatibility (Axis S) and one coordination tension (the seam).

### RESOLVED — Axis M (3A-tower ⊗ 3B-single-IR): the hard incompatibility DISSOLVED
Pass-1's only load-bearing incompatibility — a small fixed tower of distinct typed value levels ⊗ one
collapsed IR — is **gone at pass-2**. PROTO-1 resolved it toward the hybrid (per-pass contracts on one
IR localize the defect a typed level would); the standalone tower was BANKED; and **research-A pass-2
no longer poses a tower** — 3A's product is "a verifier lattice welded onto the edges that already
exist," which *presupposes* the 3B single-IR substrate. 3A and 3B now **compose**: one IR (3B) + a
`verify(grammar, attrs)` predicate + a per-edge differential digest per pass (3A discipline). The named
cross-pollination (registry) is exactly this graft: 3A's C1/C2/C3 are the coverage/consume/parity
contracts 3B's Q-3B-3 asks for. **No longer an incompatibility; a mandated composition.**

### COLLAPSED — Axis R (3D-forced-BTA ⊗ 3C-authored-rewrite): the contested seat
Pass-1 posed "who generalizes PROVE-12 — forced BTA (3D) or one authored grammar-rewrite (3C)?" At
pass-2 **neither theory owns the seat via a live artifact**: PROVE-12's `u128` target is retired; the
live residual (`ScanAlphabet`, `const &[u8]`) ships **imperatively**, needing neither a BTA nor a
rewrite engine. 3D adds a *fact* (`Stage`) over the imperative emit; 3C needs a *born-RED engine*
(GATE-3, equal-difficulty). The seat is therefore **not contested — it is already occupied by
imperative code**, and both families are asking to *re-describe* it, not to *win* it. The residual
question is narrow: does either re-description buy a NEW-site win (3D's JSON dispatch, G-3D-3) or a
retirement (3C's declared taxonomy, GATE-2) the imperative floor does not? That is an evaluation, not
an incompatibility.

### LIVE — Axis S (3C-grammar-substance ⊗ hand-written-enums): the one remaining incompatibility
3C alone claims the IR's *substance* should be bbnf-specified-and-generated rather than hand-written
Rust enums. This is genuinely incompatible with 3A/3B/3D's shared assumption (they all manipulate
hand-written `ExprKind`/`BackendExpr`). But it is **BLOCKED at the schema level** — `DirectTypeKind`
is Struct-only; the sum-node algebra is un-emittable until GN-3.W1 lands `Sum`. So the incompatibility
is **latent, not live-contested this pass**: 3C cannot demonstrate its substance claim until the
external primitive lands, and 3A/3B/3D proceed on hand enums meanwhile. When GN-3.W1 lands, this
becomes the real Axis-S adjudication (generate `ir/src/lib.rs` from `ir.bbnf`, or keep it hand-written)
— deferred to the moment 3C unblocks, resolved at GESTALT-B.

### The coordination tension (compatible, but must agree at ONE seam)
- **The `:204` thread is single-owner** (registry directive 1, §0 fact 2). 3A verifies it, 3B stores
  it, 3C reads it as a sum-payload, 3D erases it as an S0 residual — **all four claim the same two
  lines.** They are compatible ONLY IF the thread is owned once (a GN-3 wave) and the other three
  CONSUME. Fund it four ways and it is four re-derivations — the exact anti-pattern the CONSUME contract
  (C2) forbids. **This is the gestalt's single most important coordination obligation.**
- **The seam-dissolution is single-funded to 3B** (registry). `has_rule("value")`@98 and
  `derive_materialization_roles`@1403 are the same live hardcodes 3C would "dissolve via grammar" and
  3B would "retire via fold." The gestalt funds ONE: 3B (ADVANCED) owns it; 3C's grammar-dissolution is
  the banked alternative that reactivates only if 3C unblocks. **New at pass-2:** research splits the
  seam — `has_rule("value")` retires CLEANLY (→ `backend_shape` attribute, a fact); but
  `derive_materialization_roles` retirement is CONDITIONAL on JSON gaining `->` (else RELOCATION), a
  LOCK-7 grammar-totality obligation carried as a born-RED edge. The two hardcodes have **different
  dispositions** and must not be conflated.

### The net shape for the gestalt (unchanged in structure, sharpened in evidence)
The convergent spine remains **one IR (3B substrate) + per-pass contracts (3A discipline) + a stage
attribute with residualization (3D erasure)**, **3C banked** until GN-3.W1 (sums) + a named consuming
wave unblock it. Pass-2's contribution: the three layers now share **one identical `:204` wire** (the
single-owner gate), the two collapse-gates that earn the spine remain **UNRUN** (3B-G1 byte-identity;
the 3A differential runner), and 3D's distinctive half (type-plane = stage-0, G-3D-2) is **the un-done
thread that pass-3 must execute or 3D stays BANKED**.

---

## DISPOSITIONS (for the registry)

| Family | Disposition | Missing primitive? | One-line reason |
|---|---|---|---|
| **3A** | ADVANCED (discipline; tower BANKED) | none | Verifier lattice over the existing edges; flagship contract non-vacuous (RED on HEAD); the distinctive differential runner is fully specified but UNRUN; composes with 3B, no longer competes. |
| **3B** | ADVANCED (convergent substrate) | none (Stratum 1) | Second/third IR deletable today behind a byte-identity gate; standing laws hold by construction; the killer gate (G-3B-1) remains UNRUN. |
| **3C** | **BLOCKED-candidate** | `DirectTypeKind::Sum` (GN-3.W1, **bounded**) for the node-algebra half; the E3 rewrite engine (LOCK-2/7, **equal-difficulty**) for the transposition half; a live consumer | Node-algebra half blocked on ONE bounded schema construct GN-3 lands anyway; transposition half on an equal-difficulty primitive; consumer design-strong, live-weak. Bank, don't retire. |
| **3D** | **BANKED** (stage-attr → 3B; `u128` flagship RETIRED) | none for the kernel | Architecturally a 3B move; distinctive unification (type-plane = stage-0, G-3D-2) UNPROVEN; surviving live claim already ships as `ScanAlphabet`. Reactivates only if G-3D-2 lands concretely. |

**Convergence eligibility:** none this pass (pass 2 of ≥3). The two gates that earn the spine
(3B-G1 byte-identity collapse; the 3A differential runner + `:204` RED→GREEN re-anchor) are still
UNRUN; 3D's distinctive contribution is unproven; 3C is blocked on an external primitive. The two
sharpest uncertainties below target exactly the unrun gates.

---

## THE TWO SHARPEST UNCERTAINTIES (next-pass prototype briefs)

Returned in full via StructuredOutput. In brief:

1. **THE SINGLE-OWNER `:204` THREAD + CONSUME/PARITY GATE** (bbnf-worktree). Discharges the
   quadruple-homed gate pass-1 discharged in none: thread `ty`@204 → `node_types` →
   `DirectBuildField.target`; verify predicate RED-on-HEAD / GREEN-after on Sheets; prove the
   schema-DERIVED typed-direct output is byte-identical to the hand `real_typed_schema.rs` baseline
   (closing R6 §8b). Resolves whether a threaded node-type reproduces the hand schema and composes
   under projection chains — the CONSUME that 3A verifies, 3B stores, 3D erases.

2. **THE 3B ONE-FOLD BYTE-IDENTITY COLLAPSE (G1) + SEAM DISPOSITION** (bbnf-worktree). Runs the one
   falsifiable gate that earns the substrate: `emit(&GrammarIr,&Attributes)` for {json, sheets, typed},
   all regen/check byte-identical (CSS a declared island); `has_rule("value")`@98 GONE; name whether
   `derive_materialization_roles`@1403 RETIRES or RELOCATES (the JSON-`->` LOCK-7 edge). Resolves
   whether the second IR carries an irreproducible emit-order/collection fact (the `SinkOnlyFacts`
   caveat) — the substrate 3A-discipline and 3D-stage-attribute both layer onto.
