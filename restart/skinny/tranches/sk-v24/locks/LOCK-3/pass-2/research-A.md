# LOCK-3 · PASS-2 · FAMILY 3A — THE CONTRACT TOWER

*A small fixed IR tower whose every level is a total lowering with a machine-checked
contract, verified per-pass and localized per-edge by one differential runner over the
regen corpus. Pinned to live HEAD `c93fc43143` (the directive's `b3cb8c16b` and R6's
`7f3be27c2` are stale ancestors; every `path:line` below re-verified on c93fc43).*

---

## 0. The mechanism, in one paragraph

The live spine already IS a tower of typed transpositions with `derive(Serialize)` on every
mid node — what it lacks is a *contract per edge* and a *verifier that reads them*. 3A adds
exactly two organs and nothing structural: (1) a per-pass `verify(level)` predicate, run after
each lowering, asserting that level's well-formedness invariant (the rustc `-Zvalidate-mir` /
GHC `-dcore-lint` shape); (2) one `differential(edge, corpus)` runner that digests each
level's artifact and, on an injected mis-lowering, names the edge whose digest first diverged.
The contracts are *consumers* (Constitution viii, Y.13): the L0→L1 contract's whole content is
"the `->` node-type threaded at `passes/src/lib.rs:204` reaches its consumer" — so 3A's answer
to the Shared question is that threading is not a feature added for its own sake, it is the
payload the tower's first contract exists to witness.

---

## 1. The labeled edge-DAG on live (the enumeration the directive asks for)

Traced by opening every hop. **Five** transposition edges, **four** distinct value nodes,
plus a schema side-branch and a name-gated 3-way emit fan. "Three levels" is achievable only
by collapsing E4 into E5 as a proven pure projection.

```
 SOURCE .bbnf
   │  E1  grammar::parse_grammar                       (crate grammar)
   ▼
 GrammarIr  { ExprKind 10-arm: Seq Alt Repeat Optional Literal Regex Ref
   │          Annotation Project Discard }              ir/src/lib.rs:185 (Project eval)
   │
   ├─ E2  Infer::infer_expr → facts.node_types: HashMap<ExprId,Type>
   │        passes/src/lib.rs:184-213  Type 7-arm (:234)  BuiltinTy 7-arm (:245)
   │        ***Project@204 returns infer_expr(body); the declared ty is dropped via `..`***
   │        node_types is a SIDE table — today NO value-shaping consumer reads it
   │
   ▼  E3  passes lower_expr (:1340) + materialize_rule (:1362)
 BackendIr { BackendExpr 13-arm (ir:376): …ValueProject{body,target,ty}(:407),
   │          SpanMark(:396) TapeEmit{TapeKind 9-arm}(:400/:453) DirectBuild(:403) Return
   │          BackendShape 5-arm (ir:361) }
   │        materialize_rule splices  Seq[ SpanMark(Start), body, SpanMark(End),
   │          TapeEmit{kind}, DirectBuild{shape,fields}, Return ]  — the emit-order framing
   │        fed by MaterializationPlan (:1139); DirectBuildTarget = None @:1157 (ALWAYS)
   │
   ▼  E4  codegen lower/sink_only.rs:126 lower_program
 SinkOnlyProgram { entry_rule, rules:SinkOnlyExpr, direct_shapes:BTreeSet,
   │                dispatch_alt_count, span_kinds, literals, policy_summary }  (:20)
   │        re-walks BackendExpr → SinkOnlyExpr; recomputes shapes it already had
   │
   ▼  E5  codegen runtime_generator::emit_sink_only  (lib.rs:210)
 RUST SOURCE ── name-gate has_rule("value") @runtime_generator.rs:98 fans to:
      • JSON offset-tape templates (include_str! json_templates/*)   [value rule]
      • json_sink_direct::render (eager sink)                        [value rule]
      • general_descent::render (Sheets/CSV lazy)                    [no value rule]
   plus SCHEMA BRANCH:  emit_typed_from_source (lib.rs:130) →
      schema_direct::lower_program(BackendIr, DirectSchemaSet) → json_typed_direct::render
      — DirectSchemaSet is HAND-authored (xtask/real_typed_schema.rs), not derived.
```

**Reconciliation (materialize_rule vs SinkOnlyProgram).** The SpanMark/TapeEmit/DirectBuild/
Return quintuple is *not new data* — it is an emit-order encoding of the `MaterializationPlan`
descriptor (label, kind, shape, fields) laid inline into a `BackendExpr::Seq` so E5 can walk
it linearly. `SinkOnlyProgram` (E4) then re-extracts `direct_shapes`/`dispatch_alt_count` from
that Seq — a **recomputation**, not a new fact. E4 is therefore a collapse candidate: it
carries nothing absent from `BackendIr`, which makes the honest tower **three value levels** —
L0 Grammar(+types), L1 Backend(shaped), L2 Emit — with E4 folded into L2 as a derivation. 3A
does not *require* the fold; it requires E4 satisfy a "pure projection of L1" contract, the
checkable form of the same claim (§7.3).

**"How many distinct value levels actually exist": 4 nodes / 5 edges, reducible to 3** by
proving E4 ≡ projection(L1). `TapeKind` carries 9 variants but the live JSON corpus exercises
7 (`Member`, `Element` unused — R6 §5c) → two born-zero-witness constructs the runner flags.

---

## 2. The Shared question: threading `->` from :204 to its consumer, and 3A's *consumption*

**HEAD state (RED).** `passes/src/lib.rs:204` `Project { body, .. } => self.infer_expr(*body)`
stores the *body's* structural `Type` into `node_types` (:212). The declared projection tag
`ExprKind::Project.ty: Option<String>` (`ir/src/lib.rs:185`) never enters `node_types`. It
*does* survive syntactically to `BackendExpr::ValueProject.ty` (`passes:1349-1352`, verbatim
clone) — but the type plane never sees it, and the *value-shaping* consumer
`derive_materialization_plan` sets `DirectBuildTarget = None` unconditionally (`:1157`),
re-deriving shape from byte-literal heuristics (`derive_materialization_roles` :1403).

**The named live consumer.** The directive's `TypeAnalysis::from_ir` (`backend/driver/
analysis.rs`, XL-F) **does not exist in the live skinny tree** (`grep -rn TypeAnalysis` → 0;
`grep 'fn from_ir'` → 0). It is a term from a pre-restart/greenfield surface. On live the
*actual* downstream type consumers are `passes::shapes::derive_shape_facts`
(`passes:259-299`, today consuming only `MaterializationPlan.descriptors.kind`, never
`node_types`) and `passes::extract::derive_materialization_plan` (`:1139`, the
`DirectBuildTarget` populator). **3A binds its L0→L1 contract to these two and names
`TypeAnalysis::from_ir` a greenfield rename to reconcile at GN-3, not a live seam** (else the
contract would bind a phantom — Constitution vii).

**3A CONSUMES, it does not re-derive (the directive's demand).** End-to-end thread:
1. Give `Type` a projection carrier — a `facts.projection_types: HashMap<ExprId,ProjectionTy>`
   side-table (safest: zero effect on the existing 7-arm `Type` and its users) or a new
   `Type::Projected` arm (`passes:234`). Populate it at :204 instead of discarding.
2. `derive_materialization_plan` (:1139) reads `facts.projection_types[expr]` and fills
   `DirectBuildField.target = Some(DirectBuildTarget{ type_ref, presence, cardinality,
   representation, decode })` (`ir:482`) — the schema struct that already exists and is today
   fed by hand. `type_ref` from the *threaded* `ty`; `cardinality` from `Repeat`/`Optional`
   already in `node_types`; `decode` from `SpanKind`. This single site turns the discarded tag
   into a consumed fact.
3. `derive_direct_schema(grammar, plan) -> DirectSchemaSet` (R6 §8b) reads those targets and
   *replaces* the hand file `xtask/real_typed_schema.rs`; proof of consumption is
   `derived == hand` byte-for-byte (§8, T3).

The contract "L0→L1 is sound" is *exactly* "`DirectBuildTarget.is_some()` ⟺ the Project
carried `ty` at :204, and its `type_ref` equals the threaded tag" — unsatisfiable by the
heuristic re-derivation, which never sees `ty`. That is how 3A's contract *forces* consumption
over re-derivation.

---

## 3. The per-pass `verify` predicate over Project@204 (RED→GREEN, zero emit footprint)

`verify_L0(grammar, facts) -> Result<(), Vec<Span>>`:

```
for expr in grammar.exprs_reachable_from_entry():
    facts.node_types.contains_key(expr)                     // totality (no silent Epsilon)
    if let ExprKind::Project { ty: Some(_), .. } = expr.kind:
        facts.projection_types.get(expr)                    // ← the gated clause
            .is_some_and(|p| p.declared == expr.ty)         // consumed, equal to source tag
```

- **RED on HEAD**: the gated clause fails for every Sheets `->` node (Sheets is the sole
  grammar exercising projections — R6 §0; `json.bbnf` has zero `->`). It *cannot* pass because
  `node_types` provably never stored the tag. Evidence = the predicate returning
  `Err(span_of_each_project)` on the Sheets IR.
- **GREEN once ty threads**: after §2 step 1 the clause reads the populated carrier; same
  predicate returns `Ok(())` on the same IR.
- **Zero emit footprint**: the thread populates a *new* field; E3/E5 still read the old
  `ValueProject.ty` path until GN-3 wires the consumer. So `cargo xtask
  {regen,check}-{json,css-l4-declaration-values,sheets}` is **byte-identical** with the
  predicate compiled in vs out — the third RED→GREEN leg and the discriminator between
  "threaded a fact" (footprint 0) and "changed behavior" (footprint ≠ 0). The byte-identity
  gate already exists (`xtask/src/regen.rs:36` `check_target`); 3A reuses it as the no-side-
  effect witness. This is the LLVM `-verify-each` / MIR-validate discipline: a pure observer,
  RED before the obligation is met, GREEN after, with a determinism gate proving nothing moved.

---

## 4. The `differential(edge, corpus)` runner (generalizing the two live oracles)

**The two live oracles it subsumes.**
- `bbnf-bench/src/parity.rs:23` `assert_parity`: track1 vs track2 by (a) offset-stream
  equality (`tape().offsets()/flag_cursors()/flag_values()`), (b) payload-counter == 0 (lazy-
  projection invariant), (c) canonical-string equality. — the **E5/runtime** edge oracle.
- `bbnf-bench/src/direct_struct.rs:41` `JsonDirectDigest` 4-way `{track1, track2, serde,
  sonic}` `Mismatch`: a commutative fingerprint (`mix`/`hash_bytes`) plus counters
  `{max_depth, strings, string_bytes, numbers, bools}`. — the **direct-to-struct** oracle.

**The generalization.** One runner: `differential(edge: EdgeId, corpus) -> EdgeReport`, with a
`digest(artifact)` per level:
- **L0** `digest(GrammarIr, facts)` — the IR's `derive(Serialize)` canonical form + an order-
  stable BTree fold over `node_types`/`projection_types`.
- **L1** `digest(BackendIr)` — `BackendIr` already derives `Serialize/Deserialize` (`ir:369`);
  hash the canonical serde form **plus** a per-variant histogram over `BackendExpr`/`TapeKind`
  (a dropped `DirectBuild` moves the histogram, not just an opaque hash).
- **L2** `digest(EmittedSource)` — the existing regen byte-diff (`check_target`) **and** the
  runtime `JsonDirectDigest` + `assert_parity` triple. L2 *is* the two live oracles; the runner
  reproduces their digests bit-for-bit by *calling the same functions* (the directive's
  "reproduce those digests").

**Localizing an injected mis-lowering** — report the first edge whose digest diverges:
- drop the `DirectBuild` node in `materialize_rule` → L0 identical, **L1 histogram diverges**
  (DirectBuild −1) → E3.
- corrupt a hunk in `json_templates/view.rs` → L0,L1 identical, **L2 regen byte-diff + runtime
  digest diverge** → E5.
- drop `ty` at :204 (the RED state) → L0 `projection_types` histogram diverges, L1/L2 unchanged
  → E2, *and* proves E2's fact is currently unconsumed (a diagnostic the tower surfaces).

**Coverage metric & zero-witness.** Coverage = per-edge construct coverage: fraction of each
level's variant alphabet (`ExprKind` 10, `BackendExpr` 13, `TapeKind` 9, `BackendShape` 5)
instantiated by the corpus. A variant with **no** witness (`TapeKind::Member/Element`, `SimdMode`
≠ `Exact`, `BackendShape::EventTape`) is reported **UNVERIFIED**, categorically distinct from
**SOUND** — the runner never certifies a construct it never saw (the translation-validation
honesty line: validate the runs you have, name the ones you don't). Soundness-claim strength =
Σ verified-variant-weight; the report ships the unverified set as named debt.

---

## 5. Substrate & integration seams (path:line, all live c93fc43)

| Organ | Live seam | 3A use |
|---|---|---|
| L0 carrier | `passes/src/lib.rs:204` (drop), `:212` (`node_types.insert`), `Type` `:234` | add `projection_types` side-table / `Type::Projected` |
| L0 verify | `passes` infer module (new `verify_L0`) | per-pass predicate §3 |
| L1 node | `ir/src/lib.rs:376` `BackendExpr` (Serialize), `:453` `TapeKind`, `:482` `DirectBuildTarget`, `:492` `DirectBuildTypeRef` | L1 digest + target populate |
| L1 splice | `passes:1349` (`ValueProject`), `:1362` `materialize_rule`, `:1139` `derive_materialization_plan`, `:1157` (`target:None`) | consume threaded ty here |
| heuristic to retire | `passes:1403` `derive_materialization_roles`, `:277-291` shape fallbacks, `general_descent/view.rs:20-46` `classify_projection` | replaced by consumed types (contract forbids re-derive) |
| E4 fold candidate | `codegen/src/lower/sink_only.rs:20/:126` `SinkOnlyProgram`/`lower_program` | prove ≡ projection(L1) |
| name-gate | `codegen/src/runtime_generator.rs:98` `has_rule("value")` | **blocks L1→L2 totality** (§7.1) |
| schema branch | `codegen/src/lib.rs:130` `emit_typed_from_source`, `schema_direct::lower_program`, `direct_schema.rs`, hand `xtask/real_typed_schema.rs` | `derive_direct_schema` replaces hand file |
| L2 / runtime oracle | `bbnf-bench/src/parity.rs:23`, `direct_struct.rs:41` | terminal-edge digest, reproduced verbatim |
| regen byte-gate | `xtask/src/regen.rs:36` `check_target`; targets `main.rs:39-52` | zero-footprint witness + L2 divergence |

---

## 6. Prior art (the contract-tower is a settled discipline; the citations)

- **Nanopass framework** (Sarkar-Waddell-Dybvig, ICFP'04; Keep-Dybvig, ICFP'13): many tiny
  passes, **each with a formally specified source/target language**, and the framework
  *auto-generates* a checker that a pass's output inhabits its declared language — 3A's exact
  thesis (per-edge language contract, machine-verified), validated at production scale (Chez).
- **rustc MIR validator** (`rustc_mir_transform::validate`, `-Zvalidate-mir`): a `MirPass` run
  *after each MIR pass* asserting well-formedness — §3's per-pass predicate in a production
  tower (HIR→THIR→MIR→LLVM).
- **GHC Core Lint** (`-dcore-lint`): re-type-checks Core after every Core-to-Core pass — the
  canonical cheap total verifier; models "RED before the invariant holds."
- **LLVM `verifyModule` + `-verify-each`**: the IR verifier runnable after every pass; the
  "zero emit footprint" analogue is that the verifier mutates nothing.
- **MLIR op/region verifiers + dialect-conversion legality** (`ConversionTarget` legal/illegal
  ops): per-op contracts, *legality as a checkable relation* — the model for the L1 shape-legal
  contract (every `->` reaches a lawful `DirectBuild`).
- **Translation validation** (Pnueli-Siegel-Singerman'98; Necula'00; CompCert per-pass
  simulation): validate *this run* rather than prove the pass — `differential(edge, corpus)` is
  translation validation specialized to the regen corpus, coverage metric as its honesty bound.
- **egg/egglog explanations** (Willsey et al., POPL'21): proof-producing rewrites — the optional
  witness form if an edge is ever an e-graph rewrite (LOCK-2 boundary), not required by 3A.

---

## 7. Named risks & counterexamples

1. **The name-gate breaks the L1→L2 total-function contract.** `has_rule("value")@98` makes
   emission a function of *the rule name*, not of `BackendIr` alone. Counterexample: two
   grammars whose `value`-named rules differ in shape both route to JSON templates; a third
   with identical shape but a rule named `document` routes to general-descent — so `emit` is not
   determined by L1. **3A's L2 contract is FALSE on HEAD until the name-gate is retired (not
   relocated).** PROVE-13's structural predicate `dispatch_alt_count > 0 && direct_shapes.len()
   >= 2` (both fields already on `SinkOnlyProgram`, `sink_only.rs:23/:26`) is the cure and is a
   **GN-1.W4 landing, absent on c93fc43**. 3A carries this as a born-RED dependency edge owned
   by GN-1.W4; L2 goes GREEN only after the gate dies.
2. **Digest coarseness masks reorderings.** `JsonDirectDigest` counters are commutative;
   swapping two sibling members can leave counters (and, on some inputs, the mix-fingerprint)
   invariant. The L1 histogram inherits this. Mitigation: the L1 digest must include a
   *position-sensitive* canonical serde byte-hash — histogram localizes *which variant*, byte-
   hash localizes *whether at all*. A digest that can't distinguish sibling swaps is a coverage
   hole the metric must not score SOUND.
3. **E4 is not provably a pure projection until measured.** If `SinkOnlyProgram` reads a
   `LowerCtx` policy fact absent from `BackendIr` (`policy_summary`, `same_substrate_union`),
   the "three-level" collapse fails and the honest tower is four. The prototype must trace E4's
   input set (T2) before any "three levels" claim ships.
4. **Zero-witness overclaim.** Sheets is the *only* projection witness; JSON exercises no `->`.
   Proving L0→L1 soundness on the JSON corpus proves nothing about the type thread. The runner
   must refuse to certify the projection contract off a corpus with zero Project witnesses.
5. **Phantom-seam binding.** Binding a contract to `TypeAnalysis::from_ir` (nonexistent on live)
   would be a spec-only substrate. 3A binds the real `derive_shape_facts`/
   `derive_materialization_plan` and names the rename as a GN-3 reconcile.

---

## 8. What a prototype must test to move 3A (the falsifiable set)

- **T1 — RED→GREEN verify + zero footprint.** Thread `ty` at `passes:204` into
  `projection_types`; implement `verify_L0`. Show `Err` on the Sheets IR at HEAD, `Ok` after
  the thread, **and** `regen/check` of json+css+sheets byte-identical with the predicate in vs
  out. *Pass = executed RED→GREEN + 3 byte-identical regens.* (§3)
- **T2 — the differential runner localizes.** Implement `digest(L0/L1/L2)` reusing the IR serde
  + `parity.rs`/`direct_struct.rs`. Inject three mis-lowerings (drop DirectBuild @E3; corrupt
  template @E5; drop ty @E2) and show the runner names the correct edge each; emit the coverage
  report flagging `TapeKind::Member/Element` and `SimdMode`≠`Exact` UNVERIFIED; reproduce the
  live track1/track2/serde/sonic digests as the L2 instance bit-for-bit; trace E4's input set to
  adjudicate 3-vs-4 levels. *Pass = 3×3 localization matrix + reproduced digests + coverage
  table.* (§4, §7.3)
- **T3 — consumption, not re-derivation.** Wire `projection_types` into
  `derive_materialization_plan` (`:1157` `target: Some(...)`), author `derive_direct_schema`,
  show the derived `DirectSchemaSet` equals hand `xtask/real_typed_schema.rs` byte-for-byte,
  then delete the hand file. *Pass = derived ≡ hand + hand file removed + typed emit regen
  byte-identical.* The single proof the tower's first contract is a *consumer* (Y.13),
  discharging the path toward GN-3.W2. (§2)

Run against a corpus of Sheets (projection witnesses) + JSON + CSS, these three move 3A from
"the tower is drawn" to "its contracts are executed, its verifier reads RED then GREEN, and its
runner names the edge."
