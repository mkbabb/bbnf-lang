# LOCK-3 · PASS-2 · Family 3A — THE CONTRACT TOWER

*A small fixed tower, each lowering a total function with a checkable contract; round-trip /
parity oracles per level, CI-enforced.*

Pinned to live `skinny@061c79fa7` (the observed workspace HEAD; the directive named
`b3cb8c16b`, absent from this tree — every pointer below re-verified at 061c79fa7). Designs
target greenfield (GN-1 tree); ports scope organ-level.

---

## 0. The live spine, enumerated as an edge-DAG (the directive's first evidence demand)

Walking `codegen::emit_from_source` (`codegen/src/lib.rs:101`) end to end, the transposition
spine is **six labeled edges over four distinct value levels** — not three, and the gap between
"four observed" and "three fixed" is precisely what this family must adjudicate.

| Edge | Transform | Site | Value produced |
|---|---|---|---|
| **E1** parse | `grammar::parse_grammar` | `codegen/src/lib.rs:102` | `GrammarIr` (`ExprKind` 10-arm sum, `ir/src/lib.rs`) |
| **E2** infer | `passes::Infer::infer_expr` | `passes/src/lib.rs:170-213` | `Type` (7-arm) stored per-`ExprId` in `facts.node_types` (`:212`) |
| **E3** lower | `lower_expr` | `passes/src/lib.rs:1349` (`Project→ValueProject`) | `BackendExpr` (13-arm, `ir/src/lib.rs:388-413`) |
| **E4** materialize | `materialize_rule` | `passes/src/lib.rs:1362` | *same* `BackendExpr`, splicing `SpanMark(Start)·body·SpanMark(End)·TapeEmit{kind}·DirectBuild{shape,fields}·Return` |
| **E5** sink-lower | `lower::sink_only` | `codegen/src/lower/sink_only.rs` | `SinkOnlyProgram` (`SinkOnlyExpr` mirrors E4's `SpanMark/TapeEmit/DirectBuild`) |
| **E6** emit | `runtime_generator::emit_sink_only` | `runtime_generator.rs:89-160` | `EmittedSource` (Rust text) via 3 routes |

**The four levels are L0 `GrammarIr`+`node_types`, L1 `BackendExpr`, L2 `SinkOnlyProgram`, L3
emitted Rust.** The `SpanMark/TapeEmit/DirectBuild/Return` framing the directive asks to
reconcile is **not a level** — E4 is a *rewrite within L1* (`materialize_rule` returns
`BackendExpr`, `:1366`), keyed by `MaterializationPlan.descriptors.get(&rule_id)` (`:1367`).
`SinkOnlyProgram` (L2) then **re-encodes those same three node kinds** near-isomorphically
(`SinkOnlyExpr::{SpanMark,TapeEmit,DirectBuild}`, `general_descent/view.rs:77-79`). So L2 is a
*redundant transposition* of the materialized L1 — it carries no value L1 lacks; it exists only
to strip `ValueProject`/`Ref`-target detail the emitter does not read. **This is the tower's
central design fact: the live spine has one honest structural level (L0→L1) and one
serialization re-encoding (L1→L2) that a three-level contract tower should either collapse or
justify by a checkable difference.** The disciplined three-level tower is therefore **L0 typed
HIR → L1 materialized mid-IR (BackendShape as attributes) → L3 emission**, with L2 collapsed
into L1's serialize step unless a contract distinguishes them.

**Distinct value planes crossing these edges:** the *structural* plane (`Type` 7-arm, always
present), the *materialization* plane (`TapeKind` 9-arm + `DirectBuild` shape/fields), and the
*type* plane (`DirectBuildTarget.type_ref`, `ir/src/lib.rs:485` — **the honest full type IR,
`target: Option<_>` at `:470`, populated as `None` on every live descriptor**,
`passes/src/lib.rs:1149-1287`). The type plane is the one the `->` value never reaches (§2).

---

## 1. The mechanism: contracts as total functions with per-level oracles

A **contract** is a decidable predicate `C_k(in_k, out_k)` attached to each lowering edge,
run at regen and in CI. Three contracts, each with an executable oracle already resident in the
tree:

- **`C_{L0→L1}` (structural + type totality).** Every `ExprId` that inference visited has a
  `node_types` entry (`facts.node_types`, `:212`); every rule the role classifier admits
  (`derive_materialization_roles`, `:1403`) maps to exactly one `MaterializationDescriptor`, and
  every `->` node's threaded type equals the `DirectBuildTarget.type_ref` its rule emits
  (§2's identity). Oracle: a walk asserting (a) no `node_types` gap, (b) no unclassified rule
  reaching `materialize_rule` (the `descriptors.get` `None`-fallthrough at `:1367-1369` is the
  *un-contracted partial function today* — it silently returns `body` for any rule the heuristic
  missed; the contract makes that RED).
- **`C_{L1→L2}` (serialize faithfulness).** `SinkOnlyExpr` preserves the `SpanMark/TapeEmit/
  DirectBuild` sequence and their labels/kinds. Oracle: structural equality of the two node
  streams (cheap; this is the contract that *justifies or condemns* L2 as a level).
- **`C_{L2→L3}` (emission determinism/parity).** Oracle **already shipped**:
  `xtask::regen::check_target` (`xtask/src/regen.rs:37`) — regen byte-identity per grammar
  (`check-json`/`check-css-l4-declaration-values`/`check-sheets`), plus the semantic parity
  digests (§3). This is the CONTRACT-TOWER's strongest existing leg.

The tower is **CI-enforced** by wiring `--verify-each` into the `xtask` regen dispatch
(`xtask/src/main.rs:39-52`): after each edge, run `C_k`; a violation fails the wave. This is the
MLIR `mlir-opt --verify-each` / GHC `-dcore-lint` / rustc `-Zvalidate-mir` discipline (§4).

---

## 2. The `->` type value at the infer edge — the shared question, answered for a contract

`ExprKind::Project { body, .. }` at `passes/src/lib.rs:204` returns `self.infer_expr(*body)?`,
dropping `target: String` and `ty: Option<String>` (verified: the `..` binds both). Threading it
end-to-end on live requires exactly three edits, and **the contract's job is to consume the one
threaded node-type, never re-derive it**:

1. **Produce (E2).** Replace the `:204` arm with one that resolves `ty` (a bare name) — and
   `target` when `ty` is `None` — to a `Type`, storing it at `facts.node_types[project_expr_id]`
   (`:212`) while still inferring `body` for its structural children (no silent epsilon: an
   unresolvable name is an explicit `PassError`, not a fallthrough). Resolution is the same
   name→leaf map `regex_type` already models (`:224-230`).
2. **Carry (E4).** `derive_materialization_plan` (`:1136-1287`) reads
   `facts.node_types[project_expr_id]` and fills `DirectBuildField.target =
   Some(DirectBuildTarget { type_ref, … })` (was hardcoded `None`, `:1149-1287`).
3. **Consume (contract oracle).** The `C_{L0→L1}` predicate asserts the **identity**
   `node_types[project_expr_id] ≡ DirectBuildTarget.type_ref` for that rule — producer equals
   consumer. It reads the threaded node-type *once* and checks equality; it does not
   independently re-derive the projection type. That single equality **is** the family's
   consumption contract.

**Crucial correction for the directive's XL-F pointer.** `TypeAnalysis::from_ir` /
`backend/driver/analysis.rs` **do not exist in live skinny** — they survive only in the
*pre-restart outer tree* `crates/core/src/backend/driver/analysis.rs:22`, where the shape is
`rule_types: Vec<(RuleId, TypeDesc)>` (a **per-`RuleId`** type table). That relic is the *design
template for the contract's type-level key*, not a live consumer. On live, the sole consumer of a
threaded `->` type is the `DirectBuildTarget.type_ref` population feeding
`json_typed_direct::render` (`codegen/src/json_typed_direct.rs`, the working direct-to-struct
emitter, 1245 LOC). So the type-level contract is **born-RED with a named owner: GN-3.W2** —
which is precisely the edit above (`docs/tranches/sk-v24/GN-3.md §W2`). Family 3A supplies the
*checkable contract* that GN-3.W2's thread must satisfy; it does not itself land the thread.

**Producer key vs consumer key (the directive's per-`RuleId`/per-`ExprId` question).** The type
is *produced* per-`ExprId` (`node_types.insert(expr_id, …)`, `:212`) and *consumed* per-`RuleId`
(the materialization descriptor and `TypeAnalysis::from_ir`'s `Vec<(RuleId, TypeDesc)>` are both
rule-keyed; `materialize_rule` keys on `rule_id`, `:1367`). The contract therefore lives at the
**rule boundary**: it collapses the per-`ExprId` type of a rule's root projection into the
per-`RuleId` `DirectBuildTarget`. A per-`RuleId` key **suffices for the whole fleet**
(json/css/sheets each project at rule roots); no fleet grammar needs a per-`ExprId` *consumer*
key — the per-`ExprId` map is producer-internal.

---

## 3. The `verify` predicate and the `differential(edge, corpus)` runner (the two hard evidences)

**The Project@204 verify predicate — RED→GREEN, zero emit footprint.** Wire `C_{L0→L1}` as a
`debug_assert`-grade predicate over the `Project` node: on HEAD it reads **RED** because
`node_types[project]` holds the *body* type (not the projected `ty`) and every
`DirectBuildField.target` is `None` — the producer≡consumer identity is false (LHS = body type,
RHS = absent). Once §2's thread lands it reads **GREEN**. The predicate touches no emitter, so
its footprint is provably zero: `check-json`/`check-css`/`check-sheets` regen **byte-identical
with the predicate on or off** (it reads `facts`, writes nothing). This is the executed
RED→GREEN transition the directive wants, and its zero-footprint proof is the standing
regen-byte-diff gate (`xtask/src/regen.rs:37`). Caveat: JSON is a **zero-witness corpus** for
this predicate — `json.bbnf` has no `->` (R6), so the *only* live witnesses are sheets (4
projections) and GN-3.W3's typed grammars; the predicate is **UNVERIFIED (vacuously green)** on
JSON and must report so, never claim soundness there.

**The `differential(edge, corpus)` runner — generalizing the shipped digests.** The substrate
exists: `bbnf-bench/src/direct_struct.rs` carries four parallel tracks over one corpus —
`track1_digest` (tape path, `:421`), `track2_digest` (typed DirectBuild path, `:428`),
`serde_digest` (`:432`), `sonic_digest` (`:436`) — reconciled by `parity_digests` via
`same_shape_as` (`:445`) and folded by `typed_checksum` (`:464`). Generalize to
`differential(edge, corpus)`:

- **(a) Reproduce.** Specialize `edge = E5/E6` (materialization→emission), `corpus = json` →
  the runner *is* `parity_digests`; the four digests are four crossings of the same edge
  (internal track1/track2 + external serde/sonic oracles). The XOR-fold checksum
  (`typed_checksum`, `:464`) and the strict product (`track2_strict_product`, `:467`) reproduce
  bit-for-bit.
- **(b) Localize an injected mis-lowering.** Inject at a *named* edge — e.g. flip a `TapeKind` in
  `materialize_rule` (`:1380`, `TapeKind::Container`→`Sequence`) or drop the `DirectBuild` splice
  (`:1383`). Because each edge has its own digest crossing, the injected fault flips exactly the
  digest(s) whose path traverses the mutated edge while upstream-edge digests still match — the
  disagreement set **localizes to the injected edge**. (This is *translation validation* à la
  Necula/Pnueli: validate each run, not the pass once.)
- **Corpus-coverage metric.** Bound the runner by **per-arm witness count**: instrument the walk
  to tally which `ExprKind`/`BackendExpr`/`TapeKind` arms the corpus exercises. Coverage =
  arms-with-≥1-witness / total-arms. **Zero-witness arms are reported UNVERIFIED, not sound** —
  the tower distinguishes *"checked and passed"* from *"never exercised"* (the vacuous-truth
  trap). This mirrors rustc's ui-test corpus discipline and Csmith/EMI differential coverage.

---

## 4. Prior art (the CONTRACT-TOWER lineage, with citations)

- **Nanopass framework** — Sarkar, Waddell, Dybvig, *"A Nanopass Infrastructure for Compiler
  Education"* (ICFP 2004) and Keep & Dybvig (ICFP 2013). Many small IRs, each pass a *total
  function between two precisely-defined languages*; `define-language` grammars auto-generate the
  input/output conformance check. This is the CONTRACT-TOWER's exact ancestor — the contract is
  the language definition, checked by construction. The lesson for 3A: keep levels **few and
  precise**, and let the level *definition* generate the verifier (argues against L2 as a
  hand-maintained redundant level).
- **CompCert** — Leroy, *"Formal verification of a realistic compiler"* (CACM 2009). Per-pass
  *forward-simulation* refinement relations between IR levels (Cminor→RTL→LTL→…), each a proved
  semantics-preservation theorem. 3A's lightweight analog: the per-level *parity oracle* stands
  in for the machine-checked simulation — differential testing where CompCert proves.
- **Translation validation** — Pnueli, Siegel, Singerman (TACAS 1998); Necula, *"Translation
  validation for an optimizing compiler"* (PLDI 2000). Validate each *run* of a pass with an
  a-posteriori checker rather than proving the pass. This is exactly `differential(edge, corpus)`
  — a per-run, per-edge validator.
- **MLIR verifier discipline** — every op/dialect carries a `verify()` invariant; `mlir-opt
  --verify-each` runs it after every pass; `ConversionTarget` legality (legal/illegal ops)
  encodes *"this lowering must eliminate construct X"* — the totality contract made
  machine-checkable. Direct blueprint for wiring `C_k` into `xtask --verify-each`.
- **GHC Core Lint** (`-dcore-lint`) — re-typechecks Core after each Core-to-Core pass, catching
  any pass that produced ill-typed Core. The precise analog of 3A's *type-level* contract: after
  threading `->`, Lint-check that `DirectBuildTarget.type_ref` is well-formed and consistent with
  `node_types`. rustc's `-Zvalidate-mir` / `MirPass` pre/post validation is the same idea.
- **Differential/metamorphic compiler testing** — Yang et al., *Csmith* (PLDI 2011); Le,
  Afshari, Su, *"Compiler validation via equivalence modulo inputs"* (PLDI 2014). The corpus-
  coverage + injected-fault-localization methodology for §3's runner.

---

## 5. Named risks and counterexamples

- **R1 — "three levels" is a designer's assertion, not a live fact.** The spine has four value
  levels (§0); forcing three either **collapses L2 into L1's serialize** (correct, but must prove
  `C_{L1→L2}` is an identity, else information is lost) or **splits a phantom level**. *Counter:*
  if `SinkOnlyProgram` drops any `ValueProject.ty` the emitter later needs (it does not read `ty`
  today, R6 §5b — but GN-3 will), L2 becomes a *lossy* re-encoding and the collapse is illegal.
  The prototype must measure the L1→L2 information delta before fixing the level count.
- **R2 — the type-level contract has no live consumer.** `TypeAnalysis::from_ir` is a
  pre-restart relic (§2); on live the `->` type dies at `:204` and `DirectBuildTarget` is unfed.
  So `C_{L0→L1}`'s type conjunct is **born-RED, owner GN-3.W2** — 3A must ship it as a declared
  dependency edge, not claim a passing contract on HEAD.
- **R3 — the un-contracted partial function at E4.** `materialize_rule`'s
  `descriptors.get(&rule_id)` else-return-`body` (`:1367-1369`) and the byte-literal role
  classifier (`derive_materialization_roles`, `:1403`; the `>= 4` value-ref heuristic, `:1463`)
  are a **heuristic, silently-partial** function — a rule the classifier misses lowers with *no*
  materialization and *no* error. A "total function with a checkable contract" cannot rest on it.
  *Counter:* the CSV/TOML fleet (GN-5) or a re-ordered value alternation defeats the `{`+`}` /
  `>= 4`-ref heuristics → a container silently un-materialized. The contract must make role
  assignment **total** (every rule → a role or explicit `NoRole`), turning the name-gate at
  `runtime_generator.rs:98` (`has_rule("value")`) and the byte-literal gates into contract
  violations, not routing.
- **R4 — zero-witness vacuity.** JSON exercises zero `->` (§3); a contract that reports green on
  JSON's type edge is *vacuously* green. Conflating "unverified" with "sound" is the exact defect
  the constitution's clause vii forbids (running prototypes, not vague optimism).
- **R5 — regen byte-identity is necessary, not sufficient.** `C_{L2→L3}` (byte-diff) catches
  emission drift but is **blind to a mis-typed-but-identically-emitted** DirectBuild (the type
  plane costs zero at runtime, so a wrong `type_ref` that renders the same Rust passes the
  byte-diff). The semantic digests (§3) are the required complement — a contract tower that leans
  only on regen byte-identity misses type-plane faults.

---

## 6. Exactly what a prototype must test to move family 3A

1. **Mechanically enumerate the edge-DAG.** A pass that walks `GrammarIr → BackendExpr →
   SinkOnlyProgram`, emits the six edges + per-edge value-type, and **measures the L1→L2
   information delta** (does `SinkOnlyExpr` drop anything L1 carries?). *Moves:* resolves R1 —
   whether the fixed tower is three levels or four, by evidence not assertion.
2. **Wire the Project@204 `verify` predicate and drive it RED→GREEN.** Land the §2 thread on a
   worktree; show `C_{L0→L1}`'s producer≡consumer identity flips RED→GREEN; prove **zero emit
   footprint** by byte-identical `check-json`/`check-css`/`check-sheets` with the predicate on and
   off. *Moves:* proves the type-level contract is checkable and cost-free; discharges R2's
   born-RED edge against GN-3.W2.
3. **Build `differential(edge, corpus)` over the shipped digests.** (a) Reproduce
   `parity_digests`/`typed_checksum` on the JSON corpus; (b) inject a `TapeKind` flip at
   `materialize_rule:1380` and a dropped `DirectBuild` splice, and show the runner **localizes
   each to its edge**; (c) emit **per-arm witness counts** and flag every zero-witness arm
   UNVERIFIED. *Moves:* proves faults localize per-edge and the coverage metric distinguishes
   sound from unexercised (R3, R4).
4. **Make role assignment total.** Prototype a `derive_materialization_roles` that returns a
   role or explicit `NoRole` for *every* rule, and a `C_{L0→L1}` conjunct that fails on any
   `NoRole` reaching `materialize_rule`. Test it against a re-ordered JSON value alternation and
   the CSV grammar (`grammar/misc/csv.bbnf`). *Moves:* converts the E4 partial function into the
   total, contracted function the family's charter names — the sharpest gap between the live
   spine and 3A's thesis.

**Substrate (KISS, no new god module):** a `Contract` trait per edge
(`check(&In, &Out) -> Result<(), ContractViolation>`), the existing `check_target`
byte-diff (`xtask/src/regen.rs:37`) as `C_{L2→L3}`, the generalized `direct_struct` digests
(`bbnf-bench/src/direct_struct.rs:421-482`) as the differential runner, and an `xtask
--verify-each` flag that runs the contracts at regen — the MLIR/GHC-Lint pattern, resident in
the tree the tranche already ships.
