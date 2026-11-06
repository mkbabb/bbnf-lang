MODEL: gpt-5.6-sol; REASONING: ultra; WORKFLOW: v2

# BB-C.W0 ambient optimizer authority — R1 adjudication

- Adjudicated: `2026-07-22T17:22:40Z`
- Terminal verdict: **R1 RED / REJECTED AS BB-C.W0 ITEM 3**
- Retained fact: **the live production grammar-egraph saturation/write-back
  edge is absent**
- Credit: **zero BB-C.W0, wave, lock, tranche, or movement credit**

R1 is a correct and parsimonious deletion of one production edge, but it is not
BB-C.W0 item 3. The independent-minimum and priority CSP failure routes remain
live, the new witness specifies a VM result contradicted by the VM source, and
no executable semantic receipt exists. These are technical RED findings, not
ownership blockers.

## Authority and exact subject

| Record | SHA-256 |
|---|---|
| `waves/BB-C/W0.md` | `00bb99d6b3c379c7c15ad7a70ab7598f8b883f9cd8d3974733323bdbc6026df2` |
| `W0-AMBIENT-R1-AUTHOR.md` | `08fb239e2a106bf7b75b89c48eff1c0fd5ea75c173698669e37f136dd191978b` |
| `W0-AMBIENT-R1-CRITIC-A.md` | `503271b7167d9375af45e166f79840a7291d974f997daa17510e7afc9fffaa09` |
| `W0-AMBIENT-R1-CRITIC-B.md` | `f33ea960eac6f36dabcb629409b6a01d676d186f03e76747ec1b3d7102f3b699` |

The exact five-file R1 subject matches the author record:

| Subject | SHA-256 |
|---|---|
| `crates/core/src/pipeline/compile/pipeline.rs` | `8ee0c2ff7d61b2d9be7791a7aa267b3861f303778f84febcb133be132c2ac020` |
| `crates/ir/src/egraph/mod.rs` | `04b675f97b050661a3c6551d4caf87da29fec8b3472e41f02c9ee842933e80ab` |
| `crates/ir/src/passes/transform/mod.rs` | `320772ce08b618596513a531fe052982ff8f27f6955d804a25f03982522e5343` |
| `crates/ir/src/passes/materialization/classify.rs` | `7a4d8fb861582e3744cf93da6880b05be648ddfbd930c56e7950ef7752fd51a2` |
| `crates/core/tests/direct_pipeline_semantics.rs` | `d438007ff2e54ef071c49b1b6e46477a78dafe38e73e16c5ee06ed00c05eef83` |

Primary adjudication anchors were also frozen before judgment:

| Source | SHA-256 |
|---|---|
| `crates/ir/src/passes/csp_strategy/mod.rs` | `709c9b453a2efee0fd9c394a8bef7db698c892a102316922d3ef8338f701273a` |
| `crates/ir/src/cost_config.rs` | `c4ed10361a591f2d680b5b4019e46f4733db2d446a355c552d13d861d2e2563a` |
| `crates/core/src/lower/mod.rs` | `c279cd8a89880bb0c9245ac4a359200450faf28b1b28eed0e42928ab8c9dd2d3` |
| `vm/compiler/compound.rs` | `6a467482bb21add9da4bc629058f1c13c1de4050d82d321d32bce62ac587eb7d` |
| `vm/compiler/rule.rs` | `6744b6f9be734c56be1601bd031c03e9d1b1362d7e3a6770d5728fb9f90207a5` |
| `vm/compiler/node.rs` | `59cd56b3a0c364306a7f85278e4553f8ad7e8993d69b696a5b6ccfa2bd203997` |
| `vm/interpreter/mod.rs` | `d83e21da0e0b12eb89ecbe3b85743e4d90b464285383db52b41ec9562b04c1b4` |
| `vm/interpreter/control.rs` | `c77e5bbea8c6385decdddafbf9a4c546a0c2ec0d048be3b85f7378d103ba1a8f` |
| `vm/interpreter/construct.rs` | `fd568f83dee0f0a6fa108dd21d8a9ed837e68a3f3095e60f4ba181f1171865c1` |
| `vm/interpreter/leaves.rs` | `599774762968aa5a713d01fdc7d5370be72353e62fcbd414d32db54b5a1cd782` |

No Cargo command was run for this adjudication. Findings below are source,
hash, diff, and call-route facts.

## Findings adjudicated

### 1. Production grammar-egraph deletion — correct narrow fact

Critic B finding 1 and both critics' retained narrow deletion are **correct**.
`compile_ast_common` has no call to grammar `build_and_saturate`,
`write_back_optimized`, `pool.write_back`, or the former timer span. The exact
production-core symbol census returns no row. The tracked subject is `+20/-76`
(net `-56`), and no flag, alias, shim, disabled route, or replacement egraph
write-back was added. Retaining `crates/ir/src/egraph` as a non-authoritative
research substrate is consistent with W0.

This proves only the egraph clause of item 3. It does not prove item 3, W0, or
semantic parity.

### 2. Item-3 scope — critics correct

Critic A finding 2 and critic B finding 2 are **correct**. W0 item 3 is one
conjunction:

1. disable ambient whole-program grammar-egraph write-back; and
2. disable independent-minimum CSP fallback.

R1 implements only the first. Production still calls
`solve_grammar_components` and installs its decisions and materialization
refinements. The CSP still:

- returns empty maps when `ir.dag` is absent;
- calls `decode_min_cost_per_variable` after budget exhaustion, explicitly
  discarding installed constraints;
- calls `decode_fallback` after unsatisfiability, publishing a separate priority
  cascade; and
- writes earlier component decisions into aggregate maps before later
  components have proved total success.

The zero-constraint direct-minimum path is mathematically exact for a truly
factorized objective, but it has no returned proof/report distinguishing that
case from failure fallbacks. It therefore cannot rescue R1's item-3 claim.
The author scope must be read as “item 3 egraph clause only.”

### 3. Rejection oracle — critic A correct

Critic A finding 1 is **correct**, including the concrete result. For `!x`:

1. the outer `start` sequence saves offset/value state;
2. `!` succeeds, leaving offset `1` and `Span(0,1)`;
3. `branch` fails at the missing `z`; its branch/dispatch checkpoint restores
   to offset `1` and removes branch-local values;
4. because `Ref(branch)` is the outer sequence's final child,
   `compile_seq` reaches `DropState` rather than its restore block, preserving
   offset `1` and the `!` span while retaining the error flag;
5. `compile_rule` emits `MakeTagged` even on error, so `start` becomes a partial
   tagged value over `(0,1)`; and
6. `Interpreter::run` pops and returns that value even though `success=false`.

Diagnostics use the furthest rule's FOLLOW set, not the failing literal. The
furthest point is offset `2` in `branch`; `branch` is terminal in `start`, and
the implementation has no EOF sentinel, so its FOLLOW vector is empty.

The witness's expected offset `0`, value `None`, and `[z]` expectation are thus
source-provably wrong. The test would fail if its executable ran.

### 4. Typed-map proof — critics correct

Critic A finding 3 and critic B finding 4 are **correct**. A recursive global
count of two `IrNode::Map` values proves none of the following:

- one map is in each `branch` arm;
- either map's exact `FnId`;
- `FnDescriptor::Expr` payloads `IntLit(0)` and `IntLit(1)`;
- the exact `TypeDesc::U8` projection; or
- map survival and placement after legacy extraction.

Only the `x` arm executes. The VM compiler deliberately lowers `Map` by
compiling only its inner node, so VM span equality is not typing evidence.

### 5. Legacy differential and configuration — critics correct

Critic A finding 4 and critic B findings 3 and 5 are **correct**.

The removed edge formerly ran immediately after pattern hoisting and before
alt sorting, span refinement, FOLLOW/lookahead/token restructuring, DAG build,
mining, dispatch, materialization, and strategy CSP. The R1 test first runs the
entire current pipeline, then saturates that later-state clone. That is not an
isomorphic replay of the removed pipeline edge, and the clone is neither
recompiled nor executed.

The lowering path constructs `CostConfig::from_env`; both grammar extraction
weights and egraph scheduler limits accept `BBNF_COST_*`. R1 neither removes nor
content-binds those variables. `build_and_saturate` keeps its scheduler report
local and returns no completion report to the witness. The asserted legacy
shape therefore lacks a closed configuration, complete-saturation proof, and
deterministic replay contract.

### 6. Stale authority prose — critics correct

Critic A finding 5 and critic B finding 7 are **correct in substance**. The
current tree still describes the deleted grammar-egraph route as production or
pipeline authority in active source, test, and example prose, including:

- `crates/core/tests/egraph_semantic.rs`;
- `crates/core/tests/named_pipeline_probe.rs`;
- `crates/core/tests/named_type_preservation.rs`;
- `crates/core/examples/egraph_fire_probe.rs`;
- `crates/ir/src/egraph/rules/suffix.rs`;
- `crates/ir/tests/egraph/egraph_analysis.rs`;
- `crates/ir/tests/lattices/materialization_eclass_gate.rs`;
- `crates/ir/src/types/grammar.rs`;
- `crates/ir/src/passes/transform/pattern_dedup.rs`; and
- `crates/ir/src/passes/mod.rs`.

`pipeline.rs`'s residual “post-extraction” label is stale nomenclature rather
than a surviving call edge, but it still misdescribes the current phase. These
comments do not reactivate code; they do invalidate the author's claim that the
authority prose/census is closed.

### 7. Census — critic B correct

Critic B finding 8 is **correct**. The author's narrow `crates/core/src` query
proves absence of the former grammar-egraph caller. It does not census the CSP
half of item 3, optimizer failure routes, active tests/examples, or all stale
authority descriptions. The unlabeled 13-path mention list is not the result of
a declared reproducible query and omits live relevant paths. It must not be
called an exact optimizer-authority inventory.

### 8. Replay evidence — critics correct; historical event unproven

Critic A finding 6 and critic B finding 10 are **correct**. There is no R1 raw
log, exact command/environment receipt, exit file, or content-addressed replay
artifact. The author reports a native ARM link SIGKILL before test execution;
without a durable receipt, that historical event is **unproven**, not disproved.
Even accepting the prose, it grants no semantic, typing, determinism, mutation,
or witness credit because the executable never ran.

### 9. Structure — critics correct

Critic A finding 7 and critic B finding 6 are **correct under the binding
campaign structure precept**. A new flat
`crates/core/tests/direct_pipeline_semantics.rs` is not isomorphic to
`crates/core/src/pipeline/compile/pipeline.rs` and adds another top-level test
island. Existing sand is not precedent for new sand. The production-pipeline
witness and research-egraph differential also have different owners and should
not share one flat integration file.

### 10. Preservation and authorship — split judgment

Critic B finding 9 is **correct as split**:

- **proved:** all five post-state hashes, protected Cargo/G3 hashes, and tracked
  `+20/-76` line delta match the author record;
- **proved limitation:** ordinary `git diff --check` does not inspect the
  untracked witness;
- **unproven:** shared-worktree authorship/provenance beyond the exact pre/post
  bytes stated by the author.

These limitations do not invalidate the narrow static deletion; they prevent a
broader preservation or provenance claim.

## Retained facts and rejected movement

Retained:

- the five subject hashes are exact;
- protected Cargo/G3 bytes cited by the author are unchanged;
- production grammar-egraph build/extract/write-back has no live core caller;
- no legacy egraph route, flag, shim, or alias replaced it;
- the research substrate remains available without production authority; and
- the author correctly granted zero credit and treated technical RED as
  non-blocking.

Rejected or unproven:

- completion of W0 item 3;
- the `!x` rejection assertions;
- full typed-map preservation;
- an isomorphic or deterministic legacy differential;
- a complete optimizer/CSP/authority census;
- executable semantic or replay evidence; and
- any W0, lock, tranche, or movement credit.

## Smallest cohesive R2

R2 must retain the narrow egraph deletion and repair the item as one cohesive
slice. It must not broaden into G3, later W0 inventory/mutation waves, or a new
optimizer design.

1. **Finish item 3's CSP half.** Replace tuple/partial-map publication with a
   typed all-or-nothing solve result carrying a complete per-component report.
   Missing DAG, budget exhaustion, unsatisfiability, missing best/child/value,
   or incomplete assignment must be typed `OptimizationUnavailable` failures.
   No prior component decision may escape on a later failure. Delete the
   budget-to-independent-minimum route and the unsat priority decoder. The
   pipeline must propagate the typed unavailable result rather than install
   partial decisions. A zero-constraint component may retain a renamed direct
   factorized optimum only if its report proves zero installed constraints and
   complete domain coverage; it is an exact case, never a failure fallback.
2. **Separate and correct the witnesses.** Put the production witness in an
   isomorphic pipeline/compile test module. Assert each branch's exact `Map`
   placement, distinct `FnId`, exact `MapExpr::IntLit(0)` / `IntLit(1)` payloads
   paired with `TypeDesc::U8`, and `U8` type projection; execute both `!xz` and
   `!yz`. Specify `!x` from the
   actual VM contract above, or use a rejection shape whose expected rollback
   is independently established. Do not treat VM Map erasure as typing proof.
3. **Move the research differential to the IR egraph suite.** Build the exact
   pre-edge IR state, apply the explicit research helper there, bind an explicit
   `CostConfig` and scheduler limits without process-environment authority, and
   require a returned complete saturation/extraction report before asserting a
   shape. Remove the non-isomorphic post-pipeline clone comparison from the
   production witness.
4. **Close active prose and census.** Correct the active stale files named
   above. Publish separate reproducible censuses for production grammar-egraph
   calls, CSP failure/publication routes, and stale production-authority prose.
   Do not rewrite historical evidence as if it were current source.
5. **Make the evidence executable and durable.** Born-RED mutations must cover
   discarded hard constraints, budget exhaustion, unsat, and partial
   multi-component publication. Run only a separately authorized exact native
   ARM replay, preserve raw output/environment/exit hashes, inspect the
   untracked witness with an explicit no-index whitespace check, and then send
   the resulting R2 to two fresh hostile critics and a fresh adjudicator.

Until that R2 is implemented, executable GREEN, twice challenged, and freshly
adjudicated, BB-C.W0 item 3 remains RED. This adjudication grants no permission
to advance BB-C.W1 or any downstream movement gate.
