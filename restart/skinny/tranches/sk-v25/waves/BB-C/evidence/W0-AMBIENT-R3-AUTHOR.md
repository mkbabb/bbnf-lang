MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: R3 implementation author.

# BB-C.W0 ambient optimizer authority — R3 author freeze

- Frozen: `2026-07-22T19:03:05Z`
- Repository HEAD: `af15f63e0d2d3d719938c13b906a50acbb92ea3b`
- Host observation: Apple `arm64`, macOS `26.4.1`
- Scope: the smallest cohesive R3 repair ordered by the R2 adjudication,
  items 1–7 only
- Execution status: **STATIC AUTHOR FREEZE ONLY**
- Credit: **zero BB-C.W0, wave, lock, tranche, or movement credit**
- Hostile clean count: **0/2**
- Advancement: **BB-C.W1 may not begin**

No Cargo command, build, test executable, benchmark, package/lock/config
mutation, generated-runtime regeneration, x86 route, or cross-repository mutation
was performed by this author pass. Native Apple-ARM execution is a separate root
step after this freeze. This record is an implementation handoff, not GREEN,
acceptance, or proof that BB-C.W0 item 3 is complete.

## Governing frozen authority

| Record | SHA-256 |
|---|---|
| `waves/BB-C/W0.md` | `00bb99d6b3c379c7c15ad7a70ab7598f8b883f9cd8d3974733323bdbc6026df2` |
| `W0-AMBIENT-R1-ADJUDICATION.md` | `64aaa76d74c2ea533d43c656a5f793695dbc584c6c9cbbcf317446a670216a63` |
| `W0-AMBIENT-R2-AUTHOR.md` | `b98b7a54206d67f276ed132b34301d5b26ea2bdca547661dde8fff6668ab1244` |
| `W0-AMBIENT-R2-CRITIC-A.md` | `e17b8e736655cc5ee7d746b9e37ab5b1112ab33f1f29e852f8791ff594f75117` |
| `W0-AMBIENT-R2-CRITIC-B.md` | `b197f50fca87842e67df9b718febdf26ffe2013006f8afe2753c6bf7aea29630` |
| `W0-AMBIENT-R2-ADJUDICATION.md` | `ef7c8c1db203eb42ae40358e35fd2710d4e79d3cabc5495801c619b18995b9f7` |

The R2 adjudication is the controlling disposition. This pass neither edits
nor supersedes it.

## Frozen implementation subject

Known R2/pre-R3 hashes are recorded where the operand existed at handoff.
`ABSENT` means R3 created the displaced test path. The pipeline error enum was
already retained R2 state and is listed separately as a read-only dependency;
R3 did not claim its bytes as a new change.

| Path | Pre-R3 SHA-256 | R3 freeze SHA-256 | Lines / bytes |
|---|---|---|---:|
| `crates/ir/src/passes/csp_strategy/mod.rs` | `81c61d987ae71f0aa20c99dfd93e2e53ba1f75c18ffca9497acf381cbaaea6bd` | `7fdbaaf5790e09d6f7a22568b91ecadaf54e9f29f4ec4730ab250d59f4084897` | 1885 / 68151 |
| `crates/ir/src/passes/csp_strategy/components.rs` | `07a235d333992da0f70c6ff5e27e4b036828c0026bf531ba3f9414c3a910131a` | `35d23889c56888e61dc873843e446790f066dd151a3cd144a06ac3d345d7648b` | 336 / 13267 |
| `crates/ir/src/passes/csp_strategy/constraints/dispatch.rs` | `0325348d5dff060ea0c414da7f7afdda2431e6ef66c4042f3d1be4c7c564921d` | `6a546ea458d390c3dde6791d295cf2f71a224cec26dbf8943171a2e9df1658ce` | 95 / 3695 |
| `crates/ir/src/passes/csp_strategy/constraints/engine.rs` | `93742a232396052bc6131b6680ef14d551585e8b301d4aabd74870e067d33605` | `f90090f6d7e809ff616d30dcd68a11908157b3ce94d4a80945cfbff5628aa000` | 178 / 7844 |
| `crates/ir/src/passes/csp_strategy/constraints/layout.rs` | `6939fdc671d6b74fe09f847cfe3166f16125d4cc4e1b1bacebc36faceda10715` | `58c8ecf3d55ea75b8a52277d1f495e406b068adfce652007779bd11c2e7a04b4` | 106 / 4122 |
| `crates/ir/src/passes/csp_strategy/constraints/mod.rs` | `955bea2091aba369404dd1481b434ea6053b1fc631974f3cabb338014c50aa9f` | `f4c4197f4b78ef8ea02ee7be5a815f9f523ddbb8091c57ae641a5c8c35aae646` | 105 / 4562 |
| `crates/ir/src/passes/csp_strategy/constraints/shape.rs` | `6de8c4c54a8e96c12d713eb548169fb0bc0100f340a736a5da68a92adc5a742f` | `24e2a2347cf555d9aafeba89b0857ca6c5bab4035895dae89fff6055c4bf8634` | 136 / 5530 |
| `crates/ir/src/passes/mod.rs` | `ef505f512d129d358992b910b575e02f4c84b0adc455adcecf701bdcafd0b46d` | `6805c17691a53997354fc818b34e85d8fb60da9799132a8c526e2b0d7b6120f9` | 80 / 3298 |
| `crates/ir/tests/passes/csp_strategy/mod.rs` | `ABSENT` | `e217c090f529932015fb59f91efdfb82fbf02140a0571fd2c44723392a7029b9` | 373 / 11967 |
| `crates/ir/tests/vm/cross_rule_csp.rs` | `6ae94ebcf215790869d86e2a24958faa437655f6a6b638a1a90d9e452561041f` | `2bff8d52dc0b9464218e33ff544d239c37ca21473cab76ee49f488358c3c2f10` | 740 / 25551 |
| `crates/core/src/pipeline/compile/mod.rs` | `28508f3ef3087626a590e90099a55c855c0f38bc814abb8ed99cba6c719f165d` | `cb8685c44438c825de70e6efcdb0298c8fc4f4b6c7c8c9b5589c5a13a470a3b5` | 124 / 4504 |
| `crates/core/src/pipeline/compile/pipeline.rs` | `bc02d7ea79fe4c39765b164b491d040f233470df9fbd9de8b8370748e7a676ab` | `ee917eeb17347c49f8eb908015f002ac4b4a4bc5bda4f896a3865815f82c3ca8` | 460 / 20865 |
| `crates/core/tests/pipeline/compile/pipeline.rs` | `ABSENT` | `49b4d09c961512b24b030555dfde07f8182f434df5e5d424912e50c9c193aea9` | 127 / 4599 |
| `crates/lsp/src/dap/adapter.rs` | `8e3faf06e827820ca1aef0c0a573619a88cbd4796ac737036cb0e4895565ddf1` | `984ef143367e89de9cae75009cbc000f5b9c3aaa824f2c0c25e674dfe26b9625` | 280 / 9734 |

Read-only retained dependency:

| Path | SHA-256 | Lines / bytes |
|---|---|---:|
| `crates/core/src/pipeline.rs` | `5eac07248f09df5539f5265e2c9f011c6c9e793d3d0049d39dba764b109b82c4` | 105 / 3294 |

The R2 source-local untracked test
`crates/core/src/pipeline/compile/pipeline/tests.rs`, frozen at
`bff62ebf9b01db50e46b291a793b55884c4ca8f0c7f99d0d68a0a0b449cdffe8`,
is absent. Its corrected live witness is at the external isomorphic path in the
table. No compatibility alias or duplicate test path remains.

## R3 implementation, mapped to the ordered disposition

### 1–2. Born-RED objective proof and immutable cost authority

`StrategyDomain` now has one canonical ordered `entries` table of
`(StrategyValue, f64)` and a separate mutable `active` bitmap. Search removal
and restoration toggle membership only. `cost`, active iteration, and values
read the immutable canonical table. Domain validation rejects duplicate values
and non-finite costs; decode also rejects a non-finite accumulated objective.
No restored value can acquire an invented cost or a new tie position.

The displaced optimizer tests:

- prune and restore every canonical choice, requiring identical value order
  and bit-exact costs;
- exercise the adjudicated `A:0/B:2`, `C:10/D:0`, `A => C` counterexample;
- compare the solver result and immutable score with explicit exhaustive
  enumeration under `Chronological`, `FailFirst`, and `Mrv`, forward and
  reversed canonical value tables, twice; and
- remove the hard implication and require the result to invert to the
  unconstrained optimum.

### 3. One semantic authority per `(NodeId, family)`

`SiteRegistry` collects all occurrences before `Csp` construction. It creates
one site and one `VarId` for each canonical `(NodeId, DecisionFamily)` in the
component, including materialization. Repeated occurrences conjoin active
legality. A shared value with a bit-different cost is an exact
`InvalidAssignment`; an empty joined domain is typed `Unsatisfiable`.

Because joined domains become each `Variable`'s original domain before search,
`Csp::solve()` reset cannot discard the join. The decoder rejects duplicate
semantic writes and incomplete/foreign assignments. Grammar-level merge is
sorted and rejects exact family conflicts; partial local accumulation remains
unobservable on error.

The public cross-rule witnesses cover:

- repeated same-rule hash-consed nodes with one Alt and two Engine authorities,
  deduplicated token implications, full coverage, and reversed child order;
- connected rules with a shared root NodeId, one joined materialization
  authority, pinned/plain legality, and reversed rule-reference order; and
- genuinely conflicting disconnected components, in both pinned/plain orders,
  with one completed and one failing report and no result publication.

### 4. Cancellation is a first-class typed stop

`OptimizationUnavailableReason::Cancelled` and
`SolveCompletionStatus::Cancelled` are distinct from budget exhaustion and
unsatisfiability. The constrained wrapper checks `stats.cancelled` before
budget and before consuming any returned solution. It therefore discards a
best-so-far incumbent.

The tests cover a pre-search cancelled token and a deterministic
post-incumbent cancellation. The latter uses a scoped `LambdaConstraint` and
an atomic event counter; it contains no sleep, wall-clock race, or retry loop.
No `TimedOut` variant was invented because the active config has no deadline.

### 5. Compact deterministic component proof

Both success and failure now carry structured reports. Each report records:

- root and sorted component members;
- variable/site/active-domain-value counts by Alt, Wrap, Engine, and
  Materialization;
- exact counts for token-dispatch, engine, shape, layout, and dispatch
  constraints;
- semantic-site and decoded-site coverage;
- factorized-exact, optimal, cancelled, budget, unsat, or invalid completion;
- nodes explored, node budget, cancellation and budget flags;
- a bit-exact additive objective on success; and
- an exact grammar/rule/site/variable/search/merge failure locus.

Constraint installers sort NodeIds, engine variables are sorted/deduplicated,
token implications deduplicate `(Alt VarId, Engine VarId)`, and grammar merge
sorts NodeIds. Errors include all earlier completed reports plus the failing
report. Conditional environment logging is no longer the report authority.

### 6. External, isomorphic proof layout

The optimizer unit witness moved from the production module body to
`crates/ir/tests/passes/csp_strategy/mod.rs`, loaded by an exact `#[path]`.
The pipeline witness moved out of `src` to
`crates/core/tests/pipeline/compile/pipeline.rs`, also loaded by an exact
`#[path]`. The tests cover immutable objective restoration, hard-constraint
inversion, semantic-site join/collision, cancellation before and after an
incumbent, budget versus unsat, malformed/incomplete/wrong-family/non-finite
assignment, factorized tie order/objective, a late-component failure after an
early success, structured compile errors, and the direct typed-arm/VM contract.

These are frozen test designs. They have not executed in this author pass.

### 7. One structured compile-error surface and corrected authority prose

Canonical `compile_grammar` and `compile_ast` now return `CompileError` rather
than stringifying it. Request APIs already use the same type. The DAP adapter
performs the only string conversion at its pre-existing `Result<_, String>`
protocol boundary. The public optimizer report/error types are re-exported
through `bbnf_ir::passes`.

Active optimizer prose now describes the real component solve, actual
`EnginePropagation` family, typed unavailable outcome, canonical NodeId site
identity, and `ByteDispatch` trigger. Historical frozen critic records were
not rewritten.

## Exact static evidence

Formatting was applied only to the fourteen scoped Rust subjects with:

```text
rustfmt --edition 2024 <the fourteen frozen Rust paths>
```

Scoped `git diff --check` returned exit `0` with no output.

Runtime fallback/cost-corruption census:

```text
rg -n "decode_min_cost_per_variable|decode_fallback|fallback_alt|fallback_wrap|fallback_engine|costs\.push\(1\.0\)|swap_remove" \
  crates/ir/src/passes/csp_strategy \
  crates/ir/tests/passes/csp_strategy \
  crates/ir/tests/vm/cross_rule_csp.rs
```

Result: **zero rows**.

Active authority prose census (the governing W0 plus live source, excluding
immutable historical evidence records):

```text
rg -n "ParentCompatibility|TierFollowsMaterialization|per-variable trivial picks|falls back to per-variable|solve falls back|existing VM-facing API intact|parent / child variables" \
  restart/skinny/tranches/sk-v25/waves/BB-C/W0.md \
  crates/ir/src/passes/csp_strategy \
  crates/core/src/pipeline/compile
```

Result: **zero rows**.

Returned-report symbol census:

```text
rg -n "ComponentReport|SolveCompletionStatus|OptimizationFailureLocus|ObjectiveProof" \
  crates/ir/src/passes/csp_strategy crates/ir/src/passes/mod.rs \
  crates/ir/tests/passes/csp_strategy crates/ir/tests/vm/cross_rule_csp.rs | wc -l
```

Result: **82 rows**.

Canonical typed-API census:

```text
rg -n "pub fn compile_(grammar|ast)|Err\(err\.to_string\(\)\)" \
  crates/core/src/pipeline/compile/mod.rs crates/lsp/src/dap/adapter.rs
```

Result: four canonical/request signatures; both canonical signatures return
`CompileError`; **zero** `Err(err.to_string())` wrapper rows. The DAP boundary's
closure conversion is intentionally not a canonical compile API.

Test-layout census:

```text
crates/ir/src/passes/csp_strategy/mod.rs:1883:#[cfg(test)]
crates/ir/src/passes/csp_strategy/mod.rs:1884:#[path = "../../../tests/passes/csp_strategy/mod.rs"]
crates/core/src/pipeline/compile/pipeline.rs:458:#[cfg(test)]
crates/core/src/pipeline/compile/pipeline.rs:459:#[path = "../../../tests/pipeline/compile/pipeline.rs"]
SOURCE_LOCAL_TEST_ABSENT
```

## Preservation and routed remainder

This pass did not touch manifests, `Cargo.lock`, `.cargo`, packages, release
state, generated grammar/runtime sectors, csp-solver, parse-that, Value,
Glass/SCI/Atlas, playground, G3, or BB-C.W1. Concurrent unrelated dirty bytes
remain outside this subject and are neither claimed nor interpreted here.

Routed remainder, without movement:

1. run the separately authorized, exact native Apple-ARM evidence pass once;
2. freeze its command/environment/exit/output identities without a retry loop;
3. submit this R3 subject and native receipt to two fresh hostile critics;
4. submit both critics to a fresh adjudicator;
5. keep BB-C.W0 item 3 RED unless that adjudication proves it clean; and
6. even if item 3 becomes clean, retain W0 items 1, 2, 4, and 5 as independently
   unaccepted until their own governed evidence closes them.

**AUTHOR FREEZE ONLY. CREDIT 0. HOSTILE CLEAN 0/2. BB-C.W1 MAY NOT BEGIN.**
