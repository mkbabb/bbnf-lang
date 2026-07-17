# PROTO-2 — LOCK-2 THE GRAPH ENGINE · probe 2: PROVE-2-SUBSTRATE-RECONCILE

**Family 2B IR-IS-EGRAPH — the adopt-and-prove port no family scopes; settles context-freedom.**
CRIT-1 #1 pass-3 directive.

- **BASE-PIN:** HEAD `ce51c36fcbdf5108eb028005586672d9a6d2ca1b` — *is* the pin `ce51c36fc`
  (`git merge-base --is-ancestor ce51c36fc HEAD` → true; clean descent, no fast-forward
  needed). All numbers below measured on this exact HEAD. **Not** wrong-artifact-risk.
- **loadavg:** 10.16 → 7.41 over the run (recorded; the two paired `check-json` runs and
  the test are correctness/idempotency measurements, not perf claims — no timing asserted).
- **Verdict:** `RAN_MEASURED` — **PASS**. **2B context-freedom RATIFIED (0 conflicts, non-vacuous).**

---

## QUESTION

Can the e-graph engine be ported organ-level into the skinny substrate and wired to a
skinny decision seam in ONE tree — compiling, `regen∘regen` byte-identical — and does the
**REAL** `admits_sink_only` caller-conflict walk over the skinny fleet return **0 or ≥1**
conflicts?

## METHOD

Skinny worktree, base-pinned, HEAD named. Four measurements, all RAN:

1. `env -C skinny cargo build -p ir -p passes` — engine compiles in skinny.
2. `env -C skinny cargo run -p xtask -- check-json` **×2** — `regen∘regen` byte-identity.
3. `cargo test -p passes admits_sink_conflict_walk -- --nocapture` — the real conflict walk
   over the fleet (a new test authored **inside `mod recognizers`** so it calls the private
   `admits_sink_only` / `contains_direct_build` directly — no re-implementation, no mock).
4. Static reconcile of the GrammarENode organ boundary against skinny's ir type surface.

## WHAT RAN — THE NUMBERS

| measurement | command | result |
|---|---|---|
| engine build | `cargo build -p ir -p passes` (skinny) | **exit 0**, 3.63s; `egraph` + `csp-solver` crates compiled into skinny graph |
| regen run 1 | `xtask check-json` | **exit 0**, no diff |
| regen run 2 | `xtask check-json` | **exit 0**, no diff — **byte-identical**; `git status` shows **zero** generated-file (`crates/runtime/src/grammars/*`) drift across both runs |
| conflict walk | `cargo test … admits_sink_conflict_walk` | **exit 0 / 1 passed**; `conflicts=0` |

Conflict-walk emitted line (fleet = the on-disk skinny fleet, `skinny/grammars/json.bbnf`,
the sole `.bbnf`):

```
CONFLICT-WALK fleet=json rules=15 callrule_edges=28 rules_with_callers=14
              multi_caller_rules=5 live_gate_witnesses=["pair", "string"] conflicts=0
```

- **15** backend rules; **28** `CallRule` edges; **14** rules reached by ≥1 caller.
- **5** rules reached by **>1** caller.
- **2** of those multi-caller rules (`pair`, `string`) carry a `DirectBuild` — the node that
  makes `admits_sink_only`'s gate **LIVE** (`contains_direct_build(expr) == true`). These are
  the rules where a caller-varied substrate target *would* flip the verdict.
- **conflicts = 0.** No smallest witness exists — there is nothing to repair.

**Non-vacuity:** the walk is not empty theatre. There genuinely are shared-across-callers
rules (`pair`, `string`) whose sink-only admissibility is *active*, yet every caller of each
sees the identical verdict.

## WHY 0 — THE ROOT (verified on disk)

`admits_sink_only(rule_ir: &BackendRule, target: TargetFeatures)` (skinny
`passes/src/lib.rs:958`) is:

```rust
contains_direct_build(&rule_ir.expr) && (target.direct_only_output || target.direct_build_consumer)
```

Its only two inputs are (a) the **callee's own** IR structure and (b) `target`. It takes **no
caller-context argument**. And `target` is threaded **globally**: `compile()` (lib.rs:52)
builds exactly one `recognizers::TargetFeatures::host()` and passes that single value through
`derive_backend_shape_with_diagnostics`, whose per-rule loop
(`for rule in &grammar.rules { … choose_backend_shape(…, target, …) }`) reuses it verbatim.
There is no per-call-site target anywhere in the real compile path. Therefore a rule's
sink-only admissibility is provably **caller-invariant** — context-free — and the walk over
the real fleet confirms it empirically. **2B RATIFIED.**

## THE PORT — organ boundary reconciled (this is the finding PROTO-1 missed)

PROTO-1's context rider grepped the wrong tree and declared `admits_sink_only` "absent"; it
lives at `skinny/crates/passes/src/lib.rs:958` (three call-sites: 524, 661, 958). Reconciled:

- **The ENGINE organ IS separable and is ALREADY in skinny.** `skinny/crates/passes` already
  declares `egraph = { path = "../crates/egraph" }` + `csp-solver = { path = "../crates/csp-solver" }`.
  `skinny/crates/passes/src/backend_egraph.rs` builds `EGraph<DecisionNode, NoAnalysis>`, runs
  the `NormalizeDirectSinkCost` rewrite via `BackoffScheduler`, and **extracts** the winning
  candidate through `Extractor::best_node` — i.e. **a seam already consumes an extracted
  egraph candidate.** `backend_egraph::select` is the wired decision seam; it is called at
  `lib.rs:505` (`crate::backend_egraph::select(rule.id, candidates)`).
- **The GrammarENode organ is NOT independently portable.** Top-level
  `crates/ir/src/egraph/node.rs` needs `crate::{AltDispatch, FnId, RuleId, StringId}`; `mod.rs`
  needs `crate::GrammarIR` + `crate::types::count_nodes`; `cost.rs` needs
  `crate::{FnDescriptor, CostConfig}`. GrammarENode "mirrors `DagNode` variant-for-variant."
  **Skinny ir carries none of these** — it has a flat `GrammarIr` (exprs/rules), `RuleId(usize)`,
  and no interned `StringId`/`FnId`/`AltDispatch`/`DagNode`. Vendoring GrammarENode verbatim
  would drag the whole top-level ir DAG substrate — that specific move is the XL-A inversion.

**Resolution (not an escalation):** the correct organ boundary per XL-A is the `egraph`
**crate** (the engine primitives), and each substrate authors its own e-node over its own IR.
Skinny already did exactly this: `DecisionNode` over `BackendShape`/candidates is skinny's
substrate-native GrammarENode-analogue. The "adopt-and-prove port no family scopes" resolves
to: **adopt the engine (done), author substrate-native nodes (done: `DecisionNode`).** No
GESTALT-B escalation is needed — the tree-of-record contradiction is *not* load-bearing here
because the engine, not the grammar e-node, is the shared organ.

### bounded_sum via the ConstraintEnum::Custom seam

`bounded_sum.rs` is merged into the skinny-consumed `crates/csp-solver/src/constraint/`
(`pub mod bounded_sum; pub use bounded_sum::BoundedSumConstraint;`), a `Constraint<D>` impl
that enters the solver through the existing `ConstraintEnum::Custom(Box<dyn Constraint<D>>)`
seam (`csp/mod.rs:45`, `add_constraint`). It compiled clean inside the `-p passes` build
(csp-solver is a transitive dep). The brief's premise "bounded_sum absent from both base
trees" is stale for this worktree — it is present and wired via `Custom`, no new enum variant
minted.

## PASS/FAIL — all four PASS legs green

1. ✅ engine compiles in skinny (`build … -p ir -p passes` exit 0)
2. ✅ ≥1 seam consumes an extracted candidate (`backend_egraph::select` → `Extractor::best_node`)
3. ✅ `check-json` byte-identical across two runs (both exit 0, zero generated drift)
4. ✅ conflict walk returns a concrete count — **0**

**VERDICT LINE:** PASS · `admits_sink_conflict_walk` = **0 conflicts** →
**2B (IR-IS-EGRAPH) context-freedom RATIFIED**, non-vacuously (`pair`, `string` are live
multi-caller sink-only rules with uniform verdicts). Engine organ = the `egraph` crate
(already in skinny, seam extracts `DecisionNode`); GrammarENode is welded to the top-level ir
DAG and is *not* the portable organ — which is precisely why skinny authored a
substrate-native node rather than vendoring it. No GESTALT-B escalation warranted.

## DELIVERABLES

- `PROTO-2.md` (this file).
- `PROTO-2-conflict-walk.patch` — the sole code change: the `admits_sink_conflict_walk` test
  added inside `mod recognizers` in `skinny/crates/passes/src/lib.rs` (+ its `collect_callees`
  CallRule-graph walker). No production code touched; regen output unchanged (proven by the
  two byte-identical `check-json` runs).
