MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: hostile critic A.

# BB-C.W0 ambient optimizer authority — R2 hostile critic A

- Reviewed: `2026-07-22`
- Method: frozen-source, call-route, test-shape, and content-hash audit only
- Overall verdict: **R2 RED / REJECTED AS BB-C.W0 ITEM 3**
- Credit: **zero BB-C.W0, wave, lock, tranche, or movement credit**

R2 correctly deletes the two named CSP fallback implementations and correctly
delays pipeline installation until `solve_grammar_components` returns success.
It does not establish an exact constrained optimizer. Search backtracking
changes the cost function, hash-consed duplicate occurrences can silently
overwrite one another inside a component, and cooperative cancellation is
misreported as unsatisfiability or can admit a best-so-far solution. The typed
error is also not the complete per-component report required by the governing
R1 adjudication. These are semantic failures in the live production route, not
documentation niceties or missing process receipts.

No Cargo command, build, executable, x86 route, or other architecture route was
run. Critic B's report was not read. No source byte was modified by this review.

## Frozen authority and subject

The governing inputs observed were:

| Record | SHA-256 |
|---|---|
| `waves/BB-C/W0.md` | `00bb99d6b3c379c7c15ad7a70ab7598f8b883f9cd8d3974733323bdbc6026df2` |
| `W0-AMBIENT-R1-ADJUDICATION.md` | `64aaa76d74c2ea533d43c656a5f793695dbc584c6c9cbbcf317446a670216a63` |
| `W0-AMBIENT-R2-AUTHOR.md` | `b98b7a54206d67f276ed132b34301d5b26ea2bdca547661dde8fff6668ab1244` |

Every R2 subject hash listed by the author for the CSP implementation, pipeline,
and four named test files matched at review time. The seven protected hashes for
G3/Cargo/configuration also matched the author record exactly.

## Findings

### 1. RED — backtracking mutates costs and invalidates constrained exactness

`StrategyDomain` stores active values and costs in parallel mutable vectors
(`crates/ir/src/passes/csp_strategy/mod.rs:291-310`). Pruning removes both with
`swap_remove` (`:325-329`). Restoration does not recover the removed cost: it
appends the value with the invented cost `1.0` (`:335-343`). It also appends the
value rather than restoring its canonical position.

This is not a dead `Domain` method. The solver records every restriction/prune
and restores by calling `domain.add`:

- `Variable::prune` records removed values
  (`crates/csp-solver/src/variable.rs:27-35`);
- `Variable::restrict_to` records all non-selected values (`:61-74`);
- `Variable::restore` calls `self.domain.add(&val)` (`:38-47`); and
- each search branch restricts at
  `crates/csp-solver/src/solver/search.rs:245-251`, then undoes at `:260-263`.

Branch-and-bound subsequently orders values, calculates leaf cost, and computes
bounds against those mutated domains
(`crates/csp-solver/src/solver/search.rs:375-395,405-449`). Therefore the second
and later branches are not evaluated under the original objective. A value that
originally cost `0.0`, `10.0`, or any other amount costs `1.0` after its first
prune/restore. `swap_remove` plus append also changes equal-cost tie order.
Sorting returned solutions best-first (`:500-515`) cannot repair scores computed
under a changing objective.

This directly disproves all of the author's constrained-route claims of an exact
optimum, cost-minimality, and deterministic original-domain ties. It also means
`solve_exact_constrained(...).into_iter().next()` at
`crates/ir/src/passes/csp_strategy/mod.rs:731-750` can publish the wrong plan.
The factorized zero-constraint decoder does not traverse this mutation path and
is unaffected by this particular defect.

Required born-RED witnesses:

1. A domain round-trip test must remove and restore every non-unit-cost value and
   prove both `cost(value)` and canonical value order are byte-for-byte equal to
   the pre-prune state. The present implementation fails immediately because
   every restored cost is `1.0`.
2. A small constrained `StrategyDomain` CSP must compare the solver result and
   score to exhaustive enumeration under the original immutable costs, across
   all branch orderings and repeated solves. At least one branch must prune a
   differently priced later value.

The smallest coherent repair is one immutable canonical value-to-cost authority
plus mutable membership. `remove` and `add` should only toggle membership;
neither operation may create cost data or alter canonical tie order.

### 2. RED — hash-consed duplicate sites silently overwrite inside one component

The DAG explicitly maps pointer-distinct, structurally equal occurrences to one
`NodeId` (`crates/ir/src/dag/mod.rs:59-83,120-136`), and interning returns an
existing structurally equal ID (`crates/ir/src/dag/intern.rs:10-24`). R2 does not
make that identity unique in the CSP.

For every tree occurrence, `collect_sites` creates a fresh variable and site
(`crates/ir/src/passes/csp_strategy/mod.rs:927-991`). It then stores that fresh
variable into the one `by_node[NodeId]` slot (`:963-965`, `:970-973`,
`:980-983`), silently replacing the prior variable for the same NodeId/family.
`index_rule_vars` consults only that last slot (`:703-729`). Constraints likewise
resolve a NodeId through that one overwritten map, for example token-dispatch at
`:1026-1077` and the structural installers through `alt_vars`/`wrap_vars` at
`:770-790`.

All duplicate variables remain in `sites`. Decoding iterates them all
(`:851-901`), while `assign_value` stores output by NodeId using ordinary
assignment or `HashMap::insert` (`:904-924`). It does not compare an earlier
same-component value. The last occurrence silently wins. R2's conflict check at
`:472-498` runs only while merging already-decoded component maps; it cannot see
the overwritten within-component assignment.

The failure covers more than repeated descendants in one rule:

- two equal regex/Alt/Wrap occurrences in one body get distinct variables but a
  single constrained/output authority;
- two structurally equal rule bodies connected through a third caller share a
  component and root NodeId, yet get separate materialization variables; and
- equal Alt/Wrap roots in the same component can leave an earlier variable
  unpinned while the later variable alone receives the NodeId-keyed pin.

This disproves the author claims that component indexing “neither ... omits a
structurally shared node,” that domain/site coverage proves output coverage, and
that differing hash-consed decisions are never silently overwritten. The
existing test at `crates/ir/tests/vm/cross_rule_csp.rs:542-563` proves only the
later *cross-component* merge guard.

Required born-RED witnesses:

1. One rule containing two pointer-distinct equal decision subtrees must prove
   one `(NodeId, decision family)` variable and one output site, with every
   parent constraint targeting it.
2. A caller joining two rules with structurally equal roots but different
   materialization legality must prove either one coherent NodeId authority or
   a typed conflict before decode; traversal order may not select the answer.
3. Repeat both fixtures with rule/child order reversed and demand identical
   output and report.

Given the public output is NodeId-keyed, the KISS repair is to reuse the existing
VarId for each `(NodeId, decision family)` during collection and record its Site
once. If semantic occurrence identity is actually required, NodeId cannot be
the output key; silently mixing the two identities is never valid.

### 3. RED — cancellation/timeout is not a typed unavailable result

The solver has an explicit cancellation result bit:

- `SolveConfig.cancel` is a first-class input
  (`crates/csp-solver/src/config.rs:79-85`);
- `SolveStats.cancelled` is distinct from budget exhaustion (`:105-120`); and
- search sets it and stops at
  `crates/csp-solver/src/solver/search.rs:219-229`.

R2's `OptimizationUnavailableReason` has only `MissingDag`, `BudgetExceeded`,
`Unsatisfiable`, and `InvalidAssignment`
(`crates/ir/src/passes/csp_strategy/mod.rs:246-253`). After solve it checks only
`budget_exceeded` (`:731-750`). A token cancelled before search yields no
solution and is labeled `Unsatisfiable`; cancellation after a solution may
return a best-so-far solution, or fail later domain-membership validation because
the early stop bypassed normal branch undo. None is the required typed
cancel/timeout outcome.

This violates W0 item 2's explicit timeout totality and disproves the broader
author statement that constrained failure is fully typed and best-so-far values
are always discarded.

Required born-RED test: pre-cancel a `CancelToken`, run a non-empty constrained
problem through `solve_exact_constrained`, and require a distinct typed
`Cancelled`/`TimedOut` result. A second controlled mid-search cancellation must
prove zero solution publication. Check cancellation before classifying
unsatisfiable, just as budget exhaustion is checked before decoding.

### 4. RED — the promised complete per-component report does not exist

The binding R1 disposition requires a typed all-or-nothing result carrying a
complete per-component report. R2 returns only two maps on success
(`crates/ir/src/passes/csp_strategy/mod.rs:239-244`) or a reason plus one optional
root on failure (`:246-260`). It does not report component membership, variable
and site coverage, domain census, constraint families/counts, budget/statistics,
objective score, optimum proof status, or completed prior components. Its
`Display` even omits the component (`:262-265`).

Thus the typed envelope is real, but the author claim that R2 fulfills the R1
complete-report contract is false. The lack of a report also makes W0's required
“declared bounded language, validator, and report” gate unprovable. R2 need not
design a grand process system: a compact deterministic report built from facts
already computed by `solve_component` is enough.

### 5. RED — claimed mutation coverage is ordinary example coverage and misses the required witnesses

The new source-local tests at
`crates/ir/src/passes/csp_strategy/mod.rs:1394-1479` directly exercise budget
zero, an unsatisfiable not-equal pair, malformed decode input, one factorized tie,
and a NaN. They do not mutate away a hard constraint and prove the mutation
reaches output; they do not prove a former silent fallback reaches output; they
do not exercise cancellation; and they do not exercise either defect above.

The pipeline test at
`crates/core/src/pipeline/compile/pipeline/tests.rs:125-142` injects an `Err`
directly into `install_grammar_solution`. It proves that this small helper does
not mutate sidecars when handed an error. It does not create a real
multi-component solve where an early component succeeds and a later component
fails, and therefore is not the claimed late-component mutation witness.

W0 item 5 and R1's demanded born-RED mutations remain open. In addition, the
new `#[cfg(test)] mod tests` inside the 1,479-line production module and the test
subtree under `crates/core/src` conflict with the campaign's binding displaced,
source-isomorphic test-layout precept. `components.rs:338-339` itself states the
no-inline-tests convention. Repairs should move tests to the isomorphic external
test tree rather than deepen the production module.

### 6. PASS, narrowly — all-or-nothing external installation is real

Each `solve_component` returns private maps (`crates/ir/src/passes/csp_strategy/mod.rs:467-470,513-692`).
The grammar accumulator is local and `?` discards it on any solve or merge error
(`:435-464`). Although `merge_component` can mutate that local accumulator
before finding a conflict, the accumulator is not returned on error.

The core pipeline obtains the entire `Result` before installation
(`crates/core/src/pipeline/compile/pipeline.rs:324-331`), and
`install_grammar_solution` unwraps the result before assigning/extending either
map (`:349-356`). There is no fallible operation between the two map writes.
Consequently no partial optimizer output escapes a failed compile through this
route. This narrow pass does not cure the fact that a supposedly successful
component may already contain a wrong or overwritten assignment.

### 7. PASS, narrowly — named runtime fallbacks are deleted; active prose remains false

A source census finds no live `decode_min_cost_per_variable`, `decode_fallback`,
or `fallback_alt/wrap/engine` symbol in the scoped production route. Missing DAG
is typed at `crates/ir/src/passes/csp_strategy/mod.rs:405-414`; budget and
solution absence have explicit branches at `:731-750`. The production grammar
egraph call edge also remains absent from `crates/core/src`.

However, `crates/ir/src/passes/csp_strategy/components.rs:56-63` still states
that a pathological component “falls back to per-variable trivial picks.” The
parent module simultaneously describes the live solve as per-rule at `:40-53`,
then as per-component at `:55-80`, and names absent `parent`/`tier` modules at
`:121-128`. The author's symbol-only census cannot prove active authority prose
closed. Runtime deletion receives narrow credit; census/report closure does not.

### 8. PASS, narrowly — factorized decoding validates basic shape and preserves first tie

For a truly constraint-free CSP with unique sites, `decode_assignments` checks
site/variable cardinality, non-empty aligned finite cost vectors, VarId range,
solution length, type-family match, domain membership, and one Site per VarId
(`crates/ir/src/passes/csp_strategy/mod.rs:851-901`). Its strict `<` scan at
`:875-883` preserves the first canonical domain value on a tie. That is an exact
separable optimum for the stated additive objective.

This pass is conditional on unique NodeId/family authority. Finding 2 shows the
current collector can satisfy one-site-per-*variable* while still emitting many
variables for one output site, so the validation is not a proof of semantic
output coverage.

### 9. UNPROVEN — executable witness and mutation behavior

The private pipeline witness's static assertions match the VM contract already
established by the R1 adjudication: it inspects exact branch-local Maps/FnIds/U8
types and spells the accepted and rejected value/span/diagnostic shapes at
`crates/core/src/pipeline/compile/pipeline/tests.rs:13-123`. This is materially
better than R1's flat witness.

No test executed according to the author record. The reported native ARM link
SIGKILL has no separate raw log/environment/exit artifact in the R2 evidence
set. Under this review's no-build restriction, the event and all executable
behavior remain unproven. Static source inspection cannot grant GREEN,
determinism, or mutation credit.

## R2 claim disposition

| Author claim | Verdict |
|---|---|
| Typed all-or-nothing `Result` envelope | **Proved, but incomplete report and missing cancellation reason** |
| No external partial sidecar publication on `Err` | **Proved** |
| Differing duplicate NodeIds cannot silently overwrite | **Proved only across components; disproved within a component** |
| Zero-constraint exact factorized optimum | **Proved only for unique semantic sites and valid finite domains** |
| Constrained exact optimum | **Disproved by cost mutation on restore** |
| Budget best-so-far discarded | **Proved for `budget_exceeded`** |
| All early-stop best-so-far discarded | **Disproved for cancellation** |
| Unsatisfiability is typed | **Proved absent cancellation; cancellation can be mislabeled unsat** |
| Component indexing covers structurally shared nodes coherently | **Disproved** |
| Independent-minimum and priority fallbacks deleted | **Proved for runtime symbols/routes** |
| Complete mutation witnesses | **Disproved** |
| Production-isomorphic semantic/type witness | **Statically improved; executable result unproven** |
| Complete active census/prose closure | **Disproved** |
| Executable GREEN | **Explicitly absent** |

## Whole-W0 and tranche fit

R2 keeps the correct R1 grammar-egraph deletion and makes a sound architectural
move from tuple/fallback publication to a typed transaction. That direction is
KISS-forward. The live constrained solver nevertheless still has ambient
authority to publish a plan whose objective changed during search, and the
NodeId/occurrence mismatch can silently select a traversal-dependent result.
Those defects hit W0's central semantic gate directly.

W0 items 2, 4, and 5 remain open by the author's own routing. Item 3 is also not
complete because its retained CSP route has not won a bounded proof. There is no
basis to advance BB-C.W1 or claim any lock/tranche movement.

## Smallest cohesive R3

1. Preserve immutable canonical `(value, cost, order)` data across every prune
   and restore; add a round-trip invariant and exhaustive-reference constrained
   optimum test before touching broader optimizer design.
2. Allocate/reuse exactly one VarId and Site per `(NodeId, decision family)` in a
   component, or explicitly replace NodeId-keyed output with occurrence identity.
   Add same-rule and same-component shared-node reversal tests.
3. Add a distinct typed cancellation/timeout result, check it before any
   solution/unsat classification, and prove no best-so-far publication.
4. Return a compact deterministic component report sufficient to prove bounded
   language, installed constraints, complete coverage, objective/optimality, and
   the exact failing condition.
5. Replace injected-error/examples with actual born-RED mutation witnesses, and
   move all new tests out of production source into the isomorphic test tree.
6. Correct the active per-variable-fallback/per-rule/absent-module prose and run
   the separately authorized native Apple-ARM evidence pass once the static
   defects are repaired.

Until those repairs execute GREEN, survive two fresh hostile reviews, and are
freshly adjudicated, **BB-C.W0 R2 remains RED with zero credit**.
