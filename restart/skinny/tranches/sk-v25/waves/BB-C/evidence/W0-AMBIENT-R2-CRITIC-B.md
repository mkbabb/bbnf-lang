MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: hostile critic B.

# BB-C.W0 ambient optimizer authority — R2 hostile critic B

- Reviewed: `2026-07-22`
- Method: static, source-complete Apple-ARM-only audit; no Cargo command,
  executable, build, test, mutation runner, or x86 route was used.
- Frozen author record:
  `b98b7a54206d67f276ed132b34301d5b26ea2bdca547661dde8fff6668ab1244`
- Frozen primary implementation:
  `crates/ir/src/passes/csp_strategy/mod.rs` =
  `81c61d987ae71f0aa20c99dfd93e2e53ba1f75c18ffca9497acf381cbaaea6bd`
- Verdict: **R2 RED / REJECTED AS BB-C.W0 ITEM 3**.
- Credit: **zero BB-C.W0, wave, lock, tranche, or movement credit**.

The R2 deletion of the two named fallback families and its delayed pipeline
installation are sound narrow improvements. The claimed exact constrained
optimizer is nevertheless false: ordinary backtracking changes restored costs
to `1.0`, so the solver can rank a non-optimal assignment as optimal. The
NodeId transaction is also incomplete inside a component, the advertised
shared-NodeId witness cannot reach its expected error, cancellation is
misclassified or accepted as success, and no complete per-component report
exists. These are functional and proof defects independent of the unavailable
native executable.

## Binding subject and acceptance standard

The standard is not “fallback symbol absent.” W0 requires correct direct
semantics or a typed unavailable result, a declared bounded decision language,
validator, report, deterministic artifacts, and witnesses that invert
(`W0.md:3-19,21-33`). The R1 adjudication made the R2 item-3 obligation exact:
all-or-nothing solve publication, complete per-component reporting, typed
failure rather than best-so-far/fallback, exact factorized optimization only
after proof, and mutation witnesses for discarded constraints, budget, unsat,
and partial multi-component publication
(`W0-AMBIENT-R1-ADJUDICATION.md:234-272`).

The author's frozen file hashes match the primary R2 files sampled during this
review. Hash agreement identifies the subject; it is not correctness evidence.

## Disputed-claim matrix

| Author claim | Judgment | Source proof |
|---|---|---|
| constrained route proves a complete cost-minimal assignment | **False** | `StrategyDomain::remove` deletes the cost and `add` restores every value at `1.0` (`csp_strategy/mod.rs:325-343`); every search branch prunes, backtracks, and restores (`csp-solver/src/variable.rs:27-47,61-74`; `csp-solver/src/solver/search.rs:245-263`); scoring and bounds then read the mutated current-domain costs (`search.rs:375-397,405-448`) |
| missing DAG, budget, unsat, and invalid assignment are typed | **Narrowly true** | the four variants and public `Result` exist (`csp_strategy/mod.rs:239-268,394-464`), but cancellation/timeout is omitted and `InvalidAssignment` collapses distinct causes |
| every component succeeds before optimizer sidecars are installed | **True at the external pipeline boundary** | component output and grammar output remain local (`csp_strategy/mod.rs:435-464,467-498`); `install_grammar_solution` maps the error before either optimization result mutation (`pipeline/compile/pipeline.rs:328-356`) |
| hash-consed NodeId collisions cannot silently overwrite | **False** | the check exists only between already-collapsed `ComponentOutput`s (`csp_strategy/mod.rs:472-498`); site collection creates repeated variables for one NodeId and overwrites `by_node` (`927-991`), while local decoding writes the same NodeId repeatedly without equality checking (`851-924`) |
| the disconnected shared-node test proves collision rejection | **False; the test oracle is unreachable** | its fixture calls classification first (`cross_rule_csp.rs:25-71`); `preserve_identity` pins the shared NodeId globally to `MustTape` (`materialization/pin_sweep.rs:41-65,73-92`); both rule domains therefore contain only `MustTape` (`csp_strategy/mod.rs:1202-1246`), so no conflict reaches `merge_component`; `unwrap_err()` at `cross_rule_csp.rs:542-562` would panic if executed |
| cancellation/budget never publishes a best-so-far answer | **False for cancellation; true for budget** | the solver records cancellation separately (`csp-solver/src/config.rs:105-120`; `solver/search.rs:219-229`), but `solve_exact_constrained` checks only `budget_exceeded` (`csp_strategy/mod.rs:731-750`); cancellation before an incumbent becomes `Unsatisfiable`, while cancellation after one can return that unproved incumbent as success |
| result carries a complete per-component report | **False** | success contains only two maps; failure contains only a four-way reason and optional component root (`csp_strategy/mod.rs:239-268`); the sole detailed output is conditional `eprintln!` for budget (`654-685`), not a returned, complete, deterministic report |
| typed optimization failure reaches the public compile API uniformly | **False** | request APIs return `CompileError`, but `compile_grammar` and `compile_ast` stringify it into `Result<GrammarIR, String>` (`pipeline/compile/mod.rs:41-54,89-105`) |
| mutation coverage proves discarded-constraint and partial-publication safety | **False** | the “late component” test injects a fabricated `Err` directly into the installer after a successful compile (`pipeline/compile/pipeline/tests.rs:125-142`); it never creates an early successful component followed by a late solver failure, and no mutation/inversion artifact is present |
| tests obey the binding displaced/isomorphic structure rule | **False** | optimizer tests are embedded in production `csp_strategy/mod.rs:1394-1479`; pipeline witnesses sit under `crates/core/src/pipeline/compile/pipeline/tests.rs` and are included from production source (`pipeline.rs:454-459`), rather than in an isomorphic external test tree |
| independent-minimum budget fallback and unsat priority fallback are deleted | **True** | the named decoders/helpers are absent, budget maps to an error, and empty solution maps to unsat (`csp_strategy/mod.rs:731-750`) |
| zero-constraint path checks finite/aligned/non-empty domains and variable coverage | **True but insufficient** | `decode_assignments` checks vector alignment, finiteness, membership, and one visit per `VarId` (`851-901`); it does not check unique semantic `(NodeId, family)` ownership, so duplicate sites can still overwrite |
| production grammar-egraph write-back remains absent | **True narrow fact** | `pipeline/compile/pipeline.rs` runs the structural normalizer and no grammar-egraph build/extract/write-back edge in the audited production pass list |

## Critical finding 1 — backtracking corrupts the objective

`StrategyDomain` stores mutable parallel `values` and `costs` vectors
(`csp_strategy/mod.rs:291-310`). Pruning removes both entries with
`swap_remove` (`325-329`). Restoration does not recover the removed cost; it
appends the value with the literal cost `1.0` (`335-343`). The comment that
restoration “never introduces new ones” does not make the loss safe: the value
is old, but its cost is replaced.

This restoration is on the ordinary constrained-search path, not an unused
AC-3 corner. `Variable::restrict_to` logs every non-chosen value
(`csp-solver/src/variable.rs:61-74`); each search branch restricts the assigned
variable and every backtrack calls trail undo (`solver/search.rs:245-263`);
trail undo calls `Variable::restore`, which calls `Domain::add`
(`solver/mod.rs:48-59`; `variable.rs:38-47`). Branch-and-bound computes leaf
costs, optimistic bounds, and subsequent value order from the current mutable
domain (`search.rs:375-448`).

A two-variable counterexample is sufficient. Give `X={A:0,B:100}` and
`Y={C:10,D:0}`, with the hard implication `X=A => Y=C`. The true feasible
costs are `A+C=10` and `B+D=100`, so `A+C` is uniquely optimal. After the
`A` branch, backtracking restores `B` at `1.0`; the later `B+D` leaf is scored
as `1`, and can be returned first after best-first sorting
(`search.rs:453-515`; `csp_strategy/mod.rs:743-750`). R2 therefore publishes a
solution that is feasible but not cost-optimal while claiming exactness.

This defect also makes results dependent on branch history and initial value
order. Finite-cost validation before `finalize` (`csp_strategy/mod.rs:735-740`)
cannot detect it because `1.0` is finite.

### Minimum cohesive repair

Keep immutable canonical `(StrategyValue, cost)` authority and mutable
membership separately. `remove`/`add` must only toggle membership; `cost` must
always resolve through the immutable canonical table, and iteration must retain
canonical order. Do not attempt an undo-time side map or another fallback. Add
the counterexample above as a public-behavior born-RED witness plus a replay
under at least two legal variable/value orders; both must choose the same true
optimum and preserve all original costs after search.

## Critical finding 2 — one NodeId has multiple local authorities

The DAG contract explicitly hash-conses structurally equal, pointer-distinct
tree occurrences to one NodeId (`dag/mod.rs:59-83,120-136`; `dag/intern.rs:10-24`).
R2 does not make that identity authoritative inside a component:

1. `collect_sites` allocates a fresh variable for every occurrence, even when
   `(NodeId, decision family)` already exists, then overwrites the corresponding
   slot in `by_node` (`csp_strategy/mod.rs:927-991`).
2. `index_rule_vars` sees only the last overwritten variable for a NodeId
   (`703-729`). Constraints therefore do not necessarily bind every variable
   later decoded for that semantic node.
3. `decode_assignments` proves only one `Site` per `VarId`; it never proves one
   `VarId` per `(NodeId, family)` (`851-901`).
4. `assign_value` overwrites `alt_mode`, `wrap_mode`, `regex_engine`, or
   materialization for an already-written NodeId without comparing values
   (`904-924`).
5. `merge_component` can detect disagreement only after that local overwrite
   has erased it (`472-498`).

Materialization makes the failure concrete because its domain is rule-sensitive
(`rule.meta` and `debug_all`) while its output key is only NodeId
(`csp_strategy/mod.rs:574-590,1202-1246`). Put two structurally identical rule
bodies in one call-graph component, pin one rule, leave the other unpinned, and
set the shared initial class to the ordinary unpinned class. R2 creates two
materialization variables for the same NodeId; the factorized decoder can
choose different values and silently retain whichever rule is visited last.
Reordering rules changes output. Across disconnected components the later
merge would reject this; inside one component it is hidden.

The added disconnected witness does not even test the cross-component check.
Because `make_ir` runs `classify_materialization` and the pinned rule's subtree
shares the same NodeId as the plain rule, the pin sweep widens the sole shared
map entry to `MustTape`. Both domains then agree. The expected
`InvalidAssignment` is source-contradicted, just as R1's rejected VM oracle was.

### Minimum cohesive repair

Use exactly one CSP variable for each `(NodeId, decision family)` in a
component. On repeat occurrence, reuse the existing `VarId`; record one decode
site; index each rule occurrence to that same authority. Materialization needs
the same rule: if rule-local policy can legitimately disagree for one
hash-consed NodeId, either join those requirements into the single domain or
change the public identity model—never silently overwrite a NodeId-keyed map.
Add both same-rule repeated-node and connected cross-rule shared-node witnesses,
then reverse rule/child traversal and require byte-identical output.

Repair the disconnected witness by explicitly setting the shared initial class
after classification so the pinned and unpinned rule domains actually differ;
prove the merge returns typed unavailable and no maps escape.

## High finding 3 — cancellation is an untyped best-so-far route

`SolveConfig` exposes a cooperative cancellation token and `SolveStats` has an
independent `cancelled` bit (`csp-solver/src/config.rs:52-85,105-120`). The search
sets it and exits early (`solver/search.rs:219-229`). R2 checks only
`budget_exceeded` before accepting the first recorded solution
(`csp_strategy/mod.rs:731-750`). Consequently:

- cancellation before any leaf is mislabeled `Unsatisfiable`; and
- cancellation after a leaf returns an unproved best-so-far solution as exact.

Production currently constructs `cancel: None` through the default, so this is
not evidence of a current default compile firing cancellation. It is decisive
against W0's timeout/cancellation totality and against the general
`solve_exact_constrained` contract. Add a distinct typed `Cancelled` (and, when
time limits exist, `TimedOut`) reason, check it before consuming solutions, and
prove zero success payload for pre-incumbent and post-incumbent cancellation.

## High finding 4 — the required report does not exist

`GrammarSolveOutput` is two maps. `OptimizationUnavailable` is a reason and one
optional component root (`csp_strategy/mod.rs:239-268`). Neither carries:

- component membership;
- decision-language/domain coverage;
- installed hard-constraint identities/counts;
- validator result;
- nodes explored, budget/cancellation state, or completion proof;
- selected objective cost; or
- failure locus such as variable, node, expected family, or conflicting values.

The conditional budget `eprintln!` (`654-685`) is environment-dependent,
partial, and not returned to the caller. Collapsing missing values, mistyped
values, coverage failure, DAG translation mismatch, and NodeId conflicts into
`InvalidAssignment` is a typed bucket, not a complete report. R2 has therefore
not implemented the R1-adjudicated contract or W0's declared validator/report
gate.

The parsimonious design is one returned `ComponentReport` per attempted
component with a small closed status enum and exact counts/identities, included
on success and failure. It should be sufficient to reproduce the decision
without prose logs, not a second telemetry framework.

## High finding 5 — public typed-failure and test claims are split

`compile_grammar_request` and `compile_ast_request` preserve `CompileError`, but
the prominent `compile_grammar`/`compile_ast` wrappers stringify every error
(`pipeline/compile/mod.rs:41-54,89-105`). The direct R2 witness itself uses the
string-erasing wrapper (`pipeline/compile/pipeline/tests.rs:6,51-54`). Under the
clean-break/no-shim rule, the public compiler surface should return the
structured error or the masking wrappers should be removed; keeping dual typed
and string paths leaves typed unavailability non-general.

The late-publication test is also overfit. It first obtains an already
successful IR, fabricates `OptimizationUnavailable`, and passes it straight to
`install_grammar_solution` (`pipeline/compile/pipeline/tests.rs:125-142`). That
proves only `Result::map_err` occurs before two assignments. It does not exercise
an earlier successful component, a later genuine solver failure, local merge
rollback, or a discarded hard constraint. The shared-node test is invalid for
the reasons above. No mutation witness or inversion receipt exists, and no R2
test executed on the recorded host.

Finally, both new test islands violate the campaign's binding structure rule:
tests are inside the production source tree, including an inline test module in
the 1,479-line optimizer file (`csp_strategy/mod.rs:1394-1479`). Move behavior
witnesses into external trees isomorphic to their production owners. Do not
widen private production APIs merely to preserve white-box tests; test the
public solve/compile contract with grammars that force each failure.

## Correct narrow facts retained

The following R2 work is worth preserving in the repair, without movement
credit:

- `solve_grammar_components` now returns a `Result`, and missing DAG, exhausted
  node budget, empty solution set, and malformed decoded assignments have
  explicit reason variants.
- The old budget-to-independent-minimum decoder and unsat priority decoder are
  absent.
- Component outputs and the grammar aggregate remain local on failure; the
  pipeline does not install returned optimizer refinements until `Ok`.
- Cross-component differing NodeId values are compared rather than blindly
  overwritten, once the component output reaching that comparison is sound.
- The factorized route checks non-empty, aligned, finite domains, complete
  variable coverage, membership, and deterministic first-value tie behavior.
- The direct grammar witness now makes the two typed Maps and the current VM
  rejection contract materially more exact than R1's contradicted witness.
- Production grammar-egraph saturation/write-back remains absent and the two
  named CSP fallback families were not replaced by aliases or shims.

These facts do not overcome the corrupted constrained objective, local NodeId
overwrite, invalid witness, absent report, or absent executable evidence.

## Required R3, in dependency order

1. **Born-RED objective witness:** encode the `X/Y` constrained-cost
   counterexample and prove current R2 selects or scores the wrong assignment.
2. **Immutable cost authority:** separate canonical values/costs from active
   membership; prove prune/restore cannot change cost or order.
3. **Unique semantic variables:** reuse one variable per `(NodeId, family)`;
   make duplicate semantic writes impossible by construction and keep a checked
   decoder assertion as defense.
4. **Real alias witnesses:** cover same-rule duplicates, connected cross-rule
   duplicates, and disconnected duplicates with intentionally differing
   domains; reverse traversal and require invariant output or typed failure.
5. **Cancellation closure:** reject cancelled searches even with an incumbent;
   keep cancellation distinct from unsat and budget.
6. **Complete compact reports:** return per-component language/constraint/
   validation/completion facts on both success and failure.
7. **Public typed surface:** eliminate string masking for the optimizer failure
   path; no compatibility alias or dual route.
8. **External isomorphic tests and real mutations:** replace injected-error and
   inline/source-tree tests with public-behavior tests whose hard-constraint,
   late-failure, budget, unsat, cancellation, and cost mutations demonstrably
   invert.
9. **Only after static repair:** obtain one bounded native Apple-ARM compile/test
   receipt with exact command, environment, exit, and output hashes, then send
   the frozen R3 to two fresh hostile critics and a fresh adjudicator.

## Total-wave disposition

R2 does not complete item 3. Items 2, 4, and 5 also remain explicitly open; in
particular, there is no complete optimizer call-site inventory, no typed
e-graph saturation/extraction failure/report closure, and no accepted mutation
bank. BB-C.W1 must not begin on the strength of this subject.

**Terminal judgment: RED. Hostile clean count remains 0/2. CREDIT 0.**
