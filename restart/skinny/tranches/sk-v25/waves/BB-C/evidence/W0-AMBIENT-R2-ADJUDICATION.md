MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: fresh adjudicator.

# BB-C.W0 ambient optimizer authority — R2 adjudication

- Adjudicated: `2026-07-22T18:33:28Z`
- Method: independent frozen-source and call-route proof; no Cargo command,
  build, executable, mutation runner, x86 route, or non-BBNF repository was
  used.
- Terminal verdict: **R2 RED / REJECTED AS BB-C.W0 ITEM 3**
- Hostile clean count: **0/2**
- Credit: **zero BB-C.W0, wave, lock, tranche, or movement credit**
- Advancement: **BB-C.W1 may not begin**

R2 preserves several good, narrow changes: the two named failure fallbacks are
deleted, missing DAG/budget/unsat/malformed assignment have typed cases, and a
failed grammar solve cannot externally publish earlier component maps. It does
not prove a production-safe optimizer. Ordinary constrained backtracking
changes `StrategyDomain` costs and order, cancellation has no typed outcome,
the returned result has no component reports, duplicate hash-consed semantic
sites are not validated as one authority, the new disconnected collision test
cannot reach its oracle, and the claimed mutations are examples rather than
inverting mutation witnesses. These are technical RED findings, not ownership
or host blockers.

## Exact authority and frozen subject

| Governing record | SHA-256 |
|---|---|
| `waves/BB-C/W0.md` | `00bb99d6b3c379c7c15ad7a70ab7598f8b883f9cd8d3974733323bdbc6026df2` |
| `W0-AMBIENT-R1-ADJUDICATION.md` | `64aaa76d74c2ea533d43c656a5f793695dbc584c6c9cbbcf317446a670216a63` |
| `W0-AMBIENT-R2-AUTHOR.md` | `b98b7a54206d67f276ed132b34301d5b26ea2bdca547661dde8fff6668ab1244` |
| `W0-AMBIENT-R2-CRITIC-A.md` | `e17b8e736655cc5ee7d746b9e37ab5b1112ab33f1f29e852f8791ff594f75117` |
| `W0-AMBIENT-R2-CRITIC-B.md` | `b197f50fca87842e67df9b718febdf26ffe2013006f8afe2753c6bf7aea29630` |

The exact 21-file R2 subject is the path/hash manifest in the frozen author
record above. Every listed hash was independently recomputed and matched. The
principal implementation and publication identities are:

| Subject | SHA-256 |
|---|---|
| `crates/ir/src/passes/csp_strategy/mod.rs` | `81c61d987ae71f0aa20c99dfd93e2e53ba1f75c18ffca9497acf381cbaaea6bd` |
| `crates/ir/src/passes/csp_strategy/components.rs` | `07a235d333992da0f70c6ff5e27e4b036828c0026bf531ba3f9414c3a910131a` |
| `crates/ir/src/passes/mod.rs` | `ef505f512d129d358992b910b575a02f4c84b0adc455adcecf701bdcafd0b46d` |
| `crates/core/src/pipeline.rs` | `5eac07248f09df5539f5265e2c9f011c6c9e793d3d0049d39dba764b109b82c4` |
| `crates/core/src/pipeline/compile/pipeline.rs` | `bc02d7ea79fe4c39765b164b491d040f233470df9fbd9de8b8370748e7a676ab` |
| `crates/core/src/pipeline/compile/pipeline/tests.rs` | `bff62ebf9b01db50e46b291a793b55884c4ca8f0c7f99d0d68a0a0b449cdffe8` |
| `crates/ir/tests/vm/cross_rule_csp.rs` | `6ae94ebcf215790869d86e2a24958faa437655f6a6b638a1a90d9e452561041f` |

The solver and identity substrate used to adjudicate those bytes was also
content-bound:

| Proof anchor | SHA-256 |
|---|---|
| `crates/csp-solver/src/variable.rs` | `4f0bd9f90a9996e1904e05c53fd025cc50ab114bc91c1f724d1471e874a2fdc7` |
| `crates/csp-solver/src/solver/mod.rs` | `65d0dec8d167b31c998e01bffd080c6d1f833939b98b93e882399040610b4f35` |
| `crates/csp-solver/src/solver/search.rs` | `fee69885ba75c022fe478dddac586647e1e7d660276a3ce3180440de7ffe62ef` |
| `crates/csp-solver/src/config.rs` | `d1402e54a8e3835b0d053ce129893cb92b0da0275ba576133d15051568a90c24` |
| `crates/csp-solver/src/csp/solve.rs` | `eadbaad5783fb2dec73c2447e4d133de47f79d8d746be57ced6303bcf9f755b4` |
| `crates/csp-solver/src/domain/traits.rs` | `66d1a8813306e8394ee3bba71c0fec4fcc6672de857f9dce4281736fecc8bd97` |
| `crates/csp-solver/src/ordering.rs` | `8f1e1cbc2ee47cf8e93339a11d99f3fb1134d068327b1a85e527a74c8b34f5dd` |
| `crates/csp-solver/src/constraint/implication.rs` | `b3a575bd9d176adaac93e651e5a02a8957c5142424a0ac5c13a0dcc0b4ee2ad4` |
| `crates/ir/src/dag/mod.rs` | `46b7ab5abc6648347f839c8008764be3737ad8f33c2f9d58c1fa7b7f470e101f` |
| `crates/ir/src/dag/intern.rs` | `6918ab3ca6bcd084c603a60e378f24587bff1a3538b6dea4d34038718e38e15e` |
| `crates/ir/src/passes/materialization/pin_sweep.rs` | `a8bc43c4b2efb2c43a38e222f6c5f77197d2321f7d582193eb7757c66e31c7a8` |
| `crates/core/src/pipeline/compile/mod.rs` | `28508f3ef3087626a590e90099a55c855c0f38bc814abb8ed99cba6c719f165d` |

The author's `+569/-438` tracked delta and 142-line untracked private witness
recompute to the stated complete `+711/-438`. Scoped tracked `git diff --check`
is clean. These are identity and formatting facts, not correctness evidence.

## Claim-by-claim adjudication

| Claim | Judgment | Proof |
|---|---|---|
| typed all-or-nothing solve envelope | **Correct but incomplete** | `solve_grammar_components` returns `Result<GrammarSolveOutput, OptimizationUnavailable>`; the error has four reasons and an optional component root, but neither success nor error carries the required component reports |
| no optimizer sidecar publication on `Err` | **Correct** | component and grammar accumulators remain local; `install_grammar_solution` unwraps the complete result before assigning either sidecar, and the remaining two writes are infallible |
| budget and unsat fallbacks are deleted | **Correct** | the former independent-minimum and priority decoder symbols/routes are absent; budget and empty solution set return typed reasons |
| constrained route is exact and cost-minimal | **Incorrect** | every search restriction removes canonical costs and every undo restores removed values at invented cost `1.0`; later scoring and bounds read the changed objective |
| constrained ties/order are deterministic under the original domain | **Incorrect** | `swap_remove` plus append changes order as well as cost during ordinary backtracking |
| factorized zero-constraint route is exact | **Correct only for its declared per-variable objective** | it validates nonempty/aligned/finite domains, one site per VarId, membership, and complete VarId coverage, and strict `<` preserves the first active value on ties; it does not validate one semantic authority per `(NodeId, family)` |
| differing shared NodeIds cannot overwrite | **Correct only across component outputs; incorrect as a general statement** | `merge_component` compares cross-component values, but collection and decode can create and then overwrite multiple same-component variables keyed to one semantic output |
| disconnected shared-node witness reaches `InvalidAssignment` | **Incorrect** | classification and `preserve_identity` pin the one shared NodeId globally to `MustTape`; the supposedly plain rule therefore also receives a singleton `MustTape` domain, so the two component outputs agree and `unwrap_err()` is unreachable |
| budget best-so-far is discarded | **Correct** | `budget_exceeded` is checked before the returned vector is consumed |
| cancellation is fully typed and cannot masquerade as another result | **Incorrect** | `SolveStats::cancelled` is never checked and there is no cancelled/timed-out reason |
| complete per-component report exists | **Incorrect** | success is two maps; failure is a reason plus optional root; environment-dependent `eprintln!` text is not a returned report |
| production/type/VM witness is corrected | **Statically correct, executable result unproven** | the private test checks exact arm-local Maps, distinct FnIds, typed constants/U8 projection, both acceptances, and the R1-adjudicated rejection shape; no test executed |
| mutation coverage is complete | **Incorrect** | the tests are ordinary examples; the late-publication test fabricates an `Err` after a successful compile and no mutation/inversion artifacts exist |
| tests obey displaced source-isomorphic layout | **Incorrect** | optimizer tests remain inline in `src/passes/csp_strategy/mod.rs`; pipeline tests live under `src/pipeline/...`; both violate the controlling no-colocation rule |
| public typed failure is preserved | **Split** | `compile_grammar_request` and `compile_ast_request` preserve `CompileError::OptimizationUnavailable`; convenience `compile_grammar` and `compile_ast` stringify it, so preservation is not uniform |
| active authority prose/census is closed | **Incorrect** | the author's three exact censuses return no rows, but they omit live contradictory prose in `csp_strategy/components.rs` and the parent module's references to absent `parent`/`tier` installers and a per-rule solve |
| executable GREEN exists | **Incorrect by the author's own record** | both reported Apple-ARM commands ended before any test executed; no separate raw receipt is in the R2 artifact set |

## Finding 1 — constrained search changes its own objective

This is proved, but both critics' numerical example is wrong.

`StrategyDomain` stores active values and costs in parallel vectors. Its
`remove` uses `swap_remove` on both; its `add` appends the restored value with
literal cost `1.0`. `Variable::restrict_to` records every removed value, the
search restricts a variable on every branch, and `Trail::undo_to` calls
`Variable::restore`, which calls that `add`. Branch-and-bound then reads the
mutated domain for value ordering, complete-assignment cost, and optimistic
bounds. This is the ordinary constrained route, not a dead reset helper.

The critics used `X={A:0,B:100}`, `Y={C:10,D:0}`, with `A => C`, and claimed
the algorithm could prefer `B+D`. That conclusion is **incorrect**: the true
costs are `A+C=10` and `B+D=100`, so `A+C` is already the true optimum; after
the complete undo sequence the later `B+D` branch is not scored as the claimed
single unit either.

A valid counterexample uses `X={A:0,B:2}` with the same
`Y={C:10,D:0}` and implication `A => C`. Under the default fail-first tie,
variable 0 is selected first and values are cost-sorted:

1. `X=A` removes `B`; the forbidden cheaper `Y=D` branch removes/restores
   `C`, changing `C:10` to `C:1`.
2. The feasible `A+C` leaf is therefore scored as `1`, although its immutable
   objective is `10`.
3. Undo restores `B` at `1`; after the child loop both `Y` values have also
   been restored at `1`.
4. A feasible `B+D` leaf is scored at least `2`, although its immutable
   objective is `2`.
5. Best-first sorting can therefore return `A+C`, the wrong immutable optimum.

Even without that corrected witness, the loss of canonical cost and order is
itself sufficient to disprove exactness. Pre-solve finiteness checks cannot
detect an invented finite `1.0`.

## Finding 2 — NodeId identity is not validated as one local authority

The structural mechanism identified by both critics is **correct**:

- the DAG explicitly maps pointer-distinct equal occurrences to one `NodeId`;
- `collect_sites` nevertheless allocates a new variable for every Alt, Wrap,
  or Engine occurrence, then overwrites `by_node[NodeId].family`;
- each rule root also receives a new materialization variable even when its
  root NodeId is shared;
- constraint indexing resolves only the last stored strategy variable;
- `decode_assignments` proves one site per VarId, not one VarId per semantic
  `(NodeId, family)`; and
- `assign_value` overwrites a same-component output without comparing the
  earlier value. Only the later cross-component merge checks equality.

This makes the claimed validator and decision-language report incomplete and
allows an externally supplied, sidecar-inconsistent `GrammarIR` to collapse
different same-component materialization assignments silently.

The critics overstate the demonstrated live consequence. Their proposed
connected materialization fixture does **not naturally produce differing
domains after the production classifier**: pinning either occurrence writes
`MustTape` to the single shared NodeId, so the unpinned occurrence reads the
same initial `MustTape`. Likewise, identical strategy occurrences derive equal
base domains, and the current global variable maps target the last occurrence,
which is also the last decoded output. A traversal-dependent wrong production
decision from valid pipeline-built IR is therefore **unproven in R2**, not
proved by the critics.

That narrowing does not validate R2. The public solve accepts a `GrammarIR`,
its output is NodeId-keyed, and its report gate must prove whether NodeId or
occurrence is the semantic unit. It currently proves neither. The smallest
coherent contract is one variable per `(NodeId, decision family)` and a joined
legality/domain requirement across every occurrence; if occurrence identity is
actually intended, the public output key must change rather than silently
collapse it.

### Disconnected witness oracle

The added test is statically contradicted. `make_ir` builds the shared DAG and
runs `classify_materialization`. `apply_consumer_pins` sees the
`preserve_identity` rule and writes its subtree's shared literal NodeId to
`MustTape`. The second rule reads that same `MustTape` initial class. Its
non-pinned materialization domain therefore contains only `MustTape`, just like
the pinned rule. Both singleton components merge successfully; the test's
`unwrap_err()` would panic if it ran.

## Finding 3 — cancellation is untyped, but publication is narrower than claimed

`SolveConfig` carries a cancel token, search records cancellation separately in
`SolveStats::cancelled`, and `solve_exact_constrained` checks only
`budget_exceeded`. Therefore:

- cancellation before an incumbent returns no solution and is mislabeled
  `Unsatisfiable`; and
- cancellation after an incumbent is not identified as cancellation before
  the solution vector is returned.

Both critics call the second case a proven best-so-far publication. That is
**too broad at the grammar-output boundary**. The search's early `Done` path
also bypasses undo. Any observed post-incumbent cancellation occurs after a new
branch value was restricted, so the subsequent decoder normally rejects the
old incumbent as no longer in the current domain and returns
`InvalidAssignment`. The base solver did return best-so-far, but successful
grammar sidecar publication from that path is unproven. Either outcome still
fails W0: cancellation is reported as unsat/invalid rather than a distinct
typed early-stop result, and no completion proof exists.

No timeout is currently present in `SolveConfig`; R3 must not invent a timeout
variant until a real time limit exists. It must add `Cancelled` now and reserve
`TimedOut` for an actual bounded-time input.

## Finding 4 — external transaction is sound; returned proof is absent

The external all-or-nothing route is real. Each component output and the
grammar accumulator are local. A later `?` discards the accumulator, and the
core pipeline unwraps the complete `Result` before either map is installed.
Although `merge_component` can partially mutate its local accumulator before
finding a conflict, no caller can observe it on `Err`. The two final map writes
are infallible.

The required report is nevertheless absent. Neither result form records the
component members, decision families/domains, constraint identities/counts,
coverage, validation outcome, search completion status, objective, or failing
site. An optional component root is not a complete component report, and
conditional stderr is not an artifact. R2 therefore does not satisfy W0's
declared bounded-language/validator/report gate or the explicit R1 disposition.

## Finding 5 — factorized route and typed API, precisely bounded

For the CSP's actual per-variable additive objective, the zero-constraint path
is exact: every variable has one site, every domain is nonempty/aligned/finite,
the chosen value belongs to its domain, every VarId is covered once, and strict
comparison makes equal costs retain the first active value. It never calls the
broken prune/restore path.

It is not yet a complete semantic-output proof because multiple variables can
collapse to one NodeId/family output. The route can be retained after unique
semantic authority and report closure; it is not a failure fallback.

Typed error preservation is also split. The request APIs return
`CompileError::OptimizationUnavailable`, so the core pipeline does propagate a
typed result. The convenience `compile_grammar`/`compile_ast` wrappers erase it
to `String`. This does not negate the narrow transaction, but it prevents a
claim of one uniform public typed surface under the campaign's clean-break
rule.

## Finding 6 — witnesses, mutations, structure, and evidence

The new direct semantic/type witness is materially corrected relative to R1:
it checks each arm's exact Map placement, distinct FnId, exact typed constant,
U8 projection, both accepted inputs, and the already-adjudicated rejection
shape without using VM Map erasure as typing evidence. Static shape is
**correct**; executable behavior remains **unproven** because the recorded
native runs stopped before any test executed.

The late-publication test is not a multi-component failure witness. It compiles
successfully, fabricates an error, and passes it directly to the installer. The
inline tests similarly exercise examples, not mutants whose removed hard
constraint, accepted partial result, or restored fallback demonstrably reaches
output and inverts. W0 item 5 remains open.

The test layout is bindingly wrong. New tests are embedded in the 1,479-line
production optimizer module and under `crates/core/src/pipeline/...`; the
campaign requires an external tree isomorphic to source. Existing sand does
not authorize more sand.

The author's exact legacy-phrase and fallback-symbol censuses do return no
rows. They are not a complete active authority census. In particular,
`components.rs` still promises per-variable fallback on pathological
components, while the parent module describes a per-rule solve and absent
`parent`/`tier` installers. Those comments do not create executable routes, but
they invalidate report/prose closure.

## Retained R2 facts

Retain, without movement credit:

- the R1 production grammar-egraph call-edge deletion;
- deletion of the budget independent-minimum and unsat priority fallback
  implementations, with no aliases or dual paths;
- the typed `Result` envelope and typed missing-DAG/budget/unsat/malformed
  reasons;
- grammar-local and pipeline-external transactional publication;
- checked cross-component equality once component output itself is valid;
- the factorized decoder's shape, finiteness, coverage, membership, and stable
  first-active tie checks;
- the corrected static direct semantic/type witness; and
- the author's explicit zero-credit/non-blocking posture.

Reject or leave unproven:

- constrained exactness or deterministic original-cost ordering;
- a complete NodeId/occurrence authority contract;
- the disconnected collision test oracle;
- typed cancellation/timeout totality;
- complete returned component reports;
- actual mutation/inversion coverage;
- source-isomorphic test placement;
- uniform public typed-error preservation;
- executable GREEN; and
- completion of BB-C.W0 item 3 or any later W0 item.

## Smallest cohesive R3, in dependency order

R3 must repair the live item-3 CSP slice without expanding into G3, a new
optimizer family, package/lock changes, or BB-C.W1.

1. **Land born-RED objective witnesses outside `src/`.** Add a domain
   round-trip witness proving every non-unit cost and canonical order survives
   prune/restore. Add the corrected constrained problem
   `A:0/B:2`, `C:10/D:0`, `A => C`; compare the solver's selected assignment
   and immutable score with exhaustive enumeration under at least two legal
   variable/value orders. It must fail on R2 before repair.
2. **Make cost authority immutable.** Store canonical ordered `(value, cost)`
   entries once and mutable active membership separately. `remove`/`add` may
   toggle membership only; `cost`, `values`, and iteration must consult the
   immutable table in canonical order. No restore-time side map, invented
   default cost, or fallback.
3. **Choose and enforce one semantic identity.** For the present NodeId-keyed
   output, allocate one VarId/Site per `(NodeId, decision family)` per
   component and join every occurrence's legality into that domain. Keep a
   checked decode assertion against duplicate semantic writes. Add same-rule,
   connected cross-rule, and deliberately conflicting disconnected fixtures;
   reverse rule/child traversal and require identical output/report or a typed
   conflict. Repair the existing disconnected fixture by explicitly restoring
   a lower shared initial class after classification so pinned and unpinned
   domains genuinely disagree.
4. **Close cancellation with deterministic born-RED witnesses.** Add
   `OptimizationUnavailableReason::Cancelled`, inspect `stats.cancelled`
   before budget/solution classification, and discard every solution. Prove
   pre-incumbent and post-incumbent cancellation with a deterministic
   cancel-on-Nth-search-event harness, not sleeps. Add `TimedOut` only when a
   real deadline exists.
5. **Return a compact deterministic component proof.** Each attempted
   component must report root/members, counts by decision family, installed
   constraint families/counts, semantic-site coverage, solver completion
   status, nodes/budget/cancellation, objective on success, and an exact failure
   locus/status on error. Include completed and failing reports on failure;
   avoid a telemetry framework or environment-only logger.
6. **Replace examples with real external mutations.** In a test tree
   isomorphic to `passes/csp_strategy` and `pipeline/compile`, prove inversion
   for cost restoration, discarded hard constraint, budget, unsat,
   cancellation, malformed/incomplete assignment, and a genuine early-success
   plus late-component failure. Remove the inline and `src/` test modules.
7. **Close the typed surface and active prose.** Preserve the structured error
   through the canonical public compile APIs with one path, not typed/string
   dual behavior. Correct the per-variable-fallback, per-rule, and absent
   `parent`/`tier` descriptions. Publish separate exact censuses for runtime
   routes, report coverage, and active authority prose.
8. **Only after the static repair**, run one separately authorized native
   Apple-ARM evidence pass with exact command/environment/exit/output
   identities. No retry loop and no x86 route. Freeze R3, then require two
   fresh hostile critics and a fresh adjudicator.

Items 1–7 are one dependency-ordered item-3 repair: immutable optimization
authority, unique semantic identity, total early-stop typing, a returned proof,
and witnesses that actually invert. None grants W0 completion. W0 items 1, 2,
4, and 5 still require their own accepted evidence after item 3 becomes clean.

**Terminal judgment: R2 RED. CREDIT 0. BB-C.W1 MAY NOT BEGIN.**
