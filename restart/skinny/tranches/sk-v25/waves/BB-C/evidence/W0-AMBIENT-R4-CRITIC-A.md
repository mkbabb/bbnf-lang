MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: fresh hostile R4 Critic A.

# BB-C.W0 ambient optimizer authority — R4 hostile Critic A

- Reviewed: `2026-07-22`
- Method: frozen-byte review of the controlling wave and R3 adjudication, both
  R4 receipts, every cited production/test owner, raw GREEN and mutant logs,
  all seven mutation patches, and the restoration manifests. I recomputed the
  frozen hashes, line counts, and byte counts. I did not run Cargo, a build,
  a test executable, a benchmark, or a mutant; I did not inspect Critic B; and
  I did not edit production or test source.
- Terminal verdict: **R4 RED / NOT CLEAN / ITEM 3 NOT ACCEPTED**
- Critic-A clean vote: **NO**
- Credit: **zero BB-C.W0, wave, lock, tranche, or movement credit**
- Advancement: **BB-C.W1 remains barred**

The optimizer transaction is materially better than R3. Canonical immutable
costs, exact constrained and factorized stopping, checked decode, local-only
publication, a single configured solve surface, deterministic report ordering,
structured errors, the seven-owner split, and all seven named mutation kills
are real. They do not overcome the terminal valid-language lowerer defect or
the missing proof at several cancellation publication boundaries.

## Findings — severity first

### [P1 / terminal] The direct-arrow scanner is not lexical-context aware; valid division loses an outer typed RHS, and a valid block comment can manufacture a direct arrow

**Evidence.** `crates/core/src/lower/expression/wrap.rs:66-99` treats every `/`
byte exactly like a quote delimiter (`:73-75`). That is valid only when the
slash starts a regex. The imported value language expressly admits division:
`grammar/bbnf/expressions.bbnf:20-27` defines `/` as `mul_op`. The canonical
grammar also admits a parenthesized mapped factor and an outer map at
`grammar/bbnf/bbnf.bbnf:29-45`.

For the valid source

```text
subject = ("x" -> input / 2) -> 1u16 ;
```

the scanner correctly ignores the inner arrow while `closers` contains `)`,
then mistakes the division slash for the start of a regex. With no later `/`,
`skip_escaped_delimited` advances to EOF, so the real outer arrow is never
returned. This is not cosmetic source detection. Typed numeric leaves can have
empty spans under the current projection, as the owner documents at
`crates/core/src/lower/expression/wrap.rs:155-169`; the suffix fallback at
`:650-675` calls `direct_map_rhs(parent_source)`. The false `None` therefore
leaves the outer `MapExpr::IntLit(1)` without its exact `TypeDesc::U16`.

There is a dual false positive. `grammar/bbnf/bbnf.bbnf:17,39` permits block
comments around a factor. In valid source such as

```text
subject = /* a/b -> c */ "x" ;
```

the opening comment `/` is treated as a regex opener, the embedded `/` after
`a` is treated as its closer, and the comment's `->` is then visible at depth
zero. With no structural value child, `lower_mapped_factor` panics through the
fail-loud branch at `crates/core/src/lower/expression/wrap.rs:297-312`.

The five external tests at `crates/core/tests/lower/expression/wrap.rs:59-121`
cover nested groups, literal/regex arrows, and a simple typed map, but neither
division before an outer arrow nor comment lexical state. The raw G3 transcript
confirms exactly those five tests and no more at
`W0-AMBIENT-R4-GREEN-3.log:1127-1134`.

**Claim verdict.** The author claim at `W0-AMBIENT-R4-AUTHOR.md:121-135` that
the predicate is direct-depth aware and preserves exact scalar types is
**incorrect as a general valid-language claim**. The native claim at
`W0-AMBIENT-R4-NATIVE.md:39-40` that G3 proves direct-depth lowering and exact
typed placement is **unproven beyond its five fixtures**. This is an incomplete
R4-owned repair of the pre-existing grouped-map defect, not unrelated legacy
debt; R3 expressly made this repair and product witness part of R4 at
`W0-AMBIENT-R3-ADJUDICATION.md:371-374,414-421`.

**Exact repair.** Replace the byte-delimiter heuristic with a lexical-state
scanner that distinguishes regex starts, value-expression division, block
comments, escaped quotes, and balanced `()[]{}` while still reporting only an
arrow owned by the current mapped factor. Keep structural children as the only
Map-construction authority and keep the genuine direct-arrow/missing-child
panic. Add external tests for (1) a nested division expression before a genuine
outer typed arrow, asserting exact inner/outer Map placement and outer
`TypeDesc`; (2) a block comment containing both `/` and `->`, asserting no map
or panic; and (3) the existing quote, regex, delimiter, and direct-missing-child
controls. Freeze a fresh native lowerer and unchanged product witness.

### [P1 / evidence gate] The cancellation implementation has the ordered checks, but the R4 oracles do not prove the factorized or later grammar publication boundaries

**Evidence.** The implementation contains real checks before the component
loop, after component return, before final success, before component work,
before factorized acceptance, before constrained acceptance, and after solver
search at `crates/ir/src/passes/csp_strategy/solve.rs:87-100`, `:133-204`,
`:375-381`, `:502-558`, and `:602-631`. That source inspection is favorable.

The tests do not discriminate several of those checks:

- `crates/ir/tests/passes/csp_strategy/mod.rs:221-254` pre-cancels its alleged
  factorized grammar. It is intercepted by the first outer-loop check at
  `solve.rs:133-145`; deleting the actual factorized-acceptance check at
  `solve.rs:512-518` would leave this oracle green.
- `crates/ir/tests/vm/cross_rule_csp.rs:550-580` also pre-cancels before the
  call. Its own name and assertions say cancellation occurs at the **first**
  boundary and return one zero-work boundary report. It does not exercise a
  completed component followed by the post-component/between-component check,
  nor the final-success check at `solve.rs:194-203`.
- M05 mutates `solve_exact_constrained` and is killed by the final-singleton
  solver test; it does not invert the factorized, post-component, between-
  component, or final-grammar checks.

R3 required cancellation before accepting either solve kind, between
components, and before grammar success at
`W0-AMBIENT-R3-ADJUDICATION.md:361-370`, with configured factorized and mixed-
component report witnesses at `:398-413`. Static presence is not the same as a
mutation-sensitive publication proof.

**Claim verdict.** The author's implementation claim at
`W0-AMBIENT-R4-AUTHOR.md:97-105` is **correct by source inspection**. Its narrow
test descriptions at `:107-110` are literally correct. The broader native
claim at `W0-AMBIENT-R4-NATIVE.md:36-40` that G1/G2 prove cancellation across
the public transaction is **unproven at the masked boundaries**.

**Exact repair.** Add deterministic oracles, or one-hunk boundary mutants, that
would fail if each of `solve.rs:512-518`, `:173-181`, and `:194-203` were
removed. The mixed-component case must retain a completed report followed by
the exact cancelled boundary/failing report and expose no output. Avoid timing-
dependent thread races; the proof needs a deterministic cancellation seam.

### [P2 / report contract] A late merge failure is labeled `InvalidAssignment` while retaining a success objective

**Evidence.** On a merge error,
`crates/ir/src/passes/csp_strategy/solve.rs:182-190` changes `status` and
`failure` but does not clear `objective`. The
same owner explicitly clears the field for cancellation at `:240-245`, and the
controlling ordered report meaning calls it the **success objective** at
`W0-AMBIENT-R3-ADJUDICATION.md:168-177`. The external late-conflict test checks
status and locus but omits objective at
`crates/ir/tests/vm/cross_rule_csp.rs:631-653`.

The raw M07 execution makes the state independently visible. At
`W0-AMBIENT-R4-MUTANT-M07.log:17`, the second report has
`status: InvalidAssignment`, a merge `failure: Some(...)`, and
`objective: Some(...)`. M07 changes only the transaction return after that
report is formed; it does not create the inconsistent fields.

**Claim verdict.** Sorted membership and unchanged schema are correct, but the
author's report-closure claim at `W0-AMBIENT-R4-AUTHOR.md:137-150` is
**incorrect under the controlling success-objective meaning**. No frozen
contract separately defines this as a local pre-merge objective.

**Exact repair.** Clear `report.objective` when merge changes the report to
`InvalidAssignment`, and assert `None` alongside the existing exact status and
locus checks. If a local pre-merge objective is desired, it requires a newly
adjudicated schema/meaning; silently overloading the success field is not the
smallest R4 repair.

### [P2 / native proof scope] Functional GREEN covers four selectors, not every changed caller in the frozen 24-path subject

**Evidence.** The author freezes modified integration callers
`crates/ir/tests/lattices/csp_materialization.rs` and
`crates/ir/tests/lattices/csp_authority.rs` at
`W0-AMBIENT-R4-AUTHOR.md:59-60`. They are attached only by
`crates/ir/tests/lattices/main.rs:8-10`. None of G1-G4 invokes the `lattices`
integration binary: G1 is the IR library subset, G2 is `--test vm`, and G3/G4
are core library subsets (`W0-AMBIENT-R4-NATIVE.md:27-42`). Thus those two
changed callers were formatted and hashed but not type-checked by the frozen
native campaign. The modified `cost_weights_unified.rs` does compile as part of
the VM binary, but all its tests are among G2's 62 filtered tests
(`W0-AMBIENT-R4-GREEN-2.log:14-28`).

**Claim verdict.** “G1-G4 passed with the recorded cardinalities” is
**correct**. “Functional native GREEN” is **correct only for those selectors**;
GREEN for the complete author-frozen source/test subject is **unproven**. This
is a proof-coverage defect, not evidence of a compile failure and not an
independent product bug.

**Exact repair.** After the source repairs, include native Apple-ARM lattice
selectors that compile and execute the retained materialization/authority
witnesses and execute the retained cost-weight tests relevant to the migrated
public solve call. Freeze the full commands, raw bytes, exits, and hashes in the
fresh campaign.

### [P3 / R4 regression] The Goldilocks move introduced an unused import

**Evidence.** `crates/ir/src/passes/csp_strategy/mod.rs:125` imports
`crate::dag::NodeId` but the facade does not use it. Raw G2 records the exact
warning at `W0-AMBIENT-R4-GREEN-2.log:2-10`; G3 and G4 repeat it at
`W0-AMBIENT-R4-GREEN-3.log:2-11` and
`W0-AMBIENT-R4-GREEN-4.log:1-10`. The born-RED core transcript predates the
split and does not contain this facade warning.

**Claim verdict.** The author's narrow claim that `PrettyHints` and
`RuleDirectives` were removed is **correct**. A warning-clean R4 is
**incorrect**, but the native receipt explicitly says so at
`W0-AMBIENT-R4-NATIVE.md:44-52`; it does not conceal the defect.

**Exact repair.** Delete the unused import before the next freeze. Do not fold
the unrelated `bbnf` warning bank into this repair.

## Integrity and identity audit

| Frozen authority or subject | Recomputed result | Verdict |
|---|---|---|
| `waves/BB-C/W0.md` | `00bb99d6b3c379c7c15ad7a70ab7598f8b883f9cd8d3974733323bdbc6026df2` | exact |
| `W0-AMBIENT-R3-ADJUDICATION.md` | `0453ac333914204a3597693838906471999fcdcce77c7fd518be55071151b7fb` | exact |
| `W0-AMBIENT-R4-AUTHOR.md` | `7e917e2eedbeba69027231358a86091f5cddd0b5e4c394a1f38e70aeb15e7364` | exact expected SHA |
| `W0-AMBIENT-R4-NATIVE.md` | `707b91f245c7e739a5eedcff50d523909a1c22361c138f509f9fcc885f9ca12c` | exact expected SHA |
| Author's 24 Rust source/test rows | 24/24 SHA-256, line, and byte triples recompute | exact |
| Born-RED author/native, 3 RED logs, static log | every listed digest/size recomputes | exact |
| 4 GREEN logs and 7 mutant logs | every listed digest/size recomputes | exact |
| 7 patches, patch list, manifest, apply-check, restoration list | every listed digest/size recomputes | exact |
| Four restored mutation targets | domain `8a8e07ad...6271`; CSP `be015fe5...a3e3`; solve `02cf11b8...9581`; site `5b7b002a...68b9` | exact final restoration |
| `Cargo.lock` | `480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9` | exact |

There is no identity mismatch, stale digest, line-count mismatch, byte-count
mismatch, or final-source restoration failure on the frozen subject.

## Claim-by-claim verdicts

| Material author/native claim | Verdict | Hostile ruling |
|---|---|---|
| R4 is confined to reopened W0 item 3 plus its authorized grouped-map product blocker | **Correct** | R3 explicitly scopes that repair at `W0-AMBIENT-R3-ADJUDICATION.md:350-355`; it does not invade items 1, 2, 4, or 5 |
| One public configured `solve_grammar_components`, two control fields, explicit production defaults, no wrapper/`_with_config`/`TimedOut` | **Correct** | `solve.rs:25-42,76-86`; all frozen call sites use the two-argument surface |
| Cancellation checks exist at all ordered implementation boundaries | **Correct** | source contains the checks and consistently returns typed `Cancelled` without output |
| R4 tests/native prove every cancellation publication boundary | **Unproven** | the factorized and mixed-component fixtures are masked by the first outer pre-check; later boundary removals are not killed |
| Budget exhaustion is classified before consuming an incumbent | **Correct** | `solve.rs:615-631`; M03 semantically kills the inversion |
| Unsat has no independent per-variable fallback | **Correct** | empty solution returns typed `Unsatisfiable`; M04 semantically kills the fallback |
| Exact decode rejects wrong length, foreign IDs, wrong families, inactive values, duplicate writes, uncovered variables, and non-finite objectives | **Correct in implementation and baseline tests** | `site.rs:231-339`; G1 runs all baseline assertions |
| Immutable prune/restore cost/order and hard custom constraint authority survive | **Correct** | retained R3 implementation plus semantic M01/M02 kills support the claim |
| Grammar and pipeline publication is all-or-nothing | **Correct for maps/output** | accumulators remain local; core unwraps at `pipeline/compile/pipeline.rs:355-362` before either sidecar write; M07 kills `Ok` publication |
| Every failure report obeys the ordered report meaning | **Incorrect** | late merge failure retains `objective: Some` while status is `InvalidAssignment` |
| Direct-arrow detection handles valid regex/division/comment/grouping forms and exact typed RHS recovery | **Incorrect** | P1 supplies two valid lexical counterexamples directly implied by source |
| Report members and boundary members are sorted and fixed-input execution is deterministic | **Correct** | `solve.rs:219-220,355-356`, component roots `components.rs:193-208`, and merge inputs `solve.rs:260-315` are ordered; no permutation-canonical claim is needed |
| The report schema remains the adjudicated family/count shape with no edge IDs or compile sidecar | **Correct structurally** | no unauthorized schema expansion occurred; the objective-state defect is semantic, not a demand for a new field |
| All four canonical compile entrypoints preserve `CompileError`; optimizer failure is structured and sidecars remain unchanged | **Correct at the exact ordered seams** | signatures are statically bound at `crates/core/tests/pipeline/compile/pipeline.rs:61-76`; the source-external transaction test at `:150-198` exercises the exact private installer seam R3 ordered; DAP stringifies only at its protocol boundary |
| Tests are physically external/source-isomorphic and production owners contain only path attachments | **Correct** | the three `#[path]` attachments are at `csp_strategy/mod.rs:203-205`, `wrap.rs:802-804`, and `pipeline.rs:464-466`; R3 did not require public black-box integration tests |
| Goldilocks owners moved once, old bodies are absent, and public names are direct re-exports | **Correct** | domain/report/site/solve/token/components/facade ownership matches the exact R3 split; no duplicate solve body or compatibility wrapper remains |
| The facade/API is newly overexposed by R4 | **Incorrect criticism** | public `components`, public `constraints`, `ConstraintCtx`, domain values, and partition re-exports already existed before this move; reducing that API was not authorized by R3 |
| Warning cleanup is complete | **Incorrect** | the two named R3 imports are gone, but the move adds the `NodeId` warning; native correctly records warning-clean false |
| G1-G4 are native Apple-ARM GREEN with the stated counts and no retry/x86 route | **Correct for the frozen selectors** | raw logs reproduce 12, 12, 5, and 3 passes and their filters; the full 24-path subject is not all natively exercised |
| Seven exact named counterfactual patches were semantically killed | **Correct, with the M06 reach qualifier below** | every log reaches its named test and fails semantically rather than at compile time |
| All mutation targets are restored now | **Correct** | current hashes equal `SOURCE-RESTORATION.sha256` exactly |
| Each intermediate apply/reverse/hash transaction is independently replay-bound by raw bytes | **Unproven** | the native receipt attests the six-step transaction, but mutant logs contain only Cargo output; final restoration is independently verified |
| R4 accepts W0 item 5 or completes W0 | **Correctly not claimed** | these seven specimens close only the R2/R3 item-3 prerequisite; W0 items 1, 2, 4, and 5 remain open |
| Credit remains zero and BB-C.W1 cannot begin | **Correct** | a native GREEN receipt cannot award movement, and this hostile review is not clean |

## Seven-mutant and restoration challenge

| ID | Patch/log ruling | Restoration ruling |
|---|---|---|
| M01 invented restore cost | **Killed correctly**: the named test observes the altered canonical cost/order invariant | final `domain.rs` hash exact |
| M02 discarded custom hard constraint | **Killed correctly**: the named exhaustive oracle reaches the different unconstrained optimum | final CSP source hash exact |
| M03 consumed budget incumbent | **Killed correctly**: an existing incumbent escapes as `Ok` instead of typed budget failure | final `solve.rs` hash exact |
| M04 unsat per-variable fallback | **Killed correctly**: the contradictory singleton problem becomes illicit `Ok` | final `solve.rs` hash exact |
| M05 ignored solver/current-token cancellation | **Killed correctly for the constrained final-leaf seam**; it does not prove every grammar publication boundary | final `solve.rs` hash exact |
| M06 incomplete/foreign/malformed decode bundle | **Killed correctly at the first incomplete-input assertion**. The raw log at `W0-AMBIENT-R4-MUTANT-M06.log:7-17` aborts there, so the oversized and foreign-ID hunks are not dynamically reached; their individual mutation necessity is **unproven**, although the unmutated G1 baseline separately exercises and rejects all cases | final `site.rs` hash exact |
| M07 publish after late merge failure | **Killed correctly**: the raw test sees an illicit `Ok` output. The same output exposes the separate success-objective/report defect | final `solve.rs` hash exact |

The overall “7/7 exact named mutants killed” statement is fair. It must not be
inflated into proof of every cancellation boundary, every M06 sub-hunk, or the
wave-wide W0 item-5 consumer inventory.

## Pre-existing debt versus R4-owned defects

| Observation | Classification and disposition |
|---|---|
| 183 `bbnf` warnings in G3/G4 | **Pre-existing debt.** The born-RED transcript already contains the bank; do not charge it to R4. |
| Unused `NodeId` in the new facade | **R4 regression.** The move introduced it; remove it. |
| Descendant-wide grouped-map panic in R3 | **Pre-existing defect with an incomplete R4 repair.** R3 explicitly transferred ownership to R4, so the remaining division/comment failure blocks this round. |
| Late-merge report objective | **Retained item-3 contract defect, not shown to be newly introduced by R4.** It is nevertheless inside the reopened report/transaction subject and must be repaired before acceptance. |
| Public `components`/`constraints`, `ConstraintCtx`, `StrategyDomain`, `UnionFind`, and partitioning | **Pre-existing API breadth.** R4 preserved direct exports as ordered; API pruning would be an unauthorized compatibility change here. |
| Non-dense/out-of-range public `GrammarIR.rules[*].id` can reach `UnionFind::find` after the first partition loop skips it (`components.rs:251-278`) | **Pre-existing malformed-IR boundary debt.** Canonical pipeline IR maintains dense IDs, and W0 item 2 still owns general translation/typed-failure closure; do not expand item-3 R4 around it. |
| The retained engine-propagation test uses an `if let` that does not fail if either projected decision is absent (`cross_rule_csp.rs:445-460`) | **Pre-existing oracle weakness outside the new R4 proof claims.** It belongs with the later full constraint/call-site inventory, not this smallest repair. |

## Total tranche and wave fit

The controlling W0 still requires deletion of G3, typed optimizer failures and
reports, removal of ambient e-graph writeback/independent fallback, a complete
optimizer call-site inventory, and wave-wide mutations
(`waves/BB-C/W0.md:8-28`). Prior adjudications already accepted the production
grammar-egraph deletion and interpreted the live component CSP as a bounded
item-3 island whose exact factorized branch is not the deleted unsound fallback.
R4 correctly follows that controlling interpretation; demanding deletion of
all CSP work now would contradict R3.

R4 also correctly avoids claiming later items. It does not close W0 item 1
(G3 census/deletion evidence), item 2 (the complete typed stop taxonomy), item
4 (all optimizer consumers and failure behavior), or item 5 (the wave-wide
mutation inventory). The lowerer work and Goldilocks move are not scope creep:
both were expressly ordered by R3. The required lexical repair, report fix,
cancellation proof, warning deletion, and fresh native evidence remain within
that same smallest item-3 boundary.

## Dependency-ordered hostile repair

1. Repair `find_direct_arrow` with real lexical context for regex, division,
   comments, quotes, escapes, and grouping; add the exact division/comment/
   typed-RHS counterexamples while preserving structural Map authority and the
   direct-missing-child panic.
2. Clear the objective on late merge failure and assert the complete exact
   failure report, including `objective: None`.
3. Make the factorized, post-component/between-component, and final-success
   cancellation boundaries deterministically mutation-sensitive, with exact
   completed-plus-cancelled report assertions and no output.
4. Remove the R4-local unused `NodeId` import. Keep the pre-existing warning
   bank out of scope.
5. Refreeze every changed source/test identity, then run a fresh one-shot native
   Apple-ARM campaign covering the repaired G1-G4 gates plus the changed
   lattice callers and relevant cost-weight witnesses. Preserve full command,
   environment, stdout/stderr, exit, digest, and restoration preimages.
6. Require two fresh hostile reviews and a fresh adjudication. Do not reuse this
   RED review as a clean vote, award credit, open another W0 item, or begin W1.

**HOSTILE VERDICT: R4 RED. CRITIC A IS NOT CLEAN. BB-C.W0 ITEM 3 IS NOT
ACCEPTED. CREDIT 0. BB-C.W1 MAY NOT BEGIN.**
