MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: hostile R3 critic B.

# BB-C.W0 ambient optimizer authority — hostile R3 critic B

- Reviewed: `2026-07-22`
- Method: independent frozen-byte source, test, grammar, lowerer, solver, and
  consumer audit. I did not read critic A, run Cargo, execute a binary, touch an
  x86 route, or inspect another repository.
- Verdict: **R3 RED / NOT CLEAN**
- Credit: **zero BB-C.W0, wave, lock, tranche, or movement credit**
- Advancement: **BB-C.W1 may not begin**

The CSP repair is materially better than R2. Immutable costs now survive
prune/restore; one `(NodeId, family)` site is built per component; repeated
occurrences intersect legality; decode and cross-component merge reject
duplicate/conflicting publication; and cancellation is checked before an
incumbent is consumed. Those facts are retainable, but they do not close R3.
The only canonical pipeline witness is RED on valid grammar, the R2-ordered
variable-order and mutation proofs were not supplied, the production compile
path discards every successful component report, and the 1,885-line optimizer
module is a new god-module violation rather than a Goldilocks transposition.

## Frozen authority and subject

The supplied author and native identities recompute exactly:

| Record | SHA-256 |
|---|---|
| `waves/BB-C/W0.md` | `00bb99d6b3c379c7c15ad7a70ab7598f8b883f9cd8d3974733323bdbc6026df2` |
| `W0-AMBIENT-R2-ADJUDICATION.md` | `ef7c8c1db203eb42ae40358e35fd2710d4e79d3cabc5495801c619b18995b9f7` |
| `W0-AMBIENT-R3-AUTHOR.md` | `8a327e3da122dd253494f8512e2ca6504ba61107fbf9eeef935c62393ff3afe7` |
| `W0-AMBIENT-R3-NATIVE.md` | `b4fd9be7815a4af19d8f103f454e8395330758faaef85e86da8ac109e7f67f01` |

The fourteen frozen R3 source/test hashes also match the author manifest. The
principal identities independently used below are:

| Path | SHA-256 |
|---|---|
| `crates/ir/src/passes/csp_strategy/mod.rs` | `7fdbaaf5790e09d6f7a22568b91ecadaf54e9f29f4ec4730ab250d59f4084897` |
| `crates/ir/src/passes/csp_strategy/components.rs` | `35d23889c56888e61dc873843e446790f066dd151a3cd144a06ac3d345d7648b` |
| `crates/ir/tests/passes/csp_strategy/mod.rs` | `e217c090f529932015fb59f91efdfb82fbf02140a0571fd2c44723392a7029b9` |
| `crates/ir/tests/vm/cross_rule_csp.rs` | `2bff8d52dc0b9464218e33ff544d239c37ca21473cab76ee49f488358c3c2f10` |
| `crates/core/src/pipeline/compile/pipeline.rs` | `ee917eeb17347c49f8eb908015f002ac4b4a4bc5bda4f896a3865815f82c3ca8` |
| `crates/core/tests/pipeline/compile/pipeline.rs` | `49b4d09c961512b24b030555dfde07f8182f434df5e5d424912e50c9c193aea9` |
| `grammar/bbnf/bbnf.bbnf` | `eef788e29119b7eb88444e1dd0675bc744a1802468914e9cb402d78c9c267a57` |
| `grammar/bbnf/expressions.bbnf` | `f7740762b03790c3ba45ed6839e8ba9bc2a50b5f1eb353f1206195b2e06baf14` |
| `crates/core/src/lower/expression/wrap.rs` | `a626639367f2c7d3d45dfaa3f75fa7603871e28831c4fd117466aa9c622a2805` |
| `crates/csp-solver/src/variable.rs` | `4f0bd9f90a9996e1904e05c53fd025cc50ab114bc91c1f724d1471e874a2fdc7` |
| `crates/csp-solver/src/solver/search.rs` | `fee69885ba75c022fe478dddac586647e1e7d660276a3ce3180440de7ffe62ef` |

`git diff --check` is clean for the frozen Rust subject. This proves identity
and formatting only.

## Claim matrix

| R3 claim | Judgment | Exact proof |
|---|---|---|
| canonical costs/order survive prune and restore | **Correct** | `StrategyDomain` separates immutable `entries` from mutable `active` (`csp_strategy/mod.rs:390-453`); `remove`/`add` only toggle bits (`:460-490`), while solver undo calls `Domain::add` (`csp-solver/src/variable.rs:38-47`) |
| constrained search returns the immutable minimum | **Correct in the audited synthetic/algorithmic path, but the required variable-order proof is incomplete** | scoring and bounds read immutable costs; the test reverses value tables, but all three named orderings select the same first variable in the symmetric 2x2 fixture (`tests/passes/csp_strategy/mod.rs:24-74,135-155`) |
| repeated occurrences have one semantic authority | **Correct per component** | registry key is `Site(NodeId,family)` and repeat registration intersects the existing domain (`csp_strategy/mod.rs:1073-1180`); exactly one variable is then created per entry (`:799-832`) |
| repeated legality is conjoined | **Correct** | intersection retains only values active in both domains and rejects bit-different shared costs (`:425-445`); the variable is created only after all occurrences are collected (`:755-824`) |
| duplicate local writes and cross-component conflicts cannot silently publish | **Correct** | `set_once`/materialization insertion reject duplicate local writes (`:1347-1406`); sorted merge compares every occupied slot/class (`:641-698`); the caller returns `Err` before exposing its local accumulator (`:574-626`) |
| cancellation is distinct and discards best-so-far | **Correct for constrained search** | `stats.cancelled` is checked before budget and solution consumption (`:993-1023`); pre/post-incumbent witnesses are deterministic (`tests/passes/csp_strategy/mod.rs:217-288`) |
| factorized exact route is complete | **Correct for its zero-constraint additive objective** | it validates domain/site shape, membership, single coverage, and finite accumulated objective (`csp_strategy/mod.rs:1225-1345`) |
| returned reports are complete production proof | **Incorrect** | `GrammarSolveOutput` carries reports (`:213-219`), but `install_grammar_solution` installs only decisions/materialization and drops `solved.reports` (`core/pipeline/compile/pipeline.rs:349-356`); `GrammarIR` has no report sidecar/consumer |
| component members are sorted and report artifacts deterministic | **Incorrect as stated / general proof absent** | report copies `member_rules` verbatim (`csp_strategy/mod.rs:737-753`); partition buckets preserve `ir.rules` insertion order, not sorted `RuleId` order (`components.rs:269-278`), despite the author claiming sorted members |
| mutation obligations from the R2 disposition are closed | **Incorrect** | the only hard-constraint “mutant” substitutes a true lambda in a hand-built fixture (`tests/passes/csp_strategy/mod.rs:24-60,148-155`); there is no mutation runner or inversion artifact for discarded partial results, cancellation, budget, malformed assignment, or publication |
| canonical typed compile surface is proved | **Partly correct** | signatures now return `CompileError` (`core/pipeline/compile/mod.rs:40-54,88-105`), but the only passing API test induces a parse error, not `CompileError::OptimizationUnavailable` (`core/tests/pipeline/compile/pipeline.rs:122-127`) |
| direct typed-arm/VM product witness is valid | **RED** | native receipt records panic before optimizer execution (`W0-AMBIENT-R3-NATIVE.md:52-76`); source proof below shows valid grammar exposed a lowerer defect |
| native gate evidence is independently replayable from retained bytes | **Unproven** | the receipt retains command, asserted result, and a transcript hash but no transcript/log artifact; the hash alone cannot be recomputed from repository bytes (`W0-AMBIENT-R3-NATIVE.md:16-20,22-60`) |
| Goldilocks structure is preserved | **Incorrect** | `csp_strategy/mod.rs` is 1,885 lines and now owns public vocabulary, domain representation, solve orchestration, reporting, registry/join, decode, constraint wiring, domain construction, and projection; the constitution explicitly rejects god modules (`LOCK-CONSTITUTION.md:29-56,446-464`) |

## Finding 1 — the native panic is a product lowerer defect, not invalid syntax

The failing source is:

```text
branch = ("x" -> 0u8), "z" | ("y" -> 1u8), "z" ;
```

It is admitted by the canonical grammar. `term` includes parenthesized `rhs`
(`grammar/bbnf/bbnf.bbnf:29-36`); `rhs` reaches `mapped_factor`; and
`mapped_factor` admits `factor -> value_expr` (`:41-52`). `0u8` is admitted by
`int_lit` (`grammar/bbnf/expressions.bbnf:5-7`). The arrow therefore belongs to
the mapped factor *inside* the parenthesized term. The outer mapped factor has
no mapping payload, which is exactly why the panic reports one structural child.

The lowerer itself documents that direct mapping is determined from structural
children (`lower/expression/wrap.rs:43-70`) and correctly lowers the grouped
term recursively (`:202-206`). It then contradicts that structure: when the
outer `value_expr_head` is absent, it scans the outer compound's entire source
span for any unquoted arrow and panics (`:235-250`). That source span includes
arrows nested in parenthesized `rhs`, so the invariant mistakes a descendant's
valid mapping for a missing direct mapping.

This is not an optimizer failure and it is not invalid test syntax. It is a
general lowering defect for a valid grouped mapped expression. Merely deleting
the parentheses from this witness would hide the defect. The owning repair is
to make direct mapping presence structural/depth-aware: a nested arrow cannot
assert a direct `mapped_factor` payload. Add an external lowerer regression for
grouped mapped factors and retain the direct ungrouped missing-payload
invariant. Until then the canonical product proof is RED.

## Finding 2 — the R2-ordered hostile proof matrix was narrowed

R2 required comparison with exhaustive enumeration under at least two legal
**variable and value** orders. R3 reverses each value table and loops over
`Chronological`, `FailFirst`, and `Mrv`, but both variables have domain size two
and the same constraint degree (`tests/passes/csp_strategy/mod.rs:24-74`). All
three legal selectors therefore tie back to the first inserted variable. No
fixture swaps variable insertion or forces the second variable first. The
immutable-domain implementation is convincing, but the demanded trajectory
counterexample was not actually exercised.

R2 also required real external mutations. The R3 test toggles one synthetic
implication into an always-true lambda. That is a useful counterexample, not an
exhaustive mutation campaign, and it does not invert publication for the other
typed stops. W0 item 5 remains open by the author record, but the R2
adjudication explicitly made these proofs part of the cohesive item-3 repair;
R3 cannot silently narrow them.

## Finding 3 — successful production rewrites discard their proof

The solve API produces `GrammarSolveOutput { decisions, materialization,
reports }`. The production compiler then unwraps the result, installs the two
semantic sidecars, and drops `reports` (`pipeline.rs:328-339,349-356`). No
`GrammarIR` field or canonical compile output retains the successful component
proof. Error reports survive through `CompileError::OptimizationUnavailable`,
but successful semantic rewrites do not carry their declared validator/report
to any consumer or artifact.

That is narrower than the author claim that conditional logging “is no longer
the report authority.” At the production boundary there is no successful
report authority at all. The smallest repair is not telemetry: retain the
compact report as one optimizer-proof sidecar or return it in the canonical
compile artifact, and add a consumer-facing test that observes it. If the
intended contract is intentionally ephemeral, the W0 gate and author claim
must be narrowed and adjudicated rather than calling the dropped value a
production report.

## Finding 4 — report determinism is overstated

Constraint installers and merge inputs are sorted, which is good. Component
member reports are not: `member_rules.to_vec()` preserves the partition
bucket's `ir.rules` insertion order. The source comment and author freeze call
that list sorted. Site/variable order is also first-occurrence traversal order,
and the objective is accumulated as `f64` in that order (`csp_strategy/mod.rs:
1255-1328`). R3 has no general permutation witness for independent sites or
rule insertion. The two cross-rule tests reverse selected children/references,
not the full valid ordering surface.

This does not show a wrong optimizer choice in the frozen native tests, but it
does disprove the report's stronger canonical/sorted claim. Canonicalize report
members and semantic-site ordering by `(NodeId, family)` before assigning
`VarId`s; then compare complete output and report bytes under rule/site
permutations. Accumulate/report objective in that canonical order.

## Finding 5 — structure and warning debt violate the binding product plane

The R3 implementation expanded the already-large optimizer into a 1,885-line
module. Its concerns are independently nameable and already have natural
encapsulation boundaries. This is precisely the god-module condition the
constitution makes part of every implemented-wave review. The answer is not a
directory of one-function sand files. A Goldilocks split is:

1. `domain.rs` — decision vocabulary, immutable domain, and domain builders;
2. `report.rs` — output/error/report vocabulary;
3. `site.rs` — registry, intersection, and decode/coverage validation;
4. `solve.rs` — component/grammar transaction and exact solve dispatch;
5. existing `constraints/` and `components.rs` remain their current owners;
6. `mod.rs` becomes the narrow public façade plus regex decision projection.

Names inside the module omit `csp_strategy` stutter, and external tests remain
under the existing source-isomorphic tree. This is five cohesive owners, not
sand.

The native receipt also records two unused imports in the frozen R3 public
witness (`cross_rule_csp.rs:18-20`). Remove them. The other 183 warnings are
not content-addressed by raw output and are predominantly outside this slice;
they must be routed to their owners, not laundered into an R3 fix or used to
justify ignoring the two R3-local warnings.

## Retainable facts and explicit remainder

Retain without movement credit:

- immutable canonical cost entries with active-bit prune/restore;
- finite/duplicate-domain validation and overflow rejection at decode;
- one per-component semantic site per `(NodeId, family)`;
- legality intersection and exact cost disagreement rejection;
- checked local decode and checked cross-component merge;
- typed budget, unsat, cancellation, missing-DAG, and invalid-assignment
  outcomes on the audited routes;
- factorized zero-constraint exact decoding;
- public structured compile signatures; and
- native receipt claims of 9/9 optimizer and 10/10 cross-rule tests as useful
  but not independently replayable evidence.

Still RED or unproved:

- the canonical core pipeline witness;
- valid grouped mapping lowering;
- a retained successful production report;
- the required second variable-order/exhaustive proof;
- real mutation/inversion closure;
- canonical report ordering;
- independently replayable native transcripts;
- warning-clean frozen R3 witnesses;
- Goldilocks module structure; and
- all BB-C.W0 items other than this still-unaccepted item-3 slice.

## Dependency-ordered minimal repair

1. **Fix the valid grouped-mapping lowerer defect at its owner.** Replace the
   descendant-wide source-arrow assertion with direct structural/depth-aware
   mapping detection. Add external source-isomorphic positive and negative
   regressions; do not weaken the typed-map invariant or rewrite the witness to
   dodge the grammar form.
2. **Complete the exactness matrix.** Swap variable insertion/semantic-site
   order (not only value order), compare against exhaustive enumeration, and
   retain the immutable cost/order round trip.
3. **Close mutation truth.** Add bounded source-external mutation/inversion
   specimens for removed hard constraints, partial/late publication, budget,
   cancellation, malformed assignment, and fallback resurrection. No new
   process framework is needed.
4. **Retain the successful proof.** Carry `ComponentReport` through the
   canonical compile artifact/IR sidecar, or obtain an explicit adjudicated
   narrowing of the W0 report gate. Prove both success and
   `OptimizationUnavailable` through the public API.
5. **Canonicalize report order.** Sort members and semantic sites before VarId
   allocation/report accumulation; test complete reports under legitimate
   rule/site permutations.
6. **Perform the Goldilocks split and remove the two unused imports.** Preserve
   public names and behavior without aliases, wrappers, or duplicate paths.
7. **Freeze a new R4 subject, then run one Apple-ARM-only campaign.** Retain raw
   stdout/stderr plus exit/environment/command hashes so the receipt is
   independently recomputable. No retry loop and no x86 route.
8. **Require two fresh hostile critics and a fresh adjudicator.** R3 remains
   RED; even a clean item-3 R4 does not accept W0 items 1, 2, 4, or 5.

**HOSTILE R3 CRITIC B VERDICT: RED. CREDIT 0. CLEAN COUNT 0/2. BB-C.W1 MAY NOT BEGIN.**
