MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: fresh R3 adjudicator.

# BB-C.W0 ambient optimizer authority — R3 adjudication

- Adjudicated: `2026-07-22T19:27:00Z`
- Method: independent frozen-byte source, test, grammar, lowerer, consumer,
  transaction, and solver inspection. No Cargo command, build, executable,
  benchmark, mutation runner, x86 route, source edit, or other repository was
  used.
- Terminal verdict: **R3 RED / REJECTED AS BB-C.W0 ITEM 3**
- Hostile clean count: **0/2**
- Credit: **zero BB-C.W0, wave, lock, tranche, or movement credit**
- Advancement: **BB-C.W1 may not begin**

R3 retains substantial real repairs: immutable canonical costs and order,
exact completed constrained minimization, one component-local authority per
`(NodeId, decision family)`, legality intersection, checked decode and merge,
typed constrained cancellation, all-or-nothing external publication, the
structured public compile signatures, and external test placement. The
critics also identify a real valid-grammar lowerer defect, a configured
factorized-cancellation hole, missing mutation closure, two unused imports,
non-replayable native transcripts, and a structural ownership failure.

Several critic conclusions are too broad. The production public optimizer
cannot currently be passed a cancellation token: public
`solve_grammar_components` always constructs `SolveConfig { cancel: None,
.. }`, while `solve_grammar_components_with_config` is private. R3 therefore
does not prove that a *production pre-cancelled* grammar publishes; it proves
that the internal configured grammar transaction violates its own cancellation
contract and that the public cancellation surface is absent. The R2 disposition
required constraint-family/count reporting, not a reconstructable list of every
constraint edge and scope, and it required success reports to be returned by
the optimizer, not retained in the canonical compiled `GrammarIR`. R3 does both
of those required things. Finally, the alleged missing second variable order is
false: `Chronological` selects the last stack variable, whereas `FailFirst` and
`Mrv` select the first variable in the 2x2 witness. R3 exercises both variable
orders and both value-table orders against exhaustive enumeration.

The terminal RED does not depend on those overclaims. The only native core
pipeline witness failed on valid canonical syntax before reaching the optimizer;
the mapped-factor assertion is descendant-depth-blind. The private configurable
transaction ignores cancellation on its factorized branch and has no public
configuration carrier. A further source-proved seam, missed by both critics,
exists when the token becomes cancelled during the final complete assignment:
the search checks `stack.is_empty()` before checking cancellation and the wrapper
checks only `stats.cancelled`, not the token's final state, so a last-leaf
incumbent can return successfully. R3 also does not supply the complete
R2-ordered mutation/inversion evidence, its native output hashes have no frozen
preimages, and its 1,885-line optimizer module is not an acceptable permanent
owner.

## Frozen authority and subject

| Governing record | SHA-256 |
|---|---|
| `waves/BB-C/W0.md` | `00bb99d6b3c379c7c15ad7a70ab7598f8b883f9cd8d3974733323bdbc6026df2` |
| `W0-AMBIENT-R2-ADJUDICATION.md` | `ef7c8c1db203eb42ae40358e35fd2710d4e79d3cabc5495801c619b18995b9f7` |
| `W0-AMBIENT-R3-AUTHOR.md` | `8a327e3da122dd253494f8512e2ca6504ba61107fbf9eeef935c62393ff3afe7` |
| `W0-AMBIENT-R3-NATIVE.md` | `b4fd9be7815a4af19d8f103f454e8395330758faaef85e86da8ac109e7f67f01` |
| `W0-AMBIENT-R3-CRITIC-A.md` | `138c59a8674cd7211fee9c22f950ff674592743dd4bac8ae86531c8560726d7c` |
| `W0-AMBIENT-R3-CRITIC-B.md` | `b4fbcce0430dd346a0c9d9eb05574028ace077ef738895cab818df3c40a542ed` |

Every one of the author's fourteen frozen R3 source/test hashes recomputes
exactly:

| Frozen path | SHA-256 |
|---|---|
| `crates/ir/src/passes/csp_strategy/mod.rs` | `7fdbaaf5790e09d6f7a22568b91ecadaf54e9f29f4ec4730ab250d59f4084897` |
| `crates/ir/src/passes/csp_strategy/components.rs` | `35d23889c56888e61dc873843e446790f066dd151a3cd144a06ac3d345d7648b` |
| `crates/ir/src/passes/csp_strategy/constraints/dispatch.rs` | `6a546ea458d390c3dde6791d295cf2f71a224cec26dbf8943171a2e9df1658ce` |
| `crates/ir/src/passes/csp_strategy/constraints/engine.rs` | `f90090f6d7e809ff616d30dcd68a11908157b3ce94d4a80945cfbff5628aa000` |
| `crates/ir/src/passes/csp_strategy/constraints/layout.rs` | `58c8ecf3d55ea75b8a52277d1f495e406b068adfce652007779bd11c2e7a04b4` |
| `crates/ir/src/passes/csp_strategy/constraints/mod.rs` | `f4c4197f4b78ef8ea02ee7be5a815f9f523ddbb8091c57ae641a5c8c35aae646` |
| `crates/ir/src/passes/csp_strategy/constraints/shape.rs` | `24e2a2347cf555d9aafeba89b0857ca6c5bab4035895dae89fff6055c4bf8634` |
| `crates/ir/src/passes/mod.rs` | `6805c17691a53997354fc818b34e85d8fb60da9799132a8c526e2b0d7b6120f9` |
| `crates/ir/tests/passes/csp_strategy/mod.rs` | `e217c090f529932015fb59f91efdfb82fbf02140a0571fd2c44723392a7029b9` |
| `crates/ir/tests/vm/cross_rule_csp.rs` | `2bff8d52dc0b9464218e33ff544d239c37ca21473cab76ee49f488358c3c2f10` |
| `crates/core/src/pipeline/compile/mod.rs` | `cb8685c44438c825de70e6efcdb0298c8fc4f4b6c7c8c9b5589c5a13a470a3b5` |
| `crates/core/src/pipeline/compile/pipeline.rs` | `ee917eeb17347c49f8eb908015f002ac4b4a4bc5bda4f896a3865815f82c3ca8` |
| `crates/core/tests/pipeline/compile/pipeline.rs` | `49b4d09c961512b24b030555dfde07f8182f434df5e5d424912e50c9c193aea9` |
| `crates/lsp/src/dap/adapter.rs` | `984ef143367e89de9cae75009cbc000f5b9c3aaa824f2c0c25e674dfe26b9625` |

The retained public error dependency is still
`crates/core/src/pipeline.rs` at
`5eac07248f09df5539f5265e2c9f011c6c9e793d3d0049d39dba764b109b82c4`.
The inspected canonical grammar, lowerer, consumer, and solver anchors also
match the identities recorded by the critics. Identity and clean formatting are
not correctness evidence.

## Claim-by-claim adjudication

| Claim or criticism | Judgment | Frozen-source ruling |
|---|---|---|
| canonical costs and value order survive prune/restore | **Correct** | `StrategyDomain` keeps immutable ordered entries and mutable active bits; `remove`/`add` toggle bits and `cost` reads the canonical table |
| completed constrained search returns the finite additive minimum | **Correct on the audited route** | branch-and-bound reads immutable costs; cancellation and budget are rejected before its returned vector is consumed; decode rechecks family, membership, coverage, and finite objective |
| R3 omitted a second variable-order/exhaustive proof | **Incorrect critic claim** | `Chronological` chooses `stack.len()-1` (`Y` first), while tied `FailFirst`/`Mrv` choose index zero (`X` first); forward/reversed value tables are each compared with the exhaustive unique optimum |
| one component-local authority exists per `(NodeId, family)` | **Correct** | `SiteRegistry` keys by `Site`, intersects repeated occurrences before variables are created, and decode rejects duplicate semantic writes |
| shared legality and cost disagreement fail closed | **Correct** | domains are intersected; bit-different costs for a shared value are invalid; an empty joined domain is unsatisfiable |
| factorized zero-constraint selection is an exact additive optimum | **Correct absent cancellation** | it proves nonempty valid domains, membership, one coverage per variable/site, finite objective, and stable first-canonical tie selection |
| cancellation is first-class on every grammar path | **Incorrect** | only constrained search inspects solver cancellation; the factorized branch does not inspect the token, and neither component nor grammar acceptance has a token boundary check |
| a production pre-cancelled grammar is proved to publish | **Overclaimed** | the configurable grammar function is private and the public production entry constructs `cancel: None`; externally reachable production publication is not proved |
| the cancellation contract has no other hole | **Incorrect; additional defect** | a token set during the final complete assignment can remain absent from `stats.cancelled` because the leaf executes before the guard and no later nonempty search frame need observe it; the wrapper never checks the token's final state |
| no partial optimizer sidecar is externally published on `Err` | **Correct** | component and grammar accumulators are local; core unwraps the full result before two infallible sidecar mutations |
| budget and unsat do not publish the former fallback | **Correct** | budget and empty constrained solution return typed failures; the factorized route is an exact solve, not the deleted independent-minimum fallback |
| report must list exact constraint identities/scopes | **Incorrect as a terminal R3 demand** | W0 requires a bounded language, validator, and report; ordered R2 item 5 specifically requires installed constraint *families/counts*. The five typed family counts meet that clause. R2 did not require reconstructing every physical implication/pin edge from the report |
| successful reports must survive on `GrammarIR` or the public compile artifact | **Incorrect as a terminal R3 demand** | R2 required the optimizer to return completed/failing reports. `GrammarSolveOutput` returns every success report and `OptimizationUnavailable` returns completed plus failing reports. Neither W0 nor R2 requires a durable compile-artifact report sidecar |
| R3 has no production report at all because core drops success reports | **Factually narrow but dispositively wrong** | core does drop `solved.reports`, but the production decision transaction ran through a returned structured proof before installation. Adding a new IR sidecar would expand, not satisfy an omitted controlling clause |
| R3's report satisfies the ordered R2 field set | **Correct, with one prose/order qualification** | it contains root/members, family/site/domain-value counts, family constraint counts, coverage, completion, node/budget/cancel flags, objective, and exact failure locus; completed and failing reports are returned on error |
| report members are explicitly sorted | **Incorrect author claim** | `members: member_rules.to_vec()` preserves the partition bucket's `ir.rules` order; `ConstraintCtx` also calls that slice sorted without enforcing a sort |
| report execution is nondeterministic | **Unproven/overclaimed** | for a fixed valid `GrammarIR`, component roots are sorted, member and site walks follow stable input order, hash-map-derived installer inputs are sorted, and merge inputs are sorted. Permutation-canonical report bytes are not proved, but run-to-run determinism is not disproved |
| canonical public compile APIs preserve structured errors | **Correct in implementation** | both wrappers return `Result<GrammarIR, CompileError>`; the request route uses the same type; installation maps directly to `CompileError::OptimizationUnavailable`; only the DAP's pre-existing `String` protocol boundary stringifies |
| the R3 public structured-error test proves the optimizer variant | **Incorrect test claim** | its input is a parse failure. It proves the public result type and a nonempty display, not dynamic propagation of `OptimizationUnavailable` |
| the parenthesized mapped grammar is invalid | **Incorrect** | canonical `term` admits `("(" rhs ")")`; inner `rhs` reaches `mapped_factor`; `0u8`/`1u8` are valid `int_lit` value expressions |
| the native core panic is a real lowerer defect | **Correct** | the outer mapped factor has no direct map child, but its source span contains a nested arrow; `find_unquoted` skips quotes/regexes but not grouping depth, so the descendant arrow trips the outer invariant |
| deleting parentheses is an acceptable witness repair | **Incorrect** | that would hide a valid-language product defect and weaken the required pipeline witness |
| all R2-ordered mutation/inversion obligations are closed | **Incorrect** | the removed-implication counterfactual genuinely inverts, and prune/restore is a born-RED regression, but typed-stop and late-publication examples are not a complete mutation kill matrix |
| R4 must complete the whole W0 item-5 mutation campaign | **Incorrect expansion** | the item-3-owned mutation matrix imposed by R2 must close in R4; the wave-wide inventory of every discarded constraint, partial result, and silent fallback remains separately owned by W0 item 5 and receives no acceptance here |
| native 9/9 and 10/10 are independently content-verifiable | **Incorrect** | command, environment summary, exit, cardinality, and digest are an internally coherent receipt, but no raw transcript bytes exist in the frozen artifact set, so the digest preimages cannot be recomputed |
| native core RED is established by the receipt | **Useful attestation, source-corroborated** | exit 101 and the exact panic are consistent with the frozen grammar and lowerer; absent raw bytes keep it below independently replayable evidence, but the source defect independently proves RED |
| all 183 warnings are proved and owned by R3 | **Incorrect** | the count and attribution cannot be checked without raw output; only the two R3-local unused imports are statically proved |
| `PrettyHints` and `RuleDirectives` are unused | **Correct** | both occur only in the import at `cross_rule_csp.rs:19` |
| the 1,885-line module is an acceptable narrow owner | **Incorrect** | it owns public vocabulary, reports/errors, mutable domain mechanics, transaction/merge, solve dispatch, sites/join/decode, constraint wiring, domain construction, projection, and test attachment; this is a correct greater-product structural finding |
| consumer `None`/structural alternatives close or defeat item 3 | **Neither** | the cited AllLiteral, missing-decision dispatch/delim-scan, and conservative materialization arms are real, but their complete classification belongs to W0 item 4. They do not negate the audited transaction and must not broaden item-3 R4 |

## Finding 1 — exact cancellation scope and required contract

Critic A found a real branch gap but overstated its current production reach.
`solve_grammar_components_with_config` passes a `SolveConfig` to each component.
When constraints exist, `solve_exact_constrained` rejects
`stats.cancelled`. When there are no constraints, `solve_component` decodes and
returns `FactorizedExact` without consulting the config. There is also no token
check before accepting a component or returning grammar success.

The configured transaction is private. Public `solve_grammar_components(ir)`
constructs its own config with `cancel: None`, and the core pipeline calls that
single-argument function. Consequently:

- R3 does **not** prove an externally pre-cancelled production compile can
  publish factorized decisions;
- R3 **does** violate the semantics claimed for its configurable transaction;
- `OptimizationUnavailableReason::Cancelled` has no public grammar-solve input
  that can request it; and
- the cancellation tests exercise `solve_exact_constrained` directly, not the
  factorized or grammar transaction.

There is a second, source-proved constrained seam. `search` handles an empty
stack before its budget/cancel guards. If a complete-assignment constraint flips
the token on the final leaf and there is no later nonempty recursive call, the
incumbent is recorded and `stats.cancelled` remains false. Since the wrapper
checks only stats, it can return that incumbent. Cooperative cancellation needs
one authoritative boundary check of both solver stats and current token state.

R4 must replace the single-argument public solve with one canonical configured
surface carrying at least node budget and cancellation; it must not retain a
default wrapper plus a `_with_config` path. The production pipeline explicitly
passes the default control (`cancel: None`). The transaction checks cancellation
before component work, before accepting either factorized or constrained output,
between components, and before grammar success. A set token always returns typed
`Cancelled`, carries the appropriate failing/boundary report, and exposes no
grammar output. No `TimedOut` variant belongs here until a real deadline exists.

## Finding 2 — report requirements, without critic expansion

The critics correctly observe two source facts: `ConstraintCounts` contains
family totals rather than edge scopes, and core drops success reports after the
optimizer returns. Neither fact violates the controlling R2 disposition.

W0's gate is a declared bounded language, validator, and report. R2 made that
concrete in its ordered item 5: root/members, counts by decision family,
installed constraint families/counts, coverage, completion status,
nodes/budget/cancellation, success objective, and exact failure locus/status,
including earlier completed and failing reports on error. R3 returns that exact
shape. Its actual CSP plus decode/merge checks are the validator; a report was
not required to serialize the entire constraint graph. The earlier R2 phrase
“constraint identities/counts” is realized at the named-family identity level by
the five typed count fields; it does not silently impose stable edge keys or
scope vectors absent from the ordered repair.

Likewise, `GrammarSolveOutput.reports` is the returned success proof demanded by
R2, and `OptimizationUnavailable.reports` preserves failures through the public
compile error. W0/R2 never says successful reports must become a durable
`GrammarIR` or `CompileOutput` field. Adding one now would create a new sidecar,
touch every IR constructor, and broaden the repair. R4 must not do that without
a new governing disposition.

The author's narrower “sorted members” statement is false in raw source:
members preserve `ir.rules` order. That order is deterministic for a fixed valid
IR, and the remainder of the report-building path removes hash iteration from
ordered outputs. Thus no run-to-run nondeterminism is proved. R4 should sort the
member copy by `RuleId` and correct the `ConstraintCtx` prose, then retain the
existing full-report equality witnesses. It need not invent edge-report schemas,
telemetry, or a new compile artifact.

## Finding 3 — the exhaustive order proof is already closed

Critic B's “all three named orderings select the same first variable” assertion
contradicts the solver. The initial stack is `[0, 1]`:

1. `Chronological` returns `stack.len() - 1`, so variable 1 (`Y`) is selected.
2. Tied `FailFirst` scans from index zero, so variable 0 (`X`) is selected.
3. Tied `Mrv` likewise retains index zero, so variable 0 (`X`) is selected.

The R3 test therefore exercises two distinct legal variable trajectories. It
also reverses both canonical value tables and repeats each case. Every result is
compared with exhaustive enumeration of the immutable-cost product, whose
constrained optimum is unique. That satisfies R2's “at least two legal variable
and value orders.” Reversing variable insertion was an example way to meet the
gate, not an additional mandated axis. R4 must retain this witness; it need not
add a redundant insertion-order fixture.

## Finding 4 — valid grouped mapping and the owning repair

The frozen grammar

```text
branch = ("x" -> 0u8), "z" | ("y" -> 1u8), "z" ;
```

is valid. Parenthesized `rhs` is a canonical `term`, and each nested literal map
is a valid `mapped_factor`. The lowerer correctly recurses through the grouped
term. It fails afterward because the *outer* mapped factor, which has no direct
map payload, scans its whole source span with `find_unquoted`. That helper knows
quote and regex boundaries but not `()`, `[]`, or `{}` nesting, so it mistakes a
nested arrow for a missing outer `value_expr` child and panics.

The repair must preserve the typed-materialization invariant. Blanket deletion
of the assertion would allow a genuinely direct arrow whose structural payload
was lost to degrade silently. The correct owner-level repair is a depth-aware
direct-arrow predicate: skip quoted/escaped literals and regexes as today, track
balanced grouping delimiters, and report only an arrow at the mapped factor's
own depth. Structural children remain the authority for constructing `Map`; the
source predicate remains only the fail-closed check for a direct arrow whose
structural child is missing. The original parenthesized pipeline witness remains
unchanged.

## Finding 5 — typed public error surface and publication

The implementation is correct and stronger than the R3 test:

- `compile_grammar` and `compile_ast` return `CompileError` directly;
- request entry points use the same type;
- `install_grammar_solution` applies `map_err(CompileError::OptimizationUnavailable)`
  before either sidecar write; and
- DAP stringification occurs only at its declared `Result<_, String>` protocol
  boundary.

The passing test named `canonical_compile_api_preserves_structured_errors` uses
`"not a grammar"`, so it dynamically proves only `CompileError::Parse`. This is
a proof-quality defect, not a typed-API implementation defect. R4 should replace
that oracle with a source-external transaction test that constructs a genuine
`OptimizationUnavailable`, verifies the exact public variant/reason/locus/report,
and verifies sentinel decision/materialization maps remain unchanged. The
canonical wrapper signatures and direct no-stringify route remain part of the
static proof. No error alias, string compatibility wrapper, or second compile
path is permitted.

## Finding 6 — mutations: item-3 prerequisite versus later item 5

R3 contains useful born-RED and counterfactual tests. The prune/restore witness
would fail on R2's mutable cost/order implementation. The constrained/unconstrained
implication pair genuinely changes the optimum when the hard constraint is
discarded. Typed budget, unsat, cancellation, malformed assignment, and late
merge examples also cover their ordinary post-repair behavior.

They are not the complete external inversion matrix ordered by R2 item 6. In
particular, the latter examples do not each identify and kill the output-reaching
mutant: consume a budget incumbent, conflate unsat with a per-variable pick,
ignore cancellation, accept incomplete/foreign decode, or install early output
before a late failure.

That bounded matrix remains part of the cohesive item-3 repair because R2 made
it so. It does **not** accept W0 item 5. Item 5 later owns the wave-wide census
and mutations for every discarded hard constraint, partial result, and silent
fallback across all inventoried optimizer consumers. R4 must use small external
specimens and a kill manifest, not add a mutation framework or claim item-5
closure.

## Finding 7 — evidence, warnings, and Goldilocks ownership

The native receipt is internally coherent and materially stronger than R2's
non-executing attempts: it names three exact Apple-ARM commands, reports two
passing subsets, and records a core test executable reaching a precise panic.
The frozen source independently corroborates the panic. But a digest without
the hashed transcript bytes is not independently verifiable evidence. Neither
the 9/9 and 10/10 output nor the 183-warning count can be recomputed from the
artifact set. R4 must freeze raw stdout/stderr preimages, exit records, and the
digest manifest from its one authorized run.

The two R3-owned warning rows are independently proved: `PrettyHints` and
`RuleDirectives` occur only in the import at
`crates/ir/tests/vm/cross_rule_csp.rs:19`. Remove them. Do not absorb the
unverifiable/pre-existing warning remainder into this repair.

The 1,885-line `csp_strategy/mod.rs` is a real structural defect under the
implemented-wave greater-product plane. A Goldilocks split is:

1. `domain.rs` — decision vocabulary, immutable `StrategyDomain`, and domain
   builders;
2. `report.rs` — output, unavailable, completion, objective, and report types;
3. `site.rs` — canonical site registry/intersection and decode/coverage;
4. `solve.rs` — component/grammar transaction, merge, cancellation boundaries,
   and exact solve dispatch;
5. `constraints/token.rs` — the token-dispatch implication installer, beside
   the existing `engine`, `shape`, `layout`, and `dispatch` owners;
6. existing `components.rs` remains the partition owner; and
7. `mod.rs` becomes the narrow public façade and retains the small regex-engine
   projection so a one-function sand module is not created.

Definitions move once. Old bodies are deleted; public names are re-exported
directly. No wrappers, compatibility aliases, duplicate files, dual solve paths,
or report sidecars are allowed. Filenames omit `csp_strategy` stutter and the
external source-isomorphic tests remain where they are.

## Retained R3 facts

Retain without movement credit:

- immutable canonical values/costs/order with active-bit prune/restore;
- finite and duplicate-domain validation plus finite-objective decode;
- exact completed constrained optimization under two proved variable orders
  and two value orders;
- one component-local semantic site per `(NodeId, decision family)`;
- joined repeated-occurrence legality and bit-exact cost agreement;
- checked local decode and checked cross-component merge;
- typed missing-DAG, budget, unsat, invalid-assignment, and constrained
  cancellation outcomes;
- exact factorized additive selection when no cancellation is requested;
- grammar-local and pipeline-external all-or-nothing publication;
- returned R2-shaped success/failure component reports;
- the canonical public structured compile signatures and DAP-only string
  boundary;
- physically external, source-isomorphic test bodies; and
- the direct typed-arm/VM witness as the correct oracle once its valid grouped
  syntax lowers.

Reject or leave unproved:

- total cancellation across factorized/component/grammar boundaries;
- a public grammar-solve cancellation input;
- final-token-state cancellation detection;
- the author's sorted-member wording (not fixed-input determinism itself);
- dynamic public propagation of the `OptimizationUnavailable` variant;
- the full R2 item-3 mutation kill matrix;
- executable success of the direct pipeline witness;
- independently content-verifiable native GREEN subsets or warning totals;
- warning-clean R3-owned tests;
- permanent acceptance of the 1,885-line owner; and
- any completion of BB-C.W0 or permission to begin BB-C.W1.

## Smallest dependency-ordered R4

R4 is confined to the reopened item-3 implementation and the lowerer defect
that blocks its required product witness. It must not expand into G3, timeout
design, the item-4 consumer inventory, the full item-5 campaign, generated
runtime cleanup, packages/locks, another optimizer, or BB-C.W1.

1. **Freeze the born-RED R3 witnesses.** Before repair, record the valid grouped
   mapping panic, pre-cancelled factorized success, and final-leaf token success.
   Preserve the existing objective, hard-constraint, semantic-site, typed-stop,
   and late-failure fixtures.
2. **Make one public configured grammar-solve surface.** Introduce a narrow
   grammar optimization control carrying node budget and cancellation; change
   the canonical `solve_grammar_components` signature and all callers directly.
   Delete the private/default duality rather than adding a public
   `_with_config` wrapper. Production explicitly passes `cancel: None`.
3. **Centralize cancellation at transaction boundaries.** Check the token before
   component work, after constrained search using both stats and current token,
   before accepting factorized output, between components, and before grammar
   success. Every observed cancellation returns typed `Cancelled` plus the
   deterministic boundary/failing report and no output. Do not add `TimedOut`.
4. **Repair grouped-map lowering at its owner.** Replace descendant-wide arrow
   detection with direct-depth-aware detection while retaining structural
   children as map authority and retaining the fail-closed direct-arrow
   invariant. Do not alter the original grammar witness.
5. **Close only the controlling report/order and API proof.** Sort the report's
   member copy by `RuleId`, correct the false sorted-slice prose, and keep the
   present report schema. Add a genuine structured optimizer-error/no-publication
   transaction oracle. Do not add edge identities/scopes or retain success
   reports on `GrammarIR`.
6. **Complete the R2 item-3 mutation kill matrix.** Freeze exact small mutants
   for cost restoration, discarded implication, consumed budget incumbent,
   unsat fallback, ignored cancellation, incomplete/foreign decode, and
   early-success/late-failure publication; name the external test that kills
   each. Restore source hashes after each specimen. This is not W0 item-5
   acceptance.
7. **Perform the Goldilocks move and warning cleanup.** Use the seven-owner split
   above, delete original bodies, preserve direct public exports, and remove the
   two unused imports. No alias, shim, wrapper, duplicate path, or sand.
8. **Run one native Apple-ARM proof campaign after the static freeze.** Execute
   each exact command once with no retry and no x86 route. Freeze raw stdout and
   stderr, exit codes, command/environment/toolchain identities, source hashes,
   and a manifest whose digests recompute from those raw bytes.
9. **Require two fresh hostile critics and a fresh adjudicator.** Even a clean
   item-3 R4 leaves W0 items 1, 2, 4, and 5 under their own governing evidence.

### Exact external tests and proof requirements

- `crates/ir/tests/passes/csp_strategy/mod.rs`
  - pre-cancelled zero-constraint grammar returns `Cancelled`, not
    `FactorizedExact`;
  - a token flipped on the final complete assignment is observed at the wrapper
    boundary and the incumbent is discarded;
  - cancellation checked at a component boundary yields no `GrammarSolveOutput`;
  - the existing prune/restore and exhaustive implication test remains unchanged
    and continues to cover `Y`-first plus `X`-first trajectories;
  - budget, unsat, cancellation, malformed/incomplete/foreign assignment, and
    late-publication mutation specimens each kill their named mutant.
- `crates/ir/tests/vm/cross_rule_csp.rs`
  - configured public factorized cancellation and mixed-component cancellation
    return completed-plus-failing/boundary reports with no decisions;
  - same-rule, connected shared-root, and genuinely conflicting disconnected
    authority witnesses remain byte/equality stable;
  - report members are sorted and complete; remove the two unused imports.
- `crates/core/tests/lower/expression/wrap.rs` (external, source-isomorphic)
  - one and multiple levels of parenthesized inner mapping lower the `Map` only
    at the inner mapped factor;
  - an outer map around a mapped group yields the exact nested `Map` structure;
  - arrows inside single/double/backtick literals and regex literals do not
    trigger the direct-arrow invariant;
  - a genuine direct map retains exact `FnDescriptor`, scalar type, and source
    placement; the direct-missing-structural-child invariant remains fail-closed.
- `crates/core/tests/pipeline/compile/pipeline.rs`
  - keep `DIRECT_GRAMMAR` byte-for-byte and require exact two-arm `Map`/FnId/U8
    projection, both accepted VM inputs, and the frozen rejection diagnostic;
  - match an actual `CompileError::OptimizationUnavailable` with exact nested
    reason/locus/reports and prove sentinel sidecars are unchanged on error;
  - keep compile-time assertions that all four canonical compile entry points
    return the structured error type.
- Static/source proof
  - one canonical configured solve symbol and zero `_with_config`, compatibility,
    fallback, invented-cost, or duplicate-route symbols;
  - direct exports only after the Goldilocks move; old definitions absent;
  - scoped `git diff --check` clean and both R3-local unused-import rows absent;
  - frozen mutation manifest maps each exact patch digest to a failing external
    test and proves source restoration;
  - raw native files reproduce every recorded output digest and exit.

**TERMINAL JUDGMENT: R3 RED. CREDIT 0. CLEAN COUNT 0/2. BB-C.W1 MAY NOT BEGIN.**
