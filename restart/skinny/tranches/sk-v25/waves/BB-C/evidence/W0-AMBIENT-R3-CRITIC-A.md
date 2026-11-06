MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: hostile R3 critic A.

# BB-C.W0 ambient optimizer authority — hostile R3 critic A

- Reviewed: `2026-07-22`
- Method: hostile frozen-byte source, grammar, call-route, solver, consumer, and
  witness audit. I ran no Cargo command, build, test executable, benchmark,
  mutation runner, or x86 route.
- Verdict: **R3 RED / REJECTED AS BB-C.W0 ITEM 3**
- Credit: **zero BB-C.W0, wave, lock, tranche, or movement credit**
- Advancement: **BB-C.W1 may not begin**

R3 correctly repairs R2's mutable-cost backtracking defect, gives each
`(NodeId, decision family)` one component-local authority, rejects duplicate
decode writes, adds typed constrained-search cancellation, and preserves the
external all-or-nothing publication transaction. Those improvements are real
and should be retained. R3 is nevertheless not a complete item-3 proof. A
cancelled solve still publishes every factorized component because the
zero-constraint path never observes `SolveConfig::cancel`; the returned report
contains family counts rather than exact constraint identities and the
production pipeline drops every successful report; the claimed structured
optimizer-error witness only exercises a parse error; and the required native
pipeline witness exposes a real canonical-lowering defect, not an invalid
grammar. The native transcript hashes are also not independently verifiable
because the transcript bytes were not frozen.

## Frozen authority and subject identity

Every R3 subject hash below was independently recomputed and matches the
author freeze.

| Input | SHA-256 |
|---|---|
| `waves/BB-C/W0.md` | `00bb99d6b3c379c7c15ad7a70ab7598f8b883f9cd8d3974733323bdbc6026df2` |
| `W0-AMBIENT-R2-ADJUDICATION.md` | `ef7c8c1db203eb42ae40358e35fd2710d4e79d3cabc5495801c619b18995b9f7` |
| `W0-AMBIENT-R3-AUTHOR.md` | `8a327e3da122dd253494f8512e2ca6504ba61107fbf9eeef935c62393ff3afe7` |
| `W0-AMBIENT-R3-NATIVE.md` | `b4fd9be7815a4af19d8f103f454e8395330758faaef85e86da8ac109e7f67f01` |
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
| retained `crates/core/src/pipeline.rs` | `5eac07248f09df5539f5265e2c9f011c6c9e793d3d0049d39dba764b109b82c4` |
| `Cargo.lock` | `480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9` |

Relevant immutable solver, lowerer, grammar, and consumer anchors:

| Proof anchor | SHA-256 |
|---|---|
| `crates/csp-solver/src/domain/traits.rs` | `66d1a8813306e8394ee3bba71c0fec4fcc6672de857f9dce4281736fecc8bd97` |
| `crates/csp-solver/src/variable.rs` | `4f0bd9f90a9996e1904e05c53fd025cc50ab114bc91c1f724d1471e874a2fdc7` |
| `crates/csp-solver/src/csp/solve.rs` | `eadbaad5783fb2dec73c2447e4d133de47f79d8d746be57ced6303bcf9f755b4` |
| `crates/csp-solver/src/solver/search.rs` | `fee69885ba75c022fe478dddac586647e1e7d660276a3ce3180440de7ffe62ef` |
| `crates/csp-solver/src/config.rs` | `d1402e54a8e3835b0d053ce129893cb92b0da0275ba576133d15051568a90c24` |
| `crates/csp-solver/src/cancel.rs` | `46cbbedd1daf303e6f8f18f393e5765647408b16c103f490b6790563b1446baa` |
| `grammar/bbnf/bbnf.bbnf` | `eef788e29119b7eb88444e1dd0675bc744a1802468914e9cb402d78c9c267a57` |
| `grammar/bbnf/expressions.bbnf` | `f7740762b03790c3ba45ed6839e8ba9bc2a50b5f1eb353f1206195b2e06baf14` |
| `crates/core/src/lower/expression/wrap.rs` | `a626639367f2c7d3d45dfaa3f75fa7603871e28831c4fd117466aa9c622a2805` |
| `crates/core/src/lower/expression/mod.rs` | `8344aaaa39978838b4527b776871696b6c88e387f3fe6ba3a21ee8c71d243666` |
| `crates/core/src/lower/view_walk.rs` | `b4e3d620959c83c9c3964892847d91531324e2bda64bd5f1b18adc542f66c4c8` |
| `crates/core/src/backend/strategy/alt_strategy.rs` | `4f5c88d16c37dc27f04fcfe95650d978a17cbda417d7c89f4693c7af1ec55fec` |
| `crates/core/src/backend/driver/wrap.rs` | `90abed208fbcf49391944f4b342884c9a11105a8b4f06ed4c155fad22b522106` |
| `crates/core/src/backend/driver/mod.rs` | `e93ca3bc83844c689d76cb742e0b4513f549157f21f2b42315ce546e7877f5b7` |
| `crates/core/src/generate/regex/cost_model.rs` | `02df7e75af87f1bc0e950298b83473cd80a94325a18a2623611e532cad581e79` |
| `crates/core/src/generate/serialize/serialize.rs` | `51bf03de56d730fbf030c1319e11977250314d673cfcd1ec27ae33be5402293d` |

Scoped `git diff --check` is clean. The author's fallback/cost-corruption and
active-prose censuses reproduce with zero rows. Those censuses are correct for
their stated path and phrase sets, but they do not prove consumer closure.

## Claim-by-claim hostile ruling

| R3 claim | Judgment | Proof |
|---|---|---|
| canonical cost and tie order survive prune/restore | **Correct** | immutable `entries` plus mutable `active`; `remove`/`add` only toggle membership; `cost` reads the canonical table |
| constrained result is the exact finite additive optimum | **Correct for a completed constrained search** | branch-and-bound reads immutable costs, reports cancellation/budget before consuming its best-first result, and decode revalidates domain membership, family, coverage, and finite total objective |
| one semantic authority per `(NodeId, family)` | **Correct in a component** | `SiteRegistry` keys by `Site`, intersects every repeated occurrence before variable construction, and decode rejects duplicate semantic writes |
| shared legality and cost disagreements fail closed | **Correct** | repeated domains are intersected; bit-different shared costs are `InvalidAssignment`; an empty join is `Unsatisfiable` |
| cancellation is first-class and every result is discarded | **Incorrect** | only `solve_exact_constrained` inspects `stats.cancelled`; `solve_component` returns the factorized optimum at `mod.rs:874-890` without checking `config.cancel`, and there is no between-component cancellation check |
| compact complete report | **Incorrect / incomplete** | `ConstraintCounts` records five family totals, not the exact installed constraint identities/scopes ordered by the R2 disposition; successful reports are then dropped at `pipeline.rs:349-356` |
| report and output are deterministic | **Partly correct** | source walks, NodeId installers, and merge order are stabilized enough for the tested fixtures; no durable production report exists to compare, and exact constraint identities are absent |
| no partial optimizer sidecar publication | **Correct** | component results and grammar accumulator are local; core unwraps the complete `Result` before the two infallible sidecar writes |
| budget and unsat cannot publish fallbacks | **Correct on the audited solve route** | constrained budget and empty solution return typed failures; the factorized route is an exact no-constraint solve, not an independent-minimum fallback |
| canonical compile APIs preserve structured errors | **Statically correct, witness claim overstated** | both canonical wrappers return `CompileError`; the only R3 core test constructs a parse failure, not `CompileError::OptimizationUnavailable` or a late optimizer failure |
| tests are displaced and isomorphic | **Correct physically** | both test bodies are outside `src/` and reached through explicit path modules |
| mutation coverage ordered by R2 is complete | **Incorrect** | the implication test includes one explicit no-op-constraint mutant, but no frozen pre-repair RED execution exists; factorized cancellation is absent, the core structured-error test is a parse error, and the core pipeline oracle is RED |
| native optimizer/public witnesses are GREEN | **Reported, not independently proven** | the receipt says 9/9 and 10/10, but no raw transcripts are frozen; a SHA-256 without the hashed bytes is not replayable evidence |
| native core witness is RED before the optimizer | **Correct according to the receipt and source path** | the exact panic is consistent with the canonical grammar and the lowerer's depth-insensitive source-arrow check |
| 183 core warnings were predominantly generated debt | **Unproven count/proven narrow R3 warning** | no transcript exists to recount 183; `PrettyHints` and `RuleDirectives` occur only in the import at `cross_rule_csp.rs:19`, so the scoped unused-import warning is statically proved |
| source structure is parsimonious | **Incorrect** | `csp_strategy/mod.rs` is now 1,885 lines/68,151 bytes and owns public report vocabulary, domain, registry, solve, decode, constraints, domains, projection, and test attachment in one god-module |

## Finding 1 — factorized cancellation is silently ignored

`solve_grammar_components_with_config` passes the caller's `SolveConfig` into
each component, but `solve_component` consults it only through
`solve_exact_constrained`. When `constraint_counts.total() == 0`, lines
874–890 decode and return `FactorizedExact` directly. A pre-cancelled token is
therefore ignored for every zero-constraint component. An all-factorized
grammar can return a complete `GrammarSolveOutput` after cancellation; a mixed
grammar can publish locally completed factorized reports until a later
constrained component happens to observe the token.

This is not a performance detail. R3's public claim is that cancellation is a
distinct typed stop and every solution is discarded. Its two cancellation
tests call the private constrained helper directly and cannot reach the
production fast path. The smallest repair is a single cancellation authority
at grammar/component boundaries, used before any component result is accepted
and again before grammar success. A pre-cancelled factorized grammar and a
cancel-between-components grammar must both return `Cancelled`, include the
failing component report, and expose no output.

## Finding 2 — the report is neither exact enough nor retained in production

The R2 disposition required component members, decision families/domains,
constraint identities/counts, coverage, validation outcome, search completion,
objective, and exact failure locus. R3 returns useful counts and statuses, but
`ConstraintCounts` contains only five `usize`s. It cannot say which Alt/Engine
VarId pair a token implication binds, which engine pair/value implication was
installed, or which NodeId an equality pin owns. Two different constraint
graphs can have the same report.

The pipeline then destructures only `decisions` and `materialization` and drops
`reports`. Thus the production compile API that actually activates these
decisions returns no success report at all. Failure reports survive inside
`CompileError::OptimizationUnavailable`; success reports do not. A generated
count that is immediately discarded is not the W0 gate's production report.

Keep the report compact, but represent each installed hard constraint by a
stable typed identity/scoped key and retain the report on the canonical compile
artifact or an equally direct typed output. Do not add an environment logger.

## Finding 3 — the parenthesized mapping grammar is canonical and the panic is a real lowerer defect

The witness grammar is valid BBNF:

```text
branch = ("x" -> 0u8), "z" | ("y" -> 1u8), "z" ;
```

The canonical grammar admits `term = "(" , rhs ?w , ")"` at
`grammar/bbnf/bbnf.bbnf:29-36`; `rhs` reaches `mapped_factor`; and
`mapped_factor = factor , ("->" ... value_expr ...)?` at lines 41–42.
`0u8` and `1u8` are admitted by `int_lit` and `value_expr` in
`expressions.bbnf`. The native path necessarily parsed this syntax before
entering `lower_mapped_factor`, and the grouped-term lowerer explicitly lowers
the inner `rhs`. Parentheses are redundant here, but redundancy is legal; a
canonical compiler may not reject them.

The defect is at `wrap.rs:235-249`. An outer mapped-factor with no outer map
has one factor child, correctly yielding no `value_expr_head`. Its source span
still contains the *nested* arrow. `find_unquoted` skips quotes and regexes but
does not track grouping depth, so it mistakes that nested arrow for an outer
mapping omission and panics. This contradicts the same function's stated
structural-authority design.

Do not “repair” the witness by deleting parentheses. The KISS repair is to let
the mapped-factor's structural children own whether that mapped-factor has a
map and remove the recursive parent-span assertion. If a source check remains,
it must prove an arrow at this mapped-factor's own grammar depth, not merely an
unquoted descendant arrow. Add hostile grammars with nested mapped groups,
quoted/regex arrows, a genuine outer map around a group, and multiple nesting
depths; require the exact `Map` placement and VM semantics.

## Finding 4 — the structured optimizer-error and born-RED claims are too broad

`canonical_compile_api_preserves_structured_errors` binds the result to
`CompileError`, but its input is `"not a grammar"`; it proves only the Parse
variant and the wrapper's Rust type. It does not prove that an optimizer error
and its complete reports survive the canonical API. The disconnected
late-component test proves the direct solver returns `Err` after an earlier
component, but it never passes that failure through core installation.

Several R3 tests are strong post-repair examples, and the old R2 source makes
the prune/restore witness statically born-RED. There is nonetheless no frozen
R2 execution/inversion receipt. The only explicit behavioral mutant is the
removed-implication no-op constraint. The R2 demand for inversion evidence
across cancellation, budget, unsat, malformed assignment, and late publication
is therefore not fully met.

## Finding 5 — consumers still contain unreported alternate authority

This does not negate the solver transaction, but it prevents a whole-W0
authority claim. `alt_strategy.rs` still chooses `AllLiteral` outside the CSP,
uses the inline dispatch payload when no CSP answer exists, and otherwise
defaults to `Checkpoint`. `backend/driver/wrap.rs` treats a missing WrapMode as
permission to consume `delim_scan_configs` and emit `BalancedScan`. The
materialization consumers default missing entries to `MustTape`.

Some of those paths are intentionally safe direct/structural semantics and may
belong outside item 3; they are not proved away by the author's optimizer-only
symbol census. W0 item 4 must inventory and classify each one, and any
non-structural optimized compile must distinguish an intentional structural
mode from a missing mandatory decision rather than silently sharing the same
`None` arm.

## Finding 6 — native evidence and warnings

The receipt's three command lines are Apple-ARM-only and name no x86 route. Its
reported outcomes are internally coherent with the current test cardinalities
and source panic. However, only digest strings were frozen, not the complete
transcripts they supposedly digest. The exit/result/warning counts therefore
remain receipt assertions rather than independently content-verifiable
evidence.

The two R3-owned unused imports are real and trivial cleanup debt. The claimed
183-warning total and “predominantly pre-existing generated comparisons” may be
true, but cannot be ruled from absent bytes and is not a reason to broaden this
repair into generated-runtime cleanup.

## Total tranche/wave/feature fit

R3 remains an item-3 repair inside BB-C.W0. It does not close the whole wave.
Even a clean R3 would leave the independently governed G3/item-1 result and W0
items 2, 4, and 5 to their own accepted evidence. In the present bytes:

- cost immutability, constrained exactness, semantic-site joining, duplicate
  decode rejection, and external transaction should be retained;
- factorized cancellation, exact/durable reporting, optimizer-error proof,
  the valid nested-map lowerer path, and mutation receipts remain RED;
- the 1,885-line optimizer module is contrary to the campaign's Goldilocks
  modularity rule and must not become the permanent home for further proof
  vocabulary; and
- no BB-C.W0, lock, tranche, Alpha, or Omega movement follows from the two
  reported passing native subsets.

## Smallest cohesive R4

1. Add external born-RED witnesses for pre-cancelled zero-constraint grammar,
   cancellation between components, the canonical parenthesized mapping
   grammar, and `CompileError::OptimizationUnavailable` propagation with an
   early-success/late-failure transaction. Freeze the RED pre-repair outputs.
2. Centralize cancellation at the grammar/component transaction boundary so
   both factorized and constrained routes return the same typed `Cancelled`
   result and no grammar output.
3. Repair `lower_mapped_factor` structurally. Nested arrows belong to nested
   mapped-factor nodes; delete the depth-insensitive parent-span assertion.
   Preserve the current witness unchanged and add nested/outer/quoted/regex
   counterexamples.
4. Give each installed constraint a compact stable typed identity/scope in the
   component report and retain successful reports through the canonical
   production compile artifact. Keep failure reports on `CompileError`.
5. Remove the two scoped unused imports and split the 1,885-line module along
   existing cohesive boundaries (`report`, `domain`, `sites`, `solve`) with
   module-name-stripped filenames. Do not create wrappers, aliases, or dual
   paths.
6. Run one new native Apple-ARM campaign, once, freezing raw transcripts plus
   exit and digest manifests. Then require two fresh hostile critics and a
   fresh adjudicator. Do not advance BB-C.W1 even if item 3 becomes clean;
   route the remaining W0 items explicitly.

**R3 RED. CREDIT 0. BB-C.W1 MAY NOT BEGIN.**
