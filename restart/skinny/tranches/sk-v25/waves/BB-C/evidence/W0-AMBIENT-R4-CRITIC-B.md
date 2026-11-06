MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: fresh hostile Critic B (R4).

# BB-C.W0 ambient optimizer authority — R4 hostile Critic B

- Review date: `2026-07-22`
- Subject repository HEAD: `af15f63e0d2d3d719938c13b906a50acbb92ea3b`
- Governing R3 adjudication: `0453ac333914204a3597693838906471999fcdcce77c7fd518be55071151b7fb`
- R4 author freeze: `7e917e2eedbeba69027231358a86091f5cddd0b5e4c394a1f38e70aeb15e7364`
- R4 native receipt: `707b91f245c7e739a5eedcff50d523909a1c22361c138f509f9fcc885f9ca12c`
- Method: independent exact-byte/source audit. I read the controlling disposition,
  both R4 born-RED records, the R4 author/native records, every bound R4 source
  and external test, every raw RED/GREEN/mutant log, all seven patches, the
  mutation manifest/restoration records, the relevant generated parser/runtime,
  the CSP solver kernel, and the production consumers. I did **not** read the R4
  Critic-A record.
- Prohibited execution: I ran no Cargo command, build, test executable,
  benchmark, formatter, mutation, or other-repository operation. Shell use was
  read-only; this report is the sole write and was created with `apply_patch`.

## Terminal verdict

**R4 RED. HOSTILE CLEAN: NO. CREDIT 0. BB-C.W1 IS BARRED.**

The configured CSP transaction is materially better than R3 and is source-sound
for the bounded decision language on a well-formed, dense canonical `GrammarIR`.
That does not rescue this freeze. The R4-owned grouped-map repair still panics on
valid canonical grammar text and can silently lose an exact scalar type on a
second valid form. Separately, the newly public configured solve can panic before
producing its promised typed failure/report when a public/deserialized
`GrammarIR` violates the unencoded dense-`RuleId` assumption. The native GREEN
selectors do not contain either counterexample.

No W0, wave, feature-lock, or tranche completion follows from the narrow GREEN
logs. W0 items 1, 2, 4, and 5 remain expressly unaccepted even after a repaired
item-3 R4.

## Severity-first findings

### P1 — `find_direct_arrow` is not a grammar-correct direct-depth scanner

**Affected bytes**

- `crates/core/src/lower/expression/wrap.rs:49-59` treats a slash-delimited
  region as extending to the next unescaped slash.
- `crates/core/src/lower/expression/wrap.rs:66-100` enters that same state for
  every `/`, without distinguishing a regex opener, block-comment delimiter, or
  value-expression division operator.
- `crates/core/src/lower/expression/wrap.rs:301-310` panics when that scan reports
  an arrow but no structural value-expression child exists.
- `crates/core/src/lower/expression/wrap.rs:650-675` relies on the same scanner to
  recover the scalar suffix of a typed numeric leaf whose structural span is
  empty.
- `grammar/bbnf/bbnf.bbnf:17,39,42` admits block comments around a factor and
  maps the factor with a direct `->`; the block-comment body uses `/[^\*]*/`,
  which excludes only `*`, so `/` and `->` are legal comment bytes.
- `grammar/bbnf/expressions.bbnf:6,18,21,26,34` admits typed integer leaves,
  grouped value expressions, and `/` as multiplication-precedence division.
- `crates/core/src/grammar/generated/bbnf.rs:6363-6399` accepts the exact
  `/* ... */` byte form and scans `[^\*]*`, excluding only `*`;
  `:12827-13038` records factor bounds and `:13098-13262` records the whole
  mapped-factor bounds. Thus `node.span_text()` really does contain these bytes.
  `crates/core/src/runtime/bbnf/view.rs:107-138` also proves a typed `Int` has no
  direct byte span, activating the numeric recovery path.

**Source-proved valid counterexamples**

1. `subject = /*a/b -> c*/ "x" ;`

   This is a valid leading `big_comment` plus literal factor. The scanner sees
   the opening `/`, mistakes the body slash after `b` for a regex terminator,
   then observes the comment's `->` at depth zero. There is no map payload
   child, so `wrap.rs:301-310` panics on valid input.

2. `subject = ("x" -> input / 2) -> 1u8 ;`

   The outer scan correctly enters `(` and ignores the inner arrow, but then
   mistakes the division slash for a regex opener. With no later slash it skips
   to end-of-source and never sees the outer arrow. Structural lowering still
   creates the outer `Map`, but the `Int` leaf has an empty span and
   `direct_map_rhs` returns `None`; the outer `FnDescriptor::Expr` therefore
   loses `Some(TypeDesc::U8)` and receives no inferred return type.

The five tests in `crates/core/tests/lower/expression/wrap.rs:59-122` cover
quotes, a well-closed regex, grouping, and simple typed numerics, but neither
comments nor division-before-an-outer-arrow. G3's five passes therefore do not
prove the author claim at `W0-AMBIENT-R4-AUTHOR.md:121-135` that direct-depth
detection and exact typed recovery are closed over canonical syntax.

**Smallest repair:** stop reconstructing parser ownership from arbitrary source
text. Carry the parser-owned direct mapping/RHS bounds (or an explicit direct-map
marker) into lowering and use structural children for both invariant checking
and numeric suffix recovery. If a textual helper must remain temporarily, it
needs grammar-aware states for `/*...*/`, regex literals only in factor lexical
position, and value-expression division. Freeze the two exact examples above as
born-RED external tests before changing the owner.

### P1 — the public solve panics before typed recovery on malformed rule identity

`GrammarIR` and `IrRule` are public and deserializable
(`crates/ir/src/types/grammar.rs:24-35`,
`crates/ir/src/types/rule.rs:11-21`). `RuleId` is documented as an index at
`crates/ir/src/types/mod.rs:50-53`, but the type does not encode the invariant.
The configured solve accepts `&GrammarIR` directly at
`crates/ir/src/passes/csp_strategy/solve.rs:76-110`.

`partition_by_call_graph` notices an out-of-range caller and skips its edge walk
at `crates/ir/src/passes/csp_strategy/components.rs:251-267`, but then
unconditionally calls `uf.find(rule.id)` at `:269-278`. `UnionFind::find`
indexes `parent[root as usize]` at `:87-100`. A one-rule IR whose sole rule has
`id = 1` therefore panics with a one-element union-find after a DAG has been
built; it returns neither `OptimizationUnavailableReason::InvalidAssignment`
nor a component report. Duplicate in-range IDs are also not rejected: the
`rule_by_id` map overwrites one rule and component membership can contain the
same ID twice, allowing a rule body to be silently omitted from the transaction.

Canonical pipeline lowering currently emits dense IDs, so this does not refute
the dense production path. It does refute totality of the newly public,
deserializable configured surface and the retained typed translation-mismatch
posture. None of G1/G2 constructs an out-of-range, duplicate, or non-dense rule
layout.

**Smallest repair:** before partitioning, validate that rule IDs are a unique
permutation of `0..rules.len()` (and that `entry`/Refs are in range), returning a
typed `InvalidAssignment` with `OptimizationFailureLocus::Rule` and a
deterministic failure report. Alternatively make the partition fallible and key
it by actual `RuleId`s. Add exact out-of-range and duplicate-ID public API
oracles; do not merely harden `UnionFind::find` into another silent skip.

### P2 — several GREEN labels outrun the tests that actually ran

- `crates/ir/tests/vm/cross_rule_csp.rs:110-167` implements a separate
  `reference_partition`. The bbnf-shaped, transitive, symmetric, deterministic,
  and disconnected tests exercise that mirror, not production
  `partition_by_call_graph`. The useful production member-order test at
  `:532-547` is much narrower.
- `configured_public_mixed_components_cancel_at_the_first_boundary` at
  `cross_rule_csp.rs:550-580` cancels the token at `:558-559` before calling the
  solver. It proves a pre-search boundary report, not cancellation after one
  component completed or the between-component timing named by the R3 proof
  requirement. The implementation has the checks, but this selector does not
  invert them.
- `engine_propagation_unifies_component_regex_engines` at
  `cross_rule_csp.rs:418-460` guards the only equality assertion with
  `if let (Some, Some)` at `:452`. Deleting projection for one or both patterns
  leaves the test GREEN.
- `optimizer_error_preserves_exact_public_variant_and_sentinel_sidecars` at
  `crates/core/tests/pipeline/compile/pipeline.rs:151-198` constructs an error and
  calls the private `install_grammar_solution` helper directly. It proves that
  helper's error arm and sidecar non-mutation, while the four public entrypoints
  at `:62-76` are only compile-time signature assertions. The source path through
  `crates/core/src/pipeline/compile/pipeline.rs:328-362` is direct and looks
  correct, but G4 did not dynamically make a canonical public compile call
  return an optimizer failure.

These are proof-quality defects, not evidence that the corresponding dense-IR
implementation is wrong. They do make the native statements at
`W0-AMBIENT-R4-NATIVE.md:36-42` stronger than the executed oracles.

**Smallest repair:** compare the production partition against an independent
edge-set oracle; require both engine entries before equality; arrange a token
flip after the first component returns (a deterministic test hook or a
constraint callback in component one); and inject failure at a public compile
transaction seam rather than only invoking the private installer.

### P2 — R4 did not finish its own warning-clean step

`crates/ir/src/passes/csp_strategy/mod.rs:125` imports
`crate::dag::NodeId` only to leak it through the test module's `super::*`; the
non-test library build reports it unused. The exact warning is present in G2,
G3, and G4 and is correctly admitted by
`W0-AMBIENT-R4-NATIVE.md:44-52`. The R3 smallest repair called step 7 the
Goldilocks move **and warning cleanup**. Removing two older imports while adding
a new R4-local warning is not a hostile-clean completion of that step.

**Smallest repair:** import `NodeId` in the external test owner (or qualify it)
and delete the facade import, then re-freeze warning totals. No broad warning-bank
cleanup is required.

### P3 — mutation execution is real, but two specimens are coupled

All seven exact patches were applied one at a time, reached their named test,
failed semantically rather than at compile time, and were reversed to the exact
preimage. That 7/7 statement is byte-supported. It is not the same as independent
coverage of every guard bundled into a patch:

- `M05-ignore-cancellation.patch:4-12` replaces the compound
  `stats.cancelled || current_token` guard with `false`. The one executed oracle,
  whose raw failure is `W0-AMBIENT-R4-MUTANT-M05.log:7-10`, proves the final-token
  half; it does not independently kill deletion of only the solver-stats half.
- `M06-accept-incomplete-foreign-decode.patch:4-48` removes the length check,
  clamps foreign IDs, and fills absent values. Its raw log fails at the first
  empty-solution assertion (`W0-AMBIENT-R4-MUTANT-M06.log:7-10`, corresponding to
  `crates/ir/tests/passes/csp_strategy/mod.rs:413-418`), so the same execution
  never reaches oversized or foreign-ID assertions at `:425-444`.

This does not falsify “each exact patch was killed”; it limits mutation
isolation and leaves missed single-guard counterfactuals. Split those guards
into surgical specimens or expose separately selectable test cases, while
retaining the adjudicated seven-category matrix. The broader W0 item-5 campaign
remains out of scope and unaccepted in any case.

### P3 — the prescribed split is acyclic, but retains avoidable authority baggage

The seven-owner move is structurally faithful to the R3 order: definitions moved
once, direct re-exports are used, old bodies are absent, and no module dependency
cycle was found. Two KISS debts remain:

- `ConstraintCtx.mat_classes` and `rule_materialization` at
  `crates/ir/src/passes/csp_strategy/constraints/mod.rs:63-67,88-107` have no
  caller in any installer.
- `crates/ir/src/passes/csp_strategy/mod.rs:136-142,169-200` describes and
  resolves multiple engine choices for one `StringId`. For a canonical IR,
  string interning deduplicates equal text
  (`crates/core/src/lower/string_interner.rs:21-28`) and DAG interning maps every
  structural `Regex(sid)` occurrence to the same `NodeId`
  (`crates/ir/src/dag/intern.rs:10-24`, `dag/mod.rs:79-82`). A
  `RecognizerDecisionMap` therefore cannot contain different decisions for the
  same canonical regex `StringId`; the hard-coded tier conflict branch is dead
  on the stated substrate.

These are not terminal semantics defects. Delete the unused context capability
and reduce projection to its realizable invariant (or document and test a real
noncanonical input that can produce the claimed conflict) rather than carrying
future authority in the facade.

## CSP/transaction audit disposition

For well-formed dense IR, I found no counterexample to the following retained
core mechanics:

- domains retain immutable canonical values/costs and active-bit pruning;
- site registration intersects repeated `(NodeId, family)` legality and rejects
  bit-different shared costs;
- the zero-constraint path is an exact additive factorization, not the deleted
  independent-minimum fallback;
- every installed constraint family increments the count that selects the
  constrained path;
- branch-and-bound does not stop at `max_solutions = 1`; it searches to proof,
  sorts, and truncates only afterward;
- budget, cancellation, unsat, malformed decode, and non-finite/overflowing
  objective paths fail closed;
- current-token checks close the final-leaf seam and all component/grammar
  output remains local until the full transaction succeeds;
- checked cross-component merge prevents conflicting shared `NodeId` authority
  from returning `GrammarSolveOutput`;
- ordinary and boundary report member copies are sorted, reports carry the
  adjudicated family/count/coverage/status/objective/locus fields, and production
  installs neither decision sidecar on an optimizer `Err`;
- regex projection after install is infallible and, on the canonical hash-consed
  substrate, cannot introduce a different per-node solution.

This credit is deliberately bounded by the two P1 boundary failures above. It
does not validate every downstream optimizer consumer or complete the separate
W0 item-4 inventory.

## Exact-byte evidence verification

All hashes below were recomputed from current bytes; discrepancy count is zero.
The 24 source/test hashes and line/byte counts in the R4 author subject table
also recompute exactly (24/24), as do the four restoration targets and
`Cargo.lock = 480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9`.
The restoration target digests are
`domain.rs = 8a8e07ad08e15726e73e9e70127a21493098e7d7738a3b25c2c462d8fd146271`,
`csp/mod.rs = be015fe5a97a84ee50ac1e2e3cf2aa0974172f6af7562075db8327baba68a3e3`,
`solve.rs = 02cf11b8c3439a2f0f1856aad8b8998d0182121914865e3779dfdfc7772f9581`,
and `site.rs = 5b7b002acec4c3be75d2c7172af059195fce0eb8349bc8bb8ee3cc77776836b9`.

| Artifact | Recomputed SHA-256 |
|---|---|
| `W0.md` | `00bb99d6b3c379c7c15ad7a70ab7598f8b883f9cd8d3974733323bdbc6026df2` |
| `W0-AMBIENT-R3-ADJUDICATION.md` | `0453ac333914204a3597693838906471999fcdcce77c7fd518be55071151b7fb` |
| `W0-AMBIENT-R4-BORN-RED-AUTHOR.md` | `78af54f15e5e54813a3a0fb071304bbc894d74d41aef9c1695e7531592101d4c` |
| `W0-AMBIENT-R4-BORN-RED-NATIVE.md` | `6594f0b6abdbd21d7f3a1ada75b78e507dedeacf445b773d00f08414121484a6` |
| `W0-AMBIENT-R4-AUTHOR.md` | `7e917e2eedbeba69027231358a86091f5cddd0b5e4c394a1f38e70aeb15e7364` |
| `W0-AMBIENT-R4-NATIVE.md` | `707b91f245c7e739a5eedcff50d523909a1c22361c138f509f9fcc885f9ca12c` |
| `RED-1.log` | `a8a0f33765ac04956759db2ab58b577c16b5bc4b8cb412c747df9b076504899e` |
| `RED-2.log` | `7ed3e288c7c149c76f59fa798b681c2cbb98e5b76f65c3614af02899e83cb615` |
| `RED-3.log` | `aec4885bd10f8ea830229ee96146638bdfaa88f1fa1ec34f833f9a244509910d` |
| `GREEN-1.log` | `ab5e6ab06296294b10dffe4f74ac10f4eee07856655f3011de56c2abcfabb129` |
| `GREEN-2.log` | `53fcabd42493211ad43c8ea5ed8d874a76bcd3ef649b2cfaa07b023ab874cb42` |
| `GREEN-3.log` | `76d6d00f25125e31ebbf5c6efd7f029a709dfb8373de74953597cb9919b49046` |
| `GREEN-4.log` | `58145a86706d877b80c3778d0762f20d4922f9ad2c7ade2ef9d15fcb46e95ad4` |
| `MUTANT-M01.log` | `72f2cf33d15e1c27341dc92d69fcfaa6699ce2d9cc9747066559975e91f12b08` |
| `MUTANT-M02.log` | `0f60635de6cbcc609c17521fb7beb7007245f85c05d4461c3ea742cddcc80c99` |
| `MUTANT-M03.log` | `09438601a08d4c39718337ace0ea0fc7c6030afb533e0e643138d8195e358043` |
| `MUTANT-M04.log` | `96399adef4950679cc11ab21dbe95605aded70e84f8d1c88d618f6cd16948479` |
| `MUTANT-M05.log` | `38f9b31fded34048ca8c4968a5e83f1349bf64d5167b79a5f8df0206c9a46f73` |
| `MUTANT-M06.log` | `8e31369922dcb04e777ddd7eac6dffb97c16a74f96bb5037d699517cbd6af51f` |
| `MUTANT-M07.log` | `f059ecf257b36cc2effb9c3c87760063747b9a48bb6542b01699c04dbad584ad` |
| `W0-AMBIENT-R4-STATIC.log` | `ce4d1d45ae5b945228b6fb2238ad29bebecb06377a0fd4a7367507968f6ad12e` |
| `MUTATION-MANIFEST.md` | `8beb499c0db78fb704067c11a5d10a5d79fa9f19acd4a7784fbf1e8e5545aeb0` |
| `PATCHES.sha256` | `6984c68483deb7beebcc9289ad1a1b77f542ceed11381737e9c7853ec2b5ffa2` |
| `M01-cost-restoration-invents-unit-cost.patch` | `9633e1d2616ddf81e929d0b179f9af5497c9400b68e97cebed06a3eb8711f04f` |
| `M02-discard-custom-hard-constraint.patch` | `6465c96ae7bde833693582bc264dbe682a83afaa345cbf2ffe6377afe14e0d3a` |
| `M03-consume-budget-incumbent.patch` | `5c1615f10232a42769a098b3c1bff8aeb3ac806205c4f72ae18373b4b7ab9ead` |
| `M04-unsat-per-variable-fallback.patch` | `a4efe104378d6dc7b7d3043adda4d85009b7ba07d4222daf04f85f8d326b5553` |
| `M05-ignore-cancellation.patch` | `8ec5bd0a30599a7a0c50194115402b7d69c9951c164211860aa5aa366299aff5` |
| `M06-accept-incomplete-foreign-decode.patch` | `e7e6139a53e3a903da7a168da4230b926b619f2ea8c9a75c6ae4e090c8d4de42` |
| `M07-publish-before-late-merge-failure.patch` | `f8f04ff4c2c5557785fd6b878613f500e5752e823643ce58d2d9e93b97fd4e75` |
| `SOURCE-RESTORATION.sha256` | `b337b0d640665c65248321d8bb352a228296753a60c83a4a662be315070a3b47` |
| `APPLY-CHECK.log` | `cdd050df474390d067188a1987ad4439b1b8c72b8561b17a9191a77e673b3215` |

The raw ARM logs are internally consistent with the native receipt: G1 12/12,
G2 12/12 selected, G3 5/5 selected, G4 3/3 selected; all seven mutants enter
their named oracle and fail with exit 101; restoration hashes match. G2 carries
the R4-local unused import, while G3/G4 additionally carry the frozen 183-warning
`bbnf` bank. I did not rerun those executables, so this is exact preimage and
source corroboration, not a second native execution.

## Claim ledger

| Claim | Critic-B disposition | Exact reason |
|---|---|---|
| Frozen subject/evidence identities | **VERIFIED** | All governing, source/test, log, patch, restoration, and lock hashes recompute; zero mismatches. |
| Native Apple-ARM narrow gates | **VERIFIED AS RECEIPTS** | Raw bytes support the stated selectors/results/toolchain and 7 semantic kills; critic did not re-execute. |
| One public two-argument configured solve; no dual `_with_config` route | **VERIFIED** | One definition/direct export and fixed production control; no compatibility wrapper. |
| Exact factorized vs constrained solve on canonical dense IR | **VERIFIED** | Count routing, domain validation, exhaustive branch-and-bound, decode, and merge are source-sound for the declared language. |
| Cancellation never publishes observed partial output | **VERIFIED IN SOURCE; TEST CLAIM PARTIAL** | Boundary checks and current-token seam are present; the “mixed” test only pre-cancels. |
| Complete typed failure/report surface | **FALSE AS A PUBLIC TOTALITY CLAIM** | Out-of-range/duplicate `RuleId` is unvalidated; one path panics before any typed result/report. |
| Sorted, complete R2-shaped reports | **VERIFIED FOR VALID PARTITIONS** | Member copies sort and schema is complete; malformed identity can bypass reporting by panic. |
| Grouped-map direct-depth repair is canonical-syntax complete | **FALSE** | Valid block-comment slash/arrow panics; nested division hides an outer arrow from typed recovery. |
| Exact typed map placement/recovery | **FALSE BEYOND FIVE FIXTURES** | The nested division counterexample produces an outer typed numeric map without inferred `U8`. |
| Public structured compile propagation dynamically proved | **PARTIAL** | Source propagation is direct; runtime test injects at a private install helper, while public calls are signature-only. |
| Seven frozen mutants killed and sources restored | **VERIFIED** | Exact semantic failures and restoration bytes match; M05/M06 are coupled and not guard-independent. |
| Goldilocks move/direct exports/no dependency cycle | **VERIFIED WITH KISS DEBT** | Prescribed owners and direct exports are present; dead context/projection authority remains. |
| Warning cleanup / hostile clean | **FALSE** | New R4-local unused `NodeId` warning is present and admitted. |
| Item-4 consumer inventory / full item-5 mutation campaign | **NOT CLAIMED; INCOMPLETE** | R4 correctly routes both out of scope; they cannot contribute W0 completion. |
| BB-C.W0 / wave / tranche completion | **REJECTED** | Two P1 failures remain and the controlling record leaves four W0 items open. |

## Smallest dependency-ordered R5

1. Freeze the two exact valid grammar counterexamples above as born-RED raw
   preimages, then replace textual map ownership/recovery with parser-owned
   structural bounds at `wrap.rs`; do not broaden grammar syntax.
2. Add a fallible dense/unique rule-identity preflight before call-graph
   partitioning and return a deterministic typed report for malformed public IR.
3. Tighten only the four proof gaps: production partition comparison,
   non-vacuous engine projection, actual post-first-component cancellation, and
   public compile error propagation.
4. Make M05/M06 surgical (or independently selectable), remove the R4-local
   import warning, and delete the two identified dead authority seams.
5. Re-freeze exact source/evidence hashes and run the same one-shot native ARM
   gates plus the new lowerer and malformed-IR selectors. Then require two fresh
   hostile critics and fresh adjudication.

Until that sequence is complete: **R4 RED; CREDIT 0; HOSTILE CLEAN NO; BB-C.W1
MAY NOT BEGIN.**
