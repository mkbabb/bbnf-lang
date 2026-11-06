MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: BB-C.W0 item-3 R6 KISS repair author.

# BB-C.W0 ambient optimizer authority — R6 repair author receipt

- Authored: `2026-07-22`
- Repository HEAD observed: `af15f63e0d2d3d719938c13b906a50acbb92ea3b`
- Machine observation: Apple `arm64`; every invoked executable was requested
  through `/usr/bin/arch -arm64`
- Binding adjudication: `W0-AMBIENT-R5-ADJUDICATION.md`, SHA-256
  `df6c714c8f7c0a487b3f019a8c328b144e119fd661ce6f548e863d209a377da1`
- Model-truth declaration: explicit `gpt-5.6-sol`, effort `ultra`; no inherited
  model claim
- Dynamic execution: **none**. No test, mutant, build, profile, benchmark, or
  generator ran in this author lane.
- Formal credit: **zero** native GREEN, mutation, BB-C.W0 item, wave, lock,
  tranche, prototype, performance, or movement credit.
- Advancement: **BB-C.W1 remains barred**.

This receipt freezes only the smallest source/test repair and its static
mutation specimens. Root-owned native execution, generator fixed point/fleet
closure, profiling, critics, and adjudication remain mandatory.

## Exact dirty preimages

The four authorized dirty files were hashed before any author edit. Their
ledger is `AUTHOR-PREIMAGES.sha256`, SHA-256
`b2827d05d7705bac285d69314749495b6f52edb74ba1056050c3aa0f790c449b`,
4 lines / 434 bytes.

| Preimage | SHA-256 | Lines / bytes |
|---|---|---:|
| `crates/core/src/lower/expression/wrap.rs` | `10fccfa027a205294991a3e3a05e6e14777f13f6d5746db39e28ade74582a39c` | 491 / 18,126 |
| `crates/core/tests/lower/expression/wrap.rs` | `6b1190cd7bd3d26c71d2429b7eec7d0e2c5e2e9ce9ae4138a9e9708243ee8a48` | 212 / 6,749 |
| `crates/ir/src/passes/csp_strategy/solve.rs` | `22c90e77ab6debbd74de38e2e88437f4c13131fb573342d9727590c6e91aa0c7` | 728 / 26,664 |
| `crates/ir/tests/passes/csp_strategy/mod.rs` | `8c3f081962485f1c9cff0264848d72c1d8f2ea5045f052de2fbce7669d1f0436` | 679 / 22,695 |

No blind overwrite, reset, checkout, staging, or commit was performed.

## Exact repair

### 1. Structural map ownership became KISS-exact

`wrap.rs` deletes the redundant textual fixed-arrow assertion and updates its
module contract accordingly. The parser-produced child topology remains the
sole map authority. Exact RHS slicing remains
`bounded_text(input, value_lo, value_hi, ...)`; it was not widened to
`factor_hi`. Every R5 scanner/fallback deletion remains absent:
`DirectArrow`, `find_direct_arrow`, `direct_map_rhs`, escaped-delimiter source
scanning, anonymous-wrapper recovery, and parent-span map recovery are absent.

The retained structural checks still enforce one direct `Factor`, at most one
direct value-expression head, at most one direct `TypeAnnotation`, contained
UTF-8-safe bounds, a non-empty RHS, exact bounded annotation spelling, and
empty structural tails. Numeric suffix recovery remains restricted to one
structural integer or float literal.

One displaced, source-isomorphic test,
`explicit_annotations_and_qualified_call_keep_exact_utf8_bounds`, now places
`λ` and `前` literals before the mapped factors, surrounds both factors with
block comments/whitespace, and proves two routes in one grammar:

- explicit scalar annotation: exact `FnDescriptor::Expr`, `MapExpr::Input`,
  `TypeDesc::U8`, inner literal `scalar`, one map;
- named annotation plus qualified/function value path: exact
  `MapExpr::FnCall`, name `crate::codec::decode`, one `Input` argument,
  `TypeDesc::Named("Payload")`, inner literal `named`, one map;
- the full grammar contains exactly two function descriptors.

The test is a designed oracle, not executed evidence in this receipt.

### 2. Cancellation now has one monotone authority

`solve_exact_constrained` now checks `cancellation_requested(config)` directly
after `solve_optimized`. The redundant `cancellation_observed(stats, current)`
helper is deleted. Its impossible stats-only truth-table test and helper-only
current-token test are deleted. No hook, public API, alias, fallback, or second
cancellation path was added.

The real constrained production fixture
`cancellation_on_final_singleton_leaf_discards_incumbent` remains unchanged.
It is the R6-M07 oracle because the constraint cancels the shared monotone
token at the final singleton leaf while solver stats can still report no
cancellation.

## Final source/test identities and parsimony

| Final owner | SHA-256 | Lines / bytes | Physical line delta |
|---|---|---:|---:|
| `crates/core/src/lower/expression/wrap.rs` | `7edbb5bca06a63304615afb4b0560f73a0bb3d6df3439b7aa8c633f6cd323f17` | 488 / 17,932 | -3 |
| `crates/core/tests/lower/expression/wrap.rs` | `31ec57a3bc469d22f4d3ba805a12505365b2aa167ba01ebb74722b817967426c` | 270 / 8,387 | +58 |
| `crates/ir/src/passes/csp_strategy/solve.rs` | `aac939e7a68261d50dcbcf4961c638a206e71d780d95edf26dd71e1017ccf08e` | 721 / 26,443 | -7 |
| `crates/ir/tests/passes/csp_strategy/mod.rs` | `1e32d3b4ba2aae1db58cb2ee81db236c17357bb048caf256c3080006da27c9e3` | 669 / 22,447 | -10 |

Production is **net -10 lines / -415 bytes**. Tests are **net +48 lines /
+1,390 bytes** after adding the compact positive route and deleting the two
fabricated helper tests. The full four-file repair is **net +38 lines / +975
bytes**. No generated output, emitter, xtask/Apple compatibility source,
Cargo manifest, package, or lockfile was changed.

Read-only bound identities:

| Bound read-only owner | SHA-256 | Lines / bytes |
|---|---|---:|
| `crates/ir/src/passes/csp_strategy/site.rs` | `5b7b002acec4c3be75d2c7172af059195fce0eb8349bc8bb8ee3cc77776836b9` | 473 / 16,085 |
| `Cargo.lock` | `480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9` | 4,650 / 113,885 |

## Exactly eight one-hunk specimens

The unique directory `W0-AMBIENT-R6-MUTATIONS/` contains exactly M01-M05 and
M07-M09. **M06 does not exist.** Every specimen has exactly one unified-diff
hunk and was checked without application.

| ID | SHA-256 | Lines / bytes | Production owner |
|---|---|---:|---|
| M01 | `4ab384e668d8da4824913499fa28b72fbbf6b1407702a07945ebdcfec93cf1e0` | 12 / 772 | exact RHS start only |
| M02 | `f09981cd6ea09d6b30e47701ffc28eaff988c91327d4d6795f7c6ba1ac2aeff4` | 11 / 589 | failed-merge objective clearing |
| M03 | `10360927790dc81f10fd462475b494ff3cdafc6c03d5b9b846192904e593fc5b` | 19 / 770 | post-decode/pre-merge cancellation |
| M04 | `7716a1a7970cfef54bb21212f9b028f617cf971617a84aad2077e08d995b6fc9` | 23 / 849 | before-next-component cancellation |
| M05 | `ac2df3fb820c22884edfef43c8f063a1ae16959fee41f6db0d662a33abd04a0f` | 18 / 766 | final-return cancellation |
| M07 | `4b490d8ff3b354bea474b23dce17e6222425bcfc8ca5f921047e94f7b7cc2d53` | 16 / 691 | current-token post-solve guard |
| M08 | `24a200d2d169ee916e59507dc518848557f03ea6c58249d64e34d680049bb194` | 18 / 676 | exact solution cardinality |
| M09 | `fafeb9c4d589df3ceda50c562d2255605856895aa2064b0a61a4059738d5e987` | 12 / 634 | foreign `VarId` rejection |

M01 changes only `value_lo` to `factor_hi`. Its root epoch must run the
division killer and block-comment control against the same applied bytes.
M07 deletes the real post-solve token guard and is bound to the final-singleton
production fixture. M02-M05 and M08-M09 retain their R5 mechanics because they
already isolate real semantic owners.

## Static evidence identities

| Record | SHA-256 | Lines / bytes |
|---|---|---:|
| mutation manifest | `30129d456bd4912ccd558b64fd38dcd06ce06afe8723ce61fb7ac120aca6bcc2` | 74 / 7,003 |
| patch ledger | `1e5e414ba7090bbc6e7d5922b0b65dfbdfbe39123cc5bec07d2ed35f1168ea88` | 8 / 875 |
| apply/static-check log | `ecc0f5401d60c03f626dfd3f9ab72e56cf44a0f894b2c41a4a365cf5277fb36c` | 13 / 975 |
| author preimage ledger | `b2827d05d7705bac285d69314749495b6f52edb74ba1056050c3aa0f790c449b` | 4 / 434 |
| mutation restoration ledger | `e1ba667546e54fb01ccb09cc2a712bdc0e5095c74f145441532a4e55ef046f75` | 3 / 324 |

Static author checks only:

- `/usr/bin/arch -arm64 rustfmt --edition 2024` parsed/formatted the four
  authorized Rust owners;
- tracked and no-index whitespace/diff checks emitted no error;
- all 8/8 patches returned exit `0` from `git apply --check`;
- every patch contains exactly one `@@` hunk;
- all mutation targets remained at their restoration identities;
- `Cargo.lock` remained byte-identical.

These checks establish formatting and patch formation only. They confer no
dynamic or semantic pass credit.

## Retained and routed remainder

- Numeric suffix helper duplication is retained. No exact equivalence plus net
  simplification was proved; ownership reconciliation stays in W0 item 4.
- Non-dense/duplicate `RuleId` handling and malformed internal-view totality
  remain W0 item 2. This repair adds no fallback or defensive dual path.
- The generic Pratt generator rebuild, BBNF epoch fixed point, full BBNF /
  Google Sheets / CSS 7/7/2 fleet refresh, and sidecar closure remain outside
  this repair-author slice but inside R6 closure.
- Apple-ARM native GREEN, all eight mutation epochs, affected-family suites,
  paired fleet profiles, warning census, two fresh hostile critics, and fresh
  adjudication remain required.
- The selective `regen --check --grammar` precedence defect and inherited
  generated warning bank remain W0 item 4; neither is repaired here.
- No Value artifact or worktree was read or mutated. No x86 path was used.

## Stop boundary

This is a **zero-credit repair-author handoff**, not R6 acceptance. Root may
consume the exact source/test identities and static specimens for the bounded
Apple-ARM campaign. It may not infer W0 item 3 completion, begin W1, or claim
lock/tranche/prototype/performance movement until the full R6 campaign survives
two fresh hostile reviews and adjudication.
