MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: BB-C.W0 item-3 R5 born-RED author.

# BB-C.W0 ambient optimizer authority — R5 born-RED author freeze

- Frozen: `2026-07-22T21:39:02Z`
- Repository HEAD: `af15f63e0d2d3d719938c13b906a50acbb92ea3b`
- Host observation: Apple `arm64`
- Governing adjudication: `W0-AMBIENT-R4-ADJUDICATION.md` SHA-256
  `ecbeba2589a2df69e7207185200ec9967bf3b5303a7bb058cb6af67d125377db`
- Scope: only the three source-isomorphic lowerer witnesses and the late-merge
  objective assertion ordered by the R4 adjudication
- Execution status: **STATIC BORN-RED FREEZE; NO TEST OR BUILD RUN**
- Credit: **zero BB-C.W0, wave, lock, tranche, or movement credit**
- Hostile clean count: **0/2**
- Advancement: **BB-C.W1 may not begin**

The task envelope explicitly declared `gpt-5.6-sol` at `ultra` effort and
forbade inherited identity. The agent record above independently spells the
same model and effort; declaration-to-record discrepancy count is zero. No
implicit or inherited model selection is claimed.

The 712-line / 52,638-byte governing adjudication was read in full before any
edit, and its digest recomputed exactly. No Cargo command, build, test,
benchmark, script, formatter write, mutation, x86 route, source repair,
package/config/lock edit, commit, staging action, or other repository was used.

## Exact born-RED subject

Only external, source-isomorphic test bytes changed. The production lowerer,
optimizer, generator, runtime, and report schema are unchanged.

| Path | Pre-R5 SHA-256 | R5 SHA-256 | Lines / bytes |
|---|---|---|---:|
| `crates/core/tests/lower/expression/wrap.rs` | `3f7b6e3637eaeed1cec393c27760e3419994254c5d972f917b1d79be3c954e77` | `6b1190cd7bd3d26c71d2429b7eec7d0e2c5e2e9ce9ae4138a9e9708243ee8a48` | 212 / 6,749 |
| `crates/ir/tests/vm/cross_rule_csp.rs` | `d1936b1c6c97c438dbb2fb9535d322666ebd7c09973e6deef30daec9fcb8735d` | `ac5c2292151284d9193f7025aae84f038e3cbcba182f8c6fd90ae61877b24f52` | 793 / 27,493 |

`Cargo.lock` remains 4,650 lines / 113,885 bytes at SHA-256
`480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9`.
This receipt was absent before R5. Its final digest is necessarily reported by
the author handoff because a file cannot contain its own SHA-256.

The worktree contains extensive concurrent owner-authorized bytes. They were
observed but not normalized, formatted, staged, reverted, censused as an
authority claim, or otherwise touched by this slice.

## Frozen semantic witnesses

### Comment-contained slash and arrow

`block_comment_slash_arrow_is_not_a_map` uses the exact valid grammar:

```text
subject = /*a/b -> c*/ "x" ;
```

It requires parsing and lowering to complete without panic, the rule body to
remain literal `"x"`, a total Map count of zero, and no function descriptor.
The comment is grammar whitespace, so neither its slash nor its arrow may own a
map.

Against the unchanged R4 lowerer this is expected RED before assertions:
`find_direct_arrow` treats the opening comment slash as a regex delimiter,
closes it at the body slash, sees the comment arrow at direct depth, and reaches
the missing-structural-child panic.

### Division inside a mapped group

`division_inside_group_does_not_hide_outer_typed_map` uses the exact valid
grammar:

```text
subject = ("x" -> input / 2) -> 1u8 ;
```

It requires exactly two Maps. The inner descriptor must be exactly
`Expr(BinOp(Div, Input, IntLit(2)), None)` over literal `"x"`; the outer
descriptor must be exactly `Expr(IntLit(1), Some(U8))`. It also requires exactly
two function descriptors, excluding duplicated or displaced mapping.

Against the unchanged R4 lowerer this is expected RED at the outer descriptor:
the division slash is treated as an unterminated regex delimiter, so the
whole-span scan hides the genuine outer arrow and numeric recovery loses `U8`.

### Numeric recovery boundary

`numeric_suffix_recovery_is_confined_to_one_structural_literal` pairs:

```text
single = "x" -> 1u8 ;
compound = "y" -> 1 + input ;
```

The single structural literal must retain `Expr(IntLit(1), Some(U8))`. The
compound must retain exactly `Expr(BinOp(Add, IntLit(1), Input), None)` and no
descriptor in the fixture may carry `Some(TypeDesc::Named(_))`. Thus positive
suffix recovery remains available only for one bounded numeric literal, while
trailing compound-expression text cannot become a type name.

Against the unchanged R4 lowerer this is expected RED because the typed first
leaf has no text span; fallback passes the full `1 + input` remainder through
`split_numeric_suffix`, and the trailing ` + input` bytes are interned as a
spurious named return type.

### Late-merge report closure

The existing external
`conflicting_disconnected_shared_node_is_not_published` transaction witness is
unchanged except for the born-RED assertion
`error.reports[1].objective == None`. Its existing assertions still bind the
typed `InvalidAssignment`, exact merge locus, two reports, completed first
report, invalid second report, and exact failure locus.

Against the unchanged R4 solver this assertion is expected RED because the
second report retains the locally successful objective after merge invalidates
publication.

No runtime result is claimed. These RED modes are source predictions awaiting
the root-owned once-only Apple-ARM preimage campaign.

## Exact intended Apple-ARM selectors

The root campaign must run each command once before production repair, preserve
raw stdout/stderr and exit, and use no retry or x86 route:

```text
/usr/bin/arch -arm64 env CARGO_BUILD_JOBS=1 cargo test --target aarch64-apple-darwin -p bbnf --lib lower::expression::wrap::tests::block_comment_slash_arrow_is_not_a_map -- --exact --nocapture

/usr/bin/arch -arm64 env CARGO_BUILD_JOBS=1 cargo test --target aarch64-apple-darwin -p bbnf --lib lower::expression::wrap::tests::division_inside_group_does_not_hide_outer_typed_map -- --exact --nocapture

/usr/bin/arch -arm64 env CARGO_BUILD_JOBS=1 cargo test --target aarch64-apple-darwin -p bbnf --lib lower::expression::wrap::tests::numeric_suffix_recovery_is_confined_to_one_structural_literal -- --exact --nocapture

/usr/bin/arch -arm64 env CARGO_BUILD_JOBS=1 cargo test --target aarch64-apple-darwin -p bbnf-ir --test vm cross_rule_csp::conflicting_disconnected_shared_node_is_not_published -- --exact --nocapture
```

## Production-dependent counterfactual specification

The adjudication orders nine surgical one-hunk counterfactual categories. No
exact patch can honestly be derived from the current pre-repair source for a
parser-owned boundary or deterministic cancellation seam that does not yet
exist. Therefore this freeze records their specification, not fictitious patch
bytes.

After the production hunks and all named external/private-test oracles exist,
but before any post-repair native or mutant execution, the repair author must
freeze each exact unified patch, target preimage, digest, independently
selectable selector, and semantic kill. Each patch must then be applied alone,
run once on Apple ARM, reversed immediately, and followed by a byte-exact
restoration check. If one planned boundary remains in separate production
hunks, it must split into additional surgical patches rather than become a
coupled specimen.

| ID | Required one-hunk inversion after repair exists | Required independently selectable oracle(s) | Required semantic kill |
|---|---|---|---|
| R5-M01 | Delete/bypass the parser-owned direct-factor/suffix boundary check while leaving structural Map construction intact. | `division_inside_group_does_not_hide_outer_typed_map`; `block_comment_slash_arrow_is_not_a_map` as the no-false-owner control. | The mutated lowerer must either lose the exact outer `U8`, invent comment ownership, or violate the bounded-suffix invariant; a compile failure is not credit. |
| R5-M02 | Retain the local success objective when checked merge rewrites the report to `InvalidAssignment`. | `cross_rule_csp::conflicting_disconnected_shared_node_is_not_published`. | The exact `objective == None` assertion must fail after every preceding transaction assertion is reached. |
| R5-M03 | Delete only the production factorized/constrained pre-accept cancellation observation, preferably one shared boundary helper. | `passes::csp_strategy::tests::factorized_acceptance_observes_boundary_cancellation` and `passes::csp_strategy::tests::constrained_acceptance_observes_boundary_cancellation`. | Each relevant route must illicitly accept/publish instead of returning its one cancelled failing report. |
| R5-M04 | Delete only the completed-component/before-next-component cancellation observation. | `cross_rule_csp::completed_component_then_cancel_reports_completed_plus_boundary`. | The transaction must proceed past the completed-plus-boundary sequence or expose output; pre-cancel interception is not credit. |
| R5-M05 | Delete only final-success cancellation observation. | `passes::csp_strategy::tests::final_return_observes_boundary_cancellation`. | The mutated transaction must reach `Ok` or retain a success report instead of rewriting the last report to cancelled with no output. |
| R5-M06 | Delete only solver-stats cancellation rejection, retaining current-token rejection. | A frozen `passes::csp_strategy::tests::solver_stats_cancellation_is_not_publishable` fixture that reaches the stats-only observation. | The named stats-only oracle must expose acceptance; failure through a still-set current token is not credit. |
| R5-M07 | Delete only current-token cancellation rejection, retaining solver-stats rejection. | `passes::csp_strategy::tests::cancellation_on_final_singleton_leaf_discards_incumbent`, or its exact boundary-sensitive replacement. | The final-leaf incumbent must be accepted while solver stats alone remain non-cancelling; killing a compound stats-or-token patch is not credit. |
| R5-M08 | Accept incomplete or oversized solution length while leaving family/VarId validation intact. | A frozen `passes::csp_strategy::tests::decoder_rejects_incomplete_and_oversized_assignments` selector. | Both length-direction assertions must reach the mutated length guard; aborting at an earlier unrelated assertion is not credit. |
| R5-M09 | Accept a foreign `VarId` or fill a missing value while leaving length validation intact. | A frozen `passes::csp_strategy::tests::decoder_rejects_foreign_var_and_missing_value` selector. | The foreign/missing assertions must reach the mutated VarId/value guard; a length mismatch or compilation failure is not credit. |

The M03-M09 selector bodies are not part of this narrow born-RED edit and must
be authored with the production boundary repair before patch freezing. Their
names above bind the semantic contracts; the future mutation manifest must bind
the exact realized paths, line contexts, and hashes.

## Read-only static checks

The following non-mutating checks were run over the two exact test owners:

- `rustfmt --edition 2024 --check`: exit `0`;
- scoped tracked `git diff --check`: exit `0` with no output;
- no-index diff check for the untracked lowerer test owner: expected difference
  exit `1` with zero-byte whitespace diagnostics;
- exact selector census: all three new lowerer names and the late-merge
  objective assertion occur once; and
- post-edit SHA-256 and line/byte counts: equal to the subject table.

No formatter was run in write mode. No test executable was built or run.

## Explicitly routed remainder

This freeze does not repair parser/generator bounds, lowerer arrow ownership,
numeric recovery, report closure, cancellation boundaries, decode guards, or
the R4-local `NodeId` warning. It does not import malformed `RuleId` totality,
dead context, projection simplification, privacy work, broader warning cleanup,
W0 items 1/2/4/5, parse-that, Value, Glass, SCI, or Atlas.

The next root action is the four-command once-only born-RED Apple-ARM capture.
Only after those raw preimages are frozen may an R5 repair author touch
production. Post-repair native coverage, surgical mutant execution, two fresh
hostile critics, and a fresh adjudicator remain mandatory.

**R5 BORN-RED FREEZE ONLY. NO RUN. NO PRODUCTION REPAIR. CREDIT 0. HOSTILE
CLEAN 0/2. BB-C.W1 MAY NOT BEGIN.**
