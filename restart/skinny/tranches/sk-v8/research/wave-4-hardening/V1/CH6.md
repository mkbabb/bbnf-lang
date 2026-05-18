# SK-V8 W4 Hardening V1 CH6

Verdict: ACCEPT.

Confidence: 95%.

Target reviewed: HEAD `5b79d04a`
(`docs(sk-v8-wave4-plan): bound direct Track 2 scalar fold gates`).

## Findings

1. Entry posture is satisfied at the packet level. SPEC W4 requires W0/W1
   admission and W2/W3 admitted, rejected, or explicitly routed before W4.
   HANDOFF records W0-W3 closed, W2 source/product parity admitted with
   row-table admission rejected, W3 rejected/routed, and W4 as the next active
   wave.
2. Triumvirate separation is preserved by the plan artifacts. W4 has a
   research artifact marked non-source and a plan marked pending challenge.
   CH6 acceptance alone does not dispatch redress; W4 may proceed only after
   the V1 challenge cycle is consolidated without blocking REVISE/REJECT and
   the orchestrator/user dispatches implementation.
3. Lock 14 risk is bounded by the plan. The W4 plan nominates only
   `skinny/crates/bbnf-bench/src/direct_struct.rs` and explicitly leaves
   generated Track 1, runtime, codegen, BIR, directives, substrate, and
   generic crates unchanged.
4. The plan introduces no new directive, BIR variant, substrate, public
   substrate API, parser-owned cursor, or side substrate. That matches SPEC
   non-negotiables and W4 pre-blocks.
5. Track 1 / Track 2 separation remains intact if implementation follows the
   plan. Selected rows are Track-2-only misses where generated Track 1 already
   clears the same-run direct floor.
6. No digest-as-product claim is made. W4 remains direct digest guard triage;
   typed product proof remains owned by `real_typed_struct`, and residual rows
   are routed rather than upgraded by digest evidence.
7. Redress requirements are adequate. The plan names row floors, existing GO
   guards, non-target -2.0% maintain, correctness parity, and report-gate
   handling.
8. W4 can proceed to implementation after challenge only conditionally. It
   cannot close without measured selected-row floors, maintained guard rows,
   REDRESS/RESULTS/HANDOFF consistency, and no digest/product overclaim.

## Required Folds

None before implementation.

Carry-forward constraint: if the checked report gate refuses a row-table
refresh because of known W0 run-id drift, W4 must not claim `skinny/RESULTS.md`
admission. Record the outcome explicitly, and add REDRESS if the behavior
attempt fails or cannot be admitted under the checked gate.

Verification note: during review, the worktree already showed
`M skinny/crates/bbnf-bench/src/direct_struct.rs`. This CH6 accepts only the W4
plan at HEAD and does not admit any uncommitted source patch.
