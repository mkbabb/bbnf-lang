# SK-V8 W4 Hardening V2 CH5

Verdict: REVISE.

Confidence: high.

## Findings

1. Blocking: `HANDOFF.md` overclaims closure before V2/V3. It says W4 is
   closed and the next move is W5, but only `wave-4-hardening/V1` exists;
   `wave-4-hardening/V2`, `wave-4-hardening/V3`, and
   `V3/HARDENING-W4-V3-CONSOLIDATED.md` are absent. The W4 plan itself
   correctly says W4 closes only after V2/V3 acceptance.
2. Blocking: `HANDOFF.md` cites nonexistent V3 closure authority.
3. Consistency risk: SPEC still supports gating, while HANDOFF advances state.
   HANDOFF's W5-active wording conflicts with the unresolved hardening state.

## Folds

- `skinny/crates/bbnf-bench/src/direct_struct.rs` is reverted/clean: no
  `git diff` or status entry.
- `skinny/RESULTS.md` is unchanged.
- Rejected patch path is recorded in `skv8-W4-plan.md`, `HANDOFF.md`, and
  `REDRESS.md`.
- `/tmp/skv8-wave4-track2-scalar-fold-rejected.patch` exists and touches only
  `skinny/crates/bbnf-bench/src/direct_struct.rs`.
- REDRESS item 93 coherently records the selected rows, failed Criterion gate,
  no Lock 14 allowance, no admitted source patch, and unchanged RESULTS.
