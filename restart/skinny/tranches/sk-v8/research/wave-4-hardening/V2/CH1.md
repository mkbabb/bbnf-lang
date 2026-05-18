# SK-V8 W4 Hardening V2 CH1

Verdict: REVISE.

Confidence: 92%.

## Findings

1. Blocking: the package overclaims closure authority. `HANDOFF.md` says W4 is
   closed and W5 is active, and cites
   `wave-4-hardening/V3/HARDENING-W4-V3-CONSOLIDATED.md` as closure authority.
   That file does not exist in the current tree; W4 hardening currently
   contains only V1 files. This is not honest for a V2 review.
2. The rejection itself is honest. V1 records that correctness passed but
   native Criterion falsified the selected-row gate: Apache passed, `random`
   missed, and `numbers` regressed by +6.3287% time. `skinny/RESULTS.md` and
   `skinny/crates/bbnf-bench/src/direct_struct.rs` have zero diff from HEAD.
3. W4 may reject with source reverted and `RESULTS.md` unchanged. SPEC allows
   behavior waves to either meet row/full-table gates or reject with REDRESS
   evidence, and Section 7's revert protocol explicitly keeps a triage report
   plus REDRESS after failed attempts.
4. V1 REVISE findings are folded sufficiently for rejection. The revised docs
   carry the missing full-table maintain proof, W4-aware checked report path,
   Lock 14 allowance, preblocked-route closure, and digest-arithmetic backstop
   as admission/reopen requirements, not as work required after selected-row
   failure.

## Required Folds

- Replace the W4-closed / W5-active / V3-closure-authority language with
  provisional V2 wording, or add the actual accepted V2/V3 hardening artifacts
  before claiming closure.
- Keep `skinny/RESULTS.md` and `direct_struct.rs` unchanged for this rejection
  path.
- Do not add W4-aware gate/report or Lock 14 source allowance in this rejected
  wave; those remain required only for a future source or row-table admission.
