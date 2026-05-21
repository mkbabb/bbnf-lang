# SK-V13 S-P3 V1 CH4 Cost Challenge

Lens: CH4 cost, wave alignment, hard caps, and same-wave-consumer budget.
Disposition vocabulary: ACCEPT / REVISE / REJECT.

## Verdict

REVISE.

The V1 packet has real cost discipline: P3-A keeps the shortlist to eight
families, P3-B gives LOC and redress caps, SPEC carries owner paths and phase
caps, and DISPATCH requires every wave packet to include caps, rerun ceilings,
revert slices, and consumers. The blocking issue is wave-alignment drift:
P3-B resolves the full-SOTA addendum by packing the campaign into W0-W11 with
explicit bracket-forward semantics, while SPEC/DISPATCH expand the manifest to
W0-W15 plus W10.N/W11.N/W14.N subwave series. That is not yet a single
dispatchable cost model.

## Findings

1. Shortlist size is acceptable. CH4 asks whether the shortlist is <=8 and every
   wave carries cost data (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:128`).
   P3-A lists P3A-0 through P3A-7, eight candidate families, each with owner
   paths, scalar/checkasm status, same-wave consumer, grammar-neutral verdict,
   threshold style, and pre-block notes
   (`restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md:63`).

2. P3-B's cost strategy is coherent by itself. It gives per-wave caps and owner
   families for W0-W11, calls out decision/union/SIMD 45-minute source-edit caps,
   and serializes shared decision-engine work
   (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:65`,
   `restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:72`).
   It also explicitly states that packed subwaves are not permission to exceed a
   hard cap and that any separately dispatched subwave counts against the
   12-wave ceiling
   (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:87`).

3. P3-B correctly identifies the bracket-ceiling problem, but SPEC/DISPATCH do
   not fold that model. P3-B says the literal addendum expansion is over 60
   triumvirate dispatches and not an SK-V13 12-wave bracket, so SK-V13 must use a
   packed W0-W11 sequence and bracket SK-V14 if a packed CSS/JSON lane cannot fit
   (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:130`).
   SPEC instead defines Pre-W0 plus W0-W15 and subwave series W10.N, W11.N, and
   W14.N (`restart/skinny/tranches/sk-v13/SPEC.md:276`). DISPATCH mirrors W0-W15
   and the subwave series (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:150`).
   The SPEC does mention that mechanical 12-wave escalation must not drop pinned
   rows (`restart/skinny/tranches/sk-v13/SPEC.md:296`), but that is not a single
   costed bracket plan.

4. Phase caps are mostly present. SPEC carries research, plan, challenge, and
   redress caps, including the 45+15 minute redress cap for W5-W9 and W12
   (`restart/skinny/tranches/sk-v13/SPEC.md:301`). DISPATCH repeats the
   triumvirate caps and owner-path discipline
   (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:43`). This should be
   preserved, but folded into one canonical wave map.

5. Rerun ceilings and ledger serialization are acceptable. SPEC names rerun
   ceilings by wave family (`restart/skinny/tranches/sk-v13/SPEC.md:310`), and
   P3-B serializes RESULTS, REDRESS, and rolling-delta writes even when worktrees
   benchmark in parallel
   (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:207`).

6. Same-wave-consumer cost is partly covered and overlaps with CH6. DISPATCH
   requires every wave packet to include the consumer, cap, rerun ceiling, and
   revert slice (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:61`). P3-A
   supplies consumer families for the shortlist
   (`restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md:63`).
   The fold should still make the SPEC's W10.N/W11.N/W13/W14.N consumer lines
   explicit so cost and CH6 agree.

## Required Fold Items

1. Choose one canonical wave manifest for V2. Either fold P3-B's packed W0-W11
   bracket into SPEC/DISPATCH, or keep SPEC W0-W15 but make the bracket-ceiling
   accounting explicit enough that each top-level wave and each separately
   dispatched subwave has an unambiguous cost and escalation rule.

2. If SPEC retains W10.N/W11.N/W14.N as subwave series, add a table that states
   which subwaves are planning labels inside a top-level wave and which become
   real triumvirates. For every real triumvirate, account against the skinny
   bracket ceiling and require bracket-forward rather than silent scope drop.

3. Harmonize phase caps between P3-B, SPEC, and DISPATCH. The decision-engine
   and union/SIMD redress cap amendment must be stated the same way in all three
   artifacts.

4. Fold CH6's consumer-line requirement into the cost table: every top-level
   wave and every real subwave needs a same-wave consumer and revert slice before
   dispatch.

## Evidence

- Read P3-A through P3-F, SPEC, DISPATCH, `PASS-3-SYNTHESIS-PLAN.md`,
  `SKINNY-TRIUMVIRATE.md`, and ORCHESTRATOR CH4/3Z rules.
- Local check requested by dispatch:
  `git diff --check -- restart/skinny/tranches/sk-v13/research/p3/hardening/V1/CH4.md`
  (PASS).
