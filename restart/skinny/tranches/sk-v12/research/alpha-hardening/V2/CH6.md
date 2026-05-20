# SK-V12 Pass Alpha CHALLENGE V2 - CH6 Next-Tranche Impact

Pass: Pass Alpha SK-V11 -> SK-V12.
Cycle: V2.
Lens: CH6 - Next-Tranche Impact / anti-paper-close.
Date: 2026-05-20.
Disposition: ACCEPT.

## Scope

This V2 review checks whether the CH6 blockers from V1 were folded after
commit `18f4b931` and whether the next-tranche contract is safe to present at
G-Alpha without paper-close, role merger, or unbounded implementation drift.

Primary materials read:

- `restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CH6.md`.
- `restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CONSOLIDATED.md`.
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`.
- `restart/skinny/tranches/sk-v12/HANDOFF.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md`.
- `restart/skinny/tranches/sk-v11/research/close/close-redress.md`.
- `restart/skinny/tranches/sk-v11/research/g-alpha/G-ALPHA-SK-V11.md`.

V1 CH6 and the V1 consolidated report required three CH6 folds before G-Alpha:
candidate-local or equivalent revert protocols, minute hard caps, and a
G-Alpha-ready summary with predicted close state
(`restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CH6.md:268-274`;
`restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CONSOLIDATED.md:35-38`).
All three are now present.

## Disposition Summary

| Axis | Disposition | Blocking? |
|---|---|---|
| Candidate-local / equivalent revert protocols | ACCEPT | no |
| Hard caps | ACCEPT | no |
| G-Alpha summary and predicted close state | ACCEPT | no |
| Triumvirate role separation | ACCEPT | no |
| Measurable close condition | ACCEPT | no |
| S-P1/S-P2/S-P3 clarity | ACCEPT | no |

Overall CH6 disposition is ACCEPT. No V2 CH6 blocker remains.

## Findings

### CH6-1 - Revert protocols are now present

Disposition: ACCEPT.

Alpha-E now carries an Alpha-level cost, cap, and revert matrix for E1-E5 with
candidate wave slot, LOC budget, risk, plan cap, redress cap, same-wave
consumer, split-before-dispatch rule, and revert protocol
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:35-51`).
Each candidate section points back to that matrix for its hard cap and revert
rule (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:114-117`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:165-168`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:213-216`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:275-278`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:338-340`).

The protocols cover the slices V1 asked to name: codegen/runtime or selected
source, bench, report, gate, `skinny/RESULTS.md`, rejected patch preservation,
and dependent-wave blocking or preservation rules. Alpha-E also requires every
failed behavior wave to record a numbered REDRESS entry and preserve the
rejected patch
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:43-51`).
Alpha-F, `SYNTHESIS.md`, and `HANDOFF.md` carry the same revert seed into the
contract surface (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:188-203`;
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:181-197`;
`restart/skinny/tranches/sk-v12/HANDOFF.md:90-103`).

### CH6-2 - Hard caps are explicit and binding

Disposition: ACCEPT.

E1-E5 now each have a 30 minute plan cap and 75 minute redress cap, alongside
LOC budgets and risk class
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:41-51`).
Alpha-F states that exceeding the LOC budget or redress cap returns REVISE
before behavior dispatch unless the user grants an extension after CHALLENGE
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:190-203`).
`SYNTHESIS.md` and `HANDOFF.md` repeat the same cap matrix and require CHALLENGE
plus user escalation to widen it (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:181-197`;
`restart/skinny/tranches/sk-v12/HANDOFF.md:90-103`).

That satisfies the CH6 hard-cap requirement in `PASS-ALPHA` and the orchestrator
rule that every dispatch carries an explicit minute cap
(`restart/prompts/pass-contracts/PASS-ALPHA.md:47-49`;
`restart/prompts/ORCHESTRATOR.md:224-227`).

### CH6-3 - G-Alpha has a presentable summary and predicted close state

Disposition: ACCEPT.

The public contract now contains a G-Alpha summary naming the targeted rows,
the intervention delta, JSON residual routing, and the predicted close state:
success is one generated non-JSON baseline plus one same-plane intervention;
honest block is a measured generated-baseline block inside the accepted owner
surface; JSON companion work remains conditional
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:290-306`). Alpha-F and Handoff
carry the same presentation seed (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:239-254`;
`restart/skinny/tranches/sk-v12/HANDOFF.md:124-136`).

The rows/interventions summary is backed by the E1-E5 cost/cap/revert matrices
and the pre-blocked route list, so the G-Alpha presentation has the required
rows targeted, interventions, LOC budgets, hard caps, pre-blocked routes, and
predicted close state (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:181-244`;
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:35-51`).

### CH6-4 - Triumvirate role separation is preserved

Disposition: ACCEPT.

`SYNTHESIS.md` states that Pass Alpha is not behavior implementation authority
and does not create `SPEC.md` or `DISPATCH-PROMPT.md`; S-P3 owns the later wave
plan after S-P1/S-P2 converge
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:5-8`). `HANDOFF.md` repeats that
boundary (`restart/skinny/tranches/sk-v12/HANDOFF.md:5-8`).

The refusal conditions also block source edits before the selected S-P3 wave
entry gate exists and before the implementation packet converges
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:246-267`;
`restart/skinny/tranches/sk-v12/HANDOFF.md:105-122`). This matches the
triumvirate rule that research, plan, and redress remain separate and that no
redress dispatch occurs without an antecedent plan
(`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:190-200`).

### CH6-5 - Close remains measurable and bench-bound

Disposition: ACCEPT.

The SK-V12 close condition is now a concrete bench-gated target: generated
non-JSON baseline first, one grammar-generalized intervention clearing
`ceil(baseline_mbps * 1.01)`, guard preservation, parse-only diagnostic status,
direct residual pre-blocks, telemetry before behavior, and close-doc agreement
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:25-81`).

Alpha-E gives per-candidate falsifiability gates for E1-E3 baselines, E4
intervention, and E5 conditional JSON companion, including finite positive
throughput, strict equality, sample recording, independent oracle/Track 2
requirements, and concrete floor thresholds where JSON companion rows are
eligible (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:102-113`;
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:154-164`;
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:201-212`;
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:264-274`;
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:326-337`).

The close posture also honors the SK-V11 feedback: SK-V11 closed only as a
measured fixpoint, with no generated non-JSON admission, and routed SK-V12 to
solve the generated non-JSON baseline before another JSON-only micro-wave
(`restart/skinny/tranches/sk-v11/research/close/close-redress.md:52-82`).

### CH6-6 - S-P1/S-P2/S-P3 clarity is sufficient

Disposition: ACCEPT.

The next move is explicitly S-P1 Profile after G-Alpha or user pin
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:308-314`;
`restart/skinny/tranches/sk-v12/HANDOFF.md:124-131`). S-P1 must freeze the
SK-V12-open surface, profile guards, inventory the generated non-JSON blocker,
audit the REDRESS 111 report lane, and name the smallest runnable generated
non-JSON baseline candidate
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:269-288`).

S-P2's role is bounded to grounding the Alpha-F candidate space, not source
dispatch, and S-P3 owns the eventual implementation packet and wave sequencing
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:167-180`;
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:308-314`). That is clear enough
for the next tranche: G-Alpha presents the contract, S-P1 profiles, S-P2
grounds, and S-P3 later authors `SPEC.md` / `DISPATCH-PROMPT.md`.

## Verdict

ACCEPT.

The V1 CH6 blockers are folded. The SK-V12 Alpha packet now gives G-Alpha a
bounded, role-separated, bench-verifiable contract with E1-E5 caps and revert
protocols, explicit predicted close states, and a clear S-P1 -> S-P2 -> S-P3
handoff path.
