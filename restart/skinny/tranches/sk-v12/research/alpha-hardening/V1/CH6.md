# SK-V12 Pass Alpha CHALLENGE V1 - CH6 Next-Tranche Impact

Pass: Pass Alpha SK-V11 -> SK-V12.
Cycle: V1.
Lens: CH6 - Next-Tranche Impact / anti-paper-close.
Date: 2026-05-20.
Disposition: REVISE.

## Scope

This review checks whether the SK-V12 Alpha packet is ready to hand the next
tranche to G-Alpha and then S-P1/S-P2/S-P3 without paper-close, role merger, or
unbounded implementation drift. The reviewed axes are revert protocol coverage,
hard caps, triumvirate discipline, measurable close conditions, G-Alpha
presentation readiness, and S-P1/S-P2/S-P3 dispatch clarity.

Primary authorities:

- `restart/prompts/pass-contracts/PASS-ALPHA.md` Section 3 and Sections 7-10.
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.
- `restart/prompts/ORCHESTRATOR.md`.
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`.
- `restart/skinny/tranches/sk-v12/HANDOFF.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md`.
- `restart/skinny/tranches/sk-v11/research/close/close-redress.md`.

## Disposition Summary

| Axis | Disposition | Blocking? |
|---|---|---|
| Revert protocol coverage | REVISE | yes |
| Hard caps | REVISE | yes |
| Triumvirate discipline | ACCEPT | no |
| Measurable close conditions | ACCEPT | no |
| G-Alpha presentation readiness | REVISE | yes, inherits hard-cap/revert gaps |
| S-P1/S-P2/S-P3 dispatch clarity | ACCEPT | no |

Overall CH6 disposition is REVISE. There is no REJECT: the contract's next
tranche direction is coherent and bench-bound, but G-Alpha is not presentation
ready until the two critical folds below are added.

## Critical Findings

### CH6-1 - Candidate-local revert protocol is missing

Disposition: REVISE.

`PASS-ALPHA` asks CH6 to verify whether the SK-V{N+1} contract specifies
revert protocol per intervention (`restart/prompts/pass-contracts/PASS-ALPHA.md:47`).
It also says S-P3's wave plan must carry a revert protocol for each wave
(`restart/prompts/pass-contracts/PASS-ALPHA.md:114`-
`restart/prompts/pass-contracts/PASS-ALPHA.md:120`). The triumvirate plan
schema is explicit: each selected intervention needs owner paths,
falsifiability gate, hard cap, revert protocol, same-wave consumer, and
pre-blocked routes (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:55`-
`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:62`). Redress failure
must revert, record evidence, and save the rejected patch
(`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:67`-
`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:75`).

Alpha-E gives owner paths, scalar/oracle status, same-wave consumers,
falsifiability gates, LOC budgets, risk, and pre-blocked adjacency for E1-E5
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:37`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:297`).
It does not name a revert protocol for any candidate. Alpha-F and the public
contract do add fail-closed refusal conditions, but they do not say what is
rolled back when a generated runtime/report/bench row lands and the gate fails
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:175`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:197`;
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:217`-
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:230`).

This is critical because E1/E2/E3 cross first-of-class codegen/runtime/report
surfaces, and E4/E5 are explicitly conditional on prior row evidence. Without a
candidate-local failure protocol, S-P3 can still write one later, but G-Alpha
cannot honestly present the current Alpha packet as CH6-complete.

Required fold:

- Add a `Revert protocol` field to each Alpha-E candidate, or add an equivalent
  SK-V12 Alpha revert matrix consumed by `SYNTHESIS.md` / `HANDOFF.md`.
- Each protocol must name the rollback slice for source, generated runtime
  output, benchmark/report/gate edits, `skinny/RESULTS.md`, and `skinny/REDRESS.md`.
- Each protocol must state whether the failure blocks dependent candidates
  such as E4 after E1 failure or E5 without E4 evidence.
- Each failed implementation wave must save `/tmp/skv12-wave{W}-rejected.patch`
  and record the REDRESS entry, matching triumvirate redress discipline.

### CH6-2 - Hard caps are not present at candidate or G-Alpha depth

Disposition: REVISE.

`PASS-ALPHA` makes hard caps part of the CH6 review question
(`restart/prompts/pass-contracts/PASS-ALPHA.md:47`) and part of the G-Alpha
presentation summary (`restart/prompts/pass-contracts/PASS-ALPHA.md:169`-
`restart/prompts/pass-contracts/PASS-ALPHA.md:174`). `ORCHESTRATOR.md` also
requires every dispatch to carry an explicit minute cap and halt/escalate at
the cap (`restart/prompts/ORCHESTRATOR.md:224`-
`restart/prompts/ORCHESTRATOR.md:226`).

Alpha-E names LOC budgets for E1-E5
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:79`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:129`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:176`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:225`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:287`),
but those are not minute hard caps. `SYNTHESIS.md` and `HANDOFF.md` correctly
defer the detailed wave plan to S-P3
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:5`-
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:8`;
`restart/skinny/tranches/sk-v12/HANDOFF.md:5`-
`restart/skinny/tranches/sk-v12/HANDOFF.md:8`), but neither file supplies the
hard-cap summary that G-Alpha must present.

This is critical because the highest-risk candidates are first-of-class
runtime/codegen work and can otherwise become an open-ended "stand up the
baseline" effort before S-P3 has constrained the wave.

Required fold:

- Add candidate-level maximum wall-clock caps for S-P3 planning and redress
  intent, or state a binding default that S-P3 must preserve unless it escalates
  to the user.
- Make the G-Alpha summary able to present both LOC budgets and minute caps for
  E1-E5.
- Mark E1/E4 first-of-class and therefore challenge-mandatory before redress
  under the triumvirate rule
  (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:112`-
  `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:125`).

### CH6-3 - G-Alpha presentation is not yet ready

Disposition: REVISE.

The G-Alpha boundary is present: no SK-V12 dispatch occurs before G-Alpha or
user pin (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:27`-
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:29`;
`restart/skinny/tranches/sk-v12/HANDOFF.md:102`-
`restart/skinny/tranches/sk-v12/HANDOFF.md:109`). `ORCHESTRATOR.md` makes
G-Alpha mandatory and requires explicit user confirmation before advancing
(`restart/prompts/ORCHESTRATOR.md:163`-
`restart/prompts/ORCHESTRATOR.md:172`).

However, `PASS-ALPHA` says the user-facing G-Alpha presentation must include a
summary of rows targeted, interventions, LOC budget, hard caps, and pre-blocked
routes, plus the predicted close state
(`restart/prompts/pass-contracts/PASS-ALPHA.md:169`-
`restart/prompts/pass-contracts/PASS-ALPHA.md:174`). The current materials have
the pieces for rows, interventions, LOC budgets, telemetry, pre-blocks, and
close target, but the missing hard caps and revert protocols above leave the
presentation incomplete. The packet also does not yet provide one concise
G-Alpha-ready summary surface tying the Alpha-A/B current deltas to the
Alpha-E/F predicted SK-V12 close state.

Required fold:

- Add a G-Alpha summary section or artifact that enumerates E1-E5 with target
  rows, LOC budget, minute hard cap, pre-blocked routes, and failure/revert
  action.
- Include the predicted close state explicitly: "baseline + intervention
  admitted", "measured BLOCKED generated baseline", or "revise/escalate" with
  the evidence needed for each.

## Accepted Findings

### CH6-4 - Triumvirate role separation is preserved

Disposition: ACCEPT.

The packet does not merge Alpha, S-P1/S-P2/S-P3, and wave redress roles.
`SYNTHESIS.md` says Alpha is not behavior implementation authority and does not
create `SPEC.md` or `DISPATCH-PROMPT.md` because S-P3 owns the later wave plan
after S-P1/S-P2 convergence (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:5`-
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:8`). It also refuses source edits
before a selected S-P3 wave entry gate exists and passes
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:217`-
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:223`). `HANDOFF.md` repeats that
S-P3 owns `SPEC.md` / `DISPATCH-PROMPT.md`
(`restart/skinny/tranches/sk-v12/HANDOFF.md:5`-
`restart/skinny/tranches/sk-v12/HANDOFF.md:8`) and refuses source edits before
the implementation packet exists
(`restart/skinny/tranches/sk-v12/HANDOFF.md:83`-
`restart/skinny/tranches/sk-v12/HANDOFF.md:100`).

This aligns with the load-bearing triumvirate rule that research, plan, and
redress commits stay separate and that redress cannot dispatch without an
antecedent plan (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:190`-
`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:200`).

### CH6-5 - The close condition is measurable and bench-bound

Disposition: ACCEPT.

SK-V12 close is not a prose claim. The contract requires G-Alpha/S-P1/S-P2/S-P3
convergence, a fresh S-P1 baseline, exactly one generated non-JSON baseline
before JSON-only work, one grammar-generalized intervention clearing
`ceil(baseline_mbps * 1.01)`, guard-row preservation, parse-only diagnostics
only, and same-wave gate consumption
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:25`-
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:70`). The close target refuses
another JSON-only cycle first
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:72`-
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:76`).

Alpha-E supplies measurable candidate gates: generated non-JSON baseline rows
must have finite Track 1 and independent oracle/Track 2 Mbps, strict equality,
and sample counts (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:67`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:78`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:118`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:128`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:164`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:175`).
The first intervention must clear the baseline by at least 1%
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:214`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:224`).
The conditional JSON companion has named direct floors and is lowest priority
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:235`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:297`).

### CH6-6 - SK-V11 close feedback is carried forward correctly

Disposition: ACCEPT.

The SK-V11 close authority says W9 made no behavior, generated runtime,
benchmark, gate, or `skinny/RESULTS.md` change and preserved the unchanged
`N-direct / NoGo` surface (`restart/skinny/tranches/sk-v11/research/close/close-redress.md:10`-
`restart/skinny/tranches/sk-v11/research/close/close-redress.md:28`). It also
states that every residual direct row has a per-row fixpoint proof under
REDRESS 119 and that SK-V12 should solve the generated non-JSON baseline before
another JSON-only micro-wave
(`restart/skinny/tranches/sk-v11/research/close/close-redress.md:46`-
`restart/skinny/tranches/sk-v11/research/close/close-redress.md:82`).

SK-V12 inherits that posture. Its direct residual table is marked as a reopen
ledger, not the first target, and it requires fresh material evidence before
any JSON direct row reopens
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:57`-
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:64`;
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:90`-
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:129`).

### CH6-7 - S-P1/S-P2/S-P3 dispatch clarity is sufficient

Disposition: ACCEPT.

The next move is S-P1 Profile, not implementation. `SYNTHESIS.md` tells S-P1 to
freeze the SK-V12-open surface, profile guard rows, inventory the generated
non-JSON blocker, audit the W1a report lane, identify the smallest runnable
generated non-JSON baseline, keep JSON direct data diagnostic unless the
REDRESS 114-119 reopen rule is met, and separate microbench inventory from
dispatch authority (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:240`-
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:259`). `HANDOFF.md` repeats the
same next move and instructs S-P1 to name the first runnable generated non-JSON
baseline candidate for S-P2/S-P3
(`restart/skinny/tranches/sk-v12/HANDOFF.md:102`-
`restart/skinny/tranches/sk-v12/HANDOFF.md:109`).

S-P2 and S-P3 are also bounded: S-P1/S-P2/S-P3 convergence is required in the
close condition (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:27`-
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:29`), Alpha-F only names candidate
space for S-P1 to profile and S-P2 to ground
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:155`-
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:167`), and S-P3 later authors the
implementation packet (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:261`-
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:267`).

## Required Folds Before G-Alpha

1. Add candidate-level revert protocols for E1-E5 or an equivalent Alpha-level
   revert matrix.
2. Add minute hard caps for E1-E5 and expose them in the G-Alpha summary.
3. Add or update a G-Alpha-ready summary tying Alpha-A/B current deltas to the
   Alpha-E/F predicted SK-V12 close state.

After those folds, CH6 should be able to move to ACCEPT if no new candidate
adds an unmeasurable gate, role merger, or hidden implementation dispatch.
