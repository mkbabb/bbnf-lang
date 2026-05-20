# SK-V12 S-P3 CHALLENGE V1: CH4 Cost

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-20.
Lens: CH4 COST.
Output: this file.

Disposition: REVISE.

## Findings

### CH4-1 - Promoted cost tables drop risk and diverge on redress caps

P3-B carries the cost surface CH4 needs: the manifest has a `LOC / risk`
column and per-wave caps, including W0 and W4 as `<=90 min wall, redress
<=75 min` (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:67`,
`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:69`,
`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:73`).
The promoted-facing draft loses that structure: P3-F lists `LOC cap` and
`Redress cap` but no risk column, and gives W0/W4 `<=90 min`
(`restart/skinny/tranches/sk-v12/research/p3/p3f-spec-draft.md:79`,
`restart/skinny/tranches/sk-v12/research/p3/p3f-spec-draft.md:81`,
`restart/skinny/tranches/sk-v12/research/p3/p3f-spec-draft.md:85`). SPEC
also omits risk and sets W0/W4 `Implementation/redress cap` to `<=90 min`
(`restart/skinny/tranches/sk-v12/SPEC.md:248`,
`restart/skinny/tranches/sk-v12/SPEC.md:250`,
`restart/skinny/tranches/sk-v12/SPEC.md:254`), then repeats a 90-minute
redress allowance for W0/W4 (`restart/skinny/tranches/sk-v12/SPEC.md:256`,
`restart/skinny/tranches/sk-v12/SPEC.md:263`). DISPATCH carries only hard
caps, no LOC or risk budget (`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:39`,
`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:45`).

That conflicts with the per-wave contract: redress is `60 min implementation
+ 15 min measurement` (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:65`,
`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:71`) and the hard-cap
table binds redress at 75 minutes (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:165`,
`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:172`). It also weakens
the CH4 question, which asks every wave to carry LOC budget, hard cap, phase
breakdown, and same-wave-consumer requirements (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:128`,
`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:132`).

Fold revision:

- In `p3f-spec-draft.md`, `SPEC.md`, and `DISPATCH-PROMPT.md`, add a risk
  column to the wave manifest and copy the P3-B risk classes exactly: W0
  low-medium, W1 high, W2 high, W3 high, W4 medium.
- Normalize caps to `wall cap` and `redress cap`. Redress is `<=75 min` for
  every wave. W0/W4 may retain `<=90 min wall` only as plan/close/gate
  overhead outside the redress slice.
- Align SPEC Section 2 phase caps with SKINNY-TRIUMVIRATE: research 30 min,
  plan 30 min, CHALLENGE 60-90 min when required, redress 75 min.

### CH4-2 - W1 fallback mechanics can hide multiple baseline waves inside one redress

The W1 fallback order is stated as CSS L4, then Sheets, then BBNF-self
(`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:86`,
`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:90`;
`restart/skinny/tranches/sk-v12/SPEC.md:376`,
`restart/skinny/tranches/sk-v12/SPEC.md:385`). The W1 task list is not a
small fixture flip: it must break the JSON-provider-only emission blocker,
build a generated runtime, add fixture/oracle coverage, measure Track 1 and
Track 2, and gate-consume provenance and guard state
(`restart/skinny/tranches/sk-v12/SPEC.md:387`,
`restart/skinny/tranches/sk-v12/SPEC.md:397`). SPEC gives separate LOC caps
for CSS, Sheets, and BBNF-self but does not state whether those caps are
alternative or cumulative inside a single redress
(`restart/skinny/tranches/sk-v12/SPEC.md:251`). P3-C already sketches a legal
split if W1 preflight cannot fit, W1a generator/runtime unblock followed by
W1b baseline throughput (`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:58`,
`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:63`),
but SPEC leaves that split to later CHALLENGE acceptance instead of putting it
in the dispatchable wave plan (`restart/skinny/tranches/sk-v12/SPEC.md:423`,
`restart/skinny/tranches/sk-v12/SPEC.md:425`).

As written, an implementation agent could attempt CSS, then Sheets, then
BBNF-self during one W1 redress and still claim it was "inside the wave." That
is cost-hidden multi-wave work.

Fold revision:

- Make W1 target selection plan-time only. The W1 plan may evaluate CSS,
  Sheets, and BBNF-self preflight, but redress attempts exactly one selected
  target.
- If the selected target blocks during redress, W1 records REDRESS `BLOCKED`
  or `REJECTED`; it does not fall through to the next grammar in the same
  redress.
- If S-P3 wants fallback execution in the same bracket, promote P3-C's W1a/W1b
  split into the manifest now, with each sub-wave carrying its own LOC, risk,
  cap, gate, revert protocol, and consumer. The bracket remains below the
  12-wave ceiling even with W1a/W1b.

### CH4-3 - W2 accounts for scalar/checkasm/microbench gates, but not their cost slices

The packet correctly requires scalar reference, checkasm/parity, microbench,
and same-wave generated consumer before W2/W3 primitives reach redress
(`restart/skinny/tranches/sk-v12/SPEC.md:296`,
`restart/skinny/tranches/sk-v12/SPEC.md:309`). W2 entry and exit gates also
require scalar reference, microbench, parity/checkasm where applicable, and a
same-wave generated consumer (`restart/skinny/tranches/sk-v12/SPEC.md:440`,
`restart/skinny/tranches/sk-v12/SPEC.md:450`,
`restart/skinny/tranches/sk-v12/SPEC.md:462`,
`restart/skinny/tranches/sk-v12/SPEC.md:470`). P3-A shows why this matters:
the candidate families can touch SIMD sources, scalar references, checkasm
tests, parse-that, generated runtime, report, and gate in the same slice
(`restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:169`,
`restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:178`,
`restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:196`,
`restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:202`,
`restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:221`,
`restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:227`).

The cost table gives W2 a single `<=430 source/test/gate LOC` cap and names
generated output separately (`restart/skinny/tranches/sk-v12/SPEC.md:252`).
That is plausible only if the W2 plan breaks the cap into scalar reference,
parity/checkasm, microbench, generated consumer, and gate/report slices. The
current SPEC requires the proof, but does not require that cost breakdown.

Fold revision:

- Add to SPEC Section 5 and DISPATCH Phase 2: every W2 plan must include a
  five-part cost table: scalar reference LOC, parity/checkasm LOC, microbench
  LOC, generated consumer LOC, and report/gate LOC.
- If the selected W2 family cannot fit those slices within `<=430` non-generated
  LOC and `<=75 min` redress, the plan must return REVISE before source work or
  split into a separately listed wave under the <=12 bracket ceiling.

## Passing Checks

- Wave count is within the ceiling. P3-B sequences W0-W4 and states the bracket
  is five waves, below the <=12 skinny ceiling
  (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:77`,
  `restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:84`);
  ORCHESTRATOR escalates only when a skinny wave bracket exceeds 12 waves
  (`restart/prompts/ORCHESTRATOR.md:125`,
  `restart/prompts/ORCHESTRATOR.md:128`).
- Shortlist size is within the cap. P3-A defines C1-C3 plus C4-C8, exactly
  eight candidates (`restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:50`,
  `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:54`,
  `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:58`,
  `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:245`);
  PASS-3 caps P3-A at <=8 candidates
  (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:56`,
  `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:59`).
- Revert protocols exist for all waves: W0, W1, W2, W3, and W4
  (`restart/skinny/tranches/sk-v12/SPEC.md:358`,
  `restart/skinny/tranches/sk-v12/SPEC.md:360`,
  `restart/skinny/tranches/sk-v12/SPEC.md:418`,
  `restart/skinny/tranches/sk-v12/SPEC.md:421`,
  `restart/skinny/tranches/sk-v12/SPEC.md:481`,
  `restart/skinny/tranches/sk-v12/SPEC.md:483`,
  `restart/skinny/tranches/sk-v12/SPEC.md:535`,
  `restart/skinny/tranches/sk-v12/SPEC.md:537`,
  `restart/skinny/tranches/sk-v12/SPEC.md:577`,
  `restart/skinny/tranches/sk-v12/SPEC.md:578`).
- Same-wave consumer requirements exist for all waves: W0, W1, W2, W3, and W4
  (`restart/skinny/tranches/sk-v12/SPEC.md:351`,
  `restart/skinny/tranches/sk-v12/SPEC.md:352`,
  `restart/skinny/tranches/sk-v12/SPEC.md:410`,
  `restart/skinny/tranches/sk-v12/SPEC.md:411`,
  `restart/skinny/tranches/sk-v12/SPEC.md:473`,
  `restart/skinny/tranches/sk-v12/SPEC.md:474`,
  `restart/skinny/tranches/sk-v12/SPEC.md:526`,
  `restart/skinny/tranches/sk-v12/SPEC.md:527`,
  `restart/skinny/tranches/sk-v12/SPEC.md:571`).

## Required Fold

Revise V2 so the promoted packet has one coherent cost contract:

1. Risk appears in `p3f-spec-draft.md`, `SPEC.md`, and `DISPATCH-PROMPT.md`.
2. Redress is capped at 75 minutes for every wave; W0/W4 may carry a separate
   90-minute wall cap only outside redress.
3. W1 redress attempts one selected grammar target, or W1a/W1b split waves are
   explicitly added with their own budgets.
4. W2 plan requirements include scalar/checkasm/microbench cost slices, not only
   correctness gates.

With those folds, CH4 should be able to ACCEPT unless another lens finds a
blocking interaction.
