# SK-V15 S-P3 V3 CH4 Cost Challenge

Verdict: ACCEPT.

Scope: CH4 cost review of the active S-P3 packet at HEAD `efe1e4b01`.
Inputs read: `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`,
active P3-B/P3-C/P3-F, `SPEC.md`, `DISPATCH-PROMPT.md`, V2 CH4, and
P3-A for the shortlist-count invariant.

## Evidence Table

| check | verdict | evidence | cost disposition |
|---|---|---|---|
| Shortlist <=8 | ACCEPT | P3-A scopes the shortlist to `<=8` at line 5 and carries exactly candidates 1-8 in the deliverable table at lines 20-29. | The S-P3 CH4 shortlist ceiling holds. |
| Wave count <=12 | ACCEPT | P3-B fixes the order as `W0 -> ... -> W11` at line 17, states it is exactly 12 waves at lines 19-20, and forbids an added top-level wave without removing/folding an existing wave. | The skinny-bracket wave ceiling holds. |
| No W12 / no CHALLENGE overflow ambiguity | ACCEPT | P3-B lines 28-33, SPEC lines 165-170, and DISPATCH lines 60-63 all say over-budget work cannot spawn W12 or use CHALLENGE time as implementation overflow; the allowed outcomes are row-level intrinsic block, revert/REDRESS, or gate-routed wave-graph amendment before redress. | The V2 CH4 overflow ambiguity is closed. |
| Phase caps and phase breakdown | ACCEPT | P3-B lines 35-44, SPEC lines 157-170, and DISPATCH lines 32-39 carry research <=20m, plan <=15m, redress <=30m, 0.9x checkpoint, cap halt, and distinct research/plan/redress roles. | Per-wave triumvirate timing is explicit and dispatch-visible. |
| Per-wave cost columns | ACCEPT | P3-B lines 56-69 and SPEC lines 172-185 enumerate W0-W11 with risk, manual LOC, generated-output status, docs/ledger LOC, entry gate, and exit gate. | Every wave has the CH4 cost metadata needed before dispatch. |
| Generated-output separation | ACCEPT | P3-B lines 52-54 excludes generated output from manual LOC and requires deterministic generator attribution plus non-writing check or same-wave regeneration; SPEC lines 172-185 separately columns manual LOC and generated status. | Large generated diffs cannot silently consume manual redress budget. |
| Dispatch budget quotation | ACCEPT | DISPATCH lines 56-59 require each plan to quote the current wave's SPEC risk, manual source/test LOC budget, generated-output status, docs/ledger LOC budget, phase caps, and split/intrinsic-block trigger, and reject redress if absent or over budget. | The V2 CH4 missing dispatch-visible budget check is closed. |
| W5/W6 split plausibility | ACCEPT | P3-B lines 23-24 split old W5 into W5 CSS typed provider and W6 same-workload retime / old-proof retirement. P3-B lines 63-64 and SPEC lines 179-180 give separate owner scopes, LOC bands, generated status, entry gates, and exits. | The CSS provider plus benchmark/proof-retirement overload is cap-plausibly split. |
| W8/W9 split plausibility | ACCEPT | P3-B lines 25-26 split old W7 lowerer work into W8 EagerTape/OffsetTape and W9 EventTape/SinkOnly/CollapsedStage plus all-five gate. P3-B lines 66-67 and SPEC lines 182-183 carry separate budgets and exits. | The lowerer work is cap-plausibly split into two implementation waves. |
| Same-wave consumer visibility | ACCEPT | P3-B's wave table includes an `Exit / same-wave consumer` column at line 56 and per-wave consumers through W0-W11 at lines 58-69. P3-C further names same-wave consumers for W0-W11 in its gate sections, including CSS typed provider/retime, decision, lowerer, FNV, and PASS-IMPL V2 consumers. | CH4's same-wave-consumer requirement is represented in both sequencing and gates. |

## Disposition

No CH4 REVISE remains. The V2 CH4 required follow-ups are folded:

1. Ceiling overflow is no longer ambiguous: no W12 and no CHALLENGE-time
   implementation overflow.
2. DISPATCH now rejects plans that omit the current wave's cost budget,
   generated-output status, phase caps, and split/intrinsic-block trigger.
3. The W5/W6 and W8/W9 splits are narrow enough to be plausible under the
   30-minute redress cap, provided each plan obeys its cited LOC and
   generated-output budget.

## Exact Edits If REVISE

None. Verdict is ACCEPT.

## Verification

Commands run:

```sh
git status --short
git rev-parse --short=9 HEAD
sed -n '1,260p' restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md
sed -n '1,260p' restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md
sed -n '1,620p' restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md
sed -n '1,320p' restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md
sed -n '1,760p' restart/skinny/tranches/sk-v15/SPEC.md
sed -n '1,360p' restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
sed -n '1,260p' restart/skinny/tranches/sk-v15/research/p3/hardening/V2/CH4.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md | sed -n '1,180p'
nl -ba restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md | sed -n '1,90p'
nl -ba restart/skinny/tranches/sk-v15/SPEC.md | sed -n '145,190p'
nl -ba restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md | sed -n '28,70p'
rg -n "W0-W11|12-wave|W12|CHALLENGE time|manual LOC|Generated|Docs LOC|risk|split|intrinsic-block|redress cap|<=20|<=15|<=30|W5|W6|W8|W9" restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md restart/skinny/tranches/sk-v15/research/p3/hardening/V2/CH4.md
```
