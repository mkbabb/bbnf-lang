# SK-V15 S-P3 V4 CH4 Cost Challenge

Verdict: ACCEPT.

Scope: CH4 cost review of the active S-P3 packet at HEAD `21ae60663`.
Owned file: `restart/skinny/tranches/sk-v15/research/p3/hardening/V4/CH4.md`.

Inputs read: `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`,
active P3-B/P3-C/P3-F, `SPEC.md`, `DISPATCH-PROMPT.md`, V3 CH4, and
P3-A for the shortlist-count invariant.

## Evidence Table

| check | verdict | evidence | cost disposition |
|---|---|---|---|
| CH4 criteria | ACCEPT | `PASS-3-SYNTHESIS-PLAN.md` lines 128-132 require LOC budget, hard cap, phase breakdown, same-wave consumer, wave count <=12, and shortlist <=8. | This review applies the intended CH4 lens. |
| Shortlist <=8 | ACCEPT | P3-A scopes the shortlist to `<=8` at line 5 and carries exactly candidates 1-8 in the deliverable table at lines 20-29. | The shortlist ceiling holds. |
| Wave count <=12 | ACCEPT | P3-B fixes `W0 -> ... -> W11` at line 17, states this is exactly 12 waves at lines 19-20, and SPEC dispatch lock lists only W0-W11 at lines 36-43. | The skinny-bracket wave ceiling holds. |
| No W12 / no CHALLENGE overflow | ACCEPT | P3-B lines 28-33, SPEC lines 165-170, and DISPATCH lines 60-63 forbid W12 and forbid using CHALLENGE time as implementation overflow; accepted outcomes are row-level intrinsic block, revert/REDRESS, or gate-routed wave-graph amendment before redress. | The V3 cost guard remains load-bearing in V4. |
| Phase caps | ACCEPT | P3-B lines 35-44 and SPEC lines 157-170 carry research <=20m, plan <=15m, redress <=30m, 0.9x checkpoint, and halt-at-cap. DISPATCH lines 32-39 bind the same split to committed research, plan, and redress roles. | Wave time accounting is explicit and dispatch-visible. |
| Per-wave cost metadata | ACCEPT | P3-B lines 56-69 and SPEC lines 172-185 enumerate W0-W11 with risk, manual LOC, generated-output status, docs/ledger LOC, entry gate, and exit gate. | Every wave has the CH4 cost columns needed before dispatch. |
| Generated-output separation | ACCEPT | P3-B lines 52-54 excludes generated output from manual LOC and requires deterministic generator attribution plus a non-writing check or same-wave regeneration. SPEC lines 172-185 split manual LOC from generated status. | Generated diffs cannot silently consume manual redress budget. |
| Dispatch budget quote | ACCEPT | DISPATCH lines 56-59 require each plan to quote the SPEC risk, manual source/test LOC budget, generated-output status, docs/ledger LOC budget, phase caps, and split/intrinsic-block trigger; redress rejects absent or over-budget estimates. | The budget guard reaches the per-wave plan boundary. |
| Same-wave consumers | ACCEPT | P3-B line 56 includes `Exit / same-wave consumer`, and lines 58-69 name consumers for W0-W11. P3-C lines 143-345 further bind same-wave consumers to each wave gate. | Primitive/gate outputs cannot close as producer-only cost spillover. |
| W5/W6 split plausibility | ACCEPT | P3-B lines 23-24 split old W5 into W5 CSS typed provider and W6 same-workload retime / old-proof retirement. P3-B lines 63-64 and SPEC lines 179-180 give separate owner scopes, LOC bands, generated status, entry gates, and exits. DISPATCH lines 186-216 separately require provider output before retime/retirement. | CSS provider work and benchmark/proof-retirement work are cap-plausibly separated. |
| W8/W9 split plausibility | ACCEPT | P3-B lines 25-26 split old lowerer work into W8 EagerTape/OffsetTape and W9 EventTape/SinkOnly/CollapsedStage plus all-five gate. P3-B lines 66-67 and SPEC lines 182-183 give separate budgets and exits. DISPATCH lines 238-280 separates the required consumers. | Backend lowerer work is cap-plausibly split across two waves. |
| Close-wave cost posture | ACCEPT | SPEC lines 447-465 and DISPATCH lines 301-316 make W11 reconciliation consume prior evidence and PASS-IMPL V2 rather than reopen implementation work. | Close does not become a hidden implementation overflow wave. |

## Disposition

No CH4 REVISE remains. The active packet satisfies the CH4 cost lens at
V4: 12 waves exactly, no W12 escape hatch, no CHALLENGE-time overflow, clear
research/plan/redress caps, per-wave cost columns, dispatch-visible budget
quotation, and plausible W5/W6 plus W8/W9 splits.

## Exact Edits If REVISE

None. Verdict is ACCEPT.

## Verification

Commands run:

```sh
git rev-parse --short=9 HEAD
git status --short
wc -l restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH4.md restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md
nl -ba restart/skinny/tranches/sk-v15/SPEC.md
nl -ba restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH4.md
nl -ba restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md | sed -n '1,90p'
rg -n "W0 ->|12-wave|W12|CHALLENGE time|Research|Plan|Redress|Manual LOC|Generated|Docs LOC|split|intrinsic-block|W5|W6|W8|W9|shortlist|candidate" restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md
```
