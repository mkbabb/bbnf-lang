# SK-V15 S-P3 V2 CH4 Cost Challenge

Verdict: REVISE.

Scope: cost, cap, and wave-load review of the committed S-P3 V2 packet at
HEAD `39e186ee3`. Inputs read: P3-A through P3-F, `SPEC.md`,
`DISPATCH-PROMPT.md`, `SYNTHESIS.md`,
`HARDENING-S-P3-V1-CONSOLIDATED.md`, and V1 redeploy
`COST-CONSUMER-FOLD-NOTES.md`.

## Findings

| id | disposition | evidence | cost impact | required follow-up |
|---|---|---|---|---|
| CH4-V2-01 | ACCEPT | The packet reindexes to exactly `W0 -> ... -> W11` and declares the two V2 splits load-bearing (`p3b-wave-sequencing.md:17`-`:26`). SPEC carries W0-W11 with per-wave risk, manual LOC, generated status, docs LOC, entry, and exit gates (`SPEC.md:161`-`:174`). | The old W0-W9 overload is gone and the 12-wave ceiling is respected. | Preserve W0-W11; do not add W12 without removing/folding an existing wave through the gate. |
| CH4-V2-02 | ACCEPT | The packet carries the hard caps: research <=20m, plan <=15m, redress <=30m (`p3b-wave-sequencing.md:28`-`:37`; `SPEC.md:150`-`:159`; `DISPATCH-PROMPT.md:32`-`:39`). | Phase timing is explicit enough for per-wave orchestration. | None beyond the REVISE items below. |
| CH4-V2-03 | ACCEPT | Generated output is separated from manual LOC in P3-B (`p3b-wave-sequencing.md:45`-`:49`) and SPEC (`SPEC.md:138`-`:139`, `:161`-`:174`), with generated-output expectations in the generality table (`SPEC.md:193`-`:200`). | Large generated diffs do not silently consume manual redress budget if they are generator-attributed and checked. | Preserve non-writing regen/check or same-wave regenerate proof. |
| CH4-V2-04 | ACCEPT | W5 is limited to typed CSS provider construction and W6 owns same-workload retime plus old-proof retirement (`p3b-wave-sequencing.md:56`-`:57`; `p3c-falsifiability-gates.md:179`-`:212`; `SPEC.md:297`-`:329`). | The former CSS provider + benchmark + proof-retirement overload is now cap-plausible. | Keep W5 from mutating RESULTS floors or retiring old proof unless W6-grade proof lands in-wave. |
| CH4-V2-05 | ACCEPT | W7 is Decision spine only; W8 owns harness/EagerTape/OffsetTape; W9 owns EventTape/SinkOnly/CollapsedStage plus all-five gate (`p3b-wave-sequencing.md:58`-`:60`; `p3c-falsifiability-gates.md:214`-`:273`; `DISPATCH-PROMPT.md:149`-`:200`). | The former Decision + five-lowerer overload is split into cap-plausible slices with same-wave consumers. | Keep W8/W9 row movement blocked unless generated fixture/gate evidence lands in the owning wave. |
| CH4-V2-06 | REVISE | V1 redeploy notes required a hard-cap rule: if estimates exceed LOC/generated status/30-minute redress cap, split before redress or record intrinsic block (`COST-CONSUMER-FOLD-NOTES.md:131`-`:139`). Current SPEC says "If a wave cannot fit the cap, split before redress" (`SPEC.md:158`-`:159`), while P3-B spends all 12 top-level waves and forbids adding another without removing/folding one (`p3b-wave-sequencing.md:17`-`:20`). | Overflow handling is ambiguous at the ceiling. A wave that proves too large can be misrouted into an illegal W12 or silent cap overrun instead of an intrinsic block / gate-routed wave-graph amendment. | Add the exact ceiling-aware rule to P3-B, SPEC, and DISPATCH: because W0-W11 consumes the 12-wave ceiling, any plan estimate beyond the LOC budget, generated-output status, or 30-minute redress cap must record row-level intrinsic block or route a wave-graph amendment through the gate before redress; it may not spawn W12 or use CHALLENGE time as implementation overflow. |
| CH4-V2-07 | REVISE | DISPATCH pre-dispatch checks verify prior wave state, owner paths, delete dependencies, pre-blocks, and aarch64 (`DISPATCH-PROMPT.md:41`-`:56`), but do not require the plan to cite the SPEC/P3-B manual LOC budget, docs LOC budget, generated-output status, or split trigger. A targeted grep found those budget terms only in P3-B/SPEC, not DISPATCH. | A wave agent following DISPATCH can enter redress without an executable cost estimate, even though CH4 acceptance depends on per-wave budget discipline. | Add a DISPATCH pre-redress check: each plan must quote the current wave's risk, manual source/test LOC budget, generated-output status, docs/ledger LOC budget, phase cap, and split/intrinsic-block trigger from SPEC/P3-B. Redress is rejected if the estimate is absent or over budget. |

## Required Follow-Up

1. Patch `DISPATCH-PROMPT.md` Section 2 to require explicit per-wave cost
   budget citation before plan acceptance and before redress.
2. Patch `SPEC.md` Section 2 and `p3b-wave-sequencing.md` Section 1/2 to
   resolve the ceiling conflict: no W12; overflow becomes intrinsic block,
   revert/REDRESS, or gate-routed wave-graph amendment before redress.
3. Re-run CH4 after the follow-up. If the dispatch-visible budget check and
   ceiling overflow rule are present, CH4 should be able to ACCEPT.

## Verification

Commands run:

```sh
git status --short && git rev-parse --short=9 HEAD
nl -ba restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md
nl -ba restart/skinny/tranches/sk-v15/SPEC.md
nl -ba restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
nl -ba restart/skinny/tranches/sk-v15/SYNTHESIS.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/hardening/V1/redeploy/COST-CONSUMER-FOLD-NOTES.md
rg -n "Split trigger|LOC budget|generated-output status|30-minute redress cap|Manual LOC|Docs LOC|Generated LOC|cannot fit|0\\.9x cap" restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md
rg -n "W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6 -> W7 -> W8 -> W9 -> W10 -> W11|No additional top-level wave|If a wave cannot fit|split before redress|intrinsic block|record intrinsic block" restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md restart/skinny/tranches/sk-v15/research/p3/hardening/V1/redeploy/COST-CONSUMER-FOLD-NOTES.md
```

Recommended follow-up verification after the fold:

```sh
rg -n "LOC budget|generated-output status|split trigger|30-minute redress cap|record intrinsic block|no W12|CHALLENGE time is review time" restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md
rg -n "W12|thirteenth|implementation overflow" restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md
```
