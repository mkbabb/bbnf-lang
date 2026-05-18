# SK-V8 S-P3 V2 Hardening Fold

Pass: S-P3 Synthesis-Plan.
Cycle: V2 fold.
Date: 2026-05-18.
Scope: disposition of S-P3 challenge V1 objections into the live SK-V8 packet.

## Inputs

- `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md`

## Folded Dispositions

| V1 challenge | V1 verdict | V2 fold |
|---|---|---|
| CH1 correctness | REVISE | SPEC Section 0.5 now carries the W2 candidate typed seed table and constrains W2 selection to that table unless a later accepted S-P3 revision expands it. Future wave research artifacts are referenced through concrete directories plus naming patterns rather than unresolved wildcard links. Local P3 citations were normalized away from stale live-packet line numbers. |
| CH4 cost | REVISE | SPEC, DISPATCH, and HANDOFF now carry per-wave source/edit LOC budgets. The budgets are conjunctive with the 90-minute implementation/redress cap. W3 now requires a pre-redress fit estimate covering touched source/test LOC, generated LOC, gate/report LOC, docs/RESULTS/REDRESS edits, and revert slice. Over-budget W3 plans must split or return REVISE before redress. |
| CH2 generality | ACCEPT | Preserved. No new directive, BIR, `BackendShape`, substrate surface, `UnionTape`, public substrate API, or grammar-specific generic policy was introduced. |
| CH3 regression | ACCEPT | Preserved. The fold changes planning constraints only and does not authorize implementation or row-status movement. |
| CH5 hidden coupling | ACCEPT | Preserved. W3 remains one retained `Tape` by representation replacement, with no parser-owned cursor/facts, sidecar substrate, or telemetry-only production consumer. |
| CH6 anti-paper-close | ACCEPT | Preserved. No implementation wave dispatches from S-P3. G-Alpha remains required, and `G-Alpha closed` authorizes W0 only. |

## V2 Packet State

The V2 packet remains planning-only. It is ready for S-P3 V2 challenge, not
implementation dispatch. The dispatch lock remains:

- no SK-V8 implementation wave before G-Alpha;
- `G-Alpha closed` dispatches W0 only;
- W1-W6 require W0 close, exact wave plan, required challenge acceptance, owner
  paths, same-wave consumer, row gates, revert protocol, LOC/time fit, and
  REDRESS routing.

Self-verdict: ACCEPT.

Confidence: 96%.
