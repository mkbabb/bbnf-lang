# SK-V15 S-P3 V4 CH5 HIDDEN COUPLING

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V4. Lens: CH5.
Date: 2026-05-28.
HEAD: `21ae60663`.
Scope: audit the active S-P3 packet for hidden broadcast admission,
Track 1 / Track 2 coupling, full forbidden vocabulary, EventTape
anti-sidecar discipline, FNV quarantine, generic Decision/lowerer
coupling, and self-exempting gate reports.

## Verdict

ACCEPT.

The active S-P3 packet keeps the V3 CH5 repairs load-bearing. CH5 finds no
hidden-coupling orphan in P3-C, P3-D, P3-E, SPEC, or DISPATCH.

## Evidence

| id | status | axis | evidence | disposition |
|---|---|---|---|---|
| CH5-V4-01 | ACCEPT | Broadcast admission and hidden one-to-N measurement stamps are rejected. | P3-D requires `measurement_row_id`, `measurement_origin`, `css_comparator_workload`, and `broadcast_group_id`, rejects duplicate admitting measurement ids, rejects hidden identical measurement signatures, and permits the old CSS W8R shape only as aggregate diagnostic or fresh independent typed rows (`restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md:46-55`, `:65-77`, `:90-92`). P3-C forbids W8R as a floor and requires fresh W6 same-run `cssparser` typed comparison (`restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:27-38`, `:238-247`). SPEC binds this through `DEP-W1-CSS-BROADCAST` and W6 (`restart/skinny/tranches/sk-v15/SPEC.md:194`, `:361-376`); DISPATCH requires fresh typed comparator proof and no W8R floors (`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:204-212`). | No broadcast admit can close. |
| CH5-V4-02 | ACCEPT | Track 1 / Track 2 independence remains protected. | P3-D preserves Track 2 independence as inherited SK-V8 telemetry (`p3d-telemetry-schema.md:36-37`). P3-E globally blocks Track 1 == Track 2 collapse, requires W0 source provenance and Track 2 independence, and rejects measuring the same private parser/source path twice (`restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md:20-24`, `:53`, `:67-68`, `:236`). SPEC and DISPATCH carry `Track 1 == Track 2 sidecar` in the load-bearing CH5 vocabulary (`SPEC.md:148-153`, `:240-244`; `DISPATCH-PROMPT.md:325-331`). | Comparator independence is not self-attested. |
| CH5-V4-03 | ACCEPT | Full forbidden vocabulary is present in final dispatch surfaces. | SPEC makes parser-owned structural projection, retained cursor/list, aux density/projection table, sidecar event vector, parallel source pass, second tape, public `UnionTape`, retained class/structural/cursor stream, Track 1 == Track 2 sidecar, new substrate API, new or sixth `BackendShape`, alternate document projection, production FNV arbiter, and production hash correctness proof non-negotiable (`SPEC.md:147-153`) and repeats the vocabulary in the Lock 14/general gate and pre-block table (`SPEC.md:235-244`, `:476-484`). DISPATCH repeats it in the same-wave mandate (`DISPATCH-PROMPT.md:325-331`). | The V2 hidden-coupling vocabulary remains load-bearing. |
| CH5-V4-04 | ACCEPT | EventTape cannot reopen a parallel substrate or sixth shape. | P3-C constrains EventTape to one existing BackendShape lowerer, requires all-five gate proof, and rejects sidecar vector, sixth shape, retained parser-owned stream, public substrate API, and alternate document projection (`p3c-falsifiability-gates.md:289-308`). P3-E repeats the W9 pre-block (`p3e-preblocked-ledger.md:213-225`). SPEC wires `DEP-W9-LOWERERS-B`, exact five-shape gate proof, and W9 anti-sidecar language (`SPEC.md:202`, `:235-237`, `:412-428`). DISPATCH requires the same W9 plan/redress conditions and consumers (`DISPATCH-PROMPT.md:257-280`). | EventTape remains one of five lowerers, not a sidecar substrate. |
| CH5-V4-05 | ACCEPT | FNV closed-enum products remain quarantined. | P3-C requires W10 quarantine, strict-product gate consumption, production `fnv|FNV` scan, and adversarial semantic fixtures (`p3c-falsifiability-gates.md:310-326`). P3-E blocks FNV-keyed closed-enum arbiters, Track 1 shared sidecars, digest relabels, and production migration (`p3e-preblocked-ledger.md:227-243`, `:297`). SPEC binds `DEP-W10-FNV-QUARANTINE` and production scan/adversarial fixtures (`SPEC.md:203`, `:430-445`, `:484`). DISPATCH requires strict-product metadata, production scan, and adversarial fixtures (`DISPATCH-PROMPT.md:282-299`). | Production FNV arbiter/hash-proof coupling is blocked. |
| CH5-V4-06 | ACCEPT | Generic Decision and lowerer coupling has receivers and executable consumers. | P3-C requires surface-specific non-JSON receivers for lowerers, `backend_egraph.rs`, `decision_csp.rs`, CostFacts, `xtask`, and gate/report code (`p3c-falsifiability-gates.md:109-124`) and same-wave consumers for W7-W9 (`p3c-falsifiability-gates.md:249-308`). SPEC binds `DEP-W7-DECISION-SPINE`, `DEP-W8-LOWERERS-A`, `DEP-W9-LOWERERS-B`, the receiver matrix, and executable gate posture (`SPEC.md:200-218`, `:378-428`). DISPATCH names concrete W7-W9 consumer commands and scaffold-failing lowerer tests (`DISPATCH-PROMPT.md:218-280`). | Decision and lowerer claims cannot close as grammar-named facts or label scaffolds. |
| CH5-V4-07 | ACCEPT | Self-exempting gate reports reject and are consumed. | P3-C defines the Lock 14 / Lock 16 exclusion schema and rejects silent allowlists, self-exempting grep/checkasm rules, and unconsumed scan reports (`p3c-falsifiability-gates.md:89-107`). P3-D rejects `gate_exclusion_report=self-exempting:*` and any exclusion of validators, scan roots, checkasm target, generated roots, or files under test unless surfaced as non-admission (`p3d-telemetry-schema.md:54-66`, `:94`, `:110`). SPEC and DISPATCH require included roots, excluded roots, reason, owner, self-scan status, primitive status, gate consumer, affected rows, and disposition (`SPEC.md:217`, `:233-239`; `DISPATCH-PROMPT.md:64-66`, `:92-102`, `:143-155`). | Gates cannot prove cleanliness by omitting their blind spots. |

## Required Edits

None. CH5 accepts the active V4 packet.

## Verification

Commands run:

```sh
git rev-parse --short=9 HEAD
git status --short -- restart/skinny/tranches/sk-v15/research/p3/hardening/V4/CH5.md
sed -n '1,260p' restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md
sed -n '1,220p' restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md
sed -n '1,220p' restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md
sed -n '1,560p' restart/skinny/tranches/sk-v15/SPEC.md
sed -n '1,360p' restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
sed -n '1,220p' restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH5.md
```

Result: HEAD was `21ae60663`; V4 `CH5.md` did not pre-exist. The reviewed
surfaces bind broadcast detection, Track 1 / Track 2 independence, the full
CH5 forbidden vocabulary, EventTape anti-sidecar discipline, FNV quarantine,
generic Decision/lowerer receivers, and self-exempting gate rejection to
specific gates, dependency rows, or same-wave consumers.
