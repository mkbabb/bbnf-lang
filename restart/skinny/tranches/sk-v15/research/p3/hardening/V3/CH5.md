# SK-V15 S-P3 V3 CH5 HIDDEN COUPLING

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V3. Lens: CH5.
Date: 2026-05-28.
HEAD: `efe1e4b01`.
Scope: audit the active S-P3 packet for hidden broadcast admission,
Track 1 / Track 2 coupling, EventTape sidecar relapse, FNV production
migration, generic Decision/lowerer coupling, and self-exempting gate reports.

## Verdict

ACCEPT.

The V2 CH5 blocker is folded. The final dispatch surfaces now carry the full
hidden-coupling vocabulary as load-bearing rejection language, and each
reviewed hidden-coupling axis has a same-wave gate or dependency consumer. No
CH5 REVISE remains.

## Evidence

| id | status | axis | evidence | disposition |
|---|---|---|---|---|
| CH5-V3-01 | ACCEPT | Full forbidden vocabulary is load-bearing in final dispatch surfaces. | `SPEC.md` makes the complete CH5 vocabulary a non-negotiable: parser-owned structural projection, retained cursor/list, aux density/projection table, sidecar event vector, parallel source pass, second tape, public `UnionTape`, retained class/structural/cursor stream, Track 1 == Track 2 sidecar, new substrate API, new or sixth `BackendShape`, alternate document projection, production FNV arbiter, and production hash correctness proof (`restart/skinny/tranches/sk-v15/SPEC.md:147-153`). The same vocabulary is repeated in the global Lock 14 / generality gate (`SPEC.md:240-244`), the pre-block table (`SPEC.md:476-484`), and the final dispatch same-wave mandate (`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:325-331`). | V2 CH5 blocker closed. |
| CH5-V3-02 | ACCEPT | Broadcast-admission detection and Track 1 / comparator independence are explicit. | P3-D requires `measurement_row_id`, `measurement_origin`, `css_comparator_workload`, and `broadcast_group_id` (`p3d-telemetry-schema.md:21-24`) and rejects duplicate admitting rows plus hidden identical measurement signatures (`p3d-telemetry-schema.md:46-55`, `:66`, `:90-92`). P3-C forbids W8R as a CSS floor and requires fresh same-workload typed output (`p3c-falsifiability-gates.md:27-38`, `:238-247`). SPEC binds the same rows through `DEP-W1-CSS-BROADCAST` and W6 typed retime (`SPEC.md:194`, `:361-376`). DISPATCH requires no W8R floors and a fresh typed `cssparser` comparator command (`DISPATCH-PROMPT.md:204-212`). | Hidden one-to-N measurement and wrong-plane CSS comparison are blocked. |
| CH5-V3-03 | ACCEPT | Track 1 / Track 2 coupling is pre-blocked. | P3-E globally rejects Track 1 == Track 2 collapse (`p3e-preblocked-ledger.md:20-24`), requires W0 source provenance and Track 2 independence (`p3e-preblocked-ledger.md:67-68`), and forbids shared private parser/source paths being labeled Track 1 / Track 2 (`p3e-preblocked-ledger.md:53`). SPEC and DISPATCH carry Track 1 == Track 2 sidecar in the CH5 forbidden vocabulary (`SPEC.md:150-153`; `DISPATCH-PROMPT.md:325-331`). | Strict-product and comparator independence remain protected. |
| CH5-V3-04 | ACCEPT | EventTape anti-sidecar discipline preserves the five-shape canon. | P3-C limits EventTape to an existing BackendShape lowerer and rejects sidecar vector, sixth shape, retained parser-owned stream, public substrate API, or alternate document projection (`p3c-falsifiability-gates.md:289-308`). P3-E repeats the same W9 pre-block (`p3e-preblocked-ledger.md:213-225`). SPEC wires `DEP-W9-LOWERERS-B`, all-five gate proof, and the W9 EventTape ban (`SPEC.md:202`, `:235-237`, `:412-428`). DISPATCH requires the same W9 plan/redress conditions (`DISPATCH-PROMPT.md:257-280`). | No parallel EventTape substrate path is admitted. |
| CH5-V3-05 | ACCEPT | FNV remains bench-only and production migration is blocked. | P3-C requires W10 quarantine, a production FNV scan, and adversarial semantic fixtures (`p3c-falsifiability-gates.md:310-326`). P3-E forbids FNV-keyed closed-enum arbiters or Track 1 sidecars in production (`p3e-preblocked-ledger.md:227-243`). SPEC binds `DEP-W10-FNV-QUARANTINE`, production scan, adversarial fixtures, and no production arbiter/correctness proof (`SPEC.md:203`, `:430-445`, `:484`). DISPATCH requires the strict-product gate, production scan, and adversarial fixtures (`DISPATCH-PROMPT.md:282-299`). | Production FNV arbiter/hash-proof coupling is blocked. |
| CH5-V3-06 | ACCEPT | Generic Decision/lowerer coupling has executable consumers and non-JSON receiver gates. | P3-C requires surface-specific non-JSON receivers for lowerers, `backend_egraph.rs`, `decision_csp.rs`, CostFacts, `xtask`, and gate/report code (`p3c-falsifiability-gates.md:109-124`) and W7-W9 same-wave consumers (`p3c-falsifiability-gates.md:249-308`). SPEC binds dependency rows for Decision and lowerers (`SPEC.md:200-202`), receiver matrix rows (`SPEC.md:206-218`), and W7-W9 executable gates (`SPEC.md:378-428`). DISPATCH requires concrete W7-W9 consumers and scaffold-failing lowerer tests (`DISPATCH-PROMPT.md:218-280`). | Generic decisions and lowerers cannot close as label scaffolds or grammar-named facts. |
| CH5-V3-07 | ACCEPT | Self-exempting gate reports are rejected and consumed. | P3-C defines the Lock 14 / Lock 16 exclusion schema and rejects silent allowlists/self-exempting rules (`p3c-falsifiability-gates.md:89-107`). P3-D rejects `gate_exclusion_report=self-exempting:*` and any exclusion of validators, scan roots, checkasm targets, generated roots, or files under test unless surfaced as non-admission (`p3d-telemetry-schema.md:54-65`, `:94`). SPEC requires reported exclusions, self-scan status, gate consumers, and gate/report code receivers (`SPEC.md:64-65`, `:217`, `:233-239`). DISPATCH makes the schema a pre-dispatch requirement and W2 close condition (`DISPATCH-PROMPT.md:64-66`, `:92-102`, `:143-155`). | No gate can prove cleanliness by omitting its own blind spots. |

## Required Edits

None. CH5 accepts the active V3 packet.

## Verification

Commands run:

```sh
git rev-parse --short HEAD
git status --short
wc -l restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md restart/skinny/tranches/sk-v15/research/p3/hardening/V2/CH5.md
nl -ba restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md
nl -ba restart/skinny/tranches/sk-v15/SPEC.md
nl -ba restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/hardening/V2/CH5.md
rg -n "broadcast_group_id|measurement_row_id|Hidden one-to-N|hidden one-to-N|W8R|2319\\.041|2362\\.037|929\\.281|same-workload|cssparser" restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n 'parser-owned structural projection|retained cursor/list|aux density/projection table|sidecar event vector|parallel source pass|second tape|public `UnionTape`|retained class/structural/cursor stream|Track 1 == Track 2|new substrate API|new/sixth|new or sixth|alternate document projection|production FNV arbiter|production hash correctness proof|sixth `BackendShape`' restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "self-exempting|exclusion report|gate_exclusion_report|included roots|excluded roots|self-scan status|primitive status|gate consumer|affected rows|disposition|consume their own exclusion" restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n 'EventTape|sidecar|sixth shape|public substrate API|alternate document projection|retained parser|all-five|BackendShape' restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n 'FNV|fnv|closed-enum|production arbiter|correctness proof|production migration|bench-only|runtime selector' restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n 'Decision Engine|e-graph|CSP|lowerer|Lowerer|EagerTape|OffsetTape|SinkOnly|CollapsedStage|grammar-neutral|json_\*|css_\*|label-string|scaffold' restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
```

Result: `HEAD` was `efe1e4b01`; unrelated dirty implementation files were
present and untouched. The corrected CH5 vocabulary search found the full
forbidden set in the final `SPEC.md` and `DISPATCH-PROMPT.md` surfaces. The
broadcast, self-exemption, EventTape, FNV, and Decision/lowerer searches all
resolved to explicit gates, dependency rows, or same-wave consumers.
