# SK-V15 S-P3 V3 CH7 OVERFIT-PRUNE / GATE-EXCLUSION

Pass: S-P3 Synthesis-Plan.
Cycle: V3.
Lens: CH7 overfit-prune / gate-exclusion.

Verdict: ACCEPT.

## Findings

| id | verdict | evidence | disposition |
|---|---|---|---|
| CH7-V3-01 hidden broadcast admission | ACCEPT | The active packet treats W8R CSS as a negative fixture only: P3-C rejects W8R live floors and requires `broadcast_group_id` plus diagnostic origin (`p3c-falsifiability-gates.md:27-35`, `:137-161`); P3-D defines `measurement_row_id`, `measurement_origin`, `gate_exclusion_report`, and `broadcast_group_id`, rejects hidden one-to-N signatures, and gives the W8R residue only diagnostic or independently retimed outcomes (`p3d-telemetry-schema.md:46-55`, `:65-77`, `:90-94`); SPEC and DISPATCH repeat no W8R live admit (`SPEC.md:97-121`, `:268-279`, `DISPATCH-PROMPT.md:138-139`, `:344-345`). | Preserve the W0/W1 telemetry gate and W6 typed-retime floor rule. |
| CH7-V3-02 gate self-exemption | ACCEPT | P3-C defines the Lock 14 / Lock 16 exclusion schema and rejects silent allowlists, self-exempting grep/checkasm rules, and producer-only reports (`p3c-falsifiability-gates.md:89-106`, `:117-124`). P3-D rejects `gate_exclusion_report=self-exempting:*` and gate/report self-exclusions (`p3d-telemetry-schema.md:54`, `:65`, `:94`, `:110`). SPEC and DISPATCH require included roots, excluded roots, reason, owner, self-scan status, primitive status, gate consumer, affected rows, and disposition (`SPEC.md:233-235`, `DISPATCH-PROMPT.md:89-91`, `:148-150`). | No CH7 revision. |
| CH7-V3-03 EventTape / retained-sidecar relapse | ACCEPT | SPEC carries the full hidden-coupling vocabulary including parser-owned structural projection, retained cursor/list, aux density/projection table, sidecar event vector, second tape, public `UnionTape`, retained streams, and new/sixth `BackendShape` (`SPEC.md:144-153`, `:236-244`, `:476-477`). P3-C/P3-E specifically reject retained structural/cursor/class streams, density tables, second tapes, public `UnionTape`, and EventTape-as-sidecar (`p3c-falsifiability-gates.md:354-358`, `p3e-preblocked-ledger.md:45-55`, `:218-225`). | EventTape remains an existing BackendShape lowerer only. |
| CH7-V3-04 FNV closed-enum migration | ACCEPT | W10 is quarantine-only: P3-C blocks runtime selector, production arbiter, and correctness proof roles and requires production FNV scan plus adversarial fixtures (`p3c-falsifiability-gates.md:310-326`); SPEC mirrors quarantine and blocks production FNV arbiter/hash-correctness proof (`SPEC.md:430-445`, `:484`); DISPATCH requires production scan and adversarial fixtures (`DISPATCH-PROMPT.md:282-298`). | Preserve as bench-only until a future Alpha/G-Omega contract reopens it. |
| CH7-V3-05 stale pre-block gaps | ACCEPT | The shared pre-block list is normalized across P3-C, P3-E, SPEC, and DISPATCH: `28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, and FNV closed-enum production migration` (`p3c-falsifiability-gates.md:350-358`, `p3e-preblocked-ledger.md:35`, `SPEC.md:469-484`, `DISPATCH-PROMPT.md:333-335`). | No orphan pre-block found. |
| CH7-V3-06 stale topology and overfit terms | ACCEPT | Active-surface grep for `Cycle: V1|S-P3 V1|W0-W9|W1-W9|W0 through W9|P3-B does not exist|PRUNE-WAVE|REBUILD-WAVE|209\\.\\.213|96/97/98|930\\.281` over P3-A..P3-F, SPEC, and DISPATCH returned no matches. P3-D/P3-E/SPEC/DISPATCH block x86/AVX-512 admission anchors, PMULL/CSSC production promotion from ISA/checkasm alone, numeric/digit old framing without fresh P1, and W8R positive proof (`p3d-telemetry-schema.md:52`, `:70`, `:107-111`; `p3e-preblocked-ledger.md:290-297`; `SPEC.md:136`, `:480`; `DISPATCH-PROMPT.md:344-345`). | No V3 revision. |

## Evidence Commands

```sh
rg -n "Cycle: V1|S-P3 V1|W0-W9|W1-W9|W0 through W9|P3-B does not exist|PRUNE-WAVE|REBUILD-WAVE|209\\.\\.213|96/97/98|930\\.281" \
  restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md \
  restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md \
  restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md \
  restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md \
  restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md \
  restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md \
  restart/skinny/tranches/sk-v15/SPEC.md \
  restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
```

Result: no active-surface matches.
