# SK-V15 S-P3 V4 CH7 OVERFIT-PRUNE / GATE-EXCLUSION

Pass: S-P3 Synthesis-Plan.
Cycle: V4.
Lens: CH7 overfit-prune / gate-exclusion.
Input commit: `21ae60663`.

Verdict: ACCEPT.

## Findings

| id | verdict | evidence | disposition |
|---|---|---|---|
| CH7-V4-01 hidden broadcast admission | ACCEPT | P3-C makes the SK-V14 W8R CSS tuple diagnostic-only and requires `broadcast_group_id` / diagnostic origin for current W8R rows (`p3c-falsifiability-gates.md:27-35`, `:137-161`). P3-D rejects duplicate admitting rows, hidden one-to-N measurement signatures, and self-exempting reports (`p3d-telemetry-schema.md:46-55`, `:65-77`, `:90-94`). SPEC and DISPATCH carry no-W8R-live-admit gates (`SPEC.md:97-121`, `:268-279`; `DISPATCH-PROMPT.md:138-139`, `:344-345`). | Preserve W0/W1 anti-broadcast gate and W6 typed-retime floor rule. |
| CH7-V4-02 gate self-exemption | ACCEPT | P3-C rejects silent allowlists, self-exempting grep/checkasm rules, and scan reports not consumed by the gate (`p3c-falsifiability-gates.md:89-106`, `:117-124`). P3-D rejects `gate_exclusion_report=self-exempting:*` (`p3d-telemetry-schema.md:54`, `:65`, `:94`, `:110`). SPEC and DISPATCH require included roots, excluded roots, reason, owner, self-scan status, primitive status, gate consumer, affected rows, and disposition (`SPEC.md:233-235`; `DISPATCH-PROMPT.md:89-91`, `:148-150`). | No V4 CH7 revision. |
| CH7-V4-03 EventTape and sidecar relapse | ACCEPT | SPEC binds the full CH5/CH7 forbidden vocabulary, including parser-owned structural projection, retained cursor/list, aux density/projection table, sidecar event vector, second tape, public `UnionTape`, retained streams, and new/sixth `BackendShape` (`SPEC.md:144-153`, `:236-244`, `:476-477`). P3-C/P3-E reject retained structural/cursor/class streams, density tables, second tapes, public `UnionTape`, and EventTape-as-sidecar (`p3c-falsifiability-gates.md:354-358`; `p3e-preblocked-ledger.md:45-55`, `:218-225`). | EventTape remains one existing BackendShape lowerer only. |
| CH7-V4-04 FNV closed-enum migration | ACCEPT | W10 is quarantine-only: P3-C blocks FNV as runtime selector, production arbiter, or correctness proof and requires production scan/adversarial fixtures (`p3c-falsifiability-gates.md:310-326`). SPEC mirrors the quarantine and blocks production FNV arbiter/hash-correctness proof (`SPEC.md:430-445`, `:484`); DISPATCH requires production scan plus adversarial fixtures (`DISPATCH-PROMPT.md:282-298`). | Bench-only until a future Alpha/G-Omega contract reopens it. |
| CH7-V4-05 stale pre-block gaps | ACCEPT | P3-C, P3-E, SPEC, and DISPATCH all carry the normalized pre-block list: `28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, and FNV closed-enum production migration` (`p3c-falsifiability-gates.md:350-358`, `p3e-preblocked-ledger.md:35`, `SPEC.md:469-484`, `DISPATCH-PROMPT.md:333-335`). | No orphan pre-block found. |
| CH7-V4-06 stale topology and overfit route sweep | ACCEPT | Active-surface grep for `Cycle: V1|S-P3 V1|W0-W9|W1-W9|W0 through W9|P3-B does not exist|PRUNE-WAVE|REBUILD-WAVE|209\\.\\.213|96/97/98|930\\.281` over P3-A..P3-F, SPEC, and DISPATCH returned no matches. P3-D/P3-E/SPEC/DISPATCH reject x86/AVX-512 admission anchors, PMULL/CSSC production promotion from ISA/checkasm alone, old numeric/digit framing without fresh P1, and W8R positive proof (`p3d-telemetry-schema.md:52`, `:70`, `:107-111`; `p3e-preblocked-ledger.md:290-297`; `SPEC.md:136`, `:480`; `DISPATCH-PROMPT.md:344-345`). | No V4 CH7 revision. |

## Evidence Commands

```sh
git rev-parse --short HEAD

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

Observed: HEAD `21ae60663`; stale-token grep returned no active-surface matches.
