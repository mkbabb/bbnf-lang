# G-Alpha SK-V11 Presentation

Date: 2026-05-19.
Gate: G-Alpha.
Tranche: SK-V11.
Status: PRESENTED / PASS.

## Authority

- `restart/skinny/tranches/sk-v11/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `restart/skinny/tranches/sk-v11/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v11/research/alpha/alpha-B-competitor-deltas.md`
- `restart/skinny/tranches/sk-v11/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v11/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v11/research/alpha/alpha-F-contract-draft.md`
- `restart/skinny/tranches/sk-v11/research/alpha-hardening/HARDENING-ALPHA-V1-CONSOLIDATED.md`

## Presented Contract

SK-V11 opens from the measured SK-V10 close state:

- 17 `parse_only` rows are diagnostic `S / NO-GO`, not SOTA targets.
- 6 `direct_to_struct` rows are `A / GO` and must be guarded.
- 11 `direct_to_struct` rows are `N-direct / NO-GO` and form the JSON close
  frontier.
- 7 `real_typed_struct` rows are `A / GO` and must be guarded.
- The SK-V9 W3 union/event/class-column/streaming-cursor/sidecar substrate
  family is REDRESS-falsified and pre-blocked.

SK-V11 advances three axes together:

1. Direct plane closure or measured fixpoint for the 11 residual direct rows.
2. One admitted benchmarked non-JSON grammar intervention through a generated
   direct or typed parser.
3. Aarch64 Apple Silicon SIMD/ASM only, behind micro-prove-first, scalar
   reference, differential/checkasm where applicable, feature gate, and
   same-wave consumer.

## Gate Result

G-Alpha is PASS. The next authority is SK-V11 S-P1 Profile with full
orchestration under `restart/prompts/skinny/PASS-1-PROFILE.md`.

No implementation wave is authorized by G-Alpha. `SPEC.md` and
`DISPATCH-PROMPT.md` remain S-P3 outputs after S-P1 and S-P2 converge.
