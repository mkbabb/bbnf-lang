# SK-V13 S-P3 V2 CH1 Correctness Challenge

Pass: S-P3 Synthesis-Plan.
Cycle: V2.
Date: 2026-05-21.
Lens: CH1 correctness.
Commit under review: `9f8bbfce531fc294edccd27f13e57d3da05660cb`.
Output: `restart/skinny/tranches/sk-v13/research/p3/hardening/V2/CH1.md`.

## Verdict

ACCEPT.

The V1 CH1 fold landed for the requested correctness surfaces. P3-B through
P3-E are no longer treated as absent, P3A-0 is classified as W0 governance
substrate, P3A-1 through P3A-7 have a trace matrix, stale fixed maintain gates
were replaced with W0-derived formula language, and SPEC/DISPATCH use one
coherent W0-W15 dispatch map with P3-B W0-W11 labels demoted to aliases.

## Findings

No CH1 correctness defects remain for the V1 CH1 fold items.

| Check | Evidence | Disposition |
|---|---|---|
| CH1 standard | S-P3 CH1 requires S-P2/S-P1 traceability, measurable gates, SK-V13-open exit comparisons, and strict comparator deltas (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:110`, `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:114`). Orchestrator CH1 requires cited claims, measurable gates, and strictness-plane deltas (`restart/prompts/ORCHESTRATOR.md:83`). | ACCEPT |
| P3-B/C/D/E not absent | P3-F states P3-A through P3-E are present and binding, with P3-B owning cost/dependency/bracket accounting, P3-C formulas, P3-D telemetry/gate-json, and P3-E REDRESS route-state (`restart/skinny/tranches/sk-v13/research/p3/p3f-spec-draft.md:27`, `restart/skinny/tranches/sk-v13/research/p3/p3f-spec-draft.md:35`). SPEC lists P3-A through P3-F plus V1 hardening in authority (`restart/skinny/tranches/sk-v13/SPEC.md:5`, `restart/skinny/tranches/sk-v13/SPEC.md:23`). DISPATCH repeats that P3-A through P3-E are current required inputs and forbids treating them as absent (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:31`, `restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:34`). | ACCEPT |
| P3A-0 governance substrate | P3-A's V2 fold note says P3A-0 is `W0-GOVERNANCE-SUBSTRATE`, not an S-P2 intervention candidate (`restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md:10`, `restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md:15`). The shortlist row repeats that it is a gate family, not a parser primitive or S-P2 intervention survivor (`restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md:72`). P3-F mirrors the reclassification (`restart/skinny/tranches/sk-v13/research/p3/p3f-spec-draft.md:27`, `restart/skinny/tranches/sk-v13/research/p3/p3f-spec-draft.md:29`). | ACCEPT |
| P3A-1..7 trace matrix | P3-A adds `P3A-1 Through P3A-7 Trace Matrix`, states P3A-0 is W0 governance substrate, and maps P3A-1 through P3A-7 to S-P2 source rows, S-P1 antecedents, and fresh-evidence limits (`restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md:83`, `restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md:96`). | ACCEPT |
| W0-derived formulas | P3-C defines `before` as the W0-captured `SK-V13-open` row and makes copying pre-W0 numbers after W0 a gate failure (`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:74`, `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:77`). SPEC requires W0 to regenerate a gate-consumed threshold table from live same-run benches (`restart/skinny/tranches/sk-v13/SPEC.md:258`, `restart/skinny/tranches/sk-v13/SPEC.md:260`) and expresses the declaration-values maintain gate as `Track1_after >= max(lightningcss_open + 1.0, 0.98 * SK-V13-open Track1)` plus strict equality (`restart/skinny/tranches/sk-v13/SPEC.md:464`, `restart/skinny/tranches/sk-v13/SPEC.md:465`). | ACCEPT |
| SPEC/DISPATCH wave IDs | P3-B states the canonical dispatch identifiers are the folded SPEC/DISPATCH W0-W15 names and that P3-B W0-W11 is no longer dispatch authority after V2 (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:10`, `restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:18`). SPEC declares the W0-W15 manifest authoritative, defines W10.N/W11.N/W14.N as planning subwave series until a real triumvirate is declared, and accounts for bracket overflow (`restart/skinny/tranches/sk-v13/SPEC.md:314`, `restart/skinny/tranches/sk-v13/SPEC.md:340`). DISPATCH mirrors the same W0-W15 wave map and the same canonical wave-accounting rule (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:163`, `restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:194`). | ACCEPT |

## Required Fold Items

None for CH1. The V1 CH1 required items are folded. Later lenses may still
disposition cost, regression, generality, hidden-coupling, or anti-paper-close
surfaces independently, but those are outside this CH1 verdict.

## Evidence

- V1 CH1 required stale absence removal, P3A-0 reclassification, P3A-1..7 trace
  matrix, SPEC/DISPATCH gate formula fold, P3-D telemetry fold, and P3-E
  pre-block fold (`restart/skinny/tranches/sk-v13/research/p3/hardening/V1/CH1.md:31`,
  `restart/skinny/tranches/sk-v13/research/p3/hardening/V1/CH1.md:45`).
- V1 consolidated hardening made the same CH1 fold items blocking for V2
  (`restart/skinny/tranches/sk-v13/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:38`,
  `restart/skinny/tranches/sk-v13/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:55`).
- P3-D telemetry is folded into SPEC Section 0.4 required fields and gate-json
  rejection rules (`restart/skinny/tranches/sk-v13/SPEC.md:133`,
  `restart/skinny/tranches/sk-v13/SPEC.md:246`), matching P3-D's required field
  and rejection model (`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:120`,
  `restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:180`,
  `restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:260`,
  `restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:284`).
- P3-E route-state vocabulary and wave-family pre-block matrix are folded into
  SPEC Section 20 and mirrored in DISPATCH pre-blocks
  (`restart/skinny/tranches/sk-v13/research/p3/p3e-preblocked-ledger.md:91`,
  `restart/skinny/tranches/sk-v13/research/p3/p3e-preblocked-ledger.md:101`,
  `restart/skinny/tranches/sk-v13/SPEC.md:975`,
  `restart/skinny/tranches/sk-v13/SPEC.md:986`,
  `restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:219`,
  `restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:251`).

## Validation

`git diff --check -- restart/skinny/tranches/sk-v13/research/p3/hardening/V2/CH1.md`
passed with no output.
