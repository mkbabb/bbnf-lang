# SK-V15 S-P2 V1 Hardening Consolidated

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-28.
Inputs: `restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH1.md` through `CH6.md`.
Verdict: REVISE.

## Disposition Summary

| lens | disposition | blocking findings |
|---|---|---|
| CH1 CORRECTNESS | REVISE | Numeric/digit candidates and `EOB_PAD_CLAMP` lack surviving named S-P1 hot-leaf antecedents; escape/unescape surfaces need tighter P1 bridges; mutable comparator citations and P2-C host evidence/schema need repair. |
| CH2 GENERALITY | REVISE | P2-A uses candidate wording while P2-F only covers P2-B/C/D/E; V2 must explicitly demote or map P2-A rows. |
| CH3 REGRESSION | ACCEPT | Named REDRESS pre-blocks, W11/FNV quarantine, and rejected instruction routes are honoured. |
| CH4 COST | REVISE | Non-REJECT candidates lack per-candidate LOC budget, risk class, wave alignment, and hard cap. |
| CH5 HIDDEN COUPLING | REVISE | `offset_tape_capacity_policy_v2` must remove the second-source-scan escape hatch. |
| CH6 ANTI-PAPER-CLOSE | ACCEPT | Comparator, ISA, scalar-reference, and verdict surfaces are evidence-backed and not self-closing. |

ACCEPT rate: 2/6 = 33.3%. This does not meet §3Z convergence.

Open REJECT surfaces for V2 fold:

- `raw_number_span_classify`, A64 `UDOT` digit helper, and `digit_run_span_accumulate` are rejected as implementation candidates until a current BBNF-side numeric hot leaf exists.
- `EOB_PAD_CLAMP` is demoted to existing support inventory, not an S-P2 candidate.

Open REVISE surfaces for V2 fold:

- Add an explicit P2-A alias/disposition bridge in P2-F.
- Add simdjson strictness-plane wording and pin mutable comparator source heads to observed commit SHAs.
- Commit and cite Apple M5 Max/aarch64 host-feature probe evidence for P2-C.
- Restore P2-C schema headings to the S-P2 prompt wording.
- Add CH4 cost fields for every non-REJECT survivor: scalar reference, parity gate, same-wave consumer, LOC budget, risk class, wave alignment, hard cap, orphan-risk disposition.
- Tighten `offset_tape_capacity_policy_v2` so no second source scan, pre-scan capacity oracle, or sidecar capacity plane is permitted.

## Fold Directive

V2 must patch the P2 packet before redispatching CH1-CH6. The fold is docs-only and does not touch `skinny/` source. No S-P2 candidate advances to S-P3 until V2 hardening returns zero REJECTs, zero orphan REVISEs, and the required two consecutive ≥95% ACCEPT cycles.
