# SK-V13 S-P3 V2 CH3 Regression / REDRESS

Pass: S-P3 Synthesis-Plan.
Cycle: V2 CHALLENGE.
Date: 2026-05-21.
Lens: CH3 REGRESSION / REDRESS.
Output: `restart/skinny/tranches/sk-v13/research/p3/hardening/V2/CH3.md`.

## Verdict

ACCEPT.

The V2 packet folds the V1 CH3 blocker. SPEC and DISPATCH now name P3-A through
P3-E as current authorities, consume P3-E's REDRESS route-state matrix, preserve
admitted-row maintain gates, and block source/RESULTS/REDRESS work until
G-Omega plus S-P3 convergence. No CH3 regression blocker remains.

## Evidence

| Check | Evidence | CH3 reading | Disposition |
|---|---|---|---|
| V1 blocker folded | DISPATCH assigns P3-B to cost/dependency/bracket accounting, P3-C to formulas, P3-D to telemetry/gate-json, and P3-E to REDRESS route-state authority (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:32`-`:34`). SPEC's authority list includes P3-D and P3-E (`restart/skinny/tranches/sk-v13/SPEC.md:20`-`:21`). | The stale "P3-B through P3-E absent" defect is gone. | ACCEPT |
| W1 maintain formula | SPEC requires the admitted declaration-values row to maintain strict equality and `Track1_after >= max(lightningcss_open + 1.0, 0.98 * SK-V13-open Track1)` (`restart/skinny/tranches/sk-v13/SPEC.md:464`-`:465`), matching P3-A's folded maintain gate (`restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md:123`). | The prior CSS admit cannot silently degrade under W1. | ACCEPT |
| Telemetry gate consumption | SPEC requires every emitted field to be consumed by `gate-json`, a CSS companion gate, or rolling SOTA delta in the same wave, and rejects producer-only telemetry (`restart/skinny/tranches/sk-v13/SPEC.md:240`-`:245`). DISPATCH requires required telemetry fields in every wave packet (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:81`-`:84`). | Telemetry is not a sidecar or paper producer. | ACCEPT |
| Per-wave REDRESS matrix | SPEC Section 20 gives a wave-family matrix for Pre-W0/W0, CSS, decision, policy, union, direct, SIMD/ASM, typed, parse-only, and close waves (`restart/skinny/tranches/sk-v13/SPEC.md:975`-`:986`). DISPATCH mirrors the same per-wave REDRESS blocks (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:221`-`:251`). | P3-E is folded as a dispatch-grade matrix, not only a global warning. | ACCEPT |
| No silent demotions | SPEC G7 forbids silent demotion (`restart/skinny/tranches/sk-v13/SPEC.md:73`-`:75`), W1 requires JSON guards no silent demotion (`restart/skinny/tranches/sk-v13/SPEC.md:468`), and W15 requires rolling delta no silent demotion (`restart/skinny/tranches/sk-v13/SPEC.md:947`). DISPATCH requires admit packets to confirm no silent demotion across admitted JSON/CSS rows (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:258`-`:260`). | Admitted rows cannot be downgraded without measured REDRESS. | ACCEPT |
| Same-wave consumer and SIMD zero-orphan rules | SPEC bans primitives, kernels, generated paths, resolvers, union substrate, and telemetry producers without same-wave measured consumers (`restart/skinny/tranches/sk-v13/SPEC.md:297`-`:299`). W9/C3 SIMD must exit with `orphan_count_after = 0`, strict checkasm, scalar-reference status, delete/demote/revert protocol, and same-wave production consumer evidence (`restart/skinny/tranches/sk-v13/SPEC.md:748`-`:751`). DISPATCH applies the same zero-orphan rule to any wave touching `bbnf-simd`, including W9/C3 (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:212`-`:217`). | No later-wave SIMD cleanup dependency remains. | ACCEPT |
| Bracket accounting | SPEC defines the canonical W0-W15 manifest and counts W10.N, W11.N, and W14.N subwaves as real triumvirates against the active skinny bracket (`restart/skinny/tranches/sk-v13/SPEC.md:314`-`:318`). DISPATCH repeats that P3-B labels are aliases and each concrete subwave counts against bracket accounting (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:188`-`:193`). | Wave accounting is no longer split between incompatible manifests. | ACCEPT |
| No support-only waves | SPEC forbids support-only behavior waves (`restart/skinny/tranches/sk-v13/SPEC.md:305`-`:306`). DISPATCH says behavior waves must move a named RESULTS row or record a measured architectural block, and API/e-graph/cost telemetry/oracle/scaffold landings do not close alone (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:197`-`:200`). W5-W7 each name row-maintain or architectural-block exits and same-wave consumers (`restart/skinny/tranches/sk-v13/SPEC.md:589`-`:595`, `:622`-`:632`, `:654`-`:669`). | The decision-engine sequence cannot close as support plumbing. | ACCEPT |
| G-Omega / REDRESS mutation block | SPEC blocks implementation, `skinny/RESULTS.md`, and `skinny/REDRESS.md` edits until G-Omega closes and S-P3 converges or is pinned (`restart/skinny/tranches/sk-v13/SPEC.md:35`-`:42`). DISPATCH repeats the same no-source/no-RESULTS/no-REDRESS block (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:40`-`:47`). | No wave can begin by reopening REDRESS before the gate. | ACCEPT |

## Required Fold Items

None for CH3.

## Verification

- Read current HEAD `81c042e1c0ba203126b1595f5b21c3e83c0ab733`; V2 CH3 reviews the folded S-P3 packet at that head, with the SK-V13 P3 fold at `9f8bbfce5` and later commits Omega-only.
- Verified `restart/skinny/tranches/sk-v13/research/p3/hardening/V2/CH3.md` did not exist before this replacement write.
- No source, `skinny/RESULTS.md`, `skinny/REDRESS.md`, staging, or commit action was performed.
