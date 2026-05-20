# Handoff SK-V12

Date: 2026-05-20.

Status: Pass Alpha SK-V11 -> SK-V12 converged under V2 hardening and G-Alpha
PASS. SK-V11 closed under REDRESS 120 as a measured fixpoint with unchanged
overall `N-direct / NoGo`. SK-V12 starts from that evidence. This handoff does
not create `SPEC.md` or `DISPATCH-PROMPT.md`; S-P3 owns those files after S-P1
and S-P2 converge.

## 1. Read First

1. `restart/prompts/ORCHESTRATOR.md`
2. `restart/prompts/pass-contracts/PASS-ALPHA.md`
3. `restart/prompts/skinny/PASS-1-PROFILE.md`
4. `restart/prompts/skinny/PASS-2-RESEARCH.md`
5. `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
6. `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
7. `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md`
8. `restart/skinny/tranches/sk-v12/research/alpha-hardening/V2/CONSOLIDATED.md`
9. `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md`
10. `restart/skinny/tranches/sk-v11/research/close/close-redress.md`
11. `restart/skinny/tranches/sk-v11/SYNTHESIS.md`
12. `restart/skinny/tranches/sk-v11/HANDOFF.md`
13. `restart/skinny/tranches/sk-v11/SPEC.md`
14. `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md`
15. `skinny/RESULTS.md`
16. `skinny/REDRESS.md` through REDRESS 120

## 2. Current State

SK-V11 close is the SK-V12 seed authority:

| Family | State | SK-V12 role |
|---|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | diagnostic only |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | guards plus pre-blocked residual fixpoint |
| `real_typed_struct` | 7 `A / GO` | product-plane guard surface |
| non-JSON generated parser | no admitted generated baseline | first material target |
| Overall | `N-direct / NoGo` | seed outcome |

The direct residual rows are not first-wave targets. REDRESS 119 gives each row
a measured fixpoint proof, and REDRESS 120 routes SK-V12 to solve the generated
non-JSON baseline first. Reopen any JSON direct row only with fresh material
evidence beyond REDRESS 114-119. Source anchors:
`skinny/RESULTS.md:5-45`, `skinny/RESULTS.md:143-146`, and
`skinny/REDRESS.md:3497-3553`.

## 3. SK-V12 Goalset

SK-V12 priority order is binding:

1. Create one generated non-JSON direct or typed parser baseline first,
   preferred order CSS L4 declaration values, Sheets, then BBNF-self.
   Before behavior redress, the selected grammar must pass the executable
   baseline pre-gate: generated emission seam or per-grammar runtime path,
   runtime module build, fixture corpus, same-plane independent oracle,
   compile/equality smoke, and REDRESS 111 gate consumption.
2. Admit one measured grammar-generalized intervention against that baseline,
   with generated Track 1 at least `ceil(baseline_mbps * 1.01)` unless S-P3
   sets a stricter threshold, independent oracle/Track 2, strict output
   equality, and gate consumption. The intervention follows the selected
   baseline grammar: CSS L4, Sheets, or BBNF-self.
3. Preserve admitted direct and typed guard rows. The 4 direct `A / GO` rows
   and 7 typed `A / GO` rows cannot silently demote.
4. Keep `parse_only` diagnostic. No parse-only row can count as SOTA admission.
5. Carry W3, parse-only, and JSON direct residual pre-blocks into S-P1/S-P2/S-P3.

## 4. Routed Remainder

W1a admitted only a non-JSON report lane. W1b rejected the generated non-JSON
baseline because the skinny codegen/runtime path remained JSON-profiled and no
generated CSS L4 runtime existed. W2 blocked because it could not create the
first measurable baseline row inside an intervention wave.

W3 through W7 exhausted the JSON direct candidate families in SK-V11:
numeric slot, container-tail dispatch, bounded string span, escaped segment,
and output digest host sink. W8 closed the 13 direct residual rows as measured
fixpoint. W9 closed SK-V11 and routed SK-V12 to the generated non-JSON
baseline.

## 5. Telemetry Binding

The generated non-JSON baseline and intervention must be gate-consumed, either
in `skinny/RESULTS.md` with all consumers updated or in a companion report with
an explicit gate command. Required evidence includes grammar/domain/workload
identity, generated Track 1 provenance, independent Track 2 or oracle source,
strict output equality, Track 1 and oracle Mbps, run id, host, flags, sample
count, output plane, same-wave consumer class, JSON guard status if refreshed,
and fail-closed checks for producer-only telemetry, stale run ids, oracle
coupling, parse-only SOTA claims, W3 reopen claims, and JSON policy leaks.

## 5.1 Alpha Cost And Revert Seed

| Candidate | Wave slot | LOC budget | Risk | Plan cap | Redress cap | Revert / block rule |
|---|---|---:|---|---:|---:|---|
| E1 CSS baseline | W1 preferred | <=520 | high | 30 min | 75 min | revert codegen/runtime/bench/report/gate/RESULTS, save rejected patch, block E4 |
| E2 Sheets baseline | W1 fallback | <=480 | medium-high | 30 min | 75 min | revert selected Sheets slice, save rejected patch, use Sheets intervention only after admit |
| E3 BBNF-self baseline | W1 fallback | <=460 | medium-high | 30 min | 75 min | revert selected BBNF-self slice, save rejected patch, use BBNF-self intervention only after admit |
| E4 selected-baseline intervention | W2 after baseline | <=430 | high | 30 min | 75 min | revert intervention slice, save rejected patch, preserve baseline evidence |
| E5 JSON direct companion | W3+ conditional | <=300 | high | 30 min | 75 min | reject before non-JSON priority resolves; revert JSON slice if attempted |

S-P3 may tighten these caps. Widening them requires CHALLENGE and user
escalation before behavior redress. If baseline preflight fails, split into a
generator/runtime unblock wave and a later baseline-report wave, or record a
measured `BLOCKED` route.

## 6. Refusal Conditions

Refuse any dispatch that:

- asks Alpha-F to create `SPEC.md` or `DISPATCH-PROMPT.md`;
- edits source before S-P3 has created and converged the implementation packet;
- schedules JSON-only direct work before the generated non-JSON baseline and
  measured grammar-generalized intervention priority is satisfied or explicitly
  blocked;
- reopens W3 substrate, parse-only SOTA movement, or a JSON direct residual
  without fresh material evidence beyond REDRESS 114-119;
- admits direct or typed rows without the correct output-plane comparator,
  independent Track 2/oracle, provenance, and gate consumption;
- claims grammar generalization by prose or stale hand-only witness modules;
- emits telemetry without a same-wave gate consumer;
- weakens admitted guard rows without measured disposition;
- adds a directive, BIR variant, public substrate, parser-owned sidecar, second
  retained substrate, generic-crate JSON policy, or x86 implementation target.

## 7. Next Move

Next move: ready-for-S-P1-profile-sk-v12.

S-P1 should freeze the SK-V12-open surface, profile guard rows, treat
`parse_only` and JSON direct residuals as diagnostic/pre-blocked, inventory the
generated non-JSON codegen/runtime gap, audit the REDRESS 111 report lane, and
name the first runnable generated non-JSON baseline candidate for S-P2/S-P3.

G-Alpha presentation seed: target one generated non-JSON baseline plus the
same row's intervention delta; carry JSON direct residuals as pre-blocked
ledger rows; predicted close is either baseline + intervention admit, or a
measured BLOCKED generated-baseline route.
