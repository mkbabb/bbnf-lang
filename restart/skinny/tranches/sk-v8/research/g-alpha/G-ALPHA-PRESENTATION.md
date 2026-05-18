# SK-V8 G-Alpha Presentation

Date: 2026-05-18.
Scope: Present the converged SK-V8 packet for the mandatory G-Alpha decision. This artifact is not an implementation dispatch.

## Authority

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/prompts/ORCHESTRATOR.md`
- `restart/skinny/tranches/sk-v8/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v8/research/alpha-hardening/V2/CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V5-CONSOLIDATED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

## Pass Alpha Fold

Pass Alpha V2 accepted the final SK-V8 contract for G-Alpha presentation with W0-only dispatch authority. The later S-P2 and S-P3 passes do not replace Pass Alpha; they harden the packet by adding the substrate-ceiling finding and the converged W0-W6 wave plan.

Folded state:

- Pass Alpha: ACCEPT for G-Alpha presentation after V2 revisions to Lock 14 gates, W2 full-table maintain, cost/LOC caps, sidecar freshness, and generated/RESULTS review accounting.
- S-P2: converged after V6 and V7 consecutive 6/6 ACCEPT cycles. It nominates W3 Tier A tape plus structural-projection union as a lead hypothesis but authorizes no implementation.
- S-P3: converged after V4 and V5 consecutive 6/6 ACCEPT cycles. It binds the wave manifest, per-wave falsifiability gates, telemetry schema, pre-block ledger, and dispatch prompt.

## Decision Summary

Recommended decision: `G-Alpha closed` for W0 only.

Reason: the packet is converged for planning and hardening, but the measured current state is still observability-bound. W0 is the only safe first dispatch because it creates `SK-V8-open`, required telemetry, profile artifacts, strict comparator metadata, sidecar freshness, CostFacts placeholders, and gate rejection for missing telemetry before behavior waves can admit.

`G-Alpha closed` authorizes only:

- SK-V8 W0 research, plan, and telemetry-only redress under `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`;
- 0 production behavior LOC;
- <=350 report/gate/schema/test/doc LOC;
- <=90 minutes inclusive of implementation, generation, verification, RESULTS/REDRESS updates, and rollback;
- no parser, scanner, SIMD, asm, codegen, generated parser output, or product-plane behavior change.

It does not authorize W1-W6. Those waves require W0 admission plus their own exact entry gates, plans, challenge where required, and dispatch.

## Current Row Surface

| Family | Opening state | G-Alpha posture |
|---|---:|---|
| `parse_only` | 17 `K / NO-GO` rows | W0 telemetry only; substrate-guard non-admission until measured-path strict gates exist. |
| `direct_to_struct` | 6 `A / GO`, 11 `N-direct / NO-GO` rows | Direct digest rows remain guard rows, not typed product proof. |
| `real_typed_struct` | 4 `A / GO` rows | Existing typed rows are maintain gates; W2 may extend only after W0/W1 and a fresh plan. |

Required W0 close signal: all 38 current main rows have required telemetry and every throughput cell stays within +/-1.0% of the captured `SK-V8-open` seed.

## Wave Manifest At G-Alpha

| Wave | Status at G-Alpha | Hard cap | Source/edit LOC budget |
|---|---|---:|---:|
| W0 Baseline Profile And Telemetry Lock | Dispatchable only after `G-Alpha closed` | <=90 min | 0 production behavior LOC; <=350 report/gate/schema/test/doc LOC |
| W1 CostFacts Gate Binding | Blocked until W0 closes | <=90 min | 0 parser/generated behavior LOC; <=300 CostFacts/report/gate/test LOC |
| W2 Typed Product Plane Expansion | Blocked until W0/W1 and W2 plan update | <=90 min | <=650 source/test LOC |
| W3 Tier A Tape Plus Structural-Projection Union | Blocked until W0/W1, fresh W3 plan, and challenge ACCEPT | <=90 min | <=450 source/test LOC default; <=650 only with accepted pre-redress fit proof |
| W4 Direct Guard Triage | Blocked until W0/W1 and W2/W3 disposition | <=90 min | <=300 source/test LOC and <=3 selected rows |
| W5 Grammar-Neutral Audit | Blocked until W1-W4 dispositions | <=90 min | 0 source LOC default; <=150 named Lock 14 cleanup LOC |
| W6 Close And Alpha Feedback | Blocked until W0-W5 dispositions | <=90 min | 0 source LOC |

## Protected Constraints

- Strict-vs-strict comparator discipline is mandatory.
- Lock 14 grammar neutrality is mandatory.
- No new directive, BIR variant, substrate, `BackendShape`, `UnionTape`, public substrate API, sidecar substrate, parser-owned cursor/facts, or consumer-later primitive.
- No stale sidecar, permissive, lossy, `parse_only`, `tape_vs_tape`, or telemetry-only row can close a strict SOTA claim.
- Every primitive, substrate, generated path, or materializer needs a same-wave production consumer.
- Every miss rejects, routes to REDRESS, or splits before redress; no deferral closes a wave.
- Research, plan, challenge, and redress roles remain separated.

## G-Alpha Choice

The mandatory user gate has two valid outcomes:

- `G-Alpha closed`: dispatch SK-V8 W0 only.
- `G-Alpha revise`: revise the named packet sections before any SK-V8 wave dispatch.

No SK-V8 implementation wave dispatches before this decision.
