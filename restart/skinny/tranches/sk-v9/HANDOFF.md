# Handoff SK-V9

Date: 2026-05-18.

Status: G-Alpha is closed by user instruction on 2026-05-18. W0 telemetry-lock
is closed with `skinny/RESULTS.md` rendered and consumed as `SK-V9-open`.
SK-V9 S-P1 V1 remains an honest opening gap ledger, not a completed S-P1
profile. Behavior waves remain blocked until a fresh post-W0 S-P1 profile rerun
converges and `G-BEHAVIOR-RELEASE` passes.

## 1. Read First

1. `restart/prompts/pass-contracts/PASS-ALPHA.md`
2. `restart/skinny/tranches/sk-v9/SYNTHESIS.md`
3. `restart/skinny/tranches/sk-v9/SPEC.md`
4. `restart/skinny/tranches/sk-v9/DISPATCH-PROMPT.md`
5. `restart/skinny/tranches/sk-v9/research/skv9-W0-plan.md`
6. `restart/skinny/tranches/sk-v9/research/skv9-W0-close.md`
7. `restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md`
8. `restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md`
9. `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
10. `restart/skinny/tranches/sk-v9/research/skv9-W0-r1-gate-report-baseline.md`
11. `restart/skinny/tranches/sk-v9/research/skv9-W0-r2-criterion-metadata.md`
12. `restart/skinny/tranches/sk-v9/research/skv9-W0-r3-diagnostic-fences.md`
13. `restart/skinny/tranches/sk-v9/research/skv9-W0-r4-typed-direct-fences.md`
14. `restart/skinny/tranches/sk-v9/research/skv9-W0-r5-lock14-redress.md`
15. `restart/skinny/tranches/sk-v9/research/skv9-W0-r6-spec-dispatch-shape.md`
16. `restart/skinny/tranches/sk-v8/HANDOFF.md`
17. `restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md`
18. `restart/skinny/tranches/sk-v8/research/wave-6-hardening/V2/HARDENING-W6-V2-CONSOLIDATED.md`
19. `skinny/RESULTS.md`
20. `skinny/REDRESS.md` entries 91, 92, and 93

## 2. Current State

SK-V8 is closed by W6 V1+V2 hardening convergence. SK-V9 W0 is now closed as a
telemetry-lock recovery. The current benchmark authority is the W0-rendered
`skinny/RESULTS.md` `SK-V9-open` report:

```text
sk-v9-open:criterion-fnv64-cd1673844eeea12f
```

| Family | State |
|---|---|
| `parse_only` | 17 `S / NO-GO` |
| `direct_to_struct` | 3 `A / GO`, 14 `N-direct / NO-GO` |
| `real_typed_struct` | 4 `A / GO` |

All current main rows remain `Strictness=deferred`. Native Rust comparators are
same-run in the W0 report; C++ sidecars are historical or absent unless a later
accepted gate creates a structured same-run sidecar manifest. Structural scan,
masking probes, PMU, and cycles surfaces remain diagnostic non-producers.

## 3. Candidate Boundaries

SK-V9 Alpha may carry only the three W6 residual behavior routes. Alpha-E also
names two non-behavior prerequisites for comparator/report freshness; those are
gate-only enablers and cannot dispatch row-moving implementation.

| Candidate | Boundary |
|---|---|
| Apache/CITM measured typed rows | REDRESS 91 admits source/product parity only. A V9 row-table wave must own fresh run-id/metadata validation and measured rows before claiming six measured `real_typed_struct A / GO` rows. |
| Retained class/event grammar and `ValueRef` cursor proof | REDRESS 92 rejected SK-V8 W3 before source redress. No structural-heavy parse implementation reopens until the retained grammar and cursor proof are accepted. |
| Direct output/control-path contract | REDRESS 93 rejected scalar-parent folding. Direct digest misses remain guard-plane rows until a direct output contract or control-path tranche exists. |
| Comparator sidecar same-run manifest | Gate-only evidence ingestion. It cannot produce parser data, retained tape data, row output, substrate, or strict admission by itself. |
| SK-V9-open telemetry/gate refresh | Gate-only report refresh. It cannot move throughput cells, admit Apache/CITM measured rows, or alter parser/scanner/SIMD/codegen behavior. |

Pass Omega owns SC-6-L1-R1, broad lock amendments, canonical path cleanup, and
top-level surface refresh. Those are not SK-V9 skinny defaults unless Omega
has separately ratified them.

Alpha cost binding for any later S-P3 plan:

| Candidate | LOC budget | Hard cap | Row effect before future S-P3 |
|---|---:|---|---|
| Apache/CITM typed row-table admission | 300 | <=90 min implementation/redress, split before dispatch if exceeded | May admit measured typed rows only with fresh row/run evidence |
| Retained class/event grammar plus `ValueRef` proof | 450 | <=90 min implementation/redress, split before dispatch if production consumer does not fit | Proof-only; no `RESULTS.md` row movement at Alpha depth |
| Direct output/control-path contract | 600 | <=90 min implementation/redress, split before dispatch if exceeded | May move direct guard rows only under future accepted direct contract gates |
| Comparator sidecar same-run manifest | 500 | <=90 min implementation/redress, split before dispatch if exceeded | No behavior movement; freshness/plane fields only |
| SK-V9-open telemetry/gate refresh | 450 | <=90 min implementation/redress, split before dispatch if exceeded | No behavior movement; row additions require their own accepted gate |

## 4. Next Move

1. Treat G-Alpha and `G-W0-TELEMETRY-LOCK` as closed.
2. Rerun S-P1 against the SK-V9-open baseline:
   `sk-v9-open:criterion-fnv64-cd1673844eeea12f`.
3. Challenge S-P1 to convergence; do not use absent samply/PMU/cycles rows,
   stale SK-V4/SK-V8 fused evidence, source-eligible-only typed rows, or
   Criterion-slope-only hot leaves as behavior ancestry.
4. Only after `G-S-P1-RERUN-CONVERGED`, rerun/revise S-P2 and S-P3 before any
   W1+ behavior dispatch.

## 5. Pre-Blocked Routes

Do not reopen under SK-V9 without fresh measured evidence, exact owner paths,
same-wave consumer, no-regression gate, REDRESS citation, and challenge
acceptance:

- Apache/CITM measured-row overclaim from REDRESS 91.
- `canada/real_typed_struct` without full-fixture DirectBuild-vs-serde checksum
  proof.
- W3 structural implementation without retained class/event grammar plus
  retained `ValueRef` cursor proof.
- W4 scalar-parent fold or renamed parent-digest fold without a V9-aware checked
  gate, full-table maintain proof, and independent Track 2 digest-arithmetic
  backstop.
- REDRESS 73 helper-shape transfer from generated retained parsing to hand
  Track 2 or control-path work without direct hand-parser code-layout profiling.
- Sidecar substrate, parser-owned cursor/fact slots, `UnionTape`, new
  `BackendShape`, new directive/BIR, public substrate API, and `tape_vs_tape`
  as production consumer.
- PMULL prefix-XOR and CTZ/bulk production rewires as default hot paths.
- Generic JSON policy leaks or Lock 14 weakening.

The full prior pre-block ledger in
`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md` is
binding by reference. Any future candidate touching a rejected ownership
boundary must cite the REDRESS item, state why the shape is materially
different, and pass challenge before implementation planning.

## 6. Close Posture

The SK-V9 contract is post-G-Alpha and post-W0, but still pre-behavior. W0
closed the telemetry-lock; it did not admit rows, strictness, parser behavior,
scanner behavior, SIMD behavior, codegen behavior, generated output, or typed
product shortcuts. Behavior waves must not dispatch until fresh S-P1 rerun
convergence and `G-BEHAVIOR-RELEASE`.
