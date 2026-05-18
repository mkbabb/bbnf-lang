# Handoff SK-V9

Date: 2026-05-18.

Status (2026-05-18, updated): SK-V9's research + planning track is
**fully converged**. The PMU blocker that stalled S-P1 V2 was resolved
— `xcode-select` was switched to the full Xcode toolchain, the Xcode
licence accepted, and `xctrace` (CPU Counters + Time Profiler) now
captures real per-symbol PMU data with no sudo. The three skinny passes
have each converged per `ORCHESTRATOR.md` §3Z:

- **S-P1 Profile** — converged (V3-V6). Real PMU across 17 corpora ×
  {Track 1, Track 2}; deep hot-leaf attribution; the structural
  correlation OLS; the substrate-neutral primitive vocabulary.
  Authority: `research/p1/hardening/HARDENING-S-P1-CONVERGED.md`.
- **S-P2 Research** — converged (V1-V4). Six intervention designs:
  union event-model, retained-grammar proof, Apache/CITM admission,
  aarch64 ASM opportunities, unicode-escape codec, SOTA teardown.
  Authority: `research/p2/hardening/HARDENING-S-P2-CONVERGED.md`.
- **S-P3 Synthesis-Plan** — converged (V1-V4). The SK-V9 wave plan.
  Authority: `research/p3/hardening/HARDENING-S-P3-CONVERGED.md`.

`SPEC.md` and `DISPATCH-PROMPT.md` are the converged S-P3 wave plan,
promoted from the `research/p3/skv9-p3-F-*` drafts. The W0 telemetry-
lock remains closed (`SK-V9-open`).

The next phase is the **implementation track**: the wave triumvirate
executes W1 → W2 → W3 → W4a..W4d → W5 per `SPEC.md`, each a
research → plan → redress cycle landing measured source change. W1
(Apache/CITM measured-row admission) is the independent, lowest-risk
first wave; W3 (union event-model) is the structural fix that deletes
the scalar `consume_structural` rediscovery pass and wires in the
discarded SIMD structural index. Behavior waves are unblocked.

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
10. `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md`
11. `restart/skinny/tranches/sk-v9/research/skv9-W0-r1-gate-report-baseline.md`
12. `restart/skinny/tranches/sk-v9/research/skv9-W0-r2-criterion-metadata.md`
13. `restart/skinny/tranches/sk-v9/research/skv9-W0-r3-diagnostic-fences.md`
14. `restart/skinny/tranches/sk-v9/research/skv9-W0-r4-typed-direct-fences.md`
15. `restart/skinny/tranches/sk-v9/research/skv9-W0-r5-lock14-redress.md`
16. `restart/skinny/tranches/sk-v9/research/skv9-W0-r6-spec-dispatch-shape.md`
17. `restart/skinny/tranches/sk-v8/HANDOFF.md`
18. `restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md`
19. `restart/skinny/tranches/sk-v8/research/wave-6-hardening/V2/HARDENING-W6-V2-CONSOLIDATED.md`
20. `skinny/RESULTS.md`
21. `skinny/REDRESS.md` entries 91, 92, and 93

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

S-P1 V2 partial profile state:

- 106 fresh samply profile/sidecar pairs exist under `/tmp/skv9-p1-rerun`.
- P1-A/P1-B/P1-C/P1-E/P1-F have fresh SK-V9-open evidence.
- P1-D is blocked: `perf` absent, `xctrace` requires full Xcode, and
  `powermetrics` requires superuser access unavailable to this run.
- `HARDENING-S-P1-V2-CONSOLIDATED.md` records 4/6 ACCEPT and BLOCKED.

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

1. Treat G-Alpha, `G-W0-TELEMETRY-LOCK`, and S-P1 V2 partial samply evidence as
   recorded.
2. Do not dispatch S-P2 or W1+ behavior waves; `G-S-P1-RERUN-CONVERGED` did not
   pass.
3. Resolve the P1-D PMU/cycles blocker by providing a real counter source
   (`perf`, full-Xcode `xctrace`, privileged `powermetrics`, or an accepted
   contract amendment). Do not estimate c/B from ns/B.
4. After P1-D is repaired, rerun/challenge S-P1 to convergence before any
   revised S-P2/S-P3 or behavior dispatch.

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

The SK-V9 contract is post-G-Alpha, post-W0, and post-S-P1-V2-BLOCKED, but
still pre-behavior. W0 closed the telemetry-lock; S-P1 V2 produced useful fresh
samply evidence but did not converge because P1-D PMU/cycles is blocked.
Behavior waves must not dispatch until S-P1 convergence and
`G-BEHAVIOR-RELEASE`.
