# Handoff SK-V9

Date: 2026-05-19.

Status (2026-05-19, updated): SK-V9's research + planning track is
**fully converged**, and implementation W1 is admitted. The PMU blocker
that stalled S-P1 V2 was resolved
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
lock remains closed (`SK-V9-open`). W1 closed the REDRESS 91 row-table
gap for Apache/CITM measured typed rows only.

The implementation track now proceeds W2 → W3 → W4a..W4d → W5 per
`SPEC.md`, each a research → plan → CHALLENGE where required → redress
cycle landing measured source change. W2 (retained class/event grammar
plus `ValueRef` proof) is the next dispatch and must pass its proof
CHALLENGE before W3 can open. W3 (union event-model) is the structural
fix that deletes the scalar `consume_structural` rediscovery pass and
wires in the discarded SIMD structural index.

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
21. `restart/skinny/tranches/sk-v9/research/skv9-W1-research.md`
22. `restart/skinny/tranches/sk-v9/research/skv9-W1-plan.md`
23. `skinny/REDRESS.md` entries 91, 92, 93, and 94

## 2. Current State

SK-V8 is closed by W6 V1+V2 hardening convergence. SK-V9 W0 is closed as a
telemetry-lock recovery, and W1 is closed as a measured row-table admission.
The current benchmark authority is the W1-rendered `skinny/RESULTS.md`
`SK-V9-open` report:

```text
sk-v9-open:criterion-fnv64-a1e8a51ae806d386
```

| Family | State |
|---|---|
| `parse_only` | 17 `S / NO-GO` |
| `direct_to_struct` | 3 `A / GO`, 14 `N-direct / NO-GO` |
| `real_typed_struct` | 6 `A / GO` |

All current main rows remain `Strictness=deferred`. Native Rust comparators are
same-run in the current report; C++ sidecars are historical or absent unless a
later accepted gate creates a structured same-run sidecar manifest. Structural
scan, masking probes, PMU, and cycles surfaces remain diagnostic non-producers.

W1 admission facts:

- `apache_builds/real_typed_struct` is `A / GO`: 8174 Mbps Track 1 versus
  8110 Mbps sonic-rs typed strict, with independent Track 2/oracle at
  6728 Mbps.
- `citm_catalog/real_typed_struct` is `A / GO`: 35102 Mbps Track 1 versus
  22058 Mbps sonic-rs typed strict, with independent Track 2/oracle at
  19143 Mbps.
- `canada/real_typed_struct` remains absent/rejected. W1 admits Apache and
  CITM only.

## 3. Candidate Boundaries

SK-V9 Alpha may carry only the three W6 residual behavior routes. Alpha-E also
names two non-behavior prerequisites for comparator/report freshness; those are
gate-only enablers and cannot dispatch row-moving implementation.

| Candidate | Boundary |
|---|---|
| Apache/CITM measured typed rows | Admitted by SK-V9 W1 / REDRESS 94 with fresh run-id/metadata validation and six measured `real_typed_struct A / GO` rows. |
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
| Apache/CITM typed row-table admission | 300 | <=90 min implementation/redress, split before dispatch if exceeded | Closed by W1 / REDRESS 94 |
| Retained class/event grammar plus `ValueRef` proof | 450 | <=90 min implementation/redress, split before dispatch if production consumer does not fit | Proof-only; no `RESULTS.md` row movement at Alpha depth |
| Direct output/control-path contract | 600 | <=90 min implementation/redress, split before dispatch if exceeded | May move direct guard rows only under future accepted direct contract gates |
| Comparator sidecar same-run manifest | 500 | <=90 min implementation/redress, split before dispatch if exceeded | No behavior movement; freshness/plane fields only |
| SK-V9-open telemetry/gate refresh | 450 | <=90 min implementation/redress, split before dispatch if exceeded | No behavior movement; row additions require their own accepted gate |

## 4. Next Move

1. Treat G-Alpha, `G-W0-TELEMETRY-LOCK`, `G-S-P1-RERUN-CONVERGED`, and
   `G-W1-TYPED-ADMISSION` as recorded.
2. Dispatch W2 next: retained class/event grammar plus `ValueRef` proof.
3. Run the mandatory W2 CHALLENGE before redress. W3 remains blocked until W2
   proof acceptance.
4. Preserve the W1 boundary: Apache/CITM measured typed rows are admitted;
   Canada typed and direct guard-plane claims remain blocked.

## 5. Pre-Blocked Routes

Do not reopen under SK-V9 without fresh measured evidence, exact owner paths,
same-wave consumer, no-regression gate, REDRESS citation, and challenge
acceptance:

- Apache/CITM measured-row overclaim from REDRESS 91 is closed only by W1's
  fresh measured row-table admission. No Canada/direct row inherits it.
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

The SK-V9 contract is post-G-Alpha, post-W0, post-S-P1/S-P2/S-P3
convergence, and post-W1. W1 admitted the row-table-only Apache/CITM
measured typed rows. The next live dependency is W2 proof acceptance; W3 and
all W4 sub-waves remain blocked behind that proof/cascade order.
