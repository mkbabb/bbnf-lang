# Handoff SK-V9

Date: 2026-05-19.

Status (2026-05-19, updated): SK-V9's research + planning track is
**fully converged**, implementation W1 and W2 are admitted, two W3
redress candidates are rejected with measurement, and W3 V3 CHALLENGE
has rejected the only remaining mechanically distinct class-lane-only
route as non-admissible under the current gate. The PMU blocker
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
gap for Apache/CITM measured typed rows only. W2 closed the REDRESS 92
proof precursor with a retained `EventGrammar` contract and split-marker
`ValueRef<'doc, 'input, K, G>` cursor proof. The first W3 redress attempt
deleted `consume_structural`, added the class-column substrate, and wired in a
move-consumed `scan_structurals` index, but it missed every W3 must-improve row
and every W10b maintain floor. REDRESS 96 records the reject, and the rejected
patch is saved at `/tmp/skv9-waveW3-rejected.patch`. The W3 V2 follow-up
removed the full structural-position vector and used an allocation-free
streaming cursor over the aarch64 scanner, but it also missed every W3
must-improve row and every W10b maintain floor. REDRESS 97 records that reject,
and the rejected patch is saved at `/tmp/skv9-waveW3-v2-rejected.patch`.
W3 V3 researched the remaining emit-site class-lane-only route; CHALLENGE
V4 rejected it for source redress because it cannot satisfy the current
parse-only producer gate.

The implementation track is blocked at W3. W4a..W4d remain conditional on a
closed W3 union substrate. The next live action is an orchestrator/user
decision: amend or split W3, resequence SK-V9 around another shortlist route,
or retire the current W3 gate. Do not dispatch W4 under the current state.

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
23. `skinny/REDRESS.md` entries 91, 92, 93, 94, 95, 96, and 97
24. `restart/skinny/tranches/sk-v9/research/skv9-W2-research.md`
25. `restart/skinny/tranches/sk-v9/research/skv9-W2-plan.md`
26. `restart/skinny/tranches/sk-v9/research/skv9-W2-challenge.md`
27. `restart/skinny/tranches/sk-v9/research/skv9-W2-challenge-v2.md`
28. `restart/skinny/tranches/sk-v9/research/skv9-W3-research.md`
29. `restart/skinny/tranches/sk-v9/research/skv9-W3-plan.md`
30. `restart/skinny/tranches/sk-v9/research/skv9-W3-challenge.md`
31. `restart/skinny/tranches/sk-v9/research/skv9-W3-challenge-v2.md`
32. `restart/skinny/tranches/sk-v9/research/skv9-W3-research-v2.md`
33. `restart/skinny/tranches/sk-v9/research/skv9-W3-plan-v2.md`
34. `restart/skinny/tranches/sk-v9/research/skv9-W3-challenge-v3.md`
35. `restart/skinny/tranches/sk-v9/research/skv9-W3-research-v3.md`
36. `restart/skinny/tranches/sk-v9/research/skv9-W3-plan-v3.md`
37. `restart/skinny/tranches/sk-v9/research/skv9-W3-challenge-v4.md`
38. `skinny/REDRESS.md` entries 96 and 97

## 2. Current State

SK-V8 is closed by W6 V1+V2 hardening convergence. SK-V9 W0 is closed as a
telemetry-lock recovery, W1 is closed as a measured row-table admission, W2 is
closed as a retained grammar proof, and the first two W3 source candidates are
rejected. W3 V3 CHALLENGE rejects class-lane-only source redress under the
current W3 gate, so SK-V9 is blocked at W3 pending orchestrator/user
adjudication.
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

W2 admission facts:

- `EventGrammar`, `AnyGrammar`, JSON and Sheets witnesses, and proof tests are
  admitted under `G-W2-RETAINED-PROOF`.
- `ValueRef` now has separate zero-sized node-kind and event-grammar axes:
  `ValueRef<'doc, 'input, K = AnyKind, G: EventGrammar = AnyGrammar>`.
- The proof moved zero rows and left `skinny/RESULTS.md` unchanged.

W3 reject facts:

- The attempted class-column substrate passed local correctness and scan parity
  tests, and the attempted generated parser contained no `consume_structural`
  symbol.
- Native Criterion was captured under `/tmp/skv9-w3-criterion` with
  `RUSTFLAGS="-C target-cpu=native"`. `gate-json` refused to write a report
  while Lock 14 frozen roots were dirty with the uncommitted source candidate,
  so `skinny/RESULTS.md` remains the W1-rendered baseline.
- The attempted integration missed all W3 must-improve floors:
  twitter 9284 / 17685, apache_builds 7700 / 14124, update_center 6854 / 14370,
  and distinct_values 6229 / 15731 Mbps.
- It also missed all W10b maintain floors:
  canada 11221 / 15866, citm_catalog 13611 / 28630, instruments 9539 / 15865,
  marine_ik 8012 / 11831, mesh 10087 / 12186, and numbers 13407 / 17596 Mbps.
- The REDRESS 97 V2 follow-up replaced the full structural-position vector
  with an allocation-free streaming cursor over the aarch64 scanner. Correctness
  checks, scan/cursor parity, Track 2/parity, codegen tests, and the proof
  build were green before measurement. The attempted patch contained no
  `consume_structural`, no `into_positions`/`structural_positions`, and no
  value source-byte rediscovery.
- Targeted native Criterion was captured under `/tmp/skv9-w3-v2-target` after
  the partial full run had already falsified the gate rows. The V2 attempt
  missed all W3 must-improve floors:
  twitter 7520 / 17685, apache_builds 6710 / 14124, update_center 5534 / 14370,
  and distinct_values 5338 / 15731 Mbps.
- It also missed all W10b maintain floors:
  canada 8293 / 15866, citm_catalog 9997 / 28630, instruments 7305 / 15865,
  marine_ik 5540 / 11831, mesh 6835 / 12186, and numbers 9542 / 17596 Mbps.
- W3 V3 research found the only remaining materially distinct source shape:
  emit-site class-lane-only. It is mechanically feasible as a future
  preparatory proof slice, but it is parser-produced, preserves scalar
  delimiter discovery, carries no same-wave structural producer, and cannot
  plausibly move the current `track1_generated` parse-only floors. CHALLENGE
  V4 rejects it as current-W3 source redress authority and records the blocker.

## 3. Candidate Boundaries

SK-V9 Alpha may carry only the three W6 residual behavior routes. Alpha-E also
names two non-behavior prerequisites for comparator/report freshness; those are
gate-only enablers and cannot dispatch row-moving implementation.

| Candidate | Boundary |
|---|---|
| Apache/CITM measured typed rows | Admitted by SK-V9 W1 / REDRESS 94 with fresh run-id/metadata validation and six measured `real_typed_struct A / GO` rows. |
| Retained class/event grammar and `ValueRef` cursor proof | Admitted by SK-V9 W2 / REDRESS 95. |
| Union class-column substrate | First SK-V9 W3 redress rejected by REDRESS 96; V2 streaming-cursor redress rejected by REDRESS 97; V3 class-lane-only route rejected by CHALLENGE V4 as non-admissible under the current gate. The rejected patches are `/tmp/skv9-waveW3-rejected.patch` and `/tmp/skv9-waveW3-v2-rejected.patch`; W3 remains open and W4 remains blocked. |
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
| Retained class/event grammar plus `ValueRef` proof | 450 | <=90 min implementation/redress, split before dispatch if production consumer does not fit | Closed by W2 / REDRESS 95; proof-only, no `RESULTS.md` row movement |
| Direct output/control-path contract | 600 | <=90 min implementation/redress, split before dispatch if exceeded | May move direct guard rows only under future accepted direct contract gates |
| Comparator sidecar same-run manifest | 500 | <=90 min implementation/redress, split before dispatch if exceeded | No behavior movement; freshness/plane fields only |
| SK-V9-open telemetry/gate refresh | 450 | <=90 min implementation/redress, split before dispatch if exceeded | No behavior movement; row additions require their own accepted gate |

## 4. Next Move

1. Treat G-Alpha, `G-W0-TELEMETRY-LOCK`, `G-S-P1-RERUN-CONVERGED`,
   `G-W1-TYPED-ADMISSION`, `G-W2-RETAINED-PROOF`, REDRESS 96, REDRESS 97, and
   W3 CHALLENGE V4 as recorded.
2. Do not dispatch W4. The cascade-lock still requires a closed W3 union
   substrate.
3. Escalate W3 to orchestrator/user decision. The available choices are:
   amend or split W3, resequence SK-V9 around another shortlist route, or
   retire the current `G-W3-UNION-SUBSTRATE` gate.
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
- W3 structural implementation outside the accepted W2 retained grammar plus
  retained `ValueRef` cursor proof boundary.
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
convergence, post-W1, post-W2, post-two-W3-redress-rejects, and post-W3 V3
CHALLENGE blocker. W1 admitted the row-table-only Apache/CITM measured typed
rows. W2 admitted the retained grammar proof. REDRESS 96 rejects the first W3
source shape, REDRESS 97 rejects the V2 streaming-cursor source shape, and
CHALLENGE V4 rejects the class-lane-only route as non-admissible under the
current W3 gate. The next live dependency is still W3 union-substrate
acceptance, but it now needs orchestrator/user adjudication before any further
source redress; all W4 sub-waves remain blocked behind that cascade order.
