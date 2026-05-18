# Handoff SK-V9

Date: 2026-05-18.

Status: SK-V9 Pass Alpha contract draft materialized. V9 implementation is not
dispatched. After alpha challenge convergence the orchestrator must present
G-Alpha, and only after `G-Alpha closed` can skinny passes begin. No
`SPEC.md` or `DISPATCH-PROMPT.md` exists for SK-V9 from this alpha-F slice.

## 1. Read First

1. `restart/prompts/pass-contracts/PASS-ALPHA.md`
2. `restart/skinny/tranches/sk-v9/SYNTHESIS.md`
3. `restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md`
4. `restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md`
5. `restart/skinny/tranches/sk-v8/HANDOFF.md`
6. `restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md`
7. `restart/skinny/tranches/sk-v8/research/wave-6-hardening/V2/HARDENING-W6-V2-CONSOLIDATED.md`
8. `skinny/RESULTS.md`
9. `skinny/REDRESS.md` entries 91, 92, and 93

## 2. Current State

SK-V8 is closed by W6 V1+V2 hardening convergence. W6 made no source,
generated-output, benchmark-row, `skinny/RESULTS.md`, or `skinny/REDRESS.md`
change. The current benchmark authority is still the W0-rendered
`skinny/RESULTS.md` report:

| Family | State |
|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` |
| `direct_to_struct` | 3 `A / GO`, 14 `N-direct / NO-GO` |
| `real_typed_struct` | 4 `A / GO` |

All current main rows remain `Strictness=deferred`. Native Rust comparators are
same-run in the W0 report; C++ sidecars are historical or absent unless a later
accepted gate creates a structured same-run sidecar manifest.

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

1. Run alpha challenge on the SK-V9 contract draft.
2. Fold any REVISE dispositions into the alpha artifacts, `SYNTHESIS.md`, and
   `HANDOFF.md`.
3. When alpha challenge converges, present G-Alpha to the user.
4. If the user returns `G-Alpha revise`, revise the contract and re-challenge.
5. If the user returns `G-Alpha closed`, the skinny pass sequence may begin.
   Downstream S-P3 authors the future SK-V9 SPEC from the Section 0 /
   Section 4.1-Section 4.3 goalset after its own P1/P2 entry conditions are met.
   No implementation wave dispatches before that downstream plan converges.

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

The SK-V9 contract remains pre-dispatch until G-Alpha. The detailed wave plan
is intentionally absent here. If a later agent needs wave structure, it must
wait for `G-Alpha closed` and then run the skinny pass substrate; downstream
S-P3 creates the future SPEC from the Pass Alpha goalset only after its entry
conditions are met.
