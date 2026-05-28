---
agent: 3B
pass: T-P3-synthesis
cycle: V3
generated_at: 2026-05-28T08:13:05Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
t_p2_dossiers_consumed: [2A, 2B, 2C, 2D, 2E, 2F]
v1_surface_targeted: MASTER-PLAN.md
proposed_deltas_count: 11
delta_summary:
  carried_from_prior_cycle: [MP-3B-V1-D01, MP-3B-V1-D02, MP-3B-V1-D03, MP-3B-V1-D04, MP-3B-V1-D05, MP-3B-V1-D06, MP-3B-V1-D07, MP-3B-V1-D08, MP-3B-V1-D09, MP-3B-V1-D10, MP-3B-V1-D11]
  removed: []
  answered: [CH4-COST-01, CH4-COST-02, CH4-COST-04, CH6-V1-01, CH4-V2-001]
  newly_added: []
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised:
    - "CH4-COST-01: W7/W8/W9 receiver rows now use 2D costed bands and state consumer/gate, hard-cap fit, fail action, and no-W12 route."
    - "CH4-COST-02: W4 Pattern H is split into provenance gate, generator/check proof, runtime projection, destructive deletion, and close transcript sub-rows."
    - "CH4-COST-04: W5/W6 CSS typed provider rows are explicitly scoped; broad CSSOM rewrite is not hidden in either wave."
    - "CH6-V1-01: Open Questions table now carries receiver, blocker, and gate fields for every row."
    - "CH4-V2-001: Every carried 3B delta now has row-level LOC, numeric propagation count, risk, wave alignment, consumer/gate, hard-cap fit, and fail action."
---

## Executive Summary

`restart/MASTER-PLAN.md` is one live skinny tranche behind the locked SK-V15
contract. Its A-J tranche stubs remain pending, and the H-tranche ledger already
marks several scoped landings, partials, and refuted routes. The stale part is
the receiver framing: SK-V14/SK-V13 MP.NW rows still read as the current route
while SK-V15 S-P3 V4 has locked W0-W11 as the prune-before-rebuild
implementation contract. The reconciliation should not revive refuted CSS
admits, delete-before-provider paths, checkasm-only primitive admits, or
Decision Engine scaffolds. It should add one SK-V15 MASTER receiver block,
reclassify old CSS and SK-V14 rows as historical/pre-block evidence, preserve
JSON 51-row guard evidence as landed-scoped rather than generalisation proof,
and route Pattern H, Lock 14/16, CSS typed provider, Decision Engine/lowerers,
FNV quarantine, and PASS-IMPL V2 to W0-W11 with explicit LOC/risk/wave
alignment. T-P3 only proposes these MASTER deltas; Pass Omega CRUD must apply
any accepted text after G-Omega.

V3 folds the V1 hardening REVISE findings and the V2 `CH4-V2-001` row-field
coverage finding that affect this MASTER proposal. The fold remains
proposal-only: it changes no live V1 spec surface and only narrows the proposed
receiver map, cap routing, and question routing that Pass Omega may later apply.

## V3 Delta Summary

| bucket | ids | note |
|---|---|---|
| carried | MP-3B-V1-D01..D11 | V3 keeps the same proposed MASTER delta set and revises cost/gate wording inside D03, D05, D07, D08, and the Open Questions table. |
| removed | none | No prior proposed MASTER delta is removed. |
| answered | CH4-COST-01, CH4-COST-02, CH4-COST-04, CH6-V1-01, CH4-V2-001 | W7-W9 use 2D costed bands; W4 is split; W5/W6 CSS scope is explicit; every open question has receiver, blocker, and gate; every carried 3B delta has row-level CH4 cap/fail coverage. |
| newly added | none | V3 adds no new MASTER delta id; it folds V1 and V2 REVISE findings into the existing proposal-only slice. |

## Wave Classification Ledger

### A-J Tranche Set

The A-J tranche set remains a pending V1 implementation skeleton. MASTER says
the A-J counts are planning stubs and records a current concrete census of 59
stub waves, not landed tranche work (`restart/MASTER-PLAN.md:189-223`). SK-V15
adds a front-loaded skinny receiver block before any V1 implementation tranche
can be treated as live route, because S-P3 locked W0-W11 and states W0 is the
first legal implementation wave after required authorization
(`restart/skinny/tranches/sk-v15/SPEC.md:29-43`;
`restart/skinny/tranches/sk-v15/research/p3/hardening/HARDENING-S-P3-V4-CONSOLIDATED.md:35-49`).

| MASTER wave group | V1 status | evidence | allocation note |
|---|---|---|---|
| A.W0..A.W4 | pending | `restart/MASTER-PLAN.md:290-307` | Keep as pending; no implementation tranche starts before SK-V15 W0-W11 and Pass Omega/G-Omega routing. |
| B.W0..B.W4 | pending | `restart/MASTER-PLAN.md:351-370` | Keep as pending runtime-substrate body work; reject any hidden sidecar/parallel substrate route per SK-V15 pre-blocks (`restart/skinny/tranches/sk-v15/SPEC.md:467-484`). |
| C.W0..C.W5 | pending | `restart/MASTER-PLAN.md:386-419` | Keep pending; C.W4/C.W5 receive W7 Decision Engine proof only after W7 emits nonzero rewrite/CSP evidence. |
| D.W0..D.W5 | pending | `restart/MASTER-PLAN.md:433-453` | Keep pending; parse-that/regex work must use scalar oracle and generated consumer gates (`restart/audit/totality/p2/2F-parse-that-gaps.md:73-80`). |
| E.W0..E.W4 | pending | `restart/MASTER-PLAN.md:468-484` | Keep pending; no BIR/new-shape shortcut may repair lowerer debt because SK-V15 forbids new/sixth `BackendShape` (`restart/skinny/tranches/sk-v15/SPEC.md:147-153`). |
| F.W0..F.W5 | pending | `restart/MASTER-PLAN.md:499-533` | Keep pending; F.W5 inherits Pattern H/generated-output truth repair from SK-V15 W4. |
| G.W0..G.W4 | pending | `restart/MASTER-PLAN.md:548-565` | Keep pending; future-grammar proof depends on Lock 14 full-surface and non-JSON receivers (`restart/skinny/tranches/sk-v15/SPEC.md:206-217`). |
| I.W0..I.W4 | pending | `restart/MASTER-PLAN.md:884-900` | Keep pending; no SK-V15 evidence changes I ordering. |
| J.W0..J.W5 | pending | `restart/MASTER-PLAN.md:915-935` | J.W1/J.W5 must consume SK-V15 W11/PASS-IMPL V2, not old CSS W8R admits (`restart/skinny/tranches/sk-v15/SPEC.md:447-465`). |

### H-Tranche Rows

| MASTER wave | V1 classification | evidence | SK-V15 allocation |
|---|---|---|---|
| H.W0 | landed-scoped | MASTER marks preflight/capacity/escape-mask prerequisite landed, not a throughput admit (`restart/MASTER-PLAN.md:577-578`). | Preserve as prerequisite evidence only. |
| H.W1 | landed-scoped / partial | MASTER marks Rust-state substrate/backend-shape derivation landed with throughput recovery pending (`restart/MASTER-PLAN.md:578-580`; `restart/MASTER-PLAN.md:620`). | Keep scoped; W0/W11 recapture row movement before prescribing new kernels. |
| H.W2 | partial | MASTER marks consumed primitive subset admitted and new primitives gated (`restart/MASTER-PLAN.md:579-580`; `restart/MASTER-PLAN.md:621-622`). | Reclass source-present primitive work through SK-V15 W2 manifest and same-wave consumer gates. |
| H.W2.5 | partial / pending gate | MASTER keeps primitive vocabulary but blocks contract-only macros until consumed/deleted/demoted/blocked (`restart/MASTER-PLAN.md:580`; `restart/MASTER-PLAN.md:622`). | W2 primitive-status report; future W7/W8/W9 only with named consumer. |
| H.W3 | split: number landed, UTF-8 fusion refuted | MASTER says number landed and UTF-8 fusion is refuted as close (`restart/MASTER-PLAN.md:581`; `restart/MASTER-PLAN.md:623`). | Preserve number evidence; no old UTF-8/string replay without fresh W0/P1 evidence. |
| H.W4 | partial | MASTER says generated `SinkOnly` is correctness-green but overall direct/typed gate remains open (`restart/MASTER-PLAN.md:582`; `restart/MASTER-PLAN.md:624`). | W9 handles SinkOnly equality/all-five gate; no Track 1 == Track 2 sidecar. |
| H.W4.LOCK14 | partial | MASTER says some cleanup landed but fleet-wide Lock 14 remains pending (`restart/MASTER-PLAN.md:583`; `restart/MASTER-PLAN.md:625`). | Split into W2 scan, W3 codegen leaks, W4 Pattern H, W5/W6 CSS provider proof. |
| H.W5 | landed-scoped | MASTER marks consumed arm64/generic set landed, with no-orphan rule mandatory (`restart/MASTER-PLAN.md:584`; `restart/MASTER-PLAN.md:626`). | Preserve only consumed primitive evidence; W2 rejects source inventory as admission. |
| H.W6 | refuted/replaced | MASTER says old H.W6 is replaced by the SK-V13 full-SOTA receiver map (`restart/MASTER-PLAN.md:585`; `restart/MASTER-PLAN.md:627`). | Supersede again with SK-V15 W1/W5/W6 honesty/value/retime contract. |
| H.W7 | pending | MASTER keeps Pratt recognizer facts and `PrattSpine` pending on C/E closure (`restart/MASTER-PLAN.md:586`; `restart/MASTER-PLAN.md:628`). | Keep pending; no SK-V15 close dependency. |

### MP.NW And Historical Receiver Blocks

| MASTER receiver | V1 classification | reason | proposed receiver |
|---|---|---|---|
| MP.NW0 | pending process | G-Omega/Totality ratification remains required before implementation waves (`restart/MASTER-PLAN.md:634`; `restart/skinny/tranches/sk-v15/SPEC.md:31-35`). | Reword to SK-V15 W0 precondition. |
| MP.NW1 | pending / modify | Current-state authority and telemetry must move from SK-V12/SK-V14 framing to SK-V15 fields (`restart/MASTER-PLAN.md:635`; `restart/skinny/tranches/sk-v15/SPEC.md:100-122`). | W0 telemetry lock. |
| MP.NW2..MP.NW4 | refuted as live CSS close route | They still describe CSS feature expansion under strict lightningcss parity (`restart/MASTER-PLAN.md:636-638`), but SK-V15 says current CSS broadcast is diagnostic and lightningcss counts only after comparable CSSOM/value output (`restart/skinny/tranches/sk-v15/SPEC.md:61-63`; `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21-31`). | W1 demotion, W5 typed provider, W6 cssparser retime. |
| MP.NW5 | landed-scoped / pending recapture | JSON 51-row guard is valid in RESULTS notes, but SK-V15 W0 must establish `SK-V15-open` and W11 must maintain it (`skinny/RESULTS.md:139-149`; `restart/skinny/tranches/sk-v15/SPEC.md:250-262`). | W0/W11 guard. |
| MP.NW6 | pending / split | Lock 14 provider/config/sink/fact repair remains needed, but SK-V15 splits it across gate, codegen, Pattern H, CSS provider, and retime waves (`restart/MASTER-PLAN.md:640`; `restart/skinny/tranches/sk-v15/SPEC.md:172-185`). | W2/W3/W4/W5/W6. |
| MP.NW7 | pending | Regex/HIR facts remain an import boundary, not a runtime regex substrate (`restart/MASTER-PLAN.md:641`; `restart/audit/totality/p2/2F-parse-that-gaps.md:73-80`). | D/H plus W7 generated-selection consumer only. |
| MP.NW8 | pending / split | Decision replacement is required, but T-P2 cost splits it into W7 Decision, W8 lowerers A, W9 lowerers B/all-five (`restart/MASTER-PLAN.md:642`; `restart/audit/totality/p2/2D-cost-model.md:70-76`). | W7/W8/W9. |
| MP.NW9 | pending / conditional | AArch64 ASCII/run-skip production split needs scalar oracle, strict parity, same-wave consumer, and row movement (`restart/MASTER-PLAN.md:643`; `restart/audit/totality/p2/2B-primitive-vocabulary.md:101-115`). | W2 gate first; later only if selected. |
| MP.NW10 | refuted unless fresh material differential | MASTER already requires fresh union/substrate variant or architectural block (`restart/MASTER-PLAN.md:644`), while SK-V15 forbids public `UnionTape`, second tape, retained streams, and new substrate APIs (`restart/skinny/tranches/sk-v15/SPEC.md:147-153`). | Preserve refusal/pre-block row; no implementation shortcut. |
| MP.NW11 | pending | Both Sheets and BBNF-self/future grammar witnesses still matter for Lock 14 (`restart/MASTER-PLAN.md:645`; `restart/audit/totality/p2/2C-grammar-neutrality.md:72-75`). | W3/W5/W7/W8/W9 receiver matrix. |
| MP.NW12 | pending / close | Rolling SOTA/no-demotion remains needed, but CSS old rows are not live admits (`restart/MASTER-PLAN.md:646`; `restart/skinny/tranches/sk-v15/SPEC.md:455-465`). | W11 plus J.W1/J.W5. |
| SK-V14 W0..W11 block | historical / superseded | MASTER carries SK-V14 W0..W11 as authoritative receiver block (`restart/MASTER-PLAN.md:751-827`), but SK-V15 S-P3 locked a new W0..W11 contract (`restart/skinny/tranches/sk-v15/research/p3/hardening/HARDENING-S-P3-V4-CONSOLIDATED.md:35-49`). | New SK-V15 W0..W11 receiver block. |
| MP-NW-01..14 block | historical / superseded except refusal rows | MASTER's 14 NEW rows mirror older MP.NW/SK-V14 commitments (`restart/MASTER-PLAN.md:837-869`). SK-V15 now consumes the same failure classes through W0-W11 and explicit DEP rows (`restart/skinny/tranches/sk-v15/SPEC.md:187-205`). | Keep as history/pre-block; do not use as current dispatch manifest. |

## SK-V15 New Allocation Proposal

These are proposed MASTER receiver rows, not implementation dispatch. They are
copied from the locked SK-V15 SPEC/S-P3 plan, which requires research/plan/redress
caps and forbids W12 overflow (`restart/skinny/tranches/sk-v15/SPEC.md:155-170`;
`restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md:28-44`).

| new receiver | status | manual LOC / generated / docs | risk | MASTER alignment | consumer / gate | cap-fit and fail route | evidence |
|---|---|---:|---|---|---|---|---|
| MP.SK15.W0 Baseline and telemetry lock | new pending | 60-160 / none / 80-180 | medium | H/J/BENCH current-state ledger | Gate consumes SK-V15 telemetry; CSS broadcast non-admit | Fits SPEC cap; missing consumed telemetry blocks W1, records REDRESS/intrinsic block, no W12. | `restart/skinny/tranches/sk-v15/SPEC.md:172-185`; `restart/skinny/tranches/sk-v15/SPEC.md:246-262` |
| MP.SK15.W1 CSS admission honesty | new pending | 80-200 / none / 80-180 | medium | H.W6/J.W1 CSS truth repair | W1 gate demotes or collapses the 24 CSS broadcast admits | Fits SPEC cap; failure keeps CSS diagnostic and blocks W5/W6 admission routing, no W12. | `restart/skinny/tranches/sk-v15/SPEC.md:172-185`; `restart/skinny/tranches/sk-v15/SPEC.md:264-281` |
| MP.SK15.W2 Lock 14 / Lock 16 gate restoration | new pending | 120-280 / reports/fixtures / 80-180 | high | A.W4/H.W4.LOCK14/MP.NW6 | W2 gate consumes included roots, exclusions, and primitive source status | Fits SPEC cap only as gate/report work; source-present unwired primitives become manifest dispositions, no W12. | `restart/skinny/tranches/sk-v15/SPEC.md:172-185`; `restart/skinny/tranches/sk-v15/SPEC.md:283-299` |
| MP.SK15.W3 Codegen leak abrogation | new pending | 150-320 / regen/check evidence / 80-180 | high | F.W3/F.W5/MP.NW6 | Same-wave generator consumer removes one coherent generic leak family | Fits SPEC cap for one family; broader fanout records REDRESS or G-Omega wave-graph amendment before redress, no W12. | `restart/skinny/tranches/sk-v15/SPEC.md:172-185`; `restart/skinny/tranches/sk-v15/SPEC.md:300-316` |
| MP.SK15.W4 Pattern H generated discipline | new pending | gate-only 120-280 here; full split below | high | A/F Pattern H census | W4/W11 consume provenance, regen/check, projection, deletion, and transcript sub-rows | Cap fit depends on sub-row split; header-only or hidden projection close records intrinsic block/REDRESS, no W12. | `restart/skinny/tranches/sk-v15/SPEC.md:172-185`; `restart/skinny/tranches/sk-v15/SPEC.md:318-334`; `restart/audit/totality/p1/1D-skinny-lessons.md:174-180` |
| MP.SK15.W5 CSS typed Value provider | new pending | 300-900 scoped provider / 220-440 named generated provider / 80-180 | high | G.W2/G.W3/H.W6/J.W1 | W5 typed CSS value/document/view/visitor provider tests and generated consumer | Fits only as scoped typed provider; full CSSOM rewrite or lightningcss parity is intrinsic-block/G-Omega amendment work, no W12. | `restart/skinny/tranches/sk-v15/SPEC.md:172-185`; `restart/skinny/tranches/sk-v15/SPEC.md:336-355`; `restart/audit/totality/p1/1D-skinny-lessons.md:174-180` |
| MP.SK15.W6 CSS retime and old-proof retirement | new pending | 160-340 / reports/results / 100-220 | high | H.W6/J.W1/BENCH | W6 same-workload typed cssparser comparison and old-proof retirement scans | Fits SPEC cap as retime/retirement only; it does not implement broad CSSOM, and mismatch leaves CSS audit-demoted, no W12. | `restart/skinny/tranches/sk-v15/SPEC.md:172-185`; `restart/skinny/tranches/sk-v15/SPEC.md:357-376`; `restart/audit/totality/p1/1D-skinny-lessons.md:174-180` |
| MP.SK15.W7 Decision Engine spine | new pending | 900-1400 / selection fixtures / 80-180 | high | C.W4/C.W5/H.W4/MP.NW8 | `DEP-W7-DECISION-SPINE`: e-graph rewrite count, cost extraction, CSP deletion/alteration fixtures | Fits only if lowerer output is out of scope; fail action is intrinsic block, revert/REDRESS, or G-Omega wave-graph amendment before redress, no W12. | `restart/audit/totality/p2/2D-cost-model.md:70-76`; `restart/skinny/tranches/sk-v15/SPEC.md:378-392` |
| MP.SK15.W8 BackendShape harness plus EagerTape/OffsetTape | new pending | 700-1100 / golden fixtures / 80-180 | high | E/F/H lowerer boundary | `DEP-W8-LOWERERS-A`: old-scaffold failure fixtures plus runtime-relevant output/equality checks | Fits only if limited to EagerTape/OffsetTape with minimal shared helpers and EventTape deferred; fail action is intrinsic block or REDRESS, no W12. | `restart/audit/totality/p2/2D-cost-model.md:70-76`; `restart/skinny/tranches/sk-v15/SPEC.md:394-410` |
| MP.SK15.W9 EventTape/SinkOnly/CollapsedStage plus all-five gate | new pending | 850-1300 / golden/report fixtures / 100-220 | high | H.W4/H.W7/BackendShape canon | `DEP-W9-LOWERERS-B`: EventTape anti-sidecar, SinkOnly equality, CollapsedStage diagnostic/aarch64 gate, exact five-shape report | Fits only if CollapsedStage remains diagnostic unless 2E supplies aarch64 route; fail action is gate-consumed block/REDRESS, no W12. | `restart/audit/totality/p2/2D-cost-model.md:70-76`; `restart/skinny/tranches/sk-v15/SPEC.md:412-428` |
| MP.SK15.W10 FNV quarantine | new pending | 80-220 / 100-240 / 80-180 | medium | J.W1/J.W5/bench-only guard | W10 production FNV scan and adversarial strict-product fixtures | Fits SPEC cap; production FNV correctness migration is blocked or deleted, no W12. | `restart/skinny/tranches/sk-v15/SPEC.md:172-185`; `restart/skinny/tranches/sk-v15/SPEC.md:430-445` |
| MP.SK15.W11 Close and PASS-IMPL V2 handoff | new pending | 0-80 / none / 120-420 | medium | J.W5 / Master close | PASS-IMPL V2 accepts each axis or records row-level intrinsic block proof | Fits only as audit/handoff over prior evidence; orphan dependency rows block SK-V16 routing, no W12. | `restart/skinny/tranches/sk-v15/SPEC.md:172-185`; `restart/skinny/tranches/sk-v15/SPEC.md:447-465` |

### W4 Pattern H Budget Split

W4 is not a single 120-280 LOC implementation claim. That band is only the
MASTER gate-row proposal. Any accepted MASTER text must preserve these sub-rows
so provenance repair, runtime projection, destructive deletion, and transcript
work cannot hide inside one cap.

| W4 sub-row | bounded LOC | consumer / gate | fail action | cap-fit statement |
|---|---:|---|---|---|
| Provenance gate | 120-280 manual/docs | W4 gate consumes `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l`, line-1 provenance scan, and exclusion report. | Header-only provenance or changed 67-file census records REDRESS/intrinsic block. | Fits as gate/report work only; it does not claim generator repair by itself. |
| Generator/check proof | 1,500-3,000 generator/provenance repair | `DEP-W4-PATTERN-H-PROVENANCE` and W11 consume non-writing regen/check plus true generator source for all 67 root runtime files. | Missing regen/check proof blocks W4 close; do not delete or demote files to paper over provenance. | Fits only if W4 spends its redress budget on generator/provenance proof and excludes runtime projection. |
| Runtime projection | 700-1,200 per named projection | `DEP-W4-W6-CSS-LEGACY-RUNTIME-SHIM` or a named non-CSS receiver gate consumes the projected runtime diff and generated consumer. | If no same-wave consumer exists, record intrinsic block; do not infer projection from provenance. | Fits only one named projection at a time; multiple projections require G-Omega wave-graph amendment before redress. |
| Destructive deletion | 0-160 deletion diff plus proof output | Matching DEP row consumes same-wave replacement proof before deletion. | Delete-before-provider is reverted or REDRESSed; no deletion-only close. | Fits only when replacement proof has already landed or lands in the same wave; otherwise blocked, no W12. |
| Close transcript | 600-1,200 docs/evidence | W11/PASS-IMPL V2 consumes transcript tying provenance, regen/check, projection, deletion, and REDRESS/intrinsic-block outcomes. | Missing transcript leaves W4 unresolved at W11. | Fits as evidence collation over prior outputs; it cannot create implementation overflow or W12. |

## Proposed Delta Table

| proposed delta | source T-P1/T-P2 finding-id cited | affected V1-surface section | rationale, LOC/risk/wave alignment |
|---|---|---|---|
| MP-3B-V1-D01: Add an SK-V15 current-state authority note before the tranche census. | T-P1 COH-001 says top-level surfaces still route from SK-V14 while SK-V15 authority exists (`restart/audit/totality/p1/1F-coherence-scan.md:122`); S-P3 V4 is locked (`restart/skinny/tranches/sk-v15/research/p3/hardening/HARDENING-S-P3-V4-CONSOLIDATED.md:12-31`). | `restart/MASTER-PLAN.md` Section 5 and Section 25. | 60-120 doc LOC, high routing risk, Pass Omega CRUD only. Aligns all later rows to SK-V15 W0-W11 before V1 implementation. |
| MP-3B-V1-D02: Preserve A-J stub waves as pending, and label scoped H landings as scoped, partial, or refuted rather than V1/root close. | MASTER already says 59 stubs remain pending and scoped skinny landings are not V1/root close (`restart/MASTER-PLAN.md:204-223`); T-P1 says JSON is guard and CSS is demoted (`restart/audit/totality/p1/1D-skinny-lessons.md:77-95`). | Section 5 tranche set and Section 13 H ledger. | 80-160 doc LOC, medium regression risk, no implementation wave. Prevents old scoped achievements from becoming paper close. |
| MP-3B-V1-D03: Add a new SK-V15 W0-W11 MASTER receiver block with LOC/risk/consumer columns. | SK-V15 SPEC W0-W11 manifest carries risk, LOC, generated, docs, entry, and exit gates (`restart/skinny/tranches/sk-v15/SPEC.md:172-185`); P3-B says W0-W11 consumes the 12-wave ceiling (`restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md:15-33`). | New Section 13.x after current Section 13.4. | 220-420 doc LOC, high routing risk, new pending waves MP.SK15.W0..W11. Same-wave consumers are required for every row. |
| MP-3B-V1-D04: Reclassify Section 13.3 SK-V14 W0..W11 and Section 13.4 MP-NW-01..14 as historical/pre-block evidence, not current dispatch. | MASTER currently carries SK-V14 W0..W11 and old MP-NW rows (`restart/MASTER-PLAN.md:751-869`); SK-V15 SPEC says S-P3 produces the active contract and W0 dispatches after required authorization (`restart/skinny/tranches/sk-v15/SPEC.md:486-495`). | Sections 13.3 and 13.4. | 120-240 doc LOC, high CH3 regression risk, aligned to MP.SK15.W0..W11. Preserve REDRESS/pre-block value without stale dispatch. |
| MP-3B-V1-D05: Demote current CSS L4 row-admit language and route CSS through W1/W5/W6. | PASS-IMPL says all 24 CSS admits are one broadcast, workload-mismatched, and generated from `CSS_GENERATED_RS` (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21-31`); T-P2 2A refutes current CSS admission and lightningcss close (`restart/audit/totality/p2/2A-sota-landscape.md:59-63`). | Sections 13.2 MP.NW2..4, H.W6, J.W1. | W1 80-200 manual LOC, W5 300-900 scoped typed provider plus generated 220-440, W6 160-340 retime/retirement, critical/high risk. W5/W6 do not hide a broad CSSOM rewrite; if CSSOM/value parity is required for close, route intrinsic block or G-Omega wave-graph amendment. |
| MP-3B-V1-D06: Add anti-broadcast and gate-exclusion telemetry as MASTER receiver obligations. | SK-V15 adds ten fields and gate consumption (`restart/skinny/tranches/sk-v15/SPEC.md:100-122`); P3-D defines duplicate-measurement and exclusion rejection rules (`restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md:46-69`). | Sections 13.2 MP.NW1, 23 Risk Register, 24 Carry Ledger. | W0/W1/W2; 80-220 gate/doc LOC, high risk. Prevents producer-only telemetry and self-exempting gates. |
| MP-3B-V1-D07: Split Lock 14/codegen/Pattern H repair into W2/W3/W4 and reject delete-before-provider or header-only close. | T-P1 COH-005/006/009 classify Lock 14 holes, 67/0 Pattern H provenance, and CSS generated facade (`restart/audit/totality/p1/1F-coherence-scan.md:126-130`); T-P2 2C gives Pattern H and full-surface Lock 14 close gates (`restart/audit/totality/p2/2C-grammar-neutrality.md:147-148`). | Sections 6, 11, 13.2 MP.NW6, 24 Carry Ledger. | W2 120-280 and W3 150-320 stay gate/codegen slices; W4 splits into 120-280 provenance gate, 1,500-3,000 generator/check proof, 700-1,200 per named projection, 0-160 destructive deletion diff, and 600-1,200 close transcript. Deletion only after same-wave provider proof; no W12 overflow. |
| MP-3B-V1-D08: Replace Decision Engine scaffold language with W7/W8/W9 split and all-five BackendShape gate. | PASS-IMPL says Decision Engine is scaffold and 4/5 lowerers are stubs (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:49-55`); T-P2 2D costs W7/W8/W9 and preserves exactly five shapes (`restart/audit/totality/p2/2D-cost-model.md:70-76`). | Sections 8, 13 H.W4/H.W7, 13.2 MP.NW8. | W7 900-1400, W8 700-1100, W9 850-1300 manual LOC, high risk. W7 excludes lowerers, W8 excludes EventTape, and W9 keeps CollapsedStage diagnostic unless aarch64 proof exists; failures become intrinsic block/REDRESS/G-Omega amendment, not W12. |
| MP-3B-V1-D09: Reframe H.W2/H.W2.5 primitive vocabulary as W2 primitive manifest plus later selected consumers. | T-P2 2B requires scalar oracle, strict differential, aarch64 hardware gate, same-wave consumer, row movement, LOC/risk/wave owner (`restart/audit/totality/p2/2B-primitive-vocabulary.md:101-115`); source-present unwired is refuted as close (`restart/audit/totality/p2/2B-primitive-vocabulary.md:176-185`). | Sections 13 H.W2/H.W2.5, Lock 16 allowlist references in Section 13.1. | W2 120-280 gate LOC; future primitive slices 80-350 each only with consumer. High risk if source inventory is admitted. |
| MP-3B-V1-D10: Add W10 FNV quarantine row and bench-only guard to MASTER. | PASS-IMPL flags W11L/W11N/W11O FNV closed-enum products as bench-only (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:60-65`); T-P1 COH-016 routes generated CSS FNV hashes to W10 quarantine (`restart/audit/totality/p1/1F-coherence-scan.md:154-177`). | Sections 13, 23, 24, J.W1/J.W5. | W10 80-220 manual plus 100-240 bench fixture/report LOC, medium risk. No production FNV arbiter or correctness proof. |
| MP-3B-V1-D11: Update implementation order from stale SK-V13/SK-V14 language to SK-V15 Pass Omega/G-Omega then W0. | MASTER currently says G-Omega before SK-V13 W0 and blocks SK-V13 waves (`restart/MASTER-PLAN.md:1222-1242`); T-P3 dispatch says synthesis proposes and Pass Omega applies spec surfaces, with G-Omega mandatory (`restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:27-40`). | Section 25 Implementation Order and Section 24 Carry Ledger. | 40-100 doc LOC, medium governance risk, no implementation wave. Aligns with T-P3 proposal-only boundary. |

## CH4 Coverage Matrix

| delta | LOC | propagation count | risk | wave alignment | consumer / gate | hard-cap fit | fail action |
|---|---:|---:|---|---|---|---|---|
| D01 | 60-120 doc | 3 | high routing | Pass Omega/G-Omega before W0 | Pass Omega CRUD and G-Omega acceptance gate | Fits as doc-only authority repair; no implementation cap claim. | Reject CRUD or record REDRESS; block SK-V15 W0 dispatch until authority is current. |
| D02 | 80-160 doc | 2 | medium regression | No implementation wave; H/J close evidence only | Pass Omega CRUD plus J close-report reference gate | Fits as classification text only; no scoped landing becomes V1/root close. | Reject overclaim, keep rows scoped/partial/refuted, and block paper close. |
| D03 | 220-420 doc | 4 | high routing | MP.SK15.W0..W11 | Pass Omega CRUD plus each MP.SK15 same-wave gate | Fits as receiver-map documentation; implementation cap remains per W0-W11 row. | Missing row fields block CRUD; non-fit wave work routes intrinsic block, REDRESS/revert, or G-Omega amendment, no W12. |
| D04 | 120-240 doc | 2 | high CH3 regression | MP.SK15.W0..W11 supersedes SK-V14/MP-NW history | Pass Omega CRUD and W0 dispatch-authority gate | Fits as historical/pre-block marking only; old rows gain no dispatch cap. | Reject live reuse of stale rows; keep pre-block evidence or route REDRESS before W0. |
| D05 | W1 80-200; W5 300-900 plus 220-440 generated; W6 160-340 | 5 | critical/high | W1/W5/W6 | W1 CSS demotion, W5 typed provider tests/generated consumer, W6 retime/retirement gate | Fits only as scoped typed-provider and retime work; broad CSSOM/value parity is non-fit. | Record intrinsic block or G-Omega wave-graph amendment before redress; no hidden W5/W6 overflow and no W12. |
| D06 | 80-220 gate/doc | 4 | high | W0/W1/W2 | W0 telemetry lock plus W1/W2 anti-broadcast and exclusion gates | Fits as consumed telemetry/gate schema work only. | Reject self-exempting or producer-only gates; block dependent W1/W2 rows or REDRESS/revert, no W12. |
| D07 | W2 120-280; W3 150-320; W4 120-280 gate, 1,500-3,000 generator, 700-1,200/projection, 0-160 deletion, 600-1,200 transcript | 4 | high | W2/W3/W4 | W2 Lock gate, W3 codegen consumer, W4 provenance/generator/projection/deletion/transcript gates | Fits only through the preserved W4 sub-row split; delete-before-provider and header-only close are non-fit. | Revert or REDRESS delete-before-provider; otherwise intrinsic block or G-Omega amendment, no W12. |
| D08 | W7 900-1400; W8 700-1100; W9 850-1300 | 3 | high | W7/W8/W9 | `DEP-W7-DECISION-SPINE`, `DEP-W8-LOWERERS-A`, and `DEP-W9-LOWERERS-B` gates | Fits only with W7 excluding lowerers, W8 excluding EventTape, and W9 preserving the all-five gate. | Intrinsic block, REDRESS/revert, or G-Omega wave-graph amendment before redress; no W12. |
| D09 | W2 120-280 plus 80-350 per selected future primitive | 3 | high | W2 then selected consumer waves | W2 primitive manifest and future scalar/checkasm/same-wave consumer gates | Fits as manifest plus selected consumed slices only; source-present inventory is non-fit. | Block/demote unwired primitives, require row movement or REDRESS, no W12. |
| D10 | W10 80-220 plus 100-240 fixture/report | 3 | medium | W10 | W10 production FNV scan and adversarial strict-product fixture gate | Fits as bench quarantine and scan/report work only; production FNV correctness migration is non-fit. | Delete/block production FNV migration or REDRESS quarantine evidence, no W12. |
| D11 | 40-100 doc | 3 | medium governance | Pass Omega/G-Omega then W0 | Pass Omega CRUD, G-Omega authorization, and W0 entry gate | Fits as implementation-order governance text only. | Block SK-V15 W0 until order is accepted; route unresolved remainder to REDRESS or intrinsic block, no W12. |

## Open Questions

| lens | question | receiver | blocker | gate |
|---|---|---|---|---|
| CH1 CORRECTNESS | Should Pass Omega replace Sections 13.3/13.4 text in place or add a new Section 13.5 SK-V15 block while marking old blocks historical? | Pass Omega CRUD owner for `restart/MASTER-PLAN.md`. | 3A/3F cross-surface wording and line-level edit shape are not selected. | Pass Omega CRUD acceptance before G-Omega authorization. |
| CH3 REGRESSION | Should W1 collapse the 24 CSS rows to one diagnostic aggregate, or keep 24 rows with explicit non-admission broadcast metadata? | SK-V15 W1 CSS admission honesty row. | W0 telemetry carrier must exist before deciding aggregate-vs-row metadata shape. | W1 exit gate over broadcast demotion/collapse and consumed telemetry fields. |
| CH4 COST | If W5, W7, W8, or W9 exceeds the SK-V15 hard cap, is the correct route intrinsic block or a G-Omega wave-graph amendment? | SK-V15 wave governance plus the affected wave owner. | Cap evidence from W0/W1 telemetry and the scoped W5/W7/W8/W9 plans; W12 is unavailable. | Plan/redress cap gate: record row-level intrinsic block, REDRESS/revert, or G-Omega wave-graph amendment before redress. |
| CH5 HIDDEN COUPLING | Which exact Lock 1/14 text from 3C will classify EventTape/FNV/source-sidecar surfaces in MASTER cross-references? | 3C LOCKS crystallisation and Pass Omega LOCKS CRUD owner. | Accepted v+1 Lock text is not yet selected. | LOCKS v+1 acceptance gate plus MASTER cross-reference CRUD gate. |
| CH6 ANTI-PAPER-CLOSE | What exact PASS-IMPL V2 acceptance fields should W11 require before routing SK-V16? | W11/PASS-IMPL V2 owner with 3F/HANDOFF alignment. | Pass Omega/HANDOFF wording from 3F is not yet accepted. | W11 PASS-IMPL V2 gate requiring accept, REDRESS/revert, or row-level intrinsic-block proof for every axis. |
