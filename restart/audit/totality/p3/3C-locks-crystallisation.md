---
agent: 3C
pass: T-P3-synthesis
cycle: V2
generated_at: 2026-05-28T07:50:49Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
t_p2_dossiers_consumed: [2A, 2B, 2C, 2D, 2E, 2F]
v1_surface_targeted: LOCKS.md
proposed_deltas_count: 12
delta_summary:
  carried_from_prior_cycle: [SK-V15-T-P3-3C-V1-addendum]
  removed: []
  answered: [LAC-1E-V1-01, LAC-1E-V1-02, LAC-1E-V1-03, LAC-1E-V1-04, LAC-1E-V1-05, LAC-1E-V1-06, LAC-1E-V1-07, LAC-1E-V1-08, LAC-1E-V1-09, LAC-1E-V1-10, LAC-1E-V1-11, LAC-1E-V1-12, LAC-1E-V1-13, LAC-1E-V1-14, LAC-1E-V2-15, T2A-V1-LAC-01, T2A-V1-LAC-02, T2A-V1-LAC-03, T2A-V1-LAC-04, T2A-V1-LAC-05, LAC-2B-V2-01, LAC-2B-V2-02, LAC-2B-V2-03, LAC-2B-V2-04, LAC-2C-SK15-01, LAC-2C-SK15-02, LAC-2C-SK15-03, LAC-2C-SK15-04, LAC-2C-SK15-05, LAC-2C-SK15-06, LAC-2D-01, LAC-2D-02, LAC-2D-03, LAC-2D-04, LAC-2D-05, LOCK16-A64-HOST-GATE, LOCK16-PMU-ROW-LOCAL, LOCK16-SVE2-SEPARATION, LAC-2F-V1-01, LAC-2F-V1-02, LAC-2F-V1-03, LAC-2F-V1-04]
  newly_added: [D-L01-substrate-factstream-sidecar, D-L02-layout-live-state, D-L03-empty-path-proof, D-L04-solver-bridge, D-L06-regeneration-delete-provider, D-L08-row-plane-broadcast, D-L09-borrow-surface, D-L10-decision-five-shape, D-L11-L12-topology-archive, D-L14-generated-provider-generalisation, D-L15-profile-scope, D-L16-primitive-manifest]
prior_cycle_dispositions_folded:
  accepted: [T-P1-V5-clean-final-G1-auto-pinned, T-P2-V3-normal-3Z-lock]
  rejected: []
  revised:
    - "CH1-V1-001: proposed LOCKS hunk context regenerated without the inherited out-of-range 2F line citation."
    - "CH4-COST-03: every D-L* clause now carries doc LOC, risk, affected waves, consumer/gate, and propagation count."
    - "CH5-V1-01: regex ownership now uses canonical parse-that-regex wording; legacy bbnf-regex is path-only and non-admissible as a future owner."
    - "CH5-V1-02: runtime regex/DFA manifest and consumer proof are necessary but never sufficient without prior G-Omega Lock 1 amendment."
    - "CH6-V1-01: Open Questions now route receiver, blocker, and gate."
---

# 3C LOCKS Crystallisation

## Executive Summary

This packet disposes every live 1E and 2A-2F `LOCKS-AMENDMENTS-CANDIDATE`: 42 rows total, with 23 `ACCEPT`, 19 `MODIFY`, 0 `REJECT`, and 0 `DEFER`. The proposed `LOCKS.md` delta is an addendum, not an edit to the live governance file. It preserves the 16 numbered locks, preserves the five `BackendShape` variants, and creates no new directive, BIR variant, substrate, public substrate API, retained sidecar, or sixth shape. T-P1 is carried honestly as clean-final / G1-auto-pinned rather than a normal two-clean-cycle lock (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:21`-`28`). T-P2 is carried as a normal Section 3Z lock (`restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:15`-`19`). The line-level proposed diff is emitted separately in `3C-locks-v+1-diff.md`.

## V2 Delta Summary

| class | count | notes |
|---|---:|---|
| carried | 1 | V1 proposed addendum carried forward with CH1/CH4/CH5/CH6 repairs folded. |
| removed | 0 | No live LAC is silently dropped. |
| answered | 42 | Every 1E and 2A-2F LAC receives a disposition below. |
| newly added | 12 | The 42 LACs are folded into 12 lock-addendum clauses. |

## Proposed Delta Table

| proposed delta | source finding ids | affected LOCKS section | rationale |
|---|---|---|---|
| D-L01-substrate-factstream-sidecar | LAC-1E-V1-01, T2A-V1-LAC-04, LAC-2C-SK15-02, LAC-2F-V1-02 | Lock 1 | Fact streams are output products, not `BackendShape` variants or sidecars; retained cursor/list/class-column/sidecar and runtime-regex substrate routes stay blocked. Runtime regex/DFA manifest plus consumer proof is necessary but never sufficient without prior G-Omega amendment to Lock 1 (`restart/audit/totality/p1/1E-locks-evidence.md:130`, `restart/audit/totality/p2/2A-sota-landscape.md:110`, `restart/audit/totality/p2/2F-parse-that-gaps.md:120`). |
| D-L02-layout-live-state | LAC-1E-V1-02 | Lock 2 | `LayoutFacts.backend_shape` is live, but Lock 2 closure cannot cite absent `Layout` / `LayoutSink` names (`restart/audit/totality/p1/1E-locks-evidence.md:131`). |
| D-L03-empty-path-proof | LAC-1E-V1-03 | Lock 3 | Empty-path cursor elision requires generated-code proof, not a missing-symbol claim (`restart/audit/totality/p1/1E-locks-evidence.md:132`). |
| D-L04-solver-bridge | LAC-1E-V1-04 | Lock 4 / Lock 6 | The egraph/CSP dependency state must be split or accepted explicitly; it cannot be closure evidence by silence (`restart/audit/totality/p1/1E-locks-evidence.md:133`). |
| D-L06-regeneration-delete-provider | LAC-1E-V1-05, LAC-1E-V2-15, LAC-2C-SK15-04 | Lock 6 / Lock 14 | Generated headers, byte-equivalent regen, and provider-before-delete proof are mandatory; header-only or delete-before-provider paths stay blocked (`restart/audit/totality/p1/1E-locks-evidence.md:134`, `restart/audit/totality/p1/1E-locks-evidence.md:144`, `restart/audit/totality/p2/2C-grammar-neutrality.md:147`). |
| D-L08-row-plane-broadcast | LAC-1E-V1-07, LAC-1E-V1-08, T2A-V1-LAC-01, T2A-V1-LAC-02, T2A-V1-LAC-03, LAC-2C-SK15-03, LAC-2F-V1-04 | Lock 8 / Lock 14 | Workload planes, `measurement_row_id`, `broadcast_group_id`, same-workload CSS comparators, and typed CSS provider proof are mandatory before CSS admission (`restart/audit/totality/p2/2A-sota-landscape.md:107`-`110`, `restart/audit/totality/p2/2C-grammar-neutrality.md:146`, `restart/audit/totality/p2/2F-parse-that-gaps.md:122`). |
| D-L09-borrow-surface | LAC-1E-V1-09 | Lock 9 | Borrow/Cow is partial; `parse_in` and `parse_owned` remain open obligations until generated tests prove them (`restart/audit/totality/p1/1E-locks-evidence.md:138`). |
| D-L10-decision-five-shape | LAC-1E-V1-10, LAC-2D-01, LAC-2D-02, LAC-2D-03, LAC-2D-04, LAC-2D-05 | Lock 10 / Lock 16 | Decision Engine close requires nonzero e-graph rewrite, measurement-bearing costs, non-tautological CSP, real lowerer output, exact five-shape gate, and aarch64-only CollapsedStage admission (`restart/audit/totality/p2/2D-cost-model.md:114`-`118`). |
| D-L11-L12-topology-archive | LAC-1E-V1-06 | Lock 7 / Lock 11 / Lock 12 | Root topology and archive closure must be split from skinny claims and proven by workspace/archive evidence (`restart/audit/totality/p1/1E-locks-evidence.md:135`). |
| D-L14-generated-provider-generalisation | LAC-1E-V1-11, LAC-1E-V1-12, LAC-2C-SK15-01, LAC-2C-SK15-05, LAC-2C-SK15-06, LAC-2F-V1-03 | Lock 14 / Lock 16 | Generic code consumes generated manifests/facts only; scans report included/excluded roots; future grammar onboarding is grammar-source/metadata only; CSS semantics cannot be JSON string/number APIs (`restart/audit/totality/p2/2C-grammar-neutrality.md:144`, `restart/audit/totality/p2/2C-grammar-neutrality.md:148`-`149`, `restart/audit/totality/p2/2F-parse-that-gaps.md:121`). |
| D-L15-profile-scope | LAC-1E-V1-13 | Lock 15 | Skinny release-profile compliance is not root workspace compliance, and `target-cpu=native` rows remain host-bound (`restart/audit/totality/p1/1E-locks-evidence.md:142`). |
| D-L16-primitive-manifest | LAC-1E-V1-14, T2A-V1-LAC-05, LAC-2B-V2-01, LAC-2B-V2-02, LAC-2B-V2-03, LAC-2B-V2-04, LOCK16-A64-HOST-GATE, LOCK16-PMU-ROW-LOCAL, LOCK16-SVE2-SEPARATION, LAC-2F-V1-01 | Lock 16 | Primitive admission requires owner, scalar oracle, strict parity/checkasm, aarch64 hardware gate, same-wave consumer, row movement, status, fallback state, emitted-asm proof when relevant, PMU row-locality, and SVE2/NEON separation. Canonical regex ownership is `parse-that-regex`; legacy `skinny/crates/bbnf-regex` is a non-admissible path alias until Lock 11 rename cleanup, not a peer owner. Runtime regex/DFA manifest and consumer proof are necessary but never sufficient without prior G-Omega amendment to Lock 1 (`restart/audit/totality/p2/2B-primitive-vocabulary.md:201`-`204`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:139`-`141`, `restart/audit/totality/p2/2F-parse-that-gaps.md:119`). |

## Per-Clause Cost Matrix

This matrix costs the proposed `LOCKS.md` addendum text and its governance propagation only. It authorizes no implementation work, and any wave that cannot consume a clause inside its cap must record intrinsic-block, REDRESS, or G-Omega wave-graph amendment rather than hiding overflow.

| proposed delta | doc LOC | risk | affected waves | consumer/gate | propagation count |
|---|---:|---|---|---|---:|
| D-L01-substrate-factstream-sidecar | 4-6 | high | W0/W1/W5/W7/W11 | Lock 1 substrate manifest plus runtime-regex G-Omega gate | 5 |
| D-L02-layout-live-state | 2-3 | medium | W7/W8/W9/W11 | Lock 2 closure review over `Layout` / `LayoutSink` evidence | 3 |
| D-L03-empty-path-proof | 2-3 | medium | W3/W11 | Generated-code empty-path unit/golden gate | 3 |
| D-L04-solver-bridge | 2-4 | medium | W7/W11 | egraph/CSP dependency split-or-exception gate | 3 |
| D-L06-regeneration-delete-provider | 4-6 | high | W3/W4/W5/W6/W11 | generator provenance, non-writing regen/check, provider-before-delete gates | 5 |
| D-L08-row-plane-broadcast | 5-8 | high | W0/W1/W5/W6/W11 | telemetry duplicate-signature, CSS typed provider, and same-workload retime gates | 6 |
| D-L09-borrow-surface | 2-3 | medium | W8/W11 | generated API tests for `parse_in` / `parse_owned` lifetime discipline | 3 |
| D-L10-decision-five-shape | 5-8 | high | W7/W8/W9/W11 | Decision Engine, lowerer output, and exact all-five `BackendShape` gates | 5 |
| D-L11-L12-topology-archive | 3-5 | medium | Pass Omega CRUD/W11 | root topology, legacy-name routing, and archive-inventory gate | 4 |
| D-L14-generated-provider-generalisation | 5-8 | high | W2/W3/W5/W6/W7/W11 | Lock 14 scan, generated provider manifest, CSS typed semantic gate | 6 |
| D-L15-profile-scope | 2-3 | medium | W0/W11 | root profile and host-bound row admission gate | 3 |
| D-L16-primitive-manifest | 6-10 | high | W2/W5/W7/W8/W9/W11 | Lock 16 manifest, strict parity/checkasm, aarch64, same-wave consumer gate | 6 |

## Disposition Matrix

| candidate | source | target | disposition | delta | evidence | rationale |
|---|---|---|---|---|---|---|
| LAC-1E-V1-01 | 1E | L01/L08/L14 | MODIFY | D-L01, D-L08 | `restart/audit/totality/p1/1E-locks-evidence.md:130`; CSS value gap at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:56`-`58` | Fold into typed fact-stream and CSS provider clauses; keep `FactStream` out of `BackendShape`. |
| LAC-1E-V1-02 | 1E | L02 | MODIFY | D-L02 | `restart/audit/totality/p1/1E-locks-evidence.md:131`; live side-table note at `restart/audit/totality/p1/1E-locks-evidence.md:91` | Do not narrow Lock 2 automatically; state closure needs either live `Layout` / `LayoutSink` or Pass Omega removal. |
| LAC-1E-V1-03 | 1E | L03 | ACCEPT | D-L03 | `restart/audit/totality/p1/1E-locks-evidence.md:132`; divergence at `restart/audit/totality/p1/1E-locks-evidence.md:122` | Add proof gate for empty-path cursor elision. |
| LAC-1E-V1-04 | 1E | L04/L06 | MODIFY | D-L04 | `restart/audit/totality/p1/1E-locks-evidence.md:133`; divergence at `restart/audit/totality/p1/1E-locks-evidence.md:120` | Do not choose the implementation route in locks; require explicit split-or-exception evidence. |
| LAC-1E-V1-05 | 1E | L06/L14 | MODIFY | D-L06 | `restart/audit/totality/p1/1E-locks-evidence.md:134`; Pattern H floor at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:37`-`43` | Merge with Pattern H provenance and regen discipline. |
| LAC-1E-V1-06 | 1E | L07/L11/L12 | MODIFY | D-L11-L12 | `restart/audit/totality/p1/1E-locks-evidence.md:135`; drift table at `restart/audit/totality/p1/1E-locks-evidence.md:96`-`101` | Split root topology/archive closure from skinny claims; no rename or archive close by prose. |
| LAC-1E-V1-07 | 1E | L08 | ACCEPT | D-L08 | `restart/audit/totality/p1/1E-locks-evidence.md:136`; broadcast blocker at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`-`29` | Add broadcast-admission detection. |
| LAC-1E-V1-08 | 1E | L08 | ACCEPT | D-L08 | `restart/audit/totality/p1/1E-locks-evidence.md:137`; comparator mismatch at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:29` | Add same-workload comparator-plane requirement. |
| LAC-1E-V1-09 | 1E | L09 | ACCEPT | D-L09 | `restart/audit/totality/p1/1E-locks-evidence.md:138`; drift table at `restart/audit/totality/p1/1E-locks-evidence.md:98` | Preserve partial borrow/Cow evidence and keep bump/owned open. |
| LAC-1E-V1-10 | 1E | L10 | MODIFY | D-L10 | `restart/audit/totality/p1/1E-locks-evidence.md:139`; scaffold finding at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:49`-`54` | Merge into Decision Engine / all-five evidence clause. |
| LAC-1E-V1-11 | 1E | L14/L16 | ACCEPT | D-L14, D-L16 | `restart/audit/totality/p1/1E-locks-evidence.md:140`; gate holes at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:45`-`47` | Gate exclusions must self-report and fail when leak roots are hidden. |
| LAC-1E-V1-12 | 1E | L13/L14 | MODIFY | D-L14 | `restart/audit/totality/p1/1E-locks-evidence.md:141`; Pattern H invariant at `restart/audit/totality/p1/1E-locks-evidence.md:84` | Treat Pattern H 67 as current-count invariant plus provenance proof, not success. |
| LAC-1E-V1-13 | 1E | L15 | ACCEPT | D-L15 | `restart/audit/totality/p1/1E-locks-evidence.md:142`; profile drift at `restart/audit/totality/p1/1E-locks-evidence.md:104` | Split skinny/root profile compliance and host-bound admission. |
| LAC-1E-V1-14 | 1E | L16 | ACCEPT | D-L16 | `restart/audit/totality/p1/1E-locks-evidence.md:143`; strict default caveat at `restart/audit/totality/p1/1E-locks-evidence.md:105` | Add primitive traceability manifest and strict evidence requirement. |
| LAC-1E-V2-15 | 1E | L08/L14 | ACCEPT | D-L06, D-L08 | `restart/audit/totality/p1/1E-locks-evidence.md:144`; dependency table at `restart/skinny/tranches/sk-v15/SPEC.md:192`-`204` | Add delete/rebuild dependency proof. |
| T2A-V1-LAC-01 | 2A | L08 | ACCEPT | D-L08 | `restart/audit/totality/p2/2A-sota-landscape.md:107`; comparator classes at `restart/skinny/tranches/sk-v15/SPEC.md:86`-`92` | Add workload-plane gate. |
| T2A-V1-LAC-02 | 2A | L08 | MODIFY | D-L08 | `restart/audit/totality/p2/2A-sota-landscape.md:108`; telemetry fields at `restart/skinny/tranches/sk-v15/SPEC.md:100`-`122` | Fold with 1E and 2C broadcast duplicate clauses. |
| T2A-V1-LAC-03 | 2A | L14 | MODIFY | D-L08, D-L14 | `restart/audit/totality/p2/2A-sota-landscape.md:109`; CSS close floor at `restart/skinny/tranches/sk-v15/SPEC.md:54`-`63` | Fold with CSS typed provider boundary and delete-before-provider guard. |
| T2A-V1-LAC-04 | 2A | L01 | ACCEPT | D-L01 | `restart/audit/totality/p2/2A-sota-landscape.md:110`; V2 fold requirement at `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:74`-`79` | Retained sidecar-like routes remain pre-blocked. |
| T2A-V1-LAC-05 | 2A | L16 | ACCEPT | D-L16 | `restart/audit/totality/p2/2A-sota-landscape.md:111`; scalar-first rule at `restart/skinny/tranches/sk-v15/SPEC.md:143`-`145` | SIMD/ASM cannot admit from citation alone. |
| LAC-2B-V2-01 | 2B | L16 | ACCEPT | D-L16 | `restart/audit/totality/p2/2B-primitive-vocabulary.md:201`; manifest cells at `restart/audit/totality/p2/2B-primitive-vocabulary.md:101`-`115` | Add mandatory primitive manifest schema. |
| LAC-2B-V2-02 | 2B | L16 | ACCEPT | D-L16 | `restart/audit/totality/p2/2B-primitive-vocabulary.md:202`; scalar delegate rows at `restart/audit/totality/p2/2B-primitive-vocabulary.md:146`-`151` | Define `scalar-delegated` as fallback status, not SIMD movement. |
| LAC-2B-V2-03 | 2B | L14/L16 | ACCEPT | D-L16 | `restart/audit/totality/p2/2B-primitive-vocabulary.md:203`; source inventory refutation at `restart/audit/totality/p2/2B-primitive-vocabulary.md:180` | Source inventory is not admission; require delete/block/rebuild. |
| LAC-2B-V2-04 | 2B | telemetry | ACCEPT | D-L16, D-L08 | `restart/audit/totality/p2/2B-primitive-vocabulary.md:204`; telemetry fields at `restart/skinny/tranches/sk-v15/SPEC.md:106`-`117` | Primitive-influenced rows need `lock16_status` and parity status consumed by the gate. |
| LAC-2C-SK15-01 | 2C | L14/L06 | MODIFY | D-L14 | `restart/audit/totality/p2/2C-grammar-neutrality.md:144`; grammar switch refutation at `restart/audit/totality/p2/2C-grammar-neutrality.md:70` | Fold into generated provider manifest discipline. |
| LAC-2C-SK15-02 | 2C | L01/L08/L14 | MODIFY | D-L01, D-L08 | `restart/audit/totality/p2/2C-grammar-neutrality.md:145`; typed provider target at `restart/audit/totality/p2/2C-grammar-neutrality.md:64` | Fold into CSS typed close boundary and fact-stream diagnostic rule. |
| LAC-2C-SK15-03 | 2C | L08/L14 | MODIFY | D-L08 | `restart/audit/totality/p2/2C-grammar-neutrality.md:146`; broadcast refutation at `restart/audit/totality/p2/2C-grammar-neutrality.md:68` | Duplicate of 1E/2A broadcast clauses; fold into one gate. |
| LAC-2C-SK15-04 | 2C | L06/L14 | MODIFY | D-L06 | `restart/audit/totality/p2/2C-grammar-neutrality.md:147`; Pattern H assertion at `restart/audit/totality/p2/2C-grammar-neutrality.md:95`-`101` | Fold into Pattern H provenance before deletion. |
| LAC-2C-SK15-05 | 2C | L14/L16 | MODIFY | D-L14 | `restart/audit/totality/p2/2C-grammar-neutrality.md:148`; gate-exclusion row at `restart/audit/totality/p2/2C-grammar-neutrality.md:74` | Fold with 1E exclusion-report clause. |
| LAC-2C-SK15-06 | 2C | L14/L16 | MODIFY | D-L14 | `restart/audit/totality/p2/2C-grammar-neutrality.md:149`; no new directive/BIR/sixth-shape guard at `restart/audit/totality/p2/2C-grammar-neutrality.md:52`-`54` | Future grammar onboarding is source/metadata only; no new surface. |
| LAC-2D-01 | 2D | L10 | MODIFY | D-L10 | `restart/audit/totality/p2/2D-cost-model.md:114`; zero-rule refutation at `restart/audit/totality/p2/2D-cost-model.md:93` | Fold into active Decision Engine definition. |
| LAC-2D-02 | 2D | L10 | MODIFY | D-L10 | `restart/audit/totality/p2/2D-cost-model.md:115`; all-zero candidate costs at `restart/audit/totality/p2/2D-cost-model.md:96` | Fold into measurement-bearing cost evidence rule. |
| LAC-2D-03 | 2D | L10 | MODIFY | D-L10 | `restart/audit/totality/p2/2D-cost-model.md:116`; current CSP refutation at `restart/audit/totality/p2/2D-cost-model.md:94` | Fold into non-tautological CSP fixture requirement. |
| LAC-2D-04 | 2D | L10 | ACCEPT | D-L10 | `restart/audit/totality/p2/2D-cost-model.md:117`; enum proof at `skinny/crates/ir/src/lib.rs:339`-`345` and helper at `skinny/crates/ir/src/cost.rs:333`-`339` | Preserve exactly five `BackendShape` variants and all-five gate. |
| LAC-2D-05 | 2D | L16/L10 | MODIFY | D-L10, D-L16 | `restart/audit/totality/p2/2D-cost-model.md:118`; wrong-host refutation at `restart/audit/totality/p2/2D-cost-model.md:97` | Fold into CollapsedStage aarch64 admission and diagnostic x86 rule. |
| LOCK16-A64-HOST-GATE | 2E | L16 | ACCEPT | D-L16 | `restart/audit/totality/p2/2E-host-arch-esoterica.md:139`; host close route at `restart/audit/totality/p2/2E-host-arch-esoterica.md:25`-`32` | Add aarch64 primitive status and emitted-asm proof requirement. |
| LOCK16-PMU-ROW-LOCAL | 2E | L16/L08 | ACCEPT | D-L16, D-L08 | `restart/audit/totality/p2/2E-host-arch-esoterica.md:140`; PMU row at `restart/audit/totality/p2/2E-host-arch-esoterica.md:80` | PMU only supports row-local equality/timing, never broadcast laundering. |
| LOCK16-SVE2-SEPARATION | 2E | L16 | ACCEPT | D-L16 | `restart/audit/totality/p2/2E-host-arch-esoterica.md:141`; SVE2 refutation at `restart/audit/totality/p2/2E-host-arch-esoterica.md:82` | Separate SVE/SVE2 from NEON/AdvSIMD. |
| LAC-2F-V1-01 | 2F | L16 | ACCEPT | D-L16 | `restart/audit/totality/p2/2F-parse-that-gaps.md:119`; crate ownership floor at `restart/audit/totality/p2/2F-parse-that-gaps.md:27`-`40` | Add parse-that-family owner taxonomy using canonical `parse-that-regex`; legacy `skinny/crates/bbnf-regex` remains path-only and is not a future owner. |
| LAC-2F-V1-02 | 2F | L16/L01 | ACCEPT | D-L01, D-L16 | `restart/audit/totality/p2/2F-parse-that-gaps.md:120`; runtime import refutation at `restart/audit/totality/p2/2F-parse-that-gaps.md:98`-`100` | Runtime regex/DFA engines remain blocked unless G-Omega first amends Lock 1; manifest, consumer proof, and CH3/CH5 review are necessary but not sufficient. |
| LAC-2F-V1-03 | 2F | L14/L16 | ACCEPT | D-L14, D-L16 | `restart/audit/totality/p2/2F-parse-that-gaps.md:121`; CSS semantic refutation at `restart/audit/totality/p2/2F-parse-that-gaps.md:101`-`102` | Byte kernels may be reused; JSON semantic APIs cannot be the CSS parser. |
| LAC-2F-V1-04 | 2F | L08/L16 | ACCEPT | D-L08, D-L16 | `restart/audit/totality/p2/2F-parse-that-gaps.md:122`; broadcast block at `restart/audit/totality/p2/2F-parse-that-gaps.md:103` | Primitive row movement is blocked while broadcast telemetry is unresolved. |

## Consequences

Positive: the diff turns scattered candidate wording into one lock addendum and keeps the invariant boundaries visible. It also resolves duplicate CSS broadcast, primitive-manifest, and five-shape clauses without adding a sixth shape.

Cost: this is a documentation-only proposal with per-clause propagation costs stated above. Pass Omega CRUD must decide whether to keep the addendum as one section or distribute each clause into the affected lock body. No implementation is authorized, and no clause may be used to launder wave overflow into challenge time.

Propagation: this touches `LOCKS.md` only. 3A/3B/3E/3F should cross-reference the same 12 delta ids where their surfaces mention architecture, waves, grammar generalisation, or handoff.

## Open Questions

| lens | question | receiver | blocker | gate |
|---|---|---|---|---|
| CH1 | Should Pass Omega distribute the addendum under each lock instead of adding one SK-V15 addendum section? | Pass Omega CRUD owner for `restart/locks/LOCKS.md` | Governance-style placement only; no LAC is deferred by this decision. | CH1 path-resolution plus clean `git apply --check` of `3C-locks-v+1-diff.md`. |
| CH4 | Are the 12 clauses too dense for downstream wave owners? | Pass Omega CRUD owner plus 3A/3B/3E/3F propagation owners | Downstream owner split may be needed if one addendum section hides wave/gate responsibility. | CH4 per-clause cost matrix and same-wave consumer/gate review. |
| CH6 | Can any candidate be treated as already satisfied by current `LOCKS.md` text? | Pass Omega CH1/CH6 reviewers | Current text must already contain resolving evidence with valid path:line citations and no paper close. | CH1 citation-resolution matrix and CH6 anti-paper-close review. |
