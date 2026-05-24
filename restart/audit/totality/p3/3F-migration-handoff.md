---
agent: "3F"
pass: "T-P3-synthesis"
cycle: "V1"
generated_at: "2026-05-24T03:03:40Z"
sk_cycle: "SK-V14"
t_p1_lock_commit: "0a9c0fe65"
t_p2_lock_commit: "34a28f5c1"
s_p3_lock_commit: "626cb06cc"
t_p1_inventories_consumed:
  - "1A-substrate-evidence"
  - "1B-codegen-evidence"
  - "1C-runtime-evidence"
  - "1D-skinny-lessons"
  - "1E-locks-evidence"
  - "1F-coherence-scan"
  - "1F-anti-pattern"
  - "1F-past-corpora"
  - "HARDENING-T-P1-V5-CONSOLIDATED"
t_p2_dossiers_consumed:
  - "2A-sota-landscape"
  - "2B-primitive-vocabulary"
  - "2C-grammar-neutrality"
  - "2D-cost-model"
  - "2E-host-arch-esoterica"
  - "2F-parse-that-gaps"
  - "T-P2-V2-FOLD-ADDENDUM"
  - "T-P2-V3-FOLD-ADDENDUM"
  - "T-P2-V4-FOLD-ADDENDUM"
  - "HARDENING-T-P2-V5-CONVERGED"
s_p3_artefacts_consumed:
  - "sk-v14/SPEC.md"
  - "sk-v14/DISPATCH-PROMPT.md"
  - "research/p3/p3a-candidate-shortlist.md"
  - "research/p3/p3b-wave-sequencing.md"
  - "research/p3/p3c-falsifiability-gates.md"
  - "research/p3/p3d-telemetry-schema.md"
  - "research/p3/p3e-preblocked-ledger.md"
  - "research/p3/p3f-spec-draft.md"
  - "research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md"
v1_surface_targeted:
  - "restart/MIGRATION.md"
  - "restart/HANDOFF.md"
proposed_deltas_count: 14
delta_summary:
  carried_from_prior_cycle:
    - "Proposal-only authority (T-P3 PROPOSES; Pass Omega CRUD edits MIGRATION.md + HANDOFF.md)"
    - "G-Omega remains user gate; SK-V13 W0 blocking pattern carries forward to SK-V14 W0 blocking pattern"
    - "Archive-proof requirement for ser/gorgeous/simd-scan/bbnf-path/bbnf-path-ts (LAC-1E-06 sustained)"
  removed:
    - "SK-V13 HANDOFF body text (V1.1 Pass Omega close at 2026-05-22T03:52:18Z is now historical lineage, not current authority)"
    - "Rename-only treatment for legacy crates without Pattern H census + substrate-doc cleanup"
    - "Cycle V3 references to SK-V12 CSS L4 admission as forward authority — SYNTHESIS.md SK-V14 reopens 24 CSS L4 admits AUDIT-FALSIFIED per SPEC.md:178-180"
  answered:
    - "Whether T-P3 may edit governance surfaces: no, proposal-only until Pass Omega CRUD post-G-Omega per PASS-3-SYNTHESIS.md §8.6"
    - "Whether SK-V14 W0 may start from S-P3 LOCK alone: no, the gating sequence is T-P3 §3C → G-Omega → Pass Omega CRUD → wave-triumvirate dispatch"
    - "How rename/abrogate/refactor interact with PRUNE waves: PRUNE-3 (W5) + PRUNE-4 (W6 9 sub-waves) + PRUNE-5 (W7) are the receiver waves; MIGRATION rows must name them"
  newly_added:
    - "3F-MIG-001 through 3F-MIG-006 (refresh: archive-proof, generated-provider, fact-stream telemetry, decision-engine receiver, primitive manifest, PRUNE/SKELETON rename-abrogate-refactor row)"
    - "3F-HANDOFF-001 through 3F-HANDOFF-005 (SK-V14 top-level state; T-P1/T-P2/S-P3 LOCK commits; G-Omega gate; SK-V14 W0 block; three-surface grammar-onboarding wording; concurrency posture)"
    - "3F-DISPATCH-001 next-cycle directive (wave-triumvirate per SKINNY-TRIUMVIRATE.md §1-3 + Pass Alpha re-entry for SK-V15 bracket post-R10)"
    - "3F-MIG-007 dispatch_value → dispatch rename for Lock 14 v+1 + SKELETON triple-DELETE refusal row"
prior_cycle_dispositions_folded:
  accepted:
    - "G-T-P1-V5 COHORT §3Z LOCK (commit 0a9c0fe65)"
    - "G-T-P2-V5 6/6 ACCEPT 100% (commit 34a28f5c1)"
    - "G-S-P3-V3 COHORT §3Z LOCK (commit 626cb06cc)"
    - "Cycle V3 3F proposal-only authority"
    - "Cycle V3 archive-proof requirement"
  rejected:
    - "SKELETON triple DELETE proposal (FSM_DISPATCH_THREADED + FRAME_PUSH/POP_BOUNDED) — doubly inadmissible per T-P2 V3 LOCK cohort refutation density 32:69 = 31.7% (`HARDENING-T-P2-V3-CONSOLIDATED.md:76,172,187,295`)"
    - "REDRESS-96/97/98 union-substrate revival without material differential"
    - "Producer-only or orphan SIMD/source-present primitives as admission"
  revised:
    - "Cycle V3's SK-V12 CSS L4 admission narrative — SYNTHESIS.md SK-V14 reopens 24 CSS L4 + 22 JSON admits AUDIT-FALSIFIED"
    - "Cycle V3 3F-MIG-002 generated-provider row — now binds explicitly to PRUNE-3 + PRUNE-4 receiver waves"
    - "Cycle V3 3F-HANDOFF top-level state — refreshed to SK-V14 LOCKED packet"
---

## Executive Summary

3F proposes 14 V1-surface deltas: 7 for `restart/MIGRATION.md`, 5 for `restart/HANDOFF.md`, plus 1 next-cycle dispatch directive + 1 SKELETON-DELETE-refusal row. T-P3 is proposal-only per `restart/prompts/totality/PASS-3-SYNTHESIS.md:21-24` + §8.6; V1 spec edits land through Pass Omega CRUD post-G-Omega (`restart/prompts/pass-contracts/PASS-OMEGA.md:67,96-110`). The SK-V14 LOCKED packet (T-P1 `0a9c0fe65` + T-P2 `34a28f5c1` + S-P3 `626cb06cc`) refreshes the entire 3F surface against five new lock-amendment candidates (LAC-1E-12 CH7 binding + LAC-1E-13 R4 round-trip + LAC-1E-14 CSS L4 FactStream + LAC-1E-15 Pattern H 67 + LAC-1E-16 audit-overlay columns; `restart/audit/totality/p1/1E-locks-evidence.md:118-126`), the elevated LAC-2F-V5-02 substrate-union v+1 (canonical T-P2 V3 LOCK evidence at `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:182-192`; V5 was confirmation cycle re-passing V4 packet unchanged per `HARDENING-T-P2-V5-CONVERGED.md`), and the SK-V14 12-wave SPEC (W0..W11 with PRUNE-1..PRUNE-5 enumerated as W1/W4/W5/W6/W7; `restart/skinny/tranches/sk-v14/SPEC.md:41,238-247`). The proposed MIGRATION delta replaces SK-V13's rename-only and admit-only treatment with PRUNE-receiver-named archive/refactor gates. The proposed HANDOFF delta updates top-level state from V1.1 Pass Omega close to T-P3 SK-V14 cycle in flight, preserves G-Omega as the only mandatory relinquish per the SK-V14 ORCHESTRATOR-PROMPT pin, and routes wave-triumvirate dispatch (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`) as the post-G-Omega next move.

## V1 Delta Summary

| bucket | summary | evidence |
| --- | --- | --- |
| carried | Proposal-only authority survives; 3F never edits MIGRATION.md or HANDOFF.md directly. | `restart/prompts/totality/PASS-3-SYNTHESIS.md:21-24`, `restart/prompts/totality/PASS-3-SYNTHESIS.md:197-198` |
| carried | Archive-proof requirement for `ser`, `gorgeous`, `simd-scan`, `bbnf-path`, `bbnf-path-ts` sustained from V4 LAC-1E-06. | `restart/audit/totality/p1/1E-locks-evidence.md:116` (LAC-1E-06), `restart/MIGRATION.md:38,70,77` |
| carried | G-Omega user gate is the only mandatory relinquish per SK-V14 ORCHESTRATOR-PROMPT pin override on PASS-3-SYNTHESIS.md §6. | `restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:5`, `restart/prompts/pass-contracts/PASS-OMEGA.md:96-110` |
| removed | SK-V13-era HANDOFF body text describing V1.1 Pass Omega close as current authority is stale; current authority is SK-V14 T-P3 in-flight. | `restart/HANDOFF.md:3-18` (V1.1 close 2026-05-22T03:52:18Z), `restart/skinny/tranches/sk-v14/SYNTHESIS.md:1-8` SK-V14 active contract |
| removed | Cycle V3's SK-V12 CSS L4 admission as forward authority — SK-V14 reopens at audit-zero baseline. | `restart/skinny/tranches/sk-v14/SPEC.md:178-180` (24 CSS L4 ADMITTED → 0 ADMITTED; all reopen), `restart/skinny/tranches/sk-v14/SYNTHESIS.md:213-218` audit-zero |
| answered | Whether SK-V14 W0 may start before G-Omega: no, gating chain is T-P3 §3C → G-Omega → Pass Omega CRUD → wave-triumvirate. | `restart/skinny/tranches/sk-v14/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:78-98`, `restart/prompts/pass-contracts/PASS-OMEGA.md:96-110` |
| answered | How PRUNE waves consume migration deltas: PRUNE-3 (W5 trait dispatch) + PRUNE-4 (W6 9 sub-waves) + PRUNE-5 (W7 W8/W9 wire-up) are the receiver waves named in MIGRATION rows. | `restart/skinny/tranches/sk-v14/SPEC.md:242-244`, `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md:31-33` |
| new | 7 MIG deltas + 5 HANDOFF deltas + 1 dispatch directive + 1 SKELETON-DELETE-refusal row. | this artefact §§Proposed Delta Table + Next-Cycle Dispatch Directive |

## Proposed Delta Table

| proposed delta | source finding-id | affected V1-surface section | rationale |
| --- | --- | --- | --- |
| 3F-MIG-001 | LAC-1E-06, T2B-ABROGATE | `restart/MIGRATION.md:38, 70, 77, 87, 116, 119, 540-548` (legacy-crate disposition + 0.1 V1.1 receiver) | Sustain archive-proof gates for `ser`, `gorgeous`, `simd-scan`, `bbnf-path`, `bbnf-path-ts`; refresh wording so rename to `bbnf-simd` is NOT a Lock 16 close, only the primitive-boundary opening move. Lock 16 closure requires LAC-1E-10 traceability manifest (`restart/audit/totality/p1/1E-locks-evidence.md:120`). |
| 3F-MIG-002 | P1-1B-D7, P1-1C-D1, LAC-1E-08, T2C-LOCK14 | `restart/MIGRATION.md:39, 80-95, 134-178` (mixed-fate crosswalk + crate disposition) | Hardcoded `RuntimeProvider` enum + 8 hardcoded match arms + 30 grammar-parser-name leaks across 15 files (`restart/audit/totality/p1/1C-runtime-evidence.md:125`; ~190 LOC + 2.5× consumer-rewire band per CH2 V2) are ABROGATE-REPLACE; receiver is **SK-V14 W5 PRUNE-3** (trait dispatch + grammar-agnostic generator template; `restart/skinny/tranches/sk-v14/SPEC.md:242, 626-684`). Generic crates must consume generated registry/config/facts. |
| 3F-MIG-003 | P1-1C-D2, P1-1C-D7, LAC-1E-08, LAC-1E-15 | `restart/MIGRATION.md:155, 240, 359` (per-grammar runtime + Pattern H census) | 67 hand-written per-grammar runtime files across 9 dirs (`restart/audit/totality/p1/1E-locks-evidence.md:102` D-1E-15; +3 vs V13 from css_pretty) are ABROGATE-REPLACE; receiver is **SK-V14 W6 PRUNE-4 with 9 sub-waves NOT 8** (`bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math`; `restart/skinny/tranches/sk-v14/SPEC.md:243, 687-775`). Substrate templates `builder_template.rs:13-31` + `arena_template.rs:1-31` opt-out doc-comments are themselves Lock 14 violations per LAC-1E-15. |
| 3F-MIG-004 | LAC-1E-14, 1C-D5, CH2 V3 F2 | `restart/MIGRATION.md:41, 553-577` (non-JSON telemetry + generated code) | CSS L4 fact-stream lacks formal substrate category alongside OffsetTape/EventTape/SinkOnly/CollapsedStage; LAC-1E-14 lands `FactStream` as the 5th admitted-product category at the Lock 1 SUBSTRATE manifest (alongside OffsetTape/EventTape/SinkOnly/CollapsedStage), NOT a 6th `BackendShape` variant — the 5-shape `BackendShape` search domain at Lock 10 (`{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`) holds. The two axes (Lock 1 substrate manifest vs Lock 10 BackendShape search domain) are ORTHOGONAL; LAC-1E-14 touches the manifest axis only. MIGRATION row binds CSS L4 row to `admitted_fact_output` substrate_target with comparator provenance + gate-consumed telemetry mandatory per 3C V1 ACCEPT at `restart/audit/totality/p3/3C-locks-crystallisation.md:32` (3C-L01-factstream-fifth-category) + V4-3 hunk verbatim at `restart/audit/totality/p3/3C-locks-v+1-diff.md:118`-`140`. |
| 3F-MIG-005 | T2D-DECISION, T2F-IMPORT, LAC-1E-08 (W8/W9 SCAFFOLD) | `restart/MIGRATION.md:42` (decision engine row) + `restart/MIGRATION.md:613,619` (cost-model + parse-that crate fates) | P1-P8 cascade + thin `CostFacts` + opaque regex programs are ABROGATE-REPLACE; receiver is **SK-V14 W7 PRUNE-5** wiring W8 per-grammar policy + W9 same-substrate union from SCAFFOLD-ONLY to LOAD-BEARING (`restart/skinny/tranches/sk-v14/SPEC.md:244, 779-838`). Note: W7 `same_substrate_union` is an ENFORCEMENT-LAYER pass NOT the SK-V9 W3 retired retained-class-column-union data structure (PERMANENT-PRE-BLOCK per REDRESS 96/97/98; `restart/skinny/tranches/sk-v14/SPEC.md:806`). |
| 3F-MIG-006 | T2B-L16, T2E-SOURCE, LAC-1E-10 | `restart/MIGRATION.md:43, 91, 120, 174-175, 275-286, 505-525` (bbnf-simd + primitive admissions) | Every SIMD/ASM/table/mask/carry/source-present primitive requires manifest identity, source state, strict mode, first consumer, command, scalar fallback or architectural block. Rename `simd-scan` → `bbnf-simd` is necessary but insufficient: Lock 16 closure requires LAC-1E-10 traceability manifest mapping every intrinsic/`asm!` use to allowlist row + scalar parity + corpus parity + same-wave consumer. |
| 3F-MIG-007 | NEW SK-V14 | `restart/MIGRATION.md:38-44` (0.1 V1.1 receiver) + new rename row | **dispatch_value → dispatch** rename per Lock 14 v+1: the `RuntimeProvider` enum's `dispatch_value` arm (8 hardcoded match arms; `skinny/crates/codegen/src/lib.rs:167-209`) becomes `dispatch` under the trait-dispatch refactor (PRUNE-3 receiver). Per `[no-backward-compat]`, the rename is migration-full not aliased; consumers update in same wave. **SKELETON triple DELETE** (FSM_DISPATCH_THREADED + FRAME_PUSH_BOUNDED + FRAME_POP_BOUNDED) is **REJECTED** per T-P2 V3 LOCK cohort refutation density 32:69 = 31.7% (1:2 anti-paper-close pattern; canonical T-P2 V3 figure at `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:76,172,187,295`). MIGRATION carries a refusal row preserving the three primitives as non-shortlist-blocker evidence under SRC-V2-FOLD support, not deletion. |
| 3F-HANDOFF-001 | PASS3-LOCK, OMEGA-CRUD | `restart/HANDOFF.md:3-18` (Current Override block) | Replace V1.1 Pass Omega closed wording with: "T-P3 Synthesis SK-V14 cycle V1 in flight on the LOCKED packet (T-P1 `0a9c0fe65`, T-P2 `34a28f5c1`, S-P3 `626cb06cc`). T-P3 PROPOSES; Pass Omega CRUD edits governance surfaces post-G-Omega. Until G3 auto-pass + G-Omega close, `ARCHITECTURE.md`, `MASTER-PLAN.md`, `LOCKS.md`, `MIGRATION.md`, `HANDOFF.md`, source, generated runtime, gate output, `skinny/RESULTS.md`, `skinny/REDRESS.md` remain unchanged except by their authorized pass owners." |
| 3F-HANDOFF-002 | T-P1 V5 LOCK + T-P2 V5 LOCK + S-P3 V3 LOCK | `restart/HANDOFF.md:19-34` (reading order) | Refresh reading order to: (1) `restart/prompts/ORCHESTRATOR.md`; (2) `restart/prompts/totality/PASS-3-SYNTHESIS.md`; (3) `restart/prompts/pass-contracts/PASS-OMEGA.md`; (4) `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`; (5) `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md`; (6) `restart/audit/totality/p2/hardening/HARDENING-T-P2-V5-CONVERGED.md`; (7) `restart/skinny/tranches/sk-v14/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md`; (8) `restart/skinny/tranches/sk-v14/SPEC.md`; (9) `restart/skinny/tranches/sk-v14/SYNTHESIS.md`; (10) `restart/audit/totality/p3/3{A..F}-*.md`; (11) `restart/locks/LOCKS.md` + `restart/MIGRATION.md` + this file; (12) `skinny/RESULTS.md` + `skinny/REDRESS.md`. Demote SK-V13 packet + V1.1 CRUD-LOG to historical lineage. |
| 3F-HANDOFF-003 | SKV14-SYNTHESIS, SPEC | `restart/HANDOFF.md:36-52` (current skinny state) | Replace SK-V13 skinny bar wording with SK-V14 current state: "SK-V14 audit-corrected baseline: 0/17 × 3 JSON planes + 0/24 CSS L4 ADMITTED at audit-zero (`SPEC.md:178-180`). 25 CSS + 5 parse_only + 4 direct + 7 typed AUDIT-FALSIFIED admits revert at W1 PRUNE-1 + W4 PRUNE-2 (`SPEC.md:191-198`). SK-V14 close requires PRUNE-1..PRUNE-5 dispatch BEFORE any new-admit wave (W8 R6 + W9 R7 + W10 R8)." |
| 3F-HANDOFF-004 | SKV14-G-OMEGA-BLOCK, OMEGA-GATE | `restart/HANDOFF.md:49-52` (Dispatch rule) | Refresh concurrency/refusal block: "SK-V14 W0 and later source/gate/RESULTS/REDRESS edits are authorized only after (a) G3 auto-passes T-P3 cohort §3Z LOCK; (b) Pass Omega CRUD completes; (c) G-Omega user gate closes; (d) wave-triumvirate dispatch per `SKINNY-TRIUMVIRATE.md §1-§3`. Until then, S-P3/SPEC text is planning authority but not implementation authority. T-P3 + Pass Omega prep + Pass Alpha planning may continue in parallel." |
| 3F-HANDOFF-005 | COH-002, T2C-LOCK14 | `restart/HANDOFF.md:43-47` (grammar onboarding) | Sustain three-surface onboarding wording: grammar source `.bbnf` + workspace metadata + optional per-grammar declaration crate for host functions. Generated per-grammar names allowed only as generator output from rostered metadata (LAC-1E-08 V+1 generated-output allowance; `restart/audit/totality/p1/1E-locks-evidence.md:118`). Generic crates must not grow grammar switches, grammar-named public APIs, or hand-written per-grammar runtime files (LAC-1E-15 Pattern H census + substrate-doc cleanup; `restart/audit/totality/p1/1E-locks-evidence.md:125`). |
| 3F-DISPATCH-001 | PASS-OMEGA §5-6, SKINNY-TRIUMVIRATE §1-3 | next-cycle directive | Author next-cycle dispatch: (a) G3 auto-passes T-P3 cohort §3Z LOCK per SK-V14 ORCHESTRATOR-PROMPT override; (b) Pass Omega dispatches with T-P3 packet + SK-V14 LOCKED inputs; (c) Pass Omega CRUD prepares diffs against MIGRATION/HANDOFF/LOCKS/ARCHITECTURE/MASTER-PLAN; (d) G-Omega user gate (mandatory); (e) post-G-Omega: wave-triumvirate dispatches W0 first per `SKINNY-TRIUMVIRATE.md §1`; (f) post-R10 close (per SYNTHESIS §0.1): Pass Alpha re-entry for SK-V15 bracket per `restart/prompts/pass-contracts/PASS-ALPHA.md`. |

## Proposed MIGRATION.md Delta Text

Use these paragraphs as Pass Omega CRUD input, not as direct edits. They map 1:1 to the rows in 3F-MIG-001..3F-MIG-007 above.

1. **Archive-proof legacy crates row (3F-MIG-001).** Append to `restart/MIGRATION.md:38` (V1.1 Legacy crate fates): "Archive/remove proof is required for `ser`, `gorgeous`, `simd-scan`, `bbnf-path`, and `bbnf-path-ts` before they may be counted closed per Lock 11/12/14/16; rename to `bbnf-simd` is the primitive-boundary opening, not closure. Lock 16 closure additionally requires LAC-1E-10 traceability manifest mapping every intrinsic/`asm!` use to allowlist row + scalar parity + corpus parity + same-wave consumer (`restart/audit/totality/p1/1E-locks-evidence.md:120`)."

2. **Generated-provider migration row (3F-MIG-002).** Append to `restart/MIGRATION.md:39` (V1.1 Generated-provider roster): "Hardcoded `RuntimeProvider` enum + 8 hardcoded match arms + 30 grammar-parser-name leaks across 15 files (`crates/core/src/runtime/{json,bbnf,css_l4,google_sheets}/{parse_with,mod,document,builder,serialize}.rs`; ~190 LOC + 2.5× consumer-rewire band per CH2 V2 mechanical extraction) are ABROGATE-REPLACE; receiver is **SK-V14 W5 PRUNE-3** (trait-dispatch refactor + grammar-agnostic generator template per `restart/skinny/tranches/sk-v14/SPEC.md:626-684`). Per `[no-backward-compat]`, the `dispatch_value` enum arm renames to `dispatch` trait method in same wave; consumers update without alias period."

3. **Per-grammar runtime + Pattern H census row (3F-MIG-003).** Append to `restart/MIGRATION.md:40` (V1.1 Per-grammar runtime roots): "67 hand-written per-grammar runtime files across 9 dirs under `crates/core/src/runtime/{bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math}/` (+3 vs V13 baseline 64 from css_pretty addition; `restart/audit/totality/p1/1E-locks-evidence.md:102` D-1E-15) are ABROGATE-REPLACE; receiver is **SK-V14 W6 PRUNE-4 with 9 sub-waves NOT 8** per S-P0 §2.3. Substrate templates `builder_template.rs:13-31` + `arena_template.rs:1-31` opt-out doc-comments are themselves Lock 14 violations per LAC-1E-15 — substrate-doc cleanup is a PRUNE-4 sub-task. Per-tranche Pattern H census via `find crates/core/src/runtime -mindepth 2 -maxdepth 2 -type f -name '*.rs' | wc -l` cited at every wave commit."

4. **CSS L4 fact-stream telemetry row (3F-MIG-004).** Append to `restart/MIGRATION.md:41` (V1.1 Non-JSON telemetry): "CSS L4 declaration-values is admitted same-plane fact-stream row evidence (`skinny/RESULTS.md:94`) but lacks formal runtime substrate category alongside OffsetTape/EventTape/SinkOnly/CollapsedStage. Per 3C V1 ACCEPT at `restart/audit/totality/p3/3C-locks-crystallisation.md:32` (3C-L01-factstream-fifth-category) and 3C V4 hunk V4-3 verbatim at `restart/audit/totality/p3/3C-locks-v+1-diff.md:118`-`140`, LAC-1E-14 lands `FactStream` as the **5th admitted-product category at the Lock 1 SUBSTRATE manifest** (alongside `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage`); a fact-stream row carries `substrate_target = admitted_fact_output`. The 5th category is a substrate-manifest classification only; it is **NOT a 6th `BackendShape` variant**. The 5-shape `BackendShape` search domain at Lock 10 — `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` — HOLDS. The two axes (Lock 1 substrate manifest vs Lock 10 BackendShape search domain) are ORTHOGONAL; LAC-1E-14 touches the manifest axis only. Any 6th `BackendShape` variant remains G-Omega gated per Lock 10 v+1 + PASS-3 §8.1 (not in scope at this MIGRATION row). MIGRATION binds CSS L4 row to fenced telemetry with strict comparator provenance + gate-consumed telemetry per Lock 1 V+1 fact-stream wording (`restart/locks/LOCKS.md:66-71`). Doc-only delta with zero impl-tail: W8 re-admit consumer-plane cost is accounted at `3C-L01-factstream-fifth-category` (60-150 docs per `restart/audit/totality/p3/3C-locks-crystallisation.md:158`); 3F-MIG-004 itself adds no separate W8 implementation budget."

5. **Decision-engine receiver row (3F-MIG-005).** Append to `restart/MIGRATION.md:42` (V1.1 Decision engine): "P1-P8 cascade + thin `CostFacts` + opaque regex programs + marker-string lowerers are ABROGATE-REPLACE; receiver is **SK-V14 W7 PRUNE-5** wiring W8 per-grammar policy SCAFFOLD + W9 same-substrate union SCAFFOLD to LOAD-BEARING (`restart/skinny/tranches/sk-v14/SPEC.md:779-838`). The W7 `same_substrate_union` module is an ENFORCEMENT-LAYER pass proving substrate-union compliance (every shape consumer reuses existing `Tape` substrate — zero new retained surface); it is NOT the SK-V9 W3 retired retained-class-column-union data structure (PERMANENT-PRE-BLOCK per REDRESS 96/97/98). Active cost extraction + eqsat + CSP + candidate generation + strict equivalence/cost evidence required before admission."

6. **Lock 16 primitive manifest row (3F-MIG-006).** Append to `restart/MIGRATION.md:43` (V1.1 Primitive manifest): "Every SIMD/ASM/hardware/table/mask/carry/source-present primitive must close as wired, deleted, scalar-delegated, or architectural-blocked with REDRESS evidence. Manifest fields required per LAC-1E-10: identity, source state, strict mode, first consumer, command, scalar fallback OR architectural block, LOC/risk, rollback path, abrogate threshold. Rename `simd-scan` → `bbnf-simd` is necessary but insufficient. Inventory demotion is not a close state."

7. **dispatch_value → dispatch rename + SKELETON refusal row (3F-MIG-007).** Insert new row in `restart/MIGRATION.md` §3.1.1 Mixed-Fate Crosswalk: "`RuntimeProvider::dispatch_value` enum arm (8 hardcoded match arms; `skinny/crates/codegen/src/lib.rs:167-209`) | ABROGATE-REPLACE → trait-method `dispatch` | W5 PRUNE-3 generator | Generic-crate Lock 14 violation; rename is migration-full per `[no-backward-compat]`." Add refusal note: "**SKELETON triple DELETE proposal** (FSM_DISPATCH_THREADED + FRAME_PUSH_BOUNDED + FRAME_POP_BOUNDED) is **REJECTED** per T-P2 V3 LOCK cohort refutation density 32:69 = 31.7% (1:2 anti-paper-close pattern; canonical T-P2 V3 figure at `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:76,172,187,295`); the three primitives remain non-shortlist-blocker support under SRC-V2-FOLD. Re-proposal requires same-wave consumer + first consumer path + executable command per T-P2 V4 non-shortlist criteria."

## Proposed HANDOFF.md Top-Level State Delta

Use this block as the replacement top-level state once G3 closes and Pass Omega prepares CRUD.

```
# Handoff — bbnf-lang Greenfield Restart

## Current Totality Override — 2026-05-24

Status: **T-P3 Synthesis SK-V14 cycle V1 IN FLIGHT.** T-P1 LOCKED at
`0a9c0fe65` (V5 COHORT §3Z LOCK; sub-axis 100% × 2 cycles; zero orphan
REVISEs); T-P2 LOCKED at `34a28f5c1` (V5 6/6 ACCEPT 100% × 2 cycles);
S-P3 LOCKED at `626cb06cc` (V3 COHORT §3Z LOCK; per-lens 2-cycle chain
satisfied at V≤5 ceiling EXACTLY). T-P3 PROPOSES; Pass Omega CRUD edits
governance surfaces post-G-Omega per `restart/prompts/totality/PASS-3-SYNTHESIS.md §8.6`
+ `restart/prompts/pass-contracts/PASS-OMEGA.md §4-§6`.

Until G3 auto-pass + Pass Omega CRUD + G-Omega close, `ARCHITECTURE.md`,
`MASTER-PLAN.md`, `LOCKS.md`, `MIGRATION.md`, `HANDOFF.md`, source,
generated runtime, gate output, `skinny/RESULTS.md`, `skinny/REDRESS.md`
remain unchanged except by their authorized pass owners.

After G3 auto-passes the T-P3 cohort, dispatch Pass Omega. Pass Omega
consumes T-P1 V5 + T-P2 V5 + S-P3 V3 + T-P3 packet + current V1
surfaces. Pass Omega CRUD prepares concrete diffs; G-Omega remains the
user gate before any V1 amendment merge and before SK-V14 W0 dispatch.

Current measured authority remains `skinny/RESULTS.md` (SK-V14
audit-zero baseline: 0/17 × 3 JSON planes + 0/24 CSS L4 ADMITTED per
`restart/skinny/tranches/sk-v14/SPEC.md:178-180`; 22 JSON + 24 CSS
admits AUDIT-FALSIFIED per `:191-198`).

Current SK-V14 LOCKED contract surfaces:
- `restart/skinny/tranches/sk-v14/SPEC.md` (1187 lines; 12 waves W0..W11;
  PRUNE-1..PRUNE-5 at W1/W4/W5/W6/W7 dispatch BEFORE any new-admit wave).
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` (R1..R10 + P-1..P-7).
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` (344 lines).
- `restart/skinny/tranches/sk-v14/research/p3/p3{a,b,c,d,e,f}-*.md`.

Read in order for current work:

1. `restart/prompts/ORCHESTRATOR.md`.
2. `restart/prompts/totality/PASS-3-SYNTHESIS.md`.
3. `restart/prompts/pass-contracts/PASS-OMEGA.md`.
4. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.
5. `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md`.
6. `restart/audit/totality/p2/hardening/HARDENING-T-P2-V5-CONVERGED.md`.
7. `restart/skinny/tranches/sk-v14/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md`.
8. `restart/skinny/tranches/sk-v14/SPEC.md` + `SYNTHESIS.md` + `DISPATCH-PROMPT.md`.
9. `restart/audit/totality/p3/3{A..F}-*.md` (T-P3 packet; this file is 3F).
10. `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/locks/LOCKS.md`,
    `restart/MIGRATION.md`, and this file (current V1 surfaces).
11. `skinny/RESULTS.md` + `skinny/REDRESS.md`.

The SK-V14 skinny bar is the audit-corrected baseline: every CSS L4 + JSON
admit must clear R1 strict comparator + R2 per-iter equality oracle; W1
PRUNE-1 + W4 PRUNE-2 revert audit-falsified admits; W5 PRUNE-3 + W6
PRUNE-4 + W7 PRUNE-5 close Lock 14 + decision-engine surfaces BEFORE
W8/W9/W10 R6/R7/R8 re-admit. The SK-V12 single CSS row that admitted
under SK-V13 is now AUDIT-FALSIFIED-along-with-23-others; SK-V14 reopens.

Grammar onboarding remains three declarative surfaces only: grammar
source `.bbnf`, workspace metadata, and an optional per-grammar
declaration crate for host functions. Generated per-grammar names are
allowed only as generator output from the rostered metadata per LAC-1E-08
V+1 generated-output allowance. Generic crates must not grow grammar
switches, grammar-named public APIs, or hand-written per-grammar runtime
files per LAC-1E-15 Pattern H census + substrate-doc cleanup.

Dispatch rule: SK-V14 W0 and later source/gate/RESULTS/REDRESS edits
are authorized only after (a) G3 auto-passes T-P3 cohort §3Z LOCK; (b)
Pass Omega CRUD completes; (c) G-Omega user gate closes; (d)
wave-triumvirate dispatches W0 first per `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md §1-§3`.
Until then, S-P3/SPEC text is planning authority but not implementation
authority. T-P3 + Pass Omega prep + Pass Alpha planning may continue in
parallel.

## Historical Pass Omega V1.1 close (not current authority)

Pass Omega V1.1 closed at user sign-off `2026-05-22T03:52:18Z`; record at
`restart/audit/totality/astral/V1/G-OMEGA-SIGNOFF.md`; CRUD-LOG at
`restart/audit/totality/astral/V1/CRUD-LOG.md`. The V1.1 surface state
seeded the SK-V14 reopening, which produced the audit-zero baseline that
is now current authority. SK-V13 packet is historical lineage.

## Historical SK-V6 Handoff body (not current authority)

[existing §1..§9 body retained verbatim under archive lineage heading;
demote from current-authority section to historical-lineage section.]
```

## Next-Cycle Dispatch Directive (3F-DISPATCH-001)

Dispatch sequence after T-P3 closes: G3 auto-pass → Pass Omega → Omega CRUD → G-Omega (user) → wave-triumvirate W0 → … → post-R10 Pass Alpha re-entry for SK-V15 bracket.

### Entry conditions for the cycle Pass Omega CRUD wave hands forward (after G-Omega)

1. **G3 auto-pass closure recorded.** Per SK-V14 ORCHESTRATOR-PROMPT pin override (`restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:5`), T-P3 cohort §3Z LOCK at the close of the V1 cycle (≥95% × 2 cycles + zero orphan REVISEs + V≤5 ceiling per `restart/prompts/ORCHESTRATOR.md §3Z`) auto-closes G3; only G-Omega triggers user relinquish.
2. **Pass Omega packet prepared.** Six parallel sub-agents Ω-A..Ω-F dispatch per `restart/prompts/pass-contracts/PASS-OMEGA.md §2`, consuming T-P1 V5 + T-P2 V5 + S-P3 V3 + the six T-P3 3A..3F artefacts + the LOCKS v+1 diff at `restart/audit/totality/p3/3C-locks-v+1-diff.md`.
3. **Pass Omega CHALLENGE convergence.** Six-lens CHALLENGE per PASS-OMEGA.md §3; convergence rule per ORCHESTRATOR.md §3Z (≥95% × 2 cycles).
4. **Pass Omega CRUD prepares diffs.** CRUD-1..CRUD-6 per `restart/prompts/pass-contracts/PASS-OMEGA.md §4` write proposed-diff files at `restart/audit/totality/astral/V{V}/`; LOCKS amendments live in `locks-diff.md`; MIGRATION + HANDOFF deltas land via CRUD-4 (per `:67-76`).
5. **G-Omega user gate (mandatory).** User reads CONSOLIDATED verdict + LOCKS diff + master-plan delta + MIGRATION/HANDOFF delta + next-cycle directive per `restart/prompts/pass-contracts/PASS-OMEGA.md §6:96-108`. G-Omega closed → CRUD merges diffs to V1 surfaces.

### Wave-triumvirate dispatch context (SK-V14 SPEC W0 first)

Post-G-Omega, wave-triumvirate per `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md §1-§3` dispatches W0 first. SK-V14 SPEC §2 sequencing (`restart/skinny/tranches/sk-v14/SPEC.md:41-55`):

| Wave | Receiver | Sequencing constraint | Source |
| --- | --- | --- | --- |
| W0 | Mechanical workspace + comparator scaffolding | None | `restart/skinny/tranches/sk-v14/SPEC.md:237` |
| W1 | C-2 + C-5 PRUNE-1 (comparator rebind + per-iter equality + JSON revert) | Conditional on W0 | `SPEC.md:238` |
| W2 | R4 regen-css xtask | MUST precede W4 PRUNE-2 per S-P0 §2.1 | `SPEC.md:239, 511` |
| W3 | R5 CSS L4 production corpora ≥800 KB | Conditional on W2 close | `SPEC.md:240, 562` |
| W4 | C-5 PRUNE-2 (delete 7 CSS templates + revert 24 CSS admits) | Conditional on W2 + W3 close | `SPEC.md:241, 566-624` |
| W5 | C-1 PRUNE-3 (Lock-14 trait dispatch + grammar-agnostic generator) | Conditional on W4 close | `SPEC.md:242, 626-684` |
| W6 | C-1 PRUNE-4 (9 sub-waves: per-grammar runtime collapse) | Conditional on W5 close | `SPEC.md:243, 687-775` |
| W7 | C-4 PRUNE-5 (wire W8 + W9 SCAFFOLD → LOAD-BEARING) | Conditional on W6 close per S-P0 §2.2 | `SPEC.md:244, 779-838` |
| W8 | CSS L4 re-admit (R6 grammar-derived + production corpora + work-equivalent comparator) | Conditional on W7 close | `SPEC.md:245, 840-880` |
| W9 | JSON direct + typed re-admit (R7) | Conditional on W1 close | `SPEC.md:246` |
| W10 | JSON parse_only distinct path + re-admit (R8) + Stage-0 F-V2-P1ABC-RERECORD UNCONDITIONAL | Conditional on W1 + W9 close | `SPEC.md:247, 982` |
| W11 | SK-V14 close + R10 | Conditional on W0..W10 close | `SPEC.md` close section |

Wave-triumvirate per-wave roles: **research-axis triumvirate** (~20 min cap research; ~15 min cap plan; ~30 min cap redress per `[dispatch-hard-cap]`); each wave runs (a) RESEARCH dispatch → (b) PLAN dispatch → (c) REDRESS dispatch with hard caps + commit at 0.9N + halt at N.

### Pass Alpha re-entry for SK-V15 bracket (after R10 close per SYNTHESIS §0.1)

Post-R10 wave close (W11 closes SK-V14; per SYNTHESIS §0.1 R10 is the close condition), Pass Alpha re-enters per `restart/prompts/pass-contracts/PASS-ALPHA.md` to open the SK-V15 bracket. Pass Alpha consumes the SK-V14 close packet (final RESULTS.md + REDRESS.md + LOCKS state at v+2) + the next-cycle T-P1..T-P3 dispatch packet + any G-Omega-V2 amendments. SK-V15 SPEC drafting binds three deliverables: (a) post-R10 axis re-baseline; (b) carry-forward of every non-closed SK-V14 row with explicit receiver/blocker/gate triple; (c) Pass Alpha closure of any LAC the SK-V14 close created (anticipated: LAC-1E-14 FactStream resolution, LAC-1E-15 Pattern H residual, LAC-1E-16 audit-overlay column population proof).

### Measurable dispatch checklist

| gate | measurable condition | source |
| --- | --- | --- |
| G3 (auto) | 3A..3F + 3C-v+1-diff present; T-P3 cohort CHALLENGE V1 ≥95% sub-axis + zero orphan REVISE; per SK-V14 pin auto-closure on §3Z LOCK. | `restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:5`, `restart/prompts/ORCHESTRATOR.md §3Z` |
| Pass Omega entry | T-P1 V5 + T-P2 V5 + S-P3 V3 + T-P3 packet + V1 surfaces cited in Ω source maps; HANDOFF declares ready-for-Omega. | `restart/prompts/pass-contracts/PASS-OMEGA.md §1`, this artefact 3F-HANDOFF-001 |
| Pass Omega CHALLENGE convergence | Ω CHALLENGE ≥95% × 2 cycles + zero orphan REVISE + V≤5 ceiling. | `restart/prompts/pass-contracts/PASS-OMEGA.md §3`, `restart/prompts/ORCHESTRATOR.md §3Z` |
| CRUD entry | Consolidated Ω verdict + per-surface CRUD instruction packets exist. | `restart/prompts/pass-contracts/PASS-OMEGA.md §4:57-76` |
| G-Omega (user) | User receives summary + CONSOLIDATED verdict + locks-diff + master-plan delta + MIGRATION/HANDOFF delta + next-cycle directive. | `restart/prompts/pass-contracts/PASS-OMEGA.md §6:96-108` |
| SK-V14 W0 dispatch | G-Omega closed; wave-triumvirate per `SKINNY-TRIUMVIRATE.md §1-§3` ready; SK-V14 SPEC §2 W0 entry-gate satisfied. | `restart/skinny/tranches/sk-v14/SPEC.md:237`, `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md §1` |
| SK-V14 close (R10) | RESULTS audit-zero baseline cleared by R6/R7/R8 re-admits under R1/R2 gates; W11 close per SYNTHESIS §0.1. | `restart/skinny/tranches/sk-v14/SYNTHESIS.md §0.1` |
| SK-V15 Pass Alpha re-entry | SK-V14 close packet delivered; Pass Alpha dispatch per `PASS-ALPHA.md`. | `restart/prompts/pass-contracts/PASS-ALPHA.md` |

## Consequences

| category | consequence | propagation |
| --- | --- | --- |
| positive | `MIGRATION.md` becomes a gate ledger naming PRUNE-3/4/5 receivers instead of a rename-only ledger. | Propagates to all crate-disposition rows in §3.1.1 mixed-fate crosswalk + V1.1 receiver block + new dispatch_value→dispatch + SKELETON refusal rows. |
| positive | `HANDOFF.md` top-level state matches SK-V14 LOCKED inputs and demotes V1.1 + SK-V6 bodies to historical lineage. | Propagates to reading order; orchestrator dispatch order; SK-V14 W0 refusal conditions. |
| positive | SKELETON triple DELETE refusal row formalises T-P2 V3 LOCK cohort refutation density 32:69 = 31.7% (`HARDENING-T-P2-V3-CONSOLIDATED.md:76,172,187,295`) as a re-proposal gate (same-wave consumer + command + first consumer required). | Propagates to T-P2 V4-style non-shortlist criteria as the future-proposal template. |
| cost | Pass Omega CRUD-4 must edit both MIGRATION.md (7 row inserts) and HANDOFF.md (5 section rewrites). | CRUD-4 effort scales linearly; ordering tension between "CRUD before G-Omega presentation" and "G-Omega before merge" resolved by treating pre-G-Omega CRUD as proposed-diff artefacts, post-G-Omega as authoritative merge per Pass Omega §4-§6. |
| cost | SK-V14 W0 dispatch is gated on three serial closes (G3 → CRUD → G-Omega) before wave-triumvirate may dispatch. | Wave-triumvirate hard caps per `[dispatch-hard-cap]` carry; W1..W11 11 wave-triumvirate sets queued. |
| propagation | Decision-engine MIGRATION row routes into W7 PRUNE-5 work; primitive-manifest row routes into Lock 16 LAC-1E-10 traceability work. | SK-V14 §3 C-4 + Lock 16 V+1 primitive manifest gates inherit. |
| propagation | Pass Alpha re-entry post-R10 opens SK-V15 bracket; LAC-1E-14 FactStream + LAC-1E-15 Pattern H residual + LAC-1E-16 column population gates re-enter as SK-V15 entry conditions. | Three SK-V14 close LACs forward into SK-V15 SPEC entry constraints. |

## V1 Cost And Routing Ledger

Sustained from V3 V2-cost-ledger format; refreshed against SK-V14 PRUNE-receiver mapping + SKELETON-refusal accounting.

| delta | LOC budget | propagation surfaces | risk class | wave alignment | same-wave consumer / receiver | hard cap or abrogate gate |
| --- | ---: | ---: | --- | --- | --- | --- |
| 3F-MIG-001 | 60-140 docs | 4 | Medium-high | Omega CRUD-4 / A.W0 archive | Receiver: archive/removal proof rows. | Block rename-only closure; require archive proof OR reconstituted generated surface. |
| 3F-MIG-002 | 120-260 docs | 4 | High | SK-V14 W5 PRUNE-3 | Receiver: trait dispatch + grammar-agnostic generator template. | Abrogate hardcoded `RuntimeProvider` enum + 8 match arms + ~190 LOC consumer rewrite per CH2 V2. |
| 3F-MIG-003 | 120-260 docs | 4 | Very-high | SK-V14 W6 PRUNE-4 (9 sub-waves) | Receiver: emitted runtime + substrate-doc cleanup + Pattern H census. | Block hand-owned per-grammar runtime; ABROGATE with 9-sub-wave hard cap ≤90 min each (aggregate ≤810 min). |
| 3F-MIG-004 | 80-180 docs/report (doc-only; zero impl tail) | 4 | Medium | Doc-only at this row; W8 re-admit consumer-plane budget accounted at 3C-L01-factstream-fifth-category (60-150 docs per `restart/audit/totality/p3/3C-locks-crystallisation.md:158`) | Receiver: CSS L4 fact-stream telemetry plane (substrate manifest entry); W8 R6 CSS L4 re-admit consumer per 3C-L01-factstream-fifth-category receiver. | Block forcing CSS fact stream into EventTape; bind to fenced telemetry per 3C V1 ACCEPT (`3C-locks-crystallisation.md:32`) + V4-3 hunk (`3C-locks-v+1-diff.md:118`-`140`); cross-reference 3C-L01 budget rather than double-counting. |
| 3F-MIG-005 | 120-260 docs | 5 | High | SK-V14 W7 PRUNE-5 | Receiver: decision-engine wire-up (W8 policy + W9 union SCAFFOLD → LOAD-BEARING). | Abrogate if no candidate generation + eqsat + CSP + active cost + strict evidence. |
| 3F-MIG-006 | 120-260 docs | 5 | High | Lock 16 V+1 primitive manifest + LAC-1E-10 traceability wave | Receiver: per-use-site manifest mapping. | Block source-present primitive retention without wired consumer + scalar delegate + delete OR architectural block. |
| 3F-MIG-007 | 40-100 docs | 3 | Medium | SK-V14 W5 PRUNE-3 + refusal row | Receiver: dispatch trait method + SKELETON-refusal note. | Block SKELETON triple DELETE re-proposal without same-wave consumer + command + first consumer (T-P2 V4 criteria). |
| 3F-HANDOFF-001 | 40-90 docs | 3 | High process | Top-level current override | Receiver: replacement state block. | Block any claim that T-P3 edits V1 surfaces; require SK-V14 LOCK commits cited. |
| 3F-HANDOFF-002 | 60-140 docs | 4 | Medium | Entry-packet + reading-order refresh | Receiver: reading-order list. | Block stale V1.1 / SK-V6 / N-direct state as current authority. |
| 3F-HANDOFF-003 | 80-180 docs | 4 | High process | SK-V14 current skinny state | Receiver: current skinny state + next-move summary. | Block SK-V13 admission counts; require SK-V14 audit-zero baseline cited. |
| 3F-HANDOFF-004 | 80-180 docs | 4 | High process | Concurrency/refusal block | Receiver: dispatch rule. | Block source/gate/RESULTS/REDRESS edits until G3 + CRUD + G-Omega + wave-triumvirate ready. |
| 3F-HANDOFF-005 | 60-140 docs | 3 | Medium-high | Grammar-onboarding language | Receiver: three-surface onboarding wording. | Block two-surface or generic-branch grammar onboarding language; require LAC-1E-08 + LAC-1E-15 cite. |
| 3F-DISPATCH-001 | 100-200 docs | 5 | High process | Next-cycle dispatch | Receiver: G3 → Omega → CRUD → G-Omega → wave-triumvirate W0..W11 → Pass Alpha SK-V15. | Block W0 dispatch from G-Omega-skip; require wave-triumvirate per-wave cap per `[dispatch-hard-cap]`. |

## V1 Gated Open Questions

| lens | question | receiver | blocker | gate |
| --- | --- | --- | --- | --- |
| CH1 | Does the G3 packet include all 3A-3E source-linked deltas needed by Ω-F, or must 3F be revised after sibling artefacts land? | G3 packet owner / Pass Omega Ω-F. | 3F depends on sibling artefact finality at this cycle. | G3 source map must cite all 3A-3F V1 artefacts or route a revise before Omega. |
| CH2 | RESOLVED: LAC-1E-14 lands `FactStream` as 5th admitted-product category at the **Lock 1 SUBSTRATE manifest** (NOT a 6th `BackendShape` variant); the 5-shape `BackendShape` search domain at Lock 10 (`{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`) HOLDS. The two axes (Lock 1 substrate manifest vs Lock 10 BackendShape search domain) are ORTHOGONAL. Forward question: does Pass Omega Ω-C ARCH-CRUD acceptance of the substrate-category-not-shape carrier wording surface any downstream coherence drift in 3A/3B/3D/3E? | Pass Omega Ω-C ARCH-CRUD intake. | Already dispositioned at 3C V1 ACCEPT (`restart/audit/totality/p3/3C-locks-crystallisation.md:32` 3C-L01-factstream-fifth-category) + V4-3 verbatim hunk (`restart/audit/totality/p3/3C-locks-v+1-diff.md:118`-`140`); no pending §3C work. | Ω-C confirms substrate-category-not-shape carrier wording lands cleanly across 3A executive summary + 3B coherence matrix + 3D fold row + 3E L14-HC-07 hardening clause before CRUD-1 §9.2 fold. |
| CH3 | How does W7 `same_substrate_union` enforcement-pass differ from SK-V9 W3 retired retained-class-column-union data structure, and how is the naming proximity disambiguated in MIGRATION row text? | Pass Omega CRUD-4 + 3F-MIG-005. | REDRESS-96/97/98 PERMANENT-PRE-BLOCK history. | MIGRATION row text MUST cite "ENFORCEMENT-LAYER pass NOT data structure" + REDRESS-96/97/98 pre-block per SPEC.md:806. |
| CH4 | Does the 9-sub-wave PRUNE-4 carry per-sub-wave commit-and-halt hard cap, or does the aggregate ≤810 min budget gate at the W6 wave level only? | 3F-MIG-003 + Pass Omega Ω-D + wave-triumvirate dispatcher. | `[dispatch-hard-cap]` 0.9N commit + N halt discipline at per-wave OR per-sub-wave level. | W6 dispatch packet MUST name per-sub-wave hard caps per `restart/skinny/tranches/sk-v14/SPEC.md:243`. |
| CH5 | Does the dispatch_value → dispatch rename leak into runtime hot path via dynamic dispatch overhead, or is the trait monomorphised per grammar at codegen? | 3F-MIG-007 + W5 PRUNE-3 implementer. | Lock 10 cost-model + Lock 15 inline budget. | W5 entry gate cites measurement on `parse_value_at` hot path baseline; regression > +5% blocks admission per LAC-1E-09. |
| CH6 | When does the SKELETON triple DELETE refusal row migrate from MIGRATION to LOCKS as a permanent pre-block, or does it stay as MIGRATION-row refusal indefinitely? | T-P3 §3C disposition + Pass Omega Ω-C. | Lock-amendment singularity per `restart/prompts/totality/PASS-3-SYNTHESIS.md §8.1`. | T-P3 §3C either (a) adds SKELETON-refusal to LOCKS preface + retires MIGRATION row, OR (b) keeps as MIGRATION refusal + cites T-P2 V3 LOCK cohort refutation density 32:69 = 31.7% (`HARDENING-T-P2-V3-CONSOLIDATED.md:76,172,187,295`) as the LOCKS-strengthening evidence per 3D skinny-fold per §8.1. |

## Discipline Citations

- HARD CAP 45 min per agent per `restart/prompts/totality/PASS-3-SYNTHESIS.md §7:200-206`.
- WRITE-ONLY for docs per `restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md §2`.
- T-P3 PROPOSES; Pass Omega CRUD edits per `restart/prompts/totality/PASS-3-SYNTHESIS.md §8.6:215-216` + `restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md §2`.
- Frontmatter per `restart/prompts/totality/PASS-3-SYNTHESIS.md §2.1:59-78`.
- Every claim cites T-P1 inventory or T-P2 dossier or V1-surface path:line per `restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md §2`.
- No new directive / BIR variant / substrate silently synthesised per `restart/prompts/totality/PASS-3-SYNTHESIS.md §8.5:214`.
- Lock 14 binding generalisation discipline per `restart/prompts/totality/PASS-3-SYNTHESIS.md §8.3:212` (3F MIGRATION rows carry non-JSON story; no JSON-narrowing amendment proposed).
- Skinny→totality fold monotonic per `restart/prompts/totality/PASS-3-SYNTHESIS.md §8.4:213` (3F sustains skinny S-P3 V3 LOCK as input to T-P3 fold via 3D consumer; never dictates back).
- 5-shape BackendShape canon coherent across 3A + 3B + 3E per `restart/prompts/totality/PASS-3-SYNTHESIS.md §8.2:211` (3F MIG-004 fact-stream row mirrors 3C V4 hunk V4-3 verbatim: `FactStream` lands as 5th admitted-product category at the Lock 1 SUBSTRATE manifest, NOT a 6th `BackendShape` variant; 5-shape Lock 10 search domain HOLDS; the two axes are orthogonal).
