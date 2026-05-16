# SK-V7 Restructure R1 — `restart/skinny/tranches/` Inventory + Pruning Strategy

Date: 2026-05-16
Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Scope: read-only inspection of `restart/skinny/tranches/` and inbound references from `restart/skinny/INDEX.md`, `restart/HANDOFF.md`, `restart/MASTER-PLAN.md`, `restart/ARCHITECTURE.md`, `restart/locks/LOCKS.md`.

Total surface: 32,844 LOC across 104 markdown files in five subtrees (14 top-level + 15 SK-V5 + 51 SK-V6 + 18 SK-V7 + 6 V9.5-PSI).

---

## §0. Single Most Consequential Proposed Change

**Retire the entire `SK-V6-COHORT/` redress sub-series (`skv6-R*` and `skv6-schema-*`, 33 files, 5,562 LOC) into `archive/sk-v6/cohort-redress/`, and promote only the 12 SK-V6 A/B/C reports (which the SK-V7 cohort actively cites) to remain inline.** This single move evicts 17 % of audit LOC, eliminates the only iteration where the cohort dir mixed primary (A/B/C) and redress (R*, schema-*) artefacts, and re-establishes the V5/V7 invariant that a `*-COHORT/` directory contains exactly one tier of agent reports.

---

## §1. Full Inventory

Total LOC by tier (from `wc -l`):

| Tier | Files | LOC | KB-total |
|---|---|---|---|
| Top-level masters | 14 | 6,293 | ~310 |
| `SK-V5-COHORT/` | 15 | 5,559 | ~232 |
| `SK-V6-COHORT/` | 51 | 11,083 | ~496 |
| `SK-V7-COHORT/` | 18 | 8,060 | ~408 |
| `V9.5-PSI-EXCAVATION/` | 6 | 1,849 | ~136 |
| **Total** | **104** | **32,844** | **~1.58 MB** |

`SK-V6-COHORT/` decomposes into 6 A-reports + 6 B-reports + 6 C-reports + 30 R\*-redress + 3 schema-\* = 51 files. The R\*/schema cluster (33 files, 5,562 LOC) was added mid-iteration during SK-V6 redress dispatches and now structurally dominates the cohort dir.

---

## §2. Per-File Classification — Top-Level (14 files)

| # | File | LOC | Classification | Rationale (cited) |
|---|---|---|---|---|
| 1 | `GRAND-SYNTHESIS-SK-V5.md` | 463 | KEEP-ARCHIVE → `archive/sk-v5/` | `HANDOFF-SK-V5.md:5` self-declares `SUPERSEDED FOR DISPATCH BY SK-V6`; `restart/HANDOFF.md:26` calls it `substrate-history authority` only. Still referenced by `restart/skinny/INDEX.md:168`, REDRESS 50-72. |
| 2 | `GRAND-SYNTHESIS-SK-V6.md` | 1,204 | RENAME-TO `GRAND-SYNTHESIS-SK-V6-WAVE1.md` AND KEEP-ARCHIVE → `archive/sk-v6/` | Self-declares `Wave 1 PLAN artifact` (`GRAND-SYNTHESIS-SK-V6.md:7`). The current SK-V6 synthesis is the `-ASMJSON-DAV1D` variant per `restart/skinny/INDEX.md:6` and `GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md:5-9` (`supplements, rather than replaces, GRAND-SYNTHESIS-SK-V5.md`). Internally still references `skv6-R*` cohort 5 times (only top-level doc to do so). Filename collision with `-ASMJSON-DAV1D` variant invites confusion. |
| 3 | `GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md` | 248 | KEEP-ARCHIVE → `archive/sk-v6/` AS `GRAND-SYNTHESIS-SK-V6.md` | The actual canonical SK-V6 synthesis per `restart/skinny/INDEX.md:6`, `restart/HANDOFF.md:9`. After SK-V7 supersedes dispatch, archive but make this the sole `GRAND-SYNTHESIS-SK-V6.md` in the V6 archive. |
| 4 | `GRAND-SYNTHESIS-SK-V7.md` | 304 | KEEP-CANONICAL | Current dispatch (`HANDOFF-SK-V7.md:13`); cohort attribution at `:5-8`. |
| 5 | `HANDOFF-SK-V5.md` | 285 | KEEP-ARCHIVE → `archive/sk-v5/` | `:5` declares supersession; `restart/HANDOFF.md:28` lists it among "packet history and partial landed state". |
| 6 | `HANDOFF-SK-V6.md` | 94 | KEEP-ARCHIVE → `archive/sk-v6/` | Currently routed via `restart/skinny/INDEX.md:8` and `restart/HANDOFF.md:9` as part of SK-V6 dispatch chain. Superseded by `HANDOFF-SK-V7.md` and SK-V7 reading order at `HANDOFF-SK-V7.md:7-15`. |
| 7 | `HANDOFF-SK-V7.md` | 206 | KEEP-CANONICAL | Active handoff (`:5` says `SK-V7 spec materialized. Wave 0 ready for dispatch`). |
| 8 | `IMPLEMENTATION-AGENT-PROMPT-SK-V5.md` | 347 | DELETE | `IMPLEMENTATION-AGENT-PROMPT-SK-V6.md:5-7` self-declares `single source of dispatch authority` superseding SK-V5; no V7 cohort cites this file (`grep` returns zero hits in `SK-V7-COHORT/`). The packet is preserved separately. |
| 9 | `IMPLEMENTATION-AGENT-PROMPT-SK-V6.md` | 502 | KEEP-ARCHIVE → `archive/sk-v6/` | Active SK-V6 dispatch authority per `restart/HANDOFF.md:25`. After SK-V7 lands its own prompt (not yet present at top level — gap noted §10), this archives. Still cited by `restart/skinny/INDEX.md:166` and by REDRESS. |
| 10 | `IMPLEMENTATION-PACKET-SK-V5.md` | 868 | KEEP-ARCHIVE → `archive/sk-v5/` | 7-wave SK-V5 packet (`restart/skinny/INDEX.md:169`); waves 0-7 enumerated. Substrate-history value only — `HANDOFF-SK-V5.md:5` superseded. |
| 11 | `IMPLEMENTATION-PACKET-SK-V6-SOTA-RECOVERY.md` | 288 | KEEP-ARCHIVE → `archive/sk-v6/` AS `IMPLEMENTATION-PACKET-SK-V6.md` | Current SK-V6 packet per `restart/skinny/INDEX.md:7`. The `-SOTA-RECOVERY` suffix was a scope qualifier during the renaming-for-clarity pass; archive form should normalize the name. |
| 12 | `IMPLEMENTATION-PACKET-SK-V7.md` | 437 | KEEP-CANONICAL | Active packet per `HANDOFF-SK-V7.md:14`. |
| 13 | `NUKE-PLAN-SK-V5.md` | 497 | KEEP-ARCHIVE → `archive/sk-v5/` | SK-V5-specific nuke catalogue (`:5-7` cites SK-V5 grand synthesis + A4/B2/D4/D5/D6 cohort reports). `restart/skinny/INDEX.md:170` calls it `SK-V5 nuke catalogue (476 LOC; 16 sections): decisions recorded in Wave 0, deletions land in Wave 4`. The deletions have landed; the doc is now historical. SK-V6 and SK-V7 did not author equivalents — they performed in-line nukes via REDRESS entries instead. |
| 14 | `SOTA-BEAT-DESIGN.md` | 550 | KEEP-CANONICAL AND MOVE-TO `design/SOTA-BEAT-DESIGN.md` | Self-declared `HISTORICAL DESIGN INPUT, superseded for dispatch by IMPLEMENTATION-PACKET-SK-V6-SOTA-RECOVERY.md` (`SOTA-BEAT-DESIGN.md:3-4`), but lists "Still-live material: structural-index-driven lowering, generated SinkOnly, bbnf-simd primitive vocabulary, and the x86 CollapsedStage / asmjson research shape" (`:5-7`). Cited from outside the audit dir by `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/HANDOFF.md:90`, `restart/locks/LOCKS.md`, and inside the dir by 12+ files including V9.5 excavation. Authority spans iterations → cross-iteration `design/` subdir is the natural home. See §5. |

---

## §3. Per-File Classification — `SK-V5-COHORT/` (15 files, 5,559 LOC)

The cohort is referenced by `restart/HANDOFF.md:27-28`, by `restart/skinny/INDEX.md:172`, and cross-cited 19 times from `SK-V6-COHORT/` and 22 times from `SK-V7-COHORT/` (e.g. `SK-V7-COHORT/skv7-A4-parse-that-gaps.md` cites `skv5-A3` directly; `skv7-A2-sota-strict-beat.md` cites SK-V5 cohort once). Every file remains live evidence.

| File | LOC | Classification | Rationale |
|---|---|---|---|
| `skv5-A1-comparative.md` | 495 | KEEP-ARCHIVE → `archive/sk-v5/cohort/` | Cited by SK-V7 cohort A-reports (e.g. `skv7-A2`); foundational asmjson/simdjson/yyjson/sonic-rs comparison. |
| `skv5-A2-dav1d-process.md` | 903 | KEEP-ARCHIVE → `archive/sk-v5/cohort/` | Largest cohort file. The dav1d-process material is now folded into `GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md`; SK-V7 cites its conclusions via the synthesis, not the raw report (no `skv5-A2` hits in `SK-V7-COHORT/`). |
| `skv5-A3-parse-that-gaps.md` | 482 | KEEP-ARCHIVE → `archive/sk-v5/cohort/` | `SK-V7-COHORT/skv7-A4-parse-that-gaps.md:176-543` cites successors `skv6-R*e` rather than `skv5-A3`; archived but inbound chain (`A3 → skv6-A* → skv7-A4`) is intact. |
| `skv5-A4-tape-union-audit.md` | 311 | KEEP-ARCHIVE → `archive/sk-v5/cohort/` | Cited by `NUKE-PLAN-SK-V5.md:5-7` as nuke authority for tape-union deletions; deletions landed. |
| `skv5-A5-grammar-generalization.md` | 361 | KEEP-ARCHIVE → `archive/sk-v5/cohort/` | Cited by `restart/skinny/INDEX.md:172`; grammar-generalization findings folded into V1 architecture. |
| `skv5-A6-research-ledger.md` | 624 | KEEP-ARCHIVE → `archive/sk-v5/cohort/` | `:1-7` declares it the V9.2 → V9.5 → SK-V4 consolidation ledger. Authority for the validated/invalidated table. Cited by V7 A6 successor. |
| `skv5-B1-parse-attribution.md` | 333 | KEEP-ARCHIVE → `archive/sk-v5/cohort/` | Authority for parse-attribution feature flag landing (REDRESS history). |
| `skv5-B2-direct-attribution.md` | 445 | KEEP-ARCHIVE → `archive/sk-v5/cohort/` | Source of the SK-V5 Eisel-Lemire/UTF-8 close-route hypothesis (later refuted by REDRESS 50-55 per `IMPLEMENTATION-AGENT-PROMPT-SK-V6.md:7-10`). |
| `skv5-B3-native-sidecars.md` | 424 | KEEP-ARCHIVE → `archive/sk-v5/cohort/` | Native sidecar strictness ledger; cited by `SK-V7-COHORT/skv7-A1-comparator-repair.md:185` via successor `skv6-R5`. |
| `skv5-D1-eisel-novelty.md` | 165 | KEEP-ARCHIVE → `archive/sk-v5/cohort/` | Novelty audit for Eisel-Lemire vendoring (landed in REDRESS history). |
| `skv5-D2-utf8-novelty.md` | 89 | KEEP-ARCHIVE → `archive/sk-v5/cohort/` | Refuted-route record. Per memory `redispatch-empty-return` + V7 cohort: keep for negative-evidence value. |
| `skv5-D3-derive-shape-novelty.md` | 138 | KEEP-ARCHIVE → `archive/sk-v5/cohort/` | Foundational for `BackendShape` derive landing. |
| `skv5-D4-simd-split-novelty.md` | 165 | KEEP-ARCHIVE → `archive/sk-v5/cohort/` | Cited by `NUKE-PLAN-SK-V5.md` for `simd-scan` crate deletion. |
| `skv5-D5-sinkonly-novelty.md` | 299 | KEEP-ARCHIVE → `archive/sk-v5/cohort/` | Generated `SinkOnly` novelty audit (landed); referenced from REDRESS chain. |
| `skv5-D6-class-ab-novelty.md` | 325 | KEEP-ARCHIVE → `archive/sk-v5/cohort/` | Class A/B primitive admission audit; SK-V7 A4 inherits the framework. |

Verdict: keep ALL 15 SK-V5 cohort files; move ALL to `archive/sk-v5/cohort/`. No deletions, no consolidations — the inbound citation graph from SK-V6/V7 cohorts is too dense to flatten.

---

## §4. Per-File Classification — `SK-V6-COHORT/` (51 files, 11,083 LOC)

The cohort has three tiers:

- **A/B/C tier (18 files, 5,521 LOC)** — original 12-agent + later 6-agent SK-V6 dispatch reports.
- **R\* tier (30 files, 4,791 LOC)** — redress sub-series (R1, R1b-g, R2, R2b-g, R3, R3b-g, R4, R4b-c, R5, R5b-c, R6, R6b-c) authored during SK-V6 redress dispatches.
- **schema-\* tier (3 files, 591 LOC)** — output-schema sub-series authored during SK-V6 Wave 3 typed-directbuild work.

### §4.1 A/B/C tier (canonical SK-V6 cohort)

| File | LOC | Classification | Rationale |
|---|---|---|---|
| `skv6-A1-asmjson-generalization.md` | 318 | KEEP-ARCHIVE → `archive/sk-v6/cohort/` | Cited by `SK-V7-COHORT/skv7-A3-dav1d-esoterica.md`. |
| `skv6-A2-dav1d-asm-process.md` | 506 | KEEP-ARCHIVE → `archive/sk-v6/cohort/` | Cited by `SK-V7-COHORT/skv7-A3`. Superseded for dispatch by `GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md`. |
| `skv6-A3-comparator-planes.md` | 585 | KEEP-ARCHIVE → `archive/sk-v6/cohort/` | Cited by `SK-V7-COHORT/skv7-A1-comparator-repair.md`. |
| `skv6-A4-history-validated-invalidated.md` | 431 | KEEP-ARCHIVE → `archive/sk-v6/cohort/` | Cited by `SK-V7-COHORT/skv7-A6`. |
| `skv6-A5-general-grammar-abstraction.md` | 456 | KEEP-ARCHIVE → `archive/sk-v6/cohort/` | Cited by `SK-V7-COHORT/skv7-A5-lock-audit.md`. |
| `skv6-A6-host-asm-instruction-map.md` | 360 | KEEP-ARCHIVE → `archive/sk-v6/cohort/` | Cited by `SK-V7-COHORT/skv7-A3`, `skv7-A4`. |
| `skv6-B1-asmjson-challenge.md` | 214 | KEEP-ARCHIVE → `archive/sk-v6/cohort/` | Cites `SOTA-BEAT-DESIGN.md`; targeted asmjson challenge. |
| `skv6-B2-checkasm-hardening-plan.md` | 512 | KEEP-ARCHIVE → `archive/sk-v6/cohort/` | Cited by `SK-V7-COHORT/skv7-B6-checkasm-hardening.md` as its predecessor. |
| `skv6-B3-profile-retained-three-way.md` | 87 | KEEP-ARCHIVE → `archive/sk-v6/cohort/` | Profile baseline; cited by V7 C1. |
| `skv6-B4-profile-direct-three-way.md` | 60 | KEEP-ARCHIVE → `archive/sk-v6/cohort/` | Profile baseline. |
| `skv6-B5-primitive-gap-inventory.md` | 92 | KEEP-ARCHIVE → `archive/sk-v6/cohort/` | Primitive inventory; cited by `SK-V7-COHORT/skv7-A4-parse-that-gaps.md`. |
| `skv6-B6-spec-edit-map.md` | 874 | KEEP-ARCHIVE → `archive/sk-v6/cohort/` | Spec-edit map fed `restart/skinny/INDEX.md` and `restart/HANDOFF.md` edits; cited by V7 A5. |
| `skv6-C1-retained-profile.md` | 130 | KEEP-ARCHIVE → `archive/sk-v6/cohort/` | Profile artifact; cited by V7 C1. |
| `skv6-C2-direct-profile.md` | 153 | KEEP-ARCHIVE → `archive/sk-v6/cohort/` | Profile artifact; cited by V7 C2. |
| `skv6-C3-sidecar-planes.md` | 85 | KEEP-ARCHIVE → `archive/sk-v6/cohort/` | Sidecar planes; cited by V7 A1. |
| `skv6-C4-host-asm-profile.md` | 299 | KEEP-ARCHIVE → `archive/sk-v6/cohort/` | Cited by `SK-V7-COHORT/skv7-C4-pmu.md:253` as the host-ASM precedent. |
| `skv6-C5-parse-that-gaps.md` | 69 | KEEP-ARCHIVE → `archive/sk-v6/cohort/` | Parse-that-regex gap snapshot; cited by V7 A4. |
| `skv6-C6-generality-costfacts.md` | 146 | KEEP-ARCHIVE → `archive/sk-v6/cohort/` | CostFacts ledger; cited by `SK-V7-COHORT/skv7-A6-ledger-generalization.md` as the lineage origin (`A6` records CostFacts substrate absent — see `HANDOFF-SK-V7.md` re A6). |

### §4.2 R\* tier (SK-V6 redress sub-series)

The R-series (R1 through R6, with R1b-g, R2b-g, R3b-g, R4b-c, R5b-c, R6b-c suffixes) was authored as the cohort grew mid-iteration. Per the `new-tranche-new-doc` memory convention this is exactly the pattern that should have opened a new SK letter. Now it lives wrong-tier.

Inbound citations from outside `SK-V6-COHORT/`:
- `GRAND-SYNTHESIS-SK-V6.md:301-302, 361, 429-430` (5 hits) — the only top-level doc citing them.
- `SK-V7-COHORT/skv7-A4-parse-that-gaps.md` cites `skv6-R1b`, `skv6-R1e`, `skv6-R2c`, `skv6-R2e`, `skv6-R3`, `skv6-R3e` (10 hits at `:176-543`).
- `SK-V7-COHORT/skv7-C4-pmu.md:253` cites `skv6-R6` and `skv6-R6c`.
- `SK-V7-COHORT/skv7-A1-comparator-repair.md:185` cites `skv6-R5`.

Verdict per file:

| File group | Files | LOC | Classification | Rationale |
|---|---|---|---|---|
| `skv6-R1-parse-regressed.md` + R1b-g | 7 | 1,400 | KEEP-ARCHIVE → `archive/sk-v6/cohort-redress/` | R1, R1b, R1e cited from V7 cohort A4; cluster integrity favored. |
| `skv6-R2-parse-original-g.md` + R2b-g | 7 | 1,397 | KEEP-ARCHIVE → `archive/sk-v6/cohort-redress/` | R2c, R2e cited from V7 cohort A4. |
| `skv6-R3-direct-attribution.md` + R3b-g | 7 | 942 | KEEP-ARCHIVE → `archive/sk-v6/cohort-redress/` | R3, R3e cited from V7 cohort A4. |
| `skv6-R4-skv4-diff.md` + R4b-c | 3 | 312 | KEEP-ARCHIVE → `archive/sk-v6/cohort-redress/` | Only intra-V6 references; SK-V4 diff value is historical. |
| `skv6-R5-sidecar-refresh.md` + R5b-c | 3 | 510 | KEEP-ARCHIVE → `archive/sk-v6/cohort-redress/` | R5 cited from V7 A1. |
| `skv6-R6-icache-branch.md` + R6b-c | 3 | 760 | KEEP-ARCHIVE → `archive/sk-v6/cohort-redress/` | R6, R6c cited from V7 C4. |

Net: 30 files (4,791 LOC) move to `archive/sk-v6/cohort-redress/`. No deletions (citations preserve them). The structural fix is separation: A/B/C primaries in `cohort/`, R/schema in `cohort-redress/`.

### §4.3 schema-\* tier (3 files, 591 LOC)

Cited only from `GRAND-SYNTHESIS-SK-V6.md:1040-1046` and `skv6-A1:297`. No inbound from V7.

| File | LOC | Classification | Rationale |
|---|---|---|---|
| `skv6-schema-A-output-schema-boundary.md` | 103 | KEEP-ARCHIVE → `archive/sk-v6/cohort-redress/` | Output-schema boundary fact; quoted in synthesis. |
| `skv6-schema-B-generated-typed-directbuild.md` | 318 | KEEP-ARCHIVE → `archive/sk-v6/cohort-redress/` | Typed-directbuild redress facts. |
| `skv6-schema-C-redress-gates.md` | 170 | KEEP-ARCHIVE → `archive/sk-v6/cohort-redress/` | Redress gates; landed verbatim into REDRESS. |

---

## §5. Per-File Classification — `SK-V7-COHORT/` (18 files, 8,060 LOC)

The active iteration. `HANDOFF-SK-V7.md:15` routes the agent here. Every file is canonical evidence; none are eligible for archive.

| File | LOC | Classification | Rationale |
|---|---|---|---|
| `skv7-A1-comparator-repair.md` | 334 | KEEP-CANONICAL | Cited by `GRAND-SYNTHESIS-SK-V7.md`, `HANDOFF-SK-V7.md` (sonic-rs flaw-probe verdict). |
| `skv7-A2-sota-strict-beat.md` | 482 | KEEP-CANONICAL | SOTA-beat shape; cites SK-V5/V6 lineage. |
| `skv7-A3-dav1d-esoterica.md` | 239 | KEEP-CANONICAL | DAV1D process residual research. |
| `skv7-A4-parse-that-gaps.md` | 588 | KEEP-CANONICAL | Largest V7 cohort file; primary parse-that gap inventory; cites 31 SK-V6 cohort items. |
| `skv7-A5-lock-audit.md` | 463 | KEEP-CANONICAL | Lock 14 audit (46 HIGH leaks per `HANDOFF-SK-V7.md`). |
| `skv7-A6-ledger-generalization.md` | 463 | KEEP-CANONICAL | CostFacts substrate absent finding (per `HANDOFF-SK-V7.md`). |
| `skv7-B1-uxxxx-tbl.md` | 559 | KEEP-CANONICAL | `\uXXXX` table primitive design; per-row applicability finding (`HANDOFF-SK-V7.md` B1). |
| `skv7-B2-costfacts.md` | 528 | KEEP-CANONICAL | CostFacts substrate design. |
| `skv7-B3-lock14-sequence.md` | 400 | KEEP-CANONICAL | Lock 14 remediation sequence. |
| `skv7-B4-stub-lowering.md` | 457 | KEEP-CANONICAL | Stub-lowering remediation. |
| `skv7-B5-mesh-typed.md` | 502 | KEEP-CANONICAL | Mesh typed DirectBuild remediation (`HANDOFF-SK-V7.md`: mesh DirectBuild blocked by `DirectTypeRef::Vec` shape-blindness). |
| `skv7-B6-checkasm-hardening.md` | 454 | KEEP-CANONICAL | Successor to `skv6-B2`. |
| `skv7-C1-parse-profile.md` | 399 | KEEP-CANONICAL | Per-`\uXXXX` hypothesis applies to 2 of 4 rows (C1 critical correction per `HANDOFF-SK-V7.md`). |
| `skv7-C2-direct-profile.md` | 398 | KEEP-CANONICAL | Eisel-Lemire 5.2 % of mesh cost. |
| `skv7-C3-typed-profile.md` | 446 | KEEP-CANONICAL | twitter 151.5 % skip-work finding. |
| `skv7-C4-pmu.md` | 370 | KEEP-CANONICAL | PMU blocked on M5 Max → static disassembly fall-back. |
| `skv7-C5-correlation.md` | 520 | KEEP-CANONICAL | Cohort correlation synthesis. |
| `skv7-C6-sidecars.md` | 458 | KEEP-CANONICAL | Sidecar truth ledger. |

Verdict: full retention. The cohort is symmetric (6 A + 6 B + 6 C, no R/schema overflow), validating the "open a new SK letter for scope pivots" rule.

---

## §6. `V9.5-PSI-EXCAVATION/` (6 files, 1,849 LOC) — Special Case

Date: 2026-05-12 (per `V9.5-PSI-EXCAVATION/01-git-history.md:3`). Cross-iteration archaeology of the pre-skinny "PSI + DTA + columnar substrate" era (~572 tranche-tagged commits, Era V, per `02-archive-deep-read.md:3-6`).

Inbound citations:
- `HANDOFF-SK-V7.md`, `IMPLEMENTATION-AGENT-PROMPT-SK-V5.md`, `IMPLEMENTATION-AGENT-PROMPT-SK-V6.md`, `IMPLEMENTATION-PACKET-SK-V5.md`, `SOTA-BEAT-DESIGN.md` — all 5 inbound from active-or-archive docs.
- `SK-V7-COHORT/skv7-A2-sota-strict-beat.md`, `skv7-A6-ledger-generalization.md`, `skv7-A3-dav1d-esoterica.md`, `skv7-B4-stub-lowering.md`, `skv7-C4-pmu.md` — 5 V7 cohort docs.
- `SK-V6-COHORT/skv6-B1-asmjson-challenge.md` — 1 hit.

Per-file:

| File | LOC | Classification | Rationale |
|---|---|---|---|
| `01-git-history.md` | 187 | KEEP-CANONICAL AND MOVE-TO `excavation/v9.5-psi/` | Commit census (3,005 commits); inbound from `06-go-no-go-synthesis.md:7`. |
| `02-archive-deep-read.md` | 492 | KEEP-CANONICAL AND MOVE-TO `excavation/v9.5-psi/` | Verbatim path:line filesystem archaeology. |
| `03-failure-anatomy.md` | 360 | KEEP-CANONICAL AND MOVE-TO `excavation/v9.5-psi/` | 5+1 named failure modes; cites `SOTA-BEAT-DESIGN.md` precedents. |
| `04-skv3-vs-psi-diff.md` | 125 | KEEP-CANONICAL AND MOVE-TO `excavation/v9.5-psi/` | Claim-by-claim architectural diff; UNDETERMINED entries flagged (`:4`). |
| `05-fsm-correctness.md` | 353 | KEEP-CANONICAL AND MOVE-TO `excavation/v9.5-psi/` | Direct critique of `SOTA-BEAT-DESIGN.md` §5/§5.1. |
| `06-go-no-go-synthesis.md` | 332 | KEEP-CANONICAL AND MOVE-TO `excavation/v9.5-psi/` | Synthesis of 01-05 with 5/6 companion reports landed (`:5-12`). |

Verdict: keep ALL 6 as a cohesive unit. Move to `excavation/v9.5-psi/` (lowercase, cross-iteration scope). Consolidation rejected — `06-synthesis` already plays that role and the underlying 5 reports are cited from V7 cohort by raw path. Do not collapse; do retire from top-level into `excavation/`.

---

## §7. `SOTA-BEAT-DESIGN.md` — Special Case (550 LOC)

Status: HISTORICAL DESIGN INPUT (per `:3-4`), but **not superseded** for its surviving sections. Cross-iteration authority — cited by:

- `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/HANDOFF.md:90`, `restart/locks/LOCKS.md` (4 cites from V1 spec layer).
- `restart/skinny/INDEX.md:167` (skinny gateway).
- `GRAND-SYNTHESIS-SK-V5.md`, all 6 `V9.5-PSI-EXCAVATION/` files, 6 `SK-V5-COHORT/` files, 2 `SK-V6-COHORT/` files, 2 `SK-V7-COHORT/` files (16 cites from audit).

Verdict: KEEP-CANONICAL AND MOVE-TO `design/SOTA-BEAT-DESIGN.md`. The doc spans iterations and is referenced from V1 spec — the cross-iteration `design/` subdir is the correct lifecycle. Rationale: a top-level slot is reserved for current-iteration dispatch; SOTA-BEAT-DESIGN is permanent-or-evolving design context, not current dispatch.

The 16 inbound citations from inside `audit/` plus 4 from outside `audit/` must all retarget. Citation rewrite is mechanical (one path swap).

---

## §8. Per-Iteration Master Doc Audit

| Iteration | GRAND-SYNTHESIS | IMPLEMENTATION-PACKET | HANDOFF | NUKE-PLAN | AGENT-PROMPT | Status |
|---|---|---|---|---|---|---|
| SK-V3 | absent | absent | absent | absent | absent | Pre-skinny-audit; superseded entirely, no top-level docs. |
| SK-V4 | absent | absent | absent | absent | absent | Mid-iteration deletion (`IMPLEMENTATION-AGENT-PROMPT-SK-V6.md:144` lists `IMPLEMENTATION-PACKET-V2.md` as deleted Pre-SK-V3 packet). |
| SK-V5 | `GRAND-SYNTHESIS-SK-V5.md` (463) | `IMPLEMENTATION-PACKET-SK-V5.md` (868) | `HANDOFF-SK-V5.md` (285) | `NUKE-PLAN-SK-V5.md` (497) | `IMPLEMENTATION-AGENT-PROMPT-SK-V5.md` (347) | Superseded by SK-V6 for dispatch (`HANDOFF-SK-V5.md:5`). Substrate-history authority. |
| SK-V6 | `GRAND-SYNTHESIS-SK-V6.md` (1204, Wave 1) AND `GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md` (248, canonical per INDEX) | `IMPLEMENTATION-PACKET-SK-V6-SOTA-RECOVERY.md` (288) | `HANDOFF-SK-V6.md` (94) | absent | `IMPLEMENTATION-AGENT-PROMPT-SK-V6.md` (502) | Active dispatch per `INDEX.md`/`HANDOFF.md`; superseded by SK-V7 per `HANDOFF-SK-V7.md:5`. Two GRAND-SYNTHESIS files is anomalous — see §10. |
| SK-V7 | `GRAND-SYNTHESIS-SK-V7.md` (304) | `IMPLEMENTATION-PACKET-SK-V7.md` (437) | `HANDOFF-SK-V7.md` (206) | absent | **absent** | Current. Missing AGENT-PROMPT is a real gap. |

Recommendations:

- SK-V5 master suite (5 files, 2,460 LOC): retire to `archive/sk-v5/`. Substrate-history value preserved.
- SK-V6 master suite (5 files, 2,336 LOC): retire to `archive/sk-v6/`. Two-GRAND-SYNTHESIS pathology fixed at archive-time by renaming the 1204-LOC Wave 1 doc to `GRAND-SYNTHESIS-SK-V6-WAVE1.md` and promoting `-ASMJSON-DAV1D` to the singular `GRAND-SYNTHESIS-SK-V6.md` (per its own self-declared role).
- SK-V7 master suite: keep top-level. Add `IMPLEMENTATION-AGENT-PROMPT-SK-V7.md` to close the missing-AGENT-PROMPT gap (orchestrator dispatch chain currently routes through `restart/prompts/pass-contracts/PASS-ALPHA.md` + `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` per `HANDOFF-SK-V7.md:9-11`, which may make a dedicated AGENT-PROMPT redundant — confirm before authoring).
- NUKE-PLAN convention: SK-V6 and SK-V7 absorbed nuke decisions into REDRESS instead of standalone files. Do not back-author NUKE-PLAN-SK-V7; the REDRESS ledger is the equivalent.

---

## §9. Cohort Dir Audit Summary

| Cohort | Files | LOC | Tier purity | Inbound from next iteration | Recommendation |
|---|---|---|---|---|---|
| `SK-V5-COHORT/` | 15 | 5,559 | Clean: 9 A/B + 6 D-novelty | 19 cites from V6, 22 from V7 | KEEP-ARCHIVE → `archive/sk-v5/cohort/` |
| `SK-V6-COHORT/` | 51 | 11,083 | **MIXED**: 18 A/B/C + 30 R\* + 3 schema-\* | V7 cites 12 A/B/C primaries + 13 R-series + 0 schema | SPLIT-INTO-2: `archive/sk-v6/cohort/` (18 files) + `archive/sk-v6/cohort-redress/` (33 files) |
| `SK-V7-COHORT/` | 18 | 8,060 | Clean: 6 A + 6 B + 6 C | n/a (current) | KEEP-CANONICAL → `current/cohort/` |

SK-V6 is the only mixed-tier cohort and the only one over 5,000 LOC. The split is mechanical.

---

## §10. Naming Consistency Proposal

Current top-level masters: `{ROLE}-SK-V{n}.md` and `{ROLE}-SK-V{n}-{VARIANT}.md`. Two anomalies:

- `GRAND-SYNTHESIS-SK-V6.md` + `GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md` — same role, different scope-qualifiers; INDEX routes through the qualified one.
- `IMPLEMENTATION-PACKET-SK-V6-SOTA-RECOVERY.md` — qualified role.

Proposed convention (mechanical, minimal-rename):

1. **Top-level masters**: keep `{ROLE}-SK-V{n}.md` exactly. No periods, no lowercase prefixes — the existing pattern is consistent with `restart/HANDOFF.md` style.
2. **Iteration variants**: when a sub-variant is the canonical for the iteration, archive-time normalization promotes it to the unqualified name and renames the legacy Wave-1 doc with a `-WAVE1` suffix. Per §8, this fixes the two-GRAND-SYNTHESIS-SK-V6 anomaly.
3. **Cohort dirs**: keep `SK-V{n}-COHORT/` capitalization at top-level; lowercase to `cohort/` at archive-time (the archive parent encodes the iteration).
4. **Cohort files**: keep `skv{n}-{Tier}{N}{suffix}-{topic}.md` — already consistent and grep-friendly.
5. **Cross-iteration dirs**: lowercase, scope-named (`design/`, `excavation/v9.5-psi/`).

This is a minimal lift: only 3 top-level renames are required at archive-time (V6 Wave1, V6 packet rename, V6 ASMJSON promotion), and the cohort-file scheme is unchanged.

---

## §11. Proposed Restructure

```
restart/skinny/tranches/
├── current/                                        (SK-V7, ~8,800 LOC)
│   ├── GRAND-SYNTHESIS-SK-V7.md                    304
│   ├── IMPLEMENTATION-PACKET-SK-V7.md              437
│   ├── HANDOFF-SK-V7.md                            206
│   └── cohort/                                     (18 files, 8,060 LOC)
│       ├── skv7-A1-comparator-repair.md
│       ├── ... (A1-A6, B1-B6, C1-C6)
│       └── skv7-C6-sidecars.md
│
├── archive/                                        (SK-V5 + SK-V6, ~19,400 LOC)
│   ├── sk-v5/
│   │   ├── GRAND-SYNTHESIS-SK-V5.md                463
│   │   ├── IMPLEMENTATION-PACKET-SK-V5.md          868
│   │   ├── HANDOFF-SK-V5.md                        285
│   │   ├── NUKE-PLAN-SK-V5.md                      497
│   │   └── cohort/                                 (15 files, 5,559 LOC)
│   │       └── skv5-{A1..A6,B1..B3,D1..D6}-*.md
│   │
│   └── sk-v6/
│       ├── GRAND-SYNTHESIS-SK-V6.md                248  ← was -ASMJSON-DAV1D
│       ├── GRAND-SYNTHESIS-SK-V6-WAVE1.md          1204 ← was unqualified
│       ├── IMPLEMENTATION-PACKET-SK-V6.md          288  ← was -SOTA-RECOVERY
│       ├── HANDOFF-SK-V6.md                         94
│       ├── IMPLEMENTATION-AGENT-PROMPT-SK-V6.md    502
│       ├── cohort/                                 (18 files, 5,521 LOC)
│       │   └── skv6-{A1..A6,B1..B6,C1..C6}-*.md
│       └── cohort-redress/                         (33 files, 5,562 LOC)
│           ├── skv6-R{1..6}{,b..g}-*.md
│           └── skv6-schema-{A,B,C}-*.md
│
├── design/                                         (cross-iteration, 550 LOC)
│   └── SOTA-BEAT-DESIGN.md
│
└── excavation/                                     (pre-skinny, 1,849 LOC)
    └── v9.5-psi/
        ├── 01-git-history.md
        ├── 02-archive-deep-read.md
        ├── 03-failure-anatomy.md
        ├── 04-skv3-vs-psi-diff.md
        ├── 05-fsm-correctness.md
        └── 06-go-no-go-synthesis.md
```

Rationale: four lifecycle classes — current (active dispatch), archive (per-iteration historical), design (cross-iteration permanent), excavation (pre-skinny archaeology). The triumvirate `current/`-`archive/`-`design/`-`excavation/` matches the four use-cases for audit material: dispatch, history, design context, root-cause reference.

Variant rejected: a flat `iterations/sk-v5/`, `iterations/sk-v6/`, `iterations/sk-v7/` layout would treat current and archive symmetrically. Rejected because the "current" iteration is read-write while archives are read-only; the lifecycle distinction is operationally meaningful (different write permissions in CI, different grep defaults).

---

## §12. Pruning Summary

Baseline (BEFORE):
- 32,844 LOC across 104 files.
- 5 top-level directories at `restart/skinny/tranches/`.

Proposed (AFTER):
- 32,497 LOC across 103 files (single deletion: `IMPLEMENTATION-AGENT-PROMPT-SK-V5.md`, 347 LOC).
- 4 top-level directories: `current/`, `archive/`, `design/`, `excavation/`.

| Operation | Count | LOC |
|---|---|---|
| **Deleted** | 1 | 347 |
| **Moved (top-level → `current/`)** | 3 | 947 |
| **Moved (top-level → `archive/sk-v5/`)** | 4 | 2,113 |
| **Moved (top-level → `archive/sk-v6/`)** | 5 | 2,336 |
| **Moved (top-level → `design/`)** | 1 | 550 |
| **Moved (`SK-V5-COHORT/` → `archive/sk-v5/cohort/`)** | 15 | 5,559 |
| **Moved (`SK-V6-COHORT/{A,B,C}` → `archive/sk-v6/cohort/`)** | 18 | 5,521 |
| **Moved (`SK-V6-COHORT/{R*,schema-*}` → `archive/sk-v6/cohort-redress/`)** | 33 | 5,562 |
| **Moved (`SK-V7-COHORT/` → `current/cohort/`)** | 18 | 8,060 |
| **Moved (`V9.5-PSI-EXCAVATION/` → `excavation/v9.5-psi/`)** | 6 | 1,849 |
| **Renamed (archive-time normalization)** | 3 | n/a |

Renames:
- `GRAND-SYNTHESIS-SK-V6.md` → `GRAND-SYNTHESIS-SK-V6-WAVE1.md`
- `GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md` → `GRAND-SYNTHESIS-SK-V6.md` (in `archive/sk-v6/`)
- `IMPLEMENTATION-PACKET-SK-V6-SOTA-RECOVERY.md` → `IMPLEMENTATION-PACKET-SK-V6.md` (in `archive/sk-v6/`)

Totals:
- **Files deleted**: 1
- **Files moved**: 103
- **Files renamed**: 3
- **LOC before**: 32,844
- **LOC after**: 32,497
- **Net LOC reduction**: 347 (1.06 %)

The pruning intent is structural (separate lifecycle classes, separate primary cohort from redress cohort), not LOC-reduction. The single deletion (`IMPLEMENTATION-AGENT-PROMPT-SK-V5.md`) is the only doc whose authority is wholly superseded with no inbound citations from any active or archive doc beyond INDEX-style listings.

---

## §13. Inbound Citation Rewrite Burden

Every move requires a path update in inbound citations. Counts:

| Source doc | Citation count touched |
|---|---|
| `restart/skinny/INDEX.md` | ~20 paths |
| `restart/HANDOFF.md` | ~10 paths |
| `restart/MASTER-PLAN.md` | ~3 paths |
| `restart/ARCHITECTURE.md` | ~2 paths |
| `restart/locks/LOCKS.md` | ~2 paths |
| `skinny/REDRESS.md` | ~30 paths (uncounted; large) |
| `skinny/RESULTS.md` | ~5 paths |
| Intra-audit citations | ~150 paths (one-time rewrite) |

Total mechanical edits: ~220 path swaps. None are content-changes. A single regex-driven `sd`/`sed -i` pass per source-to-target mapping handles each class.

The rewrite must precede the move to keep `git log --follow` history intact: rename-with-content-change defeats rename detection above the 50 % similarity threshold. Recommended sequence:
1. Path-rewrite pass on inbound docs (no file moves yet).
2. `git mv` files into new layout.
3. Verify commit shows only renames + path-string edits in the inbound docs.

---

## §14. Open Questions (Not Addressed In R1)

- Whether `IMPLEMENTATION-AGENT-PROMPT-SK-V7.md` should exist at all, given `HANDOFF-SK-V7.md:7-15` routes via `restart/prompts/pass-contracts/PASS-ALPHA.md` + `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`. If those `restart/prompts/` files now serve the AGENT-PROMPT role, the V5/V6 inline prompt pattern is obsolete and the V6 AGENT-PROMPT archives without a V7 successor.
- Whether `archive/sk-v6/GRAND-SYNTHESIS-SK-V6-WAVE1.md` (1204 LOC) should be partially redacted to its load-bearing diagnoses (3.A retained string-wrapper cluster, etc.) and the rest archived as raw cohort excerpts. R1 keeps it whole; an R2 pass could measure how much of the 1204 LOC is verbatim from `skv6-R*` reports.
- Whether `SOTA-BEAT-DESIGN.md:5-7` "still-live material" should be migrated into a current SK-V7 design doc, leaving `SOTA-BEAT-DESIGN.md` itself as a pure historical record. R1 keeps it cross-iteration in `design/`.

---

End R1.
