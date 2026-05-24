# Ω-A Coherence Audit — Pass Omega V2 (T-P3 V4-LOCKED apply)

Pass: Pass Omega V2. Source: T-P3 V4 LOCK at commit `69eea1c5c`.
Date: 2026-05-24. Receiver: ARCH-CRUD-1.
Authority: `restart/audit/totality/p3/3A-architecture-synthesis.md` +
`restart/audit/totality/p3/3E-grammar-generalisation.md` +
post-CRUD-3 `restart/locks/LOCKS.md` state (commit `85a043224`; 779 lines;
16 locks; 9 V4-NEW hunks applied).

## Application Summary

- 12 3A deltas applied (ARCH-3A-D01..D12); 0 skipped; 0 abrogated.
- 12 3E deltas applied (3E-D01..D12); folded into §7.3 (BackendShape
  matrix + primitive vocabulary transfer), §12 (7-step onboarding +
  L14-HC clauses), and §13.1 (lint manifest cross-reference).
- D06 Part (b) Ω-A ARCH-CRUD-1 receiver/blocker/gate resolved per
  3A:38 triple: §9.2 carries explicit `cursor-shape ratify-or-unify
  pending Ω-A` carrier note rather than asserting cursor-shape
  unification.
- D06 Part (a) cross-call retention already DISPOSED at 3C V1 via
  LAC-2F-V5-02 ELEVATED at `restart/locks/LOCKS.md:137-158`; §9.2
  carries that elevation reference verbatim.
- 5-shape canon preserved verbatim (no 6th variant); every "6th"/"sixth"
  mention is NEGATIVE/G-Omega-gated.
- 16-lock count preserved
  (`grep -cE "^[0-9]+\. \*\*" restart/locks/LOCKS.md` → 16).
- ARCHITECTURE.md line delta: 1960 → 2324 (+364 lines for 24 deltas).

## Per-delta apply log

| delta-id | section | ARCHITECTURE.md line(s) | status | notes |
|---|---|---|---|---|
| ARCH-3A-D01 | §0 Authority/Conflict | 19-36 | applied | Replaced SK-V6 fold-back paragraph with SK-V14 totality-status block citing 5-cohort LOCK convergence + Pass Omega V2 G-Omega CLOSED. |
| ARCH-3A-D02 | §7.2 Backend IR | 1015-1027 (Live BIR Coverage Status table) | applied | Inserted Live BIR Coverage Status table separating 20-variant target from 13 live + SimdScan separately; 6 unimplemented variants enumerated. |
| ARCH-3A-D03 | §7.3 Side Tables | 1109-1166 (Cost-model derivation pipeline) | applied | Replaced hardcoded P1-P8 cascade narrative with candidate generation → bounded saturation → CSP feasibility → cost extraction pipeline; cascade preserved as diagnostic-only ordering; substrate_target binding cited. |
| ARCH-3A-D04 | §7.3 CostFacts row | 1075 (extended) | applied | Extended CostFacts row to register ActiveCostFacts + DecisionCspFacts + W7 admission gate at `lower_to_rust`. |
| ARCH-3A-D05 | §7.3 Live BackendShape Admission Ledger | 1186-1210 (new table) | applied | Added per-shape admission ledger with 1/5 admission rate (SinkOnly only); 4 non-admitted shapes flagged as marker-string lowerers. |
| ARCH-3A-D06 (a) | already DISPOSED at 3C V1 | n/a (referenced at §9.2 carrier note) | n/a | LAC-2F-V5-02 ELEVATED at LOCKS.md:137-158; ARCHITECTURE.md §9.2 mirrors elevation. |
| ARCH-3A-D06 (b) | §9.2 carrier note | 1849-1872 | applied | Added Substrate-Union Resolution Disposition block with explicit `cursor-shape ratify-or-unify pending Ω-A` carrier note for Part (b); §9.2 prose does not assert cursor-shape unification at HEAD. |
| ARCH-3A-D07 | §9 Output-Plane Substrate Taxonomy | 1773-1797 (new table) | applied | Added 4-plane taxonomy: retained tape / direct sink / admitted fact-stream output / transient scanner; CSS fact-stream classified as 5th SUBSTRATE-manifest per LAC-1E-14, NOT 6th BackendShape. |
| ARCH-3A-D08 | §9 Live Pattern H Status | 1799-1818 (new table) | applied | Added per-grammar 67-file census across 9 dirs; 0/9 `@generated`; 0/9 §9 template files; Lock 14 verification cmd cited. |
| ARCH-3A-D09 | §7.4 Generic-Crate Leak Surface | 1238-1257 (new subsection) | applied | Enumerated 4 leak classes (a-d) + pass-layer JSON leaks (e); HEAD counts: 8 enum variants, 30 parser-name sites, 127 reexports, 67 hand-written files. |
| ARCH-3A-D10 | §7.3 PrimitiveFacts row | 1077 (extended) | applied | Extended PrimitiveFacts row to 8-cell manifest per LAC-1E-12; registered SKELETON triple DELETED; policy_owner + atomic close-state vocabulary. |
| ARCH-3A-D11 | §7.3 architecture-pressure boundary | 1213-1221 (after admission ledger) | applied | Added AVX-512 x86-architecture-pressure boundary note; aarch64 `CollapsedStage` mechanically refused at `admits_collapsed_stage` predicate per LAC-2D-06. |
| ARCH-3A-D12 | §7.5 Parse-That/Regex Import Boundary | 1373-1395 (new subsection before §7.5) | applied | Added 4-row inadmissible-pattern table + admissible `bbnf-regex::HirFacts` import gate; CH3 pre-flight reflex cited per LOCKS.md:563-579. |
| 3E-D01 | §7.3 5×15 CSS L4 BackendShape matrix | 1224-1252 (new table) | applied | Added canonical non-JSON companion matrix for all 15 CSS L4 sub-grammars. |
| 3E-D02 | §7.3 Cost-model derivation pipeline (folded with D03) | 1109-1166 | applied | Resolver pipeline wording binds Lock 10. |
| 3E-D03 | §7.4 leak surface (folded with D09) + §12 L14-HC clauses | 1238-1257 + 2099-2129 | applied | Generated provider manifest + generated sink/fact/value/flag ownership clauses. |
| 3E-D04 | §7.3 primitive vocabulary transfer table | 1265-1280 (new table) | applied | Added 9-row primitive vocabulary transfer table: byte-set / byte-range / string-escape / digit-number / sink / regex / resolver / cross-chunk / SIMD-ASM. |
| 3E-D05 | §9 output-plane taxonomy (folded with D07) | 1773-1797 | applied | CSS fact streams classified as admitted output-plane evidence, not retained sidecar; NOT 6th BackendShape. |
| 3E-D06 | §12 7-step onboarding protocol | 2046-2080 (new subsection) | applied | Added 7-step protocol mirroring 2C V4 §344-405 verbatim; close-anchor noted to SK-V15 Pass Alpha per 3F:113. |
| 3E-D07 | §12 L14-HC-05 row + §13.1 lint manifest cross-reference | 2099-2129 | applied | CSS L4 + Sheets/BBNF-self negative-control mandate folded into L14-HC clauses table. |
| 3E-D08 | §13.1 lint manifest + §12 L14-HC-03 row | 2099-2129 | applied | Grammar-shape leak census folded into L14-HC clauses. |
| 3E-D09 | §12 L14-HC-09 row | 2099-2129 | applied | V3→V4 RuntimeProvider 2→8 enum-drift fault baseline; HEAD enum at `skinny/crates/codegen/src/grammar_profile.rs:17-26`. |
| 3E-D10 | §12 L14-HC-10 row + §7.4 leak class (e) | 1257 + 2099-2129 | applied | Pass-layer recognizer (1B-D8) + materialization-role (1B-D10) + LAC-2C-02 label leaks at `passes/src/lib.rs:1059/1079/1102`. |
| 3E-D11 | §12 L14-HC-11 row + §7.4 leak classes (c)/(d) | 1252-1257 + 2099-2129 | applied | 127 reexports + 30 parser-name sites pinned as gate-consumed monotonic-decrease numbers. |
| 3E-D12 | §12 L14-HC-12 row + §7.3 PrimitiveFacts (folded with D10) | 1077 + 2099-2129 | applied | policy_owner (LAC-2B-03) + FlagSchema (LAC-2C-03) + `byte_class_from_range_64` sibling (LAC-2F-V5-03) + atomic close-state (LAC-2B-07). |

## Cross-reference resolution

Every lock citation introduced in ARCHITECTURE.md by these deltas resolves
in the post-CRUD-3 LOCKS.md (779 lines, commit `85a043224`):

| Cited LOCKS.md anchor | Lock | Resolves to (verified by spot-check) |
|---|---|---|
| `restart/locks/LOCKS.md:100-116` | Lock 1 v+1 FactStream 5th SUBSTRATE category (LAC-1E-14) | line 100 `**v+1 FactStream 5th substrate category (LAC-1E-14)**: `FactStream` is the` |
| `restart/locks/LOCKS.md:117-127` | Lock 1 v+1 substrate_target / retention_lifetime / policy_owner manifest | line 117 (substrate manifest enumeration) |
| `restart/locks/LOCKS.md:137-158` | Lock 1 v+1 substrate-union ELEVATION (LAC-2F-V5-02) | line 137 `**2026-05-23 v+1 substrate-union ELEVATION (LAC-2F-V5-02; T-P3 §3C` |
| `restart/locks/LOCKS.md:220` | Lock 14 verification commands | line 220 (verification command set) |
| `restart/locks/LOCKS.md:222-238` | Lock 14 v+1 three declarative surfaces | line 222 (allowance text) |
| `restart/locks/LOCKS.md:225-233` | Lock 8 v+1 numeric abrogate-gate binding (T2A-LAC-V1-05) | line 225 (6 numeric bounds) |
| `restart/locks/LOCKS.md:349` | Lock 14 generated-output allowance + per-wave gate | line 349 (Lock 14 body verification cmd) |
| `restart/locks/LOCKS.md:402-405` | Lock 14 v+1 Pattern H census (LAC-1E-15) with `-mindepth 2` discipline | line 402 `**v+1 Pattern H per-tranche census (LAC-1E-15)**:` |
| `restart/locks/LOCKS.md:426-434` | Lock 14 v+1 `byte_class_from_range_64` sibling (LAC-2F-V5-03) | line 426 `**v+1 abstract-primitive sibling (LAC-2F-V5-03)**:` |
| `restart/locks/LOCKS.md:506-513` | Lock 16 v+1 atomic close-state vocabulary | line 506 `At close, every source-present primitive is exactly one of \`wired\`,` |
| `restart/locks/LOCKS.md:563-579` | Lock 16 v+1 CH3 pre-flight reflex (V6 F-CH3-2F-08) | line 563 `**CH3 pre-flight reflex (V6 F-CH3-2F-08, LOW prophylactic)**:` |

All 11 anchors resolve. No dangling lock citation introduced in this CRUD.

## Live verification cross-checks

- `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`
  returns **67** at HEAD (verified this cycle). Pattern H 67-file
  recurrence vector preserved verbatim per LAC-1E-15.
- `grep -cE "^[0-9]+\. \*\*" restart/locks/LOCKS.md` returns **16**
  (Lock 17 does not appear). 16-lock count preserved.
- `grep -nE "6th|sixth" restart/ARCHITECTURE.md` returns 4 matches, all
  in NEGATIVE/G-Omega-gated context (FactStream is NOT 6th BackendShape;
  L14-HC-07 fact streams do not create 6th `BackendShape`). 5-shape
  canon `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`
  preserved verbatim at LOCKS.md:108 and ARCHITECTURE.md §7.3.
- ARCHITECTURE.md line count: 2324 (was 1960 pre-CRUD; +364 lines for 24
  deltas applied additively).

## Boundary

This artefact records the CRUD-1 ARCHITECTURE surface edit only. CRUD-2
MASTER-PLAN, CRUD-4 HANDOFF/MIGRATION, CRUD-5 skinny-corpus, and CRUD-6
back-reference reconciliation defer to subsequent Pass Omega V2 turns per
PASS-OMEGA §4.

No source files, generated runtime, gate output, `skinny/RESULTS.md`,
`skinny/REDRESS.md`, or SK-V13 W0 surfaces were edited by this CRUD.
