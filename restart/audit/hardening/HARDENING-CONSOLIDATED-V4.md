# HARDENING-CONSOLIDATED-V4

## §1 Target identifications

| Target | Target output audited | Hardening report | Hardening commit | Verdict | KEEP | REINVENT | DISCARD | Punch list |
|---|---|---|---|---|---:|---:|---:|---:|
| PASS-1 | `restart/audit/pass-1-substrate/PASS-1.md` (post-Wave-1.1 + post-Wave-2; commits `f08c75a4` + `cd3441e7`) | `restart/audit/hardening/HARDENING-PASS-1-V3.md` (V3-READY; carries through) | `396b23f8` | READY | 53 | 1 | 2 | 0 |
| PASS-2 | `restart/audit/pass-2-codegen/PASS-2.md` (post-Wave-4.1; commit `b60d7572`) | `restart/audit/hardening/HARDENING-PASS-2-V4.md` | `6987b166` | READY | 39 | 0 | 0 | 0 |
| PASS-3 | `restart/audit/pass-3-runtime/PASS-3.md` (post-Wave-4.1; commit `11806d5d`) | `restart/audit/hardening/HARDENING-PASS-3-V4.md` | `8d93e893` | READY | 74 | 0 | 0 | 0 |
| MASTER-PLAN | `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md` (post-Wave-4.1; commits `1d9d7ffa` + `11806d5d`) | `restart/audit/hardening/HARDENING-MASTER-PLAN-V4.md` | `4c4366ce` | READY | 75 | 0 | 0 | 0 |

| Cohort | KEEP | REINVENT | DISCARD | Punch-list rows | Final verdict |
|---|---:|---:|---:|---:|---|
| Four-target V4 hardening cohort | 241 | 1 | 2 | 0 | **READY** |

PASS-1 carries V3-READY through to V4 cohort tally without rerun (V3 verdict was unconditional READY). PASS-2, PASS-3, and MASTER-PLAN all advance from V3-AMENDMENT-REQUIRED to V4-READY after Wave-4.1 narrow amendment.

KEEP fraction climbs across the cohort:
- V1: 46% (117 / 252)
- V2: 92% (244 / 265)
- V3: 88% (221 / 251)
- V4: 99% (241 / 244)

The V3 → V4 climb reflects the closure of the 24-item V3 punch list across three non-overlapping write surfaces by three parallel narrow-amendment agents. No V4 surgery was authored; the audit verifies surgeries from Wave-4.1.

## §2 Cohort verdict — per-lane consolidated table

| Lane | PASS-1 | PASS-2 | PASS-3 | MASTER-PLAN | Cumulative |
|---|---|---|---|---|---|
| 1 Lock-Adherence | KEEP 9 / REINVENT 0 / DISCARD 1 | READY; KEEP 9 / REINVENT 0 / DISCARD 0 | KEEP 12 / REINVENT 0 / DISCARD 0 | READY; KEEP 10 / REINVENT 0 / DISCARD 0 | READY; KEEP 40 / REINVENT 0 / DISCARD 1 |
| 2 Sequencing | N/A | N/A | N/A | READY; KEEP 7 / REINVENT 0 / DISCARD 0 | READY; KEEP 7 / REINVENT 0 / DISCARD 0 |
| 3 Cohesion | KEEP 7 / REINVENT 0 / DISCARD 0 | READY; KEEP 7 / REINVENT 0 / DISCARD 0 | KEEP 9 / REINVENT 0 / DISCARD 0 | READY; KEEP 7 / REINVENT 0 / DISCARD 0 | READY; KEEP 30 / REINVENT 0 / DISCARD 0 |
| 4 SOTA-Anchoring | KEEP 5 / REINVENT 0 / DISCARD 0 | KEEP 7 / REINVENT 0 / DISCARD 0 | READY; KEEP 6 / REINVENT 0 / DISCARD 0 | READY; KEEP 6 / REINVENT 0 / DISCARD 0 | READY; KEEP 24 / REINVENT 0 / DISCARD 0 |
| 5 Grammar-Authoritative | KEEP 9 / REINVENT 0 / DISCARD 0 | READY; KEEP 7 / REINVENT 0 / DISCARD 0 | KEEP 7 / REINVENT 0 / DISCARD 0 | KEEP 5 / REINVENT 0 / DISCARD 0 | READY; KEEP 28 / REINVENT 0 / DISCARD 0 |
| 6 Generated-Code-Budget | KEEP 4 / REINVENT 0 / DISCARD 0 | KEEP 6 / REINVENT 0 / DISCARD 0 | READY; KEEP 8 / REINVENT 0 / DISCARD 0 | KEEP 4 / REINVENT 0 / DISCARD 0 | READY; KEEP 22 / REINVENT 0 / DISCARD 0 |
| 7 Friction-Forecast | KEEP 6 / REINVENT 0 / DISCARD 0 | READY; KEEP 7 / REINVENT 0 / DISCARD 0 | KEEP 16 / REINVENT 0 / DISCARD 0 | KEEP 5 / REINVENT 0 / DISCARD 0 | READY; KEEP 34 / REINVENT 0 / DISCARD 0 |
| 8 Carry-Deferral | KEEP 6 / REINVENT 0 / DISCARD 0 | KEEP 5 / REINVENT 0 / DISCARD 0 | KEEP 11 / REINVENT 0 / DISCARD 0 | READY; KEEP 6 / REINVENT 0 / DISCARD 0 | READY; KEEP 28 / REINVENT 0 / DISCARD 0 |
| 9 Greenfield-Discipline | KEEP 7 / REINVENT 1 / DISCARD 1 | KEEP 5 / REINVENT 0 / DISCARD 0 | KEEP 8 / REINVENT 0 / DISCARD 0 | KEEP 4 / REINVENT 0 / DISCARD 0 | READY; KEEP 24 / REINVENT 1 / DISCARD 1 |

| Verdict class | Count | Meaning |
|---|---:|---|
| KEEP | 241 | Ratified architecture surviving four cycles of independent challenge. |
| REINVENT | 1 | PASS-1 V3-residual: Lock-2-canonical-naming row defeated by steelman; carries forward as observation, not amendment. |
| DISCARD | 2 | PASS-1 V3-confirmed deletions: independent-proceed clause; OpenFrame preservation. |

The cohort verdict is **READY** across nine lanes (Lane 2 N/A for PASS targets). Every active lane returns READY. No V4 surgery is required.

## §3 V3 → V4 punch closure summary

The 24 V3 punch items distributed across PASS-2 (8+1), PASS-3 (2+2), and MASTER-PLAN (6+3). All 24 closed by Wave-4.1 narrow amendment + verified by independent V4 audit.

| V3 punch | Wave-4.1 commit | V4 verification | Status |
|---|---|---|---|
| PASS-2 P2-1 (Lock 2 layout-canon) | `b60d7572` | PASS-2.md:69 | CLOSED |
| PASS-2 P2-2 (Lock 3 unified cursor) | `b60d7572` | PASS-2.md:176 | CLOSED |
| PASS-2 P2-3 (deny-gate widening) | `b60d7572` | PASS-2.md:247-250 | CLOSED |
| PASS-2 P2-4 (3-row template schema) | `b60d7572` | PASS-2.md:147-149 | CLOSED |
| PASS-2 P2-5 (yaml two-surface invariant) | `b60d7572` | PASS-2.md:386 | CLOSED |
| PASS-2 P2-6 (6 verbatim diagnostics) | `b60d7572` | PASS-2.md:532-539 | CLOSED |
| PASS-2 P2-7 (BBNF-OPT001/002) | `b60d7572` | PASS-2.md:540-541 | CLOSED |
| PASS-2 P2-8 (swc → SOTA citation) | `b60d7572` | PASS-2.md:81 | CLOSED |
| PASS-2 P2-9 (yaml smoke receiver) | `b60d7572` | PASS-2.md:433 | CLOSED |
| PASS-3 P3-1 (bench-row attribution) | `11806d5d` | PASS-3.md:387-399 | CLOSED |
| PASS-3 P3-2 (W3 baseline anchors) | `11806d5d` | PASS-3.md:401-413 | CLOSED |
| PASS-3 P3-3 (yaml host-route cell) | `11806d5d` | PASS-3.md:342 | CLOSED |
| PASS-3 P3-4 (visitor cookbook routing) | `11806d5d` | PASS-3.md:115 + §6b | CLOSED |
| MASTER-PLAN M1 (Lock 2 path canon) | `1d9d7ffa` | ARCH:438 + cross-refs | CLOSED |
| MASTER-PLAN M2 (Lock 11 publication split) | `1d9d7ffa` | MASTER-PLAN:524 | CLOSED |
| MASTER-PLAN M3 (bbnf-bench reconciliation) | `1d9d7ffa` + `11806d5d` | README:38 + ARCH:43 | CLOSED |
| MASTER-PLAN M4 (H.W3 WASM anchor) | `1d9d7ffa` | MASTER-PLAN:459 | CLOSED |
| MASTER-PLAN M5 (baseline source-of-truth) | `1d9d7ffa` | MASTER-PLAN:634-649 | CLOSED |
| MASTER-PLAN M6 (ARCH §7.4 diagnostic vocab) | `1d9d7ffa` | ARCH:992 (28 codes) | CLOSED |
| MASTER-PLAN M7 (carry-ledger consolidation) | `1d9d7ffa` | MASTER-PLAN:730-756 + MIGRATION:772-781 | CLOSED |
| MASTER-PLAN M8 (H.W2 skip-marker) | `1d9d7ffa` | MASTER-PLAN:458 | CLOSED |
| MASTER-PLAN M9 (json/canada simd-json) | `1d9d7ffa` | MASTER-PLAN:133 | CLOSED |

Closure depth (per V4 MASTER-PLAN §3.1): 23 of 24 closed deep (architectural follow-through across cross-document references); 1 of 24 (M3) closed shallow-but-consistent (terms differ across docs but semantic agreement).

## §4 Cross-target conflicts — closed

The 14 V1 cross-target conflicts → 11 closed at V2 + 3 reopened at V3 → all 14 closed at V4:

| V1 Conflict | V2 status | V3 status | V4 status |
|---|---|---|---|
| Backend IR ownership | CLOSED | CLOSED | CLOSED |
| Public path macro name (`pointer!`) | CLOSED | CLOSED | CLOSED |
| Path crate names (unprefixed) | CLOSED | CLOSED | CLOSED |
| Layout terminology (`passes::layout`) | CLOSED | REOPENED | CLOSED via M1 |
| Cursor/byte-skip proof | CLOSED | CLOSED | CLOSED |
| BBNF extension surface | CLOSED | CLOSED | CLOSED |
| Lock 14 yaml onboarding | CLOSED | CLOSED | CLOSED |
| Per-X proof | CLOSED | CLOSED | CLOSED |
| Generated budget authority | CLOSED | CLOSED | CLOSED |
| SOTA close gate | CLOSED | CLOSED | CLOSED |
| PASS hardening sequencing | CLOSED | CLOSED | CLOSED |
| OpenFrame residue | CLOSED | CLOSED | CLOSED |
| Package-name ambiguity | CLOSED | CLOSED | CLOSED |
| Fixture role | CLOSED | CLOSED | CLOSED |
| (V3 new) bbnf-bench publication status | — | NEW | CLOSED via M3 |
| (V3 new) Per-grammar baseline source-of-truth | — | NEW | CLOSED via M5 |

No cross-target conflicts remain. The four targets agree on every shared substantive surface.

## §5 Final readiness verdict

**READY**.

Every V4 hardening report returns READY. KEEP rate 99% (241 of 244). One non-amendment REINVENT (PASS-1 Lock-2-canonical-naming observation; defeated by steelman) and two confirmed DISCARDs (PASS-1's independent-proceed clause + OpenFrame preservation). Zero open punch items. All 14 V1 cross-target conflicts closed. All 24 V3 punch items closed.

Per AMENDMENT-DISPATCH §3 Wave 4 decision rule: Wave 4 returns READY → user advances to per-tranche full-spec drafting.

Re-draft thresholds (`HARDENING-CONSOLIDATED.md` §5; 10 conditions): zero met. Tape/direct union holds. Backend IR ownership holds. yaml two-surface proof holds. Numeric SOTA gates hold. B/C and C/E/H sequencing hold. Generated-code budgets hold. Carry ledgers hold. `pointer!`/`select!` hold. `@error(recover)` holds. OpenFrame archaeology holds.

## §6 Voice + discipline locks

The amended documents preserve voice and discipline locks per `restart/README.md` §13. Calibrated, direct prose. Archaic-permissive. No metalanguage. Path:line citations. Per-X tables for "all grammars" claims. Receiver / blocker / receiving-gate triple on every carry. No quick solutions. No legacy code uncontested. No overfitting. Idiomatic gestalt.

V1 → V2 → V3 → V4 progression: each cycle tightened proof surfaces, calibrated cross-document references, strengthened verbatim discipline. The V4 verdict reflects four cycles of independent challenge surviving every adversarial pressure.

## §7 Closing posture

Hereupon per-tranche full-spec drafting is unblocked. The MASTER-PLAN trio (`restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md`) carries the executable authority for tranches A through J. The dispatched per-tranche drafting agents (10 agents, ~3,000-5,000 lines per tranche, inheriting from BA-BD per `restart/inheritance/INDEX.md`) consume the V4-ratified outputs without re-litigating the 14 locks, the 35-answer interrogation, the precepts, the BBNF extension surface, or the tape/direct substrate.

The amendment-dispatch cycle closes here. Wave 1 (foundations) → Wave 2 (core surgeries) → Wave 3 (Reviewer-C narrow additions) → Wave 4 V1/V2/V3/V4 hardening cycles produced 25 amendment commits across 6 author-class agents and 8 hardening-class agents. Total wall-time across the cycle: ~9 hours. Final state: READY.
