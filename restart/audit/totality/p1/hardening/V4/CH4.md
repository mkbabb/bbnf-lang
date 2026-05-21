---
agent: CH4
pass: T-P1-excavation
cycle: V4
lens: COST
generated_at: 2026-05-21T12:00:00-04:00
disposition: ACCEPT
audited_artifacts:
  - restart/prompts/totality/PASS-1-EXCAVATION.md
  - restart/prompts/ORCHESTRATOR.md
  - restart/audit/totality/p1/hardening/HARDENING-T-P1-V3-CONSOLIDATED.md
  - restart/audit/totality/p1/hardening/V3/CH4.md
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-past-corpora.md
---

## Lens Basis

`restart/prompts/totality/PASS-1-EXCAVATION.md:121-123` requires every divergence to carry a realistic LOC-delta estimate and risk class, requires 1E amendment candidates to state wave alignment, and marks amendment candidates without supporting path:line evidence REVISE. `restart/prompts/ORCHESTRATOR.md:86` adds the CH4 hardening focus: LOC budget, risk class, wave alignment, hard cap, and same-wave consumer per kernel/primitive. The orchestrator dispatch contract also requires every dispatch to carry a hard cap at `restart/prompts/ORCHESTRATOR.md:132-140`, and the non-negotiables require scalar/checkasm parity before SIMD/ASM wiring, same-wave consumers, and no contrivance at `restart/prompts/ORCHESTRATOR.md:197-212`.

V3 accepted the cost posture: the consolidated V3 result says CH4 ACCEPT because 1A-1E carry the planning schema inline and the 1F files explicitly declare their ID-keyed metadata tables authoritative at `restart/audit/totality/p1/hardening/HARDENING-T-P1-V3-CONSOLIDATED.md:20-27`. The required V4 fold was metadata/provenance only, with accepted CH2-CH6 evidence unchanged, at `restart/audit/totality/p1/hardening/HARDENING-T-P1-V3-CONSOLIDATED.md:29-45`. V3 CH4 itself accepted that every divergence/amendment row had `loc_budget`, `risk`, `wave`, `hard_cap`, `same_wave_consumer`, and `evidence_basis`, inline or via explicit authoritative ID-keyed metadata tables, at `restart/audit/totality/p1/hardening/V3/CH4.md:30-41`.

## Findings

| disposition | finding | evidence | required action |
|---|---|---|---|
| ACCEPT | V4 is the metadata-only fold V3 requested, not a substantive cost rewrite. | 1A, 1B, 1C, 1D, 1E, 1F anti-pattern, 1F coherence, and 1F past-corpora all identify `cycle: V4` and carry explicit `v4_metadata_fold` declarations at `restart/audit/totality/p1/1A-substrate-evidence.md:4-9`, `restart/audit/totality/p1/1B-codegen-evidence.md:4-10`, `restart/audit/totality/p1/1C-runtime-evidence.md:4-9`, `restart/audit/totality/p1/1D-skinny-lessons.md:4-9`, `restart/audit/totality/p1/1E-locks-evidence.md:4-9`, `restart/audit/totality/p1/1F-anti-pattern.md:4-9`, `restart/audit/totality/p1/1F-coherence-scan.md:4-9`, and `restart/audit/totality/p1/1F-past-corpora.md:4-9`. | None. |
| ACCEPT | 1A preserves full inline CH4 metadata for divergences and its Lock 1 amendment candidate. | The divergence table defines `loc_budget`, `risk`, `wave`, `hard_cap`, `same_wave_consumer`, and `evidence_basis` at `restart/audit/totality/p1/1A-substrate-evidence.md:50`; rows `1A-DIV-001` through `1A-DIV-007` populate the fields at `restart/audit/totality/p1/1A-substrate-evidence.md:52-58`. The Lock 1 amendment candidate carries the same fields at `restart/audit/totality/p1/1A-substrate-evidence.md:82-84`. | None. |
| ACCEPT | 1B preserves full inline CH4 metadata for divergences and amendment candidates. | The divergence table includes `loc_budget`, `risk`, `wave`, `hard_cap`, `same_wave_consumer`, and `evidence_basis` at `restart/audit/totality/p1/1B-codegen-evidence.md:73`; rows `P1-1B-D1` through `P1-1B-D10` populate those fields at `restart/audit/totality/p1/1B-codegen-evidence.md:75-84`. Amendment candidates carry the same schema at `restart/audit/totality/p1/1B-codegen-evidence.md:107-110`. | None. |
| ACCEPT | 1C preserves realistic runtime cost posture, including per-wave hard-cap treatment for the nine-grammar matrix. | Runtime counts remain approximate/verify-action-gated at `restart/audit/totality/p1/1C-runtime-evidence.md:38-46`. The divergence table carries the full CH4 schema at `restart/audit/totality/p1/1C-runtime-evidence.md:96`; rows `1C-D1` through `1C-D7` populate it at `restart/audit/totality/p1/1C-runtime-evidence.md:98-104`, including the `1C-D4` multi-wave grammar enablement cap at `restart/audit/totality/p1/1C-runtime-evidence.md:101`. | None. |
| ACCEPT | 1D preserves numeric planning metadata for skinny-derived lessons and keeps same-wave consumer requirements on SIMD/ASM and grammar-general routes. | The V3 fold summary states CH4 required numeric metadata at `restart/audit/totality/p1/1D-skinny-lessons.md:31-39`. JSON and grammar-neutral findings carry planning metadata at `restart/audit/totality/p1/1D-skinny-lessons.md:58-66` and `restart/audit/totality/p1/1D-skinny-lessons.md:70-81`. The divergence table defines and populates the full CH4 schema at `restart/audit/totality/p1/1D-skinny-lessons.md:102-109`. | None. |
| ACCEPT | 1E preserves cost routing for lock divergences and all eleven lock amendment candidates. | The V1 hardening fold records the CH4 field addition at `restart/audit/totality/p1/1E-locks-evidence.md:31-40`. The lock implementation table includes LOC/risk, hard cap, same-wave consumer, and wave alignment at `restart/audit/totality/p1/1E-locks-evidence.md:61-78`. Lock divergences carry the full schema at `restart/audit/totality/p1/1E-locks-evidence.md:82-94`; amendment candidates carry it at `restart/audit/totality/p1/1E-locks-evidence.md:98-110`. | None. |
| ACCEPT | 1F anti-pattern preserves CH4 via an explicit authoritative ID-keyed metadata table. | The file states that its ID-keyed `V2 Planning Metadata` table is the authoritative CH4 carrier at `restart/audit/totality/p1/1F-anti-pattern.md:43-45`. Its index rows are at `restart/audit/totality/p1/1F-anti-pattern.md:47-58`, and the authoritative table defines and populates `loc_budget`, `risk`, `wave`, `hard_cap`, `same_wave_consumer`, and `evidence_basis` at `restart/audit/totality/p1/1F-anti-pattern.md:60-74`. | None. |
| ACCEPT | 1F coherence preserves CH4 via an explicit authoritative ID-keyed metadata table. | The authoritative-carrier sentence is present at `restart/audit/totality/p1/1F-coherence-scan.md:46-48`. The index rows are at `restart/audit/totality/p1/1F-coherence-scan.md:50-57`, and the authoritative table defines and populates the required fields at `restart/audit/totality/p1/1F-coherence-scan.md:59-73`. | None. |
| ACCEPT | 1F past-corpora preserves CH4 via an explicit authoritative ID-keyed metadata table, including zero-LOC pre-block realism and producer-only hard pre-blocks. | The authoritative-carrier sentence is present at `restart/audit/totality/p1/1F-past-corpora.md:55-57`. The index rows are at `restart/audit/totality/p1/1F-past-corpora.md:59-66`, and the authoritative table defines and populates the required fields at `restart/audit/totality/p1/1F-past-corpora.md:68-79`. Producer-only SIMD/union/resolver/codegen artifacts require same-wave consumer measurement at `restart/audit/totality/p1/1F-past-corpora.md:50`; the metadata preserves that requirement at `restart/audit/totality/p1/1F-past-corpora.md:78`. | None. |
| REVISE | No CH4 revision required. | No V4 inventory drops `loc_budget`, `risk`, `wave`, `hard_cap`, `same_wave_consumer`, or `evidence_basis`; no amendment candidate lacks path:line evidence. | Not applicable. |
| REJECT | No CH4 rejection. | The V4 fold preserves the accepted V3 cost posture and the hard-cap realism contract from PASS-1 and ORCHESTRATOR. | Not applicable. |

## Coverage Check

| artifact | divergence rows checked | amendment rows checked | CH4 carrier |
|---|---:|---:|---|
| `1A-substrate-evidence.md` | 7 | 1 | inline |
| `1B-codegen-evidence.md` | 10 | 2 | inline |
| `1C-runtime-evidence.md` | 7 | 0 | inline |
| `1D-skinny-lessons.md` | 6 | 0 | inline |
| `1E-locks-evidence.md` | 11 | 11 | inline |
| `1F-anti-pattern.md` | 11 ID-keyed planning rows | 0 | authoritative metadata table |
| `1F-coherence-scan.md` | 9 ID-keyed planning rows | 0 | authoritative metadata table |
| `1F-past-corpora.md` | 8 ID-keyed planning rows | 0 | authoritative metadata table |

## Cycle Verdict

Disposition: ACCEPT.

V4 satisfies CH4. The metadata-only fold preserves the V3 accepted cost posture: every divergence/amendment remains budgeted, risked, wave-aligned, hard-capped, tied to a same-wave consumer where required, and grounded in an evidence basis either inline or through an explicitly authoritative ID-keyed planning metadata table.
