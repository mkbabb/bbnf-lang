---
agent: 1F
pass: T-P1-excavation
cycle: V2
generated_at: 2026-05-28T04:20:00Z
status: superseded-historical-auxiliary
authoritative_live_inventory: restart/audit/totality/p1/1F-coherence-scan.md
---

# 1F Past-Corpora Auxiliary - Superseded For SK-V15 V2

This file is retained only as a historical pointer. It is not an authoritative live SK-V15 T-P1 inventory. The prior contents were a stale SK-V14 past-corpora ledger and failed SK-V15 V1 freshness checks because SK-V15 now distinguishes JSON as a valid 51-row guard baseline while CSS L4 is audit-demoted.

Use these current sources instead:

- `restart/audit/totality/p1/1F-coherence-scan.md` for live SK-V15 T-P1 coherence evidence.
- `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md` for PASS-IMPL V1 JSON/CSS/Pattern-H/Decision-Engine truth.
- `restart/skinny/tranches/sk-v15/SYNTHESIS.md` for the active prune/rebuild close condition and CH3/CH5/CH7 addenda.
- `skinny/REDRESS.md` for historical REDRESS entries, re-executed at current path/line before citation.

## Carry-Forward Rules

| rule | binding |
|---|---|
| JSON | Preserve as SK-V15 guard baseline unless a later audit falsifies a specific row. |
| CSS L4 | Treat SK-V14 admits as audit-demoted history; do not cite as independent SOTA proof until PRUNE/REBUILD repairs measurement broadcast, generator provenance, comparator workload, and CSS Value API. |
| Historical pre-blocks | REDRESS pre-blocks remain binding only when re-anchored to current entries or current SK-V15 synthesis rows. |
| Future citations | Cite current path:line evidence from REDRESS, SK-V15 synthesis, PASS-IMPL, or `1F-coherence-scan.md`; do not cite this auxiliary as live proof. |
