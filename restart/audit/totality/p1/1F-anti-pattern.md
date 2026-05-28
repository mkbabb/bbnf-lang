---
agent: 1F
pass: T-P1-excavation
cycle: V2
generated_at: 2026-05-28T04:20:00Z
status: superseded-historical-auxiliary
authoritative_live_inventory: restart/audit/totality/p1/1F-coherence-scan.md
---

# 1F Anti-Pattern Auxiliary - Superseded For SK-V15 V2

This file is no longer an authoritative live SK-V15 T-P1 inventory. The prior contents were a stale SK-V14 anti-pattern ledger and failed SK-V15 V1 CH1, CH2, CH6, and CH7 freshness checks.

Use `restart/audit/totality/p1/1F-coherence-scan.md` for current SK-V15 live coherence and anti-pattern evidence.

## Carry-Forward Rules

| rule | binding |
|---|---|
| Historical evidence | Prior REDRESS or audit findings may be cited only when the current artifact re-anchors them to live HEAD evidence. |
| Live LOC / symbol claims | This file carries none. Do not cite it for current LOC counts, provider-module counts, runtime profile rosters, or codegen symbols. |
| Current anti-pattern rows | Current rows are COH-004 through COH-015 in `1F-coherence-scan.md`, especially Lock 14/16 gate exclusions, Pattern H provenance, Decision Engine scaffold, root `OnceCell<StructuralIndex>`, and CSS source-sidecar comparator coupling. |
| Gate-exclusion discipline | Current Lock 14 / Lock 16 gate-exclusion carrier is in `1F-coherence-scan.md` V2. |

## Superseded Claims Not To Reuse As Live Evidence

- Stale provider-enum and eight provider-module claims.
- Stale LOC counts for `report.rs`, `gate.rs`, `generated_real_typed.rs`, `lock14_baseline.rs`, and runtime generated files.
- SK-V14-only CSS sidecar line anchors unless re-anchored by the V2 coherence rows.
- Any prior-cycle claim as current SK-V15 evidence.
