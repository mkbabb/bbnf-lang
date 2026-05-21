# T-P3 V2 Hardening Consolidated

Pass: T-P3 Synthesis. Cycle: V2.
Date: 2026-05-21.
Scope: six-lens challenge verdict for the V2 totality synthesis packet.
Output: this file.

## Verdict

`G-T-P3-V2-CHALLENGE`: REVISE.

V2 fixed the V1 load-bearing blockers. CH4 accepts the cost/LOC/risk/wave
ledgers and CH6 accepts the receiver/blocker/gate routing, including the
Omega CRUD versus G-Omega sequencing. The remaining defect is narrow CH1
source-map hygiene: several V2 artifacts still contain stale cycle wording
that describes the current packet as V1, and 3C contains bare
`PASS-3-SYNTHESIS.md` citations that should use the resolved prompt path
before G3.

Acceptance rate: 5/6 = 83.3%.
Consecutive accepted cycles: 0.

| Lens | Disposition | Load-bearing finding |
|---|---|---|
| CH1 correctness | REVISE | V2 repair surfaces exist, but 3A, 3B, and 3E contain stale current-artifact V1 wording; 3C has bare `PASS-3-SYNTHESIS.md` citations. |
| CH2 generality / Lock 14 | ACCEPT | V2 preserves generated-output boundaries, five-shape discipline, Lock 14 per-wave gates, and CSS/Sheets/BBNF-self negative-control routing. |
| CH3 regression / REDRESS | ACCEPT | V2 keeps REDRESS evidence historical and gated; JSON row reopen, union, SIMD, and CSS parity routes require material differential, strict comparator, and same-wave consumer or block evidence. |
| CH4 cost | ACCEPT | V2 adds per-delta LOC budgets, propagation counts, risk classes, wave alignment, receivers/consumers, and hard caps/abrogate gates; 3C keeps ACCEPT/MODIFY as lock-text dispositions only. |
| CH5 hidden coupling | ACCEPT | V2 does not smuggle comparator sidecars, fact streams, provider manifests, primitive bridges, decision-engine routes, or union reopen work into retained coupling. |
| CH6 anti-paper-close | ACCEPT | V2 names receivers, blockers, and gates for open routes; support-only closure remains blocked; pre-G-Omega CRUD is proposed diffs/logs only. |

## Required V3 Fold

1. Replace stale current-artifact V1 wording in:
   - `restart/audit/totality/p3/3A-architecture-synthesis.md`
   - `restart/audit/totality/p3/3B-master-plan-reconciliation.md`
   - `restart/audit/totality/p3/3E-grammar-generalisation.md`
   with V3-accurate wording such as "no prior accepted T-P3 cycle is carried"
   while preserving "V1 surface" when it names the target totality surface.
2. Normalize 3C's bare prompt citations from `PASS-3-SYNTHESIS.md` to
   `restart/prompts/totality/PASS-3-SYNTHESIS.md`.
3. Bump the T-P3 synthesis packet to V3, commit it, and rerun the full
   six-lens challenge cycle. V2 does not count toward the two consecutive
   ACCEPT convergence requirement.

## Disposition

T-P3 does not converge on V2. Fold the CH1 revise set into V3, commit the
revised T-P3 synthesis packet, then rerun the full six-lens challenge.
