# SK-V15 S-P2 V2 Hardening Consolidated

Cycle: S-P2 Research V2.
Date: 2026-05-28.
Input packet: `restart/skinny/tranches/sk-v15/research/p2/`.
Hardening root: `restart/skinny/tranches/sk-v15/research/p2/hardening/V2/`.

## Verdict

ACCEPT-RATE: 7/7 = 100%.

Cycle verdict: ACCEPT for V2, not pass convergence yet. `ORCHESTRATOR.md` §3Z requires at least 95% ACCEPT for two consecutive cycles, zero orphan REVISEs, and zero open critical defects. V2 is the first clean cycle after the V1 fold; S-P2 must run V3 confirmation before dispatching S-P3.

## Lens Dispositions

| Lens | Disposition | Output | Summary |
|---|---|---|---|
| CH1 CORRECTNESS | ACCEPT | `V2/CH1.md` | V1 correctness defects are folded: numeric/digit and EOB rows are rejected or support-only; comparator, source, strictness, and host claims are citable. |
| CH2 GENERALITY | ACCEPT | `V2/CH2.md` | P2-F covers the prior alias orphan, every survivor has a grammar-neutral verdict, and JSON-only policy is fenced out of generic primitives. |
| CH3 REGRESSION | ACCEPT | `V2/CH3.md` | REDRESS-blocked routes remain blocked; PMULL, CSSC, numeric, retained-substrate, CSS broadcast, and gate-exclusion risks are not reopened. |
| CH4 COST | ACCEPT | `V2/CH4.md` | The grouped CH4 fold supplies scalar reference, parity/checkasm, same-wave consumer, LOC/risk, wave alignment, and cap discipline for non-REJECT survivors. |
| CH5 HIDDEN COUPLING | ACCEPT | `V2/CH5.md` | `offset_tape_capacity_policy_v2` forbids second scans, pre-scan capacity oracles, retained capacity sidecars, and parallel source passes; no Lock 1 split remains. |
| CH6 ANTI-PAPER-CLOSE | ACCEPT | `V2/CH6.md` | Comparator/process/ISA/primitive claims resolve to evidence, and deferred-looking language is paired with rejection or explicit admission preconditions. |
| CH7 OVERFIT-PRUNE / GATE-EXCLUSION | ACCEPT | `V2/CH7.md` | Diagnostic and rejected rows are reported rather than hidden; gate-exclusion discipline is explicit and must be preserved by S-P3. |

## Open REVISE / REJECT List

None.

## Orphan Disposition Check

V1 open items are closed by the V2 fold:

- CH1 unsupported numeric, EOB, and host/source issues: folded into rejection/support-only rows and host/sysctl evidence.
- CH2 P2-A alias orphan: folded into P2-F §2.1 alias mapping and §2 candidate dispositions.
- CH4 missing cost details: folded into P2-F §2.2 cost table.
- CH5 capacity-policy second-scan risk: folded into P2-D and P2-F same-substrate wording.

## Next Dispatch

Dispatch S-P2 V3 confirmation over the same folded packet. V3 may be a confirmation hardening cycle because V2 has no open fold work, but it must still produce CH1-CH7 files and a consolidated verdict. If V3 returns ≥95% ACCEPT with zero orphan REVISEs, S-P2 locks and hands off to S-P3.
