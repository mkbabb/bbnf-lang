# HARDENING T-P3 V2 Consolidated

Target packet: `7885b29ab` (`docs(sk-v15-t-p3): fold V1 hardening into V2 synthesis`).
Challenge context: `d1d073a50` (`docs(sk-v15-t-p3): open V2 hardening context`).
Cycle: `V2`.

## Verdict

`REVISE`.

V2 does not count as a clean §3Z cycle. Seven lenses completed: CH1, CH2, CH3,
CH5, CH6, and CH7 returned `ACCEPT`; CH4 returned `REVISE`; zero lenses
returned `REJECT`. Required executable checks passed across the wave: the target
packet edits only the seven T-P3 proposal artifacts; `git diff --check` is
clean; the extracted `3C-locks-v+1-diff.md` applies cleanly to current
`LOCKS.md`; live invariants read 16 numbered locks and 67 Pattern H runtime
files; the stale-pattern scan is empty; and path-line validation passed.

The V1 defects are substantively folded, but V3 must close a narrower CH4 field
coverage gap: every carried delta must state hard-cap fit and fail action at
row level, including the 3C `D-L*` clause matrix.

## Lens Results

| lens | verdict | primary result |
|---|---|---|
| CH1 correctness | `ACCEPT` | Target packet scope is proposal-only; 855 explicit path-line citations resolve in range; the 3C LOCKS diff applies; 3C covers all 42 live LAC candidates; V1 CH1 stale citation and missing companion-prompt repairs are closed. |
| CH2 generality | `ACCEPT` | Lock 14 holds; no JSON narrowing, forbidden surface addition, retained sidecar, public substrate API, sixth `BackendShape`, new directive, new BIR variant, or new substrate enters the packet. CSS plus Sheets/BBNF-self receivers remain concrete. |
| CH3 regression | `ACCEPT` | REDRESS routes are not reopened; stale SK-V13/SK-V14 blocks remain historical or pre-block evidence; delete-before-provider sequencing remains blocked; V3 through V8 corrective gates remain fail-closed. |
| CH4 cost | `REVISE` | V1 W4/W7-W9/CSSOM/CRUD-4 cost defects are closed in substance, but carried proposal/governance deltas in 3A/3B/3D/3E/3F lack row-level hard-cap-fit and fail-action fields, and the 3C per-clause cost matrix lacks those columns. |
| CH5 hidden coupling | `ACCEPT` | No parallel substrate, sidecar producer, runtime regex/DFA substrate, Track 1 == Track 2 dishonesty, `FactStream` shape leak, x86 close evidence, or legacy `bbnf-regex` owner coupling remains. |
| CH6 anti-paper-close | `ACCEPT` | No prose-only implementation closure, engineered deferral, uncited validation claim, G3/G-Omega confusion, CRUD-4 cleanup loophole, or receiver/blocker/gate omission remains. |
| CH7 overfit-prune | `ACCEPT` | Forward-lens addenda remain active: wave-graph cycles, broadcast admission, gate exclusions, CSS fake parity, wrong-host close evidence, FNV bench leakage, delete-before-provider sequencing, and self-exempting grep gates are blocked. |

## Required V3 Fold

| finding | required repair | owner |
|---|---|---|
| `CH4-V2-001` | Add a compact CH4 coverage matrix, or extend each consequences table, for every carried delta in 3A, 3B, 3D, 3E, and 3F. Each row must name LOC, numeric propagation count, risk, wave alignment, consumer/gate, hard-cap fit, and fail action. Doc/governance-only rows may use Pass Omega CRUD/G-Omega as the gate, but must still state cap-fit and fail route. | 3A, 3B, 3D, 3E, 3F |
| `CH4-V2-002` | Add `hard-cap fit` and `fail action` columns to the 3C per-clause cost matrix. For each `D-L*`, state whether it is Pass Omega doc-only, which wave consumes it, and the exact non-fit action: intrinsic-block, REDRESS/revert, or G-Omega wave-graph amendment; no W12 or challenge-time implementation overflow. | 3C |

## Accepted Ground

- T-P3 remains proposal-only; no live V1 spec surface was edited by the target
  packet.
- T-P1 remains clean-final/G1-auto-pinned, not normal §3Z.
- T-P2 remains normal §3Z locked at `cafb95682`.
- The 16-lock count and exact five-shape `BackendShape` canon are preserved.
- The 3C candidate matrix covers all 42 live LACs with zero silent drops.
- The SK-V15 PRUNE-before-REBUILD order remains intact, and implementation
  waves remain blocked until Pass Omega/G-Omega.
- V1 CH1, CH5, and CH6 repairs are closed; V1 CH4 high-risk cost repairs are
  closed in substance and only the row-field completeness defect remains.

## Next Action

Dispatch a V3 synthesis fold limited to `CH4-V2-001` and `CH4-V2-002`. The fold
must not edit live V1 spec surfaces. After V3 is committed, run CH1-CH7 again.
V3 can become clean-cycle 1 only if all seven lenses return `ACCEPT` with no
orphan `REVISE`.
