# HARDENING T-P3 V4 Consolidated

Target packet: `e6c1c2a84` (`docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`).
Challenge context: `40528179e` (`docs(sk-v15-t-p3): open V4 confirmation hardening context`).
Cycle: `V4`.

## Verdict

`REVISE`.

V4 does not lock T-P3. Seven lenses completed: CH2, CH3, CH4, CH5, CH6, and CH7
returned `ACCEPT`; CH1 returned `REVISE`; zero lenses returned `REJECT`.

Required executable checks otherwise passed across the wave: the target packet
scope is limited to the seven T-P3 proposal artifacts; `git diff --check` is
clean; the extracted `3C-locks-v+1-diff.md` applies cleanly to current
`LOCKS.md`; live invariants read 16 numbered locks and 67 Pattern H runtime
files; the stale-pattern scan is empty; and 3C covers all 42 live 1E/2A-2F
candidates.

The remaining defect is a single out-of-range citation in 3A. It is mechanical
and narrow, but it prevents V4 from being the second consecutive clean cycle.

## Lens Results

| lens | verdict | primary result |
|---|---|---|
| CH1 correctness | `REVISE` | `3A-architecture-synthesis.md:56` cites `restart/audit/totality/p3/hardening/V2/CH4.md:38`-`47`, but `CH4.md` has only 41 lines and the direct `CH4-V2-001` evidence is at line 36. |
| CH2 generality | `ACCEPT` | Lock 14 holds; no JSON narrowing, forbidden surface addition, retained sidecar, public substrate API, sixth `BackendShape`, new directive, new BIR variant, or new substrate enters the packet. |
| CH3 regression | `ACCEPT` | REDRESS routes are not reopened; stale receiver blocks remain historical/pre-block; delete-before-provider sequencing remains blocked; V3 through V8 corrective gates remain fail-closed. |
| CH4 cost | `ACCEPT` | The V3 CH4 coverage matrices close `CH4-V2-001` and `CH4-V2-002` without W12, broad CSSOM parity, challenge-time implementation overflow, or doc-only implementation gates. |
| CH5 hidden coupling | `ACCEPT` | No parallel substrate, sidecar producer, runtime regex/DFA substrate, Track 1 == Track 2 dishonesty, `FactStream` shape leak, x86 close evidence, or renamed-scanner Lock 1 violation remains. |
| CH6 anti-paper-close | `ACCEPT` | No prose closure, engineered deferral, uncited validation, G3/G-Omega confusion, CRUD-4 cleanup loophole, or unrouted open question remains. |
| CH7 overfit-prune | `ACCEPT` | Wave-graph cycles, broadcast admission, gate exclusions, CSS fake parity, wrong-host close evidence, FNV leakage, delete-before-provider sequencing, and self-exempting grep gates are blocked. |

## Required V5 Fold

| finding | required repair | owner |
|---|---|---|
| `CH1-V4-001` | In `restart/audit/totality/p3/3A-architecture-synthesis.md`, replace the out-of-range citation `restart/audit/totality/p3/hardening/V2/CH4.md:38`-`47` with an in-range citation to the direct `CH4-V2-001` evidence at `restart/audit/totality/p3/hardening/V2/CH4.md:36`, preserving the V3 repair claim and rerunning path-line validation. | 3A |

## Accepted Ground

- T-P3 remains proposal-only; no live V1 spec surface was edited by the target
  packet.
- T-P1 remains clean-final/G1-auto-pinned, not normal §3Z.
- T-P2 remains normal §3Z locked at `cafb95682`.
- The 16-lock count and exact five-shape `BackendShape` canon are preserved.
- The 3C candidate matrix covers all 42 live LACs with zero silent drops.
- The SK-V15 PRUNE-before-REBUILD order remains intact, and implementation
  waves remain blocked until Pass Omega/G-Omega.
- V3 CH4 coverage is substantively accepted; V5 should be a citation-only fold.

## Next Action

Dispatch a V5 fold limited to `CH1-V4-001`, then run confirmation hardening. V5
can lock T-P3 only if all seven lenses return `ACCEPT` with zero orphan
`REVISE`.
