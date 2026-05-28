# HARDENING T-P3 V1 Consolidated

Target packet: `0a0508acd` (`docs(sk-v15-t-p3): add V1 synthesis packet`).
Challenge context: `3b8d99f2f` (`docs(sk-v15-t-p3): open V1 hardening context`).
Cycle: `V1`.

## Verdict

`REVISE`.

V1 does not count as a clean §3Z cycle. Seven lenses completed: CH2, CH3, and
CH7 returned `ACCEPT`; CH1, CH4, CH5, and CH6 returned `REVISE`; zero lenses
returned `REJECT`. Required executable checks passed across the wave: the target
packet has 807 insertions across seven T-P3 proposal artifacts; `git diff
--check` is clean; the extracted `3C-locks-v+1-diff.md` applies cleanly to the
current `LOCKS.md`; live invariants read 16 numbered locks and 67 Pattern H
runtime files.

The packet is directionally sound and proposal-only, but V2 must repair
path-resolution hygiene, cap-realism, hidden regex/runtime-DFA coupling, and
open-question routing before any clean-cycle claim.

## Lens Results

| lens | verdict | primary result |
|---|---|---|
| CH1 correctness | `REVISE` | 3C covers all 42 live 1E/2X LACs and the LOCKS diff applies cleanly, but V2 must remove the inherited out-of-range `2F-parse-that-gaps.md:518` citation from the proposed diff context and resolve the absent SK-V15 `ORCHESTRATOR-PROMPT.md` reference. |
| CH2 generality | `ACCEPT` | Lock 14 holds; no JSON narrowing, sixth `BackendShape`, directive, BIR variant, substrate, public substrate API, or retained sidecar enters the packet. Non-JSON proof stays CSS plus Sheets/BBNF-self. |
| CH3 regression | `ACCEPT` | No REDRESS route is reopened; stale SK-V13/SK-V14 receiver blocks remain historical/pre-block evidence; V3/V4/V5/V6/V7/V8 delete-before-provider failures are not reintroduced. |
| CH4 cost | `REVISE` | W4 Pattern H and W7-W9 Decision/lowerer budgets are not cap-real against T-P1/T-P2 cost carriers; 3C lacks per-clause cost/risk/wave/gate fields; CSS provider scope needs an explicit no-broad-CSSOM bound; 3F needs executable CRUD-4 cap handling. |
| CH5 hidden coupling | `REVISE` | The broader substrate/fact-stream boundary holds, but regex wording reintroduces legacy `bbnf-regex` as an active owner and can imply runtime regex/DFA admission through Lock 16 without restating the Lock 1/G-Omega substrate gate. |
| CH6 anti-paper-close | `REVISE` | 3A/3B/3C/3D/3E Open Questions do not consistently carry receiver/blocker/gate triads, and 3F leaves a small engineered-deferral aperture around CRUD-4 follow-up cleanup. |
| CH7 overfit-prune | `ACCEPT` | The packet fail-closes the PASS-IMPL V1 contrivance classes: wave-graph cycles, broadcast admission, gate exclusions, CSS fake parity, wrong-host close, FNV leakage, and delete-before-provider sequencing. |

## Required V2 Fold

| finding | required repair | owner |
|---|---|---|
| `CH1-V1-001` | Regenerate or rewrite `3C-locks-v+1-diff.md` so the proposed hunk context no longer repeats the stale out-of-range `restart/audit/totality/p2/2F-parse-that-gaps.md:518` citation. Preserve `git apply --check`. | 3C |
| `CH1-V1-002` | Resolve the absent SK-V15 `ORCHESTRATOR-PROMPT.md` reference by routing current SK-V15 authority to the extant `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`, unless a separately owned task creates the missing file before citation. | 3F / dispatch-context owner |
| `CH4-COST-01` | Replace W7/W8/W9 budget rows with the 2D costed bands, or explicitly narrow each to gate-only/intrinsic-block scope. Each row must state consumer/gate, hard-cap fit, fail action, and no-W12 route. | 3B, mirrored by 3A/3D/3E where referenced |
| `CH4-COST-02` | Split W4 Pattern H into provenance gate, generator/check proof, runtime projection, destructive deletion, and close-transcript sub-rows with bounded LOC, consumer/gate, fail action, and cap-fit statement. | 3B, mirrored by 3A/3D/3E |
| `CH4-COST-03` | Add a per-clause cost matrix for every 3C `D-L*` clause: doc LOC, risk class, affected waves, consuming gate or same-wave consumer, and propagation count. | 3C |
| `CH4-COST-04` | State explicitly that W5 is a scoped typed CSS provider, not a broad CSSOM rewrite. If CSSOM parity is required for close, route intrinsic block or G-Omega wave-graph amendment instead of hiding scope in W5/W6. | 3E with 3B/3D alignment |
| `CH4-COST-05` / `CH6-V1-02` | Replace the CRUD-4 follow-up-cleanup question with executable cap handling: either CRUD-4 completes current-state cleanup before G-Omega, or records a blocked/extension decision with exact remainder, receiver, blocker, and gate. Current dispatch truth blocks implementation until complete. | 3F |
| `CH5-V1-01` | Replace active `bbnf-regex` owner wording with canonical `parse-that-regex`, or mark `skinny/crates/bbnf-regex` only as a temporary legacy path awaiting Lock 11 rename and not an admissible future owner. Do not list both names as peer owners in Lock 16. | 3A and 3C |
| `CH5-V1-02` | Restate everywhere runtime regex/DFA appears: manifest and consumer proof are necessary but never sufficient for runtime substrate admission; any runtime regex/DFA substrate requires prior G-Omega amendment to Lock 1. | 3A and 3C |
| `CH6-V1-01` | Rewrite all Open Questions tables in 3A/3B/3C/3D/3E to include `receiver`, `blocker`, and `gate` fields for every row. Remove or answer rows that cannot name all three. | 3A, 3B, 3C, 3D, 3E |

## Accepted Ground

- T-P3 remains proposal-only; no live V1 spec surface was edited by the target
  packet.
- T-P1 is carried honestly as clean-final/G1-auto-pinned, not normal §3Z.
- T-P2 is carried as a normal §3Z lock.
- The 16-lock count and exact five-shape `BackendShape` canon are preserved.
- The 3C candidate matrix covers all 42 live LACs with zero silent drops.
- The SK-V15 PRUNE-before-REBUILD order remains intact, and implementation
  waves remain blocked until Pass Omega/G-Omega.

## Next Action

Dispatch a V2 synthesis fold over the seven V1 T-P3 artifacts. The fold must be
limited to the required repairs above and must not edit live V1 spec surfaces.
After V2 is committed, run CH1-CH7 again. V2 can become clean-cycle 1 only if
all seven lenses return `ACCEPT` with no orphan `REVISE`.
