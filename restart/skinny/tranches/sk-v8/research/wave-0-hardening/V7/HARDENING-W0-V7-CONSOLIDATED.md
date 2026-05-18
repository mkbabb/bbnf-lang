# SK-V8 W0 Hardening V7 Consolidated

Date: 2026-05-18.

Target: `f452e837d9711bc9ef9e266de6f088421e67ac42`
(`fix(sk-v8-wave0): fold hardening V6 run identity and cost governance`).

## Verdict

ACCEPT.

Accept rate: 6/6 = 100%.

Convergence: not yet complete. V7 is the first qualifying ACCEPT cycle after
the V6 rejection reset the counter. Under `restart/prompts/ORCHESTRATOR.md`
Section 3Z, W0 still requires one unchanged re-challenge cycle at >=95% ACCEPT,
with zero critical defects and no unresolved REVISE, before W0 closes. W1-W6
remain blocked.

## Dispositions

| Lens | Verdict | Confidence | Disposition |
|---|---|---:|---|
| CH1 | ACCEPT | 96% | Exact row identity, outcome/verdict, throughput baseline, run-id binding, strict-vs-strict admission, sidecar freshness, and row-manifest Criterion gates held under focused and full `bbnf-bench` tests plus copied-root Criterion injection. |
| CH2 | ACCEPT | 92% | Lock 14/generalization checks held across W0 owner surfaces, non-JSON proof xtasks, and gate replay. |
| CH3 | ACCEPT | 92% | No admitted-row regression, REDRESS route reopen, behavior-surface drift, stale run-id acceptance, or schema-only paper close found. |
| CH4 | ACCEPT | 93% | V6 cost-governance blocker is resolved: post-V6 fold is 67/7 LOC under the new cap, frozen behavior diff is empty, gate runtime is practical, and rollback simulation of named W0 commits plus HEAD had no conflicts. |
| CH5 | ACCEPT | 94% | No hidden coupling found; exact row-manifest fingerprinting, run-id closure, sidecar boundaries, and same-wave consumer boundaries held. |
| CH6 | ACCEPT | 95% | Anti-paper-close checks held; run-id drift failed closed, strict/stale sidecar checks held, and the artifact explicitly blocks W1-W6 until a second accept cycle. |

## Preserved Evidence

- `cargo test -p bbnf-bench w0_ -- --nocapture`
- `cargo test -p bbnf-bench strict -- --nocapture`
- `cargo test -p bbnf-bench sidecar_same_run -- --nocapture`
- `cargo test -p bbnf-bench`
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
- copied-root Criterion injection probes for unvalidated future rows,
  valid-fixture/unvalidated real-typed rows, and run-id drift
- `cargo xtask check-json`
- `cargo xtask check-real-typed`
- `cargo xtask check-conformance`
- frozen behavior-surface diff over grammar/runtime/tape/SIMD/codegen/generated/
  Track 2/parity/scan/materialization paths
- `git diff --check`

## Required V8 Action

Do not fold or edit W0 before the next challenge unless a critical defect is
found. Dispatch V8 as an unchanged re-challenge of target `f452e837`. If V8
also returns >=95% ACCEPT with no critical defects and no unresolved REVISE, W0
may close and W1 can dispatch under the SK-V8 packet.
