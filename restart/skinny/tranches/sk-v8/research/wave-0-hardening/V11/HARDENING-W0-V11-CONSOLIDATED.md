# SK-V8 W0 Hardening V11 Consolidated

Date: 2026-05-18.

Target: `61d5cc3b4312883e026060174e876a0c18b34703`
(`fix(sk-v8-wave0): fold hardening V10 cost and metadata blockers`).

## Verdict

ACCEPT.

Accept rate: 6/6 = 100%.

Convergence: V11 is the first qualifying ACCEPT cycle after the V10 reset. W0
is not yet closed. ORCHESTRATOR Section 3Z still requires one further
consecutive qualifying ACCEPT cycle with zero critical defects and no unresolved
REVISE before W1-W6 may dispatch.

## Dispositions

| Lens | Verdict | Confidence | Disposition |
|---|---|---:|---|
| CH1 | ACCEPT | 95% | W0 manifest semantics are executable, strict admission rejects non-GO and hard-failure outcomes, strict-vs-strict discipline holds, and the V11 evidence commands resolve from the skinny workspace. |
| CH2 | ACCEPT | 96% | V11 remains report/gate telemetry only; no generic crate, directive, BIR, substrate, public API, `BackendShape`, or `UnionTape` surface moved; Lock 14/non-JSON obligations remain intact. |
| CH3 | ACCEPT | 96% | No REDRESS route or admitted row regressed; W0 row identities, outcomes, verdicts, run id, CostFacts sentinels, redress sentinel, Track 2 status, and substrate tuples remain pinned. |
| CH4 | ACCEPT | 95% | The live post-`00c3485a` report footprint is `118 insertions / 13 deletions`, under the `<=120` post-V6 W0 fold cap, and empty `arch`/`cpu`/`os`/`simd` metadata now fails closed. |
| CH5 | ACCEPT | 96% | No hidden coupling was introduced: no parallel substrate, sidecar producer, renamed scanner path, Track 1/Track 2 coupling, parser-owned cursor/facts, or W3 telemetry-substitution route. |
| CH6 | ACCEPT | 96% | V11 close claims are backed by live tests, gate replay, xtask checks, dynamic Criterion probes, frozen behavior diffs, row-count audit, and diff checks; no deferral or self-report closes V10 CH4. |

## Fold Status

V10 CH4 required four folds:

1. Bring the live telemetry-consumption source footprint under the existing
   `<=120` post-V6 W0 fold cap.
2. Tighten W0 build metadata validation so host `arch`/`cpu` and feature
   `arch`/`os`/`simd` values are non-empty, with exact `target_cpu=native`.
3. Add focused negative tests for empty host/feature payloads.
4. Preserve V10-accepted evidence for CostFacts sentinels, redress, Track 2,
   exact substrate tuples, run-id/content fingerprinting, Criterion-root
   filtering, frozen behavior diff, strict hard-failure rejection, and
   commit-sliced rollback.

All six lenses accepted that V11 folds these requirements without widening W0
scope.

## Evidence

- `git diff --numstat 00c3485a..61d5cc3b -- skinny/crates/bbnf-bench/src/report.rs`
  reported `118 13`.
- `git show --stat 61d5cc3b -- skinny/crates/bbnf-bench/src/report.rs`
  reported one touched file, `58 insertions / 109 deletions`.
- `cargo test -p bbnf-bench w0_ -- --nocapture` passed.
- `cargo test -p bbnf-bench strict -- --nocapture` passed.
- `cargo test -p bbnf-bench sidecar_same_run -- --nocapture` passed.
- `cargo test -p bbnf-bench` passed.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
  passed.
- `cargo xtask check-json`, `cargo xtask check-real-typed`, and
  `cargo xtask check-conformance` passed.
- Dynamic non-W0 Criterion-group injection was ignored by the W0 run id.
- Dynamic admitted-row Criterion mutation failed closed on run-id drift.
- The frozen W0 behavior-surface diff from `0bd16f6d` remained empty.
- `git diff --check` passed.
- CH4 rollback simulation in a throwaway clone succeeded for the W0
  implementation/fold slice through `61d5cc3b`.

## Residuals

- W0 still requires one more consecutive qualifying ACCEPT cycle. V12 should
  re-challenge the unchanged V11-accepted target unless new code or artifact
  movement occurs.
- W1-W6 remain blocked until V12 also ACCEPTs with no critical defect and no
  unresolved REVISE.
- CH1 noted that `sample_cost` validation is still shape-based in
  `report.rs`; this is not a W11 blocker because `gate-json` generates the
  field from finite timing data, but W1/W6 should consider numeric parsing if
  the manifest becomes externally supplied.

## Governance

The consecutive ACCEPT counter is now 1. Dispatch V12 immediately against the
unchanged `61d5cc3b` W0 target. Only after V12 ACCEPTs may W0 close and W1
planning/redress begin.
