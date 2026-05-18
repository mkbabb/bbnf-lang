# SK-V8 W0 Hardening V12 Consolidated

Date: 2026-05-18.

Target: `61d5cc3b4312883e026060174e876a0c18b34703`
(`fix(sk-v8-wave0): fold hardening V10 cost and metadata blockers`).

## Verdict

ACCEPT.

Accept rate: 6/6 = 100%.

Convergence: achieved. V11 accepted 6/6 as the first qualifying cycle after the
V10 reset. V12 re-challenged the unchanged target and accepted 6/6 with zero
critical defects and no unresolved REVISE. This satisfies ORCHESTRATOR Section
3Z for W0.

W0 is closed. W1 may dispatch under the SK-V8 SPEC Section 4 entry gate.

## Dispositions

| Lens | Verdict | Confidence | Disposition |
|---|---|---:|---|
| CH1 | ACCEPT | 96% | No drift since V11; W0 manifest semantics remain executable, strict admission rejects non-GO and hard-failure outcomes, strict-vs-strict comparator discipline holds, and evidence commands resolve from the skinny workspace. |
| CH2 | ACCEPT | 97% | Lock 14 and grammar neutrality still hold; no JSON policy entered generic crates, no new directive/BIR/substrate/API/`BackendShape`/`UnionTape` surface appears, and non-JSON proof obligations remain unchanged. |
| CH3 | ACCEPT | 97% | No REDRESS route or admitted row regressed; W0 row identities, outcomes, verdicts, throughput, run id, CostFacts/redress/Track 2 sentinels, and substrate tuple consumption remain pinned. |
| CH4 | ACCEPT | 96% | The live post-`00c3485a` report footprint remains `118 insertions / 13 deletions`, empty metadata rejects, gate consumption is present, rerun evidence is current, and rollback remains commit-sliced. |
| CH5 | ACCEPT | 96% | No hidden coupling surfaced; W0 stays telemetry/report-local, sidecar signals stay planning-only, Track 1/Track 2 independence remains gate-consumed, and W3 remains blocked on its own plan/challenge. |
| CH6 | ACCEPT | 97% | V12 close claims are backed by live tests, gate replay, xtask checks, dynamic Criterion probes, row audit, frozen behavior diffs, and diff checks; no self-report or deferral closes W0. |

## Closure Basis

The target is unchanged from the V11-accepted W0 fold:

- Current source/report surfaces are unchanged after `61d5cc3b` except for the
  V11 hardening archive.
- `git diff --numstat 00c3485a..61d5cc3b -- skinny/crates/bbnf-bench/src/report.rs`
  remains `118 13`.
- The V10 empty-metadata blocker remains folded: host `arch`/`cpu` and feature
  `arch`/`os`/`simd` must be non-empty, with exact `target_cpu=native`.
- W0 still consumes CostFacts/redress/Track 2 sentinels, exact substrate tuples,
  run id, profile artifact, hot leaf, sample cost, build/host/feature metadata,
  sidecar freshness, and comparator evidence through `gate-json`.
- The frozen behavior-surface diff from `0bd16f6d` remains empty.

## Evidence

- `cargo test -p bbnf-bench w0_ -- --nocapture` passed.
- `cargo test -p bbnf-bench strict -- --nocapture` passed.
- `cargo test -p bbnf-bench sidecar_same_run -- --nocapture` passed.
- `cargo test -p bbnf-bench` passed.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
  passed against the captured W0 Criterion root.
- `cargo xtask check-json`, `cargo xtask check-real-typed`, and
  `cargo xtask check-conformance` passed.
- Dynamic non-W0 Criterion injection was ignored by the W0 run id.
- Dynamic admitted-row Criterion mutation failed closed on run-id drift.
- Row audit reported 38 manifest rows, 38 `gate_only` rows, 38 `SK-V8-open`
  rows, and 38 frozen-run-id rows.
- Frozen behavior-surface diffs and `git diff --check` passed.
- CH4 rollback simulation in a throwaway worktree exited 0 for the W0
  implementation/fold slice through `61d5cc3b`.

## Residuals Routed To Later Waves

- W1 owns replacing `none:pre-W1` CostFacts sentinels with real gate-consumed
  CostFacts before any behavior wave can cite route quality.
- CH1's residual `sample_cost` numeric parsing note is routed to W1/W6 if the
  manifest becomes externally supplied. It is not a W0 blocker because
  `gate-json` currently constructs the field from finite positive timing data.
- W3 remains blocked on W0/W1 admission plus a fresh accepted W3 plan/challenge.
  W0 `gate_only` telemetry is not a structural-projection production consumer.

## Governance

W0 satisfies the two-consecutive-cycle convergence rule:

- V11: 6/6 ACCEPT, minimum confidence 95%.
- V12: 6/6 ACCEPT, minimum confidence 96%.

W0 is admitted as the SK-V8 baseline profile and telemetry lock. Dispatch W1
CostFacts And Comparator Gate Binding next, under SPEC Section 4 and
DISPATCH-PROMPT conditional-wave gates.
