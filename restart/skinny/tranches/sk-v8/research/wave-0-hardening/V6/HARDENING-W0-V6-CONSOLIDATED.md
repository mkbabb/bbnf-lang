# SK-V8 W0 Hardening V6 Consolidated

Date: 2026-05-18.

Target: `6c0bc15d44142abf0b965d9daee7070b1f32dd99`
(`fix(sk-v8-wave0): fold hardening V5 row identity blockers`).

## Verdict

REJECT.

Accept rate: 4/6 = 66.7%.

Convergence: not achieved. V6 is not a qualifying ACCEPT cycle under
`restart/prompts/ORCHESTRATOR.md` Section 3Z because CH1 and CH4 found material
W0 gate and cost-governance blockers. W1-W6 remain blocked.

## Dispositions

| Lens | Verdict | Confidence | Disposition |
|---|---|---:|---|
| CH1 | REJECT | 94% | `run_id` is only checked as a required non-empty telemetry string; `Report::validate_sk_v8_w0()` still accepts `sk-v8-open:test`, so row evidence is not bound to the computed Criterion fingerprint. |
| CH2 | ACCEPT | 86% | Lock 14 and non-JSON proof checks held; the lower confidence records a stale local target residual, not a material CH2 blocker. |
| CH3 | ACCEPT | 96% | V5 row-manifest fingerprint blocker is closed; copied-root valid-fixture/unvalidated-row mutation no longer perturbs W0. |
| CH4 | REJECT | 94% | W0 implementation scope exceeds the stated `<=350` report/gate/schema/test/doc LOC cap and weakens the promised one-slice revert protocol. |
| CH5 | ACCEPT | 96% | Hidden-coupling checks held: exact row-manifest fingerprinting, row identity gates, and Track 1/Track 2 separation remain intact. |
| CH6 | ACCEPT | 96% | Anti-paper-close checks held; V6 would have counted only as a first qualifying cycle if it had accepted. |

## Required V7 Fold

1. Bind W0 `run_id` validation to an exact `SK-V8-open` fingerprint value, not
   merely a non-empty string.
2. Reject mixed `run_id` values across W0 rows.
3. Add negative tests for a same-prefix non-fingerprint value such as
   `sk-v8-open:test` and for a single-row run-id mutation.
4. Preserve exact row identity, outcome/verdict, throughput, comparator,
   deferred validation, sidecar, and row-manifest fingerprint gates from the V5
   fold.
5. Resolve the W0 cost blocker by making the larger W0 report/gate/Lock 14
   scope explicit in the governing packet, including a realistic rollback
   protocol and exact accounting that separates generated/results artifacts
   from source/test gate code. If the scope is not re-authorized, split and
   route excess hardening before W0 can close.

## Evidence To Rerun After Fold

- `cargo test -p bbnf-bench w0_ -- --nocapture`
- `cargo test -p bbnf-bench`
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
- dynamic `json_unvalidated_future` Criterion mutation
- dynamic valid-fixture/unvalidated-row Criterion mutation
- `cargo xtask check-json`
- `cargo xtask check-real-typed`
- `cargo xtask check-conformance`
- frozen behavior-surface diff
- W0 source/results LOC accounting from the admitted baseline
- `git diff --check`

## Governance

V6 rejection resets the consecutive ACCEPT counter. After the V7 fold, W0 must
receive two consecutive challenge cycles at at least 95% ACCEPT, with no open
critical defects, before W0 can close and W1-W6 can dispatch.
