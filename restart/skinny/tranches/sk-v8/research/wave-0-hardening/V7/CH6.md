# SK-V8 W0 Hardening V7 CH6

Date: 2026-05-18.

Target: `f452e8373ed717731dd5e720c1d947c086cc22c9`
(`fix(sk-v8-wave0): fold hardening V6 run identity and cost governance`).

## Verdict

ACCEPT.

Confidence: 95%.

This is an anti-paper-close accept for the V7 fold only. The V6 CH1 paper-close
hole now has executable failure evidence: `gate-json` computes the run fingerprint
from the W0 Criterion inputs, stamps every W0 row with that run id, and the report
validator rejects any row whose run id is not the frozen `SK-V8-open` id
(`skinny/crates/bbnf-bench/src/bin/gate.rs:383`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:390`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:474`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:490`,
`skinny/crates/bbnf-bench/src/report.rs:336`,
`skinny/crates/bbnf-bench/src/report.rs:660`). The V6 CH4 cost-governance reject
is also folded into the live packet by reauthorizing the measured W0 gate/report/
Lock 14 slice and limiting post-V6 work to run-id binding and focused tests
(`restart/skinny/tranches/sk-v8/SPEC.md:322`,
`restart/skinny/tranches/sk-v8/SPEC.md:339`,
`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`).

This does not close W0. V6 was a REJECT and reset the consecutive-ACCEPT counter
(`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:10`,
`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:61`);
ORCHESTRATOR requires two consecutive qualifying ACCEPT cycles before convergence
(`restart/prompts/ORCHESTRATOR.md:118`,
`restart/prompts/ORCHESTRATOR.md:120`). A V7 consolidated ACCEPT would be only
the first qualifying cycle after V6 rejection.

## Reviewed Surfaces

- ORCHESTRATOR CH6 and convergence governance:
  `restart/prompts/ORCHESTRATOR.md:74`,
  `restart/prompts/ORCHESTRATOR.md:88`,
  `restart/prompts/ORCHESTRATOR.md:118`,
  `restart/prompts/ORCHESTRATOR.md:120`.
- SK-V8 W0 requirements, cost fold, sidecar boundary, and dispatch locks:
  `restart/skinny/tranches/sk-v8/SPEC.md:73`,
  `restart/skinny/tranches/sk-v8/SPEC.md:103`,
  `restart/skinny/tranches/sk-v8/SPEC.md:142`,
  `restart/skinny/tranches/sk-v8/SPEC.md:288`,
  `restart/skinny/tranches/sk-v8/SPEC.md:322`,
  `restart/skinny/tranches/sk-v8/SPEC.md:346`,
  `restart/skinny/tranches/sk-v8/SPEC.md:360`,
  `restart/skinny/tranches/sk-v8/SPEC.md:372`.
- Handoff and dispatch prompt:
  `restart/skinny/tranches/sk-v8/HANDOFF.md:5`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:40`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:148`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:238`;
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:6`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:87`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:99`.
- Current RESULTS and REDRESS surfaces:
  `skinny/RESULTS.md:44`,
  `skinny/RESULTS.md:46`,
  `skinny/RESULTS.md:48`,
  `skinny/RESULTS.md:85`,
  `skinny/RESULTS.md:141`;
  `skinny/REDRESS.md:43`.
- Prior V6 rejection and required V7 fold:
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:14`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:22`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:25`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:29`.
- W0 implementation code:
  `skinny/crates/bbnf-bench/src/report.rs`,
  `skinny/crates/bbnf-bench/src/gate.rs`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs`,
  `skinny/xtask/src/main.rs`.

## Commands And Evidence

- `git rev-parse HEAD`: `f452e8373ed717731dd5e720c1d947c086cc22c9`.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v7-test-target RUSTFLAGS='-C target-cpu=native' cargo test -p bbnf-bench w0_report_accepts_exact_opening_baseline -- --nocapture`: PASS. The full-baseline test now rejects both a single-row `sk-v8-open:test` mutation and a uniform `sk-v8-open:test` mutation (`skinny/crates/bbnf-bench/src/report.rs:1976`, `skinny/crates/bbnf-bench/src/report.rs:1980`).
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v7-test-target RUSTFLAGS='-C target-cpu=native' cargo test -p bbnf-bench w0_ -- --nocapture`: PASS; 12 report W0 tests and 8 gate-bin W0 tests passed.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v7-test-target RUSTFLAGS='-C target-cpu=native' cargo test -p bbnf-bench strict -- --nocapture`: PASS; strict admission rejects deferred/view-boundary claims, `K`/`S`, stale sidecars, plane mismatch, and unstructured sidecar same-run evidence.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v7-test-target RUSTFLAGS='-C target-cpu=native' cargo test -p bbnf-bench sidecar_same_run -- --nocapture`: PASS.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`: PASS against committed `skinny/RESULTS.md`.
- Dynamic run-id drift probe: copied `/tmp/skv8-w0-target/criterion` to `/tmp/skv8-ch6-v7-runid.*`, appended one newline to the copied `criterion/json_twitter/track1_generated/new/estimates.json`, then ran `CARGO_TARGET_DIR=<tmp> RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`. Expected failure occurred: output reported `Schema/W0 validation failure: json/twitter/parse_only/main run_id moved from SK-V8-open baseline sk-v8-open:criterion-fnv64-9a37562ed3d0383a to sk-v8-open:criterion-fnv64-a5417170e7ed57aa`; command exit status was nonzero.
- `awk` over `skinny/RESULTS.md`: `main_rows=38`, `manifest_rows=38`.
- `git diff --name-only 0bd16f6d..HEAD -- skinny/crates/runtime skinny/crates/bbnf-simd skinny/crates/codegen skinny/crates/generated-json skinny/crates/test-fixtures skinny/crates/bbnf-bench/benches skinny/crates/bbnf-bench/src/direct.rs skinny/crates/bbnf-bench/src/scan.rs skinny/crates/bbnf-bench/src/real_typed_struct.rs`: no output; no frozen behavior-surface paths changed.
- `git diff --check`: PASS.

## Findings

1. No blocker: the V6 `run_id` paper close is folded into live gate behavior.
   `RunFacts::probe()` computes `sk-v8-open:criterion-fnv64-*` from admitted W0
   Criterion inputs (`skinny/crates/bbnf-bench/src/bin/gate.rs:383`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:390`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:673`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:717`), every row receives that run
   id (`skinny/crates/bbnf-bench/src/bin/gate.rs:474`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:490`), and validation rejects drift
   before RESULTS comparison (`skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:327`,
   `skinny/crates/bbnf-bench/src/report.rs:336`,
   `skinny/crates/bbnf-bench/src/report.rs:340`). The dynamic copied-root
   mutation exercises the live `gate-json` path, not just a unit fixture.

2. No blocker: W0 closure claims are backed by live gate evidence, not only text.
   The current report has 38 manifest rows and states that `gate-json` consumes
   the manifest (`skinny/RESULTS.md:44`, `skinny/RESULTS.md:48`,
   `skinny/RESULTS.md:85`, `skinny/RESULTS.md:141`). `gate-json` builds the
   report, validates schema plus W0 semantics, and only then compares committed
   RESULTS (`skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:329`). The passing gate replay used
   the warmed W0 target and matched committed RESULTS.

3. No blocker: required W0 fields are consumed by the report/gate path. The
   validator reads every required telemetry text field, then adds semantic gates
   for grammar/domain, row identity, outcome, wave, run id, sample count/cost,
   profile artifact, hot leaf, CostFacts alternatives, same-wave consumer,
   comparator evidence, baseline row count, outcome/verdict drift, missing rows,
   and Track 1/Track 2 drift (`skinny/crates/bbnf-bench/src/report.rs:277`,
   `skinny/crates/bbnf-bench/src/report.rs:322`,
   `skinny/crates/bbnf-bench/src/report.rs:328`,
   `skinny/crates/bbnf-bench/src/report.rs:330`,
   `skinny/crates/bbnf-bench/src/report.rs:342`,
   `skinny/crates/bbnf-bench/src/report.rs:349`,
   `skinny/crates/bbnf-bench/src/report.rs:355`,
   `skinny/crates/bbnf-bench/src/report.rs:361`,
   `skinny/crates/bbnf-bench/src/report.rs:375`,
   `skinny/crates/bbnf-bench/src/report.rs:499`,
   `skinny/crates/bbnf-bench/src/report.rs:517`,
   `skinny/crates/bbnf-bench/src/report.rs:529`).

4. No blocker: sidecar, lossy, permissive, stale, historical, and view-boundary
   evidence cannot strict-admit. Strict admission requires a strict row, measured
   UTF-8, measured validation, complete escapes, matching output plane, strict
   comparator evidence, same-run native freshness, and `sidecar_freshness=n/a`
   (`skinny/crates/bbnf-bench/src/gate.rs:135`,
   `skinny/crates/bbnf-bench/src/gate.rs:145`,
   `skinny/crates/bbnf-bench/src/gate.rs:151`,
   `skinny/crates/bbnf-bench/src/gate.rs:157`,
   `skinny/crates/bbnf-bench/src/gate.rs:160`,
   `skinny/crates/bbnf-bench/src/gate.rs:163`,
   `skinny/crates/bbnf-bench/src/gate.rs:172`). W0 non-strict rows remain
   deferred/view-boundary (`skinny/crates/bbnf-bench/src/report.rs:1012`,
   `skinny/crates/bbnf-bench/src/report.rs:1022`), sidecar comparators reject
   `sidecar-same-run` without a structured manifest
   (`skinny/crates/bbnf-bench/src/report.rs:1211`,
   `skinny/crates/bbnf-bench/src/report.rs:1235`), and native comparator sources
   are workload/plane-specific with `sidecar_freshness=n/a`
   (`skinny/crates/bbnf-bench/src/report.rs:1261`,
   `skinny/crates/bbnf-bench/src/report.rs:1272`,
   `skinny/crates/bbnf-bench/src/report.rs:1303`).

5. No blocker: V7 is not framed as W0 closure. The live packet says W0 is the
   only currently authorized implementation wave, and W1-W6 remain blocked until
   W0 closes plus their own plans, owner paths, row gates, challenges, and
   dispatch authority (`restart/skinny/tranches/sk-v8/SPEC.md:31`,
   `restart/skinny/tranches/sk-v8/SPEC.md:36`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:236`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:238`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:99`).

## Material Blockers

None found.

## Residual Risks

- If V7 consolidates as ACCEPT, it is only the first consecutive qualifying
  cycle after V6 rejection. W0 still needs a second consecutive qualifying
  challenge cycle with zero open critical defects before closure or W1-W6 dispatch
  can be claimed (`restart/prompts/ORCHESTRATOR.md:120`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:61`).
- Some W0 telemetry fields are consumed as required present text plus producer
  generation discipline rather than deep enum validation, notably host/build
  strings, `redress_entry`, and `track2_independence_status`
  (`skinny/crates/bbnf-bench/src/report.rs:287`,
  `skinny/crates/bbnf-bench/src/report.rs:295`,
  `skinny/crates/bbnf-bench/src/report.rs:313`). I do not classify this as a
  CH6 blocker because the material W0 closure predicates are gate-bound: row
  identity, run id, sample/profile evidence, strict admission, sidecar freshness,
  and baseline drift.
- The run-id binding is a frozen W0-opening fingerprint, not a general future
  report parser contract. Later waves that refresh the benchmark root will need a
  deliberately versioned opening baseline or a new accepted gate rule rather than
  reusing the W0 constant.
