# SK-V8 W0 Hardening V9 CH5

Date: 2026-05-18.

Target reviewed: `00c3485a8774296e796c2f68b74fd3d559627f0a`
(`fix(sk-v8-wave0): fold hardening V8 strict hard-failure blocker`).

## Verdict

ACCEPT.

Confidence: 95%.

This CH5 result can count as a V9 ACCEPT contribution for W0, but it cannot
close W0 by itself. V8 rejected and reset the consecutive ACCEPT counter
(`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/HARDENING-W0-V8-CONSOLIDATED.md:14`);
the orchestrator requires two consecutive challenge cycles at >=95% ACCEPT with
zero open critical defects and no orphan unresolved REVISE
(`restart/prompts/ORCHESTRATOR.md:120`). W1-W6 remain blocked under the live
handoff (`restart/skinny/tranches/sk-v8/HANDOFF.md:236`).

## Reviewed Surfaces

- CH5 hidden-coupling lens and no-paper-close governance:
  `restart/prompts/ORCHESTRATOR.md:87`,
  `restart/prompts/ORCHESTRATOR.md:88`,
  `restart/prompts/ORCHESTRATOR.md:116`.
- Strict comparator discipline and W0 hard-failure boundary:
  `restart/skinny/tranches/sk-v8/SPEC.md:65`,
  `restart/skinny/tranches/sk-v8/SPEC.md:73`,
  `restart/skinny/tranches/sk-v8/SPEC.md:97`,
  `restart/skinny/tranches/sk-v8/SPEC.md:203`.
- W0 telemetry-only and frozen behavior-surface rules:
  `restart/skinny/tranches/sk-v8/SPEC.md:348`,
  `restart/skinny/tranches/sk-v8/SPEC.md:357`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:139`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:154`.
- V8 required fold:
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/HARDENING-W0-V8-CONSOLIDATED.md:30`.
- Current W0 gate/report code:
  `skinny/crates/bbnf-bench/src/gate.rs:135`,
  `skinny/crates/bbnf-bench/src/report.rs:328`,
  `skinny/crates/bbnf-bench/src/report.rs:499`,
  `skinny/crates/bbnf-bench/src/report.rs:1012`,
  `skinny/crates/bbnf-bench/src/report.rs:1240`.

## Challenge Result

No material CH5 blocker found.

The V8 strict relabel route is closed in both places V8 required. Helper-level
strict admission now rejects every non-`GO` outcome before comparator evidence is
considered (`skinny/crates/bbnf-bench/src/gate.rs:139`), and its regression test
covers `D`, `E`, `F-positive`, `F-noise`, `G`, `I`, `J`, `K`, `L`, `M`,
`N-direct`, and `S` (`skinny/crates/bbnf-bench/src/gate.rs:459`). Full W0 report
validation also freezes current opening rows as `strictness=deferred`,
`measured_validation_path=view-boundary`, `parse_utf8=view-boundary`, and
`escape_complete=yes` (`skinny/crates/bbnf-bench/src/report.rs:1012`). The
specific Canada hard-failure repro is covered by mutating
`json/canada/parse_only/main` to strict/measured/DOM while preserving the W0
row; the report must reject it (`skinny/crates/bbnf-bench/src/report.rs:1954`).

I did not find a remaining strict-vs-strict bypass through comparator evidence.
W0 validates exact row identity, outcome, verdict, run id, and throughput
baseline before accepting a report (`skinny/crates/bbnf-bench/src/report.rs:328`,
`skinny/crates/bbnf-bench/src/report.rs:336`,
`skinny/crates/bbnf-bench/src/report.rs:517`,
`skinny/crates/bbnf-bench/src/report.rs:529`). Native comparator evidence is
still checked for expected workload plane, strictness, same-run-native freshness,
`sidecar_freshness=n/a`, value presence, and exact Criterion source
(`skinny/crates/bbnf-bench/src/report.rs:1240`). Because W0 rows themselves are
frozen as deferred/view-boundary, those strict anchors remain telemetry and
cannot become W0 strict admission.

I also did not find a hidden substrate or production-consumer route. The only
implementation fold since V8 touches `skinny/crates/bbnf-bench/src/gate.rs` and
`skinny/crates/bbnf-bench/src/report.rs`; the frozen behavior-surface diff over
runtime/parser/tape/SIMD/codegen/generated/Track2/parity/scan/materialization/
fixtures/grammar paths is empty. `gate-json` remains the same-wave W0 consumer,
and later waves are still blocked from treating W0 telemetry, sidecars, or
CostFacts placeholders as production evidence.

## Commands And Evidence

- `git show --stat --oneline --name-status 00c3485a`: only `gate.rs` and
  `report.rs` changed in the V9 fold.
- `git diff --name-only 0bd16f6d..HEAD -- <frozen behavior paths>`: no output.
- `cargo test -p bbnf-bench w0_ -- --nocapture` from `skinny/`: PASS, 20 focused
  W0 tests.
- `cargo test -p bbnf-bench strict -- --nocapture` from `skinny/`: PASS, 5
  strict-admission tests.
- `cargo test -p bbnf-bench sidecar_same_run -- --nocapture` from `skinny/`:
  PASS.
- `cargo test -p bbnf-bench` from `skinny/`: PASS, 52 lib tests, 8 gate-bin
  tests, 0 doctests.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
  from `skinny/`: PASS; rendered all W0 rows as deferred/view-boundary and
  retained `Overall outcome N-direct / NoGo`.
- `git diff --check`: PASS.

## Blockers

None.

## Required Fold

None for CH5.

## Residual Risks

- This is a V9 CH5 ACCEPT only, not W0 convergence. One more consecutive
  qualifying challenge cycle is still required after a consolidated V9 ACCEPT.
- `none:pre-W1` CostFacts placeholders remain acceptable only for W0. W1 must
  still make missing CostFacts evidence fatal before later waves cite it.
- FNV64 remains stale-artifact identity evidence, not a security boundary.
