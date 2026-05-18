# SK-V8 W0 Hardening V9 CH1 - Correctness

Date: 2026-05-18.

Target reviewed: `00c3485a8774296e796c2f68b74fd3d559627f0a`
(`fix(sk-v8-wave0): fold hardening V8 strict hard-failure blocker`).

Verdict: ACCEPT.

Confidence: 96%.

CH1 reviewed the V8 blocker fold for correctness/admission/strictness. I found
the hard-failure strict-admission hole closed in both required places: current
W0 rows are frozen as deferred/view-boundary baseline telemetry, and strict
admission now rejects every non-`GO` outcome before considering comparator
freshness, plane, or measured-validation evidence. I found no W0 row that can
be relabeled as strict/measured/admitted while preserving baseline identity, and
I found no path for hard-failure outcomes to reach strict admission.

## Reviewed Surfaces

- CH1 authority and convergence rule:
  `restart/prompts/ORCHESTRATOR.md:83`,
  `restart/prompts/ORCHESTRATOR.md:120`.
- SK-V8 strictness and outcome contract:
  `restart/skinny/tranches/sk-v8/SPEC.md:73`,
  `restart/skinny/tranches/sk-v8/SPEC.md:79`,
  `restart/skinny/tranches/sk-v8/SPEC.md:97`,
  `restart/skinny/tranches/sk-v8/SPEC.md:100`.
- Current W0 posture:
  `restart/skinny/tranches/sk-v8/HANDOFF.md:42`,
  `skinny/RESULTS.md:10`,
  `skinny/RESULTS.md:141`.
- V8 blocker and required fold:
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/HARDENING-W0-V8-CONSOLIDATED.md:23`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/HARDENING-W0-V8-CONSOLIDATED.md:32`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/HARDENING-W0-V8-CONSOLIDATED.md:36`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/HARDENING-W0-V8-CONSOLIDATED.md:40`.
- Folded code:
  `skinny/crates/bbnf-bench/src/gate.rs:72`,
  `skinny/crates/bbnf-bench/src/gate.rs:135`,
  `skinny/crates/bbnf-bench/src/report.rs:275`,
  `skinny/crates/bbnf-bench/src/report.rs:328`,
  `skinny/crates/bbnf-bench/src/report.rs:1012`.

## Commands And Evidence

- `git rev-parse HEAD` returned
  `00c3485a8774296e796c2f68b74fd3d559627f0a`.
- `git status --short` returned clean before this report file was added.
- `git diff --unified=80 f452e837..HEAD -- skinny/crates/bbnf-bench/src/gate.rs skinny/crates/bbnf-bench/src/report.rs`
  shows only the V8 blocker fold in the reviewed code: strict admission changed
  from rejecting only `K`/`S` to rejecting any outcome whose `verdict() !=
  Verdict::Go`, and W0 row admission changed from a strict-claim branch to an
  unconditional deferred/view-boundary freeze.
- `cargo test -p bbnf-bench strict -- --nocapture` passed: 5 tests.
- `cargo test -p bbnf-bench w0_report_accepts_exact_opening_baseline -- --nocapture`
  passed: the exact-baseline fixture rejects the V8 repro shape.
- `cargo test -p bbnf-bench w0_ -- --nocapture` passed: 20 focused W0 tests.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
  passed against committed `skinny/RESULTS.md` and retained `Overall outcome
  N-direct / NoGo`.
- `awk -F'|' ... skinny/RESULTS.md` census returned
  `main=38 deferred=38 strict=0 hard_or_nonadmit=31 admit_like=7`.
- `git diff --name-only 0bd16f6d..HEAD -- skinny/crates/runtime skinny/crates/codegen skinny/crates/bbnf-simd skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/generated_json_track2.rs skinny/grammars skinny/fixtures`
  returned no paths, so the checked parser/scanner/SIMD/codegen/generated
  behavior surfaces did not move in this fold.

## Findings

1. The hard-failure strict-admission helper hole is closed.

   `Outcome::verdict()` maps `G`, `I`, `K`, `L`, `M`, `N-direct`, and `S` to
   `NoGo`, `J` to `Invalid`, and `D`/`E`/`F-*` to non-`Go` dispositions
   (`skinny/crates/bbnf-bench/src/gate.rs:72`,
   `skinny/crates/bbnf-bench/src/gate.rs:78`,
   `skinny/crates/bbnf-bench/src/gate.rs:82`,
   `skinny/crates/bbnf-bench/src/gate.rs:83`). `validate_strict_admission()`
   now rejects any parsed outcome whose verdict is not `Go` before testing row
   strictness or comparator metadata (`skinny/crates/bbnf-bench/src/gate.rs:135`,
   `skinny/crates/bbnf-bench/src/gate.rs:139`). The focused unit test proves
   `D`, `E`, `F-positive`, `F-noise`, `G`, `I`, `J`, `K`, `L`, `M`,
   `N-direct`, and `S` all fail strict admission while the canonical `A` strict
   evidence still passes (`skinny/crates/bbnf-bench/src/gate.rs:459`,
   `skinny/crates/bbnf-bench/src/gate.rs:461`,
   `skinny/crates/bbnf-bench/src/gate.rs:462`,
   `skinny/crates/bbnf-bench/src/gate.rs:478`).

2. Current W0 rows can no longer be relabeled strict/measured while preserving
   baseline identity.

   `TelemetryRow::validate_sk_v8_w0()` still validates required telemetry,
   row identity, W0 outcome vocabulary, `SK-V8-open` marker, exact run id,
   profile artifact, hot leaf, CostFacts alternatives, parse non-admission,
   and comparator evidence (`skinny/crates/bbnf-bench/src/report.rs:275`,
   `skinny/crates/bbnf-bench/src/report.rs:328`,
   `skinny/crates/bbnf-bench/src/report.rs:336`,
   `skinny/crates/bbnf-bench/src/report.rs:349`,
   `skinny/crates/bbnf-bench/src/report.rs:367`,
   `skinny/crates/bbnf-bench/src/report.rs:375`). The admission-boundary
   branch now unconditionally requires `strictness=deferred`,
   `measured_validation_path=view-boundary`, `parse_utf8=view-boundary`, and
   `escape_complete=yes` for W0 rows (`skinny/crates/bbnf-bench/src/report.rs:1012`,
   `skinny/crates/bbnf-bench/src/report.rs:1013`,
   `skinny/crates/bbnf-bench/src/report.rs:1019`,
   `skinny/crates/bbnf-bench/src/report.rs:1025`,
   `skinny/crates/bbnf-bench/src/report.rs:1031`). This satisfies the V8 fold
   requirement that a strict/measured-row relabel of any current W0 row reject.

3. The exact V8 repro is covered.

   The report-level baseline test builds every `SK_V8_OPEN_BASELINE` row,
   verifies the exact opening baseline, then mutates
   `json/canada/parse_only/main` from exact `L / NO-GO` baseline telemetry to
   `strict`, `measured-row`, and `DOM`; validation rejects that mutated report
   (`skinny/crates/bbnf-bench/src/report.rs:1920`,
   `skinny/crates/bbnf-bench/src/report.rs:1954`,
   `skinny/crates/bbnf-bench/src/report.rs:1958`,
   `skinny/crates/bbnf-bench/src/report.rs:1960`,
   `skinny/crates/bbnf-bench/src/report.rs:1964`). A row-level test also
   rejects a strict/measured W0 claim on an otherwise valid current row
   (`skinny/crates/bbnf-bench/src/report.rs:1673`,
   `skinny/crates/bbnf-bench/src/report.rs:1692`,
   `skinny/crates/bbnf-bench/src/report.rs:1696`).

4. Baseline identity and strictness plane remain aligned with the committed
   result surface.

   The committed W0 table keeps the current `canada/parse_only` hard failure as
   `L / NO-GO`, `Strictness=deferred`, `parse_utf8=view-boundary`, and borrowed
   view output plane (`skinny/RESULTS.md:10`). The report-level census found 38
   main rows, 38 deferred rows, zero strict rows, and 31 hard/non-admission rows.
   The live `gate-json --advisory --check-results` run passed against that same
   surface, so the executable gate and rendered report agree.

## Blockers

None.

## Required Fold

None for CH1. V9 may count as the first qualifying ACCEPT after the V8
correctness reset; W0 still needs a second consecutive accepting challenge cycle
under `restart/prompts/ORCHESTRATOR.md:120` before closure.

## Residual Risk

`validate_w0_outcome()` still accepts internal W0 enum spellings such as `I`,
`J`, and `M` before report-level exact-baseline validation
(`skinny/crates/bbnf-bench/src/report.rs:960`,
`skinny/crates/bbnf-bench/src/report.rs:964`). This is not a CH1 blocker for
the current W0 closure candidate because exact baseline validation prevents row
movement and strict admission rejects all non-`GO` outcomes, but a later cleanup
should align the row-level allowlist with the rendered SPEC vocabulary.
