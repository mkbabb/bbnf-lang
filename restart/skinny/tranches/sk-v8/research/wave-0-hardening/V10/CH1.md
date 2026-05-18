# SK-V8 W0 Hardening V10 CH1 - Correctness

Date: 2026-05-18.

Target reviewed: `3a9fa32675cedb1f8a0d31247df229fe505068be`
(`fix(sk-v8-wave0): fold hardening V9 telemetry consumption blocker`).

Verdict: ACCEPT.

Confidence: 97%.

CH1 reviewed correctness/admission/strictness after the V9 telemetry-consumption
fold. The V8 strict-hard-failure blocker remains closed: current W0 rows are
still frozen as deferred/view-boundary baseline telemetry, strict admission still
rejects every non-`GO` outcome before comparator evidence can matter, and the
V9 manifest-semantic fold did not add a route that can relabel a W0 row as
strict/measured/admitted while preserving baseline identity.

## Reviewed Surfaces

- CH1 authority: correctness claims, measurable gates, and comparator deltas on
  the strictness plane (`restart/prompts/ORCHESTRATOR.md:83`,
  `restart/prompts/ORCHESTRATOR.md:208`).
- SK-V8 strictness contract: strict admission requires matching row/comparator
  plane, strict same-run native comparator evidence, and in-row validation;
  deferred/view-boundary, stale sidecar, sidecar-only, historical, and plane
  mismatch evidence are guard telemetry only
  (`restart/skinny/tranches/sk-v8/SPEC.md:73`,
  `restart/skinny/tranches/sk-v8/SPEC.md:79`).
- SK-V8 outcome contract: `L`, `N-direct`, and `S` are current W0 outcomes, but
  neither hard-failure outcomes nor `S` may support strict SOTA admission
  (`restart/skinny/tranches/sk-v8/SPEC.md:97`,
  `restart/skinny/tranches/sk-v8/SPEC.md:100`).
- W0 exit gate: all 38 current rows satisfy required telemetry, parse rows stay
  substrate-guard non-admission or preserved hard failure, sidecars remain
  non-manifest planning signals, and no behavior surface may move
  (`restart/skinny/tranches/sk-v8/SPEC.md:348`,
  `restart/skinny/tranches/sk-v8/SPEC.md:350`,
  `restart/skinny/tranches/sk-v8/SPEC.md:353`,
  `restart/skinny/tranches/sk-v8/SPEC.md:357`).
- V8 blocker: a hard-failure row could previously become a strict measured
  claim while preserving exact `L / NO-GO`, throughput, row id, and run id
  (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/HARDENING-W0-V8-CONSOLIDATED.md:23`).
- V8 required fold: freeze W0 rows as deferred/view-boundary, reject hard
  failure/non-admission outcomes in strict admission, add the canada repro
  negative, and preserve row identity/run-id/throughput evidence
  (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/HARDENING-W0-V8-CONSOLIDATED.md:32`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/HARDENING-W0-V8-CONSOLIDATED.md:36`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/HARDENING-W0-V8-CONSOLIDATED.md:40`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/HARDENING-W0-V8-CONSOLIDATED.md:44`).
- V9 required fold: semantic consumption for substrate tuple, CostFacts
  sentinel, redress, Track 2 independence, build/run metadata, and negatives
  while preserving the accepted V9 strictness fixes
  (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V9/HARDENING-W0-V9-CONSOLIDATED.md:31`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V9/HARDENING-W0-V9-CONSOLIDATED.md:38`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V9/HARDENING-W0-V9-CONSOLIDATED.md:42`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V9/HARDENING-W0-V9-CONSOLIDATED.md:44`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V9/HARDENING-W0-V9-CONSOLIDATED.md:48`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V9/HARDENING-W0-V9-CONSOLIDATED.md:51`).

## Concrete Evidence

1. Helper-level strict admission rejects hard-failure and non-admission
   outcomes before any strictness, plane, freshness, or measured-path evidence
   can make them acceptable. `Outcome::verdict()` maps `G`, `I`, `K`, `L`, `M`,
   `N-direct`, and `S` to `NoGo`, and `J` to `Invalid`
   (`skinny/crates/bbnf-bench/src/gate.rs:83`,
   `skinny/crates/bbnf-bench/src/gate.rs:89`). `validate_strict_admission()`
   immediately rejects any outcome whose verdict is not `Verdict::Go`
   (`skinny/crates/bbnf-bench/src/gate.rs:135`,
   `skinny/crates/bbnf-bench/src/gate.rs:139`) before checking row strictness,
   comparator strictness, parse UTF-8, output plane, measured path, or freshness
   (`skinny/crates/bbnf-bench/src/gate.rs:145`,
   `skinny/crates/bbnf-bench/src/gate.rs:172`). The focused unit test covers
   `D`, `E`, `F-positive`, `F-noise`, `G`, `I`, `J`, `K`, `L`, `M`,
   `N-direct`, and `S` as strict-admission rejects while `A` remains the valid
   strict fixture (`skinny/crates/bbnf-bench/src/gate.rs:459`,
   `skinny/crates/bbnf-bench/src/gate.rs:461`,
   `skinny/crates/bbnf-bench/src/gate.rs:478`).

2. W0 row validation freezes current rows as baseline telemetry, so a W0 row
   cannot be relabeled strict/measured while preserving identity. Each W0 row
   validates row identity, W0 outcome vocabulary, exact `SK-V8-open` run id,
   profile artifact, hot leaf, manifest semantics, comparator evidence, and the
   admission boundary (`skinny/crates/bbnf-bench/src/report.rs:328`,
   `skinny/crates/bbnf-bench/src/report.rs:336`,
   `skinny/crates/bbnf-bench/src/report.rs:349`,
   `skinny/crates/bbnf-bench/src/report.rs:355`,
   `skinny/crates/bbnf-bench/src/report.rs:370`,
   `skinny/crates/bbnf-bench/src/report.rs:371`). The admission-boundary check
   requires `strictness=deferred`, `measured_validation_path=view-boundary`,
   `parse_utf8=view-boundary`, and `escape_complete=yes`
   (`skinny/crates/bbnf-bench/src/report.rs:1121`,
   `skinny/crates/bbnf-bench/src/report.rs:1128`,
   `skinny/crates/bbnf-bench/src/report.rs:1134`,
   `skinny/crates/bbnf-bench/src/report.rs:1140`).

3. The exact V8 repro remains covered after the V9 telemetry fold. The report
   baseline test first accepts the complete `SK_V8_OPEN_BASELINE`
   (`skinny/crates/bbnf-bench/src/report.rs:1994`,
   `skinny/crates/bbnf-bench/src/report.rs:2041`), then mutates
   `json/canada/parse_only/main` to `strict`, `measured-row`, and `DOM` while
   preserving the rest of the report shape; validation must fail
   (`skinny/crates/bbnf-bench/src/report.rs:2075`,
   `skinny/crates/bbnf-bench/src/report.rs:2085`).

4. The V9 telemetry-consumption fold is additive to strictness safety. It adds
   exact pre-W1 CostFacts sentinel validation, `redress_entry=none`,
   `track2_independence_status=independent_verified`, structured build metadata,
   and workload-specific substrate tuples
   (`skinny/crates/bbnf-bench/src/report.rs:1007`,
   `skinny/crates/bbnf-bench/src/report.rs:1031`,
   `skinny/crates/bbnf-bench/src/report.rs:1091`). Its negatives mutate those
   fields while preserving baseline report identity and assert W0 validation
   fails (`skinny/crates/bbnf-bench/src/report.rs:2087`,
   `skinny/crates/bbnf-bench/src/report.rs:2120`). The V9 commit changed only
   `skinny/crates/bbnf-bench/src/report.rs`; `gate.rs` strict-admission logic
   from the V8 fold was not weakened.

5. The committed `RESULTS.md` surface matches the closed boundary. The main
   table shows canada parse as `L / NO-GO`, `Strictness=deferred`,
   `parse_utf8=view-boundary`, and borrowed-view output plane
   (`skinny/RESULTS.md:10`). The W0 telemetry manifest is gate-consumed and
   states native Rust comparators are same-run while C++ sidecars are historical
   or absent and never W0 strict anchors (`skinny/RESULTS.md:141`). A census of
   the committed main rows returned
   `main=38 deferred=38 strict=0 nonadmit=31 go=7`.

## Commands Run

- `git rev-parse HEAD` returned
  `3a9fa32675cedb1f8a0d31247df229fe505068be`.
- `git diff --name-only 00c3485a..3a9fa326` showed the V9 fold changed only
  `skinny/crates/bbnf-bench/src/report.rs` plus V9 hardening docs.
- `git diff --name-only 0bd16f6d..HEAD -- skinny/crates/runtime skinny/crates/codegen skinny/crates/bbnf-simd skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/generated_json_track2.rs skinny/grammars skinny/fixtures`
  returned no behavior-surface paths.
- `cargo test -p bbnf-bench strict -- --nocapture` passed: 5 focused tests.
- `cargo test -p bbnf-bench w0_report_accepts_exact_opening_baseline -- --nocapture`
  passed: 1 focused test.
- `cargo test -p bbnf-bench w0_ -- --nocapture` passed: 20 focused W0 tests.
- `cargo test -p bbnf-bench` passed: 52 library tests, 8 gate-bin tests, and
  doc-tests.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
  passed and retained `Overall outcome N-direct / NoGo`.

## Blockers

None.

## Required Fold

None for CH1. V10 may count as an accepting CH1 review for the post-V9 target;
W0 convergence still depends on the full six-lens V10 disposition and the
orchestrator's two-consecutive-cycle rule.

## Residual Risk

`validate_w0_outcome()` still accepts internal W0 enum spellings such as `I`,
`J`, and `M` before exact report-level baseline validation
(`skinny/crates/bbnf-bench/src/report.rs:955`,
`skinny/crates/bbnf-bench/src/report.rs:961`). This is not a CH1 blocker for
the current closure candidate because report-level baseline identity and the
strict-admission helper both fail closed for non-`GO` outcomes, but a later
cleanup could align the row-level allowlist with the SPEC's rendered enum.
