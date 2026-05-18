# SK-V8 W0 Hardening V7 CH1

## Verdict

ACCEPT.

Confidence: 96%.

CH1 correctness review of current HEAD `f452e8373ed717731dd5e720c1d947c086cc22c9`
after the V6 fold. I found the V6 `run_id` blocker folded into executable
validation, and I found no remaining material CH1 blocker for exact row identity,
outcome/verdict identity, throughput baseline, strict-vs-strict comparator
validation, row-manifest Criterion fingerprinting, sidecar freshness, or required
W0 telemetry consumption.

## Reviewed Surfaces

- CH1/iteration authority: `restart/prompts/ORCHESTRATOR.md:74`,
  `restart/prompts/ORCHESTRATOR.md:83`, `restart/prompts/ORCHESTRATOR.md:104`,
  `restart/prompts/ORCHESTRATOR.md:118`.
- W0 packet contract: `restart/skinny/tranches/sk-v8/SPEC.md:63`,
  `restart/skinny/tranches/sk-v8/SPEC.md:73`,
  `restart/skinny/tranches/sk-v8/SPEC.md:103`,
  `restart/skinny/tranches/sk-v8/SPEC.md:142`,
  `restart/skinny/tranches/sk-v8/SPEC.md:159`,
  `restart/skinny/tranches/sk-v8/SPEC.md:288`,
  `restart/skinny/tranches/sk-v8/SPEC.md:310`,
  `restart/skinny/tranches/sk-v8/SPEC.md:346`.
- V7 dispatch/governance fold: `restart/skinny/tranches/sk-v8/SPEC.md:322`,
  `restart/skinny/tranches/sk-v8/SPEC.md:339`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:127`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:148`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`.
- Prior V6 consolidation and CH1 blocker:
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:20`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:29`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/CH1.md:37`.
- Current report and W0 code:
  `skinny/RESULTS.md:5`, `skinny/RESULTS.md:42`,
  `skinny/RESULTS.md:44`, `skinny/RESULTS.md:48`,
  `skinny/RESULTS.md:85`, `skinny/RESULTS.md:141`;
  `skinny/crates/bbnf-bench/src/report.rs`;
  `skinny/crates/bbnf-bench/src/bin/gate.rs`;
  `skinny/crates/bbnf-bench/src/gate.rs`.

## Commands And Evidence

- `git status --short && git rev-parse HEAD`: clean worktree before this artifact;
  HEAD `f452e8373ed717731dd5e720c1d947c086cc22c9`.
- `cargo test -p bbnf-bench w0_ -- --nocapture`: PASS; 12 report W0 tests and 8
  gate-bin W0 tests passed.
- `cargo test -p bbnf-bench strict -- --nocapture`: PASS; strict admission
  rejection tests passed.
- `cargo test -p bbnf-bench report::tests::w0_report_accepts_exact_opening_baseline -- --nocapture`:
  PASS; the fixture now uses the exact W0 run id and mutates throughput,
  outcome/verdict, single-row run id, and uniform bad run id to rejection.
- `cargo test -p bbnf-bench w0_criterion_fingerprint_excludes_derendered_probe_estimates -- --nocapture`:
  PASS; volatile probes, unvalidated corpus rows, and valid-fixture unadmitted
  rows are excluded from the fingerprint while a true W0 input changes it.
- `cargo test -p bbnf-bench`: PASS; 52 library tests, 8 gate-bin tests, and doc
  tests passed.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`:
  PASS against committed `skinny/RESULTS.md`.
- `awk ... skinny/RESULTS.md`: `main=38 manifest=38`.
- Copied-root Criterion probe: copied `/tmp/skv8-w0-target/criterion`, injected
  `json_unvalidated_future/track1_generated/new/estimates.json` and
  `json_canada/sonic_rs_real_typed_struct/new/estimates.json`, then ran
  `gate-json --advisory --check-results`; PASS, and the rendered run id stayed
  `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`.
- Frozen behavior-surface diff:
  `git diff --name-only 0bd16f6d..HEAD -- skinny/crates/runtime skinny/crates/codegen skinny/crates/bbnf-simd ...`
  returned no paths for the checked parser/scanner/SIMD/codegen/product surfaces.
- V7 cost-accounting cross-check:
  `git diff --shortstat 0bd16f6d..6c0bc15d -- ...` returned
  `7 files changed, 3532 insertions(+), 253 deletions(-)`, matching the folded
  SPEC accounting.

## Findings

1. No blocker: exact row identity, outcome, verdict, and Track 1/Track 2 baseline
   are gate-consumed.

   `Report::validate_sk_v8_w0()` requires exactly the `SK_V8_OPEN_BASELINE` row
   count, rejects duplicate or unknown row ids, validates each row before report
   acceptance, rejects outcome movement, rejects verdict movement, checks Track 1
   and Track 2 against the opening baseline, and then checks every baseline row is
   present (`skinny/crates/bbnf-bench/src/report.rs:499`,
   `skinny/crates/bbnf-bench/src/report.rs:507`,
   `skinny/crates/bbnf-bench/src/report.rs:514`,
   `skinny/crates/bbnf-bench/src/report.rs:517`,
   `skinny/crates/bbnf-bench/src/report.rs:523`,
   `skinny/crates/bbnf-bench/src/report.rs:529`,
   `skinny/crates/bbnf-bench/src/report.rs:532`). The baseline table stores
   `row_id`, `outcome_id`, `verdict`, Track 1, and Track 2
   (`skinny/crates/bbnf-bench/src/report.rs:652`,
   `skinny/crates/bbnf-bench/src/report.rs:674`). The focused test mutates a
   Track 1 value, `twitter/parse_only` outcome, and `twitter/direct_to_struct`
   outcome/verdict and expects rejection
   (`skinny/crates/bbnf-bench/src/report.rs:1905`,
   `skinny/crates/bbnf-bench/src/report.rs:1953`,
   `skinny/crates/bbnf-bench/src/report.rs:1957`,
   `skinny/crates/bbnf-bench/src/report.rs:1966`).

2. No blocker: the V6 `run_id` false accept is folded.

   The report now has a fixed `SK_V8_OPEN_RUN_ID` of
   `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`
   (`skinny/crates/bbnf-bench/src/report.rs:660`) and row validation rejects any
   telemetry run id that differs from it
   (`skinny/crates/bbnf-bench/src/report.rs:336`). The V7 test adds both the
   single-row mutation and a uniform same-prefix non-fingerprint mutation
   (`skinny/crates/bbnf-bench/src/report.rs:1976`,
   `skinny/crates/bbnf-bench/src/report.rs:1980`). The producer computes the live
   run id from the Criterion fingerprint (`skinny/crates/bbnf-bench/src/bin/gate.rs:383`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:390`) and assigns it into every W0
   telemetry row (`skinny/crates/bbnf-bench/src/bin/gate.rs:489`). The committed
   manifest carries that exact value from first through last telemetry row
   (`skinny/RESULTS.md:48`, `skinny/RESULTS.md:85`).

3. No blocker: strict-vs-strict comparator validation is executable and fails
   closed.

   Strict admission evidence carries outcome, row/comparator strictness, UTF-8,
   escape, output-plane, freshness, sidecar freshness, and measured path
   (`skinny/crates/bbnf-bench/src/gate.rs:59`). `validate_strict_admission()`
   rejects reserved/non-admitting outcomes `K` and `S`, non-strict rows,
   non-strict comparators, non-measured UTF-8, incomplete escapes, output-plane
   mismatch, non-measured validation path, historical/stale/absent freshness, and
   unsupported freshness (`skinny/crates/bbnf-bench/src/gate.rs:135`,
   `skinny/crates/bbnf-bench/src/gate.rs:145`,
   `skinny/crates/bbnf-bench/src/gate.rs:151`,
   `skinny/crates/bbnf-bench/src/gate.rs:157`,
   `skinny/crates/bbnf-bench/src/gate.rs:160`,
   `skinny/crates/bbnf-bench/src/gate.rs:163`,
   `skinny/crates/bbnf-bench/src/gate.rs:172`). W0 row validation routes any
   strict claim through this gate (`skinny/crates/bbnf-bench/src/report.rs:1012`,
   `skinny/crates/bbnf-bench/src/report.rs:1043`,
   `skinny/crates/bbnf-bench/src/report.rs:1060`).

4. No blocker: sidecar freshness and source evidence are measurable and
   non-admitting.

   The producer emits native strict comparators as `same-run-native` and C++
   sidecar values as either `historical:sk-v7-sidecar-profile` or
   `absent:not-collected-for-{workload}` (`skinny/crates/bbnf-bench/src/bin/gate.rs:526`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:565`). The validator requires native
   comparator plane/source/freshness/sidecar semantics and exact Criterion source
   shape (`skinny/crates/bbnf-bench/src/report.rs:1261`,
   `skinny/crates/bbnf-bench/src/report.rs:1291`,
   `skinny/crates/bbnf-bench/src/report.rs:1297`,
   `skinny/crates/bbnf-bench/src/report.rs:1303`,
   `skinny/crates/bbnf-bench/src/report.rs:1315`). Sidecars must be DOM, strict,
   have matching comparator and sidecar freshness, reject `sidecar-same-run`, and
   use exact historical/absence source artifacts
   (`skinny/crates/bbnf-bench/src/report.rs:1211`,
   `skinny/crates/bbnf-bench/src/report.rs:1223`,
   `skinny/crates/bbnf-bench/src/report.rs:1229`,
   `skinny/crates/bbnf-bench/src/report.rs:1235`,
   `skinny/crates/bbnf-bench/src/report.rs:1241`). The committed report states
   sidecars are historical or absent and never strict anchors in W0
   (`skinny/RESULTS.md:141`).

5. No blocker: row-manifest Criterion fingerprinting is scoped to admitted W0
   inputs.

   Fingerprinting walks only `estimates.json` and `metadata.toml` files that pass
   `is_w0_criterion_input()` (`skinny/crates/bbnf-bench/src/bin/gate.rs:673`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:707`). That predicate accepts only
   loaded fixtures whose derived workload has an exact `sk_v8_open_baseline` row
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:717`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:733`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:736`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:745`). The unit test excludes
   volatile probe estimates, an unvalidated future corpus, and a valid fixture's
   unadmitted real-typed row, then proves a true W0 estimate changes the
   fingerprint (`skinny/crates/bbnf-bench/src/bin/gate.rs:1770`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1781`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1788`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1794`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1800`).

6. No blocker: required telemetry fields are consumed before write/compare.

   W0 row validation checks the required SK-V8 text fields, grammar/domain,
   row identity, outcome support, baseline marker, exact run id, sample count,
   sample cost, profile artifact, hot leaf binding, CostFacts alternatives,
   same-wave consumer class, parse non-admission, comparator evidence, and
   admission boundary (`skinny/crates/bbnf-bench/src/report.rs:275`,
   `skinny/crates/bbnf-bench/src/report.rs:322`,
   `skinny/crates/bbnf-bench/src/report.rs:328`,
   `skinny/crates/bbnf-bench/src/report.rs:330`,
   `skinny/crates/bbnf-bench/src/report.rs:336`,
   `skinny/crates/bbnf-bench/src/report.rs:342`,
   `skinny/crates/bbnf-bench/src/report.rs:349`,
   `skinny/crates/bbnf-bench/src/report.rs:355`,
   `skinny/crates/bbnf-bench/src/report.rs:361`,
   `skinny/crates/bbnf-bench/src/report.rs:367`,
   `skinny/crates/bbnf-bench/src/report.rs:375`). The executable gate validates
   schema and W0 telemetry before writing or comparing `RESULTS.md`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:329`). The committed report has 38
   main rows and 38 telemetry manifest rows (`skinny/RESULTS.md:5`,
   `skinny/RESULTS.md:42`, `skinny/RESULTS.md:48`, `skinny/RESULTS.md:85`).

## Material Blockers

None found.

## Residual Risks

- This ACCEPT is only the first possible qualifying cycle after V6 REJECT. W0
  still needs the orchestrator's required consecutive accepting challenge cycle
  before closure or W1-W6 dispatch (`restart/prompts/ORCHESTRATOR.md:118`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:14`).
- `SK_V8_OPEN_RUN_ID` is intentionally exact and hard-coded to the current
  `SK-V8-open` capture. Any legitimate recapture requires a new accepted fold,
  not an in-place report refresh (`skinny/crates/bbnf-bench/src/report.rs:660`).
- Row-level `validate_w0_outcome()` still admits internal hard-failure ids `I`,
  `J`, and `M` before report-level exact-baseline validation
  (`skinny/crates/bbnf-bench/src/report.rs:960`), while SPEC Section 0.3 lists
  the current rendered outcome vocabulary (`restart/skinny/tranches/sk-v8/SPEC.md:83`).
  Exact report validation prevents this from moving current W0 rows, but a later
  cleanup should align the local row allowlist with the rendered vocabulary.
