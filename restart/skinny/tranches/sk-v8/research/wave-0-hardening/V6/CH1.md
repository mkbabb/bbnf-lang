# CH1 W0 V6 Hardening Challenge

## Verdict

REJECT.

Confidence: 94%.

## Reviewed Surfaces

- Current target HEAD: `6c0bc15d44142abf0b965d9daee7070b1f32dd99`.
- CH1/iteration authority: `restart/prompts/ORCHESTRATOR.md:74`-`restart/prompts/ORCHESTRATOR.md:83` and `restart/prompts/ORCHESTRATOR.md:104`-`restart/prompts/ORCHESTRATOR.md:123`.
- W0 packet: `restart/skinny/tranches/sk-v8/SPEC.md:63`-`restart/skinny/tranches/sk-v8/SPEC.md:81`, `restart/skinny/tranches/sk-v8/SPEC.md:103`-`restart/skinny/tranches/sk-v8/SPEC.md:146`, and `restart/skinny/tranches/sk-v8/SPEC.md:288`-`restart/skinny/tranches/sk-v8/SPEC.md:347`.
- Dispatch/handoff: `restart/skinny/tranches/sk-v8/HANDOFF.md:29`-`restart/skinny/tranches/sk-v8/HANDOFF.md:48`, `restart/skinny/tranches/sk-v8/HANDOFF.md:139`-`restart/skinny/tranches/sk-v8/HANDOFF.md:146`, and `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`-`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:89`.
- Evidence ledgers: `skinny/RESULTS.md:3`-`skinny/RESULTS.md:6`, `skinny/RESULTS.md:44`-`skinny/RESULTS.md:50`, `skinny/RESULTS.md:141`, and `skinny/REDRESS.md:2107`-`skinny/REDRESS.md:2115`, `skinny/REDRESS.md:2152`-`skinny/REDRESS.md:2183`.
- W0 code: `skinny/crates/bbnf-bench/src/report.rs`, `skinny/crates/bbnf-bench/src/bin/gate.rs`, plus strict admission in `skinny/crates/bbnf-bench/src/gate.rs`.

## Commands And Evidence

- `git status --short && git rev-parse HEAD`: clean worktree before artifact write; HEAD `6c0bc15d44142abf0b965d9daee7070b1f32dd99`.
- `cargo test -p bbnf-bench w0_`: PASS. Covered 12 report W0 tests and 8 gate-bin W0 tests, including row identity, outcome/verdict movement, sidecar evidence, native comparator semantics, metadata coherence, SIMD metadata, and Criterion fingerprint filtering.
- `cargo test -p bbnf-bench strict -- --nocapture`: PASS. Strict admission rejects `K`/`S`, deferred/view-boundary strict claims, plane mismatch, stale sidecars, and sidecar-same-run without a structured manifest.
- `cargo test -p bbnf-bench report::tests::w0_report_accepts_exact_opening_baseline -- --nocapture`: PASS. This is also the run-id false-accept reproducer below because the accepted full-table test uses `run_id = "sk-v8-open:test"`.
- `cargo test -p bbnf-bench w0_criterion_fingerprint_excludes_derendered_probe_estimates -- --nocapture`: PASS. The V6 fingerprint test excludes `json_unvalidated_future` and `json_canada/sonic_rs_real_typed_struct` while a real W0 main estimate changes the hash.
- `cargo run -p bbnf-bench --bin gate --release -- --advisory >/dev/null`: local live gate did not produce closure evidence. It failed before RESULTS comparison with `twitter SIMD metadata invalid: SIMD metadata has unsupported capture policy`; local `target/criterion` metadata shows stale/default-cpu capture facts, not the checked-in W0 native capture.
- `awk` row counts over `skinny/RESULTS.md`: 38 main rows and 38 telemetry-manifest rows.

## What Holds

- V5's row identity blocker is folded. `SK_V8_OPEN_BASELINE` now stores `row_id`, `outcome_id`, `verdict`, Track 1, and Track 2 (`skinny/crates/bbnf-bench/src/report.rs:646`-`skinny/crates/bbnf-bench/src/report.rs:666`), and report-level validation rejects outcome movement, verdict movement, and Track 1/2 drift (`skinny/crates/bbnf-bench/src/report.rs:493`-`skinny/crates/bbnf-bench/src/report.rs:528`). The focused baseline test mutates throughput, `twitter/parse_only` outcome, and `twitter/direct_to_struct` outcome/verdict and expects rejection (`skinny/crates/bbnf-bench/src/report.rs:1897`-`skinny/crates/bbnf-bench/src/report.rs:1967`).
- Strict-vs-strict admission is measurable and fails closed in the code path reviewed. `validate_strict_admission` requires strict row/comparator, measured-row UTF-8, complete escapes, matching output plane, measured validation path, and same-run native freshness (`skinny/crates/bbnf-bench/src/gate.rs:135`-`skinny/crates/bbnf-bench/src/gate.rs:175`). W0 report validation routes strict claims through that gate (`skinny/crates/bbnf-bench/src/report.rs:1004`-`skinny/crates/bbnf-bench/src/report.rs:1062`).
- Sidecar same-run and stale/planning evidence are not accepted as strict anchors. Native comparator sources are workload/plane-specific (`skinny/crates/bbnf-bench/src/report.rs:1253`-`skinny/crates/bbnf-bench/src/report.rs:1315`), sidecar freshness must match and `sidecar-same-run` rejects without a structured manifest (`skinny/crates/bbnf-bench/src/report.rs:1203`-`skinny/crates/bbnf-bench/src/report.rs:1251`), and RESULTS labels C++ sidecars as historical or absent (`skinny/RESULTS.md:48`-`skinny/RESULTS.md:50`).
- The unvalidated Criterion-row fingerprint hole appears folded. The fingerprint includes only admitted W0 row membership (`skinny/crates/bbnf-bench/src/bin/gate.rs:717`-`skinny/crates/bbnf-bench/src/bin/gate.rs:759`), and the test excludes both an unvalidated corpus and a valid fixture's unadmitted real-typed estimate (`skinny/crates/bbnf-bench/src/bin/gate.rs:1769`-`skinny/crates/bbnf-bench/src/bin/gate.rs:1806`).

## Material Blocker

1. BLOCKER: `run_id` drift is not actually validated by the W0 validator.

   W0 requires run telemetry and same-wave gate consumption: required fields include `Run id` (`restart/skinny/tranches/sk-v8/SPEC.md:110`-`restart/skinny/tranches/sk-v8/SPEC.md:140`), every field must be consumed by `gate-json` (`restart/skinny/tranches/sk-v8/SPEC.md:142`-`restart/skinny/tranches/sk-v8/SPEC.md:146`), and W0's same-wave consumer is the gate (`restart/skinny/tranches/sk-v8/SPEC.md:336`-`restart/skinny/tranches/sk-v8/SPEC.md:337`). The rendered manifest claims `gate-json` consumes that manifest (`skinny/RESULTS.md:141`), and the live RESULTS rows carry a fingerprint-shaped run id such as `sk-v8-open:criterion-fnv64-9a37562ed3d0383a` (`skinny/RESULTS.md:48`-`skinny/RESULTS.md:50`).

   The code only requires `run_id` to be non-empty. `SkV8Telemetry` has a `run_id` field (`skinny/crates/bbnf-bench/src/report.rs:59`-`skinny/crates/bbnf-bench/src/report.rs:61`), and `validate_sk_v8_w0` includes it in the required-text list (`skinny/crates/bbnf-bench/src/report.rs:275`-`skinny/crates/bbnf-bench/src/report.rs:320`), but after that it validates grammar/domain, row identity, outcome, wave id, sample fields, profile/hot leaf, CostFacts placeholder, consumer class, parse non-admission, comparator evidence, and strict boundary without checking run-id format, equality across rows, or equality to the computed Criterion fingerprint (`skinny/crates/bbnf-bench/src/report.rs:322`-`skinny/crates/bbnf-bench/src/report.rs:370`, `skinny/crates/bbnf-bench/src/report.rs:493`-`skinny/crates/bbnf-bench/src/report.rs:531`).

   `gate.rs` does compute a meaningful run id from the exact Criterion fingerprint (`skinny/crates/bbnf-bench/src/bin/gate.rs:383`-`skinny/crates/bbnf-bench/src/bin/gate.rs:393`) and assigns it into telemetry (`skinny/crates/bbnf-bench/src/bin/gate.rs:474`-`skinny/crates/bbnf-bench/src/bin/gate.rs:491`), but the report validator does not bind to that value. The accepted full-baseline unit fixture sets `run_id` to the non-fingerprint string `sk-v8-open:test` (`skinny/crates/bbnf-bench/src/report.rs:1512`-`skinny/crates/bbnf-bench/src/report.rs:1533`) and still asserts the entire W0 report validates OK (`skinny/crates/bbnf-bench/src/report.rs:1897`-`skinny/crates/bbnf-bench/src/report.rs:1944`). The executed command `cargo test -p bbnf-bench report::tests::w0_report_accepts_exact_opening_baseline -- --nocapture` passed, so this is not a theoretical path.

   Minimal no-source-change repro from existing tests:

   ```sh
   cd skinny
   cargo test -p bbnf-bench report::tests::w0_report_accepts_exact_opening_baseline -- --nocapture
   ```

   Expected if run-id drift were validated: the full-table fixture with `sk-v8-open:test` should reject unless tests inject the actual `sk-v8-open:criterion-fnv64-...` value or a validator-owned expected run id. Actual: it passes.

   Required fold: make W0 validation reject any `run_id` that is not exactly the computed `RunFacts.run_id` for the current report, and reject mixed run ids across rows. Add negative tests for a single-row run-id mutation and a same-prefix/non-fingerprint value such as `sk-v8-open:test`.

## Residual Risks If This Is Overruled

- A bad regenerated report can carry correct row ids, outcomes, verdicts, throughput cells, and comparator evidence while silently changing the run identity, then pass `Report::validate_sk_v8_w0()` if the changed run id is non-empty.
- The current gate still depends on local Criterion artifacts. My live advisory gate hit stale/default-cpu target metadata before reaching RESULTS comparison, which reinforces that W0 closure needs a validator-bound run identity, not only a rendered run-id string.
- W1-W6 should remain blocked under the ORCHESTRATOR CH1/3Z standard until run-id drift has an executable rejection path and the next challenge cycle can cite it.
