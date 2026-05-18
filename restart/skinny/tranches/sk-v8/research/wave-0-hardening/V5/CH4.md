# SK-V8 W0 Hardening V5 CH4 Review

## Verdict

REJECT.

Confidence: 96%.

## Scope

Reviewed target: `0c49fabd6d6facd136e1e69b8482aa4f239561ae`
(`fix(sk-v8-wave0): fold hardening V4 gate blockers`).

Lens: CH4 cost/reproducibility pressure on W0 telemetry: `run_id`
validated-input scope, fixture manifest binding, volatile probe exclusion, W0
Criterion fingerprint, SIMD metadata coherence, SIMD parity source, and
`--update-results` / `--check-results` semantics. This review edits only this
CH4 artifact.

## Evidence

- ORCHESTRATOR Section 3W assigns CH4 cost/wave-alignment pressure
  (`restart/prompts/ORCHESTRATOR.md:86`), and Section 3Z requires two
  consecutive >=95% ACCEPT challenge cycles with no open critical defects before
  advancing (`restart/prompts/ORCHESTRATOR.md:118`,
  `restart/prompts/ORCHESTRATOR.md:120`, `restart/prompts/ORCHESTRATOR.md:123`).
- V4 required `criterion_fingerprint` to be scoped to the validated W0
  fixture/row manifest, with `json_unvalidated_future/track1_generated/new/estimates.json`
  either ignored or rejected before any update path
  (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V4/HARDENING-W0-V4-CONSOLIDATED.md:38`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V4/HARDENING-W0-V4-CONSOLIDATED.md:40`).
- W0 packet constraints remain telemetry-only and same-wave gate-consumed:
  required fields must be consumed by `gate-json`
  (`restart/skinny/tranches/sk-v8/SPEC.md:103`,
  `restart/skinny/tranches/sk-v8/SPEC.md:142`), W0 has 38 current rows
  (`restart/skinny/tranches/sk-v8/SPEC.md:159`), W0 may not change parser,
  scanner, SIMD, asm, codegen behavior, product-plane behavior, or generated
  parser output (`restart/skinny/tranches/sk-v8/SPEC.md:333`), and W1-W6 remain
  blocked until W0 is admitted (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:92`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:96`).
- `cargo test -p bbnf-bench` passed: 52 lib tests, 8 gate-bin tests, 0
  failures. The focused `cargo test -p bbnf-bench w0_ -- --nocapture` also
  passed the W0 subset: 12 report tests and 8 gate-bin tests.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
  exited 0 against the committed W0 output.
- Focused V4 regression check: after copying `/tmp/skv8-w0-target/criterion` to
  `/tmp/skv8-ch4-v5.9Z5Mhd/criterion` and adding only
  `json_unvalidated_future/track1_generated/new/estimates.json`,
  `CARGO_TARGET_DIR=/tmp/skv8-ch4-v5.9Z5Mhd RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
  exited 0 and preserved the committed run id
  `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`.
- Focused row-manifest negative: after copying the same Criterion root to
  `/tmp/skv8-ch4-v5-row2.XqO98E/criterion` and adding only
  `json_canada/sonic_rs_real_typed_struct/new/estimates.json`,
  `CARGO_TARGET_DIR=/tmp/skv8-ch4-v5-row2.XqO98E RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
  exited 1 with `RESULTS.md is stale; rerun cargo xtask gate-json --update-results --advisory`.
  No W0 row was validated for `json/canada/real_typed_struct/main`; the only
  changed input was an unvalidated comparator estimate for a valid fixture.

## Findings

1. BLOCKER: `run_id` is fixture-name-bound, but still not bound to the exact W0
   fixture/row manifest.

   The V5 fold passes fixture names into `RunFacts::probe`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:45`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:50`) and into
   `criterion_fingerprint` (`skinny/crates/bbnf-bench/src/bin/gate.rs:384`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:392`). That fixes the exact V4
   `json_unvalidated_future` corpus case, because
   `is_w0_criterion_input` now requires `group.strip_prefix("json_")` to be in
   the fixture set (`skinny/crates/bbnf-bench/src/bin/gate.rs:735`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:747`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:750`).

   The remaining gap is per-row scope. `W0_CRITERION_BENCHES` is a global bench
   allowlist (`skinny/crates/bbnf-bench/src/bin/gate.rs:717`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:733`), so every valid fixture
   admits every listed W0 bench name into the fingerprint
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:747`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:751`). Current W0 has only four
   `real_typed_struct` rows, but the fingerprint still accepts
   `sonic_rs_real_typed_struct` under non-real-typed fixtures such as `canada`.
   The report generator only emits a real-typed row when
   `track1_real_typed_struct` exists (`skinny/crates/bbnf-bench/src/bin/gate.rs:206`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:260`), so a comparator-only future
   estimate changes the fingerprint without adding a row that
   `Report::validate_sk_v8_w0()` can reject (`skinny/crates/bbnf-bench/src/report.rs:493`,
   `skinny/crates/bbnf-bench/src/report.rs:519`).

   The focused `json_canada/sonic_rs_real_typed_struct/new/estimates.json`
   mutation therefore reproduces the V4 class in a narrower form: the unknown
   corpus case is fixed, but a valid-fixture/unvalidated-row Criterion file still
   makes `--check-results` stale. Because the gate validates before the stale
   comparison and before writing (`skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:331`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:332`), this stale-only failure
   implies that `--update-results` would be allowed to rewrite the committed
   `run_id` from an unvalidated comparator-only input.

2. Accepted: the exact `json_unvalidated_future` V4 fingerprint hole is fixed.

   The unit test
   `w0_criterion_fingerprint_excludes_derendered_probe_estimates` covers both
   `json_probes_twitter` and `json_unvalidated_future`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:1765`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1788`). The dynamic temp-root check
   above exited 0 and kept the committed `run_id`.

3. Accepted with the row-manifest caveat: volatile probe exclusion is still
   materially correct for the de-rendered probe class.

   The write path rejects `--include-volatile-probes` combined with
   `--update-results` or `--write-results`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:21`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:33`), probe rows are only added
   when explicitly requested (`skinny/crates/bbnf-bench/src/bin/gate.rs:261`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:269`), and the fingerprint test
   proves probe estimates do not perturb the hash.

4. Accepted: SIMD metadata coherence and SIMD parity source remain fail-closed.

   The gate recomputes scalar and SIMD structural hashes from the same fixture
   bytes, reads the SIMD metadata row, and validates it before report validation
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:65`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:79`). SIMD metadata validation
   checks fixture hash/bytes, bench semantics, same capture, capture policy, and
   scalar parity hash (`skinny/crates/bbnf-bench/src/bin/gate.rs:1381`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1439`). The benchmark writes SIMD
   metadata only after scalar/SIMD parity equality
   (`skinny/crates/bbnf-bench/benches/simd_scan.rs:16`,
   `skinny/crates/bbnf-bench/benches/simd_scan.rs:26`).

5. Accepted: deferred-row validation semantics are folded and executable.

   Non-strict W0 rows must keep `strictness=deferred`,
   `measured_validation_path=view-boundary`, `parse_utf8=view-boundary`, and
   `escape_complete=yes` (`skinny/crates/bbnf-bench/src/report.rs:920`,
   `skinny/crates/bbnf-bench/src/report.rs:948`). The focused unit
   `w0_rejects_deferred_validation_semantic_drift` mutates `parse_utf8=none` and
   `escape_complete=n/a` and expects rejection
   (`skinny/crates/bbnf-bench/src/report.rs:1640`,
   `skinny/crates/bbnf-bench/src/report.rs:1662`).

## Required Disposition If Rejected

Fold the W0 fingerprint one more time. `criterion_fingerprint` must be driven by
the validated W0 row manifest, not by `fixture_names x W0_CRITERION_BENCHES`.
At minimum:

- Build the accepted Criterion input set from the same W0 row manifest that
  `Report::validate_sk_v8_w0()` enforces: parse/direct rows for all current
  fixtures, real-typed rows only for the four current real-typed fixtures, all
  required metadata specs for those rows, SIMD metadata for current fixtures,
  and the admitted Canada SIMD estimate.
- Ignore or fail explicitly on valid-fixture/unvalidated-row files such as
  `json_canada/sonic_rs_real_typed_struct/new/estimates.json` before
  `--update-results` can write a new `run_id`.
- Keep the existing `json_unvalidated_future` and `json_probes_*` tests, and add
  a focused negative for a valid fixture with an unvalidated W0 bench name.

W0 remains blocked. This V5 cycle cannot count as a qualifying ACCEPT cycle
under ORCHESTRATOR Section 3Z until the row-manifest fingerprint gap is folded
and re-challenged.

## Residual Risks

- The current `run_id` is still a single capture-level hash. That can be
  acceptable for W0 after exact row-manifest binding, but later waves adding row
  families should consider row-local or family-local capture ids to reduce
  unrelated churn.
- SIMD parity is strict for W0 structural-scan metadata, but only Canada SIMD
  throughput is rendered as the W0 throughput note. That is packet-consistent
  for W0; later SIMD behavior waves still need row-specific scalar/checkasm
  gates before wiring.
- I did not run a fresh Criterion benchmark capture. This review used the
  committed `/tmp/skv8-w0-target` capture, package tests, gate replay, and
  focused temp-root Criterion mutations.
