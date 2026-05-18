# CH1 W0 V4 Hardening Challenge

## Verdict

REJECT.

Confidence: 97%.

## Scope

Correctness challenge of `077aadad fix(sk-v8-wave0): fold hardening V3 gate blockers`.
Lens: strict outcome/admission row identity, `parse_utf8` / `escape_complete`
invariants, strict-vs-strict comparator discipline, and no new directive/BIR/
substrate. This review edited only this artifact.

## Evidence

- ORCHESTRATOR CH1 requires file/line-grounded correctness claims and strictness
  plane discipline (`restart/prompts/ORCHESTRATOR.md:83`); the global
  non-negotiables require strict-vs-strict comparator gates and no new
  directive/BIR/substrate (`restart/prompts/ORCHESTRATOR.md:201`,
  `restart/prompts/ORCHESTRATOR.md:202`, `restart/prompts/ORCHESTRATOR.md:203`,
  `restart/prompts/ORCHESTRATOR.md:208`).
- The W0 packet requires strict admission to reject unless output plane,
  strictness, same-run native anchor identity, and measured-row UTF-8/control/
  escape validation all hold (`restart/skinny/tranches/sk-v8/SPEC.md:73`,
  `restart/skinny/tranches/sk-v8/SPEC.md:76`, `restart/skinny/tranches/sk-v8/SPEC.md:79`).
  It also requires every emitted telemetry field to be consumed by `gate-json`
  in the same wave (`restart/skinny/tranches/sk-v8/SPEC.md:142`,
  `restart/skinny/tranches/sk-v8/SPEC.md:146`, `restart/skinny/tranches/sk-v8/SPEC.md:336`,
  `restart/skinny/tranches/sk-v8/SPEC.md:337`).
- Current rendered rows do carry the intended W0 values: `SYNTHESIS.md` says
  every current main row records `Strictness=deferred`, `parse_utf8=view-boundary`,
  and `escape_complete=yes` (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:36`,
  `restart/skinny/tranches/sk-v8/SYNTHESIS.md:37`), and `RESULTS.md` renders
  those columns in the main table (`skinny/RESULTS.md:3`, `skinny/RESULTS.md:5`).
- Positive checks run during this review: `cargo test -p bbnf-bench` passed 51
  library tests plus 8 gate-bin tests; `CARGO_TARGET_DIR=/tmp/skv8-w0-target
  RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory
  --check-results` exited 0; `cargo xtask check-json`, `cargo xtask
  check-real-typed`, and `cargo xtask check-conformance` exited 0, with
  conformance reporting 21 valid fixtures accepted and 7 invalid fixtures
  rejected.
- Positive folds: row identity is now bound to exact baseline row ids and
  throughput deltas (`skinny/crates/bbnf-bench/src/report.rs:493`,
  `skinny/crates/bbnf-bench/src/report.rs:501`, `skinny/crates/bbnf-bench/src/report.rs:508`,
  `skinny/crates/bbnf-bench/src/report.rs:514`, `skinny/crates/bbnf-bench/src/report.rs:962`);
  unsupported comparator ids reject before strict admission
  (`skinny/crates/bbnf-bench/src/report.rs:973`, `skinny/crates/bbnf-bench/src/report.rs:1017`,
  `skinny/crates/bbnf-bench/src/report.rs:1051`); sidecar same-run claims still
  reject without a structured manifest (`skinny/crates/bbnf-bench/src/report.rs:1101`,
  `skinny/crates/bbnf-bench/src/report.rs:1125`).

## Findings

1. BLOCKER: `parse_utf8` and `escape_complete` are not semantically consumed for
   current non-strict W0 rows. `TelemetryRow::parse` and `TelemetryRow::workload`
   emit the intended W0 pair `view-boundary` / `yes`
   (`skinny/crates/bbnf-bench/src/report.rs:121`,
   `skinny/crates/bbnf-bench/src/report.rs:122`,
   `skinny/crates/bbnf-bench/src/report.rs:123`,
   `skinny/crates/bbnf-bench/src/report.rs:159`,
   `skinny/crates/bbnf-bench/src/report.rs:160`,
   `skinny/crates/bbnf-bench/src/report.rs:161`), but schema validation only
   checks those fields for non-empty text (`skinny/crates/bbnf-bench/src/report.rs:219`,
   `skinny/crates/bbnf-bench/src/report.rs:226`,
   `skinny/crates/bbnf-bench/src/report.rs:227`,
   `skinny/crates/bbnf-bench/src/report.rs:234`). The W0 validator then exits
   the admission boundary early for non-strict rows after checking only
   `measured_validation_path == view-boundary`
   (`skinny/crates/bbnf-bench/src/report.rs:920`,
   `skinny/crates/bbnf-bench/src/report.rs:921`,
   `skinny/crates/bbnf-bench/src/report.rs:923`,
   `skinny/crates/bbnf-bench/src/report.rs:930`). The stricter `parse_utf8` and
   `escape_complete` checks exist only inside strict-admission evidence
   (`skinny/crates/bbnf-bench/src/report.rs:938`,
   `skinny/crates/bbnf-bench/src/report.rs:941`,
   `skinny/crates/bbnf-bench/src/report.rs:942`,
   `skinny/crates/bbnf-bench/src/gate.rs:151`,
   `skinny/crates/bbnf-bench/src/gate.rs:154`). That means W0 can accept a
   deferred row with `parse_utf8=none` and `escape_complete=n/a`, even though the
   packet says every emitted telemetry field is gate-consumed.

   Minimal reproduction, run from the repo root; it mutates only an isolated
   temporary archive of `077aadad`. The test should fail if the W0 validator
   consumes these fields for deferred rows, but it passes:

   ```sh
   set -e
   tmp=$(mktemp -d /Users/mkbabb/Programming/skv8-w0-ch1.XXXXXX)
   git archive 077aadad | tar -x -C "$tmp"
   cd "$tmp/skinny"
   perl -0pi -e 's/"view-boundary",\n            "yes"/"none",\n            "n\/a"/g' crates/bbnf-bench/src/report.rs
   cargo test -p bbnf-bench report::tests::w0_report_accepts_exact_opening_baseline
   ```

2. No CH1 blocker found in strict comparator identity after the V4 fold. Native
   strict anchors are id-allowlisted and require source path, plane, strictness,
   same-run-native freshness, sidecar `n/a`, and finite Mbps
   (`skinny/crates/bbnf-bench/src/report.rs:841`,
   `skinny/crates/bbnf-bench/src/report.rs:1058`,
   `skinny/crates/bbnf-bench/src/report.rs:1151`,
   `skinny/crates/bbnf-bench/src/report.rs:1175`,
   `skinny/crates/bbnf-bench/src/report.rs:1181`,
   `skinny/crates/bbnf-bench/src/report.rs:1187`,
   `skinny/crates/bbnf-bench/src/report.rs:1193`,
   `skinny/crates/bbnf-bench/src/report.rs:1199`,
   `skinny/crates/bbnf-bench/src/report.rs:1205`). The negative test for an
   unknown same-run sidecar strict-admission shape exists and passed
   (`skinny/crates/bbnf-bench/src/report.rs:1717`).

3. No CH1 blocker found in outcome row identity. W0 report validation enforces
   exactly the `SK_V8_OPEN_BASELINE` row count, rejects duplicate/unknown/missing
   row ids, binds `row_id` back to rendered corpus/workload, and validates
   Track 1/Track 2 within +/-1.0% of the opening baseline
   (`skinny/crates/bbnf-bench/src/report.rs:493`,
   `skinny/crates/bbnf-bench/src/report.rs:501`,
   `skinny/crates/bbnf-bench/src/report.rs:508`,
   `skinny/crates/bbnf-bench/src/report.rs:511`,
   `skinny/crates/bbnf-bench/src/report.rs:514`,
   `skinny/crates/bbnf-bench/src/report.rs:850`,
   `skinny/crates/bbnf-bench/src/report.rs:962`).

4. No new directive/BIR/substrate blocker found in the V4 fold. The commit diff
   touches packet docs, `skinny/RESULTS.md`, and `skinny/crates/bbnf-bench`
   gate/report files only; it does not edit grammar, parser, IR, codegen,
   runtime, SIMD source, or generated parser roots. The Lock 14 tests passed
   inside `cargo test -p bbnf-bench`.

## Required Disposition If Rejected

Fold a W0 validator guard before `validate_w0_admission_boundary` returns OK for
non-strict rows:

- For W0 deferred rows, require `parse_utf8 == "view-boundary"` and
  `escape_complete == "yes"` unless a strict/measured-row path is being
  validated by `gate::validate_strict_admission`.
- Add a report-level negative test that mutates an otherwise valid
  `SK_V8_OPEN_BASELINE` row to `parse_utf8=none` and/or `escape_complete=n/a`
  while leaving `strictness=deferred` and `measured_validation_path=view-boundary`,
  then asserts `report.validate_sk_v8_w0().is_err()`.
- Rerun `cargo test -p bbnf-bench` and
  `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo
  xtask gate-json --advisory --check-results`.

## Residual Risks

- The rejected issue is gate-honesty, not a measured behavior regression: the
  current committed `RESULTS.md` rows still render `view-boundary` / `yes`.
- This CH1 rejection keeps W1-W6 blocked under ORCHESTRATOR §3Z until folded and
  re-challenged; the positive row identity, comparator-id, sidecar, run-id, SIMD
  metadata, and no-new-substrate evidence should be retained in the next fold.
