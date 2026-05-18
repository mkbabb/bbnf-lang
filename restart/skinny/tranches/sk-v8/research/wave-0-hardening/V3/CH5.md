# CH5 W0 V3 Hardening Challenge

Verdict: REJECT

Confidence: 91%

Reviewed commit: `61d5d30407d96ed176cc59e410f7884e30ed30ba`
(`fix(sk-v8-wave0): fold hardening V2 gate blockers`).

Scope: adversarial review of W0 after the V2 fold, with CH5 focus on SK-V8 row
schema, profile/hot leaf, run-id stability, rendered SPEC/RESULTS consistency,
W1-W6 blocking, and whether W0 can support later wave dispatch without semantic
debt.

## Findings

1. Blocking: `SPEC.md` is no longer consistent with the rendered
   `skinny/RESULTS.md` row state that W1-W6 would consume.

   `SPEC.md` says the current table has 17 `parse_only` `K / NO-GO` rows,
   6 `direct_to_struct` `A / GO` rows, 11 `N-direct / NO-GO` rows, and 4
   `real_typed_struct` `A / GO` rows (`restart/skinny/tranches/sk-v8/SPEC.md:148`).
   The actual rendered report has 16 `parse_only` `S / NO-GO` rows, 1
   `parse_only` `L / NO-GO` row, 3 direct `A / GO` rows, 14 direct
   `N-direct / NO-GO` rows, and 4 real-typed `A / GO` rows
   (`skinny/RESULTS.md:5`, `skinny/RESULTS.md:10`, `skinny/RESULTS.md:13`,
   `skinny/RESULTS.md:20`, `skinny/RESULTS.md:32`, `skinny/RESULTS.md:38`).

   Impact: later-wave dispatch would inherit contradictory row facts. For
   example, `SPEC.md` still lists `apache_builds/direct_to_struct`,
   `mesh/direct_to_struct`, and `numbers/direct_to_struct` as existing direct GO
   guard-floor rows (`restart/skinny/tranches/sk-v8/SPEC.md:168`), while
   `RESULTS.md` renders all three as `N-direct / NO-GO` (`skinny/RESULTS.md:13`,
   `skinny/RESULTS.md:20`, `skinny/RESULTS.md:32`). This is semantic debt for W4
   triage and W6 reconciliation, and it violates the close-condition expectation
   that `RESULTS.md`, `REDRESS.md`, and `HANDOFF.md`/SPEC agree before closure
   (`restart/skinny/tranches/sk-v8/SPEC.md:60`).

2. Blocking: the rendered W0 `run_id` is stable for today's checkout but not
   stable as a row-set identifier for later waves.

   `RunFacts::probe` renders every row with
   `sk-v8-open:criterion-fnv64-{criterion_fingerprint(criterion_root)}`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:379`). The fingerprint recursively
   includes every `estimates.json` and `metadata.toml` under the entire Criterion
   root (`skinny/crates/bbnf-bench/src/bin/gate.rs:662`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:678`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:691`), while the row validator only
   consumes the W0 fixture/workload rows selected by `read_metadata_rows`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:967`). Current RESULTS therefore
   stamps all rows with one root-wide id such as
   `sk-v8-open:criterion-fnv64-2dcb1beddbcc83fd` (`skinny/RESULTS.md:48`).

   Impact: any later W1-W6 benchmark artifact or unrelated stale Criterion file
   in the same target tree can change all W0 manifest run ids without changing a
   single W0 row measurement. That forces either spurious RESULTS churn or
   `--check-results` failure, and it makes `run_id` identify the mutable local
   Criterion directory rather than the validated 38-row SK-V8-open capture.

3. Blocking: W0 still does not satisfy the SPEC requirement for malformed
   sidecar-manifest rejection.

   The W0 task and exit gate require sidecar freshness validation and rejection
   of one intentionally malformed sidecar manifest
   (`restart/skinny/tranches/sk-v8/SPEC.md:357`,
   `restart/skinny/tranches/sk-v8/SPEC.md:369`,
   `restart/skinny/tranches/sk-v8/SPEC.md:370`). The implementation hard-codes
   sidecar values in `sidecar_comparators` (`skinny/crates/bbnf-bench/src/bin/gate.rs:824`)
   and renders populated sidecars as `historical:sk-v7-sidecar-profile` string
   evidence (`skinny/crates/bbnf-bench/src/bin/gate.rs:554`). The validator
   checks source-string shape and rejects `sidecar-same-run` claims without a
   structured manifest (`skinny/crates/bbnf-bench/src/report.rs:1083`,
   `skinny/crates/bbnf-bench/src/report.rs:1089`), but there is no manifest
   parser or malformed-manifest input path for `gate-json` to reject.

   Impact: populated sidecar cells are no longer usable as strict anchors, which
   is good, but W0 still cannot truthfully claim the SPEC's malformed sidecar
   manifest gate. Later waves would have to reinterpret these historical strings
   or add a real manifest contract after W0, exactly the kind of schema debt W0 is
   supposed to burn down.

## Accepted V3 Fold Items

- The V2 CH5 row-identity blocker is folded. `TelemetryRow::validate_sk_v8_w0`
  now calls `validate_w0_row_identity` (`skinny/crates/bbnf-bench/src/report.rs:328`),
  which parses `sk_v8.row_id` and requires it to match rendered corpus/workload
  (`skinny/crates/bbnf-bench/src/report.rs:962`).
- The native comparator plane/source blocker is folded. W0 now requires
  workload-specific Criterion sources, expected comparator planes, strict native
  comparator metadata, `same-run-native` freshness, `sidecar_freshness=n/a`, and
  finite Mbps (`skinny/crates/bbnf-bench/src/report.rs:1109`).
- Profile/hot-leaf placeholders are rejected by exact row-derived
  `criterion-slope-profile` paths and hot-leaf equality
  (`skinny/crates/bbnf-bench/src/report.rs:879`,
  `skinny/crates/bbnf-bench/src/report.rs:892`).
- W1-W6 are still blocked at the document-authority level: W1-W6 remain blocked
  until W0 closes and later wave plans/challenges/dispatch exist
  (`restart/skinny/tranches/sk-v8/SPEC.md:36`,
  `restart/skinny/tranches/sk-v8/SPEC.md:385`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:225`).

## Evidence Run

- `cargo test -p bbnf-bench`: passed 49 library tests, 6 `gate` bin tests, and
  doc tests.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`: passed and matched the committed `skinny/RESULTS.md`.
- `git diff --check`: passed.
- Worktree was clean before this report; only this CH5 report path is owned.

## Mandatory Fold Items

1. Reconcile `SPEC.md`/handoff row facts with the rendered W0 RESULTS state, or
   explicitly move stale pre-W0 floors into an archive section and make W1-W6
   dispatch consume only `SK-V8-open` rows/floors derived from current RESULTS.
2. Make W0 `run_id` a stable identifier for the validated 38-row capture, not a
   fingerprint of every Criterion file under the mutable target root.
3. Either add a structured sidecar manifest parser plus a malformed-manifest
   negative gate path, or mark populated sidecar values absent/non-manifest and
   remove the SPEC claim that W0 rejects malformed sidecar manifests.
