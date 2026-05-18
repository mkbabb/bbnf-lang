# CH6 W0 V2 Hardening Challenge

Date: 2026-05-18.

Lens: CH6 - anti-paper-close. Focus: end-to-end gate commands, malformed
sidecar/parser tests, RESULTS reproducibility, commit evidence, and whether W0
can close without hidden deferrals.

Commit reviewed: `cb0fdba0dd05042adccb6554fa32c8e704cb6da5`.

## Verdict

ACCEPT.

Confidence: 96%.

W0 V2 is no longer a paper close for the CH6 lens. The fold wires the W0
telemetry checks into the live gate path, makes report mutation explicit,
preserves hard parse outcomes in the rendered baseline, and supplies executable
negative tests for the V1 anti-paper-close blockers. This is the first
qualifying ACCEPT cycle for W0; it does not by itself satisfy the two-cycle
convergence rule in `restart/prompts/ORCHESTRATOR.md` Section 3Z.

## Evidence Reviewed

| Check | Result |
|---|---|
| `cargo test -p bbnf-bench` | PASS: 45 library tests, 2 gate-bin tests. |
| `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results >/dev/null` | PASS, check-only gate exited 0. |
| `git status --short` after check-only gate | Clean except this CH6 file after authoring. |
| `git diff --exit-code -- skinny/RESULTS.md` after check-only gate | PASS: no report rewrite from validation. |
| Frozen Lock 14 path diff command over grammar/runtime/tape/SIMD/codegen/IR/passes/bench frozen roots | PASS: no diff. |

## Findings

1. Live gate mutation is now controlled rather than implicit. The bench gate
   parses `--update-results` and `--write-results` as the only write controls,
   treats `--include-volatile-probes` as opt-in, and otherwise compares the
   rendered report to the committed `RESULTS.md` before exiting
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:20`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:294`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:304`). The xtask wrapper accepts
   `--check-results` while passing through only the bounded W0 flag set
   (`skinny/xtask/src/main.rs:240`). This closes the V1 failure where
   validation could rewrite the report by default.

2. Strict admission is now gate-consumed, not merely helper-tested.
   `validate_sk_v8_w0` invokes outcome validation, profile/hot-leaf validation,
   admission-boundary validation, and comparator-evidence validation in the row
   validator (`skinny/crates/bbnf-bench/src/report.rs:275`,
   `skinny/crates/bbnf-bench/src/report.rs:328`,
   `skinny/crates/bbnf-bench/src/report.rs:342`,
   `skinny/crates/bbnf-bench/src/report.rs:368`). The strict-admission evidence
   now includes `parse_utf8` and `escape_complete`, and rejects view-boundary
   UTF-8, incomplete escapes, stale/historical sidecars, plane mismatch, and
   non-strict row/comparator claims (`skinny/crates/bbnf-bench/src/gate.rs:59`,
   `skinny/crates/bbnf-bench/src/gate.rs:135`).

3. The malformed sidecar test is sufficient for the current W0 architecture.
   W0 does not parse `RESULTS.md` as an input manifest; it generates a `Report`,
   validates it, and then check-only compares the rendered markdown to the
   committed report. The negative test mutates the same `SkV8ComparatorEvidence`
   structure consumed by the live gate and verifies rejection
   (`skinny/crates/bbnf-bench/src/report.rs:1411`). Whole-file check-only
   comparison then covers committed markdown tampering
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:304`). No separate markdown parser
   is required to close W0.

4. RESULTS reproducibility is acceptable for W0. The committed report contains
   the W0 manifest with stable criterion-content run ids, not HEAD/path-derived
   identifiers (`skinny/RESULTS.md:44`, `skinny/RESULTS.md:48`). The run id is
   computed from sorted Criterion `estimates.json` and `metadata.toml` inputs
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:648`), and the check-only run
   produced no `RESULTS.md` diff.

5. Hard parse failures are preserved in the report instead of being papered over
   as generic substrate guard rows. The parse demotion function keeps I/J/K/L/M
   intact (`skinny/crates/bbnf-bench/src/bin/gate.rs:339`), and W0 validation
   permits only hard-failure or reserved non-admission outcomes for parse rows
   (`skinny/crates/bbnf-bench/src/report.rs:360`). The current report exposes
   `canada/parse_only` as `L / NO-GO` rather than suppressing it
   (`skinny/RESULTS.md:10`).

6. Comparator provenance is workload-specific and gate-checked. Native strict
   comparators are emitted from parse/direct/typed Criterion paths according to
   the row workload (`skinny/crates/bbnf-bench/src/bin/gate.rs:476`), and the
   validator rejects native source-artifact mismatches
   (`skinny/crates/bbnf-bench/src/report.rs:1046`). Sidecar slots remain
   historical or absent and cannot satisfy strict admission
   (`skinny/crates/bbnf-bench/src/report.rs:1005`,
   `skinny/RESULTS.md:141`).

7. Lock 14 W0 freeze evidence is live enough for this wave. The gate validates
   the allowlist, frozen git status/diff against the named roots, and the
   `BackendShape` surface before report generation
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:35`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:336`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:394`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:457`). Negative tests cover
   dirty frozen-root status and backend-shape drift
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:557`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:563`).

8. Commit evidence is present and aligned with the fold. The `cb0fdba0` commit
   body names the V1 rejection causes, states the landed gate/freeze/report
   changes, records the check commands, and explicitly keeps W1-W6 blocked
   until V2/V3 hardening closes W0.

## Missed Tests Or Evidence

No blocker remains under CH6. Two non-blocking notes should be carried into the
consolidation:

- The W0 SPEC still says parse rows are `K`, or `S` if W0 amends the schema
  (`restart/skinny/tranches/sk-v8/SPEC.md:366`), while the accepted V1 fold
  intentionally preserves `L`/`M` hard failures and the current report has one
  `L`. The W0 close/handoff note should name this as the V1 hard-failure
  preservation amendment rather than letting later waves infer that all parse
  rows are `K`/`S`.
- The malformed-sidecar coverage is structural rather than markdown-parser
  based. That is acceptable because the committed markdown is not the source of
  truth for gate input, but a future manifest-ingest design would need parser
  negative tests.

## Mandatory Fold Items

None for CH6.

## Closure Discipline

W0 cannot close from this CH6 file alone. Per ORCHESTRATOR Section 3Z, the
aggregator still needs the full V2 cohort and then a second consecutive
qualifying challenge cycle at at least 95% ACCEPT, unless the user explicitly
pins W0 final at the corresponding sign-off gate.
