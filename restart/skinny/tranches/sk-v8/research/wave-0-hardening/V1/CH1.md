# CH1 W0 Hardening V1 Review

## Decision

REJECT.

## Acceptance Probability

58%.

## Blocking Findings

1. Strict-admission refusal is implemented but not wired into the W0 gate path.

   `skinny/crates/bbnf-bench/src/gate.rs:133-169` defines `validate_strict_admission`, and its tests cover unsupported outcome, K/S, deferred rows, view-boundary rows, plane mismatch, and stale sidecars at `skinny/crates/bbnf-bench/src/gate.rs:443-479`. However `rg` finds no production caller outside those tests. The actual `gate-json` path only calls `report.validate_schema_v3().and_then(|_| report.validate_sk_v8_w0())` at `skinny/crates/bbnf-bench/src/bin/gate.rs:281-284`; `TelemetryRow::validate_sk_v8_w0` validates required telemetry and parse-row K/S status at `skinny/crates/bbnf-bench/src/report.rs:275-367`, but never constructs `StrictAdmissionEvidence` or calls `validate_strict_admission`.

   Impact: the strict-vs-strict contract is still partly paper-enforced. Current `RESULTS.md` contains view-boundary/deferred GO rows, for example `skinny/RESULTS.md:7`, `skinny/RESULTS.md:9`, `skinny/RESULTS.md:18`, `skinny/RESULTS.md:21`, `skinny/RESULTS.md:27`, `skinny/RESULTS.md:28`, and `skinny/RESULTS.md:38`. If any consumer treats `Outcome=A`/`Verdict=GO` as admission, gate-json has not executably attached the Section 0.2 strict-admission refusal to those rows. Parse rows are demoted to `S`/NO-GO, but non-parse GO rows remain view-boundary/deferred without an executable "non-strict only" classification.

2. The strict-admission predicate omits `parse_utf8` and `escape_complete`, so even if it is wired later it can miss a required rejection condition.

   SPEC Section 0.2 says `parse_utf8=view-boundary` is guard telemetry only (`restart/skinny/tranches/sk-v8/SPEC.md:73-79`). `StrictAdmissionEvidence` carries outcome, row strictness, planes, freshness, and `measured_validation_path`, but not `parse_utf8` or `escape_complete` (`skinny/crates/bbnf-bench/src/gate.rs:58-68`). The validator checks `measured_validation_path != "measured-row"` at `skinny/crates/bbnf-bench/src/gate.rs:152-154`, but has no way to reject a row that claims `measured-row` while the rendered row still says `parse_utf8=view-boundary`.

   Impact: view-boundary strict admission can still pass the helper if the caller supplies `measured_validation_path=measured-row`.

3. W0 does not reject unsupported report outcomes in the report validator, and the parser accepts outcomes outside the SK-V8 W0 schema.

   SPEC Section 0.3 lists `A`, `C`, `G`, `K`, `L`, `N-direct`, and `S` (`restart/skinny/tranches/sk-v8/SPEC.md:81-99`). `parse_outcome_id` accepts additional legacy outcomes `B`, `D`, `E`, `F-positive`, `F-noise`, `I`, `J`, and `M` at `skinny/crates/bbnf-bench/src/gate.rs:112-130`. The rendered/report schema only checks that `Outcome` is non-empty at `skinny/crates/bbnf-bench/src/report.rs:219-272`; W0 row validation only adds a parse-row guard check at `skinny/crates/bbnf-bench/src/report.rs:360-365`.

   Impact: a non-parse row can carry an unsupported outcome string or legacy enum outcome and still pass W0 row validation if the rest of the baseline fields match. That fails the W0 requirement that `gate-json` reject unsupported outcomes.

4. Reserved `S` is over-broad in the parse-row demotion path.

   `w0_parse_non_admission` preserves only `I`, `J`, and `K`; every other parse classifier result becomes `S` (`skinny/crates/bbnf-bench/src/bin/gate.rs:317-321`). But `classify` can return `L` for SIMD throughput and `M` for memory residency before normal parse classification (`skinny/crates/bbnf-bench/src/gate.rs:211-218`). `S` is reserved for explicit substrate-guard / non-SOTA spelling, not for masking unrelated hard failures (`restart/skinny/tranches/sk-v8/SPEC.md:95-99`).

   Impact: a parse-row SIMD or memory failure could be rendered as `S` rather than its real failure class, weakening the hardening value of the reserved substrate-guard outcome.

## Nonblocking Findings

- The `S` outcome itself is correctly NO-GO: `Outcome::SSubstrateGuardNonAdmission` maps to `Verdict::NoGo` at `skinny/crates/bbnf-bench/src/gate.rs:81-87`, and it is more severe than `G` in `worst_outcome` ordering at `skinny/crates/bbnf-bench/src/gate.rs:297-313`.
- Parse rows are forced away from strict-looking `A`/`GO` in the normal W0 binary path by `w0_parse_non_admission` at `skinny/crates/bbnf-bench/src/bin/gate.rs:317-321`, and W0 validation rejects parse rows outside `K|S` at `skinny/crates/bbnf-bench/src/report.rs:360-365`.
- Sidecar telemetry is materially better than before: W0 populates historical or absent sidecar evidence at `skinny/crates/bbnf-bench/src/bin/gate.rs:491-512`, and the report validator rejects populated sidecars marked absent plus missing sidecar slots at `skinny/crates/bbnf-bench/src/report.rs:905-940`.
- The 38-row baseline lock and +/-1.0% movement check are explicit at `skinny/crates/bbnf-bench/src/report.rs:489-515` and `skinny/crates/bbnf-bench/src/report.rs:844-859`.

## Evidence Inspected

- Commit `6d8cb70138a73e87252aab5e0ea712390801a6a0` (`feat(sk-v8-wave0): enforce telemetry manifest gate`) and current HEAD tree.
- `restart/skinny/tranches/sk-v8/SPEC.md` Section 0.2, Section 0.3, and Section 3.
- `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md` W0 protocol and pre-blocked routes.
- `restart/skinny/tranches/sk-v8/research/wave-0-plan.md`.
- `skinny/crates/bbnf-bench/src/gate.rs`.
- `skinny/crates/bbnf-bench/src/report.rs`.
- `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- `skinny/RESULTS.md` current W0 output and manifest.
- `rg` proof that `validate_strict_admission` has no production caller outside its own tests.
- Focused tests run:
  - `cargo test -p bbnf-bench rejects_` passed.
  - `cargo test -p bbnf-bench w0_` passed.

## Exact Remediation If Rejected

1. Wire strict-admission validation into the W0 gate/report path. In `Report::validate_sk_v8_w0` or a helper it calls, build `StrictAdmissionEvidence` from the actual `TelemetryRow` plus each comparator used as admission evidence. If a row is not a strict admission, require an explicit consumed non-admission/guard classification instead of allowing `A`/`GO` with `Strictness=deferred` and `measured_validation_path=view-boundary`.
2. Extend `StrictAdmissionEvidence` with `parse_utf8` and `escape_complete`; reject strict admission when `parse_utf8=view-boundary`, `parse_utf8=post-parse`, `parse_utf8=none`, or `escape_complete != yes`.
3. Add a W0 outcome validator and call it from `TelemetryRow::validate_sk_v8_w0`. It should reject values outside the SPEC Section 0.3 set unless SPEC is explicitly amended, and tests must mutate non-parse rows to `Q`, `B`, `F-positive`, and `M` to prove rejection.
4. Narrow `w0_parse_non_admission`: preserve hard failures such as `I`, `J`, `K`, `L`, and `M`; only demote admission-capable parse outcomes into `S`. Add tests proving `L` and `M` are not rewritten to `S`.
5. Add integrated negative tests, not only isolated helper tests:
   - `w0_report_rejects_a_go_view_boundary_row_as_strict_admission`.
   - `w0_report_rejects_historical_sidecar_strict_admission_claim`.
   - `w0_report_rejects_plane_mismatch_admission`.
   - `w0_report_rejects_unsupported_non_parse_outcome`.
   - `w0_parse_non_admission_preserves_l_and_m`.
6. Re-run focused `bbnf-bench` tests and the W0 `gate-json` path, then confirm `skinny/RESULTS.md` remains generated/idempotent after the stricter gate.
