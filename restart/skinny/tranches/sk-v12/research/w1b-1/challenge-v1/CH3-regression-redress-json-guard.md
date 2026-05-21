# SK-V12 W1b-1 CH3 - Regression / REDRESS / JSON Guard

Date: 2026-05-20.
Scope: CH3 adversarial review of the W1b-1 CSS L4 scaffold plan for regression
protection, JSON guard floors, REDRESS disposition, rollback, and RESULTS
overclaim.
Output: this file.

## Verdict

ACCEPT WITH HARD REDRESS PRECONDITIONS.

The plan has adequate CH3 regression protection for dispatch into redress: it
keeps W1b-1 as a scaffold/equality wave, separates W1b-2 as the lightningcss
admission wave, requires a full JSON guard rerun, forbids main `RESULTS.md`
movement and `lightningcss_mbps` placeholders, and names a rollback patch plus
measured REDRESS disposition.

The acceptance is conditional on making the executable gate as strong as the
plan. Current `SkV12NonJsonRow` validation still lacks the W1b-1 provenance /
size / Lock-status fields and still maps `direct_to_struct` to `direct_sink`;
that current validator cannot legally PASS the selected
`css_l4_declaration_value_fact_stream` row. This is not a plan-time CH3 blocker
because SPEC Section 6 owns `report.rs` / gate edits and the plan explicitly
requires the extension. It becomes a redress blocker if left unfixed.

## Evidence

- SPEC Section 0 makes JSON direct and typed guard floors binding: guard floors
  must hold or a miss must be recorded as a measured REDRESS demotion
  (`SPEC.md:61-62`), and close documents must agree (`SPEC.md:63-64`).
- SPEC Section 0.4 requires same-wave gate consumption for emitted non-JSON
  telemetry and rejects producer-only telemetry, unsupported outcomes, stale run
  ids, policy leaks, parse-only admission, and orphan SIMD (`SPEC.md:120-172`).
- SPEC Section 6 authorizes W1b-1 as a generated Track 1 + independent oracle
  scaffold without the lightningcss throughput gate (`SPEC.md:389-392`), owns
  the needed report/gate/bench/runtime/result/redress paths (`SPEC.md:394-417`),
  and requires report consumption of generated size, checksums, strictness,
  validation/profile artifacts, Lock 14/16, scalar-reference, parity, and JSON
  guard state (`SPEC.md:443-447`).
- SPEC Section 6 exit/revert is measurable: PASS requires generated Track 1
  plus independent Track 2/oracle to compile, execute, emit strict-equal
  `css_l4_declaration_value_fact_stream`, finite Mbps, and generated-size
  telemetry (`SPEC.md:449-453`); FAIL/BLOCKED records REDRESS and does not open
  Sheets/BBNF fallback (`SPEC.md:454-458`); rollback saves
  `/tmp/skv12-waveW1b-1-rejected.patch` (`SPEC.md:460-461`).
- The plan prevents admission overclaim: W1b-1 admits no CSS SOTA row, does not
  use `lightningcss`, does not touch SIMD/aarch64, and leaves the
  `track1_mbps > lightningcss_mbps + 1` bar to W1b-2 (`PLAN.md:8-12`).
- The plan requires generated proof and oracle independence: CSS-owned
  provider/templates emit runtime files and a reproducibility test byte-compares
  generated output (`PLAN.md:14-25`), while the oracle is forbidden from calling
  generated Track 1, `runtime::generated_json`, root CSS runtime,
  `lightningcss`, `parse_that_regex`, or `bbnf-simd` (`PLAN.md:77-85`).
- The plan demands companion gate fields without changing the main JSON table:
  `SkV12NonJsonRow` validation is extended, not `TelemetryRow`; W1b-1 requires
  exact row id, output plane, `C/GO`, finite Track 1/oracle Mbps, sample count,
  equality, independence, generated size, `lock14_status`, scalar-only Lock 16,
  scalar reference, and parity status (`PLAN.md:93-131`).
- The plan explicitly forbids outcome/schema/RESULTS overclaim: no new outcome
  variant, no main `RESULTS.md` JSON columns, and no `lightningcss_mbps`
  placeholder in W1b-1 (`PLAN.md:133-134`).
- The plan requires full JSON guard refresh because generic codegen selection,
  runtime exports, report validation, and bench dependencies move
  (`PLAN.md:153-162`), with `check-json`, `check-real-typed`, and
  `check-conformance` added if generated JSON output or JSON behavior moves
  unexpectedly (`PLAN.md:165-167`).
- The plan includes a preservation audit for forbidden couplings and unchanged
  `RESULTS.md` (`PLAN.md:190-198`) and a rollback protocol that saves the patch,
  reverts only the W1b-1 owner slice, records measured `BLOCKED/FAIL`, and
  forbids substituting Sheets, BBNF-self, JSON rows, root CSS runtime,
  `complex-errors.css`, or a report-only close (`PLAN.md:200-212`).
- Current ledgers make the next REDRESS entry unambiguous. W1a is REDRESS 121
  and passed refreshed JSON guard floors (`REDRESS.md:3555-3601`). W2 is
  REDRESS 122 and made no production scanner, generated JSON, gate,
  `RESULTS.md`, or row admission change (`REDRESS.md:3603-3632`). W1b-1 redress
  should therefore record REDRESS 123.
- Current executable non-JSON validation is weaker than the plan. The struct
  lacks `strictness`, grammar/input checksums, input bytes,
  `measured_validation_path`, `profile_artifact`, generated LOC/module bytes,
  `grammar_size_guard`, Lock 14/16 status, scalar-reference status, and parity
  status (`report.rs:174-203`). The validator still expects
  `direct_to_struct` output plane `direct_sink` (`report.rs:1897-1905`), which
  conflicts with the selected W1b-1 plane. Redress must close this gap before
  PASS.

## Required Redress Preconditions

1. Extend `SkV12NonJsonRow` and `validate_skv12_non_json_row` before accepting
   any W1b-1 report. The executable gate must require the plan's new fields,
   accept only `css_l4/declaration_values/direct_to_struct/main` with
   `css_l4_declaration_value_fact_stream`, and reject missing/bad checksums,
   size telemetry, Lock 14/16 status, scalar-reference status, parity status,
   stale run ids, coupled oracle paths, and producer-only artifacts.
2. Because W1b-1 necessarily moves generic codegen selection, runtime export,
   bench dependencies, and report/gate validation, the W1b-1 report must record
   `json_guard_state = refreshed:<run-id>:guards-pass`. The existing
   `not_refreshed:no_behavior_drift` allowance is not admissible for this wave.
3. Run the W1b-1 test/bench/gate commands from the plan, including generated
   runtime reproducibility, runtime CSS tests, `bbnf-bench` non-JSON tests,
   Criterion bench for the selected CSS row, and
   `xtask gate-json --skv12-non-json-report ... --check-results`.
4. Run the full JSON guard refresh and the AWK floor check exactly because
   codegen/runtime/report/bench surfaces move. If any JSON guard row falls
   below the SPEC Section 0.5 floor, W1b-1 cannot PASS unless the miss is
   recorded as a measured REDRESS demotion under the SPEC.
5. Keep `skinny/RESULTS.md` unchanged for W1b-1. No CSS SOTA row, no
   `lightningcss_mbps`, no threshold/admission placeholder, no new outcome
   variant, and no new main JSON telemetry column may land in this wave.
6. Record REDRESS 123 on either outcome. PASS must say W1b-1 is a generated
   scaffold/equality admit only, not CSS ADMIT. FAIL/BLOCKED must save
   `/tmp/skv12-waveW1b-1-rejected.patch`, revert only the W1b-1 owner slice,
   preserve unrelated work, and keep Sheets/BBNF fallback blocked until W1b-2
   records measured CSS lightningcss redress or the user re-pins.

## Blockers

No CH3 plan-time blocker remains.

Redress is blocked if the executable non-JSON gate is not strengthened to match
the plan, if JSON guard floors are stale or below floor without measured
demotion, if `RESULTS.md` moves, if W1b-1 emits lightningcss/admission
placeholders, or if failure handling skips REDRESS 123 and the rejected patch
artifact.

CH3 does not adjudicate whether the CSS provider scaffold is sufficiently
"generated"; if CH1/CH2/CH6 reject that premise, W1b-1 must record
`BLOCKED/FAIL` under `G-W1b-1-CSS-L4-ORACLE` rather than papering over it.
