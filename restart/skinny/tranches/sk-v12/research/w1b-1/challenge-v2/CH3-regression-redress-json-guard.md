# SK-V12 W1b-1 CHALLENGE V2 CH3 - Regression / REDRESS / JSON Guard

Date: 2026-05-20.
Scope: CH3 adversarial review of the W1b-1 Plan V2 CSS L4 scaffold for
REDRESS honesty, JSON guard preservation, no `RESULTS.md` overclaim, rollback,
and refreshed guard requirements.
Output: this file.

## Verdict

ACCEPT WITH HARD REDRESS PRECONDITIONS.

Plan V2 preserves the CH3 safety surface. It keeps W1b-1 as a scalar
generated-scaffold/equality wave, not a CSS SOTA admission; leaves the
`track1_mbps > lightningcss_mbps + 1` bar to W1b-2; carries a full refreshed
JSON guard; keeps the main JSON `RESULTS.md` table unchanged; names REDRESS
and rollback handling; and now owns the Lock 14 executable gate file whose
same-wave result must be consumed by the non-JSON report.

No CH3 plan-time blocker remains. Redress may not PASS unless the executable
gate is strengthened to match the plan and the refreshed JSON guard is real,
same-wave, and recorded in the W1b-1 report.

## Evidence

- `challenge-v1/CONSOLIDATED.md` rejected V1 for CH2 and CH4 only. CH3 already
  accepted the direction with hard preconditions: strengthen the non-JSON gate,
  record the next REDRESS item, keep `RESULTS.md` unchanged, and rerun JSON
  guards.
- `PLAN.md` V2 preserves the admission boundary: W1b-1 admits no CSS SOTA row,
  does not use `lightningcss`, does not touch `bbnf-simd` or aarch64 code, and
  leaves lightningcss admission to W1b-2.
- `PLAN.md` V2 repairs the V1 Lock 14 owner gap by selecting
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs` and requiring the report to
  record `lock14_status = pass` only when the executable Lock 14 path runs in
  the same gate command.
- `SPEC.md` Section 6 now owns `lock14_baseline.rs`, the non-JSON report/gate
  paths, the CSS fixture/report/artifacts, and `skinny/RESULTS.md` /
  `skinny/REDRESS.md`. Its tasks require generated-size telemetry,
  grammar/input checksums, validation/profile artifacts, Lock 14/16 status,
  scalar-reference status, parity status, and JSON guard state to be consumed
  in the same wave.
- `SPEC.md` Section 6 exit remains measurable and non-paper: PASS requires
  generated Track 1 plus independent Track 2/oracle to compile, execute,
  produce strict-equal `css_l4_declaration_value_fact_stream`, and emit finite
  Mbps plus generated-size telemetry. FAIL/BLOCKED records REDRESS and keeps
  Sheets/BBNF fallback blocked until later measured CSS redress or re-pin.
- `PLAN.md` V2 requires a refreshed JSON guard because generic codegen
  selection, runtime exports, report validation, and bench dependencies move.
  It also requires `git diff --exit-code -- RESULTS.md` as the preservation
  audit.
- `skinny/RESULTS.md` currently has no CSS L4 row and still records the JSON
  campaign surface. W1b-1 must not add CSS admission placeholders, new main
  telemetry columns, a `lightningcss_mbps` field, or a new outcome variant.
- `skinny/REDRESS.md` records W1a as item 121 with refreshed JSON guard floors
  held, and W2 as item 122 with no production scanner, generated JSON, gate,
  `RESULTS.md`, or row-admission change. The next W1b-1 redress entry is
  therefore REDRESS 123.
- The current executable non-JSON validator is still weaker than Plan V2: it
  lacks the new W1b-1 provenance/size/Lock/parity fields, still maps
  `direct_to_struct` to `direct_sink`, and still permits
  `not_refreshed:no_behavior_drift`. That is acceptable only as pre-redress
  source state because Section 6 owns the report/gate changes. It becomes a
  CH3 redress blocker if not fixed before PASS.

## Required Redress Preconditions

1. Strengthen `SkV12NonJsonRow` and `validate_skv12_non_json_row` before any
   W1b-1 PASS. The gate must require the exact row
   `css_l4/declaration_values/direct_to_struct/main`, the exact output plane
   `css_l4_declaration_value_fact_stream`, strict equality, finite Track 1 and
   oracle Mbps, generated LOC/module bytes, grammar/input checksums,
   validation/profile artifacts, Lock 14/16 status, scalar-reference status,
   parity status, retained artifact paths, and sample-count/run-id freshness.
2. For this row, reject `json_guard_state = not_refreshed:no_behavior_drift`.
   W1b-1 must record `json_guard_state = refreshed:<run-id>:guards-pass`
   produced by the same-wave JSON guard commands.
3. Run the W1b-1 scaffold commands from Plan V2: codegen reproducibility,
   runtime CSS tests, non-JSON oracle/equality tests, Criterion measurement for
   the selected CSS row, and `xtask gate-json --skv12-non-json-report ...`.
4. Run the full JSON guard refresh and the SK-V12 floor AWK check. If a JSON
   guard floor misses, W1b-1 cannot PASS unless the miss is recorded as a
   measured REDRESS demotion under SPEC Section 0.5.
5. Keep `skinny/RESULTS.md` unchanged in W1b-1. No CSS SOTA admission,
   lightningcss comparator placeholder, new outcome identifier, or new main
   JSON telemetry column may land in this wave.
6. Record REDRESS 123 on either outcome. PASS must state that W1b-1 is only a
   generated CSS scaffold/equality admit. FAIL/BLOCKED must save
   `/tmp/skv12-waveW1b-1-rejected.patch` if a patch was attempted, revert only
   the W1b-1 owner slice, preserve unrelated work, and keep Sheets/BBNF
   fallback blocked until W1b-2 records measured CSS lightningcss redress or
   the user re-pins.
7. The report may not self-certify guard facts. Lock 14, Lock 16
   `not_applicable:scalar_only`, scalar-reference status, parity status, and
   JSON guard state must be produced or checked by executable same-wave gate
   code.

## Blockers

No CH3 V2 plan-time blocker remains.

Redress is blocked if the executable non-JSON gate remains weaker than Plan V2,
if the JSON guard is stale or below floor without measured demotion, if
`skinny/RESULTS.md` moves, if W1b-1 emits lightningcss/admission placeholders,
if REDRESS 123 is skipped, or if failure handling does not preserve the
rejected patch and unrelated work.

CH3 does not adjudicate whether the CSS scaffold is sufficiently generated or
whether the <=30 minute redress cap is plausible. Those remain CH1/CH2/CH4/CH6
surfaces; if they fail, W1b-1 must record `BLOCKED/FAIL` under
`G-W1b-1-CSS-L4-ORACLE` rather than paper-closing the row.
