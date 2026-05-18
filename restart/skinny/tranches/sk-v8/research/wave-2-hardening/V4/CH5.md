# SK-V8 W2 Hardening V4 CH5 Review

Date: 2026-05-18.
Target reviewed: `74fe4e1b`
(`fix(sk-v8-wave2-gate): bind real typed metadata expectations to measured W0 rows`).
Lens: CH5 challenge-cycle integrity, hidden coupling, and governance discipline.

## Verdict

ACCEPT.

Confidence: 93%.

## Findings

1. V2 and V3 records are present and internally coherent. The reviewed commit
   archives the six V2 challenge files plus
   `HARDENING-W2-V2-CONSOLIDATED.md`, and the six V3 challenge files plus
   `HARDENING-W2-V3-CONSOLIDATED.md`. V2 consolidates as `ACCEPT, 6/6` with
   minimum confidence 93. V3 consolidates as `REVISE, 5/6 ACCEPT and 1/6
   REVISE` with minimum confidence 90. The V3 consolidated blocker matches
   V3 CH3: the checked report gate required Apache/CITM real typed Criterion
   metadata because it derived the requirement from the source fixture map
   rather than the W0 measured baseline row table.

2. The V3 REVISE fold is materially closed in `74fe4e1b`. The gate now calls
   `w0_real_typed_metadata_expected(&fixture.name)`, which derives the real
   typed metadata requirement from
   `sk_v8_open_baseline("json/{fixture}/real_typed_struct/main")`, not from
   `real_typed_struct::fixture_for_name`. The added regression test proves the
   intended split for W0 measured rows (`twitter`, `update_center`) versus W2
   source-only rows (`apache_builds`, `citm_catalog`).

3. The row-table boundary is still honest. `skinny/RESULTS.md` is unchanged in
   `74fe4e1b`; the measured W0 manifest still has four real typed rows and no
   measured Apache/CITM/Canada `real_typed_struct` rows. REDRESS 91 and HANDOFF
   now name the V3 gate mismatch and keep Apache/CITM as source/product parity
   rows until a later accepted benchmark row-table wave.

4. Commit-per-cycle discipline is imperfect but not blocking for CH5. V2 and
   V3 challenge archives both land in the same implementation fold commit
   instead of separate docs archive commits. That is weaker audit history than
   the W0 hardening pattern, but the files pin their reviewed target to
   `8ce03af4`, the V3 consolidated file remains explicitly REVISE, and
   `74fe4e1b` does not use V2/V3 as two qualifying convergence cycles. The
   combined commit body also names that it folds W2 hardening V2/V3 evidence
   plus the V3 checked-report objection, so the archive/fold merger is visible
   rather than hidden.

5. Evidence naming is sufficient. The commit body names the focused regression
   test, Lock 14, real typed, codegen typed-direct, JSON/conformance, checked
   gate, and whitespace verification commands. Its claimed checked-gate result
   matches the observed failure mode after the fold: the standard
   `gate-json --advisory --check-results` path reaches the recorded W0 run-id
   drift instead of failing first on missing Apache/CITM real typed metadata.

6. No unresolved CH5 REVISE fold remains. The only V3 REVISE requirement was to
   bind real typed metadata expectations to measured W0 rows while preserving
   the W0 run-id strict validator. The executable gate, regression test,
   REDRESS text, and HANDOFF text now agree on that split. The remaining
   checked-gate failure is the pre-existing W0 run-id drift, not a W2 typed
   metadata blocker.

## Verification

- `git status --short` before writing this file: clean.
- `find restart/skinny/tranches/sk-v8/research/wave-2-hardening -maxdepth 3 -type f`
  confirmed V1/V2/V3 each have six `CH*.md` records plus one consolidated file.
- `rg` over V2/V3 verdicts confirmed V2 is 6/6 ACCEPT and V3 is 5/6 ACCEPT
  with CH3 REVISE.
- `git log --oneline -- restart/skinny/tranches/sk-v8/research/wave-2-hardening/V2`
  and the same command for `V3` confirmed both archive sets were introduced by
  `74fe4e1b`.
- `git diff --exit-code 74fe4e1b^ 74fe4e1b -- skinny/RESULTS.md` passed.
- `git diff --check 74fe4e1b^ 74fe4e1b` passed.
- `cargo test -p bbnf-bench w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures -- --nocapture`
  passed.
- `cargo xtask gate-json --advisory --check-results` failed at the known W0
  run-id strict drift (`json/twitter/parse_only/main run_id moved ...`) and did
  not fail on missing Apache/CITM real typed metadata.

## Required Folds

None for CH5.

The next consolidation should not count V2/V3 as consecutive ACCEPT cycles:
V3 is a recorded REVISE, and V4 is the first post-fold challenge cycle for
`74fe4e1b`.
