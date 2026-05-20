# SK-V12 W1a CH3 - Regression / REDRESS Challenge

Scope: mandatory W1a CHALLENGE, CH3 regression and REDRESS lens only.

Read set:

- `docs/precepts/instructions/tranche/CHALLENGE.md`
- `restart/skinny/tranches/sk-v12/SPEC.md` Section 4, with Sections 0.5, 2.1,
  and 11 as guard context
- `restart/skinny/tranches/sk-v12/research/w1a/PLAN.md`
- `restart/skinny/tranches/sk-v12/research/w1a/PLAN-P1-grammar-profile.md`
- `restart/skinny/tranches/sk-v12/research/w1a/PLAN-P2-lock14-gate.md`
- `restart/skinny/tranches/sk-v12/research/w1a/A1-codegen-template-leaks.md`
  through `A6-json-guard-redress.md`
- `restart/skinny/tranches/sk-v12/research/w1a/CONSOLIDATED.md`
- `skinny/RESULTS.md`
- tail of `skinny/REDRESS.md`

## Verdict

DISPOSITION: REVISE

The selected W1a route is not rejected: codegen-private grammar metadata,
generated JSON-local config, and a Lock 14 scan consumed by `gate-json` are the
right regression shape for Section 4. The plan also correctly avoids CSS row
admission, lightningcss comparison, Sheets fallback, BBNF-self fallback, report
schema churn for the scan, and direct-row reclamation.

The plan must be revised before redress because the accounting artifacts still
leave two CH3 gaps:

1. REDRESS 121 PASS wording must not allow the selected JSON-touching route to
   record `not_refreshed:no_behavior_drift`, and must not treat a generated CSS
   metadata compile proof as W1a evidence.
2. The canonical rejected-patch slice must include all generated W1a outputs,
   including `skinny/crates/bbnf-bench/src/generated_real_typed.rs` when typed
   output changes. `PLAN-P2` and `A6` have broad patch slices that can catch
   this; `PLAN.md` and `PLAN-P1` omit it.

## Claim Checks

| Claim / risk | Evidence | CH3 disposition | Plan consequence |
|---|---|---|---|
| JSON guard refresh | SPEC Section 0.5 requires guard rerun or no-touch proof for any JSON-producing path movement. `PLAN.md` and `PLAN-P1` select codegen/runtime JSON-producing edits and explicitly say `not_refreshed:no_behavior_drift` is not valid. `PLAN-P2` narrows no-touch to the case where no JSON-producing path moved. | Narrowed / revise accounting | REDRESS 121 PASS for this selected route must record `json_guard_state = refreshed:<run-id>:guards-pass`. If native refresh is skipped, stale, or below floor, W1a is BLOCKED/REJECTED, not PASS. |
| SPEC Section 0.5 floors | `skinny/RESULTS.md` currently has 41 JSON rows and the named direct/typed guard rows clear the Section 0.5 floors. `A4` correctly notes current executable validation does not exactly enforce the whole SK-V12 floor table. `PLAN-P2` names every direct and typed floor. | Accepted with executable guard | Final evidence needs an exact floor consumer: either an additive validator that does not change schema/output, or a mechanical post-refresh floor check recorded in REDRESS 121. Existing `gate-json` checks alone are not enough if they do not consume the exact table. |
| Generated exactness | `A4` proves `check-json` is byte-exact for expected files but does not reject stale extra files, and that real typed output is generated separately at `skinny/crates/bbnf-bench/src/generated_real_typed.rs`. The plan requires JSON runtime roster exactness and `check-real-typed`. | Accepted, with patch-slice revision | If `config.rs` or any roster change lands, stale extra `.rs` files in `runtime/src/grammars/json` must fail. If typed output changes, `generated_real_typed.rs` is a W1a generated output for patch save/revert accounting. |
| REDRESS 121 accounting | `skinny/REDRESS.md` ends at Item 120. A6 and the plans route W1a to Item 121 under `G-W1a-GRAMMARCONFIG-LOCK14` and prohibit CSS row/SOTA/close claims. A6's PASS template still includes a no-touch option and "generated CSS L4 metadata compile proof." | Revise | Item 121 should say W1a is a legality gate only. PASS evidence is Lock 14 scan, JSON generated parity, exact JSON guard floors after refresh, generated-size facts, and no forbidden surface expansion. Remove or narrow any CSS metadata proof language unless it is only a non-admitting codegen fail-closed test with no CSS generated files, rows, or comparator claim. |
| Rejected patch path | SPEC Section 4 requires `/tmp/skv12-waveW1a-rejected.patch` on FAIL. The plan uses that path and requires path inspection before revert. `PLAN.md` / `PLAN-P1` save only selected bench files and omit `generated_real_typed.rs`; `PLAN-P2` / A6 use broader `skinny/crates/bbnf-bench` slices. | Revise | Converge the canonical command before redress. It must save only W1a candidate files, include generated JSON and typed outputs if touched, include `skinny/RESULTS.md` if regenerated, and avoid saving unrelated user or parallel-agent edits. Inspect and split before any revert. |
| Prior REDRESS routes | REDRESS 111 is the SK-V11 schema-only companion lane; 112/113 block generated non-JSON baseline/intervention; 114-119 block or fixpoint JSON direct residual routes; 120 closes SK-V11 as a measured fixpoint. SPEC Section 11 keeps report-only CSS, stale sidecar, parse-only admission, generic grammar policy, union/event-model, and ASM-gen replay routes blocked without material differential and CHALLENGE. | Accepted | W1a may use companion-report tests only as regression tests, not as CSS evidence. REDRESS 121 must explicitly carry forward 112/113, not cite 114-119 JSON direct residuals as CSS authority, and not treat SK-V11 close as SK-V12 close. |

## Required Revision

Before W1a redress dispatch, update the plan/accounting text so these are
unambiguous:

- `json_guard_state = not_refreshed:no_behavior_drift` is illegal for the
  selected W1a implementation because it moves JSON-producing codegen/runtime
  paths.
- REDRESS 121 PASS must not include a CSS L4 parser row, lightningcss result,
  CSS SOTA claim, generated-baseline admission, Sheets fallback, BBNF-self
  fallback, SK-V12 close, or future-phase promise.
- The `/tmp/skv12-waveW1a-rejected.patch` command must cover every W1a
  generated output that can move, including `generated_real_typed.rs` when
  typed regen changes, while preserving unrelated dirty/staged work.
- SPEC floor proof must be exact and tied to the refreshed run id used for
  `skinny/RESULTS.md` exactness.

After those revisions, CH3 has no remaining objection to the W1a legality route.
