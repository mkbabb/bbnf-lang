# SK-V12 W1b-2b CH3 - Regression / REDRESS

Date: 2026-05-20.
Wave: W1b-2b - CSS L4 Lightningcss SOTA Report + Admission Gate.
Lens: CH3 regression / REDRESS.
Disposition: ACCEPT.

## Findings

PLAN-V2 clears the prior CH3 blocker. It binds W1b-2b to `REDRESS-125` only,
uses the exact exit gate `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA`, and explicitly
supersedes stale A1/A6 and initial PLAN text. This matches SPEC Section 7.2 and
the current REDRESS tail, where W1b-2a consumed item 124 and W1b-2b is next.

JSON guard handling is admissible. PLAN-V2 requires the CSS report to validate
first, then routes `--check-results` / `--with-cost-facts` into the existing
JSON gate path using `/tmp/skv12-w1a-json-guard-criterion`; it also states that
CSS-only Criterion roots must fail closed. That satisfies SPEC Section 0.5 and
Section 7.2 without pretending the CSS Criterion root is a JSON guard root.

No-write behavior is now concrete enough for redress. `PASS-MEASURED-BASELINE`
must leave `skinny/RESULTS.md` byte-identical before/after the no-write guard
command. For `PASS-ADMIT-CANDIDATE`, RESULTS movement is allowed only for the
CSS row or an accepted measured JSON guard demotion, which preserves the prior
challenge's movement rule and avoids a report-only status edit.

Failure routing is acceptable. PLAN-V2 routes missing report, invalid/stale
Criterion lanes, equality failure, bad JSON guard root, no-write matrix failure,
or source/gate failure to `BLOCKED/FAIL`, REDRESS-125, and
`/tmp/skv12-waveW1b-2b-rejected.patch`.

## Redress Watchpoints

- The implementation must reject any W1b-2b report that names `REDRESS-124`,
  `W1b-2`, or a non-exact gate label.
- `PASS-ADMIT-CANDIDATE` must be derived from live Criterion `new/` lane means,
  not report-provided Mbps alone; equality at `lightningcss_mbps + 1` remains a
  baseline, not an admit.
- The companion parser must reject mixed companion reports, update/write flags,
  volatile probes, missing path, flag-as-path, and unrelated extra args before
  any JSON or RESULTS side effect.
- If `skinny/RESULTS.md` moves on admit, the redress entry must state exactly
  whether the movement is a CSS row addition or a measured JSON guard demotion.

## Verdict

ACCEPT. No CH3 regression / REDRESS blocker remains in PLAN-V2; enforce the
watchpoints during redress.
