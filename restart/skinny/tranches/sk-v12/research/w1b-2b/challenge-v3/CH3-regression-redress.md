# SK-V12 W1b-2b CH3 - Regression / REDRESS Review

Date: 2026-05-20.
Lens: CH3 regression / REDRESS.
Plan under review: `restart/skinny/tranches/sk-v12/research/w1b-2b/PLAN-V3.md`.

## Disposition

ACCEPT.

PLAN-V3 is admissible for redress from the CH3 lens. It no longer reopens the
V2 regression concerns: it binds W1b-2b to `REDRESS-125`, separates CSS
Criterion evidence from the JSON guard root, routes `RESULTS.md`
reconciliation to W5 close, preserves no-write behavior for W1b-2b, and names
the rejection patch path required by SPEC Section 7.2.

## Review

- REDRESS numbering is correct. `skinny/REDRESS.md` currently ends W1b-2a at
  item 124, and PLAN-V3 uses `REDRESS-125` only. This avoids the stale
  REDRESS-124 ambiguity from earlier research text and matches the SPEC 7.2
  gate surface.
- The two-command evidence protocol clears the regression risk in V2. CSS
  SOTA validation reads the live `nonjson_css_l4` Criterion lanes from the CSS
  root, while the JSON guard/stale check runs separately against
  `/tmp/skv12-w1a-json-guard-criterion` with no CSS report flag. This prevents
  an empty or wrong Criterion root from being treated as both CSS evidence and
  JSON guard authority.
- `RESULTS.md` routing is bounded. PLAN-V3 makes W1b-2b a measured companion
  gate/report disposition only, with final CSS RESULTS reconciliation deferred
  to W5. That matches the V2 consolidated revision request and avoids broad
  renderer or stale-results rewrites in this wave.
- No-write behavior is executable enough for redress. PLAN-V3 requires the
  JSON guard command to run without the CSS flag and requires a before/after
  byte comparison of `skinny/RESULTS.md`; a no-write miss routes to
  `BLOCKED/FAIL`.
- Failure routing is named. PLAN-V3 carries the SPEC 7.2 rollback path
  `/tmp/skv12-waveW1b-2b-rejected.patch` and routes missing report, stale
  Criterion lanes, stale retained artifacts, comparator isolation failure, bad
  JSON guard root, no-write failure, or source/gate failure to `BLOCKED/FAIL`.

## Non-Blocking Redress Notes

- The redress entry should state explicitly whether `PASS-ADMIT-CANDIDATE` or
  `PASS-MEASURED-BASELINE` was recorded, because neither outcome moves
  `skinny/RESULTS.md` in W1b-2b.
- If failure occurs before source edits exist, the redress agent should still
  record that no source patch was available at the named patch path rather than
  silently omitting the rollback evidence.
