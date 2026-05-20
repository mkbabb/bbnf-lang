# SK-V12 W0 PIN Research A3 - Results Surface

Date: 2026-05-20.
Scope: read-only `skinny/RESULTS.md` seed and guard surface.
Verdict: REVISE.

## Findings

The JSON seed counts match SK-V12 Section 0.5:

- `parse_only`: 16 `S / NO-GO` plus 1 `L / NO-GO`;
- `direct_to_struct`: 4 `A / GO` plus 13 `N-direct / NO-GO`;
- `real_typed_struct`: 7 `A / GO`.

The CSS L4 row is absent, as expected for W0. `RESULTS.md` is still a JSON
bench result table and has no `css_l4`, `lightningcss`, or SK-V12 CSS
admission row.

The named JSON guard floors currently hold:

- direct guards: `citm_catalog`, `apache_builds`, `marine_ik`,
  `unicode_basic`;
- typed guards: `twitter`, `citm_catalog`, `apache_builds`, `github_events`,
  `update_center`, `mesh`, `marine_ik`.

The defect is metadata drift. `RESULTS.md` still declares
`## SK-V9 W0 Telemetry Manifest`, uses `SK-V9-open` and
`sk-v9-open:criterion-fnv64-c8d7e0468358f98c` in manifest rows, and ends the
notes with `SK-V9 W0 telemetry`. W0 requires current pin run/status
reconciliation before `G-W0-PIN-TELEMETRY` can PASS.

## Redress Input

W0 plan should select a narrow `skinny/RESULTS.md` manifest/status
reconciliation: preserve row measurements and outcomes, but replace stale
SK-V9-open authority labels with the SK-V12 pin revalidation authority and
record that the JSON numbers are carried seed rows, not fresh CSS admission.

## Sources

- `skinny/RESULTS.md`
- `restart/skinny/tranches/sk-v12/SPEC.md` Section 0.5 and Section 3
