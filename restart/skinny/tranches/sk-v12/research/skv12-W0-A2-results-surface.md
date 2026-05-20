# SK-V12 W0-A2: Results Surface

Date: 2026-05-20.
Scope: SK-V12 W0 read-only audit of `skinny/RESULTS.md` row counts, opening
outcomes, and guard floors.
Output: this file.

## Section 1 - Findings

`skinny/RESULTS.md` carries exactly 41 JSON main rows at SK-V12-open.

| Plane | Count | Opening outcome summary |
|---|---:|---|
| parse diagnostic | 17 | 16 `S / NO-GO`, 1 `L / NO-GO` (`canada`) |
| direct to struct | 17 | 4 `A / GO`, 13 `N-direct / NO-GO` |
| real typed struct | 7 | 7 `A / GO` |

The direct A/GO rows are `citm_catalog`, `apache_builds`, `marine_ik`, and
`unicode_basic`.

The typed A/GO rows are `twitter`, `citm_catalog`, `apache_builds`,
`github_events`, `update_center`, `mesh`, and `marine_ik`.

The audited guard floors pass for the four direct A/GO rows and the seven typed
A/GO rows. No count or floor mismatch blocks W0.

## Section 2 - Recommendations

W0 should preserve the SK-V12-open JSON row surface. Any row movement during W0
would violate Section 3 because W0 is a telemetry/report lock, not a behavior
wave.

## Section 3 - Risks

The parse diagnostic plane remains conceded and should not be interpreted as a
W0 failure. The 13 direct `N-direct / NO-GO` rows are SK-V12 carry-in work, not
opening-lock drift.

## Section 4 - Sources

- `skinny/RESULTS.md`
- `restart/skinny/tranches/sk-v12/SPEC.md` Section 3
- `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md`
