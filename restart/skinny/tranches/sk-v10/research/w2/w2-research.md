# SK-V10 W2 Research - Direct Row-Table Reclamation

Pass: Wave Research.
Cycle: W2.
Date: 2026-05-19.
Scope: read-only evaluation of opening direct rows under the W1 contract.

## Inputs

- W0 closed under REDRESS 99 and fixed the SK-V10 opening telemetry freeze.
- W1 closed under REDRESS 100 and added the executable direct row movement
  contract to `Report::validate_sk_v8_w0`.
- SPEC Section 5 authorizes zero behavior-source movement only: direct rows
  may move if generated Track 1 and independent Track 2 both satisfy the
  Section 0.2 direct floor on the same strict direct comparator plane.
- The frozen same-run Criterion authority remains
  `CRITERION_HOME=target/skv9-w1/criterion` with run id
  `sk-v9-open:criterion-fnv64-a1e8a51ae806d386`. W2 source does not change
  benchmark behavior, so this capture is the fresh same-run direct report for
  row-table reclamation.

## Opening Direct Floor Scan

The opening `skinny/RESULTS.md` direct rows were scanned against the Section
0.2 floors:

| Corpus | Outcome | Track 1 | Track 2 | sonic-rs direct | Floor | Disposition |
|---|---:|---:|---:|---:|---:|---|
| `twitter` | `N-direct / NO-GO` | 11931 | 11064 | 15224 | 13840 | floor miss |
| `canada` | `N-direct / NO-GO` | 10466 | 10326 | 12074 | 10977 | floor miss |
| `apache_builds` | `N-direct / NO-GO` | 11157 | 10145 | 11021 | 10020 | W2 candidate |
| `github_events` | `N-direct / NO-GO` | 11983 | 11091 | 15800 | 14364 | floor miss |
| `update_center` | `N-direct / NO-GO` | 8356 | 7561 | 11176 | 10160 | floor miss |
| `mesh` | `N-direct / NO-GO` | 8431 | 8769 | 9807 | 8916 | floor miss |
| `random` | `N-direct / NO-GO` | 7685 | 6927 | 8507 | 7734 | floor miss |
| `gsoc-2018` | `N-direct / NO-GO` | 14676 | 14126 | 23078 | 20980 | floor miss |
| `instruments` | `N-direct / NO-GO` | 11708 | 10803 | 12194 | 11086 | Track 2 miss |
| `numbers` | `N-direct / NO-GO` | 12182 | 11803 | 12966 | 11788 | W2 candidate |
| `unicode_mixed` | `N-direct / NO-GO` | 4609 | 4562 | 10245 | 9314 | floor miss |
| `unicode_escapes` | `N-direct / NO-GO` | 5131 | 5025 | 13779 | 12527 | floor miss |
| `distinct_values` | `N-direct / NO-GO` | 6052 | 5241 | 11024 | 10022 | floor miss |
| `y_string_unicode` | `N-direct / NO-GO` | 4887 | 3669 | 8829 | 8027 | floor miss |

Only `apache_builds` and `numbers` clear both generated Track 1 and independent
Track 2 direct floors. These are also the two W0 no-admission clamp rows whose
sonic slack already classified as direct passes before W0 intentionally held
them at `N-direct / NO-GO`.

## Guard Rows

Direct guard rows preserve their direct maintain floors:

| Corpus | Track 1 | Floor | Status |
|---|---:|---:|---|
| `citm_catalog` | 21129 | 18145 | pass |
| `marine_ik` | 9205 | 7575 | pass |
| `unicode_basic` | 8973 | 7841 | pass |

Typed guard rows preserve their typed maintain floors:

| Corpus | Track 1 | Floor | Status |
|---|---:|---:|---|
| `twitter` | 18302 | 14424 | pass |
| `citm_catalog` | 35102 | 20053 | pass |
| `apache_builds` | 8174 | 7373 | pass |
| `update_center` | 11847 | 11365 | pass |
| `mesh` | 10032 | 8428 | pass |
| `marine_ik` | 10728 | 7369 | pass |

## Implementation Shape

W2 should not edit parser/runtime behavior. The row-table move can be expressed
in the `gate-json` renderer:

- keep `classify_direct_projection` unchanged;
- replace the W0 no-admission clamp with a W2 reclamation predicate for
  `apache_builds` and `numbers` only when Track 1 and Track 2 meet Section 0.2
  floors;
- render admitted rows with W1-required strict direct contract fields:
  `strictness=strict`, `parse_utf8=measured-row`,
  `measured_validation_path=measured-row`,
  `same_wave_consumer_class=gate_json_direct_contract`,
  `redress_entry=REDRESS-101`, and `wave_id=SK-V10-W2`;
- leave all non-candidate rows and existing direct/typed guards unchanged.

## Recommendation

Proceed to W2 plan with a measured admission of `apache_builds` and `numbers`
direct rows only. All other `N-direct` rows remain `NO-GO` and should be named
as routed remainder in REDRESS.
