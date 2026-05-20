# SK-V11 W4-R5 Row Floors

Date: 2026-05-20.
Scope: research-only row-floor extraction for W4 generated dispatch and
byte-set control.
Output: this file.

## Inputs

- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v11/SPEC.md` Sections 0.4, 0.5, and 8
- `/tmp/skv11-open-criterion-3ce75df`
- `/tmp/skv11-w3-criterion`

## Baseline And Latest Evidence

- SK-V11-open authority is `/tmp/skv11-open-criterion-3ce75df`, commit
  `3ce75df4`, run id `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`.
- W4 can dispatch because W3 now has a measured REDRESS rejection. W2's
  non-JSON axis remains blocked and must be carried forward.
- The latest available `/tmp/skv11-*criterion*` root is
  `/tmp/skv11-w3-criterion`. It contains only `mesh` direct-row evidence for
  the rejected W3 numeric route:

| Bench | Measured Mbps |
|---|---:|
| `json/mesh/track1_direct_to_struct` | 3835 |
| `json/mesh/track2_direct_to_struct` | 3614 |
| `json/mesh/sonic_rs_direct_to_struct` | 4413 |
| `json/mesh/serde_json_direct_to_struct` | 3191 |

This W3 evidence does not replace the SK-V11-open seed floors, because the W3
patch was rejected and reverted. It is still a route signal: a numeric-slot
factor did not move `mesh`; W4 should not treat `mesh` as cheap unless the W4
dispatch/control route is proven distinct by a same-host caller microbench.

## Candidate Direct Rows

Floor is SPEC Section 0.4: `ceil(sonic-rs strict direct Mbps / 1.10)`.
Positive gaps are Mbps still needed from the SK-V11-open measurement.

| Candidate row | Floor | W0 Track 1 | T1 gap | W0 Track 2 | T2 gap | W0 sonic direct | Candidate-specific guard burden | W4 read |
|---|---:|---:|---:|---:|---:|---:|---|---|
| `canada/direct_to_struct` | 10637 | 10316 | 321 | 9819 | 818 | 11700 | No same-corpus typed guard. Global direct/typed guards still apply. | Plausible second row if the chosen D1/D2 helper targets container tails in both Track 1 and Track 2. Mostly numeric/array, so keep W3 numeric rejection separate. |
| `mesh/direct_to_struct` | 8675 | 8561 | 114 | 8652 | 23 | 9542 | `mesh/real_typed_struct` guard 9214 / 7739. | Lowest W0 gap but latest W3 route evidence is bad. Keep as probe or optional target only after CHALLENGE proves W4 is not replaying the rejected numeric path. |
| `random/direct_to_struct` | 7878 | 7693 | 185 | 6949 | 929 | 8665 | No same-corpus typed guard. Global direct/typed guards still apply. | Best W4 primary. It is mixed object/array/string/number/literal work, has a small Track 1 lift, and no same-corpus typed guard burden. Track 2 must be part of the same plan. |
| `update_center/direct_to_struct` | 10059 | 8187 | 1872 | 7474 | 2585 | 11064 | `update_center/real_typed_struct` guard 11613 / 10150. | Dispatch-relevant, but the required lift is too large for a one-shape W4 unless pre-redress probes show major movement. Treat as diagnostic/probe, not a target. |
| `github_events/direct_to_struct` | 13403 | 11918 | 1485 | 10596 | 2807 | 14743 | `github_events/real_typed_struct` guard 11633 / 12029. | Too much Track 2 lift for the first W4 slice. Keep for later dispatch/string/fixpoint work unless caller probes are unusually strong. |
| `twitter/direct_to_struct` | 13740 | 11613 | 2127 | 10816 | 2924 | 15113 | `twitter/real_typed_struct` guard 17385 / 15593. | High signal but high floor delta. Better as a W5/string or W8 residual target unless W4 microbench shows both-track movement above 20%. |

## Guard Floors

W4 exit requires selected direct rows to meet their floors on both tracks and
the SPEC Section 0.5 guard rows to hold.

Direct guards:

| Guard row | Track 1 maintain | Track 2 maintain |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 |

Typed guards:

| Guard row | Track 1 maintain | Track 2 oracle guard |
|---|---:|---:|
| `twitter/real_typed_struct` | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8308 | 6754 |
| `github_events/real_typed_struct` | 11633 | 12029 |
| `update_center/real_typed_struct` | 11613 | 10150 |
| `mesh/real_typed_struct` | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11552 | 9894 |

## Recommendation

Recommended one-row W4 target: `random/direct_to_struct`.

Recommended two-row W4 target, if the pre-redress microbench shows the same
helper moves both tracks: `random/direct_to_struct` plus
`canada/direct_to_struct`.

Do not spend the third row by default. The at-most-three rule is a ceiling, and
W4's exit gate is easier to falsify with a tight one/two-row plan. Keep
`mesh/direct_to_struct` as a probe or fallback only if CHALLENGE records why
the W4 D1/D2 dispatch-control shape is independent from the rejected W3 numeric
slot route. Keep `update_center`, `github_events`, and `twitter` out of the
initial target set unless same-host caller probes show unusually large
both-track movement before source dispatch.

Any generated-only C6 route is insufficient for admission: Track 2 must clear
the same Section 0.4 floor, so the plan must either mirror the helper in the
independent Track 2 path or name another same-wave reason Track 2 will move.

## Sources

- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/research/w3/redress/w3-redress-rejection.md`
