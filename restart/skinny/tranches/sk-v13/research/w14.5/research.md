# SK-V13 W14.5 Research - Mesh Parse-Only Admission

Date: 2026-05-22.
Wave: W14.5.
Scope: `json/mesh/parse_only/main`.

## Authority

W14.5 inherits the W14 parse-only re-pin and row-local material differential:
REDRESS 102 classified parse-only rows as view-boundary under the old campaign
bar, while W14.N admits only rows with gate-consumed DOM strict equality,
same-host Criterion evidence, and an explicit `JsonParseOnlyAdmissionSpec`.

## Current Row

After W14.4, `mesh` is the only remaining positive-margin OPEN parse-only row
that can be moved by the W14 table-admission pattern:

| field | value |
|---|---:|
| row | `json/mesh/parse_only/main` |
| status | `S / NO-GO` |
| rolling Track 1 | `12917` Mbps |
| rolling sonic strict + 1 | `11835` Mbps |
| rolling margin | `1082` Mbps |
| exact input bytes | `723597` |
| criterion group | `json_mesh` |

The current `RESULTS.md` row still carries the old
`borrowed view over offset tape vs DOM` status even though the same-run
Criterion lanes already show Track 1 above sonic strict. W14.5's work is to
bind that evidence to the measured-row DOM contract and gate it through
`gate-json`.

## Measurement Lanes

Required Criterion lanes:

- `json_mesh/track1_generated`
- `json_mesh/track2_handcoded`
- `json_mesh/sonic_rs_anchor`
- `json_mesh/serde_json`

Current seed estimates under the W14.4 capture are:

| lane | mean ns | slope ns |
|---|---:|---:|
| Track 1 generated | `448137.984` | `448144.403` |
| Track 2 handcoded | `479216.433` | `477309.359` |
| sonic-rs strict | `489354.588` | `489183.600` |
| serde_json | `1255070.499` | `1251308.140` |

These are sufficient for research only. Redress must refresh the `json_mesh`
lanes with `RUSTFLAGS="-C target-cpu=native"` before admission.

## Scope Boundary

W14.5 is not a runtime optimization wave. It must not edit parser runtime,
generated JSON parser bodies, SIMD code, union substrate, output digest paths,
or decision-engine policy. The only behavior-facing change is adding one
configured parse-only admission spec for `mesh`, then measuring and consuming
the corresponding report.

## Research Verdict

Dispatchable. `mesh` has positive same-run parse-only headroom against
sonic-rs strict and a well-defined W14 gate. After W14.5 closes, no remaining
JSON row is status/report-only admissible; subsequent waves must make real
implementation changes.
