# SK-V11 W8 Redress - Direct Residual Fixpoint

Date: 2026-05-20.

Wave: W8 - Direct Residual Fixpoint And Row Reclamation.

Gate: `G-W8-DIRECT-FIXPOINT`.

Disposition: CLOSED as measured direct fixpoint; no row admission.

## Summary

W8 executed SPEC Section 12 as docs-only direct residual fixpoint accounting.
Research found no legal W8a source split. Plan selected no behavior source,
gate schema, validator semantic, telemetry, or `RESULTS.md` row movement, so
SPEC permitted CHALLENGE to be skipped for gate/report-only fixpoint
accounting.

All 13 direct residual rows remain `N-direct / NO-GO`, but each now has a
per-row fixpoint proof tied to W3-W7 measured rejection or accepted entry
block. The non-JSON generated-intervention axis remains BLOCKED from
REDRESS 113 and is carried to W9 / Pass Alpha; W8 does not close it.

## Verification

- `git diff --exit-code -- skinny/RESULTS.md` passed.
- `CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p bbnf-bench --bin gate -- --advisory`
  passed with the unchanged `N-direct / NoGo` surface.

## Direct Residual Fixpoint Table

| Row | Track 1 | Track 2 | sonic direct | floor | W8 proof |
|---|---:|---:|---:|---:|---|
| `twitter/direct_to_struct` | 11613 | 10816 | 15113 | 13740 | W5 string-span route blocked by REDRESS 116; W7 digest route blocked by REDRESS 118; no W8a source candidate remains. |
| `canada/direct_to_struct` | 10316 | 9819 | 11700 | 10637 | W3 numeric route measured-rejected on sibling `mesh`; `canada` has larger Track 2 floor gap; no W8a numeric candidate remains. |
| `github_events/direct_to_struct` | 11918 | 10596 | 14743 | 13403 | W5 string-span route blocked; W7 digest visible-bucket math cannot close both tracks; no W8a candidate remains. |
| `update_center/direct_to_struct` | 8187 | 7474 | 11064 | 10059 | W5 string-span route blocked; W7 digest route floor-insufficient; no W8a candidate remains. |
| `mesh/direct_to_struct` | 8561 | 8652 | 9542 | 8675 | W3 `number_span_emit_slot` measured 3835 / 3614 against 8675 and was reverted; row remains uncloseable in SK-V11. |
| `random/direct_to_struct` | 7693 | 6949 | 8665 | 7878 | W4 `container_tail_next` probe measured 3518 / 3498 against 7878 and was reverted; W5/W7 blocked; no W8a candidate remains. |
| `gsoc-2018/direct_to_struct` | 2665 | 2578 | 4110 | 3737 | Movemask/string-scan residual; W5 and W7 leave no accepted source authority; no W8a candidate remains. |
| `instruments/direct_to_struct` | 11569 | 10736 | 9865 | 8969 | Numerically above floor but W0-clamped; no W3-W8 measured behavior provenance, so docs-only admission is pre-blocked. |
| `numbers/direct_to_struct` | 4479 | 2366 | 2667 | 2425 | Track 2 misses floor and row is W0-clamped; W3 numeric route rejected; no W8a candidate remains. |
| `unicode_mixed/direct_to_struct` | 3753 | 2427 | 2846 | 2588 | Track 2 misses floor and row is W0-clamped; W6 decoded-source route blocked by REDRESS 117; no W8a candidate remains. |
| `unicode_escapes/direct_to_struct` | 1345 | 1341 | 3785 | 3441 | Unicode escape route blocked by W5/W6 and SK-V10 REDRESS 107/108 proof-only limits; no W8a candidate remains. |
| `distinct_values/direct_to_struct` | 1750 | 1625 | 2923 | 2658 | W5 string route blocked; W7 digest bucket insufficient; no W8a candidate remains. |
| `y_string_unicode/direct_to_struct` | 1983 | 1029 | 4344 | 3950 | Unicode escape/string route blocked by W5/W6 and prior proof-only limits; no W8a candidate remains. |

## Consequence

W8 enables W9 close/Alpha feedback only as a fixpoint close, not an overall
direct `GO`. W9 must state that SK-V11 did not close the direct plane or the
non-JSON intervention axis; it instead produced measured row/family exhaustion
for W3-W8 and must feed those constraints into the next tranche.
