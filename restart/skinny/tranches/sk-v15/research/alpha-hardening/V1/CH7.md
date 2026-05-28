# CH7 Overfit-Prune — SK-V15 Alpha V1

Date: 2026-05-27.

## Verdict

REVISE, folded.

The first draft stated `NEW-CH7-V5-03`, but the load-bearing gates narrowed
to Lock 14 and did not consistently bind Lock 16 or gate exclusions.

## Folded Fixes

- `SYNTHESIS.md`, `HANDOFF.md`, `alpha-C`, `alpha-E`, and `alpha-F` now bind
  Lock 14 / Lock 16 exclusion reporting.
- Gate exclusions must be emitted as findings; a self-exempting grep gate
  cannot close the tranche.
- The owner paths in `alpha-E` pin Lock 14 to
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs` and Lock 16 to the
  `primitive-checkasm`, `bbnf-simd` checkasm, report telemetry, and gate
  consumer surfaces.
- `lock16_status`, `checkasm_or_parity_status`, and
  `gate_exclusion_report` are now Alpha telemetry fields.
- The binding close condition rejects self-exempting grep/checkasm gates.

## Residual Risk

None at Alpha scope. S-P0 must verify these owner surfaces at HEAD before
expanding the gate work.
