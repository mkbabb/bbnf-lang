# SK-V11 S-P1 Hardening Converged

Pass: S-P1 Profile.
Date: 2026-05-19.
Status: CONVERGED.

## Basis

S-P1 ran the full six-lens hardening sequence after the SK-V11-open baseline
and profile cohort:

| Cycle | CH1 | CH2 | CH3 | CH4 | CH5 | CH6 | Disposition |
|---|---|---|---|---|---|---|---|
| V1 | REVISE | REVISE | REVISE | REVISE | ACCEPT | ACCEPT | Folded provenance, source-map, Lock 14, and REDRESS citation defects. |
| V2 | ACCEPT | REVISE | ACCEPT | ACCEPT | ACCEPT | ACCEPT | Folded residual object/array summary vocabulary. |
| V3 | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | First all-ACCEPT cycle. |
| V4 | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | Second consecutive all-ACCEPT cycle. |

V3 and V4 are two consecutive six-of-six ACCEPT cycles with zero critical or
open REVISE findings. This satisfies the S-P1 challenge convergence rule.

## Profile Authority

- Run id: `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`.
- W0 Criterion root: `/tmp/skv11-open-criterion-3ce75df`.
- P1 capture root: `/tmp/skv11-p1`.
- Profile binary source SHA: `3ce75df4`; documentation/results freeze SHA:
  `9c8da194`.
- Host/toolchain: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`;
  `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`; `RUSTFLAGS="-C target-cpu=native"`.

## Accepted Findings

- Current W0 surface remains overall `N-direct / NoGo`: `parse_only` is
  diagnostic only at 16 `S / NO-GO` plus `canada` as `L / NO-GO`;
  `direct_to_struct` is 4 `A / GO` plus 13 `N-direct / NO-GO`; and
  `real_typed_struct` is 7 `A / GO`.
- Direct residuals are the primary S-P2/S-P3 closure surface, but
  `instruments`, `numbers`, and `unicode_mixed` remain W0-clamped
  non-admissions until a behavior wave measures them.
- The hot families accepted as research antecedents are
  `bounded_plain_string_scan`, `string_escape_decode`,
  `unicode_escape_hex_decode`, `number_digit_span`,
  `ascii_whitespace_skip`, `container_dispatch`, `simd_movemask`, and
  `output_digest_hash`.
- PMU/cycles, structural scan, masking probes, parse-only facts, samply
  sidecar symbol maps, lazy-tape facts, and W0-clamped throughput are
  diagnostic planning evidence only. They do not admit rows.
- The SK-V9 W3 union/substrate family, sidecar/cursor variants, PMULL/CTZ
  default rewires, rejected string/materialization families, generic numeric
  fallback, and object/value-byte carry routes remain REDRESS-pre-blocked as
  documented in the P1-E matrix.
- JSON-only profile telemetry may nominate primitive families for S-P2, but it
  does not prove CSS L4, Sheets, or BBNF-self behavior. SK-V11 still requires
  a measured non-JSON generated direct or typed parser intervention.

## Advancement

S-P1 is closed. S-P2 may research interventions from this accepted profile
surface, subject to micro-prove-first, strict-vs-strict comparators, non-JSON
exercise, and the closed parse-only / retired W3 constraints carried by the
SK-V11 opening contract.
