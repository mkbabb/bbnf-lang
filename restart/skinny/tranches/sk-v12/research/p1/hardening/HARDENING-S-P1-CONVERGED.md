# SK-V12 S-P1 Hardening Converged

Pass: S-P1 Profile.
Date: 2026-05-20.
Status: CONVERGED.

## Basis

S-P1 ran the full six-lens hardening sequence after the SK-V12-open baseline
and profile cohort:

| Cycle | CH1 | CH2 | CH3 | CH4 | CH5 | CH6 | Disposition |
|---|---|---|---|---|---|---|---|
| V1 | REVISE | REVISE | ACCEPT | REVISE | ACCEPT | REVISE | Folded fresh self-time, replay provenance, PMU arithmetic, Mode III absence, and Lock 14 wording defects. |
| V2 | REVISE | ACCEPT | ACCEPT | REVISE | ACCEPT | ACCEPT | Folded line-zero source anchors, exact replay enumeration, and samply artifact-only policy. |
| V3 | REVISE | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | Folded residual line-zero pseudo-symbols in the self-time TSV symbol fields. |
| V4 | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | First all-ACCEPT cycle. |
| V5 | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | Second consecutive all-ACCEPT cycle. |

V4 and V5 are two consecutive six-of-six ACCEPT cycles with zero critical or
open REVISE findings. This satisfies the S-P1 challenge convergence rule.

## Profile Authority

- Source baseline: `50bd1648` (`docs(sk-v12-g-alpha): present converged alpha
  contract`).
- Capture root: `/tmp/skv12-p1`.
- Target directory: `/tmp/skv12-profile-target-50bd1648`.
- Host/toolchain: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`;
  `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`; `RUSTFLAGS="-C target-cpu=native"`.
- Replay authority:
  `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv` with 506
  replay rows.
- Self-time authority:
  `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv` and
  `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv`, derived from exported
  xctrace Time Profiler XML.

## Accepted Findings

- Current surface remains overall `N-direct / NoGo`: `parse_only` is diagnostic
  at 16 `S / NO-GO` plus `canada` as `L / NO-GO`; `direct_to_struct` is 4
  `A / GO` plus 13 `N-direct / NO-GO`; `real_typed_struct` is 7 `A / GO`.
- SK-V12 must solve the generated non-JSON baseline priority before reopening
  JSON direct residual rows. REDRESS 119-120 remain binding fixpoint and route
  authority.
- The accepted hot families for S-P2 antecedents are
  `bounded_plain_string_scan`, `container_dispatch`,
  `unicode_escape_hex_decode`, `number_digit_span`, `simd_movemask`,
  `string_escape_decode`, `output_digest_hash`, `ascii_whitespace_skip`,
  `typed_direct_projection`, and `serde_json_oracle_read_parse`.
- The replay TSV is the exact command surface for PMU, samply, xctrace CPU
  Counter, primary Time Profiler, Time Profiler export, and product-v2
  Time Profiler replay. The samply lane is retained artifact-only evidence;
  xctrace XML is the self-time authority.
- Mode III remains an absence boundary. The SK-V12-open capture has no fresh
  Mode III call-stack rows and no fresh structural-scan-only xctrace lane.
- PMU aggregate values are parse `2.920217 c/B`, direct `4.290305 c/B`, and
  typed guard `3.123172 c/B`; these are profile evidence only and move no row.
- JSON-only profile telemetry may nominate primitive families for S-P2, but it
  does not prove CSS L4, Sheets, or BBNF-self behavior. SK-V12 still requires a
  measured generated non-JSON direct or typed parser baseline before behavior
  implementation waves.

## Advancement

S-P1 is closed. S-P2 may research interventions from this accepted profile
surface, subject to the SK-V12 handoff: generated non-JSON baseline first,
strict-vs-strict comparator discipline, scalar-reference/checkasm process,
grammar-neutral abstraction, and the closed parse-only / retired W3 constraints.
