# SK-V12 S-P1 Hardening Status

Pass: S-P1 Profile.
Date: 2026-05-20.
Status: PRE-PIN CONVERGENCE SUPERSEDED; PIN CONVERGENCE IN PROGRESS.

## Basis

The original SK-V12 S-P1 hardening sequence converged before
`USER-PIN-W1-CSS-L4-SOTA.md`. That convergence is retained as historical
context only. It is not live S-P1 authority for the pinned campaign.

The pin-aware S-P1 profile rerun is the current authority surface. It has:

- initial profile fold `b1043383`;
- PIN-V1 fold `d4ef80b2`;
- PIN-V2 fold `9559a2c4`;
- PIN-V3 fold `1669c551`;
- PIN-V4 fold `ecda8b13`, the first all-ACCEPT pin cycle;
- PIN-V5 in review, with CH5 requiring this authority cleanup before a second
  all-ACCEPT cycle can count.

S-P1 remains in hardening until two consecutive pin cycles are all ACCEPT under
`restart/prompts/ORCHESTRATOR.md` §3Z. After that fold, this file may be
rewritten from status to final pin convergence.

## Pin Profile Authority

- Capture source commit: `cf7848b2` (`docs(sk-v12-alpha-hardening): converge
  pin-aware G-Alpha V4`).
- Initial committed S-P1 fold: `b1043383` (`docs(sk-v12-p1-profile): fold
  pin-aware profile capture`).
- Capture root: `/tmp/skv12-pin-p1`.
- Build root: `/tmp/skv12-pin-profile-target-cf7848b2`.
- Host/toolchain: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`;
  `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`;
  `RUSTFLAGS="-C target-cpu=native"`.
- Replay authority:
  `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv` with
  458 pin replay rows.
- Capture status authority:
  `/tmp/skv12-pin-p1/pmu/capture_status.tsv`,
  `/tmp/skv12-pin-p1/samply/capture_status.tsv`, and
  `/tmp/skv12-pin-p1/xctrace/capture_status.tsv`.
- Self-time authority:
  `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv` and
  `/tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv`, derived from exported
  xctrace Time Profiler XML.

## Accepted Pin Findings

- Current surface remains overall `N-direct / NoGo`: `parse_only` is
  diagnostic, `direct_to_struct` is a JSON guard and routed ledger, and
  `real_typed_struct` remains the JSON typed guard surface.
- CSS L4 is the authoritative first target. The pin profile root contains no
  generated CSS L4 Track 1 runtime, no same-plane lightningcss comparator row,
  and no strict equality oracle row; this absence is routed to S-P2/S-P3.
- Sheets and BBNF-self are fallback-only after a measured CSS L4 redress
  attempt, not preflight-equivalent substitutes.
- The accepted hot families for S-P2 antecedents are
  `bounded_plain_string_scan`, `container_dispatch`,
  `unicode_escape_hex_decode`, `number_digit_span`, `simd_movemask`,
  `string_escape_decode`, `output_digest_hash`, `ascii_whitespace_skip`,
  `typed_direct_projection`, and `serde_json_oracle_read_parse`.
- The pin replay TSV is the exact tracked command surface for PMU, samply,
  xctrace CPU Counter, primary Time Profiler, Time Profiler export, and
  product-v2 Time Profiler replay. The samply lane is retained
  artifact-only evidence; xctrace XML is the self-time authority.
- Mode III remains an absence boundary. The pin capture has no fresh Mode III
  call-stack rows and no fresh structural-scan-only xctrace lane.
- PMU aggregate values are parse `2.971206 c/B`, direct `4.411311 c/B`, and
  typed guard `3.137378 c/B`; these are profile evidence only and move no row.
- JSON-only profile telemetry may nominate primitive families for S-P2, but it
  does not prove CSS L4, Sheets, or BBNF-self behavior. SK-V12 still requires a
  measured generated CSS L4 baseline before behavior implementation waves can
  claim the user-pin admission target.

## Advancement

S-P1 is not closed by the superseded pre-pin convergence. Advancement to S-P2
requires the pin-aware hardening sequence to record two consecutive all-ACCEPT
cycles with zero open critical defects and no orphan unresolved REVISE.
