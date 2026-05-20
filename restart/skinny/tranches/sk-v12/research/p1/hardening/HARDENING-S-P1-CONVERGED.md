# SK-V12 S-P1 Hardening Converged

Pass: S-P1 Profile.
Date: 2026-05-20.
Status: CONVERGED UNDER USER PIN.

## Basis

The original SK-V12 S-P1 convergence predated
`USER-PIN-W1-CSS-L4-SOTA.md` and is superseded. The pin-aware S-P1 profile
rerun is the live authority surface for S-P2 and later passes.

| Cycle | CH1 | CH2 | CH3 | CH4 | CH5 | CH6 | Disposition |
|---|---|---|---|---|---|---|---|
| PIN-V1 | REVISE | ACCEPT | ACCEPT | REVISE | REVISE | REVISE | Folded stale authority, export semantics, Track 1/Track 2 aggregation, generated-size/O(N) routing, and pre-pin citations. |
| PIN-V2 | REVISE | ACCEPT | ACCEPT | REVISE | ACCEPT | ACCEPT | Folded malformed samply replay modes, stdout-backed `rc=54` wording, and capture-source/current-head wording. |
| PIN-V3 | REVISE | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | Folded the two PMU parse `update_center` replay corpus keys and added a corpus-key sanity check. |
| PIN-V4 | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | First all-ACCEPT cycle, later reset by PIN-V5. |
| PIN-V5 | ACCEPT | ACCEPT | ACCEPT | ACCEPT | REVISE | ACCEPT | Folded stale pre-pin convergence and SPEC profile authority paths. |
| PIN-V6 | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | First all-ACCEPT cycle after the PIN-V5 reset. |
| PIN-V7 | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | Second consecutive all-ACCEPT cycle after the PIN-V5 reset. |

PIN-V6 and PIN-V7 are two consecutive six-of-six ACCEPT cycles with zero open
critical defects and no unresolved REVISE. This satisfies
`restart/prompts/ORCHESTRATOR.md` §3Z for S-P1.

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

## Accepted Findings

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

S-P1 is closed. S-P2 may research interventions from this accepted profile
surface, subject to the SK-V12 handoff: CSS L4 first, strict-vs-strict
comparator discipline, scalar-reference/checkasm process, grammar-neutral
abstraction, micro-prove-first for SIMD/substrate candidates, and the user-pin
union plus ASM-gen reopen rules.
