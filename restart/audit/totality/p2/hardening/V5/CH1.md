# T-P2 V5 CH1 Source Grounding / Provenance

Verdict: ACCEPT

Scope: unchanged-packet confirmation of the V4 T-P2 research packet under CH1.
This check re-ran source-count arithmetic, 2E counted-ID registry alignment,
moving-source pin/source-date coverage, and inherited support-only handling.

## Evidence

- CH1 requires cited papers, source paths, path lines, and benchmark provenance
  to be real; provenance gaps or confabulated citations are challenge failures
  (`restart/prompts/totality/PASS-2-RESEARCH.md:100-104`). V5 is explicitly an
  unchanged-packet confirmation because V4 consolidated accepted the challenge
  and carried forward one more confirmation cycle
  (`restart/audit/totality/p2/hardening/HARDENING-T-P2-V4-CONSOLIDATED.md:31-37`).
- The V4 2E repair holds. `counted_source_ids` in 2E lists exactly 11 IDs:
  `SRC-A64-ACLE`, `SRC-A64-NEON`, `SRC-A64-SVE2-MATCH`, `SRC-X86-INTEL`,
  `SRC-SCOPE`, `SRC-REDRESS`, `SRC-BBNF-A64`, `SRC-BBNF-DISPATCH`,
  `SRC-BBNF-CHECKASM`, `SRC-BBNF-X86`, and `SRC-V2-FOLD`
  (`restart/audit/totality/p2/2E-host-arch-esoterica.md:7-8`). The 2E Source
  Registry defines the same 11 IDs and no standalone `SRC-FFMPEG` or
  `SRC-DAV1D` rows (`restart/audit/totality/p2/2E-host-arch-esoterica.md:45-65`).
- Count arithmetic is reproducible for all dossiers: 2A `15 = 15`, 2B
  `24 = 24`, 2C `7 = 7`, 2D `11 = 11`, 2E `11 = 11`, and 2F `21 = 21` by
  comparing `primary_sources_cited` with `counted_source_ids`
  (`restart/audit/totality/p2/2A-sota-landscape.md:7-8`,
  `restart/audit/totality/p2/2B-primitive-vocabulary.md:7-8`,
  `restart/audit/totality/p2/2C-grammar-neutrality.md:7-8`,
  `restart/audit/totality/p2/2D-cost-model.md:10-11`,
  `restart/audit/totality/p2/2E-host-arch-esoterica.md:7-8`,
  `restart/audit/totality/p2/2F-parse-that-gaps.md:7-8`).
- Moving-source authority remains pinned or source-dated. V2 pins simdjson,
  sonic-rs, yyjson, FFmpeg, dav1d, egg, OR-Tools, RE2, Rust regex,
  fast_float, and conditional parse-that authority, while demoting Sneller
  repository authority (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:19-36`).
  V3 adds source-dated memchr and xxHash pins
  (`restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:22-35`). The current
  packet cites those pinned HEADs/URLs where the moving sources are used.
- Support-only inherited rows stay fenced. V4 says FFmpeg, dav1d, simdjson,
  sonic-rs, yyjson, egg, OR-Tools, RE2, Rust regex, fast_float, Sneller, and
  parse-that remain inherited support under `SRC-V2-FOLD` unless a dossier
  registers and counts them locally
  (`restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:35-39`). 2E repeats the
  same rule for FFmpeg/dav1d and counts only `SRC-V2-FOLD` for that inherited
  provenance (`restart/audit/totality/p2/2E-host-arch-esoterica.md:47-51`,
  `restart/audit/totality/p2/2E-host-arch-esoterica.md:65`).

## Required Repairs

None. The V4 packet passes CH1 provenance confirmation as unchanged research
authority; no new research edits are requested.
