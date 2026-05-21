# T-P2 V4 CH1 Source Grounding / Provenance

Verdict: ACCEPT

Scope: CH1 source grounding/provenance only. This review checks source-count
arithmetic, 2E source-registry alignment, generic moving-source pinning, and
whether inherited support rows are excluded from counted evidence.

## Evidence

- CH1 authority is provenance-focused: every cited paper/source/pathline must be
  real and provenance gaps are challenge failures
  (`restart/prompts/totality/PASS-2-RESEARCH.md:100-104`).
- V3's open CH1 defect was precise: 2E counted `SRC-INTEL-X86`,
  `SRC-FFMPEG`, and `SRC-DAV1D` while its registry used `SRC-X86-INTEL` and had
  no FFmpeg/dav1d rows
  (`restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:24-27`).
- V4 repairs that defect. The V4 addendum says 2E's counted evidence is exactly
  its local registry IDs, and that inherited FFmpeg/dav1d pins remain available
  only through `SRC-V2-FOLD`
  (`restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:24-39`). 2E frontmatter
  now lists `SRC-X86-INTEL`, `SRC-REDRESS`, and `SRC-BBNF-A64`, with no
  standalone `SRC-FFMPEG` or `SRC-DAV1D`
  (`restart/audit/totality/p2/2E-host-arch-esoterica.md:7-8`), and the Source
  Registry defines exactly those 11 IDs
  (`restart/audit/totality/p2/2E-host-arch-esoterica.md:45-65`).
- `primary_sources_cited` equals the `counted_source_ids` length for every
  dossier:

| dossier | count evidence |
|---|---|
| 2A | 15 sources, 15 counted IDs (`restart/audit/totality/p2/2A-sota-landscape.md:7-8`) |
| 2B | 24 sources, 24 counted IDs (`restart/audit/totality/p2/2B-primitive-vocabulary.md:7-8`) |
| 2C | 7 sources, 7 counted IDs (`restart/audit/totality/p2/2C-grammar-neutrality.md:7-8`) |
| 2D | 11 sources, 11 counted IDs (`restart/audit/totality/p2/2D-cost-model.md:10-11`) |
| 2E | 11 sources, 11 counted IDs (`restart/audit/totality/p2/2E-host-arch-esoterica.md:7-8`) |
| 2F | 21 sources, 21 counted IDs (`restart/audit/totality/p2/2F-parse-that-gaps.md:7-8`) |

- Generic moving-source roots are pinned or source-dated. V2 pins simdjson,
  sonic-rs, yyjson, FFmpeg, dav1d, egg, OR-Tools, RE2, Rust regex, fast_float,
  and parse-that, and demotes Sneller repository authority
  (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:19-36`). V3 adds
  source-dated pins for memchr and xxHash
  (`restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:22-35`). The current
  dossiers cite pinned source URLs/HEADs for the moving GitHub sources they use
  (`restart/audit/totality/p2/2A-sota-landscape.md:156-199`,
  `restart/audit/totality/p2/2D-cost-model.md:206`,
  `restart/audit/totality/p2/2F-parse-that-gaps.md:73-82`).
- Support-only inherited rows are clearly not counted. V4 states FFmpeg, dav1d,
  simdjson, sonic-rs, yyjson, egg, OR-Tools, RE2, Rust regex, fast_float,
  Sneller, and parse-that remain inherited support under `SRC-V2-FOLD` unless a
  dossier registers and counts them locally
  (`restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:35-39`). 2E repeats the
  rule for FFmpeg/dav1d and counts only `SRC-V2-FOLD` for inherited provenance
  (`restart/audit/totality/p2/2E-host-arch-esoterica.md:47-51`,
  `restart/audit/totality/p2/2E-host-arch-esoterica.md:65`).

## Required Repairs

None for CH1 provenance. V4 is acceptable on the requested source grounding
checks.
