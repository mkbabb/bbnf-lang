# SK-V13 S-P2 V2 CH6: Anti-Paper-Close

## Verdict

ACCEPT.

## Evidence

- The CH6 bar is explicit: comparator claims need source anchors, ISA
  claims need manual or intrinsic sections, primitive claims need scalar
  sketches, and future-wave deferral is a paper-close
  (`restart/prompts/skinny/PASS-2-RESEARCH.md:133`-`:138`). V2 now
  leaves the candidate pool in an orchestrator-citable shape rather than
  relying on "researched" self-report.
- The V1 dav1d blocker is resolved. P2-B's title and scope now name the
  authority as FFmpeg / VideoLAN checkasm, with dav1d bounded to
  project-level lineage context only (`p2b-dav1d-process.md:1`-`:6`).
  Its finding says no dav1d-specific implementation or gate text may be
  copied into S-P3 without exact dav1d source-file anchors
  (`p2b-dav1d-process.md:18`-`:25`), and the risk/source sections repeat
  that S-P3 may cite FFmpeg/VideoLAN but not dav1d file-level mechanics
  (`p2b-dav1d-process.md:143`-`:148`, `:181`-`:186`). The cited primary
  sources support the narrowed claim: FFmpeg `checkasm.c` identifies
  checkasm as an assembly testing/benchmarking tool, FFmpeg `checkasm.h`
  exposes the checkasm call/check helpers, and the VideoLAN checkasm page
  shows reference-call, optimized-call, compare, then benchmark flow.
- P2-B also supplies a non-paper admission process. Every S-P3 candidate
  must have a P1 antecedent, primitive contract, executable scalar
  reference, differential checkasm, microbench, same-wave consumer,
  grammar policy, and strict row gate (`p2b-dav1d-process.md:37`-`:48`).
  B1-B5 then name scalar-reference status, checkasm expectations, and
  same-wave consumers or orphan rejection boundaries
  (`p2b-dav1d-process.md:50`-`:118`).
- The Arm citation blocker is resolved tightly enough for S-P3 gate text.
  P2-C now names concrete ACLE feature macros and Neon intrinsic entries
  for CSSC CTZ, PMULL, UDOT, TBL/TBX, EXT/shift, and EOR3/SHA3 in the
  findings (`p2c-arch-esoterica.md:16`-`:24`) and in the V2 citation
  tightening block (`p2c-arch-esoterica.md:75`-`:93`). The primary Arm
  sources carry those exact anchors: `__ARM_FEATURE_CSSC`,
  `__ARM_FEATURE_DOTPROD`, `__ARM_FEATURE_SHA3`, `vmull_p64`,
  `vmull_high_p64`, `vdotq_u32`, `vqtbl4q_u8`, `vqtbx4q_u8`,
  `vextq_u8`, `vshlq_n_u16`, and `veor3q_u8`.
- Inventory/drop decisions are now binding rather than shortlist
  authority. P2-C stamps rows as `NOT-S-P3-ELIGIBLE` unless a later fold
  adds a named S-P1 hot expression, scalar reference, checkasm/parity, and
  same-wave row consumer (`p2c-arch-esoterica.md:30`-`:33`), then applies
  that to EOR3 and standalone `byte_context` (`p2c-arch-esoterica.md:42`
  -`:43`). P2-F preserves the same exclusion list for EOR3/BCAX, cache
  hints, standalone prefix/next/bulk bitmap primitives, standalone
  `byte_context`, LD4/TBX/SMIN/SMAX refinements, and standalone D1 lazy
  capacity (`p2f-grammar-neutral.md:146`-`:161`, `:238`-`:243`).
- P2-A is still citable and not overclaiming comparator authority. It
  keeps sonic-rs strict as the binding JSON admission comparator while
  treating simdjson, yyjson, and asmjson as architecture pressure until
  same-run same-plane sidecars exist (`p2a-sota-teardown.md:42`-`:52`,
  `:140`-`:143`). Its C1-C8 table names shape, scalar status, checkasm
  expectation, P1 antecedent, and same-wave consumer/reject boundary for
  every comparator-derived primitive (`p2a-sota-teardown.md:72`-`:81`).
- P2-D removes the substrate paper-close. Its load-bearing conclusion is
  that admissible substrate work changes how generated code emits or
  consumes the single tape/sink event projection, with same-wave consumer
  and row-moving gate, rather than retaining scanner output
  (`p2d-substrate-tape.md:125`-`:137`). D1-D5 each carries scalar
  reference, parity/checkasm expectation, and same-wave consumer language
  (`p2d-substrate-tape.md:146`-`:185`, `:187`-`:227`, `:229`-`:276`,
  `:278`-`:314`, `:316`-`:351`), and P2-D excludes retained
  `StructuralIndex`, class columns, streaming cursors, aux density tables,
  whitespace bitmaps, and `UnionTape` (`p2d-substrate-tape.md:353`-`:363`).
- P2-E's parse-that gaps are grounded enough for CH6. P2E-1 through
  P2E-8 each names a shape, scalar reference sketch, checkasm or unit
  parity expectation, architecture status, P1 antecedent, and same-wave
  consumer note (`p2e-parse-that-gaps.md:35`-`:121`). It explicitly warns
  that crate extraction without resolver/codegen consumption is
  support-only and should fail CH6 (`p2e-parse-that-gaps.md:101`-`:110`).
- P2-F completed the V2 cross-read and maps sibling P2-A/B/C/D/E
  candidates into one Lock-14 verdict table (`p2f-grammar-neutral.md:12`
  -`:18`, `:63`-`:115`). The grammar-neutral table carries scalar
  reference, checkasm/parity, and consumer expectations for selectable or
  conditional rows, while inventory-only rows are fenced from S-P3
  shortlist use (`p2f-grammar-neutral.md:84`-`:93`, `:146`-`:161`).

## Blockers / Fold Requirements

None for CH6.

## Disposition

V2 removed the paper-close risks identified by V1 CH6. S-P3 may use the
V2 P2 packet as research authority only under the documented candidate
boundaries: no dav1d source-file gate authority, no broad ISA citations
without named feature/intrinsic anchors, no inventory/drop rows as
shortlist authority, and no SIMD/ASM or substrate candidate without
scalar reference, parity/checkasm expectation, and same-wave consumer.
