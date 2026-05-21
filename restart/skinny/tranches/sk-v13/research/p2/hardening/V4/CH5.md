# SK-V13 S-P2 V4 CH5: Hidden Coupling / Lock 1

Verdict: ACCEPT.

## Evidence

- V4 confirms the unchanged V3 acceptance. V3 explicitly leaves S-P2 at
  `ACCEPT-PENDING-CONFIRMATION`, with CH5 accepted because CSS comparator/oracle
  lanes are not runtime substrate and the single-substrate/no-sidecar boundary
  remains unchanged
  (`restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md:10`-`:17`,
  `:25`-`:29`).

- The retained substrate remains one lazy offset tape. P2-D names `ValueRef` as
  a cursor into the single `Tape`, with `TapeBuilder` writing offsets before
  sealing; capacity/tape-shape evidence does not justify a second structural
  list or eager payload materialization
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:19`-`:35`,
  `:53`-`:60`).

- Direct output remains legal only as `SinkOnly`, not as a second retained
  document. P2-D requires generated sink-event or same-tape/sink-only projection
  changes for direct rows, not retained tape walks or a new direct struct tree
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:87`-`:105`).

- Structural SIMD is fenced as transient producer evidence. P2-D says the scan
  signal does not reopen a retained SIMD-position vector, streaming cursor,
  class column, or parser-owned structural cursor, and a legal structural
  projection union is the tape itself, not a sidecar
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:62`-`:85`,
  `:125`-`:137`).

- The union candidates preserve Lock 1 boundaries. D2 is codegen-private
  same-tape routing with no public `UnionTape`; D4 is a sink-only event adapter
  sharing generated event vocabulary without building a retained document
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:187`-`:223`,
  `:278`-`:310`).

- CSS comparator and oracle lanes remain row evidence, not runtime substrate.
  P2-F classifies CSS rows as `CSS-ROW-SCOPE-CONDITIONAL`: generated
  row/fact-stream scopes that require fresh narrow CSS parser profiling or
  same-wave strict lightningcss/cssparser row movement, not primitive
  admissions
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:44`-`:47`,
  `:69`-`:74`, `:123`-`:132`, `:238`-`:245`).

- No aux density table, parser-owned cursor/list, public substrate API, or
  comparator-as-substrate path is introduced. P2-D rejects Track 2 parser
  rewrites as substrate proof without Track 1 row movement, and its
  grammar-neutrality table keeps D2/D4 codegen-private with no new
  directive/BIR/`BackendShape`/public API
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:356`-`:363`,
  `:367`-`:373`).

## Blockers / Fold Requirements

None for CH5 V4.

Carry forward the V3 constraints unchanged: S-P3 may plan union, structural
scan, CSS fact-stream, sink, or SIMD-adjacent work only as one retained tape or
sink-only projection, with no `UnionTape`, no sidecar vector, no parser-owned
cursor/list, no aux density table, no public substrate API, no second source
scan, and no oracle/comparator lane promoted into runtime substrate.

## Disposition

CH5 confirms V3 acceptance. The unchanged S-P2 V3 packet preserves Lock 1:
retained output is one tape, direct output is sink-only, structural SIMD is
transient unless it writes the tape projection, and CSS fact/comparator lanes
remain generated row outputs and independent oracle evidence.
