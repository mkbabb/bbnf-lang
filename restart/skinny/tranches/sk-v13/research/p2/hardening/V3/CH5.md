# SK-V13 S-P2 V3 CH5: Hidden Coupling / Lock 1

Verdict: ACCEPT.

## Evidence

- The CH5 lens rejects a parallel substrate, sidecar producer, renamed scanner,
  Track 1 / Track 2 dishonesty, second source scan, retained cursor, auxiliary
  density table, or parser-owned structural projection
  (`restart/prompts/skinny/PASS-2-RESEARCH.md:126`-`:131`). Lock 1 sets the same
  boundary: tape/direct is the substrate union, `SinkOnly` has no retained
  queryable document, and retained structural offsets are the tape projection
  itself rather than a sidecar (`restart/locks/LOCKS.md:52`).

- V3 preserves the V2 CH5 substrate conclusion. P2-D still identifies the live
  retained surface as one lazy offset tape: `ValueRef` is a cursor into the
  single `Tape`, and `TapeBuilder` writes offsets before sealing
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:19`-`:35`).
  Direct-to-struct remains legal only as `SinkOnly`, not as a second retained
  document (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:87`-`:105`).

- Structural SIMD remains a transient producer, not a runtime substrate. P2-D
  says the structural scan signal does not reopen a retained SIMD-position
  vector, streaming cursor, class column, or parser-owned structural cursor
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:62`-`:85`).
  P2-A C6 keeps masks transient and permits retention only when the projection is
  the tape, rejecting class-column, streaming-cursor, and class-lane-only
  variants (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:79`).
  P2-B B3/B4 repeats that structural positions and bitmaps are legal only with a
  same-wave consumer and no sidecar vector, parser-owned cursor, or parallel
  `UnionTape` (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:80`-`:90`,
  `:96`-`:104`).

- V3 keeps CSS comparator and oracle lanes from becoming substrate authority.
  P2-F reclassifies CSS stylesheet/selectors, declaration-value extension,
  visual functions, at-rules/media, nesting, and vendor/custom at-rules as
  `CSS-ROW-SCOPE-CONDITIONAL`: generated row/fact-stream scopes requiring fresh
  narrow CSS parser profiling or same-wave strict row movement, not primitive
  hot-leaf admissions (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:44`-`:47`,
  `:69`-`:74`, `:123`-`:132`, `:238`-`:245`). The lightningcss/cssparser
  references are equality/comparator oracles for those rows, not alternate
  runtime substrates (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:123`-`:126`,
  `:158`-`:161`).

- CSS fact streams are framed as generated row outputs over the same substrate
  boundary, not sidecar storage. P2-D D2 allows same-tape event projection only
  as codegen-private offset/fact/sink routing with no public `UnionTape` and one
  sealed `Tape` for retained output (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:187`-`:223`).
  D4 makes CSS fact streams a sink-only output over the same generated event
  vocabulary, with parity against lightningcss and an independent CSS oracle
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:278`-`:310`).
  P2-F admits fact-stream digest only if it hashes grammar-neutral fact streams,
  not `JsonDigestSink` internals or a parser-speed shortcut
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:97`,
  `:119`).

- V3 avoids public substrate APIs and sidecar abstractions. P2-D's legal
  structural projection union must keep `Tape`, `ValueRef`, and `TapeBuilder`
  stable and avoid `UnionTape`, sidecar vectors, parser-owned cursor/list, new
  directive, new BIR variant, or new `BackendShape`
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:125`-`:137`).
  Its grammar-neutral table repeats that D2 has no new directive/BIR/
  `BackendShape` or public substrate API, and that D4's generated sink must not
  become a public generic-crate arm
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:367`-`:373`).
  P2-F likewise rejects public `GrammarConfig` as an unapproved V1 surface and
  requires generated `pub(crate)` per-grammar modules
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:193`-`:197`).

- Track 2 and external comparators remain evidence planes only. P2-D rejects
  Track 2 parser rewrites as substrate proof and says Track 2 cannot justify
  Track 1 substrate claims without Track 1 row movement
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:361`-`:363`).
  P2-A keeps simdjson, yyjson, asmjson, and absent/historical C++ sidecars as
  architecture pressure unless same-run same-plane sidecars are produced
  (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:42`-`:52`,
  `:140`-`:143`).

## Blockers

None for CH5 V3. The V3 CSS fold does not introduce a new substrate. It narrows
CSS work to row-production and equality/comparator evidence, while preserving
the one tape or sink-only substrate boundary accepted in V2.

## Fold Requirements

- S-P3 may shortlist D2/D3/D4, B3/B4, C-P2C-2, P2E-8, C6, or Union C1/C2/C3
  only with an explicit Lock 1 gate: one retained tape or sink-only projection,
  no `UnionTape`, no retained structural side vector, no parser-owned
  cursor/list, no aux density table, no public substrate API, no second source
  scan, and same-wave measured row consumer.

- CSS stylesheet/selectors, declaration-value extension, visual functions,
  at-rules/media, nesting, and vendor/custom at-rule scopes must remain
  `CSS-ROW-SCOPE-CONDITIONAL`: row-production scopes with fresh narrow CSS parser
  profiling or same-wave strict lightningcss/cssparser row movement. Their
  comparator/oracle lanes cannot become runtime substrate lanes.

- Track 2, serde_json, sonic-rs, simdjson, yyjson, asmjson, lightningcss,
  cssparser, and other oracle lanes remain independent correctness/comparator
  planes. They cannot substitute for Track 1 substrate evidence or row movement.

- Support-only SIMD helpers, resolver scaffolding, capacity policy, tape-shape
  work, and fact-stream hashing may fold only with scalar/parity evidence and a
  measured consumer, or as deletion/demotion evidence. They cannot land as
  substrate-first infrastructure.

## Disposition

CH5 accepts S-P2 V3. V3 preserves the Lock 1 substrate union: retained output is
one tape, direct output is sink-only, structural SIMD is transient unless it is
the tape projection, CSS fact/comparator lanes are row-output and oracle lanes
rather than runtime substrates, and no sidecar substrate or public substrate API
is introduced.
