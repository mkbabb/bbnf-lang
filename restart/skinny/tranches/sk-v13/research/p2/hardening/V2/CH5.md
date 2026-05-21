# SK-V13 S-P2 V2 CH5: Hidden Coupling / Lock 1

Verdict: ACCEPT.

## Evidence

- The CH5 test rejects any candidate that introduces a parallel substrate,
  sidecar producer, renamed scanner, Track 1 / Track 2 dishonesty, second
  source scan, retained cursor, aux density table, or parser-owned structural
  projection (`restart/prompts/skinny/PASS-2-RESEARCH.md:126`-`:131`).
  Lock 1 sets the same line: tape/direct union is the substrate; transient SIMD
  masks are allowed, but retained structural offsets are the tape projection,
  not a sidecar (`restart/locks/LOCKS.md:52`).

- V2's P2-D reaches the required substrate conclusion. The current retained
  surface is one lazy offset tape: `ValueRef` is a cursor into the single
  `Tape`, and `TapeBuilder` writes offsets into that tape before sealing
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:19`-`:33`).
  Direct-to-struct is legal only as `SinkOnly`, with no retained second
  document (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:87`-`:105`).

- V2 does not allow sidecar structural vectors. P2-D says structural SIMD is a
  scanner micro-signal, but REDRESS 96/97 falsified retaining it as a side
  structure; the packet explicitly refuses a retained SIMD-position vector,
  streaming cursor, class column, or parser-owned structural cursor
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:62`-`:87`).
  P2-A's C6 has the same fence: masks may be consumed into the existing offset
  tape, direct sink, or CSS fact sink, and retained class-column, streaming
  cursor, or class-lane-only variants are rejected
  (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:79`).

- V2 avoids parser-owned cursors/lists and aux density tables. P2-D's load-
  bearing conclusion keeps `Tape`, `ValueRef`, and `TapeBuilder` stable, with
  no `UnionTape`, sidecar vectors, parser-owned cursor/list, new directive, new
  BIR variant, or new `BackendShape`
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:125`-`:137`).
  Its non-candidate list rejects retained `StructuralIndex`, class column,
  streaming cursor, aux density table, whitespace bitmap, and `UnionTape`
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:353`-`:359`).

- V2 avoids new public substrate APIs. D2's same-tape event projection is
  codegen-private and must not introduce a public `UnionTape`
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:187`-`:223`).
  P2-D's grammar-neutral table repeats the same gate: no new directive, BIR,
  `BackendShape`, or public substrate API
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:367`-`:372`).
  P2-F generalizes that across the V2 packet: union variants are legal only as
  codegen-private tape/fact routing over one substrate; a retained side vector,
  parser-owned cursor/list, or public substrate API violates Lock 1 and Lock 14
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:142`-`:144`).

- V2 keeps structural SIMD and parse-that candidates transient unless consumed
  by the one substrate. P2-B B3 permits `scan_structurals` / `scan_tail` only as
  transient mask/position input and forbids new sidecar vectors, parser-owned
  cursors, or parallel `UnionTape`
  (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:78`-`:90`).
  P2-C C-P2C-2 is Lock-1 legal only if the emitted projection is the tape/union
  itself, not a parallel sidecar
  (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:38`,
  `:50`). P2-E P2E-8 receives transient block masks or emitted offsets and must
  not retain a sidecar
  (`restart/skinny/tranches/sk-v13/research/p2/p2e-parse-that-gaps.md:112`-`:121`).

- V2 does not use Track 2 as substrate proof. P2-D excludes Track 2 parser
  rewrites as substrate proof and says Track 2 is only an independent oracle
  unless Track 1 substrate evidence exists
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:361`-`:363`).
  D4's equality requirement keeps independent Track 2, serde_json, sonic-rs,
  lightningcss, and CSS oracle lanes as comparison evidence, not as replacement
  substrate authority (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:293`-`:305`).

- V2 preserves V1's accepted CH5 fold. The V1 consolidated packet required V2
  to preserve inventory/drop decisions so speculative support primitives could
  not re-enter as shortlist authority, while CH5's accepted limit carried
  forward no side substrates, no retained cursors, no aux density tables, no
  public substrate APIs, and no Track-2-as-substrate proofs
  (`restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md:31`-`:56`).
  The V2 p2a..p2f artifacts satisfy that preservation.

## Blockers

None for CH5 V2. The packet still contains high-risk union candidates, but they
are gated as same-substrate or sink-only routes and the prohibited side
substrates are explicitly rejected.

## Fold Requirements

- S-P3 may shortlist D2/D3, B3, C-P2C-2, P2E-8, or Union C1/C2/C3 only with an
  explicit Lock 1 gate: one tape or sink-only projection, no `UnionTape`, no
  retained structural side vector, no parser-owned cursor/list, no aux density
  table, no public substrate API, no second source scan, and same-wave measured
  row consumer.

- Track 2, serde_json, sonic-rs, lightningcss, cssparser, and other oracle lanes
  remain independent correctness/comparator planes. They cannot substitute for
  Track 1 substrate evidence or row movement.

- Support-only SIMD helpers, resolver scaffolding, capacity policy, or
  tape-shape work may fold only as part of a measured consumer wave, deletion /
  demotion evidence, or explicit measurement question. They cannot land as
  substrate-first infrastructure.

## Disposition

CH5 accepts S-P2 V2 for Lock 1. V2 avoids sidecar substrates, retained
structural side vectors, parser-owned cursors/lists, aux density tables, new
public substrate APIs, and Track2-as-substrate proof. Its dangerous candidates
remain conditional same-tape or sink-only routes with same-wave consumer gates,
so there is no CH5 blocker to convergence.
