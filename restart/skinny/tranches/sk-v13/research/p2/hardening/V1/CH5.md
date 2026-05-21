# SK-V13 S-P2 V1 CH5: Hidden Coupling / Lock 1

Verdict: ACCEPT.

## Evidence

- The S-P2 CH5 test is explicit: reject any candidate that introduces a
  parallel substrate, sidecar producer, renamed scanner, Track 1 / Track 2
  dishonesty, second source scan, retained cursor, aux density table, or
  parser-owned structural projection
  (`restart/prompts/skinny/PASS-2-RESEARCH.md:126`-`:131`). Lock 1 gives the
  same boundary: the tape/direct union is the substrate; SIMD masks may be
  transient producers, but retained structural offsets are the tape projection,
  not a sidecar (`restart/locks/LOCKS.md:52`).

- P2-D reaches the required substrate conclusion. It says a legal structural
  projection union is a same-tape projection, keeps `Tape`, `ValueRef`, and
  `TapeBuilder` stable, and forbids `UnionTape`, sidecar vectors,
  parser-owned cursors/lists, new directives, new BIR variants, and new
  `BackendShape` surfaces (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:125`-`:137`).
  Its non-candidate list explicitly rejects retained `StructuralIndex`,
  class-column, streaming-cursor, aux-density-table, whitespace-bitmap, and
  Track-2-as-substrate-proof routes (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:349`-`:359`).

- The union-adjacent candidates are high risk, but they are not hidden-coupled
  as written. P2-D D2 is codegen-time same-tape event projection with no
  retained class side vector or runtime cursor list
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:183`-`:223`).
  D3 writes mask-selected positions directly into the active `TapeBuilder`;
  if the projection is retained, it is the tape, and scanner microbench evidence
  is insufficient without a same-wave retained/CSS row consumer
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:225`-`:272`).

- The SIMD and parse-that candidates preserve the transient-mask boundary.
  P2-B B3 allows structural scan consumption only as a transient
  mask/position primitive and rejects retained sidecar vectors,
  parser-owned cursors, and parallel `UnionTape`
  (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:65`-`:77`).
  P2-C's PMULL/CSSC union candidate is general only if the emitted tuple schema
  is the tape/union itself, not a parallel sidecar
  (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:42`-`:58`).
  P2-E's `GrammarStructuralScan` likewise requires a consumer callback over
  transient masks or emitted offsets and says a retained `StructuralIndex`,
  parser-local cursor, or class side vector repeats rejected history
  (`restart/skinny/tranches/sk-v13/research/p2/p2e-parse-that-gaps.md:107`-`:117`).

- The cohort does not treat support primitives as admissible substrates.
  P2-A marks raw movemask packers, cache hints, orphan bitmap delegates,
  comparator harness adapters, and standalone egraph/CSP scaffolding as
  support-only unless attached to scalar reference, checkasm/parity, and a
  same-wave row consumer
  (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:83`-`:87`).
  P2-F repeats that standalone prefix-XOR, next-bit, bulk emit, `cache_hints`,
  EOR3, TBX/LD4/SMIN/SMAX, and JSON-specific wrappers are not eligible without
  a measured consumer or must be rejected as written
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:132`-`:145`).

- Track 1 / Track 2 independence is preserved. P2-A keeps simdjson, yyjson, and
  asmjson as architecture pressure unless S-P3 wires same-run same-plane
  sidecars (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:42`-`:52`,
  `:133`-`:136`). P2-D says Track 2 rewrites are oracle evidence, not Track 1
  substrate proof (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:357`-`:359`),
  and D4 requires strict same-plane equality across generated Track 1,
  independent Track 2, serde_json, sonic-rs, lightningcss, and CSS oracle lanes
  where applicable (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:293`-`:306`).

- The S-P1 packet remains non-admissive, so P2 does not smuggle profile sidecars
  into gate authority. S-P1 converged with every profile candidate still
  non-admission until S-P2/S-P3 selection and later redress measurement
  (`restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md:53`-`:61`).

## Fold Requirements

- S-P3 may shortlist D2/D3, C-P2C-2, P2E-8, or Union C1/C2/C3 only with an
  explicit Lock 1 gate: one tape or sink-only projection, no `UnionTape`, no
  retained structural side vector, no parser-owned cursor/list, no aux density
  table, no public substrate API, and same-wave measured row consumer.

- Any Track 2 or oracle path remains an independent correctness/comparator lane,
  never a substitute for Track 1 row movement or substrate evidence.

- Any support-only SIMD or resolver primitive without a same-wave row consumer
  must be cut, merged into its consuming wave, or recorded as measured REDRESS
  evidence; it cannot land as substrate-first infrastructure.

## Blockers

None for CH5 V1. The cohort is CH5-acceptable because the dangerous candidates
are fenced as conditional same-substrate routes and the explicit rejection
boundaries are already in the research packet.
