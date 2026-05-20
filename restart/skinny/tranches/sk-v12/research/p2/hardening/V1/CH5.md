# SK-V12 S-P2 CHALLENGE V1 — CH5 Hidden Coupling

Disposition: ACCEPT.

## Scope

Lens: hidden coupling. I audited the six S-P2 research artifacts for
parallel substrates, sidecar producers, renamed scanners, Track 1 == Track 2
dishonesty, retained cursors, aux tables, and parser-owned structural
projection. Read set: `restart/prompts/skinny/PASS-2-RESEARCH.md` §3/§8,
`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md`,
`p2b-dav1d-process.md`, `p2c-arch-esoterica.md`,
`p2d-substrate-tape.md`, `p2e-parse-that-gaps.md`,
`p2f-grammar-neutral.md`, `restart/locks/LOCKS.md`,
`skinny/REDRESS.md`, and the live skinny runtime tape/generated files.

## Findings

1. P2-D correctly preserves Lock 1's single-substrate rule. The live runtime
   evidence agrees with the artifact: `ParserState` owns one `TapeBuilder`,
   parse finishes into one `JsonRoot`, and `attach_structural_index` is a
   no-op rather than a retained scanner output
   (`skinny/crates/runtime/src/grammars/json/parser.rs:7`,
   `skinny/crates/runtime/src/grammars/json/parser.rs:47`,
   `skinny/crates/runtime/src/grammars/json/generated.rs:14`).
   The sealed `Tape` stores source, one offset vector, sparse flags, payloads,
   and an id, and `ValueRef` is `&Tape + cursor`
   (`skinny/crates/runtime/src/tape/mod.rs:94`,
   `skinny/crates/runtime/src/tape/mod.rs:175`). `JsonNodeKind::at_cursor`
   derives kind from `source[offsets[cursor]]`, not a hidden class column
   (`skinny/crates/runtime/src/grammars/json/value.rs:28`).

2. The rejected coupling shapes are not deferred as candidates. P2-D names
   `structural_class_lane_union` only to reject it: class columns,
   structural-position vectors, streaming structural cursors, `UnionTape`,
   parser-owned projections, and event sidecars are ruled out
   (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:53`,
   `restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:78`).
   That matches REDRESS 50/51/53 and REDRESS 96/97/98, which rejected aux side
   tables, event cursors, parser-local structural cursors, and W3 union
   substrate variants despite correctness-green implementations
   (`skinny/REDRESS.md:715`, `skinny/REDRESS.md:742`,
   `skinny/REDRESS.md:784`, `skinny/REDRESS.md:2795`,
   `skinny/REDRESS.md:2850`, `skinny/REDRESS.md:2908`).

3. The classifier/SIMD candidates keep masks transient. P2-A's
   `class_mask64_transient` is explicitly short-lived and not a retained
   sidecar (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:29`).
   P2-B rejects retained class columns, parser-owned sidecars, second scans,
   retained cursors, side tables, and retained vectors in the relevant gates
   (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:38`,
   `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:40`,
   `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:45`,
   `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:46`).
   P2-C's risk section repeats the same sidecar prohibition for TBL/LD4/run-skip
   and mask emit shapes
   (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:106`,
   `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:107`,
   `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:112`,
   `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:118`).

4. The parse-that candidates expose cursor-returning scalar mechanics but do
   not retain cursors or create structural projection. P2-E's byte-set skip
   returns a local end offset and explicitly retains no mask, cursor sidecar, or
   structural index; the artifact later generalizes this to all candidates as
   no parallel structural stream, decoded-byte cache, retained cursor list, or
   grammar-specific sidecar
   (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:58`,
   `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:372`).
   Its escaped-segment visitor is allocation-neutral and forbids decoded
   sidecars/output-statistics lanes as REDRESS reopenings
   (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:305`,
   `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:358`).

5. Track 1 and Track 2 remain distinct. P2-A cites the current RESULTS
   distinction between generated Track 1 and independent Track 2 evidence and
   warns that historical C++ sidecars are not strict anchors
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:97`).
   P2-B classifies `OUTPUT_DIGEST_HASH_ORACLE_GATE` as report/output equality,
   not a parser primitive or typed-proof substitute
   (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:49`).
   P2-F likewise makes F7 oracle-only and forbids using digest as typed/direct
   proof (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:35`,
   `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:58`).

6. Two coupling-adjacent items are acceptable only because the artifacts bind
   them down. P2-D's `retained_cursor_skip_projection` is diagnostic/flagged,
   not selectable from SK-V12 P1, and must stay same-tape if retained traversal
   ever becomes hot (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:77`,
   `restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:80`).
   P2-C's `a64_ld4_interleaved_classifier64x4` is admissible only when an
   already-canonical stream is naturally interleaved; it is not neutral if it
   creates a second scan stream or retained deinterleaved sidecar
   (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:48`,
   `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:107`).

## Revise List

None for CH5. Carry forward these ACCEPT guards into S-P3: do not shortlist
`retained_cursor_skip_projection` from current P1 evidence; do not treat LD4
deinterleave as a new stream; do not convert transient masks or emitted
positions into retained indexes; do not use output digest/oracle plumbing as
Track 1 == Track 2 proof.
