# SK-V8 W2 Hardening V1 CH2

Verdict: REVISE

Confidence: 91%

Scope: reviewed commit `12aff1e4` at current HEAD, plus the live W2
research/plan, SPEC, DISPATCH, HANDOFF, RESULTS, and REDRESS surfaces needed to
judge typed product-plane admission. I ran:

- `cargo xtask check-real-typed`
- `cargo test -p bbnf-bench real_typed -- --nocapture`
- `cargo test -p codegen typed_direct -- --nocapture`
- `cargo xtask check-json`
- `cargo xtask check-conformance`
- `git diff --check`

All executed checks passed. During review, `restart/skinny/tranches/sk-v8/HANDOFF.md`
and `skinny/REDRESS.md` became dirty from concurrent external edits. I did not
edit them and do not count those edits as part of HEAD.

## Findings

1. Generated parser/schema consistency is acceptable. `12aff1e4` adds exactly
   the W2 typed roots to `skinny/xtask/src/real_typed_schema.rs`, regenerates
   `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, and wires the same
   roots through `RealTypedFixture`, `track1_typed`, serde, sonic, and checksum
   dispatch in `skinny/crates/bbnf-bench/src/real_typed_struct.rs`. The generated
   file is current under `cargo xtask check-real-typed`, and the commit does not
   touch runtime parser, tape, direct digest, scan, parity, or materialization
   code.

2. CITM map-entry semantics are acceptable for the admitted source slice. The
   schema models `events` as `map_entries(... CitmEventEntry<'i> ...)`; the
   generated parser reads object keys in stream order and pushes `{ key, value }`
   entries; serde/sonic use a custom `MapAccess` visitor that also pushes keyed
   entries into a `Vec`. The checksum folds event count, key, and selected event
   fields (`id`, `name`, `subTopicIds`, `topicIds`), so Track 1 cannot pass by
   silently dropping keys or flattening the map to values only. The full CITM
   fixture test is the right guard for the actual map shape.

3. The serde/sonic parity story needs wording or implementation correction.
   `track2_typed` delegates directly to `serde_typed`, and
   `assert_real_typed_parity` then calls `serde_typed` again as a separate lane.
   Sonic is an independent parser, and Track 1 remains independent from both,
   but Track 2 and serde are not independent of each other. This is not a source
   blocker if W2 intentionally uses serde as the Track 2/oracle path; it is a
   governance blocker for any claim that the proof has generated Track 1 plus
   three independent oracle lanes.

4. Minimal/full fixture coverage is adequate for source parity. The new minimal
   Apache and CITM tests exercise selected fields plus unknown-field skipping,
   and `w2_full_real_typed_fixtures_match_sidecars` parses both full W2 payloads
   through Track 1, Track 2/serde, explicit serde, and sonic. This is enough for
   parser/schema parity, but it is not throughput admission evidence.

5. W2 close is not reconciled at HEAD. Commit `12aff1e4` changes only the three
   source/generated files and leaves `skinny/RESULTS.md`, `skinny/REDRESS.md`,
   and `restart/skinny/tranches/sk-v8/HANDOFF.md` unchanged. SPEC Section 5
   still requires at least two new generated typed rows to pass their declared
   same-plane gate, preserve existing typed GO rows and direct GO guards, and
   route failed rows through REDRESS. P3-C defines the selected typed row gate as
   `Track 1 Mbps >= ceil(sonic-rs strict Mbps / 1.10)`. The committed source
   slice proves parity, not measured same-plane row admission. The committed W2
   plan permits leaving `RESULTS.md` unchanged only if the benchmark/report
   refresh is explicitly routed; that routing is not present in HEAD.

## Required Folds

1. Fold REDRESS/HANDOFF into committed state, not just dirty workspace state.
   REDRESS must record the admitted Apache/CITM source slice, the Canada
   rejection/falsifier, and the no-RESULTS benchmark/report exception if that
   exception remains the chosen route.

2. Reconcile W2 status language with the evidence. Either add measured
   `RESULTS.md` rows proving Apache/CITM `real_typed_struct` pass the strict
   same-plane gate while preserving existing typed/direct guards, or mark W2 as
   source-parity admitted with performance close explicitly routed rather than
   fully closed for W3 dispatch.

3. Fix the oracle-independence claim. Either implement a non-serde Track 2
   structural oracle for the W2 typed rows, or update W2 plan/REDRESS/HANDOFF
   language to say the oracle is serde-backed Track 2 plus separate sonic parity,
   not Track 2, serde, and sonic as three independent lanes.

4. Preserve the current source slice boundaries. Apache/CITM generated typed
   schema, parser output, carriers, checksums, and full-fixture parity tests can
   be accepted as the source fold; do not widen W2 into runtime/parser/direct
   guard changes while addressing the governance gaps.
