# SK-V12 S-P2 CHALLENGE V3 - CH5 Hidden Coupling

Disposition: ACCEPT.

## Scope

Lens: CH5 HIDDEN COUPLING convergence check. I audited the V2-folded S-P2
packet at commit `6b8be238` for parallel substrate, sidecar producer,
retained cursor, renamed scanner, aux density table, parser-owned structural
projection, and Track 1 / Track 2 collapse. Read set: `PASS-2-RESEARCH.md`
§3/§8, the folded S-P2 artifacts, V1/V2 hardening and consolidated folds,
P2-D, Locks 1/14/16, REDRESS sidecar/substrate entries, and live skinny
runtime tape/generated/bench surfaces.

## Evidence

1. The PASS-2 hidden-coupling contract is still hard-fail. CH5 rejects a
   second source scan, retained cursor, aux density table, parser-owned
   structural projection, parallel substrate, sidecar producer, renamed
   scanner, or Track 1 / Track 2 dishonesty
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:126`-`:131`). PASS-2 §8
   repeats that splitting tape from structural projection or adding a sidecar
   event vector violates Lock 1 (`restart/prompts/skinny/PASS-2-RESEARCH.md:237`-`:240`).

2. V1 and V2 folded the only P2-D coupling-adjacent ambiguity into a stable
   boundary. The V1 consolidated fold required capacity, sparse-flag, and
   retained cursor-skip entries to become diagnostic/ineligible unless fresh
   profile evidence names the exact same-tape locus, while keeping
   `structural_class_lane_union` rejected
   (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md:36`-`:39`).
   The V2 consolidation accepted that boundary and restated that P2-D has no
   current selectable tape-substrate candidate; its same-tape items are
   diagnostic/ineligible and `structural_class_lane_union` remains rejected
   (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:29`-`:30`).
   The folded P2-D table agrees: selectable candidate count is 0, same-tape
   diagnostics are 3, and the parallel-substrate route is rejected
   (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:69`-`:79`).

3. The live runtime still has one retained substrate. `ParserState` owns one
   `TapeBuilder`, calls the generated parser, and finishes into one `JsonRoot`
   (`skinny/crates/runtime/src/grammars/json/parser.rs:7`-`:51`).
   `attach_structural_index` is a no-op debug assertion, not a retained scanner
   output; `consume_structural` writes source offsets into the tape
   (`skinny/crates/runtime/src/grammars/json/generated.rs:14`-`:17`,
   `:292`-`:305`). `Tape` owns source, one offset vector, sparse flags,
   payloads, and an id, while `ValueRef` is `&Tape + cursor`
   (`skinny/crates/runtime/src/tape/mod.rs:94`-`:101`,
   `:175`-`:222`). `JsonNodeKind::at_cursor` derives kind from
   `source[offsets[cursor]]`, not a hidden class lane
   (`skinny/crates/runtime/src/grammars/json/value.rs:28`-`:47`).

4. The folded SIMD and parse-that research keeps masks and cursors transient.
   P2-B requires grammar-owned byte-set callers and rejects retained class
   columns, parser-owned sidecars, second scans, retained cursors, and retained
   vectors (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:38`,
   `:40`, `:45`-`:46`, `:74`). P2-C keeps LD4 as inventory unless an already
   canonical stream exists and forbids sidecar storage or retained position
   streams for mask emission (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:138`,
   `:143`). P2-E's byte-set skip returns a local offset and retains no mask,
   cursor sidecar, or structural index; its common rule forbids parallel
   structural streams, decoded-byte caches, retained cursor lists, and
   grammar-specific sidecars (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:58`,
   `:372`). P2-F marks F8 as same-tape accounting only and forbids recreating
   sidecars, second scanners, retained class columns, `UnionTape`, streaming
   cursors, or alternate structural indexes
   (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:40`,
   `:57`).

5. Track 1 and Track 2 remain honestly separated. `skinny/RESULTS.md` states
   Track 1 is `runtime::generated_json::parse`, Track 2 is the independent
   hand-coded parser over `runtime::tape`, and Track 2 never calls
   `runtime::generated_json::parse` (`skinny/RESULTS.md:143`-`:145`).
   P2-A treats historical C++ sidecars as non-strict anchors rather than fresh
   proof (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:119`).
   P2-B and P2-F keep digest/output work as oracle or output-plane equality,
   not parser proof or Track 1 / Track 2 equivalence
   (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:49`,
   `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:39`,
   `:62`).

6. REDRESS preblocks remain carried and are not reopened under new names.
   REDRESS 50/51/53 reject aux side tables, event/byte-class cursors, and
   parser-local structural cursors (`skinny/REDRESS.md:715`,
   `skinny/REDRESS.md:742`, `skinny/REDRESS.md:784`). REDRESS 96/97/98 reject
   and retire W3 class-column, streaming-cursor, and union-substrate routes
   after correctness-green implementations regressed target and maintain rows
   (`skinny/REDRESS.md:2795`, `skinny/REDRESS.md:2850`,
   `skinny/REDRESS.md:2908`). REDRESS 119/120 keep JSON direct residuals fixed
   and route SK-V12 to generated non-JSON baseline work first
   (`skinny/REDRESS.md:3495`, `skinny/REDRESS.md:3529`).

## Verdict

ACCEPT. The V3 convergence check finds no hidden-coupling regression from the
V2 accepted packet. P2-D contributes no selectable substrate candidate under
current S-P1 evidence; same-tape diagnostics remain ineligible; transient masks
and local cursors are not retained; digest/oracle work does not collapse Track 1
and Track 2; and `structural_class_lane_union` remains rejected. No parallel
substrate, sidecar producer, retained cursor, renamed scanner, aux density
table, parser-owned structural projection, or Track 1 / Track 2 dishonesty is
admitted.

## Revise List

None for CH5. Carry forward the V2 guards unchanged: do not shortlist P2-D
diagnostics as behavior waves without fresh same-tape profile evidence; do not
turn transient masks, emitted positions, or local offsets into retained indexes;
do not treat LD4 as a second stream; do not use digest/oracle plumbing as Track
1 / Track 2 proof.
