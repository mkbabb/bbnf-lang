# SK-V12 S-P2 CHALLENGE V2 - CH5 Hidden Coupling

Disposition: ACCEPT.

## Scope

Lens: CH5 HIDDEN COUPLING. I audited the V2-folded S-P2 research
cohort for parallel substrates, sidecar producers, retained cursors,
renamed scanners, aux density tables, parser-owned structural projections,
and Track 1 / Track 2 dishonesty. Read set: `PASS-2-RESEARCH.md` §3 and
§8, all six S-P2 artifacts, V1 CHALLENGE plus consolidation, P2-D
closely, Locks 1/14/16, REDRESS sidecar/substrate entries, and the live
skinny runtime tape/generated files.

## Evidence

1. PASS-2 keeps this lens hard-fail. CH5 rejects a second source scan,
   retained cursor, aux density table, parser-owned structural projection,
   parallel substrate, sidecar producer, renamed scanner, or Track 1 /
   Track 2 collapse (`restart/prompts/skinny/PASS-2-RESEARCH.md:126`-
   `:131`). §8 repeats that splitting tape from structural projection or
   adding a sidecar event vector violates Lock 1 (`restart/prompts/skinny/PASS-2-RESEARCH.md:237`-
   `:240`).

2. The V1 fold resolved the only coupling-adjacent P2-D ambiguity instead
   of worsening it. V1 required `offset_tape_capacity_policy`,
   `sparse_flag_lookup_policy`, and `retained_cursor_skip_projection` to
   become diagnostic/ineligible unless fresh profile evidence names the
   exact locus, and required `structural_class_lane_union` to stay rejected
   (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md:36`-
   `:39`). The folded P2-D now states current selectable substrate
   candidates are zero, with three same-tape diagnostics and one rejected
   parallel-substrate route (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:69`-
   `:79`).

3. P2-D's substrate statement matches the live code. `ParserState` owns
   one `TapeBuilder`, calls the generated parser, and finishes into one
   `JsonRoot` (`skinny/crates/runtime/src/grammars/json/parser.rs:7`-
   `:51`). `attach_structural_index` remains a no-op debug assertion, while
   `consume_structural` writes source offsets directly into the tape
   (`skinny/crates/runtime/src/grammars/json/generated.rs:14`-`:17`,
   `:292`-`:305`). `Tape` stores source, one offset vector, sparse flags,
   payloads, and an id; `ValueRef` is `&Tape + cursor`
   (`skinny/crates/runtime/src/tape/mod.rs:94`-`:101`,
   `:175`-`:222`). `JsonNodeKind::at_cursor` re-derives kind from
   `source[offsets[cursor]]`, not from a class lane
   (`skinny/crates/runtime/src/grammars/json/value.rs:28`-`:47`).

4. The V2 P2 artifacts keep comparator and SIMD masks transient. P2-A
   says simdjson's retained structural index is comparator architecture,
   not importable bbnf substrate, and C1's class mask must be consumed
   immediately, not retained (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:14`,
   `:31`, `:53`-`:61`). P2-B requires grammar-owned byte-set callers and
   rejects retained class columns, parser-owned sidecars, second scans,
   retained cursors, and retained vectors in the relevant gates
   (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:38`,
   `:40`, `:45`-`:46`, `:74`). P2-C demotes LD4 and SHA3 to inventory,
   requires one canonical stream for LD4, and keeps mask emit support from
   persisting positions or structural sidecars
   (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:55`-
   `:65`, `:122`-`:133`, `:138`, `:143`).

5. The parse-that and grammar-neutral folds do not smuggle a sidecar under
   a new API name. P2-E's byte-set skip returns a local end offset and
   retains no mask, cursor sidecar, or structural index; its common rule
   forbids parallel structural streams, decoded-byte caches, retained
   cursor lists, and grammar-specific sidecars
   (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:58`,
   `:372`). P2-F marks F7 as oracle-only, F8 as same-tape accounting only,
   and explicitly forbids recreating sidecars, second scanners, retained
   class columns, `UnionTape`, streaming cursors, or alternate structural
   indexes (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:39`-
   `:40`, `:52`-`:57`, `:62`).

6. Track 1 and Track 2 remain honestly distinct. RESULTS states Track 1 is
   `runtime::generated_json::parse`, Track 2 is the independent hand-coded
   parser over `runtime::tape`, and Track 2 never calls
   `runtime::generated_json::parse` (`skinny/RESULTS.md:143`-`:145`).
   P2-A repeats that historical C++ sidecars are not strict anchors
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:119`).
   P2-B and P2-F classify digest/output work as report/oracle/product
   equality, not a parser primitive or typed/direct proof substitute
   (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:49`,
   `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:39`,
   `:62`).

7. The REDRESS substrate preblocks remain carried forward. REDRESS 50/51/53
   rejected parse-time aux side tables, event cursors, and parser-local
   structural cursors (`skinny/REDRESS.md:715`, `:742`, `:784`). REDRESS
   96/97/98 rejected and retired W3 class-column, streaming-cursor, and
   union-substrate routes after correctness-green implementations regressed
   all target and maintain rows (`skinny/REDRESS.md:2795`, `:2850`,
   `:2908`). REDRESS 119/120 keep direct residuals fixed and route SK-V12
   to generated non-JSON baseline work first (`skinny/REDRESS.md:3495`,
   `:3529`).

## Verdict

ACCEPT. The V1 folds did not introduce hidden coupling. They tightened the
boundary: substrate work is diagnostic/ineligible under current S-P1 unless
fresh evidence names the exact same-tape locus; classifier/SIMD outputs are
transient; LD4/SHA3 inventory is non-selectable; digest stays oracle/output
evidence; and `structural_class_lane_union` remains rejected. No parallel
substrate, sidecar producer, retained cursor, renamed scanner, or Track 1 /
Track 2 dishonesty is admitted by the V2 S-P2 research packet.

## Revise List

None for CH5. Carry forward to S-P3: do not shortlist P2-D diagnostics as
behavior waves without fresh same-tape profile evidence; do not convert
transient masks or emitted positions into retained indexes; do not treat LD4
as a second stream; do not use digest/oracle plumbing as Track 1 / Track 2
proof.
