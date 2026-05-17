# S-P2 V4 CH5 - Hidden Coupling

Role: CH5 Hidden Coupling reviewer for the V4-folded S-P2 substrate-ceiling
cohort and packet docs.

Verdict: ACCEPT.

Score: 96/100.

## Blocking Findings

None.

## Notes

1. **One producer / one retained `Tape` is now the controlling invariant.**
   SC-3 states that the SIMD scan writes the offset and class columns once,
   `StructuralIndex` is move-consumed into `Tape`, and no query surface, clone
   path, cache path, attach-after-build path, parser-owned cursor, or post-build
   API may survive
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:109`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:118`).
   The migration sketch repeats the same cardinality rule for every slice and
   forbids generated parsers from owning an independent structural cursor
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:407`).

2. **The old offset-append fallback is fail-closed.** SC-3 deletes
   `push_plain_offset` / `reserve_offsets_cold` so the offset column arrives
   whole from SIMD, not from recursive-descent append
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:453`).
   SC-6 requires the old offset-append constructor/API to be deleted rather
   than kept as a cold fallback
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:323`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:641`).
   SPEC's W3 exit gate makes the same rule executable: any retained projection
   beside the existing tape is a sidecar, while a projection that becomes the
   tape passes
   (`restart/skinny/tranches/sk-v8/SPEC.md:483`).

3. **No `UnionTape` / public API aperture remains.** SC-6's co-routed
   architecture text admits representation replacement of `OffsetTape` and the
   retained form of `EventTape`, with no `UnionTape` node, public substrate type,
   new `BackendShape`, BIR variant, BBNF directive, grammar-name branch, or
   public generic grammar/substrate API
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:305`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:314`).
   SYNTHESIS and SPEC preserve the same boundary
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:154`,
   `restart/skinny/tranches/sk-v8/SPEC.md:441`).

4. **Parser-owned facts/cursors are blocked.** SC-3 narrows facts to opaque
   generated ids stored in the retained `Tape`, explicitly banning density
   tables, quote caches, skip caches, profile counters, parser-owned slots,
   per-consumer caches, and independent fact lifetime
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:192`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:201`).
   SPEC's Lock 14 gate keeps event-role and recovery/layout meaning inside
   generated grammar modules rather than generic substrate code
   (`restart/skinny/tranches/sk-v8/SPEC.md:257`).

5. **Tier A's consumer set is consistent enough for CH5.** SC-3 identifies
   generated Track 1 retained JSON parse as the only Tier A production consumer,
   marks retained view / `ValueRef` as touched or proven untouched, and keeps
   `path!`, direct/SinkOnly, generated direct rows, and Track 2 outside Tier A
   unless S-P3 explicitly expands scope
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:416`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:471`).
   SPEC aligns by requiring scanner/parser/template paths to prove consumption
   in the same retained JSON parser/tape loop or name same-wave
   direct/SinkOnly/path owners and tests
   (`restart/skinny/tranches/sk-v8/SPEC.md:465`).

6. **`tape_vs_tape` is not a W3 production consumer.** SPEC marks it as a routed
   residual, not default W0/W1 scope, and says it cannot satisfy W3's production
   same-wave consumer
   (`restart/skinny/tranches/sk-v8/SPEC.md:125`). HANDOFF repeats that it is
   W0/W1 telemetry only and explicitly residual for this packet
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:79`). SC-5's own recommendation
   also says it is gate-binding telemetry, not W3 production consumption or
   current SOTA-admission evidence
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:194`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:251`).

7. **The carry-forward promise from V3 is folded.** SC-1 R2 now says a
   number-heavy maintain-budget miss rejects the current Tier A candidate for
   this cycle; later reconsideration needs fresh W0 evidence plus a newly
   accepted S-P3/W3 plan, and is not a carry-forward promise from S-P2
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:394`).
   That satisfies this lens. The remaining phrase about routing a grammar class
   to a separate S-P3 proof is not a CH5 blocker because it still rejects the
   old-producer coexistence path
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:366`).

8. **Minor wording watchpoints, not blockers.** SC-1 still says "parallel
   co-indexed `Vec<u8>` class lane" when discussing offset packing
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:395`);
   read in isolation, "parallel" can sound like a sidecar. SC-3 and SPEC close
   that interpretation by requiring internal tape columns only and by banning
   any surviving old producer/API surface. SC-2 also lists "generated Track 1"
   among non-proof paths while the table later means generated Track 1 retained
   parse is the Tier A production consumer
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:311`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:475`);
   the table and SPEC make the intended split clear enough.

## Required Folds

None.
