# S-P2 V3 CH5 - Hidden Coupling

Role: CH5 Hidden Coupling reviewer for the V3-folded S-P2 substrate-ceiling
cohort and packet docs.

Verdict: ACCEPT.

Score: 95/100.

## Blocking Findings

None.

## Non-Blocking Notes

1. **The one-producer / one-retained-`Tape` invariant is now explicit enough for
   CH5.** SC-3 states that `StructuralIndex` is move-consumed into `Tape` and
   leaves no query, clone, cache, attach-after-build, parser-owned cursor, or
   post-build API surface
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:115`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:120`).
   It also states the parser stops producing offsets and becomes a consumer of
   the class column
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:122`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:126`),
   and the Lock 1 proof frames the union as one producer, one retained `Tape`,
   and one cursor identity
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:283`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:292`).

2. **The old offset-append fallback aperture from V2 is closed.** SC-3 requires
   the migration slice to keep exactly one retained `Tape`, consume
   `StructuralIndex` into it, ban post-build position/class query APIs, and
   forbid generated parsers from owning an independent structural cursor
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:400`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:409`).
   The concrete runtime slice deletes `push_plain_offset` /
   `reserve_offsets_cold` so the offset column arrives whole from SIMD rather
   than being appended by the recursive-descent parser
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:443`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:453`).
   SPEC's W3 gate repeats the fail-closed rule: a retained projection passes
   only if it replaces the offset tape outright, deletes scalar rediscovery, and
   leaves no parallel producer
   (`restart/skinny/tranches/sk-v8/SPEC.md:470`-`restart/skinny/tranches/sk-v8/SPEC.md:476`).

3. **The `UnionTape` / public substrate aperture is removed.** SYNTHESIS says V3
   removes any `UnionTape` node option and admits only representation
   replacement of `OffsetTape` and retained `EventTape`, not a new substrate
   node, `BackendShape`, BIR variant, directive, or public substrate type
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:147`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:157`).
   SC-6's co-routed architecture note likewise says no `UnionTape` node, public
   substrate type, or alternate materialisation surface is introduced
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:305`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:320`),
   and its risk section explicitly rejects a sixth `BackendShape` / public API
   drift path
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:657`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:666`).

4. **The fact lane is no longer a broad recovery/layout cache.** SC-3 narrows
   facts to opaque generated ids stored in `Tape`; generic runtime may store and
   search them but not interpret grammar meaning
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:189`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:196`).
   It bans density tables, quote caches, skip caches, profile counters,
   parser-owned slots, per-consumer caches, and independent lifetimes
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:198`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:201`).
   The fact admission matrix now names producer, consumer, owner path, cursor
   domain, lifetime, and challenge gate for the admitted JSON fact and marks
   JSONL/layout rows as Lock 14 examples only
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:203`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:214`).

5. **The Tier A consumer set is consistent enough.** Tier A is now the generated
   retained JSON parser consuming positions/classes from the retained `Tape`;
   direct, `SinkOnly`, `path!`, retained view traversal, generated Track 1, and
   Track 2 are not Tier A proof unless owner paths and same-wave verification are
   named
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:307`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:312`).
   SC-3's Tier A table marks generated Track 1 retained parse as the only Tier A
   production consumer, retained view / `ValueRef` as touched, and `path!`,
   direct/SinkOnly, generated direct rows, and Track 2 as proven-untouched unless
   S-P3 expands scope
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:466`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:477`).
   SPEC aligns with this by requiring any scanner/parser/template path to prove
   consumption in the same retained parser/tape loop or explicitly expand scope
   with direct/SinkOnly/path owners and tests
   (`restart/skinny/tranches/sk-v8/SPEC.md:450`-`restart/skinny/tranches/sk-v8/SPEC.md:458`).

6. **`tape_vs_tape` is not being smuggled in as a W3 production consumer.** SC-5
   calls it W0/W1 gate-binding telemetry, not W3 production consumption or
   current SOTA-admission evidence
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:194`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:206`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:251`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:259`).
   SPEC and HANDOFF repeat the same boundary
   (`restart/skinny/tranches/sk-v8/SPEC.md:439`-`restart/skinny/tranches/sk-v8/SPEC.md:444`;
   `restart/skinny/tranches/sk-v8/HANDOFF.md:71`-`restart/skinny/tranches/sk-v8/HANDOFF.md:83`).

7. **Lock 14 and strictness constraints support the CH5 result rather than
   hiding a coupling.** SPEC now requires strict admission to reject plane
   mismatch, non-strict comparator evidence, stale sidecars, deferred strictness,
   and validation outside the measured row
   (`restart/skinny/tranches/sk-v8/SPEC.md:117`-`restart/skinny/tranches/sk-v8/SPEC.md:123`).
   The generality gate allows generic code to store generated class ordinals and
   opaque fact ids, while parser-state meaning stays in generated grammar modules
   (`restart/skinny/tranches/sk-v8/SPEC.md:239`-`restart/skinny/tranches/sk-v8/SPEC.md:260`).
   SC-6 confirms the same boundary with no public grammar API, no grammar branch,
   no new directive, and no hand-written per-grammar structural table
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:508`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:535`).

8. **Minor wording watchpoint, not a blocker:** SC-1 R3 still says "parallel
   co-indexed `Vec<u8>` class lane"
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:394`).
   Read in isolation, "parallel" could be mistaken for a sidecar. The surrounding
   and downstream fold closes that interpretation: SC-1 earlier says the
   preferred shape is co-indexed representation inside the retained `Tape`
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:294`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:303`),
   and SC-3/SPEC enforce internal tape columns only, with no surviving parallel
   offset append path.

## Required Fold Actions

None. Preserve the current V3 invariants during consolidation: one producer,
one retained `Tape`, no old offset append constructor, no parser-owned
facts/cursors, no `UnionTape` / public substrate API, and no telemetry-only row
counted as a production same-wave consumer.
