# S-P2 V2 CH5 - Hidden Coupling

Role: CH5 Hidden Coupling reviewer for the S-P2 substrate-ceiling V2 fold.

Verdict: REVISE.

Score: 84/100.

## Blocking Findings

1. **The low-density fallback reintroduces the old offset-tape producer.**

   The governing rule is strict: CH5 rejects a second scan, retained cursor, aux
   density table, or parser-owned structural projection
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:126`-`restart/prompts/skinny/PASS-2-RESEARCH.md:131`),
   and SPEC says a retained projection passes only if it replaces the offset tape
   outright, deletes scalar rediscovery, and leaves no parallel producer
   (`restart/skinny/tranches/sk-v8/SPEC.md:430`-`restart/skinny/tranches/sk-v8/SPEC.md:436`).
   V2 mostly folds that rule, but SC-1 still says that if canada/mesh/numbers
   miss the maintain budget, the plan should keep `OffsetTape`-via-recursive-
   descent for low-quote-density rules via `derive_backend_shape`
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:363`).
   That is an old producer path surviving beside the union candidate unless it is
   explicitly a rejection/reroute outcome. SC-6 names the forbidden version:
   building the structural index and also materializing an offset tape for an
   interim consumer re-creates the parallel substrate, and the constructor must
   be deleted, not merely unused
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:610`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:620`).

2. **The same-wave production consumer set is inconsistent across the fold.**

   SC-2 says `direct_to_struct` and `real_typed_struct` materialize fields by
   reading intervals from the fused index tape
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:307`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:316`),
   and SC-3 says `SinkOnly` walks the union columns to drive typed-field writes
   before dropping the tape
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:262`).
   But SC-3's Tier A "exact consumers" only name JSON retained `OffsetTape`,
   existing JSON `EventTape` sparse fact patching, `ValueRef::offset()`, and JSON
   `consume_structural` call sites
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:361`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:367`).
   SPEC requires any scan/parser/template path to prove consumption in the same
   parser/tape or `SinkOnly` loop
   (`restart/skinny/tranches/sk-v8/SPEC.md:416`-`restart/skinny/tranches/sk-v8/SPEC.md:421`),
   while SC-6 requires same-wave migration of the cursor, `ValueRef`, `path!`,
   and any retained-view/direct consumers touched
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:617`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:620`).
   The fold must either narrow Tier A to retained JSON parse only, or enumerate
   the direct/SinkOnly/path consumers as same-wave production consumers with owner
   paths and verification.

3. **`EventTape` recovery/layout facts remain an unowned cache aperture.**

   SC-3 correctly bans density tables, quote caches, skip caches, profile
   counters, parser-owned slots, per-consumer caches, and independent lifetimes
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:190`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:194`).
   But the admitted `facts` lane still includes "EventTape-required
   recovery/layout facts" without an inventory, producer, owner path, or
   same-wave consumer
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:157`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:164`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:257`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:261`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:382`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:388`).
   That phrase can hide a parser-owned recovery/layout cache under a neutral
   name. Under SPEC's Lock 1 gate, any aux table, density cache, sidecar event
   vector, or retained projection beside the tape fails
   (`restart/skinny/tranches/sk-v8/SPEC.md:430`-`restart/skinny/tranches/sk-v8/SPEC.md:436`).

4. **SC-6 still leaves a new `UnionTape` architecture node as an allowed option.**

   SC-6's amendment text says co-routed architecture work may make the pipeline
   gain a `UnionTape` node, or re-specify `OffsetTape` so its retained form is the
   structural index
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:301`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:304`).
   The same section then says no new `BackendShape` variant is allowed and the
   union is only the representation of `OffsetTape` and retained `EventTape`
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:307`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:312`),
   and R2 repeats that a sixth shape would be confusion
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:622`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:627`).
   The `UnionTape node` alternative is therefore a hidden API/substrate aperture.
   The fold must remove that alternative and specify representation replacement
   only.

## Non-Blocking Notes

- V1 CH5's main sidecar fixes are materially folded: `StructuralIndex` is
  move-consumed with no post-build query/clone/cache/attach API
  (`SC-3-union-substrate-design.md:115`-`SC-3-union-substrate-design.md:120`,
  `SC-3-union-substrate-design.md:352`-`SC-3-union-substrate-design.md:357`),
  and SC-4 now makes quote/backslash/parity masks transient unless they become
  co-indexed tape facts inside the singular `Tape`
  (`SC-4-string-plane-gap.md:281`-`SC-4-string-plane-gap.md:295`,
  `SC-4-string-plane-gap.md:353`-`SC-4-string-plane-gap.md:355`).
- The strict-vs-strict posture is not a CH5 blocker in V2. SC-4 marks the
  string-density table as diagnostic, not admission evidence
  (`SC-4-string-plane-gap.md:178`-`SC-4-string-plane-gap.md:182`), and SC-5 keeps
  `tape_vs_tape` as telemetry/gate-binding work until same-run structural-index
  competitor rows exist
  (`SC-5-k-classification-adjudication.md:178`-`SC-5-k-classification-adjudication.md:190`,
  `SC-5-k-classification-adjudication.md:288`-`SC-5-k-classification-adjudication.md:291`).
- No new directive or BIR variant is proposed in the folded SC-3 constraints
  (`SC-3-union-substrate-design.md:14`-`SC-3-union-substrate-design.md:19`).
  The remaining risk is substrate/API ownership, not directive or BIR surface.
- The P1/P3/Omega posture is mostly disciplined: SC-1 and SC-2 keep the union
  research-only until S-P3/W3 fills owner paths, thresholds, revert protocol, and
  accepted challenge proof
  (`SC-1-offset-tape-teardown.md:262`-`SC-1-offset-tape-teardown.md:266`,
  `SC-2-two-stage-sota.md:340`-`SC-2-two-stage-sota.md:342`), and SPEC requires
  either Pass Omega ratification or Lock-1-as-written proof before W3
  (`restart/skinny/tranches/sk-v8/SPEC.md:405`-`restart/skinny/tranches/sk-v8/SPEC.md:414`).

## Required Fold Actions

1. Rewrite SC-1 R2 so number-heavy maintain failure rejects or routes the union
   candidate. Do not preserve `OffsetTape`-via-recursive-descent as a mixed
   low-density path inside the same retained-index design unless a later S-P3 plan
   proves one substrate per selected rule and no shared parallel API.
2. Align SC-2, SC-3, SC-6, SPEC, and HANDOFF on the Tier A production consumer
   set. Either remove direct/SinkOnly/path claims from Tier A, or add exact owner
   paths and same-wave verification for `path!`, direct/SinkOnly generated loops,
   retained view, cursor, and `ValueRef`.
3. Replace the broad "EventTape-required recovery/layout facts" allowance with a
   fact admission matrix: fact name, producer, consumer, owner file, cursor
   domain, lifetime, and challenge gate. Unspecified recovery/layout facts are out
   of scope for Tier A.
4. In SC-6, delete the `UnionTape node` option. The only admitted fold is that
   `OffsetTape` and retained `EventTape` are re-specified to use the union
   representation; no new substrate node, BackendShape variant, BIR variant, or
   public substrate type is introduced.
5. Re-run CH5 after those edits against the concrete invariants: one producer,
   one retained `Tape`, no old offset append path beside a retained index, no
   parser-owned facts/cursors, and no telemetry row counted as a production
   same-wave consumer.
