# S-P2 V5 CH6 - Anti-Paper-Close Review

Role: CH6 ANTI-PAPER-CLOSE.

Scope: current HEAD `1d2eafcf`, with primary review of
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`
against ORCHESTRATOR §3W/§3Z, the S-P2 CH6 rule, strict-vs-strict comparator
discipline, Lock 14 grammar-neutrality, no new directive/BIR/substrate, and no
deferrals.

Verdict: ACCEPT.

Confidence: 95%.

## Blockers

None.

## Basis

1. **The V4 CH6 blocker is folded.** V4 blocked on SC-1 preserving the phrase
   "rejected or routed to a separate S-P3 proof" and on packet governance that
   needed to name V1 through V4 as non-converged before a V5 challenge
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V4-CONSOLIDATED.md:28`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V4-CONSOLIDATED.md:66`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V4-CONSOLIDATED.md:81`).
   Current SC-1 now rejects the union candidate for this S-P2 packet if Tier A
   cannot satisfy the grammar-class invariant, and any later reconsideration
   requires fresh W0 evidence plus a newly accepted S-P3/W3 plan with owners,
   tests, thresholds, same-wave production consumer, and challenge acceptance
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:366`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:380`).
   HANDOFF, SPEC, and SYNTHESIS now all state that V1, V2, V3, and V4 did not
   converge and that a V5 ACCEPT alone does not dispatch S-P3
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:71`-`restart/skinny/tranches/sk-v8/HANDOFF.md:85`,
   `restart/skinny/tranches/sk-v8/SPEC.md:454`-`restart/skinny/tranches/sk-v8/SPEC.md:457`,
   `restart/skinny/tranches/sk-v8/SYNTHESIS.md:188`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:192`).

2. **SC-6 does not paper-close the Lock 1 amendment as already binding.**
   SC-6 marks SC-6-L1-R1 as a Pass Omega candidate, says it originates in the
   skinny track, and says it does not bind the V1 spec until Pass Omega ratifies
   it
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:247`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:261`).
   Its risk section keeps the governance fork explicit: W3 either waits for
   Omega ratification or includes a Lock-1-as-written proof plus a routed Omega
   residual
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:685`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:695`).
   SYNTHESIS and SPEC preserve the same fork
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:251`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:262`,
   `restart/skinny/tranches/sk-v8/SPEC.md:459`-`restart/skinny/tranches/sk-v8/SPEC.md:461`).

3. **The no-deferral rule holds for the candidate itself.** ORCHESTRATOR CH6
   forbids self-reported completion without evidence and forbids future-phase
   deferral (`restart/prompts/ORCHESTRATOR.md:83`-`restart/prompts/ORCHESTRATOR.md:88`);
   the S-P2 prompt says a candidate deferred to "a future wave will detail" is
   paper-close
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:133`-`restart/prompts/skinny/PASS-2-RESEARCH.md:138`).
   SC-6 instead states concrete same-wave consumer requirements: the union lands
   only with touched/proven-untouched rows for cursor, `ValueRef`, `path!`,
   retained-view, and direct/SinkOnly generated consumers
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:641`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:655`).
   SC-3 supplies the corresponding owner/test/revert table for Tier A
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:469`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:480`),
   and SPEC refuses W3 selection until W0/W1 closure, fresh owner paths,
   same-wave production consumer, revert protocol, thresholds, pre-blocks, and
   challenge acceptance exist
   (`restart/skinny/tranches/sk-v8/SPEC.md:447`-`restart/skinny/tranches/sk-v8/SPEC.md:452`).

4. **Strict-vs-strict evidence is not being laundered through SC-6.** SC-6 is
   a Lock 1 / Lock 14 amendment document, not a comparator-admission document.
   Packet-level strict admission is nevertheless executable: `gate-json` must
   reject strict admission unless comparator plane, strictness, freshness, and
   measured-row validation all hold; deferred strictness, view-boundary UTF-8,
   stale sidecars, C++ sidecar-only evidence, and plane mismatch are guard
   telemetry only (`restart/skinny/tranches/sk-v8/SPEC.md:117`-`restart/skinny/tranches/sk-v8/SPEC.md:123`).
   SC-2's Tier A table confines row targets to same-run strict JSON parse plane
   and forbids sidecar, view-boundary, post-parse, or stale comparator evidence
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:340`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:347`).
   SC-4's quote-density table is diagnostic, not an admission gate
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:190`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:193`).

5. **Lock 14 grammar-neutrality is grounded, not postponed.** SC-6 defines
   `StructuralAlphabet` as generated per-grammar data: structural byte set,
   byte-to-ordinal table, escape/quote byte sets, and tail policy, while generic
   code may only filter bytes, compact offsets, copy ordinals, and compare
   ordinals for equality
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:338`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:395`).
   SC-6 then exercises JSON, CSS L4, Sheets, and arbitrary user grammars,
   including reused punctuation, doubled-quote Sheets strings, and empty-alphabet
   grammars that route to `EagerTape`
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:397`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:506`).
   SPEC makes the future implementation gate executable through public API,
   grammar-branch, primitive/table, role/fact-boundary, template/provider, and
   non-JSON proof scans
   (`restart/skinny/tranches/sk-v8/SPEC.md:247`-`restart/skinny/tranches/sk-v8/SPEC.md:269`).

6. **No new directive, BIR variant, BackendShape, public substrate type, or
   parallel substrate is admitted.** SC-6's co-routed amendments keep
   `OffsetTape` / retained `EventTape` as representation replacements and state
   that no `UnionTape` node, public substrate type, alternate materialisation
   surface, sixth `BackendShape`, BIR variant, BBNF directive, or grammar-name
   branch is introduced
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:301`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:328`).
   The risk table repeats the same prohibition and adds the old offset-append
   constructor to the forbidden set
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:657`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:666`).
   SPEC's W3 exit gate makes substrate cardinality the test: a retained
   projection added beside the offset tape is a sidecar and fails; a projection
   that becomes the tape passes
   (`restart/skinny/tranches/sk-v8/SPEC.md:483`-`restart/skinny/tranches/sk-v8/SPEC.md:489`).

## Disposition Of V4 Blocker

Closed. The only V4 REVISE was CH6's deferral-shaped SC-1 phrase plus the need
to carry V1-V4 non-convergence into the packet
(`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/V4/CH6.md:11`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/V4/CH6.md:37`,
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/V4/CH6.md:82`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/V4/CH6.md:98`).
Current SC-1, HANDOFF, SPEC, and SYNTHESIS fold both requirements
(`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:375`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:380`,
`restart/skinny/tranches/sk-v8/HANDOFF.md:71`-`restart/skinny/tranches/sk-v8/HANDOFF.md:85`,
`restart/skinny/tranches/sk-v8/SPEC.md:454`-`restart/skinny/tranches/sk-v8/SPEC.md:457`,
`restart/skinny/tranches/sk-v8/SYNTHESIS.md:188`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:192`).

## Residual Non-Blocking Risks

1. **V5 hard ceiling governance must be explicit in consolidation.** This CH6
   ACCEPT does not converge S-P2 by itself. ORCHESTRATOR requires two
   consecutive qualifying ACCEPT cycles or a user pin, and also caps S-P2 at V5
   (`restart/prompts/ORCHESTRATOR.md:118`-`restart/prompts/ORCHESTRATOR.md:127`;
   `restart/prompts/skinny/PASS-2-RESEARCH.md:155`-`restart/prompts/skinny/PASS-2-RESEARCH.md:162`).
   Since V4 was REVISE, V5 can be only the first qualifying ACCEPT after REVISE.
   The consolidator must therefore record no automatic S-P3; absent an explicit
   user pin, the hard-ceiling outcome is escalation rather than an implicit V6.

2. **SC-6's Lock 14 verification line is a future implementation gate, not a
   completed code audit.** SC-6 includes a Lock-14-style `rg` command and says it
   returns zero generic role leaks
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:508`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:535`).
   This is non-blocking because SPEC independently requires the actual scans and
   non-JSON proof when generic crates are edited
   (`restart/skinny/tranches/sk-v8/SPEC.md:247`-`restart/skinny/tranches/sk-v8/SPEC.md:269`).
   A later W3 plan must run current workspace paths, not cite SC-6 prose as
   verification evidence.

3. **Tail policy placement remains a wording watchpoint.** SC-6 places
   `pad/clamp policy` inside `StructuralAlphabet`
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:359`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:364`).
   V4 CH2 already treated this as non-blocking because tail policy is Lock 16
   primitive discipline rather than JSON semantics
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/V4/CH2.md:100`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/V4/CH2.md:106`).

## Required Folds If REVISE

None. Verdict is ACCEPT.
