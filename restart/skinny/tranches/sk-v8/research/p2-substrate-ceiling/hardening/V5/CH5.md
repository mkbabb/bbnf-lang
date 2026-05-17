# S-P2 V5 CH5 - Hidden Coupling Review

Role: CH5 HIDDEN COUPLING.
Scope: current HEAD `1d2eafcf` after the V4 fold, with primary review of
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md`
and cross-checks against the S-P2 substrate-ceiling packet, ORCHESTRATOR
§3W/§3Z, strict-vs-strict comparator discipline, Lock 14 grammar-neutrality,
no new directive/BIR/substrate, and no deferrals.

Verdict: ACCEPT.
Confidence: 96%.

## Blockers

None.

## Hidden-Coupling Findings

1. **CH5's substrate test is satisfied by one retained `Tape`, not by a second
   scanner or sidecar.** ORCHESTRATOR defines CH5 as "No parallel substrate,
   sidecar producer, renamed-scanner Lock 1 violation, or Track 1 ≡ Track 2
   dishonesty" (`restart/prompts/ORCHESTRATOR.md:83`-`restart/prompts/ORCHESTRATOR.md:88`).
   The S-P2 prompt sharpens that: a second source scan, retained cursor, aux
   density table, or parser-owned structural projection violates Lock 1
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:126`-`restart/prompts/skinny/PASS-2-RESEARCH.md:131`).
   SC-3 now states the intended shape directly: the union has one producer, one
   retained artefact, and one cursor identity; `StructuralIndex` becomes the
   tape's own column, and `consume_structural` is removed
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:286`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:295`).

2. **No `UnionTape`, `BackendShape`, directive, BIR, or public substrate API is
   admitted.** SC-3's frontmatter bars a new directive, new BIR variant, and
   parallel/sidecar substrate
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:14`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:19`).
   SC-6 says the co-routed architecture fold is representation replacement of
   `OffsetTape`/retained `EventTape`, with no `UnionTape` node, public substrate
   type, alternate materialisation surface, sixth `BackendShape`, BIR variant,
   BBNF directive, grammar-name branch, or public generic grammar/substrate API
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:305`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:320`;
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:657`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:666`).
   SPEC preserves the same non-negotiables
   (`restart/skinny/tranches/sk-v8/SPEC.md:180`-`restart/skinny/tranches/sk-v8/SPEC.md:192`).

3. **SC-5's `tape_vs_tape` row is not hidden production coupling.** SC-5 prices
   `tape_vs_tape` as possible W0/W1 gate-binding telemetry, not current SOTA
   admission evidence and not W3 production consumption
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:194`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:206`;
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:326`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:346`).
   SPEC makes that route explicit: `tape_vs_tape` is a routed residual outside
   default W0/W1 scope, requires a later accepted plan with owner files/tests/LOC
   and an extra gate refresh, and cannot satisfy W3's production same-wave
   consumer (`restart/skinny/tranches/sk-v8/SPEC.md:125`-`restart/skinny/tranches/sk-v8/SPEC.md:131`).
   HANDOFF repeats the same constraint
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:82`-`restart/skinny/tranches/sk-v8/HANDOFF.md:85`).

4. **Strict-vs-strict comparator discipline is no longer a hidden scoreboard
   bypass.** SC-5 separates positive and negative same-run sonic-strict signals
   from simdjson/yyjson sidecar planning signals
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:39`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:61`).
   The executable SPEC rule rejects strict admission unless comparator plane,
   strictness, freshness/native anchor status, and measured-row validation all
   hold; deferred strictness, view-boundary UTF-8, stale sidecars, C++ sidecar-only
   evidence, and plane mismatch remain guard telemetry only
   (`restart/skinny/tranches/sk-v8/SPEC.md:117`-`restart/skinny/tranches/sk-v8/SPEC.md:123`).
   This closes the hidden coupling where a DOM sidecar could otherwise be read as
   a strict parse-plane admission anchor.

5. **Lock 14 grammar-neutrality is bounded by generated data and opaque ids.**
   Lock 14 forbids grammar-specific code, grammar-named modules/API, and
   grammar-name branches in generic crates (`restart/locks/LOCKS.md:60`).
   SC-3 confines `facts` to opaque generated fact ids, bars generic runtime code
   from matching fact ids or naming JSON/JSONL/CSS/indentation policy, and keeps
   density tables, quote caches, skip caches, parser-owned slots, and independent
   lifetimes out of Tier A
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:192`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:204`).
   SPEC's generality gate requires public API scans, grammar-branch scans,
   primitive/table scans, role/fact interpretation boundaries, template/provider
   boundaries, and non-JSON proof for CSS L4, Sheets, and BBNF-self when generic
   crates are edited (`restart/skinny/tranches/sk-v8/SPEC.md:247`-`restart/skinny/tranches/sk-v8/SPEC.md:269`).

6. **Track 1 / Track 2 independence is not collapsed.** SC-3's Tier A table keeps
   Track 2 as an independent oracle, not a production consumer, and requires any
   Track 2 source diff to route separately rather than hide inside Tier A
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:479`).
   The generated Track 1 retained parser is the Tier A production consumer and
   must validate UTF-8/control/escape work in the measured row, with no
   `tape_vs_tape` substitute
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:475`).

7. **The V4 deferral aperture is folded without creating a new CH5 aperture.**
   V4's cycle blocker was CH6's "rejected or routed to a separate S-P3 proof"
   phrase, not a CH5 substrate rejection
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V4-CONSOLIDATED.md:23`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V4-CONSOLIDATED.md:28`;
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V4-CONSOLIDATED.md:66`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V4-CONSOLIDATED.md:81`).
   Current SC-1 now rejects the candidate for this S-P2 packet if the grammar
   invariant cannot hold, and any later reconsideration requires fresh W0
   evidence, a newly accepted S-P3/W3 plan, named owners, tests, thresholds,
   same-wave production consumer, and challenge acceptance
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:366`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:380`).

## Disposition Of V4 Blocker(s)

Closed for CH5.

V4 CH5 had no blocker and accepted one retained `Tape`, no old offset append
fallback, no `UnionTape`, and no telemetry-as-consumer aperture
(`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/V4/CH5.md:16`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/V4/CH5.md:82`).
The V4 cycle blocker was CH6. HEAD folds both required V5 actions: SC-1 no
longer carries the "separate S-P3 proof" wording
(`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:375`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:380`),
and HANDOFF names V1, V2, V3, and V4 as non-converged with V5 only a first
qualifying ACCEPT unless followed by another ACCEPT or a user pin
(`restart/skinny/tranches/sk-v8/HANDOFF.md:71`-`restart/skinny/tranches/sk-v8/HANDOFF.md:85`).

## Residual Non-Blocking Risks

1. **S-P2 still cannot advance on one V5 ACCEPT.** ORCHESTRATOR requires two
   consecutive `>=95% ACCEPT` cycles with no unresolved REVISE, or a user pin
   (`restart/prompts/ORCHESTRATOR.md:118`-`restart/prompts/ORCHESTRATOR.md:123`).
   SPEC and HANDOFF now state V1-V4 did not converge and that V5 ACCEPT would be
   only the first qualifying cycle after REVISE
   (`restart/skinny/tranches/sk-v8/SPEC.md:454`-`restart/skinny/tranches/sk-v8/SPEC.md:457`;
   `restart/skinny/tranches/sk-v8/HANDOFF.md:71`-`restart/skinny/tranches/sk-v8/HANDOFF.md:85`).

2. **`tape_vs_tape` remains a future coupling temptation.** Its shape is useful
   telemetry, but it must stay residual until a later accepted plan assigns owner
   files, LOC, focused refusal tests, rerun budget, and same-run structural-index
   competitor rows (`restart/skinny/tranches/sk-v8/SPEC.md:125`-`restart/skinny/tranches/sk-v8/SPEC.md:131`;
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:326`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:346`).

3. **SC-6-L1-R1 remains a Pass Omega candidate, not current V1 lock text.** SC-6
   says the refinement must fold through Pass Omega and does not bind the V1 spec
   until ratified
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:256`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:261`).
   SPEC therefore correctly requires W3 either to follow Omega ratification or
   prove Lock 1 as written at challenge
   (`restart/skinny/tranches/sk-v8/SPEC.md:459`-`restart/skinny/tranches/sk-v8/SPEC.md:461`).

4. **SC-5 contains stale amendment-era wording about `S`.** SC-5 still says SPEC
   "currently freezes" `{A,C,G,K,L,N-direct}` and that adding `S` requires a
   REDRESS/SPEC amendment
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:220`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:232`;
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:347`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:351`).
   SPEC has already folded `S` and says neither `K` nor `S` may support strict
   SOTA admission (`restart/skinny/tranches/sk-v8/SPEC.md:57`-`restart/skinny/tranches/sk-v8/SPEC.md:77`).
   This is stale explanatory prose, not a hidden-coupling blocker.

## Required Folds If REVISE

None. Verdict is ACCEPT.
