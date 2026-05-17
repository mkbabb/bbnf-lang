# S-P2 V6 CH5 - Hidden Coupling Review

Title: CH5 hidden-coupling review of the exceptional V6 substrate-ceiling packet.

Scope: current HEAD `f20fbc46` after the user-authorized exceptional V6
governance fold, with primary review of
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md`
and cross-checks against ORCHESTRATOR §3W/§3Z, strict-vs-strict comparator
discipline, Lock 14 grammar-neutrality, hidden coupling/no sidecar, no new
directive/BIR/substrate, no deferrals, and no automatic S-P3 dispatch from one
V6 ACCEPT.

Verdict: ACCEPT.

Confidence: 96%.

## Blockers

None.

## Hidden-Coupling Findings

1. **The V6 exception does not hide a normal post-ceiling path.** ORCHESTRATOR
   requires two consecutive qualifying ACCEPT cycles or an explicit user pin
   before a pass advances, and says the next pass does not dispatch until
   convergence holds (`restart/prompts/ORCHESTRATOR.md:118`-`restart/prompts/ORCHESTRATOR.md:123`).
   The S-P2 prompt repeats the same two-cycle/user-pin rule and the V <= 5 hard
   ceiling (`restart/prompts/skinny/PASS-2-RESEARCH.md:155`-`restart/prompts/skinny/PASS-2-RESEARCH.md:162`).
   V5 consolidation escalated because CH4 found the old packet text implied a
   normal V6 path after the V5 ceiling
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V5-CONSOLIDATED.md:35`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V5-CONSOLIDATED.md:42`),
   but it also named user authorization of an exceptional V6 as a valid
   escalation option
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V5-CONSOLIDATED.md:64`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V5-CONSOLIDATED.md:78`).
   HEAD now records exactly that: V6 is user-authorized, and one V6 ACCEPT is
   only the first qualifying cycle after V5 REVISE unless the user pins final or
   authorizes another over-ceiling cycle
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:99`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:102`,
   `restart/skinny/tranches/sk-v8/SYNTHESIS.md:191`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:195`,
   `restart/skinny/tranches/sk-v8/SPEC.md:454`-`restart/skinny/tranches/sk-v8/SPEC.md:460`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:71`-`restart/skinny/tranches/sk-v8/HANDOFF.md:80`).

2. **The substrate union remains a replacement, not a sidecar.** CH5 rejects a
   parallel substrate, sidecar producer, renamed-scanner Lock 1 violation, or
   Track 1 == Track 2 dishonesty (`restart/prompts/ORCHESTRATOR.md:83`-`restart/prompts/ORCHESTRATOR.md:88`);
   S-P2 sharpens this to reject a second source scan, retained cursor, aux
   density table, or parser-owned structural projection
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:126`-`restart/prompts/skinny/PASS-2-RESEARCH.md:131`).
   SC-3's union has one producer, one retained `Tape`, and one cursor identity;
   `StructuralIndex` becomes the tape column and `consume_structural` is removed
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:286`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:295`).
   SPEC makes the implementation gate adversarial: any source-byte second scan,
   retained cursor, aux table, density cache, or sidecar event vector running
   alongside the offset tape fails Lock 1; a projection added beside the existing
   tape fails, while a projection that becomes the tape passes
   (`restart/skinny/tranches/sk-v8/SPEC.md:486`-`restart/skinny/tranches/sk-v8/SPEC.md:492`).

3. **No new directive, BIR variant, public substrate type, or BackendShape is
   admitted.** SC-3 frontmatter bars a new BBNF directive, new BIR variant, and
   parallel/sidecar substrate
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:14`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:19`).
   SC-6 keeps the co-routed architecture fold as representation replacement of
   `OffsetTape`/retained `EventTape`, with no `UnionTape`, public substrate type,
   alternate materialisation surface, sixth `BackendShape`, BIR variant, BBNF
   directive, grammar-name branch, or public generic grammar/substrate API
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:305`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:320`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:657`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:666`).
   SPEC preserves the same non-negotiables: no new directives, no new BIR
   variant, no new substrate without same-wave consumer, no JSON policy in
   generic crates, and only W0 dispatches after G-Alpha from this packet
   (`restart/skinny/tranches/sk-v8/SPEC.md:180`-`restart/skinny/tranches/sk-v8/SPEC.md:192`).

4. **`tape_vs_tape` remains telemetry, not a hidden production consumer.** SC-5
   introduces `tape_vs_tape` as W0/W1-plan telemetry or gate-binding work, not a
   W3 production consumer and not current SOTA-admission evidence
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:194`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:206`).
   Its risk section prices the work as a possible later 120-180 LOC
   gate-binding augmentation and says a later plan must add owner files, tests,
   LOC, and rerun budget before it can consume wave scope
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:326`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:346`).
   SPEC and HANDOFF preserve that routing: `tape_vs_tape` is residual telemetry,
   not SOTA admission evidence and not W3's production same-wave consumer
   (`restart/skinny/tranches/sk-v8/SPEC.md:125`-`restart/skinny/tranches/sk-v8/SPEC.md:131`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:84`-`restart/skinny/tranches/sk-v8/HANDOFF.md:90`).

5. **Strict-vs-strict comparator discipline is not coupled to sidecar scores.**
   SPEC classifies same-run strict anchors separately from flaw probes and
   sidecar planning signals (`restart/skinny/tranches/sk-v8/SPEC.md:50`-`restart/skinny/tranches/sk-v8/SPEC.md:55`).
   Its strict-admission rule rejects admission unless output plane, strictness,
   freshness/native-anchor status, and in-row UTF-8/control/escape validation
   all hold; deferred strictness, view-boundary UTF-8, stale sidecars,
   C++ sidecar-only evidence, and plane mismatch are guard telemetry only
   (`restart/skinny/tranches/sk-v8/SPEC.md:117`-`restart/skinny/tranches/sk-v8/SPEC.md:123`).
   SC-5 demotes `parse_only` from the SOTA scoreboard while preserving both
   positive and negative comparator deltas, so residual losses cannot disappear
   behind the substrate-guard label
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:180`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:216`).

6. **Lock 14 grammar-neutrality is still expressed through generated data and
   opaque ids.** SC-3 confines facts to opaque generated fact ids and forbids
   generic runtime code from matching fact ids, naming JSON/JSONL/CSS/indentation
   policy, or branching on a grammar
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:192`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:204`).
   SC-6's proposed Lock 1 refinement says generic substrate code consumes only
   generated byte sets and opaque ordinals, while event-role meaning stays inside
   generated grammar modules keyed by parser state plus class/byte
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:279`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:287`).
   SPEC turns that into an implementation gate with public API scans,
   grammar-branch scans, primitive/table scans, role/fact-boundary checks,
   template/provider-boundary checks, and non-JSON proof for CSS L4, Sheets, and
   BBNF-self (`restart/skinny/tranches/sk-v8/SPEC.md:247`-`restart/skinny/tranches/sk-v8/SPEC.md:269`).

7. **No deferral or automatic S-P3 route is hidden inside the W3 nomination.**
   The S-P2 prompt rejects future-wave deferral
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:133`-`restart/prompts/skinny/PASS-2-RESEARCH.md:138`).
   SYNTHESIS says the hypothesis is not selected by S-P2 and that W3 still
   requires W0/W1 closure, a fresh S-P3/W3 plan, exact owner paths, same-wave
   production consumer, revert protocol, measurement thresholds, measured-path
   validation proof, and challenge acceptance
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:180`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:189`).
   SPEC repeats that the nomination does not select W3 and does not advance
   S-P2 (`restart/skinny/tranches/sk-v8/SPEC.md:447`-`restart/skinny/tranches/sk-v8/SPEC.md:460`).

## Disposition Of V5 CH4 Blocker

Closed for this V6 CH5 review.

V5 CH4 blocked because the packet text still budgeted a normal qualifying cycle
after V5 even though S-P2 hard-caps at V5
(`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/V5/CH4.md:11`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/V5/CH4.md:15`).
V5 consolidation said the legal alternatives were user pin, user revision of
the orchestration instruction to authorize exceptional V6, or keeping S-P2 and
SK-V8 waves blocked
(`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V5-CONSOLIDATED.md:64`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V5-CONSOLIDATED.md:78`).
Current HEAD takes the second alternative and records the user-authorized V6
while keeping S-P3 blocked after a single V6 ACCEPT
(`restart/skinny/tranches/sk-v8/SYNTHESIS.md:99`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:102`,
`restart/skinny/tranches/sk-v8/SPEC.md:454`-`restart/skinny/tranches/sk-v8/SPEC.md:460`,
`restart/skinny/tranches/sk-v8/HANDOFF.md:77`-`restart/skinny/tranches/sk-v8/HANDOFF.md:80`).

## Residual Non-Blocking Risks

1. **V6 ACCEPT is still not pass convergence.** The governance text correctly
   says V6 ACCEPT would be only the first qualifying cycle after V5 REVISE, but
   the V6 consolidation must preserve that exact state. It may record this CH5
   ACCEPT as one lens disposition, not as S-P2 convergence or S-P3 authorization
   (`restart/prompts/ORCHESTRATOR.md:118`-`restart/prompts/ORCHESTRATOR.md:123`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:77`-`restart/skinny/tranches/sk-v8/HANDOFF.md:90`).

2. **SC-6-L1-R1 remains unratified totality-lock text.** SC-6 marks the Lock 1
   refinement as a Pass Omega candidate that does not bind the V1 spec until
   ratified (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:256`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:261`).
   SPEC handles the fork by requiring W3 either to follow Omega ratification or
   prove Lock 1 as written at challenge
   (`restart/skinny/tranches/sk-v8/SPEC.md:462`-`restart/skinny/tranches/sk-v8/SPEC.md:464`).

3. **`tape_vs_tape` is still a future coupling temptation.** It can become a
   useful plane-matched substrate row only after a later accepted plan names
   owner files, tests, LOC, rerun budget, and same-run structural-index
   competitor rows; until then it must not become SOTA evidence or W3's
   production consumer
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:326`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:346`,
   `restart/skinny/tranches/sk-v8/SPEC.md:125`-`restart/skinny/tranches/sk-v8/SPEC.md:131`).

4. **SC-5 has stale amendment-era wording about `S`.** SC-5 still says SPEC
   currently freezes `{A,C,G,K,L,N-direct}` and that adding `S` requires a
   REDRESS/SPEC amendment
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:220`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:232`).
   SPEC has already folded `S` while preserving that neither `K` nor `S` may
   support strict SOTA admission (`restart/skinny/tranches/sk-v8/SPEC.md:57`-`restart/skinny/tranches/sk-v8/SPEC.md:77`).
   This is stale explanatory prose, not a CH5 blocker.

## Required Folds If REVISE

None. Verdict is ACCEPT.
