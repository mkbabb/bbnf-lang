# S-P2 V7 CH5 - Hidden Coupling Review

Title: CH5 hidden-coupling review of the exceptional V7 substrate-ceiling
authorization packet.

Scope: current HEAD `4953d0a0af9e63b2b4ad7281822cf44198601ef1`
(`docs(sk-v8-p2-research): authorize exceptional substrate-ceiling V7`) after
the user-authorized exceptional V7 governance fold. Primary review surfaces:
`restart/prompts/ORCHESTRATOR.md`,
`restart/prompts/skinny/PASS-2-RESEARCH.md`,
`restart/skinny/tranches/sk-v8/SYNTHESIS.md`,
`restart/skinny/tranches/sk-v8/SPEC.md`,
`restart/skinny/tranches/sk-v8/HANDOFF.md`,
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md`,
and
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V6-CONSOLIDATED.md`.
The review targets ORCHESTRATOR §3W/§3Z, hidden coupling/no sidecar,
strict-vs-strict comparator discipline, Lock 14 grammar-neutrality, no new
directive/BIR/substrate, no deferrals, and no automatic S-P3 unless V7 is the
second consecutive qualifying ACCEPT.

Verdict: ACCEPT.

Confidence: 96%.

## Blockers

None.

## Hidden-Coupling Findings

1. **The V7 authorization is bounded, not a hidden convergence shortcut.**
   ORCHESTRATOR requires two consecutive qualifying ACCEPT cycles or an explicit
   user final pin before a pass advances, and bars the next pass until
   convergence holds (`restart/prompts/ORCHESTRATOR.md:118`-`restart/prompts/ORCHESTRATOR.md:123`).
   The S-P2 prompt repeats the two-cycle/user-pin rule and the V <= 5 normal
   hard ceiling (`restart/prompts/skinny/PASS-2-RESEARCH.md:155`-`restart/prompts/skinny/PASS-2-RESEARCH.md:162`).
   V6 consolidation recorded V6 as ACCEPT but not converged, with no S-P3 or
   implementation-wave authorization (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V6-CONSOLIDATED.md:7`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V6-CONSOLIDATED.md:18`).
   It then made exceptional V7 a legal next action only by explicit user
   authorization (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V6-CONSOLIDATED.md:69`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V6-CONSOLIDATED.md:79`).
   Current HEAD records exactly that: V7 is authorized as a challenge, not as a
   convergence shortcut (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:99`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:106`,
   `restart/skinny/tranches/sk-v8/SPEC.md:454`-`restart/skinny/tranches/sk-v8/SPEC.md:464`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:71`-`restart/skinny/tranches/sk-v8/HANDOFF.md:84`).
   SYNTHESIS now states the automatic S-P3 boundary cleanly: S-P3 may dispatch
   only if V7 returns the second consecutive qualifying ACCEPT cycle, or the
   user explicitly pins S-P2 final (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:195`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:199`).

2. **The substrate union remains a replacement, not a parallel sidecar.**
   CH5 rejects a parallel substrate, sidecar producer, renamed-scanner Lock 1
   violation, or Track 1 == Track 2 dishonesty
   (`restart/prompts/ORCHESTRATOR.md:83`-`restart/prompts/ORCHESTRATOR.md:88`).
   The S-P2 CH5 specialization rejects a second source scan, retained cursor,
   aux density table, or parser-owned structural projection
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:126`-`restart/prompts/skinny/PASS-2-RESEARCH.md:131`).
   SC-3 explicitly constrains the union to no parallel/sidecar substrate
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:14`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:19`)
   and defines the fold as one producer, one retained `Tape`, and one cursor
   identity, with `StructuralIndex` becoming the tape column rather than a
   free-standing sidecar
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:286`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:295`).
   Current SPEC preserves the adversarial gate: any source-byte second scan,
   retained cursor, aux table, density cache, or sidecar event vector running
   beside the offset tape fails Lock 1; only a projection that becomes the tape
   passes (`restart/skinny/tranches/sk-v8/SPEC.md:490`-`restart/skinny/tranches/sk-v8/SPEC.md:496`).

3. **No new directive, BIR variant, public substrate API, `BackendShape`, or
   `UnionTape` is admitted.** SPEC keeps the tranche non-negotiables explicit:
   no new BBNF directives, no new BIR variant, no new substrate without a
   same-wave consumer, no JSON policy in generic crates, strict-vs-strict only,
   no implementation wave before G-Alpha, and only W0 after G-Alpha
   (`restart/skinny/tranches/sk-v8/SPEC.md:180`-`restart/skinny/tranches/sk-v8/SPEC.md:192`).
   SC-6 keeps the co-routed surface to representation replacement of
   `OffsetTape` and retained `EventTape`, with no `UnionTape`, public substrate
   type, alternate materialisation surface, sixth `BackendShape`, BIR variant,
   BBNF directive, grammar-name branch, or public generic grammar/substrate API
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:300`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:322`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:657`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:666`).
   SYNTHESIS mirrors the same boundary: no `UnionTape` node, `BackendShape`, BIR
   variant, directive, or public substrate type
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:161`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:171`).

4. **Strict-vs-strict comparator discipline is preserved; sidecars stay
   planning signals.** SPEC separates same-run strict anchors from flaw probes
   and sidecar planning signals
   (`restart/skinny/tranches/sk-v8/SPEC.md:48`-`restart/skinny/tranches/sk-v8/SPEC.md:55`).
   Its executable strict-admission rule requires plane match, strict comparator
   status, same-run/native-anchor freshness, and in-row validation; deferred
   strictness, view-boundary UTF-8, stale sidecars, C++ sidecar-only evidence,
   or plane mismatch remain guard telemetry only
   (`restart/skinny/tranches/sk-v8/SPEC.md:117`-`restart/skinny/tranches/sk-v8/SPEC.md:123`).
   SC-5 demotes `parse_only` from the SOTA scoreboard while requiring the
   substrate-guard rows to retain positive and negative deltas, and routes
   `tape_vs_tape` as future plane-matched telemetry that cannot support SOTA
   admission until same-run structural-index competitors exist
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:179`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:216`).
   Current SPEC keeps `tape_vs_tape` residual, non-default, telemetry-only, and
   unavailable as W3's production same-wave consumer
   (`restart/skinny/tranches/sk-v8/SPEC.md:125`-`restart/skinny/tranches/sk-v8/SPEC.md:131`).

5. **Lock 14 grammar-neutrality remains a gate, not an assertion.** ORCHESTRATOR
   binds CH2 to Lock 14 grammar-neutrality across CSS L4, Sheets, and BBNF-self,
   not only JSON (`restart/prompts/ORCHESTRATOR.md:83`-`restart/prompts/ORCHESTRATOR.md:88`).
   SC-6's proposed Lock 1 refinement confines generic substrate code to
   generated byte sets and opaque ordinals; event-role meaning remains inside
   generated grammar modules keyed by parser state plus class/byte
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:263`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:287`).
   SPEC turns that into concrete scans and proof obligations: public API scan,
   grammar-branch scan, primitive/table scan, role/fact interpretation boundary,
   template/provider boundary, and non-JSON proof for CSS L4, Sheets, and
   BBNF-self when generic CostFacts, codegen, runtime, SIMD, or parser-template
   paths change (`restart/skinny/tranches/sk-v8/SPEC.md:247`-`restart/skinny/tranches/sk-v8/SPEC.md:269`).

6. **The packet does not defer implementation proof into an automatic W3 route.**
   CH6 forbids future-phase deferral
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:133`-`restart/prompts/skinny/PASS-2-RESEARCH.md:138`).
   Current SYNTHESIS states that the structural-projection hypothesis is not
   selected by S-P2, and W3 still needs W0/W1 closure, a fresh S-P3/W3 plan,
   owner paths, same-wave production consumer, revert protocol, thresholds,
   measured-path validation proof, and challenge acceptance
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:183`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:199`).
   SPEC repeats that W3 nomination does not select W3, and comparator telemetry
   such as `tape_vs_tape` cannot satisfy a parser/substrate primitive's
   production same-wave consumer
   (`restart/skinny/tranches/sk-v8/SPEC.md:447`-`restart/skinny/tranches/sk-v8/SPEC.md:464`).
   HANDOFF preserves the same no-dispatch boundary
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:84`-`restart/skinny/tranches/sk-v8/HANDOFF.md:93`).

7. **G-Alpha and Pass Omega boundaries are still separated from S-P2 V7.**
   G-Alpha remains limited to W0 only in SYNTHESIS, SPEC, and HANDOFF
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:271`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:277`,
   `restart/skinny/tranches/sk-v8/SPEC.md:594`-`restart/skinny/tranches/sk-v8/SPEC.md:601`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:182`-`restart/skinny/tranches/sk-v8/HANDOFF.md:188`).
   Pass Omega remains the owner of lock amendments and top-level CRUD; SK-V8 may
   cite SC-6-L1-R1 but does not amend `LOCKS.md` itself
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:252`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:268`).

## Disposition Of V6/V7 Governance

V6 governance is accepted as bounded: V6 returned 6/6 ACCEPT but was not
convergence because it was only the first qualifying ACCEPT after V5 REVISE
(`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V6-CONSOLIDATED.md:7`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V6-CONSOLIDATED.md:18`).
It closed the V5 CH4 hard-ceiling blocker only for the explicit V6 exception
and preserved strict-vs-strict, Lock 14, one-retained-substrate,
same-wave-consumer, scalar/checkasm, and no-directive/BIR/substrate boundaries
(`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V6-CONSOLIDATED.md:31`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V6-CONSOLIDATED.md:44`).

V7 governance is accepted as a second exceptional challenge authorization, not
an advance. HEAD records user authorization for V7 and says it is not a
convergence shortcut (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:99`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:106`,
`restart/skinny/tranches/sk-v8/SPEC.md:454`-`restart/skinny/tranches/sk-v8/SPEC.md:464`,
`restart/skinny/tranches/sk-v8/HANDOFF.md:71`-`restart/skinny/tranches/sk-v8/HANDOFF.md:84`).
This CH5 ACCEPT is only one lens disposition. Automatic S-P3 is lawful only
after the V7 consolidation records a second consecutive qualifying ACCEPT cycle
with zero open critical defects and no orphan unresolved REVISE, or after an
explicit user final pin.

## Residual Non-Blocking Risks

1. **V7 consolidation must not promote this single CH5 ACCEPT to pass
   convergence.** The governing rule is all-cycle convergence, not per-lens
   success (`restart/prompts/ORCHESTRATOR.md:118`-`restart/prompts/ORCHESTRATOR.md:123`;
   `restart/prompts/skinny/PASS-2-RESEARCH.md:155`-`restart/prompts/skinny/PASS-2-RESEARCH.md:162`).

2. **SC-6-L1-R1 remains unratified Lock 1 text.** SC-6 explicitly says the
   refinement is a Pass Omega candidate and does not bind the V1 spec until
   Omega ratifies it
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:252`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:261`).
   The current packet handles this by requiring W3 either to follow Omega
   ratification or prove Lock 1 as written at challenge
   (`restart/skinny/tranches/sk-v8/SPEC.md:466`-`restart/skinny/tranches/sk-v8/SPEC.md:468`).

3. **`tape_vs_tape` remains a coupling temptation.** It is a useful future
   plane-matched telemetry shape, but SC-5 and SPEC both require a later accepted
   plan with owner files, tests, LOC, rerun budget, and same-run
   structural-index competitors before it consumes wave scope or supports any
   strict adjudication
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:326`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:346`;
   `restart/skinny/tranches/sk-v8/SPEC.md:125`-`restart/skinny/tranches/sk-v8/SPEC.md:131`).

4. **The `S` outcome has moved ahead of SC-5's older amendment wording.** SC-5
   still describes `S` as an amendment against a frozen enum
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:220`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:232`),
   while SPEC has already folded `S` with the necessary non-admission boundary
   (`restart/skinny/tranches/sk-v8/SPEC.md:57`-`restart/skinny/tranches/sk-v8/SPEC.md:77`).
   This is stale explanatory prose, not a CH5 blocker, because strict admission
   remains forbidden for both `K` and `S`.

## Required Folds If REVISE

None. Verdict is ACCEPT.
