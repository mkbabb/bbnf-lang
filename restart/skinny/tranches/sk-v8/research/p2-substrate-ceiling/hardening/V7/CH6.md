# S-P2 V7 CH6 - Anti-Paper-Close Review

Title: CH6 anti-paper-close review of the user-authorized exceptional S-P2 V7.

Scope: current HEAD `4953d0a0`, with primary review of
`restart/skinny/tranches/sk-v8/SYNTHESIS.md`,
`restart/skinny/tranches/sk-v8/SPEC.md`,
`restart/skinny/tranches/sk-v8/HANDOFF.md`,
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`,
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V5-CONSOLIDATED.md`,
and `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V6-CONSOLIDATED.md`.
The review tests ORCHESTRATOR §3W/§3Z, S-P2 CH6 anti-paper-close, strict-vs-strict
comparator discipline, Lock 14 grammar-neutrality, Lock 1 substrate cardinality,
no new directive/BIR/substrate, no deferrals, and no automatic S-P3 unless V7 is
the second consecutive qualifying ACCEPT.

Verdict: ACCEPT.

Confidence: 95%.

## Blockers

None.

## Basis

1. **The V7 authorization is bounded and does not paper-close convergence.**
   ORCHESTRATOR requires two consecutive qualifying ACCEPT cycles with no open
   critical defects and no orphan unresolved REVISE, or an explicit user pin,
   before a pass advances (`restart/prompts/ORCHESTRATOR.md:118`-`restart/prompts/ORCHESTRATOR.md:123`).
   It also says V5 without convergence escalates to the user
   (`restart/prompts/ORCHESTRATOR.md:125`-`restart/prompts/ORCHESTRATOR.md:128`).
   V5 did escalate with CH4 REVISE and no S-P3 dispatch authorization
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V5-CONSOLIDATED.md:13`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V5-CONSOLIDATED.md:20`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V5-CONSOLIDATED.md:64`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V5-CONSOLIDATED.md:78`).
   V6 then ACCEPTed 6/6 but explicitly remained not converged and unauthorized
   for S-P3 (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V6-CONSOLIDATED.md:7`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V6-CONSOLIDATED.md:18`).
   Current HEAD records the May 18 authorization as an exceptional V7 challenge,
   not as a shortcut (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:96`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:106`,
   `restart/skinny/tranches/sk-v8/SPEC.md:454`-`restart/skinny/tranches/sk-v8/SPEC.md:464`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:71`-`restart/skinny/tranches/sk-v8/HANDOFF.md:84`).

2. **No S-P3 or W3 dispatch is implied by the research nomination.** S-P2 is a
   research pass that selects nothing and sequences nothing
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:3`-`restart/prompts/skinny/PASS-2-RESEARCH.md:11`).
   Current HEAD says S-P3 may dispatch only if V7 returns the second consecutive
   qualifying ACCEPT, or if the user explicitly pins S-P2 final
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:195`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:199`).
   W3 still requires W0/W1 closure, a fresh plan, exact owner paths, same-wave
   production consumer, revert protocol, measurement thresholds, measured-path
   validation proof, and challenge acceptance
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:183`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:193`,
   `restart/skinny/tranches/sk-v8/SPEC.md:447`-`restart/skinny/tranches/sk-v8/SPEC.md:452`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:85`-`restart/skinny/tranches/sk-v8/HANDOFF.md:93`).

3. **Strict-vs-strict comparator discipline remains intact.** ORCHESTRATOR bans
   strict admission from permissive rows
   (`restart/prompts/ORCHESTRATOR.md:207`-`restart/prompts/ORCHESTRATOR.md:211`).
   SPEC separates same-run strict anchors from flaw probes and sidecar planning
   signals (`restart/skinny/tranches/sk-v8/SPEC.md:44`-`restart/skinny/tranches/sk-v8/SPEC.md:55`),
   and its executable refusal rule rejects strict admission unless comparator
   plane, strictness, freshness/native-anchor status, and measured-row validation
   all hold (`restart/skinny/tranches/sk-v8/SPEC.md:117`-`restart/skinny/tranches/sk-v8/SPEC.md:123`).
   SYNTHESIS likewise keeps same-run strict evidence separate from simdjson/yyjson
   sidecars and historical rows (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:127`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:135`).

4. **Lock 14 and grammar-neutrality are live gates, not deferred assurances.**
   SPEC requires public API scans, grammar-branch scans, primitive/table scans,
   role/fact-boundary proof, template/provider-boundary proof, and non-JSON proof
   for CSS L4, Sheets, and BBNF-self when generic crates are edited
   (`restart/skinny/tranches/sk-v8/SPEC.md:247`-`restart/skinny/tranches/sk-v8/SPEC.md:269`).
   SYNTHESIS requires generated per-grammar byte-set tables plus opaque ordinals,
   with grammar meaning interpreted only inside generated grammar modules
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:161`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:171`,
   `restart/skinny/tranches/sk-v8/SYNTHESIS.md:213`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:219`).
   SC-6 generalises through JSON, CSS L4, Sheets, and arbitrary user grammars via
   generated byte sets and opaque class ordinals rather than generic grammar roles
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:418`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:535`).

5. **The no-new-substrate, no-new-directive, and no-new-BIR boundaries hold.**
   SPEC states the non-negotiables directly: no new BBNF directives, no new BIR
   variant, no new substrate without a same-wave consumer, no JSON policy in
   generic crates, and strict-vs-strict only for admission
   (`restart/skinny/tranches/sk-v8/SPEC.md:180`-`restart/skinny/tranches/sk-v8/SPEC.md:190`).
   SYNTHESIS says the admitted fold is representation replacement of `OffsetTape`
   and retained `EventTape`, not a new substrate node, `BackendShape`, BIR
   variant, directive, or public substrate type
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:161`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:171`).
   SC-6 marks SC-6-L1-R1 as a Pass Omega candidate that does not bind V1 until
   ratified (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:247`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:261`),
   and it explicitly rejects `UnionTape`, a new `BackendShape`, a BIR variant, a
   BBNF directive, public substrate API, public generic grammar API, and
   grammar-name branches
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:301`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:328`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:657`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:666`).

6. **The packet demotes unresolved evidence instead of deferring closure.**
   S-P2 CH6 rejects candidate claims deferred to a future wave
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:133`-`restart/prompts/skinny/PASS-2-RESEARCH.md:138`).
   Current HEAD demotes string-density/knee evidence to diagnostic telemetry
   until a later gate names the row set, formula, numeric target, maintain budget,
   and pass/fail rule (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:127`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:135`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:66`-`restart/skinny/tranches/sk-v8/HANDOFF.md:69`).
   It also keeps `tape_vs_tape` as routed residual telemetry, not a W3 production
   same-wave consumer (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:149`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:159`,
   `restart/skinny/tranches/sk-v8/SPEC.md:125`-`restart/skinny/tranches/sk-v8/SPEC.md:131`).

## Disposition Of V6/V7 Governance

V6 governance is accepted and preserved: V6 closed the V5 CH4 authorization
blocker for that cycle, returned 6/6 ACCEPT, and still did not converge because
it was only the first qualifying ACCEPT after V5 REVISE
(`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V6-CONSOLIDATED.md:31`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V6-CONSOLIDATED.md:44`,
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V6-CONSOLIDATED.md:46`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V6-CONSOLIDATED.md:68`).

V7 governance is accepted for CH6: current HEAD authorizes an exceptional V7
challenge and states that S-P2 may advance only if V7 actually returns the second
consecutive qualifying ACCEPT, or if the user explicitly pins S-P2 final
(`restart/skinny/tranches/sk-v8/SYNTHESIS.md:195`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:199`).
This CH6 ACCEPT is one lens disposition only; the V7 consolidation must still
count CH1-CH6, record zero open critical defects and no orphan unresolved REVISE,
and only then may treat V7 as the second qualifying ACCEPT under ORCHESTRATOR §3Z
(`restart/prompts/ORCHESTRATOR.md:110`-`restart/prompts/ORCHESTRATOR.md:123`).

## Residual Non-Blocking Risks

1. **V7 consolidation wording remains the main paper-close hazard.** The
   consolidator must say "V7 returned the second consecutive qualifying ACCEPT"
   only if all six V7 lenses ACCEPT and there are no open critical defects or
   orphan unresolved REVISE; a CH6 ACCEPT alone is not pass convergence
   (`restart/prompts/ORCHESTRATOR.md:118`-`restart/prompts/ORCHESTRATOR.md:123`).

2. **Pass Omega timing still matters for Lock 1.** SC-6-L1-R1 remains a
   skinny-track Pass Omega candidate until ratified; W3 must either consume the
   ratified refinement or prove Lock 1 as written while routing the Omega residual
   explicitly
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:685`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:695`,
   `restart/skinny/tranches/sk-v8/SYNTHESIS.md:250`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:269`).

3. **SC-6's grep-style Lock 14 verification is research evidence, not future
   implementation proof.** Later generic-crate edits still need live public API,
   grammar-branch, primitive/table, template/provider, and non-JSON proof scans
   at wave close (`restart/skinny/tranches/sk-v8/SPEC.md:247`-`restart/skinny/tranches/sk-v8/SPEC.md:269`;
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:508`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:535`).

4. **Tier A can still be oversold later as string-plane closure.** Current HEAD
   prevents that by keeping Tier A to structural-class cursor migration and
   routing string-boundary, quote/backslash/parity, CostFacts-template, and
   non-JSON production migration to Tier B
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:137`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:147`,
   `restart/skinny/tranches/sk-v8/SPEC.md:431`-`restart/skinny/tranches/sk-v8/SPEC.md:439`).

## Required Folds If REVISE

N/A. Verdict is ACCEPT.
