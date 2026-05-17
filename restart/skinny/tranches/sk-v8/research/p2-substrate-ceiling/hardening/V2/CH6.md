# CH6 - Anti-Paper-Close Review (S-P2 V2)

Role: CH6 (Anti-Paper-Close)

Verdict: REVISE

Score: 82/100

## Blocking Findings

1. **S-P2 convergence is still collapsible to a single challenge cycle.**

   The orchestrator requires two consecutive ACCEPT cycles, with no unresolved
   REVISE, before a pass advances (`restart/prompts/ORCHESTRATOR.md:118`-`restart/prompts/ORCHESTRATOR.md:123`), and the S-P2 prompt repeats that S-P2 advances to S-P3 only at `>=95% ACCEPT for two consecutive cycles` or explicit user pin (`restart/prompts/skinny/PASS-2-RESEARCH.md:155`-`restart/prompts/skinny/PASS-2-RESEARCH.md:158`). V1 did not converge (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md:5`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md:7`). The folded handoff says the next framework step is the six-lens CHALLENGE wave "then S-P3 Synthesis-Plan" (`restart/skinny/tranches/sk-v8/HANDOFF.md:67`-`restart/skinny/tranches/sk-v8/HANDOFF.md:70`) while also saying SC-1..SC-6 authorize no W3 plan by themselves (`restart/skinny/tranches/sk-v8/HANDOFF.md:72`-`restart/skinny/tranches/sk-v8/HANDOFF.md:75`). That leaves an enforceability gap: a clean V2 can be mistaken for S-P2 convergence even though it is only the first possible ACCEPT cycle after V1 REVISE.

2. **Strict-vs-strict cleanup is incomplete in SC-1 and SC-5.**

   The governing rule is strict-vs-strict only for admission; permissive or stale rows are flaw probes or planning signals (`restart/prompts/skinny/PASS-2-RESEARCH.md:214`-`restart/prompts/skinny/PASS-2-RESEARCH.md:219`, `restart/skinny/tranches/sk-v8/SPEC.md:44`-`restart/skinny/tranches/sk-v8/SPEC.md:52`). SC-1 still labels one table `Delta vs sonic-strict` while mixing "no strict anchor", SK-V6, and lossy/sidecar evidence into the same win/loss summary (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:99`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:114`), then later cites numbers and marine_ik as parse wins in the same evidence family (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:188`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:190`). SC-5 says bbnf is faster than sonic-rs strict on seven rows, but the list includes rows SC-4 marks as historical or non-strict planning signals, including numbers, marine_ik, instruments, and unicode_escapes (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:42`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:47`; compare `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:178`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:183`). This is not yet a false implementation dispatch, but it is still paper-close rhetoric because the folded evidence lets non-admission rows wear strict-win language.

3. **SC-4 keeps an unmeasurable admission metric.**

   SC-4 says success is moving the string-fraction knee from about 0.14 "toward" 1.0 (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:307`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:310`) and calls that knee-displacement metric the admission criterion before implementation (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:372`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:375`). The packet-level W3 gate correctly requires selected rows, declared thresholds, and a full-table maintain budget (`restart/skinny/tranches/sk-v8/SPEC.md:423`-`restart/skinny/tranches/sk-v8/SPEC.md:429`), but SC-4 does not name an in-repo command, row set, threshold value, or pass/fail formula for the knee. "Toward 1.0" is a direction, not an acceptance criterion.

## Non-Blocking Notes

- The V1 paper-promotion issue is mostly folded: SYNTHESIS now calls the union a lead hypothesis and says it is not selected by S-P2 (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:159`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:167`), SPEC repeats that W3 selection requires W0/W1 closure, owner paths, same-wave consumer, thresholds, and challenge acceptance (`restart/skinny/tranches/sk-v8/SPEC.md:405`-`restart/skinny/tranches/sk-v8/SPEC.md:410`), and HANDOFF says SC-1..SC-6 authorize no W3 plan by themselves (`restart/skinny/tranches/sk-v8/HANDOFF.md:67`-`restart/skinny/tranches/sk-v8/HANDOFF.md:75`).
- G-Alpha sequencing is strong at the packet level: G-Alpha closed authorizes only W0, and W1-W6 require W0 closure plus plan augmentation (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:239`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:245`, `restart/skinny/tranches/sk-v8/SPEC.md:534`-`restart/skinny/tranches/sk-v8/SPEC.md:541`, `restart/skinny/tranches/sk-v8/HANDOFF.md:5`-`restart/skinny/tranches/sk-v8/HANDOFF.md:7`).
- The Lock 1/Omega fork is concrete enough for CH6: W3 either waits for Pass Omega ratification or proves Lock 1 as written and routes the Omega residual (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:226`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:237`, `restart/skinny/tranches/sk-v8/SPEC.md:412`-`restart/skinny/tranches/sk-v8/SPEC.md:415`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:647`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:657`).
- SC-3 Tier A excludes non-JSON grammar implementation work (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:361`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:367`). That is acceptable only if the W3 plan still applies the SPEC Section 2.1 non-JSON proof gate for generic edits (`restart/skinny/tranches/sk-v8/SPEC.md:227`-`restart/skinny/tranches/sk-v8/SPEC.md:244`).

## Required Fold Actions

1. **Add an explicit S-P2 convergence guard to the packet and handoff.** State that a V2 ACCEPT cycle does not dispatch S-P3 unless it is the second consecutive ACCEPT cycle or the user explicitly pins S-P2 final. If V2 is the first ACCEPT after V1 REVISE, the next action is another S-P2 cycle/challenge or user pin, not automatic S-P3.

2. **Rewrite SC-1 and SC-5 strictness wording.** Split strict same-run rows from historical SK-V6 rows and sidecar/permissive rows. Do not call non-strict rows strict wins. Use "planning signal" or "diagnostic signal" outside the strict same-run plane.

3. **Replace SC-4's knee language with an executable gate or demote it.** Either define an in-repo row set, formula, command, numeric target, maintain budget, and pass/fail rule, or state that knee movement is diagnostic only and cannot be an admission criterion.

4. **Preserve the Lock 14 proof burden for Tier A.** If W3 touches generic runtime, SIMD, codegen, or tape paths, the plan must include the SPEC Section 2.1 CSS L4, Sheets, and BBNF-self proof even if non-JSON implementation is out of Tier A scope.
