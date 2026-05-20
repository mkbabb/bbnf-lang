# SK-V12 Pass Alpha CHALLENGE V1 - CH6 Anti-Paper-Close / Next-Tranche Impact

Pass: Pass Alpha SK-V11 -> SK-V12 re-bracket.
Cycle: V1.
Lens: CH6 - Anti-paper-close / next-tranche impact.
Date: 2026-05-20.
Disposition: REVISE.

## Scope

This review evaluates the pin-aware Alpha A-F packet against the USER PIN and
Pass Alpha CH6 duties. It asks whether SK-V12 can be presented at G-Alpha
without a paper close, without future-phase CSS promises, and without a
relinquish gap between Alpha and the next skinny passes.

Authorities read:

- `restart/prompts/ORCHESTRATOR.md` Section 3W and Section 3Z.
- `restart/prompts/pass-contracts/PASS-ALPHA.md` Section 3 and Section 7.
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`.
- Alpha A-F under `restart/skinny/tranches/sk-v12/research/alpha/`.
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`.
- `restart/skinny/tranches/sk-v12/HANDOFF.md`.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md` through REDRESS 120.

## Disposition Summary

| Axis | Disposition | Blocking? |
|---|---|---|
| No paper CSS close | ACCEPT | no |
| No future-phase CSS promise | ACCEPT | no |
| ADMIT / FIXPOINT measurability | REVISE | yes, comparator inequality mismatch |
| G-Alpha presentation seed | REVISE | yes |
| S-P3 next move / no relinquish gap | REVISE | yes |
| Rollback protocols | REVISE | yes, incomplete rejected-patch coverage |
| Triumvirate boundary | ACCEPT | no |

Overall CH6 disposition is REVISE. The pin-aware Alpha packet correctly
replaces the obsolete Sheets-first route with CSS L4 and blocks future-phase
CSS promises, but it is not ready for G-Alpha because it still leaves concrete
next-tranche control gaps.

## Critical Findings

### CH6-1 - The next move skips the required skinny pass sequence

Disposition: REVISE.

`PASS-ALPHA.md` says that after G-Alpha closes, the orchestrator can dispatch
SK-V{N+1} P1, the first skinny pass of the new iteration
(`restart/prompts/pass-contracts/PASS-ALPHA.md:167`-`178`). The per-wave
triumvirate contract also places wave execution beneath S-P3, after the skinny
P1/P2/P3 track has produced `SPEC.md`
(`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:3`-`5`). The current
pin-aware packet instead repeatedly states that the next move is S-P3:

- `alpha-F-contract-draft.md:203`
- `SYNTHESIS.md:207`-`222`
- `HANDOFF.md:91`-`109`
- `HANDOFF.md:140`-`144`

That is a next-tranche relinquish gap. It lets SK-V12 jump straight to W1
planning even though the user pin requires fresh CSS L4 profile/research
authority before S-P3 scopes the wave. It also risks reusing pre-pin S-P1/P2
facts as if they were CSS-targeted gate evidence.

Required fold:

- Replace "Next move: S-P3 re-derives W1 under the pin" with the pass-order
  sequence: G-Alpha -> SK-V12 S-P1 Profile under the pin -> S-P2 Research under
  the pin -> S-P3 Synthesis-Plan under the pin -> wave triumvirates.
- S-P3 may still own the W1 plan, but only after S-P1 and S-P2 either rerun or
  record explicit pin-aware revalidation evidence with fresh CSS L4 profile,
  lightningcss comparator, JSON guard, union, and ASM-gen inputs.
- If the orchestrator chooses to reuse any pre-pin S-P1/P2 artifact, the reuse
  must be a measured revalidation decision, not an implicit shortcut.

### CH6-2 - Candidate hard caps are still absent

Disposition: REVISE.

`PASS-ALPHA.md` assigns CH6 to verify revert protocols, hard caps,
triumvirate discipline, and bench-verifiable goalsets
(`restart/prompts/pass-contracts/PASS-ALPHA.md:33`-`49`). The G-Alpha
presentation must include rows targeted, interventions, LOC budget, hard caps,
and pre-blocked routes (`restart/prompts/pass-contracts/PASS-ALPHA.md:167`-
`174`). `ORCHESTRATOR.md` requires every dispatch to carry an explicit minute
cap and halt/escalate at the cap (`restart/prompts/ORCHESTRATOR.md:218`-`227`).

Alpha-E now has useful LOC and risk data for E1-E5
(`alpha-E-candidate-shortlist.md:52`-`60`), and each candidate repeats a LOC
budget later (`alpha-E-candidate-shortlist.md:112`-`120`,
`159`-`163`, `196`-`200`, `246`-`251`, `302`-`306`). It still does not state
candidate-level minute caps. Alpha-F's G-Alpha seed likewise omits hard caps
(`alpha-F-contract-draft.md:194`-`205`).

Required fold:

- Add a hard-cap matrix for E1-E5. It can use the current campaign default
  caps if that is the selected policy, but the minute numbers must be explicit.
- Carry those caps into the G-Alpha seed with LOC, risk, and pre-blocked-route
  summary.
- State that S-P3 cannot widen a candidate cap without an explicit extension
  decision.

### CH6-3 - The CSS admission inequality is inconsistent

Disposition: REVISE.

The USER PIN raises the CSS close target to generated CSS L4 Track 1 beating
`lightningcss_mbps + 1`. Alpha-F and SYNTHESIS use the strict form:

- `alpha-F-contract-draft.md:70`-`73`
- `SYNTHESIS.md:39`-`41`
- `HANDOFF.md:67`-`69`

Alpha-E's candidate gates use `>= lightningcss_mbps + 1` for E1, E4, and E5
(`alpha-E-candidate-shortlist.md:101`-`107`,
`236`-`243`, `294`-`301`). That mismatch can admit a borderline row under the
candidate gate while the contract-level close condition would not admit it.

Required fold:

- Normalize every CSS L4 candidate gate to the exact close expression:
  `generated_track1_mbps > lightningcss_mbps + 1`.
- If S-P3 wants an integer floor interpretation instead, it must state the
  rounding rule once and make Alpha-E, SYNTHESIS, HANDOFF, RESULTS, and the
  gate consumer use the same rule.

### CH6-4 - Rollback coverage is present but not uniform enough for redress

Disposition: REVISE.

The earlier revert-protocol gap is materially improved: Alpha-E now names a
rollback slice for each candidate. E1 and E4 explicitly include source,
generated output, report/gate/RESULTS/REDRESS, and a rejected-patch path
(`alpha-E-candidate-shortlist.md:118`-`120`,
`249`-`251`). E2, E3, and E5 describe rollback, but do not uniformly require
the failed-wave rejected patch artifact (`alpha-E-candidate-shortlist.md:162`-
`163`, `199`-`200`, `304`-`306`).

The triumvirate failure protocol requires a REDRESS entry with measurement
evidence and the reverted patch saved at
`/tmp/skv{N}-wave{W}-rejected.patch`
(`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:65`-`75`).

Required fold:

- Every E1-E5 candidate must say that a failed redress saves
  `/tmp/skv12-waveW{n}-rejected.patch` or the exact wave-specific equivalent.
- E2 must state whether a Lock 14 failure rolls back E1 generated CSS output
  and blocks CSS emission until replanned.
- E3 must state that failure keeps all later SIMD/ASM candidates blocked.
- E5 must state whether a parity-pass / row-miss result removes the native body,
  demotes it to inventory, or records an admitted non-orphan consumer.

### CH6-5 - The G-Alpha presentation seed is too thin

Disposition: REVISE.

Alpha-F has the right high-level G-Alpha seed: CSS L4 target, `lightningcss + 1`
floor, fallback discipline, W0 revalidation, and fixpoint requirements
(`alpha-F-contract-draft.md:194`-`205`). SYNTHESIS repeats the ADMIT/FIXPOINT
summary (`SYNTHESIS.md:224`-`231`). But `PASS-ALPHA.md` requires the
presentation to include targeted rows, interventions, LOC budget, hard caps,
pre-blocked routes, and predicted close state
(`restart/prompts/pass-contracts/PASS-ALPHA.md:169`-`174`).

The current G-Alpha seed does not list E1-E5 with LOC, hard cap, pre-blocked
REDRESS adjacency, rollback action, or predicted close outcome. That makes it
impossible to present a complete G-Alpha packet without the orchestrator
assembling extra unstated context.

Required fold:

- Add a compact G-Alpha table covering E1-E5: target row, role, LOC, minute
  cap, REDRESS adjacency, close contribution, and failure action.
- Name the predicted close state explicitly:
  `ADMIT if E1/E2 close CSS L4 > lightningcss`, or `FIXPOINT only after CSS,
  union-substrate, and ASM-gen measured attempts plus zero orphan primitives`.
- State that stale `SPEC.md` / `DISPATCH-PROMPT.md` are not dispatch authority
  until S-P3 regenerates them under the pin.

## Accepted Findings

### CH6-6 - The packet does not paper-close CSS L4

Disposition: ACCEPT.

Alpha-A says there is no current generated CSS L4 row, no lightningcss number,
and no generated CSS runtime (`alpha-A-results-extraction.md:52`-`72`,
`229`-`237`). Alpha-B makes the CSS competitor delta `UNMEASURED` until the
same-host lightningcss fields exist (`alpha-B-competitor-deltas.md:79`-`99`).
Alpha-C blocks report-only and future-phase closes
(`alpha-C-redress-digest.md:120`-`130`, `230`-`246`). Alpha-F's ADMIT route
requires generated Track 1, strict equality, gate-consumed provenance, Lock 14,
Lock 16, and JSON guard state (`alpha-F-contract-draft.md:66`-`91`).

That is not a paper close. The packet names the absence honestly and makes the
bench row load-bearing.

### CH6-7 - Future-phase CSS promises are blocked

Disposition: ACCEPT.

The packet correctly demotes Sheets and BBNF-self to post-CSS-redress fallbacks:
Alpha-C says CSS L4 must be attempted first and future-phase CSS promises stay
blocked (`alpha-C-redress-digest.md:123`-`130`); Alpha-D invalidates the
pre-pin Sheets plan (`alpha-D-validated-invalidated.md:124`-`138`);
Alpha-E excludes Sheets and BBNF-self from the shortlist until after CSS
redress (`alpha-E-candidate-shortlist.md:308`-`316`); HANDOFF refuses skipping
CSS L4 before a CSS redress attempt (`HANDOFF.md:125`-`138`).

### CH6-8 - FIXPOINT is measurable, not rhetorical

Disposition: ACCEPT, subject to CH6-1/CH6-2/CH6-3 folds.

The FIXPOINT route requires a measured CSS L4 redress attempt, a new measured
union-substrate implementation attempt, a new measured ASM-gen attempt, fresh
profile/microbench/parity/consumer evidence, zero orphan production primitives,
and REDRESS entries for misses (`alpha-F-contract-draft.md:92`-`112`;
`SYNTHESIS.md:61`-`80`; `HANDOFF.md:75`-`83`). This satisfies the user pin's
campaign-fixpoint structure. It remains blocked only because the pass sequence,
hard-cap, and inequality defects above must be folded.

### CH6-9 - Triumvirate boundaries are preserved

Disposition: ACCEPT.

Alpha-F correctly refuses to edit `SPEC.md` or `DISPATCH-PROMPT.md` and marks
them stale until downstream S-P3 rewrites them
(`alpha-F-contract-draft.md:207`-`209`). SYNTHESIS and HANDOFF say the current
Alpha packet authorizes no source work and leaves implementation authority to
the later S-P3/wave packet (`SYNTHESIS.md:207`-`222`; `HANDOFF.md:140`-`144`).

The boundary is correct; the required fold is to put S-P1 and S-P2 back in
front of S-P3 rather than dispatching S-P3 directly.

## Required V2 Fold

Pass Alpha V1 cannot converge under CH6. V2 must:

1. Reword the next-tranche sequence to G-Alpha -> S-P1 -> S-P2 -> S-P3 -> waves.
2. Add explicit minute hard caps for E1-E5 and carry them into G-Alpha.
3. Normalize CSS admission gates to the same strict `> lightningcss_mbps + 1`
   expression, or define one rounding rule consumed by every gate document.
4. Make rollback/rejected-patch evidence uniform for all E1-E5 candidates.
5. Expand the G-Alpha seed into a presentation-ready intervention table.

Until those folds land, any G-Alpha presentation would require unstated
orchestrator interpretation and would fail CH6 anti-paper-close discipline.
