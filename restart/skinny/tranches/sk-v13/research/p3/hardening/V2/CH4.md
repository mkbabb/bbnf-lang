# SK-V13 S-P3 V2 CH4 Cost Challenge

Lens: CH4 cost, wave alignment, hard caps, bracket accounting, and same-wave-consumer budget.
Commit under review: `9f8bbfce5`.
Disposition vocabulary: ACCEPT / REVISE / REJECT.

## Verdict

ACCEPT.

The V1 CH4 fold landed. S-P3 requires every wave to carry LOC budget, hard cap,
phase breakdown, same-wave-consumer requirement, shortlist <=8, and a wave count
under the skinny-bracket ceiling (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:128`-`:132`).
The orchestrator likewise binds CH4 to realistic LOC/risk/wave/hard-cap and
same-wave consumer checks, and escalates brackets over 12 waves
(`restart/prompts/ORCHESTRATOR.md:83`-`:88`,
`restart/prompts/ORCHESTRATOR.md:125`-`:128`). V2 now supplies a single
dispatch authority, bracket-forward accounting, subwave accounting, hard caps,
and same-wave consumer/revert/cost rules sufficient for CH4.

## Fold Items

| Fold item | Verdict | Evidence |
|---|---|---|
| One canonical manifest | ACCEPT | V1 required one manifest rather than split P3-B W0-W11 vs SPEC/DISPATCH W0-W15 accounting (`restart/skinny/tranches/sk-v13/research/p3/hardening/V1/CH4.md:73`-`:76`). P3-B now says SPEC/DISPATCH W0-W15 identifiers are canonical and the P3-B W0-W11 table is V1 packing rationale only (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:10`-`:18`). SPEC declares the canonical V2 dispatch manifest (`restart/skinny/tranches/sk-v13/SPEC.md:314`-`:320`), and DISPATCH mirrors that rule (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:188`-`:194`). |
| Bracket-ceiling and subwave accounting | ACCEPT | SPEC states W10.N, W11.N, and W14.N are planning subwave series until a concrete triumvirate is declared, then every real subwave counts against active skinny-bracket accounting and overflow closes `REJECT-BRACKET` with SK-V14 bracket-forward, dropping no pinned row (`restart/skinny/tranches/sk-v13/SPEC.md:314`-`:320`). DISPATCH repeats the same real-subwave accounting and overflow rule (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:184`-`:194`). P3-B also preserves overflow/bracket-forward accounting at the alias layer (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:16`-`:18`). |
| Hard caps and 45-minute consistency | ACCEPT | The user pin raises redress from 30 to 45 minutes for W5-W9 and W12, with research/plan unchanged at 20/15 (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:129`-`:134`). SPEC's manifest gives LOC and redress caps per wave, including W5-W9/W12 at 45+15 and other waves at 30+15 (`restart/skinny/tranches/sk-v13/SPEC.md:322`-`:340`), and its phase table restates research 20, plan 15, and W5-W9/W12 redress 45+15 (`restart/skinny/tranches/sk-v13/SPEC.md:347`-`:354`). DISPATCH uses the same redress cap rule (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:53`-`:59`). P3-B's decision/union/SIMD 45-minute source-edit note is compatible with the canonical SPEC/DISPATCH rule (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:75`-`:80`). |
| Per-wave consumer, revert, and cost rule | ACCEPT | SPEC requires every wave plan to carry exact owner paths, thresholds, revert slice, same-wave consumer, and pre-block list before redress (`restart/skinny/tranches/sk-v13/SPEC.md:41`-`:43`). DISPATCH rejects any wave packet missing same-wave consumer, LOC cap, phase cap, rerun ceiling, revert slice, or RESULTS/REDRESS/rolling updates (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:65`-`:86`). SPEC sections now carry consumer/revert coverage for W0-W9 (`restart/skinny/tranches/sk-v13/SPEC.md:434`-`:438`, `:470`-`:476`, `:505`-`:508`, `:531`-`:537`, `:559`-`:562`, `:595`-`:601`, `:632`-`:638`, `:669`-`:676`, `:707`-`:713`, `:748`-`:757`) and for W10.N/W11.N/W12/W13/W14.N/W15 (`restart/skinny/tranches/sk-v13/SPEC.md:780`-`:784`, `:816`-`:820`, `:833`-`:854`, `:882`-`:886`, `:918`-`:922`, `:934`-`:951`). |
| Same-wave consumer minimums for real subwaves | ACCEPT | V1 asked for explicit W10.N, W11.N, W13, and W14.N consumer lines (`restart/skinny/tranches/sk-v13/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:80`-`:82`). DISPATCH now names the required consumers for W10.N, W11.N, W13, and W14.N (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:203`-`:210`), and SPEC mirrors those same consumer lines in each section (`restart/skinny/tranches/sk-v13/SPEC.md:780`-`:784`, `:816`-`:820`, `:882`-`:886`, `:918`-`:922`). |
| SIMD/union zero-orphan same-wave closure | ACCEPT | V1 required `orphan_count_after = 0`, delete/demote/revert, strict checkasm, and production consumer evidence for every SIMD-touching section including W9/C3 union (`restart/skinny/tranches/sk-v13/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:75`-`:78`). SPEC now makes those same-wave predicates binding for W9 if it touches `bbnf-simd` or selects C3, and forbids relying on W12 later (`restart/skinny/tranches/sk-v13/SPEC.md:748`-`:751`). DISPATCH repeats the SIMD same-wave zero-orphan rule and rejects later W12 cleanup as an admissible dependency (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:212`-`:217`). |
| Per-wave RESULTS/REDRESS consumer/revert/cost handling | ACCEPT | P3-B serializes RESULTS, REDRESS, and rolling-delta writes through one finalizer per wave/subwave and saves failed lanes outside ledger edits (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:217`-`:227`). DISPATCH outcome rules require admit updates, reject reverts, REDRESS append, routed differential/block evidence, and same-wave consumer evidence (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:253`-`:267`). |

## Evidence

- Read PASS-3, ORCHESTRATOR, P3-A through P3-F, SPEC, DISPATCH, V1 CH4, and the V1 consolidation.
- The reviewed commit's stated intent is the V1 challenge fold; it reports canonical wave accounting, same-wave consumers, row-movement/architectural-block gates, and SIMD zero-orphan closure in `SPEC.md` and `DISPATCH-PROMPT.md` (`git show --format=fuller 9f8bbfce5`).
- CH4 residual risk: P3-B still contains its original W0-W11 cost table, but its V2 fold note explicitly downgrades that table to non-dispatch V1 packing rationale and assigns canonical dispatch authority to SPEC/DISPATCH (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:10`-`:18`). That is not a CH4 blocker because SPEC/DISPATCH now own dispatch accounting.

## Local Check

PASS: `git diff --check -- restart/skinny/tranches/sk-v13/research/p3/hardening/V2/CH4.md` produced no output.
