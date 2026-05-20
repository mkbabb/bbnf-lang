# SK-V11 S-P3 CHALLENGE V2 CH4 Cost / Wave Budget

Pass: S-P3 Synthesis-Plan CHALLENGE.
Cycle: V2.
Date: 2026-05-20.
Scope: cost, hard-cap, sub-wave, same-wave consumer, checkasm/micro-proof, row-count, and close-wave budget review for SK-V11.
Output: this file.
Disposition: ACCEPT.
Accept rate contribution: 1.

## Verdict

ACCEPT. The V2 packet folds the V1 CH4 cost defects. The bracket is now
explicitly W0, W1a, W1b, W2-W9: 11 waves with one spare split before the
skinny `> 12` escalation rule. W1 is split into a gate/report schema lane and
a generated baseline/oracle lane. Budgets distinguish handwritten
source/test/gate LOC from regenerated output. Behavior waves have row-count
caps and micro-prove-first entry gates, and W8 source work is no longer hidden
inside close accounting.

## Fold Checks

| V1 CH4 defect | V2 status | Evidence |
|---|---|---|
| W1 split | FOLDED | P3-B declares W1a as non-JSON gate/report and W1b as generated baseline/oracle, with W1b blocking W2 (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:67-69`, `:80-84`). SPEC gives W1a and W1b separate sections, owner paths, gates, and no-row-admit boundaries (`restart/skinny/tranches/sk-v11/SPEC.md:283-377`). |
| 11-wave bracket with one spare slot | FOLDED | P3-B states the bracket is 11 waves and has exactly one spare slot for either W8a or one behavior-wave rescue (`p3b-wave-sequencing.md:43-46`). SPEC and DISPATCH repeat the same budget and escalation boundary (`SPEC.md:206-211`, `DISPATCH-PROMPT.md:65-72`). |
| Separate handwritten/generated budgets | FOLDED | SPEC separates handwritten source/test/gate caps from regenerated-output caps for W1a-W8 (`SPEC.md:187-199`) and requires generated output to come from named generator/schema inputs (`SPEC.md:178-179`, `:243-244`). DISPATCH mirrors the split (`DISPATCH-PROMPT.md:51-63`). |
| Row-count caps | FOLDED | W3 defaults to one or two rows unless microbench data justifies more; W4 is capped to at most three rows; W5 selects one string/key consumer and at most two target rows; W6 is limited to the three Unicode/escape rows; W7 is limited to a bounded post-W6 profiled subset (`p3b-wave-sequencing.md:107-111`, `SPEC.md:451-454`, `:506-509`, `:520-523`, `:557-559`, `:570-573`, `:610-630`, `:660-676`). |
| W8 source split trigger | FOLDED | P3-B makes W8 docs/gate/result accounting by default and requires source work to be CHALLENGE-accepted W8a consuming the spare slot (`p3b-wave-sequencing.md:75`). SPEC states W8 source work is outside default W8, must route to W8a first with one candidate and one row subset, and only then W8 remains accounting (`SPEC.md:198`, `:693-720`). |
| Mandatory W1a/W1b CHALLENGE | FOLDED | P3-B states W1a and W1b are CHALLENGE-gated because they establish first non-JSON gate/report and baseline authority (`p3b-wave-sequencing.md:57-62`). SPEC requires CHALLENGE for W1a-W7 (`SPEC.md:206-211`), and DISPATCH makes CHALLENGE mandatory for W1a and W1b before redress (`DISPATCH-PROMPT.md:98-115`). |
| Same-wave consumer/checkasm/micro-proof cost fit | FOLDED | SPEC blocks W2-W7 redress until the plan records scalar/oracle proof, strict differential/checkasm where applicable, same-host microbench, same-wave consumer path, row gate, fallback, and REDRESS reject boundary (`SPEC.md:213-227`). P3-B and DISPATCH both make missing consumer/checkasm evidence a reject-before-redress condition rather than deferred work (`p3b-wave-sequencing.md:115-119`, `DISPATCH-PROMPT.md:91-94`, `:123-125`). |

## Cost Assessment

The revised budget fits the skinny triumvirate contract. Each behavior wave
keeps the 75-minute target / 90-minute hard cap visible and avoids broad row
sweeps. The expensive first-of-class non-JSON work is split across W1a and
W1b, while W2 consumes the W1b baseline instead of creating the first baseline
and admitting an intervention in the same redress wave. Generated-output review
is bounded by named inputs, so the handwritten redress budget remains
auditable.

W8/W9 close is now costed as accounting, not as hidden implementation. If a
final source route is needed, it must become W8a and consume the only spare
slot. A second split escalates rather than silently extending the bracket.

## Residual Risk

The remaining risk is executional: a later wave plan could still select too
large a row subset or try to treat checkasm-only proof as production evidence.
V2 gives CH4 enough explicit gates to reject that at wave-plan time, so this is
not an S-P3 packet defect.

## Required Action

None for CH4. Proceed only if the other V2 CHALLENGE lenses also accept.
