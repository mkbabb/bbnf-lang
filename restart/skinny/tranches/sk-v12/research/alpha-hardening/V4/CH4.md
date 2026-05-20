# SK-V12 Pass Alpha Hardening V4 - CH4 Cost / Tranche Budget

Date: 2026-05-20.
Lens: CH4 cost / tranche budget.
Scope: USER-PIN V1, Pass Alpha contract, skinny triumvirate caps, V3
consolidated hardening, current Alpha-E/F, SYNTHESIS/HANDOFF, and
`research/g-alpha/G-ALPHA-SK-V12.md`.
Output: `restart/skinny/tranches/sk-v12/research/alpha-hardening/V4/CH4.md`.

## Verdict

PASS.

V4 is CH4-clean. The G-Alpha replacement fixes the V3 cost-surface blocker
without changing the accepted tranche budget: it presents only G-Alpha, then
pin-aware S-P1/S-P2/S-P3, and then W0-W5 after downstream S-P3 materializes
replacement implementation authority. The existing W1a/W1b split, 20/15/30
caps, hand LOC caps, generated LOC ceilings, O(N) accounting, and rollback
actions remain intact.

## Checks

| Check | Disposition | Evidence |
|---|---|---|
| Pass/wave split | PASS | G-Alpha now authorizes only S-P1, S-P2, S-P3, and later W0-W5 after S-P3 produces replacement `SPEC.md`/`DISPATCH-PROMPT.md`; Alpha itself authorizes no behavior source edits (`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:38`-`:48`). Alpha-F and SYNTHESIS carry the same boundary (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:11`-`:16`; `restart/skinny/tranches/sk-v12/SYNTHESIS.md:5`-`:11`). |
| 20/15/30 caps | PASS | Alpha-E binds all wave seeds to 20 min research, 15 min plan, and 30 min redress (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:73`-`:75`). G-Alpha repeats 20/15/30 for W0-W5 (`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:118`-`:124`). |
| Hand LOC caps | PASS | Alpha-E lists W1a/E2 <=360, W1b/E1 <=620, W2/E3 <=180, W3/E4 <=420, W4/E5 <=430, and W5 docs-only (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:58`-`:65`). G-Alpha carries the same W1a-W5 hand caps (`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:119`-`:124`). |
| Generated LOC ceilings | PASS | Generated output remains budgeted in Alpha-E rather than the summary G-Alpha table: W1a <=1200 smoke/regenerated LOC, W1b <=12000 CSS declaration-value generated LOC unless S-P3 proves a tighter/full-stylesheet ceiling, W2 zero, W3 <=4000 generated delta over E1, W4 <=2500 generated delta over current CSS runtime, and W5 zero (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:58`-`:65`). G-Alpha retains Alpha-E as authority (`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:21`-`:27`). |
| O(N) discipline | PASS | Alpha-E requires each S-P3 generated output slice to report pre/post generated LOC and return REVISE on unexplained O(N) increase (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:73`-`:75`). G-Alpha does not weaken that rule because it delegates wave authority to downstream S-P3 and retains Alpha-E/F/SYNTHESIS as authorities (`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:14`-`:29`, `:41`-`:48`). |
| W1a/W1b separation | PASS | Alpha-E states E2 and E1 are sequential, not one redress slice (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:67`-`:72`). Alpha-F requires W1a to legalize/prove `GrammarConfig` and W1b to create the CSS L4 row plus lightningcss comparator (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:168`-`:171`). G-Alpha preserves that split in the seed table (`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:119`-`:120`). |
| Rollback/failure actions | PASS | Alpha-E gives rejected-patch paths and dependency blockers for W1b, W1a, W2, W3, and W4 (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:135`-`:138`, `:188`-`:191`, `:227`-`:229`, `:282`-`:284`, `:341`-`:345`). G-Alpha carries the summary failure actions for W0-W5 (`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:118`-`:124`). |
| G-Alpha seed table | PASS | The V4 seed table matches the accepted Alpha-F/SYNTHESIS shape: S-P1/S-P2/S-P3 are pass-prompt/docs gates, W0 is docs-only revalidation, W1a/W1b remain split, W2 gates SIMD correctness, W3/W4 reserve union and ASM-gen attempts, and W5 closes or synthesizes SK-V13 on unmet close (`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:111`-`:124`; `restart/skinny/tranches/sk-v12/SYNTHESIS.md:260`-`:276`). |

## Precise Folds

None required under CH4.

Non-blocking note: G-Alpha is a presentation summary, so it need not duplicate
the generated LOC ceiling and O(N) rows already carried by Alpha-E. It remains
CH4-acceptable because it retains Alpha-E/F/SYNTHESIS as authority and does not
create direct wave dispatch authority.
