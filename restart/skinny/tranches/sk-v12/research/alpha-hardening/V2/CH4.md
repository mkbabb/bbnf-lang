# SK-V12 Pass Alpha Hardening V2 - CH4 Cost / Tranche Budget

Date: 2026-05-20.
Lens: CH4 cost / tranche budget.
Scope: USER-PIN V1, Pass Alpha contract, skinny triumvirate caps, V1
consolidated hardening, and V2-folded Alpha-E/F, SYNTHESIS, and HANDOFF.
Output: `restart/skinny/tranches/sk-v12/research/alpha-hardening/V2/CH4.md`.

## Verdict

PASS.

V2 now carries a plausible cost/tranche shape for G-Alpha presentation. The
budget-critical V1 defects are folded: the packet restores pass order, splits
W1 legality from W1 row movement, applies the tighter 20/15/30 campaign cap,
states hand LOC caps and generated LOC ceilings, requires generated LOC
regression accounting, and names rollback/failure actions for the wave seed.

## Checks

| Check | Disposition | Evidence |
|---|---|---|
| Pass/wave split | PASS | `SYNTHESIS.md` requires G-Alpha, then SK-V12 S-P1 Profile, S-P2 Research, and S-P3 Synthesis-Plan before any downstream wave authority (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:237`-`restart/skinny/tranches/sk-v12/SYNTHESIS.md:254`). `HANDOFF.md` mirrors that boundary and keeps source work out of Alpha (`restart/skinny/tranches/sk-v12/HANDOFF.md:105`-`restart/skinny/tranches/sk-v12/HANDOFF.md:125`, `restart/skinny/tranches/sk-v12/HANDOFF.md:168`-`restart/skinny/tranches/sk-v12/HANDOFF.md:173`). |
| 20/15/30 caps | PASS | Alpha-E binds all wave seeds to 20 min research, 15 min plan, and 30 min redress (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:71`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:73`). Alpha-F and SYNTHESIS repeat 20/15/30 for W0-W5 (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:220`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:226`, `restart/skinny/tranches/sk-v12/SYNTHESIS.md:265`-`restart/skinny/tranches/sk-v12/SYNTHESIS.md:271`). |
| Hand LOC caps | PASS | Alpha-E names W1a/E2 <=360, W1b/E1 <=620, W2/E3 <=180, W3/E4 <=420, W4/E5 <=430, and W5 docs-only (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:56`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:63`). Alpha-F and SYNTHESIS carry the same seed caps into G-Alpha (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:215`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:226`, `restart/skinny/tranches/sk-v12/SYNTHESIS.md:260`-`restart/skinny/tranches/sk-v12/SYNTHESIS.md:271`). |
| Generated LOC ceilings | PASS | Alpha-E supplies generated ceilings: W1a <=1200 smoke/regenerated LOC, W1b <=12000 CSS declaration-value generated LOC unless S-P3 proves a tighter/full-stylesheet ceiling, W2 zero, W3 <=4000 generated delta over E1, W4 <=2500 generated delta over current CSS runtime, W5 zero (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:56`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:63`). |
| O(N) regression discipline | PASS | Alpha-E requires each S-P3 generated output slice to report pre/post generated LOC and return REVISE on an unexplained O(N) increase (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:71`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:73`). |
| W1a/W1b separation | PASS | Alpha-E states E2 and E1 are sequential, with E2 as the generic-crate prerequisite and E1 as the row-moving CSS admission attempt (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:65`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:67`). Alpha-F requires W1a to legalize/prove the grammar-neutral config surface and W1b to create the CSS L4 row plus lightningcss comparator (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:168`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:171`). |
| Rollback/failure action | PASS | Alpha-E gives detailed revert actions for E1 through E5, including rejected patch paths and dependency blocking (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:133`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:136`, `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:182`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:185`, `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:221`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:223`, `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:276`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:278`, `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:335`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:339`). Alpha-F/SYNTHESIS add the per-wave failure-action table for W0-W5 (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:215`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:226`, `restart/skinny/tranches/sk-v12/SYNTHESIS.md:260`-`restart/skinny/tranches/sk-v12/SYNTHESIS.md:271`). |

## Precise Folds

None required under CH4.

Non-blocking note: `HANDOFF.md` uses a compact seed table that lists the W1a-W5
hand caps and patch paths, while Alpha-F and SYNTHESIS carry the full minute-cap
columns. This is acceptable because the authoritative V2 packet has the
20/15/30 caps in two folded surfaces and Alpha-E binds them across all wave
seeds.
