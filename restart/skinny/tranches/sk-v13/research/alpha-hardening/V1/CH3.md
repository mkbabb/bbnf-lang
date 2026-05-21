# CH3 Regression / Redress - SK-V13 Pass Alpha V1

Date: 2026-05-21.
Role: Alpha CH3 REGRESSION/REDRESS for SK-V13 Pass Alpha V1.
Verdict: REVISE.

## Scope Read

Authority read: `restart/prompts/ORCHESTRATOR.md` §3W, `restart/prompts/pass-contracts/PASS-ALPHA.md` §3, SK-V13 Alpha artifacts, the 2026-05-21 user-pin addendum, `skinny/REDRESS.md` through REDRESS-127, `skinny/RESULTS.md`, and the SK-V13 scoping packet.

CH3 asks whether the Alpha packet reopens prior REDRESS routes, identifies pre-blocks correctly, and avoids silent regression. The answer is mostly yes for the binding `SYNTHESIS.md`, `HANDOFF.md`, Alpha-C, Alpha-D, and Alpha-E surfaces, but one scoping artifact and one Alpha-C summary line can be read too narrowly against the addendum. Those require revision before convergence.

## Findings

### R1 - Reopen Candidate Survey Silently Narrows A2

Disposition: REVISE.

`sk-v13-scoping-profile-truth.md` classifies only three REDRESS-119 direct rows as reopen candidates and marks the other ten `NO` or `CONDITIONAL` (`twitter`, `github_events`, and `update_center` are the only positive set). That conflicts with the binding addendum, which says the ten rows outside the old three-row shortlist are equally reopen-eligible and every row must be re-attempted in light of W5-W12 outputs. It also conflicts with the SK-V13 `SYNTHESIS.md` and Alpha-C surfaces, which correctly say all direct residual rows reopen under A2.

Required fold: change the survey language from row eligibility to priority/risk. The three rows may remain "first measurable candidates", but all 13 REDRESS-119 direct rows must remain W11.N obligations unless already admitted or architecturally blocked. Do not use "NO (no route)" or "NO (prior hard rejection)" as close/pre-block language under SK-V13; use "lower-priority until W5-W12 material differential exists".

Citations: `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:60-74`, `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md:105-130`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:95-109`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-C-redress-digest.md:98-110`.

### R2 - Alpha-C Mandatory Reopen Set Omits Ten Typed Rows

Disposition: REVISE.

Alpha-C's classification summary names "13 JSON direct residuals, 17 JSON parse-only rows, 23 CSS parity features" as the mandatory reopen set. That is true but incomplete under A2/G5: the binding bar is all 17 corpora x 3 planes, and Alpha-A records 10 absent `real_typed_struct` rows that must become explicit admissions or explicit blockers. The binding `SYNTHESIS.md` corrects this with "all 51 JSON rows"; Alpha-C should not leave a narrower mandatory set in the redress digest.

Required fold: revise Alpha-C summary to include all 51 JSON rows: 13 direct residuals, all 17 parse-only rows, all 17 real-typed rows including 10 absent typed rows, plus guard-refresh/no-demotion obligations for already admitted rows. This is a redress accounting issue, not a route permission issue.

Citations: `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:33-46`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:95-109`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:161-180`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:69-99`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-C-redress-digest.md:207-215`.

### A1 - REDRESS-119/120 Are Correctly History, Not Active Blockers

Disposition: ACCEPT.

The Alpha packet correctly demotes REDRESS-119 and REDRESS-120 from active closure authority to history. REDRESS-119 was a measured direct fixpoint with no source intervention and no row movement; REDRESS-120 closed SK-V11 as measured fixpoint, not overall direct GO or grammar-generalization admission. The addendum explicitly lifts that fixpoint and makes each row wave-eligible. `SYNTHESIS.md`, `HANDOFF.md`, Alpha-C, Alpha-D, and Alpha-E all carry that posture.

Citations: `skinny/REDRESS.md:3497-3527`, `skinny/REDRESS.md:3531-3553`, `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:58-74`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:102-109`, `restart/skinny/tranches/sk-v13/HANDOFF.md:62-65`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-C-redress-digest.md:13-28`.

### A2 - 96/97/98 Category Reopen Logic Is Correctly Bounded

Disposition: ACCEPT.

REDRESS-96 and REDRESS-97 measured two faithful union substrate implementations and both regressed; REDRESS-98 retired that SK-V9 gate and specifically rejected class-lane-only proof without a producer as paper-close. Alpha-C and `SYNTHESIS.md` preserve the exact-route pre-blocks while allowing category-level reopen only with material differential, same-tape discipline, parity/checkasm where applicable, and same-wave measured consumer. That is the right CH3 posture: no silent reopening of the old class-column/vector, streaming-cursor, or proof-only routes, but no category ban after the user pin.

Citations: `skinny/REDRESS.md:2797-2848`, `skinny/REDRESS.md:2852-2906`, `skinny/REDRESS.md:2910-2940`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:73-82`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-C-redress-digest.md:81-83`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-C-redress-digest.md:120-137`.

### A3 - 88/89/90 ASM/SIMD Reopen Logic Is Correctly Bounded

Disposition: ACCEPT.

REDRESS-88 rejected PMULL as the default hot prefix-XOR body after JSON hard-row regressions; REDRESS-89 rejected the CTZ/bulk consumer after parse-only maintain failures; REDRESS-90 admitted only B6 stack-canary hardening and explicitly left both bitmap ASM bodies rejected. Alpha-C and the SK-V13 contract correctly pre-block those exact implementations while allowing only row-consumed, scalar/checkasm-proven, materially different SIMD/ASM routes. REDRESS-126 is also correctly treated as route evidence, not production admission.

Citations: `skinny/REDRESS.md:2510-2540`, `skinny/REDRESS.md:2544-2585`, `skinny/REDRESS.md:2589-2598`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-C-redress-digest.md:76-78`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-C-redress-digest.md:139-147`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:84-93`.

### A4 - No Silent Demotion / No Implementation-Limited Close Is Binding

Disposition: ACCEPT.

The Alpha-F contract carries the addendum's close bar: implementation-limited misses are reopens, support-only landings are invalid, old A/GO rows cannot silently demote, and any incomplete tranche must bracket forward unless every row/feature is admitted or architecturally blocked. The handoff repeats the refusal conditions, blocks W0 before G-Omega, and requires fresh JSON/CSS profile truth before implementation waves. This satisfies CH3's regression guard, subject to R1/R2 being folded.

Citations: `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:48-56`, `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:89-102`, `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:151-170`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:30-36`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:123-129`, `restart/skinny/tranches/sk-v13/HANDOFF.md:138-151`.

## Disposition

Overall V1 CH3 verdict: REVISE, not REJECT.

The core Alpha-F contract is regression-safe: exact rejected routes remain pre-blocked, category-level reopens require material differential and measured same-wave consumers, REDRESS-119/120 are history only, and implementation-limited close is refused. The two revisions are accounting/fold issues: the profile survey must not demote ten direct residuals from mandatory reopen obligations, and Alpha-C must include the full 51-row JSON target in its mandatory reopen summary.
